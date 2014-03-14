;;; cl-ircd.lisp --- A Common Lisp IRC Server

;;; Copyright (C) 2012, 2014 David Vázquez

;; This file is part of cl-ircd.
;;
;; cl-ircd is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-ircd is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-ircd.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :cl-ircd
  (:use :cl :cl-ircd-system)
  (:nicknames :ircd)
  (:export #:start-server #:stop-server))

(in-package :cl-ircd)

(defvar *servername* "clircd")
(defvar *server*)

;;; Return X if it is a list, or a list whose single element is X
;;; otherwise.
(defun mklist (x)
  (if (listp x) x (list x)))

;;; Map FUNCTION on list and append the resulting lists.
(defun mappend (function list)
  (reduce #'append (mapcar function list)))

(defun first-line (string)
  (with-input-from-string (in string)
    (read-line in nil)))

;;; Remove X from PLACE and set the result in PLACE again.
(defmacro removef (x place)
  (let ((placevar (gensym))
        (valuevar (gensym)))
    `(let ((,valuevar ,x)
           (,placevar ,place))
       (setf ,place (remove ,valuevar ,placevar)))))

;;; Compare two characters case-insensitively.
(defun char-ci= (char1 char2)
  (char= (char-upcase char1) (char-upcase char2)))

;;; Compare two strings case-insensitively.
(defun string-ci= (string1 string2)
  (every #'char-ci= string1 string2))

;;; Check if CHAR is a whitespace (newline or space).
(defun whitespacep (char)
  (or (char= char #\newline) (char= char #\space)))

;;; Read characters from STREAM until it finds a space, then discard
;;; all the remainder spaces.
(defun parse (stream)
  (with-output-to-string (out)
    (loop
       for ch = (peek-char nil stream nil)
       until (or (null ch) (whitespacep ch))
       do (write-char (read-char stream) out))
    (loop
       for ch = (peek-char nil stream nil)
       while (and ch (whitespacep ch))
       do (read-char stream))))


;;; Classes

(defclass server ()
  ((port
    :initarg :port
    :reader server-port)
   (uptime
    :initarg :uptime
    :initform (get-universal-time)
    :reader server-uptime)
   (nicknames
    :initform (make-hash-table :test #'equalp)
    :reader server-nicknames)
   (users
    :initform nil
    :type list
    :accessor server-users)
   (channels
    :initform (make-hash-table :test #'equal)
    :reader server-channels)
   (socket
    :initarg :socket
    :reader server-socket)))

(defun server-channel-list (&optional (server *server*))
  (let ((result nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value result))
             (server-channels server))
    result))


(defclass channel ()
  ((name
    :initarg :name
    :reader channel-name)
   (topic
    :initarg :topic
    :initform nil
    :type (or null string)
    :accessor channel-topic)
   (users
    :initform nil
    :type list
    :accessor channel-users)))

(defmethod print-object ((x channel) stream)
  (print-unreadable-object (x stream :type t)
    (write (channel-name x) :stream stream)))


(defclass client ()
  ((nickname
    :type (or string null)
    :initform nil
    :accessor user-nickname)
   (socket
    :initarg :socket
    :reader user-socket)
   (hostname
    :type string
    :accessor user-hostname)))

(defclass user (client)
  ((username
    :type string
    :accessor user-username)
   (registered-p
    :initform nil
    :type boolean
    :accessor user-registered-p)
   (channels
    :initform nil
    :type list
    :accessor user-channels)
   (realname
    :type string
    :accessor user-realname)
   (input-buffer
    :initform (make-string-output-stream)
    :reader user-input-buffer)
   (last-activity
    :initform (get-universal-time)
    :type integer
    :accessor last-activity)))

(defun start-server (&key (port 6667) (host usocket:*wildcard-host*))
  (let* ((socket (usocket:socket-listen host port :reuse-address t :backlog 20))
	 (server (make-instance 'server :port port :socket socket)))
    (bordeaux-threads:make-thread
     (lambda () (loop (accept-connection server))))
    (bordeaux-threads:make-thread
     (lambda () (loop (handle-event server))))
    (setf *server* server)))

(defun stop-server ()
  (ignore-errors
    (usocket:socket-close (server-socket *server*)))
  (dolist (user (server-users *server*))
    (ignore-errors
      (usocket:socket-close (user-socket user)))))

(defun accept-connection (server)
  (let ((socket (usocket:socket-accept (server-socket server))))
    (push (make-instance 'user :socket socket) (server-users server))))

(defvar *user*)
(defun handle-event (server)
  (let ((sockets (mapcar #'user-socket (server-users server))))
    (cond
      ;; If there are not users, then go to sleep.
      ((null sockets)
       (sleep 1))
      (t
       (usocket:wait-for-input sockets :timeout 5 :ready-only t)
       (dolist (user (server-users server))
         (let ((*user* user))
           ;; Collect data in the input buffer of the user until newline
           ;; is found. Then process the input line.
           (with-slots (socket input-buffer) user
             (let ((stream (usocket:socket-stream socket)))
               (handler-case
                   (loop for ch = (read-char-no-hang stream) while ch do
                        (if (char= ch #\newline)
                            (let ((line (string-right-trim #(#\Return) (get-output-stream-string input-buffer))))
                              (process-input user line))
                            (write-char ch input-buffer)))
                 #+sbcl
                 (sb-int:closed-stream-error ()
                   (irc-quit))
                 (end-of-file ()
                   (irc-quit)))))))))))

(defvar *command-table*
  (make-hash-table :test #'equalp))

(defmacro define-command (name args &body body)
  (let ((funcname (concatenate 'string "IRC-" (prin1-to-string name))))
    `(progn (defun ,(intern funcname) ,args ,@body)
            (setf (gethash ,(prin1-to-string name) *command-table*) ',(intern funcname)))))

(defun parse-input-arguments* (in)
  (loop
     for ch = (peek-char nil in nil) while ch
     collect (if (char= ch #\:)
                 (progn (read-char in)
                        (if (not (peek-char nil in nil nil))
                            ""
                            (read-line in nil)))
                 (parse in))))

(defun parse-input-arguments (string)
  (with-input-from-string (in string)
    (parse-input-arguments* in)))

(defun parse-input-line (string)
  (with-input-from-string (in string)
    (values (and (char= (peek-char nil in) #\:)
                 (progn (read-char in) (parse in)))
            (parse in)
            (parse-input-arguments* in))))


(defun parse-list (string)
  (when (and string (plusp (length string)))
    (loop
       for begin = 0 then (1+ end)
       for end = (position #\, string :start begin)
       collect (subseq string begin end)
       while end)))

;;; Send a message: SOURCE COMMAND ARGUMENTS... to RECIPIENTS.
(defun message (recipients source command &rest arguments)
  (dolist (recipient (mklist recipients))
    (let ((stream ( usocket:socket-stream (user-socket recipient))))
      (when (output-stream-p stream)
        (let ((stream (make-broadcast-stream *standard-output* stream)))
          (when source
            (format stream ":~a " source))
          (if (numberp command)
              (format stream "~3,'0d" command)
              (princ command stream))
          (loop for (arg more) on arguments
             while more do (format stream " ~a" arg)
             finally (format stream " ~@[~*:~]~a" (or (find #\space arg) (string= arg "")) arg))
          (write-char #\Return stream)
          (terpri stream)
          (force-output stream))))))

;;; Send a message: <server> COMMAND <user> ARGUMENTS to the user.
(defun rpl (command &rest arguments)
  (apply #'message *user* *servername* command (user-nickname *user*) arguments))

;;; Send a message: user COMMAND ARGUMENTS... to RECIPIENTS.
(defun propagate (recipients command &rest arguments)
  (let ((source
         (format nil "~a!~a@~a"
                 (user-nickname *user*)
                 (user-username *user*)
                 (user-hostname *user*))))
    (apply #'message recipients source command arguments)))

(defun visible-users (user)
  (delete-duplicates (mappend #'channel-users (user-channels user))))


(defun process-input (user line)
  (format t "<< ~a~%" line)
  (when (string= line "")
    (return-from process-input))
  (multiple-value-bind (source command args)
      (parse-input-line line)
    (declare (ignorable source))
    (setf (last-activity user) (get-universal-time))
    (unless (or (user-registered-p *user*)
                (find command '("USER" "PASS" "NICK" "PING") :test #'string-ci=))
      (rpl 451 "You have not registered")
      (return-from process-input))
    (let ((handler (gethash command *command-table*)))
      (if handler
          (with-simple-restart (discard-message "Discard message.")
            (apply handler args))
          (rpl 421 "Unknown command")))))

;;; IRC Commands

(defun ip-to-string (ip)
  (format nil "~a.~a.~a.~a" (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))

(defun timestamp-string (timestamp)
  (multiple-value-bind (second minute hour date month year dow daylight timezone)
      (decode-universal-time timestamp 0)
    (declare (ignore daylight timezone))
    (let ((days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
          (months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
      (format nil "~a ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d GMT" (aref days dow) (aref months month)
              date year hour minute second))))

(defun try-to-register-user ()
  (when (and (slot-boundp *user* 'nickname)
             (slot-boundp *user* 'username)
             (not (user-registered-p *user*)))
    (setf (user-registered-p *user*) t)
    (flet ((reply (code fmt &rest args)
             (rpl code (apply #'format nil fmt args))))
      (reply 001 "Welcome to the Internet Relay Network ~a!~a@~a"
             (user-nickname *user*)
             (user-username *user*)
             (user-hostname *user*))
      (reply 002 "Your host is ~a, runnning ~a" *servername* *software*)
      (reply 003 "This server was created ~a" (timestamp-string (server-uptime *server*)))
      (rpl   004 *servername* *software* "i" "o")
      ;; Send MOTD
      (cond
        ((probe-file "MOTD")
         (reply 375 "~a Message of the day - " *servername*)
         (with-open-file (in "MOTD")
           (loop
              for line = (read-line in nil)
              while line
              do (loop
                    for begin = 0 then end
                    for end = (min (length line) (+ begin 80))
                    while (< begin end)
                    do (rpl 372 (subseq line begin end)))))
         (rpl 376 "End of MOTD command"))
        (t
         (rpl 422 "MOTD File is missing"))))))


;;; Registration commands

(define-command pass (password)
  (declare (ignorable password)))

(define-command nick (name)
  (with-slots (nicknames) *server*
    (when (and (user-registered-p *user*)
               (string= name (user-nickname *user*)))
      (return-from irc-nick))
    ;; If the new nickname already exists
    (when (gethash name nicknames)
      (message *user* *servername* 433 "*" name "Nickname is already in use")
      (return-from irc-nick))
    ;; Register the nickname and propagate it if it is a change.
    (remhash (user-nickname *user*) nicknames)
    (when (user-registered-p *user*)
      (propagate (adjoin *user* (visible-users *user*)) "NICK" name))
    (setf (user-nickname *user*) name)
    (setf (gethash name (server-nicknames *server*)) *user*)
    (try-to-register-user)))

(define-command user (username hostname servername realname)
  (declare (ignore hostname servername))
  (setf (user-username *user*) username)
  (setf (user-realname *user*) realname)
  (setf (user-hostname *user*) (ip-to-string (usocket:get-peer-address (user-socket *user*))))
  (try-to-register-user))

;;; Channel commands

(defun find-channel (channel-name)
  (gethash channel-name (server-channels *server*)))

(defun create-channel (channel-name)
  (or (find-channel channel-name)
      (let ((channel (make-instance 'channel :name channel-name)))
        (setf (gethash channel-name (server-channels *server*)) channel)
        channel)))

(defun join-1 (channel-name &optional password)
  (declare (ignorable password))
  (let ((channel (create-channel channel-name)))
    (push *user* (channel-users channel))
    (push channel (user-channels *user*))
    (propagate (channel-users channel) "JOIN" channel-name)
    (if (channel-topic channel)
        (rpl 332 channel-name (channel-topic channel))
        (rpl 331 channel-name "No topic is set")) ;TPL_TOPIC
    (irc-names channel-name)))

(define-command join (channel-list &optional pass-list)
  (when (string= channel-list "0")
    (return-from irc-join))
  (let ((channels (parse-list channel-list))
        (passwds (and pass-list (parse-list channel-list))))
    (loop
       for channel-name in channels
       for passwd-tail = passwds then (cdr passwd-tail)
       for passwd = (car passwd-tail)
       do (join-1 channel-name passwd))))

(defun part (channel)
  (removef channel (user-channels *user*))
  (removef *user* (channel-users channel)))

(define-command part (channel-list &optional message)
  (dolist (channame (parse-list channel-list))
    (let ((channel (find-channel channame)))
      (apply #'propagate (channel-users channel) "PART" (channel-name channel) (mklist message))
      (part channel))))

(define-command names (&optional channame-list target)
  (declare (ignore target))
  ;; TODO: If CHANNEL-NAME is omitted, list all the channels.
  (when channame-list
    (let ((channels (remove nil (mapcar #'find-channel (parse-list channame-list)))))
      (dolist (channel channels)
        (let* ((name (channel-name channel))
               ;; :<servername> 353 nickname = channel-name :<nicknames>....\r\n
               (width  (- 512 (length *servername*) 12 (length (user-nickname *user*)) (length name)))
               (remainder width)
               (nicknames nil))
          (dolist (user (channel-users channel))
            (when (< remainder (length (user-nickname user)))
              (rpl 353 "=" name (format nil "~{~a~^ ~}" nicknames)) ; RPL_NAMREPLY
              (setf nicknames nil)
              (setf remainder width))
            (decf remainder (1+ (length (user-nickname user))))
            (push (user-nickname user) nicknames))
          (when nicknames
            (rpl 353 "=" name (format nil "~{~a~^ ~}" nicknames))) ; RPL_NAMREPLY
          (rpl 366 name "End of NAMES list") ; RPL_ENDOFNAMES
          )))))

(define-command list (&optional channels target)
  (declare (ignore target))
  (princ channels)
  (let ((list (if (and channels (plusp (length channels)))
                  (mapcar #'find-channel (parse-list channels))
                  (server-channel-list))))
    (rpl 321)                           ; deprecated, but ERC expects
    (dolist (item list)
      (rpl 322 (channel-name item) (length (channel-users item)) (channel-topic item)))
    (rpl 323 "End of LIST")))

(define-command mode (target &optional mode)
  (declare (ignore target mode)))

(define-command topic (channame &optional message)
  (let ((channel (find-channel channame)))
    (when channel
      (if (null message)
          (rpl 332 channame (channel-topic channel))
          (let ((newtopic (if (string= message "") nil message)))
            (setf (channel-topic channel) newtopic)
            (propagate (channel-users channel) "TOPIC" channame message))))))

;;; Message commands

(defun find-user (nickname)
  (gethash nickname (server-nicknames *server*)))

(defun list-of-users (string)
  (if (find (char string 0) "#")
      (let ((channel (find-channel string)))
        (if channel
            (channel-users channel)))
      (list (find-user string))))

(define-command privmsg (recipient message)
  (let ((users (list-of-users recipient)))
    (if (null users)
        (rpl 401 recipient recipient "No such nick/channel")
        (propagate (remove *user* users) "PRIVMSG" recipient message))))

(define-command notice (recipient message)
  (let ((users (list-of-users recipient)))
    (if (null users)
        (rpl 401 recipient recipient "No such nick/channel")
        (propagate (remove *user* users) "NOTICE" recipient message))))


;;; Misc

(define-command ping (server1 &optional server2)
  (declare (ignore server2))
  (message *user* nil "PONG" server1))

(define-command quit (&optional message)
  (ignore-errors 
    (apply #'propagate (visible-users *user*) "QUIT" (mklist message)))
  (ignore-errors 
    (message *user* "ERROR"
             (format nil "Closing Link: ~a ~@[~a~]"
                     (user-hostname *user*)
                     message)))

  (removef *user* (server-users *server*))
  (mapc #'part (user-channels *user*))
  (removef *user* (server-users *server*))
  (remhash (user-nickname *user*) (server-nicknames *server*))
  (usocket:socket-close (user-socket *user*)))


;;; cl-ircd.lisp ends here
