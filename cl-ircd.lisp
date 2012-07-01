;;; cl-ircd.lisp --- A Common Lisp IRC Server

;;; Copyright (C) 2012 David Vázquez

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
  (:nicknames :ircd))

(in-package :cl-ircd)

(defvar *servername* "clircd")
(defvar *server*)

(defun mklist (x)
  (if (listp x) x (list x)))

(defun mappend (function list)
  (reduce #'append (mapcar function list)))

(defun char-ci= (char1 char2)
  (char= (char-upcase char1) (char-upcase char2)))

(defun string-ci= (string1 string2)
  (every #'char-ci= string1 string2))

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

(defclass server ()
  ((port
    :initarg :port
    :reader server-port)
   (uptime
    :initarg :uptime
    :initform (get-universal-time)
    :reader server-uptime)
   (nicknames
    :initform (make-hash-table :test #'equalp :weakness :value)
    :reader server-nicknames)
   (users
    :initform nil
    :type list
    :accessor server-users)
   (channels
    :initform (make-hash-table :test #'equal :weakness :value)
    :reader server-channels)
   (socket
    :initarg :socket
    :reader server-socket)))

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
    :type string
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

(defun start-server (&key (port 6668) (host usocket:*wildcard-host*))
  (let* ((socket (usocket:socket-listen host port :reuse-address t :backlog 20))
	 (server (make-instance 'server :port port :socket socket)))
    (bordeaux-threads:make-thread
     (lambda () (loop (accept-connection server))))
    (bordeaux-threads:make-thread
     (lambda () (loop (handle-event server))))
    (setf *server* server)))

(defun stop-server ()
  (usocket:socket-close (server-socket *server*))
  (dolist (user (server-users *server*))
    (usocket:socket-close (user-socket user))))

(defun accept-connection (server)
  (let ((socket (usocket:socket-accept (server-socket server))))
    (push (make-instance 'user :socket socket) (server-users server))))

(defun handle-event (server)
  (let ((sockets (mapcar #'user-socket (server-users server))))
    (cond
      ;; If there are not users, then go to sleep.
      ((null sockets)
       (sleep 1))
      (t
       (usocket:wait-for-input sockets :timeout 5)
       (dolist (user (server-users server))
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
               (end-of-file ()
                 (setf (server-users server)
                       (remove user (server-users server))))))))))))

(defvar *command-table*
  (make-hash-table :test #'equalp))

(defvar *user*)

(defmacro define-command (name args &body body)
  (let ((funcname (concatenate 'string "IRC-" (prin1-to-string name))))
    `(progn (defun ,(intern funcname) ,args ,@body)
            (setf (gethash ,(prin1-to-string name) *command-table*) ',(intern funcname)))))

(defun parse-input-line (string)
  (with-input-from-string (in string)
    (values (and (char= (peek-char nil in) #\:) (parse in))
            (parse in)
            (loop
               for ch = (peek-char nil in nil) while ch
               collect (if (char= ch #\:) (read-line in) (parse in))))))

(defun parse-list (string)
  (loop
     for begin = 0 then (1+ end)
     for end = (position #\, string :start begin)
     collect (subseq string begin end)
     while end))

(defun message (recipents source command &rest arguments)
  (dolist (recipent (mklist recipents))
    (let ((stream (make-broadcast-stream *standard-output* (usocket:socket-stream (user-socket recipent)))))
      (when source
        (format stream ":~a " source))
      (if (numberp command)
          (format stream "~3,'0d" command)
          (princ command stream))
      (loop for (arg more) on arguments
         while more do (format stream " ~a" arg)
         finally (format stream " ~@[~*:~]~a" (find #\space arg) arg))
      (terpri stream)
      (force-output stream))))

(defun rpl (command &rest arguments)
  (apply #'message *user* *servername* command (user-nickname *user*) arguments))

(defgeneric user-source (user)
  (:method ((user user))
    (format nil "~a!~a@~a" (user-nickname user) (user-username user) (user-hostname user))))

(defun propagate (command &rest arguments)
  (let ((source (user-source *user*))
        (recipents ))
    (apply #'message recipents source command arguments)))

(defun process-input (user line)
  (format t "<< ~a~%" line)
  (when (string= line "")
    (return-from process-input))
  (multiple-value-bind (source command args)
      (parse-input-line line)
    (declare (ignorable source))
    (setf (last-activity user) (get-universal-time))
    (let ((*user* user))
      (unless (or (user-registered-p *user*)
                  (find command '("USER" "NICK" "PING") :test #'string-ci=))
        (rpl 451 "You have not registered")
        (return-from process-input))
      (let ((handler (gethash command *command-table*)))
        (if handler
            (handler-case (apply handler args)
              (simple-condition (error)
                (rpl "NOTICE" 
                     (apply #'format nil
                            (simple-condition-format-control error)
                            (simple-condition-format-arguments error)))))
            (rpl 421 "Unknown command"))))))

;;; IRC Commands

(defun ip-to-string (ip)
  (format nil "~a.~a.~a.~a" (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))

(defun timestamp-string (timestamp)
  (multiple-value-bind (second minute hour date month year dow daylight timezone)
      (decode-universal-time timestamp)
    (declare (ignore daylight timezone))
    (let ((days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
          (months #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
      (format nil "~a ~a ~d ~d at ~2,'0d:~2,'0d:~2,'0d" (aref days dow) (aref months month)
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
    ;; If the new nickname already exists
    (when (gethash name nicknames)
      (cond
        ((eq (gethash name nicknames) *user*)
         (remhash (user-nickname *user*) nicknames))
        (t
         (message *user* *servername* 433 "*" "Nickname is already in use")
         (return-from irc-nick))))
    ;; Register the nickname and propagate it if it is a change.
    (setf (user-nickname *user*) name)
    (setf (gethash name (server-nicknames *server*)) *user*)
    (when (user-registered-p *user*)
      (message (delete-duplicates (mappend #'channel-users (user-channels *user*)))
               (user-source *user*)
               "NICK" name))
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
    (message (channel-users channel) (user-source *user*) "JOIN" channel-name)
    (if (channel-topic channel)
        (rpl 332 channel-name (channel-topic channel))
        (rpl 331 channel-name "No topic is set")) ;TPL_TOPIC
    (dolist (user (channel-users channel))
      ;; TODO: Optimize several nicks in the same message.
      (rpl 353 "=" channel-name (user-nickname user))) ;; RPL_NAMREPLY
    (rpl 366 channel-name "End of NAMES list")) ; RPL_ENDOFNAMES
  )

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
  (setf (user-channels *user*)
        (remove channel (user-channels *user*)))
  (setf (channel-users channel)
        (remove *user* (channel-users channel))))

(define-command part (channel-list &optional message)
  (dolist (channame (parse-list channel-list))
    (let ((channel (find-channel channame)))
      (part channel)
      (apply #'message
             (channel-users channel)
             (user-source *user*)
             "PART" (channel-name channel) (mklist message)))))


(define-command mode (target &optional mode)
  (declare (ignore target mode)))

;;; Message commands

(defun find-user (nickname)
  (gethash nickname (server-nicknames *server*)))

(define-command privmsg (recipent message)
  (declare (ignore recipent message)))

(define-command notice (recipent message)
  (declare (ignore recipent message)))


;;; Misc

(define-command ping (server1 &optional server2)
  (declare (ignore server2))
  (message *user* nil "PONG" server1))

(define-command quit (&optional message)
  (mapc #'part (user-channels *user*))
  (apply #'message
         (delete-duplicates (mappend #'channel-users (user-channels *user*)))
         (user-source *user*)
         "QUIT" (mklist message)))


;;; cl-ircd.lisp ends here
