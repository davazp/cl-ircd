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
  (:use :cl)
  (:nicknames :ircd))

(in-package :cl-ircd)

(defvar *servername* "clircd.localhost")

(defun whitespacep (char)
  (or (char= char #\newline) (char= char #\space)))

;;; Read characters from STREAM until it finds a space, then discard
;;; all the remainder spaces.
(defun parse (stream)
  (string-right-trim '(#\Return)
   (with-output-to-string (out)
     (loop
        for ch = (peek-char nil stream nil)
        until (or (null ch) (whitespacep ch))
        do (write-char (read-char stream) out))
     (loop
        for ch = (peek-char nil stream nil)
        while (and ch (whitespacep ch))
        do (read-char stream)))))

(defclass server ()
  ((port
    :initarg :port
    :reader server-port)
   (nicknames
    :initform (make-hash-table :weakness :value)
    :reader server-nicknames)
   (users
    :initform nil
    :type list
    :accessor server-users)
   (socket
    :initarg :socket
    :reader server-socket)))

(defclass channel ()
  ((name
    :initarg :name
    :reader channel-name)
   (users
    :initform nil
    :accessor channel-users)))


(defclass user ()
  ((username
    :type string
    :accessor user-username)
   (realname
    :type string
    :accessor user-realname)
   (nickname
    :type string
    :accessor user-nickname)
   (input-buffer
    :initform (make-string-output-stream)
    :reader user-input-buffer)
   (socket
    :initarg :socket
    :reader user-socket)
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
    server))

(defun stop-server (server)
  (usocket:socket-close (server-socket server))
  (dolist (user (server-users server))
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
                          (process-input server user (get-output-stream-string input-buffer))
                          (write-char ch input-buffer)))
               (end-of-file ()
                 (setf (server-users server)
                       (remove user (server-users server))))))))))))

(defun sendline (user fmt &rest args)
  (let ((socket (usocket:socket-stream (user-socket user))))
    (apply #'format socket fmt args)
    (force-output socket)))

(defvar *command-table*
  (make-hash-table :test #'equalp))

(defvar *server*)
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

(defun rpl (command &rest arguments)
  (let ((stream (usocket:socket-stream (user-socket *user*))))
    (format stream ":~a " *servername*)
    (prin1 command stream)
    (loop for (arg more) on arguments
       while more do (format stream " ~a" arg)
       finally (format stream " ~@[~*:~]~a" (find #\space arg) arg))
    (terpri stream)
    (force-output stream)))

(defun process-input (server user line)
  (multiple-value-bind (source command args)
      (parse-input-line line)
    (declare (ignorable source))
    (setf (last-activity user) (get-universal-time))
    (let ((*user* user)
          (*server* server))
      (let ((handler (gethash command *command-table*)))
        (if handler
            (apply handler args)
            (rpl 421 "Unknown command"))))))


(define-command nick (name)
  (when (gethash name (server-nicknames *server*))
    ;; nickname at use.
    )
  (setf (user-nickname *user*) name))


(define-command user (username hostname servername realname)
  (declare (ignore hostname servername))
  (setf (user-username *user*) username)
  (setf (user-realname *user*) realname))


;;; cl-ircd.lisp ends here
