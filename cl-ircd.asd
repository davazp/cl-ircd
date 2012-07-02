;;; cl-ircd.asd --- 

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

(defpackage :cl-ircd-system
  (:use :cl :asdf)
  (:export #:*version* #:*software*))

(in-package :cl-ircd-system)

(defvar *version* "0.1")
(defvar *software* (format nil "cl-ircd-~a" *version*))

(defsystem :cl-ircd
    :name "cl-ircd"
    :description "Common Lisp IRC Server"
    :version #.*version*
    :depends-on (:usocket :bordeaux-threads)
    :serial t
    :components
    ((:static-file "COPYING")
     (:file "cl-ircd")))

;;; cl-ircd.asd ends here
