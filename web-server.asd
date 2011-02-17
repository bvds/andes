;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; This sets up a web server to use json-rpc to
;;;; handle request methods.

(in-package :cl-user)
(defpackage :web-server-asd (:use :cl :asdf))
(in-package :web-server-asd)

;;  make lisp source file extension "cl"  See asdf manual
#+asdf2 (defclass my-cl-source-file (cl-source-file) ((type :initform "cl")))
#-asdf2 (defmethod source-file-type ((f my-cl-source-file) (m module))
	  (declare (ignorable f m)) "cl")

;;  Match instructions in the INSTALL file:  don't do ssl when
;;  loading huncentoot.
(push :hunchentoot-no-ssl *features*) ;we have apache to do this

(defsystem :web-server
  :name "Web Server"
  :default-component-class my-cl-source-file
  :description "Web Server"
  :depends-on (hunchentoot cl-json)
  :components (
	       (:module "Base"
			:components ((:file "web-server")))))
