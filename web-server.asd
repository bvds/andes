;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; This sets up a web server to use json-rpc to
;;;; handle request methods.

(in-package :cl-user)
(defpackage :web-server-asd (:use :cl :asdf))
(in-package :web-server-asd)

;;;;   Load the source file, without compiling
;;;;   asdf:load-op reloads all files, whether they have been
;;;;   changed or not.

(defclass no-compile-file (cl-source-file) ())
(defmethod perform ((o compile-op) (s no-compile-file)) nil)
(defmethod output-files ((o compile-op) (s no-compile-file))
  (list (component-pathname s)))

;;  Match instructions in the INSTALL file:  don't do ssl when
;;  loading huncentoot.
(push :hunchentoot-no-ssl *features*) ;we have apache to do this

(defsystem :web-server
  :name "Web Server"
  :description "Web Server"
  :depends-on (hunchentoot cl-json clsql clsql-mysql)
  :components (
	       (:module "Base"
			:components ((:file "web-server")))))

;;;  make lisp source file extension "cl"  See asdf manual

(defmethod source-file-type ((c cl-source-file) 
			     (s (eql (find-system :web-server)))) "cl")
