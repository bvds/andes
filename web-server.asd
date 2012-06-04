;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; This sets up a web server to use json-rpc to
;;;; handle request methods.

(in-package :cl-user)
(defpackage :web-server-asd (:use :cl :asdf))
(in-package :web-server-asd)

;;  Match instructions in the INSTALL file:  don't do ssl when
;;  loading huncentoot.
(push :hunchentoot-no-ssl *features*) ;we have apache to do this
(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8) ;for rfc2388.asd

(defsystem :web-server
  :name "Web Server"
  :description "Web Server"
  :depends-on (hunchentoot cl-json)
  :default-component-class cl-source-file.cl ;make *.cl default extension
  :components (
	       (:module "Base"
			:components ((:file "log-condition")
				     (:file "web-server"
					    :depends-on ("log-condition"))))))
