;;;; -*- Lisp -*-

(in-package :cl-user)
(defpackage :help-asd (:use :cl :asdf))
(in-package :help-asd)

;;;;   Load the source file, without compiling
;;;;   asdf:load-op reloads all files, whether they have been
;;;;   changed or not.

(defclass no-compile-file (cl-source-file) ())
(defmethod perform ((o compile-op) (s no-compile-file)) nil)
(defmethod output-files ((o compile-op) (s no-compile-file))
  (list (component-pathname s)))


(defsystem :lon-capa
  :name "LON-CAPA"
  :description "Creat LON-CAPA courses"
  :depends-on (problems)
  :components (
	       (:module "lon-capa"
			:components ((:file "assignments")
				     (:file "sets")))))

;;;  make source file extension "cl"  See asdf manual

(defmethod source-file-type ((c cl-source-file) 
			     (s (eql (find-system :lon-capa)))) "cl")
