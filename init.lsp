;;;;
;;;;  gcl specific initialization
;;;;
;;;;
;;;; need asdf, this is where I have it locally
(load "/usr/local/asdf/asdf.lisp")

(setf asdf:*central-registry* ; set default search path
      ;; The current directory
      '(*default-pathname-defaults*
      ;; List additional places ....
	))