;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; The following was stolen from maxima.asd
;;;; See http://www.math.utexas.edu/pipermail/maxima/2003/005630.html
#+(or sbcl openmcl)
(or (find-package "USER")
    (rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("USER")))
(in-package :user)
(in-package :asdf)

;;;;   Load the source file, without compiling
;;;;   asdf:load-op reloads all files, whether they have been
;;;;   changed or not.

(defclass no-compile-file (asdf:cl-source-file) ())
(defmethod asdf:perform ((o asdf:compile-op) (s no-compile-file))
  nil)
(defmethod asdf:output-files ((o asdf:compile-op) (s no-compile-file))
  (list (component-pathname s)))

(defsystem :problems
  :name "problems"
  :description "Problem definitions"
  :default-component-class no-compile-file
  :components (
	       (:file "kinematics-problems")
	       (:file "dynamics-problems")
	       (:file "fluids-problems")
	       (:file "waves-problems")
	       (:file "work-energy-problems")
	       ;; depends on "waves":
	       (:file "oscillations-problems")
;;; Hey, these are the wrong mountains
	       ;; (:file "PyreneesProblems")
	       (:file "electromagnetism-problems")  
	       (:file "optics-problems")          
	       (:file "momentum-impulse-problems") 
	       (:file "rocket-problems") 
	       (:file "circuit-problems")
	       (:file "statics-problems") 
	       ))

;;;  make lisp source file extension "cl"  See asdf manual

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :problems))))
   "cl")
