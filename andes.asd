;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; Use this to compile:
;;;;  (asdf:operate 'asdf:load-op 'andes)
;;;; or use command (rkb) if it is defined.
;;;  To turn on solver logging:
#|
:cd /home/bvds/Andes2/ 
(solve-do-log "t") 
|#


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


(defsystem :andes
  :name "Andes"
  :description "Andes physics tutor system"
  :components (
;;;    this should eventually be removed
	       (:file "andes-path")
	       (:module "Base"
;;;			:description "Common Utilities"
			:components ((:file "Htime")
				     (:file "Unification")
				     (:file "Utility")))
	       (:module "Solver_Release"
			:depends-on ("andes-path")
			:components ((:file "solver")))
	       (:module "HelpStructs"
			:depends-on ("Base")
			:components ((:file "PsmGraph")
				     (:file "SystemEntry"
					    :depends-on ("PsmGraph"))
				     ))
	       (:module "Knowledge"
			:depends-on ("Base" "Solver_Release" "HelpStructs")
			:components ((:file "eqn")         
				     (:file "Nogood")    
				     (:file "Operators")  
				     (:file "qvar")
				     (:file "BubbleGraph" 
				     ;; also depends on HelpStructs
					    :depends-on ("qvar" "eqn"))  
				     (:file "ErrorClass")  
				     ;; NLG not defined
				     (:no-compile-file "Ontology")  
				     (:file "Problem") ;depends on HelpStructs
				     (:file "Solution")))	       
	       (:module "KB"
;;;	    	:description "Knowledge Base"
			:depends-on ("Knowledge" "Base" "SGG")
			:serial t  ;real dependancies would be better
			:components ((:file "Physics-Funcs")
				     ;; TELL and NLG not defined
				     (:no-compile-file "Ontology" )
				     (:file "circuit-ontology")  
				     ;; AXES-DRAWNP not defined
				     (:no-compile-file "Newtons2") 
				     (:file "NewtonsNogoods")  
				     (:no-compile-file "Problems")
				     (:no-compile-file "impulse-problems") 
				     (:file "waves")
				     (:no-compile-file "waves-problems")
				     ;; depends on "waves":
				     (:no-compile-file "oscillations-problems")
				     ;; lots of outside dependencies:
				     (:no-compile-file "errors")
				     (:no-compile-file "force-problems")  
;;; Hey, these are the wrong mountains
				     ;; (:file "PyreneesProblems")
				     (:file "forces")          
				     (:file "optics")          
				     (:file "vectors")
				     (:file "makeprob")        
				     (:no-compile-file "vectors-problems")
				     ;; there is some ugly code here
				     (:no-compile-file "circuits")
				     (:no-compile-file "reset-KB")
				     ))
	       (:module "SGG"
;;;			:description "Solution Graph Generator" 
			:depends-on ("Base" "Knowledge" "HelpStructs")
			:components ((:file "Qsolver") ;depends on HelpStructs
				     (:file "Exec" 
					    :depends-on ("Qsolver"))
				     (:file "Macros")         
				     (:file "SolutionPoint")
				     (:file "GraphGenerator")
				     (:file "ProblemSolver")
				     (:file "SolutionSets")))
))

;;;  make lisp source file extension "cl"  See asdf manual

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :andes))))
   "cl")

