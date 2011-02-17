;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; Use this to compile:
;;;;  (asdf:operate 'asdf:load-op 'andes)
;;;; or use command (rkb) if it is defined.
;;;  To turn on solver logging:  :cd ~/Andes2/ 

(in-package :cl-user)
(defpackage :andes-asd (:use :cl :asdf))
(in-package :andes-asd)

;;  make lisp source file extension "cl"  See asdf manual
#+asdf2 (defclass my-cl-source-file (cl-source-file) ((type :initform "cl")))
#-asdf2 (defmethod source-file-type ((f my-cl-source-file) (m module))
	  (declare (ignorable f m)) "cl")


;;;;   Load the source file, without compiling
;;;;   asdf:load-op reloads all files, whether they have been
;;;;   changed or not.

;(defclass no-compile-file (cl-source-file) ((type :initform "cl")))
;(defmethod perform ((o compile-op) (s no-compile-file)) nil)
;(defmethod output-files ((o compile-op) (s no-compile-file))
;  (list (component-pathname s)))


(defsystem :andes
  :name "Andes"
  :default-component-class my-cl-source-file
  :description "Andes physics tutor system"
  :depends-on (cl-json)  ;KB/principles.cl
  :components (
;;;    this should eventually be removed
	       (:file "andes-path")
	       (:module "Base"
;;;			:description "Common Utilities"
			:components ((:file "Htime")
				     (:file "Unification")
				     (:file "auxiliary")
				     (:file "hash")
				     (:file "match")
				     (:file "Utility")))
	       (:module "Algebra"
			:components ((:file "solver")))
	       (:module "HelpStructs"
			:depends-on ("Base")
			:components ((:file "PsmGraph")
				     (:file "StudentEntry")
				     (:file "SystemEntry"
					    :depends-on ("PsmGraph"))
				     ))
	       (:module "Knowledge"
			:depends-on ("Base" "HelpStructs" "Algebra")
			:components ((:file "eqn")         
				     (:file "Nogood") 
				     ;; depends on HelpStructs
				     (:file "Problem")
				     (:file "Ontology")  

				     ;; Natural language
				     (:file "nlg"
					    ;; also Help/symbols.cl
					    :depends-on ("Problem" "Ontology"))
				     (:file "all-quantities"
					    :depends-on ("nlg" "Problem"))

				     (:file "Operators"
					    :depends-on ("nlg"))  
				     (:file "qvar"
					    :depends-on ("Problem"))
				     (:file "BubbleGraph" 
				     ;; also depends on HelpStructs
					    :depends-on ("qvar" "eqn" "Problem"))
				     (:file "ErrorClass"
					    :depends-on ("Problem"))
				     (:file "Solution"
					    :depends-on ("Problem"))))	       
	       (:module "KB"
;;;	    	:description "Knowledge Base"
			;; Also depends on nlg
			:depends-on ("Knowledge" "Base")
			;:default-component-class no-compile-file
  			:serial t  ;real dependancies would be better
			:components (
				     ;; treat these as normal lisp files
				     (:file "Physics-Funcs")
				     (:file "makeprob")        
				     
				     ;; must be before any ontology
				     (:file "reset-KB")
				     (:file "principles")
				     (:file "quantities")
				     (:file "constants")
				     ;; lots of outside dependencies:
				     (:file "errors")
				     ;; TELL and NLG not defined
				     (:file "Ontology" )
				     (:file "circuit-ontology")  
				     ;; AXES-DRAWNP not defined
				     (:file "problem-solving")
				     (:file "kinematics")
				     (:file "dynamics")
				     (:file "vectors")
				     (:file "NewtonsNogoods")  
				     (:file "fluids")
				     (:file "waves")
				     (:file "work-energy")
				     (:file "optics")          
				     (:file "circuits")
				     (:file "momentum-impulse")
				     (:file "electromagnetism")          
				     ))
	       (:module "SGG"
;;;			:description "Solution Graph Generator" 
			:depends-on ("Base" "Knowledge" "HelpStructs" "Algebra")
			:components (
				     (:file "Qsolver") ;depends on HelpStructs
				     (:file "Exec" 
					    :depends-on ("Qsolver"))
				     (:file "Macros"
					    :depends-on ("Qsolver"))         
				     (:file "SolutionPoint")
				     (:file "GraphGenerator")
				     (:file "ProblemSolver")
				     (:file "print-solutions")
				     (:file "SolutionSets")))
))
