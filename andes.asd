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

(defsystem :andes
  :name "Andes"
  :description "Andes physics tutor system"
  :depends-on (cl-json)  ;KB/principles.cl
  :default-component-class cl-source-file.cl ;use *.cl as default
  :components (
;;;    this should eventually be removed
	       (:file "andes-path")
	       (:module "Base"
;;;			:description "Common Utilities"
			:components ((:file "Unification")
				     (:file "auxiliary")
				     (:file "hash")
				     (:file "match")
				     (:file "log-condition")
				     (:file "Utility")))
	       (:module "Algebra"
			:components ((:file "solver")))
	       (:module "HelpStructs"
			:depends-on ("Base")
			:components ((:file "PsmGraph")
				     (:file "StudentEntry")
				     (:file "hint-symbols")
				     ;;for make-hint-seq in KB/errors.cl
				     (:file "TutorTurn") 
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
