;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;;  Use this to compile:
;;;;  (asdf:operate 'asdf:load-op 'andes)
;;;;

;;;; This was stolen from maxima.asd
#+(or sbcl openmcl)
(or (find-package "USER")
    (rename-package "COMMON-LISP-USER" "COMMON-LISP-USER" '("USER")))
(in-package :user)
(in-package :asdf) 

(defsystem :andes
  :name "Andes"
  :description "Andes physics tutor system"
  :components (
	       (:module "Base"
;			:description "Common Utilities"
			:components ((:file "Htime")
				     (:file "Unification")
				     (:file "Utility")))	       
	       (:module "Knowledge"
			:depends-on ("Base")
			:components ((:file "eqn")         
				     (:file "Nogood")    
				     (:file "Operators")  
				     (:file "qvar")
				     (:file "BubbleGraph")  
				     (:file "ErrorClass")  
				     (:file "Ontology")  
				     (:file "Problem")    
				     (:file "Solution")))	       
	       (:module "KB"
;			:description "Knowledge Base"
			:depends-on ("Knowledge" "Base")
			:components ((:file "Physics-Funcs")
				     (:file "Newtons2")        
				     (:file "Problems")   
;;;;
;;;; The remaining files are just needed for the help system???
;;;;
#|				     (:file "errors")
				     (:file "NewtonsNogoods")  
				     (:file "psm-list")
				     (:file "force-problems")  
				     (:file "Ontology")        
				     (:file "PyreneesProblems")
				     (:file "circuit-ontology")  
				     (:file "forces")          
				     (:file "optics")          
				     (:file "vectors")
				     (:file "circuit-problems")  
				     (:file "makeprob")        
				     (:file "vectors-problems")
				     (:file "circuits")
|#
))
	       (:module "SGG"
;			:description "Solution Graph Generator"
			:depends-on ("Base" "Knowledge")
			:components ((:file "Qsolver")
				     (:file "Exec")       
				     (:file "Macros")         
				     (:file "SolutionPoint")
				     (:file "GraphGenerator") 
				     (:file "ProblemSolver")
				     (:file "SolutionSets")))))
;;;;
;;;;  make source file extension "cl"  See asdf manual
;;;;

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :andes))))
   "cl")
