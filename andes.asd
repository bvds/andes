;;;; -*- Lisp -*-
;;;; above sets emacs major mode to Lisp
;;;;
;;;; Use this to compile:
;;;;  (asdf:operate 'asdf:load-op 'andes)
;;;; We shouldn't have to do the following:
;;;;  #+LINUX (defparameter *Root-Path* "/home/bvds/Andes2/")
;;;;  :cd /home/bvds/Andes2/ 
;;;;   ; needed for temp file andes241.tlz


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
(defclass load-only-andes-source-file (source-file) ())
(defmethod output-files ((o compile-op) (s load-only-andes-source-file))
  (list (component-pathname s)))
(defmethod perform ((o compile-op) (s load-only-andes-source-file))
  nil)
(defmethod perform ((o load-op) (s load-only-andes-source-file))
  (load (component-pathname s)))
(defmethod source-file-type ((s load-only-andes-source-file) y)
  "cl")


(defsystem :andes
  :name "Andes"
  :description "Andes physics tutor system"
  :components (
	       (:module "Base"
;			:description "Common Utilities"
			:components ((:file "Htime")
				     (:file "Unification")
				     (:file "Utility")))
	       (:module "Solver_Release"
			:components ((:file "solver")))
	       (:module "HelpStructs"
			:depends-on ("Base")
			:components ((:file "PsmGraph")
#|				     (:file "SystemEntry")
				     (:file "StudentEntry")
				     (:file "TutorTurn")
				     (:file "Error-Interp")
				     (:file "StudentAction")
				     (:file "CMD")
				     (:file "RuntimeTestScore")
				     (:file "RuntimeTest")
				     |#
))
	       (:module "Knowledge"
			:depends-on ("Base" "Solver_Release")
			:components ((:file "eqn")         
				     (:file "Nogood")    
				     (:file "Operators")  
				     (:file "qvar")
				     (:file "BubbleGraph" 
					    :depends-on ("qvar"))  
				     (:file "ErrorClass")  
				     (:file "Ontology")  
				     (:file "Problem")    
				     (:file "Solution")))	       
	       (:module "KB"
;			:description "Knowledge Base"
			;; These are not compiled
			:default-component-class load-only-andes-source-file
			:depends-on ("Knowledge" "Base" "SGG")
			:components ((:file "reset-KB")
				     (:file "Ontology" )        
				     (:file "NewtonsNogoods")  
				     (:file "Physics-Funcs")
				     (:file "circuit-ontology")  
				     (:file "Newtons2")        
				     (:file "Problems")
;;;;
;;;; The remaining files are just needed for the help system???
;;;;
#|				     (:file "errors")
				     (:file "force-problems")  
				     (:file "PyreneesProblems")
				     (:file "forces")          
				     (:file "optics")          
				     (:file "vectors")
				     (:file "makeprob")        
				     (:file "vectors-problems")
				     (:file "circuits")
|#
))
	       (:module "SGG"
;			:description "Solution Graph Generator"
			:depends-on ("Base" "Knowledge")
			:components ((:file "Qsolver")
				     (:file "Exec" 
					    :depends-on ("Qsolver"))
				     (:file "Macros")         
				     (:file "SolutionPoint")
				     (:file "GraphGenerator") 
				     (:file "ProblemSolver")
				     (:file "SolutionSets")))
#|
	       (:module "Help"
			:components (
				     ;; Solution graph
				     (:file "SolutionGraph")

                                     (:file "utilities")
				     (:file "lrdc-errors")
				     (:file "History")
				     (:file "StudentFile")
				     (:file "tell") ; tracing tool
				     				     
				     ;; Entry Intepreter: generic + non-eq
				     (:file "symbols")
				     (:file "State")
				     (:file "clips-vars")
				     (:file "Entry-API")
				     
				     ;; Equation parser/interpreter
				     (:file "grammar")
				     (:file "physics-algebra-rules")
				     (:file "parse")
				     (:file "pre2in")
				     (:file "in2pre")
				     (:file "parse-andes")
				     (:file "interpret-equation")
				     
				     ;;  Help
				     (:file "HelpMessages")
				     (:file "whatswrong")
				     (:file "NextStepHelp")
				     (:file "IEA")
				     (:file "nlg") ; Natural language.
				     
				     ;; Automatic statistics code.
				     (:file "Statistics")
				     
				     ;; Top-level manager
				     (:file "Interface") ; The interface api.
				     (:file "Commands")
				     (:file "API")
				     (:file "Andes2-Main")))
|#
))

;;;  make source file extension "cl"  See asdf manual

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :andes))))
   "cl")

;;;;
;;;;  install command
;;;;

(defun rkb ()
  "Reset the lists in KB and reload all files using asdf"
  (asdf:operate 'asdf:load-op 'andes))
