;;;; -*- Lisp -*-

(in-package :cl-user)
(defpackage :help-asd (:use :cl :asdf))
(in-package :help-asd)

;;;;   Load the source file, without compiling
;;;;   asdf:load-op reloads all files, whether they have been
;;;;   changed or not.

(defclass no-compile-file (cl-source-file) ())
(defmethod perform ((o compile-op) (s no-compile-file))
  nil)
(defmethod output-files ((o compile-op) (s no-compile-file))
  (list (component-pathname s)))


(defsystem :andes-help
  :name "Andes help"
  :description "Andes physics tutor system: helpsystem"
  :depends-on (problems web-server)
  :components (
	       (:module "Base"
			:components ((:file "memoize")
				     ;; mt19937 had its own asd file, 
				     ;; but we don't use it
				     (:file "mt19937") 
				     (:file "random" 
					    :depends-on ("mt19937"))))
	       (:module "HelpStructs"
			:depends-on ("Base")
			;; PSMgraph and SystemEntry are defined in "andes"
			:components ((:file "StudentEntry")
				     (:file "TutorTurn"
					    :depends-on ("CMD"))
				     (:file "ErrorInterp")
				     (:file "StudentAction"
					    :depends-on ("TutorTurn")
					    :depends-on ("StudentEntry"))
				     (:file "CMD")
				     (:file "RuntimeTestScore")
				     (:file "RuntimeTest")
				     ))
	       (:module "Help"
			:depends-on ("HelpStructs" "Base")
			:components (
 				     ;; Solution graph
	 			     (:file "SolutionGraph")
				     
                                     (:file "utilities")
				     (:file "lrdc-errors")
				     (:file "StudentFile")
				     				     
				     ;; Entry Intepreter: generic + non-eq
				     (:file "symbols")
				     (:file "State")
				     (:file "clips-vars")
				     (:file "Entry-API"
					    :depends-on ("HelpMessages")
					    :depends-on ("SolutionGraph"))
				     
				     ;; Equation parser/interpreter
				     (:file "grammar")
				     (:file "physics-algebra-rules")
				     (:file "parse"
					    :depends-on ("utilities"))
				     (:file "pre2in")
				     (:file "in2pre")
				     (:file "parse-andes"
					    :depends-on ("SolutionGraph"))
				     (:file "interpret-equation"
					    :depends-on ("SolutionGraph"))
				     
				     ;;  Help
				     (:file "HelpMessages")
				     (:file "whatswrong")
				     (:file "NextStepHelp")
				     (:file "IEA")
				     (:file "nlg") ;Natural language.
				     
				     ;; Automatic statistics code.
				     (:file "Statistics")
				     
				     ;; Top-level manager
		 		     (:file "Interface") ;The interface api.
	 			     (:file "Commands"
					    :depends-on ("Entry-API"))
 				     (:file "API")
				     (:file "Andes2-Main")))
	       (:module "Testcode"
			:depends-on ("Help" "HelpStructs")
			:components (
				     (:file "StackProcessing")
				     (:file "CMDTests")
				     (:file "StackTests")
				     (:file "EntryPair")
				     (:file "ProcDepth")
				     (:file "UtilFuncs")
				     ;; file must be loaded before compile
				     (:no-compile-file "Tests"
						       ;;    :in-order-to ((compile-op (load-source-op "Tests")))
						       )
				     ))
	       ))

;;;  make source file extension "cl"  See asdf manual

(defmethod source-file-type ((c cl-source-file) 
			     (s (eql (find-system :andes-help)))) "cl")
