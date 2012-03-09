;;;; -*- Lisp -*-

(in-package :cl-user)
(defpackage :help-asd (:use :cl :asdf))
(in-package :help-asd)

(defsystem :andes-help
  :name "Andes help"
  :description "Andes physics tutor system: helpsystem"
  :default-component-class cl-source-file.cl ;use *.cl as default extension
  :depends-on (problems web-server)
  :components (
	       (:module "Base"
			:components ((:file "memoize")
				     (:file "match")
				     ;; mt19937 had its own asd file, 
				     ;; but we don't use it
				     (:file "mt19937") 
				     (:file "garbage-collect")
				     (:file "mysql-connect")
				     (:file "random" 
					    :depends-on ("mt19937"))))
	       (:module "HelpStructs"
			:depends-on ("Base")
			;; PsmGraph, StudentEntry SystemEntry are also 
			;; defined in "andes"
			:components ((:file "StudentEntry")
				     (:file "hint-symbols")
				     (:file "graded")
				     (:file "SystemEntry"
					    :depends-on ("graded"))
				     (:file "PsmGraph")
				     (:file "TutorTurn"
					    :depends-on ("CMD"))
				     (:file "ErrorInterp")
				     (:file "CMD")
				     (:file "session")
				     ))
	       (:module "Help"
			:depends-on ("HelpStructs" "Base")
			:components (
				     (:file "grade")

				     ;; Solution graph
	 			     (:file "SolutionGraph"
					    :depends-on ("grade"))
				     
                                     (:file "utilities")
				     (:file "database")
				     (:file "icons")
				     (:file "model"
					    :depends-on ("database" "icons"))

				     				     
				     ;; Entry Intepreter: generic + non-eq
				     (:file "symbols")
				     (:file "wrong-quantities")
				     (:file "State"
					    :depends-on ("grade" "symbols" "grammar"))
				     (:file "Entry-API"
					    :depends-on ("HelpMessages" "symbols"
							 "model"
							 "SolutionGraph" "icons"
							 "wrong-quantities"))
				     
				     ;;  Help
				     (:file "HelpMessages")
				     (:file "whatswrong")
				     (:file "NextStepHelp"
					    :depends-on ("icons" "symbols"))
				     (:file "IEA")

				     ;; Equation parser/interpreter
				     (:file "physics-algebra-rules")
				     (:file "parse"
					    :depends-on ("utilities"))
				     (:file "grammar" :depends-on ("parse"))
				     (:file "algebra"
					    :depends-on ("symbols"))
				     (:file "interpret-equation"
					    :depends-on ("SolutionGraph"))
				     (:file "parse-andes"
					    :depends-on ("SolutionGraph" 
							 "symbols"
							 "whatswrong"
							 "interpret-equation"
							 "grammar" "icons"
							 "Entry-API" "database"))

				     (:file "test-all-quantities")
				     
				     ;; Top-level manager
		 		     (:file "Interface"
					    :depends-on ("model"))
	 			     (:file "Commands"
					    :depends-on ("Entry-API" "symbols"
							 "Interface" "icons"))
 				     (:file "API")
                                     (:file "fade"
					    :depends-on ("icons"))
				     (:file "word-suggest")
				     (:file "sessions"
					    ;; Mostly for *help-env-vars*
					    :depends-on ("NextStepHelp"
							 "grade"
							 "parse" "State" 
							 "database" "fade"
							 "word-suggest"
							 "grammar" "symbols" 
							 "Commands"))))
	       (:module "dashboard"
			:depends-on ("Help")
			:components (
				     (:file "dashboard")))
	       (:module "Testcode"
			:depends-on ("Help" "HelpStructs")
			:components (
				     (:file "StackProcessing")
;				     (:file "StackTests")
				     ))
	       ))
