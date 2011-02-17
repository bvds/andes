;;;; -*- Lisp -*-

(in-package :cl-user)
(defpackage :help-asd (:use :cl :asdf))
(in-package :help-asd)

;;  make lisp source file extension "cl"  See asdf manual
#+asdf2 (defclass my-cl-source-file (cl-source-file) ((type :initform "cl")))
#-asdf2 (defmethod source-file-type ((f my-cl-source-file) (m module))
	  (declare (ignorable f m)) "cl")

;;;;   Load the source file, without compiling
;;;;   asdf:load-op reloads all files, whether they have been
;;;;   changed or not.

(defclass no-compile-file (cl-source-file) ())
(defmethod perform ((o compile-op) (s no-compile-file)) nil)
(defmethod output-files ((o compile-op) (s no-compile-file))
  (list (component-pathname s)))


(defsystem :andes-help
  :name "Andes help"
  :default-component-class my-cl-source-file
  :description "Andes physics tutor system: helpsystem"
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
				     (:file "SystemEntry")
				     (:file "PsmGraph")
				     (:file "TutorTurn"
					    :depends-on ("CMD"))
				     (:file "ErrorInterp")
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
				     (:file "database")
				     				     
				     ;; Entry Intepreter: generic + non-eq
				     (:file "symbols")
				     (:file "wrong-quantities")
				     (:file "State"
					    :depends-on ("symbols" "grammar"))
				     (:file "Entry-API"
					    :depends-on ("HelpMessages" "symbols"
							 "SolutionGraph" "icons"
							 "wrong-quantities"))
				     
				     ;; Equation parser/interpreter
				     (:file "physics-algebra-rules")
				     (:file "parse"
					    :depends-on ("utilities"))
				     (:file "grammar" :depends-on ("parse"))
				     (:file "algebra"
					    :depends-on ("symbols"))
				     (:file "parse-andes"
					    :depends-on ("SolutionGraph" "symbols"
							 "grammar" "icons"))
				     (:file "interpret-equation"
					    :depends-on ("SolutionGraph"))

				     (:file "test-all-quantities")
				     
				     ;;  Help
				     (:file "icons")
				     (:file "HelpMessages")
				     (:file "whatswrong")
				     (:file "NextStepHelp"
					    :depends-on ("icons" "symbols"))
				     (:file "IEA")
				     
				     ;; Automatic statistics code.
				     (:file "Statistics")
				     
				     ;; Top-level manager
		 		     (:file "Interface") ;The interface api.
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
				     (:file "StackTests")
				     (:file "UtilFuncs")
				     ;; file must be loaded before compile
				     (:file "Tests"
					      :in-order-to ((compile-op (load-source-op "Tests")))
						       )
				     ))
	       ))
