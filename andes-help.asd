;;;; -*- Lisp -*-

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


(defsystem :andes-help
  :name "Andes help"
  :description "Andes physics tutor system: helpsystem"
  :depends-on (andes)
  :components (
	       (:module "HelpStructs"
			;; PSMgraph and SystemEntry are defined in "andes"
			:components ((:file "StudentEntry")
				     (:file "TutorTurn"
					    :depends-on ("CMD"))
				     (:file "Error-Interp")
				     (:file "StudentAction"
					    :depends-on ("TutorTurn"))
				     (:file "CMD")
				     (:file "RuntimeTestScore")
				     (:file "RuntimeTest")
				     ))
	       (:module "Help"
			:depends-on ("HelpStructs")
			:components (
 				     ;; Solution graph
	 			     (:file "SolutionGraph")
				     
                                     (:file "utilities" 
					    :depends-on ("tell"))
				     (:file "lrdc-errors")
				     (:file "History")
				     (:file "StudentFile"
					     :depends-on ("tell"))
				     (:file "tell") ;tracing tool
				     				     
				     ;; Entry Intepreter: generic + non-eq
				     (:file "symbols"
					     :depends-on ("tell"))
				     (:file "State")
				     (:file "clips-vars")
				     (:file "Entry-API"
					    :depends-on ("HelpMessages"))
				     
				     ;; Equation parser/interpreter
				     (:file "grammar"
					    :depends-on ("tell"))
				     (:file "physics-algebra-rules"
					     :depends-on ("tell"))
				     (:file "parse"
					     :depends-on ("tell"))
				     (:file "pre2in"
					     :depends-on ("tell"))
				     (:file "in2pre" :depends-on ("tell"))
				     (:file "parse-andes"
					     :depends-on ("tell"))
				     (:file "interpret-equation"
					     :depends-on ("tell"))
				     
				     ;;  Help
				     (:file "HelpMessages")
				     (:file "whatswrong"
					     :depends-on ("tell"))
				     (:file "NextStepHelp")
				     (:file "IEA")
				     (:file "nlg" ;Natural language.
					     :depends-on ("tell")) 
				     
				     ;; Automatic statistics code.
				     (:file "Statistics")
				     
				     ;; Top-level manager
		 		     (:file "Interface" ;The interface api.
					    :depends-on ("tell")) 
	 			     (:file "Commands")
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
