#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMfile.cl 
;; Collin Lynch
;; 04/20/2001
;;
;; This file handles andes module
;; loading for the root Andes2 help system.
|#

(require 'Andes-Module-System);; '("c:/andes2/AndesModules.cl"))

(define-andes-module Help
    :path "Help/"
    :Files (;; general utilities
	    "utilities"
	    "lrdc-errors"
	    "History"
	    "StudentFile"
	    "tell" 	;; tracing tool

	    ;; Solution graph
	    "SolutionGraph"
	    
	    ;; Entry Intepreter: generic + non-eq
	    "symbols"
	    "State"
	    "clips-vars"
	    "Entry-API"

	    ;; Equation parser/interpreter
	    "grammar"
	    "physics-algebra-rules"
	    "parse"
	    "pre2in"
	    "in2pre"
	    "parse-andes"
	    "interpret-equation"

	    ;;  Help
	    "HelpMessages"
	    "whatswrong"
	    "NextStepHelp"     
	    "IEA"
	    "nlg"              ;; Natural language.

	    ;; Automatic statistics code.
	    "Statistics"

	    ;; Top-level manager
	    "Interface"  ;; The interface api.
	    "Commands"
	    "API"
	    "Andes2-Main"
	    )
    :compiles t
    
    :requires (Base HelpStructs Knowledge AlgebraDLL)
    
    :Specifications "Knowledge structures and storage."
    :switches (**debug-wwh-conditions** *print-nogood-messages*))

 