#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testcode AMFile
;;; Collin Lynch.
;;; 8/12/2003
;;; 
;;; The code in this Andes module is used to define runtime (and log-time)
;;; testsets.  It consists of individual test predicates that operate on
;;; cmd stacks, test predicates that operate at runtime and other factors.
|#

(require 'Andes-Module-System); '("c:/andes2/AndesModules.cl"))

(define-andes-module testcode
    :path "Testcode/"
    :Files ("StackProcessing"
	    "CMDTests"
	    "StackTests"
	    "EntryPair"
	    "ProcDepth"
	    "UtilFuncs"
	    "Tests"
	    )
    :compiles ("StackProcessing" 
	       "CMDTests" "StackTests" 
	       "EntryPair" "ProcDepth"
	       "UTilFuncs")
    
    :requires (Base HelpStructs Knowledge Help)
    
    :Specifications "Runtime Test code and storage."
    :switches ())
