;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFIle.cl
;; Collin Lynch
;; 12/12/2000
;;
;; This file defines the loading information for the
;; Andes2 Physics knowledge base and problem base.  
;; It is used to load up the knowledge base in andes2.
;;
;; This variant is to be used when loading kb needed by the help system.
;; It differs in not loading the problem definitions, 
;; since problems will be loaded from files as needed.

(require 'Andes-Module-System);; '("c:/andes2/AndesModules.cl"))

(define-andes-module Physics-KB
    :Path "KB/"
    :Files ( ;; Lisp utility functions
            "Physics-Funcs"

	    ;; Note: where the definitions of a kb component are broken up into more than 
	    ;; one file below, the load order matters: as noted, the first file in the list 
	    ;; is the main one and always begins with the appropriate call to reset the 
	    ;; relevant list in memory.  Following files constitute additions to the core
	    ;; database and don't contain such a call, and assume the main file will be
	    ;; loaded earlier.  This should probably be cleaned up, but it works for now.

	    ;; ontology definition files. First one resets ontology db lists.
	    "Ontology"		
	    "circuit-ontology"

	    ;; KB operator files. First one resets op-list
	    "Newtons2"
	    "circuits"
  	    "forces"
            "vectors"
	    "optics"

	    ;; Nogoods for pruning solution sets. First one resets nogood list
	    "NewtonsNogoods"

	    ;; Error handlers -- first one resets error handler list
	    "errors"
	    
;No need to include problems with helpsys

	    )
    :compiles ("Physics-Funcs" "errors")
    
    :requires (Knowledge)
    
    :switches (*lk-hack*)
    
    :Specifications "Knowledge elements.")
