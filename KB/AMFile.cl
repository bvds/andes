#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFIle.cl
;; Collin Lynch
;; 12/12/2000
;;
;; This file defines the loading information for the
;; Andes2 Physics knowledge base and problem base.  
;; It is used to load up the knowledge base in andes2.
|#

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
	    
	    ;; Problem files -- first one resets problem list
	    ;"PyreneesProblems"
	    "Problems"
	    ;"circuit-problems"
     	    "force-problems"
            ; "vectors-problems"

	    ; helper for generating problems
	    "makeprob"  	
	    )
    :compiles ("Physics-Funcs" "Errors")
    
    :requires (Knowledge)
    
    :switches (*lk-hack*)
    
    :Specifications "Knowledge elements.")


(defun rkb ()
  "Reload the Knowledge base code."
  (compile-named-andes-module 'Physics-KB)
  (load-named-andes-module 'Physics-KB))

;; For use while developing circuits kb before merging into Andes above:
;; Defininiton of alternate Circuits-KB andes module:
;; Andes-module struct is manually defined so it is not automatically
;; added to database of loaded modules so won't be automatically loaded by
;; ra/rca/rsa functions.  To play with circuits-kb, load it manually with 
;; "rcb" below. Because the relevant files clear the kb contents when loaded,
;; this will overwrite any existing loaded kb contents (operators, 
;; problems, ontology) with the circuits versions.
;; rkb or ra etc will reload the mechanics KB contents.
(defvar *circuits-module*)
(setf *circuits-module* 
	(make-Andes-Module    :Name 'Circuits-KB
			      :Path "KB\\"
			      :Files '(
	    		        "circuit-Ontology"
				"circuits"
	                        ;"circuit-Problems"
			      )
			      :compiles NIL
			      :requires '(Knowledge)
			      :specs NIL
			      :compile-code NIL
			      :load-code NIL
			      :Switches NIL))
    
(defun rcb ()
   "Load/Reload the circuits KB"
   (load-andes-module *circuits-module*))
