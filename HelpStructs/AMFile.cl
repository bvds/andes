#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFile.cl
;; Collin Lynch 
;; 4/19/2000
;;
;; This file defines the module info for the knowledge structures.
;;
;; ChangeLog: 
;;  6/10/2003:  Htime has now been moved to the Base module.
|#

(require 'Andes-Module-System);; '("c:/andes2/AndesModules.cl"))

(define-andes-module HelpStructs
    :path "HelpStructs/"
    :Files ("SystemEntry"
	    "PsmGraph"
	    "StudentEntry"
	    "TutorTurn"
	    "Error-Interp"
	    "StudentAction"
	    "CMD"
	    "RuntimeTestScore"
	    "RuntimeTest"
	    )
    :compiles t
    
    :requires (Base);; Atlas)
    
    :Specifications "Knowledge structures and storage."
    :switches (*print-nogood-messages*))
