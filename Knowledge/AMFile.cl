#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFile.cl
;; Collin Lynch 
;; 12/21/2000
;;
;; This file defines the module info for the knowledge structures.
|#

(require 'Andes-Module-System);; '("c:/andes2/AndesModules.cl"))

(define-andes-module Knowledge
    :path "Knowledge/"
    :Files ("Problem"
	    "Operators"
	    "Ontology"
	    "eqn"
	    "qvar"
	    "Nogood"
	    "BubbleGraph"
	    "Solution"
	    "ErrorClass"
	    )
    :compiles t
    
    :requires (Base HelpStructs)
    
    :Specifications "Knowledge structures and storage."
    :switches (*print-nogood-messages*))
