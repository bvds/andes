#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFile.cl
;; Collin Lynch
;; 2/7/2001
;;
;; Andes Module file for the Solution Graph Generator.
;; 
|#

(require 'andes-module-system)

(define-andes-module Solution-Graph-Generator                  
    :path "SGG/"
    
    :Files ("Qsolver"
	    "Exec" 
	    "Macros" 
	    "GraphGenerator" 
	    "SolutionSets"
	    "SolutionPoint"
	    "ProblemSolver"
	    ;;"interface"
	    )
    
    :compiles t
    
    :requires (Base Physics-KB Knowledge AlgebraDLL)
    
    :Specifications "Solution Graph generator system."
    :switches (*debug* *actions* *S-Print-Graph* *Debug-cs* 
		       *S-Print-Steps* *Debug-efq* *Debug-efe* 
		       *Debug-rn*)
    )





