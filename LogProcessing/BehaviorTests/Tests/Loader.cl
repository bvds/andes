#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests/Loader.cl
;; Collin Lynch
;;
;; This file is used to speed up the loading
;; process for the individual testsets. 
;;
|#

(load "./Tests/StackTests")
(load "./Tests/CMDTests")
(load "./Tests/ProcDepth")
(load "./Tests/EntryPair")
(load "./Tests/SetDefs.cl")

(defparameter **Standard-testsets** (list **command-testset** **stack-testset**))
