#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFile.cl
;; Collin Lynch
;; 2/7/2001
;;
;; This file defines loading code for the Utility element and should
;; be called to load files when the base system is meant to be used.
|#

(require 'Andes-Module-System);; '("c:/andes2/AndesModules.cl"))

(define-andes-module Base
    :Path "Base/"
    :Files ("Utility" "Unification" "Htime")
    :compiles t
    :Specifications "General Utility files."
    )


