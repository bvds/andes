#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMFIle.cl
;; Collin Lynch
;; 03/02/2001
;;
;; This file defines the loading information for the
;; Andes2 Solver system which consists of a set of 
;; lisp header files connected to c dlls.
;;
;; GBecause we do note yet have a version of the DLL for 
;; Linux this code will not load the solver file for now.
;; Instead it will simply load an empty module.
|#

(require 'Andes-Module-System)


(define-andes-module AlgebraDLL
    :Path "Solver_Release/"
    ;;#+MSWINDOWS :Files #+MSWINDOWS ("solver")
    :Files ("solver")
    :compiles nil
    :Specifications "algebra dll interface.")
