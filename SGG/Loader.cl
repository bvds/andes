;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loader.cl
;; Collin Lynch
;; 2/7/2001
;;
;; This file is used to load the interface form of the 
;; Andes 2 solution graph generator in the process it
;; loads the load-me.cl file for the sgg module.
;;


; Ensure Lisp will read given values into double-precision floating points.
; This precision is needed in a few cases by the algebra module.
(setf *read-default-float-format* 'double-float)


(defparameter *Root-Path* "c:\\andes2\\")

(defparameter *Bsolver-Load-Files*                                     ;;Defining module paths.
    '("Base\\AMFile.cl"
      "HelpStructs\\AMFile.cl"
      "Knowledge\\AMFile.cl"
      "KB\\AMFile.cl"
      "Solver_Release\\AMFile.cl"
      "sgg\\AMFile.cl"))
      

(load (pathname (concatenate 'string *Root-Path* "AndesModules.cl")))   ;; Load the module system.

(setup-Andes-Module-system *Root-Path*)                                 ;; Setup the system.

(dolist (F *Bsolver-Load-Files*)                                        ;; Setup each of the modules.
  (load (pathname (concatenate 'string *Root-Path* F))))

(load-andes-modules)                                                    ;; Load the modules.

(solver-load (concatenate 'string *Root-Path* *DLL-NAME*))        ;; Load solver dll



