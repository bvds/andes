#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loader.cl
;; Collin Lynch
;; 4/20/2001
;;
;; This file is used to load the interface information for the 
;; Andes2 help system at runtime.
;;
|#


; Ensure Lisp will read given values into double-precision floating points.
; This precision is needed in a few cases by the algebra module.
(setf *read-default-float-format* 'double-float)


(defparameter *Root-Path* "c:\\andes2\\")

(defparameter *Bsolver-Load-Files*                                     ;;Defining module paths.
    '("Solver_Release\\AMFile.cl"
      ;; include when loading Andes/Atlas for kcd help:
      ;; "Atlas\\AMFile.cl"
      "Base\\AMFile.cl"
      "HelpStructs\\AMFile.cl"
      "Knowledge\\AMFile.cl"
      ; Now helpsys is loading kb dynamically
      ; "KB\\AMFile.cl"
      "Help\\AMFile.cl"
      "Testcode\\AMFile.cl"))
      

(load (pathname (concatenate 'string *Root-Path* "AndesModules.cl")))   ;; Load the module system.

(setup-Andes-Module-system *Root-Path*)                                 ;; Setup the system.

(dolist (F *Bsolver-Load-Files*)                                        ;; Setup each of the modules.
  (load (pathname (concatenate 'string *Root-Path* F))))

(load-andes-modules)                                                    ;; Load the modules.




