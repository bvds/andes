;;#! /usr/local/share/acl/acl62/alisp -#!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load everything.

; Ensure Lisp will read given values into double-precision floating points.
; This precision is needed in a few cases by the algebra module.
(setf *read-default-float-format* 'double-float)

;; This will set the system to load the files appropriately depending 
;; upon the current OS.  This relies on the features present in the 
;; allegro *features* list.
#+MSWINDOWS (defparameter *Root-Path* "c:/andes2/")
#+LINUX (defparameter *Root-Path* "./")

(pprint *Root-Path*)

(defparameter *Bsolver-Load-Files*                                     ;;Defining module paths.
    '("Solver_Release/AMFile.cl"
      "Base/AMFile.cl"
      "HelpStructs/AMFile.cl"
      "Knowledge/AMFile.cl"
      "KB/AMFile.cl"
      "SGG/AMFile.cl"
      "Help/AMFile.cl"
      "Testcode/AMFile.cl"
      ))
      

(load (pathname (concatenate 'string *Root-Path* "AndesModules.cl")))   ;; Load the module system.

(setup-Andes-Module-system *Root-Path*)                                 ;; Setup the system.

(dolist (F *Bsolver-Load-Files*)                                        ;; Setup each of the modules.
  (load (pathname (concatenate 'string *Root-Path* F))))

(load-andes-modules)                                                    ;; Load the modules.

;; Since the solver does not yet work under linux we will initialize it
;; only when we are on windows.
#+MSWINDOWS (solver-initialize)                           ;; Initialize the algebra solver.
