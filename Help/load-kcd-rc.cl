;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-kcd-rc.cl -- variant Andes loader that forces a recompile of 
;;                   everything after loading it all including the kcd.
;; This is mainly for use when loading Andes in context of 
;; generate-application-spawned child Lisp.  We force recompilation
;; to get any code that is different in the context of runtime-dist.
;;

; Ensure Lisp will read given values into double-precision floating points.
; This precision is needed in a few cases by the algebra module.
(setf *read-default-float-format* 'double-float)

(defparameter *Root-Path* "c:\\andes2\\")

(defparameter *Bsolver-Load-Files*                                     ;;Defining module paths.
    '("Solver_Release\\AMFile.cl"
      "Base\\AMFile.cl"
      "Atlas\\AMFile.cl"
      "HelpStructs\\AMFile.cl"
      "Knowledge\\AMFile.cl"
      "KB\\AMFile.cl"
      "Help\\AMFile.cl"))
      

(load (pathname (concatenate 'string *Root-Path* "AndesModules.cl")))   ;; Load the module system.

(setup-Andes-Module-system *Root-Path*)                                 ;; Setup the system.

(dolist (F *Bsolver-Load-Files*)                                        ;; Setup each of the module definitions.
  (load (pathname (concatenate 'string *Root-Path* F))))

(format t ";;; Recompiling and loading all Andes modules~%")
(rsa) ; load all latest sources
(rca) ; recompile and reload
