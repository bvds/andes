;;; A simple loader file.

; Ensure Lisp will read given values into double-precision floating points.
; This precision is needed in a few cases by the algebra module.
(setf *read-default-float-format* 'double-float)


(load "c:\\Andes2\\AndesModules.cl")
(defparameter *Root-Path* "c:\\andes2\\")
(setup-andes-module-system *Root-Path*)
(load "c:\\Andes2\\Base\\AMFile.cl")
(load "c:\\Andes2\\LogProcessing\\CmdReader\\AMFile.cl")
(load-andes-modules)



