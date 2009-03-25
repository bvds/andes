;(format t "loading initialization file .clisprc.lisp~%")
; asdf 1.127 is newest version that doesn't have define-method-combination
(load #P"C:\\cygwin\\home\\Administrator\\asdf.lisp")
;(require :asdf)

(pushnew #P"C:\\cygwin\\home\\Administrator\\" asdf-util:*source-dirs* :test #'equal)
(pushnew #P"C:\\cygwin\\home\\Administrator\\Andes2\\" asdf-util:*source-dirs* :test #'equal)

;;;  load cffi
;(pushnew #P"C:\\cygwin\\home\\Administrator\\cffi_0.10.4\\" asdf-util:*source-dirs*)
;(asdf:oos 'asdf:load-op :cffi) 
;(require :uffi-compat)
;(pushnew :uffi cl:*features*)  ;; used by solver to detect uffi

;;;;
;;;;  load andes system
;;;;

(defun rkb ()
  "Reset the lists in KB and reload all files using asdf"
  (asdf:oos 'asdf:load-op 'problems))

(defun rhelp ()
  "Load or reload help system using asdf"
  (asdf:oos 'asdf:load-op 'andes-help))

 (asdf:oos 'asdf:load-op 'andes-help)
