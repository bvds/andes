;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makehelp -- build runtime image of Andes2 help system
;;
;; In a console lisp do
;;
;;      :cd <this directory>
;;      :ld makehelp
;;
;; Leaves Andes2.dxl in C:\Andes2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)


;; Following is executed to make a new dist. This will spawn a new lisp,
;; which will load all the specified modules then dump an image of itself.
;; Note the normal Andes loader file will load existing compiled code if 
;; it exists.  But some modules need to be compiled differently for 
;; the runtime distribution if conditionalized with #+*allegro-cl-runtime*
;; (Currently there is only one module, Andes2-main, that depends on this.
;; But could be more in the future). For that reason we specify a variant 
;; loader file "load-runtimehelp" that recompiles this before loading.

(generate-application 
 "Andes2"
 "c:\\cygwin\\home\\Administrator\\Andes2\\"
 ; list copied from Andes1, not clear if all of these are needed
 '(:defctype :ffcompat :foreign :list2 :loop :seq2 :sock :defftype 
   ; new in 8.0: multiprocessing functions
   :process :acldns
   "load-runtimehelp.cl")
 :image-only nil        ; changed for ACL70 version per documentation
 :runtime :standard     
 :allow-existing-directory t
 :autoload-warning t
; :application-files '("c:\\Andes2\\Solver_Release\\solver.dll")
 :application-type :exe
 :copy-shared-libraries nil
 :debug T
 ; deprecated
 ; :exit-after-image-build nil
 :discard-source-file-info t
 :discard-xref-info nil  ; changed for ACL70 version -- prevent load of xref module
 :discard-local-name-info t
 ; no longer valid:
 ; :include-common-graphics nil
 :include-devel-env nil
 :splash-from-file nil
; :purify t
 :show-window :normal
 :include-compiler t
 :discard-compiler t
 :include-debugger nil
 ; deprecated:
 ; :debug-on-error nil
 ; following is to debug build process:
 ; :build-debug :interactive causes spawned lisp to enter debugger on Lisp error
 :build-debug :interactive
 :build-input "input.txt" :verbose t :build-output "output.txt"
 ; use the following to also debug on warnings
 ; :pre-load-form '(setf *break-on-signals* t) 
 :include-ide nil
 ; note: setting include-tpl as attempt to avoid startup crash (because 
 ; Franz advised this to work around a (different) startup bug in the past)
 ; caused *read-default-float-format* to be set to 'single-float in the
 ; runtime image. Need to ensure this is set correctly in Andes initialization.
 :include-tpl nil  
 :restart-init-function 'cl-user::andes-start
 :restart-app-function nil)

#| ; following patches default command line args into resource segment of a Windows executable
 #+windows
  (win:set-default-command-line-arguments "Andes2.exe" '("+c"))
|#

; OK, recompile this file to leave a development version
(format T "Deleting runtime-conditional files~%")
(delete-file "Help/Andes2-main.fasl")
