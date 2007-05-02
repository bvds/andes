;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dllapi.cl -- Lisp side of the Andes help system dll interface 
;;
;; The ability to provide a dll interface depends on features specific to 
;; Allegro Common Lisp. See the Allegro docs for explanation. 
;;
;; The connection between Lisp and C requires that both sides load
;; an intermediate dll (our helpifc.dll) that implements functions required
;; for the hookup. The C-side must also use the Allegro-provided lnkacl.dll, 
;; and the lisp image must include the allegro-provided lnk.cl module.
;;
;; This provides two API functions for use by the workbench:
;;    helpsys-initialize
;;    helpsys-execute
;; This also defines send-fbd-command which help system code may be use to 
;; to send a wb command string for execution by the workbench.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :user)

(eval-when (load eval)
  (load "helpifc.dll"))

; C side in helpifc.dll will provide the following to us:
(ff:def-foreign-call c_set_lisp_entrypoint (address))

(ff:def-foreign-call c_copy_result 
    ((str1 :unsigned-long) (str2 (* :char)) (n :int)))

(ff:def-foreign-call c_execute_cmd
    ((str (* :char))))

; following is a custom initialization routine to be called through the
; RemoteCmd in lnkacl.dll. It wires up the linkage by invoking a callback
; to communicate the Lisp function pointer for C side use.
(defun helpsys-initialize ()

  ; initialization for dll linkage:
  #-(version>= 7) (mp:start-customs "Helpsys")
  #+(version>= 7) (mp:start-scheduler)
  (c_set_lisp_entrypoint (ff:register-function #'helpsys-execute nil t))

  ; When building the dll image, we set andes-init as the :restart-init-function
  ; to be called in the course of the Lisp startup routine. It seems to be necessary 
  ; to adjust the crucial-for-us control variable *read-default-float-format* there to 
  ; be sure it takes effect; attempting to set it here didn't seem to work for later (perhaps
  ; because later code runs in a different Lisp thread with its own thread-local value?)
  ; That routine will also load the solver and initialize help system data structures, so 
  ; there is no further initialization for us to do here.
  (format *debug-help* "Attached help system to workbench~%")
)

; helpsys-execute -- main API for client calls to the help system
; 
(ff:defun-c-callable (helpsys-execute :c)
    ((c_cmdstr :unsigned-long) (resultbuf :unsigned-long) (bufsize :unsigned-long))
    :long
  (let ((cmdstr (native-to-string c_cmdstr)))
    (format *debug-help* "helpsys-execute: ~A~%" cmdstr)
    (handler-case
      (progn
	(c_copy_result resultbuf
		       (format nil "~A" (do-execute cmdstr))
		       bufsize)
	1) ;; return 1 if successful
      (serious-condition ()
        0))) ;; return 0 if unsuccessful
)

(defun do-execute (cmdstr)  ; assumes caller will trap errors
  (do-dispatch-stream-event cmdstr T) ; T means expects reply
)

; following redefines function in Andes2-main.cl
; used to make a call into the workbench by sending a command string.
(defun send-fbd-command (cmdstr)
  (format T "sending command: ~S~%" cmdstr)
  (c_execute_cmd cmdstr))
