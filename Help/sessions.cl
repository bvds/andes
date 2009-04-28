;; Author(s):
;;   Mike Ringenberg (mr) <
;;   Linwood H. Taylor (lht) <lht@lzri.com>
;;   Anders Weinstein (a?w) <andersw+@pitt.edu>
;; Modified:
;;   unknown - created
;;   7 February 2001 - (lht) - editing for documentation and new help dialog
;;   ?????????? 2001 - (a?w) - added fucntionality fro debugging and added
;;                             support for symbol table processing
;;   23 April 2001 - (lht) - renamed from tcp-wb.lsp to Andes-Main.cl
;;                           adding some polishing to support final code
;;    5 June 2001 - (lht) - editied to load/initialize new parseing/etc.
;;; Modifications by Brett van de Sande, 2005-2009
;;; Copyright 2009 by Kurt Vanlehn and Brett van de Sande
;;;  This file is part of the Andes Intelligent Tutor Stystem.
;;;
;;;  The Andes Intelligent Tutor System is free software: you can redistribute
;;;  it and/or modify it under the terms of the GNU Lesser General Public 
;;;  License as published by the Free Software Foundation, either version 3 
;;;  of the License, or (at your option) any later version.
;;;
;;;  The Andes Solver is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with the Andes Intelligent Tutor System.  If not, see 
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Global Variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *andes-path* -- pathname of directory in which Andes files are installed
;;                 as logical pathname object

(defun andes-path (relative-path)
"merge relative path with *andes-path* returning new pathname"
    (merge-pathnames relative-path *andes-path*))

(defvar *debug-help* t
  "The stream showing help system runtime activities.")
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function for starting up the webserver
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-help (&key (port 8080))
  "start a server with help system, optionally specifying the port."
  ;; global setup

  ;; Mainly for safety in runtime image: ensure Lisp reads floating point 
  ;; numbers into doubles, no matter what setting had been in effect before.
  (setq *read-default-float-format* 'double-float)


  ;; in runtime version only: set *andes-path* to process working directory
  #+allegro-cl-runtime (setf *andes-path* 
			     (make-pathname 
			      :host (pathname-host *default-pathname-defaults*)
			      :device (pathname-device *default-pathname-defaults*)
			      :directory (pathname-directory *default-pathname-defaults*)
			      :name nil :type nil))
  ;; We also fix up the AndesModule system's compiled-in base-name var 
  ;; (set when helpsys was built) so runtime use loads from the runtime 
  ;; Andes directory.
  #-asdf (setf *Base-Andes-Module-Path* (namestring *andes-path*))
  (format T "Starting Andes, *andes-path* = ~A~%" *andes-path*)

  (doSafety :in2pre)

  (physics-algebra-rules-initialize) ;initialize grammar

  ;; start webserver
  (webserver:start-json-rpc-service "/help" :port port))

(defun stop-help () 
  "stop the web server running this service"
  (webserver:stop-json-rpc-service))

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx
	*help-env-vars* 
	;; These are all the variables that are be set by API commands
        ;; listed in Andes2 log files or their descendants.
	'(*CP* **STUDENTFILE** **CURRENT-STUDENT-NAME** 
	  **NSH-NEXT-CALL** *NSH-NODES* *NSH-FIRST-PRINCIPLES*
	  *NSH-CURRENT-SOLUTIONS* *NSH-LAST-NODE* *NSH-SOLUTION-SETS* 
	  *NSH-GIVENS* *NSH-AXIS-ENTRIES* *NSH-BODYSETS* *NSH-VALID-ENTRIES* 
	  *NSH-PROBLEM-TYPE* **ALTERNATE-COMMAND-INTERPRETER** *VARIABLES* 
	  *STUDENTENTRIES* *SG-EQNS* *SG-ENTRIES* *SG-SOLUTIONS*
	  ;; Session-specific variables in Help/Interface.cl
	  **last-api-call** **current-cmd-stack** **current-cmd** 
	  *last-tutor-turn* *last-score*
	  )
	#-sbcl "List of global variables that need to be saved between turns in a session."
	#+sbcl #'equalp
	)

;; New method with 
(defstruct help-env "Quantities that must be saved between turns of a session.  Member vals contains list of values for *help-env-vars*." 
	   student problem vals)

(defmacro env-wrap (&body body)
  "Make session-local copy of global variables, retrieving values from webserver:*env* at the beginning of a turn and saving them there at the end of that turn"
  (let ((result (gensym)) (vals 'webserver:*env*))
    `(progn
      ;; should check for use of "assert" in other code I have written.
      ;; maybe grep for "(unless .*(error " or "(when .*(error "

      ;; An error here indicates that the student is trying to work
      ;; on a session that has timed out or has not been initialized:  
      ;; probably should have a appropriate handler that gives instructions
      ;; to start a new session
      (assert ,vals)
      ;; further sanity check.
      (assert (help-env-p ,vals))
      (let ,(cons result 
		  (mapcar 
		   #'(lambda (x) (list x `(pop (help-env-vals ,vals))))
		   *help-env-vars*))
	(setf ,result (progn ,@body))
	(when ,vals (setf (help-env-vals ,vals) (list ,@*help-env-vars*)))
	,result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The methods themselves.  Right now, these are just dummy functions
;;  and not connected with the help system.
;;  It would be great to entirely decouple the sessions from the 
;;  methods.

(webserver:defun-method "/help" open-problem (&key time problem user) 
  "initial problem statement" 
  ;; Need to think about error handling for the case where 
  ;; the session already exists.
  (assert (null webserver:*env*))
  ;; tracing/debugging print
  (format webserver:*stdout* "open-problem opening problem ~A~%" problem)
  (setq webserver:*env* (make-help-env :student user :problem problem))
  ;; webserver:*env* needs to be initialized before the wrapper
  (env-wrap
    ;; might be replaced with session-env struct?
    (setq **base-Htime** (universal-time->htime (get-universal-time)))
    (solver-load)
    (solver-logging *solver-logging*)

    ;; Andes2 had calls to:
    ;;   set-session-id
    ;;   do-read-student-info
    ;;   set-condition none
    ;;   read-problem-info  (useful)
    ;;   check-entries T
    ;;     load any history from log files
    ;;     [define-variable assert-x-axis assert-object lookup-vector
    ;;      lookup-eqn-string check-answer etc.]
    ;;   check-entries nil
    ;;   set-stats (if there was an old score)

    `(((:action . "new-object") (:id . 0) (:type . "text") (:mode . "locked")
       (:x . 3) (:y . 5) (:text-width . 80) (:text . "A spherical ball with a mass of 2.00 kg rests in the notch ..."))
      ((:action . "new-object") (:id . 1) (:type . "graphics") (:mode . "locked")
       (:x . 53) (:y . 15) (:dx . 150) (:dy . 180)
       (:href . "/images/s2e.gif")))))

;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" solution-step 
    (&key time id action type mode x y text-width
			text dx dy radius symbol x-label y-label angle) 
  "problem-solving step"
  (env-wrap 
    ;; Andes2 had calls to:
    ;; define-variable define-angle-variable
    ;; assert-x-axis assert-object
    ;; assert-compound-object
    ;; label-radius label-angle
    ;; lookup-force lookup-torque lookup-line
    ;; lookup-vector lookup-eqn-string
    ;; lookup-mc-answer
    ;; delete-object
    ;; check-answer
    ;; calculate-equation-string (find variable on lhs of equation)
    ;;                           (probably not in Andes3)
    (cond
      ((string= action "new-object")
       `(((:action . "log") 
	  (:assoc . (("DEFINE-MASS" . "(DEFINE-VAR (MASS BALL))"))) 
	  (:id . ,id))
	 ((:action . "log") (:parse . "(= m_BALL (DNUM 2.0 kg))"))
		((:action . "set-score") (:score . 40))
	 ((:action . "modify-object") (:id . ,id) (:mode . "right"))))
      ((string= action "modify-object")
       `(((:action . "set-score") (:score . 57))
	 ((:action . "modify-object") (:id . ,id) (:mode . "right"))))
      ((string= action "delete-object")
       `(((:action . "set-score") (:score . 52))))
      (t (error "undefined action ~A" action)))))

;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" seek-help 
    (&key time action href value text) 
  "ask for help, or do a step in a help dialog" 
  (env-wrap 
    ;; Andes2 had calls to:
    ;; get-proc-help (help-hint)
    ;; explain-more
    ;; handle-student-response  (choose a quantity or a principle; in new version, typed in help chat window)
    ;; why-wrong-equation (what's wrong error)
    ;; why-wrong-object
    ;; solve-for-var (could also be under solve steps..., or own method)

    (cond
      ((string= action "get-help")
       '(((:action . "show-hint") 
	  (:text . "Because the vector is parallel to the Y axis 
but in the negative direction, the projection equation is Fearth_y = - Fearth so
 Fearth_y stands for a negative value."))
	 ((:action . "show-hint-link") 
	  (:text . "Explain more") 
	  (:value . "Explain-More"))))
      ((string= action "help-button")
       '(((:action . "show-hint") 
	  (:text . "Now that you have stated all of the given information, you should start on the major principles. What quantity is the problem seeking?"))
	 ((:action . "focus-hint-text-box"))))
      ((string= action "principles-menu")
       '(((:action . "show-hint") 
	  (:text . "Right indeed. Notice that the ball is at rest at T0."))
	 ((:action . "show-hint-link") 
	  (:text . "Explain more") 
	  (:value . "Explain-More"))))
      (t (error "undefined action ~A" action)))))

(webserver:defun-method "/help" close-problem (&key  time) 
  "shut problem down" 
  (env-wrap 
    ;; Andes2 had calls to:
    ;; get-stats (instead, we need to send grade to LMS)
    ;; close-problem
    ;; need to maybe store state
    (solver-unload)

    (let ((prob (env-problem webserver:*env*)))
      ;; this tells the session manager that the session is over.
      (setf webserver:*env* nil)
      `(((:action . "show-hint") 
	 (:text . ,(format nil "Finished working on problem ~A." prob)))
	((:action . "log") 
	 (:score . (("NSH_BO_Call_Count" . (-0.05 0)) 
		    ("WWH_BO_Call_Count" . (-0.05 0))
		    ("Correct_Entries_V_Entries" . (0.05 17 19))
		    ("Correct_Answer_Entries_V_Answer_Entries" . (0.05 1 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For runtime dist, trap all Lisp errors and return special :error value
;; instead. When debugging, just use apply to debug on errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+allegro-cl-runtime 
(defvar *ignore-errors* T)	; T => trap LISP errors in command execution
#-allegro-cl-runtime 
(defvar *ignore-errors* NIL)    

;; Apply function to args, trapping Lisp errors and returning :error in this
;; case as controlled by *ignore-errors* flag
(defun safe-apply (fn &optional (args nil)) 
  (if (not *ignore-errors*) 
      (apply fn args)
  ; else trap errors
  (let (result)
    (handler-case 
	(setf result (apply fn args))
      (error (c) 
    	(format nil "Executing command: ~A" c)
        :error)
      (:no-error (c) 
	(declare (ignore c))
    	result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
