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

  (parse-initialize)   ;set up memoization for parse functions
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
	'(*CP* **STUDENTFILE** 
	  **NSH-NEXT-CALL** *NSH-NODES* *NSH-FIRST-PRINCIPLES*
	  *NSH-CURRENT-SOLUTIONS* *NSH-LAST-NODE* *NSH-SOLUTION-SETS* 
	  *NSH-GIVENS* *NSH-AXIS-ENTRIES* *NSH-BODYSETS* *NSH-VALID-ENTRIES* 
	  *NSH-PROBLEM-TYPE* **ALTERNATE-COMMAND-INTERPRETER** *VARIABLES* 
	  *STUDENTENTRIES* *SG-EQNS* *SG-ENTRIES* *SG-SOLUTIONS*
          **Condition**  mt19937::*random-state* **grammar**
	  ;; Session-specific variables in Help/Interface.cl
	  **last-api-call** **current-cmd-stack** **current-cmd** 
	  *last-tutor-turn* *last-score*
          ;; Variables set in Config.cl, which is loaded for each session.
          *Runtime-Testset* *Runtime-Score-Testset*
	  **Testing-Pause-Time-Threshold** **Filter-Constraint-losses**
          *slot-flag-frequency* *followup-problems*
	  ;; Variables holding session-local memos.
	  *parse-memo* *grammar-get-rhs-memo* *grammar-get-rhs-with-first*
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
;;                   The methods themselves.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(webserver:defun-method "/help" open-problem (&key time problem user) 
  "initial problem statement" 
  
  ;; Need to think about error handling for the case where 
  ;; the session already exists.
  (assert (null webserver:*env*))
  ;; tracing/debugging print
  (format webserver:*stdout* "open-problem opening problem ~A~%" problem)
  
  ;; webserver:*env* needs to be initialized before the wrapper
  (setq webserver:*env* (make-help-env :student user :problem problem))
  
  (env-wrap
    (let (replies)
      (setq **base-Htime** (universal-time->htime (get-universal-time)))
      (solver-load)
      (solver-logging *solver-logging*)
      
      ;; Andes2 had the following calls that can be found in log files:
      ;;   read-student-info; the only remaining step is:
      (Load-Config-File)			
      ;;   set-condition none
      (set-condition 'none) ;Any experimental condition
      ;;  Most of the set-up is done here
      ;; The return for this may be of some use.
      (read-problem-info problem)
      ;;
      ;;   check-entries T
      ;;     load any history from log files
      ;;     [define-variable assert-x-axis assert-object lookup-vector
      ;;      lookup-eqn-string check-answer etc.]
      ;;   check-entries nil
      ;;   set-stats (if there was an old score)
      ;;
      (let ((y 10) (i 0))
	(dolist  (line (problem-statement *cp*))
	  (push `((:action . "new-object") (:type . "phrase") 
		  (:id . ,(format nil "statement~A" (incf i))) 
		  (:mode . "locked") (:x . 3) (:y . ,(setf y (+ y 10))) 
		  (:width . 80) (:text . ,line)) replies))

	(when (problem-graphic *cp*)
	  (let ((dims (problem-graphic-dimensions (problem-graphic *cp*))))
	    (if dims		
		(push `((:action . "new-object") (:id . "graphic") 
			(:type . "graphics") (:mode . "locked") 
			(:x . 10) (:y . ,(+ y 5)) 
			(:width . ,(car dims)) (:height . ,(cadr dims))
			;; This is the URL for the graphic, which may not
			;; match its location on the server filesystem.
			(:href . ,(strcat "/images/" (problem-graphic *cp*))))
		replies)
		(error "Problem graphic file ~A missing" 
		       (problem-graphic *cp*))))))
      
      ;;  New:  return any predefs, any work done, and score to the client.
      
      (push `((:action . "new-object") (:id . "a2") (:type . "phrase") 
	      (:mode . "right") (:x . 200) (:y . 55) 
	      (:text . "g = 9.8 m/s^2 is the gravitational acceleration \nnear the surface of the earth.")) 
	    replies)
	
      (push `((:action . "new-object") (:id .  "a2.5") (:type . "phrase") 
		(:mode . "right") (:x . 200) (:y . 75) (:text . "T0 is the time.")) 
	    replies)
      
      (push `((:action . "log") 
	      (:subscores . (("NSH_BO_Call_Count" . (0 0))
			     ("WWH_BO_Call_Count" . (0  0))
			     ("Correct_Entries_V_Entries" . (0 0 0)) 
			     ("Correct_Answer_Entries_V_Answer_Entries" .  (0 0 0)))))
	    replies)
      
      (push `((:action . "set-score") (:score . 0)) replies)
      
      replies)))

(defmacro assoc-from-variables (&rest x) 
  "Make alist of symbol value pairs from all variables that are non-null."
  `(remove nil 
    (list ,@(mapcar #'(lambda (y) (list 'cons (intern (string y) "KEYWORD") y)) 
		    x))
    :key #'cdr))

(defun update-alist (x update)
  "Update elements in first alist by matching elements in second alist; returns nil."
  (let (a) 
    (dolist (ue update)
      (if (setf a (assoc (car ue) x))
         (setf (cdr a) (cdr ue))
         (warn "member ~A missing from alist ~A" (car ue) x)))))

;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" solution-step 
    (&key time id action type mode x y
			text width height radius symbol x-label y-label angle) 
  "problem-solving step"
  (env-wrap 
    ;; Andes2 also had calls to:
    ;; define-variable define-angle-variable
    ;; assert-x-axis assert-object
    ;; assert-compound-object
    ;; label-radius label-angle
    ;; lookup-force lookup-torque lookup-line
    ;; lookup-vector
    ;; lookup-mc-answer
    ;; check-answer
    ;; calculate-equation-string (find variable on lhs of equation)
    ;;                           (probably not in Andes3)

    ;; get or create the appropriate student entry
    (let ((entry (find-entry id)))

  #|    (cond ((string= action "new-object")
	     (if entry 
		 (progn (warn "New object ~A already exists, updating old object." id)
			;; update old object
			nil)
		 ;; create new object
		 (make-entry ???)))
	    ((or (string= action "delete-object")
		 (string= action "modify-object"))
	     (if entry
		 ;; update entry with new attributes, check object type
		 nil
		 ;;
		 (warn "Trying to modify non-existant object ~A, creating new object."))))
|#
       
    (cond
      ((string= action "delete-object")
       (delete-object entry))

      ;; Andes2 does not distinguish between a new entry and a 
      ;; modified entry since all the information about an object 
      ;; is resent every  time.  So, it just clobbers
      ;; the old information, if the entry already existed.
     
      ;; Andes3 allows partial information to be sent in the case
      ;; of an update.  We need a way of merging attributes in the
      ;; case of a modified object.  

      ;; The following tests should probably occur when the update
      ;; of *StudentEntries* is attempted:

      ;; type can't be updated, needs stored elsewhere
    ;  (assoc-from-variables mode x y
	;		text width height radius symbol x-label y-label angle) 


      ;; Since "text" is the only attribute and is required, 
      ;; we don't have to worry about merging.
      ;; But eventually, we will want to include position.
      ((string= type "equation")
       (unless text (error "Equation must always have text"))
       (lookup-eqn-string text id)) ;Unmodified Andes2 call

      ((string= type "phrase")
       (unless text (error "Definition must always have text"))
       (define-variable :text text :symbol symbol :id id))

      ;; Andes2 does not distinguish between a new entry and a 
      ;; modified entry since all the information about an object 
      ;; is resent every  time.  So, it just clobbers
      ;; the old information, if the entry already existed.
     
      ;; Andes3 allows partial information to be sent in the case
      ;; of an update.  We need a way of merging attributes in the
      ;; case of a modified object.  

      ;; The following tests should probably occur when the update
      ;; of *StudentEntries* is attempted:

      ((and (string= action "modify-object")
	    (not (find-entry id)))
       (error "Attempting to modify object ~A which doesn't exist." id))

      ((and (string= action "new-object")
	    (find-entry id))
       (error "Attempting to create object ~A which already exists." id))

      ;; Since "text" is the only attribute and is required, 
      ;; we don't have to worry about merging.
      ;; But eventually, we will want to include position.
      ((string= type "equation")
       (unless text (error "Equation must always have text"))
       (lookup-eqn-string text id)) ;Unmodified Andes2 call

      ((string= type "phrase")
       (unless text (error "Definition must always have text"))
       (define-variable :text text :symbol symbol :id id))

      ((string= action "new-object")
       (cond 
	 ((string= type "circle")
	  `(((:action . "log") 
	     (:assoc . (("DRAW-BODY" . "(BODY BALL)"))) 
	     (:id . ,id))
	    ((:action . "set-score") (:score . 15))
	    ((:action . "modify-object") (:id . ,id) (:mode . "right"))))
	 ((string= type "axes")
	 `(((:action . "log") 
	    (:assoc . (("DRAW-UNROTATED-AXES" . "(DRAW-AXES 0)"))) 
	    (:id . ,id))
	   ((:action . "set-score") (:score . 25))
	   ((:action . "modify-object") (:id . ,id) (:mode . "right"))))
	 (t 
	  `(((:action . "log") 
	     (:assoc . (("DRAW-NORMAL" . "VECTOR (FORCE BALL WALL1 NORMAL :TIME 1) (DNUM 120 deg))"))) 
	     (:id . ,id))
	    ((:action . "log") (:parse . "(= m_BALL (DNUM 2.0 kg))"))
	    ((:action . "set-score") (:score . 40))
	    ((:action . "modify-object") (:id . ,id) (:mode . "right"))))))

      ((string= action "modify-object")
       (cond 
	 (t  `(((:action . "set-score") (:score . 57))
	       ((:action . "modify-object") (:id . ,id) (:mode . "right"))))))

      (t (error "Undefined action ~A." action))))))

;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" seek-help 
    (&key time action href value text) 
  "ask for help, or do a step in a help dialog" 
  (env-wrap 
    ;; Andes2 had calls to:
    ;; next-step-help
    ;; explain-more
    ;; handle-student-response  (choose a quantity or a principle)
    ;; do-whats-wrong (for why-wrong-equation & why-wrong-object)
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
    ;; need to maybe store state

    (do-close-problem)
    (solver-unload)

    (let ((prob (help-env-problem webserver:*env*)))
      ;; this tells the session manager that the session is over.
      (setf webserver:*env* nil)
      `(((:action . "problem-closed") 
	 (:URL . "http://www.webassign.net/someting/or/other"))
	((:action . "log") 
	 (:subscores . (("NSH_BO_Call_Count" . (-0.05 0)) 
			("WWH_BO_Call_Count" . (-0.05 0))
			("Correct_Entries_V_Entries" . (0.05 17 19))
			("Correct_Answer_Entries_V_Answer_Entries" . (0.05 1 2)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

