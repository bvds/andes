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
	'(*CP* **NSH-NEXT-CALL** *NSH-NODES* *NSH-FIRST-PRINCIPLES*
	  *NSH-CURRENT-SOLUTIONS* *NSH-LAST-NODE* *NSH-SOLUTION-SETS* 
	  *NSH-GIVENS* *NSH-AXIS-ENTRIES* *NSH-BODYSETS* *NSH-VALID-ENTRIES* 
	  *NSH-PROBLEM-TYPE* *VARIABLES* *STUDENTENTRIES* 
	  *SG-EQNS* *SG-ENTRIES* *SG-SOLUTIONS*
          **Condition**  mt19937::*random-state* **grammar**
	  ;; slot mapping for Algebra/solver.cl
	  *id-solver-slot-map* *solver-free-slots*
	  ;; Session-specific variables in Help/Interface.cl
	  **current-cmd-stack** **current-cmd** *last-tutor-turn* *last-score*
          ;; Variables set in Config.cl, which is loaded for each session.
	  **Testing-Pause-Time-Threshold** **Filter-Constraint-losses**
          *followup-problems*
	  ;; Variables used for scoring in Help/RunTimeTest.cl
          *Runtime-Testset* *Runtime-Score-Testset*
	  *Runtime-testset-current-Solindex*
	  *Runtime-Testset-current-total-score*
	  ;; Variables holding session-local memos.
	  *parse-memo* *grammar-get-rhs-memo* *grammar-get-rhs-with-first*
	  ;; Cache variables in Testcode/Tests.cl
	  *test-cache-eqn-entries* *test-cache-given-eqn-entries*
	  *test-cache-axis-entries* *test-cache-objects* 
	  *test-cache-drawing-entries* **Current-Body-Expression-Form**
	  **Current-Prob-Requires-nonanswer-entries** **entry-entered**
	  )
	#-sbcl "List of global variables that need to be saved between turns in a session."
	#+sbcl #'equalp
	)

;; New method with 
(defstruct help-env "Quantities that must be saved between turns of a session.  Member vals contains list of values for *help-env-vars*." 
	   student problem vals)

;; Should be useful for debugging.
(defun get-session-variable (session var)
  "Get a session local variable for a given session (string)."
  (nth (position var *help-env-vars*)
       (help-env-vals (webserver:get-session-env session))))


(defmacro env-wrap (&body body)
  "Make session-local copy of global variables, retrieving values from webserver:*env* at the beginning of a turn and saving them again at the end of the turn"
  (let ((save-help-env-vals
	 ;; Save local variables back to *env*.
	 `(setf (help-env-vals webserver:*env*) (list ,@*help-env-vars*)))) 
    `(progn
      ;; An error here indicates that the student is trying to work
      ;; on a session that has timed out or has not been initialized:  
      ;; probably should have a appropriate handler that gives instructions
      ;; to start a new session
      (assert webserver:*env*)
      ;; further sanity check.
      (assert (help-env-p webserver:*env*))
      (let ,(mapcar 
	     #'(lambda (x) (list x '(pop (help-env-vals webserver:*env*))))
	     *help-env-vars*)
	;; If there is an error, need to save current values
	;; back to the environment variable before passing control
	;; on to error handler.
	(prog1 (handler-bind
		   ((error #'(lambda (c) (declare (ignore c)) 
				     ,save-help-env-vals)))
		 ,@body)
	  ,save-help-env-vals)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                   The methods themselves.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(webserver:defun-method "/help" open-problem (&key time problem user) 
  "initial problem statement" 

  ;; Need to think about error handling for the case where 
  ;; the session already exists.
  (when webserver:*env* 
    (warn "webserver:*env* already exists.  Session in progress?"))
  
  ;; webserver:*env* needs to be initialized before the wrapper
  (setq webserver:*env* (make-help-env :student user :problem problem))
  
  (let (replies solution-step-replies)
    (env-wrap
      (setq **base-Htime** (universal-time->htime (get-universal-time)))
      (solver-load)
      (solver-logging *solver-logging*)
      
      ;; Config modifies *runtime-testset*, so we
      ;; need to make the session-local copy first. 
      (session-local-runtime-testset)

      ;; Andes2 had the following calls that can be found in log files:
      ;;   read-student-info; the only remaining step is:
      (Load-Config-File)			
      ;;   set-condition none
      (set-condition 'none) ;Any experimental condition
      ;;  Most of the set-up is done here.
      ;; The return for this may be of some use.
      (execute-andes-command #'read-problem-info problem)
      ;;
      
      ;; do predefs
      (dolist (predef (problem-predefs *cp*))
	(push (cdr predef) replies))
      
      ;;  Push any work done, to the client here. (to do)
      
    (check-entries t))
 
    ;; Send pre-defined quantities to the help system by sending them
    ;; to the solution-step method.  
    ;; Execute outside of env-wrap and with check-entries turned on.
    (dolist (reply replies)
      (setf solution-step-replies
	    (append (apply #'solution-step 
			   ;; flatten the alist
			   (mapcan #'(lambda (x) (list (car x) (cdr x))) 
				   reply))
		    solution-step-replies)))

    (env-wrap
      (check-entries nil)      

      ;;  Push times to client.  (to do)
      (push `((:action . "new-object") (:id .  "a2.5") (:type . "statement") 
	      (:mode . "locked") (:x . 200) (:y . 75) (:text . "T0 is the time.")) 
	    replies)
      
      (let ((y 10) (i 0))
	(dolist  (line (problem-statement *cp*))
	  (push `((:action . "new-object") (:type . "statement") 
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
		(warn "Problem graphic file ~A missing" 
		       (problem-graphic *cp*))))))

  
      ;;   set-stats (if there was an old score) (to do)

      (set-stats '(("NSH_BO_Call_Count" . 0)
		   ("WWH_BO_Call_Count" . 0)
		   ("Correct_Entries_V_Entries" . (0 0)) 
		   ("Correct_Answer_Entries_V_Answer_Entries" .  (0 0))))
   
      (push `((:action . "log") 
	      (:subscores . (("NSH_BO_Call_Count" . 0)
			     ("WWH_BO_Call_Count" . 0)
			     ("Correct_Entries_V_Entries" . (0 0)) 
			     ("Correct_Answer_Entries_V_Answer_Entries" .  (0 0)))))
	    replies)      
      (push `((:action . "set-score") (:score . 0)) replies))
      
    (append replies solution-step-replies)))

;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" solution-step 
    (&key time id action type mode x y
	  text width height radius symbol x-label y-label z-label angle) 
  "problem-solving step"
  ;; fixed attributes:      type id
  ;; updatable attributes:  mode x y text width height radius symbol 
  ;;                         x-label y-label z-label angle
  (env-wrap 
    ;; Andes2 also had calls to:
    ;; define-angle-variable assert-compound-object
    ;; label-angle
    ;; lookup-mc-answer
    ;; calculate-equation-string (find variable on lhs of equation)
    ;;                           (probably not in Andes3)
    ;; Andes2 but not in Andes3:  label-radius lookup-torque lookup-force
    
    ;; Andes2 does not distinguish between a new entry and a 
    ;; modified entry since all the information about an object 
    ;; is present every  time.  So, it just clobbers
    ;; the old information, if the entry already existed.
    
    ;; Andes3 allows partial information to be sent in the case
    ;; of an update.  We need a way of merging attributes in the
    ;; case of a modified object.  
    
    ;; The strategy here is to first make a minimal change to the 
    ;; old set-up and get things working, and then make changes to
    ;; move the handling of StudentEntries to the top level.

    (let ((old-entry (find-entry id)) new-entry 
	  (ans "Answer:"))

      (when (and old-entry (equal action "new-object"))
	(warn "Object ~A already exists, updating old object." id))

      (when (and (not old-entry) (or (equal action "modify-object")
				     (equal action "delete-object")))
	(warn "Object ~A does not exist, creating new object." id))

      (when (and type old-entry 
		 (not (equal type (StudentEntry-type old-entry))))
	(warn "Attempting to change type from ~A to ~A"
	      (StudentEntry-type old-entry) type))
      ;; If time slot is missing, set time to zero.
      ;; Predefs generally don't have a time slot.
      ;; Client always should send a time.
      (unless time (setf time 0.0))

     ;; create new object
      (setf new-entry (make-StudentEntry :id id :type type :time time))
 
      ;; update attributes from old object (but not time!)
      (when old-entry
	(update-entry-from-entry 
	 new-entry old-entry 
	 type mode x y text width height radius symbol 
	 x-label y-label z-label angle))

      ;; update new object from non-null variables
      (update-entry-from-variables 
       new-entry  
       mode x y text width height radius symbol x-label y-label z-label angle)

      (cond
	((equal action "delete-object")
	 ;; We should pass the object to be deleted rather than the id.
	 (delete-object (StudentEntry-id new-entry)))

	;; Look for answer box marked by "Answer: "
	;; This should come before "equation" and "statement"
	((and (> (length text) (length ans))
	      (string-equal (string-left-trim *whitespace* text)
			    ans :end1 (length ans)))
	 ;; In Andes2 this was set in do-check-answer
	 (setf (StudentEntry-verbatim new-entry) 
	       (string-trim *whitespace* (subseq text (length ans))))
	 (execute-andes-command #'check-answer new-entry))
	
	((equal (StudentEntry-type new-entry) "equation")
	 (let ((eq (search "=" text)))
	   (cond 
	     ;; solve for variable:
	     ;; Right now, look for an "=" and subsequent "?"
	     ;; Should be done using parsed expression, checking that
	     ;; the LHS is a single variable.
	     ((and eq (search "?" (subseq text eq)))
	      (setf (StudentEntry-symbol new-entry) 
		    (string-trim *whitespace* (subseq text 0 eq)))
	      (execute-andes-command #'solve-for-var new-entry))
	     ;; Default case: ordinary equation
	     (t (execute-andes-command #'lookup-eqn-string new-entry)))))
	
	((equal (StudentEntry-type new-entry) "statement")
	 (execute-andes-command #'define-variable new-entry))
	
	((equal (StudentEntry-type new-entry) "graphics")
	 (warn "Can't modify a graphic object, id=~A" 
	       (studententry-id new-entry)))
	
	((equal (StudentEntry-type new-entry) "circle")
	 (execute-andes-command #'assert-object new-entry))

	((equal (StudentEntry-type new-entry) "rectangle")
	 (execute-andes-command #'assert-object new-entry))

	((equal (StudentEntry-type new-entry) "axes")
	 (execute-andes-command #'assert-x-axis new-entry))

	((equal (StudentEntry-type new-entry) "vector")
	 (execute-andes-command #'lookup-vector new-entry))

	((equal (StudentEntry-type new-entry) "line")
	 (execute-andes-command #'lookup-line new-entry))

      (t (warn "Undefined type ~A, doing nothing."  
	       (StudentEntry-type new-entry)))))))


;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" seek-help 
    (&key time action href value text) 
  "ask for help, or do a step in a help dialog" 
  (env-wrap 
    ;; Doesn't correctly handle case where "Explain-more" is clicked after
    ;; a bottom-out hint.
    (cond
      ;; Press help button.  
      ;; call next-step-help or do-whats-wrong
      ((equal action "help-button")
       ;; Find if there are any current errors.
       (let ((mistakes (member **incorrect** *studententries* 
			       :key #'StudentEntry-state)))
	 (if mistakes
	     (execute-andes-command  #'do-whats-wrong 
				     ;; find most recent mistake, time-wise
				     (car (sort mistakes #'> 
						:key  #'StudentEntry-time)))
	     (execute-andes-command  #'next-step-help))))
      ;; Student has typed text in help pane.
      ((and (equal action "get-help") text)
       (execute-andes-command  #'handle-student-response text))
      ;; Student has clicked a link associated with the help.
      ((and (equal action "get-help") value)
       (let ((response-code (find-symbol (string-upcase value))))
	 (unless response-code (warn "Unknown value ~A, using nil." value))
	 (execute-andes-command  #'handle-student-response response-code)))
      ((equal action "principles-menu")
       (execute-andes-command  #'handle-student-response value))
      (t (warn "undefined action ~A, doing nothing." action)))))

(webserver:defun-method "/help" close-problem 
  (&key time) 
  "shut problem down" 
  (prog1
      (env-wrap 
       ;; Andes2 had calls to:
       ;; get-stats (instead, we need to send grade to LMS)
       ;; need to maybe store state
       
       (do-close-problem)
       (solver-unload)
       
       `(((:action . "problem-closed") 
	  (:URL . "http://www.webassign.net/someting/or/other"))
	 ((:action . "log") 
	  (:subscores . (("NSH_BO_Call_Count" . (-0.05 0)) 
			 ("WWH_BO_Call_Count" . (-0.05 0))
			 ("Correct_Entries_V_Entries" . (0.05 17 19))
			 ("Correct_Answer_Entries_V_Answer_Entries" 
			  . (0.05 1 2)))))))
    ;; Tell the session manager that the session is over.
    ;; Must be done after env-wrap
    (setf webserver:*env* nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

