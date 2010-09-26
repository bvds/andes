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
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main function for starting up the webserver
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *cleanup-thread* "Function to clean up idle sessions")

(defun start-help (&key (port 8080) server-log-path host db user password)
  ;; port            the port for the help server, default 8080
  ;; server-log-path path for log file
  ;; host            the ip address of the database, defaults to localhost
  ;; db              the name of the database, default "andes"
  ;; user            database user name, default "root"
  ;; password        the database password
  "start a server with help system, optionally specifying the server port, the log file path, and database access."
  ;; global setup

  ;; tune garbage collection
  (tune-generational-gc)

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

  ;; Set up database
  (andes-database:create :host host :db (or db "andes3") 
			 :user (or user "root") 
			 :password (or password "sin(0)=0"))

  ;; start webserver
  (webserver:start-json-rpc-service 
   "/help" :port port :log-function #'andes-database:write-transaction
   ;; Path for Hunchentoot server (and database access) errors.
   ;; Generally, these errors indicate something disasterous has
   ;; occurred.  Might want to find some way to inform the administrator.
   :server-log-path (or server-log-path
			(merge-pathnames "help-server.log" *andes-path*)))

    (setf *cleanup-thread*
	  #+sbcl (sb-thread:make-thread #'idle-cleanup-function)
	  #-sbcl (error "cleanup not implemented")))

(defun idle-cleanup-function ()
  "Function that periodically cleans up idle sessions and pings database."
  ;; Here, the cutoff is based entirely on idle time.
  (let ((cutoff (* 2 3600)))
    (loop
       ;; MySql drops connections that have been idle for over 8 hours.
       ;; Send trivial query, to keep connection alive.
       (andes-database:first-session-p :student "none" :section "none") 
       (sleep cutoff)
       (webserver:close-idle-sessions :idle cutoff :method 'close-problem))))

(defun stop-help () 
  "stop the web server running this service"

  (when  *cleanup-thread*
    #+sbcl (sb-thread:terminate-thread *cleanup-thread*)
    #-sbcl (error "cleanup not implemented"))

  (webserver:stop-json-rpc-service)
  ;; Stop database.
  (andes-database:destroy))

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx
	help-env-vars 
	;; These are all the variables that are be set by API commands
        ;; listed in Andes2 log files or their descendants.
	'(*CP* **NSH-NEXT-CALL** *NSH-NODES* *NSH-FIRST-PRINCIPLES*
	  *NSH-CURRENT-SOLUTIONS* *NSH-LAST-NODE* *NSH-SOLUTION-SETS* 
	  *NSH-GIVENS* *NSH-AXIS-ENTRIES* *NSH-BODYSETS* *NSH-VALID-ENTRIES* 
	  *NSH-PROBLEM-TYPE* symbols::*VARIABLES* *STUDENTENTRIES* 
	  *SG-EQNS* *SG-ENTRIES* *SG-SOLUTIONS*
          **Condition**  mt19937::*random-state* **grammar**
	  ;; List of Fade items.
	  *fades*
	  ;; Solver process (could easily be replaced by function argument
	  ;; in solver-load and solver-unload)
	  *process*
	  ;; slot mapping for Algebra/solver.cl
	  *id-solver-slot-map* *solver-free-slots*
	  ;; Session-specific variables in Help/Interface.cl
	  **current-cmd-stack** **current-cmd** *last-turn-response* 
	  *last-score*
          ;; Variables set in Config.cl, which is loaded for each session.
	  **Testing-Pause-Time-Threshold** **Filter-Constraint-losses**
          *followup-problems*
	  ;; Variables used for scoring in Help/RunTimeTest.cl
          *Runtime-Testset* *Runtime-Score-Testset*
	  *Runtime-testset-current-Solindex*
	  *Runtime-Testset-current-total-score* **Checking-entries**
	  ;; Variables holding session-local memos.
	  *parse-memo* *lexical-rules-memo* *rules-starting-with-memo*
	  ;; Cache variables in Testcode/Tests.cl
	  *test-cache-eqn-entries* *test-cache-given-eqn-entries*
	  *test-cache-axis-entries* *test-cache-objects* 
	  *test-cache-drawing-entries* **Current-Body-Expression-Form**
	  **Current-Prob-Requires-nonanswer-entries** **entry-entered**
	  *sg-systementry-optional-p-memo*
	  )
	#-sbcl "List of global variables that need to be saved between turns in a session."
	#+sbcl #'equalp
	)

(defstruct help-env  
  "Quantities that must be saved between turns of a session.  Member vals contains list of values for help-env-vars." 
	   section student problem vals)

;; Should be useful for debugging.
(defun get-session-variable (session var)
  "Get a session local variable for a given session (string)."
  (nth (position var help-env-vars)
       (help-env-vals (webserver:get-session-env session))))

(eval-when (:load-toplevel :compile-toplevel)
  (defun globally-special-p (s)
    "Utility function to determine if a variable has been declared special."
    #+abcl (values (EXT:SPECIAL-VARIABLE-P symbol t)) 
    #+cmu (eql (ext:info :variable :kind s) :special)
    #+sbcl (eql (sb-int:info :variable :kind s) :special)
    #-(or sbcl cmu abcl) 
    ;; Try to make a closure over S and return T if it won't close.
    (eval `(let ((maybe-closure (let ((,s nil))
				  (lambda () ,s))))
	     (let ((,s t))
	       (declare (ignorable ,s))
	       (funcall maybe-closure))))))

(defmacro env-wrap (&body body)
  "Make session-local copy of global variables, retrieving values from webserver:*env* at the beginning of a turn and saving them again at the end of the turn"
  (let ((save-help-env-vals
	 ;; Save local variables back to *env*.
	 `(setf (help-env-vals webserver:*env*) (list ,@help-env-vars))))
    
    ;; If the variable is not already declared special (via defvar,
    ;; for instance), then its scope will not be dynamic and env-wrap
    ;; will fail.
    (dolist (var help-env-vars) 
      (assert (globally-special-p var) nil
	      "Variable ~A not declared special" var))

    `(progn
      ;; Null webserver:*env* indicates that the student is trying to work
      ;; on a session that has been idle or has not been initialized:  
      (if (and webserver:*env* (help-env-p webserver:*env*))
	  (let ,(mapcar 
		 #'(lambda (x) (list x '(pop (help-env-vals webserver:*env*))))
		 help-env-vars)
	    ;; If there is an error or timeout, need to save current values
	    ;; back to the environment variable before passing control
	    ;; on to error handler.
	    (prog1 (handler-bind
		       ((error #'(lambda (c) (declare (ignore c)) 
				     ,save-help-env-vals))
			(sb-ext:timeout #'(lambda (c) (declare (ignore c)) 
					 ,save-help-env-vals)))
		     ,@body)
	      ,save-help-env-vals))
	  '(((:action . "show-hint")
	    (:text . "Your session is no longer active.&nbsp; Please reload this web page.")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                   The methods themselves.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *simulate-loaded-server* t "Put in delay in solution steps")

(webserver:defun-method "/help" open-problem (&key time problem user 
						   section extra) 
  "initial problem statement" 

  (declare (ignore time)) ;Time is used by logging

  ;; Need to think about error handling for the case where 
  ;; the session already exists.
  (when webserver:*env* 
    (warn "webserver:*env* already exists.  Session in progress?"))
  
  ;; webserver:*env* needs to be initialized before the wrapper
  (setq webserver:*env* (make-help-env :student user :problem problem 
				       :section section))

  ;; Update logging with session information
  (andes-database:set-session 
   webserver:*log-id* :student user :problem problem :section section
   :extra extra)
  
  (let (replies solution-step-replies predefs
		;; Override global variable on start-up
		(*simulate-loaded-server* nil))
    (env-wrap
      (setq **base-Htime** (universal-time->htime (get-universal-time)))
      (solver-load)
      (solver-logging *solver-logging*)
      
      ;; Config modifies *runtime-testset*, so we
      ;; need to make the session-local copy first. 
      (session-local-runtime-testset)

      ;; Used by some Runtime tests
      (setf **checking-entries** nil)

      ;; Andes2 had the following calls that can be found in log files:
      ;;   read-student-info; the only remaining step is:
      (Load-Config-File)			
      ;;   set-condition none
      (set-condition 'none) ;Any experimental condition
      ;;  Most of the set-up is done here.
      ;; The return for this may be of some use.
      (execute-andes-command 'read-problem-info problem)

      ;; Intialize fade list
      (initialize-fades *cp*)

      ;; Write problem statement.	      
      (let ((x 10) (y 10) (i 0)
	    (indent 30) ;indentation of buttons & answer boxes.
	    (line-sep 25) ;line separation
	    )
	(dolist  (line (problem-statement *cp*))
	  (cond ((unify line '(answer . ?rest))
		 ;; Need to do inlining for answer boxes, Bug #1689
		 (let ((id (format nil "statement~A" i)))
		   ;; Add to *StudentEntries* but don't evaluate in Help.
		   (let ((entry (make-studententry :id id :mode "unknown"
						   :type "statement")))
		     (select-sought-for-answer entry)
		     ;; sanity test
		     (unless (StudentEntry-prop entry)
		       (warn "Problem answer ~A doesn't match soughts" line))
		     (push entry *StudentEntries*))
		   (push `((:action . "new-object") (:type . "statement") 
			   (:id . ,id) (:mode . "unknown") 
			   (:x . ,(+ x indent)) (:y . ,y) 
			   (:width . 100) (:text . "Answer:       ")) 
			 replies)))
		
		;; Multiple choice.  See Bug #1551
		;; Checkboxes (with a done button) or radio buttons
		((or (unify line '(choose ?label ?a ?b . ?rest))
		     (unify line '(checkbox ?label ?a ?b . ?rest)))
		 (let* ((checkbox-p (eq (pop line) 'checkbox))
			(button-type (if checkbox-p "checkbox" "radio"))
			(label (pop line))
			dx dy)
		   ;; if labels are short enough, go horizontal
		   ;; Find longest label, add padding for button,
		   ;; and multiply by number of buttons
		   (if (> (* (+ (reduce #'max (mapcar #'length line)) 5) 
			     (length line)) 50)
		       (setf dx 0 dy line-sep)
		       (setf dx (/ 300 (length line)) dy 0))
		   
		   ;; sanity check
		   (unless (member `(choose-answer ,label . ?rest)
				   (problem-soughts *cp*)
				   :test #'unify)
		     (warn "Invalid label ~A for ~A" label button-type))

		   (let* ((id (format nil "~A" label))
			  (prop `(choose-answer ,label nil))
			  (buttons
			   (loop for choice in line and
			      value from 1 with
			      xx = (+ x indent)
			      do
			      ;; Put each button on a new row/column
				(unless (= value 1) 
				  (incf y dy) (incf xx dx))
			      collect
				`((:value . ,(format nil "~A" value))
				  (:type . ,button-type) 
				  ;; Indent buttons relative to text
				  (:x . ,xx) (:y . ,y) (:width . 300)
				  (:text . ,choice))
				)))
		     ;; checkboxes get an associated "done" button.
		     (when checkbox-p
		       (push `((:type . "done") (:label . "Enter")
			       ;; Indent buttons relative to text
			       (:x . ,(+ x indent)) (:y . ,(incf y line-sep)) 
			       (:width . 300)) 
			     buttons))
		     (push `((:action . "new-object") (:id . ,id)
			     (:type . "button") (:items . ,buttons))
			   replies)
		   
		     (push (make-studententry 
			    :id id :type "button" :mode "unknown"
			    :style button-type :prop prop) 
			   *studententries*))))
		 
		;; "I am done" button.  See Bug #1551
		;; Should have a more distinctive method
		;; of indicating this button type.
		((unify line '(choose ?label ?a))
		 (pop line)
		 (let* ((label (pop line))
			(id (format nil "doneButton~S" i))
			(prop `(done ,(or label 
					  (car (problem-soughts *cp*))))))
		   ;; Sanity checks
		   (when (and (null label) (cdr (problem-soughts *cp*)))
		     (warn "Ambiguous null label for choose"))
		   (unless (member (second prop) (problem-soughts *cp*)
				   :test #'unify)
		     (warn "Invalid label ~A for choose" label))
		   ;; Create a single push button
		   (push `((:action . "new-object") 
			   (:type . "button") (:id . ,id) 
			   (:items . (((:type . "done") (:label . "Done")
				       ;; Indent buttons relative to text
				       (:x . ,(+ x indent)) (:y . ,y) (:width . 300)
				       (:text . ,(car line)))))) replies)
		   (push (make-studententry 
			  :id id :mode "unknown" :type "button" 
			  :style "done" :prop prop)
			 *studententries*)
		   ))
		
		(t 
		 (push `((:action . "new-object") (:type . "statement") 
			 (:id . ,(format nil "statement~A" i))
			 (:mode . "locked") (:x . ,x) (:y . ,y) 
			 (:width . 400) (:text . ,line)) replies)))
	  (incf i)
	  (setf y (+ y line-sep)))
	
	(when (problem-graphic *cp*)
	  (let ((g (problem-graphic *cp*)))
	    (push `((:action . "new-object") (:id . "graphic") 
		    (:type . "graphics") (:mode . "locked") 
		    (:x . ,x) (:y . ,y) 
		    (:width . ,(second g)) (:height . ,(third g))
		    ;; This is the URL for the graphic, which may not
		    ;; match its location on the server filesystem.
		    (:href . ,(strcat "../images/" (first g))))
		  replies)
	    (setf y (+ y (third g) 15)))))
	
      ;; Second column for times and predefs.
      (let ((x 450) (y 15) (i 0))

	;;  Push times to client.
	(dolist (time-sentence (problem-times-english *cp*))
	  (push `((:action . "new-object") 
		  (:id .  ,(format nil "time~A" (incf i)))
		  (:type . "statement") (:mode . "locked") 
		      (:width . 250) (:x . ,x) (:y . ,y) 
		  (:text . ,time-sentence))
		replies)
	  (setf y (+ y 25)))
            
	;; This must be done within env-wrap since it uses *cp*
	(setf predefs (problem-predefs *cp*))
	;; position objects and add common stuff.
	(dolist (predef predefs)
	  ;; Debug text
	  ;; (format webserver:*stdout* "Working on ~A~%" (cdr predef))
	  (pushnew '(:action . "new-object") (cdr predef) :key #'car)
	  (pushnew `(:id . ,(format nil "pre~A" (incf i))) (cdr predef) :key #'car)
	  (pushnew '(:mode . "unknown") (cdr predef) :key #'car)
	  (when (and (car predef) (not (assoc :type (cdr predef))))
	    (push `(:type . ,(entryprop2type (car predef))) (cdr predef)))
	  (when (and (car predef) (assoc :symbol (cdr predef))
		     (not (assoc :text (cdr predef))))
	    (push `(:text . ,(write-definition-text 
			      (car predef) 
			      (cdr (assoc :symbol (cdr predef))))) 
		  (cdr predef)))
	  (when (and (not (assoc :width (cdr predef)))
		     (member (cdr (assoc :type (cdr predef)))
			     '("statement" "equation") :test #'equal))
	      (push '(:width . 300) (cdr predef)))
	  ;; put all predefs in a second column
	  (pushnew `(:x . ,x) (cdr predef) :key #'car)
	  (if (assoc :y (cdr predef))
	      ;; If object overlaps this column, continue below it.
	      (when (> (+ (cdr (assoc :x (cdr predef)))
			  (cdr (or (assoc :width (cdr predef))
				   (assoc :radius (cdr predef)))))
		       x)
		(setf y (max y (cdr (assoc :y (cdr predef))))))
	      (push `(:y . ,y) (cdr predef)))
	  ;; (format webserver:*stdout* "  Turned to ~A~%" (cdr predef))
	  (setf y (+ y 25))))


      (check-entries t))
    
    ;; Send pre-defined quantities to the help system via
    ;; the solution-step method.
    ;; Execute outside of env-wrap and with check-entries turned on.
    (dolist (predef (mapcar #'cdr predefs)) ;ignore any entry-prop
      ;; (format webserver:*stdout* "Sending predef ~A~%" (cdr predef))
      (let ((reply (apply #'solution-step 
			  ;; flatten the alist
			  (mapcan 
			   #'(lambda (x) (list (car x) (cdr x)))
			   predef))))
	(setf solution-step-replies 
	      (append solution-step-replies 
		      (cons predef reply))))
      ;; (format webserver:*stdout* "   done with predef ~A~%" (cdr predef))
      )
      
    (env-wrap (check-entries nil))     
    
    ;; Pull old sessions out of the database that match
    ;; student, problem, & section.  Run turns through help system
    ;; to set problem up and set scoring state.
    (dolist (old-step (andes-database:get-matching-sessions 
		       '("solution-step" "seek-help")
		       :student user :problem problem :section section
		       :extra extra))
      (let* ((method (cdr (assoc :method old-step))) 
	     ;; Remove time, since it is no longer meaningful.
	     (params (remove :time
			     (cdr (assoc :params old-step))
			     :key #'car))
	     (reply (apply 
		     (cond 
		       ((equal method "solution-step") #'solution-step)
		       ((equal method "seek-help") #'seek-help))
		     ;; flatten the alist
		     (mapcan #'(lambda (x) (list (car x) (cdr x))) 
			     params))) 
	     
	     ;; solution-steps and help results are passed back to client
	     ;; to set up state on client.
	     ;;
	     ;; Help requests are sent to the help system, to set the solution
	     ;; status and grading state.  Alternatively, we could just send 
	     ;; the solution steps to the help system state and then set 
	     ;; the grading state by brute force.
	     ;; 
	     ;; Drop actions that make modal changes to the user interface.
	     ;; Since there is no mechanism to connect log messages to specific
	     ;; solution steps or help actions, also drop log messages.
	     (send-reply (remove-if 
			  #'(lambda (x) (member (cdr (assoc :action x)) 
						'("focus-hint-text-box" "log"
						  "focus-major-principles" 
						  "focus-all-principles")
						:test #'equal))
			  reply)))

	
	;; Echo any solution step action
	(when (equal method "solution-step")
	  (push params send-reply))
	
	;; Echo any text entered in Tutor pane text box.
	(when (and (equal method "seek-help") 
		   (equal (cdr (assoc :action params)) "get-help")
		   (assoc :text params))
	  (push `((:action . "echo-get-help-text") 
		  (:text . ,(cdr (assoc :text params)))) send-reply))
	
	(setf solution-step-replies
	      (append solution-step-replies send-reply))))

    (env-wrap

      ;; Determine if this is the first session for this user.
      (when (andes-database:first-session-p :student user :section section 
					    :extra extra)
	(let ((dialog-text 
	       (if (member 'introduction (problem-features *cp*))
		   (strcat "If this is your first time using Andes, "
			   "this problem will help you get started.&nbsp; "
			   "Just follow the instructions on the right.")
		   (strcat "If this is your first time using Andes, "
			   "you should go back and try an "
			   "introductory problem."))))
	  ;; Start up special dialog box.
	  (push `((:action . "new-user-dialog")
		  (:text . ,dialog-text)) replies)))

      ;; If there was no previous session, perform initial update of 
      ;; faded items.  In the case of Fades in the Tutor pane, write 
      ;; initial instruction.
      (unless solution-step-replies (setf replies (update-fades replies)))

      ;; Enable z-axis vectors, based on problem features
      (unless (intersection '(circular rotkin angmom torque mag gauss) 
			  (problem-features *cp*))
	    (push '((:action . "set-styles")
		    (:tool . "vectorSecondary")
		    (:style . "disabled")) replies))
      
      ;; set-stats (if there was an old score) (to do)
      ;; Should this be wrapped in execute-andes-command?

      (set-stats '(("NSH_BO_Call_Count" . 0)
		   ("WWH_BO_Call_Count" . 0)
		   ("Correct_Entries_V_Entries" . (0 0)) 
		   ("Correct_Answer_Entries_V_Answer_Entries" . (0 0))))
   
      (push `((:action . "log") 
	      (:subscores . (("NSH_BO_Call_Count" . 0)
			     ("WWH_BO_Call_Count" . 0)
			     ("Correct_Entries_V_Entries" . (0 0)) 
			     ("Correct_Answer_Entries_V_Answer_Entries" . (0 0)))))
	    replies)      
      (push `((:action . "set-score") (:score . 0)) replies))

    ;; assemble list of replies to send to client.
    (append (reverse replies) solution-step-replies)))

;; helper function to guess api type from entryprop.
(defun entryprop2type (prop)
  "guess Andes3 api \"type\" from entryprop."
  (case (car prop)
    (eqn "equation")
    (define-var "statement")
    (body "ellipse") ;could also be rectangle
    (vector "vector")
    (line "line")
    (draw-axes "axes")))

;; helper function to write out definition text based on Ontology.
(defun write-definition-text (prop symbol)
  "Write variable definition text for a given prop and symbol."
  (if (find-systementry prop)
      (strcat "Let " symbol " be " 
	      (match:word-string 
	       ;; no variables have been defined: 
	       ;; remove any (var ...)
	       (expand-vars 
		(systementry-model (find-systementry prop)))))
      (progn (warn "write-definition-text:  Can't find systementry for ~S" 
		   prop)
	     (strcat symbol ":  Can't find definition"))))


;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" solution-step 
    (&key time id action type mode x y checked
	  text width height radius symbol x-statement y-statement
	  x-label y-label z-label angle cosphi) 
  "problem-solving step"
  ;; fixed attributes:      type style id
  ;; updatable attributes:  mode x y text width height radius symbol 
  ;;                         x-label y-label z-label angle cosphi checked
  (env-wrap 
    ;; Andes2 also had calls to:
    ;; define-angle-variable  (undocumented leftover from Andes1)
    ;; assert-compound-object
    ;; label-angle (removed from Andes3, may restore if angle tool added)
    ;; calculate-equation-string (find variable on lhs of equation)
    ;;                           (not in Andes3)
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
      
      (when *simulate-loaded-server* 
	(format webserver:*stdout* 
		"  *simulate-loaded-server* induced sleep.~%")
	(sleep 2))
      
      (when (and old-entry (equal action "new-object"))
	(warn "Object ~A already exists, updating old object." id))
      
      (when (and (not old-entry) (or (equal action "modify-object")
				     (equal action "delete-object")))
	(warn "Object ~A does not exist, creating new object." id))
      
      (when (and type old-entry 
		 (not (equal type (StudentEntry-type old-entry))))
	(warn "Attempting to change type from ~A to ~A"
	      (StudentEntry-type old-entry) type))

      ;; create new object
      (setf new-entry (make-StudentEntry :id id :type type :time time))
      
      ;; update attributes from old object (but not time!)
      (when old-entry
	(update-entry-from-entry 
	 new-entry old-entry 
	 type mode style x y text width height radius symbol x-statement 
         y-statement x-label y-label z-label angle cosphi checked prop))
      
      ;; update new object from non-null variables
      (update-entry-from-variables 
       new-entry  
       mode x y text width height radius symbol x-statement y-statement
       x-label y-label z-label angle cosphi checked)
      
      (add-entry new-entry)   ;remove existing info and update
      
      (update-fades
       (cond
	 ((equal action "delete-object")
	  ;; We should pass the object to be deleted rather than the id.
	  (execute-andes-command 'delete-object (StudentEntry-id new-entry)))
	 
	 ;; For debugging only, should be turned off in production
	 ((and webserver:*debug* (equal text "help-test-error")
	       (error "help-test-error response.")))
	 
	 ;; Look for text box marked by "Answer: "
	 ;; This should come before "equation" and "statement"
	 ((and (>= (length text) (length ans))
	       (string-equal (string-left-trim match:*whitespace* text)
			     ans :end1 (length ans)))
	  ;; In Andes2 this was set in do-check-answer
	  (setf (StudentEntry-verbatim new-entry) 
	       (string-trim match:*whitespace* (subseq text (length ans))))
	  (execute-andes-command 'check-answer new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "equation")
	  (let ((eq (search "=" text)))
	    (cond 
	      ;; solve for variable:
	      ;; Right now, look for an "=" and subsequent "?"
	      ;; Should be done using parsed expression, checking that
	      ;; the LHS is a single variable.
	      ((and eq (search "?" (subseq text eq)))
	       (setf (StudentEntry-symbol new-entry) 
		     (string-trim match:*whitespace* (subseq text 0 eq)))
	       (execute-andes-command 'solve-for-var new-entry))
	      ;; Default case: ordinary equation
	      (t (execute-andes-command 'lookup-eqn-string new-entry)))))
	 
	 ((equal (StudentEntry-type new-entry) "statement")
	  (execute-andes-command 'define-variable new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "graphics")
	  (warn "Can't modify a graphic object, id=~A" 
		(studententry-id new-entry)))
	
	 ((equal (StudentEntry-type new-entry) "circle")
	  (execute-andes-command 'assert-object new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "ellipse")
	  (execute-andes-command 'assert-object new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "rectangle")
	  (execute-andes-command 'assert-object new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "axes")
	  (execute-andes-command 'assert-x-axis new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "vector")
	  (execute-andes-command 'lookup-vector new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "line")
	  (execute-andes-command 'lookup-line new-entry))

	 ((equal (StudentEntry-type new-entry) "button")
	  (execute-andes-command 'lookup-mc-answer new-entry))
	 
	 (t (warn "Undefined type ~A, doing nothing."  
		  (StudentEntry-type new-entry))))))))


;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" seek-help 
    (&key time action href value text) 
  "ask for help, or do a step in a help dialog" 
  (declare (ignore time))  ;used by logging.

  ;; for choosing principle of physics, not working yet.
  (declare (ignore href))  

  (env-wrap 
    ;; Doesn't correctly handle case where "Explain-more" is clicked after
    ;; a bottom-out hint.
    (cond
      ;; Press help button.  
      ;; call next-step-help or do-whats-wrong
      ((equal action "help-button")
       ;; Find if there are any current errors.
       (let ((mistakes (remove +incorrect+ *studententries* 
			       :test-not #'eql
			       :key #'StudentEntry-state)))
	 (if mistakes
	     (execute-andes-command 'do-whats-wrong 
				    ;; find most recent mistake, time-wise
				    (reduce #'most-recent-entry
					    mistakes))
	     (execute-andes-command 'next-step-help))))
      ;; Student has typed text in Tutor pane input box.
      ((and (equal action "get-help") text)
       (execute-andes-command 'handle-student-response text))
      ;; Student has clicked a link associated with the help.
      ((and (equal action "get-help") value)
       (let ((response-code (find-symbol value)))
	 (if response-code 
	     (execute-andes-command 'handle-student-response response-code)
	     (warn "Unknown get-help value ~S, doing nothing; see Bug #1686." 
		   value))))
      ((equal action "principles-menu")
       (execute-andes-command 'handle-student-response value))
      (t (warn "undefined action ~A, doing nothing." action)))))

(defun most-recent-entry (x y)
  "most recent entry"
  ;; previous sessions have time slot removed
  (if (>= (or (studententry-time x) 0)
	 (or (studententry-time y) 0))
      x y))

(webserver:defun-method "/help" close-problem 
  (&key time) 
  "shut problem down" 
  (declare (ignore time))  ;used by logging.
  (unwind-protect
      (env-wrap
	(let ((result (execute-andes-command 'get-stats 'persist)))
		 
	  (do-close-problem)
	  (solver-unload)
	  
	  (push `((:action . "problem-closed") 
		  (:URL . "http://www.webassign.net/something/or/other"))
		result)
	  result))
    ;; Tell the session manager that the session is over.
    ;; Must be done after env-wrap
    (setf webserver:*env* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
