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
  ;; See Documentation/server.html for setting default values for
  ;; the database login.
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
  (andes-database:create :host host :db db :user user :password password)

  ;; start webserver
  (webserver:start-json-rpc-services 
   '(("/help" :json-rpc t :log-function andes-database:write-transaction)
     ("/get-score" :post t))
   :port port
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
       (andes-database:get-most-recent-tID) 
       (sleep cutoff)
       (webserver:close-idle-sessions 
	:log-function 'andes-database:write-transaction
	:idle cutoff :method 'close-problem))))

(defun stop-help () 
  "stop the web server running this service"

  (when  *cleanup-thread*
    #+sbcl (sb-thread:terminate-thread *cleanup-thread*)
    #-sbcl (error "cleanup not implemented"))

  (webserver:stop-json-rpc-services)
  ;; Stop database.
  (andes-database:destroy))

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx
	help-env-vars 
	;; These are all the variables that are set by API commands
        ;; listed in Andes2 log files or their descendants.
	'(*CP* **NSH-NEXT-CALL** *NSH-NODES* *NSH-FIRST-PRINCIPLES*
	  *NSH-CURRENT-SOLUTIONS* *NSH-LAST-NODE* *NSH-SOLUTION-SETS* 
	  *NSH-GIVENS* *NSH-AXIS-ENTRIES* *NSH-BODYSETS* *NSH-VALID-ENTRIES* 
	  *NSH-PROBLEM-TYPE* symbols::*VARIABLES* *STUDENTENTRIES* 
	  *SG-EQNS* *SG-ENTRIES* *SG-SOLUTIONS*
          mt19937::*random-state* **grammar**
	  ;; Cache for word completion utility.
	  *phrase-cache*
	  ;; List of quantities and objects not in solutions.
	  *wrong-quantities*
	  ;; List of Fade items.
	  *fades*
	  ;; Memo for word-count in matches.
	  match::*word-count-memo*
	  ;; Solver process (could easily be replaced by function argument
	  ;; in solver-load and solver-unload)
	  *process*
	  ;; slot mapping for Algebra/solver.cl
	  *id-solver-slot-map* *solver-free-slots*
	  ;; Session-specific variables in Help/Interface.cl
	  **current-cmd-stack** **current-cmd** *last-turn-response* 
	  *last-score*
	  ;; Variables used for grades
	  *grade* *random-average-score* *help-last-entries*
	  ;; Variables holding session-local memos.
	  *parse-memo* *lexical-rules-memo* *rules-starting-with-memo*
	  ;; Session state information
	  session:*user* session:*section* session:*problem* 
	  session:*state-cache*
	  )
	#-sbcl "List of global variables that need to be saved between turns in a session."
	#+sbcl #'equalp
	)

;; Useful for debugging.
(defun get-session-variable (session var)
  "Get a session local variable for a given session (string)."
  (nth (position var help-env-vars)
       (webserver:get-session-env session)))

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
	 `(setf webserver:*env* (list ,@help-env-vars))))
    
    ;; If the variable is not already declared special (via defvar,
    ;; for instance), then its scope will not be dynamic and env-wrap
    ;; will fail.
    (dolist (var help-env-vars) 
      (assert (globally-special-p var) nil
	      "Variable ~A not declared special" var))

    `(progn
      ;; Null webserver:*env* indicates that the student is trying to work
      ;; on a session that has been idle or has not been initialized:  
      (if (and webserver:*env* (listp webserver:*env*))
	  (let ,(mapcar 
		 #'(lambda (x) (list x '(pop webserver:*env*)))
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

  ;; Need to think about error handling for the case where 
  ;; the session already exists.
  (when webserver:*env* 
    (warn "webserver:*env* already exists.  Session in progress?"))

  ;; webserver:*env* needs to be initialized before the wrapper
  (setq webserver:*env* (make-list (length help-env-vars) 
				   :initial-element nil))

  ;; Update logging with session information
  ;; Should be done first so that entry in PROBLEM_ATTEMPT exists 
  ;; before starting turn logging.
  (andes-database:set-session 
   webserver:*log-id* :student user :problem problem :section section
   :extra extra)
  
  (let (replies solution-step-replies predefs
		last-client-id ;used when re-running old sessions
		;; Override global variable on start-up
		(*simulate-loaded-server* nil))
    (env-wrap

      ;; Save session information for later accessing student
      ;; model and customizations.
      (setf session:*user* user
	    session:*section* section
	    session:*problem* problem)

      (solver-load)
      (solver-logging *solver-logging*)
      
      ;;  Most of the set-up is done here.
      ;; The return for this may be of some use.
      (execute-andes-command time 'read-problem-info problem)

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

		   ;; Add weight and choices to grading
		   (let ((sysent (find-SystemEntry `(choose-answer ,label . ?rest))))
		     (if sysent
			 (setf (graded-possibilities (SystemEntry-graded sysent))
			       (length line)  ;should be list, but for now...
			       ;;(list nil nil (length line))
			       ;; Need a real determination of weight.
			       (graded-weight (SystemEntry-graded sysent))
			       11
			       )
			 (warn "No SystemEntry for multiple-choice ~A" label)))
		   
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
			;; If name of activity was supplied, use that.
			;; else search for any done button.
			(label-match (if label (list 'done label) 
					     '(done . ?rest)))
			;; Find any done button SystemEntry.
			(donners (remove label-match 
					 *sg-entries*
					 :key #'SystemEntry-prop
					 :test-not #'unify))
			(prop (when donners (SystemEntry-prop (car donners)))))
		   ;; Sanity checks
		   (when (or (null donners) (cdr donners))
		     ;; Student can't successfully solve the problem if
		     ;; this is broken.
		     (error 'webserver:log-error
			   :tag (list 'setup-button-match label 
				      (mapcar #'SystemEntry-prop donners))
		     :text "Bad SystemEntry match for button."))
		   ;; Create a single push button
		   (push `((:action . "new-object") 
			   (:type . "button") (:id . ,id) 
			   (:items . (((:type . "done") (:label . "Done")
				       ;; Indent buttons relative to text
				       (:x . ,(+ x indent)) (:y . ,y) 
				       (:width . 300)
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
	  (let* ((g (problem-graphic *cp*))
		 (width (second g)) (height (third g)))
	    (unless (and height width)
	      (setf height 100 width 100) ;give some value so we can muddle through.
	      (warn 'webserver:log-warn
		    :tag (list 'graphic-dimensions-missing (first g))
		    :text "Graphic dimensions not included."))
	    (push `((:action . "new-object") (:id . "graphic") 
		    (:type . "graphics") (:mode . "locked") 
		    (:x . ,x) (:y . ,y) 
		    (:width . ,width) (:height . ,height)
		    ;; This is the URL for the graphic, which may not
		    ;; match its location on the server filesystem.
		    (:href . ,(strcat "../images/" (first g))))
		  replies)
	    (setf y (+ y height 15)))))
	
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
	  (setf y (+ y 25)))))
    
    ;; Send pre-defined quantities to the help system via
    ;; the solution-step method.
    ;; Execute outside of env-wrap and with check-entries turned on.
    (let ((**checking-entries** t))
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
	))
    
    ;; Pull old sessions out of the database that match
    ;; student, problem, & section.  Run turns through help system
    ;; to set problem up and set scoring state.
    (andes-database:old-sessions
     (dolist (old-step (andes-database:get-matching-sessions 
			 '("solution-step" "seek-help" "record-action")
			 :student user :problem problem :section section
			 :extra extra))

       ;; Detect first turn in an old session.
       (unless (equal last-client-id (cdr old-step))
	 ;; flush state cache
	 (env-wrap 
	   (setf session:*state-cache* nil))
	 (andes-database:set-old-session-start (cdr old-step))
	 (setf last-client-id (cdr old-step)))
       
	(let* ((method (cdr (assoc :method (car old-step))))
	       (params (cdr (assoc :params (car old-step))))
	       ;; If an old session turn produces an error, convert it
	       ;; into a warning and move on to the next turn.
	       ;; Otherwise old sessions with unfixed errors cannot 
	       ;; be reopened.
	       (reply 
		(handler-case
		    (apply 
		     (cond 
		       ((equal method "solution-step") #'solution-step)
		       ;; Help requests are sent to the help system to set 
		       ;; the solution status and grading state.  
		       ;; Alternatively, we could just send the solution 
		       ;; steps to the help system state and then set 
		       ;; the grading state by brute force.
		       ((equal method "seek-help") #'seek-help)
		       ((equal method "record-action") #'record-action))
		     ;; flatten the alist
		     (mapcan #'(lambda (x) (list (car x) (cdr x))) 
			     params))
		  (error (c) (old-errors-into-warnings c method params))))
	       
	       ;; solution-steps and help results are passed back to client
	       ;; to set up state on client.
	       (send-reply (filter-reply reply)))	  
	  
	  ;; Echo any solution step action
	  (when (equal method "solution-step")
	    ;; "checked" is supposed to be a json array.
	    ;; Need special handling in case it is empty.
	    (let ((checked (assoc :checked params)))
	      (when (and checked (null (cdr checked)))
		;; Hack for creating an empty array in json
		(setf (cdr checked) (make-array '(0)))))
	    ;; Remove time from reply
	    (push (remove :time params :key #'car) send-reply))
	  
	  ;; Echo any text entered in Tutor pane text box.
	  (when (and (equal method "seek-help") 
		     (equal (cdr (assoc :action params)) "get-help")
		     (assoc :text params))
	    (push `((:action . "echo-get-help-text") 
		    (:text . ,(cdr (assoc :text params)))) send-reply))
	  
	(setf solution-step-replies
	      (append solution-step-replies send-reply)))))
    (env-wrap
      ;; After running through old session, flush state cache
      (setf session:*state-cache* nil)

      ;; Determine if student needs an informed consent dialog
      ;; initial dialog flag (usually for section) should contain
      ;; url for content.
      (when (and (andes-database:get-state-property 'consent-dialog)
		 (not (andes-database:get-state-property 
		       'informed-consent :model "client")))
	(push `((:action . "new-user-dialog")
		(:url . ,(andes-database:get-state-property 'consent-dialog)))
	      replies))

      ;; Determine if this is the first session for this user.
      ;; Do separately from seen-intro-video in case video window
      ;; is killed by pop-up blocker on client.
      (unless (andes-database:get-state-property 'has-seen-intro-dialog)
	(let ((dialog-text 
	       (if (member 'introduction (problem-features *cp*))
		   (strcat "If this is your first time using Andes, "
			   "this problem will help you get started.&nbsp; "
			   "After watching the video, "
			   "just follow the instructions on the right.")
		   (strcat "If this is your first time using Andes, "
			   "you should go back and try an "
			   "introductory problem."))))
	  (andes-database:set-state-property 
		   'has-seen-intro-dialog t)
	  ;; Start up special dialog box.
	  (push `((:action . "new-user-dialog")
		  (:text . ,dialog-text)) replies)))
      
      ;; If there was no previous session, perform initial update of 
      ;; faded items.  In the case of Fades in the Tutor pane, write 
      ;; initial instruction.
      (unless solution-step-replies
	(setf replies (update-fades replies :push-p t)))

      ;; Enable z-axis vectors, based on problem features
      (unless (intersection '(circular rotkin angmom torque mag gauss) 
			  (problem-features *cp*))
	    (push '((:action . "set-styles")
		    (:tool . "vectorSecondary")
		    (:style . "disabled")) replies))

      ;; Return user customizations.
      (dolist (preference (andes-database:get-state-properties 
			   :model "client"))
	(push `((:action . "set-preference")
		(:name . ,(car preference))
		(:value . ,(cdr preference)))
	      replies))
           
      (push `((:action . "set-score") (:score . 0)) replies))

    ;; log the user-agent http header
    (push `((:action . "log") (:log . "user-agent") 
	    (:text . ,(hunchentoot:user-agent))) replies)

    ;; assemble list of replies to send to client.
    (append (reverse replies) solution-step-replies)))


;; helper function to handle errors from old sessions
;; Turns errors into log-warn.
(defun old-errors-into-warnings (c method params)
  (warn 'webserver:log-warn
	:tag (list 'old-session-error method params 
		   (type-of c) (webserver:get-any-log-tag c))
	:text (format nil "Error from rerunning old sessions: ~A" c)))  


;; helper function to write out definition text based on Ontology.
(defun write-definition-text (prop symbol)
  "Write variable definition text for a given prop and symbol."
  (if (find-systementry prop)
      (strcat "Let " symbol " be " 
	      (match:word-string 
	       ;; no variables have been defined: 
	       ;; remove any (var ...)
	       (expand-vars 
		(systementry-model (find-systementry prop))
		:html-format t)))
      (progn (warn "write-definition-text:  Can't find systementry for ~S" 
		   prop)
	     (strcat symbol ":  Can't find definition"))))

;; Drop actions that make modal changes to the user interface.
;; Since there is no mechanism to connect log messages to 
;; specific solution steps or help actions, also drop log 
;; messages.
;;
(defun filter-reply (reply)
  "Filter out replies to send back to client."
  (let ((actions '("new-object" "modify-object" "delete-object"
		   "set-score" "set-preferences" "set-styles"))
	(hints '("show-hint" "show-hint-link" "echo-get-help-text")) 
	result)
    (dolist (line (reverse reply))
      (let ((action (cdr (assoc :action line))))
	;; Very long old sessions can have reply strings
	;; that overflow the 65535 byte text field in the database.
	;; Need to consolidate or drop some hints.  Bug #1934
	(when (or (member action hints :test #'equal)
		  (member action actions :test #'equal))
	  (push line result))))
    result))

(defun problem-times-english (problem)
  "Return list of English sentences defining times."
  (let ((times (problem-times problem)))
    (cond ((null times) '("Let T0 be the time."))
	  ((eql times 'none) nil)
	  ((listp (problem-times problem))
	   ;; mapcar copies list; subsequent operations can be destructive
	   (delete nil (mapcar #'problem-time-english times)))
	  (t (warn "Invalid time specifications ~A" times)))))
  
(defun problem-time-english (time)
  "Return English sentence for a given problem-times specification."
  (cond 
    ((eql (car time) 'during)
     (when (fourth time)
       (format nil "~A: ~A." 
	       (string-upcase (nlg (subseq time 0 3)) :end 1) (fourth time))))
    ((numberp (car time))
	 (format nil "Time ~A~:[ has been defined~;:  ~:*~A~]." 
		 (nlg (car time) 'moment) (second time)))
    (t (warn "Bad time specification ~A" time))))

(defparameter *comment-leading-characters* '(#\? #\! #\; #\: #\, #\& #\# #\%))

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

    (let ((old-entry (find-entry id)) new-entry)
      
      (when *simulate-loaded-server* 
	(format webserver:*stdout* 
		"  *simulate-loaded-server* induced sleep.~%")
	(sleep 2))
      
      (when (and old-entry (equal action "new-object"))
	(warn  'webserver:log-warn :tag (list 'create-existing-object id)
	       :text "Object already exists, updating old object."))
      
      (when (and (not old-entry) (or (equal action "modify-object")
				     (equal action "delete-object")))
	(warn 'webserver:log-warn :tag (list 'modify-non-existant-object id)
	      :text "Object does not exist, creating new object."))
      
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

      ;; For Modified help experiment.
      ;; Need a way to do this kind of stuff that is
      ;; "pluggable".  Bug #1940
      (random-help-experiment:set-current-object id)

      (model-no-tutor-turn time) ;for model of doing step without help
      
      (update-fades
       (cond
	 ((equal action "delete-object")
	  ;; We should pass the object to be deleted rather than the id.
	  (execute-andes-command time 'delete-object 
				 (StudentEntry-id new-entry)))
	 
	 ;; For debugging only, should be turned off in production
	 ((and webserver:*debug* (equal text "help-test-error")
	       (error "help-test-error response.")))

	 ;; Look for text with leading punctuation.  This indicates
	 ;; a comment, which is not analyzed by the help system.
	 ((and (> (length text) 0) 
	       (member (aref text 0) *comment-leading-characters*))
	  `(((:action . "show-hint") (:text . ,(strcat "A " *unevaluated-entry* ".")))))
	 
	 ;; Look for text box marked by "Answer..."
	 ;; This should come before "equation" and "statement"
	 ((eql 0 (search "Answer" 
			 (string-left-trim match:*whitespace* text) 
			 :test #'string-equal ;case insensitive
			 ))
	  ;; In Andes2 this was set in do-check-answer
	  (setf (StudentEntry-verbatim new-entry) 
		;; Make it case-insensitive
		(pull-out-quantity "Answer" text :test #'string-equal))
	  (execute-andes-command time 'check-answer new-entry))
	 
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
	       (execute-andes-command time 'solve-for-var new-entry))
	      ;; Default case: ordinary equation
	      (t (execute-andes-command time 'lookup-eqn-string new-entry)))))
	 
	 ((equal (StudentEntry-type new-entry) "statement")
	  (execute-andes-command time 'define-variable new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "graphics")
	  (warn "Can't modify a graphic object, id=~A" 
		(studententry-id new-entry)))
	
	 ((equal (StudentEntry-type new-entry) "circle")
	  (execute-andes-command time 'assert-object new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "ellipse")
	  (execute-andes-command time 'assert-object new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "rectangle")
	  (execute-andes-command time 'assert-object new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "axes")
	  (execute-andes-command time 'assert-x-axis new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "vector")
	  (execute-andes-command time 'lookup-vector new-entry))
	 
	 ((equal (StudentEntry-type new-entry) "line")
	  (execute-andes-command time 'lookup-line new-entry))

	 ((equal (StudentEntry-type new-entry) "button")
	  (execute-andes-command time 'lookup-mc-answer new-entry))
	 
	 (t (warn "Undefined type ~A, doing nothing."  
		  (StudentEntry-type new-entry))))))))


;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" seek-help 
    (&key time action href value text) 
  "ask for help, or do a step in a help dialog" 

  ;; for choosing principle of physics, not working yet.
  (declare (ignore href))  

  ;; Typing this string in help window will cause server to sleep.
  (when (equalp text "sleep-test") (sleep 20))

  (env-wrap 
    ;; Doesn't correctly handle case where "Explain-more" is clicked after
    ;; a bottom-out hint.
    (cond
      ;; Press help button.  
      ;; call next-step-help or do-whats-wrong
      ((equal action "help-button")
       (model-tutor-turn)
       (model-increment-state-property 'clicked-help-button)
       ;; Find if there are any current errors.
       (let ((mistakes (remove +incorrect+ *studententries* 
			       :test-not #'eql
			       :key #'StudentEntry-state)))
	 (if mistakes
	     (execute-andes-command time
				    'do-whats-wrong 
				    ;; find most recent mistake, time-wise
				    (reduce #'most-recent-entry
					    mistakes))
	     (execute-andes-command time 'next-step-help))))
      ;; Student has typed text in Tutor pane input box.
      ((and (equal action "get-help") text)
       (model-tutor-turn)
       (execute-andes-command time 'handle-student-response text))
      ;; Student has clicked a link associated with the help.
      ((and (equal action "get-help") value)
       (model-tutor-turn)
       (model-increment-state-property 'clicked-help-link)
       (let ((response-code (find-symbol value)))
	 (if response-code 
	     (execute-andes-command time 'handle-student-response response-code)
	     (warn "Unknown get-help value ~S, doing nothing; see Bug #1686." 
		   value))))
      ((equal action "principles-menu")
       (execute-andes-command time 'handle-student-response 
			      (read-from-string value)))
      (t (warn "undefined action ~A, doing nothing." action)))))

(defun most-recent-entry (x y)
  "most recent entry"
  ;; previous sessions have time slot removed
  (if (>= (or (studententry-time x) 0)
	 (or (studententry-time y) 0))
      x y))

(defparameter *tool-types*
  ;; List should match list of types in smd.
  ;; The help system is free to ignore the tool type.
  '(("rectangle" . body)
    ("circle" . body)
    ("ellipse" . body)
    ("line" . line)
    ("statement" . scalar)
    ("vector" . vector)))

(webserver:defun-method "/help" suggest-word (&key time text type symbol)
  "return possible next words"
  (declare (ignore time)) ;for logging

  (env-wrap
   (let ((words (next-word-list 
		 (to-word-list (pull-out-quantity symbol text))
		 :type (cdr (assoc type *tool-types* :test #'equalp)))))
     `(((:action . "next-words")
	;; :false is mapped onto json false via a special hack
	;; in Base/web-server.cl
	(:last-word . ,(if (member nil words) t :false))
	(:words . ,(or (remove nil words)
			;; Hack for creating an empty array in json
			(make-array '(0)))))))))


(webserver:defun-method "/help" record-action (&key time type name value)
  "Record user interface action, but send no reply.  In this case we do something only for preferences."

  (env-wrap
    (cond 
      ((and (equal type "window") (equal name "IntroVideo"))
       (cond 
	 ((equal value "focus")
	  (model-looking-at-video time))
	 ((equal value "blur")
	  (model-stop-looking-at-video time))))
      ((and (equal type "menu-choice") (equal name "menuIntroVideo"))
       (andes-database:set-state-property 'seen-intro-video 'menu))
      ((equal type "tutor-link")
       (model-tutor-turn)
       (model-increment-state-property 'clicked-help-link))
      ((equal type "set-preference")
       (andes-database:set-state-property name value :model "client"))))

  ;; This method has no return
  nil)

(webserver:defun-method "/help" close-problem 
  (&key time) 
  "shut problem down" 
  (unwind-protect
      (env-wrap
	(let (result)
		 
	  (do-close-problem)
	  (solver-unload)
	  
	  (push `((:action . "problem-closed") 
		  ;; (:URL . "http://www.webassign.net/something/or/other")
		  )
		result)
	  result))
    ;; Tell the session manager that the session is over.
    ;; Must be done after env-wrap
    (setf webserver:*env* nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;            Grade reporting service
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(webserver:defun-method "/get-score" nil (input)
  "Pull grade from database."
  (when nil
    (format webserver:*stdout* "in get-score method for ~S~%"
	    (cdr (assoc "LONCAPA_correct_answer" 
			input :test #'equal))))

  ;; This will go into the general Hunchentoot log
  ;; along with a backtrace.  Bug #1907
  (unless (cdr (assoc "LONCAPA_correct_answer" input
			    :test #'equal))
    (error "get-score received invalid input:  ~A" input))

  (let* ((ans  (json:decode-json-from-string 
		(cdr (assoc "LONCAPA_correct_answer" input
			    :test #'equal))))
	 (student (cdr (assoc :user ans)))
	 (problem (cdr (assoc :problem ans)))
	 (section (cdr (assoc :class ans)))
	 (score (andes-database:get-score
		 :student student :problem problem 
		 :section section)))
    (format nil (strcat "<loncapagrade>~%"
			"  <awarddetail>ASSIGNED_SCORE</awarddetail>~%"
			"  <awarded>~F</awarded>~%"
			;; could include message
			;; "  <message>Thank you</message>~%"
			"</loncapagrade>~%")
	    ;; null score indicates session missing.
	    (if score (/ score 100) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
