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

  ;; Set up database
  (andes-database:create)

  ;; start webserver
  (webserver:start-json-rpc-service 
   "/help" :port port :log-function #'andes-database:write-transaction)

    (setf *cleanup-thread*
	  #+sbcl (sb-thread:make-thread #'idle-cleanup-function)
	  #-sbcl (error "cleanup not implemented")))

(defun idle-cleanup-function ()
  "Function that periodically cleans up idle sessions"
  ;; Here, the cutoff is based entirely on idle time.
  (let ((cutoff (* 2 3600)))
    (loop
     (sleep cutoff)
     (webserver:close-idle-sessions :idle cutoff :method #'close-problem))))


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
	  *NSH-PROBLEM-TYPE* *VARIABLES* *STUDENTENTRIES* 
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
	  *sg-systementry-optional-p-memo*
	  )
	#-sbcl "List of global variables that need to be saved between turns in a session."
	#+sbcl #'equalp
	)

;; New method with 
(defstruct help-env "Quantities that must be saved between turns of a session.  Member vals contains list of values for help-env-vars." 
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
      ;; on a session that has timed out or has not been initialized:  
      (if (and webserver:*env* (help-env-p webserver:*env*))
	  (let ,(mapcar 
		 #'(lambda (x) (list x '(pop (help-env-vals webserver:*env*))))
		 help-env-vars)
	    ;; If there is an error, need to save current values
	    ;; back to the environment variable before passing control
	    ;; on to error handler.
	    (prog1 (handler-bind
		       ((error #'(lambda (c) (declare (ignore c)) 
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
   webserver:*log-id* :student user :problem problem :section section)
  
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
      (let ((x 10) (y 10) (i 0))
	(dolist  (line (problem-statement *cp*))
	  (cond ((unify line '(answer . ?rest))
		 (push `((:action . "new-object") (:type . "statement") 
		  (:id . ,(format nil "statement~A" i))
		  (:mode . "unknown") (:x . ,x) (:y . ,y) 
		  (:width . 100) (:text . "Answer:       ")) replies))
		(t 
		 (push `((:action . "new-object") (:type . "statement") 
		  (:id . ,(format nil "statement~A" i))
		  (:mode . "locked") (:x . ,x) (:y . ,y) 
		  (:width . 400) (:text . ,line)) replies)))
	  (incf i)
	  (setf y (+ y 25)))

	(when (problem-graphic *cp*)
	  (let ((dims (problem-graphic-dimensions (problem-graphic *cp*))))
	    (if dims		
		(push `((:action . "new-object") (:id . "graphic") 
			(:type . "graphics") (:mode . "locked") 
			(:x . ,x) (:y . ,y) 
			(:width . ,(car dims)) (:height . ,(cadr dims))
			;; This is the URL for the graphic, which may not
			;; match its location on the server filesystem.
			(:href . ,(strcat "/images/" (problem-graphic *cp*))))
		      replies)
		(warn "Problem graphic file ~A missing" 
		      (problem-graphic *cp*)))
	    (setf y (+ y (second dims) 15))))
	
	;;  Push times to client.
	(dolist (time-sentence (problem-times-english *cp*))
	  (push `((:action . "new-object") 
		  (:id .  ,(format nil "time~A" (incf i)))
		  (:type . "statement") (:mode . "locked") 
		      (:width . 250) (:x . ,x) (:y . ,y) 
		  (:text . ,time-sentence))
		replies)
	  (setf y (+ y 25))))

      ;; Second column for fades and predefs.
      (let ((x 450) (y 15) (i 0))
	(dolist (fade *fades*)
	  ;; Debug text
	  ;; (format webserver:*stdout* "Working on ~A~%" (cdr fade))
	  (pushnew '(:action . "new-object") (cdr fade) :key #'car)
	  (pushnew `(:id . ,(format nil "fade~A" (incf i))) (cdr fade) :key #'car)
	  (pushnew '(:mode . "fade") (cdr fade) :key #'car)
	  (pushnew '(:type . "statement") (cdr fade) :key #'car)
	  (when (and (not (assoc :width (cdr fade)))
		     (member (cdr (assoc :type (cdr fade)))
			     '("statement" "equation") :test #'equal))
	    (push '(:width . 300) (cdr fade)))
	  (pushnew `(:x . ,x) (cdr fade) :key #'car)
	  (if (assoc :y (cdr fade))
	      ;; If object overlaps this column, continue below it.
	      (when (> (+ (cdr (assoc :x (cdr fade)))
			  (cdr (or (assoc :width (cdr fade))
				   (assoc :radius (cdr fade)))))
		       x)
		(setf y (max y (cdr (assoc :y (cdr fade))))))
	      (push `(:y . ,y) (cdr fade)))
	  (push (cdr fade) replies)
	  ;; (format webserver:*stdout* "  Turned to ~A~%" (cdr fade))
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
	     (params (assoc :params old-step))
	     (reply (apply 
		     (cond 
		       ((equal method "solution-step") #'solution-step)
		       ((equal method "seek-help") #'seek-help))
		     ;; flatten the alist
		     (mapcan #'(lambda (x) (list (car x) (cdr x))) 
			     (cdr params))))) 

	;; solution-steps and help result are passed back to client
	;; to set up state on client.
	;;
	;; Help requests are handled silently by the help system,
	;; just to get the grading correct.  Alternatively, we
	;; could just send the solution steps to the help system
	;; and then set the grading state by brute force.
	;;  
	;; Also, if this is an admin or researcher, or instructor,
	;; previous hints and their replies should also be sent back to
	;; the client.
	(when (equal method "solution-step")
	  (setf solution-step-replies
		(append solution-step-replies (cons (cdr params) reply))))))

    (env-wrap

      ;;  Push initial hint to the client.  
      ;;  Should only do this when help and grading is available
      (push '((:action . "show-hint") (:text . "If you need help, click the help button <span dojoType=\"dijit.form.Button\" disabled=\"true\">?</span> below.&nbsp; Click the <span class=\"dojoxExpandoIcon dojoxExpandoIconRight\" style=\"float:none;display:inline-block;margin-right:6px;\" disabled=\"true\"></span> button above to hide this window.")) 
	    replies)
  
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
	      (word-string 
	       ;; no variables have been defined: 
	       ;; remove any (var ...)
	       (expand-vars 
		(systementry-new-english (find-systementry prop)))))
      (progn (warn "write-definition-text:  Can't find systementry for ~S" 
		   prop)
	     (strcat symbol ":  Can't find definition"))))


;; need error handler for case where the session isn't active
;; (webserver:*env* is null).  
(webserver:defun-method "/help" solution-step 
    (&key time id action type mode x y
	  text width height radius symbol x-statement y-statement
	  x-label y-label z-label angle) 
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
	 type mode x y text width height radius symbol x-statement y-statement
	 x-label y-label z-label angle))
      
      ;; update new object from non-null variables
      (update-entry-from-variables 
       new-entry  
       mode x y text width height radius symbol x-statement y-statement
       x-label y-label z-label angle)
      
      (add-entry new-entry)   ;remove existing info and update
      
      (update-fades
       (cond
	 ((equal action "delete-object")
	  ;; We should pass the object to be deleted rather than the id.
	  (delete-object (StudentEntry-id new-entry)))
	 
	 ;; For debugging only, should be turned off in production
	 ((and webserver:*debug* (equal text "help-test-error")
	       (error "help-test-error response.")))
	 
	 ;; Look for text box marked by "Answer: "
	 ;; This should come before "equation" and "statement"
	 ((and (> (length text) (length ans))
	       (string-equal (string-left-trim *whitespace* text)
			     ans :end1 (length ans)))
	  ;; In Andes2 this was set in do-check-answer
	  (setf (StudentEntry-verbatim new-entry) 
	       (string-trim *whitespace* (subseq text (length ans))))
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
		     (string-trim *whitespace* (subseq text 0 eq)))
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
       (let ((mistakes (member **incorrect** *studententries* 
			       :key #'StudentEntry-state)))
	 (if mistakes
	     (execute-andes-command 'do-whats-wrong 
				    ;; find most recent mistake, time-wise
				    (car (sort mistakes #'> 
					       :key  #'StudentEntry-time)))
	     (execute-andes-command 'next-step-help))))
      ;; Student has typed text in help pane.
      ((and (equal action "get-help") text)
       (execute-andes-command 'handle-student-response text))
      ;; Student has clicked a link associated with the help.
      ((and (equal action "get-help") value)
       (let ((response-code (find-symbol (string-upcase value))))
	 (unless response-code (warn "Unknown value ~A, using nil." value))
	 (execute-andes-command 'handle-student-response response-code)))
      ((equal action "principles-menu")
       (execute-andes-command 'handle-student-response value))
      (t (warn "undefined action ~A, doing nothing." action)))))

(webserver:defun-method "/help" close-problem 
  (&key time) 
  "shut problem down" 
  (declare (ignore time))  ;used by logging.
  (prog1
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

