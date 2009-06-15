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
	'(*CP* **NSH-NEXT-CALL** *NSH-NODES* *NSH-FIRST-PRINCIPLES*
	  *NSH-CURRENT-SOLUTIONS* *NSH-LAST-NODE* *NSH-SOLUTION-SETS* 
	  *NSH-GIVENS* *NSH-AXIS-ENTRIES* *NSH-BODYSETS* *NSH-VALID-ENTRIES* 
	  *NSH-PROBLEM-TYPE* **ALTERNATE-COMMAND-INTERPRETER** *VARIABLES* 
	  *STUDENTENTRIES* *SG-EQNS* *SG-ENTRIES* *SG-SOLUTIONS*
          **Condition**  mt19937::*random-state* **grammar**
	  ;; Session-specific variables in Help/Interface.cl
	  **current-cmd-stack** **current-cmd** 
	  *last-tutor-turn* *last-score*
          ;; Variables set in Config.cl, which is loaded for each session.
          *Runtime-Testset* *Runtime-Score-Testset*
	  **Testing-Pause-Time-Threshold** **Filter-Constraint-losses**
          *followup-problems*
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
		(error "Problem graphic file ~A missing" 
		       (problem-graphic *cp*))))))
      
      ;;  New:  return any predefs, any work done, and score to the client.
      
      (push `((:action . "new-object") (:id . "a2") (:type . "statement") 
	      (:mode . "correct") (:x . 200) (:y . 55) 
	      (:text . "g = 9.8 m/s^2 is the gravitational acceleration \nnear the surface of the earth.")) 
	    replies)
	
      (push `((:action . "new-object") (:id .  "a2.5") (:type . "statement") 
		(:mode . "correct") (:x . 200) (:y . 75) (:text . "T0 is the time.")) 
	    replies)
      
      (push `((:action . "log") 
	      (:subscores . (("NSH_BO_Call_Count" . (0 0))
			     ("WWH_BO_Call_Count" . (0  0))
			     ("Correct_Entries_V_Entries" . (0 0 0)) 
			     ("Correct_Answer_Entries_V_Answer_Entries" .  (0 0 0)))))
	    replies)
      
      (push `((:action . "set-score") (:score . 0)) replies)
      
      replies)))

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
    ;; label-angle lookup-force lookup-torque
    ;; lookup-mc-answer check-answer
    ;; calculate-equation-string (find variable on lhs of equation)
    ;;                           (probably not in Andes3)
    ;; Andes2 but not in Andes3:  label-radius

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

      (when (and old-entry (equal action "new-object"))
	(warn "Object ~A already exists, updating old object." id))

      (when (and (not old-entry) (or (equal action "modify-object")
				     (equal action "delete-object")))
	(warn "Object ~A does not exist, creating new object." id))

      ;; check type, if there is no old entry
      (unless (or old-entry (member type *allowed-studententry-types* :test #'equal))
	(warn "Attribute type=~A not allowed." type)
	(setf type nil))


      (when (and old-entry 
		 (not (equal type (StudentEntry-type old-entry))))
	(warn "Attempting to change type from ~A to ~A"
	      (StudentEntry-type old-entry) type))

     ;; create new object
      (setf new-entry (make-StudentEntry :id id :type type))
 
      ;; update attributes from old object
      (when old-entry
	(update-entry-from-entry 
	 new-entry old-entry 
	 type mode x y text width height radius symbol 
	 x-label y-label z-label angle))

      ;; update new object from variables
      (update-entry-from-variables 
       new-entry  
       mode x y text width height radius symbol x-label y-label z-label angle)
      
      (cond
	((equal action "delete-object")
	 ;; We should pass the object to be deleted rather than the id.
	 (delete-object (StudentEntry-id new-entry)))
	
	;; Since "text" is the  attribute and is required. 
	((equal type "equation")
	 (lookup-eqn-string new-entry)) ;Almost unmodified Andes2 call
	
	((equal type "statement")
	 (define-variable new-entry))
	 
	((equal type "graphics")
	 (warn "Can't modify a graphic object, id=~A" 
	       (studententry-id new-entry)))
	
	((equal type "circle")
	 (assert-object new-entry))

	((equal type "rectangle")
	 (assert-object new-entry))

	((equal type "axes")
	 (assert-x-axis new-entry))

	((equal type "vector")
	 (lookup-vector new-entry))

	((equal type "line")
	 (lookup-line new-entry))

      (t (warn "Undefined type ~A." type))))))

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
      ((equal action "get-help")
       '(((:action . "show-hint") 
	  (:text . "Because the vector is parallel to the Y axis 
but in the negative direction, the projection equation is Fearth_y = - Fearth so
 Fearth_y stands for a negative value."))
	 ((:action . "show-hint-link") 
	  (:text . "Explain more") 
	  (:value . "Explain-More"))))
      ((equal action "help-button")
       '(((:action . "show-hint") 
	  (:text . "Now that you have stated all of the given information, you should start on the major principles. What quantity is the problem seeking?"))
	 ((:action . "focus-hint-text-box"))))
      ((equal action "principles-menu")
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

