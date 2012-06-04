;;; Interface.cl
;;; Autor(s):  Collin Lynch <CollinL@pitt.edu>
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2008
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch and execution code for the Andes API.
;;; The execue-andes-command function located in this file 
;;; exists as the interface between the Workbench API and the
;;; help system API.  
;;;
;;; This code takes in a command and a set of arguments that 
;;; have been read from the tcp/ip stream by the code in 
;;; Andes-Main.cl.  The command is presumed to be one of the
;;; andes2 workbench api commands listed in Commands.cl and 
;;; State.cl.  
;;;
;;; This code will form the requisite cmd struct and then dispatch
;;; the api command with arguments by calling it.  
;;;
;;; The resulting turn will be used to form the
;;; cmdresult.  Lastly, the system will pass the turn to return-turn
;;; which will send it back to the workbench.
;;;
;;; The purpose of this isolation is to make out api-interface much 
;;; cleaner and to give us a central point in which to trap all of 
;;; the running code.  It also makes it easier for me to hang the 
;;; command generation code in one single place.  
;;;
;;;
;;;


;;;; ======================================================================
;;;; General Parameters.
;;;; These parameters are set and used by the code below.  And in some 
;;;; cases by other APIs in the system.  


;;; As the CMDs are generated they will be added to the **current-cmd-stack** 
;;; list until an open-problem dde is encountered.  At which point the list 
;;; will be  cleared but for the open-problem cmd. 
(defparameter **current-cmd-stack** Nil "The current CMDs.")

;;; The most recent cmd is stored here in addition to the cmd stack to 
;;; facilitate direct access.
(defparameter **current-cmd** Nil "The current Cmd.")

;;; Most recent turn responder, allowing students to continue 
;;; ongoing dialogs by clicking on links or entering text.
(defvar *last-turn-response* nil "Most recent turn responder function.")

;;; *last-score* -- last integer score we sent to workbench
;;; used to detect when score has not changed so no need to send
(defvar *last-score* NIL "last score reported to workbench")

;;;; =========================================================================
;;;; Central Dispatch call.
;;;; The Execute-andes-command function is the central dispatch call for the 
;;;; Andes2 Help system.  It is called with a Command, a set of arguments and
;;;; a flag indicating whether or not the command is a dde (if t) or dde-post
;;;; if not.  DDE commands are called expecting a reply.  DDE-Post calls do 
;;;; not and so any returned value will be thrown away rather than being tossed
;;;; onto the stream.  
;;;;
;;;; When the Andes2-Main server pulls a command off of the stream it will pass
;;;; the command data to the Execute-andes-command func.  When the result is 
;;;; returned it is assumed to be a formatted reply string that can be passed
;;;; to the workbench along with the requisite contact info.  
;;;;
;;;; The main function here will handle the maintenance of the cmd list.  



;;; ----------------------------------------------------------------------------
;;; Central API Call.
;;; The execute-andes-command function is the central entry-point to the command
;;; api from the stream.  When the user initiates an action on the workbench an
;;; appropriate command will be placed on the tcp-ip stream.  The server code in
;;; andes2-main will parse the command off of the stream and pass it to the 
;;; execute-andes-command function below.
;;;
;;; This function will generate a new cmd for the action and execute the 
;;; command.  The result from that execution will be  alist, possibly null, 
;;; or a tutor turn.  
;;;
;;; If the result is a tutor-turn it will be translated into a string for return 
;;; to the workbench again.  This is done because there are 
;;; some errors that can be signalled within this process and it is necessary
;;; to trap them before the cmdresult is formed or the results are tested.  
;;;
;;; Once that is done the result or the Str with be parsed into a cmdresult and
;;; appended to the cmd before the autograding tests are run.  The result value
;;; will be used unless it is a tutor turn and an error was thrown by return-turn
;;; when it was being processed.
;;;
;;; Once the cmdresult has been appended to the cmd then the aurograding tests
;;; will be run and any commands to update the grades will be sent.  At that 
;;; point the result (or string if the result is a tutor turn) will be returned
;;; to the workbench.  
;;;
;;; NOTE:: I am carrying out the string parsing before handling the cmdresult
;;;   in order to trap errors in the same way that they were trapped by the 
;;;   previous versions of Andes that ran the return-turn code.
;;;
;;;

(defun execute-andes-command (time Command &optional entry)
  "Execute the api call with the command and arguments."
  ;; Generally, the solution steps are StudentEntry structs
  ;; and the help calls are not.

  ;; Determine the student state and set hinting policy.
  ;; It really should be done after the student action has
  ;; been analyzed; see Bug #1956
  (learned-help-experiment:set-policy time)

  (let* ((text (and entry (StudentEntry-p entry) (StudentEntry-text entry)))
	 (Arguments (and entry (list entry)))
	 ;; Set the last api call to be this call.
	 (NewCmd (iface-generate-log-cmd Command text))
	 (Result (if (and *cp* 
			  (member 'answer-only-mode (problem-features *cp*)))
		     (answer-only-dispatcher Command Arguments)
		     (apply command Arguments)))
	 (Str (cond ((turn-p Result) (return-turn time Result)) 
		    ((every #'alistp result) result)
		    (t (warn 'log-condition:log-warn
			     :tag (list 'invalid-turn-result-format result)
			     :text "Invalid result format.")))))
    
    ;; Having nil violates the API
    (when (and (turn-p result) (member nil (turn-result result)))
      (warn 'log-condition:log-warn 
	    :tag (list 'result-contains-nil command (turn-result result))
	    :text "nil in solution-step reply"))

    ;; Once the command has been executed and any result parsed then we
    ;; need to add the cmdresult to the current cmd.
    (iface-add-cmdresult-to-cmd NewCMD Result)
    
    ;; Hints only affect grading when there has been an explicit
    ;; request for help.  These are all the criteria for 
    ;; bottom-out hints as required by Andes2 grading:
    (when (and (eql (cmd-class NewCmd) 'help)
	       ;; Test that hint is child of student request
	       ;; for help.  In Andes2, this was done by calls
	       ;; to wwh-bottom-out-hintp and proc-bottom-out-hintp 
	       (bottom-out-hintp **Current-cmd-Stack**)
	       (turn-p result) 
	       (eql (turn-type result) nil)
	       (turn-text result) ;hint given
	       (null (turn-menu result)) ;last hint in sequence
	       )	       
      (score-hint-request (turn-assoc result)
			  *help-last-entries*))

  ;; Irrespective of the entry, we need to inform the workbench of
  ;; the current total score if it has changed since last sent
  (let ((current-score (calculate-score)))
    (cond 
      ((null current-score)
	   (warn 'log-condition:log-warn :tag (list 'null-grading-score)
		 :text "null grading score"))
      ((or (null *last-score*)
	   (> (abs (- current-score *last-score*)) 0.01))
       (setf *last-score* current-score)
       (push `((:action . "set-score") 
	       (:score . ,(round (* 100 current-score)))) 
	     str))))

    (when *debug-help* (format t "Result ~A~%" Result))

    str))


;;; =========================================================================
;;; Generate the initial cmd.
;;; When the initial Commands are parsed from the stream they will be passed
;;; to this code.  Here a cmd struct will be generated and appended to the 
;;; **current-cmd-stack** list.  This struct will encapsulate the initial 
;;; description of the student's action and later wuill be filled in with
;;; the result and any dde-commands that are associated with it.  
;;;
;;; The class will be set using the lookup-commandtype->class code that is 
;;; located in the API.cl file.  The Cmd type will be one of DDE or DDE-Post.  
;;; DDE-Commands will not be generated by this code but may be generrated 
;;; below.  
;;;
;;; NOTE:: At present the time is set as the time that the generate-log-cmd
;;;   command is called.  This is not necessarily the same time as when the
;;;   student initiated the action on the workbench nor will it be the same
;;;   time as appears in the logs.  However the delay is unlikely to be more
;;;   than a second or two at most and will likely be constant.  Therefore 
;;;   the time between the entries and the total time will be close enough
;;;   for our purposes.


(defun iface-generate-log-cmd (Command text)
  "Generate an initial cmd and add it to the set for processing."
  ;; This is the only place where cmd is constructed
  (let ((C (make-cmd :Class (lookup-command->class Command)
		     :Type 'DDE
		     :command command
		     :text text  ;; used only for delete-equation-cmdp
		     )))
    (push C **Current-Cmd-Stack**)
    (setq **Current-CMD** C)
    C))

(defun bottom-out-hintp (Stack)
  ;; Functional equivalent to Andes2 test for
  ;; WWH or NSH hint being a result of 
  ;; previous student request for help.
  (let ((R (car (help-stackc Stack))))
    (when R (member (cmd-command R) 
		    '(next-step-help do-whats-wrong)))))
 

;; ---------------------------------------------------------------------
;; This code handles the task of maintaining the *last-turn-response*
;; struct as well as other efforts.  
;;
;; return-turn -- wrapper for returning turn to workbench
;; saves on last turn and converts given turn to workbench result str
;; returns alist reply.
(defun return-turn (time turn)
  ;; Only update saved turn if new turn is non-null, and not a No-Op-Turn.
  ;; This is defense against a bug when a delayed notification of equation 
  ;; deletion sent just after a next-step-help call. An empty equation 
  ;; results in a null turn, so this will leave the next-step-help reply 
  ;; turn with its responder in place to handle the student's response 
  ;; which comes later.
  (when (and turn (turn-responder turn)
	     (not (eql (turn-type Turn) +no-op-turn+)))
    (setf *last-turn-response* (turn-responder turn)))

  ;; if there is assoc info in the turn, add to reply
  ;; :assoc has the format of an alist.
  ;; Still need to properly logs into "student" and "tutor", Bug #1870
  (when (and turn (turn-assoc turn))
    (alist-warn (turn-assoc turn))
    (push `((:action . "log") (:log . "tutor")
	    (:assoc . ,(mapcar #'(lambda (x) (cons (string (car x))
						 (prin1-to-string (cdr x))))
			       (turn-assoc turn))))
	  (turn-result turn)))
  (turn->WB-Reply time turn))


;;-----------------------------------------------------------------------------
;; For forming reply alist to return to client
;;
(defun turn->WB-Reply (time turn)
  "return reply string to send to wb for given tutor turn"
  ;; null turn is special case
  (when (null turn)
    (return-from turn->WB-Reply "")) 
  ;; non-dialog result string part (status or equation)
  (let ((result (turn-result turn))
	(id (turn-id turn)))
    (when (and (turn-coloring turn) (not (turn-id turn))) 
      (warn "turn->WB-Reply has no id for ~S" turn))

    ;; Update floundering measurement.
    (when time (model-flounder time (turn-coloring turn)))
    
    (case (turn-coloring turn)
      ;;  Predefs have no time slot, don't want them to affect any model.
      (color-green (push `((:action . "modify-object") (:id . ,id)
			   (:mode . "correct")) result))
      (color-red ;; Test that an error interpretation has
		 ;; been logged.  Bug #1935
		 (when (and (notany #'student-log-line result)
			    (not **checking-entries**))
		   (warn 'log-condition:log-warn
			 :tag (list 'incorrect-missing-interp id)
			 :text "Red turn without logging interp."))
		 (push `((:action . "modify-object") (:id . ,id)
			 (:mode . "incorrect")) result))
      )
    
    ;; switch on type to see what message command we have to append to result
    ;; also have to check for an equation result to return
    (case (turn-type turn)
      (Eqn-turn (when (null (turn-text turn)) 
		  (warn "Eqn turn with no equation text!"))
		;; red -> text is hintspec for error msg. Precede with !
		;; but no show-hint command (its implicit in eq rseturns).
		(if (eq (turn-coloring turn) 'color-red)
		    (push `((:action . "show-hint")
			    (:text . ,(turn-text turn))) result)
		    ;; else just return result equation text
		    (push `((:action . "modify-object") (:id . ,id)
			    (:text . ,(turn-text turn))) result)))
      ((nil) 
       (cond 
	 ((turn-text turn) 
	  (push `((:action . "show-hint")
		  (:text . ,(turn-text turn))) result)
	  ;; add followup codes if any
	  (when (turn-menu turn)
	    (if (consp (turn-menu turn)) 
		;; alist of keyword, text pairs
		;; to be presented as a list of choices
		(dolist (choice (turn-menu turn))
		  (push `((:action . "show-hint-link")
			  (:text . ,(cdr choice)) 
			  (:value . ,(symbol-name (car choice))))
			result))
		;; else predefined menu:
		(case (turn-menu turn)
		  (explain-more 
		   (push `((:action . "show-hint-link")
			   (:text . ,*explain-more-text*) 
			   (:value . ,(symbol-name 
				       +explain-more+)))
			 result))
		  (text-input  
		   (push '((:action . "focus-hint-text-box"))
			 result))
		  ;; Add text to modal dialog box.
		  (psm-menu 
		   (push `((:action . "show-hint-link")
			   (:text . "choose a principle")
			   (:value . ,(symbol-name 'major-principle)))
			 result)
		   (push (cons '(:action . "focus-major-principles")
			       (when (turn-text turn) 
				 `((:text . ,(turn-text turn)))))
			 result))
		  (equation-menu 			      
		   (push `((:action . "show-hint-link")
			   (:text . "choose an equation")
			   (:value . ,(symbol-name 'any-principle)))
			 result)
		   (push (cons '(:action . "focus-all-principles")
			       (when (turn-text turn) 
				 `((:text . ,(turn-text turn)))))
			 result))
		  (T  (warn "WB menu code unimplemented: ~A" 
			    (turn-menu turn)))))))
	 ((use-help-button-hint-test time)
	  (push (use-help-button-hint) result))))
      (tcard-turn (warn "Training card turn with ~A." (turn-text turn)))
      (minil-turn (warn "Minilesson card turn with ~A." (turn-text turn)))
      (kcd-turn (push `((:action . "show-hint") 
			(:text . ,(turn-text turn))) result)
		(case (Turn-menu turn)
		  (Explain-More (warn "kcd explain more."))
		  (otherwize (warn "kcd otherwize."))))
      ;; Should have test of unknown type.
      )

    ;; When reply has a link, see if student needs hint to
    ;; Click on link.
    (when nil ;debug print
      (format webserver:*stdout* "Link test ~A and ~A~%" 
	      (mapcar #'reply-has-link result)
	      (model-link-click-test)))
    (when (and (some #'reply-has-link result)
	   (model-link-click-test))
      (push (model-link-click) result))

    ;; Test if student has received some sort of hint
    ;; from the tutor.
    (if (member "show-hint" result
		:key #'(lambda (x) (cdr (assoc :action x)))
		:test #'equal)
	(progn ;(format webserver:*stdout* "***turn with hint~%")
	  (model-hint-turn))
	(progn ;(format webserver:*stdout* "***turn with no hint~%")
	  (model-no-hint-turn time)))
    
    (reverse result)))

(defun reply-has-link (reply)
    (or 
     ;; look for a tutor link.
     (equal (cdr (assoc :action reply)) "show-hint-link")
     ;; Look for link in a hint, ignoring any help buttons.
     (and (equal (cdr (assoc :action reply)) "show-hint")
	  (search "<a href=" (cdr (assoc :text reply)))
	  (not (search *help-button-action* (cdr (assoc :text reply)))))))


;;; ==========================================================================
;;; Update with results.
;;; Once we heave generated a cmd and executed the command to obtain the
;;; results then we need to add the result information to the CMD before
;;; the tests are run.  The result will be one of T, NIL, or a tutor-turn.
;;; the result will be parsed based upon the command type and other values
;;; to form a cmdresult struct The result will be returned once it has been
;;; generated and then will be stored in the CMD.  
;;;
;;; This is somewhat challenging as the Exiting help system code does not 
;;; form the turns in classes but merely packs a string and relies upon the
;;; workbench to decode it appropriately.  This code will attempt to form the
;;; appropriate structs but it may seem somewhat odd at first.
;;;
;;; The code in this section is based upon turn->wb-reply above.
;;;
;;; In order to work this code has to trap the following cases:
;;;  DDE-Failed cases where the call threw an error or other 
;;;    problems arose.
;;;  T cases where the call simply returned a value but not a tutor
;;;    turn.  This is treated as if it was a color-green turn.
;;;  Nil Cases where the call simply returned a value but not a 
;;;    tutor turn.  This is treated as a color-red turn.
;;;  Tutor turn where a turn has been provded and we need to tease
;;;    out the type and meaning of the turn.  
;;
;;; NOTE:: If the CMD is a dde-post then this will not be called as 
;;;  dde-posts do not get answers that we concern ourselves with.  
;;;
;;; NOTE:: DDE-Failed values show up in the logs when a dde command has 
;;;  timed out (taken longer than 30s to return) or an error was thrown.
;;;  If this code gets stuck in a loop and times out then there is nothing
;;;  that can be done to track that here, this being the help system.  
;;;  However, that kind of behavior is unlikely and so we won't worry 
;;;  about it much.  
;;;
;;; NOTE:: It may seem odd to be writing this code to translate tutor 
;;;  turns into a different structural format within the help system.  
;;;  Especially since that format was origianlly written to reverse
;;;  engineer the meaning of the turn contents from the logs.  However
;;;  I am persisting in this course so that we can use the existing 
;;;  test code and make easy comparisons between the logs and the
;;;  runtime results.  In the future when time permits it would be 
;;;  nice to alter the structure of the tests and the files but time
;;;  does not permit it now.  

;;; 
;;; The result-type/command matching is located in API.  That code will
;;; be used here.  

;;; If it is one of T, Nil, or a turn then we need
;;; to determine what type of result the code is expecting and form it 
;;; appropriately.  The sections below contain the code necessary to do
;;; that grouped by result type, as well as utility code.
(defun iface-Add-cmdresult-to-cmd (CMD Result)
  "Generate a cmdresult appropriate for the cmd from Result and store it."
  (case (lookup-commandclass->resultclass (cmd-class Cmd))
    (Status-Return-Val (iface-add-srv-cmdresult Cmd Result))
    (Eqn-Result (iface-add-eqr-cmdresult Cmd Result))
    (Hint-Return-Val (iface-add-hrv-cmdresult Cmd Result))
    (Ignore (iface-add-ignore-cmdresult Cmd Result))))


;;; --------------------------------------------------------------------------
;;; Status-return-val
;;; Status return vals are used for entries (Eqn and non-eqn) and State 
;;; Commands.  They contain coloring, indicating success or failure, and an
;;; optional errors list (currently unused as of this writing) as well as the
;;; command and value fields.  
;;;
;;; For status return vals T and Nil will be treated as otherwise empty 
;;; color-green and color-red turns respectively.  Beyond that a turn will
;;; be split up according to its type.  
(defun iface-add-srv-cmdresult (Cmd Result)
  "Add a status return val result."
  (cond
   ((null Result) (iface-add-srv-int Cmd :Coloring 'Red))
   ((not (turn-p Result)) (iface-add-srv-int Cmd :Coloring 'Green))
   (t (iface-add-srv-turn Cmd Result))))


;;; This is an internal function that takes the individual values for
;;; the status-return-val and the cmdresult and sets them appropriately
;;; before updating the cmd.  
(defun iface-add-srv-int (Cmd &key Coloring)
  (iface-set-cmdresult Cmd :Value (make-status-return-val 
				   :Coloring Coloring)))

;;; IF we are adding a status return val based upon a tutor turn then it
;;; is necessary to pull the individual elements from the turn itself 
;;; before passing them off to iface-srv-int.  This code borrows heavily
;;; from turn->wb-reply above.
;;;
;;; We begin by setting the coloring of the val followed by the command
;;; and values.
(defun iface-add-srv-turn (Cmd Result)
  (let ((Val (make-status-return-val)))
    (iface-set-srv-turn-color Val Result)
    (iface-set-DDR-turn-command Val Result)

    ;; Generate a cmdresult and set the necessary vals.
    (iface-set-cmdresult 
     Cmd :Value Val 
     :Assoc (alist-warn (turn-assoc Result)))))


;;; There are three turn-colorings: Red, Green, and no-op.
;;; Of these, the Red and green are used most of the time.  
;;; No-op is intended to indicate no change.
;;; although it is not used except in cases where no command or
;;; value is sent.  However the turn->wb-reply code makes it 
;;; possible to send both so I will include it here.
(defun iface-set-srv-turn-color (Val Result)
  (setf (Status-return-val-coloring Val)
    (case (turn-coloring Result)
      (Color-Green 'Green)
      (Color-Red 'Red)
      (no-op Nil))))


;;; ----------------------------------------------------------
;;; Eqn-Results
;;; The Equation results contain the standard dde-result values
;;; as well as the Equation, an optional equation string that 
;;; will be inserted into an equation field.  The equation turns
;;; will only be generated by the algebra commands and, even in
;;; the event of an error an eqn-result will still be the value
;;; returned.  This code will generate and return an eqn-result
;;; setting the values as appropriate.
;;;
;;; So far as the code suggests we should always get a tutor turn
;;; when the code is executing an algebraic function.  This code
;;; will therefore throw an error if a non-tutor-turn is encountered.
;;;
;;; Any eqn turn must have turn-text of some kind.  If none is 
;;; encountered then an error will be thrown as well.
;;;
;;; NOTE:: Unlike the other code this code does not make use of the
;;;   dde-command code below.
(defun iface-add-eqr-cmdresult (Cmd Result)
  (if (not (turn-p Result)) 
      (error " Non-turn result supplied to Eqn-turn.")
    (if (null (turn-text Result))
	(error "Eqn-turn with no equation text.")
      ;; if the turn is red then the text is a hint to be 
      ;; shown.  If not then it is a correct equation to 
      ;; be shown.
      (if (eq (turn-coloring Result) +color-red+)
	  (iface-add-eqr-r-cmdresult Cmd Result)
	(iface-add-eqr-g-cmdresult Cmd Result)))))


(defun iface-add-eqr-r-cmdresult (Cmd Result)
  "Add an incorrect eqn-result."
  (iface-set-cmdresult
   Cmd
   :Assoc (alist-warn (turn-assoc Result))))


(defun iface-add-eqr-g-cmdresult (Cmd Result)
  "Add a correct eqn-result."
  (iface-set-cmdresult
   Cmd
   :Assoc (alist-warn (turn-assoc Result))))


;;; -----------------------------------------------------------------------
;;; Hint-Return-Val
;;; Hint return values are returned by the hint commands.  They contain no
;;; coloring nor do they contain any equations they are assumed to always
;;; be just returning hint text.
;;;
;;; In this case it is also assumed that the result will be a tutor turn 
;;; and not a t or nil.  If it is a turn then we are only interested in 
;;; setting the command and value fields so I will make use of the ddr 
;;; code below.

(defun iface-add-hrv-cmdresult (Cmd Result)
  (let (Assoc (Val (make-hint-return-val)))

    ;; Handle the turns.
    (when (turn-p Result) 
      (iface-set-ddr-turn-command Val Result)
      (setq Assoc (turn-assoc Result)))

    (iface-set-cmdresult
     Cmd
     :Value Val
     :Assoc (alist-warn Assoc))))

   
;;; ---------------------------------------------------------------------
;;; Ignore commands.
;;; Ignore cmdresults are largely empty and contain the command in the 
;;; result solely as a dummy measure.
(defun iface-add-ignore-cmdresult (Cmd Result)
  "Add the ignore cmdresult."
  (if (not (turn-p Result))
      (iface-set-cmdresult Cmd :Value Result)

    (iface-set-cmdresult 
     Cmd
     :Value Result ;; will need to be changed later.
     :Assoc (alist-warn (turn-assoc Result)))))


;;; ---------------------------------------------------------------------
;;; This is a general utility function that we will use to create the 
;;; cmdresult struct and then to append that information to the cmd.
;;; this code handles the general tasks of setting the Linenum
;;; time and other elements and relies upon keyword arguments to 
;;; set the class, value, assocs, and commands.
(defun iface-set-cmdresult (Cmd &key (Class 'DDE-Result) Value Assoc)
  (setf (cmd-result Cmd)
	;; this is the only place where cmdresult is created.
	(make-cmdresult
	 :Class Class
	 :Value Value
	 :Assoc Assoc)))



;;; -----------------------------------------------------------------------
;;; DDE Result commands.
;;; The command and value settings do not vary beteen status-return-vals
;;; eqn-results and hint-return-vals so this code here will be called by
;;; all of the code above.  
;;;
;;; Setting the command and values field of a status return value depends
;;; on the coloring, type, text, and menu sections of a tutor-turn.  this
;;; code will generate those and is drawn heavily from turn->wb-reply
;;; that is defined above. 
;;;
;;;
;;; NOTE:: The eqn-turn should not be called here as only the algebra commands
;;;  return eqn-turns and those will be handled by the Eqn-Result code.

(defun iface-set-DDR-turn-command (Val Result)
  "Set the command and value fields."
  (case (turn-type Result)
    (Minil-Turn (iface-set-ddr-Minil-turn-c Val Result))
    (TCard-turn (iface-set-ddr-tcard-turn-c Val Result))
    (KCD-turn (iface-set-ddr-kcd-turn-c Val Result))
    ((nil) (iface-set-ddr-dialog-turn-c Val Result))
    (Eqn-turn (error "Incorrect-turn-type-supplied."))))
    

;;; Minilesson turns are used to display help info in the workbench 
;;; browser.  This code 
(defun iface-set-ddr-minil-turn-c (Val Result)
  "Set the commands for a minilesson turn."
  (when (null (turn-text Result))
    (error "Minilesson turn with no filename text"))
  (setf (dde-result-command Val) +show-lesson+) 
  (setf (dde-result-value Val) (turn-text Result)))


;;; Training card turns are used to display info to the users via
;;; Microsoft's Training-card system. 
(defun iface-set-ddr-tcard-turn-c (Val Result)
  (when (null (turn-text Result)) 
    (error "Training card turn with no card-id text"))
  (setf (dde-result-command Val) +training-card+)
  (setf (dde-result-value Val) (turn-text Result)))


;;; KCD turns are dialog turns with show-hint commands and one of
;;; a series of menus taken directly from the turn.
(defun iface-set-ddr-kcd-turn-c (Val Result)
  (setf (dde-result-command Val) +show-hint+)
  (setf (dde-result-value Val) (turn-text Result))
  (setf (dde-result-menu Val) (turn-menu Result)))


;;; Dialog turns are used for the hint process and will be used to 
;;; handle the next-step-help and why-wrong-help.  
;;;
;;; NOTE:: This code assumes that it is being passed a tutor-turn.
(defun iface-set-ddr-dialog-turn-c (Val Result)
  (when (turn-text Result)
    ;; Set the command if a message exists.
    (setf (dde-result-command Val) +show-hint+)
    ;; Set the message.
    (setf (dde-result-value Val) (turn-text Result))
    ;; Add the menu if one exists.
    (setf (dde-result-menu Val) (turn-menu Result))))


    ;; Eqn turns can have an equation to display or an error
    ;; hint to show depending upon their coloring.  If the 
    ;; text is nil the throw an error.  If red then set the
    ;; command to be show-hint and the hint text to be the
    ;; text and menu.  Else it is an equation to be shown
    ;; and that should not appear here so throw an error.
    ;; It is possible that this won't ever be exercized but 
    ;; there it is.  
    ;;(Eqn-turn 
    ;; (cond 
    ;; ((null (turn-text Result))
    ;; (error "Eqn-turn with no -equation text"))
      


;;; ---------------------------------------------------------------------------
;;; Commands submitted to the wb by the Help System.
;;; May need to move to interface.cl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Alternate command dispatcher used to implement answer-only-mode: 
;; Answers are checked, but non-answer entries all turn black. 
;; Solver, Help requests rejected, but for unsolicited help on answers.
;;
;; Note answer-only-mode is different from the final-answer-only flag:
;;    final-answer-only is used for graph reading problems. Help does 
;;         not prompt entries, grading does not require them, but they 
;;         DO get red/green feedback according to graph if made
;;    answer-only-mode is for experimenting. It disables help system
;;         apart from answers.  This could be set on any problem. 
;;         Grading is not affected, so grade will be computed just 
;;         as without this flag and may be low. 
;;
;; We might want to add some facility to configure answer-only-mode 
;; outside of problem via flag setting or parameter in problem set to
;; be communicated to help system.
;;
(defun answer-only-dispatcher (cmd args)
"dispatch a command in answer-only mode"
   ; switch on type of command, from API.cl
   (case (lookup-command->class cmd)
      ;; all following cmd classes can just be dispatched as usual
      ((State Answer Delete Control) (apply cmd args))
      ;; Entries should be left black
      ((noneq-entry eq-entry) (make-no-color-turn 
			       :id (StudentEntry-id (car args))))
      ;; Help requests: assume explain more could only be followup to allowed 
      ;; help -- presumably unsolicited help for wrong answers -- so process it. 
      ;; Reject all others help requests.
      (Help (case cmd 
                  (explain-more (apply cmd args))
                  (otherwise (make-end-dialog-turn 
			      "Help is not available on this problem."))))
      ;; Algebra gets null answer
      (Algebra (make-eqn-failure-turn 
		(car args)
		"The Andes calculator is not available on this problem."))
      ;; anything else: empty string should function as null return value.
      (otherwise (warn "unclassifed api command: ~A~%" cmd)
                  "")))
