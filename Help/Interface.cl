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
;;; the api command with arguments by calling it with the 
;;; safe-apply function.  
;;;
;;; The result of the safe-apply is assumed to be an error trap, nil,
;;; or a tutor-turn of some kind.  That turn will be used to form the
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

;;; This parameter stores the last api call made.  This is used for internal 
;;; logging as we do not have a strict layering heiarchy for keeping track 
;;;; of the state.
(defparameter **last-api-call** () "The last call made to the help system, set by dispatch-stream-event.")

;;; An alternate command interpreter function.  If this is parameter has a 
;;; value then it will be called with the Command and arguments as its 
;;; arguments in lieu of dispatching the command directly.  This allows us 
;;; to alter how commands are 
;;; evaluated for specialized situations such as the walk-prompt mode.
(defparameter **Alternate-Command-Interpreter** () 
  "An alternate command interpreter func for later use.")

;;; As the CMDs are generated they will be added to the **current-cmd-stack** 
;;; list until an open-problem dde is encountered.  At which point the list 
;;; will be  cleared but for the open-problem cmd. 
(defparameter **current-cmd-stack** Nil "The current CMDs.")

;;; The most recent cmd is stored here in addition to the cmd stack to 
;;; facilitate direct access.
(defparameter **current-cmd** Nil "The current Cmd.")

;;; *last-tutor-turn* -- record of the last tutor turn made
;;; This is used in handle-student-response to deal with the student's 
;;; continuation of ongoing diologues.  It is set bu return-turn below.
(defvar *last-tutor-turn*)

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
;;;; The main function here will handle the maintenance of the cmd list as well 
;;;; as the storage of the **last-api-call** value.  It will also execute the
;;;; code with the **alternate-command-interpreter** if that is set.




;;; ----------------------------------------------------------------------------
;;; Central API Call.
;;; The execute-andes-command function is the central entry-point to the command
;;; api from the stream.  When the user initiates an action on the workbench an
;;; appropriate command will be placed on the tcp-ip stream.  The server code in
;;; andes2-main will parse the command off of the stream and pass it to the 
;;; execute-andes-command function below.
;;;
;;; This function will begin by logging the command in **last-api-call**.  It 
;;; will then generate a new cmd for the action and execute the command directly
;;; using a safe-apply or via the **Alternate-Command-Interpreter** if there is
;;; one again using a safe apply.  The result from that execution will be one of
;;; t, NIL, :Error or a tutor turn.  
;;;
;;; If the result is a tutor-turn it will be translated into a string for return 
;;; to the workbench again within a safe-apply.  This is done because there are 
;;; some errors that can be signalled within this process and it is necessary
;;; to trap them before the cmdresult is formed or the results are tested.  
;;;
;;; Once that is done the result or the Str with be parsed into a cmdresult and
;;; appended to the cmd before the autograding tests are run.  The result value
;;; will be used unless it is a tutor turn and an error was thrown by return-turn
;;; when it was being processed.  In that case the :Error symbol will be used.
;;;
;;; Once the cmdresult has been appended to the cmd then the aurograding tests
;;; will be run and any commands to update the grades will be sent.  At that 
;;; point the result (or string if the result is a tutor turn) will be returned
;;; to the workbench.  
;;;
;;; NOTE:: I am carrying out the string parsing before handling the cmdresult
;;;   in order to trap errors in the same way that they were trapped by the 
;;;   previous versions of Andes that ran the return-turn code within safe-apply.
;;;
;;; NOTE:: if they remain unused the **last-api-call** and 
;;;      **Alternate-Command-Interpreter** code will be removed from
;;;      this code at some point.
;;;

(defun execute-andes-command (Command Arguments DDE)
  "Execute the api call with the command and arguments."
  ;; Set the last api call to be this call.
  (setq **last-api-call** (cons Command Arguments))
  (let* (Tmp (NewCmd (iface-generate-log-cmd DDE Command Arguments))
	 (Result (iface-internal-exec-andes-command Command Arguments))
	 (Str (if (turn-p Result) (safe-apply 'return-turn (list Result)))))

    ;; Once the command has been executed and any result parsed then we
    ;; need to add the cmdresult to the current cmd iff the cmd was a 
    ;; DDE (and will therefore get a reply.  This occurs here.  The pprint
    ;; is for debugging only.  
    (when DDE 
      (setq Tmp (safe-apply 
		 'iface-add-cmdresult-to-cmd 
		 (list NewCMD (if (equalp Str :Error) Str Result))))
      (if (equalp Tmp :Error) (pprint "Error in Cmdresult addition.")))
    
    ;; This is the primary call to autograding.  It will handle the 
    ;; execution of any tests and the updating of results.  It calls 
    ;; the code in AutoCalc.cl and will handle the send-fbd command 
    ;; as necessary.
    ;;
    ;; The safe-apply should allow this to fail without throwing
    ;; an unreasonable error.
    (setq Tmp (safe-apply 'iface-handle-Statistics (list NewCmd)))
    (if (equalp Tmp :Error) (pprint "Error in Statistics."))

    (format *debug-help* "Result ~A~%" Result)

    (if Str Str Result)))



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


(defun iface-generate-log-cmd (DDE Command Arguments)
  "Generate an initial cmd and add it to the set for processing."
  (let ((C (make-cmd :Class (lookup-command->class Command)
		     :Type (if DDE 'DDE 'DDE-POST)
		     :Time (get-current-htime)
		     :Call (cons Command Arguments))))
    (push C **Current-Cmd-Stack**)
    (setq **Current-CMD** C)
    C))



;;; =======================================================================
;;; Internal Execution.
;;; Execute the supplied Andes command with the specified arguments.  
;;; If an alternate command interpreter has been supplied then use it
;;; if not then execute the command itself.  Return the result to 
;;; execute andes-command.
(defun iface-internal-exec-andes-command (Command Arguments)
  "Call the command itself with in a safe-apply and return the results."
  (if **Alternate-command-interpreter**
      (safe-apply **Alternate-command-Interpreter** 
		  (list Command Arguments))
      (safe-apply command arguments)))


;;; ===================================================================
;;; Stats
;;; The code in this section interfaces with the autocalc code.  It will
;;; handle the maintenance of the cmd list (clearing it on new problems)
;;; and the scores.  When a command is completed then this code will be 
;;; called.  
;;;
;;; If the cmd is a close-problem or read-problem-info cmd then we need
;;; to call the special-case code to store and reset the scores as well
;;; as taking care of any routine maintenance.  If not then we will 
;;; update the current scores and report the current total score to 
;;; the fbd.
;;;
;;; The Preexisting scores will be loaded when the student opens a new 
;;; problem, closes an old-problem or a read-student-info cmd is sent
;;; Note that this will not allow the system to make use of the time 
;;; that occurs between the initialization of Andes and the initial
;;; read-student-info cmd.  However this should not be much of an issue
;;; as little if anything can occur save a long delay.
(defun iface-handle-Statistics (NewCmd)
  "Handle the autocalc code for computing grades."
  ;; Handle the close-problem, read-problem and other
  ;; cases by updating and resetting the stats as 
  ;; necessary.
  (cond ((read-problem-info-cmdp NewCmd)
	 (iface-handle-stats-read NewCmd))
	((close-problem-cmdp NewCmd)
	 (iface-handle-stats-close NewCmd))
	((read-student-info-cmdp NewCmd)
	 (iface-handle-stats-student))
	(t (update-runtime-testset-scores)))
  
  ;; Irrespective of the entry we need to inform the workbench of
  ;; the current total score if it has changed since last sent
  (let ((current-score (get-current-runtime-total-score)))
    (when (not (equal current-score *last-score*))
      (setf *last-score* current-score)
      ;; needs to be formatted for json-rpc
      (format Nil "set-score ~a" current-score)
      )))

;;; AW: now we no longer load and save problem statistics in the
;;; student history file. Instead, the workbench will fetch the 
;;; few persistent score statistics from us via (get-stats 'persist),
;;; save them in the problem solution file, and restore them via
;;; an API call on problem open. Non-score statistics will not be
;;; saved (and could be dropped entirely from the test list).
;;; Following code is therefore mostly obsolete; we keep it in
;;; case we ever go back to the history file method. 
;;; Not clear if the stat updates on student/read/close events
;;; are still necessary. Time stat might change on these, but this
;;; is not part of score so is no longer accurately tracked. Not
;;; sure if any other stat could change on these events, or if
;;; any update is necessary for proper initialization.


;;; When NewCmd is a close-problem then the system will update the 
;;; current scores one last time, store the stats within the 
;;; Student.dat file.  Once that is done the stats will be reset
;;; in preparation for the next problem-instance.  
;;;
;;; This is being done so that any dde's that the student sends or
;;; time that they spend between problems will not effect the scores
;;; that they recieve on each problem-instance.
(defun iface-handle-stats-close (NewCmd)
  (update-runtime-testset-scores)  ; AW: maybe not still needed
  ; AW: no longer save stats in history file
  ; (store-runtime-test-stats 
  ;   (read-from-string (second (cmd-call NewCmd))))
  (setq **Current-Cmd-Stack** Nil)
  (setq **Current-Cmd** Nil)
  (reset-runtime-testset-scores)
  ; AW: no longer load stats from history file
  ; (load-stored-runtime-test-stats Nil)
)


;;; On a read-problem-info cmd the system will store the current 
;;; stats in the student.dat file associating them with nil to
;;; indicate no problem instance.  Following that the stats will
;;; be reset and updated to reflect the new problem instance.
(defun iface-handle-stats-read (NewCmd)
  ; AW -- no longer store stats in history file
  ; (store-runtime-test-stats Nil)
  (setq **Current-Cmd-Stack** (list NewCmd))
  (setq **current-cmd** NewCmd)
  (reset-runtime-testset-scores)
  ; AW: no longer load stats from history file
  ;(load-stored-runtime-test-stats 
  ; (read-from-string (second (cmd-call NewCmd))))
  (update-runtime-testset-scores)) ; AW: maybe not still needed


;;; On a read-student-info command we need to load in the 
;;; studentfile and to reset the stored stats variable.  
(defun iface-handle-stats-student ()
  (update-runtime-testset-scores))  ; AW: maybe not still needed


;; ---------------------------------------------------------------------
;; This code handles the task of maintianing the *last-tutor-turn*
;; struct as well as other efforts.  
;;
;; return-turn -- wrapper for returning turn to workbench
;; saves on last turn and converts given turn to workbench result str
(defun return-turn (turn)
  ;; Only update saved turn if new turn is non-null, and not a No-Op-Turn.
  ;; This is defense against a bug when a delayed notification of equation 
  ;; deletion sent just after a next-step-help call. An empty equation 
  ;; results in a null turn, so this will leave the next-step-help reply 
  ;; turn with its responder in place to handle the student's response 
  ;; which comes later.
  (when (and turn (not (equalp (turn-type Turn) **No-Op-Turn**)))
    (setf *last-tutor-turn* turn))
  ;; If there are any commands in the turn then send an async command for
  ;; each one back to the workbench before the final result is placed on
  ;; the stream.  This allows us to wrap multiple workbench actions into 
  ;; the result of a single command and is used primarily for opening and
  ;; closing separate browser windows.
  (list   
   (when (and turn (turn-commands Turn))
     (turn-commands Turn))
   ;; if there is assoc info in the turn, send async command to record it in
   ;; the workbench log. 
   (when (and turn (turn-assoc turn))
    (strcat "assoc " (write-to-string (turn-assoc turn)
				       :escape t ;make it lisp-readable
				       :pretty NIL)))   ;turn off line breaks 
  (turn->WB-Reply turn)))    


;;-----------------------------------------------------------------------------
;; For forming reply strings returned to workbench
;;
;; Note there are some problems with this routine given Andes1 workbench:
;; you can't actually construct the right string from a turn in a context-
;; independent manner; what to send depends on what the WB is expecting
;; in response to which call, either a status return with piggybacked cmd
;; or a hint string. FIXED: Andes2 workbench should now understand 
;; !show-hint command even where bare hint text was formerly expected, so we 
;; can safely use this in all cases in context-independent manner.
(defun turn->WB-Reply (turn)
"return reply string to send to wb for given tutor turn"
  ; null turn is special case
  (when (null turn)
    (return-from turn->WB-Reply "")) 

  (let (result 	    ; non-dialog result string part (status or equation)
        cmd         ; command string part (show-hint, show-lesson, delete)
	responses)  ; codes menu of responses to offer student
    (case (turn-coloring turn)
      (color-green  (setf result "T"))
      (color-red    (setf result ; may append list of slots to flag
		        (format NIL "NIL~{;~A~}" 
			    (decide-slot-flags turn))))
      ((no-op NIL)  (setf result ""))
      ; delete turn not implemented yet
      ; for delete response, turn text must be entry ID
      (delete (setf result "")
	      (when (null (turn-text turn))
	         (error "Delete turn should contain entry id in turn-text"))
	      (setf cmd (format NIL "!delete-entry ~A" (turn-text turn))))
    )
    ; switch on type to see what message commnd we have to append to result
    ; also have to check for an equation result to return
    (case (turn-type turn)
      (Eqn-turn (when (null (turn-text turn)) 
                    (error "Eqn turn with no equation text!"))
		; red -> text is hintspec for error msg. Precede with !
		; but no show-hint command (its implicit in eq rseturns).
		(if (eq (turn-coloring turn) 'color-red)
		    (setf result (format NIL "!~A" (wb-text turn)))
		 ; else just return result equation text
                 (setf result (turn-text turn))))
      (dialog-turn (when (turn-text turn) 
                    (setf cmd (format NIL "!show-hint ~A" (wb-text turn)))
		    ; add followup codes if any
		    (when (turn-menu turn)
		     (if (consp (turn-menu turn)) ; list => menu spec
		       ; for now, only one level permitted.
		       ; format list with vbar delimiters
		       (setf responses (format NIL "|~{~A|~}" (turn-menu turn)))
		      ; else predefined menu:
		      (case (turn-menu turn)
			; predefined menus have single-letter codes:
		        (explain-more      (setf responses "e"))
			(quant-menu        (setf responses "Q"))
			(psm-menu          (setf responses "P"))
			(equation-menu     (setf responses "E"))
		        (T  (warn "WB menu code unimplemented: ~A" 
			          (turn-menu turn))))))
		     ; always append tilde, even if response list is empty, so wb 
		     ; can split at last tilde even if msg happens to contain tildes
		     (setf cmd (concatenate 'string cmd "~" responses))))
      (tcard-turn (when (null (turn-text turn)) 
                    (error "Training card turn with no card-id text"))
		  (setf cmd (format NIL "!training-card ~A" (turn-text turn))))
      (minil-turn (when (null (turn-text turn)) 
                    (error "Minilesson turn with no filename text"))
		  (setf cmd (format NIL "!show-lesson ~A" (turn-text turn))))
      (end-dialog ; (setf cmd "!end-dialog") ; not implemented in WB yet
		  (setf cmd nil)
                  (setf result ""))
      (kcd-turn (case (Turn-menu turn)
		  (Free-Text (setf cmd (format NIL "!show-hint ~A~~?" (wb-text turn))))
		  (Explain-More (setf cmd (format NIL "!show-hint ~A~~e" (wb-text turn))))
		  (otherwize (setf cmd (format NIL "!show-hint ~A" (wb-text turn))))))
      ;; Format the stat turn as a list of values for the workbench.  
      ;; this code will set the return value ignoring any coloring that
      ;; may have occurred.  No cmd will be set.
      (stat-turn (setf result (format NIL "~{~a ~a ~a~}~{~{;~a ~a ~a~}~}" 
			 (car (turn-value turn)) (cdr (turn-value turn)))))
      )
    ;; assemble final result string from parts and return it
    (concatenate 'string result cmd)))
  
(defun decide-slot-flags (turn)
"return effective list of slots to flag for this turn, NIL if none"
  ; in cases in which we have a slot to flag, apply random
  ; process to use them with *flag-frequency* probability
  (when (turn-flag-slots turn)
      (turn-flag-slots turn)))

(defun wb-text (turn)
"return adjust text from turn by replacing any disallowed newlines with spaces"
  (substitute #\Space #\Newline (turn-text turn)))












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
;;;  Errors will be trapped by safe-apply and the result value in that 
;;;  case will be the symbol :Error.  In that case thos code will record
;;;  a DDE-failed and go with that.  
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

;;; There are two essential "root" cases that need to be handled.  If
;;; the code returns :Error or any other value.  The result will be 
;;; :Error if the API function crashed and threw an error that was 
;;; caught by safe-apply.  In that case a dde-failed result will be 
;;; returned.  If not then we must select an appropriate result based 
;;; upon the type of the command.  
;;; 
;;; The result-type/command matching is located in API.  That code will
;;; be used here.  

;;; If the result is :Error then we can form a cmdresult and append it 
;;; directly to the cmd.  If it is one of T, Nil, or a turn then we need
;;; to determine what type of result the code is expecting and form it 
;;; appropriately.  The sections below contain the code necessary to do
;;; that grouped by result type, as well as utility code.
(defun iface-Add-cmdresult-to-cmd (CMD Result)
  "Generate a cmdresult appropriate for the cmd from Result and store it."
  (if (equalp Result :Error) 
      (iface-set-cmdresult Cmd :Class 'DDE-Failed :Value (Cmd-Call Cmd))
    (case (lookup-commandclass->resultclass (cmd-class Cmd))
      (Status-Return-Val (iface-add-srv-cmdresult Cmd Result))
      (Eqn-Result (iface-add-eqr-cmdresult Cmd Result))
      (Hint-Return-Val (iface-add-hrv-cmdresult Cmd Result))
      (stat-result (iface-add-stat-cmdresult Cmd Result))
      (Ignore (iface-add-ignore-cmdresult Cmd Result)))))


;;; --------------------------------------------------------------------------
;;; Status-return-val
;;; Status return vals are used for entries (Eqn and non-eqn) and State 
;;; Commands.  They contain coloring, indicating success or failue, and an
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
(defun iface-add-srv-int (Cmd &key Command Value Coloring Errors Assoc Commands)
  (iface-set-cmdresult 
   Cmd
   :Value (make-status-return-val
	   :Command Command
	   :Value Value
	   :Coloring Coloring
	   :Errors Errors)
   :Assoc Assoc
   :Commands Commands))


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
     :Assoc (turn-assoc Result)
     :Commands (turn-commands Result))))


;;; There are four possible turn-colorings: Red, Green, no-op
;;; and delete-entry.  Of these, the Red and green are used 
;;; most of the time.  No-op is intended to indicate no change.
;;; although it is not used except in cases where no command or
;;; value is sent.  However the turn->wb-reply code makes it 
;;; possible to send both so I will include it here.
(defun iface-set-srv-turn-color (Val Result)
  (setf (Status-return-val-coloring Val)
    (case (turn-coloring Result)
      (Color-Green 'Green)
      (Color-Red 'Red)
      (no-op Nil)
      (delete Nil))))


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
      (if (eq (turn-coloring Result) **Color-Red**)
	  (iface-add-eqr-r-cmdresult Cmd Result)
	(iface-add-eqr-g-cmdresult Cmd Result)))))


(defun iface-add-eqr-r-cmdresult (Cmd Result)
  "Add an incorrect eqn-result."
  (iface-set-cmdresult
   Cmd
   :Value (make-eqn-result
	   :Command **Show-hint**
	   :Value (wb-text Result)
	   :Menu (turn-menu Result))
   :Assoc (turn-assoc Result)
   :Commands (turn-commands Result)))


(defun iface-add-eqr-g-cmdresult (Cmd Result)
  "Add a correct eqn-result."
  (iface-set-cmdresult
   Cmd
   :Value (make-eqn-result
	   :Command **Show-hint**
	   :Value (wb-text Result)
	   :Menu (turn-menu Result))
   :Assoc (turn-assoc Result)
   :Commands (turn-commands Result)))


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
  (let (Assoc Commands (Val (make-hint-return-val)))

    ;; Handle the turns.
    (when (turn-p Result) 
      (iface-set-ddr-turn-command Val Result)
      (setq Assoc (turn-assoc Result))
      (setq Commands (turn-commands Result)))

    (iface-set-cmdresult
     Cmd
     :Value Val
     :Assoc Assoc
     :Commands Commands)))


;;; ---------------------------------------------------------------------
;;; Stat-Results
;;; Stat result values are retuned by the autocalc grading code.  They
;;; are used to convey lists of statistical values to the workbench from
;;; the help system.  The code here will turn the appropriate turn into
;;; a cmdresult.  The assumption is that it will be a turn.
(defun iface-add-stat-cmdresult (Cmd Result)
  (when (not (turn-p Result))
    (error "non-turn passed to iface-add-stat-cmdresult."))
  (iface-set-cmdresult
   Cmd
   :Value (make-stat-result :Value (turn-value Result))
   :Assoc (turn-assoc Result)
   :Commands (turn-commands Result)))
   
;;; ---------------------------------------------------------------------
;;; Ignore commands.
;;; Ignore cmdresults are largely empty and contain the command in the 
;;; result soley as a dummy measure.
(defun iface-add-ignore-cmdresult (Cmd Result)
  "Add the ignore cmdresult."
  (if (not (turn-p Result))
      (iface-set-cmdresult Cmd :Value Result)
    (iface-set-cmdresult 
     Cmd
     :Value Result ;; will need to be changed later.
     :Assoc (turn-assoc Result)
     :Commands (turn-commands Result))))


;;; ---------------------------------------------------------------------
;;; This is a general utility function that we will use to create the 
;;; cmdresult struct and then to append that information to the cmd.
;;; this code handles the general tasks of setting the Linenum
;;; time and other elements and relies upon keyword arguments to 
;;; set the class, value, assocs, and commands.
(defun iface-set-cmdresult (Cmd &key (Class 'DDE-Result) Value Assoc Commands)
  (setf (cmd-result Cmd)
    (make-cmdresult
     :Class Class
     :Time (get-current-htime)
     :Value Value
     :Assoc Assoc
     :Commands Commands)))



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
;;; This code is complicated by the fact that a coloring of **delete-entry** 
;;; also sets the command and value.  
;;;
;;; NOTE:: The eqn-turn should not be called here as only the algebra commands
;;;  return eqn-turns and those will be handled by the Eqn-Result code.

(defun iface-set-DDR-turn-command (Val Result)
  "Set the command and value fields."
  (if (equalp (turn-coloring Result) **Delete-Entry**)
      (iface-set-ddr-delete-turn-c Val Result)
    (case (turn-type Result)
      (Minil-Turn (iface-set-ddr-Minil-turn-c Val Result))
      (TCard-turn (iface-set-ddr-tcard-turn-c Val Result))
      (KCD-turn (iface-set-ddr-kcd-turn-c Val Result))
      (End-Dialog (iface-set-ddr-end-turn-c Val Result))
      (Dialog-Turn (iface-set-ddr-dialog-turn-c Val Result))
      (Eqn-turn (error "Incorrect-turn-type-supplied.")))))

;; Andes has placed a note in the turn->wb-reply code indicating that
;; the delete turn is not complete.  I am not quite sure what to make
;; of this but for now I will assume that it is being used and set 
;; the values accordingly.  
;;
;; NOTE:: I do not believe that this code will be exercized but I am 
;;  covering the bases.
(defun iface-set-ddr-delete-turn-c (Val Result)
  (setf (dde-result-command Val) **delete-entry**)
  (setf (dde-result-value Val) (turn-text Result)))
    

;;; Minilesson turns are used to display help info in the workbench 
;;; browser.  This code 
(defun iface-set-ddr-minil-turn-c (Val Result)
  "Set the commands for a minilesson turn."
  (when (null (turn-text Result))
    (error "Minilesson turn with no filename text"))
  (setf (dde-result-command Val) **Show-Lesson**) 
  (setf (dde-result-value Val) (turn-text Result)))


;;; Training card turns are used to display info to the users via
;;; Microsoft's Training-card system. 
(defun iface-set-ddr-tcard-turn-c (Val Result)
  (when (null (turn-text Result)) 
    (error "Training card turn with no card-id text"))
  (setf (dde-result-command Val) **Training-Card**)
  (setf (dde-result-value Val) (turn-text Result)))


;;; KCD turns are dialog turns with show-hint commands and one of
;;; a series of menus taken directly from the turn.
(defun iface-set-ddr-kcd-turn-c (Val Result)
  (setf (dde-result-command Val) **Show-hint**)
  (setf (dde-result-value Val) (turn-text Result))
  (setf (dde-result-menu Val) (turn-menu Result)))



;;; End-dialog turns are effectively empty turns that will be sent
;;; to the workbench soley to signal the end of a dialog.  This 
;;; code is unused at present.
(defun iface-set-ddr-end-turn-c (Val Result)
  (declare (ignore Result))
  (setf (dde-result-command Val) Nil)
  (setf (dde-result-value Val) Nil)
  (setf (dde-result-menu Val) Nil))


;;; Dialog turns are used for the hint process and will be used to 
;;; handle the next-step-help and why-wrong-help.  
;;;
;;; NOTE:: This code assumes that it is being passed a tutor-turn.
(defun iface-set-ddr-dialog-turn-c (Val Result)
  (when (turn-text Result)
    ;; Set the command if a message exists.
    (setf (dde-result-command Val) **Show-hint**)
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
      


;;; -----------------------------------------------------------------------------
;;; Commands submitted to the wb by the Help System.
;;; May need to move to interface.cl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-WB-Open-Browser-command
;; format an open-browser command for the workbench that can be appended to 
;; the tutor turn.  The purpose of this command is to locate the workbench
;; knowledge in a single place.  
;;
;; Argument:  
;;   File -- A string-form filename that will be opened
;;           relative to the <Andes2-Dir>/Review/ directory.
;; 
;; Returns: Nil.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-wb-open-browser-command (file)
  "Send an asynchronous command to open a browser."
  (format nil "open-browser ~A" file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-WB-Close-Browser-command
;; Format an asynchronous close-browser command to be used by the help
;; system when necessary.
;;
;; Arguments:  None.
;; returns:    None.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun format-wb-close-browser-command ()
  "Tell the workbench to close a browser."
  "close-browser")

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
(defun answer-only-mode-p ()
    (member 'answer-only-mode (problem-features *cp*)))

(defun answer-only-dispatcher (cmd args)
"dispatch a command in answer-only mode"
   ; switch on type of command, from API.cl
   (case (lookup-command->class cmd)
      ;; all following cmd classes can just be dispatched as usual
      ((State Answer Statistics Delete Control) (safe-apply cmd args))
      ;; Entries should be left black
      ((noneq-entry eq-entry) (make-black-turn))
      ;; Help requests: assume explain more could only be followup to allowed 
      ;; help -- presumably unsolicited help for wrong answers -- so process it. 
      ;; Reject all others help requests.
      (Help (case cmd 
                  (explain-more (safe-apply cmd args))
                  (otherwise (make-end-dialog-turn "Help is not available on this problem."))))
      ;; Algebra gets null answer
      (Algebra (make-eqn-failure-turn "The Andes calculator is not available on this problem."))
      ;; anything else: empty string should function as null return value.
      (otherwise (warn "unclassifed api command: ~A~%" cmd)
                  "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iface-set-dispatch-mode -- set dispatcher in appropriate mode for *cp*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun iface-set-dispatch-mode ()
"set the appropriate command dispatcher mode to use for current problem'"
 (setf **alternate-command-interpreter**
     (if (answer-only-mode-p) #'answer-only-dispatcher)))

