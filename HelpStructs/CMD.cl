;; CMD.cl
;; Collin Lynch
;; 1/28/2003
;; Copyright Kurt VanLehn.
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
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file contains the CMD struct along with specific predicates
;; to test and set various portions of the struct.  This file appears
;; in both the LogProcessing/CmdReader/ and HelpStructs modules.  The 
;; CmdReader translates each lofgile into a sequence of cmds using the 
;; code in this file.  The help system will translate the students 
;; actions as they occur into a sequence of cmds for use in runtime 
;; processing.  
;;
;; Each CMD represents a single workbench or help system action as
;; recorded in the logfile.  The actions are typically DDE's DDE Posts
;; or DDE-Commands.  The functions in the file are currently geared 
;; towards these three types although they can be used to track other 
;; command types as necessary.  
;;
;; The cmdresult structs are used to represent the results of the students
;; actions such as color-red, color-green etc.  These results can be used
;; as necessary to keep track of the students behavior and the systems 
;; responses.
|#

;;;; ===================================================================
;;;; CMD Struct
;;;; when they exists and the current result.  In theory the results 
;;;; should be identical.

(defstruct CMD
  Class    ;; One of Help-Request, State, Post, etc
  Type     ;; Either DDE, DDE-POST, 
  Time     ;; The Time that it occured (Htime)
  Call     ;; The Help system call.
  Result   ;; The result either Nil DDE-Res or DDE-Failed
  )


;;;; --------------------------------------------
;;;; CMD typechecks

(defun dde-post-cmdp (Cmd)
  "Is this cmd a dde-post?"
  (equal (cmd-type CMD) 'dde-post))

(defun dde-cmdp (Cmd)
  "Is this cmd a dde-post?"
  (equal (cmd-type CMD) 'dde))

(defun dde-command-cmdp (Cmd)
  "Is this cmd a dde-post?"
  (equal (cmd-type CMD) 'dde-command))


;;;; --------------------------------------------
;;;; CMD class

;;; ALGEBRA commands deal with calls such as solve-for
;;;  and simplify.
(defun Algebra-cmdp (Cmd)
  "Is this cmd an algebra call?"
  (equalp (cmd-Class CMD) 'Algebra))

;;; Answers deal with final answer submissions either in the answer
;;; field or a bring up a status-return-val.
(defun Answer-cmdp (Cmd)
  "Is this cmd an answer post?"
  (equalp (cmd-Class CMD) 'Answer))

;;; State calls are used to update or alter the system state and 
;;; signify values being refrshed/set.  As dde's these calls will
;;; get a status return-val type
(defun State-cmdp (Cmd)
  "Is this cmd a state call?"
  (equalp (cmd-Class CMD) 'State))

;;; At present there is only one control command.  This doesn't 
;;; appear in our student docs so it should appear rarely.  when
;;; it does it should only appear as a dde post so no return type
;;; should be given.
(defun Control-cmdp (Cmd)
  "Is this cmd a control command?"
  (equalp (cmd-Class CMD) 'Control))

;;; non-eq entries such as assert object are dde's that post a 
;;; value to the system.  As such they get a status return val
(defun noneq-entry-cmdp (Cmd)
  "Is this command a non-equation entry?"
  (equalp (cmd-Class CMD) 'noneq-entry))

;;; lookup-eqn string is our only eqn entry function.  If Async-mode
;;; is 0 then this will be posted as a dde and get a status-return-val
;;; if it is 1 then this will be posted as a dde and we will get a 
;;; EQ-Result as an asynchronous call sometime later.  This distinction
;;; will be handled by the cmdreader.  
;;;
;;; If async is set to 1 and a lookup-eqn-string appears as a dde then
;;; the system will throw an error.
(defun eq-entry-cmdp (Cmd)
  "Is this cmd a equation entry?"
  (equalp (cmd-Class CMD) 'eq-entry))

;;; Deletions remove an entry either equations or nonequations from the 
;;; screen.  Deletions are distributed as dde-posts and get no return
;;; value.  If noe is found as a dde then an error will be thrown.
;;; !!! [Bug 1254]] (lookup-eqn-string "") is a deletion when it comes via a 
;;; DDE-POST (and effectively a deletion if it comes otherwise as well). 
;;; But it doesn't get the 'Delete class tag set in its Class field 
;;; (see make-cmd call which uses lookup-command->class).  So this won't 
;;; detect those deletions. See delete-equation-cmdp below.
(defun Delete-cmdp (Cmd)
  "Is this cmd a deletion?"
  (equalp (cmd-Class CMD) 'Delete))

;;; help calls are expected to bring back hint responses to they get a
;;; Hint-return-val.
(defun Help-cmdp (Cmd)
  "Is this cmd a help call?"
  (equalp (cmd-Class CMD) 'Help))

;; For whatever reson the logs contain many api calls that we no 
;; longer have documentation for.  These are those calls.
(defun Unknown-cmdp (Cmd)
  "Is this cmd a help call?"
  (equalp (cmd-Class CMD) 'UNKNOWN))



;;;; --------------------------------------------
;;;; Composite types
;;;; These predicates detect supersets of the specific classes that 
;;;; are used for some of the predicate tests.  

;;; Return t if the cmd is an entry command
;;; either noneq-entry or eq-entry
(defun entry-cmdp (cmd)
  "Is this an entry command (either eq or noneq)?"
  (or (eq-entry-cmdp Cmd)
      (noneq-entry-cmdp Cmd)))


;;; solution-action-cmdp
;;; This describes the superset of all commands that Kurt considers
;;; to be solution actions.  These being the commands that represent
;;; changes to the solution state or to the student's mental state.
(defun solution-action-cmdp (Cmd)
  "Is this a solution action command?"
  (or (entry-cmdp Cmd)
      (help-cmdp Cmd)
      (delete-cmdp Cmd)
      (Answer-cmdp Cmd)))


;;; Solution-state-change-cmdp 
;;; This superset describes all entry actions that changes the solution
;;; state of the system.  These actions include answer attempts, entries
;;; and deletions.
;;; Algebra only reifies what is already written so the state does not
;;; really change fundementally.

(defun Solution-State-Change-cmdp (Cmd)
  (or (entry-cmdp Cmd)
      (delete-cmdp Cmd)
      (answer-cmdp Cmd)))


;;; Assertion-cmdp
;;; Assertion commands change the solution state of the system by asserting
;;; new objects.  They are either Answer assertions or entry assertions.
(defun assertion-cmdp (Cmd)
  (or (entry-cmdp Cmd) (answer-cmdp Cmd)))



;;;; ---------------------------------------------------------------------
;;;; APIS
;;;; All of the cmds call APIs of some kind.  Typically we will want to 
;;;; access and compare those calls as necessary in order to determine
;;;; whether two cmds address the same object and so on.  The code in 
;;;; this section is used to identify individual commands and to extract
;;;; information from them such as the arguments to the api call, the
;;;; call itself and the ID.  This code can be addressed directly or 
;;;; called by the other comparison commands in this section as it is
;;;; below.
;;;;
;;;; Note:: The CMD does not store information such as the ID in it in
;;;;        the same way that the Type or Class information is stored.
;;;;        When the user elects to extract the api-id or arguments this
;;;;        information must be looked up from the list in a method that
;;;;        is version-dependent.  It might be better to amend this so 
;;;;        that the api-calls are stored in some other way and that the
;;;;        relavent infornmation is compiled into the cmd for faster 
;;;;        access.  For now the interface will be left as-is.

;;; -----------------------------------------
;;; Extract portions of the call.

;;; Extract the specific function that the user has called 
;;; from the cmd itself.  
(defun cmd-call-func (Cmd)
  "Collect the API function CMD calls."
  (car (cmd-call CMD)))


;;; Given a cmd return the id index if it is supposed to 
;;; have an id.  That is, if this is one of the commands 
;;; that have been encoded with an ID index in API.cl
(defun cmd-call-id-index (Cmd)
  (when (cmd-call Cmd)
    (api-lookup-id-index (cmd-call Cmd))))


;;; Given a CMD lookup its id if it has one and return
;;; it.  Return nil if it does not have an id.
(defun cmd-call-id (Cmd)
  (when (cmd-call Cmd)
    (api-Lookup-id (cmd-call Cmd))))
    


;;; Given a cmd we want to collect the api-arguments
;;; from it's call for later use.  This can include
;;; comparisons of the type below or other changes
;;; This call returns the call list sans function name
;;; and id.  This function will preserve the order of 
;;; the arguments.
(defun cmd-call-args (Cmd)
  (when (cmd-call Cmd)
    (api-collect-args (cmd-Call Cmd))))




;;; --------------------------------------------
;;; Call tests.
;;; These functions are used to id specific api-calls
;;; that we want to track and make use of.

(defun get-proc-help-cmdp (CMD)
  "Is this a get-proc-help call?"
  (and (help-cmdp CMD) 
       (equalp (cmd-call-func CMD) 'GET-PROC-HELP)))

(defun why-wrong-cmdp (CMD)
  "Is this a why-wrong-equation or why-wrong-object call?"
  (and (help-cmdp CMD)
       (or (equalp (cmd-call-func CMD) 'WHY-WRONG-OBJECT)
	   (equalp (cmd-call-func CMD) 'WHY-WRONG-EQUATION))))

(defun why-wrong-EQN-cmdp (CMD)
  "Is this a why-wrong-equation or why-wrong-object call?"
  (and (help-cmdp CMD) 
       (equalp (cmd-call-func CMD) 'WHY-WRONG-EQUATION)))

(defun why-wrong-OBJ-cmdp (CMD)
  "Is this a why-wrong-equation or why-wrong-object call?"
  (and (help-cmdp CMD) 
       (equalp (cmd-call-func CMD) 'WHY-WRONG-OBJECT)))
  
(defun delete-object-cmdp (CMD)
  "Tests whether or not this is a delete-object command."
  (and (delete-cmdp CMD) 
       (equalp (cmd-call-func CMD) 'DELETE-OBJECT)))

; Note: delete-cmdp does not work for equation deletions
; sent as DDE-POST of (lookup-eqn-string "" id) [Bug 1254]
(defun delete-equation-cmdp (CMD)
  "Test whether this is a delete-equation or lookup-eqn-string \"\""
  (and (equal (cmd-type CMD) 'DDE-POST)
       (or (and (equalp (cmd-call-func CMD) 'LOOKUP-EQN-STRING)
                (equalp (first (cmd-call-args CMD)) ""))
	   ; following call no longer sent by workbench. Possibly was 
	   ; used in some old logs.
	   (equalp (cmd-call-func CMD) 'DELETE-EQUATION))))


(defun read-problem-info-cmdp (CMD)
  "Return t if this is an open-problem command."
  (and (cmd-call Cmd) 
       (equalp (cmd-call-func Cmd) 'read-problem-info)))


(defun close-problem-cmdp (CMD)
  "Return t if this is a close-problem command."
  (and (cmd-call Cmd) 
       (equalp (cmd-call-func Cmd) 'close-problem)))



(defun read-student-info-cmdp (CMD)
  "Return t if this is an open-problem command."
  (and (cmd-call Cmd) 
       (equalp (cmd-call-func Cmd) 'read-student-info)))








;;;; ========================================================================
;;;; Result Struct
;;;; The result struct is an internal storage device for the DDE-Results and
;;;; the dde-faileds.  If the dde is a dde-failed then the value slot will 
;;;; contain only the string representing the dde call that this is associated
;;;; with.
;;;;
;;;; If this is a dde-result then the value slot will contain one of the 
;;;; dde-result classes and their value(s).  

(defstruct CMDResult
  Class    ;; one of dde-failed or dde-result.
  Time     ;; The Time that it occured (Htime)
  Value    ;; The value of the result.)
  
  Assoc    ;; An assoc if one is present
  Score    ;; A set-score if one is present.  This is new in 2004.
  Commands ;; Async commands if they are present.
  )

;;; ---------------------------------------------------------------------------
;;; dde-result structs.
;;; There are three types of dde results.  The root dde-result struct below
;;; contains only three fields the Command Value and Menu fields which will be
;;; filled if the system contains a command or hintspec (which will be treated
;;; as a show-hint command).  
;;;
;;; In practice each result will be one of the 3 specific result-type structs
;;; that I have listed below the base struct.  These add specificc fields onto 
;;; the base struct value and are used to allow specificity at runtime without
;;; pushing special-case code out into the handlers.

(defstruct dde-result
  Command ;; The dde-result command.
  Value   ;; The value of the command.
  Menu    ;; An optional menu field that goes with the command.

  )

;;; status-return-val
;;; status-return-vals are returned when calls are made to generate entries
;;; such as equations, vectors and so on.  The fields below contain the 
;;; contents of the return value.  
(defstruct (status-return-val (:include dde-result))
  Coloring ;; A coloring indicating what should be done with the entry
           ;; in question one of Red, Green, or Nil (no change).
  Errors   ;; A list of possible error fields to be used with argument
           ;; hints if necessary.
  )


;;; hint-return-val
;;; hint-return-values are retunred as a result of help calls such as
;;; get-proc-help and explain-more.  They will be a command most likely
;;; show-hint.  The Andes spec allows a naked hintspec to be supplied as
;;; a result of these.  The system will spoof these to be a show-hint.
(defstruct (hint-return-val (:include dde-result))
  )


;;; Eqn-results
;;; Equation results are returned from calls to the algebra system.  
;;; they will contain both equation strings and the usual optional
;;; command field.  
(defstruct (eqn-result (:include dde-result))
  Eqn  ;; The equation string.
  )


;;; Stat-results
;;; Calc results consist of a list of lists of stat values of the form
;;; ((<Valname> <Grading Weight> <Value>) ...) that represent the result
;;; of grading calculations on the help system side.  
;;;
;;; These structures have no values above and beyond the fields in the
;;; dde-result struct and will only load the value field at runtime.
;;; The code has no command but we could include one if it became 
;;; necessary.
(defstruct (stat-result (:include dde-result))
  )

;;; -------------------------------------------------------------------
;;; Command parameters.

(defconstant **show-hint** 'Show-hint "The show-hint command.")
(defconstant **show-lesson** 'Show-lesson "The show-lesson command.")
(defconstant **training-card** 'training-card "The training-card command.")
(defconstant **delete-entry** 'delete-entry "The delete-entry command.")

;;; -------------------------------------------------------------------
;;; dde-result command tests.

(defun ddr-show-hintp (Result)
  "Is this a show-hint dde result."
  (equal (dde-result-command Result) **show-hint**))

(defun ddr-show-lessonp (Result)
  "Is this a show-hint dde result."
  (equal (dde-result-command Result) **show-lesson**))

(defun ddr-training-cardp (Result)
  "Is this a show-hint dde result."
  (equal (dde-result-command Result) **training-card**))

;; NOTE:: This code probably will not get exercised as the deletions
;;  are currently called using a dde-post that does not return a result
;;  however, the vode exists to produce those results in turn->wb-reply
;;  so they may become effective at some point.
(defun ddr-delete-objectp (Result)
  "Is this a show-hint dde result."
  (equal (dde-result-command Result) **delete-entry**))


;;; -------------------------------------------------------------------
;;; When dealing with show-hints the value will be a tuple of the form
;;; (<HintString> <Menu>) This code facilitates getting that for the
;;; user if it is appropriate.  (note no typechecking is done.)
(defun show-hint-ddr-hintstring (Result)
  "The dde result hintstring is returned."
  ;;(nth 0 (dde-result-value Result)))
  (dde-result-value Result))
  
(defun show-hint-ddr-menu (Result)
  "The dde result menu is returned."
  ;;(nth 1 (dde-result-value Result)))
  (dde-result-menu Result))




;;; -------------------------------------------------------------------
;;; DDE Hint Results
;;; For code's sake these functions extract the hint text from a show-hint-cmd

(defun show-hint-cmd-hintstring (Cmd)
  "Extract the hint text from the cmd returns nil if this has no hint."
  (let ((Result (cmd-result CMD)))
    (when (and Result (cmdresult-p Result)
	       (setq Result (cmdresult-value Result)) ;; returns the val.
	       (dde-result-p Result)
	       (equal (dde-result-command Result) **show-hint**))
      ;;(nth 0 (dde-result-value Result)))))
      (dde-result-value Result))))
      
(defun show-hint-cmd-hintmenu (Cmd)
  "Extract the hint text from the cmd returns nil if this has no hint."
  (let ((Result (cmd-result CMD)))
    (when (and Result (cmdresult-p Result)
	       (setq Result (cmdresult-value Result)) ;; returns the val.
	       (dde-result-p Result)
	       (equal (dde-result-command Result) **show-hint**))
      ;;(nth 1 (dde-result-value Result)))))
      (dde-result-menu Result))))



;;;; ========================================================================
;;;; Command Result Tests.
;;;; The predicates in this section are used to classify commands according
;;;; to their results.

;;; ----------------------------------------------------------------------
;;; General result-tests
;;; The tests in this section check the state of the incoming commands
;;; irrespective of their type.  These are valid on any command that
;;; recives a status return-val as its result.  This includes all entries
;;; all answer attempts, and many state commands such as open-problem
;;; and read-student-info.

;;; A cmd is incorrect when it it has a result, that result has 
;;; a status return val and that status return val is color red.
(defun incorrect-cmdp (Cmd)
  (let ((Result (cmd-result CMD)))
    (and Result (cmdresult-p Result)
	 (status-return-val-p (cmdresult-value Result))
	 (equalp 'Red (status-return-val-coloring 
		       (cmdresult-value Result))))))


;;; A cmd is incorrect when it it has a result, that result has 
;;; a status return val and that status return val is color green.
(defun correct-cmdp (Cmd)
  (let ((Result (cmd-result CMD)))
    (and Result (cmdresult-p Result)
	 (status-return-val-p (cmdresult-value Result))
	 (equal 'GREEN (status-return-val-coloring 
			(cmdresult-value Result))))))


;;; A cmd is incorrect when it it has a result, that result has 
;;; a status return val and that status return val is neither
;;; red nor green. 
(defun unvalued-cmdp (Cmd)
  (let ((Result (cmd-result CMD)))
    (and Result (cmdresult-p Result)
	 (status-return-val-p (cmdresult-value Result))
	 (not (member (status-return-val-coloring 
		       (cmdresult-value Result))
		      '(RED GREEN))))))



;;; ------------------------------------------------------------------
;;; Entry status tests.
;;; These functions test whether the incoming command is an entry and
;;; test its state.

;;; General entries.
(defun incorrect-entry-cmdp (Command)
  "Is this an incorrect entry?"
  (and (entry-cmdp Command)
       (incorrect-cmdp Command)))

(defun correct-entry-cmdp (Command)
  "Is this a correct entry?"
  (and (entry-cmdp Command)
       (correct-cmdp Command)))

(defun unvalued-entry-cmdp (Command)
  "Is this an unvalued entry?"
  (and (entry-cmdp Command)
       (unvalued-cmdp Command)))


;;; ------------------------------------------------
;;; Test whether the incoming command is an entry of 
;;; the specified type and return a result.
(defun incorrect-noneq-entry-cmdp (Command)
  "Is this an incorrect noneq entry?"
  (and (noneq-entry-cmdp Command)
       (incorrect-cmdp Command)))

(defun correct-noneq-entry-cmdp (Command)
  "Is this a correct noneq entry?"
  (and (noneq-entry-cmdp Command)
       (correct-cmdp Command)))

(defun unvalued-noneq-entry-cmdp (Command)
  "Is this an unvalued noneq entry?"
  (and (noneq-entry-cmdp Command)
       (unvalued-cmdp Command)))


(defun incorrect-eq-entry-cmdp (Command)
  "Is this an incorrect eqn entry?"
  (and (eq-entry-cmdp Command)
       (incorrect-cmdp Command)))

(defun correct-eq-entry-cmdp (Command)
  "Is this a correct eqn entry?"
  (and (eq-entry-cmdp Command)
       (correct-cmdp Command)))

(defun unvalued-eq-entry-cmdp (Command)
  "Is this an unvalued eqn entry?"
  (and (eq-entry-cmdp Command)
       (unvalued-cmdp Command)))


;;; -----------------------------------------
;;; Test whether the specified command is an 
;;; answer and return the result.
(defun incorrect-Answer-cmdp (Command)
  "Is this an incorrect Answer?"
  (and (Answer-cmdp Command)
       (incorrect-cmdp Command)))

(defun correct-Answer-cmdp (Command)
  "Is this a correct Answer?"
  (and (Answer-cmdp Command)
       (correct-cmdp Command)))

(defun unvalued-Answer-cmdp (Command)
  "Is this an unvalued answer?"
  (and (answer-cmdp Command)
       (unvalued-cmdp Command)))


;;; ---------------------------------------------------
;;; Test whether the specified command is an assertion
;;; and test its state.
(defun incorrect-Assertion-cmdp (Command)
  "Is this an incorrect Assertion?"
  (and (Assertion-cmdp Command)
       (incorrect-cmdp Command)))

(defun correct-Assertion-cmdp (Command)
  "Is this a correct Assertion?"
  (and (Assertion-cmdp Command)
       (correct-cmdp Command)))

(defun unvalued-Assertion-cmdp (Command)
  "Is this an unvalued assertion?"
  (and (assertion-cmdp Command)
       (unvalued-cmdp Command)))


;;; Solution State Changes
;;; The entries and answers can be correct and will
;;; be tested.  The deletions will be treated as correct.
(defun incorrect-solution-state-change-cmdp (Command)
  "Is this an incorrect solution-state-change-cmd?"
  (and (solution-state-change-cmdp Command)
       (not (delete-cmdp Command))
       (incorrect-cmdp Command)))

(defun correct-solution-state-change-cmdp (Command)
  "Is this a correct solution-state-change-cmd?"
  (and (solution-state-change-cmdp Command)
       (or (delete-cmdp Command) 
	   (correct-cmdp Command))))

(defun unvalued-solution-state-change-cmdp (Command)
  "Is this an unvalued solution-state-change-cmd?"
  (and (assertion-cmdp Command)
       (not (delete-cmdp Command))
       (unvalued-cmdp Command)))


;;; ---------------------------------------------------
;;; Solution actions
;;; The entries and answers can be correct and will
;;; be tested.  The deletions will be treated as correct.
;;; The help calls will be treated as unvalued.
(defun incorrect-solution-action-cmdp (Command)
  "Is this an incorrect solution-state-change-cmd?"
  (and (solution-state-change-cmdp Command)
       (not (delete-cmdp Command))
       (not (help-cmdp Command))
       (incorrect-cmdp Command)))

(defun correct-solution-action-cmdp (Command)
  "Is this a correct solution-state-change-cmd?"
  (and (solution-state-change-cmdp Command)
       (not (help-cmdp Command))
       (or (delete-cmdp Command) 
	   (correct-cmdp Command))))

(defun unvalued-solution-action-cmdp (Command)
  "Is this an unvalued solution-state-change-cmd?"
  (and (assertion-cmdp Command)
       (not (delete-cmdp Command))
       (or (help-cmdp Command)
	   (unvalued-cmdp Command))))



;;;; ----------------------------------------------------------------------
;;;; Hint Tests.
;;;; These predicates test whether or not the returned value from the 
;;;; command includes a show-hint command and, if so what type of hint
;;;; it is.

;;; Return t if this command has a result, the result is a cmdresult struct.
;;; And the struct contains a show-hint command.
(defun show-hint-cmdp (CMD)
  "Does this command return a dde-result?"
  (let ((Result (cmd-result CMD)))
    (and Result (cmdresult-p Result)
	 (setq Result (cmdresult-value Result)) ;; returns the val.
	 (dde-result-p Result)
	 (equal (dde-result-command Result) **show-hint**))))


;;; Return t if the topmost cmd has has a dde-result with a show-hint command
;;; and that command has no follow-up menu.  This is either a an atomic hint or
;;; one that is at the tail of some hint sequence.
;;; 1 does it have a result
;;; 2 Is that result a cmdresult struct?
;;; 3 Does that result have a value?
;;; 4 Is that value a dde-result?
;;; 5 Does that dde-result have a show-hint?
;;; 6 Is the menu nil?
(defun tail-hint-cmdp (cmd)
  "Is this a tail hint command?"
  (let ((Result (cmd-Result cmd)))
    (and Result (cmdresult-p Result)
	 (let ((value (cmdresult-Value Result)))
	   (and Value (dde-result-p Value)
		(ddr-show-hintp Value)
		(null (show-hint-ddr-menu Value)))))))


;;; Return t if the command represents an unsolicited hint.  That is return
;;; t if it is not a help cmd but results in a show-hint command.  This will
;;; probably be an error flag such as misunderstood signs on an equation entry
;;; or some other similar problem.  
(defun unsolicited-hint-cmdp (Command)
  "Return t if this is an unsolicited hint."
  (and (not (help-cmdp Command))
       (show-hint-cmdp Command)))









;;;; ==================================================================
;;;; Command Comparison
;;;; The code in this section is used to conduct comparison tests on 
;;;; pairs of commands.  As with the other direct command tests this
;;;; code is used primarily by the predicates for testing purposes.





;;; -----------------------------------------------------------------
;;; Comparing commands for field equality.
;;;
;;; Given a pair of commands return t if they are both have the same
;;; id.  Note that I am not checking to ensure that they are both 
;;; entries or deletes only that, if they both contain ids, the ids 
;;; are the same.  This is being done so that I can attempt to match
;;; deletions with the entries that created them and so on.
;;;
;;; Note that this does not check types.  I make no attempt to match 
;;; delete-object with non-eq entries I am merely relying on the fact 
;;; that equation ids are always numbers and non-equation-ids are 
;;; always symbols of the form: TYPE-###
(defun cmds-id-equalp (Cmd1 Cmd2)
  "Check if the two cmds are entries or deletes and are id equalp."
  (and (cmd-call Cmd1) (cmd-Call Cmd2) 
       (equalp (cmd-call-id Cmd1) (cmd-call-id Cmd2))))


;;; -----------------------------------------------------------------------
;;; Return t if the two command supplied use the same api call.  This is
;;; used largely for debugging purposes.
(defun cmds-same-api-callp (C1 C2)
  (and (cmd-call C1) (cmd-call C2)
       (equalp (cmd-call-func C1)
	       (cmd-call-func C2))))



;;; --------------------------------------------------------
;;; commands time diff.
;;; Given a pair of commands relate them acc

(defun cmds-time-diff (A B)
  "Return the amount of time elapsed between Cmd A and Cmd B."
  (sub-htimes (cmd-Time A) (cmd-time B)))


(defun cmds-time-thresh-diff (A B Diff)
  "Return t if the time elapsed between Cmds A and B <= Diff."
  (Htimes>= diff (sub-htimes (cmd-Time A) (cmd-time B))))





;;; --------------------------------------------------------------------
;;; Entry Comparison
;;; Given a pair on noneq-entries and a threshold ensure that the
;;; two entries have the same api and that they have less differences
;;; between them (irrespective of the id) than the threshold amount.
(defun thresh-equalp-noneq-ents (A B Threshold)
  (and (equalp (cmd-call-func A) (cmd-call-func B))
       (>= Threshold (list-diff (cmd-call-args A) 
				(cmd-call-args B)))))
















