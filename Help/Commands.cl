;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (CL)  <Collinl@pitt.edu>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;; Modifications by Anders Weinstein 2001-2008
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

(in-package :cl-user)
(eval-when (:load-toplevel :compile-toplevel)
  (use-package :symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands.lsp/cl -- Lisp functions handling API commands sent by the Andes 
;;  Workbench to the Help System.
;;
;; This corresponds to the "Manager" in the spec. It has one handler function
;; for each API command. These handler functions implement the commands mainly
;; by delegating to worker functions in the relevant modules. This module 
;; also maintains the record of the current dialog state if any for use in 
;; responding to subsequent student input.
;;  20 May 2002 - (CL) -- Added in entry-auto-logging and other macros to
;;   permit the creation of the *Studentactions* Stack in future these may
;;   be made a stock part of the system but, for now, it is a compiler setting.
;;   The additions have been made to:
;;     Check-noneq-entries:  Added log-studententry-act
;;     Handle-student-response: Added log-turn-response-act
;;  3/12/2003 -- (Cl) -- Added declaration to lookup-andes-command-type to
;;     suppress special warning.
;;  7/10/2003 -- (CL) -- 
;;     1. Moved the execute-andes-command function to Dispatcher.cl
;;     2. Modified the State apis to return empty green-turns as opposed to the
;;        T value that they were returning previously.  
;;     3. Moved return-turn to Dispatcher.cl
;;  
;;     Lastly I completed reorganizing the commands to clean up the code and
;;     to facilitate the execution process. 
;;
;;  7/30/2003 -- (CL) -- Removing calls to return-turn from funcs.
;;
;; This file contains the API calls that are called by the Andes workbench.  
;; as such it represents the language that the workbench uses to communicate
;; with the help system.  Additional meta-information about the API is located
;; in the file API.cl which is present in the Help module and in the 
;; LogProcessing/CmdReader/ module.  Between these two files we have the 
;; complete communication language for the Andes2 system from help system
;; to workbench and back.  
;;
;; When the student initiates a command on the workbench it will produce an 
;; appropriate api call and then send it through the TCP/IP stream.  Initially
;; these calls were read directly off from the stream by the server code in 
;; Andes2-Main.cl.  Now the calls are passed on 
;; to the execute-andes-command which generates a cmd struct for them and then
;; calls them.  
;;
;; It might be feasible to move some of the processing that is done in interface.cl
;; into the functions here but I have chosen to locate it there for semantic
;; purposes.

;; Initially those calls were read directlyu off of the TCP/IP stream by the 
;; server code in Andes2-Main and called.  Now the calls
;; are passed to the execute-andes-command function located in interface.cl
;; before being called.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)


;;; ===========================================================================
;;; State API calls.
;;; The do definitions are located in state.cl


;;-----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check-entries -- start or stop loading saved entries.
;; argument:
;;  State: T or nil indicating that the workbench is beginning to or will now
;;    cease sending saved entries to the help system.
;;
;; note(s): This was added to port grading from the cmdreader to the help
;;  system and will now be used for that purpose.
;;
;; This code will set the **checking-entries** flag at runtime.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-entries (State)
  (if (equalp State **checking-entries**)
      (warn "Unmatched check-entries call made for ~A." State)
    (setq **Checking-Entries** State)))

;;; ===========================================================================
;;; Eqn-entry
;;; Equation entry commands.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solve-for-var -- solve for the given var using the equations the student has
;;  entered so far. Only uses correct equations, so the result must also be
;;  correct
;; argument(s):
;;  var: the variable to solve for a string
;;  new-id: the id number of the equation window in which to put the new sim-
;;    plified equation.
;; returns:
;;  a string representing the equation to be put into the new id-slot and op-
;;  tionally a message to be displayed in the hint window. the equation gives
;;  the value of the var if possible, otherwise it gives an equation with all
;;  other known values substituded in.
;; note(s):
;;  adds the new equation to the list of entered equations, and the value of 
;; the variable, if found, to the value of the corresponding scalar/magnitude.
;;
;; It is also designed to log the result of the call for future use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solve-for-var (entry)
  (let* ((new-id (StudentEntry-id entry))
	 (var (StudentEntry-symbol entry))
	 (tmp (student-to-canonical var))
	 (result (when tmp (solver-power-solve 
			    31 (student-to-canonical var) new-id))))
    
    (cond ((and result (listp result)) 
	   (solve-for-var-success entry result))
	  ((stringp result) 
	   (make-eqn-failure-turn 
	    (format NIL "Unable to solve for ~A: ~A" var result)
	    :id new-id))
	  (t (make-eqn-failure-turn
	      ;; implemented in next-step-help.cl
	      (get-failure-to-solve-hint var)
	      :id new-id)))))

(defun solve-for-var-success (entry result)
  (let* ((studText (algebra result)))
    
    ;; Update the studententry
    (setf (StudentEntry-verbatim entry) studText)
    (setf (StudentEntry-prop entry) 
	  `(solve-for-var ,(symbols-referent (StudentEntry-symbol entry))))
    (setf (StudentEntry-parsedEqn entry) result)
    (setf (StudentEntry-state entry) +correct+)
    
    ;; save final result as if it were a new student entry. We need to 
    ;; remember slot is occupied for add-entry to trigger automatic
    ;; cleanup of equation in algebra on new entry.
    (add-entry entry)
    ;; finally return student equation turn
    (make-eqn-turn studText :id (StudentEntry-id entry))))


;;; ===========================================================================
;;; Answer API Calls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-answer -- lookup a students answer in the answer box
;; argument(s):
;;  answer: the contents of the answer field the student entered
;;  answer-id: the author-defined id for the answer field. must start with the
;;    string "ANSWER"
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  This uses the same hack on lookup-eqn-string to obtain the entry.  It works
;;  but it ain't exactly clean.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-answer (entry)
  ;; for Skatz experiment, and maybe generally:
  ;; remember done state before checking answer to detect event of
  ;; "finishing" problem.
  (let ((was-done (all-answers-done-p))
        (result (Do-Check-Answer entry)))
    (when (and (not was-done)
               (all-answers-done-p)    ; is now done
	       (not-curr-checking-problemp)) ; ignore if in initial entry check
       (add-followup-if-needed result))
    Result))

; display followup dialog in browser on done for certain problems in 
; Skatz experiment
(defvar *followup-problems* NIL) ; now disable followups by default

(defun show-kcds ()
"true if should show post-problem kcds when available"
  (or (sym-match (get-condition) 'Experiment)
      (sym-match (get-condition) 'Experiment1)
      (sym-match (get-condition) 'Experiment2)
      (and (sym-match (get-condition) 'Control)
           (member (problem-name *cp*) '(PRETEST POSTTEST)))))

(defun add-followup-if-needed (result-turn)
  (when (and (member (problem-name *cp*) *followup-problems*)
	     (show-kcds)
	     (equal (type-of result-turn) 'Turn))
     ; Add show lesson command to result turn. Gross hackery.
     ; We have to change existing result turn (presumably type dialog turn
     ; with color green and no message) into a minilesson turn with the 
     ; appropriate URL (and color) in order to get the command piggybacked
     ; on the returned color.  If we used an async command to do it, it would
     ; enter mode before color result is returned, I think.
     ; !!! Should verify we aren't clobbering an existing command
     (setf (turn-type result-turn) +minil-turn+)
     (setf (turn-text result-turn)
       (format NIL "http://136.142.94.84/cgi-bin/navalkcds?user=~a;prob=~a"
                   (help-env-student webserver:*env*) (problem-name *cp*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-mc-answer
;;
;; Handle entries associated with buttons.
;;
;; Currently, we have multiple choice and a single "done" button.  This is 
;; used for both the n-way multiple choice questions of the type in vec9 
;; where the student is selecting from among a set of choices.  
;; It is also used in the fbd-only 
;; problems and other problems as an "I am done" button.  
;;;
;; The code below identifies the case that we are in and then calls the 
;; appropriate handler code in entry-api.  
(defun lookup-mc-answer (entry)
  ;; Use the purpose field to determine type.
  (cond
    ;; Case of an "I am done button"
    ((equal (car (StudentEntry-prop entry)) 'done)
     (let ((was-done (all-answers-done-p))
	   (result (check-mc-no-quant-done-answer-sought entry)))
       (when (and (not was-done)
		  (all-answers-done-p)    ; is now done
		  ;; ignore if in initial entry check
		  (not-curr-checking-problemp)) 
	 (add-followup-if-needed result))
       result))
    
    ;; Handle the multiple choice case by generating the entry prop
    ;; and then handling it like any other.
    ((and (eql (car (StudentEntry-prop entry)) 'choose-answer)
	  (StudentEntry-checked entry))
     ;; update with the box that has been clicked
     (let ((ch (mapcar #'read-from-string (StudentEntry-checked entry))))
       (setf (third (StudentEntry-prop entry))
	   (if (cdr (StudentEntry-checked entry))
	       (cons 'orderless ch)   ;checkboxes
	       (car ch))))            ;radio buttons
     (check-noneq-entry entry))
    
    ;; In the event that an unrecognized type is supplied handle it like so.
    (t (warn "Unrecognized button entry: ~a, checked:  ~A" 
	      (StudentEntry-prop entry)
	      (StudentEntry-checked entry)))))

  
;;; ===========================================================================
;;; Help API Types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-student-response -- dispatch student dialog response to responder 
;;                            set in most recent tutor turn.
;; Returns: result string to be sent to workbench
;; Side-effect: updates last turn via return-turn.
;;
;; A responder function should take a single response code argument and return 
;; the next tutor turn. Reponse codes are not examined here but just passed 
;; through as a magic cookie.  The response codes must be defined by the 
;; protocol with the workbench so that it identifies the response chosen by 
;; the student for the purposes of the responder functions.
(defun handle-student-response (response-code)
  ;; Response code is either a symbol or a string.
  ;; A symbol indicates a link has been clicked while a 
  ;; string indicates text that has been entered.
  (cond 
    ;; We have a responder function from a previous tutor turn.
    (*last-turn-response*
     ;; dispatch the response to the responder function.  
     ;; Since handle-student-response is wrapped by return-turn, the 
     ;; result will be logged, and *last-turn-response* will be updated.
     (prog1 (apply *last-turn-response*
		   (list response-code))
       ;; Retire this response.
       ;; This prevents the student from getting the same hint repeatedly
       ;; by clicking on "explain-more."
       ;; If there are multiple-choice responses, it may no longer
       ;; make sense to retire old ones.
       (setf *last-turn-response* nil)))
    
    ;; Student types text, but there is no responder
    ;; from last term.


    ((and (stringp response-code) (is-a-question response-code))
     (make-end-dialog-turn 
      (strcat "Sorry, I don't know how to answer your question.&nbsp; "
	      "Please " *help-button-action* " for help.")
      :Assoc '((handle-text . question))))
    
    ((and (stringp response-code) (maybe-a-question response-code))
     (make-end-dialog-turn 
      (strcat "Your comment has been recorded.&nbsp; "
	      "If you need help, " *help-button-action* ".")
      :Assoc '((handle-text . possible-question))))
     
    ;; Assume everything else is a genuine comment.
    ((stringp response-code)
     (make-end-dialog-turn "Your comment has been recorded."
			   :Assoc '((handle-text . comment))))

    ;; link clicked, but responder gone!
    (T (make-end-dialog-turn 
	(strcat "Sorry, but you have already seen this hint.&nbsp; You can " 
		*help-button-action* " for more help.")
	:Assoc '((handle-link . stale))))))

(defun is-a-question (str)
  "Determine if student phrase is a question."
    ;; Test text for presence of a question.
    ;; Based on an analysis of the WHRHS logs on Oct. 1, 2010
    ;; 88 comments, of which 43 are questions or general requests for help
    ;;    35 questions begin with "how" "is" "should" "what" "where" "why"
    ;;    8 other questions or requests.  2 start with "help".
    ;;    2/3 of questions end in ?
    ;; However, a significant number of regular comments that end in "?"
  (< (match:match-model (list (car (match:word-parse str)))
			'(or "how" "is" "should" "what" "where" "why")) 1))

(defun maybe-a-question (str)
  "Determine if student phrase may be a question."
    ;; Ending with a "?" is a less reliable indicator of a question,
    ;; Likewise, comments starting with "help" or "hint" are often not 
    ;; intended as questions.
  (or (string-ends-with '(#\?) str)
	   (< (match:match-model 
	       (list (car (match:word-parse str)))
	       '(or "help" "hint")) 1)))

(defun string-ends-with (endings x)
  "Test if string ends with a given character, trimming whitespace."
  (let ((y (string-right-trim match:*whitespace* x)))
    (member (char y (- (length y) 1)) endings)))
