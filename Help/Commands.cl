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


;;; ==========================================================================
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
	 (result (when tmp (solver-power-solve 31 tmp new-id))))
    
    (cond ((and (null tmp) (has-algebraic-operators var))
	   (make-eqn-failure-turn 
	    entry
	    "Sorry, Andes can only solve for a single variable."
	    :mode 'complicated-lhs))
	  ((null tmp)
	   (make-eqn-failure-turn 
	    entry
	    (format nil "The variable <var>~A</var> is undefined." var)
	    :mode 'undefined-var))
	  ((and result (listp result)) 
	   (solve-for-var-success entry result))
	  ((stringp result) 
	   (make-eqn-failure-turn 
	    entry
	    (format NIL "Unable to solve for ~A: ~A" var result)
	    :mode result))
	  (t (make-eqn-failure-turn 
	      entry
	      ;; implemented in next-step-help.cl
	      (get-failure-to-solve-hint var))))))

;; see function make-red-turn
(defun make-eqn-failure-turn (entry Msg &key mode)
  "Generate an eqn entry turn."
  (make-tutor-response 
   entry 
   (list msg) 
   :state +incorrect+ 
   :spontaneous t
   :diagnosis (list 'solve-for-var mode)))


(defparameter *algebraic-operators* '(#\+ #\- #\/ #\^ #\*))

(defun has-algebraic-operators (var)
  "Determine if string has algebraic operators."
  (loop for x across var 
       thereis (member x *algebraic-operators*)))

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


;;; =========================================================================
;;; Answer API Calls

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
     (check-mc-no-quant-done-answer-sought entry))
    
    ;; Handle the multiple choice case by generating the entry prop
    ;; and then handling it like any other.
    ((and (eql (car (StudentEntry-prop entry)) 'choose-answer)
	  (StudentEntry-checked entry))
     ;; update with the box that has been clicked
     (let ((ch (mapcar #'read-from-string (StudentEntry-checked entry))))
       ;; Construct new list since old prop still used in grading
       (setf (StudentEntry-prop entry)
	     (list (car (StudentEntry-prop entry))
		   (cadr (StudentEntry-prop entry))
		   (if (cdr (StudentEntry-checked entry))
		       (cons 'orderless ch)   ;checkboxes
		       (car ch)))))            ;radio buttons
     (check-noneq-entry entry))
    
    ;; In the event that an unrecognized type is supplied handle it like so.
    (t (warn "Unrecognized button entry: ~a, checked:  ~A" 
	      (StudentEntry-prop entry)
	      (StudentEntry-checked entry)))))

  
;;; ==========================================================================
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
    ;; However, a significant number of regular comments end in "?"
  (< (match:best-value
      (match:match-model (list (car (match:word-parse str)))
			 '(or "how" "is" "should" "what" "where" "why"))) 1))

(defun maybe-a-question (str)
  "Determine if student phrase may be a question."
    ;; Ending with a "?" is a less reliable indicator of a question,
    ;; Likewise, comments starting with "help" or "hint" are often not 
    ;; intended as questions.
  (or (string-ends-with '(#\?) str)
	   (< (match:best-value
	       (match:match-model 
		(list (car (match:word-parse str)))
		'(or "help" "hint"))) 1)))

(defun string-ends-with (endings x)
  "Test if string ends with a given character, trimming whitespace."
  (let ((y (string-right-trim match:*whitespace* x)))
    (member (char y (- (length y) 1)) endings)))
