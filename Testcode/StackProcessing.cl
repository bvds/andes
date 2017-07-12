;;;; StackProcessing.cl
;;;; Collin Lynch
;;;; 4/16/2003
;;;; Copyright Kurt VanLehn
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; The functions in this file are used to process LIFO stacks of CMDs
;;;; This code has been designed to support the stack testsets and other
;;;; code for parsing the contents of stacks as they execute.  The 
;;;; individual predicates have more full explanations.  
;;;;
;;;; My goal in creating this file is to centralize commonly needed stack
;;;; predicates in order to support the creation of new test predicates
;;;; without the need to rewrite common functions.  
;;;;
;;;; The utilities will be more fully commented in their own section.
;;;; The functions in this file come in the following types.
;;;;   Predicates of the form *p:
;;;;    These return nil or some value specific to the call. 
;;;;
;;;;   Counters of the form -count:
;;;;    These return an integer of some kind.
;;;;
;;;;   Stack Collectors of the form -stackc:
;;;;    These return a stack of cmds (typically in the order that they appeared 
;;;;    in the log.  Depending upon the function definition these may contain
;;;;    many different types of commands or just one type.  
;;;;
;;;;    They may change the order of the input to the output as well.  
;;;;
;;;;   List functions -lst:  These operate on the stacks as lists and are
;;;;    guaranteed to not change their order.  
;;;;
;;;; All of these functions take Stacks (list of entries in LIFO order) as 
;;;; arguments.
;;;;




;;;; ==========================================================================
;;;; Generalized stack tests.
;;;; The code in this section is used to run generic tests on the stack such as
;;;; collecting the number of entries that pass a set predicate and so on.  

;;; ---------------------------------------------------------------------------
;;; similar uni-rep
;;; Given a stack of entries and a test function return the number of in
;;; sequence from the topmost cmd in the stack that match the test.
;;;
;;; This will be a stack in oldest-first order or nil.  It may contain only 
;;; one element.

(defun uni-test-stackc (Stack Test &optional (count 0) (Result Nil))
  "Return the number of items in the seq that match test."
  (if (and Stack (funcall test (car Stack)))
      (uni-test-stackc (cdr Stack) Test (+ 1 Count) (cons (car Stack) Result))
    Result))



;;; -----------------------------------------------------------------------
;;; Similar-multi-Rep
;;; Given a stack and a test return the maximum number of repetition Pairs.  
;;; That is iterate over the stack counting the length of the sequence of 
;;; pairs for which the test returns t without interuption.  Given a stack
;;; fo the form (a b c d)  The system will first call test(a, b) then it 
;;; will call test(b, c) and if that passes repeat.  The final count will
;;; be the number of pairs that pass in succession.
;;;
;;; The result will be a list where the firstr is the number if items in 
;;; the sequqnce defined by the relationship.  The second item in the list 
;;; is the topmost element in the sequence. 
;;;
;;; Note that the system is designed to include all items that appear in the
;;; sequence including those that have no successor.  
;;;
;;; This will return a stack in oldest-first order that will contain nil or
;;; at minimum, two items.
(defun multi-test-stackc (Stack Test)
  "Return the number of pairs that match the stack test."
  (if (and (car Stack) (cadr Stack) (funcall Test (car Stack) (cadr Stack)))
      (multi-test-stackc-cont (cdr Stack) Test (list (car Stack)))))

(defun multi-test-stackc-cont (Stack Test Result)
  "Return the number of pairs that match the stack test."
  (let ((A (Car Stack)) (B (cadr Stack)))
    (cond ((and A (null B)) (cons A Result))
	  ((and A B (funcall Test A B))
	   (multi-test-stackc-cont
	    (cdr Stack) Test (cons A Result)))
	  (t Result))))



;;;; ==========================================================================
;;;; Help Stacks
;;;; The code in this section is used to identify and process help stacks. 

;;; -----------------------------------------------------------------------
;;; Help-Stackc
;;; Test whether or not the topmost portion of the stack (the last N) commands
;;; are part of a help stack.  The result will be nil if the topmost command is
;;; not part of a help stack.  If it is part of a help stack then the result 
;;; will be a list where the car is the stack cap and the cdr is a list of cmds
;;; in the stack ending with the current command.  
;;;
;;; NOTE:: that the functions in this section will also be used by other 
;;; predicate calls elsewhere.
;;;
;;; NOTE:: that stacks can contain only one entry and can for now be just any
;;; entry that returns a show-hint.
;;;
;;; Help-stackc returns a stack in lifo order that is the topmost item on the
;;; stack will be the oldest cmd in it.  It takes its arguments in in reverse 
;;; order.  The stack will contain only help commands and may contain only one
;;; entry.  


(defun Help-Stackc (Stack)
  (let ((CMD (car Stack)))
    (when (or (help-cmdp CMD) (help-stack-nohelp-capp CMD))
      (help-stackc-test Cmd (cdr Stack) nil 0))))


;;; Once we have a valid help command then test it by type and
;;; add it to the result or return the result.  Note that this 
;;; tests for delete equations as well before recursing.
;;; Deletions records the number of deletes max 1 per stack.
(defun help-stackc-test (CMD Stack Result Deletions)
  (cond ((help-stack-capp CMD) (cons CMD Result))
	
	((help-stack-cont-cmdp CMD)
	 (help-stackc-test 
	  (car Stack) (cdr Stack) (cons CMD Result) Deletions))
	
	; allow for an automatic equation delete notification right
	; after a help request (sent if focus leaves a deleted eqn's box)
	((and (delete-equation-cmdp CMD) 
	      (= 0 Deletions)
	      (help-cmdp (car Stack)))
	 (help-stackc-test (car Stack) (cdr Stack) Result 1))

	; [Bug 1268] Allow for an automatic deletion notification for an 
        ; earlier variable. This can be sent just after the variable definition 
	; in the case where a new variable entry redefines a previously-defined label.
	; if the entry triggers unsolicited help (e.g. for the given value), then
	; this will occur inside a hint sequence
	((delete-cmdp CMD)   ; general, allows any object deletion.  
	    (help-stackc-test (car Stack) (cdr Stack) Result 1))
	
	;; Not too sure how this worked previously, but things
	;; like read-problem-info end up on the stack and
	;; we simply want to conclude that this is not a help
	;; stack in such cases.
	(t nil)))

;;; a cmd is a help cont if it is an explain-more or
;;; a handle student response.
(defun help-stack-cont-cmdp (cmd)
  (member (cmd-command cmd)
	  '(EXPLAIN-MORE 
	    HANDLE-STUDENT-RESPONSE)))

;;; A help stack is capped (started) by a next-step-help, 
;;; do-whats-wrong help or any other entry that returns a 
;;; show hint command with a menu.
(defun help-stack-capp (cmd)
  (or (help-stack-help-capp cmd)
      (help-stack-nohelp-capp cmd)))

;;; Return the matching item name to signify what type of help 
;;; cap this is.  
(defun help-stack-help-capp (cmd)
  "If this is a do-whats-wrong call or a next-step-help call."
  (find (cmd-command cmd) 
	'(next-step-help 
	  do-whats-wrong)))


;;; Return t if this is not a help call but it does result in a 
;;; show-hint that has a menu.  
(defun help-stack-nohelp-capp (cmd)
  (and (not (help-cmdp cmd))
       (show-hint-cmdp Cmd)
       (show-hint-ddr-menu 
	(cmdresult-value (cmd-result cmd)))))

