;;; Modifications by Anders Weinstein 2000-2008
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nogood facility.
;; Collin Lynch 12/18/2000
;;
;; Nogoods are defined as a specific condition that cannot
;; or rather should not occur in the knowledge base.  The 
;; purpose of nogoods is to automate the testing for these
;; conditions.  Nogoods are tested against a list of 
;; assumptions that are generated at solution time.
;;
;; Assumptions are a specific set of kb expressions that 
;; describe specific decisions that the student has made 
;; such as '(applying lk-noa <Body> <Time>)'  The nogoods
;; are designed to react to specific combinations of these
;; items.
;;
;; Nogoods themselves consist of a list of preconditions 
;; similar to operators.  These preconditions can be 
;; unification forms, count executables, test executables
;; or bind executables.  Unlike operators which chain 
;; allowing for unsatisfied preconds to be dealt with by
;; other operators preconditions test strictly the assumpts
;; that are there and do not manufacture any more.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters:
;; *print-nogood-messages* -- If t then the nogood's message will
;;                          be printed if and when it is triggered.
;;
;; Nogood fields.
;;   Label:     Nogood ID.
;;   Triggers:  List of formas and executable preconds.
;;   Specs:     String specifications/Comments on the Nogood.
;;   Message:   Message to be printed when the nogood is triggered.
;;
;;------------------------------------------------------------
;; Nogoods should be defined using the defnogood macro.  This
;; will generate and then store the nogood for later use.
;;
;; (defnogood <label> <triggers> [&key <specs> <message>])
;;
;; (clear-nogoods)  -- Clear the list of stored nogoods.
;;
;; (test-for-nogood <Assumpts> {&optional <Bindings: no-bindings>}) 
;;    -- Test each of the known nogoods against the assumptions 
;;       and return the first nogood that is triggered.
;;
;; (test-nogood <Nogood> <Assumpts> {&optional <Bindings: no-bindings>)
;;    -- Test the specific nogood against the assumptions 
;;       to see if it fires.  If so return t.


;;=============================================================================
;; Nogoood parameters.

(defparameter *nogoods* ())
(defparameter *print-nogood-messages* () "If a nogood is signalled print out the nogood message.")

;;=============================================================================
;; Nogood Struct
;;
(defstruct (nogood (:print-function Print-nogood))
  Label              ;;Unique label assigned for id purposes.
  Triggers           ;;List of unifiable triggers used to test for nogood firing.
  Specs              ;;List of strings describing this nogood.
  Message            ;;Nogood message to be printed iff *print-nogood-messages* is t.
  )


(defun print-nogood (Nogood &optional (Stream t) (Level 0))
  "Print out the specifid nogood."
  
  (pprint-Indent :current Level Stream)
  (format Stream "<Nogood: ~A~%" (Nogood-Label Nogood))
  (format-string-list (nogood-specs Nogood) Stream Level)
  (pprint-Indent :current Level Stream)
  (format Stream " Triggers ~w~%" (Nogood-Triggers Nogood)))



;;=============================================================================
;; Nogood functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defnogood (public)
;; Define a nogood and register it in the system for later use.

(defmacro defnogood (label triggers &key (specs nil) (message nil))
  (let ((ng (make-nogood :label Label          ;;Generate a new nogood struct.
			 :triggers triggers
			 :specs specs
			 :message message)))
    (push ng *nogoods*)                       ;;Store the struct.
    ng))                                      ;;and return it.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clear the nogood storage.

(defun clear-nogoods ()
  "Clear the nogood storage."
  (setq *nogoods* ()))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-for-nogood (public)
;; Given a list of ground expressions test to see if it triggers one of the 
;; registered nogoods.
;;
;; Arguments:  exps: List of expressions being tested.
;;
;; Returns: For the time being this returns t.  

(defun test-for-nogood (exps)
  "Test to determine if a given set of expressions triggers a nogood."
  
  (loop for ng in *nogoods*		; For each nogood NG
      when (test-nogood ng Exps)	; Test for its success.
      do (if *Print-Nogood-Messages*
	     (format t "Nogood Signalled ~A, ~A~%"
		     (nogood-message ng) exps))
      and return t))			; Return t iff it unifies.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-nogood (Public)
;; Given a list of ground expressions test a given nogood by attempting to
;; unify its triggers with the expressions.  If the lists unify then a nogood 
;; error string is returned.
;;
;; Arguments:  ng:   Nogood being tested.
;;             exps: List of expressions being tested.
;;
;; Returns: For the time being this returns t.  Later it will return more 
;; useful info.
;;          A signalled nogood error with unified triggers.

(defun test-nogood (ng exps &optional (Bindings no-bindings))
  "Test a given nogood against a collected list of assumptions."
  (let ((Binds (list Bindings)))               ;Generate local binding storage.
    (loop for trigger in (nogood-triggers ng)  ;For each trigger in the triggers list.
	unless (setq Binds                                                 ;;loop until the inner loop returns nil     
		 (loop for B in Binds                                      ;;For each element in the bindings list.
				
		     when (evaluate-ng-trigger Trigger Exps B)             ;;Evaluate the trigger type.
		     nconc it))                                            ;;Collect the results.
	return nil)                                                        ;;If no unification occurs return nil.
    Binds))                                                                ;;or return bindings if there are any.


;;----------------------------------------------------------------
;; Nogood evaluation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluate-ng-trigger
;; Given a nogood-trigger identify its type and
;; call the appropriate function returning the result.
;;
;; Arguments: Trigger:  The trigger to be tested.
;;            Exps:     The expressions it is being tested on.
;;            Bindings: The set of bindings to use in testing.
;;
;; Returns: The set of bindings resulting from trigger evaluation.

(defun evaluate-ng-trigger (Trigger Exps Bindings)
  "Evaluate the supplied trigger and return the result."
  
  (case (car trigger)                                      ;;test the trigger type.
    (test (ng-test-trigger (cadr Trigger) Bindings))       ;;If this is a test executable
    (bind (ng-bind-trigger (cdr trigger) Bindings))        ;;Iff this is an explicit bind
    (count (ng-count-trigger (cdr trigger) Exps Bindings)) ;;Perform a count bind and return the result.
    (otherwise  (loop for Exp in Exps                      ;;then attempt to unify the trigger with
		    when (unify trigger exp Bindings)      ;;each of the exps and return the successes
		    collect it))))                         ;;If any.
		 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ng-count-tigger
;; Given a trigger of the form (count <Var> <Form>)
;; return a set of bindings with <Var> bound to the number of
;; items that match <Form> in Exps.
;;
;; Arguments: Count:    The rest of the count expression of the form 
;;                       (<Var> <Form>)
;;            Exps:     The expressions to be tested.
;;            Bindings: The set of bindings to be used
;;
;; Returns: Bindings (if supplied) with a binding for <Var> added.

(defun ng-count-Trigger (Count Exps Bindings)
  "Implements the (Count <Var> <Form>) trigger."
  
  (let ((Exp (subst-bindings Bindings (cadr Count))))
  
    (list (extend-bindings (car Count)                                      ;;Add a binding for <Var>
			   (loop for E in Exps                              ;;to to the number of Exps
			       count (unify (rename-variables Exp)          ;;that unify with <Form>
					    E Bindings))                    ;;Given bindings.
			   Bindings))))                                      ;;returning the result.
			    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ng-test-trigger
;; Given a trigger of the form (test <Exp>)
;; evalueate Exp and return Bindings iff <Exp> returns t.
;;
;; Arguments: Test:     The lisp expression to be evaluated.
;;            Bindings: The bindings to be used.

(defun ng-test-trigger (Test Bindings)
  "Evauluate the test function."
  
  (if (eval (subst-bindings-quoted Bindings Test))  ;;evaluate the test and return
      (list Bindings)))                      ;;(Bindings) iff it is true.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ng-bind-trigger
;; Perform an explicit bind expression of the form 
;; (Bind <Var> <Exp>)
;;
;; Arguments: Bind:     The expression (<var> <Form>)
;;            Bindings: The bindings to be used.
;;
;; Returns: The set of bindings with the var added.

(defun ng-bind-trigger (Bind Bindings)
  "Execute the specified bind trigger."
  (list (extend-bindings (car Bind)         ;;add it to the bindings and return.
			 (eval (subst-bindings Bindings 
					       (cadr Bind)))
			 Bindings)))

