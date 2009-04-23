;; whatswrong.cl -- interprets the errclasses in errors.cl
;; Copyright (C) 2001 by Kurt VanLehn
;; Author(s):
;;  Kurt Van Lehn (kvl) <vanlehn@cs.pitt.edu>
;;  Collin Lynch <collinl@pitt.edu>
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

(defparameter **done-already**
    'done
  "A state (like dead-path, forbidden, premature...) for when the
   all the entries in the intended field have been entered already.")

(defparameter **debug-wwh-conditions** ()
  "If t then 'debug' conditions will be printed.")

;;; ================= called from entry-api.cl ========================
   

;;; top level call is do-whats-wrong (identifier for student entry)
(defun do-whats-wrong (id)
  "Given the id selected by the student in what's wrong help, returns a 
   tutor turn containing the associated error interpretation."
  (let ((student (find-entry id)))
    (if student 
	(progn (diagnose student)
	       (ErrorInterp-Remediation (StudentEntry-ErrInterp student)))
      (no-error-interpretation))))


;;; Given a student entry, returns a error interpretation.  If the
;;; entry has been diagnosed before, or is an equation entry (in which
;;; case parse-andes will have taken care of it) then just repeat the 
;;; hint sequence by returning the old error interpretation.  If the
;;; student entry is incorrect, then we have a new error that needs to
;;; be given an interpretation.  If the student's entry is premature
;;; or forbidden, then we need to construct an error interpretation
;;; that explains why.  If the entry is acceptable to color-by-numbers
;;; but not relevant to the solution (a yellow error), then say so.
;;; These are the only student entry states that should occur.
(defun diagnose (student)
  "Given a student entry, sets the error interpretation."
  (if (StudentEntry-ErrInterp student)
      ;; Even if the error interpretation originally caused the entry to 
      ;; turn colors make sure that this time it is just a plain dialog 
      ;; turn and does no coloring
      (setf (turn-coloring (ErrorInterp-Remediation 
			    (studentEntry-ErrInterp student)))  NIL)
    (setf (StudentEntry-ErrInterp student)
	  (let ((state (StudentEntry-State student)))
	    (cond
	     ((eq state **premature-entry**)
	      (explain-premature-entry student))
	     ((eq state **premature-subst**)
	      (explain-premature-subst student))
	     ((eq state **forbidden**)
	      (explain-forbidden student))
	     ((not (eq state **Incorrect**))
	      (make-failed-error-interpretation))
	     ((and (eq 'eqn (car (StudentEntry-Prop student)))
		   (not (solver-equation-redp 
			 (studentEntry-ParsedEqn student))))
	      (yellow-error student))
	     (T	(new-error student)))))))


(defun make-failed-error-interpretation (&optional (fn-msg 'no-error-interpretation))
 "Returns an error interpretaton indicate that Andes could not understand the student's error"
  (make-ErrorInterp
   :remediation (apply fn-msg nil)
   :diagnosis (list fn-msg)
   :order '((expected-utility . 0))))


(defun no-error-interpretation ()
  "Returns a hint sequence indicating that Andes can't figure out the student's error."
  (make-hint-seq
   (list (strcat "I cannot determine what's wrong with this entry.  Try "
		 "getting suggestions for a correct next step by clicking "
		 "on the light bulb button or 'Explain further.'")
	 '(function next-step-help))))

;;; -------------- what's wrong with a correct entry -------------
;;; These are supposed to explain why a correct entry shouldn't be
;;; entered.  They are just stubbed out for now.  Eventually, we'll
;;; need much better help since students are really going to hate
;;; these.

(defun explain-premature-entry (student)
  "Given a premature student entry, return an error interpretation"
  (declare (ignore student))
  (make-failed-error-interpretation #'ww-premature-entry))

(defun ww-premature-entry ()
  "Returns a hint sequence indicating that the student's entry is premature."
  (make-hint-seq
   (list (strcat "You need to make some other entries before this one. If "
		 "you aren't sure which ones to make, click on the light "
		 "bulb button for next step help.")
	 '(function next-step-help))))

(defun explain-premature-subst (student)
  "Given a premature student entry due to substituting numbers,
   return an error interpretation"
  (declare (ignore student))
  (make-failed-error-interpretation #'ww-premature-subst))

(defun ww-premature-subst ()
  "Returns a hint sequence indicating that the student's entry has numbers in it too early."
  (make-hint-seq
   (list (strcat "You have entered an equation that has given values in it.  "
		 "Before doing that, you should finish applying all the necesary "
		 "principles first.  If you're not sure what's missing, click on "
		 "the light bulb button for next step help.")
	 '(function next-step-help))))

(defun explain-forbidden (student)
  (declare (ignore student))
  (make-failed-error-interpretation #'ww-forbidden))

(defun ww-forbidden ()
  (make-hint-seq
   (list (strcat "The entry you've made is a reasonable way to solve the "
		 "problem, but the problem statement prohibits you from "
		 "using it.  Try a different approach.  If you want help "
		 "finding one, click on the light bulb button.")
	 '(function next-step-help))))

(defun yellow-error (student)
  "If the student's equation balances according to color by number but is not
   a combination of correct primitive equations, then it's not needed for solution"
  (declare (ignore student))
  (make-failed-error-interpretation #'ww-irrelevant))

(defun ww-irrelevant ()
  (make-hint-seq
   (list (strcat "Although the equation is a true statement about "
		 "the problem, it is not needed for deriving the answer.")
	 (strcat "When an equation turns green, it occurs in at least one "
		 "derivation of the answer.  If an equation turns red, it "
		 "occurs no derivation I know of."))))

;;; ------------ Analyzing an incorrect entry ------------------------
;;; Analysis of an error proceeds in 4 phases.  The first phase is to
;;; generate a large set of possible error interpretations.  Most of
;;; the interpretations will include a set of system (correct) entries
;;; called the intended entries.  Phase 2 is to modify the
;;; interpretations to take into account the context of the intended
;;; entries in the overall solution process.  The third phase is to
;;; select the best error interpreation.  The fourth phase is to
;;; generate the remediation turn for the selected interpretation.
  
(defun new-error (student)
  "Given an incorrect student entry that has not been given before,
   return an error interpretation"
  (let ((candidates (remove **correct** 
			    (applicable-error-analyses student) 
			    :key #'ErrorInterp-state))
	best)
    ;; (format t "Candidates are ~W" candidates)
    (contextualize candidates)
    (when (cdr candidates) ; trace conflicts, so we can vet the results
      (format *debug-help* "  Error candidates: ~W~%" 
	      (mapcar #'(lambda (x) (cons (ErrorInterp-name x)
					  (ErrorInterp-order x))) 
		      (sort (copy-list candidates) #'alist< 
			    :key #'ErrorInterp-order))))
    (setf best (if candidates 
		   (select-error-interpretation candidates)
		   (make-failed-error-interpretation)))
    (format *debug-help* "  Choose: ~A~%" (ErrorInterp-test best))
    ;; (format t "Best candidate is ~W" best)
    (setf (ErrorInterp-Remediation best) (generate-ww-turn best))
    best))

;;; ------------ Phase 1: Testing whether error conditions apply ------------
;;; given the student entry, returns an error analysis for each error
;;; handler whose conditions are true.
(defun applicable-error-analyses (student)
  ;; only use tests of type nil and 'no-match since we have already determined
  ;; no match is there
  (loop for eh in (remove 'match **entry-tests** :key #'EntryTest-apply) append
       (if (watch-this-error-class-p eh)
	   (check-err-conds-watched eh student)
	   (check-err-conds eh student))))

(defun check-err-conds-watched (error student)
  "Check the error conds with watching."
  (trace-class-checking)
  (let ((r (check-err-conds error student)))
    (untrace-class-checking)
    r))

(defun check-err-conds (test student)
  (check-err-conditions test student (EntryTest-preconditions test)))

;;; given a student entry and an error class, returns a list of error
;;; interpreations, one for each way of making the conditions of the
;;; error class's conditions true.  The arguments are: eh= the error
;;; class; st= the student entry struc; conditions = the remaining
;;; conditions to check; sy = the intended interpretation; bindings =
;;; the bindings generated so far by checking conditions.  This is a
;;; tail recursion.
;;;
;;; Note: the sy will be passed through unchanged by most of the calls
;;; only the (correct <pattern>) and (fix-eqn-by-replacing <old> <new>)
;;; will set the sy to the equation and will use that until changed by 
;;; a later entry.
(defun check-err-conditions (eh st conditions &optional sy 
				(bindings no-bindings))
  (cond
   ((null conditions)
    (list (make-ErrorInterp
	   :test (subst-bindings bindings (EntryTest-name eh))
	   :diagnosis (subst-bindings bindings (EntryTest-hint eh))
	   :state (eval (subst-bindings-quoted bindings (EntryTest-state eh)))
	   ;; for fix-eqn-by-replacing, sy has form (state . SystemEntries) 
	   ;; for correct, sy has form (SystemEntry)
	   :intended (or (cdr sy) sy)
	   ;; evaluate the cdr of each pair as a lisp expression
	   :order (mapcar #'(lambda (pair) 
			      (cons (car pair) 
				    (eval (subst-bindings-quoted 
					   bindings (cdr pair))))) 
			  (EntryTest-order eh)))))
   (t (let ((c (first conditions))  (r (rest conditions)))
	(case (first c)
	  (not (when (null (check-err-conditions eh st (second c) sy bindings))
		 (check-err-conditions eh st r sy bindings)))
	  (student (check-err-student (second c) eh st r sy bindings))
	  (old-student (check-err-old-student (second c) eh st r sy bindings))
	  (no-student (check-err-no-student (second c) eh st r sy bindings))
	  (correct (check-err-correct (second c) eh st r sy bindings))
	  (correct-nointent (check-err-correct-nointent (second c) eh st r sy bindings))
	  (no-correct (check-err-no-correct (second c) eh st r sy bindings))
	  (test (check-err-test (second c) eh st r sy bindings))
	  (problem (check-err-problem (second c) eh st r sy bindings))
	  (any-member (check-err-any-member (second c) (third c) eh st r sy bindings))
	  (student-eqn (check-err-eqn (second c) eh st r sy bindings))
	  (expr-loc (check-err-expr-loc
			    (second c) (third c) eh st r sy bindings))
	  (var-loc (check-err-var-loc
			   (second c) (third c) (fourth c) eh st r sy bindings))
	  (correct-var (check-err-correct-var 
			(second c) (third c) eh st r sy bindings))
	  (var-defn (check-err-var-defn
		     (second c) (third c) eh st r sy bindings))
	  (fix-eqn-by-replacing (check-err-fix-eqn-by-replacing
				 (second c) (third c) eh st r sy bindings))
	  (bind (check-err-bind (second c) (third c)
				eh st r sy bindings))
	  (debug (check-err-debug (cdr c) eh st r sy bindings))
	  (t nil))))))


;;; If the <pattern> of a (student <pattern>) condition unifies with
;;; the entry proposition of the student's entry, then continue
;;; checking conditions with the new bindings caused by unification.
;;; If not, then fail by returning NIL.
(defun check-err-student (pattern eh student conditions system bindings)
  (let ((b (unify (studententry-prop student) pattern bindings)))
    (and b (check-err-conditions eh student conditions system b))))

;;; If the <pattern> of a (old-student <pattern>) condition unifies with
;;; the entry proposition of an existing correct student entry, then continue
;;; checking conditions with the new bindings caused by unification.
;;; If not, then fail by returning NIL.
(defun check-err-old-student (pattern eh student conditions system bindings)
  (loop for s in *StudentEntries* with b nconc
	(when (and (equal (studentEntry-state s) **Correct**)
		 (setq b (unify (studentEntry-prop s) pattern bindings)))
	    (check-err-conditions eh student conditions system b))))

;;; If the <pattern> of a (no-student <pattern>) condition never unifies with
;;; the entry proposition of an existing correct student entry, then succeed.
(defun check-err-no-student (pattern eh student conditions system bindings)
  (when (loop for se in *StudentEntries*
	  never (unify pattern (studentEntry-prop se) bindings))
      (check-err-conditions eh student conditions system bindings)))

;;; If the <pattern> of a (correct <pattern>) condition unifies with
;;; the entry proposition of a system entry, then continue checking
;;; conditions with the new bindings caused by the unification.  If
;;; not, then skip this system entry proposition and try the next one.
;;;
;;; Note that this will recurse the call with system set to the correct
;;; entry if found.  This will 
(defun check-err-correct (pattern eh student conditions system bindings)
  (declare (ignore system))
  (loop for se in *sg-entries* with b nconc
	(and (setf b (unify pattern (systementry-prop se) bindings))
	     (check-err-conditions eh student conditions (list se) b))))


;;; If the <pattern> of a (correct-nointent <pattern>) condition 
;;; unifies with the entry proposition of a system entry, then continue 
;;; checking conditions with the new bindings caused by the unification.
;;; If not, then skip this system entry proposition and try the next one.
;;;
;;; Note, unlike correct the selected entry here will not be used for 
;;; the intended entry in the ErrorInterp.
(defun check-err-correct-nointent (pattern eh student conditions system bindings)
  (loop for se in *sg-entries* with b nconc
	(and (setf b (unify pattern (systementry-prop se) bindings))
	     (check-err-conditions eh student conditions system b))))


;;; If the <pattern> of a (no-correct <pattern>) condition never unifies with
;;; the entry proposition of a system entry, then succeed.
(defun check-err-no-correct (pattern eh student conditions system bindings)
  (when (loop for se in *sg-entries*
	  never (unify pattern (systementry-prop se) bindings))
      (check-err-conditions eh student conditions system bindings)))


;;; if the form in a (test <form>) condition is non-NIL after bindings
;;; have been substituted for the ?variables, then continue.
(defun check-err-test (form eh student conditions system bindings)
  (when (eval (subst-bindings-quoted bindings form))
      (check-err-conditions eh student conditions system bindings)))


(defun check-err-bind (var form eh student conditions system bindings)
  "The condition was (bind var form), so evaluate the form after substituting
   in values for the ? variables, bind the var to the result, and continue.
   If the form wants the condition to fail, it throws a NIL to check-err-bind"
  (catch 'check-err-bind
    (check-err-conditions eh student conditions system
			  (extend-bindings var 
					   (eval (subst-bindings-quoted bindings form)) 
					   bindings))))

;;; Check-err-debug prints out info to the system if the **debug-WWH** 
;;; variable is set to t.  If not then it moves on to the next call.
(defun check-err-debug (form eh student conditions system bindings)
  (when **Debug-WWH-Conditions**
    (apply #'format (cons t (subst-bindings Bindings Form))))
  (check-err-conditions eh student conditions system bindings))
  

;;; If the <pattern> of a (problem <pattern>) condition unifies with a
;;; proposition in the problem's working memory, then continue checking with
;;; the new bindings caused by the unificaiton.  If not, then try the
;;; next proposition.
(defun check-err-problem (pattern eh student conditions system bindings)
  (loop for proposition in (problem-wm *cp*) with b nconc
       (and (setf b (unify pattern proposition bindings))
	    (check-err-conditions eh student conditions system b))))


;;; If the condition is (any-member <var> <lisp form>), then evlauate
;;; the lisp form, and try binding <var> to each member of the
;;; resulting set.
(defun check-err-any-member (var form eh student conditions system bindings)
  (loop for x in (eval (subst-bindings-quoted bindings form)) nconc
        (check-err-conditions eh student conditions system
			      (extend-bindings var x bindings))))


;;; True if the student's entry is a variable.  The Entry prop in this
;;; case is (eqn <string>) which is useless.  The ParsedEqn slot of
;;; the student holds the students entry converted to system variables
;;; and prefix form.
(defun check-err-eqn (pattern eh student conditions system bindings)
  (let (b)
    (and (eq 'eqn (car (StudentEntry-prop Student)))
	 (StudentEntry-ParsedEqn Student)
	 (setq b (unify pattern (studentEntry-ParsedEqn Student) bindings))
	 (check-err-conditions eh student conditions system b))))

;;; If the condition is (expr-loc <loc-var> <pattern>) then
;;; find all locations in the equation the unify with <pattern> and
;;; bind them to <loc-var>
(defun check-err-expr-loc (loc-var pattern eh student conditions system bindings)
  (loop for L in (cons-cells (studententry-ParsedEqn student)) with b nconc
       (and (setq b (unify pattern (car L) bindings))
	    (check-err-conditions eh student conditions system
					  (extend-bindings loc-var L b)))))

;;; If the condition is (var-loc <?loc-var> <?variable-var> <pattern>), 
;;; then find a variable in the student's equation whose
;;; definition unifies with <pattern> and bind its location to
;;; <?loc-var> and the variable itself to <?variable-var>
(defun check-err-var-loc (loc-var var-var pattern eh student conditions system bindings)
  (loop for L in (cons-cells (StudentEntry-ParsedEqn student)) with b nconc
	(and (sysvar-p (Car L))
	     (setf b (unify pattern (sysvar-to-quant (car L)) bindings))
	     (check-err-conditions eh student conditions system
				  (extend-bindings var-var (car L)
							   (extend-bindings loc-var L b))))))

;;; Given an s-expression, returns a list of all the cons cells in it
(defun cons-cells (expr)
  (cond ((Null expr) nil)
	((atom expr) nil)
	((listp expr)
	 (cons expr
	       (append (cons-cells (car expr))
		       (cons-cells (cdr expr)))))))

;;; if the condition is (correct-var <?var> <pattern>), then find a
;;; system variable whose definition unifies with the <pattern> and
;;; bind it to the ?var.
(defun check-err-correct-var (var pattern eh student conditions system bindings)
  (loop for v in (Problem-VarIndex *cp*) with b nconc
        (and (setf b (unify pattern (qvar-exp v) bindings))
	     (check-err-conditions eh student conditions system
				   (extend-bindings var (qvar-var v) b)))))

;;; if the condition is (var-defn <?var> <pattern>), then the variable
;;; should be bound to a system variable and the variable's definition
;;; should match the pattern.
(defun check-err-var-defn (var pattern eh student conditions system bindings)
  (let ((pair (get-binding var bindings))
	b)
    (and pair
	 (sysvar-p (cdr pair))
	 (setq b (unify pattern (sysvar-to-quant (cdr pair)) bindings))
	 (check-err-conditions eh student conditions system b))))

;;; if the condition is (fix-eqn-by-replacing <old> <new>) and
;;; substituting the <new> expression for the <old> expression in the
;;; student's equation creates an equation that is correct (i.e., has
;;; a non-null set of interpretations, then find the best
;;; interpretation and treat that as the system entry.  That
;;; interpretation will consist of a state cons'ed to a set of
;;; system entries.
(defun check-err-fix-eqn-by-replacing (old new eh student conditions system bindings)
  (declare (ignore system))
  (let* ((old2 (subst-bindings bindings old))
	 (new2 (subst-bindings bindings new))
	 (neqn (subst-car new2 old2 (studentEntry-ParsedEqn student)))
	 (interps (sg-decompose-eqn neqn))
	 best)
    (when (setf best (find-most-cognitive-interpretation interps))
      (check-err-conditions eh student conditions best bindings))))

;;; returns a copy of the given expression with <new> substituted for
;;; the car of <old>
(defun subst-car (new old expr)
  (cond ((null expr) expr)
	((atom expr) expr)
	((eql old expr)
	 (cons new (copy-tree (cdr expr))))
	(T (cons (subst-car new old (car expr))
		 (subst-car new old (cdr expr))))))
							   

;;; --------- Phase 2: Determining the status of intended entry -----

;;; Given a possibly empty list of error interpretation, adjust the
;;; State fields to reflect whether the intended entry is premature,
;;; forbidden, etc.  Don't bother to change the remediation field now.  
;;; That will be done in generate-ww-turn.  If interpretation field of the
;;; ei is a singleton then must be a nonequation else the second of
;;; field is the equations interpretation and the car is the status


(defun contextualize (candidates)
  "Given error interpretations, adjust their states and order fields
   based on the SystemEntries matched."
  (dolist (ei candidates)
    ;; Test for a corresponding systementry and that
    ;; all corresponding systementries have already been done.
    (let ((done (and (ErrorInterp-intended ei)
		     (eq **correct** 
			 (SystemEntries->state (ErrorInterp-intended ei)))
		     (every #'SystemEntry-Entered (ErrorInterp-intended ei)))))
      
      ;; mark the state accordingly.
      (when done (setf (ErrorInterp-state ei) **done-already**))            
      
      ;; If entry is being ranked with expected-utility and has been done 
      ;; already, then associated interpretation is rather unlikely.
      ;; Here, we adjust expected-utility.
      ;; This is not a good strategy, because the numerical factor must
      ;; be fine-tuned, in a non-obvious way.
      (let ((expu (assoc 'expected-utility (ErrorInterp-order ei))))
	(when (and expu done)
	  (setf (cdr expu) (* 0.05 (cdr expu))))))))


;;; -------- Phase 3: Selecting an error interpretation -------------

;;; Given a possibly empty set of error interpretations, return the
;;; best one.
(defun select-error-interpretation (candidates)
  (let ((best (ErrorInterp-order (car candidates))))
    ;; Find the largest order specification.
    (dolist (b (mapcar #'ErrorInterp-order candidates))
      (when (alist< best b) (setf best b)))
    ;; select the set of optimal interpretations and make a random choice
    ;;
    ;; NOTE: when we choose randomly to break a tie, the "intended" entry 
    ;; may be very unreliable. 
    ;; Ex: solution has three axis rotations, and student picks none.  
    ;; Three wrong-axis-rotation instances will tie and one will be 
    ;; randomly chosen. 
    ;; This may not be the one nsh would prompt.  Possibly we should clear 
    ;; "intended" field if it differs among tied interps?
    (random-elt 
     (reverse ;for backwards compatibility, useful for log regression tests
		     (remove-if #'(lambda (x) (alist< x best)) candidates
				:key #'ErrorInterp-order)))))

;;; ---------- Phase 4: Generating the dialog turn --------------------

; dynamically bound to correct entry if known before calling turn
; generator, so generating functions can use it. Generating functions
; should know if error handlers set a correct entry or not.
(defvar *correct-entry* NIL)

;;; I wish there was a version of the case statement that evaluated the keys
(defun generate-ww-turn (ei)
  "Given an error interpretation, returns a tutor turn.  If the closest
   matching correct entry is weird, wraps a prefix around the tutor
   turn that would otherwise be generated."
 (let ((*correct-entry* (first (ErrorInterp-intended ei))))
  (cond ((eq **forbidden** (ErrorInterp-state ei)) 
	 (dont-bother-but 
	  ei (strcat "The closest matching CORRECT entry is forbidden by "
		     "the problem statement, so you shouldn't bother to "
		     "generate it.  If you want to anyway, click on Explain more.")))
	((eq **dead-path** (ErrorInterp-state ei))
	 (dont-bother-but 
	  ei (strcat "The closest matching CORRECT entry is does not lead "
		     "toward a solution, so you shouldn't bother to generate "
		     "it.  If you want to anyway, click on Explain more.")))
	((eq **premature-entry** (ErrorInterp-state ei))
	 (dont-bother-but 
	  ei (strcat "The closest matching CORRECT entry is premature, so you "
		     "need to enter the skipped steps before you enter this one.  "
		     "But you want help on generating this one anyway, click on "
		     "Explain more.")))
	((eq **premature-subst** (ErrorInterp-state ei))
	 (dont-bother-but 
	  ei (strcat "The closest matching CORRECT entry would contain given "
		     "values, but you have not yet finished applying all the "
		     "principles necessary for solving this problem, so Andes "
		     "will not accept it.  If you want to try entering it "
		     "anyway, click on Explain more.")))
        ((eq **done-already** (ErrorInterp-state ei))
	 (dont-bother-but 
	  ei (strcat "The closest matching CORRECT entry seems to be done "
		     "already, so you really don't need to generate it again.  "
		     "If you want to anyway, click on Explain more.")))
	(T (call-ww-turn-generator ei)))))


;;; Given an error interpretation and a string, it warns the studen by
;;; first presenting the string, then follows up with hint sequece
;;; that the error interpretation would cause if it the intended entry
;;; were correct.
(defun dont-bother-but (ei msg)
  (make-dialog-turn msg
		    'explain-more
		    :responder
		    #'(lambda (r)
			(if (equal r 'explain-more)
			    (call-ww-turn-generator ei)))))

;;; Used to be more complicated
(defun call-ww-turn-generator (ei)
  ; wrapper attaches function name as assoc info to turn
  (let ((form (ErrorInterp-diagnosis ei))
        ; need to fetch the EntryTest class to get its flag slots
        (test (find (ErrorInterp-name ei) **entry-tests** :key #'EntryTest-name))
         result-turn)
     (setf result-turn (apply (car form) (cdr form)))
     (setf (turn-assoc result-turn) (car form))
     ; Unless custom slot flags dynamically attached by error handler, 
     ; default to the static slot flags listed for the test. 
     (unless (turn-flag-slots result-turn) ; slots already specified
        (setf (turn-flag-slots result-turn) 
	      (when test (EntryTest-flag-slots test))))
     result-turn))

;;; ================ called inside errors.cl functions =================

(defun unify-in-wm (pattern &optional (bindings no-bindings))
  "Given a pattern and optional bindings, find the first fact in wm 
   that unifies with the pattern and return the resulting bindings, 
   or NIL if there is no matching fact."
  (loop for fact in (problem-wm *cp*) thereis (unify pattern fact bindings)))

(defun unify-with-student-entries (pattern &optional (bindings no-bindings))
  "Give a pattern and optional bindings, find the first student entry that
   unifies with the pattern and return the resulting bindings.
   or NIL if there is no matching student entry."
  (loop for se in *StudentEntries* thereis (unify pattern (StudentEntry-prop se) bindings)))


;;; --------------------- debugging utilities -------------------

(defun pwm ()
  "prints contents of working memory"
  (loop for fact in (problem-wm *cp*) do (print fact)))

(defun pvars ()
  "Prints correct variables"
  (loop for v in (problem-VarIndex *cp*) do (print (qvar-exp v))))

(defun pold ()
  "Prints the old student entries"
  (loop for s in *StudentEntries* do (print (studententry-prop s))))

(defun psys ()
  "Prints system entries"
  (loop for s in *sg-entries* do (print (systemEntry-prop s))))

(defparameter **watch-error-classes** nil
  "If t the error classes in **watched-error-classes** will be watched.")
(defparameter **Watched-Error-Classes** () "The Error classes being watched.")

;;; Trace the matching run for specific error tests using the 
;;; trace facility.
(defun watch-error-class (name)
  (setq **watch-error-classes** t)
  (push name **Watched-error-classes**))

(defun stop-watching-error-classes ()
  (setq **watch-error-classes** nil)
  (setq **watched-error-classes** nil))


(defun watch-this-error-class-p (class)
  (and **Watch-error-classes**
       (member (EntryTest-name class) **Watched-error-classes**)))


(defun trace-wwh ()
  (trace diagnose do-whats-wrong
	 make-failed-error-interpretation
	 no-error-interpretation
	 explain-premature-entry
	 explain-premature-subst
	 explain-forbidden
	 yellow-error 
	 new-error
	 applicable-error-analyses
	 ))

(defun trace-class-checking ()
  (trace check-err-conditions
	 check-err-student
	 check-err-old-student
	 check-err-correct
	 check-err-no-correct
	 check-err-test
	 check-err-bind
	 check-err-problem
	 check-err-any-member
	 check-err-eqn
	 check-err-expr-loc
	 check-err-var-loc))

(defun untrace-class-checking ()
  (untrace check-err-conditions
	 check-err-student
	 check-err-old-student
	 check-err-correct
	 check-err-no-correct
	 check-err-test
	 check-err-bind
	 check-err-problem
	 check-err-any-member
	 check-err-eqn
	 check-err-expr-loc
	 check-err-var-loc))
