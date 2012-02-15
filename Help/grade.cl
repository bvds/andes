;;; grade.cl
;;; Copyright 2011 by Kurt Vanlehn and Brett van de Sande
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
;;;
;;;               Grading for Andes problems
;;;
;;;  Currently (2011), old sessions are run through the help
;;;  system to set the system state.  Thus, we don't need to store
;;;  details of the grading state.
;;;
;;;  The general strategy is to attach bottom-out hints and red turns 
;;;  to steps in the solution.  If a student receives such a hint,
;;;  or a red turn, a label associated with that hint or a prop associated
;;;  with the student entry is attached to that solution step.  
;;;  This attachment persists even if the student object associated 
;;;  with that step is subsequently modified or deleted.  
;;;  However, a solution step is eligible for points only if there 
;;;  is a current green student entry associated with it.
;;;
;;;  Need to think about policy for optional steps.  
;;;  Consider two possibilities:  
;;;    A.  Include optional step in grading only if student 
;;;        sucessfully completes step.
;;;    B.  Never include step in grading.  
;;;  However, B seems to be at odds with an optional step being
;;;  preferred.  Maybe when we split optional into preferred and 
;;;  allowed, Bug #972, we would use policy A for preferred and B for allowed.
;;;
;;;  Error handlers often associate a student step with a correct
;;;  step.  Error handlers must also specify whether a given error
;;;  should have points taken off.  For instance, some errors are associated
;;;  more with Andes conventions/limitations and the student should 
;;;  not be penalized in those cases.  
;;;
;;;  If an incorrect student entry can be attached to a proposition,
;;;  and there is no associated solution step available, then we
;;;  need to specify a policy.  For instance, we could include a 
;;;  skill "correct quantities" where quantities not used in solution 
;;;  are attached.  Alternatively, maybe they don't count at all.
;;;
;;;  Eventually, weighting for steps should be determined by the 
;;;  median time it takes a student to complete that step.  This will
;;;  have to be done through a log file analysis, after the fact.
;;;  in the mean time, we need to make a best guess based on the amount
;;;  of typing and mouse movements needed to complete that step.
;;;
;;;


(defvar *grade* nil "Alist of (prop . graded object).")
(defvar *debug-grade* nil "Flag for debugging grading.")

(defconstant +random-average-score+ 0.25 "global default value")
(defvar *random-average-score* +random-average-score+
  "Possible section-specific value.")

  ;;  May have catch-all props for various object types:
  ;;         unneeded-vector
  ;;         unneeded-scalar
  ;;         unneeded-equation
  ;;         unneeded-body
  ;;         unneeded-line
  ;;         unneeded-axes


(defun initialize-grading ()
  
  (setf *random-average-score* 0.5)

  ;; Add Answer boxes, multiple-choice, and done buttons.
  ;; Initialize grading for SystemEntries that are part of 
  ;; a qualitative sub-problem.

  ;; Maybe better to add SystemEntries for these objects?
  (dolist (ans (problem-soughts *cp*))
    ;; old:  Corresponds to Multiple_Choice_Answer_Entry_Subscore
    (cond ((eq (car ans) 'choose-answer)
	   ;; These are already included in *sg-entries*
	   ;; For now, assume 15 seconds to click answer box.  
	   ;; (push (cons ans (make-graded :required t :weight 15)) *grade*)
	   )
	  
	  ;; old: Answer_Entry_Subscore 
	  ((quantity-expression-p ans)
	   ;; For now, assume 45 seconds to fill out answer box.  
	   ;; These are already defined in *sg-entries*
	  ;; (push (cons (list 'answer ans) (make-graded :required t :weight 45)) *grade*)
	   )

	  ;; Add entries for qualitative soughts.  Canonical case
	  ;; is drawing fbd.  Usually hase done button associated
	  ;; with it (should be included as separate step).
	  (t
	   ;; List of enodes associated with qual sought already
	   ;; in list of SystemEntries.
	   ;; old:  equivalent to *test-cache-drawing-entries*
	   ;; old:  corresponds to Diagram_Entry_Subscore
	   ;; Default is that entries are required.
	   ;; must set optional explicitly
	   ;;(dolist (entry (enode-required-entries
	;;		   (match-exp->enode ans (problem-graph *cp*))))
	 ;;    (setf (graded-required (SystemEntry-graded entry)) t))
	   ;; These are already defined in *sg-entries*
	   ;; Add done button, if it exists.
	   ;; old: corresponds to MC_Answer_Entry_Subscore ???
	   ))

    ;; Add weights and possiblities to *SG-Entries*
    ;; Ignore implicit-eqn
    (dolist (sysent *SG-Entries*)
      (let ((graded (SystemEntry-graded sysent)))
	(when (eql (car (SystemEntry-prop sysent)) 'implicit-eqn)
	  (setf (graded-ignore graded) t))
	;; Set optionality.  For now, test is based on tree structure,
	;; ignoring the explicit optionality operators.
	(setf (graded-optional graded)
	       (sg-systementry-optional-p sysent))
	;; For now, just put in dummy value
	(unless (graded-weight graded)
	  (setf (graded-weight graded) 13))
	;; For now, just put in dummy value
	(unless (graded-possibilities graded)
	  (setf (graded-possibilities graded) 17))))

  ;; old:  Equation_entry_Subscore
  ;; old:  Explicit_Equation_Entry_Subscore
  ;; old:  what is the difference between these?
  ;; old:  Given_Equation_Entry_Subscore
  ;; old:   Explicit_Given_Equation_entry_Subscore
  ;; old:  what is the difference between these?
  
  ;; old:   Body_Entry_Subscore10
  ;; old:   Axis_Entry_Subscore

    ))

(defun score-hint-request (hint-assoc entries)
  (when *debug-grade* ;debug print
    (format webserver:*stdout* "***score-hint-request with ~A~%    props: ~A~%" 
	    hint-assoc
	    entries))
  )

(defun update-grade-status (entries status)
  (dolist (entry entries)
    (let ((grade (SystemEntry-graded entry)))
      (setf (graded-status grade) status)
      ;; Once ignored has been set, it is always set.
      (unless (graded-ignore grade)
	(setf (graded-ignore grade) **checking-entries**)))))


(defstruct tally
  (score 0)
  (possible 0)
)

(defun grade-sysentry (tally sysent)
  "Calculate contribution of individual SystemEntry to score."
  (let* ((graded (SystemEntry-graded sysent))
	 (weight (graded-weight graded)))
    (unless weight (warn 'webserver:log-warn 
			 :tag (list 'graded-weight-missing (SystemEntry-prop sysent))
			 :text "Entry missing grading weight.")
	    (return-from grade-sysentry))
    (unless (graded-possibilities graded) 
      (warn 'webserver:log-warn 
	    :tag (list 'graded-possibilities-missing (SystemEntry-prop sysent))
	    :text "Entry missing grading possibilities.")
      (return-from grade-sysentry))
    (unless (or (graded-ignore graded) (graded-optional graded))
      (incf (tally-possible tally) weight)
      (when (eql (graded-status graded) +correct+)
	(let ((n (graded-possibilities graded))
	      (j (length (graded-incorrects graded))))
	  (incf (tally-score tally) 
		(* weight 
		   (weight-correct-after-failures j n))))))))
  

(defun calculate-score ()
  ;; Loop through grade **grade**
  ;;     test if entry is required
  ;;        accumulate points earned and weight.
  ;; Loop through systementries not in solutions 
  ;;    test if entry is required and not to be ignored
  ;;       accumulate points earned and weight.
  ;; Loop through problem solutions
  ;;   loop through systementries
  ;;     test if entry is required and not to be ignored
  ;;        accumulate points earned and weight.
  ;; 
  ;; Need to think about case where step is required in one
  ;; solution but optional in another.  Is this possible?
      

  ;; old:  NSH_BO_Call_Count
  ;; old:  WWH_BO_Call_Count

  ;; old:  this was a terrible way to think of grading:
  ;; old:  Correct_Entries_V_Entries
  ;; old:  Correct_Answer_Entries_V_Answer_Entries
  
  ;;  For testing, just loop through systementries.
  (dolist (sysent *sg-entries*)
    (when (and *debug-grade* ;debug print
	       (or t (graded-incorrects (systementry-graded sysent))
	      (graded-status (systementry-graded sysent))))
	(format webserver:*stdout* "*** Grade~@[ global~*~] ~A:~%    ~A~%"
		(null (SystemEntry-in-Sg-Solutions sysent))
		(systementry-prop sysent)
		(systementry-graded sysent))))

  (let (best-score best-possible best-number 
	(global-score (make-tally)))

    ;; Loop through non-solution SystemEntries
    (dolist (sysent *Sg-Entries*)
      (unless (SystemEntry-in-Sg-Solutions sysent)
	(grade-sysentry global-score sysent)))

    ;; Loop through solutions
    (dolist (soln *sg-solutions*)
      (let ((this-score (make-tally)))
	(dolist (sysent (sgsol-entries soln))
	  ;; Sanity test.
	  (unless  (SystemEntry-in-Sg-Solutions sysent)
	    (Warn "SystemEntry in *sg-solutions* without mark, ~A" 
		  (SystemEntry-prop sysent)))	  
	  (grade-sysentry this-score sysent))

	(when (or (null best-score) (null best-possible)
		  (> (* (+ (tally-score this-score) 
			   (tally-score global-score)) 
			best-possible) 
		     (* (+ (tally-possible this-score) 
			   (tally-possible global-score))
			best-score)))
	  (setf best-score (tally-score this-score))
	  (setf best-possible (tally-possible this-score))
	  (setf best-number (sgsol-num soln)))))

    (when *debug-grade* ;debug print
      (format webserver:*stdout* "*** Global grade ~A/~A.  "
	      (tally-score global-score) 
	      (tally-possible global-score))
      (format webserver:*stdout* "Best grade ~A/~A from solution ~A~%"
	    best-score best-possible best-number))

    ;; Need to handle case where best-score is never set
    ;; by going through solutions.
    (let ((denominator (+ (or best-possible 0) 
			  (tally-possible global-score))))
      (if (> denominator 0)
	  (/ (+ (or best-score 0) (tally-score global-score))
	     denominator)
	  0  ;zero denominator
	  ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;             Power law weighting of answers after incorrect attempts.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assume there are n possible responses and the student gets the correct 
;; response after j failures.  Let w_j be a multiplicative factor for
;; the score.  The student should get full credit if they get it 
;; right on the first attempt:  w_0=1.  They don't get any credit
;; were they to take n distinct attempts (this is impossible):  w_n=0.
;; 
;; For the case of the student randomly choosing answers, or getting
;; a bottom-out hint then we want them to get, an average grade of g
;; (+random-average-score+ in the lisp).  If the student is guessing,
;; the chance of getting the right answer after j failures is 1/n.
;; Thus we want:
;;               g = \sum_{j=0}^{n-1} w_j/n  (1)
;; In the extreme cases,
;;       g=1/n => w_0=1, w_i=0, i>0
;;       g=1 => w_j=1
;; For g outside this range [1/n,1], we simply use the end cases.
;; 
;; Consider the power law model, w_j = (1-j/n)^a, a \in (0,\infty).  
;; Eqn. (1) gives us:
;;              ng = n^{-a} \sum_{j=1}^n j^a
;;                 = \frac{n}{a+1} + 1/2 + O(n^{-a}) + O(1/n)
;; Thus, we have
;;              a = \frac{1}{g-\frac{1}{2n}} - 1 + O(n^{-a-1}) + O(n^{-2})
;; The power correction is larger when a is small or g -> 1.

;; The choice of g is arbitrary and would represent the value
;; for a student going through a worked example, step by step.
;; This might be something that we might want to vary by section.

(defun weight-correct-after-failures (j n)
  (let ((g *random-average-score*))
    (cond ((>= g 1) 1)
	  ((<= (* g n) 1) (if (= j 0) 1 0))
	  (t
	   (let ((a (+ (/ 1 (- g (/ 1 (* 2 n)))) -1)))
	     (expt (/ (- n j) n) a))))))

(defun test-weight-correct (n g)
  "Debug function to verify sum is approximately equal to g."
  (let ((*random-average-score* g))
    (/ (loop for j from 0 to n
	  sum (weight-correct-after-failures j n)) n)))
