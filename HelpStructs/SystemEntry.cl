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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SystemEntry.cl
;; Collin Lynch
;; 4/9/2001
;;
;; This file defines the System Entry struct including
;; IO operatons for it and indexing operations.
;; Tjis struct is used by the help system to compare entries.
;;

(defvar **Debug-prematurity-tests** NIL "Debugging flag.")

(defconstant +correct+ 'Correct "Correct interpretation.")
(defconstant +forbidden+ 'Forbidden "Forbidden path.")
(defconstant +dead-path+ 'Dead-Path "Dead path interpretation.")
(defconstant +incorrect+ 'Incorrect "The Entry has no interpretation.")

(defvar *SG-Entries* () "The System entries from the bubblegraph.")
(defvar *SG-Eqns* () "Equation list with eqn-index->entry mappings.")
(defvar *help-last-entries* () "List of SystemEntries for which the termination of the most recent help sequence was constructed.  Used to determine assignment of blame for bottom-out hint.")

;; dynamically bound to correct entry if known before calling turn
;; generator, so generating functions can use it. Generating functions
;; should know if error handlers set a correct entry or not.
(defvar *correct-entry* NIL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The constants below are used only by the Help Solutiongraph 
(defconstant +premature-entry+ 'Premature-Entry 
  "Reflects that the entry is premature.")
(defconstant +premature-subst+ 'Premature-Subst 
  "Reflects (for eqn entries only) a premature substitution of values.")


(defstruct (SystemEntry (:print-function print-SystemEntry))
  Index	   ;Index in the entry list.
  Prop     ;The Entry proposition for this node.
  children ;Children props, through inheritance.
  Sources  ;Cognitive steps that produced this.
  State    ;State of the system.  This is a private element:
  ;; it is only read via function SystemEntries->State
  ;; and only set via function make-SystemEntry
  Prereqs  ;The set of sets of system prerequisites 
  ;; that must be satisfied to produce this.
  CogLoad  ;The cognitive load of the systementry.
  Entered  ;A list of student entries that have entered
  ;; this system entry. If nil, it has not been entered.
  ;; For Multiple-choice, set if correct, nil if wrong or not attempted.
  ;; Set via StudentEntry-Cinterps when student entry is correct.
  ;; For answer boxes, set if correct, nil if wrong or empty.
  in-Sg-Solutions ;whether this entry is listed in *Sg-Solutions*
  model    ;Model sentence.
  optional ;level of optionality, currently Boolean; see Bug #972
  (graded (make-graded))  ;Graded object.
  )

(defun print-SystemEntry (Entry &optional (Stream t) (level 0))
  "Print out the system entry."
  (pprint-indent :block Level Stream)
  (format Stream "[SystemEntry: ~A ~S ~A ~A ~A opt:~A]~%" 
	  (SystemEntry-Index Entry) (SystemEntry-Prop Entry) 
	  (SystemEntry-State Entry) (if (SystemEntry-Entered Entry) t nil)
	  (SystemEntry-CogLoad Entry)
	  (SystemEntry-optional Entry)))

(defun print-full-SystemEntry (Entry &optional (Stream t) (level 0))
  "Print out the system entry."
  (pprint-indent :block Level Stream)
  (format Stream "[SystemEntry: ~A~%" (SystemEntry-Index Entry))
  (pprint-indent :block Level Stream)
  (format Stream "  Prop:    ~S~%" (SystemEntry-Prop Entry))
  (pprint-indent :block Level Stream)
  (format Stream "  Sources: ~A~%" (SystemEntry-Sources Entry))
  (pprint-indent :block Level Stream)
  (format Stream "  State:   ~A~%" (SystemEntry-State Entry))
  (pprint-indent :block Level Stream)
  (format Stream "  CogLoad:   ~A~%" (SystemEntry-CogLoad Entry))
  (pprint-indent :block Level Stream)
  (format Stream "  Prereqs: ~A]~%" (SystemEntry-Prereqs Entry)))


(defun SystemEntries-PropEqualp (X Y)
  "Return t iff X and Y are equalp in all elements save state and Sources."
  ;; need unify to handle keywords properly
  (unify (SystemEntry-Prop X) (SystemEntry-Prop Y)))

(defun SystemEntry-Sets-propequalp (X Y)
  "Are sets X and Y Prop Equalp."
  (loop for E in X
      unless (find E Y :test #'SystemEntries-PropEqualp)
      return nil
      finally (return t)))


(defun merge-duplicate-systementries (Ents)
  "Merge the duiplicate system entries in the list."
  (when Ents
    (let ((R (list (car Ents))) (tmp))
      (loop for E in (cdr Ents)
	  when (setq tmp (find E R :test #'SystemEntries-PropEqualp))
	  do (merge-SystemEntries E tmp)
	  else do (push E R))
      R)))


(defun merge-SystemEntries (X Y)
  "Merge SystemEntry X into SystemEntry Y."
  (when (not (SystemEntries-PropEqualp X Y))
    (format t "Incompatible System Entries for merge ~%~A~%~A~3%" X Y)
    (print-full-systementry X)
    (print-full-SystemEntry Y))
  
  (merge-SystemEntry-States X Y)
  (merge-SystemEntry-Sources X Y)
  (merge-SystemEntry-optionality X Y)
  (merge-SystemEntry-Prereqs X Y))

(defun merge-SystemEntry-Sources (X Y)
  "Merge the SystemEntry source lists."
  (setf (SystemEntry-Sources Y)		; Merge the source lists.
    (append (SystemEntry-Sources X) 
	    (SystemEntry-Sources Y)))
  
  (dolist (O (SystemEntry-Sources X))	; set the do pointers.
    (when (not (csDo-p O)) 
      (error "Non Do supplied as a source ~A" O))
    (setf (csdo-Entries O) 
      (cons Y (remove X (csdo-Entries O))))))

(defun merge-SystemEntry-States (X Y)
  "Merge the System Entry States."
  (cond ((equalp (SystemEntry-State Y) +forbidden+)
	 (if (equalp (SystemEntry-State X) +correct+)
	     (setf (SystemEntry-State Y) +correct+)))
	((equalp (SystemEntry-State Y) +dead-path+)
	 (if (equalp (SystemEntry-State X) +forbidden+)
	     (setf (SystemEntry-State Y) +forbidden+))
	 (if (equalp (SystemEntry-State X) +correct+)
	     (setf (SystemEntry-State Y) +correct+)))))


(defun merge-SystemEntry-prereqs (X Y)
  "Merge the prerequisites for X and Y."
  (cond ((SystemEntry-Prereqs Y)
	 (dolist (P (SystemEntry-Prereqs X))
	   (when (not (member P (SystemEntry-Prereqs Y) 
			      :test #'SystemEntry-sets-PropEqualp))
	     (push P (SystemEntry-Prereqs Y)))))
	(t (setf (SystemEntry-Prereqs Y) (cons nil (SystemEntry-Prereqs X))))))
  
(defun merge-SystemEntry-optionality (X Y)
  "Merge the optionality for X and Y."
  ;; Should handle cases of "allowed", "preferred", Bug #972.
  (when (null (SystemEntry-optional x))
    (setf (SystemEntry-optional y) nil)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sg-subst-preconds-syents
;; In order to deal with inconsistencies that arise from the 
;; mergeing of states the system here cycles through the preconds
;; of each entry in the index ensuring that they point to other
;; entries in the index.

(defun subst-prereqs-sysents (Entries Index)
  "Cycle through the list of sysents ensuring consistency."
  (dolist (E Entries)
    (setf (SystemEntry-Prereqs E)
      (loop for Pr in (SystemEntry-Prereqs E)
	  collect (loop for P in Pr
		      collect (find-SystemEntry (SystemEntry-Prop P)
					    Index))))))


;;
;;   Find systementry that matches a given prop
;;
(defun find-SystemEntry (Prop &optional (Entries *sg-entries*))
  (find Prop Entries
	:key #'SystemEntry-Prop
	:test #'unify))

;;----------------------------------------------
;; State testing code.


(defun systementry-equationp (Entry)
  "Is the systementry an equation?"
  (eqn-prop-p (SystemEntry-Prop Entry)))

(defun match-systementry->eqn (Entry Eqns)
  "Find a matching eqn for the systementry."
  (when (systementry-equationp Entry)
    (find (cadr (SystemEntry-Prop Entry)) Eqns
	  :key #'Eqn-Algebra :test #'equalp)))


;; (match-systementry->eqn-type sys-en (problem-eqnindex *cp*))
(defun match-systementry->eqn-type (Entry Eqns)
  "Get the eqn type of the matching systementry."
  (let ((eqn (match-systementry->eqn Entry Eqns)))
    (when Eqn (eqn-type Eqn))))

;;; In some cases we need to test whether or not the systementry
;;; is implicit I.E. optional.  This code does that by fetching
;;; the entry's eqn type and returning t iff it matches
;;; 'Implicit-eqn'  Ugly fucking hack.
(defun Systementry-implicit-eqnp (Entry)
  (eq 'Implicit-eqn (car (Systementry-Prop Entry))))


;;; When what we want out of a systementry is it's algebraic
;;; form that can be obtained directly from the prop provided
;;; that it is an eqn entry.  This function does that.
(defun get-eqn-systementry-algebra (Entry)
  (when (SystemEntry-Equationp Entry)
    (cadr (SystemEntry-Prop Entry))))

;;; There are two classes of systementry prematurity.
;;; The first class is that of premature entries.  In 
;;; This case there exist prerequisties of the entry
;;; that have not themseleces been entered.    
(defun SystemEntry-PrematureP (Entry problem)
  "Return t iff prerequisite entries of this entry have not yet been entered."
  (when (SystemEntry-Prereqs Entry)
    (let ((r (test-systementry-prereqs Entry))) 
      (format t "****************************************************************************~%")
      (format t "Match results:~% ~W~%"
	      (match-systementry->eqn-type entry (problem-eqnindex problem)))
      (when **Debug-Prematurity-Tests**
	(format t "All Prereqs: ~A~%Unfinished Prereqs:~A~%" (SystemEntry-Prereqs Entry) r))
      (not (member nil R)))))

(defun test-systementry-prereqs (Entry)
  "Remove the unfinished prereqs from the entry."
  (loop for P in (SystemEntrY-Prereqs Entry)
      collect (remove-if #'SystemEntry-Entered P)))

       
(defun SystemEntries-PrematureP (Entries problem)
  "Test if the list of system entries is premature."
  (when **debug-Prematurity-tests**
    (format t "Testing for prematurity ~%~A~%" Entries)) 
  
  (let ((r (loop for E in Entries
	       when (SystemEntry-PrematureP E problem)
	       return it)))

    (when **Debug-Prematurity-tests**
      (format t "Result: ~A ~%" R))
    
    R))

;;; The second class of prematurity is that of substitution prematurity.
;;; In this instance equation entries are tested to determine if the 
;;; student has substituted numbers in before they have defined all
;;; values.  The mechanics of this have nto yet been defined so the 
;;; system is stubbed for now.
(defun systementries-premature-substp (Entries)
  "Test whether the eqn entries represent premature substitution."
  (loop for E in Entries
      when (SystemEntry-premature-substp E)
      return it))

(defun systementry-premature-substp (Entry)
  "Stub as we are unsure of the mechanics of this."
  (declare (ignore Entry))
  nil)

(defun SystemEntries->State (Interp)
  "Get the correctness of the interpretation."
  (let ((s +correct+))
    (dolist (E Interp)
      (cond ((eq (SystemEntry-State E) +forbidden+)
	     (setq s +forbidden+))
	    ((and (eq (SystemEntry-State E) +dead-path+)
		  (not (eq s +forbidden+)))
	     (setq s +dead-path+))))
    s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               SystemEntries arranged according to solution.
;;;;
;;;;
;; ================================================
;; Solution struct.  

(defstruct sgsol
  Num
  Entries)

(defvar *SG-Solutions* () "The set of solutions to be done, type sgsol.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
