;; interpret-equation.cl -- routines to handle getting interpretations of equations
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (cl) <collinl@pitt.edu>
;; Modified:
;;  19 June 2001 - (lht) -- created
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun interpret-equation (se		;struct StudentEntry
                           &optional (location 'equation)) ; vs 'answer if answer-box entry
  (sg-match-StudentEntry se) ;; fills in PossibleCInterps
  (let* ((interps (StudentEntry-PossibleCInterps se))
	 (correct-or-premature (find-all-correct-interpretations interps location))
	 (correct1 (find-most-cognitive-interpretation (car correct-or-premature)))
	 (premature1 (find-most-cognitive-interpretation (second correct-or-premature)))
	 (deadpath (find-all-interpretations **Dead-Path** interps))
	 (deadpath1 (find-most-cognitive-interpretation deadpath))
	 (forbidden (find-all-interpretations **Forbidden** interps))
	 (forbidden1 (find-most-cognitive-interpretation forbidden))
	 (nogood (find-all-interpretations **Nogood** interps))
	 (nogood1 (find-most-cognitive-interpretation nogood))
	 (shortest (find-most-cognitive-interpretation (get-all-interpretations interps)))
	 (result nil))
    (cond
     ((null interps)
      (setf (StudentEntry-CInterp se) nil)
      (warn "interpret-equation: can't find interpretations for ~A" se)
      (setf (StudentEntry-State se) **Incorrect**)
      (setf result (make-red-turn :id (StudentEntry-id se))))
     (correct1
      (setf (StudentEntry-CInterp se) correct1)
      (setf (StudentEntry-State se) **Correct**)
      (setf result (make-green-turn :id (StudentEntry-id se))))
     (deadpath1
      (setf (StudentEntry-CInterp se) deadpath1)
      (setf (StudentEntry-State se) **Dead-Path**)
      (setf result (chain-explain-more **Dead-Path-Help**)))
     (forbidden1
      (setf (StudentEntry-CInterp se) forbidden1)
      (setf (StudentEntry-State se) **Forbidden**)
      (setf result (chain-explain-more **Forbidden-Help**)))
     (premature1
      (setf (StudentEntry-CInterp se) premature1)
      ;; changed to treat as correct, but with a warning message -- AW
      ;; (setf (StudentEntry-State se) **Premature-Entry**)
      (setf (StudentEntry-State se) **Correct**)
      (setf result (get-premature-msg se))) ; now returns green + message turn
     (nogood1
      (setf (StudentEntry-CInterp se) nogood1)
      (setf (StudentEntry-State se) **NOGOOD**)
      (setf result (chain-explain-more **NOGOOD-Help**)))
     (t
      (warn "interpret-equation: no interpretation for ~A" 
	    (StudentEntry-ParsedEqn se))
      (setf (StudentEntry-CInterp se) shortest)
      (setf (StudentEntry-State se) **Correct**)
      (setf result (make-green-turn :id (StudentEntry-id se)))))
    result))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun find-all-interpretations (name interps)
  (let ((result nil))
    (dolist (obj interps)
      (if (equal name (car obj))
	  (setf result (append result (list (cdr obj))))))
    result))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; New constraint-based prematurity checking for equation entries:
;;
;; Naming conventions used in this section:
;;  eqn = eqn struct used in problem equation index
;;  syseqn = system entry struct for an equation
;;  eqinfo = equation type information struct in ontology
;;  interp = an interpretation = plain list of syseqn's w/o correctness tag
;;  cinterp = candidate interp, car is correctness tag, cdr is interp


;; predicates of eqns (equation index entries):
(defun major-eqn-p (eqn)
  "true if eqn struct represents a major principle" 
  ;; includes many that are actually definitions
  (let ((eqinfo (lookup-expression->PSMClass (eqn-exp eqn))))
	 (and eqinfo (eq (PSMClass-complexity eqinfo) 'major))))

(defun definition-eqn-p (eqn)
"true if eqn struct represents a definition"
   (let ((eqinfo (lookup-expression->PSMClass (eqn-exp eqn))))
	 (and eqinfo (eq (PSMClass-complexity eqinfo) 'definition))))

(defun compo-eqn-p (eqn)
"true if eqn struct represents a component-form vector equation"
   (eq (first (eqn-exp eqn)) 'compo-eqn))

(defun given-eqn-p (eqn)
"true if eqn struct specifies a given value equation"
   (eq (eqn-type eqn) 'given-eqn))

(defun known-angle-value-eqn-p (eqn)
"true if eqn struct is an implicit equation giving value of an orientation or angle between variable"
   (and (eq (eqn-type eqn) 'implicit-eqn)
        (or (eq (first (eqn-exp eqn)) 'angle-between)
	    (unify (eqn-exp eqn) '(dir ?vector)))))

(defun eqn-English (eqn)
"return English name for eqn index entry, NULL if not found."
   (nlg-equation (eqn-exp eqn)))

;; We want to allow implicit removal of zero values at any time, so we 
;; remove var=0 equations from interpretations before further testing 
;; for illicit combinations. 
;; Following tests whether equation sets a value to zero.
;; Note this doesn't care whether the zero value is "given" or not.
;; This also doesn't propagate "obviously" inherited zero values, e.g
;; v = 0 GIVEN, KE = 0.5*m*v^2   => KE = 0
;; h = 0 GIVEN, Ug = m*g*h => Ug = 0

(defun zero-eqn-p (eqn)
"true if eqn assigns zero to a variable"
  (let ((lhs (second (eqn-algebra eqn)))
        (rhs (third (eqn-algebra eqn))))
   (and (symbolp lhs)         
        (or (and (dimensioned-numberp rhs)   ; in kb/physics-funcs.cl
	         (= (second rhs) 0))
            (and (numberp rhs) 
                 (= rhs 0))))))

;; We also want to allow most substitutions of equivalent variables licensed
;; by "allowed" identity equations. Note an identity may come out
;; (= netWork (+ W1)) if rhs is sum that has only one arg in this problem, and
;; we want to allow that in this case.  More complicated forms that could 
;; simplify to identities are not detected here.  (Ex: Kirchoff's Loop rule 
;; for a simple circuit with one battery and one resistor) Probably OK,
;; if the form is not simple A = B, probably want to require explicit.
(defun identity-eqn-p (eqn)
"true if eqn is an identity"
   (let ((lhs (second (eqn-algebra eqn)))
         (rhs (third (eqn-algebra eqn))))
     (and (symbolp lhs)        ; V1 = 
          (or (symbolp rhs)    ;      V2
              (and (listp rhs) ; or   (+ V2)
	           (= (length rhs) 2)
		   (eq (first rhs) '+)
		   (symbolp (second rhs)))))))


(defun combinable-identity-p (eqn)
"true if eqn is an identity allowed to be combined"
  (and (identity-eqn-p eqn)
       (not (required-identity-p eqn)))) ; in ontology.cl


; map sysentries to eqn:
(defun syseqn->eqn (syseqn)
"return eqn info for given system equation entry in current problem"
 (match-systementry->eqn SysEqn (problem-eqnindex *cp*)))

(defun given-eqn-entry-p (syseqn)
"true if system equation entry is step of writing a given equation"
  (given-eqn-p (syseqn->eqn syseqn)))

(defun syseqn-English (syseqn)
"map system equation to its English string name"
   (eqn-English (syseqn->eqn syseqn)))

; for dealing with interpretations = sets of syseqns:
(defun get-nonzero-eqns (interp)
"return list of non-zero equations in interp"
   (remove-if #'(lambda (syseqn) 
                  (zero-eqn-p (syseqn->eqn syseqn)))
	      interp))

; Identities and zero equations will be dubbed "trivial". User may
; permissibly combine them with fundamental equations in their heads,
; to drop obviously zero terms or use equivalent variables.
; Following gets all the other equations in an interp after these
; "trivial" (i.e. allowed to be combined) eqns are removed.
; Note! this function is used by grading tests to detect required eqns:
; no "trivial" equation can be required explicit. 
(defun trivial-eqn-p (eqn)
   (or (zero-eqn-p eqn)
       (combinable-identity-p eqn)))

(defun trivial-syseqn-p (syseqn)
   (trivial-eqn-p (syseqn->eqn syseqn)))

(defun get-nontrivial-eqns (interp)
   (remove-if #'trivial-syseqn-p interp))

; We will now allow equations marked "definitions" to be combined with principles.
; This is intended for the small number of definitions used in conservation laws,
; so that e.g., conservation of energy can be written out in terms of definition of 
; kinetic and potential energy, or not. 
; Note: This could lead to problems if many equations are tagged as definitions. 
; Note also: this allows substitutions of definitions into other principles,
; e.g. substituting 2*KE/m for vf^2 in vf^2 = v0^2 + 2*a*d. But this is unlikely to
; arise since kinetic energy will not be mentioned in the solution to such a problem.
;
; This is distinct from "trivial" equation predicate above since definitions are not "trivial". 
; Not clear if this matters -- depends on how "trivial-syseqn-p" is used by grading system.
; Should be OK because definitions are not considered "major" so never required explicit.
;
; We also allow implicit equations giving known angle values to be combined. This should
; allow students to skip defining angle variable in W = F*d(thetaFd) if angle between is known,
; or to write it as W = F*d(thetaF - thetaD) if both are known.
(defun combinable-eqn-p (eqn)
   (or (trivial-eqn-p eqn)
       (definition-eqn-p eqn)
       (known-angle-value-eqn-p eqn)))

(defun combinable-syseqn-p (syseqn)
   (combinable-eqn-p (syseqn->eqn syseqn)))
     
(defun get-noncombinable-eqns (interp)
    (remove-if #'combinable-syseqn-p interp))

; Test whether a given system equation entry has been entered explicitly
; Look for a studententry with a singleton interpretation equal to this
; system entry or combined acceptably with others, e.g. combined with zero 
; givens so as to drop zero-valued terms from eqns.
(defun explicit-entry-of (studEntry syseqn)
"true if given studEntry is explicit (enough) entry of syseqn"
   (let ((interp (StudentEntry-Cinterp studEntry)))
     (and (member syseqn interp :test #'equal)
	  (or ; or only others are combinable equations 
	      (null (get-noncombinable-eqns (remove syseqn interp :test #'equal)))
	      ; or it combines given magnitude with projection 
              (and (given-eqn-entry-p syseqn)
	           (allowed-compo-mag-combo interp))))))

(defun studEqnEntry-p (studEntry)
"true if given student entry is an equation entry"
   (eq 'eqn (first (StudentEntry-Prop studEntry))))

(defun entered-explicitly (syseqn &optional (EntryList *StudentEntries*)) 
"true if syseqn is explicitly entered somewhere in given set of entries (default all entries)"
  (some #'(lambda (studEntry) 
            (or (explicit-entry-of studEntry syseqn)
		; also check among dangling entries for given equations
	        (some #'(lambda (ge) (explicit-entry-of ge syseqn))
		      (studentEntry-GivenEqns studEntry))))
        EntryList))

;; For use when detecting premature substitution of numerical values:
;; Instructors also want to allow implicit combination of given magnitudes 
;; magV = K units with projection equation V_x = V cos (N deg - M deg) 
;; to get V_x = +/- K units in case where vector lies along an axis. 
;; Here we use a cheap but easy-to-code test which just allows *any* 
;; combination of a given vector magnitude and any projection equation.  
;; This will miss constraint violation where they have used value of 
;; sin or cos function to get a component value from a magnitude.  
;; It is unlikely they will do this in their heads, though some might 
;; use a calculator.  It's tolerable if we miss some violations as long 
;; as we allow what needs to be allowed.

(defun given-mag-eqn-p (eqn)
"true if eqn states given value of a vector magnitude"
   (and (given-eqn-p eqn)
	; eqn-exp for given-eqns is quantity expression
	(eq (first (eqn-exp eqn)) 'mag))) 

(defun projection-eqn-p (eqn)
"true if eqn is a projection"
    (or (eq (first (eqn-exp eqn)) 'projection) ; as sub-equation within a vector psm
        (eq (first (eqn-exp eqn)) 'proj)))     ; as psm-level eqn in compo-form solution
     
(defun allowed-compo-mag-combo (interp)
"true if interp is allowed combination of projection along axis and given magnitude value"
  (and (= (length interp) 2)
       (some #'(lambda (syseqn) 
                   (given-mag-eqn-p (syseqn->eqn syseqn)))
	     interp)
       (some #'(lambda (syseqn) 
                   (projection-eqn-p (syseqn->eqn syseqn)))
	     interp)))


; the prematurity constraints: 

; premature substitution of givens:
; Note that even after student fixes by replacing numbers with variables,
; symbolic equation may still involve combination of fundamental equations.
; This doesn't depend on whether non-given equation combined with is major or not, 
; so effectively it is treating given equations like required-explicit principles,
; requiring them to be entered explicitly by themselves, but for few exceptions.
(defun is-premature-substitution-p (interp)
"true if interp combines non-zero given values with a non-entered equation"
 (let ((nz-eqns (get-nonzero-eqns interp)))
  (and (some #'given-eqn-entry-p nz-eqns)
       (some #'(lambda (syseqn)
                   (and (not (given-eqn-entry-p syseqn))
		        (not (entered-explicitly syseqn))))
	     nz-eqns)
       (not (allowed-compo-mag-combo interp)))))

;; Premature combination of equations:
;; We treat compo-form vector equations separately from other major equations
;; only so we can give a variant message that explicitly mentions component form
;; (Student may have written a true symbolic equation using only magnitudes, e.g.
;; so we want hint to remind them what is required for book form vector principles.)
;; Note: must test for this *first* because more general subsequent test
;; will also succeed on compo-form vector equations
;; Note: test is order-independent, only requires that fundamental equation exist
;; *somewhere* in their solution. 

(defun is-premature-before-compo-eqn-p (interp)
"true if interp combines non-explicitly-entered vector compo eqn with non-trivial eqn"
  (let ((nz-eqns (get-noncombinable-eqns interp)))
    (and (cdr nz-eqns)  ;; more than 1 eqn left in interp
         (some #'(lambda (syseqn)
		     (and (compo-eqn-p (syseqn->eqn syseqn))
			  (major-eqn-p (syseqn->eqn syseqn))
                          (not (entered-explicitly syseqn))))
	        nz-eqns)))) 

(defun is-premature-before-major-eqn-p (interp)
"true if interp combines non-explicitly-entered major eqn with non-trivial eqn"
  (let ((nz-eqns (get-noncombinable-eqns interp)))
    (and (cdr nz-eqns)  ; more than 1 eqn left in interp
         (some #'(lambda (syseqn)
		     (and (major-eqn-p (syseqn->eqn syseqn))
                          (not (entered-explicitly syseqn))))
	       nz-eqns)))) 

(defun get-needed-eqns (interp)
"return list of systentries for major eqns in interp not entered explicitly"
 (remove-if-not #'(lambda (syseqn)
                     (and (major-eqn-p (syseqn->eqn syseqn))
                          (not (entered-explicitly syseqn))))
                (get-noncombinable-eqns interp))) 

(defun get-needed-eqn-names (interp)
"return list of English forms for missing explicit eqns in interp"
  (mapcar #'syseqn-English (get-needed-eqns interp)))

;; switch -- whether to enforce prematurity constraints on equations
(defvar **Check-Eqn-Constraints**  T) ; off until we work them out


(defparameter **premature-predicates** '(
    ; For now, disable premature substitution, just test explicitness
    ; NB: must also comment out of get-premature-msg
    ;is-premature-substitution-p
    is-premature-before-compo-eqn-p
    is-premature-before-major-eqn-p
 ))

(defun is-premature-p (interp)
"true if interp is premature according to some test in **premature-predicates**"
(and **Check-Eqn-Constraints**
      (some #'(lambda(predicate) 
                 (apply predicate (list interp)))
             **premature-predicates**)))

(defun find-all-correct-interpretations (cinterps &optional (location 'equation))
"collect correct interpretations, splitting out those violating prematurity constraints"
; returns pair of (correct-list premature-list), each element a list of interps
 (let ((correct-result nil)
	(premature-result nil))
    (dolist (cinterp cinterps)
      (when (equal **correct** (car cinterp))
	 (if (and (eq location 'equation) ; don't test on answer box entries
	          (is-premature-p (cdr cinterp)))
	     (setf premature-result (append premature-result (list (cdr cinterp))))
           (setf correct-result (append correct-result (list (cdr cinterp)))))))
    ;(when correct-result (format t "Correct Interpretations:~%~W~%" correct-result))
    ;(when premature-result (format t "~%Correct but Premature:~%~W~%" premature-result))
    (list correct-result premature-result)))


; ! Urg, we must run tests *again* after interpretation choice to fetch message. Should 
; recode to be able to associate msg with entry at time we detect constraint violoation.
; Last hint lists principle(s) that should be explicit, so students get
; some idea of what we're looking for.

(defun get-premature-msg (se)
  "return appropriate hint sequence for equation entry interpreted as premature"
  ; some message text in HelpMessages.cl
 ; (assert (eq (studententry-state se) **Premature-Entry**))
 (let* ((interp (studententry-cinterp se))
        (missing (get-needed-eqn-names interp)))
  (cond 
    ; For now, don't test premature substitution
    ; ((is-premature-substitution-p interp)
    ;    (chain-explain-more-green **Premature-Subst-help**))
    ; if missing exactly one and its a compo equation, mention component form in case that is their problem.
    ; !!! actually could give this message whenever *all* missing are compo-eqns
    ((and (null (cdr missing)) 
          (is-premature-before-compo-eqn-p interp)) 
       (chain-explain-more-green (list 
	   (format NIL "Although this equation is correct, you have not displayed a fundamental vector principle written in component form on a line by itself.")
           "It is good practice to identify the fundamental vector principles you are using by writing them purely symbolically in component form before combining them with other equations or given values.  Select \"Review Physics Equations\" on the Help menu to view a list of principles and their standard forms."
	   (format NIL "A good solution would include ~A, written as ~:[a separate equation~;separate equations~]." (conjoined-names missing) (cdr missing)))))
       
    (missing ; better have at least one missing to mention
       (chain-explain-more-green 
         (list 
          (format NIL "Although this equation is correct, you have not displayed a fundamental principle being used in symbolic form all by itself.")
           "It is good practice to identify the fundamental principles you are using by writing them purely symbolically in standard form before combining them with other equations or given values.  Select \"Review Physics Equations\" on the Help Menu to view  a list of principles and their standard forms."
	   (format NIL "A good solution would include ~A, written as ~:[a separate equation~;separate equations~]." (conjoined-names missing) (cdr missing)))))
       
    (T ; didn't find missing! shouldn't happen
       (format T "get-premature-msg called but couldn't find missing equations!~%")
       (make-green-turn  :id (StudentEntry-id se))))))


;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun calculate-cognitive-load (interps)
  (let ((sum 0))
    (dolist (obj interps)
      (if (SystemEntry-p obj)
	  (setf sum (+ sum (SystemEntry-CogLoad obj)))))
    sum))

(defun find-most-cognitive-interpretation (interps)
  (if (consp interps)
      (let* ((result (car interps))
	     (load (calculate-cognitive-load result)))
	(dolist (obj interps)
	  (let ((newload (calculate-cognitive-load obj)))
	    (cond
	     ((< newload load)
	      (setf load newload)
	      (setf result obj)))))
	result)
    interps))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun get-all-interpretations (interps)
  (let ((result nil))
    (dolist (obj interps)
      (setf result (append result (list (cdr obj)))))
    result))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun chain-explain-more (messages)
  (if messages
      (if (= (length messages) 1)
	  (make-dialog-turn (first messages) nil)
	(make-dialog-turn (first messages) 
			  'explain-more
			  :responder
			  #'(lambda (response)
			      (if (equal response 'explain-more)
				  (chain-explain-more (rest messages))))))))
	
;; build a color-green turn with given message list
(defun chain-explain-more-green (messages)
  (if messages
      (if (= (length messages) 1)
	  (make-green-dialog-turn (first messages) nil)
	(make-green-dialog-turn (first messages) 
			  'explain-more
			  :responder
			  #'(lambda (response)
			      (if (equal response 'explain-more)
				  (chain-explain-more (rest messages))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
