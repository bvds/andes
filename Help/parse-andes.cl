;; parse-andes.cl -- andes specific parse and grammar routines
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Kurt VanLehn  (kvl) <VanLehn@cs.pitt.edu>
;;  Collin Lynch (c?l) <CollinL@pitt.edu>
;; Modified:
;;  4 June 2001 - (lht) created
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
;;  12 July 2001 (kvl) modified to fill ErrInterp slot on student entries for many types of errors
;;  5 July 2003 (c?l) removing depreciated definition of and calls to replace-greek.
;;  12 July 2003 (c?l) added declarations:
;;   ignored some instances of unused variables to suppress warnings.
;;   commented out setting of Result and Tmp in Bad-Vars-In-Answer as they were unused.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is called when the equation to be looked up appeared in the
;; answer box.  The only real difference between this and
;; do-lookup-equation-string is the special case kludge covered above.
;; Called from Entry-API.
(defun do-lookup-equation-answer-string (eq id)
  (do-lookup-equation-string (trim-eqn eq) id 'answer))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-eqn-string -- check correctness of a student equation entry
;; argument(s):
;;  eqn-string: the equation as the student entered is
;;  id: the slot the workbench holds the students input in (0 based indexing)
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  This is a hack-ish way to get the assoc value but (for now), it works.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun lookup-eqn-string (entry)
  "Minimally modified Andes2 call"
  (let (result (eq (StudentEntry-text entry)))
    (unless eq (warn "Equation must always have text") (setf eq ""))
    (setf result (do-lookup-equation-string (trim-eqn eq) 
		   entry 'equation))
    (setf (turn-result result) 
	  (append (log-entry-info (find-entry (StudentEntry-id entry))) 
		  (turn-result result)))
    result))

(defun do-lookup-equation-string (eq entry location)
  (let ((equation eq) tmp)
    (if (= 0 (length (remove #\Space equation)))
	(setf tmp (handle-empty-equation entry))
      (let* ((parses (parse-equation **grammar** equation))
	     (complete (parse-get-complete parses))
	     (valid (parse-get-valid 'final complete)))
	(cond
	 ((null valid)
	  (setf tmp (handle-bad-syntax-equation eq entry parses)))
	 (t
	  (setf tmp (handle-ambiguous-equation eq entry valid location))))))
    ;;(format t "at end is ~A~%" tmp)
    tmp))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun test-parse (eq)
  (let ((equation eq))
    (if (= 0 (length (remove #\Space equation)))
	(format nil "Empty Equation <~W>~%" eq)
      (let* ((parses (parse-equation **grammar** equation))
	     (complete (parse-get-complete parses))
	     (valid (parse-get-valid 'final complete)))
	(cond
	 ((= (length valid) 0)
	  (format nil "No valid parses in <~W>~%" eq)
	  (dolist (p parses) (format nil "~W~%" p)))
	 ((> (length valid) 1)
	  (format nil "Multiple parses <~W>~%" equation)
	  (dolist (p valid) (format nil "~W~%" p)))
	 (t ;; as it stands now this won't happen
	  (format nil "Good parse <~W>~%~W" equation valid)))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
(defun handle-empty-equation (entry)
  (let ((id (StudentEntry-id entry)))
    (delete-object id)
    (make-noop-turn))) ; no coloring on empty eqn -- noop leaves "black"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun handle-bad-syntax-equation (equation se parses)
  "Given a student equation, its id and the parses, 
   creates a student entry, adds it to the *student-entries*,
   creates an error interpretation for it, and returns
   the first tutor turn of the error interpretation's hint sequence."
    (setf (StudentEntry-verbatim se) equation)
    (setf (StudentEntry-parsedeqn se) parses)
    (setf (StudentEntry-prop se) (list 'eqn equation))
    (setf (StudentEntry-state se) **incorrect**)
    (add-entry se)
    (setf (StudentEntry-ErrInterp se) (bad-syntax-ErrorInterp equation :id (StudentEntry-id se)))
    (ErrorInterp-remediation (StudentEntry-ErrInterp se)))

; This returns a plain ErrorInterp:
(defun bad-syntax-ErrorInterp (equation &key id)
  "Given a syntactically ill-formed equation, returns an error interpretation for it."
  (let (rem)				; remediation hint seq to be assigned
    ;; cheap tests for a few common sources of errors
    (cond				
     ((not (position #\= equation))
      (setf rem (make-hint-seq (list
				(format nil "Entry ~a is not an equation." equation)
				"The entry needs an = sign to be an equation."))))
     ((> (count #\= equation) 1)
      (setf rem (make-hint-seq (list
				(format nil "~a is not a single equation." equation)
				"You may enter only one equation on a line."))))
     ((search "sec" equation)
      (setf rem (make-hint-seq (list
				(format nil "Syntax error in ~a" equation)
				"If you are giving a value in seconds, the correct SI symbol is just s, not sec."))))
     ((search "ohms" equation)
      (setf rem (make-hint-seq (list
				(format nil "Syntax error in ~a" equation)
				"If you are giving a resistance in Ohms, the correct SI symbol is $W, not ohms."))))
     ;; BvdS:  There should be a handler for "unknown functions"
     ;; analogous to the handler for "unknown variables"
     ;; This is a work-around.
     ((and (search "log" equation) (not (search "log10" equation)))
      (setf rem (make-hint-seq (list
				;; (format nil "Syntax error in ~a" equation)
				"Use ln(x) for natural logarithms and log10(x) for logarithms base 10."))))
     ((or (search "_ " equation) (search " _" equation))
      (setf rem (make-hint-seq (list
				(format nil "Syntax error in ~a" equation)
				"There is a space next to an underscore in this equation. If you are using a component variable, make sure you type it as a single word without any spaces between the underscore and the rest of the variable name."))))
     (T (setf rem (make-hint-seq
		   (list
		    (format nil "Syntax error in ~a" equation)
		    "The equation or the {\\l units}{\\v units.html} in it are not in a recognizable form."
		    "Though I can't tell exactly what the mistake is, a few common sources of errors are: (a) {\\l Unit symbols}{\\v units.html} are case sensitive. (b) Multiplication always requires an explicit multiplication sign: W=m*g, NOT W=mg. (c) Units attach only to numbers, not to variables or expressions.")))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)

    (make-ErrorInterp
     :diagnosis '(Syntax-error-in-eqn)
     :remediation rem)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-ambiguous-equation (equation entry parses location)
  ;(prl parses)
  (let ((id (StudentEntry-id entry))
	tmp bad (cont t) result se save)
    (dolist (parse parses)
      (when (and cont (not (member parse save :test #'equal)))
	(setf save (append save (list parse)))
	;; Build a candidate entry containing this parse. The candidate 
	;; with the winning parse will be saved permanently with add-entry 
	;; when we know which one it is.
	;; Start with given entry since that has all user parameters
	;; and modify
	(setf se entry)
	(setf (StudentEntry-verbatim se) equation)
	(setf (StudentEntry-parsedeqn se) parse)
	(setf (StudentEntry-prop se) (list 'eqn equation))
	(setf (StudentEntry-State se) **InCorrect**)
	(setf tmp (parse-handler se location))
	(cond
	  ((equal **Color-Green** (turn-coloring tmp))
	   (setf (StudentEntry-State se) **Correct**)
	   ;; know this entry has winning parse so save entry now 
	   (add-entry se) 	
	   (setf result se)
	   (setf cont nil))
        (t ;;(equal **Color-Red** (turn-coloring tmp))
          (setf bad (append bad (list (list tmp se))))))))    
    (cond
      (cont
       (setf tmp (choose-ambiguous-bad-turn bad se)) ; does add-entry on winning candidate
       (if (null tmp)
	   (setf tmp (warn "Should not see this error: (1) Notify Instructor"))))
      (t
       ;; Record correct eqn in algebra. (Must happen before interpretation 
       ;; testing)
       ;; NB: If we later reject it for some reason (because forbidden, 
       ;; premature, etc), algebra slot should be cleared.
       (if (stringp (solver-studentAddOkay (StudentEntry-Id se) (StudentEntry-ParsedEqn se)))
	   (setf tmp (make-red-turn :id (StudentEntry-Id se))) ;; to trap exceptions
	   (setf tmp (interpret-equation result location)))
       (cond
	 ((equal **Color-Green** (turn-coloring tmp))
	  (sg-Enter-StudentEntry se)
	  
	  ;; also enter scalar variables whose only uses are in this entry's interp
	  (let ((eqn-interp (StudentEntry-Cinterp se))
		unneeded-vardefs)
					; collect list of variable entries no longer needed
	    (when eqn-interp ; if interp is empty, don't reduce #'union NIL, Bug 949
	      (dolist (var (reduce #'union (mapcar #'(lambda (sysent) 
						       (vars-in-eqn (sysent-algebra sysent)))
						   eqn-interp)))
		(when (and (var-to-sysentry var) ;nil for vector quantities, ignore
			   (subsetp (syseqns-containing-var var) eqn-interp))
	          (pushnew (var-to-sysentry var) unneeded-vardefs))))
	    (when unneeded-vardefs
					; temporarily munge this entry's interpretations to get variable definition entries 
					; associated with it to be marked as entered by this student entry, restore when done. 
					; Note sg-delete-StudentEntry adjusted to undo this on equation entry deletions.
	      (format *debug-help* "entering unneeded vardefs: ~s~%" 
		      unneeded-vardefs)
	      (setf (StudentEntry-Cinterp se) unneeded-vardefs)
	      (sg-Enter-StudentEntry se)
	      (setf (StudentEntry-Cinterp se) eqn-interp)))
	  )
	 (t
	  ;; empty slot since it failed
	  (solver-studentEmptySlot (StudentEntry-Id se))))))
    ;;(format t "*(*(*( ~A~%" tmp)
    tmp))

(defun sysent-algebra (sysent)
"return algebra for a system equation entry; NIL if not an eqn entry"
  (when (help-eqn-entryprop-p (systemEntry-prop sysent))) ; is an equation entry
    (second (systemEntry-prop sysent)))

(defun syseqns-containing-var (var)
"get all system entries for eqns containing var"
   (remove-if-not #'(lambda (sysent) 
                        (member var (vars-in-eqn (sysent-algebra sysent))))
		  *sg-entries*))

(defun var-to-sysentry (var)
"given system variable, find the systementry for its definition step"
 (let ((quant (sysvar-to-quant var)))
  (when quant ; look for matching define-var prop
    (find `(define-var ,quant) *sg-entries* 
           :key #'systementry-prop :test #'equal))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
; choose-ambiguous-bad-turn: Select which of several non-correct parses to return.
; PARAMETER: list of candidate (turn entry) pairs collected above
; RETURN: chosen turn to use
; Side effects: Saves chosen entry on entry list, setting its state to incorrect.
;
(defun choose-ambiguous-bad-turn (badlist entry)
  ;(prl badlist)
  (let (choice wrong unk uni mis err unused)
    (dolist (te badlist)
      ; collect sets of results of each distinguished class
      ; (format T "choose-ambiguous: parse w/tag ~a~%" (te-error-tag te))
      (case (te-error-tag te)
        (undefined-variables
	   (setf unk (append unk (list te))))
	(Unused-variables
 	   (setf unused (append unused (list te))))
	(wrong-units
	  (setf uni (append uni (list te))))
	; forgot units given for forgot-units-but-ok OR maybe-forgot-units on assignmentp
	; prefer it to maybe-forgot-units in mis set
	(forgot-units
	  (setf mis (cons te mis)))
	(maybe-forgot-units
	   (setf mis (append mis (list te))))
	(internal-error
	  (setf err (append err (list te))))
	; everything else should have OK syntax, vars & units, just plain wrong
	(otherwise 
	      ; should verify it really is wrong, but no special tag for that
	      (when (te-error-tag te)
	      	   (format T "choose-ambiguous-bad: unknown error ~A. Treated as wrong~%" 
		           (te-error-tag te)))
	      (setf wrong (append wrong (list te))))))	

    ;; now look for choice in order from most charitable to least:
    ;; big OR falls through cases in order till non-NIL:
    (setf choice 
     (or  ;;; inaccurate isn't used anymore, see parse-handler.
          ;; look for any wrong -- at least OK syntax, vars, units
          ;; Try to prefer simpler one, to make life simpler for WWH diagnosis
          ;; by avoiding including unnecessary DNUM mangled forms.
          (first (sort wrong #'simpler-parse))
	  ;; look for units error:
          ;; URGH Some ambiguous equations get both inconsistent and missing units parses:
	  ;; one parse dnum-mangles rhs of "s=-5m/s" to (* (- 5) (DNUM 1 |m/s|)) which
	  ;; then appears to have missing units on 5 if original units are wrong.  This parse
	  ;; can even get forgot-units-but-ok if the value is correct. Unless this is fixed or
	  ;; detected, we have to distrust forgot-units interp if any "inconsistent" parses exist 
	  ;; because the forgot-units reading may just be artifact of dnum mangling. 
	  ;; Prefer less committal "inconsistent" if any exist
          (first uni)		; inconsistent units
          (first mis) 		; missing units 
          ;; variable errors: unused vars is better than undefined
          (first unused)	; unused vars
     	  ;; look for unknown vars, preferring parse w/smallest number
          (first (sort unk #'(lambda (te1 te2)
		                     (< (te-unknowns te1) (te-unknowns te2)))))
          ;; else just pick the first one given (exception?) -- shouldn't happen
	  (first badlist)))

    ;; Set state to chosen entry:
    (setf (StudentEntry-State entry) **Incorrect**)
    (setf (StudentEntry-verbatim entry) 
	  (StudentEntry-verbatim (second choice)))
    (setf (StudentEntry-parsedeqn entry) 
	  (StudentEntry-parsedeqn (second choice)))
    (setf (studentEntry-prop entry) 
	  (studentEntry-prop (second choice)))
    ;; The chosen entry is the one saved on the list:
    (add-entry entry)
    ;;(format t "Entry is ~W~%Turn is ~W~%" (second choice) (first choice))
    ;; return value is chosen *turn*
    (first choice)))

;;; find tag identifying error for a given bad candidate (turn, entry) pair
;;; that's first element of diagnosis form in entry's errinterp.
(defun te-error-tag (te-pair)
   (when (StudentEntry-ErrInterp (second te-pair))
     (car (ErrorInterp-diagnosis (StudentEntry-ErrInterp (second te-pair))))))

(defun simpler-parse (cand1 cand2)
  (let ((parse1 (StudentEntry-parsedEqn (second cand1)))
        (parse2 (StudentEntry-parsedEqn (second cand2))))
    ;; WARNING: parsedEqn may contain a parse tree struct, not a prefix form list, if
    ;; candidate didn't make it all the way through parse-handler (e.g undefined vars)
    ;; This routine is supposed to be called on parseable eqs only, but best to be safe:
    ;; There was a crash in Andes7.0.0 whenver got unused vars among multiple var parses 
    ;; since these weren't filtered out of "wrongs" above.
    (and (listp parse1) (listp parse2)
         ;; cheap test: compare length of infix forms (top level only). 
	 ;; Works for "v = -N units" case we need.
         (< (length (pre2in parse1))
            (length (pre2in parse2))))))

;;; return number of unknown vars in an unknown or unused var turn-entry pair
(defun te-unknowns (te-pair)
  (if (or (eq (te-error-tag te-pair) 'undefined-variables)
          (eq (te-error-tag te-pair) 'unused-variables))
      ;; error diagnosis should hold (undefined-variables v1 v2 v3...)
      ;; BvdS:  This is completely wrong.  The number of slots in the
      ;; function call has nothing to do with the number of slots unmatched
      ;; in the student entry. See Bug #981.
      (length (cdr (ErrorInterp-diagnosis (StudentEntry-ErrInterp (second te-pair)))))
    0))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun contains-strings (eq)
  (cond
   ((null eq) nil)
   ((stringp eq) (list eq))
   ((consp eq)
    (append (contains-strings (car eq)) (contains-strings (cdr eq))))
   (t nil)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-handler (se location)
  "Given a student entry and whether it was located in the answer box or the equation window, 
   calls color-by-numbers on it, and return a tutor turn.
   If the equation is incorrect, set the ErrInterp slot of the student entry."
  (let* ((parse (StudentEntry-ParsedEqn se))
	 (answer (subst-canonical-vars (parse-pack-lhs 'unknown (parse-tree parse))))
	 (strings-in-answer (contains-strings answer)))
    (cond
     (strings-in-answer (handle-undefined-variables-equation se strings-in-answer))
     (t
      (setf answer (parse-pack-to-string-lhs 'unknown answer))
      (setf answer (parse-remove-lhs 'wspace answer))
      (setf answer (parse-pack-lhs 'r-paren answer))
      (setf answer (parse-pack-lhs 'l-paren answer))
      (setf answer (parse-pack-lhs 'equals answer))
      (setf answer (parse-pack-lhs 'bops answer))
      (setf answer (parse-pack-lhs 'number answer))
      (setf answer (parse-pack-lhs 'func answer))
      (setf answer (parse-pack-cs-lhs 'unit answer))
      (setf answer (parse-surround-lhs "(" ")" 'funcall answer))
      (setf answer (parse-surround-lhs "(" ")" 'funcall-a answer))
      (setf answer (parse-surround-lhs "(DNUM" ")" 'dnum answer))
      (setf answer (parse-collapse answer))
      (if (stringp answer)		;collapse makes it a string
	  (setf answer (andes-in2pre answer)))
      (cond ((stringp answer)		;in2pre makes a list
	     (make-red-turn "Should not see this error: (2) Notify Instructor"))
	    (t				;use equation-redp so candidate is tested but not added to slot
	     (setf (StudentEntry-ParsedEqn se) answer)
	     (case (solver-equation-redp answer location)
	       (forgot-units-but-ok
		(forgot-units-ErrorInterp se))
	       (maybe-forgot-units
		(maybe-forgot-units-ErrorInterp se))
	       (wrong-units
		(wrong-units-ErrorInterp se))
	       (inaccurate
		;; not currently used because What's wrong checks for 
		;; inaccuracy but only after checking for other error classes
		(warn "inaccurate in parse-handler")
		(make-red-turn :id (StudentEntry-id se)))
	       (wrong
		(make-red-turn :id (StudentEntry-id se)))
	       ;; Following mainly occurs for parses giving rise to bad syntax
	       ;; equations. Usually when this happens another parse will
	       ;; produce legal equation so its not a problem. But we need 
	       ;; to record this status to prefer other parses if eqn wrong.
	       (solver-exception 
		(solver-exception-interp se))
	       (otherwise
		(make-green-turn :id (StudentEntry-id se))))))))))

;
; Note: several canned routines here for particular error interpretations are
; used for errors that provide unsolicited messages. These routines:
;    1. create a hint sequence turn for use in the remediation field (rem)
;    2. construct an error interpretation object (ei) containing rem
;    3. set the error interp field of the student entry to ei
;    4. set coloring on the rem turn to color red
;    5. return rem for use as a final result turn
; Unsolicited feedback results as the remediation's red+hint turn is returned 
; up the stack for use as the final result turn for the equation entry.
; If this is not returned, the hint sequence is saved with the entry but
; is not given until student asks whats wrong.


; forgot-units is returned when equation is dimensionally inconsistent but
; balances numerically when numbers are treated as having unknown units.
(defun forgot-units-ErrorInterp (se)
  "Given a student entry, return a tutor turn that gives unsolicited feedback saying that
   the student forgot to put units on at least one number.
   Also create an error interpreation in case the student asks a follow-up question, and
   put it in the student entry's err interp field."
  (let ((rem (make-hint-seq
	      '("Forgot to put units on a number."
		"This equation is dimensionally inconsistent. When numbers are used in equations, they must include the appropriate units.  It looks like one of the numbers you've used is lacking the units."))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(forgot-units)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))


(defun assignment-eqn (parsed-eqn)
  "true if given prefix eqn parse is a numerical assignment statement"
  (and (consp parsed-eqn)           ; just sanity checks on argument
       (eq (first parsed-eqn) '=)   
       (= (length parsed-eqn) 3)
       ;; predicate defined in errors.cl takes (lhs rhs)
       (assignmentp (second parsed-eqn) (third parsed-eqn))))


; maybe-forgot units is returned when equation is dimensionally inconsistent but
; could be dimensionally OK if numbers are treated as having unknown units -- though
; it STILL fails to balance acceptably. So we are unsure what the true cause of the
; inconsistency is, but can suggest maybe they forgot units. If this occurs for a simple 
; numerical assignment statement we promote the response to the more definite "forgot units" 
; message: The value may be wrong but we are still sure they have forgotten the units on a number.
(defun maybe-forgot-units-ErrorInterp (se)
  "Given a student entry, return a tutor turn that gives unsolicited feedback saying that
   the student appears to have left units off at least one number.
   Also create an error interpreation in case the student asks a follow-up question, and
   put it in the student entry's err interp field."
  ; in case of a simple assignment statement, change to forgot-units error interpretation
  (when (assignment-eqn (StudentEntry-ParsedEqn se))
       (return-from maybe-forgot-units-ErrorInterp (forgot-units-ErrorInterp se)))
  
  (let ((rem (make-hint-seq
	      '( "The units in this equation are not consistent.  If this is a symbolic equation, there is probably an error:  check all your terms.  Another possibility is that a number has been used without correct associated units."))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(maybe-forgot-units)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))

; If this is a simple numerical assignment statement, we can say more specifically
; that units are wrong.
(defun wrong-units-ErrorInterp (se)
  "Given a student entry, return a tutor turn giving unsolicited feedback saying that
   the student equation has a dimensional inconsistency
   Also create an error interpreation in case the student asks a follow-up question, and
   put it in the student entry's err interp field."
  (let ((rem (make-hint-seq
	      '("Units are inconsistent."))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(wrong-units)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))

(defun solver-exception-interp (se)
  ;; To tag buggy unprocessable parse so can prefer others. Hopefully won't ever show this to students.
  (let ((rem (make-hint-seq '("Internal error: could not process equation."))))
    (setf (turn-coloring rem) NIL) ; leaves black. Not red, since not known wrong
     (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(internal-error)
       :remediation rem))
    rem))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun handle-undefined-variables-equation (se strings)
  "Given a student entry and a list of strings, return a tutor turn and set the err-interp field of the student entry"
  (let ((defined-but-not-sysvars (remove-if-not #'symbols-lookup strings)))
    (setf (StudentEntry-ErrInterp se)
      (if defined-but-not-sysvars
	  (unused-variables-ErrorInterp defined-but-not-sysvars
					:id (StudentEntry-id se))
	  (undef-variables-ErrorInterp strings 
				       :id (StudentEntry-id se))))
    (ErrorInterp-remediation (StudentEntry-ErrInterp se))))


(defun has-comp-vars-p (vars)
  (let ((result nil))
    (dolist (var vars)
      (if (> (length var) 2)
	  (let ((tmp (subseq var (- (length var) 2))))
	    (if (and (member tmp '("_x" "_X" "_y" "_Y" "_z" "_Z") :test #'equal)
		     (remove-if-not #'symbols-lookup
					  (list (subseq var 0 (- (length var) 2)))))
		(setf result (append result (list var)))))))
    result))

(defun near-miss-var (var)
"given a symbol return defined variables with similar names"
  ;; for now, just look for case errors, and just return first one found
  (let ((sym (find var *variables* :key #'sym-label :test #'string-equal)))
     (when sym (sym-label sym))))


(defun undef-variables-ErrorInterp (undef-vars &key id)
  "Given a list of undefined vars (as strings), returns the error interpretation that will be both stored in the student entry and used to give the student an unsolicited warning."
  (let* ((is-comp-var (has-comp-vars-p undef-vars))
	 (tmp-msg "Variables must be defined before being used in an equation.  Vectors are defined by the drawing tools (the buttons along the left edge of the window) and scalars are defined by the clicking on the 'Variable' button at the top of the window.\\n  If all variables have been defined, the problem may be incorrect unit symbols, including case errors.  For example, 'N', not 'n', is the symbol for Newtons.")
	 (near-misses (mapcar #'near-miss-var undef-vars))	; parallels undef-vars, e.g. (NIL v1 NIL v2 NIL)
	 (i-first-miss (position-if-not #'null near-misses))    ; index of first var with near-mis, else NIL
	 (rem (make-hint-seq
	       (list
		(if (null (cdr undef-vars))
		    (format nil "Undefined variable: ~a" (car undef-vars))
		  (format nil "Undefined variables: ~a" undef-vars))
		; check for near misses: currently case errors. Could try to find spelling errors later.
		(if i-first-miss 	; report the first one to fix only. Can get others next time if not fixed.
	           (format nil "Case matters in variable names: \"g\" means something different than \"G\".  You probably meant ~A instead of ~A." (nth i-first-miss near-misses) (nth i-first-miss undef-vars))
		  ;; else not near-miss: give advice if used compo notation:
		  (if is-comp-var
		      (if (cdr is-comp-var)
			  (format nil "The variables: ~a may be defined by drawing coordinate axes" is-comp-var)
			(format nil "The variable: ~a may be defined by drawing coordinate axes." (car is-comp-var)))
		    tmp-msg))))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)

    (make-ErrorInterp
     :diagnosis (cons 'Undefined-variables undef-vars)
     :remediation rem)))

(defun unused-variables-ErrorInterp (undef-vars &key id)
  "Given a list of unused vars (as strings), returns the error interpretation that will be both stored in the student entry and used to give the student an unsolicited warning."
  (let ((rem (make-hint-seq
		 (list
		  (if (null (cdr undef-vars))
		      (format nil "The variable ~a is not used in any solution I know of." (car undef-vars))
		    (format nil "These variables are not used in any solution I know of: ~a." undef-vars))
		  "I can only recognize equations and variables from solutions I know about. This variable is not used in any of the solutions I have recorded for this problem. It's possible you are pursuing a solution that I don't know about, but if so, I can't help you with it and simpler solutions are probably available."))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)  

    (make-ErrorInterp
     :diagnosis (cons 'Unused-variables undef-vars)
     :remediation rem)))


;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun find-all-in (eq expr &optional (p nil))
  ;; (find-all-in '(h a b (e f (h i j) g) c) 'h) will return '((1) (4 3 1))
  (let ((count 0))
    (mapcan
     #'(lambda (x)
	 (incf count)
	 (if (consp x)
	     (if (equal x expr)
		 (list (append p (list count)))
	       (find-all-in x expr (append p (list count))))
	   (if (equal x expr)
	       (list (append p (list count))))))
     eq)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-unused-variables (equation)
  (if (find-all-in equation 'nil) nil equation)) ;; probably a better to do this
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; strip comment and leading and trailing spaces from an eqn string.
(defun trim-eqn (eq)
  (if (> (length eq) 0)
      (let ((p (position #\; eq)))
	(if p
	    (string-trim " " (subseq eq 0 p))
	  (string-trim " " eq)))
    eq))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun undo-eqn-entry (entry)
  "clean up state on removal of student entry"
  ;; clear equation slot in algebra module.
  (solver-StudentAddOkay (StudentEntry-ID entry) "") 
)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun andes-in2pre (equation)
  (let* ((eq (read-from-string (concatenate 'string "(" equation ")")))
	 (leaveAlone nil)
	 (unary '(+ - ln abs sin cos tan log10 sqrt exp))
	 (binary '(((= r)) ((- l) (+ l)) ((* l) (/ l)) ((^ l))))
	 (special '(dnum))
	 (infixed (in2pre eq leaveAlone unary binary special))
	 (result (denum-mangle (car infixed))))
    (clean result)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun dnum (args)
  (let ((x (first args)))
    (let ((tmp (list (append (list (first x))
			     (in2pre (subseq x 1 (- (length x) 1))
				     (second args)
				     (third args)
				     (fourth args)
				     (fifth args))
			     (last x)))))
      tmp)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Our equation grammar is liberal in allowing complex arithmetic expressions
;; of constants wherever numbers are allowed so that units can attach 
;; to a complex expression.  This is done to allow such things as 
;;             omega = 3*pi/4 rad/s
;; to have an acceptable reading. It also allows things like this
;;             t= 3*3600 + 47*60 + 36 s
;; for hour, minute, second time given in kt1a (first problem many students see,
;; in which entering the given time correctly is a hurdle).
;; However, the solver's prefix-form parser does not allow this. So we convert
;; our form by "dnum-mangling" it into solver-acceptable form as follows:
;;  	(dnum (+ 2 3) |m/s|) ==> (* (+ 2 3) (dnum 1 |m/s|))
(defun denum-mangle (parse)
  (let ((tmp nil))
    (cond
     ((null parse)
      (setf tmp nil))
     ((null (consp parse))
      (setf tmp parse))
     ((consp (first parse))
      (setf tmp (list (denum-mangle (first parse))
		      (denum-mangle (rest parse)))))
     ((member (first parse) '(sin cos tan abs ln log10 sqrt exp))
      (setf tmp (list (first parse)
		      (denum-mangle (second parse)))))
     ((member (first parse) '(dnum))
      (if (consp (second parse))
	  (if (= 1 (length (second parse)))
	      (setf tmp (list (first parse) (first (second parse)) (third parse)))
	    (setf tmp (list '*
			    (second parse)
			    (list (first parse) 1 (third parse)))))
	(setf tmp (list (first parse) (second parse) (third parse)))))
     (t
      (setf tmp (list (first parse)
		      (denum-mangle (second parse))
		      (denum-mangle (third parse))))))
    tmp))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; removes a variable from the grammar
;; returns *grammar* after removal
(defun grammar-remove-variable (varin)
  (grammar-remove-identifier '**grammar** varin 'variable))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For answers, we allow student to enter either a full assignment equation of form sought_var = rhs
;; or just the answer expression rhs alone. 
;;
;; If they didn't enter a full equation, we effectively construct an appropriate equation. 
;; If a student variable for the sought is defined, we can use that to construct an equation;
;; else we temporarily install a student variable "Answer" for use while constructing a student equation
;; to test. 
;;
;; There are further tests we apply that are specific to answers:
;; - We have to check that the rhs has the form required for an answer. To allow for
;; answers as complex expressions in terms of parameters, we make sure there are no non-parameter
;; variables in the rhs expression. !! Note this currently allows complex arithmetic expressions to be accepted, 
;; even in problems for which no parameters are defined. !! Should also allow answers in terms of constants like pi.
;; - If they typed a full equation, we have to verify it is an assignment statement and lhs is the correct variable.
;;
;; Once we have an equation we can test the answer equation up to answer accuracy to see if its correct.
;;

(defun do-check-answer (inputo sought-quant id)
  ;;(format t "Okay start!!!<~A>[~A]~%" input sought-quant)
  (warn "Can't make valid studententry since type, location etc missing")
  (let ((entry (make-studententry :id id
			  ;; not system answer entries so no real prop for these,
			  ;; following will let us find if answer entered for a quant.
                          :prop `(answer ,sought-quant)
                          :verbatim inputo))
	(result-turn)
	(input (trim-eqn inputo)))
    (add-entry entry) ;save entry immediately 
    (if (quant-to-sysvar sought-quant)
	(if (/= (length (remove #\Space input)) 0)
	    (let* ((stud-var (symbols-label sought-quant)) ; student's var for sought quant, maybe NIL
		   (ep (position #\= input))
		   (lhs (if ep (remove #\Space (subseq input 0 ep)) nil))
		   (rhs (if ep (subseq input (+ ep 1) (length input)) input))
		   (valid t)
		   (why nil))
	      (if ep ; student entered a complete equation
		  (if (/= (length (remove #\Space rhs)) 0)
		      (if (/= (length (remove #\Space lhs)) 0)
			  (let ((nvar (subst-canonical-vars (list lhs))))
				  (cond
				   ((contains-strings nvar) ; lhs expr is (or contains) undefined var
				    (setf why (list 'bad-var lhs))
				    (setf valid nil))
				   ((and stud-var (equalp lhs stud-var))) ; lhs = student's var for sought 
				   (t (setf why (list 'bad-sought lhs))
				      (setf valid nil))))
			;; else eqn has empty lhs, e.g. student typed "= 5 N". Allow it.
			(setf lhs (if stud-var stud-var "Answer")))
		    ;; else eqn has empty rhs, so bad.
		    (setf valid nil))
		;; else student only entered rhs: fill in lhs student variable.
		(setf lhs (if stud-var stud-var "Answer")))
	      (if (not stud-var)
		  (symbols-enter "Answer" sought-quant id)) ;; !! NB: want to delete this temp in all paths
	      (if valid
		  (let* ((parses (parse-equation **grammar** (string-trim " " rhs)))
			 (complete (parse-get-complete parses))
			 (valid (parse-get-valid 'expression complete)))
		    ;;(format t "Parsed ~A~%" (concatenate 'string lhs "=" (string-trim " " rhs)))
		    ;;(format t "Okay parse!!!~%~A~%" valid)
		    (if valid
			(cond
			 ((not (bad-vars-in-answer valid)) ; checks no non-parameter vars in answer expression
			  ;;(format t "Okay here!!!~%")
			  ;; Check as if student equation was entered in *temp-eqn-slot*.
			  (setf result-turn (do-lookup-equation-answer-string
					     (concatenate 'string lhs "=" rhs)
					     *solver-temp-eqn-slot*))
			  ;; That saved a temp equation entry under *temp-eqn-slot*. 
			  ;; Copy relevant entry state -- esp ErrInterp for later 
			  ;; whatswrong  -- into real answer entry and remove temp.
			  ;;(format t "Result Answer is <~W>~%" result-turn)
			  (let ((temp-entry (find-entry *solver-temp-eqn-slot*)))
			    (setf (StudentEntry-State entry)
			      (StudentEntry-State temp-entry))
			    (setf (StudentEntry-ErrInterp entry)
			      (StudentEntry-ErrInterp temp-entry))
			    (setf (StudentEntry-ParsedEqn entry) ; parse maybe useful
			      (StudentEntry-ParsedEqn temp-entry))
			    ;; remove temp from saved entry list
			    ;; clears algebra slot
			    (delete-object *solver-temp-eqn-slot*))
			  (symbols-delete "Answer"))
			 (T ; answer has non-parameter vars
			  (setf (StudentEntry-ErrInterp entry) 
			    (bad-variables-vs-parameters-ErrorInterp input 
			                         (bad-vars-in-answer valid)
						 :id id))
			  (setf result-turn (ErrorInterp-remediation
					     (StudentEntry-ErrInterp entry)))))
		      (progn ; didn't parse. Note re message that input might not have been a full equation.
			(setf (StudentEntry-ErrInterp entry) 
			      (bad-syntax-ErrorInterp
			       (if ep input (strcat lhs "=" rhs)) :id id))
			(setf result-turn (ErrorInterp-remediation
					   (StudentEntry-ErrInterp entry))))))
		(cond ; failed to get a candidate to test.
		 ((and why (equal (car why) 'bad-var))
		  (warn "do-check-answer bad var ~A~%" entry)
		  (setf (StudentEntry-ErrInterp entry)
		    (bad-answer-bad-lhs-ErrorInterp input why :id id))
		  (setf result-turn (ErrorInterp-remediation (StudentEntry-ErrInterp entry))))
		 ((and why (equal (car why) 'bad-sought))
		  (warn "do-check-answer bad sought ~A~%" entry)
		  (setf (StudentEntry-ErrInterp entry)
		    (bad-answer-bad-sought-ErrorInterp input why :id id))
		  (setf result-turn (ErrorInterp-remediation (StudentEntry-ErrInterp entry))))
		 (t
		  (setf (StudentEntry-ErrInterp entry)
		    (bad-answer-syntax-ErrorInterp input :id id))
		  (setf result-turn (ErrorInterp-remediation (StudentEntry-ErrInterp entry)))))
		)))
	  ;;(format t "Zero length"))
      (warn "No system variable for ~A. Possible mismatch with answer box." sought-quant))
    (cond (result-turn) ;; if we got result from check above return it
          (T ;; else failed somewhere. !!! Should process syntax errors same as eqn.
	     ;;(format T "~&failed to get result for answer~%")
	     (setf (StudentEntry-state entry) **incorrect**)
	     (make-red-turn :id (StudentEntry-id entry))))))

(defun bad-answer-bad-lhs-ErrorInterp (equation why &key id)
  "LHS of equation is not a variable."
  (let ((rem (make-hint-seq
	      (list
	       (format nil "'~A' is not a defined variable." (second why))))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)

    (make-ErrorInterp
     :diagnosis '(answer-sought-is-undefined)
     :remediation rem)))

(defun bad-answer-bad-sought-ErrorInterp (equation why &key id)
  "Answer is malformed"
  (declare (ignore Equation)) ;suppressing warning.
  (let ((rem (make-hint-seq
	      (list
	       "Answers can be expressed as a simple equation with the name of the variable for the quantity that the problem asks you to find on the left hand side of equation."
	       (format nil "'~A' is not the value we are looking for." (second why))))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)

    (make-ErrorInterp
     :diagnosis '(answer-is-not-sought)
     :remediation rem)))

(defun bad-answer-syntax-ErrorInterp (equation &key id)
  "Answer is malformed"
  (let ((rem (make-hint-seq
	      (list
	       (format nil "Answers can be expressed as an explicit equation assigning a value to the sought, or by giving a single value only. (~a)" equation)
	       "Try removing the left-hand side of the equation."))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)

    (make-ErrorInterp
     :diagnosis '(answer-is-malformed)
     :remediation rem)))

;;; Build interpretation for disallowed variables in answer
;;; Could distinguish two cases: 1. problem asks for purely numerical answer 
;;; (so any var in answer is illegal); 2. problem asks for answer in terms of 
;;; some parameters (so only some vars illegal and we can say which are legal.)
(defun bad-variables-vs-parameters-ErrorInterp (equation badvars &key id)
  "Equation has non-parameter variables in answer"
  (let ((rem (make-hint-seq
	      (list
	       (format NIL "This expression contains variables not allowed in the answer: ~a" badvars)
	       "In most Andes problems a final answer should give an explicit numerical value (with units) for the sought. A few problems may ask you to express the value symbolically in terms of specified other quantities. Read the problem statement to see which variables, if any, are allowed in the answer."))))

    (setf (turn-id rem) id)
    (setf (turn-coloring rem) **color-red**)

    (make-ErrorInterp
     :diagnosis '(using-variables-in-answer)
     :remediation rem)))

;;; check a single parse tree for disallowed variables in answer
;;; returns list of disallowed student vars, NIL if none
(defun bad-vars-in (parse &optional (lhs 'unknown))
  (cond
   ((null parse) NIL)
   ((null (consp parse)) NIL)
   ((list-begins-with-p lhs (first parse))
    (let* ((s-var (second (parse-pack (first parse))))
           (c-var (student-to-canonical s-var)))
      ;;(format t "variable ~A~%" c-var)
      (if (or (stringp c-var) ; not translated 
	      (not (or (canonical-var-answer-var-p c-var)
		       ; Allow variables equivalent to declared answer-vars to be used as well.
		       ; For probs like rots8b which use relpos and radius of circular motion
	      	       ; (answer-var-equivalent-p c-var)
	               ; always allow physical constants like G in answer expression
	               ; since they are like numbers. (?Would we have a way to specify
	               ; exactly which constants are allowed in answer if we wanted?)
	               (physconstp c-var))))
	  (list s-var))))
   (t (append (bad-vars-in (first parse) lhs)
	      (bad-vars-in (rest parse) lhs)))))

;; check for use of disallowed variables (non-answer-vars) in answer expr
;; arguments: "valid" = list of valid parses of answer value expression
;; returns: NIL if found an OK parse, 
;;          a list of bad-vars if didn't 
(defun bad-vars-in-answer (valid)
  (let ((FoundOK NIL) (badvars NIL)) ;;(tmp nil) (result nil)
    (if (/= (length valid) 0)
	(dolist (x valid)
	  (when (not FoundOK)
	    ;;(format t "Checking <~A>~%" (parse-tree x))
	    (when (not (setf badvars (bad-vars-in (parse-tree x)))) 
	        (setf FoundOK T))
	    )))
    (if (not foundOK) 
       (remove-duplicates badvars :test #'equal))))


; check that value-str is a correct expression for given value of quant
; RETURNS: TURN with coloring and possible message
; MODIFIES: Entry, the studententry containing this, with state and error interp
; 
; Presumably this is the last check in a complex entry that has passed other tests.
;
; Value is normally simple number plus units, but might be complex
; arithmetic expression including constants such as
;      "3*$p/4 rad/s"
; This is similar to checking answer expressions: we must form
; an equation in systemese, check it, and ensure that rhs is of
; the right form. In this case must also check that value is given.
;
; Note the prop form for these is (EQN "studvar" "studvalue") which
; differs from reqular equations (only to make it simple to split).
(defun check-given-value-entry (main-entry eqn-entry)
   ; check the "subentry" alone, log its result
   ; and copy its state back into the main entry.
   (let ((result-turn (check-given-value-eqn eqn-entry)))
      ;; log the subentry details
      (setf (turn-result result-turn)
	    (append (log-entry-info eqn-entry) (turn-result result-turn)))
      ;  copy relevant info from subentry into main student entry
      (setf (StudentEntry-State main-entry) (StudentEntry-State eqn-entry))
      (setf (StudentEntry-ErrInterp main-entry) (StudentEntry-ErrInterp eqn-entry))
      ;; finally return result
      result-turn))

;; check a given value equation subentry 
;; Fills in subentry state with result of check
;; returns a result-turn to return for this.

(defun check-given-value-eqn (eqn-entry)
  (let* ((studvar (second (StudentEntry-Prop eqn-entry)))
         (value-str(third (StudentEntry-Prop eqn-entry)))
	 (quant    (symbols-referent studvar))
	 ;; want to distinguish cases where quantity is not given, so it 
	 ;; should be left unknown, from cases where it is given, but
	 ;; the value expression is wrong or bad in some other way.
	 ;; first do simple check that quantity is given. given-p defined 
	 ;; in errors.cl.  It takes a sysvar.  It treats components as given 
	 ;; if vector mag is given and lies along axis (though not the reverse)
	 ;; use given-var-p to avoid this behavior. Note it looks for
	 ;; given flag on quantities at the bubble-graph level, not implicit 
	 ;; equations, so might not work for those.
	 (sysvar   (student-to-canonical studvar))
	 ; NB: sysvar may be NIL, e.g for unused vector attribute
	 (is-optionally-given (and sysvar (optionally-given-p sysvar)))
	 (is-given (and sysvar (given-p sysvar)))
	 (is-known-constant (and sysvar (known-constantp (sysvar-to-quant sysvar))))
	 )
    
    ;; first filter case where student hasn't specified a given value 
    (cond 
     ((blank-given-value-entry eqn-entry)
      (cond (is-optionally-given 
	     (setf (StudentEntry-state eqn-entry) **correct**)
	     (make-green-turn :id (StudentEntry-id eqn-entry)))
	    (is-given 
	     (setf (StudentEntry-state eqn-entry) **Incorrect**)
	     (should-be-given-ErrorInterp eqn-entry quant))
	    (is-known-constant
	     (setf (StudentEntry-state eqn-entry) **Incorrect**)
	     (should-be-known-ErrorInterp eqn-entry quant))
	    (T ; quant is not given => OK
	     (setf (StudentEntry-state eqn-entry) **correct**)
	     (make-green-turn :id (StudentEntry-id eqn-entry)))))
     
     ;; get here => student specified a given value
     ((not (or is-given is-optionally-given is-known-constant))
      (setf (StudentEntry-state eqn-entry) **Incorrect**)
      (not-given-ErrorInterp eqn-entry quant))
     
     ;; else the quantity does have a given value:
     (t (let*  
	    ;; form a studentese equation and check it like any other equation,
	    ;; as if it were entered in *solver-temp-eqn-slot*.  This will us 
	    ;; a result turn, and record a (temp) entry struct containing its
	    ;; interp.  We remove the temp-entry when done with it. Note that
	    ;; the temp-entry != eqn-entry above, so we may have to update
	    ;; eqn-entry. (clean this up? If eqn is OK, eqn-entry will just 
	    ;; be entered again later.  Should just return the eqn entry to 
	    ;; use, and maybe set its slot before this.)
	    ((studeqn (concatenate 'string studvar "=" value-str))
	     ;; suppress normal eqn entry logging so we can do modified logging
	     ;; here, noting different errors and filling in target entry

	     ;; BvdS:  this is expecting an StudentEntry struct,
	     ;; not a raw string....
	     (result-turn (lookup-eqn-string 
			   studeqn *solver-temp-eqn-slot* :log NIL))
	     (temp-entry (find-entry *solver-temp-eqn-slot*))
	     (correct-eqn  (eq (StudentEntry-State temp-entry) **Correct**)))
	  ;; copy (provisional!) filled-in eqn check info from temp entry into 
	  ;; the main entry's dangling dependent equation subentry.  
	  ;; The subentry state will be used later for logging or entering 
	  ;; the correct interpretation later.  Possibly could just substitute
	  ;; temp entry for dangling entry to avoid copying.
	  (setf (StudentEntry-State eqn-entry) (StudentEntry-State temp-entry))
	  (setf (StudentEntry-ErrInterp eqn-entry) 
		(StudentEntry-ErrInterp temp-entry))
	  (setf (StudentEntry-ParsedEqn eqn-entry) 
		(StudentEntry-ParsedEqn temp-entry))
	  
	  ;; if it passed standard equation check, we still have to check it
	  ;; uses only givens. NB: If not, we have to make sure it is removed 
	  ;; from algebra, since correct entries get added as side effect of 
	  ;; normal eqn processing.  This is OK now, since we *always* delete 
	  ;; temp-entry; correct entries are re-added later.  But if we change 
	  ;; to only add once, must handle this.
	  (when (and correct-eqn (not (uses-only-given-eqn temp-entry)) 
		     (not is-known-constant))
	    (setf (StudentEntry-State eqn-entry) **Incorrect**) ; modify state copied above
	    (setf result-turn (more-than-given-ErrorInterp eqn-entry quant)))
	  ;; if equation is wrong but no error interpretation (syntax error, 
	  ;; missing units, etc) has been set, assume value is just plain wrong, 
	  ;; and set that here w/o using wwh
	  ;; Note: it could be a wrong expression that contains variables, we 
	  ;; still just say its wrong.
	  ;; Might want check to filter first for acceptable form above 
	  ;; (as we do for answers).
 	  (when (and is-given (not correct-eqn) (not (StudentEntry-ErrInterp temp-entry)))
	    (set-wrong-given-value-ErrorInterp eqn-entry quant))

	  (when (and is-known-constant (not correct-eqn) (not (StudentEntry-ErrInterp temp-entry)))
	    (should-be-known-ErrorInterp eqn-entry quant))

	  ;; don't save the temp equation entry on our main list anymore
	  ;; if it's correct, caller should add subentry like an implicit equation
	  ;; clears algebra slot automatically
	  (delete-object *solver-temp-eqn-slot*)
	  ;; finally return turn
	  result-turn
	  )))))

;; verify that correct studententry is an acceptable entry of a given value
(defun uses-only-given-eqn (studententry)
   (let ((interp (studententry-cinterp studententry)))
     (or (null interp)  
         (and (= (length interp) 1)
              (given-eqn-entry-p (first interp))) ; singleton given eqn
	 (allowed-compo-mag-combo interp)
	 ; If passed the test for givenness but gets an empty interp, assume it's
	 ; the value of an unused given. However, we are not verifying that the
	 ; rhs is a purely arithmetic expression in this case. 
	 (null interp))))

(defun not-given-ErrorInterp (se quant)
  (let ((rem (make-hint-seq
	      (list (format nil "The value of ~a is not given in this problem. It should be marked unknown." 
	                             (nlg quant 'var-or-quant)) ; use studvar if exists, else quant
	         ))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(should-be-unknown)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))

; fetch the systementry giving a value for this quantity.
; A bit circuitous: systementry has entry prop which embeds
; the algebra: '(EQN (= sysvar (DNUM ...))) 
(defun find-given-eqn-entry (quant)
   ; first lookup the given eqn by eqn id pattern in our index
   (let ((eqn (find-given-eqn-for quant)))
     ; then find system entry for eqn with matching algebra
     (when eqn (eqn-algebra->sysent (eqn-algebra eqn)))))

(defun find-given-eqn-for (quant) ; lookup in eqn index
 (find quant (problem-eqnIndex *cp*)
	     :key #'eqn-exp :test #'equal))

(defun eqn-algebra->sysent (algebra)
    (find `(EQN ,algebra) *sg-entries* 
	   :key #'systemEntry-prop :test #'equal))

; return an interpretation for the entry of entering the given value
; returns NIL if not found
(defun get-given-interp (quant)
  (let ((sysent (find-given-eqn-entry quant)))
    (if sysent (list sysent))))

(defun should-be-given-ErrorInterp (se quant)
  (let ((rem (make-hint-seq
	      (list (format nil "The value of ~a can be determined from the problem statement.  It should be entered in the dialog box when defining the relevant variable." 
	                             (nlg (quant-to-sysvar quant) 'algebra))
	         ))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(should-be-given)
       :intended (get-given-interp quant)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))

(defun get-known-interp (quant)
  (let ((eqn (find quant (problem-eqnIndex *cp*) :key #'eqn-exp 
		      :test #'exactly-equal)))
    (if eqn (list (eqn-algebra->sysent (eqn-algebra eqn))))))

(defun should-be-known-ErrorInterp (se quant)
  (let ((rem (make-hint-seq
	      (list (format nil "You need to enter an appropriate value for ~a."  
			    (nlg quant))
		    (format nil "Select 'Constants used in Andes' on the Help menu to find the value used in Andes.  This value should be entered in the dialog box when defining the relevant variable.")
	         ))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(should-be-known)
       :intended (get-known-interp quant)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))

; delegate to wrong-given-value (var wrongval) in kb/errors.cl which applies to equations.
; params are lhs and rhs of a systemese equation -- we better have gotten one if this
; is called.
(defun set-wrong-given-value-ErrorInterp (se quant)
  (let ((rem (wrong-given-value (second (StudentEntry-ParsedEqn se)) 
                                (third (StudentEntry-ParsedEqn se)))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(wrong-given-value)
       :intended (get-given-interp quant)
       :remediation rem))))

(defun more-than-given-ErrorInterp (se quant)
  (let ((rem (make-hint-seq
	      (list (format nil "Although this equation is a correct expression for the value of ~a, it does not simply state the given value." 
	                             (nlg (quant-to-sysvar quant) 'algebra))
	         ))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(more-than-given)
       :intended (get-given-interp quant)
       :remediation rem))

    (setf (turn-id rem) (StudentEntry-id se))
    (setf (turn-coloring rem) **color-red**)

    rem))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of parse-andes.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
