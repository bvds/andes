;; parse-andes.cl -- andes specific parse and grammar routines
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Kurt VanLehn  (kvl) <VanLehn@cs.pitt.edu>
;;  Collin Lynch (c?l) <CollinL@pitt.edu>
;; Modified:
;;  4 June 2001 - (lht) created
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2010
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
;;  12 July 2001 (kvl) modified to fill ErrInterp slot on student entries for many types of errors
;;  5 July 2003 (c?l) removing depreciated definition of and calls to replace-greek.
;;  12 July 2003 (c?l) added declarations:
;;   ignored some instances of unused variables to suppress warnings.
;;   commented out setting of Result and Tmp in Bad-Vars-In-Answer as they were unused.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)
(export 'grammar-remove-variable) ;for symbols package
(eval-when (:load-toplevel :compile-toplevel)
 (use-package :symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-eqn-string -- check correctness of a student equation entry
;; argument(s):
;;  eqn-string: the equation as the student entered is
;;  id: the slot the workbench holds the students input in (0 based indexing)
;; returns:
;;  turn
;; note(s):
;;  This is a hack-ish way to get the assoc value but (for now), it works.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup-eqn-string (entry)
  "Minimally modified Andes2 call"
  (let (result (eq (StudentEntry-text entry)))
    (unless eq (warn "Equation must always have text") (setf eq ""))
    (setf result (do-lookup-equation-string (trim-eqn eq) 
		   entry 'equation))
    
    ;; logging for incorrect entries is already done by make-red-turn
    (let ((final-entry (find-entry (StudentEntry-id entry))))
      (unless (and final-entry
		   (eql (StudentEntry-state final-entry) +incorrect+))
	(add-log-entry-info final-entry result)))
    result))

(defun do-lookup-equation-string (equation entry location)
  (if (= 0 (length (remove #\Space equation)))
      (handle-empty-equation entry)
      (let* ((parses (parse-equation **grammar** equation))
	     (complete (parse-get-complete parses))
	     (valid (parse-get-valid 'final complete)))
	(if valid
	    (handle-ambiguous-equation equation entry valid location)
	    (handle-bad-syntax-equation equation entry parses)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Not called by anyone.
(defun test-parse (eq)
 "Debugging utility to test parser."
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
    (make-noop-turn))) ; don't return anything.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun handle-bad-syntax-equation (equation se parses)
  "Given a student equation, its id and the parses, 
   creates a student entry, adds it to the *student-entries*,
   creates an error interpretation for it, and returns
   the first tutor turn of the error interpretation's hint sequence."
  (let (best)
    (dolist (parse parses)
      (when (or (null best) 
		(< (if (parse-rem parse) (length (parse-rem parse)) 0) best))
	(setf best (if (parse-rem parse) (length (parse-rem parse)) 0))))
    (unless best 
      (warn "handle-bad-syntax-equation: no best parse for ~A from ~A parses" equation (length parses))
      (setf best 0))
    (setf (StudentEntry-verbatim se) equation)
    (setf (StudentEntry-parsedeqn se) parses)
    (setf (StudentEntry-prop se) (list 'eqn equation))
    (add-entry se)
    (bad-syntax-ErrorInterp se equation 
			    :location (- (length equation) best))))

(defun list-or-join-hints (&rest hints)
  "Join hints together into a single hint, one paragraph."
  ;; Turn on experiment effect; see Bug #1940. 
  (if (random-help-experiment:help-mod-p 'no-join-hints)
      hints
      (list (reduce #'(lambda (x y) (strcat x "&nbsp; " y)) hints))))

(defun balanced-parentheses (str)
  "Test that expression has balanced parentheses"
  (loop for char across str	
	with depth = 0
	do 
	(cond
	  ((char= char #\() (incf depth))
	  ((char= char #\)) (decf depth)))
	never (< depth 0)
	finally (return (= depth 0))))
	
  
;; This returns a plain ErrorInterp:
(defun bad-syntax-ErrorInterp (entry equation &key location)
  "Given a syntactically ill-formed equation, returns a tutor turn."
  (let ((se (format nil "~A~@[<span class=\"unparsed\">~A</span>~]" 
		    (if location (subseq equation 0 location) equation)
		    (when location (subseq equation location)))))
    (cond				
      ((not (position #\= equation))
       (make-tutor-response
      entry
      (list-or-join-hints
       (format nil "Entry \"~a\" is not an equation.&nbsp; If you are trying to define a scalar quantity, ~A and use ~A instead." 
	       equation *delete-object* *text-tool*)
       "The entry needs an = sign to be an equation.")
      :diagnosis '(equation-syntax-error no-equals)
      :state +incorrect+
      :spontaneous t))
      ((> (count #\= equation) 1)
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "\"~a\" is not a single equation." se)
	 "You may enter only one equation on a line.")
	:diagnosis '(equation-syntax-error multiple-equals)
	:state +incorrect+
	:spontaneous t))
      ((search "sec" equation)
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "Syntax error in ~a." se)
	 "If you are giving a value in seconds, the correct SI symbol is just s, not sec.")
	:diagnosis '(equation-syntax-error  sec-for-seconds)
	:state +incorrect+
	:spontaneous t))
      ((search "ohm" equation :test #'char-equal) ;case insensitive
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "Syntax error in ~a." se)
	 "If you are giving a resistance in Ohms, the correct SI symbol is &Omega;, not ohms.")
	:diagnosis '(equation-syntax-error ohms-for-ohms)
	:state +incorrect+
	:spontaneous t))
      ((search "degree" equation :test #'char-equal) ;case insensitive
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "Syntax error in ~a." se)
	 "If you are expressing an angle in degrees, use \"deg\".")
	:diagnosis '(equation-syntax-error degrees-for-degrees)
	:state +incorrect+
	:spontaneous t))
      ;; BvdS:  There should be a handler for "unknown functions"
      ;; analogous to the handler for "unknown variables"
      ;; This is a work-around.
      ((and (search "log" equation) (not (search "log10" equation)))
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "Syntax error in ~a." equation)
	 "Use ln(x) for natural logarithms and log10(x) for logarithms base 10.")
	:diagnosis '(equation-syntax-error log-for-logarithm)
	:state +incorrect+
	:spontaneous t))
      ;; unbalanced parentheses.
      ((not (balanced-parentheses equation))
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "Syntax error in ~a." equation)
	 "Unbalanced parentheses.")
	:diagnosis '(equation-syntax-error unbalanced-parentheses)
	:state +incorrect+
	:spontaneous t))
      ((or (search "_ " equation) (search " _" equation))
       (make-tutor-response
      entry
      (list-or-join-hints
       (format nil "Syntax error in ~a." se)
       "There is a space next to an underscore in this equation.&nbsp; If you are using a component variable, make sure you type it as a single word without any spaces between the underscore and the rest of the variable name.")
      :diagnosis '(equation-syntax-error space-underscore)
      :state +incorrect+
      :spontaneous t))
      ;; Look for attempt at defining a variable.
      ;; Typically the parser fails at the space after the first word.
      ;; Need to distinguish from error where multiplication sign 
      ;; was forgotten.
      ;; Strategy here is to look for a RHS that consists of words and variables.
      ((and (find #\= equation)
	    (phrase-has-words (subseq equation (+ 1 (position #\= equation)))))
       (make-tutor-response
	entry
	(list
	 (format nil "\"~A\" does not look like an equation.&nbsp; If you are trying to define a scalar quantity, ~A and use ~A instead." 
		 se
		 *delete-object* *text-tool*))
	:diagnosis '(equation-syntax-error wrong-tool)
	:state +incorrect+
	:spontaneous t))
      ;; Space between number and units is a common error
      ((and location (> location 0)
	    (digit-char-p (char equation (- location 1)))
	    (alpha-char-p (char equation location)))
       (make-tutor-response
	entry
	(list-or-join-hints 
	 (format nil "Syntax error in ~A." se)
	 "Note that:  <ul><li>There must be a space between a number and a unit.&nbsp; For example:&nbsp;  2.5 m<li>Multiplication requires an explicit multiplication sign:&nbsp; d=2*r, NOT d=2r.  </ul>")
	:diagnosis '(equation-syntax-error number-units-space)
	:state +incorrect+
	:spontaneous t))
      (T 
       (make-tutor-response
	entry
	(list-or-join-hints
	 (format nil "Syntax error in ~A." se)
	 (format nil 
		 "Though I can't tell exactly what the mistake is, some possible errors are:  <ul><li>There must be a space between a number and a unit.&nbsp; For example:&nbsp;  2.5 m<li>Multiplication requires an explicit multiplication sign:&nbsp; W=m*g, NOT W=mg.<li>~A are case sensitive.<li>Units attach only to numbers, not to variables or expressions.</ul>"
		 (open-review-window-html 
		  "Unit symbols" "units.html" :title "Units"))
	 )
	:diagnosis '(equation-syntax-error nil)
	:state +incorrect+
	:spontaneous t)))))


(defun phrase-has-words (phrase)
  "See if phrase contains mostly words and variables."
  (let ((words (match:word-parse phrase))
	(total 0)
	(punct '(#\. #\, #\; #\: #\? #\!)))
    (when (cdr words)
      (dolist (word words)
	(when (or (symbols:symbols-referent word)
		  ;; Purely alpha or punctuation
		  (every #'(lambda (x) (or (alpha-char-p x)
					   (member x punct))) 
			 word))
	  (incf total)))
      ;; could adjust this fraction if we have false positives.
      (> total (* 0.8 (length words))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-ambiguous-equation (equation entry parses location)
					;(prl parses)
  (let (result bad (cont t) tmp se save)
    (dolist (parse parses)
      (when (and cont (not (member parse save :test #'equal)))
	(setf save (append save (list parse)))
	;; Build a candidate entry containing this parse. The candidate 
	;; with the winning parse will be saved permanently with add-entry 
	;; when we know which one it is.
	;; Start with given entry since that has all user parameters
	;; and modify
	(setf se (copy-StudentEntry entry))
	(setf (StudentEntry-verbatim se) equation)
	(setf (StudentEntry-parsedeqn se) parse)
	;; Answer boxes already have a prop.
	(unless (eql location 'answer)
	  (setf (StudentEntry-prop se) (list 'eqn equation)))
	(setf (StudentEntry-State se) +incorrect+)
	(setf result (parse-handler se location))
	(cond
	  ((equal +color-green+ (turn-coloring result))
	   (setf (StudentEntry-State se) +correct+)
	   ;; know this entry has winning parse so save entry now 
	   (add-entry se) 	
	   (setf tmp se)
	   (setf cont nil))
	  (t ;;(equal +color-red+ (turn-coloring result))
	   (setf bad (append bad (list (list result se))))))))
    
    (cond
      (cont
       ;; does add-entry on winning candidate
       (setf result (choose-ambiguous-bad-turn bad se)) 
       (unless result
	 (warn "Should not see this error")
	 (setf result (try-make-incorrect-reply se 'handle-ambiguous-equaation-1))))
      (t
       ;; Record correct eqn in algebra. (Must happen before interpretation 
       ;; testing)
       ;; NB: If we later reject it for some reason (because forbidden, 
       ;; premature, etc), algebra slot should be cleared.
       (setf result
	     (if (stringp (solver-studentAddOkay (StudentEntry-Id se) 
						 (StudentEntry-ParsedEqn se)))
		 ;; to trap exceptions
		 (try-make-incorrect-reply se 'handle-ambiguous-equation-2)
		 (interpret-equation tmp location)))
       (cond
	 ((equal +color-green+ (turn-coloring result))
	  (sg-Enter-StudentEntry se)
	  
	  ;; also enter scalar variables whose only uses are in this 
	  ;; entry's interp
	  (let ((eqn-interp (StudentEntry-Cinterp se))
		unneeded-vardefs)
	    
	    ;; collect list of variable entries no longer needed
	    (when eqn-interp 
	      ;; if interp is empty, don't reduce #'union NIL, Bug 949
	      (dolist (var (reduce 
			    #'union 
			    (mapcar #'(lambda (sysent) 
					(vars-in-eqn (sysent-algebra sysent)))
				    eqn-interp)))
		(when (and (var-to-sysentry var) ;nil for vector quantities, ignore
			   (subsetp (syseqns-containing-var var) eqn-interp))
	          (pushnew (var-to-sysentry var) unneeded-vardefs))))
	    (when unneeded-vardefs
	      ;; temporarily munge this entry's interpretations to get 
	      ;; variable definition entries associated with it to be 
	      ;; marked as entered by this student entry, restore when done. 
	      ;; Note sg-delete-StudentEntry adjusted to undo this on 
	      ;; equation entry deletions.
	      (when *debug-help* 
		(format t "entering unneeded vardefs: ~s~%" unneeded-vardefs))
	      (setf (StudentEntry-Cinterp se) unneeded-vardefs)
	      (sg-Enter-StudentEntry se)  ;mark grade
	      (setf (StudentEntry-Cinterp se) eqn-interp)))
	  )
	 (t
	  ;; empty slot since it failed
	  (solver-studentEmptySlot (StudentEntry-Id se))
	  
	  ;; Identical to code in Check-NonEq-Entry in Help/Entry-API.cl
	  ;; run whatswrong help to set error interp now, so diagnosis
	  ;; can be included in log even if student never asks whatswrong
	  (let ((intended (ErrorInterp-intended (diagnose se)))
		(info (make-info-provided :prop (StudentEntry-prop se)
					  :slots 1 ;should be list, but for now, just
					  :penalize t)))
	
	    (format webserver:*stdout* "++++++intended is ~S~%" intended)
	    
	    ;; Mark associated systemEntry or SystemEntries as incorrect.
	    ;; for grading purposes.
	    (update-grade-status intended +incorrect+)
	    
	    ;; Take diagnosis and add grading information.
	    ;; It looks like the "new" part does not work for multiple-choice.
	    ;; try vec1e.
	    (dolist (sysent intended)
	      (pushnew info
		       (graded-incorrects (SystemEntry-graded sysent))
		       :key #'info-provided-prop
		       :test #'unify)))))))

    ;; finally return result turn
    result))

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
;; choose-ambiguous-bad-turn: Select which of several non-correct parses to 
;; return.
;; PARAMETER: list of candidate (turn entry) pairs collected above
;; RETURN: chosen turn to use
;; Side effects: Saves chosen entry on entry list, setting its state 
;; to incorrect.

(defun choose-ambiguous-bad-turn (badlist entry)
  ;(prl badlist)
  (let (choice wrong unk uni mis err unused)
    (dolist (te badlist)
      ;; collect sets of results of each distinguished class
      ;; (format T "choose-ambiguous: parse w/tag ~a~%" (te-error-tag te))
      (case (te-error-tag te)
        (undefined-variables
	 (setf unk (append unk (list te))))
	(Unused-variables
	 (setf unused (append unused (list te))))
	(wrong-units
	 (setf uni (append uni (list te))))
	;; forgot units given for forgot-units-but-ok OR 
	;; maybe-forgot-units on assignmentp
	;; prefer it to maybe-forgot-units in mis set
	(forgot-units
	 (setf mis (cons te mis)))
	(maybe-forgot-units
	 (setf mis (append mis (list te))))
	(internal-error
	 (setf err (append err (list te))))
	;; everything else should have OK syntax, vars & units, 
	;; just plain wrong
	(otherwise 
	 ;; This scheme duplicates the weighting scheme in entryTest.
	 ;; If we assume the order of error handlers for a given
	 ;; parse is the same as the order of error handlers for 
	 ;; multiple parses, we should defer to the weighting scheme
	 ;; in entryTest.
	 ;;
	 ;; There are a substantial number of error handlers
	 ;; that are not handled by this scheme.  
	 ;; Through analysis of the log files, at least the
	 ;; following are unhandled:
	 ;; WRONG-VALUE-NON-GIVEN UNDIAGNOSED-EQN-ERROR
	 ;; SWITCHED-X-AND-Y-SUBSCRIPT
	 ;; TRIG-ARGUMENT-UNITS VAR-HAS-WRONG-TIME-SPECIFIER
	 ;; DEFAULT-SIGN-ERROR
	 ;; Surely UNDIAGNOSED-EQN-ERROR should have a very low weight
	 ;; compared to TRIG-ARGUMENT-UNITS.
	 ;;
	 ;; See Bug #1936
	 ;; This log-warn was used to find errors that are
	 ;; not properly handled:
	 (when (and nil (te-error-tag te))
	   (warn 'log-condition:log-warn
		 :tag (list 'choose-ambiguous-bad-turn (te-error-tag te))
		 :text "choose-ambiguous-bad: unknown error treated as wrong"))
	 (setf wrong (append wrong (list te))))))	
    
    ;; now look for choice in order from most charitable to least:
    ;; big OR falls through cases in order till non-NIL:
    (setf choice 
     (or  ;; inaccurate isn't used anymore, see parse-handler.
          ;; look for any wrong -- at least OK syntax, vars, units
          ;; Try to prefer simpler one, to make life simpler for WWH diagnosis
          ;; by avoiding including unnecessary DNUM mangled forms.
          (first (sort wrong #'simpler-parse))
	  ;; look for units error:
          ;; URGH Some ambiguous equations get both inconsistent and 
	  ;; missing units parses:
	  ;; one parse dnum-mangle rhs of 
	  ;; "s=-5m/s" to (* (- 5) (DNUM 1 |m/s|)) which
	  ;; then appears to have missing units on 5 if original units 
	  ;; are wrong.  This parse
	  ;; can even get forgot-units-but-ok if the value is correct. 
	  ;; Unless this is fixed or
	  ;; detected, we have to distrust forgot-units interp if any 
	  ;; "inconsistent" parses exist 
	  ;; because the forgot-units reading may just be artifact of 
	  ;; dnum mangling. 
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
    (setf (StudentEntry-State entry) +incorrect+)
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
    (< (leaf-count parse1) (leaf-count parse2))))

(defun leaf-count (x &key parent)
  "leaf count of algebraic expressions, expanding associative functions"
  (cond
    ((null x) 0)
    ((atom x) 1)
    ((listp x)
     (let ((op (car x)))
       ;; Flatten associative functions:
       (when (and (member op '(+ *)) (eql op parent)) (pop x))
       (loop for y in x sum (leaf-count y :parent op))))
    (t (warn "invalid expr ~A" x) 0)))

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

(defun contains-strings (answer)
  (handler-case 
      (do-contains-strings answer)
    (warn (c) (declare (ignore c))
	  (warn 'log-condition:log-warn
		:tag (list 'contains-strings answer)
		:text "Invalid contains-string object."))))

(defun do-contains-strings (eq)
  (cond
   ((stringp eq) (list eq))
   ((consp eq)
    (mapcan #'do-contains-strings eq))
   ((null eq) (warn "Null encountered in contains-strings."))
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
	 (answer (subst-canonical-vars 
		  ;; stringify student variables
		  (parse-pack-lhs 'unknown (parse-tree parse))))
	 (strings-in-answer (contains-strings answer)))
    (cond
     (strings-in-answer (handle-undefined-variables-equation se strings-in-answer))
     (t
      (setf answer (parse-pack-lhs 'symbol-number answer))
      (setf answer (parse-remove-lhs 'wspace answer))
      (setf answer (parse-remove-lhs 'r-paren answer))
      (setf answer (parse-remove-lhs 'l-paren answer))
      (setf answer (parse-pack-lhs 'unit-symbol answer))
      (setf answer (parse-pack-lhs 'number answer))
      (setf answer (parse-pack-lhs 'integer answer))
      (setf answer (parse-pack-lhs 'plus-minus answer))
      (setf answer (parse-pack-lhs 'times-div answer))
      (setf answer (parse-pack-lhs 'raised answer))
      ;; (format webserver:*stdout* "answer0 ~S~%" answer)
      (setf answer (parse-pack-lhs 'func answer))
      ;; (format webserver:*stdout* "answer1 ~S~%" answer)
      (setf answer (parse-to-prefix answer))
      ;; (format webserver:*stdout* "answer2 ~S~%" answer)
      (setf answer (flatten-associative answer))
      ;; (format webserver:*stdout* "answer3 ~S~%" answer)
      (setf answer (denum-mangle answer))
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
	 (warn 'log-condition:log-warn  
	       :tag 'parse-handler-inaccurate
	       :text "inaccurate in parse-handler")
	 (diagnose se)
	 (try-make-incorrect-reply se 'parse-handler-1))
	(wrong 
	 (diagnose se)
	 (try-make-incorrect-reply se 'parse-handler-2))
	;; Following mainly occurs for parses giving rise to bad syntax
	;; equations. Usually when this happens another parse will
	;; produce legal equation so its not a problem. But we need 
	;; to record this status to prefer other parses if eqn wrong.
	(solver-exception 
	 (solver-exception-interp se))
	(otherwise
	 (make-green-turn :id (StudentEntry-id se))))))))

;;
;; Note: several canned routines here for particular error interpretations are
;; used for errors that provide unsolicited messages. These routines:
;;    1. create a hint sequence turn for use in the remediation field (rem)
;;    2. construct an error interpretation object (ei) containing rem
;;    3. set the error interp field of the student entry to ei
;;    4. set coloring on the rem turn to color red
;;    5. return rem for use as a final result turn
;; Unsolicited feedback results as the remediation's red+hint turn is returned 
;; up the stack for use as the final result turn for the equation entry.
;; If this is not returned, the hint sequence is saved with the entry but
;; is not given until student asks whats wrong.


;; forgot-units is returned when equation is dimensionally inconsistent but
;; balances numerically when numbers are treated as having unknown units.
(defun forgot-units-ErrorInterp (se)
  "Given a student entry, return a tutor turn that gives unsolicited 
feedback saying that the student forgot to put units on at least one number.
Also create an error interpreation in case the student asks a follow-up 
question, and put it in the student entry's err interp field."
  (make-tutor-response
   se
   '("Forgot to put units on a number."
     "This equation is dimensionally inconsistent.&nbsp; When numbers are used in equations, they must include the appropriate units.&nbsp; It looks like one of the numbers you've used is lacking the units.")
   :diagnosis '(forgot-units)
   :state +incorrect+

    ;; Follow Raj experiment, experimental condition to make
    ;; this an unsolicited hint.  However, we follow the general 
    ;; policy of making physics-related hints solicited.
    ;; We do want students to learn the skill of recognizing and
    ;; self-correcting such errors.
    ;; Might want to make this unsolicited if the student has not
    ;; mastered some skill?  Which skill?
   :spontaneous nil))


(defun assignment-eqn (parsed-eqn)
  "true if given prefix eqn parse is a numerical assignment statement"
  (and (consp parsed-eqn)           ;just sanity checks on argument
       (eq (first parsed-eqn) '=)   
       (= (length parsed-eqn) 3)
       ;; predicate defined in errors.cl takes (lhs rhs)
       (assignmentp (second parsed-eqn) (third parsed-eqn))))


;; maybe-forgot-units is returned when equation is dimensionally 
;; inconsistent but could be dimensionally OK if numbers are treated as 
;; having unknown units -- though it STILL fails to balance acceptably. 
;; So we are unsure what the true cause of the inconsistency is, 
;; but can suggest maybe they forgot units.  If this occurs for a simple 
;; numerical assignment statement we promote the response to the more 
;; definite "forgot units" message:  The value may be wrong but we are 
;; still sure they have forgotten the units on a number.
(defun maybe-forgot-units-ErrorInterp (se)
  "Given a student entry, return a tutor turn that gives unsolicited 
feedback saying that the student appears to have left units off at 
least one number.  Also create an error interpreation in case the 
student asks a follow-up question, and put it in the student entry's 
err interp field."
  ;; in case of a simple assignment statement, change to forgot-units 
  ;; error interpretation
  (when (assignment-eqn (StudentEntry-ParsedEqn se))
    (return-from maybe-forgot-units-ErrorInterp (forgot-units-ErrorInterp se)))
  (make-tutor-response
   se
   '( "The units in this equation are not consistent.&nbsp;  If this is a symbolic equation, there is probably an error:  check all your terms.&nbsp;  Another possibility is that a number has been used without correct associated units.")
   :diagnosis '(maybe-forgot-units)
   :state +incorrect+))

;; If this is a simple numerical assignment statement, we can say more 
;; specifically that units are wrong.
(defun wrong-units-ErrorInterp (se)
  "Given a student entry, return a tutor turn giving unsolicited feedback 
saying that the student equation has a dimensional inconsistency.  
Also create an error interpreation in case the student asks a 
follow-up question and put it in the student entry's err interp field."
  (make-tutor-response
   se
   '("Units are inconsistent.")
   :diagnosis '(wrong-units)
   :state +incorrect+
   ;; See comments in forgot-units-ErrorInterp
   ))

(defun solver-exception-interp (se)
  ;; To tag buggy unprocessable parse so can prefer others. Hopefully won't 
  ;; ever show this to students.
  (make-tutor-response
   se
   '("Internal error: could not process equation.")
   :diagnosis '(internal-error)
   ;; Result should not be red/green since we don't know its
   ;; status.
   :spontaneous t))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun handle-undefined-variables-equation (se strings)
  "Given a student entry and a list of strings, return a tutor turn and set the err-interp field of the student entry"
  (let ((defined-but-not-sysvars (remove-if-not #'symbols-lookup strings)))
    (if defined-but-not-sysvars
	(unused-variables-ErrorInterp se defined-but-not-sysvars)
	(undef-variables-ErrorInterp se strings))))


(defparameter *unknown-variable-error-hint*
  (strcat 
   "<ul>"
   "<li>Variables must be defined before being used in an equation.&nbsp;  Vectors are defined with " *vector-tool* " and scalars are defined with " *text-tool* "." 
   "<li>" (open-review-window-html "Unit symbols" "units.html" :title "Units")
   " are case sensitive:&nbsp;  <var>N</var> and <var>n</var> are not the same."
   "<li>Multiplication requires an explicit multiplication sign:  W=m*g, NOT W=mg."
   "</ul>"))

(defun undef-var-error-interp (var)
  "Returns error interpretation for an undefined variable."
  ;; Possible errors in order of decreasing priority:
  ;; with underscore:
  ;;    axes not defined
  ;;    subscript doesn't match coordinates
  ;;    base quantity is a scalar (not a magnitude)
  ;;    base quantity is an object (not a vector)
  ;; without underscore:
  ;;    vector component with underscore missing
  ;;    is an object, not a scalar
  ;; either:
  ;;    capitalization
  ;;    misspelling
  ;; not handled and therefore need to be in default hint:  
  ;;   concatonation of two variables/units.
  ;;   errors associated with angle \theta...
  (let ((underscore (position #\_ var)))
    (if underscore 
	(let* ((base (subseq var 0 underscore))
	       (subscript (subseq var (+ 1 underscore))))
	  (unless (symbols-referent subscript :namespace :objects)
	    (return-from undef-var-error-interp
	      (if (symbols-label '(axis . ?rest) :namespace :objects)
		  (cons (cons 'coordinate-not-matched subscript)
			(format nil "The subscript of <var>~A</var> does not match any of the coordinates ~{ <var>~A</var>~}." 
				var
				(sort (mapcar #'sym-label 
					      (symbols-fetch '(axis . ?rest) 
						       :namespace :objects))
				      #'string<)))
		  (cons (cons 'component-without-coordinates var)
			(format nil
				"Vector components like <var>~a</var> are defined only after you have drawn coordinate axes.&nbsp; Use ~A to draw some axes." 
				var *axis-tool*)))))

	  (let ((match (symbols-lookup base)))
	    (when (and match (not (unify '(mag . ?rest) (sym-referent match))))
	    (return-from undef-var-error-interp
	      (cons (cons 'component-for-scalar (sym-referent match))
		    (format nil "<var>~a</var> is a scalar quantity.&nbsp; It does not have components." 
			    (sym-label match))))))

	  (let ((match (symbols-lookup base :namespace :objects)))
	    (when (and match 
		       (not (vector-p (sym-referent match))))
	    (return-from undef-var-error-interp
	      (cons (cons 'component-for-object (sym-referent match))
		    (format nil "<var>~a</var> is not a vector." 
			    (sym-label match)))))))

	;; regular variable
	(progn 
	  (let ((match (match-var-to-compos-without-underscore var)))
	    (when match
	      (return-from undef-var-error-interp
		(cons (cons 'dropped-underscore (sym-referent match))
		      (format nil "Vector components are written with an underscore;  write <var>~A</var> instead of <var>~A</var>." 
			      (sym-label match) var)))))

	  (let ((match (symbols-lookup var :namespace :objects)))
	    (when (and match 
		       (not (vector-p (sym-referent match))))
	    (return-from undef-var-error-interp
	      (cons (cons 'variable-for-object (sym-referent match))
		    (format nil "<var>~a</var> is not a scalar quantity.&nbsp; It cannot be used in an equation." 
			    (sym-label match)))))))))

  (let ((match (capitalization-match-var var)))
    (when match
      (return-from undef-var-error-interp
	(cons (cons 'capitalization (sym-referent match))
	      (format nil "Variable names are case sensitive:&nbsp;  You probably meant <var>~A</var> instead of <var>~A</var>." 
		      (sym-label match)
		      var)))))
  
  (let ((matches (near-miss-vars var)))
    (when matches
      (return-from undef-var-error-interp
	(cons (cons 'misspelling (mapcar #'sym-referent matches))
	      (format nil "Spelling error?&nbsp; Perhaps you meant to write <var>~A</var> instead of <var>~A</var>." 
		      (sym-label (car matches))
				 var)))))

  ;; Default
  (cons (cons 'unknown-variable var)
	*unknown-variable-error-hint*))

(defun vector-p (prop)
  (let ((qexp (lookup-expression-struct prop)))
    (and qexp (eql (exptype-rank qexp) 'vector))))

(defun match-var-to-compos-without-underscore (var)
  (find-if 
   #'(lambda (sym) (equal var (remove #\_ (sym-label sym))))
   (symbols-fetch '(compo . ?rest))))

(defun undef-variables-ErrorInterp (entry undef-vars)
  "Given a list of undefined vars (as strings), returns the error interpretation that will be both stored in the student entry and used to give the student an unsolicited warning."
  (let ((interp (some #'undef-var-error-interp undef-vars)))
    (make-tutor-response
     entry
     (list
      (format nil "Undefined variable~:[~;s: ~]~{ <var>~a</var>~}." 
	      (cdr undef-vars) undef-vars)
      (cdr interp))
     :assoc (list (car interp))
     :state +incorrect+
     :spontaneous t
     :diagnosis (cons 'Undefined-variables undef-vars))))

(defun unused-variables-ErrorInterp (entry undef-vars)
  "Given a list of unused vars (as strings), returns the error interpretation that will be both stored in the student entry and used to give the student an unsolicited warning."
  (make-tutor-response
   entry
   (list
    (if (null (cdr undef-vars))
	(format nil "The variable ~a is not used in any solution I know of." (car undef-vars))
	(format nil "These variables are not used in any solution I know of: ~a." undef-vars))
    "I can only recognize equations and variables from solutions I know about. This variable is not used in any of the solutions I have recorded for this problem. It's possible you are pursuing a solution that I don't know about, but if so, I can't help you with it and simpler solutions are probably available.")
   :state +incorrect+
   :spontaneous t
   :diagnosis (cons 'Unused-variables undef-vars)))


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
;;
;; strip comment and leading and trailing spaces from an eqn string.
(defun trim-eqn (eq)
  (if (> (length eq) 0)
      (let ((p (position #\; eq)))
	(if p
	    (string-trim match:*whitespace* (subseq eq 0 p))
	  (string-trim match:*whitespace* eq)))
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
(defun denum-mangle (expr)
  (cond
    ((atom expr) expr)
    ((eq (car expr) 'dnum)
     (if (consp (second expr))
	 (list '* (second expr) (list 'dnum 1 (third expr)))
	 expr))
    (t (cons (car expr) (mapcar #'denum-mangle (cdr expr))))))
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

;; For answers, we allow student to enter either a full assignment equation 
;; of form sought_var = rhs or just the answer expression rhs alone. 
;;
;; If they didn't enter a full equation, we effectively construct an 
;; appropriate equation.  If a student variable for the sought is defined, 
;; we can use that to construct an equation;
;; else we temporarily install a student variable "Answer" for use while 
;; constructing a student equation to test. 
;;
;; There are further tests we apply that are specific to answers:
;; - We have to check that the rhs has the form required for an answer. 
;; To allow for answers as complex expressions in terms of parameters, 
;; we make sure there are no non-parameter variables in the rhs expression. 
;; !! Note this currently allows complex arithmetic expressions to be accepted,
;; even in problems for which no parameters are defined. !! 
;; Should also allow answers in terms of constants like pi.
;; - If they typed a full equation, we have to verify it is an assignment 
;; statement and lhs is the correct variable.
;; Once we have an equation we can test the answer equation up to answer 
;; accuracy to see if its correct.
;;

(defun check-answer (entry)
  (let*
      ;; This is the only place where StudentEntry-verbatim is used.
      ((inputo (StudentEntry-verbatim entry))
       sought-quant
       (id (StudentEntry-id entry))
       result-turn
       (input (trim-eqn inputo)))
    
    ;; Determine which answer this entry corresponds to,
    ;; if this is a new entry.
    (unless (StudentEntry-prop entry) 
      (unless (select-sought-for-answer entry)
	(return-from check-answer 
	  (extra-answer-ErrorInterp entry))))
    
    (setf sought-quant (second (studentEntry-prop entry)))
    
    (if (quant-to-sysvar sought-quant)
	(if (/= (length (remove #\Space input)) 0)
	    (let* ((ep (position #\= input))
		   ;; student's var for sought quant, maybe NIL
		   (stud-var (symbols-label sought-quant))
		   (lhs (if ep (remove #\Space (subseq input 0 ep)) nil))
		   (rhs (if ep (subseq input (+ ep 1) (length input)) input))
		   (valid t)
		   why)
	      (if ep ; student entered a complete equation
		  (if (/= (length (remove #\Space rhs)) 0)
		      (if (/= (length (remove #\Space lhs)) 0)
			  (let ((nvar (subst-canonical-vars (list lhs))))
			    (cond
			      ;; lhs expr is (or contains) undefined var
			      ((contains-strings nvar) 
			       (setf why (list 'bad-var lhs))
			       (setf valid nil))
			      ;; lhs = student's var for sought 
			      ((and stud-var (equalp lhs stud-var))) 
			      (t (setf why (list 'bad-sought lhs))
				 (setf valid nil))))
			  ;; else eqn has empty lhs, 
			  ;; e.g. student typed "= 5 N". Allow it.
			  (setf lhs (if stud-var stud-var "Answer")))
		      ;; else eqn has empty rhs, so bad.
		      (setf valid nil))
		  ;; else student only entered rhs: fill in lhs student variable.
		  (setf lhs (if stud-var stud-var "Answer")))
	      (unless stud-var
		;; !! NB: want to delete this temp in all paths
		;; The function default-wrong-answer has a test for 
		;; this variable name.
		(symbols-enter "Answer" sought-quant :entries (list id)))
	      (if valid
		  (let* ((parses (parse-equation 
				  **grammar** 
				  (string-trim match:*whitespace* rhs)))
			 (complete (parse-get-complete parses))
			 (valid (parse-get-valid 'expr complete)))
		    ;;(format t "Parsed ~A~%" (concatenate 'string lhs "=" (string-trim " " rhs)))
		    ;;(format t "Okay parse!!!~%~A~%" valid)
		    (if valid
			(cond
			  ;; check no non-parameter vars in answer expression
			  ((not (bad-vars-in-answer valid)) 
			   ;;(format t "Okay here!!!~%")
			   ;; need to make a fake entry with different 
			   ;; id so real entry is not overwritten.
			   ;; Fake entry is deleted below.
			   ;; Use same prop as original entry.
			   (let ((temp-entry (copy-studententry entry)))
			     (setf (studententry-id temp-entry)
				   ;; Name used by function undo-entry
				   'check-answer-equation)
			     (setf result-turn 
				   (do-lookup-equation-string
				       (concatenate 'string lhs "=" rhs)
				     temp-entry 
				     'answer)))
			   ;; The result will reference the fake entry,
			   (setf (turn-id result-turn) id)
			   ;;
			   ;; Copy relevant entry state into real answer 
			   ;; entry and remove temp.
			   ;;
			   (let ((temp-entry (find-entry 
					      'check-answer-equation)))
			     (setf (StudentEntry-State entry)
				   (StudentEntry-State temp-entry))
			     (setf (StudentEntry-ErrInterp entry)
				   (StudentEntry-ErrInterp temp-entry))
			     ;; parse maybe useful ??
			     (setf (StudentEntry-ParsedEqn entry) 
				   (StudentEntry-ParsedEqn temp-entry)))
			   ;; remove temp from saved entry list
			   ;; clears algebra slot
			   (delete-object 'check-answer-equation)
			   ;; remove from equation parser's grammar 
			   ;; (safe if never added)
			   (grammar-remove-variable "Answer")
			   (symbols-delete "Answer"))

			  (T ; answer has non-parameter vars
			   (setf result-turn
				 (bad-variables-vs-parameters-ErrorInterp
				  entry
				  input 
				  (bad-vars-in-answer valid)))))
			  
			;; valid is null: parse failed.
			;; Note re message that input might not have 
			;; been a full equation.
			;; Ignore reply.
			(setf result-turn 
			      (bad-syntax-ErrorInterp 
			       entry
			       (if ep input (strcat lhs "=" rhs))))))

		  ;; Failed to get a candidate to test.
		  (setf result-turn
			(cond 
			  ((and why (equal (car why) 'bad-var))
			   (bad-answer-bad-lhs-ErrorInterp entry
						     input why))
			  ((and why (equal (car why) 'bad-sought))
			   (bad-answer-bad-sought-ErrorInterp entry
							input why))
			  (t 
			   (bad-answer-syntax-ErrorInterp entry input))))
		  ))
	    
	    ;; Empty answer box (this is usually just a mistake).
	    (setf result-turn (empty-answer-ErrorInterp entry)))
	
	;; quant-to-sysvar failed.
	(warn "No system variable for ~A. Possible mismatch with answer box." 
	      sought-quant))
    
    ;; Since var=value equation is not among SystemEntries, 
    ;; need to find associated SystemEntry by hand and upgrade
    ;; its status.
    (let ((sysent (find-SystemEntry (StudentEntry-prop entry))))
      (if sysent
	  (progn
	    (if (eql (StudentEntry-state entry) +correct+)
		(pushnew entry (SystemEntry-entered sysent))
		(setf (SystemEntry-entered sysent) nil))
	    ;; Grading for result.  
	    (update-grade-status (list sysent) (StudentEntry-state entry)))
	  (warn 'log-condition:log-warn :text "No matching systementry for box"
		:tag (list 'box-no-systementry (StudentEntry-prop entry)))))
    
    (unless (turn-p result-turn)
      (warn 'log-condition:log-warn :text "No reply turn for answer"
	    :tag (list 'box-no-reply (StudentEntry-prop entry)))
      (return-from  check-answer
	(make-tutor-response
	 entry
	 '("Unable to evaluate answer.&nbsp;  Please try another problem.")
	 :spontaneous t)))
    
    result-turn))


(defun select-sought-for-answer (entry)
  "Decide which answer this entry corresponds to by looking at other exisiting answers.  Fills StudentEntry-prop based on this."
  ;; Get list of all other existing answer boxes and done buttons.
  ;; Don't include multiple-choice or checkboxes.
  (let* ((previous (remove-if
		    #'(lambda (x) (not (member x '(answer done))))
		    (mapcar #'StudentEntry-prop 
			    (remove entry *StudentEntries*))
		    :key #'car))
	 ;; determine which answers don't have a box or done button.
	 (remaining (remove-if
		     #'(lambda (x) (member x previous
				   :key #'second :test #'unify))
		     ;; remove multiple-choice and checkboxes.
		     (remove 'choose-answer (problem-soughts *cp*) 
			     :key #'car))))

    ;; Assume answer boxes are ordered according to creation
    ;; time (which is what we do when creating a problem).
    ;; Use first answer which does not have an associated box.
    ;; This policy produces the expected behavior if a student moves
    ;; an answer box or deletes and re-creates an answer box.
    (when remaining
      (setf (StudentEntry-prop entry) (list 'answer (car remaining))))))

(defun bad-answer-bad-lhs-ErrorInterp (entry equation why)
  "LHS of equation is not a variable."
  (declare (ignore equation))
  (make-tutor-response 
   entry
   (list
    (format nil "'~A' is not a defined variable." (second why)))
   :state +incorrect+
   :diagnosis '(answer-sought-is-undefined)
   :spontaneous t))

(defun bad-answer-bad-sought-ErrorInterp (entry equation why)
  "Answer is malformed"
  (declare (ignore Equation)) ;suppressing warning.
  (make-tutor-response 
   entry
	      (list
	       "Answers can be expressed as a simple equation with the name of the variable for the quantity that the problem asks you to find on the left hand side of equation."
	       (format nil "'~A' is not the value we are looking for." (second why)))
   :state +incorrect+
   :diagnosis '(answer-is-not-sought)
   :spontaneous t))

(defun empty-answer-ErrorInterp (se)
  "Unsolicted hint for answer box with no content."
  (make-tutor-response 
   se
   '("This is an answer box.&nbsp;  When you have an answer, add it to this box.")
   :diagnosis '(empty-answer)
   :spontaneous t))

(defun extra-answer-ErrorInterp (se)
  "Unsolicted hint for extra answer box."
  (make-tutor-response
   se
   '("You already have enough answer boxes.&nbsp;  You don't need to create another one.")
   :diagnosis '(extra-answer)
   :state +incorrect+
   :spontaneous t))


(defun bad-answer-syntax-ErrorInterp (entry equation)
  "Answer is malformed."
  (make-tutor-response
   entry
   (list
    (format nil "Answers can be expressed as an explicit equation assigning a value to the sought, or by giving a single value only. (~a)" equation)
    "Try removing the left-hand side of the equation.")
   :state +incorrect+
   :diagnosis '(answer-is-malformed)
   :spontaneous t))

;;; Build interpretation for disallowed variables in answer
;;; Could distinguish two cases: 1. problem asks for purely numerical answer 
;;; (so any var in answer is illegal); 2. problem asks for answer in terms of 
;;; some parameters (so only some vars illegal and we can say which are legal.)
(defun bad-variables-vs-parameters-ErrorInterp (entry equation badvars)
  "Equation has non-parameter variables in answer"
  (declare (ignore equation))
  (make-tutor-response
   entry
   (list
    (format NIL "This expression contains variables not allowed in the answer: ~a" badvars)
    "In many Andes problems, a final answer should be expressed as an explicit numerical value (with units).&nbsp; Some problems may ask you to express the answer symbolically in terms of other variables.&nbsp;  Read the problem statement to see which variables, if any, are allowed in the answer.")
   :state +incorrect+
   :diagnosis '(using-variables-in-answer)
   :spontaneous t))

;;; check a single parse tree for disallowed variables in answer
;;; returns list of disallowed student vars, NIL if none
(defun bad-vars-in (parse &optional (lhs 'unknown))
  (cond
    ((atom parse) NIL)
    ((list-begins-with-p lhs (first parse))
     (let* ((s-var (second (parse-pack (first parse))))
	    (c-var (student-to-canonical s-var)))
       ;; (format webserver:*stdout* "variables ~S ~S~%" s-var c-var)
       (unless (or
		;; always allow physical constants like G in answer expression
		;; since they are like numbers. (?Would we have a way to specify
		;; exactly which constants are allowed in answer if we wanted?)
		(get-phys-const s-var)
		;; Undefined symbols are handled separately.
		;; We don't want use the bad variables handler for them.
		(not c-var)
		;; allowed parameters.
		(canonical-var-answer-var-p c-var))
	 (list s-var))))
    ;; recursion
    (t (append (bad-vars-in (first parse) lhs)
	       (bad-vars-in (rest parse) lhs)))))

;; check for use of disallowed variables (non-answer-vars) in answer expr
;; arguments: "valid" = list of valid parses of answer value expression
;; returns: NIL if found an OK parse, 
;;          a list of bad-vars if didn't 
(defun bad-vars-in-answer (valid)
  (let (FoundOK badvars)
    (dolist (x valid)
      (when (not FoundOK)
	;;(format t "Checking <~A>~%" (parse-tree x))
	(when (not (setf badvars (bad-vars-in (parse-tree x)))) 
	  (setf FoundOK T))))
    (when (not foundOK) 
      (remove-duplicates badvars :test #'equal))))


;; check that value-str is a correct expression for given value of quant
;; RETURNS: TURN with coloring and possible message
;; MODIFIES: Entry, the studententry containing this, with state 
;;           and error interp
;;
;; Presumably this is the last check in a complex entry that has passed 
;; other tests.
;;
;; Value is normally simple number plus units, but might be complex
;; arithmetic expression including constants such as
;;      "3*\pi/4 rad/s"
;; This is similar to checking answer expressions: we must form
;; an equation in systemese, check it, and ensure that rhs is of
;; the right form. In this case must also check that value is given.
;;
;; Note the prop form for these is (EQN "studvar" "studvalue") which
;; differs from reqular equations (only to make it simple to split).
(defun check-given-value-entry (main-entry eqn-entry)
  ;; check the "subentry" alone, log its result
  ;; and copy its state back into the main entry.
  (let ((result-turn (check-given-value-eqn eqn-entry)))
    ;; log the subentry details
    (add-log-entry-info eqn-entry result-turn)
    ;; copy relevant info from subentry into main student entry
    (setf (StudentEntry-State main-entry) (StudentEntry-State eqn-entry))
    (setf (StudentEntry-ErrInterp main-entry) (StudentEntry-ErrInterp eqn-entry))
    ;; finally return result
    result-turn))

;; check a given value equation subentry 
;; Fills in subentry state with result of check
;; returns a result-turn to return for this.

(defun check-given-value-eqn (eqn-entry)
  (let* ((studvar (second (StudentEntry-Prop eqn-entry)))
         (value-str (third (StudentEntry-Prop eqn-entry)))
	 (quant (symbols-referent studvar))
	 ;; want to distinguish cases where quantity is not given, so it 
	 ;; should be left unknown, from cases where it is given, but
	 ;; the value expression is wrong or bad in some other way.
	 ;; first do simple check that quantity is given. given-p defined 
	 ;; in errors.cl.  It takes a sysvar.  It treats components as given 
	 ;; if vector mag is given and lies along axis (though not the reverse)
	 ;; use given-var-p to avoid this behavior. Note it looks for
	 ;; given flag on quantities at the bubble-graph level, not implicit 
	 ;; equations, so might not work for those.
	 (sysvar (student-to-canonical studvar))
	 ; NB: sysvar may be NIL, e.g for unused vector attribute
	 (is-optionally-given (and sysvar (optionally-given-p sysvar)))
	 (is-given (and sysvar (given-p sysvar)))
	 (is-known-constant (and sysvar (known-constantp (sysvar-to-quant sysvar))))
	 )
    
    ;; first filter case where student hasn't specified a given value 
    (cond 
     ((blank-given-value-entry eqn-entry)
      (cond (is-optionally-given 
	     (setf (StudentEntry-state eqn-entry) +correct+)
	     (make-green-turn :id (StudentEntry-id eqn-entry)))
	    (is-given 
	     (setf (StudentEntry-state eqn-entry) +incorrect+)
	     (should-be-given-ErrorInterp eqn-entry quant))
	    (is-known-constant
	     (setf (StudentEntry-state eqn-entry) +incorrect+)
	     (should-be-known-ErrorInterp eqn-entry quant))
	    (T ; quant is not given => OK
	     (setf (StudentEntry-state eqn-entry) +correct+)
	     (make-green-turn :id (StudentEntry-id eqn-entry)))))
     
     ;; get here => student specified a given value
     ((not (or is-given is-optionally-given is-known-constant))
      (setf (StudentEntry-state eqn-entry) +incorrect+)
      (not-given-ErrorInterp eqn-entry quant))
     
     ;; else the quantity does have a given value:
     (t (let*  
	    ;; form a studentese equation and check it like any other equation,
	    ;; as if it were entered in 'check-answer-equation.  This will us 
	    ;; a result turn, and record a (temp) entry struct containing its
	    ;; interp.  We remove the temp-entry when done with it. Note that
	    ;; the temp-entry != eqn-entry above, so we may have to update
	    ;; eqn-entry. (clean this up? If eqn is OK, eqn-entry will just 
	    ;; be entered again later.  Should just return the eqn entry to 
	    ;; use, and maybe set its slot before this.)
	    ((studeqn (concatenate 'string studvar "=" value-str))
	     ;; suppress normal eqn entry logging so we can do modified logging
	     ;; here, noting different errors and filling in target entry
	     (temp-entry (make-StudentEntry 
			  :text studeqn 
			  :id 'check-given-value-equation))
	     ;; test equation we just created.
	     (result-turn (lookup-eqn-string temp-entry))
	     (correct-eqn  (eq (StudentEntry-State temp-entry) +correct+)))
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
	    (setf (StudentEntry-State eqn-entry) +incorrect+) ; modify state copied above
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
	  ;; if it's correct, caller should add subentry like an 
	  ;; implicit equation
	  (delete-object 'check-given-value-equation) ;also clear algebra slot
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
	 ;; If passed the test for givenness but gets an empty interp, assume it's
	 ;; the value of an unused given. However, we are not verifying that the
	 ;; rhs is a purely arithmetic expression in this case. 
	 (null interp))))

(defun not-given-ErrorInterp (se quant)
  (make-tutor-response
   se
   (list (format nil "The value of ~a is not given in this problem. It should be marked unknown." 
		 (nlg quant 'var-or-quant)) ; use studvar if exists, else quant
	 )
   :diagnosis '(should-be-unknown)
   :state +incorrect+
   :spontaneous t))

;; fetch the systementry giving a value for this quantity.
;; A bit circuitous: systementry has entry prop which embeds
;; the algebra: '(EQN (= sysvar (DNUM ...))) 
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
  (make-tutor-response
   se
   (list (format nil "The value of ~a can be determined from the problem statement.&nbsp; It should be entered as an equation after defining the relevant variable." 
		 (nlg (quant-to-sysvar quant) 'algebra)))
   :diagnosis '(should-be-given)
   :intended (get-given-interp quant)
   :state +incorrect+
   :spontaneous t))

(defun get-known-interp (quant)
  (let ((eqn (find quant (problem-eqnIndex *cp*) :key #'eqn-exp 
		      :test #'exactly-equal)))
    (if eqn (list (eqn-algebra->sysent (eqn-algebra eqn))))))

;; Generally such quantities should be pre-defined, but this 
;; is a fall-back.
(defun should-be-known-ErrorInterp (se quant)
  (make-tutor-response
   se
   (list (format nil "You need to enter an appropriate value for ~a."  
		 (nlg quant))
	 (strcat (begin-sentence *constants-menu-action*)
		 " and find the value used in Andes.  After defining the relevant variable, write an equation giving its value."))
   :diagnosis '(should-be-known)
   :intended (get-known-interp quant)
   :state +incorrect+
   :spontaneous t))

;; delegate to wrong-given-value (var wrongval) in kb/errors.cl which 
;; applies to equations.
;; params are lhs and rhs of a systemese equation -- we better have 
;; gotten one if this is called.
(defun set-wrong-given-value-ErrorInterp (se quant)
  (let ((rem (wrong-given-value (second (StudentEntry-ParsedEqn se)) 
                                (third (StudentEntry-ParsedEqn se)))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(wrong-given-value)
       :intended (get-given-interp quant)
       :remediation rem))))

(defun more-than-given-ErrorInterp (se quant)
  (make-tutor-response
   se
   (list (format nil "Although this equation is a correct expression for the value of ~a, it does not simply state the given value." 
		 (nlg (quant-to-sysvar quant) 'algebra)))
   :diagnosis '(more-than-given)
   :intended (get-given-interp quant)
   :state +incorrect+
   :spontaneous t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subst-canonical-vars (Exp)
  "Translate expression subsituting canonical vars for student vars, or standard vars throughout."
  (handler-case 
      (do-subst-canonical-vars Exp)
    (warn (c) (declare (ignore c))
	  (warn 'log-condition:log-warn
		:tag (list 'subst-canonical-vars Exp)
		:text "Invalid subst-canonical-vars object."))))

(defun do-subst-canonical-vars (Exp)
  ;; NB: student variables must be *strings* in Exp, not symbols
  ;; Strings with no matching expression pass through translation unchanged.
  (cond ((stringp exp)
	 (or (symbols-sysvar exp) ;find canonical variable
	     (get-phys-const exp) ;special symbols recognized by solver
	     exp))
	((and exp (atom Exp)) Exp)
	((consp Exp) (mapcar #'do-subst-canonical-vars Exp))
	(T (warn "subst-canonical-vars invalid structure"))))

(defun parse-to-prefix (expr)
  "Turn parsed expression into lisp prefix form, assuming parentheses and spaces have been removed."
  (cond
    ((atom expr) expr)

    ;; binary operators
    ((and (member (car expr) '(final expr factor term pterm 
			       n-expr n-factor n-term n-pterm))
	  (member (car (third expr)) '(plus-minus times-div raised equals)))
     (list (find-symbol (string (second (third expr))))
	   (parse-to-prefix (second expr))
	   (parse-to-prefix (fourth expr))))

    ;; unary +/-
    ((and (member (car expr) '(expr n-expr pterm n-pterm))
	  (eq (car (second expr)) 'plus-minus))
     (list (find-symbol (string (second (second expr))))
	   (parse-to-prefix (third expr))))

    ;; functions
    ((and (member (car expr) '(term n-term))
	  (eq (car (second expr)) 'func))
     (list
      (find-symbol (string-upcase (second (second expr))))
	   (parse-to-prefix (third expr))))

    ;; read numbers, if possible
    ((eq (car expr) 'number)
	 (or (let ((*read-default-float-format* 'double-float))
	       (read-from-string (second expr))) 
	     (second expr)))

    ;; intern pi
    ((eq (car expr) 'symbol-number)
	 (intern (second expr)))

    ((eq (car expr) 'unit)
     (parse-unit-to-prefix expr))

    ;; fall-through
    ((and (member (car expr) '(expr factor term pterm 
			       n-expr n-factor n-term n-pterm
			       unknown))
	  (= (length expr) 2))
     (parse-to-prefix (second expr)))

    (t (cons (car expr) (mapcar #'parse-to-prefix (cdr expr))))))

(defun flatten-associative (expr)
  "flatten associative operators in prefix-form algebraic expression."
  (if (atom expr) 
      expr
      (let ((x (car expr))
	    (rest (mapcar #'flatten-associative (rest expr))))
	(if (member x '(+ *))
	    (cons x (mapcan 
		     #'(lambda (y) (if (and (consp y) (eq (car y) x)) 
				       (cdr y) (list y))) 
		     rest))
	    (cons x rest)))))

(defun parse-unit-to-prefix (expr)
  "Turn parsed unit expression into lisp prefix form, assuming parentheses have been removed."
  (cond
    ((atom expr) expr)
    
    ;; binary operators
    ((and (member (car expr) '(unit unit-term))
	  (member (car (third expr)) '(times-div raised period)))
     (list (find-symbol (string (cadr (third expr))))
	   (parse-unit-to-prefix (cadr expr))
	   (parse-unit-to-prefix (fourth expr))))

    ;; read exponent integer
    ((eq (car expr) 'integer)
	 (or (let ((*read-default-float-format* 'double-float))
	       (read-from-string (cadr expr))) 
	     (cadr expr)))

    ;; intern unit symbols
    ((eq (car expr) 'unit-symbol)
     (intern (cadr expr)))

   ;; fall-through
    ((and (member (car expr) '(unit unit-term))
	  (= (length expr) 2))
     (parse-unit-to-prefix (cadr expr)))

    ;; Allow recursion through the parse tree.
    ;; Thus, this function can be applied to the top level of a parse tree.
    (t (cons (car expr) (mapcar #'parse-to-prefix (cdr expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of parse-andes.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
