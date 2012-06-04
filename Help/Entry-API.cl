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

(in-package :cl-user)
(eval-when (:load-toplevel :compile-toplevel)
  (use-package :symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ENTRY-API -- "adapter" functions to handle the Andes Workbench API calls
;;              for non-EQ student entries
;;
;; This module contains one entry-handler function for each Workbench entry
;; API call.  These handlers interpret the parameters sent in the API calls,
;; translating the calls into the representations needed by the help 
;; system. 
;;
;; For each API call foo, the API handler is called On-foo and takes the same
;; arguments.
;;
;; Each API handler function normally returns a Student Entry struct 
;; representing the entry.  As side-effects the handlers also update the 
;; symbol table with student label entries as appropriate and install the
;; entry on the entry list.
;;
;; An API handler may return T or NIL in case there is no good entry to be 
;; looked up; this value should be returned to the workbench.  This is 
;; currently only used for some angle labelling entries which need not 
;; correspond to solution graph actions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-----------------------------------------------------------------------------
;; Helper functions for workbench API arguments 
;;-----------------------------------------------------------------------------

;;
;; Vector directions
;;
(defun arg-to-dir (dir-arg &key mag-arg cosphi (modulus 360))
 "Convert WB API direction and magnitude argument pair to KB direction term"
  (cond ; zero-mag vectors have no direction: use special atom 'zero
        ((and (numberp mag-arg) (= 0 mag-arg)) 'zero)
	;; z-axis vectors
        ((and (numberp cosphi) (= cosphi 1)) 'out-of)
        ((and (numberp cosphi) (= cosphi -1)) 'into)
	; else should be xy plane angle in degrees. 
	((and (numberp dir-arg) 
	      ;; cosphi=0 is in the xy-plane
	      (or (not (numberp cosphi)) (= cosphi 0)))
	 `(dnum ,(mod dir-arg modulus) |deg|))
	(t (warn "arg-to-dir unrecognized form dir=~A cosphi=~A mag=~A"
		 dir-arg cosphi mag-arg))))

;;
;; Bodies
;;


(defun sym-match (sym1 sym2)
   "case independent comparison of symbol names"
   (string-equal (string sym1) (string sym2)))

(defun strcat-nonzero (&rest x)
  "If every argument is nonzero length, apply strcat, else return \"\"."
  (if (every #'(lambda (y) (> (length y) 0)) x)
      (apply #'strcat x)
      ""))

(defun best-value (best)
  (apply #'min (mapcar #'match:best-value best)))

(defmacro update-bound (best result)
  (let ((this (gensym)))
    `(let ((,this ,result))
       (when ,this (setf ,best ,this)))))

(defparameter *proposition-icons*
  `((define-var . ,*text-tool*)
    (vector . ,*vector-tool*)
    (body . ,*body-tool*)
    (draw-line . ,*line-tool*)))

(defun get-prop-icon (tool-prop)
  (or (cdr (assoc tool-prop *proposition-icons*))
      (warn "get-prop-icon bad proposition ~S" tool-prop)))

(defparameter *proposition-types*
  '((define-var . "a scalar quantity")
    (vector . "a vector quantity")
    (body . "an object")
    (draw-line . "a line")))

(defun get-prop-type (expr)
  (or (cdr (assoc expr *proposition-types*))
      (warn "get-prop-type bad proposition ~S" expr)))

(defun last-two-characters (x)
  "Last two characters of a string"
  (subseq x (max 0 (- (length x) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                  Match student phrase to Ontology
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  Should have something to handle extra stuff like setting
;;     given values in definition.  (either handle it or warning/error).

;; 1.  Score is number of words wrong plus one for the wrong tool.
;; 2.  In the event of a tie, a word error has higher weight than
;;     a tool error.
;; 3.  Assume, for determining bounds, that scores are integer 
;;     valued.  Thus, if we want to do better, than we demand the
;;     bound be decreased by 1.

(defun match-student-phrase0 (student tool-prop &key 
			      all-scalars
			      (cutoff-fraction 0.4) 
			      ;; If larger, too slow on benchmarks
			      (cutoff-max 3.7)) 
  "Match student phrase to Ontology, returning best matches for tool or other tool."
  ;; :cutoff-fraction is fractional length of student phrase to
  ;;     use as bound, adding 1 for the tool.  This should be adjusted to 
  ;;     balance type 1 and type 2 errors:
  ;;         bad matches that are accepted vs. 
  ;;         good matches that are missed
  ;;     See *word-cutoff* in Base/match.cl
  ;;     In the case of one word, look for match right tool using 
  ;;     cutoff-fraction, else match one word with wrong tool.
  ;;     For more than one word, look for match with right tool
  ;;     using cutoff-fraction.  If match exists, use score minus 1
  ;;     subsequent wrong tool match.  If match does not exist, use
  ;;     same bound for wrong tool match.
  ;;         
  ;; :cutoff-max is maximum allowed score to use as bound.  This should
  ;;     be adjusted to so that very long student phrases can
  ;;     be matched quickly enough.
  (let* ((sysentries (remove (cons tool-prop '?rest) *sg-entries*
			     :key #'SystemEntry-prop :test-not #'unify)) 
	 (initial-cutoff (min (* cutoff-fraction (length student))
			      cutoff-max))
	 ;; To speed up best-wrong-match for long phrases, maybe reduce 
	 ;; max cutoff a bit.
	 (wrong-cutoff-max (- cutoff-max 0))
	 ;; For any debugging prints in matching package.
	 ;; This also applies to any wrong matches below.
	 (*standard-output* webserver:*stdout*)
	 (best
	  (match:best-model-matches 
	   student
	   ;; Sought quantities can be any scalar, including ones
	   ;; that are only defined as side-effects.
	   (if all-scalars
	       (mapcar #'(lambda (x)
			   (cons (expand-vars (new-english-find x))
				 (list nil x)))
		       (mapcar #'qvar-exp (problem-varindex *cp*)))	
	       (mapcar #'(lambda (x) 
			   (cons (expand-vars (SystemEntry-model x)) 
				 (SystemEntry-prop x)))
		       sysentries))
	   :cutoff initial-cutoff))
	 ;; The value of the best correct match or the initial cutoff.
	 ;; This is used to determine cutoffs for wrong quantity searches.
	 (wrong-bound (if best (- (best-value best) 1) initial-cutoff))
	 wrong-tool-best)

    (when nil ;debug print
      (format t "Best match to ~s is~%   ~S~% from ~S~%" 
	      (when student (match:word-string student))
	      (mapcar 
	       #'(lambda (x) (cons (match:best-value x) 
				   (expand-vars (new-english-find
						 (reduce-prop 
						  (match:best-prop x))))))
	       best)
	      (mapcar #'systementry-prop sysentries)))
    
    ;; Attempt to detect a wrong tool error.  
    ;; The strategy is to treat the choice of tool as being
    ;; worth 1 word, when matching.
    ;;
    ;; Cases:
    ;; wrong-tool-best same as best-1 (using equiv)
    ;;     toss-up:  give unsolicited hint and pass through.
    ;; wrong-tool-best less than best score minus 1 or no best.
    ;;     this certainly should be given wrong tool help.
    (when (>= wrong-bound 0)
      (let* ((allowed-tools (remove tool-prop *tool-props-with-definitions*))
	     (sysentries (remove-if 
			  #'(lambda (x) (not (member 
					      (car (SystemEntry-prop x)) 
					      allowed-tools)))
			  *sg-entries*)))
	(setf wrong-tool-best 
	      (match:best-model-matches 
	       student
	       (mapcar #'(lambda (x) 
			   (cons (expand-vars (SystemEntry-model x)) 
				 (SystemEntry-prop x)))
		       sysentries)
	       ;; If there is no match in best, then the help is
	       ;; pretty weak.  In that case, just find anything
	       ;; below the cutoff.
	       :cutoff wrong-bound))))
    
    ;; Attempt to match to quantities not in solution,
    ;; assuming no wrong-tool error.
    ;; Never returns more than one quantity:  There is no
    ;; point in having the student resolve an ambiguity if
    ;; quantities are wrong anyway.
    
    ;; We don't have any way of handling inheritance for
    ;; non-solution quantities.  As a cheap work-around, 
    ;; take shortest matching proposition.
    ;; need to pre-order quantities so that shorter propositions
    ;; are matched first...
    (when (or (null best) (>= (best-value best) 1))
      (update-bound
       best
       (best-wrong-match
	student
	tool-prop
	:cutoff  (min wrong-cutoff-max
		      (if best 
			  (- (best-value best) 1)
			  initial-cutoff)
		      (if wrong-tool-best 
			  (best-value wrong-tool-best)
			  initial-cutoff)))))
    
    ;; Attempt to match to quantities not in solution where
    ;; the wrong tool has also been used.
    ;; Must be better than any quantity in solution, but allow
    ;; for ties with one plus any quantity not in solution.
    (when (and (>= wrong-cutoff-max 1)
	       (>= wrong-bound 0)
	       (or (null best) (>= (best-value best) 1)))
      (let  ((tool-props (intersection
			  ;; other tools that have natural-language.
			  (remove tool-prop *tool-props-with-definitions*)
			  ;; list of tools in this problem solution.
			  (mapcar #'(lambda (x) (car (SystemEntry-prop x)))
				  *sg-entries*))))
	(dolist (tool-prop tool-props)
	  (when (or (null wrong-tool-best) 
		    (>= (best-value wrong-tool-best) 1))
	    (when nil ;debug print
	      (format t "Starting wrong tool ~A:~%" tool-prop))
	    (update-bound
	     wrong-tool-best
	     (best-wrong-match 
	      student
	      tool-prop
	      :cutoff  (min (- wrong-cutoff-max 1)
			       (if best 
				   (- (best-value best) 1)
				   wrong-bound)
			       (if wrong-tool-best 
				   (- (best-value wrong-tool-best) 1)
				   wrong-bound))))))))
    
    (values best wrong-tool-best wrong-bound sysentries)))

(defun match-student-phrase (entry tool-prop)
  "Match student phrase to Ontology, returning best match prop, tutor turn (if there is an error) and any unsolicited hints."
  ;; :cutoff-fraction is fractional length of student phrase to use as bound.
  ;; :cutoff-count is maximum allowed score to use as bound.
  (let ((student (match:word-parse 
		  (pull-out-quantity (StudentEntry-symbol entry) 
				     (StudentEntry-text entry))))
	hints)
    
    (multiple-value-bind (best wrong-tool-best best-correct sysentries)
	  (match-student-phrase0 student tool-prop)
      
      ;; If there isn't a good match to solution quantities,
      ;; try another tool and non-solution quanitities.
      (when (>= best-correct 1)
	
	;; Give unsolicited hint when "...'s" is used
	(when (member "'s" student :test #'string-equal
		      :key #'last-two-characters)
	  (let  ((phr (strcat 
		       "Sorry, I don't understand "
		       (manual-link "possessives with <em>'s</em>"
				    "possessive" :pre "")
		       ".")))
	    (push `((:action . "show-hint")
		    (:text . ,phr)) hints)))
		
	) ;end of things to try if there isn't good solution match.
      
      (when nil ;debug print
	(format webserver:*stdout* 
		"**best=~A (~A) wrong-tool=~A (~A)~%" 
		(when best (best-value best)) (length best) 
		(when wrong-tool-best (best-value wrong-tool-best)) 
		(length wrong-tool-best)))
      
      (cond
	;; If wrong-tool-best exists, it contains scores
	;; that are one smaller than any scores in best.
	(wrong-tool-best
	 (values nil (wrong-tool-ErrorInterp 
		      entry 
		      tool-prop
		      (mapcar #'match:best-prop wrong-tool-best)) hints))
	
	((null sysentries)
	 (values nil (nothing-to-match-ErrorInterp entry tool-prop) hints))
	
	((null best)
	 (if (and (= (length (StudentEntry-symbol entry)) 0)
		  (Entries-need-variables (StudentEntry-type entry)))
	     ;; In this case, we can at least hint for adding a variable.
	     (values nil (add-variable-errorinterp entry) hints)
	     ;; "Can't understand your definition," and switch to NSH
	     ;; This is pretty weak help, so we want to avoid this 
	     ;; choice, when possible.
	     (values nil (no-matches-ErrorInterp entry) hints)))
	
	((= (length best) 1)
	 (let* ((prop (match:best-prop (car best)))
		;; If the best fit isn't too good, give an unsolicited hint.
		(hint (close-match-hint (best-value best) entry prop)))
	   (when hint (push hint hints))
	   
	   ;; Determine if the student has already done this
	   ;; in a previous step.
	   ;; In Andes2, this test was done on the user interface.
	   (dolist (se (remove entry *StudentEntries*))
	     (when (unify prop (studententry-prop se))
	       (return-from match-student-phrase 
		 (values nil (redundant-entry-ErrorInterp entry se prop) hints))))
	   
	   ;; Sanity test.
	   (unless prop (warn "Null prop for correct match"))
	   ;; Return the best fit entry.
	   (values prop nil hints)))
	
	(t 
	 (let ((props (mapcar #'match:best-prop best)))
	   (values nil (too-many-matches-ErrorInterp entry props) hints)))))))

;; Debug printout:
(defun test-student-phrase (student)
  "Function for testing ontology"
  (let* ((entries (remove-if 
		   #'(lambda (x) (member (car x) '(eqn implicit-eqn)))
		   *sg-entries* :key #'systementry-prop))
	 (best 
	  (match:best-model-matches 
	   (match:word-parse student)
	   (mapcar #'(lambda (x) 
		       (cons (expand-vars (SystemEntry-model x)) x))
		   entries))))
    (format t "Best match to ~S is~%   ~S~% from:~%    ~S~%" 
	    student
	    (mapcar 
	     #'(lambda (x) (cons (car x) 
				 (expand-vars (SystemEntry-model (cdr x)))))
	     best)
	    (mapcar #'(lambda (x) 
			(cons (systementry-prop x)
			      (expand-vars (SystemEntry-model x))))
		    entries))))

(defun close-match-hint (fit-value entry full-prop)
  ;; If the fit isn't too good, give an unsolicited hint.
  ;; Can't put in a tutor turn, since the turn might be good.
  ;;
  ;; This test must be adjusted empirically.
  ;; Example: [the] length of the beam
  ;;          the mass of the beam
  ;; A mismatch can be off by a variable name like
  ;; T0 vs. T1, so we need to display such small mismatches.
  (when (> fit-value 0.15)
    (let ((phr 
	   (format nil 
		   "I interpreted your definition ~@[of <var>~A</var> ~]as:&nbsp; ~A."
		   (when (> (length (StudentEntry-symbol entry)) 0)
		     (StudentEntry-symbol entry))
		   (def-np (reduce-prop full-prop)))))
      `((:action . "show-hint") (:text . ,phr)))))

(defun nothing-to-match-ErrorInterp (entry tool-prop)
  (make-tutor-response 
   entry
   (list (strcat "You don't need to use " (get-prop-icon tool-prop)
	    " to solve this problem.&nbsp; Please " *delete-object* 
	    " &amp; use another tool."))
   :assoc `((inappropriate-tool . ,tool-prop))
   :state +incorrect+
   :diagnosis '(nothing-to-match-definition)))

(defun quantity-html-link (qexp)
  "Create a link to the list of quantities for a given ExpType or just returns a string."
  (cond 
    ((stringp qexp) qexp)  ;no link, just plain text.
    ((member (exptype-rank qexp) '(scalar vector))
     (open-review-window-html
      (or (exptype-short-name qexp)
	  (progn 
	    (warn 'log-condition:log-warn 
		  :tag (list 'exptype-no-short-name (exptype-type qexp))
		  :text "ExpType missing short-name.")
	    (string-downcase (string (exptype-type qexp)))))
      "quantities.html"
      :section (string (exptype-type qexp))
      :title "Quantities" :value (exptype-type qexp)))
    (t (warn 'log-condition:log-warn
	     :tag (list 'exptype-non-quantity (exptype-type qexp))
	     :text "Exptype is not a quantity.")
       (string-downcase (string (exptype-type qexp))))))

(defun collect-distinct-quantities (full-props)
  "Collect a list of distinct ExpTypes or bodies for a list of SystemEntries."
  (delete-duplicates
   (mapcar #'quantity-exptype-or-phrase
	   (delete-duplicates (mapcar #'second full-props) :test #'unify))
   :test #'equal)) ;for the strings

(defun quantity-exptype-or-phrase (prop)
  "If prop is scalar or vector quantity, return ExpType, else return name."
  (let ((quant (lookup-expression-struct prop)))
    (if (and quant (member (ExpType-rank quant) '(scalar vector)))
	quant
	(def-np prop))))

(defun too-many-matches-ErrorInterp (entry full-props)
  ;; see nsh-sought-resp-ambiguous in NextStepHelp.cl
  (let* ((distinct-quantities (collect-distinct-quantities full-props))
	 (quantities-help (nlg-print-list
			   (mapcar #'quantity-html-link distinct-quantities)
			   "or" 'identity))
	 (ambiguous (format nil "Your definition ~:[~1*~;of <var>~A</var> ~]is ambiguous.&nbsp;  It looks like you were trying to define ~A." 
			    (> (length (StudentEntry-symbol entry)) 0)
			    (StudentEntry-symbol entry)
			    quantities-help)))
    (make-tutor-response
     entry
     (if (< (length full-props) 4)
	 (list 
	  ambiguous		    
	  (format nil "Did you mean?~%<ul>~%~{  <li>~A</li>~%~}</ul>"
		  (mapcar #'(lambda (x) 
			      (def-np (reduce-prop x)))
			  full-props)))
	 (list
	  (strcat ambiguous 
		  "&nbsp; I can help you choose what to do next:") 
	  ;; Should use props to inform starting point
	  ;; for NSH.
	  '(function next-step-help)))
     :assoc `((too-many-matches . ,full-props))
     :state +incorrect+
     :diagnosis '(definition-has-too-many-matches))))


(defun wrong-tool-ErrorInterp (entry tool-prop full-props)
  "Make a hint sequence for using other tool."
  ;; Cases:
  ;;   unique short-name match
  ;;      "Note that [short-name match] is a [vector, scalar, body, line]."
  ;;      "Delete your entry and use *tool* instead."
  ;;   more than one match, one tool
  ;;      "Are you trying to define a [vector, scalar, body, line]?"
  ;;      "If so, delete your entry and use *tool* instead."
  ;;   more than one match, multiple tools.
  ;;      "I don't think you want to use *this-tool*.
  ;;       Perhaps you should delete this entry & use another tool."
  (let* ((tool-propositions (remove-duplicates (mapcar #'car full-props)))
	 ;; there may be several matches that have the same quantity.
	 (distinct-quantities (collect-distinct-quantities full-props)))
    (make-tutor-response
     entry
     (cond ((and (= (length distinct-quantities) 1)
		 (= (length tool-propositions) 1))
	    (list
	     (strcat "Note that " 
		     (quantity-html-link (car distinct-quantities)) 
		     " is " 
		     (get-prop-type (car tool-propositions))
		     ".")
	     (strcat "If you meant to define " 
		     (get-prop-type (car tool-propositions))
		     ", please " *delete-object* " and use "
		     (get-prop-icon (car tool-propositions)) 
		     " instead.")))
	   ((= (length tool-propositions) 1)
	    (list 
	     (strcat "Are you trying to define "
		     (get-prop-type (car tool-propositions))
		     "?")
	     (strcat "If so, " *delete-object* " and use "
		     (get-prop-icon (car tool-propositions))
		     " instead.")))
	   (t
	    (list      
	     (strcat "I don't think you want to use "
		     (get-prop-icon tool-prop)
		     " for this definition.&nbsp; "
		     "Perhaps you should " *delete-object* " &amp; "
		     "use another tool."
		     "<p>I can help you decide what to do next:"
		     )	
	     ;; Should use matches to inform starting point
	     ;; for NSH.
	     '(function next-step-help))))
	   :assoc `((wrong-tool . ,full-props))
	   :state +incorrect+
	   :diagnosis '(wrong-tool-error))))

(defparameter *type-entryprop*
  '((eqn . "equation")
    (define-var . "statement")
    (body . "ellipse")
    (body . "rectangle")
    (body . "circle")
    (vector . "vector")
    (line . "line")
    (draw-axes . "axes"))
  "map between entryprops and user interface tools.")

;; helper function to guess api type from entryprop.
(defun entryprop2type (prop)
  "Determine Andes3 api \"type\" from entryprop."
  (or (cdr (assoc (car prop) *type-entryprop*))
      (warn "entryprop2type: bad prop ~A" prop)))

;; helper function to guess api type from entryprop.
(defun type2entryprop (type)
  "Determine entryprop associated with Andes3 api \"type\"."
  (or (car (rassoc type *type-entryprop* :test #'string-equal))
      (warn "type2entryprop: bad prop ~A" type)))

(defun Entries-need-variables (typein)
  "Determine whether uncompleted SystemEntries of this type generate any variables in the algebra."
  (let* ((type (type2entryprop typein))
	 ;; Select uncompleted SystemEntry props of this type
	 (props (delete type 
			(mapcar #'SystemEntry-prop 
				(remove-if #'SystemEntry-entered 
					   *sg-entries*))
			:key #'car :test-not #'eql))
	 ;; Retrieve quantity props, getting parent props for any
	 ;; components, angles, or magnitudes.
	 (quants (mapcar #'(lambda (x) (get-vector-parent-prop (qvar-exp x)))
			 (problem-varindex *cp*))))
    (some #'(lambda (prop) (member prop quants :test #'unify))
	  (mapcar #'second props))))

(defun Add-variable-errorInterp (entry)
  (make-tutor-response
   entry
   (list (strcat "You should " *define-variable* 
		 " for this entry.")
	 (strcat "Double-click on the text box and " 
		 *define-variable* "."))
   :state +incorrect+
   :diagnosis '(no-variable-defined nil)
   :spontaneous t))

(defun no-matches-ErrorInterp (entry)
  (let ((equal-sign (when (find #\= (StudentEntry-text entry))
		      (strcat "If you are trying to write an equation, "
			      *delete-object* " and use "
			      *equation-tool* " instead."))))
    (make-tutor-response
     entry
     (list (format nil "Sorry, I don't understand your ~:[~1*entry~;definition of <var>~A</var>~].~@[&nbsp; ~A~]&nbsp; I can help you choose what to do next:" 
		   (> (length (StudentEntry-symbol entry)) 0)
		   (StudentEntry-symbol entry) equal-sign)
	   '(function next-step-help))
     :assoc '((no-matches . nil))
     :state +incorrect+
     :diagnosis '(definition-has-no-matches))))


;; This was based on examples in parse-andes.cl
;; Most unsolicited hints in Andes2 were associated with equations.
(defun redundant-entry-ErrorInterp (se old prop)
  "Given a student entry, return a tutor turn giving unsolicited feedback saying that the entry has already been done.  Also create an error interpretation in case the student asks a follow-up question, and put it in the student entry's err interp field."
  (make-tutor-response
   se
   (list (format nil 
		 "You have already defined ~A~:[ as ~A~1*~;~1* to be <var>~A</var>~]."
		 (def-np (reduce-prop prop))
		 (> (length (StudentEntry-symbol old)) 0)
		 (studentEntry-text old)
		 (StudentEntry-symbol old)))
   :assoc `((redundant-entry . ,prop))
   :diagnosis '(already-defined) 
   :state +incorrect+
   :spontaneous t))


(defun pull-out-quantity (symbol text &key (test #'string=))
  "Pull the quantity phrase out of a definition:  should match variablename.js"
  (when (> (length symbol) 0) ;variablename.js returns empty string on no match
    (if (not (search symbol text :test test))
	(warn 'log-condition:log-warn
	      :tag (list 'symbol-definition-mismatch symbol text)
	      :text "Symbol does not match definition.")
	;; Find first occurence of symbol in text and take rest of text.
	;; this should be done as a parser.
	(let* ((si (+ (search symbol text :test test) (length symbol)))
	       (nosym (string-left-trim match:*whitespace* (subseq text si))))
	  ;; Find any subsequent equality in string.
	  ;; The empty string is a catch-all in case there is no match
	  (dolist (equality '("is " ":" "=" "be " "as " "to be " ""))
	    (when (and (>= (length nosym) (length equality))
		       (string= equality (string-downcase nosym) 
				:end2 (length equality)))
	      (return-from pull-out-quantity
		(string-trim match:*whitespace* 
			     (subseq nosym (length equality)))))))))
  text)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;            Handle case where there is no text.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; There is a question whether this should be considered a "user interface"
;; or physics skill.  So far, we have treated this as a user interface
;; skill, giving an unsolicited hint and not coloring the object.
;; For the experiment, we treat this as a "physics skill" in the control
;; group and a user interface skill in experimental group.

(defun no-text-handler (entry)
  "Return unsolicited hint for entry with no text."
  (make-tutor-response
   entry
	  (list "Generally, objects should be labeled."
		(strcat "Select the "
			;; We use "text tool" in the hints and manual.
			(if (string-equal (StudentEntry-type entry) 
					  "statement") 
			    "text"
			    (StudentEntry-type entry))
			" again, double-click on " 
			"the text box and " *add-label* "."))
	  :assoc `((no-label . ,(StudentEntry-type entry)))
	  :diagnosis (cons 'no-label (StudentEntry-type entry))
	  :state +incorrect+
	  ;; Student gets unsolicited hint if they have not mastered skill.
	  :spontaneous (incremented-property-test 'object-with-label 3)))

(defun with-text-handler ()
  "Update skill of making entry with text"
  (model-increment-state-property 'object-with-label))

(defun no-text-handler-test (entry prop)
  (and (= (length (string-trim match:*whitespace* 
			       (studentEntry-text entry))) 0)
       (member prop *sg-entries* 
	       :key #'(lambda (x) (car (SystemEntry-prop x))))))
		 
;;-----------------------------------------------------------------------------
;; Workbench Entry API Handler functions
;;-----------------------------------------------------------------------------

;;
;; Following entry handling calls are ordered in roughly the order the student 
;; should follow: First choose body, then draw vectors and other diagram 
;; entries, maybe define some variables, draw axes, maybe draw components, 
;; then write equations, solve for desired variable and enter answer. 
;;

;; Note on entry ids and symbol table manipulations:
;;
;; The workbench allows the student to modify an existing entry by editing 
;; its properties in a dialog. In this case the workbench sends the same entry 
;; id for the revised submission as it did with the first. This edit can 
;; modify any property of the entry including the label.  The effect should 
;; be just the same as if the student had deleted the existing entry and 
;; then submitted a new one with the same id.
;;
;; To handle this possibility we have to be sure to delete any existing entry 
;; and undo all its effects on our state (on the symbol table entries, e.g.) 
;; before updating our state with the new entry. This is now handled 
;; automatically by the entry-list manager in the add-entry call. This call
;; will remove any existing entry with the given id, and this will call back 
;; to our cleanup routine "undo-entry" to do the work of undoing its 
;; effects, e.g. by calling symbols-delete-dependents to remove symbol
;; table entries dependent on the earlier entry contents.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-object -- checks the correctness of a student defined single body
;; argument(s):
;;  label:  the label of the body
;;  name(s): the name the body(s) was assigned in the problem description
;;  time:  for backward compatability
;;  id: is assigned to this object by the work-bench
;; returns: StudentEntry
;; note(s):
;;  marks the corresponding system entry as "entered". defines a mass variable
;;  whose name has "m" concatenated to the given label. Enters into the symbol
;;  table this name paired with the system's name for the same quantity.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-object (entry)

  ;; Case of no label at all, don't turn color, but give unsolicited hint.
  (if (no-text-handler-test entry 'body)
      (return-from assert-object (no-text-handler entry))
      (with-text-handler))

  (let ((id (StudentEntry-id entry))
	(symbol (StudentEntry-symbol entry)))
    (multiple-value-bind (prop tturn hints)
	(match-student-phrase entry 'body)

      (cond 
	(prop
	 (setf (StudentEntry-prop entry) prop)
	 ;; OK if there is no symbol defined.
	 (check-symbols-enter symbol (second (StudentEntry-prop entry)) id 
			      :namespace :objects)
	 ;; finally return entry 
	 (check-noneq-entry entry :unsolicited-hints hints))
	(t 
	 (setf (turn-result tturn) (append (turn-result tturn) hints))
	 tturn)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-vector -- check the correctness of a vector drawn by the student. May
;;  be any vector type except force
;; argument(s):
;;  label: the vector label
;;  avg-inst: is the vector an average or instantanepus quantity? value is
;;    either 'average or 'instantaneous
;;  type: the type of vector: velocity, acceleration, displacement, etc.
;;  system: the body that is moving. May be system label (student defined) or
;;    body name (given)
;;  dir: angle of the motion vector from horizontal (0->360 degrees) or a nega-
;;    tive number coding a z-axiz direction as follows (-1->out of plane; -2
;;    is into plane; -3 unknown but along z axis
;;  mag: magnitude of the vector or nil if unspecified
;;  time: the time period during which the vector is constant. 
;;    if nil and system is a student defined system, the time will be 
;;    taken from the system definition
;;  id: id assigned to vector by the workbench
;; returns: StudentEntry
;; note(s):
;;  if the vector is correct, the help system marks the corresponding system
;;  entry as "entered", defines the magnitude and direction variables, and
;;  enters the variables in the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup-vector (entry)
  
  ;; Case of no label at all, don't turn color, but give unsolicited hint.
  (if (no-text-handler-test entry 'vector)
      (return-from lookup-vector (no-text-handler entry))
      (with-text-handler))
  
  (let* ((id (StudentEntry-id entry))
	 (symbol (StudentEntry-symbol entry))
	 (drawn-mag (StudentEntry-radius entry))
	 ;;
	 ;; defined after match
	 action vector-term vector-mag-term vector-dir-term
	 ;;
	 ;; In Andes3, there is no user-interface command to specify
	 ;; the direction as "unknown."  If the student happens to get
	 ;; the correct direction (if the direction can eventually be 
	 ;; calculated), then fine.  
	 ;; Otherwise, we check the drawn direction of the vector against 
	 ;; other vector and axis directions, and their opposites.
	 ;; If there is a match, provide unsolicited hint that another 
	 ;; direction should be chosen.  Something like,
	 ;; "The direction of F1 is unknown.  Drawing it in the direction
	 ;;  180 degrees suggests that it is parallel to the velocity of 
	 ;;  the ball.  Please choose another direction."
	 (dir-term (arg-to-dir (StudentEntry-angle entry) 
			       :mag-arg drawn-mag
			       :cosphi (StudentEntry-cosphi entry)))
	 ;; 
	 ;; xy plane vectors get theta prefix, z axis ones get phi
	 ;; Greek symbols expressed in LaTeX form, for now.
	 ;; Don't append to empty symbol.
	 (dir-label (strcat-nonzero 
		     (if (z-dir-spec dir-term) "\\phi" "\\theta")
		     symbol)))
    
    (multiple-value-bind (prop tturn hints)
	(match-student-phrase entry 'vector)
      
      (cond
	(prop    
	 ;; Use the quantity from the best match with angle we actually got,
	 ;; unless the best-match quantity is unknown.
	 (setf action (list 'vector (second prop) 
			    (if (member (third prop) '(unknown z-unknown))
				(third prop)
				dir-term)))
	 
	 (setf vector-term (second action))
	 (setf vector-mag-term `(mag ,vector-term))
	 (setf vector-dir-term `(dir ,vector-term))
	 
	 (setf (StudentEntry-prop entry) action)
	 (check-symbols-enter symbol vector-term id :namespace :objects)
	 (check-symbols-enter symbol vector-mag-term id)
	 (check-symbols-enter dir-label vector-dir-term id)
	 
	 ;; if any axes are defined must add all component variables as well
	 (dolist (axis-sym (symbols-fetch '(axis ?xyz ?dir) :namespace :objects))
	   (let* ((axis-label (sym-label axis-sym))
		  (axis-term (sym-referent axis-sym))
		  (axis-entry-id (first (sym-entries axis-sym)))
		  ;; Don't append to empty symbol.
		  (compo-var (strcat-nonzero symbol "_" axis-label))
		  (compo-term (vector-compo vector-term axis-term)) ; Physics-Funcs
		  )
	     (check-symbols-enter compo-var compo-term (list id axis-entry-id))))
	 
	 ;; Different given values are handled in different ways:
	 ;; 1. Direction value or unknown or zero-mag values get checked 
	 ;; automatically as part of the vector entry proposition.  These continue 
	 ;; to use the implicit equation machinery so as to record their equations 
	 ;; as side effects, but not to check them. 
	 ;; 2. For non-zero-mag vectors, given mag or compos handled via the given
	 ;; equation mechanism, which checks their values.
	 ;; We currently rely on drawn-mag argument to detect zero-mag vector 
	 ;; This is OK because workbench updates drawn-mag if non-zero mag is 
	 ;; specified.
	 
	 ;; !!! Now if student specifies values by components, workbench 
	 ;; automatically sends dir as unknown.  This could be a problem if dir 
	 ;; represented as known.
	 
	 ;; if vector is zero-length, associate implicit equation magV = 0
	 ;; also add component eqns vc = 0 for all component variables in solution.
	 (when (equal dir-term 'zero)
	   (add-implicit-eqn entry (make-implicit-assignment-entry symbol 0))
	   (dolist (syscomp (get-soln-compo-vars vector-term))
	     ;; skip make-implicit-assignment-entry since we have sysvar, 
	     ;; not studvar
	     (add-implicit-eqn entry (make-implicit-eqn-entry `(= ,syscomp 0)))))
	 
	 
	 ;; if vector is a unit vector, associate implicit equation magV = 1 
	 (when (eq (first vector-term) 'unit-vector)
	   (add-implicit-eqn entry (make-implicit-assignment-entry symbol 1)))
	 ;; if direction is known, associate implicit equation dirV = dir deg.
	 (when (and (degree-specifierp (third prop))
		    (degree-specifierp dir-term))          ;known xy plane direction
	   (add-implicit-eqn entry (make-implicit-assignment-entry dir-label dir-term)))
	 (when (and (z-dir-spec (third prop))
		(z-dir-spec dir-term)) ;known z axis direction
	   (add-implicit-eqn entry (make-implicit-assignment-entry dir-label (zdir-phi dir-term))))
	 
	 ;; Associated eqns will be entered later if entry is found correct.

	 ;; finally return entry
	 (check-noneq-entry entry  :unsolicited-hints hints))
	(t 
	 (setf (turn-result tturn) (append (turn-result tturn) hints))
	 tturn)))))


; fetch list of system vars denoting components of vector term
(defun get-soln-compo-vars (vector-term)
  (let ((compo-pattern (vector-compo vector-term '(axis ?xyz ?rot))))
   (mapcar #'qvar-var
     (remove-if-not #'(lambda (qvar) 
                           (unify (qvar-exp qvar) compo-pattern))
                    (problem-varIndex *cp*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-line -- check the correctness of a line drawn by the student.
;; argument(s):
;;  label: the line label
;;  body:  the ``body'' associated with the line.
;;  dir: angle of the line from horizontal (0->360 degrees) or a nega-
;;    tive number coding a z-axiz direction as follows (-1->out of plane; -2
;;    is into plane; -3 unknown but along z axis
;;  mag: length of line or nil if unspecified
;;  time: the time period during which the vector is constant. if nil and
;;    system is a student defined system, the time will be taken from
;;     the system definition
;;  id: id assigned to vector by the workbench
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  if the line is correct, the help system marks the corresponding system
;;  entry as "entered", defines the magnitude and direction variables, and
;;  enters the variables in the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup-line (entry)

  ;; Case of no label at all, don't turn color, but give unsolicited hint.
  (if  (no-text-handler-test entry 'draw-line)
      (return-from lookup-line (no-text-handler entry))
      (with-text-handler))

  (let* ((id (StudentEntry-id entry))
	 (symbol (StudentEntry-symbol entry))
	 (drawn-mag (StudentEntry-radius entry))
	 ;; See comments about unknown directions in function lookup-vector.
	 (dir-term (arg-to-dir (StudentEntry-angle entry)
			       :mag-arg drawn-mag
			       :cosphi (StudentEntry-cosphi entry)
			       :modulus 180)) ;lines defined mod 180 deg
	 ;; this defines magnitude and direction variables
	 line-term
	 line-mag-term 
	 line-dir-term
	 action
	 ;; xy plane lines get theta prefix, z axis ones get phi
	 ;; These are the input forms
	 ;; Don't append to empty symbol
	 (dir-label (strcat-nonzero 
		     (if (z-dir-spec dir-term) "\\phi" "\\theta")
		     symbol)))

    (multiple-value-bind (prop tturn hints)
	(match-student-phrase entry 'draw-line)

      (cond 
	(prop
	 ;; Use the quantity from the best match with angle we actually got,
	 ;; unless the best-match quantity is unknown.
	 (setf action (list 'draw-line (second prop) 
			    (if (member (third prop) '(unknown z-unknown))
				(third prop)
				dir-term)))
	 (setf line-term (second action))
	 (setf line-mag-term `(mag ,line-term))
	 (setf line-dir-term `(dir ,line-term))

	 (setf (StudentEntry-prop entry) action)
	 (check-symbols-enter symbol line-term id :namespace :objects)
	 (check-symbols-enter symbol line-mag-term id)
	 (check-symbols-enter dir-label line-dir-term id)
	 
	 ;; if direction is known, associate implicit equation dirV = dir deg.
	 (when (and (degree-specifierp (third prop))
		    (degree-specifierp dir-term))         ;known xy plane direction
	   (add-implicit-eqn entry 
			     (make-implicit-assignment-entry 
			      dir-label dir-term)))
	 (when (and (z-dir-spec dir-term)
		    (z-dir-spec dir-term))   ;known z axis direction
	   (add-implicit-eqn entry 
			     (make-implicit-assignment-entry 
			      dir-label (zdir-phi dir-term))))
	 ;; Associated eqns will be entered later if entry is found correct.
	 
	 ;; Include implicit equation cos angle = dummy, where dummy is 
	 ;; nonnegative.  This is associated with Snell's law, but it
	 ;; is convenient to add this eqn when drawing the line.
	 ;; Treat this equation as entered by the student so the solve tool 
	 ;; can solve student's system the same way as at sgg time.
	 (dolist (eqn (Problem-EqnIndex *cp*))
	   (when (and (unify '(angle-constraint t orderless . ?quants) 
			     (eqn-exp eqn)) (member line-term (eqn-exp eqn) 
						    :test #'unify))
	     (add-implicit-eqn entry 
			       (make-implicit-eqn-entry (eqn-algebra eqn)))))

	 ;; finally return entry
	 (check-noneq-entry entry :unsolicited-hints hints))
	(t 
	 (setf (turn-result tturn) (append (turn-result tturn) hints))
	 tturn)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-variable - define a variable to stand for a certain quantity. this is
;;  called when the student uses the "variable" menu, but not when variables are
;;  defined by other tools such as the force drawing tool or the body tool.
;; returns: StudentEntry
;; note(s):
;;  if the variable definition is correct, marks the corresponding system entry
;;  as "entered" and enters the student's variable name into the symbol table
;;  paired with the corresponding system variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun define-variable (entry)
  (let ((id (StudentEntry-id entry))
	(text (StudentEntry-text entry))
	(symbol (StudentEntry-symbol entry))
	value) ;not in Andes3

    ;; This should be handled as regular help
    (unless text (warn "Definition must always have text")
	       (setf text ""))
    
    ;; match up text with SystemEntry
    ;;
    (multiple-value-bind (prop tturn hints)
	(match-student-phrase entry 'define-var)
      
      (cond 
	(prop
	 (setf (StudentEntry-prop entry) prop)
	 	 
	 ;; install new variable in symbol table
	 (check-symbols-enter symbol 
			      ;; strip off the 'define-var for the scalars namespace
			      (second (StudentEntry-prop entry)) 
			      id)
	 
	 ;; record associated given value equation entry
	 (when (and symbol value)  ;NIL => unspecified. (Empty string => unknown)
	   (add-given-eqn entry (make-given-eqn-entry symbol value 'value)))
	 ;; NB! make-given-eqn-entry can return NIL if no system var found for 
	 ;; studvar.
	 ;; Normally means var def will be incorrect. No given-eqn added in this case.
	 ;; But maybe better have a dangling given eqn entry anyway?
	 
	 ;; finally return entry 
	 (check-noneq-entry entry :unsolicited-hints hints))
	(t 
	 (setf (turn-result tturn) (append (turn-result tturn) hints))
	 tturn)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-x-axis - check correctness of coordinate axis drawn by student
;; argument(s):
;;  body: the label given to the body the axis applies to
;;  dir: the angle of the x-axis form horizontal (0 -> 360)
;;  id: is assigned to the object by the workbench
;;  x-label: label given to x axis by the student
;;  y-label: label given to y axis by the student
;; returns: StudentEntry
;; note(s):
;;  adds x and y axes to (student entries) -- asserts observed to assessor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-x-axis (entry)
  ;; workbench doesn't associate axes with bodies yet, so we leave this
  ;; out of the entry proposition we match (see KB/Ontology.cl).
  ;; !! need to make sure axis between zero and 90 and canonicalize if
  ;; not
  (let* ((dir (StudentEntry-angle entry))
	 (id (StudentEntry-id entry))
	 (x-label (StudentEntry-x-label entry))
	 (y-label (StudentEntry-y-label entry))
	 (z-label (StudentEntry-z-label entry))
	 (action `(draw-axes ,dir)) ; dir is naked degree value
	 (x-term `(axis x ,dir))
	 (y-term `(axis y ,dir))
	 (z-term `(axis z ,dir))
	 (xdir-dnum `(dnum ,dir |deg|)))

    (setf (StudentEntry-prop entry) action)
    ;; install symbols for x, y, and z axes
    ;; these can't be used by themselves in equations but are needed by us
    ;; later when autodefining vector component variables for existing axes. 
    ;; They would also be needed for referring to the axes by label in help 
    ;; messages if there is more than one set of axes.
    (add-entry entry)
    (when x-label (check-symbols-enter x-label x-term id
				       :namespace :objects))
    (when y-label (check-symbols-enter y-label y-term id
				       :namespace :objects))
    (when z-label (check-symbols-enter z-label z-term id
				       :namespace :objects))

    ;; automatically define \thetax as label for direction of positive x-axis
    (when x-label (check-symbols-enter 
		   (strcat-nonzero "\\theta" x-label)
		   xdir-dnum 
		   id :sysvar xdir-dnum))
    
    ;; if any vectors have been drawn, add all component variables along 
    ;; these new axes as well
    (dolist (vent (remove '(vector . ?rest) *StudentEntries* :test-not #'unify
			  :key #'StudentEntry-prop))
      (let ((vector (second (StudentEntry-prop vent))))
	(dolist (axis-label (remove nil (list x-label y-label z-label)))
	  (let ((compo-var (strcat-nonzero  
			    (symbols-label vector :namespace :objects) 
			    "_" axis-label))
		;; in Physics-Funcs
		(compo-term (vector-compo vector
					  (symbols-referent 
					   axis-label
					   :namespace :objects))))
	    (check-symbols-enter compo-var compo-term 
				 ;; id of axes and vector
				 (list id (StudentEntry-id vent)))))))
    
    ;; finally return entry
    (check-noneq-entry entry)))

;-----------------------------------------------------------------------------
;; for processing deletions of entries
;-----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo-entry -- undo all state effects of a particular Student entry
;; Argument: enty -- the StudentEntry to be undone.
;;
;; Note: This worker routine is automatically invoked by the delete-object 
;; function to cleanup state on removal of an entry from the entry list -- 
;; a bit like a C++ destructor for student entries.  That ensures that
;; entry effects are always undone when a student entry is removed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun undo-entry (entry)
  ;; undo entry effects specific to correct entries:
  (when (equal (StudentEntry-state entry) +correct+)
	;; unmark entry interpretations as done in solution graph
  	(sg-delete-StudentEntry entry)
        ;; undo any implicit eqn entry associated with this
  	(dolist (ie (StudentEntry-ImplicitEqns entry))
           (undo-entry ie))
        ;; undo any given eqn entry associated with this
	(dolist (ge (StudentEntry-GivenEqns entry))
	  (when (not (blank-given-value-entry ge))
	    (undo-entry ge))))

  ;; remove all labels dependent on it from symbol table
  (mapcar #'grammar-remove-variable
	  (symbols-delete-dependents (StudentEntry-ID entry)))

  ;; special to equation entries: remove from algebra system
  (when (or (member (first (StudentEntry-prop entry)) 
		    '(eqn solve-for-var implicit-eqn))
	    (eql (StudentEntry-id entry) 'check-answer-equation))
	(undo-eqn-entry entry)))


;;----------------------- Checking Entries ----------------------------------
;; 
;; Following functions implement the non-eqn portion of the
;; EntryInterpreter. EntryInterpreter for equations is in another file

;;
;; check-symbols-enter -- make an entry in the symbol table of student
;;                        variables, checking for redefinition.
;;
;; This takes the same arguments as symbols-enter in symbols.cl, but
;; wraps error checking around it. In case of symbol definition error, 
;; the entry is tagged incorrect with the appropriate error interpretation.
;; NB: if entries is not a singleton, the error is hung on the first
;; one, which should be the principle entry being evaluated
(defun check-symbols-enter (label referent entry-ids &key sysvar namespace)
  ;; turn entry-ids into a list
  (when (atom entry-ids) (setf entry-ids (list entry-ids)))
  (cond
    ((= (length label) 0)
     ;; There is a number of situations where it is legitimate to 
     ;; not define a symbol:  any body definitions, or drawing vectors
     ;; in a free body diagram problem (no equations).
     ;;
     ;; Look for associated sysvar as indication that symbol is expected.
     ;; Default namespace is :scalars.  This is the namespace associated
     ;; with sysvars.
     (when (and (or (null namespace) (eql namespace :scalars))
		(quant-to-valid-sysvar referent))
       (let ((entry (find-entry (first entry-ids))))
	 (make-tutor-response
	  entry
	  (list
	   (format nil "You need to ~A for ~A."
		   *define-variable*
		   ;; for compo angle and mag,
		   ;; define parent object.
		   (nlg (get-vector-parent-prop
			 referent)))
	   (strcat "Double-click on the text and enter:<br>"
		   (text-box 
		    (strcat "<em>var</em> is "
			    (nlg (get-vector-parent-prop referent))))
		   "<br>where <em>var</em> is the variable "
		   "name you have chosen."))
	  :state +incorrect+
	  :diagnosis (list 'no-variable-defined 
			   (get-vector-parent-prop referent))
	  :spontaneous t))))
   
    ((symbols-lookup label :namespace namespace) ;variable already defined!
     ;; find entry. entry-ids arg may be atom or list, but should not be nil
     (let ((entry (find-entry (first entry-ids))))
       ;; build the error remediation turn
       (make-tutor-response
	entry
	(list
	 (format nil "The variable ~A is in use to define ~A.&nbsp; Please choose a different label." 
		 label (nlg (symbols-referent 
			     label
				      :namespace namespace))))
	:assoc `((variable-in-use 
		  . ,(symbols-referent label :namespace namespace)))
	:state +incorrect+
	:diagnosis '(variable-already-in-use)
	:spontaneous t)))

    ;; Test for valid variable names.  This should match
    ;; 'unknown in Help/physics-algebra-rules.cl
    ((or (digit-char-p (char label 0))
	 (char= (char label 0) #\_))
     (let ((entry (find-entry (first entry-ids))))
       (make-tutor-response
	entry
	(list
	 (format nil
		 "Variable names may not start with a ~:[underscore~;number~].&nbsp; Please choose a different name." (digit-char-p (char label 0))))
	 :assoc (list (list 'invalid-variable-name label))
	 :state +incorrect+
	 :diagnosis (list 'invalid-variable-name label)
	 :spontaneous t)))
    
    ;; else no conflict: just make the definition
    (T 
     ;; Sanity test to verify that referent is in Ontology.
     ;; new-english-find itself emits a warning if there is no match.
     (new-english-find referent)
     (symbols-enter label referent :entries entry-ids :sysvar sysvar 
		    :namespace namespace))))

(defun get-vector-parent-prop (x)
  "For vector (or line) compo, dir, or mag, return parent prop."
  (cond ((eql (car x) 'mag) (second x))
	((eql (car x) 'dir) (second x))
	((eql (car x) 'compo) (fourth x))
	(t x)))
;;
;; Check-NonEq-Entry -- Generic checker for non-equation student entry 
;; Adds grading information.
;; Returns: tutor turn
;;
;; Note, could be done differently with a cond or two.
(defun Check-NonEq-Entry (entry &key unsolicited-hints)
  
  ;; special case: Entry-API handler can attach an error interp for certain
  ;; errors such as symbol redefinitions that prevent the entry from
  ;; being processed further. Return appropriate turn with unsolicited 
  ;; error message in this case.
  (when (studentEntry-ErrInterp entry) 
    (when *debug-grade* (warn `log-condition:log-warn
			      :text "Not grading update 1" 
			      :tag (list 'not-grading 1 entry)))
    (return-from Check-NonEq-Entry 
      (ErrorInterp-remediate (studentEntry-ErrInterp entry))))
  
  ;; else have a real student entry to check
  (when *debug-help* 
    (format t "Checking entry: ~S~%" (StudentEntry-prop entry)))
  (let (cand		; candidate (state . interpretation) pair
        match 		; correct system entry matched
        result) 	; final result to return

    
    ;; Special for vector entries: If any given eqns are set, ensure the 
    ;; form of the givens is correct before we do any other checking. 
    ;; This avoids bad error handlers when source is now really wrong form.
    ;; Testing for the presence of GivenEqns is just to make this backwards 
    ;; compatible with old logs which never set any.
    (when (and (eq (first (StudentEntry-prop Entry)) 'vector)
               (StudentEntry-GivenEqns Entry))
      (setf result (Check-Vector-Given-Form Entry))
      (when (not (eq (turn-coloring result) +color-green+))
	(when *debug-grade* (warn `log-condition:log-warn
				  :text "Not grading update 2" 
				  :tag (list 'not-grading 2 entry)))
	(return-from Check-NonEq-Entry result))) ; early exit
    
    ;; Get set of candidate interpretations into PossibleCInterps.  
    ;; For generality needed for equation entries, an "interpretation" 
    ;; is a list of primitive system entries -- steps -- effected by this 
    ;; student entry.  Each *candidate* interpretation is a pair whose car 
    ;; is the status [CORRECT, DEAD-PATH, PREMATURE, FORBIDDEN etc.] and 
    ;; whose cdr is an interpretation = sysent list, thus candidate list is 
    ;; of form ((PREMATURE sysent1 sysent2) (DEAD-PATH sysent2 sysent3) ...)
    ;; For non-equations, we should always get singleton set of candidates
    ;; or NIL, so we always just use the first one or mark incorrect if none.
    ;; Interp itself should contain exactly one sysent.
    (sg-match-StudentEntry Entry)
    (unless  (setf cand (first (StudentEntry-PossibleCInterps Entry))) 
      (when *debug-help* 
	(format t "No matching system entry found~%"))
      (setf (StudentEntry-state entry) +incorrect+)

      ;; Identical to code in handle-ambiguous-equation in Help/parse-andes.cl
      ;; run whatswrong help to set error interp now, so diagnosis
      ;; can be included in log even if student never asks whatswrong
      (let ((intended (ErrorInterp-intended (diagnose Entry)))
	    (info (make-info-provided :prop (studententry-prop entry)
				      :slots 1 ;should be list, but for now, just
				      :penalize t)))
	
	;; Mark associated SystemEntry or SystemEntries, as incorrect.
	(update-grade-status intended +incorrect+)
	
	;; Take diagnosis and add grading information.
	;; It looks like the "new" part does not work for multiple-choice.
	;; try vec1e.
	(dolist (sysent intended)
	  (pushnew info
		   (graded-incorrects (SystemEntry-graded sysent))
		   :key #'info-provided-prop
		   :test #'unify)))
      
      (setf result (try-make-incorrect-reply entry 'Check-NonEq-Entry-no-rem))
      (setf (turn-result result) 
	    (append unsolicited-hints (turn-result result)))
      (return-from Check-NonEq-Entry result)) ; go no further
    
    ;; else got a match: set state and correctness from candidate
    (setf (StudentEntry-State entry) (car cand))
    (setf (StudentEntry-CInterp entry) (cdr cand))
    (setf match (first (StudentEntry-CInterp entry)))
    (when *debug-help* 
      (format t "Matched ~A system entry: ~S~%" (car cand) match))
    
    ;; decide what to return based on major state of entry
    (case (StudentEntry-State entry)
      (correct 

       ;; Grading:  attach grade to SystemEntry
       (update-grade-status (StudentEntry-CInterp entry) +correct+)
    
       ;; check any given value equations associated. At first one that is wrong, its
       ;; result turn becomes the result for the main entry; checking routine 
       ;; updates main entry record with error interp of the bad equation.
       (dolist (e (StudentEntry-GivenEqns entry))
	 (let ((result (Check-Given-Value-Entry entry e)))
	   (when (not (eq (turn-coloring result) +color-green+))
	     (return-from Check-NonEq-Entry result))))
       
       ;; enter step as done in solution graph
       (sg-enter-StudentEntry Entry)
       ;; if entry has associated implicit equations, enter them as done well
       (dolist (e (StudentEntry-ImplicitEqns entry))
	 (enter-implicit-entry e))
       ;; if entry has associated given value equations, enter them as well
       ;; Note we need parsed equation in systemese
       (dolist (e (StudentEntry-GivenEqns entry))
         (enter-given-eqn e))
       ;; Everything is OK!
       (setf result (make-green-turn :id (StudentEntry-id entry))))
      
      ;; give special messages for some varieties of incorrectness:
      (Forbidden (setf result (make-tutor-response 
			       entry +forbidden-help+)))
      (Premature-Entry (setf result (make-tutor-response 
				     entry +premature-entry-help+)))
      (Dead-Path (setf result (make-tutor-response 
			       entry +dead-path-help+)))
      (Nogood (setf result (make-tutor-response entry +nogood-help+)))
      (otherwise (warn "Unrecognized interp state! ~A~%" 
		       (StudentEntry-state entry))
		 (setf result (try-make-incorrect-reply entry 'check-non-eq-entry-1))))


    ;; Note for variables with given equations, we are
    ;; logging correctness of the variable definition substep, 
    ;; but the given value substep, hence whole entry, 
    ;; might still be wrong.

    (setf (turn-result result) 
	  (append unsolicited-hints (turn-result result)))
    
    (add-log-entry-info entry result)
    
    ;; finally return result
    result))

(defun student-log-line (line)
  (and (assoc :action line)
       (equal (cdr (assoc :action line)) "log")
       (assoc :log line)
       (equal (cdr (assoc :log line)) "student")))


(defun sol-gives-magdir (vector-quant)
"non-nil if magnitude or direction or vector are given in current problem"
  ; fetch vector entry prop to see if drawn with known direction
  (let ((vector-entry (sg-find-vector-entry vector-quant)))
    ; true if either drawn non-unknown... 
    (or (and vector-entry    ; prop form is (vector ?quant ?dir)
             (not (eq (third (SystemEntry-prop vector-entry)) 
	               'unknown)))
	; ... or mag has a given value
        (given-quant-p `(mag ,vector-quant)))))

(defun sol-gives-component (vector-quant)
"non-nil if some component of vector is given in current problem"
  ; !!! 0 rotation assumes given compos always along standard axes. 
  ; True for now but could change in future
  (some #'given-quant-p
	 `((compo x 0 ,vector-quant)
	   (compo y 0 ,vector-quant)
	   (compo z 0 ,vector-quant))))

 ; Code below uses 'compo and 'magdir as form codes
(defun sol-has-givens (vector-quant form)
 "true if solution has given values of specified form"
 (if (eq form 'compo) (sol-gives-component vector-quant)
   (sol-gives-magdir vector-quant)))

(defun other-form (form)
   (if (eq form 'compo) 'magdir 'compo))

(defun compo-assignment-p (entry)
  "true if given student eqn entry states value of a component variable" 
    (and (eq (first (StudentEntry-prop entry)) 'eqn)
         (componentp (symbols-referent (second (StudentEntry-prop entry))))))
	
;; Give special message if student chooses wrong form (mag/dir vs.
;; components) for given values of vector attributes.
(defun Check-Vector-Given-Form (entry)
"check given values sent with a vector entry"
  (let ((stud-form   (if (some #'compo-assignment-p
                                (StudentEntry-GivenEqns entry)) 'compo
	               'magdir))
	(vector-quant (second (StudentEntry-prop entry))))
    ;; Check for mismatch in form of givens.
    ;; NB: if system gives nothing, either form should be OK for unknown.
    ;; A student's form is incorrect iff no attribute of the student form 
    ;; is given AND some attribute of the other form is given. 
    (when (and (not (sol-has-givens vector-quant stud-form))
               (sol-has-givens vector-quant (other-form stud-form)))
          (format T "mismatch: student form: ~A system form: ~A~%" 
                     stud-form (other-form stud-form))
          ;; flag main entry as wrong and fill in error interp
          (setf (StudentEntry-state entry) +incorrect+)
	  (return-from Check-Vector-Given-Form
	   (if (eq stud-form 'compo) (should-be-magdir-form entry vector-quant)
	     (should-be-compo-form entry vector-quant)))))

   ; get here=> no errors. Signal with green turn
   (make-green-turn :id (StudentEntry-id entry)))

(defun should-be-compo-form (se quant)
  (declare (ignore quant))
  (make-tutor-response
   se
   (list "Use component form for this vector.")
   :diagnosis '(should-be-compo-form)
   :state +incorrect+
   :spontaneous t))

(defun should-be-magdir-form (se quant)
  (declare (ignore quant))
  (make-tutor-response
   se
   (list "Use mag/dir form for this vector.")
   :diagnosis '(should-be-magdir-form)
   :state +incorrect+
   :spontaneous t))
;; 
;; add-log-entry-info -- insert log info for entry into Andes log
;;
;; This function can be used for any student entry including eqns, 
;; after status and possibly error info has been assigned.
;; Logs: parsedEqn (eqns only), error label (if assigned), 
;;       target step and op lists (if one is found)
;; Runs wwh to get an error handler if one is unset.
;; Sends async commands to workbench to do the logging, so
;; the entries go before the final result in the log.

(defun add-log-entry-info (entry result-turn)
  ;; Add log entry to result turn.
  (let ((log-line (log-entry-info Entry)))
    (when log-line
      (when (member-if #'student-log-line (turn-result result-turn))
	(warn "Already have log in result for ~A"
	      (StudentEntry-prop entry)))
      (push log-line (turn-result result-turn)))))

(defun log-entry-info (entry)
  "Wrapper to inhibit logging when rerunning old sessions."
  (when (and entry (not **checking-entries**))
    (do-log-entry-info entry)))

(defun do-log-entry-info (entry)
  (let (target-entries
	(parse (StudentEntry-ParsedEqn entry)))
    ;; fetch target entry list for correct or incorrect entries 
    (cond ((eq (StudentEntry-state entry) +incorrect+)
	   (unless (StudentEntry-ErrInterp entry) 
	     (when *debug-grade*
	       ;; StudentEntry-ErrInterp should have been set before
	       ;; calling this function, perhaps by a call to (diagnose entry).
	       ;; Otherwise, this means that the grading has not been done.
	       (warn 'log-condition:log-warn
		     :tag (list 'undiagnosed-entry (StudentEntry-prop entry))
		     :text "Undiagnosed error for studententry."))
	     (diagnose entry))
	   (setf target-entries (ErrorInterp-Intended 
				 (StudentEntry-ErrInterp Entry))))
	  
	  ((eq (StudentEntry-state entry) +correct+)
	   (setf target-entries (studententry-Cinterp entry))))
    
    ;; OK, do the logging
    (let ((result '((:log . "student") (:action . "log")))
	  (*print-pretty* NIL)) ;no line breaks when formatting cmd strings!
      
      ;; log the parse if we have it.  Note for syntax errors it may be list 
      ;; containing partial parse tree printed as 
      ;; (#S(PARSE :TREE (LM m) :REM =x8*/7))
      ;; print parse of ? for this
      (when (consp parse)  ; non-NIL => either prefix eqn or list of parse trees
	(push `(:parse . ,(prin1-to-string 
			   (if (eq (type-of (first parse)) 
				   'parse) '? parse)))
	      result))
      
      ;; For non-eq entries, show entry prop in our notation, so we can 
      ;; identify common errors.   For correct non-eq entries, it will be 
      ;; the step, but for errors we add it, if it is known.
      (when (and (studentEntry-prop entry)
		 (not (eq (first (studentEntry-prop entry)) 'eqn))
		 (eq (StudentEntry-state entry) +incorrect+))
	(push `(:entry . ,(prin1-to-string (studentEntry-prop entry)))
	      result))
      
      ;; log the error tag if one was found
      (when (StudentEntry-ErrInterp entry)
	(push `(:error-type . ,(prin1-to-string
				(ErrorInterp-diagnosis 
				 (StudentEntry-ErrInterp Entry))))
	      result))
      
      ;; log the target entry info if we have any. This shows 
      ;; pairs of opnames and entry props "steps."
      (when target-entries
	(push `(:Assoc . ,(alist-warn (mapcar #'opname-prop-pair 
					      target-entries)))
	      result))
    (reverse result))))

(defun opname-prop-pair (x)
  (cons (sg-map-SystemEntry->opname x) 
	(prin1-to-string (SystemEntry-prop x))))


; following does the work of entering an implicit entry associated with
; a principal entry, either an implicit equation or an implicit variable
; entry, assuming the principal entry is correct. Arg is implicit entry.
(defun enter-implicit-entry (entry)
   (cond ((eq (first (studentEntry-prop entry)) 'implicit-eqn) 
	  ;; enter an implicit equation defined by correct non-eqn entry
          (enter-subentry-eqn entry (second (StudentEntry-prop entry))))
	 (T ; other type, i.e. auto mass variable
	    (when (sg-match-studententry entry)  ; correct
	      (setf (studententry-cinterp entry) 
	          (cdr (first (studententry-PossibleCinterps entry))))
	      (setf (studentEntry-state entry) +correct+)
	      (sg-enter-StudentEntry entry)))))

(defun enter-given-eqn (eqn-entry)
"enter given value equation defined by correct non-eqn entry"
 ; don't enter if entry says value is unknown. 
 (when (not (blank-given-value-entry eqn-entry))
   (enter-subentry-eqn eqn-entry (studentEntry-ParsedEqn eqn-entry))))

; enter some dependent equation
(defun enter-subentry-eqn (eqn-entry eqn)
  (let* (; Set entry id to free high equation slot, aborting entry
	 ; if ran out of slots (should ensure enough so never happens)
         (slot (if *solver-free-slots*
		   (setf (StudentEntry-ID eqn-entry) 
			 ;; Need a name that won't collide with Andes3 or Andes2 
			 ;; StudentEntry-ID's and be unique.
			 (format nil "subentry-eqn-~a" (car *solver-free-slots*)))
	           (return-from enter-subentry-eqn)))
	 ;; verify it with algebra and enter it in slot
         (result (solver-StudentAddOkay slot eqn)))

     ; if equation is not judged algebraically correct something is seriously 
     ; wrong, since original entry has been judged correct.
     (unless (and (numberp result) 
                  (or (= result 0) (= result 7)))
         (warn "Implicit eqn ~A judged bad by algebra!! (result=~A)~%" 
	        eqn result)) 
     (setf (StudentEntry-State eqn-entry) +correct+)
 
     ; To choose interpretation for solution graph marking, delegate to 
     ; interpret equation. This routine knows how to process the tagged
     ; sets of interpretations returned by sg and choose the best one, 
     ; leaving it into entry's Cinterps and ignoring its tutor turn result.
     (interpret-equation eqn-entry)
     ; The above routine might select an interpretation in which the entry 
     ; is premature or has some other flaw, but we don't care for implicit 
     ; equations: just go on to mark it done in the solution graph in any case
     (sg-enter-StudentEntry eqn-entry)
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MC Answers


;; When the students are working on non-quant problems and the student has 
;; completed a goal then they will will check an "I am done" box.  
;; That command will be sent to the help system as a button click.
;; The interpretation of the button click is stored in 
;; (studententry-prop entry).
;;
;; If they have pressed the done button then we will deterimine if
;; the associated psm has been completed.  If so, then it will return 
;; a green turn else it will return a red turn.
;; If possible this will associate an automatic error interp that says 
;; "you have not completed all of the steps necessary." 

;; Given an mc-no-quant done answer lookup the corresponding sought.  
;; Having done that look up the corresponding PSM and determine if the PSM 
;; has been completed if so then the value is green if not then don't. 
(defun check-mc-no-quant-done-answer-sought (entry)
  (let* ((id (second (StudentEntry-prop entry))) ;prop of task to be done
	 ;; Corresponding systementry for done button.
	 (sysent (find-SystemEntry (StudentEntry-prop entry)))
	 ;; Find enode that is associated with the done button.
	 (PSM (find-if
		  #'(lambda (x) (member sysent x))
		  (bubblegraph-enodes (problem-graph *cp*))
		  :key #'enode-entries)))
    (cond 
      ((null sysent)
       (error 'log-condition:log-error 
	      :tag (list 'done-button-without-systementry id)
	      :text "No SystemEntry for button"))
      
      ((null PSM) 
       (error 'log-condition:log-error 
	       :tag (list 'done-button-without-enode id)
	       :text "No problem step found for button"))
      
      ;; If this is not a non-quant psm then we also need to throw an error 
      ;; asserting that fact. 
      ((not (enode-has-mark? PSM 'non-quant))
       (error 'log-condition:log-error 
	       :tag (list 'bad-mark-for-button-enode psm id)
	       :text "Unmarked enode matching non-quant IDNum"))
      
      ;; Otherwise, test to see if task is completed by finding prerequisite
      ;; do-steps and seeing if all associated SystemEntries are done.
      ((every #'(lambda (x) (or (null (csdo-entries x)) 
				(csdo-enteredp x)))
	      (find-prop-in-path id (enode-path PSM)))
       (setf (StudentEntry-state entry) +CORRECT+)
       (pushnew entry (SystemEntry-entered sysent))
      ;; This parallels code in Check-NonEq-Entry; should merge
       ;; Update grading
       (update-grade-status (list sysent) +correct+)
       (make-green-turn :id (StudentEntry-id entry)))
      ;; Activity is incomplete, give a hint
      ;; based on goalprop, and then defer to NSH.
      (T (setf (StudentEntry-state entry) +INCORRECT+)
	 (setf (SystemEntry-entered sysent) nil)
	 ;; This parallels code in Check-NonEq-Entry; should merge
	 ;; Update grading
	 (update-grade-status (list sysent) +incorrect+)
	 ;; Take diagnosis and add grading information.
	 (pushnew (make-info-provided :prop (studententry-prop entry)
				      :slots 1 ;should be list, but for now, just
				      :penalize t)
		  (graded-incorrects (SystemEntry-graded sysent))
		  :key #'info-provided-prop
		  :test #'unify)
	 (let ((rem (walk-psm-path "You have not finished " 
				   (bgnode-path psm) nil)))
	   (setf (StudentEntry-ErrInterp entry)
		 (make-ErrorInterp 
 		  :diagnosis (cons 'goal-incomplete ID)
		  :remediation rem))
	   (make-incorrect-reply entry rem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Create turn struct for incorrect student entry.
;;  This adds logging of error to the reply.
;;
;; see function make-eqn-failure-turn
(defun make-red-turn (entry)
  (let ((id (StudentEntry-id entry))
	(log (log-entry-info entry)))
    (unless id (warn "no id in make-red-turn for ~A"
		     (StudentEntry-prop entry)))
    (unless (StudentEntry-errInterp entry)
      (warn "make-red-turn needs errInterp for ~A" 
	    (StudentEntry-prop entry)))
    (unless (eql (StudentEntry-state entry) +incorrect+)
      (warn "make-red-turn entry ~A state is ~A; should be incorrect" 
	    (StudentEntry-prop entry)
	    (StudentEntry-state entry)))
    (make-turn :coloring +color-red+
	       :id id
	       :result (when log (list log)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-tutor-response (entry hints &key state spontaneous 
			    diagnosis intended assoc)
  "Construct response turn to student entry with coloring, logging, and possible hint sequence.  Incorrect turns must have a hint sequence."
  (cond 
    ((eql state +incorrect+)
     (let ((rem (make-hint-seq hints :assoc assoc)))
       ;; But only do if not done already, 
       ;; so only report on the first error found.
       (unless (studentEntry-ErrInterp entry)
	 (setf (studentEntry-state entry) +INCORRECT+)
	 ;; Hint when help button is pressed, just give same
	 ;; hint again.
	 (setf (studentEntry-ErrInterp entry)
	       (make-ErrorInterp :diagnosis diagnosis
				 :intended intended
				 :remediation rem)))
       (make-incorrect-reply entry rem :spontaneous spontaneous)))

    ((and (null state) spontaneous)
     (let ((rem (make-hint-seq hints :assoc assoc)))
       (setf (studentEntry-ErrInterp entry)
	     (make-ErrorInterp :diagnosis diagnosis
			       :remediation rem))
       (when (functionp rem) (setf rem (funcall rem)))
       (setf (turn-id rem) (StudentEntry-id entry))
       rem))
    
     (t (warn 'log-condition:log-warn
	      :tag (list 'make-tutor-response-bad-arg state spontaneous)
	      :text "unsupported state"))))

(defun try-make-incorrect-reply (entry error-tag)
  "Take Entry and attempt to make it optionally spontaneous."
  ;; This is a hopefully temporary replacement to
  ;; Calls to make-red-turn.
  (if (and (StudentEntry-ErrInterp entry)
	   (ErrorInterp-remediation
	    (StudentEntry-ErrInterp entry)))
      (make-incorrect-reply entry (ErrorInterp-remediation
				   (StudentEntry-ErrInterp entry)))
      (progn
	(warn 'log-condition:log-warn
	      :tag (list 'reply-missing-rem error-tag 
			 (when (StudentEntry-ErrInterp entry) t)
			 (studentEntry-prop entry))
	      :text "No rem for entry")
	(make-red-turn entry))))

(defun hint-log-line (x)
  `((:action . "log") (:log . "tutor") (:assoc . ((spontaneous-hint . ,x)))))

(defun make-incorrect-reply (entry rem &key spontaneous)
  (let ((hint-policy
	 (get-state-property 'spontaneous-hint :model "server")))
    (if (or spontaneous
	    (eql hint-policy 1)
	    ;; Turn on experiment effect; see Bug #1940.
	    (random-help-experiment:help-mod-p 'give-spontaneous-hint))
	(progn 
	  (when (functionp rem) (setf rem (funcall rem)))
	  (setf (turn-id rem) (StudentEntry-id entry))
	  ;; Only turn red if rem is this reply.
	  (setf (turn-coloring rem) +color-red+)
	  ;; Add log message to return
	  (add-log-entry-info entry rem)
	  ;; In the case where there was change to spontaneous hint
	  ;; a hint to spontaneous, log that fact.
	  (unless spontaneous
	    (push (hint-log-line hint-policy) (turn-result rem)))
	  ;; Unsolicited hint.
	  rem)
	;; Turn without hint.
	(let ((result-turn (make-red-turn Entry)))
	  ;; Log that we could have given sponaneous hint and didn't
	  (when hint-policy
	    (push (hint-log-line hint-policy) (turn-result result-turn)))
	  result-turn))))
