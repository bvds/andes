;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unification.cl
;; This file contains the pattern matching code and unification code
;; for the Andes2 project and provides the "Unification" package when
;; loaded.  At the moment all of this code is taken directly from 
;; Peter Norvig's book; Paradigms of AI programming.  
;; This code was downloaded from:
;; ftp.mkp.com/pub/Norvig on 08/02/2000 
;; and is based upon the files patmatch.lisp, Prolog1.lisp and 
;; unify.lisp In the furture this code may be replaced with a 
;; custom system for the time being it is all Norvig's code.
;;
;; It has since been modified by Collin Lynch to include the
;; variable-boundp function.


;;(load "c:/SolutionGraph/Utility.cl")

;;(package "Unification")

;;================================================================
;; Constants and parameters

(defconstant fail nil "Indicates pat-match failure")

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
	no-bindings '((t . t))
	#-sbcl "Indicates pat-match success, with no variables."
	#+sbcl #'equalp)

(defvar *occurs-check* t "Should we do the occurs check?")


;;==================================================================
;; Pattern matching functions.


(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input bindings))  
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) 
                               bindings)))
        (t fail)))

(defun variable-boundp (v Bindings)
  "Has the variable x been bound already in the bindings."
  (and (variable-p v)
       (member v Bindings :test #'equalp :key #'car)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))


(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable-p exp))


(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) 
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
          (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))  

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) 
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator 
       (input rules &key (matcher #'pat-match) 
        (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some 
    #'(lambda (rule)
        (let ((result (funcall matcher (funcall rule-if rule) 
                               input)))
          (if (not (eq result fail))
              (funcall action result (funcall rule-then rule)))))
    rules))


;;;;
;;;; ============= test if expression contains variables ====================
;;;;

(defun groundp (form)
  "True if the given form contains no variables"
  (cond ((variable-p form) NIL)
	((atom form) t)
	(t (and (groundp (car form))
		(groundp (cdr form))))))

(defun ground-expression (form bindings)
  "If all the variables in the given form are bound, 
   returns a ground instance of it. Otherwise, returns NIL"
  (and (all-boundp form bindings)
       (subst-bindings bindings form)))

(defun all-boundp (form bindings)
  "True if the given form's variables all have bindings"
  (cond ((variable-p form)
	 (get-binding form bindings))
	((atom form) t)
	(t (and (all-boundp (car form) bindings)
		(all-boundp (cdr form) bindings)))))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))


;;;; ================================================================== 
;;;;                              Unification
;;;; ================================================================== 

(defun unify (x y &optional (bindings no-bindings) test test-function)
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
	((and test (funcall test x) (funcall test y)) 
	 (funcall test-function x y bindings))
	((and (orderless-p x) (orderless-p y)) 
	 (unify (orderless-sort (cdr x) bindings) 
		(orderless-sort (cdr y) bindings) bindings))
;;; Match any keyword pairs
	((valid-keyword-pair x) (unify-keyword x y bindings))
	((valid-keyword-pair y) (unify-keyword y x bindings))
        ((and (consp x) (consp y))
	 (unify (rest x) (rest y) 
			 (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

(defun exactly-equal (x y)
  "test equality under unification without any bindings"
  (equal (unify x y) no-bindings))

;;;
;;;                    Match to keywords pairs
;;;
;;;  Keywords pairs may occur in any order in a list.  One may
;;;  optionally specify a default value
;;;  The format is 
;;;       :keyword value [default]
;;;  If there is no match, value is bound to default or nil
;;;

(defun valid-keyword-pair (x)
  "Check x is list starting with a keyword and value."
  (and (consp x) (keywordp (car x)) (consp (cdr x))))

(defmacro get-any-default-value (x)
  "Remove any non-keyword from beginning of x"
  `(when (and (consp ,x) (not (keywordp (car ,x)))) (pop ,x)))

(defun unify-keyword (x y bindings)
  "Find match in y for first keyword pair in x"
  ;; keyword pair is removed from x:
  (let* ((ykey (member (pop x) y)) (var (pop x))
	 ;; remove any default value from x
	 (default (get-any-default-value x)))
    (cond
     ;; if value in x is nil, just remove keyword pair
     ((null var) (unify x y bindings))
     ;; if keyword pair is in y, match values:
     ((consp (cdr ykey))	;Is there a keyword and value in y?
      (let ((post (cddr ykey)))
	(get-any-default-value post)	;discard any default value from y
	(unify x (append (ldiff y ykey) post) ;keyword pair removed
	       ;; if y value is nil, bind to default instead
	       (unify var (or (second ykey) default) bindings))))
     ;; no match:  bind var to default
     (t (unify x y (unify var default bindings))))))

;;; Orderless lists with (orderless ...)

;; (orderless ...) must be a proper list with no unbound variables
;; valid expressions include:  (orderless a b c ...)
;;                             (orderless . ?a)
;;                             (orderless ?a)      

(defun orderless-p (x)
  (and (consp x) (eq (car x) 'orderless)))

(defun orderless-sort (x bindings)
  (cond ((variable-p x) x)                 ;(orderless . ?a)
	((and (listp x) (null (cdr x))) x) ;(orderless ?a)
	;; (orderless a b c ...)
	((and (listp x) (null (cdr (last x))) ;check for proper list
	      ;; In principle, we only need bindings sufficient for the sort
	      ;; to be unambiguous. 
	      (all-boundp x bindings))        ;with *all* variables bound
	 (sort (copy-list (subst-bindings bindings x)) #'expr<))
	(t (error "Invalid orderless ~A.~%  Need a proper list with all variables bound." 
		  (cons 'orderless x)))))


;;; ===========================================================================

(defun unify-with-list (i L &optional (Bindings No-Bindings))
  "Attempt to unify item i with some element in list L returning all 
   possible unifications."
  (loop for i2 in L
      when (unify i i2 Bindings)
      collect it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filter-values
;; Search through a list of expressions and pull out the subset of each 
;; expression that are accepted by the filter.
;;
;; Arguments: Filter: The filter expression to be used.
;;            Exps:   The expressions to be filtered.
;;     &optional
;;             Bindings:  An optional set of bindings to use.

(defun filter-expressions (Filter Exps &optional (Bindings no-bindings))
  (let ((R))
    (dolist (E Exps)
      (if (unify Filter E Bindings)
	  (push E R)))
    R))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate-bindings
;; Given a list of variables and a list of matching bindings generate 
;; a dotted pair style bindings list from the two.
;;
;; This is used with the Operator-Variables and OpApp-Values elements.
;;
;; Arguments:
;;   Vars: A list of variables to be matched.
;;   Vals: The list of values to be matched with them.
;;
;; Returns: The result of matching the variables and values.

(defun generate-bindings (Vars Vals)
  "Obtain a list of bindings for the qual listf of vars and vals."
  
  (if (not (equal (length Vars)
		  (length Vals)))
      (error "Incompatable Variable/Value lists supplied to generate-bindings. ~A ~A" Vars Vals)
  
    (mapcar #'cons Vars Vals)))


;;=============================================================================
;; Code by Kurt VanLehn.

;;; the regular subst-bindings is recursive, but we only want the top level 
;;; value to have a quote around it.  For instance, if ?x is bound to ?y which 
;;; is bound to 5, then we want (mod ?x 90) to become (mod '5 90).  Thus at the
;;; first occurance in the cons-tree walk of a variable, we switch from this 
;;; function to the regular subst-bindings function (KVL)

(defun subst-bindings-quoted(Bindings X)
   "Returns X with all variable inside a quote, ready for eval'ing"
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((variable-p x)
         (list 'quote (subst-bindings bindings (lookup x bindings))))
        ((atom x) x)
        (t (cons (subst-bindings-quoted bindings (car x))
                 (subst-bindings-quoted bindings (cdr x))))))


;;=============================================================================
;; Code by Collin Lynch

;;; We may want to perform recursive binds or more complex binds in which 
;;; case it will become necessary to call some function for each element to 
;;; be bound.  
;;; Subst-bindings-func does so by calling the specified predicate function 
;;; and setting its value in the location specified.

(defun subst-bindings-func (Bindings Func X)
   "Returns X with all variables inside replaces by the value of Func(B)."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((variable-p x)
	 (funcall Func (subst-bindings Bindings (lookup x bindings))))
        ((atom x) x)
        (t (cons (subst-bindings-func bindings Func (car x))
                 (subst-bindings-func bindings Func (cdr x))))))



;;; Given a list of elements this code collects up all those members of
;;; the list that unify with the supplied pattern.  Otional keywoyd arguments
;;; can be supplied for key and bindings if any.

(defun collect-unifiers (set pattern &key (bindings no-bindings) 
					  (key #'identity))
  "collect all of the expressions that unify with the pattern in set."
  (remove-if-not 
   #'(lambda (x)
       (unify (funcall key x) 
	      pattern bindings))
   set))
  

;;; given a format spec in need of unification and bindings
;;; substitute the bindings into it and call format on the
;;; results.
(defun format-subst (form bindings)
  "Apply format to the result of binding the elements."
  (apply 'format (cons nil (subst-bindings bindings form))))

;;;; ==================================================================
;;;; Strip off the leading question mark from a variable

(defun strip-variable-qm (var)
  "Get back the symbol for the var sans the quote mark."
  (if (not (variable-p var))
      (error "Non-var supplied to strip-variable-qm.")
    (intern (string-upcase (subseq (format nil "~a" var) 1)))))

(defun strip-expression-var-qms (exp)
  "Apply strip-variable-qm to all vars in exp."
  (cond ((null exp) ())
	((variable-p (car exp))
	 (cons (strip-variable-qm (car exp))
	       (strip-expression-var-qms (cdr exp))))
	((listp (car exp)) 
	 (cons (strip-expression-var-qms (car exp))
	       (strip-expression-var-qms (cdr exp))))
	(t (cons (car exp) (strip-expression-var-qms (cdr exp))))))


;;; Cycle through all of the variables in the expression replacing them
;;; with an upper-case string form sans question mark.
(defun strip-replace-exp-vars (exp)
  "Apply strip-variable-qm to all vars in exp."
  (cond ((null exp) ())
	((variable-p exp) (list (string-upcase 
				 (subseq (format nil "~w" exp) 1)) ))
	((atom exp) (list exp))
	((variable-p (car exp))
	 (cons (string-upcase (subseq (format nil "~w" (car exp)) 1)) 
	       (strip-replace-exp-vars (cdr exp))))
	((listp (car exp))
	 (cons (strip-replace-exp-vars (car exp))
	       (strip-replace-exp-vars (cdr exp))))
	(t (cons (car exp) (strip-replace-exp-vars (cdr exp))))))


;;;; =======================================================================
;;;; Non-destrictively modify bindings
;;;; Given a list of bindings we may want to modify them by either removing 
;;;; a variable binding or replacing an existing binding with a new one.
;;;; This code does so.

;;; Given a list of bindings remove a variable binding
;;; from it (by variable) if it exists.
(defun reduce-bindings (Var Bindings)
  "Remove the variable from bindings if it is there."
  (remove Var Bindings :key #'car))


;;; Given a list of bindings change the binding for a specified
;;; var from its current value to the supplied value. (nondestructive)
;;; But change it only if the variable already has a value.
(defun change-bindings (Var Value Bindings)
  "Change the bindings value for var from its initial form the new one."
  (when (get-binding Var Bindings)
    (extend-bindings Var Value (reduce-bindings Var Bindings))))


#| Unused for now.
;;; One expression (containing variables) subsumes another if:
;;; 1. They are of the same length and
;;; 2. All positions that contain bound constants in one 
;;;    expression contain identical bound constants in the
;;;    other and
;;; 3. If there exist any unbound variables within the
;;;    subsuming expression then at least one of the
;;;    corresponsing locations in the subsumed expression
;;;    is bound.
;;;
;;; Therefore foo(a ? c) subsumes foo(a x c) but not 
;;; foo(a ? ?)
;;;
;;; This is a recursive process of testing.

(defun expression-subsumes? (subsumer subsumed &optional (bindings no-bindings))
  "Does the subsumer subsume the subsumed?"
  (declare (optimize (speed 0)))
  (cond ((eq bindings NIL) NIL)
	((eql subsumer subsumed) bindings)
	;;((and (variable-p subsumer) (variable-p subsumed)) bindings)
	((and (consp subsumer) (consp subsumed)) 
	 (pprint (LIST subsumer subsumed))
	 (expression-subsumes? 
	  (cdr subsumer) (cdr subsumed)
	  (expression-subsumes? 
	   (car subsumer) (car subsumed) bindings)))
	(t nil)))
|#







