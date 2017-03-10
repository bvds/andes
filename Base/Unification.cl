;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;; Unification.cl
;; This code is from Peter Norvig's book; Paradigms of AI programming.  
;; This code was downloaded from: ftp.mkp.com/pub/Norvig on 08/02/2000 
;; and is based upon the files patmatch.lisp, Prolog1.lisp and 
;; unify.lisp. 
;;; Copyright (c) 1991 Peter Norvig
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2008

;;(load "c:/SolutionGraph/Utility.cl")

;;(package "Unification")
(export 'unify)   ;for import into symbols package

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
  (sublis (mapcar #'(lambda (var) (when (and nil (search "AAAA" (string var))) (format t "*** rename-variables list ~S for ~S~%" (variables-in x) x) (break)) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun sublis-comma (x y)
  "sbcl, version 1.2.2 represents comma as a struct. 
   Modify sublis to treat backquotes as part of a tree."
  #-sbcl (sublis x y)
  #+sbcl (cond ((consp y)
                (reuse-cons (sublis-comma x (car y)) (sublis-comma x (cdr y)) y))
               ((sb-impl::comma-p y)
                (setf (sb-impl::comma-expr y)
                      (sublis-comma x (sb-impl::comma-expr y)))
                y)
               (t (sublis x y))))


(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

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
         (when (and t (search "AAAA" (string x))) (break))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
	;; remove null keyword pairs
	((and (valid-keyword-pair x)
	      (null (subst-bindings bindings (cadr x))))
	 (subst-bindings bindings (cddr x)))
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun canonicalize-unify (x)
  "Canonicalize keyword pairs and orderless in bound expression."
  ;; This allows normal expression compare and hashification with equal
  (cond ((variable-p x) (error "Cannot canonicalize unbound expression ~A" x))
	((atom x ) x)
	((eql (car x) 'orderless)
	 (let ((z (cons (car x)
			(sort (mapcar #'canonicalize-unify (cdr x)) #'expr<))))
	   (if (equal z x) x z)))  ;reuse object, when possible
	((valid-keyword-pair x)
	 (let ((z (apply #'nconc (sort (keywords-to-pointers x) 
				       #'string< :key #'car))))
	   (if (equal z x) x z))) ;reuse object, when possible
	(t (reuse-cons (canonicalize-unify (car x))
		       (canonicalize-unify (cdr x))
		       x))))

(defun keywords-to-pointers (x)
  ;; Turn keyword pairs into a list of lists, removing nils.
  (when x 
    (assert (valid-keyword-pair x))
    (let ((z (canonicalize-unify (cadr x))))
      (if z
	  (cons (list (car x) z) (keywords-to-pointers (cddr x)))
	  (keywords-to-pointers (cddr x))))))

;;;; ================================================================== 
;;;;                              Unification
;;;; ================================================================== 

(defun unify (x y &optional (bindings no-bindings) test test-function)
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
	;; allow 1.0 and 1 to match
	((and (numberp x) (numberp y) (= x y)) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
	;; this allows for arbitrary matching functions
	((and test (funcall test x) (funcall test y)) 
	 (funcall test-function x y bindings))
	;; handle orderless objects
	((and (orderless-p x) (orderless-p y)) 
	 (unify (order-expr (cdr x) #'orderless-sort bindings) 
		(order-expr (cdr y) #'orderless-sort bindings) 
		bindings test test-function))
	;; handle symmetry under cyclic permutations
	((and (cyclic-p x) (cyclic-p y)) 
	 (unify (order-expr (cdr x) #'cyclic-sort bindings) 
		(order-expr (cdr y) #'cyclic-sort bindings) 
		bindings test test-function))
	;; Match any keyword pairs; only compare keyword pair against 
	;; a proper list.  
	((and (valid-keyword-pair x) (listp y) (null (cdr (last y))))
	 (unify-keyword x y bindings))
	((and (valid-keyword-pair y) (listp x) (null (cdr (last x))))
	 (unify-keyword y x bindings))
	;; Then test that at least one order did work.
	((or (valid-keyword-pair x) (valid-keyword-pair y)) 
	 (error "Can't unify keyword pairs in ~S~%     and ~S~%" x y)) 
	;; Recursion for cons
        ((and (consp x) (consp y))
	 (unify (rest x) (rest y) 
			 (unify (first x) (first y) 
				bindings test test-function) 
			 test test-function))
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
  "Find match in y for first keyword pair in x; y must be a proper list."
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


;; valid expressions include:  (symmetry-type a b c ...)
;;                             (symmetry-type . ?a)
;;                             (symmetry-type ?a)      


(defun order-expr (x sort-function bindings)
  "handle any variables when ordering a list according to symmetry-type"
  (cond 
   ((variable-p x) x)                 ;(symmetry-type . ?a)
   ((and (listp x) (null (cdr x))) x) ;(symmetry-type ?a)
   ;; (orderless a b c ...)
   ((and (listp x) (null (cdr (last x)))) ;check for proper list
    ;; Sort everything before any keywords
    (let ((keywords (member-if #'keywordp x)))
      (nconc 
       (funcall sort-function 
		(copy-list (subst-bindings bindings (ldiff x keywords))))
	 keywords)))
   (t (error "Invalid list ~S.~%  Need a proper list with all variables bound." 
	     x))))

;;; Orderless lists with (orderless ...)

(defun orderless-p (x)
  (and (consp x) (eq (car x) 'orderless)))

(defun orderless-sort (x) (sort x #'expr<))

;;; Symmetry under cyclic permutations with (cyclic ...)

(defun cyclic-p (x)
  (and (consp x) (eq (car x) 'cyclic)))

;; use (setf *print-circle* t) to debug
(defun cyclic-sort (x)
  "canonical order under cyclic permutations using expr<, destructive"
  (let ((lx (length x)) (best x))
    (setf (cdr (last x)) x) ;make circular list
    (do ((y (cdr x) (cdr y))) ((eq y x)) ;; iterate over cyclic permutations
	(when (circular-expr< y best lx) (setf best y))) ;compare with best
    (setf (cdr (nthcdr (- lx 1) best)) nil)    ;undo circular list
    best))

(defun circular-expr< (a b n)
  "version of expr< for use with circular lists"
  (cond ((< n 1) nil)
	((expr< (car a) (car b)) t)
	((expr< (car b) (car a)) nil)
	((circular-expr< (cdr a) (cdr b) (- n 1)))))

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
  (remove-if-not #'(lambda (e) (unify e filter bindings)) Exps))



;;=============================================================================
;; Code by Kurt VanLehn.

;;; the regular subst-bindings is recursive, but we only want the top level 
;;; value to have a quote around it.  For instance, if ?x is bound to ?y which 
;;; is bound to 5, then we want (mod ?x 90) to become (mod '5 90).  Thus at the
;;; first occurance in the cons-tree walk of a variable, we switch from this 
;;; function to the regular subst-bindings function (KVL)

(defun subst-bindings-quoted (bindings x &key unground-ok)
  "Returns x with all variables inside a quote, ready for eval'ing"
  ;; If bindings equals no-bindings, we still need to test for 
  ;; unground variables.
  (cond ((eq bindings fail) fail)
        ((variable-p x)
	 (let ((y (subst-bindings bindings x)))
	   (if (or unground-ok (groundp y))
	       (list 'quote y)
	       (error "subst-bindings-quoted unground expression ~A" y))))
	((atom x) x)
	;; Just substitute in variables, allowing ungrounded expressions
	((eq (car x) 'quote) (subst-bindings bindings x))
	;; allow lisp expression to test for variables or report errors
	((member (car x) '(groundp error warn)) 
	 (reuse-cons (car x)
		     (subst-bindings-quoted bindings (cdr x) :unground-ok t)
		     x))
        (t (reuse-cons (subst-bindings-quoted bindings (car x) 
					      :unground-ok unground-ok)
		       (subst-bindings-quoted bindings (cdr x)
					      :unground-ok unground-ok)
		       x))))


;;; Given a list of elements, this code collects up all those members of
;;; the list that unify with the supplied pattern.  Optional keyword arguments
;;; can be supplied for key and bindings if any.

(defun collect-unifiers (set pattern &key (bindings no-bindings) 
					  (key #'identity))
  "collect all of the expressions that unify with the pattern in set."
  (remove-if-not 
   #'(lambda (x)
       (unify (funcall key x) 
	      pattern bindings))
   set))
  

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







