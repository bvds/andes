;; nlg.cl -- Natural Language Generator code and data
;; Author(s):
;;  Collin Lynch (c?l) <collinl@pitt.edu>
;;  Linwood H. Taylor (lht) <lht@lzri.com>
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
;; Modified:
;;  ??? - (c?l) -- created
;;  29 June 2001 - (lht) -- begin to implement working code
;;  3/10/2003 - (c?l) -- Added support for entryprops.
;;  3/12/2003 - (c?l) -- Fixed compiler warnings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nlg (x &optional (type 'def-np) &rest args)
  (if (null type)
      (if (consp x)
	  (format nil "~(~A~)" x)
	  (format nil "~(~A~)" x))
      (if (variable-p x)
	  (format nil "~@[~*at ~]some ~(~A~)" (eq type 'pp) (var-rootname x))
	  (if args
	      (apply (if (equal type 'time) 'moment type) x args)
	      (funcall (if (equal type 'time) 'moment type) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-print-list (x joiner art)
  (cond ((= (length x) 1) (nlg (first x) art))
	((= (length x) 2) (format nil "~A ~A ~A" (nlg (first x) art) joiner 
				  (nlg (second x) art)))
	((= (length x) 3) (format nil "~A, ~A, ~A ~A" (nlg (first x) art) 
				  (nlg (second x) art)
				  joiner (nlg (third x) art)))
	(t (format nil "~A, ~A" (nlg (first x) art) 
		   (nlg-print-list (rest x) joiner art)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-find (x lst ff ef)
  (let (bindings)
    (dolist (rule lst)
	(when (setf bindings (unify (funcall ff rule) x no-bindings))
	    (return-from nlg-find 
	        (nlg-bind rule ef bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-bind (rule ef bindings) ; ef is accessor to get nlg spec from struct
  (let* ((spec (funcall ef rule)) ; may be NIL if no english specified
         (format (first spec))
	 (args (subst-bindings-quoted bindings (rest spec))))
    (when spec
      (eval `(format nil ,format ,@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun def-np (x)
  (let ((y (expand-vars (new-english-find x) :html-format t)))
    (when y (match:word-string y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-atom-default (x)
  (format nil "~(~A~)" x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun var-rootname (var)
  "return variable name sans question marks and gensym digits"
  ;; eg (var-rootname '?body1234) => "body" 
  (remove-if #'(lambda (c) 
		 (or (digit-char-p c) (char= c #\?)))
	     (string var)))


;;; Most of our body names are common nouns like "car", "block", "man", 
;;; which require an article, so that is default in def-np, But some problems 
;;; use "block1", "charge2", etc. which are proper names that shouldn't have 
;;; an article.  We use a simple heuristic to distinguish proper names 
;;; by testing whether the name ends in a number. 
;;; This will give wrong answer for the very few common nouns we might use 
;;; such as "F-14" that end in a number, but that's preferable to being wrong 
;;; on all the block1's etc.

(defun proper-namep (x)
  "true if given symbol is probably a proper name"
  (and (symbolp x) 
       ;; ends with digit:
       (numberp (read-from-string (subseq (string x) 
					  (1- (length (string x))))))))

;;; Our circuit elements are named R1, C1, etc. For these we want to suppress 
;;; the default lower-casing of names done by def-np.  Similarly for 
;;; single-character names A, B, C.  
;;; These are treated as proper names (no "the").
(defun upper-case-namep (x)
  "true if name symbol is probably best left all upper case"
  (or (member x '(YP M1 M2))		; list of explicit names to lower case
      (equal (length (string x)) 1)
      (and (numberp (read-from-string (subseq (string x) 
					      (1- (length (string x))))))
	   (<= (length (string x)) 2))
      ))
;;
;;  list of pronouns
;;
(defun pronounp (x)
  (member x '(me)))			;add to list as needed


;; special translation for agents terms may be 'unspecified
;; as in "force on car due to ....."
;;(defun agent (x)
;;   (if (eq x 'UNSPECIFIED) "an unspecified agent"
;;    (nlg x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun def-np-model (x)
  (if (listp x)
      x  
      ;; Assume x is an atom
      (cond ((numberp x) (format nil "~A" x))
	    ((pronounp x) (format nil "~(~A~)" x))
	    ((upper-case-namep x) (format nil "~A" x))
	    ((proper-namep x) 
	     ;; heuristic is not always correct, allow "the"
	     `((allowed "the") ,(format nil "~(~A~)" x)))
	    ;; else assume x is a common noun
	    (T `((preferred "the") ,(format nil "~(~A~)" x))))
      ))

(defun variable-defnp (x)
  "Find English phrase for a quantity, allowing for possibility of an associated variable name."
  (let ((y (expand-vars 
	     ;; Create a model phrase.
	     (expand-new-english `(or (variable ,x) ,x)) :html-format t)))
    (when y (match:word-string y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun indef-np (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (if (stringp x)
	  (if (member (char x 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U ))
	      (format nil "an ~(~A~)" x)
	    (format nil "a ~(~A~)" x))
	(if (member (char (symbol-name x) 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U ))
	    (format nil "an ~(~A~)" x)
	  (format nil "a ~(~A~)" x)))
    (def-np x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun pp (x &rest args) 
  "return a temporal prepositional phrase for the given time"
  (declare (ignore args))
  (cond ((null x) NIL) ; NB: must be prepared for NIL for timeless things.
	((listp x) (def-np x)) ;handles (during ...)
	((numberp x) (format nil "at T~D" (- x 1)))
	(t (format nil "at ~(~A~)" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defparameter adjectives
    ;; includes some non-adj ids needing special translations
    '(; accel specs in motion descriptors:
      (speed-up . "speeding up")
      (slow-down . "slowing down")
      ;; special vector angle specifiers:
      (into . "into the plane")
      (out-of . "out of the plane")
      (z-unknown . "unknown, but either into or out of the plane")
      (zero . "zero magnitude")
      ;; rotation specs in rotational motion descriptors
      (cw . "clockwise")
      (ccw . "counterclockwise")
      ;; vector type ids needing special translation:
      (accel . "acceleration")
      (ang-displacement . "angular displacement")
      (ang-velocity . "angular velocity")
      (ang-accel . "angular acceleration")
      (ang-momentum . "angular momentum")
      (relative-position . "relative position")
      ;; energy type ides needing special translation
      (grav-energy . "gravitational potential energy")
      (spring-energy . "elastic potential energy")
      (electric-energy . "electric potential energy")
      ;; sign abbreviations
      (pos . "positive")
      (neg . "negative")
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adjective (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (let ((answer (assoc x adjectives)))
	(if answer
	    (format nil "~A" (cdr answer))
	  (format nil "~(~A~)" x)))
    (def-np x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adj (x &rest args)
  (adjective x args))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; for concise reference to quantities in algebraic contexts:
(defun var-or-quant (x)
"return student's var for quant if one exists, else full quantity def."
    (if (symbols-label x) 
	(strcat "<var>" (symbols-label x) "</var>")
        (def-np x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun conjoined-defnp (x &optional (conjunction "and"))
  "Expand a list of objects using conjoin."
  (let ((y (expand-vars 
	    (expand-new-english
	     ;; Create a model phrase.
	     (list* 'conjoin conjunction x)) :html-format t)))
    (when y (match:word-string y))))

(defun conjoined-names (x &rest args)
  "assume list is proper names"
  (declare (ignore args))
  (if (atom x)
      (format nil "~A" x)
    (nlg-print-list x "and" 'identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun moment (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (format nil "T~(~A~)" (if (numberp x) (1- x) x))
    (def-np x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is a shortcut for including time when it exists
(defun at-time (x &rest args)
  (when (cdr args) (warn "unexpected extra args ~A in at-time" (cdr args)))
  (if (= (length args) 1)
      (format nil "~A~@[ ~A~]" (def-np x)
	      (def-np (list 'time (car args))))
      (def-np x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun lower-case (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (format nil "~(~A~)" x)
    (def-np x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun goal (x &rest args)
  (declare (ignore args))
  (or 
   (nlg-find x *Ontology-GoalProp-Types* #'GoalProp-Form 
	     #'GoalProp-nlg-english)
   ;; Want backtrace if this fails
   (progn (warn "Goal ~A not found in *Ontology-GoalProp-Types*" x)
	  (format nil "[GOAL: ~A]" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun psm-exp (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (nlg-atom-default x)
    (or (nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-ExpFormat)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-ExpFormat)
	(nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-nlg-english)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-nlg-english)
	(format nil "[PSM: ~A]" x))))


(defun psm-exp-hintable (prop)
  "See if psm-exp can return something from Ontology."
  (let ((x (lookup-expression->psmclass prop))
	(y (lookup-expression->psmgroup prop)))
    (or (and x (or (psmclass-nlg-english x) (psmclass-expformat x)))
	(and y (or (psmgroup-nlg-english y) (psmgroup-expformat y))))))


(defun psm-english (x)
  (if (atom x)
      (nlg-atom-default x)
    (or (nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-nlg-english)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-nlg-english)
	(format nil "[PSM: ~A]" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given an Equation proposition, find associated English.
(defun nlg-equation (x)
  (cond ((atom x) (format nil "~A" x))
	((nlg-find x *Ontology-PSMClasses* #'PSMClass-form #'PSMClass-nlg-english))
	(t (warn "nlg-equation failure for ~A" x)
	   (format nil "equation:[~A]" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun new-english-find (prop)
  "Match proposition to Ontology."
  ;; First, determine if there is any problem-specific
  ;; Ontology match.
  (dolist (rule (problem-english *cp*))
    (let ((bindings (unify (car rule) prop)))
      (when bindings 
	(return-from new-english-find 
	  (expand-new-english (cdr rule) bindings)))))

  ;; Then run through general Ontology to find match.
  (multiple-value-bind (rule bindings)
      (lookup-expression-struct prop)
    ;; some members of Ontology are just for internal use
    ;; and have no natural language phrase associated with them.
    (when (and bindings (ExpType-new-english rule))
      (return-from new-english-find
	(expand-new-english (ExpType-new-english rule) bindings))))

  ;; If it is a goalprop, use that.  
  ;; GoalProp still uses old format for english.
  ;; Returns a single string expression.
  (let ((goal (nlg-find prop *Ontology-GoalProp-Types* 
                       #'GoalProp-Form #'GoalProp-nlg-english)))
    (when goal
      ;; Want backtrace to see whose fault this is.
      (warn "Prop ~A found in goals, but not english." prop)
      (return-from new-english-find goal)))

  ;; If it is a symbol, use improved version of def-np.
  (when (atom prop)
    (return-from new-english-find (def-np-model prop)))

  ;; On failure, warn and return nil
  (warn "new-english-find:  no ontology match for ~S" prop))

 

(defun expand-new-english (model &optional (bindings no-bindings))
  "Expand model tree, expanding ontology expressions, parse strings into list of words, substituting bindings, evaluating lisp code, and removing nils."
  (cond ((stringp model) model)
	((null model) model)
	((variable-p model) 
	 (if (all-boundp model bindings)
	     (expand-new-english (subst-bindings bindings model))
	     (warn "expand-new-english:  Unbound variable ~A" model)))
	((and (consp model) (member (car model) 
				    '(preferred allowed key 
				      case-insensitive case-sensitive)))
	 (when (cddr model) 
	   (warn "expand-new-english:  ~(~A~) with more than one argument:  ~A"
		 (car model) model))
	 (let ((arg (expand-new-english (cadr model) bindings)))
	   (when arg 
	     ;; reuse cons, if possible
	     (if (eql arg (cadr model)) model (list (car model) arg)))))
	((and (consp model) (member (car model) '(and or conjoin)))
	 (let ((args (expand-new-english-list (cdr model) bindings)))
	   (when args (reuse-cons (car model) args model))))
	;; expansion of var must be done at run-time.
	((and (consp model) (eql (car model) 'var)) 
	 (subst-bindings bindings model))
	((and (consp model) (eql (car model) 'eval-compiled))
	 ;; For extensibility, allow eval to have more arguments.
	 ;; Here, we just ignore them.
	 (expand-new-english 
	  (apply (second model) (subst-bindings bindings (third model)))))
	((and (consp model) (eql (car model) 'eval))
	 ;; For extensibility, allow eval to have more arguments.
	 ;; Here, we just ignore them.
	 (expand-new-english
	  (eval (subst-bindings-quoted bindings (second model)))))
	;; ordered sequence
	((match:test-for-list model)
	 (expand-new-english-list model bindings))
	(t 
	 ;; Bindings are local to one operator in the ontology
	 ;; so we need to substitute in here.
	 ;; Assume any recursive calls are covered by New-English.
	 (new-english-find (subst-bindings-careful bindings model)))))

(defun subst-bindings-careful (bindings x)
  "Do subst. bindings, but watch out for eval."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
	((atom x) (subst-bindings bindings x))
	;; At this point, it is a cons.
	((eql (car x) 'eval) 
	 ;; Drop any subsequent variable bindings from the eval.
	 ;; One could to a sanity test here with the binding lists.
	 (list 'eval (subst-bindings-quoted bindings (second x))))
	;; Substitute into the parameter list
	((eql (car x) 'eval-compiled)
	 (list 'eval-compiled (second x) (subst-bindings bindings (third x))))
	      
	(t (reuse-cons (subst-bindings-careful bindings (car x))
		       (subst-bindings-careful bindings (cdr x))
		       x))))

;; Should be "private" to nlg
(defun expand-new-english-list (x &optional (bindings no-bindings))
  "If object is a list, expand.  Null elements are not removed."
  ;; Handles cases where members of a list are atoms in Ontology
  ;; and lists with bindings of the form (... . ?rest)
  ;; along with (... . (eval ...))
  (cond ((null x) x)
	;; Handle (... . ?rest)
	((variable-p x) (expand-new-english-list 
			 (subst-bindings bindings x)))
	;; Handle (... . (eval ...))
	((and (consp x) (member (car x) '(eval eval-compiled)))
	 (let ((result (expand-new-english x bindings)))
	   (unless (consp result)
	     (warn "eval must return a list:  ~A returned ~A" x result))
	   result))
	;; recursion, dropping nil terms
	((consp x) 
	 (let ((arg (expand-new-english (car x) bindings)))
	   (if arg 
	       (reuse-cons arg
			   (expand-new-english-list (cdr x) bindings)
			   x)
	       (expand-new-english-list (cdr x) bindings))))
	(t (warn "expand-new-english-list:  invalid list structure in ~A" x))))


(defun expand-vars (model &key html-format)
  "Expand (var ...) expressions and remove nils from model tree."
  (cond ((stringp model) model)
	((null model) model)
	((member (car model) '(preferred allowed key case-sensitive 
			       case-insensitive))
	 (let ((arg (expand-vars (second model) :html-format html-format)))
	   (when arg 
	     ;; reuse cons, if possible
	     (if (eql arg (second model)) model (list (car model) arg)))))
	((member (car model) '(and or conjoin))
	 ;; untrapped error when second arg of conjoin expands to nil
	 (let ((args (expand-vars-list (cdr model) html-format)))
	   (when args (reuse-cons (car model) args model))))
	((match:test-for-list model) ;plain list
	 (expand-vars-list model html-format))
	;; expansion of var must be done at run-time.
	((eql (car model) 'var)
	 (let ((var (apply #'symbols-label (cdr model))))
	   (if (and var html-format) (strcat "<var>" var "</var>") var)))
	(t (warn "expand-vars:  invalid expand ~A" model) model)))

(defun expand-vars-list (model html-format)
  (cond ((atom model) model)
	((consp model)
	 (let ((arg (expand-vars (car model) :html-format html-format)))
	   (if arg ;drop any members of list that expand to nil.
	       (reuse-cons arg
			   (expand-vars-list (cdr model) html-format)
			   model)
	       (expand-vars-list (cdr model) html-format))))))

(defun pull-out-keywords (model &optional in)
  "Pull out any keywords from expanded model."
  (cond ((and in (stringp model)) 
	 (remove-duplicates (match:word-parse model) :test #'string-equal))
	((atom model) nil)
	;; Ignore any sub-models that are optional,
	;; even if they contain keywords.
	((and (consp model) 
	      (member (car model) '(var allowed preferred))) nil)
	((and (consp model) (eql (car model) 'key)
	      (pull-out-keywords (second model) t)))
	((consp model)
	 (union (pull-out-keywords (car model) in)
		(pull-out-keywords (cdr model) in)
		:test #'string-equal))))

(defun pull-out-required-words (model)
  "Pull out all required words from expanded model."
  (cond ((stringp model) 
	 (remove-duplicates (match:word-parse model) :test #'string-equal))
	((atom model) nil)
	;; Ignore any sub-models that are optional.
	((and (consp model) 
	      (member (car model) '(var allowed preferred))) nil)
	((consp model)
	 (union (pull-out-required-words (car model))
		(pull-out-required-words (cdr model))
		:test #'string-equal))))
