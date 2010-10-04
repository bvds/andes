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
;;;  <http:;;;www.gnu.org/licenses/>.
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
(defun get-default-phrase (x)
  (when x
    (match:word-string (expand-vars (new-english-find x)))))

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
(defun def-np (x &rest args)
  (declare (ignore args))
  (if (listp x)
      (get-default-phrase x)  
    ;; Assume x is an atom
    (cond ((numberp x)      (format nil "~A" x))
	  ((pronounp x) (format nil "~(~A~)" x))
	  ((upper-case-namep x) (format nil "~A" x))
	  ((proper-namep x) (format nil "~(~A~)" x))
	  ;; else assuming x is a common noun
	  (T                (format nil "the ~(~A~)" x)))
    ))

(defun def-np-model (x)
  (if (listp x)
      x  
      ;; Assume x is an atom
      (cond ((numberp x)      (format nil "~A" x))
	    ((pronounp x) (format nil "~(~A~)" x))
	    ((upper-case-namep x) (format nil "~A" x))
	    ((proper-namep x) 
	     ;; heuristic is not always correct, allow "the"
	     `((allowed "the") ,(format nil "~(~A~)" x)))
	    ;; else assume x is a common noun
	    (T `((preferred "the") ,(format nil "~(~A~)" x))))
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun indef-np	(x &rest args)
  (declare (ignore args))
  (if (atom x)
      (if (stringp x)
	  (if (member (char x 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U ))
	      (format nil "an ~(~A~)" x)
	    (format nil "a ~(~A~)" x))
	(if (member (char (symbol-name x) 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U ))
	    (format nil "an ~(~A~)" x)
	  (format nil "a ~(~A~)" x)))
    (get-default-phrase x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun pp (x &rest args) 
  "return a temporal prepositional phrase for the given time"
  (declare (ignore args))
  (cond ((null x) NIL) ; NB: must be prepared for NIL for timeless things.
	((listp x) (get-default-phrase x)) ;handles (during ...)
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
    (get-default-phrase x)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adj (x &rest args)
  (adjective x args))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; for concise reference to quantities in algebraic contexts:
(defun var-or-quant (x &rest args)
"return student's var for quant if one exists, else full quantity def."
    (or (symbols-label x)
        (def-np x args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun conjoined-defnp (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (nlg-atom-default x)
    (nlg-print-list x "and" 'def-np)))

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
    (get-default-phrase x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This is a shortcut for including time when it exists
(defun at-time (x &rest args)
  (when (cdr args) (warn "unexpected extra args ~A in at-time" (cdr args)))
  (if (= (length args) 1)
      (format nil "~A~@[ ~A~]" (get-default-phrase x)
	      (get-default-phrase (list 'time (car args))))
      (get-default-phrase x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun lower-case (x &rest args)
  (declare (ignore args))
  (if (atom x)
      (format nil "~(~A~)" x)
    (get-default-phrase x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun goal (x &rest args)
  (if (atom x)
      (lower-case x args)
    (or (nlg-find x *Ontology-GoalProp-Types* #'GoalProp-Form #'GoalProp-nlg-english)
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

(defun psm-english (x)
  (if (atom x)
      (nlg-atom-default x)
    (or (nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-nlg-english)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-nlg-english)
	(format nil "[PSM: ~A]" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling Entry-Props
(defun nlg-entryprop (x &rest args)
  (declare (ignore args))
  (if (atom x) 
      (nlg-atom-default x)
    (or (nlg-find x *Ontology-EntryProp-Types* #'EntryProp-KBForm #'EntryProp-nlg-english)
	(nlg-find x *Ontology-EntryProp-Types* #'EntryProp-HelpForm #'EntryProp-nlg-english)
	(format Nil "[EntryProp: ~a]" x))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given an Equation nlg it resulting in the appropriate form.
(defun nlg-equation (x)
  (cond ((atom x) (format nil "~A" x))
	((nlg-find x *Ontology-PSMClasses* #'PSMClass-form #'PSMClass-nlg-english))
	(t (format nil "equation:[~A]" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;;   Generate dictionary from set of model sentences.
;;;;

(defun dictionary-handler (model dictionary)
  "Extend model to include (var ...), which is ignored."
  (unless (and (consp model) (eql (car model) 'var))
      (warn "dictionary-handler:  unknown model ~A" model))
  dictionary)

(defun generate-dictionary (sysentries)
  "Generate a list of possible words from a list of models, ignoring (var ...)"
  (let ((match:add-to-dictionary-handler 'dictionary-handler)
	dictionary)
    (dolist (x sysentries)
      (setf dictionary (match:add-to-dictionary 
		    (SystemEntry-model x) dictionary)))
    dictionary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qnode-new-english (qnode)
  "Match qnode to Ontology, pulling out model sentence, including any var."
  ;; use same strategy as for systementries.
  (or (Qnode-model qnode)   ;Use Qnode-model as a cache
      (setf (Qnode-model qnode)
	    `(or (var ,(qnode-exp qnode))
		 ,(new-english-find (qnode-exp qnode))))))

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
  (dolist (rule *Ontology-ExpTypes*)
    (let ((bindings (unify (Exptype-form rule) prop)))
      (when bindings 
	(return-from new-english-find
	 (expand-new-english (ExpType-new-english rule) bindings)))))
  
  ;; If it is a symbol, use improved version of def-np.
  (when (atom prop)
    (return-from new-english-find (def-np-model prop)))
  
  ;; On failure, warn and return nil
  (warn "new-english-find:  no ontology match for ~S" prop))

 

(defun expand-new-english (model &optional (bindings no-bindings))
  "Expand model tree, expanding ontology expressions, parse strings into list of words, substituting bindings, evaluating lisp code, and removing nils."
  (cond ((stringp model) 
	 ;; If there is more than one word, break up into list of words.
	 (let ((this (match:word-parse model))) (if (cdr this) this model)))
	((null model) model)
	((variable-p model) 
	 (if (all-boundp model bindings)
	     (expand-new-english (subst-bindings bindings model))
	     (warn "expand-new-english:  Unbound variable ~A" model)))
	((and (consp model) (member (car model) '(preferred allowed)))
	 (when (cddr model) 
	   (warn "expand-new-english:  ~(~A~) with more than one argument:  ~A"
		 (car model) model))
	 (list (car model) (expand-new-english (cadr model) bindings)))
	((and (consp model) (member (car model) '(and or conjoin)))
	 (let ((args (expand-new-english-list (cdr model) bindings)))
	   (when args (cons (car model) args))))
	;; expansion of var must be done at run-time.
	((and (consp model) (eql (car model) 'var)) 
	 (subst-bindings bindings model))
	((and (consp model) (eql (car model) 'eval))
	 ;; For extensibility, allow eval to have more arguments.
	 ;; Here, we just ignore them.
	 (expand-new-english
	  (eval (subst-bindings-quoted bindings (second model)))))
	;; ordered sequence, remove empty elements
	((and (consp model) (or (stringp (car model)) (listp (car model))))
	 (remove nil (expand-new-english-list model bindings)))
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
	((eql (car x) 'eval) (subst-bindings-quoted bindings x))
	(t (reuse-cons (subst-bindings-careful bindings (car x))
		       (subst-bindings-careful bindings (cdr x))
		       x))))

;; Should be "private" to nlg
(defun expand-new-english-list (x &optional (bindings no-bindings))
  "If object is a list, expand"
  ;; Handles cases where members of a list are atoms in Ontology
  ;; and lists with bindings of the form (... . ?rest)
  ;; along with (... . (eval ...))
  (cond ((null x) x)
	;; Handle (... . ?rest)
	((variable-p x) (expand-new-english-list 
			 (subst-bindings bindings x)))
	;; Handle (... . (eval ...))
	((and (consp x) (eql (car x) 'eval))
	 (let ((result (expand-new-english x bindings)))
	   (unless (consp result)
	     (warn "eval must return a list:  ~A returned ~A" x result))
	   result))
	;; recursion
	((consp x) (cons (expand-new-english (car x) bindings)
			 (expand-new-english-list (cdr x) bindings)))
	(t (warn "expand-new-english-list:  invalid list structure in ~A" x))))


(defun expand-vars (model)
  "Expand (var ...) expressions and remove nils from model tree."
  (cond ((stringp model) model)
	((null model) model)
	((member (car model) '(preferred allowed and or conjoin))
	 ;; untrapped error when second arg of conjoin expands to nil
	 (let ((args (expand-vars (cdr model))))
	   (when args (cons (car model) args))))
	((or (stringp (car model)) (listp (car model))) ;plain list
	 ;; mapcar copies list; subsequent operations can be destructive
	 (delete nil (mapcar #'expand-vars model)))
	;; expansion of var must be done at run-time.
	((eql (car model) 'var)
	 (apply #'symbols-label (cdr model)))
	(t (warn "expand-vars:  invalid expand ~A" model) model)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;          Use quantity ontology as a parser
;;;
;;;
;; Working on using match ontology as a parser.  Outstanding problems:
;;
;; Recursion of bindings as we recurse through members of ontology
;; is broken.  We currently use a single binding list so any time
;; there are multiple operators using the same variable name, we
;; have problems.  Two solutions:  munge the variable names (like is done
;; in problem generation), or make the bindings a list of lists.
;;
;; The other problem is that strings in the ontology need to be
;; expanded into a list of words.



(defmacro update-bound (best x bind)
  (let ((this (gensym)))
    `(let ((,this ,x))
       (when (< (car ,this) (car ,best))
	 (push ,bind (cdr ,this)) ;add bind to result tree
	 (setf ,best ,this))))) 

(defvar *local-bindings*)
(defvar *atom-bindings*)
(defvar *parent-ontology*) ;for debugging

(defun model-subst-bindings (bindings model)
  "Apply subst-bindings to model, using subst-bindings-quoted for eval."
  ;; Right now, we apply bindings ASAP.  This is less efficient than waiting.
  (cond 
    ((atom model) (subst-bindings bindings model))
    ((eq (car model) 'eval)
     (subst-bindings-quoted bindings model))
    (t (cons (model-subst-bindings bindings (car model))
	     (model-subst-bindings bindings (cdr model))))))

(defun word-count-handler (model &key max)
  (cond
    ((variable-p model) 
     (if (member model *local-bindings* :key #'car)
	 ;; If a binding exists, apply it.
	 (match:word-count (subst-bindings *local-bindings* model) :max max)
	 ;; else, make a guess from *atom-bindings*
	 (apply (if max #'max #'min)
		(mapcar #'(lambda (a) (match:word-count a :max max))
			*atom-bindings*))))
    ((and (consp model) (eql (car model) 'var))
     (match:word-count (apply #'symbols-label (cdr model)) :max max))
    ;; subsequent arguments of (eval ...) are to to be cons'es
    ;; of a variable and an expression that can be eval'ed to a list
    ;; of possible values for that variable.
    ;;     (eval .... (?a . *problem-atoms*) (?t . '((time 0) (time 1))))
    ((and (consp model) (eql (car model) 'eval))
     (if (cddr model)
	 (let* ((tm (third model))
		(rm (remove tm model))
		(var (car tm))
		(best (if max 0 10000)))
	   ;; iterate through possible values for variable
	   (dolist (val (eval (cdr tm)))
	     ;; Add var-val pair to binding list, if consistent 
	     ;; with any existing bindings, making local version of bindings.
	     (let ((*local-bindings* (or (unify val var *local-bindings*)
					 (warn "word-count-handler model ~A inconsistent with bindings ~A" model *local-bindings*)
					 ;; if inconsistent, go with parent 
					 ;; bindings
					 *local-bindings*)))
		    (let ((x (match:word-count rm :max max)))
		      (when (if max (> x best) (< x best)) (setf best x)))))
	   best)
	 ;; no bindings left, just evaluate.
	 (let ((mm (subst-bindings-quoted *local-bindings* (second model))))
	   (if (groundp mm)
	       (match:word-count (eval mm) :max max)
	       (progn (warn "unbound variables in ~A" model)
		      (if max 10000 0))))))

    ;; Now assume we have a member of the Ontology.
    ;; Follow method in new-english find
    ;; First, determine if there is any problem-specific
    ;; Ontology match.
    ((dolist (rule (problem-english *cp*))
       (let ((*local-bindings* (unify (car rule) model *local-bindings*)))
	 (when *local-bindings*
	   (return
	     (match:word-count (cdr rule) :max max))))))
    
    ;; Then run through general Ontology to find match.
    ((dolist (rule *Ontology-ExpTypes*)
       (let ((parent-bindings (copy-list *local-bindings*))
	     (*parent-ontology* 
	      (cons (list (exptype-form rule) *local-bindings*) 
		    *parent-ontology*))
	     (*local-bindings* (unify (Exptype-form rule) model *local-bindings*)))
	 (when (and (not *local-bindings*) (unify (exptype-form rule) model))
	   (warn " word-count-handler:  bad binding list for ~A and ~A~%    bindings ~A~%    parents:  ~A" 
		 model (exptype-form rule) parent-bindings
		 *parent-ontology*))
	 (when *local-bindings*
	   (return (match:word-count (Exptype-new-english rule) :max max))))))
    
    ;; If it is a symbol, use improved version of def-np.
    ((atom model)
     (match:word-count (def-np-model model) :max max))
    
    ;; On failure, warn and worst possible case
    (t 
     (warn "word-count-handler:  no ontology match for ~S" model)
     (if max 1000 0))))

(defun other-object-handler (student model &key best)
  (cond
    ((variable-p model) 
     (if (member model *local-bindings* :key #'car)
	 ;; If a binding matches, apply it.
	 (match:match-model student 
			    (subst-bindings *local-bindings* model) 
			    :best best)
	 ;; else, make guesses from *atom-bindings*
	 (let ((best-result (list best)))
	   (dolist (a *atom-bindings*)
	     (update-bound best-result
			   (match:match-model student a :best best)
			   (cons model a))) ;binding into result
	   best-result)))
    ((and (consp model) (eql (car model) 'var))
     (match:match-model student 
			(apply #'symbols-label (cdr model)) :best best))
    ;; subsequent arguments of (eval ...) are to to be cons'es
    ;; of a variable and an expression that can be eval'ed to a list
    ;; of possible values for that variable.
    ;;     (eval .... (?a . *problem-atoms*) (?t . '((time 0) (time 1))))
    ((and (consp model) (eql (car model) 'eval))
     (if (cddr model)
	 (let* ((tm (third model))
		(rm (remove tm model))
		(var (car tm))
		(best-result (list best)))
	   ;; iterate through possible values for variable
	   (dolist (val (eval (cdr tm)))
	     ;; Add var-val pair to binding list, if consistent 
	     ;; with any existing bindings, making local version of bindings.
	     (let ((*local-bindings* (or (unify val var *local-bindings*)
					 (warn "word-count-handler model ~A inconsistent with bindings ~A" model *local-bindings*)
					 ;; if inconsistent, go with parent 
					 ;; bindings
					 *local-bindings*)))
	       (update-bound best-result
			     (match:match-model student rm 
						:best (car best-result))
			     (cons var val))))
	   best-result)
	 ;; no bindings left, just evaluate.
	 (let ((mm (subst-bindings-quoted *local-bindings* (second model))))
	   (if (groundp mm)
	       (match:match-model student (eval mm) :best best)
	       (progn (warn "unbound variables in ~A, bindings ~A" 
			    model *local-bindings*)
		      '(10000))))))

    ;; Now assume we have a member of the Ontology.
    ;; Follow method in new-english find
    ;; First, determine if there is any problem-specific
    ;; Ontology match.
    ((dolist (rule (problem-english *cp*))
       (let ((*local-bindings* (unify (car rule) model *local-bindings*)))
	 (when *local-bindings* 
	   (return (match:match-model student (cdr rule) :best best))))))
    
    ;; Then run through general Ontology to find match.
    ((dolist (rule *Ontology-ExpTypes*)
       (let ((parent-bindings *local-bindings*)
	     (*parent-ontology* 
	      (cons (list (exptype-form rule) *local-bindings*) 
		    *parent-ontology*))
	     (*local-bindings* (unify (Exptype-form rule) model *local-bindings*)))
	 (when (and (not *local-bindings*) (unify (exptype-form rule) model))
	   (warn "bad binding list for ~A and ~A bindings ~A" 
		 model (exptype-form rule) parent-bindings))
	 (when *local-bindings*
	   (return (match:match-model student (Exptype-new-english rule) :best best))))))
    
    ;; If it is a symbol, use improved version of def-np.
    ((atom model)
     (match:match-model student (def-np-model model) :best best))
    
    ;; On failure, warn and return nil
    (t 
     (warn "other-object-handler:  no ontology match for ~S" model)
     '(10000))))


;; (rhelp)
;; (andes-init)
;; (read-problem-info 'kt1a)
;; (best-model-parses (match:word-parse "the mass of the motocycle") *ontology-exptypes*)

(defun get-atom-bindings ()
  (second (find 'bodies (problem-choices *cp*) :key #'car)))

(defun best-model-parses (student ontology &key (cutoff 5) (equiv 1.25) 
			  (epsilon 0.25))
  "Use given ontology and the problem ontology to find find the best parse of the student phrase, returning alist of best match propsitions."
  ;; analog to best-model-matches
  ;; assume given ontology is a qexp
  (let ((best (/ cutoff equiv)) quants bound
	(*atom-bindings* (get-atom-bindings)) ;should be in knowledge?
	(match:unknown-object-handler 'other-object-handler)
	(match:word-count-handler 'word-count-handler)) 
    (dolist (qexp ontology)
      (setf bound (max epsilon (* best equiv)))
      (let* ((*local-bindings* no-bindings) ;used in other-object-handler
	     (*parent-ontology* (list (list (exptype-form qexp) *local-bindings*)))  ;for debugging
	     (this (match:match-model student (exptype-new-english qexp) :best bound)))
	(when (< (car this) bound) 
	  (push (cons this 
		      (subst-bindings (pull-out-bindings (cdr this)) 
				      (exptype-form qexp))) quants))
	(when (< (car this) best) (setf best (car this)))))
    ;; Remove any quantities that are not equivalent with best fit
    ;; and return result. 
    (delete-if #'(lambda (x) (> (car (car x)) (* best equiv))) quants)))

(defun pull-out-bindings (x)
  "pull out binding pairs from tree returned with model"
  ;; The tree structure consists of proper lists, bindings pairs (?var . val),
  ;; nil and strings.
  (cond ((and (consp x) (variable-p (car x))) (list x))
	((consp x) (mapcan #'pull-out-bindings x))
	((null x) nil)
	((stringp x) nil)
	(t (warn "pull-out-bindings:  unknown object ~A" x))))


(defun subst-ordered-bindings (m-bindings x)
  "m-bindings is a list of binding lists, each to be applied successively."
  ;; It would be more efficient to first recurse through x,
  ;; before applying bindings.
  (if (= (length m-bindings) 1)
      (subst-bindings (car m-bindings) x)
      (subst-ordered-bindings (cdr m-bindings) 
			      (subst-bindings (car m-bindings) x))))

;; (let ((match:unknown-object-handler 'other-object-handler)) (match:match-model '("fred") '(or "fred" "barney")))
