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
	((match:test-for-list model)
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
	((match:test-for-list model) ;plain list
	 ;; mapcar copies list; subsequent operations can be destructive
	 (delete nil (mapcar #'expand-vars model)))
	;; expansion of var must be done at run-time.
	((eql (car model) 'var)
	 (apply #'symbols-label (cdr model)))
	(t (warn "expand-vars:  invalid expand ~A" model) model)))

