;; all-quantities.cl -- Natural Language Generator code and data
;; Author(s):
;;  Brett van de Sande 2010
;;; Copyright 2010 by Kurt Vanlehn and Brett van de Sande
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                   Expand Ontology into set of possible phrases.
;;;;
;;;; (trace get-ontology-bindings get-list-ontology-bindings)
;;;; 
;;;;  remaining problems:
;;;;  (test-ontology-bindings '(eqcap1c eqres1c ind3c static1t))

(defvar *ontology-bindings*)

;; Student never defines a vector component directly
;; angle-between is just too hard.
(defparameter *disallowed-quantities* '(compo angle-between))


(defun test-ontology-bindings (&rest topics)
  "Load problems and test ontology bindings"
  (let (results)
    (andes-init)
    (dolist (p (choose-working-probs topics))
      (read-problem-info (string (problem-name P)))
      (when *cp* 
	(format t "Loaded ~A~%" (problem-name p))
	(try-ontology-bodies P)
	(push (cons (try-ontology-bindings) (problem-name p)) results)))
    (sort results #'> :key #'car)))

(defun try-ontology-bindings (&optional names)
  (let ((sum 0))
    ;; The problem-specific ontology is generally specific
    ;; cases of the general ontology and we are looking
    ;; for a general set of propositions...
    (dolist (rule *Ontology-ExpTypes*)
      (when (and (exptype-rank rule) 
		 (not (member (exptype-type rule) *disallowed-quantities*))
		 (or (null names) (member (exptype-type rule) names)))
	(let (*ontology-bindings* 
	      (vars (variables-in (exptype-form rule)))
	      term)
	      ;; (format t "starting ~A~%" (ExpType-form rule))
	  (get-ontology-bindings (ExpType-new-english rule))
	     ;; (format t "final list ~A~%" *ontology-bindings*)
	  
	  ;; calculate number of possible propositions
	  (setf term (apply #'* 
			    (mapcar #'length 
				    (mapcar #'cdr *ontology-bindings*))))
	  (setf sum (+ sum term))
	  ;; (format t "  with ~A possibilities: ~A~%" term (mapcar #'(lambda (x) (cons (car x) (length (cdr x)))) *ontology-bindings*))

	  ;; Test that all variables have actually been bound.	  
	  (when (set-difference vars (mapcar #'car *ontology-bindings*))
	    (format t "unbound variables for ~A~%   got ~A~%" 
		    (exptype-form rule)
		    *ontology-bindings*))

	  ;; Test that all solution props are included in bindings found.
	  (dolist (entry *sg-entries*)
	    (dolist (binding (unify (second (systementry-prop entry))
				    (exptype-form rule)))
	      (unless (or
		       ;; if there are no variables, unify returns no-bindings
		       (equal binding (car no-bindings))
		       (member (cdr binding)
			       (cdr (assoc (car binding) 
					   *ontology-bindings*))
			       :test #'unify)) 
		(format t "    unmatched binding ~S for ~A~%      rule ~A~%      list ~S~%" 
			binding (exptype-type rule) (exptype-form rule) 
			*ontology-bindings*))))

	  )))
    sum))

(defun try-ontology-bodies (problem)
  "Test that all bodies in the problem solution are included in (problem-bodies-and-compounds ...)."
  (let ((bodies (problem-bodies-and-compounds problem)))
    (dolist (entry *sg-entries*)
      (when (eql (car (systemEntry-prop entry)) 'body)
	(unless (member (second (systementry-prop entry))
			bodies
			:test #'unify)
	  (format t "    unmatched body ~S in ~S~%" 
		  (second (systementry-prop entry)) bodies))))))

;;; (progn (rhelp) (andes-init) (read-problem-info 'kt1a) (setf *phrase-cache* nil))
;;; (generate-all-words '(force)) 
;;;  (progn (setf *result* (generate-all-words)) nil)

;;; Right now, caching involving variable is not done intelligently.
;;; So word suggestion will break if the student modifies two
;;; objects so that their names are swapped.

(defvar *phrase-cache*)

(defun get-cached-phrases (type)
  (or (assoc type *phrase-cache*)
      (let ((this (cons type nil)))
	(push this *phrase-cache*)
	this)))

(defun get-cached-triples (type words)
  (cdr (assoc words (cdr (get-cached-phrases type))
	      :test #'equalp)))

(defun cached-triples-p (type words)
  (assoc words (cdr (get-cached-phrases type))
	 :test #'equalp))

(defun to-word-list (text)
  ;; canonicalize words
  (mapcar #'string-downcase (match:word-parse text)))

(defun triples-to-distinct-words (triples)
  "Get distinct words from a list of triples, substituting in variables."
  (remove-duplicates 
   (remove nil (mapcar #'expand-vars
		       (mapcar #'car triples)))
	   :test #'string-equal))

(defun next-word-list (words &key type)
  "Assumes list of words.  Substitute in for any vars."
  ;; the argument word should be given the result of (to-word-list ...)
  (triples-to-distinct-words
   (cond 
     ;; If already cached, go with that.
     ((cached-triples-p type words)
      (get-cached-triples type words))
     
     ;; Generate initial word set, add to cache and return.
     ((null words)
      (let  ((x (generate-initial-words type)))
	(push (cons words x) (cdr (get-cached-phrases type)))
	x))
     
     (t 
      ;; if not in cache, first see if previous words
      ;; are in cache, if they are not, generate them.
      (unless (cached-triples-p type (butlast words))
	(next-word-list (butlast words) :type type))
      
      ;; Now, use result to generate next set of words. 
      (let (triples (word (car (last words))))
	(dolist (triple (get-cached-triples type (butlast words))) 
	  ;; Evaluate any vars against student 
	  ;; string here.
	  (when (string-equal (expand-vars (car triple)) word)
	    (setf triples
		  (append triples (get-next-words triple)))))
	;; Cache result
	(push (cons words triples) (cdr (get-cached-phrases type)))
	triples)))))

(defun get-next-words (triple)
  (mapcar #'(lambda (x) (list (car x) (second triple) (cdr x)))
	  (get-first-model-words (third triple) nil)))

(defun get-word-list-props (text &key type)
  "Return list of quantity propositions associated wtih an exact match to phrase.  Any final vars are expanded here."
  (let ((words (to-word-list text)) result)
    ;; If not in cache, add to cache.
    (unless (cached-triples-p type (butlast words))
      (next-word-list (butlast words) :type type))
    ;; Go through list of triples and find matches to words.
    (dolist (triple (get-cached-triples type (butlast words)))
      (when (and (string-equal (car (last words)) 
			       (expand-vars (car triple)))
		 (member nil (get-first-model-words (third triple) nil)))
	(pushnew (second triple) result :test #'unify)))
    result))


(defun generate-initial-words (type &optional names)
  (let (result)
    (cond 
      ((or (eql type 'vector) (eql type 'scalar))
       (dolist (rule *Ontology-ExpTypes*)
	 (when (and (eql (exptype-rank rule) type)
		    (not (member (exptype-type rule) *disallowed-quantities*))
		    (or (null names) (member (exptype-type rule) names)))
	   (let (*ontology-bindings*) 
	     (get-ontology-bindings (ExpType-new-english rule))
	     (setf result 
		   (append result
			   (expand-with-bindings *ontology-bindings* nil 
						 (exptype-form rule))))))))
      
      ((eql type 'body)
       (dolist (prop (problem-bodies-and-compounds *cp*))
	 (setf result (append result (prop-to-triples prop)))))
      
      (t (warn "generate-initial-words invalid type ~A" type)))
    result))

	
(defun expand-with-bindings (binding-sets bindings prop)
  "Returns a list of triples (<string> <prop> <model>)."
  (if binding-sets
      (loop for binding in (cdr (car binding-sets))
	    append (expand-with-bindings (cdr binding-sets)
					 (cons (cons (car (car binding-sets))
						     binding)
					       bindings)
					 prop))
      (let ((bound-prop (subst-bindings bindings prop)))
	;; (format t "starting prop ~A~%" bound-prop)
	(prop-to-triples bound-prop))))

(defun prop-to-triples (prop)
  (mapcar #'(lambda (x) (list (car x) prop (cdr x)))
	  (get-first-model-words (new-english-find prop) nil)))


(defun merge-with-ontology-bindings (variable possibilities)
  (let ((known (find variable *ontology-bindings* :key #'car)))
    (cond (known
	   (setf (cdr known) (union (cdr known)
				    possibilities :test #'unify)))
	  ;; The case where there are no possibilities should be
	  ;; understood to mean that the associated quantity 
	  ;; is not relevant to this problem.
	  ((null possibilities)
	   (push (list variable) *ontology-bindings*))
	  ;; This allows some non-variables to pollute *ontology-bindings*
	  ((some #'(lambda (x) (unify variable x)) possibilities)
	   (push (cons variable possibilities) *ontology-bindings*))
	  (t
	   (warn "merge-with-ontology-bindings: ~S inconsistent~%      with ~S~%" 
		 variable possibilities)))))

(defun get-ontology-bindings (model &optional (bindings no-bindings))
  "Descend through model tree, collecting list of possible bindings."
  (cond ((stringp model))
	((null model))
	((variable-p model)
	 (if (variable-boundp model bindings)
	     (get-ontology-bindings (subst-bindings bindings model))
	     ;; Assume all "bare" variables are bound to atoms
	     (merge-with-ontology-bindings model (problem-atoms *cp*))))
	((and (consp model) (member (car model) 
				    '(preferred allowed or and conjoin)))
	 (get-list-ontology-bindings (cdr model) bindings))
	;; ordered sequence
	((match:test-for-list model)
	 (get-list-ontology-bindings model bindings))
	;; expansion of var must be done at run-time.
	((and (consp model) (eql (car model) 'var))) 
	((and (consp model) (eql (car model) 'eval))
	 ;; Can't actually evaluate the eval, since
	 ;; we cannot bind all the variables.
	 ;;
	 ;; subsequent arguments of (eval ...) are to to be cons'es
	 ;; of a variable and an expression that can be eval'ed to a list
	 ;; of possible values for that variable.  For instance:
	 ;;     (eval .... (?a . (problem-atoms *cp*)) 
	 ;;                (?t . '((time 0) (time 1))))
	 ;;
	 (dolist (pair (cddr model))
	   ;; If variable has been bound by parent, defer
	   ;; to parent bindings.
	   (when (variable-p (subst-bindings bindings (car pair)))
	     (merge-with-ontology-bindings 
	      (subst-bindings bindings (car pair))
	      (eval (subst-bindings-quoted bindings (cdr pair)))))))
	(t 
	 (ontology-bindings-find model bindings))))

(defun get-list-ontology-bindings (model &optional (bindings no-bindings))
  (cond
    ((null model) nil)
    ((variable-p model)
     (if (variable-boundp model bindings)
	 (get-list-ontology-bindings (subst-bindings bindings model))
	 (merge-with-ontology-bindings 
	  model 
	  (generate-subsets (problem-atoms *cp*)))))
    ((consp model)
     (get-ontology-bindings (car model) bindings)
     (get-list-ontology-bindings (cdr model) bindings))
    (t (warn "get-list-ontology-bindings unexpected ~A" model))))


(defun ontology-bindings-find (prop &optional (bindings no-bindings))

  ;; First, run through general Ontology to find match.
  ;; The problem-specific ontology gives cases special to 
  ;; the solution of the problem and we are looking for
  ;; a larger set of possibilities.
  (dolist (ruler *Ontology-ExpTypes*)
    ;; Bindings are local to one operator in the ontology.
    (let* ((rule (rename-variables (cons (Exptype-form ruler)
					 (ExpType-new-english ruler))))
	   (bindings (unify (car rule) prop bindings)))
      (when bindings 
	;; (format t "  general ontology for ~A ~A~%    bindings=~A~%    parent=~A~%" prop (Exptype-form ruler) bindings *ontology-bindings*)
	(get-ontology-bindings (cdr rule) bindings)
	;; (format t "    this=~A~%" *ontology-bindings*)
	(return-from ontology-bindings-find))))

  ;; Then, we do problem-specific ontology.
  (dolist (ruler (problem-english *cp*))
    (let* ((rule (rename-variables ruler))
	   (bindings (unify (car rule) prop bindings)))
      (when bindings
	;; (format t "  problem ontology for ~A ~A~%    bindings=~A~%    parent=~A~%" prop (car rule) bindings *ontology-bindings*)
	(get-ontology-bindings (cdr rule) bindings)
	;; (format t "    this=~A~%" *ontology-bindings*)
	(return-from ontology-bindings-find))))
  
  ;; If it is a symbol, test that it exists in problem-atoms.
  (when (atom prop)
    (unless (member prop (problem-atoms *cp*))
      (warn "Unmatched prop ~A" prop))
    (return-from ontology-bindings-find))

  ;; On failure, warn and return nil
  (warn "ontology-bindings-find:  no ontology match for ~S" prop))


;; Previously, tried to generate a tree that could
;; be used for multiple quantities.  In that case, one cannot have 
;; multiple pointers to a given subtree without considering which
;; quantities allow those pointer.  Generating a tree without multiple
;; pointers to a subtree causes the tree to become too large and 
;; lisp crashes.

(defmacro dolist-union (vars &rest exprs)
  "Like dolist, except that results are accumulated via union."
  (let ((result (gensym)))
    `(let (,result)
      (dolist ,vars
	(setf ,result (union ,result (progn ,@exprs))))
      ,result)))

(defun get-first-model-words (model more-model)
  "Get possible first words of a model, returning an alist of word-model pairs., where the model represents possible remaining words and word can be either a string or a (var ...)."
  ;; Assume all ontology and evals have been expanded.
  ;;
  ;; Since the model can contain nil's, we need to keep track
  ;; of the remaining parts of the model until we have identified
  ;; an initial word.  Thus, we have to construct the model containing
  ;; the remaining possible words anyway.
  (cond ((or (stringp model)
	     (and (consp model) (eql (car model) 'var)))
	 (list (cons model more-model)))
	((null model) 
	 (if more-model 
	     (get-first-model-words more-model nil)
	     '(nil)))
	((and (consp model) (member (car model) '(preferred allowed)))
	 (union 
	  (get-first-model-words more-model nil)
	 (get-first-model-words (cdr model) more-model)))
	((and (consp model) (eql (car model) 'or))
	 (if (cdr model)
	     (dolist-union (x (cdr model))
			   (get-first-model-words x more-model))
	     (get-first-model-words more-model nil)))
	((and (consp model) (eql (car model) 'and))
	 (if (cdr model)
	     (dolist-union 
	      (x (cdr model))
	      (get-first-model-words x (list (remove x model) more-model)))
	     (get-first-model-words more-model nil)))
	((and (consp model) (eql (car model) 'conjoin))
	 (cond ((cddddr model)
		(dolist-union 
		 (x (cddr model))
		 (get-first-model-words x (list (remove x model) more-model))))
	       ((cdddr model)
		(union
		 (get-first-model-words (third model) 
				  (list (second model) (fourth model) 
					more-model))
		 (get-first-model-words (fourth model) 
				  (list (second model) (third model) 
					more-model))))
	       ((cddr model)
		(get-first-model-words (third model) more-model))
	       (t 
		(get-first-model-words more-model nil))))
	((match:test-for-list model)
	 (get-first-model-words (car model)
			       (if (and (cdr model) more-model)
				   (list (cdr model) more-model)
				   (or (cdr model) more-model))))
	(t (warn "get-first-model-words invalid model ~A" model))))
