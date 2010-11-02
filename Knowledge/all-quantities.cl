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

(defstruct (triple (:type list))
  word
  prop
  model)


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
	      :test #'equal)))

(defun cached-triples-p (type words)
  (assoc words (cdr (get-cached-phrases type))
	 :test #'equal))

(defun to-word-list (text)
  ;; canonicalize words
  (mapcar #'string-downcase (match:word-parse text)))

  
(defun triples-to-distinct-words (triples)
  "Get distinct words from a list of triples, substituting in variables.  nil indicates possibility of end of definition.  Drop any (var ...) that doesn't have a match."
  ;; Maintain the order of the list, retaining the first 
  ;; instance of any duplicate.
  (let (result)
    (dolist (word (mapcar #'triple-word triples))
      (if (or (null word) (stringp word))
	  (pushnew word result :test #'equal)
	  (let ((x (expand-vars word)))
	    (when x (pushnew x result :test #'equal)))))
    (reverse result)))

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
	  (when (string-equal (expand-vars (triple-word triple)) word)
	    
	    ;; Test of bad termination of phrase
	    (when (and nil ;turn test on here
		       (stringp word) 
		       (member word '("of" "the" "and" "or" "on") 
			       :test #'string-equal)
		       (some #'(lambda (x) (null (triple-word x))) 
			     (get-next-words triple)))
	      (warn "next-word-list:  bad termination \"~A\" for ~S~%"
		    word (triple-prop triple)))
	      
	      (setf triples 
		    (append triples (get-next-words triple)))))
	
	;; Test for word repeats in result
	(when nil ;turn test on
	  (dolist (triple triples)
	    ;; could be either string or (var ...)
	    (when (if (and (consp (triple-word triple)) (consp word))
		      (unify (triple-word triple) word)
		      (equal (triple-word triple) word))
	      (warn "Repitition of \"~A\" for ~A, model:~%    ~A" 
		    word (triple-prop triple)
		    (new-english-find (triple-prop triple))))))
	
	;; Cache result
	(push (cons words triples) (cdr (get-cached-phrases type)))
	
	;; return list
	triples)))))

(defun get-next-words (triple)
  (mapcar #'(lambda (x) (make-triple :word (car x) :prop (triple-prop triple)
				     :model (cdr x)))
	  (get-first-model-words (triple-model triple) nil)))

(defun get-word-list-props (words &key type)
  "Return list of quantity propositions associated wtih an exact match to phrase.  Any final vars are expanded here."
  (let (result)
    ;; If not in cache, add to cache.
    (unless (cached-triples-p type (butlast words))
      (next-word-list (butlast words) :type type))
    ;; Go through list of triples and find matches to words.
    (dolist (triple (get-cached-triples type (butlast words)))
      (when (and (string-equal (car (last words)) 
			       (expand-vars (triple-word triple)))
		 (member nil (get-first-model-words 
			      (triple-model triple) nil)))
	(pushnew (triple-prop triple) result :test #'unify)))
    result))


(defun generate-initial-words (type &optional names)
  (declare (notinline get-ontology-bindings))
  ;; This list could be pre-computed
  (let (result)
    (cond
      ((assoc type (problem-phrases *cp*))
       (dolist (this (cdr (assoc type (problem-phrases *cp*))))
	 (let ((rule (lookup-exptype-struct (car this))))
	   ;; sanity test
	   (unless (equal (second this) (exptype-form rule))
	     (warn "generate-initial-words:  cached variables for ~A do not match current ontology." 
		   (car this)))
	   (setf result 
		 (append result
			 (expand-with-bindings (cddr this) nil rule)))
	   )))
	   
      ((member type '(vector scalar))
       (warn "generate-initial-words:  No cached quantities for problem ~A"
	     (problem-name *cp*))
       (dolist (rule *Ontology-ExpTypes*)
	 (when (and (eql (exptype-rank rule) type)
		    (not (member (exptype-type rule) *disallowed-quantities*))
		    (or (null names) (member (exptype-type rule) names)))
	   (let (*ontology-bindings*) 
	     (get-ontology-bindings (ExpType-new-english rule))
	     (setf result 
		   (append result
			   (expand-with-bindings *ontology-bindings* 
						 nil rule)))))))
      
      ((eql type 'body)
       (dolist (prop (problem-bodies-and-compounds *cp*))
	 (setf result (append result (prop-to-triples prop prop)))))
      
      (t (warn "generate-initial-words invalid type ~A" type)))
    
    ;; Sort so that solution quantity propositions appear first.
    ;; This is giving some information to the students,
    ;; so we need to determine whether this is good pedagogy.
    (let ((relevant (mapcar #'(lambda (x) (second (systementry-prop x)))
			    *sg-entries*))
	  in out)
      (dolist (triple result)
	(if (member (triple-prop triple) relevant :test #'unify)
	    (push triple in)
	    (push triple out)))
      (append (reverse in) (reverse out)))))

	
(defun expand-with-bindings (binding-sets bindings rule)
  "Returns a list of triples (<string> <prop> <model>)."
  (if binding-sets
      ;; Expand binding possibilities, building binding list.
      (loop for binding in (cdr (car binding-sets))
	    append (expand-with-bindings (cdr binding-sets)
					 (cons (cons (car (car binding-sets))
						     binding)
					       bindings)
					 rule))
      (prop-to-triples (subst-bindings bindings (exptype-form rule))
		       (subst-bindings bindings (exptype-new-english rule)))))

(defun prop-to-triples (prop &optional model)
  ;; Apply bindings to Ontology proposition.
  (mapcar #'(lambda (x) (make-triple :word (car x) :prop prop :model (cdr x)))
	  (if model
	      (get-first-model-words model nil)
	      (get-first-model-words-find prop nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;    Generate possible quantities associated with a given problem.
;;;;    The result is a list of ontology rules together with
;;;;    a set of possible bindings associated with each rule          
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-initial-bindings (&optional names)
  "Return an object containing ontology rules and associated bindings for possible quantitites associated with a problem."
  (let* ((types '(vector scalar))
	 (result (mapcar #'list types)))
    (dolist (type '(vector scalar))
       (dolist (rule *Ontology-ExpTypes*)
	 (when (and (eql (exptype-rank rule) type)
		    (not (member (exptype-type rule) *disallowed-quantities*))
		    (or (null names) (member (exptype-type rule) names)))
	   (let (*ontology-bindings*) 
	     (get-ontology-bindings (ExpType-new-english rule))
	     (push (list* (exptype-type rule) (exptype-form rule) 
			  *ontology-bindings*) 
		   (cdr (assoc type result)))))))
    result))

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
	((and (consp model) (member (car model) '(eval eval-compiled)))
	 ;; Can't actually evaluate the eval, since
	 ;; we cannot bind all the variables.
	 ;;
	 ;; subsequent arguments of (eval ...) are to to be cons'es
	 ;; of a variable and an expression that can be eval'ed to a list
	 ;; of possible values for that variable.  For instance:
	 ;;     (eval .... (?a . (problem-atoms *cp*)) 
	 ;;                (?t . '((time 0) (time 1))))
	 ;;
	 (dolist (pair (if (eq (car model) 'eval) (cddr model) (cdddr model)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;     Find the next word in a New-English model.          
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (cond ((stringp model)
	 ;; If there is more than one word, break up into list of words.
	 (let ((this (match:word-parse model)))
	   (if (cdr this)
	       (get-first-model-words this more-model)
	       (list (cons model more-model)))))
	((and (consp model) (eql (car model) 'var))
	 (list (cons model more-model)))
	((null model) 
	 (if more-model 
	     (get-first-model-words more-model nil)
	     (list nil)))
	((variable-p model)
	 (warn "expand-new-english:  Unbound variable ~A" model) (list nil))
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

	((and (consp model) (eql (car model) 'eval-compiled))
	 ;; For extensibility, allow eval to have more arguments.
	 ;; Here, we just ignore them.
	 (get-first-model-words (apply (second model) (third model)) more-model))
	
	((and (consp model) (eql (car model) 'eval))
	 ;; For extensibility, allow eval to have more arguments.
	 ;; Here, we just ignore them.
	 (get-first-model-words (eval (second model)) more-model))

	((match:test-for-list model)
	 (get-first-model-words (car model)
			       (if (and (cdr model) more-model)
				   (list (cdr model) more-model)
				   (or (cdr model) more-model))))

	(t 
	 ;; Bindings are local to one operator in the ontology
	 ;; so we need to substitute in here.
	 ;; Assume any recursive calls are covered by New-English.
	 (get-first-model-words-find model more-model))))

(defun get-first-model-words-find (prop more-model)
  "Match proposition to Ontology."
  ;; First, determine if there is any problem-specific
  ;; Ontology match.
  (dolist (rule (problem-english *cp*))
    (let ((bindings (unify (car rule) prop)))
      (when bindings 
	(return-from get-first-model-words-find
	  (get-first-model-words (subst-bindings-careful bindings (cdr rule)) 
				 more-model)))))

  ;; Then run through general Ontology to find match.
  (dolist (rule *Ontology-ExpTypes*)
    (let ((bindings (unify (Exptype-form rule) prop)))
      (when bindings 
	(return-from get-first-model-words-find
	 (get-first-model-words 
	  (subst-bindings-careful bindings (ExpType-new-english rule))
				more-model)))))

  ;; If it is a symbol, use improved version of def-np.
  (when (atom prop)
    (return-from get-first-model-words-find
      (get-first-model-words (def-np-model prop) more-model)))
  
  ;; On failure, warn and return nil
  (warn "get-first-model-words-find:  no ontology match for ~S" prop))
