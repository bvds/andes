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

;; Student never defines a vector component, magnitude, or direction directly
;; angle-between is just too hard.
(defparameter *disallowed-quantities* '(mag dir compo angle-between))
  
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
    (dolist (type types)
       (with-ontology-exptypes rule
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
	((and (consp model) 
	      (member (car model) '(preferred allowed key case-sensitive 
				    case-insensitive)))
	 (get-ontology-bindings (second model) bindings))
	((and (consp model) (member (car model) '(or and conjoin)))
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
  (with-ontology-exptypes ruler
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
;;;;            Generate list of keywords, pointing to propositions
;;;;
;;;;
(defvar *quant-keywords*)
(defvar *quant-required-words*)

(defun generate-types-keyword-pointers (phrase-types)
  (when t
    (mapcar #'(lambda (x) (cons (car x) (generate-keyword-pointers (cdr x))))
	    phrase-types)))

(defun generate-keyword-pointers (quants)
  (let (keywords required-words)
    (dolist (quant quants)
      ;; (format t "starting quant ~A~%" (car quant))
      (let ((rule (lookup-exptype-struct (car quant)))
	    *quant-keywords*
	    *quant-required-words*)
	(keyword-expand-with-bindings (cddr quant) no-bindings rule)
	(dolist (word *quant-keywords*)
	  (unless (assoc word keywords :test #'equal) 
	    (push (list word) keywords))
	  (push (car quant) (cdr (assoc word keywords :test #'equal))))
	(dolist (word *quant-required-words*)
	  (unless (assoc word required-words :test #'equal) 
	    (push (list word) required-words))
	  (push (car quant) (cdr (assoc word required-words :test #'equal))))))
    (cons keywords required-words)))

;; this is a list of the most common words in the English language
;; http://en.wikipedia.org/wiki/Most_common_words_in_English
(defparameter *common-English-words* 
  '("the" "be" "to" "of" "and" "a" "in" "that" "have" "I" "it"
    "for" "not" "on" "with" "he" "as" "you" "do" "at" "this" "but"
    "his" "by" "from" "they" "we" "say" "her" "she" "or" "an"
    "will" "my" "one" "all" "would" "there" "their" "what" "so"
    "up" "out" "if" "about" "who" "get" "which" "go" "me"))
(defparameter *excluded-words* (append *common-English-words* '("&")))
     
(defun keyword-expand-with-bindings (binding-sets bindings rule)
  (if binding-sets
      ;; Expand binding possibilities, building binding list.
      (dolist (binding (cdr (car binding-sets)))
	(keyword-expand-with-bindings
	 (cdr binding-sets)
	 (cons (cons (car (car binding-sets)) binding)
	       bindings)
	 rule))
      ;; canonicalize case so we can use string-downcase
      (let* ((model (expand-new-english 
		     (exptype-new-english rule) bindings))
	     (keywords (mapcar #'string-downcase (pull-out-keywords model)))
	     (required-words (mapcar #'string-downcase 
				    (pull-out-required-words model))))
	
	;; Remove common words from list.
	(setf required-words 
	      (set-difference required-words *excluded-words* :test #'equal))

	(cond (keywords
	       (setf *quant-keywords*
		     (union *quant-keywords* keywords :test #'equal)))
	      (required-words
	       (setf *quant-required-words*
		     (union *quant-required-words* required-words 
			    :test #'equal)))
	      (t (error "No possible word match for ~A" rule))))))
  