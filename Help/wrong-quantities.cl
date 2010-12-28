;; wrong-quantities.cl -- Match to quantities not in problem solutions.
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

;; Acts as a cache of expanded models for various quantities.
(defvar *wrong-quantities*)

(defun get-tool-type-quantity (tool-type)
  (unless (assoc tool-type *wrong-quantities*)
    (push (list tool-type) *wrong-quantities*))
      (assoc tool-type *wrong-quantities*))

(defun quantity-model-p (tool-type ontology-type)
  (assoc ontology-type (cdr (get-tool-type-quantity tool-type))))

(defun get-quantity-models (tool-type ontology-type)
  (cdr (assoc ontology-type (cdr (get-tool-type-quantity tool-type)))))

(defun set-quantity-models (tool-type ontology-type models)
  (push (cons ontology-type models) 
	(cdr (get-tool-type-quantity tool-type))))

(defparameter *tools-with-definitions*
 '((define-var . scalar)
    (body . body)
    (vector . vector)
    (draw-line . line))
  "Tools that have natural language definitions associated with them.")

(defparameter *tool-props-with-definitions*
  (mapcar #'car *tools-with-definitions*))

;; top-level routine.  
(defun get-wrong-quantities (tool-prop ontology-type)
  "Return alist of model-quantity pairs."
    (if (quantity-model-p tool-prop ontology-type)
	;; If already cached, go with that.
	(get-quantity-models tool-prop ontology-type)
	;; Generate initial word set, add to cache and return.
	(let  ((x (generate-wrong-quantities tool-prop ontology-type)))
	  (set-quantity-models tool-prop ontology-type x)
	  (when nil ;debug print
	    (format t "get-wrong-quantities ~A for ~A got ~A~%" 
		    tool-prop ontology-type (length x)))
	  x)))

(defvar *wrong-quantity-result*)

(defun generate-wrong-quantities (tool-prop ontology-type)
  ;; This function has lots of code copied from all-quantities.cl
  (declare (notinline get-ontology-bindings))
  ;; This list could be pre-computed
  (let* ((type (cdr (assoc tool-prop *tools-with-definitions*)))
	 ;; Quantities of the same tool in the solution.
	 (relevant (mapcar #'(lambda (prop) (canonicalize-unify (second prop)))
			   (remove tool-prop
				   (mapcar #'systementry-prop *sg-entries*)
				   :key #'car :test-not #'eql)))
	 *wrong-quantity-result*)
    (cond
      ((and ontology-type
	    (member ontology-type (cdr (assoc type (problem-phrases *cp*)))
		    :key #'car))
       (let ((this (find ontology-type 
			  (cdr (assoc type (problem-phrases *cp*)))
			  :key #'car))
	      (rule (lookup-exptype-struct ontology-type)))
	   ;; sanity test
	   (unless (equal (second this) (exptype-form rule))
	     (warn "generate-wrong-quantities:  cached variables for ~A do not match current ontology." 
		   (car this)))
	   (expand-with-bindings-w (cddr this) no-bindings rule)))
      
      ((member type '(body line))
       (dolist (prop (problem-bodies-and-compounds *cp*))
	 (push (cons (new-english-find prop) prop) 
	       *wrong-quantity-result*)))
     
      (t (warn "generate-wrong-quantities invalid (or uncached) type ~A" 
	       type)))
    
    (when nil ;debug print
      (format t "got ~A of type ~A~% relevant: ~A~%" 
	      (length *wrong-quantity-result*) type relevant))
    ;; canonicalize-unify has been applied to both prop and relevant.
    (nreverse
     (delete-if #'(lambda (prop) (member prop relevant :test #'equal))
	       *wrong-quantity-result* :key #'cdr))))

(defun expand-with-bindings-w (binding-sets bindings rule)
  "Returns (<model> . <prop>) pair."
  (if binding-sets
      ;; Expand binding possibilities, building binding list.
      (dolist (binding (cdr (car binding-sets)))
	(expand-with-bindings-w 
	 (cdr binding-sets)
	 (cons (cons (car (car binding-sets)) binding)
	       bindings)
	 rule))
      ;; This allows possible repeats but if faster than checking.
      (push 
       (cons (expand-new-english (exptype-new-english rule) bindings)
	     (canonicalize-unify  ;allow equal for compare.
	      (subst-bindings bindings (exptype-form rule))))
       *wrong-quantity-result*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           Lookup keywords
;;
(defun lookup-quantity-keyword-props (student tool-prop)
  (let* ((type (cdr (assoc tool-prop *tools-with-definitions*)))
	 ;; this will be null for body tool
	 (quantities (cdr (assoc type (problem-keywords *cp*)))))
    ;; (format t "tool prop ~A~%" tool-prop)
    (cond 
      (quantities
       (let ((ontology-types 
	      (or 
	       ;; First try to see if student matches any keywords.
	       (lookup-words-in-quantities (mapcar #'string-downcase student)
					   (car quantities))
	       ;; Else try required words.
	       (lookup-words-in-quantities (mapcar #'string-downcase student)
					   (cdr quantities)))))
	 ;; (format t "   using ontology types ~A~%" ontology-types)
	 (loop for ontology-type in ontology-types
	       append (get-wrong-quantities tool-prop ontology-type))))
            
      ((member type '(body line)) 
       (get-wrong-quantities tool-prop nil))

      (t 
       (warn "lookup-quantity-keyword-props invalid (or uncached) type ~A" 
	       type)))))

(defun lookup-words-in-quantities (words quantities)
  ;; Take all shortest but non-empty matches.
  (let (results best)
    (dolist (word words)
      (let ((this (cdr (assoc word quantities :test #'equal))))
	(when (and this (or (null best) (< (length this) best)))
	  (setf best (length this))
	  (push this results))))

    ;; Merge any ties.
    (remove-duplicates 
     (apply #'append 
	    (reverse ;push reverses order of results.
	     ;; Remove any collected lists of quantities
	     ;; that are larger than the shorted non-trivial list
	     (remove-if #'(lambda (x) (> (length x) best)) results))))))
