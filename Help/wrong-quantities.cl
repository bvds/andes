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
;; This function provides caching for the matches to ontology type.
;; This is largely superfluous because generate-wrong-quantities
;; simply looks up the pre-computed bindings in problem-phrases.
(defun get-wrong-quantities (tool-prop ontology-type)
  "Return a list of triples, with the model, the Ontology prop and any bindings."
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
  (let ((type (cdr (assoc tool-prop *tools-with-definitions*)))
	;; Quantities of the same tool in the solution.
	(relevant (mapcar #'(lambda (prop) (canonicalize-unify (second prop)))
			  (remove tool-prop
				  (mapcar #'systementry-prop *sg-entries*)
				  :key #'car :test-not #'eql))))
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
	 (list (list* (exptype-new-english rule) (exptype-form rule) 
		      (cddr this)))))
      
      ;; ignore ontology type
      ((member type '(body line))
       (delete-if 
	;; Remove any that are in the solution
	#'(lambda (x) (member (cadr x) relevant :test #'equal))
	(loop for prop in (problem-bodies-and-compounds *cp*)
	   ;; there are no bindings
	   collect (list (new-english-find prop) 
			 (canonicalize-unify prop)))))
      
      (t (warn "generate-wrong-quantities invalid (or uncached) type ~A" 
	       type)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;           Lookup keywords
;;
(defun lookup-quantity-keyword-props (student tool-prop)
  (let* ((type (cdr (assoc tool-prop *tools-with-definitions*)))
	 ;; this will be null for body tool
	 (quantities (cdr (assoc type (problem-keywords *cp*)))))
    (when nil ;debug print
      (format t "tool prop ~A~%" tool-prop))
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
	 (when nil ;debug print
	   (format t "   using ontology types ~A~%" ontology-types))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;   Functions for extending match-model to dynamically
;;;;   assign bindings.
;;;;

(defun extend-match-model (student model best)
  "Extend match-model to handle unbound variables."
  ;; Substitute in any variables in *iteration-bindings*,
  ;; iterate ovar any remaining variables using *ontology-bindings*,
  ;; and expand.
  ;; After match is done, append bindings used.
  (let ((best-result (match:make-best :value best)))
    (dolist (it-bindings (iterate-over-bindings model))
      (let* ((all-bindings (append it-bindings match:*iteration-bindings*))
	     (this
	      (match:match-model
	       student
	       (expand-vars
		(expand-new-english model all-bindings))
	       :best (match:best-value best-result))))
	;; This will include some repeats of *iteration-bindings*
	;; final bindings list.
	(setf (match:best-extra this) all-bindings)
	(when (< (match:best-value this) (match:best-value best-result))
	  (setf best-result this))))
    best-result))

(defun extend-word-count (model max)
  "Extend match-model to handle unbound variables."
  ;; Substitute in any variables in *iteration-bindings*,
  ;; iterate ovar any remaining variables using *ontology-bindings*,
  ;; and expand.
  ;; After match is done, append bindings used.
  (let (best)
    (dolist (it-bindings (iterate-over-bindings model))
      (let* ((all-bindings (append it-bindings match:*iteration-bindings*))
	     (this
	      (match:word-count
	       (expand-vars
		(expand-new-english model all-bindings))
	       :max max)))
	(when (or (null best) (if max (> this best) (< this best))) 
	  (setf best this))))
    
    ;; Note that eval cannot have local unbound variables, since
    ;; these are collected by variables-in below.
    ;; This can cause an error.
    (unless best (error 'iterate-over-bindings
			:tag (list model (iterate-over-bindings model))
			:text "extend-word-count not setting best"))
    best))

(defun iterate-over-bindings (model)
  (iterate-over-vars 
   (set-difference 
    (variables-in model)
    (mapcar #'car match:*iteration-bindings*))))
    
(defun iterate-over-repeated-variables (models)
  "Take a list of models, find any variables that are common to more than one member, and return a list of possible bindings."
  (let* ((vars (apply #'nconc (mapcar #'variables-in models)))
	 (repeats (remove-if #'(lambda (var) (eql (count var vars) 1))
			     (remove-duplicates vars))))
    (iterate-over-vars repeats)))

(defun iterate-over-vars (vars &optional bindings)
  (if vars
      (loop for value in (cdr (assoc (car vars) *ontology-bindings*))
	   append (iterate-over-vars (cdr vars) 
				    (cons (cons (car vars) value) bindings)))
      ;; no-bindings is case where original vars is empty
      ;; still want to do one iteration
      (list (or bindings no-bindings))))

;; Top level routine.
(defun best-wrong-match (student tool-prop &key cutoff)
  "Find one or more best matches below or equal to cutoff to ontology.  Returns either nil or a list containing some matches.  The prop is set to the matched prop."
  (let ((best cutoff)
	(epsilon 0.01)
	result)
    (dolist (m-f-b (lookup-quantity-keyword-props student tool-prop))
      (let (match:*iteration-bindings* ;empty at top level
	    (match:iteration-function 'iterate-over-repeated-variables)
	    (match:word-count-handler 'extend-word-count)
	    (match:unknown-object-handler 'extend-match-model)
	    (*ontology-bindings* (cddr m-f-b))
	    ;; handle floating pointing point ties
	    (bound (+ best epsilon)))
	(let ((this (match:match-model student (car m-f-b) :best bound)))
	  (if (match:best-p this)
	      (when (< (match:best-value this) bound)
		(when (< (match:best-value this) best)
		  (setf best (match:best-value this)))
		;; Set proposition using bindings collected during match.
		(setf (match:best-prop this)
		      ;; If there are no bindings to match, best-extra
		      ;; is nil.  In cases where there are no bindings
		      ;; and there is a single ontology expand,
		      ;; extend-match-model may never be called.
		      (subst-bindings (or (match:best-extra this) no-bindings)
				      (list tool-prop (cadr m-f-b))))
		;; Matching to the model may not fully bind 
		;; the proposition (in the case of optional sub-models).
		;; In this case, we iterate through any unbound variables.
		(setf result (make-fully-bound-props this))
		(when nil ;debug print
		  (format t "Got match of ~A for props:~%  ~A~%"
			  (match:best-value (car result))
			  (mapcar #'match:best-prop result))))
	      ;; This indicates an error in match-model.
	      (warn "Match-model returned ~A for ~A" this m-f-b)))))
    result))

(defun make-fully-bound-props (this)
  "Iterate through any unbound variables in prop, creating copies of match result."
  (if (groundp (match:best-prop this))
      (list this)
      (let ((partially-bound-prop (match:best-prop this))
	    results)
	(dolist (bindings (iterate-over-vars 
			   (variables-in partially-bound-prop)))
	  (setf (match:best-prop this)
		(subst-bindings bindings partially-bound-prop))
	  (push (match:copy-best this) results))
	results)))
