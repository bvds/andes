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


(defvar *wrong-quantities*)

(defparameter *tools-with-definitions*
 '((define-var . scalar)
    (body . body)
    (vector . vector)
    (draw-line . line))
  "Tools that have natural language definitions associated with them.")

(defparameter *tool-props-with-definitions*
  (mapcar #'car *tools-with-definitions*))

;; top-level routine.  
(defun get-wrong-quantities (tool-prop)
  "Return alist of model-quantity pairs."
    (if (assoc tool-prop *wrong-quantities*)
	;; If already cached, go with that.
	(cdr (assoc tool-prop *wrong-quantities*))
	;; Generate initial word set, add to cache and return.
	(let  ((x (generate-wrong-quantities tool-prop)))
	  (push (cons tool-prop x) *wrong-quantities*)
	  x)))

(defmacro union-with (result set)
  "Merge set with result using untion."
  `(setf ,result (union ,result ,set :key #'cdr :test #'unify)))

(defun generate-wrong-quantities (tool-prop &optional names)
  ;; This function has lots of code copied from all-quantities.cl
  (declare (notinline get-ontology-bindings))
  ;; This list could be pre-computed
  (let ((type (cdr (assoc tool-prop *tools-with-definitions*)))
	result)
    (cond
      ((assoc type (problem-phrases *cp*))
       (dolist (this (cdr (assoc type (problem-phrases *cp*))))
	 (let ((rule (lookup-exptype-struct (car this))))
	   ;; sanity test
	   (unless (equal (second this) (exptype-form rule))
	     (warn "generate-initial-words:  cached variables for ~A do not match current ontology." 
		   (car this)))
	   (union-with result
		       (expand-with-bindings-w (cddr this) nil rule)))))
	   
      ((member type '(vector scalar))
       (warn "generate-initial-words:  No cached quantities for problem ~A"
	     (problem-name *cp*))
       (with-ontology-exptypes rule
	 (when (and (eql (exptype-rank rule) type)
		    (not (member (exptype-type rule) *disallowed-quantities*))
		    (or (null names) (member (exptype-type rule) names)))
	   (let (*ontology-bindings*) 
	     (get-ontology-bindings (ExpType-new-english rule))
	     (union-with result 
			  (expand-with-bindings-w *ontology-bindings* 
						  nil rule))))))
      
      ((member type '(body line))
       (dolist (prop (problem-bodies-and-compounds *cp*))
	 (union-with result  (list (cons (new-english-find prop) prop)))))
      
      (t (warn "generate-wrong-quantities invalid type ~A" type)))
    
  (let ((relevant (mapcar #'second
		      (remove tool-prop
			      (mapcar #'systementry-prop *sg-entries*)
			      :key #'car :test-not #'eql))))
    (when nil   ;debug print
      (format webserver:*stdout* "got ~A of type ~A~% relevant: ~A~%" 
	    (length result) type relevant))
    (delete-if #'(lambda (prop) (member prop relevant :test #'unify))
	       result :key #'cdr))))
	
(defun expand-with-bindings-w (binding-sets bindings rule)
  "Returns (<model> . <prop>) pair."
  (if binding-sets
      ;; Expand binding possibilities, building binding list.
      (let (result)
	(dolist (binding (cdr (car binding-sets)))
	  (union-with result
		      (expand-with-bindings-w 
		       (cdr binding-sets)
		       (cons (cons (car (car binding-sets)) binding)
			     bindings)
		       rule)))
	result)
      (list (cons (expand-new-english (exptype-new-english rule) bindings)
		  (subst-bindings bindings (exptype-form rule))))))
