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
