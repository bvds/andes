;; algebra.cl -- convert from lisp format to input-able string
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  4 June 2001 - (lht) -- created
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wrap-parentheses (parent expr)
    "Add parentheses to string when parent is true"
  (if parent (strcat "(" expr ")") expr))

(defun algebra (eq &key parent)
  "Convert prefix form to input-ready string."
  (cond
    ((stringp eq) eq)   
    ((numberp eq)
     ;; don't want to use "d" for exponent
     (let ((*read-default-float-format* 'double-float)) 
       (princ-to-string eq)))
    ((symbolp eq) 
     ;; Substitute student vars for their canonical counterparts.
     ;; atoms may be system vars, if no counterpart studvar, leave unchanged.
     (or (symbols:canonical-to-student eq)
	 ;; But should have some lookup for this to get correct string.
	 ;; Bug #1677.
	 (string eq)))
    ;; unary special functions 
    ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
     (when (cddr eq) (warn "Two arguments to function ~A" eq))
     (strcat (string-downcase (string (first eq)))  
	     (wrap-parentheses t (algebra (second eq)))))
    ;; empty plus
    ((and (member (first eq) '(+)) (null (cdr eq))) "0")
    ;; unary arithematic operator
    ((and (member (first eq) '(+ *)) (null (cddr eq)))
     (algebra (second eq)))
    ;; unary minus
    ((and (eql (car eq) '-) (null (cddr eq)))
     (strcat "-" (algebra (second eq) :parent -)))
    ;; Use Ontology for dimensioned numbers.
    ((eql (first eq) 'dnum)
     ;; may add parentheses if there are units or errors
     (wrap-parentheses (and parent (or (third eq) (member :error eq)))
		       (def-np eq)))
    ;; binary operators
    ((member (car eq) '(/ ^))
     (when (cdddr eq) (warn "binop with ~A" eq))
     (let ((op (pop eq)))
       (wrap-parentheses (or (member parent '(t))
			     (and (member op '(/)) (eql parent '^)))
			 (strcat (algebra (first eq) :parent op) (string op)
				 (algebra (second eq) :parent op)))))

    ;; Equals
    ((eql (car eq) '=)
     (when parent (warn "Equality ~A with parent ~A" eq parent))
     (when (cdddr eq) (warn "Equality with ~A" eq))
     (strcat (algebra (second eq)) " = " (algebra (third eq))))

    ;; n-ary operators.
    ;; Right now, this does not assume left-to-right 
    ;; associativity.
    ((member (car eq) '(- + * |.|)) ;|.| is for units
     (let* ((op (pop eq))
	    (result (algebra (pop eq) :parent op)))
       (dolist (term eq)
	 (setf result (strcat result (string op) 
			      (algebra term :parent op))))
       (wrap-parentheses (or (member parent '(t - / ^))
			     ;; units don't have +/-
			     (and (member op '(+ -)) (eql parent '*)))
			 result)))
    (t (warn "Unknown object ~A" eq) 
       (princ-to-string eq))))

