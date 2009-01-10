;;; Macro2.cl
;;; 11/25/00
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
;;; This file defines the Andes2 Macro facilty.  Macros are similar to
;;; executables in that they can be embedded within Andes2 script as
;;; preconditions.  However, when a macro is encountered, it will be
;;; expanded into a possibly empty set of preconditions, which take its
;;; place in the precondition list.
;;;
;;; Macros do not appear as a unit in the solution graphs or in the 
;;; state histories but are replaced by their expanded forms.

(defun precond-macro-p (M)
  "Return t iff Exp is a Macro expression."
  (and (listp M)
       (member (car M) '(foreach map))))


(defun Expand-precond-macro (M State)
  "Expand the specified macro and return the result."
  (case (car M)
    (foreach (expand-foreach M State))
    (map (expand-map M State))))


;;; This code implements the macro (foreach <Var> <Form> <Literal>) It
;;; lisp-evaluates the given form, and expects to get a list back.  It
;;; then creates a list of goals that are copies of the given literal
;;; with the given variable replaced by members of the list.  For each
;;; of the remaining variables in the given literal, the variable is
;;; left in if it is bound and replaced by a new variable otherwise.
;;; Thus, unbound variables in the original literal act as dont-cares
;;; in the resulting list of goals that replace the foreach macro.
;;; Bound variables appear to be retained in all the resulting goals.
;;; Must use get-binding instead of Lookup because when a variable is
;;; bound to NIL, lookup returns NIL while Get-binding returns
;;; (<var>).

(defun expand-foreach (M State)
  "Returns a list of goals to replace the (foreach <var> <form> <prop>) 
   precondition macro"
  (let* ((var (second M))
	 (prop (fourth M))
	 (unbound-vars (remove-if #'(lambda (x) (get-binding x (st-bindings State)))
				  (variables-in prop)))
	 (goals))
    (if (not (member var unbound-vars))
	(error "Variable is not in the proposition of~%   ~a" M))
    (dolist (Value (eval (subst-bindings-quoted (St-Bindings State) (third M))))
      (push (sublis (mapcar #'(lambda (x) (cons x (gensym (string x)))) 
			    unbound-vars)
		    (subst Value Var Prop))
	    goals))
    (nreverse goals)))
	

;;; This code implements the Map macro. The syntax is (map <input-var>
;;; <form> <prop> <output-var> <set-var>) The form is evaluated and
;;; should return a list.  For each member of the list, a goal is
;;; created by copying the proposition and replaceing <input-var> with
;;; the member of the list.  The bound variables in <prop> are
;;; left in and the unbound variables are replaced by new
;;; variables.  The output-var should occur in the goal and be
;;; unbound, so it is replaced by new variables with each iteration of
;;; the loop.  These variables are collected into a list, and a goal
;;; of the form (bind <set-var> (list . <vars>)) is appended to the
;;; end of the goals, where <vars> are the new version of the
;;; output-var.  This means that the solver will prove each of the
;;; goals then then bind set-var to a set consisting of the values
;;; "output" by the goal.
;;; Modified by Collin Lynch.
;;; I added the reverse to the renamed-output-vars list to ensure that
;;; the variables remain in the listing in the order in which they appear.
;;; This was causing output bugs in some of the equation files.

(defun expand-map (M State)
  "Returns a list of goals to replace the 
   (map <input-var> <form> <prop> <output-var> <Setvar>) precondition macro"
  (let* ((Input-var (second M))
	 (prop (fourth M))
	 (unbound-vars (remove-if #'(lambda (x) 
				      (get-binding x (st-bindings State)))
				  (variables-in prop)))
	 (output-var (fifth M))
	 (goals)
	 (renamed-output-vars)
	 (pairs))
    (if (not (member input-var unbound-vars))
	(error "Input var is not in the Proposition of~%    ~a" M))
    (if (not (member output-var unbound-vars))
	(error "Output var is not in the Proposition of~%    ~a" M))
    (dolist (Value (eval (subst-bindings-quoted (St-Bindings State) (third M))))
      (setq pairs (mapcar #'(lambda (var) (cons var (gensym (string var)))) 
			  unbound-vars))
      (push (sublis pairs (subst Value Input-var prop)) 
	    goals)
      (push (cdr (assoc output-var pairs))
	    renamed-output-vars))
    (push (list 'bind (sixth M) (cons 'list (reverse renamed-output-vars)))    ; Added the reverse to clear up order problem.
	  goals)
    (nreverse goals)))
	










