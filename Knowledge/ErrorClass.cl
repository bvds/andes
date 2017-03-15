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
;;; Defines the struct and the macro for defining error classes, which
;;; appear in errors.cl and are interpreted by whatswrong.cl

(defvar **entry-tests** ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  One may want to still return a hint even in a case with a correct match.
;;     "This is correct, but you are being a little sloppy with your drawing."
;;  Also, one often has matching system entries for an incorrect student
;;  entry.  Currently, this is assumed to be determined by the last
;;  successful (correct ...) or (fix-eqn-by-replacing ...) match.
;;  Thus, correct returns only a Boolean value.
;;

(defstruct EntryTest
  name          ; name of error
  arguments     ; list of variables specifying instance of error.
  preconditions  ;ordered list of conditions (see whatswrong.cl)
              ;this determines a match
  apply       ;Conditions of application
					;no-match:  use after no match found
					;match:  use after match found
					;nil:  always apply
  state       ;state of student entry +correct+ or +incorrect+  
					;Match contained in last match
					;with (correct ...)
  hint            ;Lisp evaluable form giving resulting hint sequence
					;to replace function call
  order           ;List of dotted pairs giving order specification
					;when several tests are apply
					;choose those with maximal order
  )

(defun clear-entry-tests ()
  (setf **entry-tests** nil))

;;;
;;;  Make tests associated with errors.  This is mostly
;;;  for backwards compatibility.
;;;

(defmacro def-error-class (name arguments conditions &key (Probability 0.1)
				(Utility 1.0))
  ;; Doesn't really need to be at end ...
  `(push-to-end (make-EntryTest   :name ',name
		 :arguments ',arguments
		 :preconditions (sbcl-expand-backquote ',conditions)
		 :apply 'no-match
		 :state '+incorrect+ ;these are all errors
		 :hint '(make-hint-seq ,(cons name arguments))
		 :order '((expected-utility .
			   (* ,probability ,utility))))
    **entry-tests** :key #'EntryTest-name))

;;;
;;;   More general utility to perform tests
;;;


(defmacro def-entry-test (name arguments &key preconditions apply state hint
			  (order '((global . 1))))
  ;; Doesn't really need to be at end ...
  `(push-to-end (make-EntryTest :name ',name
		 :arguments ',arguments
		 :preconditions ',preconditions
		 :apply ',apply
		 :state ',state
		 :hint (list 'make-hint-seq (sbcl-expand-backquote ',hint))
		 :order ',order)
    **entry-tests** :key #'EntryTest-name))


(defun print-error-names ()
  "Print list of names of all errors, for debugging."
  (sort (loop for test in **entry-tests**
	when (eql (eval (EntryTest-state test)) +incorrect+)
	collect (EntryTest-name test)) #'string<))
