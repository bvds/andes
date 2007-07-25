;;; Defines the struct and the macro for defining error classes, which
;;; appear in errors.cl and are interpreted by whatswrong.cl

(defvar **entry-tests**)

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
  Name        ;name of the test
  preconditions  ;ordered list of conditions (see whatswrong.cl)
              ;this determines a match
  apply       ;Conditions of application
					;no-match:  use after no match found
					;match:  use after match found
					;nil:  always apply
  state       ;state of student entry **correct** or **incorrect**  
					;Match contained in last match
					;with (correct ...)
  hint            ;Lisp evaluable form giving resulting hint sequence
					;to replace function call
  order           ;List of dotted pairs giving order specification
					;when several tests are apply
					;choose those with maximal order
  flag-slots    ; list of ids of workbench dialog slots to flag 
  )

(defun clear-entry-tests ()
  (setf **entry-tests** nil))

;;;
;;;  Make tests associated with errors.  This is mostly
;;;  for backwards compatibility.
;;;

(defmacro def-Error-Class (name arguments conditions &key (Probability 0.1) 
				(Utility 1.0) (flag NIL))
  `(push (make-EntryTest :name (quote ,name)
			   :preconditions (quote ,conditions)
			   :apply 'no-match
			   :state '**incorrect** ;these are all errors
			   :hint (quote ,(cons name arguments))
			   :order (quote ((expected-utility .
					   (* ,probability ,utility))))
			   :flag-slots (quote ,flag)
			   )
	 **entry-tests**))

;;;
;;;   More general utility to perform tests
;;;


(defmacro def-entry-test (name arguments &key preconditions apply state hint
			       (order '((global . 1))))
  (when (member (cons name arguments) **entry-tests** 
		:key #'EntryTest-name :test #'unify)
    (error "entry test ~A already exists." name))
  (let ((e (make-EntryTest :name name
			    :preconditions preconditions  
			    :apply apply
			    :state state
			    :hint `(make-hint-seq ,hint) 
			    :order order
			    )))
    (push e **entry-tests**)
    t))

(defun get-error-class-by-name (name)
 (find name **entry-tests** :key #'EntryTest-name))
