;;; Defines the struct and the macro for defining error classes, which
;;; appear in errors.cl and are interpreted by whatswrong.cl

(defvar **error-classes**)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct Error-Class
   Name            ;; atom
   Arguments       ;; ordered list of ?variables or forms
   Conditions      ;; ordered list of conditons (see whatswrong.cl)
   Probability     ;; Lisp evaluable form that returns a floating point number
   Utility         ;; Lisp evaluable form that returns a floating point number
   )

(defun error-class-initialize ()
  (setf **error-classes** nil))

(defmacro def-Error-Class (name arguments conditions &key (Probability 0.1) (Utility 1.0))
  `(push (make-error-class :arguments (quote ,arguments)
			   :name (quote ,name)
			   :conditions (quote ,conditions)
			   :probability (quote ,probability)
			   :utility (quote ,utility))
	 **error-classes**))


