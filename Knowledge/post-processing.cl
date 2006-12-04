;;;;
;;;;     Macros to apply to a problem after solutions have been found
;;;;
;;;;

(defparameter *post-processing* () "list of operations to apply after a solution has been found")

(defun clear-post-processing ()
  (setq *post-processing* nil))

(defstruct postoperator
  name  ;name, for internal use
  args  ;list of arguments for function
  comment ;any comment
  body ;function to apply
)

(defmacro post-process (name args &rest body)
  (when (find name *post-processing* :key #'postoperator-name)
    (error "post-process function ~A already exists." name))
  (when (or (not (consp args))(cdr args))
    (error "post-process takes one arguement, the problem name."))
  (defun name args . body)
  (let ((e  (make-postoperator :name name :args args 
			       :comment (when (stringp (car body)) (pop body))
			       :body body)))
    (push e *post-processing*)
    e))

(defun run-post-processing (problem)
  (dolist (f *post-processing*)
    (funcall (postoperator-name f) problem)))
;	(funcall #'(lambda (postoperator-args f) (postoperator-body f)) 
;		 problem)))