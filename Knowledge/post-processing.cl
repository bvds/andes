;;;;
;;;;     Macros to apply to a problem after solutions have been found
;;;;
;;;;

(defparameter *debug-pp* t)

(defparameter *post-processing* () "list of operations to apply after a solution has been found")

(defun clear-post-processing ()
  (setq *post-processing* nil))

(defstruct postoperator
  name  ;name, for internal use
  comment ;any comment
  lambda ;function
)

(defmacro post-process (name args &rest body)
  (when (or (not (consp args))(cdr args))
    (error "post-process takes one arguement, the problem name."))
  (when (find name *post-processing* :key #'postoperator-name)
    (error "post-processing function ~A already exists." name))
  ;; Here, one might want to define a regular "defun" function 
  ;; which could be called directly.
  (let ((e  (make-postoperator :name name
			       :comment (when (stringp (car body)) (pop body))
			       :lambda (compile nil `(lambda ,args ,@body)))))
    (push e *post-processing*)
    e))

(defun run-post-processing (problem)
  (ps-bp "Apply post-process functions:  ~A" (Problem-Name Problem))
  (dolist (f *post-processing*)
    (when *debug-pp* (format t ";;;;  ~A~%" (postoperator-name f)))
    (funcall (postoperator-lambda f) problem)))

