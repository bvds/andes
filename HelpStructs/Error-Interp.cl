;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  defines the struct used by whatswrong.cl to represent error interpretations
;;;  Kurt VanLehn
;;;  Copyright Kurt VanLehn 2001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defstruct (Error-Interp (:print-function write-Error-Interp))
  test          ; name of test that provided this interp
  Intended      ; interpretation of student's intended action
  Remediation   ; a non-empty list of tutor turns
  Diagnosis     ; lisp expression whose evaluation returns a list of hints.
  Order         ; alist of specifications to determine priority
  State         ; (The following list is obsolete) 
;;; One of forbidden, premature, premature-subst, done-already, inefficient or none
  Correct       ; Boolean if test returns student entry correct.
  ;; to be removed in favor of method using ranking
  Expected-Utility ; a floating point number
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun write-Error-Interp (E &optional (Stream t) (level 0))
  (declare (ignore level))
  (format Stream "[Error-Interp name:~A~%]" 
          (car (error-interp-diagnosis e))))

