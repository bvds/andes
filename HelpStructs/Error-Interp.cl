;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  defines the struct used by whatswrong.cl to represent error interpretations
;;;  Kurt VanLehn
;;;  Copyright Kurt VanLehn 2001
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defstruct (Error-Interp (:print-function write-Error-Interp))
  Intended      ;; interpretation of student's intended action
  Remediation   ;; a non-empty list of tutor turns
  Diagnosis     ;; a list whose car is the atom that names the error-handler
  Order         ;; alist of specifications to determine priority
  State         ;; One of forbidden, premature, premature-subst, done-already, inefficient or none
  Correct       ;; Boolean if test returns student entry correct.
  ;; to be removed in favor of method using ranking
  Expected-Utility ;; a floating point number
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun write-Error-Interp (E &optional (Stream t) (level 0))
  (declare (ignore level))
  (format Stream "[Error-Interp state: ~a expected utility: ~,2f ~%diagnosis: ~a~ correct ~A~%"
	  (error-interp-state e)
	  (or (error-interp-Expected-Utility e) 0)
	  (error-interp-diagnosis e)
	  (error-interp-intended e)))

