;;;;
;;;;   Generic hash-related functions
;;;;

;;   The built-in sxhash function does not work well for long lists or
;;   lists with sublists in either Allegro or SBCL  
;;
(defun my-sxhash (expr)
  "Modification of sxhash that works for long lists and sublists"
  (if (consp expr)
      (sxhash (cons (my-sxhash (car expr)) (my-sxhash (cdr expr))))
      (sxhash expr)))

;; just like in perl :-)
(defun hash-keys (hash)
  "return a list of keys associated with a hash"
  (let ((keys))
    (maphash #'(lambda (key value) (declare (ignore value))
		       (push key keys)) hash) keys))
