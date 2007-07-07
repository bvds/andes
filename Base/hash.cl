;;;;
;;;;   Hash function to be used for long expressions
;;;;

;;   The built-in sxhash function does not work well for  long lists or
;;   lists with sublists in either Allegro or SBCL  
;;
(defun my-sxhash (expr)
  "Modification of sxhash that works for long lists and sublists"
  (if (consp expr)
      (+ (* 2 (my-sxhash (car expr))) (my-sxhash (cdr expr)))
      (sxhash expr)))
