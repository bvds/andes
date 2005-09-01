;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre2in.cl -- convert from prefix to infix
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  4 June 2001 - (lht) -- created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun old-pre2in (eq)
  (cond
   ((null eq)
    (Tell :pre2in "null ->~W" eq)
    nil)
   ((null (consp eq))
    (Tell :pre2in "Not list ->~W" eq)
    (list eq))
   ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
    (Tell :pre2in "Function ->~W" eq)
    (append (list (first eq)) (list (pre2in (second eq)))))
   ((and (member (first eq) '(- +)) (= (length eq) 2))
    (Tell :pre2in "Function ->~W" eq)
    (append (list (first eq)) (pre2in (second eq))))
   ((member (first eq) '(dnum))
    (Tell :pre2in "DNUM ->~W" eq)
    (if (consp (second eq))
	(append (pre2in (subseq eq 1 (- (length eq) 1))) (last eq))
      (rest eq)))
   ((third eq)
    (Tell :pre2in "three ->~W" eq)
    (if (consp (second eq))
	(append (pre2in (second eq)) (list (first eq)) (pre2in (third eq)))
      (append (list (list (second eq)) (list (first eq)) (pre2in (third eq))))))
   ((second eq)
    (Tell :pre2in "two ->~W" eq)
    (list (pre2in (second eq)) (first eq)))
   (t
    (Tell :pre2in "one ->~W" eq)
    (pre2in (first eq)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pre2in (eq)
  (cond
   ((null eq)
    (Tell :pre2in "null -> ~W" eq)
    nil)
   ((atom eq)
    (Tell :pre2in "atom -> ~W" eq)
    eq)
   ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
    (Tell :pre2in "function -> ~W" eq)
    (list (first eq) (pre2in (second eq))))
   ((and (member (first eq) '(+)) (null (cdr eq)))
     (Tell :pre2in "0-element sum -> ~W" eq)
     0)   ;; just turn empty sum into a zero - AW
   ((and (member (first eq) '(- +)) (= (length eq) 2))
    (Tell :pre2in "unary -> ~W" eq)
    (if (equal (first eq) '+)
	(pre2in (second eq))
      (list (first eq) (pre2in (second eq)))))
   ((member (first eq) '(dnum))
    (Tell :pre2in "dnum -> ~W" eq)
    (let ((s (subseq eq 1 (- (length eq) 1))))
      (if (first (last eq)) ; has non-NIL units
         (list (pre2in s) (first (last eq)))
      ;; else dimensionless number
	(list (pre2in s))) ; maybe drop parens?
      ))
   ((third eq)
    (Tell :pre2in "other -> ~W" eq)
    (if (equal (first eq) '=)
	(let ((f (pre2in (second eq)))
	      (s (pre2in (third eq))))
	  (append (if (consp f) f (list f)) '(=) (if (consp s) s (list s))))
      (if (member (first eq) '(+ *))
	  (let ((lst (list (pre2in (second eq)))))
	    (dolist (obj (cddr eq))
	      (setf lst (append lst (list (first eq) (pre2in obj)))))
	    lst)
	(list (pre2in (second eq)) (first eq) (pre2in (third eq))))))
   ((second eq)
    (Tell :test "Error in pre2in"))
   (t (car eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file pre2in.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

