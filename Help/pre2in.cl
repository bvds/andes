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
    nil)
   ((null (consp eq))
    (list eq))
   ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
    (append (list (first eq)) (list (pre2in (second eq)))))
   ((and (member (first eq) '(- +)) (= (length eq) 2))
    (append (list (first eq)) (pre2in (second eq))))
   ((member (first eq) '(dnum))
    (if (consp (second eq))
	(append (pre2in (subseq eq 1 (- (length eq) 1))) (last eq))
      (rest eq)))
   ((third eq)
    (if (consp (second eq))
	(append (pre2in (second eq)) (list (first eq)) (pre2in (third eq)))
      (append (list (list (second eq)) (list (first eq)) (pre2in (third eq))))))
   ((second eq)
    (list (pre2in (second eq)) (first eq)))
   (t
    (pre2in (first eq)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pre2in (eq)
  (cond
   ((null eq)
    nil)
   ((atom eq)
    eq)
   ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
    (list (first eq) (pre2in (second eq))))
   ((and (member (first eq) '(+)) (null (cdr eq)))
     0)   ;; just turn empty sum into a zero - AW
   ((and (member (first eq) '(- +)) (= (length eq) 2))
    (if (equal (first eq) '+)
	(pre2in (second eq))
      (list (first eq) (pre2in (second eq)))))
   ((member (first eq) '(dnum))
    (let ((s (subseq eq 1 (- (length eq) 1))))
      (if (first (last eq)) ; has non-NIL units
         (list (pre2in s) (first (last eq)))
      ;; else dimensionless number
	(list (pre2in s))) ; maybe drop parens?
      ))
   ((third eq)
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
   ((second eq))
   (t (car eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file pre2in.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

