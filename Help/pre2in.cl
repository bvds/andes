;;; Modifications by Anders Weinstein 2000-2008
;;; Modifications by Brett van de Sande, 2005-2008
;;; Copyright 2009 by Kurt Vanlehn and Brett van de Sande
;;;  This file is part of the Andes Intelligent Tutor Stystem.
;;;
;;;  The Andes Intelligent Tutor System is free software: you can redistribute
;;;  it and/or modify it under the terms of the GNU Lesser General Public 
;;;  License as published by the Free Software Foundation, either version 3 
;;;  of the License, or (at your option) any later version.
;;;
;;;  The Andes Solver is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with the Andes Intelligent Tutor System.  If not, see 
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre2in.cl -- convert from prefix to infix
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  4 June 2001 - (lht) -- created
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


