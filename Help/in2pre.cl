;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (cl) <collinl@pitt.edu>
;; Modified:
;;  16 May 2001 - (lht) -- created
;;; Modifications by Anders Weinstein 2001-2008
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
;; in2pre.cl -- routines for converting infix lists to prefix lists ... 
;;  facilities for special handling of unary, binary, stamp and other special 
;;  operators with additional functionality for supporting operator precedence.
;;  12/6/2003 - (cl) -- removed unused vars that were causing compiler 
;;    warnings:
;;    in2pre-position-of-first now uses the Len variable.
;;    in2pre:  replaced "(if (not (null obj)) (let ((jnk nil))" statement 
;;             with a when.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in2pre
;; argument(s):
;;  expression -- a list with infixed operators; an example of such a list:
;;           '((3 + 4) * 17 = 10 * 7 + 7 * 7)
;;  leaveAlone -- a list of atoms that specify lists that are not to be 
;;           translated.   For example: leaveAlone = '(stamp) would cause 
;;           the expression (4 + (stamp this & that)) to become 
;;           '(+ 4 (stamp this & that))
;;  unary -- a list of unary operators or functions; for example: if unary is 
;;           '(+ - sin) then '((4 + - 3) * sin 30) will translate as: 
;;           '(* (+ 4 (- 3)) (sin 30)
;;  binary -- a list of lists -- each sublist will be a list of operators and 
;;           their associativity.  The order of the lists implies their 
;;           precedence.  For example, if binary has the value:
;;           '(((= r)) ((+ r) (- r)) ((* r) (/ r)) ((^ l))) then we are 
;;           stating that ^ has the highest precedence and that the left-most 
;;           ^ should be considered first, the next highest in precedence will 
;;           be * and / with equal precedence and the right-most should be 
;;           considered first, and so on.
;;  special -- a list of symbols that are also the names of functions that will
;;           be called when encountered; the argument of the functions will be 
;;           a list of 5 elements: the first is the list which contains a 
;;           symbol in special and the remaining 4 are the same as the last
;;           four arguments sent to in2pre: as an example: we have a function 
;;           defined as:  (defun ditto (x) (car x)); special contains '(ditto)
;;           and the expression is:  '((5 + 4) * (ditto 30)) -- we will get 
;;           back '(* (+ 5 4) (ditto 30)) ... while this example seems to 
;;           duplicate both the functionality of unary and leaveAlone, special 
;;           provides a mechanism for treating subexpression in any fashion 
;;           whatsoever.
;;  returns a transformed expression the exact nature of this expression 
;;  relise heavily on the arguments specified
(defun in2pre (expression leaveAlone unary binary special)
  (let ((r nil));r is the final infixed expression
    (dolist (obj expression)
      (if (consp obj)
	  (cond
	   ((member (car obj) leaveAlone)
	    (setf r (append r (list obj))))
	   ((member (car obj) special)
	    (setf r (append r (doSafe :in2pre (car obj) obj 
				      leaveAlone unary binary special))))
	   ((member (car obj) unary)
	    (setf r (append r (list
			       (cons (car obj)
				     (in2pre (rest obj) 
					     leaveAlone unary binary special))))))
	   (t 
	    (setf r (append r (in2pre obj leaveAlone unary binary special)))))
	(when (not (null obj))
	  ;;  Formerly (if (not (null obj))
	  ;;  Jnk not needed  (let ((jnk nil))
	  (setf r (append r (list obj))))))
    (setf r (in2pre-support r leaveAlone unary binary special))
    (setf r (clean r))
    r))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in2pre-support -- deals with binary operators primarily as all other issues 
;; are dealt with in
;;   in2pre
;; argument(s): as in2pre above
(defun in2pre-support (expression leaveAlone unary binary special)
  (cond
   ((null binary) expression)
   (t
    (let* ((pl (in2pre-position-of-first (car binary) expression)) 
	   (p (if pl (first pl) nil)))
      (if p
	  (list (append (list (second pl))
			(in2pre-support (subseq expression 0 p) 
					leaveAlone unary binary special)
			(in2pre-support (subseq expression (+ p 1)) 
					leaveAlone unary binary special)))
	(in2pre-support expression leaveAlone unary (rest binary) special))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; in2pre-position-of-first returns the position of the operator to consider
;; argument(s):
;;  ops -- list of operators and their left-right associativity, '((+ r) (- r))
;;  expression -- the expression being searched.
;; returns:
;;  a list of the form (position(0-indexed) operator-matched 
;;                                          operators-associativity)
;; example(s):
;;  (position-of-first '((+ r) (- r)) '(a = b - c + 6)) will return (3 - r)
;; note:  if the operators have differing associativity the operators with 
;; right 
;; associativity are given preference
(defun in2pre-position-of-first (ops expression)
  (if ops
      (let ((p nil) (pt nil))
	(dolist (op ops)
	  (setf pt
	    (if (equal 'r (second op))
		(list (position (first op) expression)
		      (position (first op) expression)
		      (first op)
		      (second op))
	      (let* ((len (length expression))
		     (pos (position (first op) expression :from-end Len)))
		(if pos
		    (list (- Len (position (first op) 
					   expression :from-end Len) 1)
			  (position (first op) expression :from-end Len)
			  (first op)
			  (second op))
		  (list nil nil (first op) (second op))))))
	  (if (not (first pt)) (setf pt nil))
	  (if pt (if p
		     (if (< (first pt) (first p))
			 (setf p pt))
		   (setf p pt))))
	(if (and p (first p)) (rest p) nil))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file in2pre.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
