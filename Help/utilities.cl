;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2010
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
;; utilities.cl -- the lisp calls that echo the commands sent by the Andes 2
;;  Workbench to the Dialog Manager. They are grouped here for that common rea-
;;  son alone. Supportive functions can be found else-where.
;; Modified:
;;  19 April 2001 - (lht) -- added packaging code
;;  10 May 2001 - (lht) -- removed packaging code and adjusted for new grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun find-all (item sequence
		 &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
	   :test (complement test) keyword-args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list printer
(defun prl (x)
  (cond
   ((null x)
    (format t "NIL"))
   ((consp x)
    (dolist (obj x) (format t "~W~%" obj)))
   (t
    (format t "~W~%" x))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-begins-with-p - predicate to test if a list starts with a certain item (uses equal)
;; argument(s):
;;  lhs the element that we want the list to start with
;;  lst - the list to examine
;; returns:
;;  nil if the first of list is not equal to lhs otherwise returns t
(defun list-begins-with-p (lhs lst)
  (if (and (consp lst) (equal (first lst) lhs)) t nil))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; removes extraneous nils
(defun clean (x)
	      (let ((tmp nil))
		(dolist (obj x)
		  (if (consp obj)
		      (setf tmp (append tmp (list (clean obj))))
		    (if (not (equal 'nil obj))
			(setf tmp (append tmp (list obj)))))) tmp))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file utilitiess.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
