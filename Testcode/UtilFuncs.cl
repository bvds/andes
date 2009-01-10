;;; UtilFuncs.cl
;;; Collin Lynch
;;; 8/13/2003
;;; Modifications by Anders Weinstein 2004-2008
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
;;;
;;; This code is used to support the runtime tests.  These
;;; functions act as wrappers to provide regular processing
;;; tasks and to allow for efficient re-use.

;;;; ======================================================
;;;; Fractional updates.
;;;; Some of the runtime tests are intended to represent a value within 
;;;; a set.  For example, the correct answer subscore is used to calculate 
;;;; the number of correct answers that the student has made out of the 
;;;; number required.  Within the runtime tests this fractional value will 
;;;; be represented as a 2-tuple of the form (<Numerator> <Denominator)  
;;;; For the answer subscore this is (<NumEntered> <NumNeeded>).
;;;;
;;;; The code in this section will be used to support tests that
;;;; deal with those codes.  

;;; Given a function that returns a single number execute it and 
;;; generate an initial fractional amount.  This is used to set 
;;; the initial value for fractional tests such as the answer-
;;; subscore by calculating the number of answers needed in the 
;;; problem.  Given a function F this will return: (0 result(F)).
(defun static-fractional-init (Test)
  (list :FRACT 0 (funcall Test)))


;;; Given a function that returns a single number and a fractional
;;; amount (stored in the form (<numerator> <denominator>)).  Execute
;;; the function with the numerator as its argument and return a 
;;; list of the form (<Result> <Denominator).  
;;;
;;; Note that This code does not (at present) allow for the numerator
;;; to be greater than the denominator.  Therefore this code will 
;;; throw an error if Result is greater than Denominator.

(defun static-fractional-update (Test Fract)
  (let ((Result (funcall Test (second Fract))))
    (if (> Result (third Fract))
	(error "Fractional result greater than denominator.")
      (list :FRACT Result (second Fract)))))


;;; Given a function that takes no arguments and a fractional amount
;;; stored in the form (<Numerator> <Denominator>) update the value
;;; by calling the function and returning (<Result> <Denominator>).
(defun static-fractional-update-0 (Test Fract)
  (let ((Result (funcall Test)))
    (if (> Result (third Fract))
	(error "Fractional result greater than denominator.")
      (list :FRACT Result (third Fract)))))
  



;;;; ==================================================================


;;; Temporary for development, this will be moved later on.
;;; Given a problem solution collect the set of systementries
;;;(defun collect-solution-entries
