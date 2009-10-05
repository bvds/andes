;;;  defines the struct used by whatswrong.cl to represent error interpretations
;;;  Kurt VanLehn
;;;  Copyright Kurt VanLehn 2001
;;; Modifications by Anders Weinstein 2002-2008
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

;;
(defstruct (ErrorInterp (:print-function write-ErrorInterp))
  Intended      ; interpretation of student's intended action
  Remediation   ; a tutor turn. Typically contains a hint sequence
  Diagnosis     ; a cons containing name and parameters, for assoc.
  hints         ; evaluates to a hint sequence
  Order         ; alist of specifications to determine priority
  State         ; (The following list is obsolete) 
;;; One of forbidden, premature, premature-subst, done-already, inefficient or none
  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun write-ErrorInterp (E &optional (Stream t) (level 0))
  (declare (ignore level))
  (format Stream "[ErrorInterp name:~A~%]" 
          (car (ErrorInterp-diagnosis e))))

