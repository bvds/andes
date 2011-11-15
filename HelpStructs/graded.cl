;;; graded.cl
;;; Copyright 2011 by Kurt Vanlehn and Brett van de Sande
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


;; Need to add these to Documentation/data_structures.html


;; These are attached to SystemEntry objects.
(defstruct (graded  (:print-function print-graded))
  incorrects ;list of info-provided
  hints ;list of bottom-out hints associated with entry.
  eqns  ;alist of (StudentEntry . length) overlapping with SystemEntry.
  status     ;Current status.  Can be:
  ;; +correct+ +incorrect+ or nil (deleted or not created)
  optional  ;Flag if step is optional, default (nil) is required.
  ;;  Can be: nil 'allowed 'preferred
  weight     ;Weight is an estimate of time needed (seconds).
  possibilities ;list of number of possible choices for each slot of prop
  ignore ;Flag to ignore entry.  
  ;; For instance, for pre-defined quantities or implicit equations.
)

(defun print-graded (grade &optional (Stream t) (level 0))
  (pprint-indent :block level stream)
  (format Stream "[grade: ~A ~@[~A ~]weight ~A ~@[ignore~]~%"
	  (graded-status grade) (graded-optional grade) 
	  (graded-weight grade) (graded-ignore grade))
  (when (graded-hints grade) 
    (format Stream "    hints: ~A~%" (graded-hints grade)))
  (when (graded-incorrects grade) 
    (format Stream "    incorrects: ~A~%" (graded-incorrects grade)))
  (when (graded-eqns grade) 
    (format Stream "    ~A~%"
	    (mapcar #'(lambda (x) (cons (StudentEntry-prop (car x)) (cdr x)))
		    (graded-eqns grade))))
(format Stream "    ]~%"))
	   
	   


(defstruct info-provided
  prop  ;prop associated with incorrect attempt or label for a hint.
  slots ;list showing fraction of information given for 
  ;; each slot of the solution step prop.  A list member may be
  ;; nil.  Information given must be derived from the associated 
  ;; hint or error handler.
  ;; May eventually try something more sophisticated.
  penalize ;Flag if points should be taken off for this error.
  ;; User interface or Andes-specific conventions should not
  ;; be penalized.
)
