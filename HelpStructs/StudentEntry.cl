;; Student Entry.cl
;; Collin Lynch
;; 4/25/2002
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
;; This file defines the Student entry facility 
;; complete with printing functions and other
;; facility elements.  It is called and used
;; primarily by 
;; 
;; Student entry -- represents a workbench action where the student made a 
;;                  problem solving entry, such as writing an equation or 
;;                  drawing a vector.  Fields are


(defstruct (StudentEntry (:print-function print-StudentEntry))
  Id	 ;Client assigned Identifier.
  type   ;can be phrase, equation, circle, rectangle, axes, vector, line
  ;; The following are properties of an object
  mode x y text width height radius symbol x-label y-label z-label angle
  ;; State overlaps with mode (need to fix this).
  State           ;One of correct, inefficient, dead-path, forbidden, incorrect.
  Prop   ;Entry proposition (Equalp to SystemEntry-prop.)
  CInterp         ;The Selected set of any SystemEntries that constitute 
  ;; the final Interpretation of the student entry.
  PossibleCInterps ;A list of all the sets of possible correct interpretations.
  Verbatim             ;The student's entry as they typed it.
  ParsedEqn            ;will contain the lisp (prefixed) form of the parsed equation
  ErrInterp            ;nil or an error interpretation
  ;; Some non-eqn student entries carry associated equation entries with them. 
  ;; These associated entries are dependent in that they must be deleted if 
  ;; the main entry is deleted.
  ;; Dependent equation entries hang off the following fields in 
  ;; the main entry:
  ImplicitEqns         ;list of any associated implicit equation entries
  GivenEqns	       ;list of any associated given value equation entries
  (Time (get-current-htime))  ;The entry's time.  
  ;; Typically the time it was create, but not necessarily.
  )

(defun print-StudentEntry (Entry &optional (Stream t) (Level 0))
  "Print the system entry for human consumption."
  (pprint-indent :block Level Stream)
  (format Stream "[Entry: ~A ~A ~A ~A~%" (StudentEntry-ID Entry) 
	  (StudentEntry-Prop Entry) (StudentEntry-State Entry)
	  (Studententry-time Entry))
  (pprint-indent :block Level Stream)
  (format Stream "        ~A~%" 
	  (StudentEntry-Verbatim Entry))
  (pprint-indent :block Level Stream)
  (format Stream "CorrectInterp:    ~A~%" (StudentEntry-CInterp Entry))
    (pprint-indent :block Level Stream)
  (format Stream "PossibleCorrectInterps:   ~A~%" (StudentEntry-PossibleCInterps Entry))
  (pprint-indent :block Level Stream)
  (format Stream "ErrInterp: ~A~%" (StudentEntry-ErrInterp Entry))
  (pprint-indent :block Level Stream)
  (format Stream "ParsedEqn: ~A~%" (StudentEntry-ParsedEqn Entry)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Student entry list functions.

(defun get-studententry-symbol (y)
  (or (find-symbol (strcat "STUDENTENTRY-" (symbol-name y)))
      (error "StudentEntry does not have member ~A" y)))

(defmacro update-entry-from-variables (entry &rest x) 
  "Update StudentEntry object from given list of variables."
  (cons 'progn 
	(mapcar #'(lambda (y) 
		    `(when ,y 
		      (setf ,(list (get-studententry-symbol y) entry) ,y))) 
		x)))

(defmacro update-entry-from-entry (new-entry old-entry &rest x) 
  "Update StudentEntry object from old for given list of members."
  (cons 'progn 
	(mapcar #'(lambda (y) 
		    `(setf ,(list  (get-studententry-symbol y) new-entry) 
		      ,(list (get-studententry-symbol y) old-entry)))
		x)))

(defparameter *allowed-studententry-types* 
  '("phrase" "graphics" "equation" "circle" "rectangle" "axes" "vector" "line"))

(defun get-id (ID Entries)
  "Return the student entry for the specified ID if it has been done."
  (find ID Entries :test #'equalp :key #'StudentEntry-ID))

(defun get-entry-prop (Prop Entries)
  "Return the student entry for the specified prop if it has been done."
  (find Prop Entries :test #'equalp :key #'StudentEntry-Prop))

(defun get-verbatim-done (Verb Entries)
  "Return the verbatim element iff it has been done in the list."
  (find Verb Entries :test #'equalp :#'StudentEntry-Verbatim))

(defun get-state-entries (State Entries)
  "Return the subset of all entries that are in the specified state."
  (subset State Entries :test #'equalp :key #'StudentEntry-State))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-entry -- lookup student entry by ID
;; Arguments: id   workbench-assigned entry id
;; Returns: student entry structure or NIL if not found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-entry (Id)
  "find student entry by workbench assigned entry id"
  (find id *StudentEntries* :key #'StudentEntry-ID :test #'equal))

