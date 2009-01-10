;;; StudentAction.cl
;;; Collin Lynch
;;; 7/8/2002
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
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The studentaction represents a single student 
;;; action in the help system embodying the entry and
;;; its result value.  
;;;
;;; Fields: 
;;;    Type is a class specifier such as 'delete, or 
;;;    'dialog-turn-response (see the comment above).  
;;;
;;; Call: 
;;;    The lisp command that generated the action, (type 
;;;    is usuall car of the call but not always).  
;;;
;;; Result:
;;;    The (usually a tutor turn) that resulted from the
;;;    Call.  In the case of actions these will be simple
;;;    Color-Red or Color-Green turns but others may be 
;;;    supplied.
;;;
;;; Assoc:  
;;;    This is a spare pointer that can indicate whatever it 
;;;    is that I find necessary.  In the case of student entries
;;;    it points to the entry itself.  Other uses may vary.
;;;
;;; Time:
;;;    A universal time  indicating when the act occured.  
;;;    At runtime this will be set to the default values.  
;;;    If the help system is being run off of the HelpDriver 
;;;    it can spoof these by using the Set-last-action-time
;;;    command.
|#

(defstruct (StudentAction (:print-function print-StudentAction))
  Type   ;; An atom id'ng the type of action such as draw-body get-help, etc.
  Call   ;; The Workbench call corresponding to this action.
  Result ;; The Help System's response to the call.
  Assoc  ;; A general pointer for the result suchg as the StudentEntry struct.
  (Time (get-current-htime))   ;; The Universaltime that the act occured.
  )


(defun print-StudentAction (Act &optional (Stream t) (Level 0))
  "Print out the specified Act."
  (pprint-indent :block Level)
  (format Stream "Act ~a: ~w => ~a ~a~%" 
	  (StudentAction-Type Act) (StudentAction-Call Act)
	  (StudentAction-Result Act) (StudentAction-Assoc Act)))


;;;; =================================================================
;;;; Utility functions for easy analysis and comparison.


;;; Compare the calls in the two StudentActions 
;;; and return t if they are equalp.
(defun studentAction-calls-equalp (A1 A2)
  (equalp (studentaction-call A1)
	  (studentaction-call A2)))


;;; Are the studentactions of the same type?
(defun studentactions-type-equalp (A B)
  (equalp (studentaction-type A)
	  (studentaction-type B)))


;;; Return t iff the studentaction is one of the specified types.
(defun studentaction-type-member? (Action Types)
  "Is the studentaction's type a member of the list?"
  (member (Studentaction-Type Action) Types))


(defun studentaction-of-type? (Action Type)
  "Is the StudentAction ACTION of type TYPE?"
  (equalp Type (Studentaction-Type Action)))


;;; Return t iff the studentaction is an entry type (I.E. has a 
;;; studententry associated with it).  
(defun studentaction-entry-p (Action)
  "Is this an entry-generating action?"
  (studententry-p (studentaction-Assoc Action)))


;;; Does the action have a turn as its Result?
(defun Studentaction-turn-responderp (Action)
  "Does the action have a turn as its Result?"
  (turn-p (Studentaction-Result Action)))


;;; Does the action have a **Color-Red** turn as its responder?
(defun Studentaction-color-red-resultp (Action)
  "Does the action have a **Color-Red** turn as its responder?"
  (and (turn-p (Studentaction-Result Action))
       (equalp **Color-Red** (turn-coloring (studentaction-result Action)))))


;;; Does the action have a **Color-Green** turn as its responder?
(defun Studentaction-color-green-resultp (Action)
  "Does the action have a **Color-Green** turn as its responder?"
  (and (turn-p (Studentaction-Result Action))
       (equalp **Color-Green** (turn-coloring (studentaction-result Action)))))



