;;;; StackTests
;;;; Collin Lynch 1/9/2003 
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
;;;;
;;;; The code in this file is used to run stack tests.  All of the testing functions
;;;; defined in this file take a stack of CMD structs stored in LIFO order.  The
;;;; tests will return t if the stack passes or nil otherwize.  All of the tests in
;;;; this file are labelled with their support code listed.
;;;;
;;;; The testset is defined at the end of the file.  Like the command-testset this
;;;; system maintains a list of integers that reciord the number of times that each
;;;; specific test returned t.  
;;;;
;;;; The tests themselvers are grouped into conceptual sections in the file.  Most of
;;;; the functions that they call are located in Base/cmd.cl and Base/StackProcessing.cl



;;;; =======================================================================
;;;; Help Stack Tests
;;;; The code in this section handles help stacks.  It tests to determine 
;;;; if there is a help stack rooted at the current command and, if necessary
;;;; whether it meets some other threshold requirement.  
;;;;
;;;; A help stack is defined as a sequence of help commands and replies that
;;;; start at the top with a help call such as a next-step-help, why-wong-*
;;;; or some command that returns a show-hint result. It is then continued
;;;; with interim help commands such as explain-more and handle-student-response.
;;;; See the utils functions for the definition of these.  
;;;;
;;;; NOTE:: A help stack can contain one and only one md, the help cap.
;;;;
;;;; NOTE:: Both of the help-stack tests return the sequence of commands that
;;;;        satisfy the search.
;;;;
;;;; NOTE:: If the cursor is active in an equation box when the student 
;;;;  calls next-step help or other help commands then the system will 
;;;;  submit a delete equation command, "", as a dde-post.  Because this is 
;;;;  asynbchronous it can show up at any time during after the call to 
;;;;  get-proc help.  The help-stackc commands will ignore up to one of 
;;;;  these in a stack.



;;; ---------------------------------------------------------------------------
;;; Bottom-out hints
;;; A command is a bottom-out-hint iff:
;;;  1. It is part of a help stack.
;;;  2. It is a handle-student-response or explain-more dde.
;;;  3. Its result is a hint-return-val with a show-hint command 
;;;     type and no menu.  
;;;     NOTE:: "OK" menus are not included as they are used as an 
;;;       interim agreement to help break a long hint comment
;;;       into several shorter ones. In the future I will confirm
;;;       this using some more sophisitcated searching.  
;;;
;;; NOTE: It might be a good idea to eliminate error hints from these listings
;;;    by testing the contents of the string itself.  This can be done but I am
;;;    putting it off for now in favor of producing more flounderiung predicates.




;;; Return a stack in fifo order comprising the help stack capping the current
;;; command iff the current command is a bottom-out-hint.  Return Nil if it does


(defun proc-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (get-proc-help-cmdp R)))) 



(defun WWH-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (why-wrong-cmdp R))))


;;; Return t if the topmost cmd in the stack is a bottom-out-hint and

(defun WWO-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (why-wrong-obj-cmdp R))))


(defun WWE-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (why-wrong-eqn-cmdp R))))







