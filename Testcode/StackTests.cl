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


;;;; =====================================================================
;;;; Loading relavent support files.
;;(load "c:/Andes2/HelpDriver/Base/Htime.cl")
;;(load "c:/Andes2/HelpDriver/Base/cmd.cl")
;;(load "c:/Andes2/HelpDriver/Base/stackprocessing.cl")
;;(load "c:/Andes2/HelpDriver/Base/LogTools.cl")





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

(defparameter **Bottom-out-Hint-Tests**
    '(bottom_out_hintp
      Proc_bottom_out_hintp
      WWH_bottom_out_hintp
      ;;WWO_bottom_out_hintp
      ;;WWE_bottom_out_hintp
      unsolicited_bottom_out_hintp))



;;; Return a stack in fifo order comprising the help stack capping the current
;;; command iff the current command is a bottom-out-hint.  Return Nil if it does
;;; not meed the above criteria.  
(defun bottom_out_hintp (Stack)
  "Return t if the topmost cmd is a bottom-out-hint."
  (bottom-out-hint-stackc Stack))


;;; Return t if the topmost cmd is a bottom-out hint and the help cap
;;; is a call to next-step-help.
(defun Proc_bottom_out_hintp (Stack) 
  (proc-bottom-out-hintp Stack))

(defun proc-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (get-proc-help-cmdp R)))) 


;;; Return t if the topmost cmd in the stack is a bottom-out-hint and
;;; the help cap is a call to why-wrong-*
(defun WWH_bottom_out_hintp (Stack)
  (WWH-bottom-out-hintp Stack))

(defun WWH-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (why-wrong-cmdp R))))


;;; Return t if the topmost cmd in the stack is a bottom-out-hint and
;;; the help cap is a call to why-wrong-object
(defun WWO_bottom_out_hintp (Stack)
  (WWO-bottom-out-hintp Stack))

(defun WWO-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (why-wrong-obj-cmdp R))))


;;; Return t if the topmost cmd in the stack is a bottom-out-hint and
;;; the help cap is a call to why-wrong-equation
(defun WWE_bottom_out_hintp (Stack)
  (WWE-bottom-out-hintp Stack))

(defun WWE-bottom-out-hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (if R (why-wrong-eqn-cmdp R))))




;;; A naked bottom-out-hint is one where the student has not called 
;;; next-step-help or some other help call but been given an unsolicited
;;; hint sequence and then followed it to the tail.
(defun unsolicited_bottom_out_hintp (Stack)
  "Is this the tail of a het-proc-help call?"
  (let ((R (car (bottom-out-hint-stackc Stack))))
    (and R (not (why-wrong-cmdp R))
	 (not (get-proc-help-cmdp R)))))









;;;; ======================================================================
;;;; Basic Entry Stacks
;;;; When the students are working on entries they can often make the same
;;;; entries and the same type of entries repeatedly especially when they
;;;; are attempting to get some entery right or are simpoly engaging in 
;;;; shallow search.  The code in this section tests for stacks of similar
;;;; entries that fulfill one or more constraints.  These constraints can
;;;; then be used for floundering tests later on.  

(defparameter **State-stack-tests**
    '(minimum_incorrect_entry_stackp
      minimum_correct_entry_stackp
      minimum_incorrect_noneq_entry_stackp
      minimum_correct_noneq_entry_stackp
      minimum_incorrect_eq_entry_stackp
      minimum_correct_eq_entry_stackp
      minimum_incorrect_answer_stackp
      minimum_correct_answer_stackp
      ))


;;;; ----------------------------------------------------------------------
;;;; Comparison tests.

(defparameter **Min-Rep-Thresh** 3 
  "The minimun number of repetitions that we care about.")

;;; ---------------------------------------------------------------------
;;; Repeated incorrect entries
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_incorrect_entry_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'incorrect-entry-cmdp))))


;;; ---------------------------------------------------------------------
;;; Repeated correct entries
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_correct_entry_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'correct-entry-cmdp))))

;;; ---------------------------------------------------------------------
;;; Repeated incorrect entries
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_incorrect_noneq_entry_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'incorrect-noneq-entry-cmdp))))


;;; ---------------------------------------------------------------------
;;; Repeated correct entries
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_correct_noneq_entry_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'correct-noneq-entry-cmdp))))

;;; ---------------------------------------------------------------------
;;; Repeated incorrect entries
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_incorrect_eq_entry_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'incorrect-eq-entry-cmdp))))


;;; ---------------------------------------------------------------------
;;; Repeated correct entries
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_correct_eq_entry_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'correct-eq-entry-cmdp))))

;;; ---------------------------------------------------------------------
;;; Repeated incorrect entriesanswers
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_incorrect_answer_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'incorrect-answer-cmdp))))


;;; ---------------------------------------------------------------------
;;; Repeated correct Answers
;;; Return t if the student has made one or more repeated incorrect entries
;;; of any type.

(defun minimum_correct_Answer_stackp (Stack &optional (Min **Min-rep-thresh**))
  "Test for a stack of incorrect entries."
  (<= Min (length (uni-test-stackc Stack #'correct-answer-cmdp))))




;;; ------------------------------------------------------------------------
;;; Sequences of specific classes

(defparameter **Min-Call-Seq-thresh** 3)

(defparameter **command-stack-tests**
    '(noneq_entry_stackp
      eq_entry_stackp
      answer_stackp
      algebra_stackp
      delete_stackp
      ))

(defun noneq_entry_stackp (Stack &optional (Thresh **Min-Call-Seq-Thresh**))
  (<= Thresh (length (class-call-stackc Stack 'NONEQ-ENTRY))))

(defun eq_entry_stackp (Stack &optional (Thresh **Min-Call-Seq-Thresh**))
  (<= Thresh (length (class-call-stackc Stack 'EQ-ENTRY))))

(defun Answer_stackp (Stack &optional (Thresh **Min-Call-Seq-Thresh**))
  (<= Thresh (length (class-call-stackc Stack 'ANSWER))))

(defun Algebra_stackp (Stack &optional (Thresh **Min-Call-Seq-Thresh**))
  (<= Thresh (length (class-call-stackc Stack 'ALGEBRA))))

(defun delete_stackp (Stack &optional (Thresh **Min-Call-Seq-Thresh**))
  (<= Thresh (length (class-call-stackc Stack 'DELETE))))








;;;; =======================================================================
;;;; Help Pairs.
;;;; The predicates in this section are made to deal with help stacks and 
;;;; the actions that immediately follow them.  The goal of these predicates
;;;; is to identify pairings of help with subsequent actions.  We are
;;;; specifically interested in determining how often the student completes
;;;; a help call and then makes an incorrect entry versus making a correct
;;;; entry and so on.  
;;;;
;;;; The predicates in this section draw on some of the code above as well
;;;; as code in CMD.cl.


;;; --------------------------------------------------------------------
;;; helpstack-help-types.
;;; These predicates can be called as is to return t if there exists some
;;; command followed by a helpstack that meets the specified requirements
;;; or used as part of a further test.
;;;
;;; This code makes use of local helpstack tests that should be centralized
;;; later on.
;;;
;;; TODO:: Replace these with Macros.

(defparameter **hpp-general-stack-tests**
    '(hpp_get_proc_help_pairp
      hpp_why_wrong_cmd_pairp
      hpp_why_wrong_EQN_pairp
      hpp_why_wrong_obj_pairp))

;;; Return t if the helpstack in the pair exists and is a next-step-help stack
;;; and the cmd-pred (if supplied) holds true.
(defun hpp_get_proc_help_pairp (Stack &optional (Cmd-Pred #'Identity))
  "Return t if the helpstack is capped by a next-step-help."
  (helpstack-predicate-pairp 
   Stack :Cmd-Pred Cmd-Pred 
   :help-Pred #'(lambda (S) (get-proc-help-cmdp (car S)))))

;;; Return t if the helpstack in the pair exists and is a why-wrong-cmd stack
;;; and the cmd-pred (if supplied) holds true.
(defun hpp_why_wrong_cmd_pairp (Stack &optional (Cmd-Pred #'Identity))
  "Return t if the helpstack is capped by a why-wrong-cmd."
  (helpstack-predicate-pairp 
   Stack :Cmd-Pred Cmd-Pred 
   :help-Pred #'(lambda (S) (why-wrong-cmdp (car S)))))


;;; Return t if the helpstack in the pair exists and is a why-wrong-EQN stack
;;; and the cmd-pred (if supplied) holds true.
(defun hpp_why_wrong_EQN_pairp (Stack &optional (Cmd-Pred #'Identity))
  "Return t if the helpstack is capped by a why-wrong-eqn."
  (helpstack-predicate-pairp 
   Stack :Cmd-Pred Cmd-Pred 
   :help-Pred #'(lambda (S) (why-wrong-EQN-cmdp (car S)))))


;;; Return t if the helpstack in the pair exists and is a why-wrong-obj stack
;;; and the cmd-pred (if supplied) holds true.
(defun hpp_why_wrong_obj_pairp (Stack &optional (Cmd-Pred #'Identity))
  "Return t if the helpstack is capped by a why-wrong-obj."
  (helpstack-predicate-pairp 
   Stack :Cmd-Pred Cmd-Pred 
   :help-Pred #'(lambda (S) (why-wrong-OBJ-cmdp (car S)))))


;;;; ----------------------------------------------------------------------
;;;; Top-Level instances
;;;;
;;; ----------------------------------------------------------------------
;;; Answer instances
;;; These helpstack predicates test whether or not the topmost cmd is an
;;; answer and the stack leading it is of the specified type.

(defparameter **hpp-answer-stack-tests**
    '(hpp_get_proc_help_answer_pairp
      hpp_why_wrong_cmdp_answer_pairp 
      hpp_why_wrong_eqnp_answer_pairp 
      hpp_why_wrong_objp_answer_pairp 
      hpp_get_proc_help_incorrect_answer_pairp 
      hpp_why_wrong_cmdp_incorrect_answer_pairp
      hpp_why_wrong_eqnp_incorrect_answer_pairp
      hpp_why_wrong_objp_incorrect_answer_pairp
      hpp_get_proc_help_correct_answer_pairp 
      hpp_why_wrong_cmdp_correct_answer_pairp
      hpp_why_wrong_eqnp_correct_answer_pairp
      hpp_why_wrong_objp_correct_answer_pairp))


(defun hpp_get_proc_help_answer_pairp (Stack)
  "Return t if the cmd is an answer and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'answer-cmdp))

(defun hpp_why_wrong_cmdp_answer_pairp (Stack)
  "Return t if the cmd is an answer and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'answer-cmdp))

(defun hpp_why_wrong_eqnp_answer_pairp (Stack)
  "Return t if the cmd is an answer and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'answer-cmdp))

(defun hpp_why_wrong_objp_answer_pairp (Stack)
  "Return t if the cmd is an answer and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'answer-cmdp))

;;; ----------------------------------------------------------------------
;;; Incorrect-Answer instances
;;; These helpstack predicates test whether or not the topmost cmd is an
;;; incorrect-answer and the stack leading it is of the specified type.

(defun hpp_get_proc_help_incorrect_answer_pairp (Stack)
  "Return t if the cmd is an incorrect-answer and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-answer-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_answer_pairp (Stack)
  "Return t if the cmd is an incorrect-answer and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-answer-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_answer_pairp (Stack)
  "Return t if the cmd is an incorrect-answer and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-answer-cmdp))

(defun hpp_why_wrong_objp_incorrect_answer_pairp (Stack)
  "Return t if the cmd is an incorrect-answer and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-answer-cmdp))

;;; ----------------------------------------------------------------------
;;; Correct-Answer instances
;;; These helpstack predicates test whether or not the topmost cmd is an
;;; correct-answer and the stack leading it is of the specified type.

(defun hpp_get_proc_help_correct_answer_pairp (Stack)
  "Return t if the cmd is an correct-answer and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-answer-cmdp))

(defun hpp_why_wrong_cmdp_correct_answer_pairp (Stack)
  "Return t if the cmd is an correct-answer and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-answer-cmdp))

(defun hpp_why_wrong_eqnp_correct_answer_pairp (Stack)
  "Return t if the cmd is an correct-answer and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-answer-cmdp))

(defun hpp_why_wrong_objp_correct_answer_pairp (Stack)
  "Return t if the cmd is an correct-answer and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-answer-cmdp))


;;; -----------------------------------------------------------------------
;;; Entry Instances 
;;; These tests handle entry commands and entries of specific types.

(defparameter **hpp-entry-stack-tests**
    '(hpp_get_proc_help_entry_pairp
      hpp_why_wrong_cmdp_entry_pairp 
      hpp_why_wrong_eqnp_entry_pairp 
      hpp_why_wrong_objp_entry_pairp 
      hpp_get_proc_help_incorrect_entry_pairp 
      hpp_why_wrong_cmdp_incorrect_entry_pairp
      hpp_why_wrong_eqnp_incorrect_entry_pairp
      hpp_why_wrong_objp_incorrect_entry_pairp
      hpp_get_proc_help_correct_entry_pairp 
      hpp_why_wrong_cmdp_correct_entry_pairp
      hpp_why_wrong_eqnp_correct_entry_pairp
      hpp_why_wrong_objp_correct_entry_pairp))


(defun hpp_get_proc_help_entry_pairp (Stack)
  "Return t if the cmd is an entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'entry-cmdp))

(defun hpp_why_wrong_cmdp_entry_pairp (Stack)
  "Return t if the cmd is an entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'entry-cmdp))

(defun hpp_why_wrong_eqnp_entry_pairp (Stack)
  "Return t if the cmd is an entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'entry-cmdp))

(defun hpp_why_wrong_objp_entry_pairp (Stack)
  "Return t if the cmd is an entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'entry-cmdp))

;;; -----------------------------------------------------------------------
;;; Incorrect-Entry Instances 
;;; These tests handle incorrect-entry commands and entries of specific types.

(defun hpp_get_proc_help_incorrect_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-entry-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-entry-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-entry-cmdp))

(defun hpp_why_wrong_objp_incorrect_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-entry-cmdp))

;;; -----------------------------------------------------------------------
;;; Correct-Entry Instances 
;;; These tests handle correct-entry commands and entries of specific types.

(defun hpp_get_proc_help_correct_entry_pairp (Stack)
  "Return t if the cmd is an correct-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-entry-cmdp))

(defun hpp_why_wrong_cmdp_correct_entry_pairp (Stack)
  "Return t if the cmd is an correct-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-entry-cmdp))

(defun hpp_why_wrong_eqnp_correct_entry_pairp (Stack)
  "Return t if the cmd is an correct-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-entry-cmdp))

(defun hpp_why_wrong_objp_correct_entry_pairp (Stack)
  "Return t if the cmd is an correct-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-entry-cmdp))




;;; -----------------------------------------------------------------------
;;; Eq-Entry Instances 
;;; These tests handle eq-entry commands and entries of specific types.

(defparameter **hpp-eq-entry-stack-tests**
    '(hpp_get_proc_help_eq_entry_pairp
      hpp_why_wrong_cmdp_eq_entry_pairp 
      hpp_why_wrong_eqnp_eq_entry_pairp 
      hpp_why_wrong_objp_eq_entry_pairp 
      hpp_get_proc_help_incorrect_eq_entry_pairp 
      hpp_why_wrong_cmdp_incorrect_eq_entry_pairp
      hpp_why_wrong_eqnp_incorrect_eq_entry_pairp
      hpp_why_wrong_objp_incorrect_eq_entry_pairp
      hpp_get_proc_help_correct_eq_entry_pairp 
      hpp_why_wrong_cmdp_correct_eq_entry_pairp
      hpp_why_wrong_eqnp_correct_eq_entry_pairp
      hpp_why_wrong_objp_correct_eq_entry_pairp))
  

(defun hpp_get_proc_help_eq_entry_pairp (Stack)
  "Return t if the cmd is an eq-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'eq-entry-cmdp))

(defun hpp_why_wrong_cmdp_eq_entry_pairp (Stack)
  "Return t if the cmd is an eq-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'eq-entry-cmdp))

(defun hpp_why_wrong_eqnp_eq_entry_pairp (Stack)
  "Return t if the cmd is an eq-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'eq-entry-cmdp))

(defun hpp_why_wrong_objp_eq_entry_pairp (Stack)
  "Return t if the cmd is an eq-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'eq-entry-cmdp))

;;; -----------------------------------------------------------------------
;;; Incorrect-Eq-Entry Instances 
;;; These tests handle incorrect-eq-entry commands and entries of specific types.

(defun hpp_get_proc_help_incorrect_eq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-eq-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-eq-entry-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_eq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-eq-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-eq-entry-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_eq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-eq-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-eq-entry-cmdp))

(defun hpp_why_wrong_objp_incorrect_eq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-eq-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-eq-entry-cmdp))

;;; -----------------------------------------------------------------------
;;; Correct-Eq-Entry Instances 
;;; These tests handle correct-eq-entry commands and entries of specific types.

(defun hpp_get_proc_help_correct_eq_entry_pairp (Stack)
  "Return t if the cmd is an correct-eq-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-eq-entry-cmdp))

(defun hpp_why_wrong_cmdp_correct_eq_entry_pairp (Stack)
  "Return t if the cmd is an correct-eq-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-eq-entry-cmdp))

(defun hpp_why_wrong_eqnp_correct_eq_entry_pairp (Stack)
  "Return t if the cmd is an correct-eq-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-eq-entry-cmdp))

(defun hpp_why_wrong_objp_correct_eq_entry_pairp (Stack)
  "Return t if the cmd is an correct-eq-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-eq-entry-cmdp))




;;; -----------------------------------------------------------------------
;;; Noneq-Entry Instances 
;;; These tests handle noneq-entry commands and entries of specific types.

(defparameter **hpp-noneq-entry-stack-tests**
    '(hpp_get_proc_help_noneq_entry_pairp
      hpp_why_wrong_cmdp_noneq_entry_pairp 
      hpp_why_wrong_eqnp_noneq_entry_pairp 
      hpp_why_wrong_objp_noneq_entry_pairp 
      hpp_get_proc_help_incorrect_noneq_entry_pairp 
      hpp_why_wrong_cmdp_incorrect_noneq_entry_pairp
      hpp_why_wrong_eqnp_incorrect_noneq_entry_pairp
      hpp_why_wrong_objp_incorrect_noneq_entry_pairp
      hpp_get_proc_help_correct_noneq_entry_pairp 
      hpp_why_wrong_cmdp_correct_noneq_entry_pairp
      hpp_why_wrong_eqnp_correct_noneq_entry_pairp
      hpp_why_wrong_objp_correct_noneq_entry_pairp))

(defun hpp_get_proc_help_noneq_entry_pairp (Stack)
  "Return t if the cmd is an noneq-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'noneq-entry-cmdp))

(defun hpp_why_wrong_cmdp_noneq_entry_pairp (Stack)
  "Return t if the cmd is an noneq-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'noneq-entry-cmdp))

(defun hpp_why_wrong_eqnp_noneq_entry_pairp  (Stack)
  "Return t if the cmd is an noneq-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'noneq-entry-cmdp))

(defun hpp_why_wrong_objp_noneq_entry_pairp (Stack)
  "Return t if the cmd is an noneq-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'noneq-entry-cmdp))

;;; -----------------------------------------------------------------------
;;; Incorrect-Noneq-Entry Instances 
;;; These tests handle incorrect-noneq-entry commands and entries of specific types.

(defun hpp_get_proc_help_incorrect_noneq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-noneq-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-noneq-entry-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_noneq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-noneq-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-noneq-entry-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_noneq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-noneq-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-noneq-entry-cmdp))

(defun hpp_why_wrong_objp_incorrect_noneq_entry_pairp (Stack)
  "Return t if the cmd is an incorrect-noneq-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-noneq-entry-cmdp))

;;; -----------------------------------------------------------------------
;;; Correct-Noneq-Entry Instances 
;;; These tests handle correct-noneq-entry commands and entries of specific types.

(defun hpp_get_proc_help_correct_noneq_entry_pairp (Stack)
  "Return t if the cmd is an correct-noneq-entry and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-noneq-entry-cmdp))

(defun hpp_why_wrong_cmdp_correct_noneq_entry_pairp (Stack)
  "Return t if the cmd is an correct-noneq-entry and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-noneq-entry-cmdp))

(defun hpp_why_wrong_eqnp_correct_noneq_entry_pairp (Stack)
  "Return t if the cmd is an correct-noneq-entry and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-noneq-entry-cmdp))

(defun hpp_why_wrong_objp_correct_noneq_entry_pairp (Stack)
  "Return t if the cmd is an correct-noneq-entry and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-noneq-entry-cmdp))


;;;; ------------------------------------------------------------------------------
;;;; Assertions
;;;; Assertions are either entry actions or answer assertions this code tests for
;;;; both.  Assertions will be set to correct/incorrect and this will catch that.

(defparameter **hpp-assertion-stack-tests**
    '(hpp_get_proc_help_assertion_pairp
      hpp_why_wrong_cmdp_assertion_pairp 
      hpp_why_wrong_eqnp_assertion_pairp 
      hpp_why_wrong_objp_assertion_pairp 
      hpp_get_proc_help_incorrect_assertion_pairp 
      hpp_why_wrong_cmdp_incorrect_assertion_pairp
      hpp_why_wrong_eqnp_incorrect_assertion_pairp
      hpp_why_wrong_objp_incorrect_assertion_pairp
      hpp_get_proc_help_correct_assertion_pairp 
      hpp_why_wrong_cmdp_correct_assertion_pairp
      hpp_why_wrong_eqnp_correct_assertion_pairp
      hpp_why_wrong_objp_correct_assertion_pairp))

(defun hpp_get_proc_help_assertion_pairp (Stack)
  "Return t if the cmd is an assertion and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'assertion-cmdp))

(defun hpp_why_wrong_cmdp_assertion_pairp (Stack)
  "Return t if the cmd is an assertion and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'assertion-cmdp))

(defun hpp_why_wrong_eqnp_assertion_pairp (Stack)
  "Return t if the cmd is an assertion and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'assertion-cmdp))

(defun hpp_why_wrong_objp_assertion_pairp (Stack)
  "Return t if the cmd is an assertion and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'assertion-cmdp))

;;; -----------------------------------------------------------------------
;;; Incorrect-Assertion Instances 
;;; These tests handle incorrect-assertion commands and entries of specific types.

(defun hpp_get_proc_help_incorrect_assertion_pairp (Stack)
  "Return t if the cmd is an incorrect-assertion and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-assertion-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_assertion_pairp (Stack)
  "Return t if the cmd is an incorrect-assertion and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-assertion-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_assertion_pairp (Stack)
  "Return t if the cmd is an incorrect-assertion and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-assertion-cmdp))

(defun hpp_why_wrong_objp_incorrect_assertion_pairp (Stack)
  "Return t if the cmd is an incorrect-assertion and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-assertion-cmdp))

;;; -----------------------------------------------------------------------
;;; Correct-Assertion Instances 
;;; These tests handle correct-assertion commands and entries of specific types.

(defun hpp_get_proc_help_correct_assertion_pairp (Stack)
  "Return t if the cmd is an correct-assertion and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-assertion-cmdp))

(defun hpp_why_wrong_cmdp_correct_assertion_pairp (Stack)
  "Return t if the cmd is an correct-assertion and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-assertion-cmdp))

(defun hpp_why_wrong_eqnp_correct_assertion_pairp (Stack)
  "Return t if the cmd is an correct-assertion and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-assertion-cmdp))

(defun hpp_why_wrong_objp_correct_assertion_pairp (Stack)
  "Return t if the cmd is an correct-assertion and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-assertion-cmdp))



;;;; ------------------------------------------------------------------------
;;;; Solution state changes.
;;;; Some of these can be correct so I have included the possibility here
;;;; note that deletions are treated as correct.

(defparameter **hpp-solution-state-change-stack-tests**
    '(hpp_get_proc_help_solution_state_change_pairp
      hpp_why_wrong_cmdp_solution_state_change_pairp 
      hpp_why_wrong_eqnp_solution_state_change_pairp 
      hpp_why_wrong_objp_solution_state_change_pairp 
      hpp_get_proc_help_incorrect_solution_state_change_pairp 
      hpp_why_wrong_cmdp_incorrect_solution_state_change_pairp
      hpp_why_wrong_eqnp_incorrect_solution_state_change_pairp
      hpp_why_wrong_objp_incorrect_solution_state_change_pairp
      hpp_get_proc_help_correct_solution_state_change_pairp 
      hpp_why_wrong_cmdp_correct_solution_state_change_pairp
      hpp_why_wrong_eqnp_correct_solution_state_change_pairp
      hpp_why_wrong_objp_correct_solution_state_change_pairp))

(defun hpp_get_proc_help_solution_state_change_pairp (Stack)
  "Return t if the cmd is an solution-state-change and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'solution-state-change-cmdp))

(defun hpp_why_wrong_cmdp_solution_state_change_pairp (Stack)
  "Return t if the cmd is an solution-state-change and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'solution-state-change-cmdp))

(defun hpp_why_wrong_eqnp_solution_state_change_pairp (Stack)
  "Return t if the cmd is an solution-state-change and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'solution-state-change-cmdp))

(defun hpp_why_wrong_objp_solution_state_change_pairp (Stack)
  "Return t if the cmd is an solution-state-change and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'solution-state-change-cmdp))

;;; -----------------------------------------------------------------------
;;; Incorrect-Solution-State-Change Instances 
;;; These tests handle incorrect-solution-state-change commands and entries of specific types.

(defun hpp_get_proc_help_incorrect_solution_state_change_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-state-change and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-solution-state-change-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_solution_state_change_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-state-change and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-solution-state-change-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_solution_state_change_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-state-change and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-solution-state-change-cmdp))

(defun hpp_why_wrong_objp_incorrect_solution_state_change_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-state-change and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-solution-state-change-cmdp))

;;; -----------------------------------------------------------------------
;;; Correct-Solution-State-Change Instances 
;;; These tests handle correct-solution-state-change commands and entries of specific types.

(defun hpp_get_proc_help_correct_solution_state_change_pairp (Stack)
  "Return t if the cmd is an correct-solution-state-change and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-solution-state-change-cmdp))

(defun hpp_why_wrong_cmdp_correct_solution_state_change_pairp (Stack)
  "Return t if the cmd is an correct-solution-state-change and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-solution-state-change-cmdp))

(defun hpp_why_wrong_eqnp_correct_solution_state_change_pairp (Stack)
  "Return t if the cmd is an correct-solution-state-change and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-solution-state-change-cmdp))

(defun hpp_why_wrong_objp_correct_solution_state_change_pairp (Stack)
  "Return t if the cmd is an correct-solution-state-change and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-solution-state-change-cmdp))


;;;; ----------------------------------------------------------------------
;;;; Solution actions 
;;;; These too can be rated but embrace nonrated actions.  

(defparameter **hpp-solution-action-stack-tests**
    '(hpp_get_proc_help_solution_action_pairp
      hpp_why_wrong_cmdp_solution_action_pairp 
      hpp_why_wrong_eqnp_solution_action_pairp 
      hpp_why_wrong_objp_solution_action_pairp 
      hpp_get_proc_help_incorrect_solution_action_pairp 
      hpp_why_wrong_cmdp_incorrect_solution_action_pairp
      hpp_why_wrong_eqnp_incorrect_solution_action_pairp
      hpp_why_wrong_objp_incorrect_solution_action_pairp
      hpp_get_proc_help_correct_solution_action_pairp 
      hpp_why_wrong_cmdp_correct_solution_action_pairp
      hpp_why_wrong_eqnp_correct_solution_action_pairp
      hpp_why_wrong_objp_correct_solution_action_pairp))

(defun hpp_get_proc_help_solution_action_pairp (Stack)
  "Return t if the cmd is an solution-action and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'solution-action-cmdp))

(defun hpp_why_wrong_cmdp_solution_action_pairp (Stack)
  "Return t if the cmd is an solution-action and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'solution-action-cmdp))

(defun hpp_why_wrong_eqnp_solution_action_pairp (Stack)
  "Return t if the cmd is an solution-action and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'solution-action-cmdp))

(defun hpp_why_wrong_objp_solution_action_pairp (Stack)
  "Return t if the cmd is an solution-action and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'solution-action-cmdp))

;;; -----------------------------------------------------------------------
;;; Incorrect-Solution-Action Instances 
;;; These tests handle incorrect-solution-action commands and entries of specific types.

(defun hpp_get_proc_help_incorrect_solution_action_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-action and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'incorrect-solution-action-cmdp))

(defun hpp_why_wrong_cmdp_incorrect_solution_action_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-action and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'incorrect-solution-action-cmdp))

(defun hpp_why_wrong_eqnp_incorrect_solution_action_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-action and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'incorrect-solution-action-cmdp))

(defun hpp_why_wrong_objp_incorrect_solution_action_pairp (Stack)
  "Return t if the cmd is an incorrect-solution-action and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'incorrect-solution-action-cmdp))

;;; -----------------------------------------------------------------------
;;; Correct-Solution-Action Instances 
;;; These tests handle correct-solution-action commands and entries of specific types.

(defun hpp_get_proc_help_correct_solution_action_pairp (Stack)
  "Return t if the cmd is an correct-solution-action and the helpstack is next-step-help capped."
  (hpp_get_proc_help_pairp Stack #'correct-solution-action-cmdp))

(defun hpp_why_wrong_cmdp_correct_solution_action_pairp (Stack)
  "Return t if the cmd is an correct-solution-action and the helpstack is why-wrong-cmdp capped."
  (hpp_why_wrong_cmd_pairp Stack #'correct-solution-action-cmdp))

(defun hpp_why_wrong_eqnp_correct_solution_action_pairp (Stack)
  "Return t if the cmd is an correct-solution-action and the helpstack is why-wrong-eqnp capped."
  (hpp_why_wrong_eqn_pairp Stack #'correct-solution-action-cmdp))

(defun hpp_why_wrong_objp_correct_solution_action_pairp (Stack)
  "Return t if the cmd is an correct-solution-action and the helpstack is why-wrong-objp capped."
  (hpp_why_wrong_obj_pairp Stack #'correct-solution-action-cmdp))



;;; ----------------------------------------------------------------------
;;; HPP-Stack-Tests
;;; The code in this section includes the full list of the hpp stack 
;;; tests this is done to make the testset definition easier to read.
(defparameter **hpp-stack-tests**
    (append **hpp-general-stack-tests**
	    **hpp-answer-stack-tests**
	    **hpp-entry-stack-tests**
	    **hpp-eq-entry-stack-tests**
	    **hpp-noneq-entry-stack-tests**
	    **hpp-assertion-stack-tests**
	    **hpp-solution-state-change-stack-tests**
	    **hpp-solution-action-stack-tests**))










#| Moved to SetDefs.cl
;;;; =====================================================================
;;;; TestSet
;;;; The testset definition that we will use for these predicates.
;;;; This testset will be stored in the parameter **Stack-TestSet**
;;;; and can be loaded as necessary.  

;;;; --------------------------------------------------------------------
;;;; The top-level stack tests parameter.
(defparameter **Stack-Test-list**
    (append **Bottom-out-hint-tests**
	    **State-Stack-Tests**
	    **Command-Stack-Tests**
	    **hpp-stack-tests**
	    ))

;;;; --------------------------------------------------------------------
;;;; Execution Function
;;;; This is the function that will be used to execute the stack tests at
;;;; runtime and supports the stack functions.  
;;;;
;;;; START HERE to finish.
(defun execute-stack-tests (Tests Command Stack &optional (Results Nil))
  (let ((R (if Results Results (make-list (length Tests) :initial-element 0))))
    (when Stack
      (dotimes (N (length Tests))
	(if (funcall (nth N Tests) Stack)
	    (incf (nth N R)))))))


;;;; ----------------------------------------------------------------------
;;;; The set itself.
(defparameter **Stack-TestSet**
    (make-testset
     :Name 'Stack_Tests
     :Tests **Stack-Test-List**
     :Executor 'Execute-Stack-Tests
     :ResultType "INTEGER"
     :ExternalType ':int
     :ResultCombiner 'sum-lists             ;; Combine using addition.
     :InitResults #'(lambda (Set) 
		      (make-list (length (Testset-Tests Set)) 
				 :initial-element 0))
     ))
|#









