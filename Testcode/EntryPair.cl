#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EntryPair.cl
;;; Collin Lynch
;;; 4/28/2003
;;;
;;; The testset in this file tracks the short-term impact of
;;; help and our assumptions about it.  The tests are designed
;;; to track how and when the students make use of help when they
;;; make errors.  For now the testsets that it employs are
;;; somewhat simplistic but these will change later.  
|#


;;;; =================================================================
;;;; Incorrect entry pairs
;;;; One of our interests is in asessing how the students did with 
;;;; respect to incorrect entries.  One thing that we want to know is
;;;; how likely they were to immediately call why-wrong-help on it.
;;;; These tests count the pairs of incorrect entry + wwh on the entry
;;;; incorrect entry + NSH, Incorrect entry + deletion of entry 
;;;; incorrect entry + modification and all incorrect entries.

;;; -------------------------------------------------------------
;;; incorrect-entry-cmdp
;;; This is a duplication

(defun ie_count (Stack)
  (incorrect-entry-cmdp (car Stack)))



;;; --------------------------------------------------------------
;;; incorrect entry + NSH
;;; Does the student follow an incorrect entry w a NSH call.

(defun ie_NSH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-entry-cmdp (nth 1 Stack))
       (get-proc-help-cmdp (nth 0 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + WWH with the same ID.
;;; Does the student immediately call WWH on the entry?

(defun ie_WWH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-entry-cmdp (nth 1 Stack))
       (why-wrong-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + deletion with the same ID.
;;; Does the student immediately delete the incorrect entry?

(defun ie_del_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-entry-cmdp (nth 1 Stack))
       (delete-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + newincorrect w same id.
;;; Does the student immediately modify the entry but still get
;;; an incorrect entry?

(defun ie_iemod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-entry-cmdp (nth 1 Stack))
       (incorrect-entry-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + correct w same id.
;;; Does the student immediately modify the entry and repair the err?

(defun ie_cmod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-entry-cmdp (nth 1 Stack))
       (correct-entry-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))

;;; --------------------------------------------------------------
;;; Incorrect entry + close-problem
;;; Does the student immediately close the problem?

(defun ie_close_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-entry-cmdp (nth 1 Stack))
       (close-problem-cmdp (nth 0 Stack))))



(defparameter **EP-General-tests**
    '(ie_count 
      ie_NSH_Count
      ie_WWH_Count
      ie_del_Count 
      ie_iemod_Count
      ie_cmod_Count
      ie_close_count)
  "The general tests irrespective of entry type.")



;;; =====================================================
;;; Eqn entry tests.
;;; -------------------------------------------------------------
;;; incorrect-eq-entry-cmdp
;;; This is a duplication

(defun ie_eq_count (Stack)
  (incorrect-eq-entry-cmdp (car Stack)))



;;; --------------------------------------------------------------
;;; incorrect entry + NSH
;;; Does the student follow an incorrect entry w a NSH call.

(defun ie_eq_NSH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-eq-entry-cmdp (nth 1 Stack))
       (get-proc-help-cmdp (nth 0 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + WWH with the same ID.
;;; Does the student immediately call WWH on the entry?

(defun ie_eq_WWH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-eq-entry-cmdp (nth 1 Stack))
       (why-wrong-eqn-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + deletion with the same ID.
;;; Does the student immediately delete the incorrect entry?

(defun ie_eq_del_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-eq-entry-cmdp (nth 1 Stack))
       (delete-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + newincorrect w same id.
;;; Does the student immediately modify the entry but still get
;;; an incorrect entry?

(defun ie_eq_iemod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-eq-entry-cmdp (nth 1 Stack))
       (incorrect-eq-entry-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + correct w same id.
;;; Does the student immediately modify the entry and repair the err?

(defun ie_eq_cmod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-eq-entry-cmdp (nth 1 Stack))
       (correct-eq-entry-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + close-problem
;;; Does the student immediately close the problem?

(defun ie_eq_close_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-eq-entry-cmdp (nth 1 Stack))
       (close-problem-cmdp (nth 0 Stack))))



(defparameter **EP-EQ-tests**
    '(ie_eq_count 
      ie_eq_NSH_Count
      ie_eq_WWH_Count
      ie_eq_del_Count 
      ie_eq_iemod_Count
      ie_eq_cmod_Count
      ie_eq_close_count)
  "The general tests irrespective of entry type.")


;;; =====================================================
;;; Obj entry tests.
;;; -------------------------------------------------------------
;;; incorrect-eq-entry-cmdp
;;; This is a duplication

(defun ie_obj_count (Stack)
  (incorrect-noneq-entry-cmdp (car Stack)))



;;; --------------------------------------------------------------
;;; incorrect entry + NSH
;;; Does the student follow an incorrect entry w a NSH call.

(defun ie_obj_NSH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-noneq-entry-cmdp (nth 1 Stack))
       (get-proc-help-cmdp (nth 0 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + WWH with the same ID.
;;; Does the student immediately call WWH on the entry?

(defun ie_obj_WWH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-noneq-entry-cmdp (nth 1 Stack))
       (why-wrong-eqn-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + deletion with the same ID.
;;; Does the student immediately delete the incorrect entry?

(defun ie_obj_del_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-noneq-entry-cmdp (nth 1 Stack))
       (delete-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + newincorrect w same id.
;;; Does the student immediately modify the entry but still get
;;; an incorrect entry?

(defun ie_obj_iemod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-noneq-entry-cmdp (nth 1 Stack))
       (incorrect-noneq-entry-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + correct w same id.
;;; Does the student immediately modify the entry and repair the err?

(defun ie_obj_cmod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-noneq-entry-cmdp (nth 1 Stack))
       (correct-noneq-entry-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + close-problem
;;; Does the student immediately close the problem?

(defun ie_obj_close_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-noneq-entry-cmdp (nth 1 Stack))
       (close-problem-cmdp (nth 0 Stack))))



(defparameter **EP-obj-tests**
    '(ie_obj_count 
      ie_obj_NSH_Count
      ie_obj_WWH_Count
      ie_obj_del_Count 
      ie_obj_iemod_Count
      ie_obj_cmod_Count
      ie_obj_close_count)
  "The general tests irrespective of entry type.")



;;; =====================================================
;;; Answer entry tests.
;;; -------------------------------------------------------------
;;; incorrect-answer-cmdp
;;; This is a duplication

(defun ans_count (Stack)
  (incorrect-answer-cmdp (car Stack)))

;;; --------------------------------------------------------------
;;; incorrect entry + NSH
;;; Does the student follow an incorrect entry w a NSH call.

(defun ans_NSH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-answer-cmdp (nth 1 Stack))
       (get-proc-help-cmdp (nth 0 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + WWH with the same ID.
;;; Does the student immediately call WWH on the entry?

(defun ans_WWH_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-answer-cmdp (nth 1 Stack))
       (why-wrong-eqn-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + deletion with the same ID.
;;; Does the student immediately delete the incorrect entry?

(defun ans_del_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-answer-cmdp (nth 1 Stack))
       (delete-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + newincorrect w same id.
;;; Does the student immediately modify the entry but still get
;;; an incorrect entry?

(defun ans_iamod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-answer-cmdp (nth 1 Stack))
       (incorrect-answer-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + correct w same id.
;;; Does the student immediately modify the entry and repair the err?

(defun ans_camod_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-answer-cmdp (nth 1 Stack))
       (correct-answer-cmdp (nth 0 Stack))
       (cmds-id-equalp (nth 0 Stack) (nth 1 Stack))))


;;; --------------------------------------------------------------
;;; Incorrect entry + close-problem
;;; Does the student immediately close the problem?

(defun ans_close_Count (Stack)
  (and (> (length Stack) 1)
       (incorrect-answer-cmdp (nth 1 Stack))
       (close-problem-cmdp (nth 0 Stack))))



(defparameter **EP-ans-tests**
    '(ans_count 
      ans_NSH_Count
      ans_WWH_Count
      ans_del_Count 
      ans_iamod_Count
      ans_camod_Count
      ans_close_count)
  "The general tests irrespective of entry type.")



#| Moved to SetDefs.cl
;;;; =====================================================================
;;;; TestSet
;;;; The testset definition that we will use for these predicates.
;;;; This testset will be stored in the parameter **Stack-TestSet**
;;;; and can be loaded as necessary.  

;;;; --------------------------------------------------------------------
;;;; The top-level stack tests parameter.
(defparameter **EP-test-list**
    (append **EP-General-tests**
	    **EP-EQ-tests**
	    **EP-obj-tests**
	    **EP-ans-tests**
	    ))

|#
#|(defun tst-foo (Lst)
  (dotimes (N (length Lst))
    (dolist (Tst **EP-Test-List**)
      (when (funcall Tst (subseq Lst N))
	(format t "~a ~a~%" N Tst)))))
	|#
#|
;;;; --------------------------------------------------------------------
;;;; Execution Function
;;;; This is the function that will be used to execute the entry pair tests
;;;; at runtime.
(defun execute-entry-pair-tests (Tests Command Stack &optional (Results Nil))
  (declare (ignore Command))
  (let ((R (if Results Results (make-list (length Tests) :initial-element 0))))
    (when Stack
      (dotimes (N (length Tests))
	(if (funcall (nth N Tests) Stack)
	    (incf (nth N R)))))))


;;;; ----------------------------------------------------------------------
;;;; The set itself.
(defparameter **Entry-Pair-TestSet**
    (make-testset
     :Name 'entry_pair_Tests
     :Tests **EP-Test-List**
     :Executor 'Execute-entry-pair-Tests
     :ResultType "INTEGER"
     :ExternalType ':int
     :ResultCombiner 'sum-lists             ;; Combine using addition.
     :InitResults #'(lambda (Set) 
		      (make-list (length (Testset-Tests Set)) 
				 :initial-element 0))
     ))

|#

