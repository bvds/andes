#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SetDefs.cl
;;; Collin Lynch
;;; 8/12/2003
;;;
;;; This File contains the testset definitions that are used
;;; at log-processing time by the HelpDriver code.  These 
;;; depend upon the other Code in this directory.
|#


;;;; ===================================================================
;;;; CMDTests.cl TestSet definition  
;;;; This section compiles the tests into a single list and defines
;;;; the testset execution function.  The testset is stored in a 
;;;; global parameter that can be loaded as necessary at runtime.

(defparameter **Command-test-list**
    (append **Command-Type-Tests**
	    **Command-Class-Tests**
	    **Composite-cmd-class-tests**
	    **Command-Call-Tests**
	    **Command-state-Tests**
	    **Entry-State-Tests**
	    **Composite-cmd-class-state-tests**
	    **Answer-State-Tests**
	    **Hint-Command-Tests**
	    )
  "The Tests that are encapsulated in the **Command-Testset**")


;;; Given a command, a stack, a set of tests, and a set of results
;;; execute the tests updating the result list.  This will only call 
;;; the tests if the Command exists.
(defun Execute-Command-Tests (Tests Command Stack &optional Results)
  (declare (ignore Stack))
  (let ((R (if Results Results (make-list (length Tests) :initial-element 0))))
    (when Command
      (dotimes (N (length Tests))
	(if (funcall (nth N Tests) Command)
	    (incf (nth N R))))
      R)))


;; This parameter contains the testset itself.  This testset
;;; makes use of the commands listed above to test the values
;;; and to store the results as necessary.
(defparameter **Command-TestSet**
    (make-testset
     :Name 'Command_Tests
     :Tests **Command-Test-list**
     :Executor 'Execute-Command-Tests
     :ResultType "INTEGER"
     :ExternalType ':Int
     :ResultCombiner 'sum-lists
     :InitResults #'(lambda (Set)
		      (make-list (length (Testset-Tests Set)) 
				 :initial-element 0))
     ))


;;;; =====================================================================
;;;; EntryPair.cl TestSet
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


#|(defun tst-foo (Lst)
  (dotimes (N (length Lst))
    (dolist (Tst **EP-Test-List**)
      (when (funcall Tst (subseq Lst N))
	(format t "~a ~a~%" N Tst)))))
	|#

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



;;;; ===========================================================
;;;; The Testsets themselves.
;;;; Unlike other files this one defines three testsets.  Since
;;;; they all share the same codebase I have located them here.

(defparameter **Proc-depth-cmd-tests**
    (append **PD-Hint-Classifiers**
	    **PD-Hint-type-classifiers**
	    **PD-Bottom-Out-Types**
	    **PD-Tail-NBO-Types**
	    **PD-Non-Tail-Types**))

(defparameter **pd-cmd-TestSet**
    (make-testset
     :Name 'pd_cmd_tests
     :Tests **proc-depth-cmd-tests**
     :Executor 'Execute-proc-depth-tests
     :ResultType "INTEGER"
     :ExternalType ':int
     :ResultCombiner 'sum-lists             ;; Combine using addition.
     :InitResults #'(lambda (Set) 
		      (make-list (length (Testset-Tests Set)) 
				 :initial-element 0))
     ))




(defparameter **Proc-depth-class-tests**
    (append **PD-Stack-class-tests**
	    **PD-Stack-bo-class-tests**
	    **PD-Stack-tail-nbo-class-tests**
	    **PD-Stack-non-tail-class-tests**))

(defparameter **pd-class-TestSet**
    (make-testset
     :Name 'pd_class_tests
     :Tests **proc-depth-class-tests**
     :Executor 'Execute-proc-depth-tests
     :ResultType "INTEGER"
     :ExternalType ':int
     :ResultCombiner 'sum-lists             ;; Combine using addition.
     :InitResults #'(lambda (Set) 
		      (make-list (length (Testset-Tests Set)) 
				 :initial-element 0))
     ))





(defparameter **Proc-depth-type-tests**
    (append **PD-NSH-Stack-class-tests**
	    **PD-wwh-Stack-class-tests**
	    **PD-wwo-Stack-class-tests**
	    **PD-wwe-Stack-class-tests**
	    **PD-unsol-Stack-class-tests**))

(defparameter **pd-type-TestSet**
    (make-testset
     :Name 'pd_type_tests
     :Tests **proc-depth-type-tests**
     :Executor 'Execute-proc-depth-tests
     :ResultType "INTEGER"
     :ExternalType ':int
     :ResultCombiner 'sum-lists             ;; Combine using addition.
     :InitResults #'(lambda (Set) 
		      (make-list (length (Testset-Tests Set)) 
				 :initial-element 0))
     ))




;;;; --------------------------------------------------------------------
;;;; Execution Function
;;;; This is the function that will be used to execute the proc depth tests 
;;;; at runtime and supports the stack functions.  
;;;;
;;;; NOTE:: For simplicitie's sake this will not run any of the tests unless
;;;;        the root is a calculable hint stack. 
(defun execute-proc-depth-tests (Tests Command Stack &optional (Results Nil))
  (declare (ignore Command))
  (let ((R (if Results Results (make-list (length Tests) :initial-element 0))))
    (when (and Stack (timable-hint-stackp Stack))
      (dotimes (N (length Tests))
	(if (funcall (nth N Tests) Stack)
	    (incf (nth N R)))))))


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