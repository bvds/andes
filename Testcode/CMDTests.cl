;;;; ===============================================================================
;;;; CmdTests
;;;; Collin Lynch 
;;;; 4/16/2003 
;;;; Copyright Kurt VanLehn.
;;;;
;;;; The code in this file handles command tests.  These are predicates that test 
;;;; the individual commands and return t if the command matches some restriction.
;;;; They do not test the stack, help system state or any other factors.  
;;;;
;;;; The functions in this file link to tests in other files.  I have changed the 
;;;; names here because the Database system does not permit '-' to be present in 
;;;; column names.  Rather than modify all of the tests in other locations I have 
;;;; made the changes here.
;;;;
;;;; Each section in the file defines a list of predicates grouped by concept.
;;;; See the header at the top of the section for an explanation and the comments
;;;; on each predicate for more information.  The testset itself is defined in 
;;;; the last section of the file.  
;;;;
;;;; All of the tests in this file are predicates that take a single CMD (the 
;;;; current command) as an argument and return t or nil.  The testset results
;;;; are a list of integers indicating the number of times that the tests 
;;;; returned t when the file was executed.  
;;;;
;;;; NOTE:: All of the predicates in this file make use of tests that already
;;;;  Exist in the CMD.cl file and other files.  The code here merely wraps 
;;;;  those functions in more logical names that use '_' in lieu of '-'.

;;; ===================================================================
;;; Loading support code.
;;(load "c:/Andes2/HelpDriver/Base/cmd.cl")
;;(load "c:/Andes2/HelpDriver/Base/Htime.cl")
;;(load "c:/Andes2/HelpDriver/Base/LogTools.cl")
;;(load "c:/Andes2/HelpDriver/Base/TestSet.cl")




;;; ====================================================================
;;; Type tests
;;; These commands tes the type of each cmd.
;;;
;;; DDE commands are synchronous calls that come from the 
;;;   workbench to the helpsystem and wait for a reply.
;;;
;;; DDE-POST commands are asynchronous (recive no reply) 
;;;   and come from the workbench to the help system.
;;;
;;; DDE-COMMAND commands are asynchronous calls from the 
;;;   help system to the workbench and recive no reply.
(defparameter **Command-Type-Tests**
    '(dde_post_commandp    ;; Is this an asynchronous dde-post command?
      dde_commandp         ;; Is this a standard synchronous dde command?
      dde_command_commandp ;; is this an async dde-command call.
      ))

(defun dde_post_commandp (Command)
  "Return t if the Command is a DDE Post."
  (dde-post-cmdp Command))

(defun dde_commandp (Command)
  "Return t if the Command is a DDE."
  (dde-cmdp Command))

(defun dde_command_commandp (Command)
  "Return t iff the command is a dde-command."
  (dde-command-cmdp Command))


;;; ==================================================================
;;; Class tests
;;;
;;; ALGEBRA: commands deal with calls such as solve-for
;;;  and simplify.
;;;
;;; ANSWERS: deal with final answer submissions either in the answer
;;; field or a bring up a status-return-val.
;;;
;;; STATE: calls are used to update or alter the system state and 
;;; signify values being refrshed/set.  As dde's these calls will
;;; get a status return-val type.
;;;
;;; CONTROL: At present there is only one control command.  This 
;;; doesn't appear in our student docs so it should appear rarely.  
;;; When it does it should only appear as a dde post so no return 
;;; type should be given.
;;;
;;; NON-EQ-ENTRIES: such as assert object are dde's that post a 
;;; value to the system.  As such they get a status return val.
;;;
;;; EQN-ENTRIES: lookup-eqn string is our only eqn entry function.  If 
;;; Async-mode is 0 then this will be posted as a dde and get a 
;;; status-return-val if it is 1 then this will be posted as a dde and 
;;; we will get a EQ-Result as an asynchronous call sometime later.  
;;; This distinction will be handled by the reader below.  
;;;
;;; If async is set to 1 and a lookup-eqn-string appears as a dde then
;;; the system will throw an error.
;;;
;;; DELETIONS: remove an entry either equations or nonequations from the 
;;; screen.  Deletions are distributed as dde-posts and get no return
;;; value.  If noe is found as a dde then an error will be thrown.
;;;
;;; HELP: calls are expected to bring back hint responses to they get a
;;; Hint-return-val.
;;;
;;; UNKNOWN: For whatever reson the logs contain many api calls that 
;;; we no longer have documentation for.  These are those calls.

(defparameter **Command-class-tests**
    '(Algebra_commandp
      State_commandp
      Control_commandp
      noneq_entry_commandp
      eq_entry_commandp
      delete_commandp
      help_commandp
      Answer_commandp
      unknown_commandp
      ))


(defun algebra_commandp (Command)
  "Return t if this is an algebra command."
  (algebra-cmdp Command))

(defun state_commandp (Command)
  "Return t if this is an state command."
  (state-cmdp Command))

(defun control_commandp (Command)
  "Return t if this is a control command."
  (control-cmdp Command))

(defun noneq_entry_commandp (Command)
  "Return t if this is a noneq_entry command."
  (noneq-entry-cmdp Command))

(defun eq_entry_commandp (Command)
  "Return t if this is an eq-entry command."
  (eq-entry-cmdp Command))

(defun delete_commandp (Command)
  "Return t if this is a delete command."
  (delete-cmdp Command))

(defun help_commandp (Command)
  "Return t if this is a help command."
  (help-cmdp Command))

(defun answer_commandp (Command)
  "Return t if this is an answer command."
  (answer-cmdp Command))

(defun unknown_commandp (Command)
  "Return t if this is an unknown command."
  (unknown-cmdp Command))


;;; ===================================================================
;;; Composite class tests
;;; The composite class tests handle supersets of the regular 
;;; classes such as entry-assertion-cmdp and others.  These 
;;; are used to test for superset entries that we are interested in.

(defparameter **composite-cmd-class-tests**
    '(entry_commandp 
      solution_action_commandp  
      solution_state_change_commandp  
      assertion_commandp))


(defun entry_commandp (Command)
  "Is this command an entry?"
  (entry-cmdp Command))

(defun solution_action_commandp (Command)
  "Is this command a solution action?"
  (solution-action-cmdp Command))

(defun solution_state_change_commandp (Command)
  "Is this command an solution state change?"
  (solution-state-change-cmdp Command))

(defun assertion_commandp (Command)
  "Is this command an assertion?"
  (assertion-cmdp Command))




;;; ======================================================
;;; Specific Call Tests.
;;; These direct calls test for specific apis.  These are
;;; used to identify the specific commands that we want to
;;; keep track of.
(defparameter **command-call-tests**
    '(get_proc_help_commandp
      why_wrong_commandp
      why_wrong_obj_commandp
      why_wrong_eqn_commandp
      delete_equation_commandp
      delete_object_commandp
      ))

(defun get_proc_help_commandp (Command)
  "Is this command a Get-Proc-Help call?"
  (get-proc-help-cmdp Command))

(defun why_wrong_commandp (Command)
  "Is this command a WWH call?"
  (why-wrong-cmdp Command))
  
(defun why_wrong_obj_commandp (Command)
  "Is this a why-wrong-eqn command?"
  (why-wrong-obj-cmdp Command))

(defun why_wrong_eqn_commandp (Command)
  "Is this a why-wrong-equation command?"
  (why-wrong-eqn-cmdp Command))
  
(defun delete_equation_commandp (Command)
  "Is this a delete equation command?"
  (delete-equation-cmdp Command))
  
(defun delete_object_commandp (Command)
  "Is this a delete object command?"
  (delete-object-cmdp Command))



;;; ====================================================================
;;; State Tests
;;; Entry commands as well as some state commands such as open-problem
;;; get a result value that indicates their "status."  This status can
;;; be one of Incorrect (red), Correct (Green), or Unvalued (No color).
;;; These tests check that status and return a value indicating if the
;;; item passes.  
;;;
;;; Note that these tests only engage on commands that recieve a status
;;; return val as their result.  That is, Commands such as get-proc-help
;;; will not be flagged as unvalued even though they will not be marked
;;; red or green becuase they never recieve a value.  
(defparameter **command-state-tests**
    '(incorrect_commandp
      correct_commandp
      unvalued_commandp))

(defun incorrect_commandp (Command)
  "Is this an incorrect command?"
  (incorrect-cmdp Command))

(defun correct_commandp (Command)
  "Is this a correct command?"
  (correct-cmdp Command))

(defun unvalued_commandp (Command)
  "Is this an unvalued command?"
  (unvalued-cmdp Command))




;;; ================================================================
;;; Entry State tests.
;;; The commands in this section test the state for specific entry
;;; classes.  This allows us to make more fine-grained assessments
;;; than we typically do.
(defparameter **Entry-State-tests**
    '(incorrect_noneq_entry_commandp
      correct_noneq_entry_commandp
      unvalued_noneq_entry_commandp
      
      incorrect_eq_entry_commandp
      correct_eq_entry_commandp
      unvalued_eq_entry_commandp
      ))

(defun incorrect_noneq_entry_commandp (Command)
  "Is this an incorrect noneq entry command?"
  (incorrect-noneq-entry-cmdp Command))

(defun correct_noneq_entry_commandp (Command)
  "Is this a correct noneq entry command?"
  (correct-noneq-entry-cmdp Command))

(defun unvalued_noneq_entry_commandp (Command)
  "Is this an unvalued noneq entry command?"
  (unvalued-noneq-entry-cmdp Command))


(defun incorrect_eq_entry_commandp (Command)
  "Incorrect equation entry command?"
  (incorrect-eq-entry-cmdp Command))

(defun correct_eq_entry_commandp (Command)
  "Correct Equation Entry Command."
  (correct-eq-entry-cmdp Command))

(defun unvalued_eq_entry_commandp (Command)
  "Is this an unvalued equation entry command?"
  (unvalued-eq-entry-cmdp Command))



;;; ========================================================================
;;; Composite-action-state-tests.
;;; These tests check the state of the composite classes such as "entry"
;;; and 'solution action'.  For a more full definition of these classes
;;; see the cmd.cl file located in base.
(defparameter **Composite-cmd-class-state-tests**
    '(incorrect_entry_commandp
      correct_entry_commandp
      unvalued_entry_commandp
      
      incorrect_solution_action_commandp 
      correct_solution_action_commandp 
      unvalued_solution_action_commandp

      incorrect_solution_state_change_commandp
      correct_solution_state_change_commandp 
      unvalued_solution_state_change_commandp

      incorrect_Assertion_commandp 
      correct_Assertion_commandp 
      unvalued_Assertion_commandp))


(defun incorrect_entry_commandp (Command)
  (incorrect-entry-cmdp Command))
   
(defun correct_entry_commandp (Command)
  (correct-entry-cmdp Command))
  
(defun unvalued_entry_commandp (Command)
    (unvalued-entry-cmdp Command))
     


(defun incorrect_solution_action_commandp (Command) 
  (incorrect-solution-action-cmdp Command))
	  
(defun correct_solution_action_commandp (Command)
  (correct-solution-action-cmdp Command)) 
	    
(defun unvalued_solution_action_commandp (Command)
  (unvalued-solution-action-cmdp Command))
	      


(defun incorrect_solution_state_change_commandp (Command)
  (incorrect-solution-state-change-cmdp Command))
	
(defun correct_solution_state_change_commandp (Command)
  (correct-solution-state-change-cmdp Command)) 
	  
(defun unvalued_solution_state_change_commandp (Command)
  (unvalued-solution-state-change-cmdp Command))
	     


(defun incorrect_Assertion_commandp (Command)
  (incorrect-Assertion-cmdp Command))
	
(defun correct_Assertion_commandp (Command)
  (correct-Assertion-cmdp Command)) 
	  
(defun unvalued_Assertion_commandp (Command)
  (unvalued-Assertion-cmdp Command))
	
      
	
      
	
;;; ==============================================================
;;; Answer Entry tests.
;;; Test the state of the Answers themselves.
(defparameter **Answer-State-tests**
    '(incorrect_answer_commandp
      correct_answer_commandp
      unvalued_answer_commandp
      ))

(defun incorrect_answer_commandp (Command)
    (incorrect-answer-cmdp Command))

(defun correct_answer_commandp (Command)
    (correct-answer-cmdp Command))

(defun unvalued_answer_commandp (Command)
    (unvalued-answer-cmdp Command))



;;; =======================================================================
;;; Hint Tests
;;; The following functions test the cmd for hint results and their values.
(defparameter **Hint-Command-Tests**
    '(show_hint_commandp
      tail_hint_commandp
      unsolicited_hint_commandp))


(defun show_hint_commandp (Command)
    (show-hint-cmdp Command))

(defun tail_hint_commandp (Command)
    (tail-hint-cmdp Command))

(defun unsolicited_hint_commandp (Command)
    (unsolicited-hint-cmdp Command))



#| Now moved to SetDefs.cl
;;;; ===================================================================
;;;; TestSet definition
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


;;; This parameter contains the testset itself.  This testset
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
|#