#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ProcDepth.cl
;;; Collin Lynch 
;;; 3/28/2003
;;;
;;; The code in this file is intended to test the processing
;;; depth that the students went to when reading help.  In 
;;; order to asess how much impact help did or did not have
;;; on the students we need to know just how "deeply" they 
;;; read the help and how that varied by type of help, whether
;;; or not they bottomed out the help, and when they were 
;;; doing it within the semester. 
;;;
;;; The oredicates in this testset record the number of times
;;; that specific processing types occurred.  The first section
;;; deals with individual hints.  The second deals with the 
;;; hint stacks as whole units.
;;;
;;; Note:: the return value for these tests are integers.  The 
;;; execution function will deal with ensuring that there is
;;; at least one hint to test.
|#


;;;; =================================================================
;;;; Individual Hints.
;;;; The predicates in this section are used to track how the students
;;;; read each individual hint on their own.  These tests will return 
;;;; t when the current hint (defined as the show-hint-cmd located 
;;;; at the second position in the stack) meets the specified criteria.


(defun Fast_hint_Count (Stack)
  "Return t iff the current hint is fast."
  (fast-hint-stackp Stack))


(defun Slow_hint_Count (Stack)
  "Return t iff the current hint is fast."
  (fast-hint-stackp Stack))



;;; Bottom-Out-Hints
;;; Note that this code embraces those hints that are "bottom out"
;;; I.E. were preceeded by some sort of hint stack not all tail hints.
;;;
;;; NOTE:: For easy re-use these predicates have been defined s.t the 
;;;  return value if t will be the help stack that preceeds them.

(defun fast_bottom_out_Count (Stack)
  "Count the fast bottom-out-hints."
  (and (fast-hint-stackp Stack)
       (bottom-out-hint-Stackc (cdr Stack))))


(defun slow_bottom_out_Count (Stack)
  "Count the fast bottom-out-hints."
  (and (slow-hint-stackp Stack)
       (bottom-out-hint-Stackc (cdr Stack))))
       


;;; tail-non-bottom-out-hints
;;; This code embraces those hints that are tail hints but not 
;;; bottom-out hints and returns the speed of them.
;;; 
;;; For re-use purposes this returns the hint itself in a list 
;;; for classification purposes if necessary.

(defun fast_tail_nbo_Count (Stack)
  "Count the fast non-bottom-out tail hints."
  (when (and (fast-hint-stackp Stack)
	     (tail-hint-cmdp (nth 1 Stack))
	     (not (bottom-out-hint-stackc (cdr Stack))))
    (list (nth 1 Stack))))


(defun slow_tail_nbo_Count (Stack)
  "Count the slow non-bottom-out tail hints."
  (when (and (slow-hint-stackp Stack)
	     (tail-hint-cmdp (nth 1 Stack))
	     (not (bottom-out-hint-stackc (cdr Stack))))
    (list (nth 1 Stack))))



;;; non-tail-hints
;;; This code deals with non-tail hints if successful it will return
;;; the help stack that preceeds the hint if not it will return nil.

(defun fast_non_tail_Count (Stack)
  "Count the fast non-tail hints."
  (and (fast-hint-stackp Stack)
       (not (tail-hint-cmdp (nth 1 Stack)))
       (help-stackc (cdr Stack))))


(defun slow_non_tail_Count (Stack)
  "Count the slow non-tail hints."
  (and (slow-hint-stackp Stack)
       (not (tail-hint-cmdp (nth 1 Stack)))
       (help-stackc (cdr Stack))))



(defparameter **PD-Hint-Classifiers**
    '(fast_hint_count
      Slow_hint_Count
      fast_bottom_out_Count
      slow_bottom_out_Count
      fast_tail_nbo_Count
      slow_tail_nbo_Count
      fast_non_tail_Count
      slow_non_tail_Count))




;;; ----------------------------------------------------------------
;;; All Hints by type.
;;; This code is intended to classify the hints by their type and to
;;; return the speed of the bottommost hint.  It makes not attempt
;;; to classify the hint as bottom-out or not 
;;;
;;; It makes use of the help-stackc code t return the topmost elements
;;; in the list as necessary.  

(defun FAST_NSH_Hint_Count (Stack)
  "Return the number of fast NSH embedded hints."
  (and (fast-hint-stackp Stack)
       (get-proc-help-Stackp (cdr Stack))))


(defun FAST_WWH_Hint_Count (Stack)
  "Return the number of fast WWH embedded hints."
  (and (fast-hint-stackp Stack)
       (why-wrong-Stackp (cdr Stack))))


(defun FAST_WWO_Hint_Count (Stack)
  "Return the number of fast WWO embedded hints."
  (and (fast-hint-stackp Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))


(defun FAST_WWE_Hint_Count (Stack)
  "Return the number of fast WWE embedded hints."
  (and (fast-hint-stackp Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))

(defun Fast_Unsol_Hint_Count (Stack)
  "Return the number of fast Unsol embedded hints."
  (and (fast-hint-stackp Stack)
       (Unsolicited-help-Stackp (cdr Stack))))



(defun Slow_NSH_Hint_Count (Stack)
  "Return the number of slow NSH embedded hints."
  (and (Slow-hint-stackp Stack)
       (get-proc-help-Stackp (cdr Stack))))


(defun Slow_WWH_Hint_Count (Stack)
  "Return the number of slow WWH embedded hints."
  (and (Slow-hint-stackp Stack)
       (why-wrong-Stackp (cdr Stack))))


(defun Slow_WWO_Hint_Count (Stack)
  "Return the number of slow WWO embedded hints."
  (and (Slow-hint-stackp Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))


(defun slow_WWE_Hint_Count (Stack)
  "Return the number of slow WWE embedded hints."
  (and (slow-hint-stackp Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))

(defun Slow_Unsol_Hint_Count (Stack)
  "Return the number of Slow Unsol embedded hints."
  (and (Slow-hint-stackp Stack)
       (Unsolicited-help-Stackp (cdr Stack))))


(defparameter **PD-Hint-Type-Classifiers**
    '(FAST_NSH_Hint_Count
      FAST_WWH_Hint_Count
      FAST_WWO_Hint_Count
      FAST_WWE_Hint_Count
      Fast_Unsol_Hint_Count
      
      Slow_NSH_Hint_Count
      Slow_WWH_Hint_Count
      Slow_WWO_Hint_Count
      slow_WWE_Hint_Count 
      Slow_Unsol_Hint_Count))

      
;;; ----------------------------------------------------------------
;;; Bottom-out-Hints by type
;;; the predicates below return t for the specified types but only
;;; on bottom-out-hints.

(defun FAST_NSH_Bottom_Out_Hint_Count (Stack)
  "Return the number of fast NSH Bottom Out hints."
  (and (fast_bottom_out_count Stack)
       (get-proc-help-Stackp (cdr Stack))))

(defun Slow_NSH_Bottom_Out_Hint_Count (Stack)
  "Return the number of slow NSH Bottom Out hints."
  (and (Slow_bottom_out_count Stack)
       (get-proc-help-Stackp (cdr Stack))))



(defun FAST_WWH_Bottom_Out_Hint_Count (Stack)
  "Return the number of fast WWH bottom out hints."
  (and (fast_bottom_out_count Stack)
       (why-wrong-Stackp (cdr Stack))))

(defun Slow_WWH_Bottom_Out_Hint_Count (Stack)
  "Return the number of slow WWH bottom out hints."
  (and (slow_bottom_out_count Stack)
       (why-wrong-Stackp (cdr Stack))))



(defun FAST_WWO_Bottom_Out_Hint_Count (Stack)
  "Return the number of fast WWO bottom out hints."
  (and (fast_bottom_out_count Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))

(defun Slow_WWO_Bottom_Out_Hint_Count (Stack)
  "Return the number of slow WWO bottom out hints."
  (and (slow_bottom_out_count Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))



(defun FAST_WWE_Bottom_Out_Hint_Count (Stack)
  "Return the number of fast WWE bottom out hints."
  (and (fast_bottom_out_count Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))

(defun slow_WWE_Bottom_Out_Hint_Count (Stack)
  "Return the number of slow WWE bottom out hints."
  (and (slow_bottom_out_count Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))



(defun Fast_Unsol_Bottom_Out_Hint_Count (Stack)
  "Return the number of fast Unsol bottom out hints."
  (and (fast_bottom_out_count Stack)
       (Unsolicited-help-Stackp (cdr Stack))))

(defun Slow_Unsol_Bottom_Out_Hint_Count (Stack)
  "Return the number of Slow Unsol bottom out hints."
  (and (slow_bottom_out_count Stack)
       (Unsolicited-help-Stackp (cdr Stack))))


(defparameter **PD-Bottom-Out-Types**
    '(FAST_NSH_Bottom_Out_Hint_Count
      Slow_NSH_Bottom_Out_Hint_Count
      FAST_WWH_Bottom_Out_Hint_Count
      Slow_WWH_Bottom_Out_Hint_Count
      FAST_WWO_Bottom_Out_Hint_Count
      Slow_WWO_Bottom_Out_Hint_Count
      FAST_WWE_Bottom_Out_Hint_Count
      slow_WWE_Bottom_Out_Hint_Count
      Fast_Unsol_Bottom_Out_Hint_Count
      Slow_Unsol_Bottom_Out_Hint_Count))
      
;;; ----------------------------------------------------------------
;;; tail-Hints by type
;;; the predicates below return t for the specified types but only
;;; on non-bottom-out tail hints.

(defun FAST_NSH_Tail_NBO_Hint_Count (Stack)
  "Return the number of fast NSH Tail-Non-Bottom-Out hints."
  (and (fast_tail_nbo_count Stack)
       (get-proc-help-Stackp (cdr Stack))))

(defun Slow_NSH_Tail_NBO_Hint_Count (Stack)
  "Return the number of slow NSH Tail-Non-Bottom-Out hints."
  (and (slow_tail_nbo_count Stack)
       (get-proc-help-Stackp (cdr Stack))))



(defun FAST_WWH_Tail_NBO_Hint_Count (Stack)
  "Return the number of fast WWH tail-non-bottom-out hints."
  (and (fast_tail_nbo_count Stack)
       (why-wrong-Stackp (cdr Stack))))

(defun Slow_WWH_Tail_NBO_Hint_Count (Stack)
  "Return the number of slow WWH tail-non-bottom-out hints."
  (and (slow_tail_nbo_count Stack)
       (why-wrong-Stackp (cdr Stack))))



(defun FAST_WWO_Tail_NBO_Hint_Count (Stack)
  "Return the number of fast WWO tail-non-bottom-out hints."
  (and (fast_tail_nbo_count Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))

(defun Slow_WWO_Tail_NBO_Hint_Count (Stack)
  "Return the number of slow WWO tail-non-bottom-out hints."
  (and (slow_tail_nbo_count Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))



(defun FAST_WWE_Tail_NBO_Hint_Count (Stack)
  "Return the number of fast WWE tail-non-bottom-out hints."
  (and (fast_tail_nbo_count Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))

(defun slow_WWE_Tail_NBO_Hint_Count (Stack)
  "Return the number of slow WWE tail-non-bottom-out hints."
  (and (slow_tail_nbo_count Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))



(defun Fast_Unsol_Tail_NBO_Hint_Count (Stack)
  "Return the number of fast Unsol tail-non-bottom-out hints."
  (and (fast_tail_nbo_count Stack)
       (Unsolicited-help-Stackp (cdr Stack))))

(defun Slow_Unsol_Tail_NBO_Hint_Count (Stack)
  "Return the number of Slow Unsol tail-non-bottom-out hints."
  (and (slow_tail_nbo_count Stack)
       (Unsolicited-help-Stackp (cdr Stack))))


(defparameter **PD-Tail-NBO-Types**
    '(FAST_NSH_Tail_NBO_Hint_Count
      Slow_NSH_Tail_NBO_Hint_Count
      FAST_WWH_Tail_NBO_Hint_Count
      Slow_WWH_Tail_NBO_Hint_Count
      FAST_WWO_Tail_NBO_Hint_Count
      Slow_WWO_Tail_NBO_Hint_Count
      FAST_WWE_Tail_NBO_Hint_Count
      slow_WWE_Tail_NBO_Hint_Count
      Fast_Unsol_Tail_NBO_Hint_Count
      Slow_Unsol_Tail_NBO_Hint_Count))


;;; ----------------------------------------------------------------
;;; non-tail-Hints by type
;;; the predicates below return t for the specified types but only
;;; on non-tail-hints.

(defun FAST_NSH_Non_tail_hint_Count (Stack)
  "Return the number of fast NSH Non-Tail hints."
  (and (fast_non_tail_count Stack)
       (get-proc-help-Stackp (cdr Stack))))

(defun Slow_NSH_Non_tail_hint_Count (Stack)
  "Return the number of slow NSH Non-Tail hints."
  (and (slow_non_tail_count Stack)
       (get-proc-help-Stackp (cdr Stack))))



(defun FAST_WWH_Non_tail_hint_Count (Stack)
  "Return the number of fast WWH non-tail hints."
  (and (fast_non_tail_count Stack)
       (why-wrong-Stackp (cdr Stack))))

(defun Slow_WWH_Non_tail_hint_Count (Stack)
  "Return the number of slow WWH non-tail hints."
  (and (slow_non_tail_count Stack)
       (why-wrong-Stackp (cdr Stack))))



(defun FAST_WWO_Non_tail_hint_Count (Stack)
  "Return the number of fast WWO non-tail hints."
  (and (fast_non_tail_count Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))

(defun Slow_WWO_Non_tail_hint_Count (Stack)
  "Return the number of slow WWO non-tail hints."
  (and (slow_non_tail_count Stack)
       (why-wrong-Obj-Stackp (cdr Stack))))



(defun FAST_WWE_Non_tail_hint_Count (Stack)
  "Return the number of fast WWE non-tail hints."
  (and (fast_non_tail_count Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))

(defun slow_WWE_Non_tail_hint_Count (Stack)
  "Return the number of slow WWE non-tail hints."
  (and (slow_non_tail_count Stack)
       (why-wrong-Eqn-Stackp (cdr Stack))))



(defun Fast_Unsol_Non_tail_hint_Count (Stack)
  "Return the number of fast Unsol non-tail hints."
  (and (fast_non_tail_count Stack)
       (Unsolicited-help-Stackp (cdr Stack))))

(defun Slow_Unsol_Non_tail_hint_Count (Stack)
  "Return the number of Slow Unsol non-tail hints."
  (and (slow_non_tail_count Stack)
       (Unsolicited-help-Stackp (cdr Stack))))


(defparameter **PD-Non-Tail-Types**
    '(FAST_NSH_Non_Tail_Hint_Count
      Slow_NSH_Non_Tail_Hint_Count
      FAST_WWH_Non_Tail_Hint_Count
      Slow_WWH_Non_Tail_Hint_Count
      FAST_WWO_Non_Tail_Hint_Count
      Slow_WWO_Non_Tail_Hint_Count
      FAST_WWE_Non_Tail_Hint_Count
      slow_WWE_Non_Tail_Hint_Count
      Fast_Unsol_Non_Tail_Hint_Count
      Slow_Unsol_Non_Tail_Hint_Count))




;;;; ============================================================
;;;; Stack Classification.
;;;; In the interests of determining how the students processed the help
;;;; overall we want to identify how they proceeded through each stack
;;;; in particular we want to identify specific patterns of reading
;;;; such as reading all of the stack fast save for the last few entries
;;;; or doing the opposite.  The code in this section classifies the 
;;;; stacks themselves and attempts to asess their berhaviors.

;;; --------------------------------------------------------------
;;; Independent stack.
;;; This is a simple measure of the number of nested stacks that the
;;; student encountered or completed.  These will be used for rate
;;; calculations.

(defun indy_nested_stack_count (Stack)
  (independent-nested-helpstackc Stack))


;;; -------------------------------------------------------------
;;; Fast-To-Last
;;; Given a complete hintstack (that is a stack s.t. it contains more
;;; than one hint, and the student requested no more hints after the 
;;; stack was called.  Return t if all the hints but the final one 
;;; rate as fast hints.  
;;;
;;; Valid if:
;;; 1. The stack has an independent-nested-helpstack
;;; 2. The last hint in the stack is slow.
;;; 3. The previous hints are fast.
;;;    This is tested using the timing code which ignores the last cmd.

;;; First test if the stack is timable,  Then test to see if the 
;;; tail hint is timable 

(defun Fast_to_last_stack_count (Stack)
  (when (timable-hint-stackp Stack)
    (let ((S (independent-nested-helpstackc Stack)))
      (when S (match-fast-hint-stackc S)))))


;;; ----------------------------------------------------------------
;;; Fast-First
;;; Given a complete-hintstack return t if the first n hints 
;;; where n is > 1 in the stack are fast and the remaining n are
;;; slow.  
;;;
;;; 1. The stack has an independent nested helpstack.
;;; 2. The last n hints in it are slow where n > 1.
;;; 3. The previous m hints in it are fase where m > 1.



(defun fast_first_stack_count (Stack)
  (when (timable-hint-stackp Stack)
    (let ((S (independent-nested-helpstackc Stack)))
      (match-fast-hint-stackc
       (match-minimum-slow-hint-stackc 2 S)))))


;;; -----------------------------------------------------------------
;;; Fast-Mid
;;; Given a complete hinststack return t iff the first n hints are
;;; slow followed by m fast hints and concluding with i slow ones.

;;; Psudocode:
;;;  if the stack is timable and there is a nested independent helpstack
;;;  then:: consume at least one slow hint off the end.  
;;;         Then consume at least one fast hint
;;;         then match the remaining possible hints (till the stack cap
;;;         is reached) as slow.  
;;;         

(defun fast_mid_stack_count (Stack)
  (when (timable-hint-stackp Stack)
    (let ((S (independent-nested-helpstackc Stack)))
      (match-slow-hint-stackc
       (match-minimum-fast-hint-stackc
	1 (match-minimum-slow-hint-stackc 1 S))))))



(defparameter **PD-Stack-class-tests**
    '(indy_nested_stack_count
      fast_to_last_stack_count
      fast_first_stack_count
      fast_mid_stack_count)
  "The generic stack class tests.")


;;;; =================================================================
;;;; Bottom-out-stack-cassification.
;;;; the tests in this section subclassify the general pd-stack-class-tests
;;;; according to whether or not the final hint (the cadr cmd) is a
;;;; bottom-out-hint or not.  For documentaton on the rest of the tests
;;;; see the comments above.

(defun indy_nested_bo_stack_count (Stack)
  "Indy Stack that goes to bottom-out."
  (and (> (length Stack) 1)
       (bottom-out-hint-stackc (cdr Stack))
       (indy_nested_stack_count Stack)))

(defun fast_to_last_bo_stack_count (Stack)
  "Fast to last bottom-out-stack."
  (and (> (length Stack) 1)
       (bottom-out-hint-stackc (cdr Stack))
       (fast_to_last_stack_count Stack)))

(defun fast_first_bo_stack_count (Stack)
  "Fast first bottom-out-stack."
  (and (> (length Stack) 1)
       (bottom-out-hint-stackc (cdr Stack))
       (fast_first_stack_count Stack)))

(defun fast_mid_bo_stack_count (Stack)
  "Fast mid bottom-out-stack."
  (and (> (length Stack) 1)
       (bottom-out-hint-stackc (cdr Stack))
       (fast_mid_stack_count Stack)))


(defparameter **PD-Stack-bo-class-tests**
    '(indy_nested_bo_stack_count
      fast_to_last_bo_stack_count
      fast_first_bo_stack_count
      fast_mid_bo_stack_count)
  "The bottom-out stack class tests.")


;;;; =================================================================
;;;; tail-hint-stack-cassification.
;;;; the tests in this section subclassify the general pd-stack-class-tests
;;;; but only if the final hint is a tail hint but not a bottom-out hint.

(defun indy_nested_tail_nbo_stack_count (Stack)
  "Indy Stack that goes to bottom-out."
  (and (> (length Stack) 1)
       (tail-hint-cmdp (nth 1 Stack))
       (not (bottom-out-hint-stackc (cdr Stack)))
       (indy_nested_stack_count Stack)))

(defun fast_to_last_tail_nbo_stack_count (Stack)
  "Fast to last bottom-out-stack."
  (and (> (length Stack) 1)
       (tail-hint-cmdp (nth 1 Stack))
       (not (bottom-out-hint-stackc (cdr Stack)))
       (fast_to_last_stack_count Stack)))

(defun fast_first_tail_nbo_stack_count (Stack)
  "Fast first bottom-out-stack."
  (and (> (length Stack) 1)
       (tail-hint-cmdp (nth 1 Stack))
       (not (bottom-out-hint-stackc (cdr Stack)))
       (fast_first_stack_count Stack)))

(defun fast_mid_tail_nbo_stack_count (Stack)
  "Fast mid bottom-out-stack."
  (and (> (length Stack) 1)
       (tail-hint-cmdp (nth 1 Stack))
       (not (bottom-out-hint-stackc (cdr Stack)))
       (fast_mid_stack_count Stack)))


(defparameter **PD-Stack-tail-nbo-class-tests**
    '(indy_nested_tail_nbo_stack_count
      fast_to_last_tail_nbo_stack_count
      fast_first_tail_nbo_stack_count
      fast_mid_tail_nbo_stack_count)
  "The tail-hint stack class tests.")



;;;; =================================================================
;;;; non-tail-hint-stack-cassification.
;;;; the tests in this section subclassify the general pd-stack-class-tests
;;;; but only if the final hint is not a tail hint or bottom-out-hint

(defun indy_nested_non_tail_stack_count (Stack)
  "Indy Stack that goes to bottom-out."
  (and (> (length Stack) 1)
       (not (tail-hint-cmdp (nth 1 Stack)))
	    (indy_nested_stack_count Stack)))

(defun fast_to_last_non_tail_stack_count (Stack)
  "Fast to last bottom-out-stack."
  (and (> (length Stack) 1)
       (not (tail-hint-cmdp (nth 1 Stack)))
       (fast_to_last_stack_count Stack)))

(defun fast_first_non_tail_stack_count (Stack)
  "Fast first bottom-out-stack."
  (and (> (length Stack) 1)
       (not (tail-hint-cmdp (nth 1 Stack)))
       (fast_first_stack_count Stack)))

(defun fast_mid_non_tail_stack_count (Stack)
  "Fast mid bottom-out-stack."
  (and (> (length Stack) 1)
       (not (tail-hint-cmdp (nth 1 Stack)))
       (fast_mid_stack_count Stack)))


(defparameter **PD-Stack-non-tail-class-tests**
    '(indy_nested_non_tail_stack_count
      fast_to_last_non_tail_stack_count
      fast_first_non_tail_stack_count
      fast_mid_non_tail_stack_count)
  "The non-tail-hint stack class tests.")



;;;; ===================================================
;;;; NSH Stack-classes
;;;; The functions in this section return t if the other conditions
;;;; are met and the specified stack is a next-step-help stack.

(defun nsh_indy_nested_stack_count (Stack)
  (and (indy_nested_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))
       
(defun nsh_fast_to_last_stack_count (Stack)
  (and (fast_to_last_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_first_stack_count (Stack)
  (and (fast_first_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_mid_stack_count (Stack)
  (and (fast_mid_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))





(defun nsh_indy_nested_bo_stack_count (Stack)
  (and (indy_nested_bo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_to_last_bo_stack_count (Stack)
  (and (fast_to_last_bo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_first_bo_stack_count (Stack)
  (and (fast_first_bo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_mid_bo_stack_count (Stack)
  (and (fast_mid_bo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))




(defun nsh_indy_nested_tail_nbo_stack_count (Stack)
  (and (indy_nested_tail_nbo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_to_last_tail_nbo_stack_count (Stack)
  (and (fast_to_last_tail_nbo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_first_tail_nbo_stack_count (Stack)
  (and (fast_first_tail_nbo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_mid_tail_nbo_stack_count (Stack)
  (and (fast_mid_tail_nbo_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))




(defun nsh_indy_nested_non_tail_stack_count (Stack)
  (and (indy_nested_non_tail_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_to_last_non_tail_stack_count (Stack)
  (and (fast_to_last_non_tail_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_first_non_tail_stack_count (Stack)
  (and (fast_first_non_tail_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))

(defun nsh_fast_mid_non_tail_stack_count (Stack)
  (and (fast_mid_non_tail_stack_count Stack)
       (get-proc-help-stackp (cdr Stack))))


(defparameter **PD-NSH-Stack-class-tests**
    '(nsh_indy_nested_stack_count
      nsh_fast_to_last_stack_count
      nsh_fast_first_stack_count
      nsh_fast_mid_stack_count
      
      nsh_indy_nested_bo_stack_count
      nsh_fast_to_last_bo_stack_count
      nsh_fast_first_bo_stack_count
      nsh_fast_mid_bo_stack_count
      
      nsh_indy_nested_tail_nbo_stack_count
      nsh_fast_to_last_tail_nbo_stack_count
      nsh_fast_first_tail_nbo_stack_count
      nsh_fast_mid_tail_nbo_stack_count
      
      nsh_indy_nested_non_tail_stack_count
      nsh_fast_to_last_non_tail_stack_count
      nsh_fast_first_non_tail_stack_count
      nsh_fast_mid_non_tail_stack_count)
  "The next-step-help PD stack class tests.")


;;;; ===================================================
;;;; WWH Stack-classes
;;;; The functions in this section return t if the other conditions
;;;; are met and the specified stack is a What's Wrong Help Stack.

(defun wwh_indy_nested_stack_count (Stack)
  (and (indy_nested_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))
       
(defun wwh_fast_to_last_stack_count (Stack)
  (and (fast_to_last_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_first_stack_count (Stack)
  (and (fast_first_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_mid_stack_count (Stack)
  (and (fast_mid_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))





(defun wwh_indy_nested_bo_stack_count (Stack)
  (and (indy_nested_bo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_to_last_bo_stack_count (Stack)
  (and (fast_to_last_bo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_first_bo_stack_count (Stack)
  (and (fast_first_bo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_mid_bo_stack_count (Stack)
  (and (fast_mid_bo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))




(defun wwh_indy_nested_tail_nbo_stack_count (Stack)
  (and (indy_nested_tail_nbo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_to_last_tail_nbo_stack_count (Stack)
  (and (fast_to_last_tail_nbo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_first_tail_nbo_stack_count (Stack)
  (and (fast_first_tail_nbo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_mid_tail_nbo_stack_count (Stack)
  (and (fast_mid_tail_nbo_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))




(defun wwh_indy_nested_non_tail_stack_count (Stack)
  (and (indy_nested_non_tail_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_to_last_non_tail_stack_count (Stack)
  (and (fast_to_last_non_tail_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_first_non_tail_stack_count (Stack)
  (and (fast_first_non_tail_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))

(defun wwh_fast_mid_non_tail_stack_count (Stack)
  (and (fast_mid_non_tail_stack_count Stack)
       (why-wrong-stackp (cdr Stack))))


(defparameter **PD-wwh-Stack-class-tests**
    '(wwh_indy_nested_stack_count
      wwh_fast_to_last_stack_count
      wwh_fast_first_stack_count
      wwh_fast_mid_stack_count
      
      wwh_indy_nested_bo_stack_count
      wwh_fast_to_last_bo_stack_count
      wwh_fast_first_bo_stack_count
      wwh_fast_mid_bo_stack_count
      
      wwh_indy_nested_tail_nbo_stack_count
      wwh_fast_to_last_tail_nbo_stack_count
      wwh_fast_first_tail_nbo_stack_count
      wwh_fast_mid_tail_nbo_stack_count
      
      wwh_indy_nested_non_tail_stack_count
      wwh_fast_to_last_non_tail_stack_count
      wwh_fast_first_non_tail_stack_count
      wwh_fast_mid_non_tail_stack_count)
  "The next-step-help PD stack class tests.")


;;;; ===================================================
;;;; WWO Stack-classes
;;;; The functions in this section return t if the other conditions
;;;; are met and the specified stack is a What's Wrong Obj Stack.

(defun wwo_indy_nested_stack_count (Stack)
  (and (indy_nested_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))
       
(defun wwo_fast_to_last_stack_count (Stack)
  (and (fast_to_last_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_first_stack_count (Stack)
  (and (fast_first_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_mid_stack_count (Stack)
  (and (fast_mid_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))





(defun wwo_indy_nested_bo_stack_count (Stack)
  (and (indy_nested_bo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_to_last_bo_stack_count (Stack)
  (and (fast_to_last_bo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_first_bo_stack_count (Stack)
  (and (fast_first_bo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_mid_bo_stack_count (Stack)
  (and (fast_mid_bo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))




(defun wwo_indy_nested_tail_nbo_stack_count (Stack)
  (and (indy_nested_tail_nbo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_to_last_tail_nbo_stack_count (Stack)
  (and (fast_to_last_tail_nbo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_first_tail_nbo_stack_count (Stack)
  (and (fast_first_tail_nbo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_mid_tail_nbo_stack_count (Stack)
  (and (fast_mid_tail_nbo_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))




(defun wwo_indy_nested_non_tail_stack_count (Stack)
  (and (indy_nested_non_tail_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_to_last_non_tail_stack_count (Stack)
  (and (fast_to_last_non_tail_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_first_non_tail_stack_count (Stack)
  (and (fast_first_non_tail_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))

(defun wwo_fast_mid_non_tail_stack_count (Stack)
  (and (fast_mid_non_tail_stack_count Stack)
       (why-wrong-obj-stackp (cdr Stack))))


(defparameter **PD-wwo-Stack-class-tests**
    '(wwo_indy_nested_stack_count
      wwo_fast_to_last_stack_count
      wwo_fast_first_stack_count
      wwo_fast_mid_stack_count
      
      wwo_indy_nested_bo_stack_count
      wwo_fast_to_last_bo_stack_count
      wwo_fast_first_bo_stack_count
      wwo_fast_mid_bo_stack_count
      
      wwo_indy_nested_tail_nbo_stack_count
      wwo_fast_to_last_tail_nbo_stack_count
      wwo_fast_first_tail_nbo_stack_count
      wwo_fast_mid_tail_nbo_stack_count
      
      wwo_indy_nested_non_tail_stack_count
      wwo_fast_to_last_non_tail_stack_count
      wwo_fast_first_non_tail_stack_count
      wwo_fast_mid_non_tail_stack_count)
  "The next-step-help PD stack class tests.")


;;;; ===================================================
;;;; WWE Stack-classes
;;;; The functions in this section return t if the other conditions
;;;; are met and the specified stack is a What's Wrong Eqn Stack.

(defun wwe_indy_nested_stack_count (Stack)
  (and (indy_nested_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))
       
(defun wwe_fast_to_last_stack_count (Stack)
  (and (fast_to_last_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_first_stack_count (Stack)
  (and (fast_first_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_mid_stack_count (Stack)
  (and (fast_mid_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))





(defun wwe_indy_nested_bo_stack_count (Stack)
  (and (indy_nested_bo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_to_last_bo_stack_count (Stack)
  (and (fast_to_last_bo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_first_bo_stack_count (Stack)
  (and (fast_first_bo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_mid_bo_stack_count (Stack)
  (and (fast_mid_bo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))




(defun wwe_indy_nested_tail_nbo_stack_count (Stack)
  (and (indy_nested_tail_nbo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_to_last_tail_nbo_stack_count (Stack)
  (and (fast_to_last_tail_nbo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_first_tail_nbo_stack_count (Stack)
  (and (fast_first_tail_nbo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_mid_tail_nbo_stack_count (Stack)
  (and (fast_mid_tail_nbo_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))




(defun wwe_indy_nested_non_tail_stack_count (Stack)
  (and (indy_nested_non_tail_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_to_last_non_tail_stack_count (Stack)
  (and (fast_to_last_non_tail_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_first_non_tail_stack_count (Stack)
  (and (fast_first_non_tail_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))

(defun wwe_fast_mid_non_tail_stack_count (Stack)
  (and (fast_mid_non_tail_stack_count Stack)
       (why-wrong-eqn-stackp (cdr Stack))))


(defparameter **PD-wwe-Stack-class-tests**
    '(wwe_indy_nested_stack_count
      wwe_fast_to_last_stack_count
      wwe_fast_first_stack_count
      wwe_fast_mid_stack_count
      
      wwe_indy_nested_bo_stack_count
      wwe_fast_to_last_bo_stack_count
      wwe_fast_first_bo_stack_count
      wwe_fast_mid_bo_stack_count
      
      wwe_indy_nested_tail_nbo_stack_count
      wwe_fast_to_last_tail_nbo_stack_count
      wwe_fast_first_tail_nbo_stack_count
      wwe_fast_mid_tail_nbo_stack_count
      
      wwe_indy_nested_non_tail_stack_count
      wwe_fast_to_last_non_tail_stack_count
      wwe_fast_first_non_tail_stack_count
      wwe_fast_mid_non_tail_stack_count)
  "The why-wrong-equation PD stack class tests.")


;;;; ===================================================
;;;; Unsolicited Help Stack-classes
;;;; The functions in this section return t if the other conditions
;;;; are met and the specified stack is an unsolicited help Stack.

(defun unsol_indy_nested_stack_count (Stack)
  (and (indy_nested_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))
       
(defun unsol_fast_to_last_stack_count (Stack)
  (and (fast_to_last_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_first_stack_count (Stack)
  (and (fast_first_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_mid_stack_count (Stack)
  (and (fast_mid_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))





(defun unsol_indy_nested_bo_stack_count (Stack)
  (and (indy_nested_bo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_to_last_bo_stack_count (Stack)
  (and (fast_to_last_bo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_first_bo_stack_count (Stack)
  (and (fast_first_bo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_mid_bo_stack_count (Stack)
  (and (fast_mid_bo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))




(defun unsol_indy_nested_tail_nbo_stack_count (Stack)
  (and (indy_nested_tail_nbo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_to_last_tail_nbo_stack_count (Stack)
  (and (fast_to_last_tail_nbo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_first_tail_nbo_stack_count (Stack)
  (and (fast_first_tail_nbo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_mid_tail_nbo_stack_count (Stack)
  (and (fast_mid_tail_nbo_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))




(defun unsol_indy_nested_non_tail_stack_count (Stack)
  (and (indy_nested_non_tail_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_to_last_non_tail_stack_count (Stack)
  (and (fast_to_last_non_tail_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_first_non_tail_stack_count (Stack)
  (and (fast_first_non_tail_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))

(defun unsol_fast_mid_non_tail_stack_count (Stack)
  (and (fast_mid_non_tail_stack_count Stack)
       (unsolicited-help-stackp (cdr Stack))))


(defparameter **PD-unsol-Stack-class-tests**
    '(unsol_indy_nested_stack_count
      unsol_fast_to_last_stack_count
      unsol_fast_first_stack_count
      unsol_fast_mid_stack_count
      
      unsol_indy_nested_bo_stack_count
      unsol_fast_to_last_bo_stack_count
      unsol_fast_first_bo_stack_count
      unsol_fast_mid_bo_stack_count
      
      unsol_indy_nested_tail_nbo_stack_count
      unsol_fast_to_last_tail_nbo_stack_count
      unsol_fast_first_tail_nbo_stack_count
      unsol_fast_mid_tail_nbo_stack_count
      
      unsol_indy_nested_non_tail_stack_count
      unsol_fast_to_last_non_tail_stack_count
      unsol_fast_first_non_tail_stack_count
      unsol_fast_mid_non_tail_stack_count)
  "The why-wrong-equation PD stack class tests.")



#| Moved to SetDefs.cl
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
|#