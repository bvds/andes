;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; StackProcessing.cl
;;;; Collin Lynch
;;;; 4/16/2003
;;;; Copyright Kurt VanLehn
;;;;
;;;; The functions in this file are used to process LIFO stacks of CMDs
;;;; This code has been designed to support the stack testsets and other
;;;; code for parsing the contents of stacks as they execute.  The 
;;;; individual predicates have more full explanations.  
;;;;
;;;; My goal in creating this file is to centralize commonly needed stack
;;;; predicates in order to support the creation of new test predicates
;;;; without the need to rewrite common functions.  
;;;;
;;;; The utilities will be more fully commented in their own section.
;;;; The functions in this file come in the following types.
;;;;   Predicates of the form *p:
;;;;    These return nil or some value specific to the call. 
;;;;
;;;;   Counters of the form -count:
;;;;    These return an integer of some kind.
;;;;
;;;;   Stack Collectors of the form -stackc:
;;;;    These return a stack of cmds (typically in the order that they appeared 
;;;;    in the log.  Depending upon the function definition these may contain
;;;;    many different types of commands or just one type.  
;;;;
;;;;    They may change the order of the input to the output as well.  
;;;;
;;;;   List functions -lst:  These operate on the stacks as lists and are
;;;;    guaranteed to not change their order.  
;;;;
;;;; All of these functions take Stacks (list of entries in LIFO order) as 
;;;; arguments.
;;;;




;;;; ==========================================================================
;;;; Generalized stack tests.
;;;; The code in this section is used to run generic tests on the stack such as
;;;; collecting the number of entries that pass a set predicate and so on.  

;;; ---------------------------------------------------------------------------
;;; similar uni-rep
;;; Given a stack of entries and a test function return the number of in
;;; sequence from the topmost cmd in the stack that match the test.
;;;
;;; This will be a stack in oldest-first order or nil.  It may contain only 
;;; one element.

(defun uni-test-stackc (Stack Test &optional (count 0) (Result Nil))
  "Return the number of items in the seq that match test."
  (if (and Stack (funcall test (car Stack)))
      (uni-test-stackc (cdr Stack) Test (+ 1 Count) (cons (car Stack) Result))
    Result))



;;; -----------------------------------------------------------------------
;;; Similar-multi-Rep
;;; Given a stack and a test return the maximum number of repetition Pairs.  
;;; That is iterate over the stack counting the length of the sequence of 
;;; pairs for which the test returns t without interuption.  Given a stack
;;; fo the form (a b c d)  The system will first call test(a, b) then it 
;;; will call test(b, c) and if that passes repeat.  The final count will
;;; be the number of pairs that pass in succession.
;;;
;;; The result will be a list where the firstr is the number if items in 
;;; the sequqnce defined by the relationship.  The second item in the list 
;;; is the topmost element in the sequence. 
;;;
;;; Note that the system is designed to include all items that appear in the
;;; sequence including those that have no successor.  
;;;
;;; This will return a stack in oldest-first order that will contain nil or
;;; at minimum, two items.
(defun multi-test-stackc (Stack Test)
  "Return the number of pairs that match the stack test."
  (if (and (car Stack) (cadr Stack) (funcall Test (car Stack) (cadr Stack)))
      (multi-test-stackc-cont (cdr Stack) Test (list (car Stack)))))

(defun multi-test-stackc-cont (Stack Test Result)
  "Return the number of pairs that match the stack test."
  (let ((A (Car Stack)) (B (cadr Stack)))
    (cond ((and A (null B)) (cons A Result))
	  ((and A B (funcall Test A B))
	   (multi-test-stackc-cont
	    (cdr Stack) Test (cons A Result)))
	  (t Result))))





;;; -----------------------------------------------------------------------
;;; Repeated calls of CLASS
;;; Given a class return the length of the sequence of calls in the stack
;;; whose class match the supplied class.

(defun class-Call-stackc (Stack Class &optional (Count 0) (Result Nil))
  "Get the length of the sequence of repeated calls of CLASS."
  (if (and Stack (cmd-p (car Stack)) (equalp Class (Cmd-Class (car Stack))))
      (class-call-stackc (cdr Stack) Class (+ 1 Count) (cons (car Stack) Result))
    Result))




;;; -------------------------------------------------------------------------
;;; Max Time-Diff Stackc
;;; Given a list of cmds collect the stack rotted at the base of commands
;;; that are within the threshold of time from one-another.  This is used
;;; to implement the abused searches and could be used as an alternative
;;; to the max-thresh-help-stackc.
;;;
;;; This takes a newest-first stack and returns an oldest-first stack 
;;; of at minimum two elements.
(defparameter **default-time-diff** (make-htime :Sec 30))

(defun max-time-diff-stackc (Stack &optional (Diff **default-time-diff**))
  "Return t if the cmds A and B are less than Diff (htime) apart."
  (multi-test-stackc 
   Stack 
   #'(lambda (A B) (htimes>= Diff (cmds-time-diff A B)))))


;;; -------------------------------------------------------------------------
;;; Min Time-Diff Stackc
;;; Given a list of cmds collect the stack rotted at the base of commands
;;; that are above the threshold of time from one-another.  
;;;
;;; This takes a newest-first stack and returns an oldest-first stack 
;;; of at minimum two elements.
(defun Min-time-diff-stackc (Stack &optional (Diff **default-time-diff**))
  "Return t if the cmds A and B are further than Diff (htime) apart."
  (multi-test-stackc 
   Stack 
   #'(lambda (A B) (htimes< Diff (cmds-time-diff A B)))))





;;;; ==========================================================================
;;;; Help Stacks
;;;; The code in this section is used to identify and process help stacks. 

;;; -----------------------------------------------------------------------
;;; Help-Stackc
;;; Test whether or not the topmost portion of the stack (the last N) commands
;;; are part of a help stack.  The result will be nil if the topmost command is
;;; not part of a help stack.  If it is part of a help stack then the result 
;;; will be a list where the car is the stack cap and the cdr is a list of cmds
;;; in the stack ending with the current command.  
;;;
;;; NOTE:: that the functions in this section will also be used by other 
;;; predicate calls elsewhere.
;;;
;;; NOTE:: that stacks can contain only one entry and can for now be just any
;;; entry that returns a show-hint.
;;;
;;; Help-stackc returns a stack in lifo order that is the topmost item on the
;;; stack will be the oldest cmd in it.  It takes its arguments in in reverse 
;;; order.  The stack will contain only help commands and may contain only one
;;; entry.  

;; (iterate-st-logfiles "Log/Sample/*/*/" :ResultFuncs '(help-stackc) :type ".hp")

(defun Help-Stackc (Stack &optional (Result Nil))
  (let ((CMD (car Stack)))
    (if (or (help-cmdp CMD) (help-stack-nohelp-capp CMD))
	(help-stackc-test Cmd (cdr Stack) Result 0))))


;;; Once we have a valid help command then test it by type and
;;; add it to the result or return the result.  Note that this 
;;; tests for delete equations as well before recursing.
;;; Deletions records the number of deletes max 1 per stack.
(defun help-stackc-test (CMD Stack Result Deletions)
  (cond ((help-stack-capp CMD) (cons CMD Result))
	
	((help-stack-cont-cmdp CMD)
	 (help-stackc-test 
	  (car Stack) (cdr Stack) (cons CMD Result) Deletions))
	
	; allow for an automatic equation delete notification right
	; after a help request (sent if focus leaves a deleted eqn's box)
	((and (delete-equation-cmdp CMD) 
	      (= 0 Deletions)
	      (help-cmdp (car Stack)))
	 (help-stackc-test (car Stack) (cdr Stack) Result 1))

	; [Bug 1268] Allow for an automatic deletion notification for an 
        ; earlier variable. This can be sent just after the variable definition 
	; in the case where a new variable entry redefines a previously-defined label.
	; if the entry triggers unsolicited help (e.g. for the given value), then
	; this will occur inside a hint sequence
	((delete-cmdp CMD)   ; general, allows any object deletion.  
	    (help-stackc-test (car Stack) (cdr Stack) Result 1))

	(t (warn "unexpected event within hint sequence ~S" CMD)
	   NIL)))



;;; a cmd is a help cont if it is an explain-more,
;;; a handle student response, a get-dialog-response, a
;;; why (1999 logs only) or a hint-next-substep (1999 logs).
(defun help-stack-cont-cmdp (cmd)
  (member (car (cmd-call cmd))
	  '(EXPLAIN-MORE 
	    HANDLE-STUDENT-RESPONSE
	    WHY                    ;; 1999 logs only.
	    HINT-NEXT-SUBSTEP      ;; 1999 Logs only.
	    GET-DIALOG-RESPONSE))) ;; Only in the Andes/Atlas logs.



;;; A help stack is capped (started) by a get-proc-help, 
;;; why-wrong-* help or any other entry that returns a 
;;; show hint command with a menu.
(defun help-stack-capp (cmd)
  (or (help-stack-help-capp cmd)
      (help-stack-nohelp-capp cmd)))


;;; Return the matching item name to signify what type of help 
;;; cap this is.  
(defun help-stack-help-capp (cmd)
  "If this is a why-wrong-* call or a get-proc-help call."
  (find (car (cmd-call cmd)) 
	'(get-proc-help 
	  why-wrong-object 
	  why-wrong-equation)))


;;; Return t if this is not a help call but it does result in a 
;;; show-hint that has a menu.  
(defun help-stack-nohelp-capp (cmd)
  (and (not (help-cmdp cmd))
       (show-hint-cmdp Cmd)
       (show-hint-ddr-menu 
	(cmdresult-value (cmd-result cmd)))))


;;; Trace the help stack utilities.
(defun trace-help-stack-utils ()
  (trace help-stack-cont-cmdp
	 help-stack-capp
	 help-stack-nohelp-capp))



;;;; --------------------------------------------------------
;;;; Hints by stack type.
;;;; Given a stack for which Help-Stackc returns a valid 
;;;; stack value, this code will return t depending upon
;;;; the type of the stack's cap.  

(defun get-proc-help-stackp (Stack)
  "Is this a helpstack capped with a get-proc-help call?"
  (let ((S (help-stackc Stack)))
    (when S (get-proc-help-cmdp (car S)))))

(defun Why-wrong-stackp (Stack)
  "Is this a helpstack capped with a WWH call?"
  (let ((S (help-stackc Stack)))
    (when S (why-wrong-cmdp (car S)))))

(defun Why-wrong-obj-stackp (Stack)
  "Is this a helpstack capped with a WWO call?"
  (let ((S (help-stackc Stack)))
    (when S (why-wrong-obj-cmdp (car S)))))

(defun Why-wrong-eqn-stackp (Stack)
  "Is this a helpstack capped with a WWE call?"
  (let ((S (help-stackc Stack)))
    (when S (why-wrong-eqn-cmdp (car S)))))

(defun unsolicited-help-stackp (Stack)
  "Is this a helpstack capped with an unsolicited help cmd?"
  (let ((S (help-stackc Stack)))
    (when S (unsolicited-hint-cmdp (car S)))))



;;;; --------------------------------------------------------
;;;; Bottom Out Hint-stackc
;;;; Given a cmdstack return t if the bottommost command in
;;;; the stack is a tail hint and there exists a help stack
;;;; above the current command.
(defun bottom-out-hint-stackc (Stack)
  "Is this the tail of a het-proc-help call?"
  (and (tail-hint-cmdp (car Stack))
       (help-stackc Stack)))



;;; ---------------------------------------------------------------------------
;;; Max-Diff-Help-Stackc
;;; Return t if the topmost cmd is part of a help stack and if the student 
;;; passed through it without pausing for more than a set amount of time 
;;; between each call.  If both of these conditions are met then the entry 
;;; capping the help stack will be returned  If not Nil will be returned.  
;;;
;;; This code is used to test for help abuse and can also be used to detect 
;;; points where the students are pausing for inordinately long times when
;;; reading through the help calls.
;;;
;;; The citeria for being a viable help stack are the same as those listed 
;;; above.
;;;
;;; The max-threshold helpstack takes a stack of entries in lifo order (the top is the
;;; newest entry) and returns a help stack or nil in fifo order (top is oldest).
;;; all members will be help cmds.  There is a minimum of two entries.  

;; (iterate-st-logfiles "Log/Sample/*/*/" :ResultFuncs '(max-thresh-help-stackc) :type ".hm")

;;(defparameter **max-abuse-time-diff** (make-htime :Sec 30))

;;(defun max-diff-help-stackc (Stack)
;;  (max-time-diff-stackc
;;   (reverse (help-stackc Stack))))




;;;; ------------------------------------------------------------------------
;;;; Helpstack-predicate-pairc
;;;; Given a stack and a pair of optional predicates cmd-pred and stack-pred
;;;; return t iff:
;;;;   1. the stack exists.
;;;;   2. It has at least two elements
;;;;   3. The topmost element satisfies the cmd-pred.
;;;;   4. The cdr of the stack is capped by a help stack.
;;;;   5. That help stack (in fifo order) satisfies the stack-pred.

(defun helpstack-predicate-pairp (Stack &key (Cmd-Pred #'identity) (Help-Pred #'identity))
  "Return t if the system satisfies the predicates."
  (and (car Stack) (cdr Stack)
       (funcall Cmd-Pred (car Stack))
       (let ((R (help-stackc (cdr Stack))))
	 (and R (funcall Help-Pred R)))))






;;;; ==================================================================
;;;; Hint Speed
;;;; In order to process some of the hints better we need a way to asess
;;;; how quickly the student read through the hint.  This is done by 
;;;; calculating the amount of time that the hint was "active" divided
;;;; by the length of the hint itself.  A hint is "active" from the time
;;;; that the cmd-result containing it is returned to the student to the
;;;; time that the next cmd arrived.  While this is crude it should be 
;;;; effective for our purposes.  
;;;;
;;;; The speed will be measured in terms of words per second.
;;;;
;;;; For classification purposes we want to be able to classify the 
;;;; hints into "fast" and "slow" hints.  For the purposes of these
;;;; tests I will define the average reading speed here.  If a hint 
;;;; is less than or equal to that speed it is "slow" if greater then
;;;; it is fast.  
;;;;
;;;; The reading rate is taken from Just and Carpenter's Psychology
;;;; of reading and Language Comprehension Chapter 14 Speed Reading.

(defparameter **Average-Reading-Rate** (/ 6 1) 
  "Threshold setting based upon Just and Carpenter 1980 ch14")

(defun fast-hint-stackp (Stack)
  "Was the last hint \"fast\""
  (> (calculate-hint-speed Stack) 
     **Average-Reading-Rate**))

(defun slow-hint-stackp (Stack)
  "Was the last hint \"slow\""
  (<= (calculate-hint-speed Stack) 
     **Average-Reading-Rate**))



;;; Given a stack s.t. the 2nd cmd in the stack is a show-hint cmd
;;; calculate the "reading rate of the hint.
(defun calculate-hint-speed (Stack)
  (let ((Time (htime->Secs
	       (sub-htimes (cmd-time (car Stack))
			   (cmd-time (nth 1 Stack)))))
	(Str (calc-numwords 
	      (show-hint-cmd-hintstring (nth 1 Stack)))))
    (if (= 0 Time) 0 (/ Str Time))))


;;; Given a string, calculate the number of words in the string where
;;; the string is assumed to be a list of words separated by whitespace
;;; characters and newlines.  
(defparameter **Word-Sep-chars** 
    '(#\Space #\Tab #\Return #\newline))

(defun calc-numwords (String)
  (if (member (char String (- (length String) 1)) 
	      **Word-Sep-Chars** :test #'char=)
      (calc-numwords-space-i String (- (length String) 2) 0)
    (calc-numwords-word-i String (- (length String) 2) 1)))

(defun calc-numwords-space-i (String Index Count)
  (if (= Index -1) Count
    (if (member (char String Index) **Word-Sep-Chars** :test #'char=)
	(calc-numwords-space-i String (- Index 1) Count)
      (calc-numwords-word-i String (- Index 1) (+ 1 Count)))))

(defun calc-numwords-word-i (String Index Count)
  (if (= Index -1) Count
    (if (member (char String Index) **Word-Sep-Chars** :test #'char=)
	(calc-numwords-space-i String (- Index 1) Count)
      (calc-numwords-word-i String (- Index 1) Count))))


;;; For detection purposes this code will tell us if the current 
;;; stack is s.t. the last cmd on the stack returned a hint.
(defun timable-hint-stackp (Stack)
  (and (< 1 (length Stack))
       (show-hint-cmdp (nth 1 Stack))))






;;;; ==================================================================
;;;; Independent-helpstackc
;;;; Given a stack of entries test to see if the n-1st entries form 
;;;; part of a help stack and if the bottommost entry is part of it.
;;;; If it is not then return t.  In order for a stack S to pass this
;;;; test it must be the case that:
;;;;  1. The 2nd cmd in the stack is a help-cmd.
;;;;  2. Help Stackc returns a stack on the cdr of the list.
;;;;  3. The Car of the list is either:
;;;;     a. a non-help cmd.
;;;;     b. Or a help cap cmd such as get-proc-help.
;;;;     this test is done by simply checking to see if this is 
;;;;     not a cont cmd.

;;;; NOTE:: This is a bug, this should be a *not* preceeding the 
;;;;  help-stack-cont-cmdp  dammnit.
(defun independent-nested-helpstackc (Stack)
  "Return t if the cdr of the list passes as a helpstack but the car does not."
  (when (help-cmdp (nth 1 Stack))
    (let ((S (reverse (help-stackc (cdr Stack)))))
      (when (not (help-stack-cont-cmdp (car Stack)))
	S))))



;;;; ==================================================================
;;;; Minimum hints num. 
;;;; In order to calculate the stack lengths and to ensure that sequences
;;;; of the specified types are present we need to have pattern matching 
;;;; predicates that will "consume" values from the stack.  The functions
;;;; in this section do exactly that.  Given an independent helpstack, 
;;;; they will test some aspect(s) of it and then consume whatever portions
;;;; of the stack they matched returning the rest.  Because we are treating
;;;; it as a stack it is a fifo queue.  
;;;;
;;;; Use these recursivly to test portions of the stack.  String them 
;;;; together to make explicit patterns.

;;; -------------------------------------------------------------------
;;; Match Single.
;;; Given a stack test to see if it is timable and if the last hint in 
;;; it is fast if so return the cdr of the stack if not return nil.
(defun match-single-fast-hint-stackc (Stack)
  (when (and (timable-hint-stackp Stack)
	     (fast-hint-stackp Stack))
    (cdr Stack)))


;;; Given a stack test to see if it is timable and if the last hint in 
;;; it is slow if so return the cdr of the stack if not return nil.
(defun match-single-slow-hint-stackc (Stack)
  (when (and (timable-hint-stackp Stack)
	     (slow-hint-stackp Stack))
    (cdr Stack)))


;;; -------------------------------------------------------------------
;;; Match minimum
;;; Given a minimum number of items to match in a sequence, consume 
;;; off of the list the minimum number, plus any additional ones that
;;; appear in sequence and match the criteria.  That is. given a request
;;; for two slow hints attempt to match the two in succession and then 
;;; consume any additional slow hints that appear in succession before
;;; returning the cdr of the stack.  Keep in mind that this will consume
;;; all but the cap in a slow stack.  


;;; Psudocode::
;;;  if the stack is no longer timable (cadr isn't a hint) or the 
;;;  hint does not meet the criteria then this code ends.  If it has
;;;  consumed the minimum number of elements then go ahead and return
;;;  the stack as is.  The min is signified by count being at or below
;;;  zero.  If not then return nil.
;;;
;;;  If however both requirements are met then recursively work on
;;;  the cdr of the stack with the count decremented.  

(defun match-minimum-fast-hint-stackc (count Stack)
  (if (or (not (timable-hint-stackp Stack))
	  (not (fast-hint-stackp Stack)))
      (if (< Count 1) Stack)
    (match-minimum-fast-hint-stackc (- Count 1) (cdr Stack))))

(defun match-minimum-slow-hint-stackc (count Stack)
  (if (or (not (timable-hint-stackp Stack))
	  (not (slow-hint-stackp Stack)))
      (if (< Count 1) Stack)
    (match-minimum-slow-hint-stackc (- Count 1) (cdr Stack))))


;;; ------------------------------------------------------------------
;;; match stacks
;;; given a help-stack return t if all of the hints in the stack from
;;; the cadr cmd to the stack cap (inclusive) meet the speed criteria.
;;; This requires at least one match to return t. 

;;;; Given a stack of cmds of length n where at least the last n-1 entries
;;;; constitute a help stack, return t if the all of those hints are fast.

(defun match-fast-hint-stackc (Stack)
  (when (timable-hint-stackp Stack)
    (if (= (length Stack) 2) 
	(fast-hint-stackp Stack)
      (and (fast-hint-stackp Stack)
	   (match-fast-hint-stackc (cdr Stack))))))

(defun match-slow-hint-stackc (Stack)
  (when (timable-hint-stackp Stack)
    (if (= (length Stack) 2) 
	(slow-hint-stackp Stack)
      (and (slow-hint-stackp Stack)
	   (match-slow-hint-stackc (cdr Stack))))))
  
#|(defun tst-foo (Stack Test)
  (dotimes (N (length Stack))
    (when (funcall Test (subseq Stack N))
      (format t "~a ~%" N))))
            |#
;; (format t "~a ~a~%" N (subseq Stack N)))))




;;;; ==================================================================
;;;; Utility functions.

(defun get-stack-subseq (Stack Base)
  "Get all the items between The Top of Stack and Base."
  (subseq Stack 0 (position Base Stack)))

