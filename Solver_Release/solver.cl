
;;;|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;;;  solver.cl - package to provide interface to c/c++ functions for andes 
;;;  equation solving.
;;;  Copyright (C) 2001 by ????????????????????????????? - All Rights Reserved.
;;;  Author(s):
;;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;;  Kurt VanLehn (kvl)
;;;  Modified:
;;;    19 February 2001 - (lht) - created with some stubbing
;;;  Notes:
;;;
;;;    Error messages returned will always be of the form:
;;;      (Error: <functionName(args)> "description")
;;;        functionName is c-name of function
;;;        args is the string that functionName was working with
;;;        description is text that describes or implies the nature of error
;;;
;;;    (new-problem) - used to initialize solver for dealing with new problem
;;;      returns t iff all is well ... otherwise returns error message
;;;      <solveClear>
;;;      
;;;    (send-problem-statement x) - used to define problem a statement at a 
;;;      time returns t iff all is well ... otherwise returns error message
;;;      <solveAdd>
;;;      
;;;    (solve-problem) - used to acquire a solution to currently defined problem
;;;      returns a string containing the first part of the solution or an error
;;;        message
;;;      <solveBubble>
;;;	
;;;    (solve-more-problem) - used to acquire more of solution to currently de-
;;;        fined problem
;;;      returns nil if solution finished or an error message if something went
;;;        wrong ... otherwise will return the next available protion of solution
;;;      <solveMoreBubble>
;;;
;;;    (power-solve strength varName dstSlot) - used to solve for specific variable
;;;        strength -- for now always 31
;;;	varName variable to solve for --- NOTE: CASE-SENSITIVE
;;;	dstSlot -- destination to place solution equation in
;;;      returns: nil if not solution; the equation (lisp format); or an error message
;;;	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; solver routines above      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    (indyAddVar (arg) - used to provide independence checker with variable names
;;;	        their values and their units (or nil) for a group of equations
;;;      arg is of the form (varName value units) where
;;;	varName - name of variable
;;;	value - value of variable
;;;	units - 'book' form units or nil if none
;;;      returns t or error message
;;;      <c_indyAddVariable>
;;;      
;;;    (indyDoneAddVar) - used to notify checker that all variables are supplied
;;;      returns t or error message
;;;      <c_indyDoneAddVariable>
;;;      
;;;    (indyAddEquation equationID equation) - adds equations to the equation 'database'
;;;        equationID is id assigned to equation
;;;	equation is the equation
;;;      returns t or error message
;;;      <c_indyAddEquation>
;;;      
;;;    (indyEmpty) - clears all for new problem
;;;      returns t or error message
;;;      <c_indyEmpty>
;;;      
;;;    (indyAddEq2Set setID equationID) - adds an equation to a set
;;;        setID - identifies which set to add equation to
;;;	 equationID - identidies which equation to add
;;;      returns t or error message
;;;      <c_indyAddEq2Set>
;;;      
;;;    (indyKeepN setID numberToKeep) - removes all but first n entered equations
;;;                                     from set
;;;        setID - identifies which set to modify
;;;	numberToKeep - the number of equations to keep in set
;;;      returns t or error message
;;;      <c_indyKeepNOfSet>
;;;      
;;;    (isIndependent setID equationID) - used to verify equation's independence
;;;        setID - identifies which set of equations to use
;;;        equationID - identifies which equation to use
;;;      returns a three element list of the form (type list1 list2) where:
;;;        type is one of:
;;;	  0 equation is definitely independant of those in set
;;;	  1 equation might be independant of those in set but appears to be
;;;	    dependant on those in list1 and possibly those in list2 as well
;;;	    as more
;;;	  2 equation is probably dependant on some combination of list1 and
;;;	    list2
;;;	  3 equation appears to be dependant on only those in list1
;;;	  4 eqaution is dependant on those in list1
;;;	list1 indexes of the equations which the equation depends on
;;;	list2 indexes of equations that the equation may depend on  
;;;      or if an error has occurred an erro message of the form:
;;;          (Error: <functionName(functionArgs) "description")
;;;      <c_indyIsIndependant>
;;;
;;;    (studentIsIndependent setID equationID) - same as indyIndependent for stu-
;;;                                              dent equations.
;;;        setID - identifies which set of equations to use
;;;        equationID - the student slot from the workbench
;;;      otherwise the same as isIndependant above
;;;      <c_studentExpandInSet>
;;;      
;;;    (studentAddOkay equationID equation) - add equation to slot equationID after
;;;                                           ensuring 'correctness)
;;;        equationID is slot assigned to equation
;;;	equation is the equation
;;;      returns 0 is added otherwise equation is not added and return value is
;;;	      1 is invalid equationID
;;;	      2 is unable to parse
;;;	      3 is not an equation
;;;	      4 is added but not diferentiable
;;;	      5 has bad units
;;;	      6 is constant but wrong
;;;	      7 slot equationID has been emptied
;;;	    any other number is unspecified illegal
;;;	    or error message of the form:
;;;	  (Error: <functionName(functionArgs) "description")
;;;      <c_indyStudentAddEquationOkay>  
;;;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

;;;;
;;;;    Set the path of the solver by os type.
;;;;

#+MSWINDOWS (defparameter *DLL-NAME* "Solver.dll") ;name of library
#+LINUX (defparameter *DLL-NAME* "libSolver.so")

(defun solver-initialize (&optional filename)
  (let ((path (if filename filename 
		(merge-pathnames *DLL-NAME* *Andes-Path*))))
    (unless (member *DLL-NAME* (ff:list-all-foreign-libraries)
		    :key #'file-namestring :test #'string-equal)
      (format T "~&Loading solver from ~A~%" path)
      (load path))))
 
(defun solver-shutdown ()
  (let ((path (merge-pathnames *DLL-NAME* *Andes-Path*)))
    (when (member *DLL-NAME* (ff:list-all-foreign-libraries) 
		  :key #'file-namestring :test #'string-equal)
       (format T "~&UnLoading solver from ~A~%" path)
       (ff::unload-foreign-library path))))

; Ensure Lisp will read given values into double-precision floating points.
; Thus constants will have the format expected by the solver.
(setf *read-default-float-format* 'double-float)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eq slot range defined in DLL. NB: must stay in sync w/DLL!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant *solver-max-eqn-slot*     69) ; provides 70 slots numbered 0 - 69
(defconstant *solver-temp-eqn-slot* *solver-max-eqn-slot*  
  "Labelling for the temporary modification slot.")  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-do-log "solverDoLog")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun solve-do-log (a)
  (excl:native-to-string (c-solve-do-log a)))
(defun solver-logging-on (x)
  (my-read-answer (solve-do-log (format nil "~A" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-start-log "solverStartLog")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun solve-start-log (a)
  (excl:native-to-string (c-solve-start-log a)))
(defun solver-log-new-name (x)
  (my-read-answer (solve-start-log (format nil "~A" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-bubble "solveBubble")
    (:void)
  :returning :int :strings-convert t)
(defun solve-bubble ()
  (excl:native-to-string (c-solve-bubble)))
(defun solver-solve-problem ()
  (my-read-answer (solve-bubble)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-more-bubble "solveMoreBubble")
    (:void)
  :returning :int :strings-convert t)
(defun solve-more-bubble ()
  (excl:native-to-string (c-solve-more-bubble)))
(defun solver-solve-more-problem ()
  (my-read-answer (solve-more-bubble)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-add "solveAdd")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun solve-add (a)
  (excl:native-to-string (c-solve-add a)))
(defun solver-send-problem-statement (x)
  (my-read-answer (solve-add (format nil "~A" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-clear "solveClear")
    (:void)
  :returning :int :strings-convert t)
(defun solve-clear ()
  (excl:native-to-string (c-solve-clear)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun solver-new-problem ()
  (my-read-answer (solve-clear)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-is-independent "c_indyCanonHowIndy")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun indy-is-independent (arg)
  (excl:native-to-string (c-indy-is-independent arg)))
(defun solver-isIndependent (setID equationID)
  (my-read-answer (indy-is-independent 
		   (format nil "(~A ~A)" setID equationID))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-add-variable "c_indyAddVariable")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun indy-add-variable (arg)
  (excl:native-to-string (c-indy-add-variable arg)))
(defun solver-indyAddVar (arg)
  (my-read-answer (indy-add-variable (format nil "~A" arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-done-add-variable "c_indyDoneAddVariable")
    (:void)
  :returning :int :strings-convert t)
(defun indy-done-add-variable ()
  (excl:native-to-string (c-indy-done-add-variable)))
(defun solver-indyDoneAddVar ()
  (my-read-answer (indy-done-add-variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-add-equation "c_indyAddEquation")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun indy-add-equation (arg)
  (excl:native-to-string (c-indy-add-equation arg)))
(defun solver-indyAddEquation (equationID equation)
  (my-read-answer (indy-add-equation 
		   (format nil "(~A ~A)" equationID equation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-empty "c_indyEmpty")
    (:void)
  :returning :int :strings-convert t)
(defun indy-empty ()
  (excl:native-to-string (c-indy-empty)))
(defun solver-indyEmpty ()
  (my-read-answer (indy-empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-add-eq-to-set "c_indyAddEq2Set")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun indy-add-eq-to-set (arg)
  (excl:native-to-string (c-indy-add-eq-to-set arg)))
(defun solver-indyAddEq2Set (setID equationID)
  (my-read-answer (indy-add-eq-to-set 
		   (format nil "(~A ~A)" setID equationID))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-indy-keep-n-of-set "c_indyKeepNOfSet")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun indy-keep-n-of-set (arg)
  (excl:native-to-string (c-indy-keep-n-of-set arg)))
(defun solver-indyKeepN (setID numberToKeep)
  (my-read-answer (indy-keep-n-of-set 
		   (format nil "(~A ~A)" setID numberToKeep))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-student-is-independent "c_indyStudHowIndy")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun student-is-independent (arg)
  (excl:native-to-string (c-student-is-independent arg)))
(defun solver-studentIsIndependent (setID equationID)
  (my-read-answer (student-is-independent 
		   (format nil "(~A ~A)" setID equationID))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-student-is-add-okay "c_indyStudentAddEquationOkay")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun student-is-add-okay (arg)
  (excl:native-to-string (c-student-is-add-okay arg)))
(defun solver-studentAddOkay (equationID equation)
  (my-read-answer (student-is-add-okay 
		   (format nil "(~A ~A)" equationID equation))))
(defun solver-studentEmptySlot (slot)
   (solver-studentAddOkay slot "")) ;result should = 8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-student-is-okay "c_indyIsStudentEquationOkay")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun student-is-okay (arg)
  (excl:native-to-string (c-student-is-okay arg)))
(defun solver-studentIsOkay (equation)
  (my-read-answer (student-is-okay (format nil "(~A)" equation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-sub-in-one-eqn "c_subInOneEqn")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun sub-in-one-eqn (arg)
  (excl:native-to-string (c-sub-in-one-eqn arg)))
(defun solver-eqn-subst-var (assignmentID eqnID destID)
  (my-read-answer (sub-in-one-eqn 
		   (format nil "(~A ~A ~A)" assignmentID eqnID destID))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-power-solve "c_powersolve")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun solve-power-solve (arg)
  (excl:native-to-string (c-solve-power-solve arg)))
(defun solver-power-solve (strength varName slot)
  (my-read-answer (solve-power-solve 
		   (format nil "(~A ~A ~A)" strength varName slot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-solve-one-eqn "c_solveOneEqn")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun solve-one-eqn (arg)
  (excl:native-to-string (c-solve-one-eqn arg)))
(defun solver-eqn-solve (name equationSlot destinationSlot)
  (my-read-answer (solve-one-eqn 
		   (format nil "(~A ~A ~A)" name equationSlot destinationSlot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ff:def-foreign-call (c-simplify-eqn "c_simplifyEqn")
    ((string (* :char)))
  :returning :int :strings-convert t)
(defun simplify-eqn (arg)
  (excl:native-to-string (c-simplify-eqn arg)))
(defun solver-eqn-simplify (equationSlot destinationSlot)
  (my-read-answer (simplify-eqn (format nil "(~A ~A)" equationSlot destinationSlot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility routine to read as lisp string only when not an error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-read-answer (x)
  "If '(Error: <' is start return string else lisp-read."
  (if (and (>= (length x) 9)
	   (equal "Error: <" (subseq x 1 9)))
      x
    (if (= 0 (length x))
	nil
      (read-from-string x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the actual 'public' or 'exported' definitions follow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconstant **Solver-Variable-Marks** 
    '(Parameter Nonnegative Positive Nonzero answer-var)
  "The list of valid variable markings that can be sent to the solver.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a generalized algorithm for use of above
;; (progn
;;   (new-problem)
;;   for all statements of problem
;;     (send-problem-statement statement<n>)
;;   if (solve-problem) is not an error
;;     handle firstportion
;;     while (solve-more-problem) is not "nil"
;;       handle next portion
;;   else
;;     deal with error)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Given a student equation and an accuracy code (answer for the
;;; answer box; anything else for the equation box), returns NIL if
;;; the equation should be turned green and an atom if it should be
;;; turned red.  The atom indicates why the equation is wrong.  The
;;; code from the solver is an integer whose bits have the following
;;; interpretation: first two bits are 0 if the equation balances to
;;; less than the expected error (this means that the equation is okay
;;; in both the answer and equation boxes), 1 if it balances to less
;;; than 100 times the expected error, 2 if it is off by more than the
;;; expected error.  The next bit is 0 if the equation balances to
;;; less than 1% (which means it is okay in the answer box but not the
;;; equation box) and 1 if it balances to greater than 1%.  The next
;;; two bits are 0 if the units check, 1 if the student forgot units
;;; on some numbers and 2 if the student put some incorrect units on
;;; some numbers.
				       
(defun solver-equation-redp (equation &optional (accuracy 'intermediate))
  "Given a student equation and an accuracy, return Nil if the equation should 
   be turned green and an atomic error code if it should be turned red."
  (let* ((code (solver-StudentIsOkay equation))
	 (acc)(units))
    (cond ((not (numberp code))
	   ;; shouldn't get error strings from the solver anymore, 
	   ;; but just in case...
	   'solver-exception) ; could mean bad syntax
	  ((> code 31)
	   ;; solver returns 32 if equation isn't parseable.  Shouldn't happen.
	   (format T "Unparseable equation in student-eqn-redp: ~a" equation)
	   'wrong)
	  ((= 1 (setq units (truncate (/ code 8))))
	   'forgot-units)
	  ((= units 2)
	   'wrong-units)
	  ((not (= units 0))
	   (format T "Bad units code of ~a in student-eqn-redp" units)
	   'wrong-units)
	  ((= 0 (setq acc (rem code 8)))
	   ;; equation is okay for both answer and equation boxes
	   NIL)
	  ((equal accuracy 'answer)
	   ;; If better than 1% accuracy, than good enough for the answer
	   (if (< acc 4) NIL 'wrong))
	  ((< acc 4)
	   ;; if better than 1% accurate, then not good enough for the 
	   ;; equation box, but we can tell the student that it almost 
	   ;; balanced if we have nothing better to say
	   'inaccurate)
	  (T 'wrong))))
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;