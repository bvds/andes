#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution.cl
;; 03/06/2001
;; Collin Lynch
;; This file genereeates a solution point for a specific
;; problem by collecting all of the variables and equations
;; for the problem's bubblegraph and passing them to the
;; solver dll.  it will then return the resulting variablew
;; values in a list that can then be used to index the 
;; variables later.
;;
;; For debugging purposes the ystem also retains the ability
;; to generate eqf files which contain the same information 
;; that would otherwize be sent to the dll.  This code is 
;; located at the end of this file.

for later update.
The solver,
 *   After giving all the solutions, it lists, if any,                  *
 *      <PARTSLVV> followed by linear equations not solved numerically. *
 *      <UNSLVEQS> followed by equations not solved and not linear      *
 *      <UNSLVVARS> followed by variables used in equations but not     *
 *              solved for                                              *
 *      <UNUSEDVARS> followed by variables not used in any equation     *
 *              (and therefore not solved for)                          *
 *    if there are no UNSLVEQS or UNSLVVARSQ,
 *    we then run the solution checker. It may report
 *     <DISCREPANCIES> followed by equations with more than 100 x error bar
 *     <INCONSISTENCIES> followed by
        "Unknown overall dimension undetermined in " and equation
        "Dimensional inconsistency at subexpression " and expression
        variable name " should be nonnegative but is " value
        variable name " should be nonzero but is " value

                   

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defparameters.
(defparameter *debug-sp* nil 
  "If t debugging information on the sp will be printed to stdout.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary function definition.

(defun get-solution-point (Vars Eqns)
  "Given the problem-graph G, generate a solution point for it."
  ; in case dll unloaded at end of helpsys test session:
  (solver-initialize (strcat *Root-Path* *DLL-NAME*)) ; ensure dll loaded
  (error-test (Solver-new-problem) 'new-problem)
  (send-solution-elements (append Vars Eqns))			
  (let ((S (reverse (collect-result-vals))))
    
    (cond ((position '<Discrepancies> S) 
	   (error (concatenate 'string 
		    "Solver found numerical discrepancies in solution:~%"
		    "~A") S))
          ((position '<Inconsistencies> S)
	   (error (concatenate 'string 
		    "Solver found inconsistencies in solution:~%"
		    "~A") S))

	  ((position-if #'tagp S)
	   (sp-debug "Incomplete Solution Point returned.~%")
	   (sp-debug "~A~%" S)
	   (collect-solution S))
	  
	  (t (sp-debug "Complete Solution Point Returned.~%")
	     (collect-solution S)))))


(defun collect-solution (S)
  (list (subseq S 0 (position-if #'tagp S)) S))
    
    
(defun send-solution-elements (Elts)
  "Send the list of elements to the solver checking for errors."
  (let ((R))
    (dolist (E Elts)
      (setq R (Solver-send-problem-statement E))
      (error-test R (list 'send-statement E)))))


(defun collect-result-vals ()
  "Collect the result values up to the specified StopVal."
  (let ((S) (R (solver-solve-problem)))
    (error-test R 'Solve-problem)
    (loop while (not (null R))
	do (push R S)
	   (setq R (solver-solve-more-problem))
	   (error-test R 'Solve-Problem))
    S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging code.

(defun sp-debug (&rest form)
  (if *debug-sp* (apply 'format (cons t form))))
  
(defun trace-solution-point ()
  (trace get-solution-point)
  (trace send-solution-elements)
  (trace collect-result-vals)
  
  (trace Solver-new-problem)
  (trace solver-send-problem-statement)
  (trace solver-solve-problem)
  (trace solver-solve-more-problem))


			    	

(defun error-test (V &optional (command 'solver-command))
  "Test to determine if the returned element is an error."
  (when (or (and (listp V) (equalp (car V) 'Error))
	    (stringp V))
    (error "Error: Solver returned ~A~% to ~A" V command))
  V)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generation of eqf files.

(defparameter *Default-eqnfile-path* "c:\\andes2\\equations\\")

(defun eqfpath (Name)
  (concatenate 'string *Default-eqnfile-path* Name ".eqf"))

(defun dump-eqf (Name VarIndex EqnIndex &optional (Path *Default-eqnfile-path*))
  "Dump the Equationfile for the problem to the oprional directory."
  (let ((*Print-Pretty* nil)
        (*Print-Gensym* nil)
        (*Print-length* nil)
        (*Print-lines* nil)
	(*Print-miser-width* nil)
        (*Print-readably* nil)
	(*Print-right-margin* nil))
	

    (ensure-directories-exist Path :verbose t)
    (with-open-file (File 
		     (concatenate 'string Path Name ".eqf")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
      
      (loop for I in (qvars->svars VarIndex)
	  do (format File "~A~%" I))
      (loop for I in (Eqns->Algebra EqnIndex)
	  do (format File "~A~%" I)))))


