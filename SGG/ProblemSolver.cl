;; Problemsolver.cl
;; Collin Lynch
;; 3/24/2001
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file defines the overall problem-solving algorithm of the
;; Andes2 SGG.  The main function here is solve-problem which takes
;; a problem-struct as its input and generates values for the dynamic
;; fields (Graph, EqnIndex, VarIndex, Solutions).  This solution will
;; be based upon the header elements of the file.
;;
;; If the problem seeks quantities (the default) then the following 
;; steps will be run:
;; 1. Generate Bubblegraph:  Call the Bubblegraph-Generator code to 
;;    produce the problem-graph based upon the givens, and soughts.
;;
;; 2. Generate Indicies:  Cycle through the Bubblegraph to generate
;;    the eqnindicies and varindicies for later use.
;;
;; 3. SolutionPoint:  Generate the solution point for the system
;;    This consists of passing variables and equations to the 
;;    algebra system and obtaining the variable values if possible
;;    or returning an error if it is not.
;;
;; 4. Mark Forbidden Nodes:  Cycle through the graph marking all 
;;    nodes that are forbidden from use by the ForbiddenPSMS field.  
;;
;; 5. Generate Solutions:  Generate the problem solutions/eqn sets
;;    using all viable nodes to find the alternate problem solution
;;    possiblities.
;;
;; 6. Mark-Optimal-Path:  Select and then mark the most optimal path
;;    in the bubblegraph for later use.
;;
;; 7. Eliminate Dead-Paths:  Cycle through the graph eliminating all
;;    of the dead-path nodes from the graph.  
;;
;; If the problem does not seek quantities then the system will generate
;; a dummy graph consisting soley of psm's  Each Sought will be given its
;; own dummy PSM and the result will be a psmgraph struct containing no
;; quantities.  The help system will know how to deal with this when it
;; gets it.  
;;

(defparameter *S-print-steps* () "Print intermediate solution steps.")
(defparameter *cp* () "Current problem.")


;;;;===============================================================
;;;; Problem solutions.

(defmacro s (Pname)
  `(let ((t0 (get-internal-run-time)))
     (setq *cp* (get-problem ',Pname))
     (solve-problem *cp*)
     (format t "~&~A  ~A: ~A solution~:p found in ~,2F minutes.~%~%"
	     (print-outline-indent 0) 
	     (problem-name *cp*) (length (problem-solutions *cp*))
	     (/ (- (get-internal-run-time) t0) 
			  (* 60 internal-time-units-per-second)))
     (values))) ; don't return any value


;;;;==================================================================
;;;; Solve problem main function  
;;;; If the problem is listed as no-quant then solve generate the 
;;;; dummy graph for it.  If not then solve it normally.
(defun solve-problem (Problem)
  "Solve the specified problem returining the results."
  ;; Wipe out any existing pointers, since the
  ;; problem graph and working memory will be updated.
  (setf (problem-pointers problem) nil)
  (if (no-quant-problem-p Problem) 
	(solve-no-quant-problem Problem)
    (solve-quant-problem Problem))
  (run-post-processing Problem)
  Problem)



;;; ===================================================================
;;; Solve quantity problems
;;; Solving a quantity problem is the typical format for our
;;; system.  We go through the steps listed below in order ang
;;; produce the appropriate indicies as normal.
(defun solve-quant-problem (Problem)	
  "Solve a problem that seeks a quantity."
  (Generate-Problem-Bubblegraph Problem)
  (Generate-Problem-Indicies Problem)
  (generate-problem-Solutionpoint Problem)
  (mark-forbidden-nodes Problem)
  (generate-problem-eqn-sets Problem)
  (mark-problem-graph Problem)
  )


;; Test whether a solution point for this problem can be 
;; obtained by the rules + solver. Only goes as far as 
;; generating solution point, omitting potentially time-consuming
;; collection of eqn-sets. Mainly intended to be used for the 
;; side-effect of generating a solver.log for the step of
;; solving the problem equations, for regression testing of the solver.
;; Doesn't mark graph, so don't use graph remaining after exit.
(defun test-solve (Problem)
  (setq *cp* (get-problem Problem))
  (Generate-Problem-Bubblegraph *cp*)
  (Generate-Problem-Indicies *cp*)
  (generate-problem-Solutionpoint *cp*))


;;-------------------------------------------------------------------
;; Step 1 Generate bubblegraph.
;; This step calls the GraphGenerator code which in turn calls the 
;; system code for generating the pasm nodes in the graph.
;;
;; This step also sets the working memory of the problem for future 
;; use.  This information is stored in the problem for access at
;; a later date by the help system.

(defun Generate-Problem-BubbleGraph (Problem)
  "Generate the BubbleGraph solution for Problem."
  (ps-bp "Generating Bubblegraph: ~A" (Problem-Name Problem))
  
  (setq **wm** nil)     ;; This is an ugly hack used
                        ;; until I can find the bug and clean it.
  (setf (Problem-Graph Problem) 
	(Generate-Bubblegraph (Problem-Soughts Problem) 
			      (Problem-Givens Problem)
			      :IgnorePSMS (problem-IgnorePSMS Problem)))
  (setf (Problem-WM Problem) **wm**)                         
  (when *S-Print-Steps*
    (format t "Problem Bubblegraph: ~A~%" (Problem-Name Problem))
    (pprint (Problem-Graph Problem))
    (print-BubbleGraph (Problem-Graph Problem)))
    (Problem-Graph Problem))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;    debug functions
;;;
;;;

(defun gpb ()
  (generate-problem-bubblegraph *cp*))

(defun pg ()
  (print-bubblegraph (problem-graph *cp*)))

(defun pgf ()
  (print-bubblegraph (problem-graph *cp*) 'full))

(defun pgn (N)
  "Print the nth node of Graph G."
  (form-print-node 
   (get-nth-bgnode N (problem-graph *cp*))))



;;------------------------------------------------------------------------
;; Step 2 generate indicies. (Knowledge code)
;; This code processes the solution graph to produce the variable and 
;; equation indicies that are used by the algebra system and by the help
;; system for solution construction and processing.

(defun generate-problem-indicies (Problem)
  "Generate the eqn index and var indicies for the problem."
  (gen-prb-eqn-index Problem)
  (gen-prb-var-index Problem)
  
  (when *S-Print-Steps*
    (format t "Problem Indicies: ~A~%" (Problem-Name Problem))
    (format t "~A~%" (Problem-VarIndex Problem))
    (format t "~A~%" (Problem-EqnIndex Problem)))
  (Problem-Graph Problem))


(defun gen-prb-eqn-index (Problem)
  (ps-bp "Generating Equation Index: ~A" (Problem-Name Problem))
  (setf (Problem-EqnIndex Problem)
    (generate-bg-eindex (Problem-Graph Problem))))


(defun gen-prb-var-index (Problem)  
  (ps-bp "Generating Var Index: ~A" (Problem-Name Problem))
  (setf (Problem-VarIndex Problem)
    (generate-bg-vindex (Problem-Graph Problem) 
			:ExpMarks (problem-VariableMarks Problem))))


(defun gpi ()
  (generate-problem-indicies *cp*))

(defun pge ()
  (pprint (sort (copy-list (Problem-EqnIndex *cp*)) #'expr< :key #'eqn-exp)))

(defun pgv ()
  (pprint (Problem-VarIndex *cp*)))



;;--------------------------------------------------------------------------
;; Step 3 Solve the algebra. (SolutionPoint.cl)
;; This code passes the equations and variables generated in steps 2 and 3
;; over to the algebra subsystem in order to obtain a set of values for each
;; of the quantity variables, the solution point.  This value is used by the
;; independence testing system and the help system.
;;
;; For solution safety reasons the system will not generate solutions using
;; equations located solely within forbidden nodes.  Therefore eqns marked with
;; 'forbidden' will be ignored.
;;
;; If the problem has both qvars and eqns then start by generating the initial 
;; problem solutionpoint.  Store the resulting variable values and equation 
;; markings.  Then, test the results.  If there were unused or unsolved 
;; equations or variables in the solution then generate another solution 
;; using only the solved values.  Repeat until an acceptable solution point 
;; is found.
;;
;; Once that is found and recorded, then move on.
;;
;; At some time I plan to make this more user-friendly adding a better output 
;; for the results at each step.
(defun generate-problem-solutionpoint (Problem)
  "Get the solution point for Problem."
  (when (and (problem-varindex Problem) (problem-eqnindex Problem))
    (ps-bp "Generating Solution Point: ~A" (Problem-Name Problem))
    (let ((Results (generate-initial-problem-sp Problem)))
      (loop while (and Results 
		       (position-if #'tagp Results)
		       (not (tagp (car Results))))
	  do (setq Results (generate-nary-problem-sp Problem)))
            
      (when (or (null Results) (tagp (car results)))
	(error "Persistent Unsolved variables in Eqn.~% Unable to generate Eqn sets."))
      Results)))

 
;;; Generate a problem solution using every variable and equation
;;; that has been defined.  This may or may not succeed (very likely 
;;; not)  And store the resulting values in the vars and eqns returning 
;;; the results list.
(defun generate-initial-problem-sp (Problem)
  "Generate the initial solution point."
  (gen-problem-sp 
   Problem 
   (qvars->svars (Problem-Varindex Problem))
   (append (mapcar #'Eqn-Algebra (Problem-eqnindex Problem))
	   ;; add special equations for solver
	   (collect-solver-eqns Problem))))


;;; Generate a problem solution point using only the variables and 
;;; equations that were solved previously.
(defun generate-nary-problem-sp (Problem)
  "Generate the nth solution point."
  (gen-problem-sp 
   Problem 
   (qvars->svars (collect-solved-qvars 
		  (Problem-varindex Problem)))
   (append (mapcar #'Eqn-Algebra (collect-solved-eqns 
				  (Problem-eqnindex Problem)))
	   ;; add special equations for solver
	   (collect-solver-eqns Problem))))

;; Sometimes the solver can't solve a set of equations
;; This is a method for getting some extra equations to the Solver
(defun collect-solver-eqns (Problem)
  "Find any (solver-eqn ...) in working memory"
  (mapcar #'second (filter-expressions '(solver-eqn . ?whatever) 
				       (problem-WM Problem))))


;;; Given a problem and a set of variables and equations to 
;;; solve for, attempt to generate a solution and store the
;;; results in the problem struct.  
(defun gen-problem-sp (Problem Vars Eqns)
  "Generate and store the solution given the vars and eqns."
  (when *S-Print-Steps* 
    (ps-bp "Generating initial Solution.")
    (format t "Variables: ~%~A~%" Vars)
    (format t "Equations: ~%~A~%" Eqns))
  
  (let ((Solution (get-solution-point Vars Eqns)))
    (set-qvar-values (car Solution) (Problem-VarIndex Problem))
    (mark-unsolved-eqns (collect-unsolved-qvars 
			 (Problem-Varindex Problem))
			(Problem-EqnIndex Problem))
    (error-check-sought-solution Problem)
    (nth 1 Solution)))


;;; This is a simple test to ensure that all of the sought quantities 
;;; in the solution did receive a value.  For problems that are labeled
;;; no-quant then we presume that some, if not all, of the soughts are
;;; not quantity expressions.  Thiose qill be removed prior to testing.
;;; if, however, the problem is not labeled "no-quant" then we want to
;;; ensure that all of the soughts have a value and so the whole lot 
;;; will be tested.  If at any time we encounter a sought quantity
;;; that does not have a value then an error will be returned.
;;; 
;;; Begin by collecting all qvars for quantity soughts that do not have 
;;; a value.  If there are any then submit an error to the user.  If 
;;; not then return nil.
(defun error-check-sought-solution (Problem)
  "Test for errors in the solution after storing it in the problem."
  (let ((Err (remove-if #'qvar-value
			(match-qexps->qvars 
			 (remove-if-not #'quantity-expression-p (problem-soughts Problem))
			 (problem-varindex Problem)))))
    (when Err
      (error "~2%Some of the sought quantities did not receive a value. ~%~A~%" Err))))



(defun gps ()
  (generate-problem-solutionpoint *cp*))

(defun pspv ()
  (print-solved-problem-vars *cp*))

(defun pupv ()
  (print-unsolved-problem-vars *cp*))

(defun pupe ()
  (print-unsolved-problem-eqns *cp*))

(defun pusbn ()
  (collect-unsolved-bgnodes (Problem-Graph *cp*)))



;;----------------------------------------------------------------------------
;; 4. Mark-Forbidden Nodes. (Ontology support code.)
;; Cycle through the graph marking all of the forbidden nodes within the graph
;; itself.  This process is one of cycling through the nodes in the graph and
;; matching them to the list of ForbiddenPSMS in the problem.  If a node 
;; matches then it is defined as forbidden and is marked as such.  
;; Forbidden nodes cannot be included in any solution path and entries 
;; below them will raise an error for the students at runtime.  

(defun mark-forbidden-nodes (Problem)
  "Mark the forbidden nodes within the problem graph."
  (ps-bp "Marking Forbidden nodes: ~A" (Problem-Name Problem))
  (mapcar-bubblegraph-enodes 
   #'(lambda (e) 
       (when (exp-of-psmtype-set?
	      (enode-id E) (Problem-ForbiddenPSMS Problem))
	 (push +forbidden+ (Enode-Marks E))))
   (Problem-Graph Problem))
  
  (when *S-Print-Steps*
    (format t "Problem-ForbiddenPSMS: ~A~%" (Problem-ForbiddenPSMS Problem))
    (dolist (n (collect-forbidden-bgnodes (Problem-Graph Problem)))
      (format t "Forbidden Nodes: ~A~%" n)
      (print-full-enode n)))
  (Problem-Graph Problem))



;;-----------------------------------------------------------------------------
;; Step 5 Equation Sets.  (SolutionSets.cl)
;; After we have generated a complete solution set for the problem we must obtain the
;; set of traversals thorugh the bubblegraph.  This consists of a list of psm nodes and
;; quantity nodes that will allow a user to solve for the sought quantity(s).

(defun generate-problem-eqn-sets (Problem)
  "Collect the solution bubbles for Problem."
  (ps-bp "Generating Solution Bubbles: ~A" (Problem-Name Problem))
  (prime-solution-indy 
   (qvars->indyvars (Problem-VarIndex Problem))
   (eqns->indyeqns (Problem-EqnIndex Problem)))
  
  (setf (Problem-Solutions Problem)
    (sort-eqnsets-by-optimality
     (collect-eqn-sets 
      (collect-solutions 
       (collect-sought-qnodes (Problem-Graph Problem))
       :forbidden (collect-forbidden-bgnodes 
		   (Problem-Graph Problem))))))
  
  
  (when *S-Print-Steps*
    (format t "Problem Solution Bubbles: ~A~%" (Problem-Name Problem))
    (format t "~A~%" (Problem-Solutions Problem)))
  (Problem-Solutions Problem))


(defun gpe ()
  "Generate solutions for the current problem."
  (generate-problem-eqn-sets *cp*))


(defun pe ()
  "Print out the problems equation sets."
  (format t "Equation sets for problem: ~A~%" (problem-name *cp*))
  (dotimes (N (length (problem-solutions *cp*)))
    (print-numbered-eqnset
     N (nth N (Problem-Solutions *cp*))))
  (format t "~%"))


(defun pep (Eset)
  "Print out the specified equation set's paths."
  (print-num-eqnset-wsols Eset (nth Eset (Problem-Solutions *cp*))))

#|	  (cadr (nth Eset (problem-solutions *cp*))))
  (dolist (Path (caddr (nth Eset (problem-solutions *cp*))))
  (print-Solution Path t 0 'Interface)
  (format t "~%"))
  |#


;;---------------------------------------------------------------------
;; Setp 6 Mark the nodes.  (SolutionSets.cl)
;; Once we have generated the set of paths through the solution graph
;; we can then mark the individual nodes indicating their status as 
;; dead-path live, invalid, etc.  Thes markings are later used by 
;; the help system.

(defun mark-problem-graph (Problem)
  "Mark the problem's path."
  (ps-bp "Marking Problem Graph: ~A" (Problem-Name Problem))
  (cond ((null (Problem-Solutions Problem))
	 (bp " ** WARNING ** " 0 "No Problem Solutions defined for marking.")
	 (format t "~2%"))
	
	(t ;(pprint (problem-graph problem))
	   (mark-optimal-path Problem)
	   (mark-dead-paths Problem)
	   ;;(format t "mark-problem-graph 1 length=~A~%" 
	;;	   (length (problem-eqnindex Problem)))
	   (Remove-dead-path-eqns Problem)
	  ;; (format t "mark-problem-graph 2 length=~A~%" 
	;;	   (length (problem-eqnindex Problem)))
	   (remove-dead-path-vars Problem)
	   
	   (when *S-Print-Steps*
	     (format t "Marked Graph: ~A~%" (Problem-Name Problem))
	     (format t "~A~%" (Problem-Graph Problem))))))
	
	
(defun mark-optimal-path (Problem)
  "Mark the nodes supplied with 'Optimal-Path'."
  (mapcar #'(lambda (n)
	      (cond ((Qnode-P n)
		     (pushnew +optimal-path+ (Qnode-Marks n)))
		    ((Enode-P n)
		     (pushnew +optimal-path+ (Enode-Marks n)))))
	  
	  (get-optimal-solution-path (Problem-Solutions Problem))))
  

(defun mark-dead-paths (Problem)
  "Mark the problem's dead-paths and remove them."
  (setf (Problem-Graph Problem)
    (index-Bubblegraph 
     (remove-bg-dead-path-nodes 
      (mark-bg-dead-path-nodes 
       (Problem-Graph Problem)
       (union (collect-eqnsets->nodes (Problem-Solutions Problem))
	      (collect-forbidden-bgnodes (Problem-Graph Problem))))))))


(defun remove-dead-path-eqns (Problem)
  "Remove the dead-path eqns from the problem."
  (let ((Eqns))
    (dolist (E (Problem-EqnIndex Problem))
      (setf (Eqn-Nodes E) 
	    ;; remove based on +dead-path+ mark
	    (remove-if #'bgnode-dead-pathp (Eqn-Nodes E)))
      (when (Eqn-Nodes E)
	(push E Eqns)))
  ;;  (format t "remove-dead-path-eqns length ~A~%" (length Eqns))
    (setf (Problem-EqnIndex Problem) 
      (index-eqn-list Eqns))))
    
    
(defun remove-dead-path-vars (Problem)
  "Remove the dead-path vars from the index."
  (let ((Vars))
    (dolist (V (Problem-VarIndex Problem))
      (setf (Qvar-Nodes V)
	(remove-if #'bgnode-dead-pathp (Qvar-Nodes V)))
      (when (Qvar-Nodes V)
	(push V Vars)))
    
    (setf (Problem-VarIndex Problem) 
      (Index-Qvar-List Vars))))

;;; ============================================================
;;; Solve Non-quantity problems
;;; Solving for a non-quantity equation consists of generating
;;; a dummy graph for consisting soley of PSM nodes.  Each 
;;; non-quantity sought will be solved using the solve-for
(defparameter **solve-soughts-separate** t)

;;; Solve for the soughts if solve-soughts-separate is t then
;;; go ahead and solve them as a individually.  Else solve for 
;;; all non-quantity soughts in one big group.  If we are solving
;;; individually and one cannot be solved for then generate an 
;;; error.

(defun solve-no-quant-problem (Problem)
  "solve the non-quantity problem."
  (setf (problem-graph Problem) 
    (generate-no-quant-problem-graph 
     (problem-Soughts Problem)
     (problem-givens Problem)))
  
  (generate-problem-indicies Problem)
  (generate-problem-solutionpoint Problem)
  (mark-forbidden-nodes Problem))


;;; Given a set of non-quantity soughts and givens solve for them
;;; returning a non-quantity (PSMS only) bubblegraph and index it
;;; if necessary.
(defun generate-no-quant-problem-graph (Soughts Givens)
  "Generate a non-quantity bubblegraph."
  (let ((Graph (if **solve-soughts-separate**
		   (generate-separate-no-quant-bg Soughts Givens)
		   (generate-group-no-quant-bg Soughts Givens))))
    
    (if (null Graph) (format t "Error :no nodes generated for no-quant graph")
	(index-bubblegraph Graph))))


;;; Given a set of soughts and givens generate a bubblegraph
;;; consisting of a separate PSM node for each sought.  The 
;;; relevant enodes will be marked 'non-quant  to distinguish
;;; them.
(defun generate-separate-no-quant-bg (Soughts Givens)
  "Generate a separate no-quant bubblegraph."
  (let ((Graph (make-bubblegraph)) Q)
    (dolist (S Soughts)
      (setq Q (solve-for-non-quantity (list S) Givens))
      (when (null Q) (error "Unable to solve for non-quant sought: ~s~%" S))
      (dolist (R Q) 
	(setf Graph 
	  (add-enode-to-bubblegraph
	   (gg-qsolres->enode R :ID (Car (Qsolres-id R)) :Marks 'non-quant) 
	   Graph))))
    Graph))


;;; Generate a bubblegraph for a non-quantity problem with a single
;;; enode for all of the soughts.  The enode will be marked '(non-quant
;;; non-quant-group).  
(defun generate-group-no-quant-bg (Soughts Givens)
  (make-bubblegraph
   (mapcar #'(lambda (S) (gg-qsolres->enode S :marks '(non-quant non-quant-group)))
	   (solve-for-non-quantity Soughts Givens))))



;;;;==============================================================
;;;; File Storage.

(defun store-problem-file (Problem &optional (Path Nil))
  "Store the specified problem to a file."
  ;; generate everything "s" does as needed
  (when (not (Problem-Graph Problem))
    (eval `(s ,(Problem-name Problem))))

  ;; make pointers if they don't exist.
  (unless (problem-pointers problem) 
    (pointerize-problem problem))

  (if Path
      (write-problem-file Problem :Path Path)
    (write-problem-file Problem)))


(defun spf ()
  (store-problem-file *cp*))


(defun rpf (ProblemName)
  (setq *cp* (read-problem-file ProblemName)))


(defun def ()
  (store-problem-eqf *cp*))

(defun store-problem-eqf (Problem &optional (Path nil))
  "Store and Equation file for the specified problem."
  (when (not (Problem-Graph Problem))
    (generate-problem-bubblegraph Problem))
  
  (if Path
      (dump-eqf (string (problem-name Problem)) 
		(Problem-VarIndex Problem) 
		(Problem-EqnIndex Problem) 
		Path)
    (dump-eqf (string (problem-name Problem)) 
	      (Problem-VarIndex Problem) 
	      (Problem-EqnIndex Problem)))) 
		

(defun dump-working-eqfs ()  
  "Dump an eqf file for all 'working' problems."
  (let ((Errs)) 
    (map-problems 
     #'(lambda (P) 
	 (handler-case (progn (solve-problem P)
			      (store-problem-eqf P))	   
	   (error (E) (progn (when (probe-file (eqfpath (format nil "~A" (Problem-Name P))))
			       (delete-file (eqfpath (format nil "~A" (Problem-Name P)))))
			     (push (list (Problem-Name P) (format nil "~A" E)) Errs)))))
     #'working-problem-p)
    Errs))
  
  
(defun dump-working-prbs ()  
  "Dump a problem file for all 'working' problems."
  (let ((Errs)) 
    (map-problems 
     #'(lambda (P) 
	 (handler-case (store-problem-file P)	   
	   (error (E) (progn (when (probe-file (eqfpath (format nil "~A" (Problem-Name P))))
			       (delete-file (eqfpath (format nil "~A" (Problem-Name P)))))
			     (push (list (Problem-Name P) (format nil "~A" E)) Errs)))))
     #'working-problem-p)
    Errs))  
    
  

;;;;==================================================================
;;
;;  This is the debug print for the top level of the problem solving

(defun ps-bp (form &rest args)
  (format t "~76@<~A~;~?~>~%" (print-outline-indent 0) form args))
