;;; Modifications by Anders Weinstein 2000-2008
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SolutionSets.cl
;; Collin Lynch
;; 3/14/2001
;; 
;; This file defines the equation set collection process for 
;; bubblegraphs.  The code in this section cycles through the 
;; graph collecting solution paths from the sought nodes to 
;; given nodes.  These sets are then used for traversal time 
;; help for the students.
;;----------------------------------------------------------
;; The search algorithm is as follows.
;; Define a set to be a tuple S K where:
;;   S is defined as a set of sought nodes.
;;   K is defined as a set of known nodes.
;;
;; Begin by generating a stack Q containing one set
;;  Where S = the set of problem soughts and K = nil.
;;
;; Generate a second empty stack of sets D
;;
;; Let F be a set of Forbidden Enodes.
;; 
;; While Q is not empty:
;;   Pop the top of Q into C.
;;   Let s be the top sought node in C->S
;;
;;     If s is a Qnode then:
;;        Let Es be the set of Enodes that 
;;        can be used to solve for s.
;;          For each e in Es
;;           Generate a new tuple C' where
;;              C'->S = C->S - s + e
;;              C'->K = C->K + s
;;           And push C' onto Q.
;;
;;     If s is an Enode then.
;;        If s is given then.
;;           Let C' be a new Set s.t.
;;             C'->S = C->S - e
;;             C'->K = C->K + e
;;           
;;           If C'->S is empty then
;;              Push C' onto D.
;;           Else 
;;              Push C' onto Q.
;;
;;       Else:
;;        Let Qs be the set of valid Qnodes 
;;        connected to s.
;;          Let C' be a new Set s.t.
;;            C'->S = C->S - s + Qs
;;            C'->k = C->K + s
;;          Push C' onto Q.
;;
;;---------------------------------------------------
;; An Enode E connected to a Qnode is considered valid iff:
;;  1. E is not a member of the forbidden set F.
;;  2. E is not already a membber of the current set S.
;;  3. E does not raise a nogood when coupled with the 
;;     other members of S.
;; 
;; A Qnode Q connected to an Enode is considered valid iff:
;;  


;;=============================================================================
;; Parameters

(defparameter *debug-cs* () "Controls debugging output for the collect-solutions loop.")

(defparameter *Indy-Var-Index* () "Var index (# Var Value Units) for the indy system.")
(defparameter *Indy-Eqn-Index* () "Eqn index (# Algebra) for the indy system.")

(defconstant *sgg-indy-threshold* 0 "Theshold for the is-indy test")

(defvar *indyset0-in-use* NIL "indyset 0 needs to be cleared before next use")   

;;=============================================================================
;; Solution Collection
;;
;;-----------------------------------------------------------------------------
;; Before Using the independence checker we have to 'prime' it by feeding it
;; all of the var indicies and equation indicies that we will use in order to 
;; run the independence test.  Once primed we can freely ask independence 
;; questions of the system and form sets as needed.

(defun prime-solution-indy (Vars Eqns)
  "Prime the Independence system with the vars and eqns indicies."
  (setq *Indy-Var-index* Vars)
  (setq *Indy-Eqn-Index* Eqns)
  (solver-indyEmpty)
  (setf *indyset0-in-use* NIL)

  (dolist (v Vars)
    (solver-indyAddVar V))
  (solver-indyDoneAddVar)
  
  (dolist (E Eqns)
    (apply #'solver-indyAddEquation E)))


;;=============================================================================
;; Collect-solutions (public)
;; Given the sought qnodes from the graph and an optional list of forbidden
;; nodes iteratively search the graph for viable solution sets.  This function
;; sets up the initial sought nodes described above and itewrates until the 
;; que itself is empty.  
;;
;; At each time step the system will test to see if the que is empty.  If so
;; then it resturns the results set.  Else it pops the top element off of the
;; que and processes it generating any extension elements (if any) and adding 
;; them back onto the que for further processing or adding them to the Results
;; set iff the system is done.
;;
;; If the topmost element in the que is seeking a qnode then the solution will
;; be passed to Expand-for-Q for processing.  If the topmost sought in the 
;; solution is an enode then it will be passed to Expand-for-E.
;; 
;; Arguments: Soughts: A list of sought qnodes to begin the graph.
;;
;; Returns: A set of traversal paths through the Graph containing Soughts.

(defun collect-solutions (Soughts &key (Forbidden nil))
  "Given a list of sought quantities generate a solution for them."
  (cs-action 0 "Collecting Solutions for ~A" Soughts)   ;Produce debugging info
  ;; Make the initial S and iterate.
  (do ((Que (list (make-solution :id 1 :Soughts Soughts)))  
       ;; Allocate other temp variables.
       (Results) (S) (cache) (Local) (clocal))                 
      ((null Que) Results)           ;Set the end test for a Null que.
    (setq S (pop Que))               ;pop the top Solution from the que.
    (cond ((null (Solution-Soughts S))    ;If there are no more soughts in S
	   ;; The provide debugging info for the
	   (cs-result 1 "Solution for quant ~A completed" (Solution-id S))   
	   ;; User if debugging is on.
	   (cs-list 1 (Solution-Knowns S))                  
	   (push S Results))           ;Then add the solution to results.
	  ;; If the top sought in S is a quantity.
	  ((Qnode-P (car (Solution-Soughts S)))  
	   ;; Expand as appropriate and append.
	   (setq Que (append (Expand-for-Q S Forbidden) Que))) 
	  ;; If the top sought is an Enode then
	  ((Enode-P (car (Solution-Soughts S)))
	   ;; Expand it via the Enode.
	   (setf Local (Expand-for-E S Forbidden))             
	   (setf clocal (find Local cache   ;Test to see if a matching solution
			      :test #'Solution-equalp)) ;has been cached.
	   (cond (clocal               
		  ;; If a matching solution has already
		  ;; been cached in the system then inform
		  ;; the user if debugging is on.
		  (cs-result 1 "Equal solutions found:")            
		  (cs-list 1 Local clocal))           
		 ;; Else if no matching solution has been
		 ;; cached then add this to the que and continue.
		 (t (push Local Que)                           
		    (push Local cache)))))))                   



;;------------------------------------------------------------------------
;; Expand-for-Q
;; Given a solution S where the topmost element of the solution is a 
;; Qnode Q, advance the solution search depending upon Q.
;;   1.  Iff Q is a Known value then pop Q from the S->Soughts and
;;       Return S for further searching.  
;;   2.  Iff Q is a parameter then Pop it from S->Soughts and add 
;;       it to S->knowns
;; Given a Solution B where the top of the soughts is a quantity Q, find
;; a set of appropriate enodes that can be used to solve for Q.  If none
;; are found then return nil.  Else return a set of alternate solutions
;; one for each of the alternate enodes.
;;
;; Arguments: B: The solution being dealt with.
;;            Forbidden:  An optional list of forbidden nodes 
;;                        that cannot be used in a solution.
;;
;; Returns: Nil if the top quantity cannot be made known.
;;          A list containing the resulting solutions otherwize.

(defun Expand-for-Q (S &optional (Forbidden nil))
  "Expand the solution B for the given quantity."
  (cs-action 1 "Expanding for Quantity ~A" (Solution-ID S))  ;debugging output
  (cs-list 1 (Solution-Soughts S))          
  
  (let ((Q (car (Solution-Soughts S)))                   ;; Get the sought Qnode Q.
	(Eqns (get-extension-eqns S Forbidden)))         ;; Get the Enodes that can extend it Eqns. 

    (cond ((member Q (Solution-Knowns S))                ;; If the quantity is already known.
	   (return-Solution-Successor S :pop-Soughts 1)) ;; Pop the quantity and return S.
	  
	  ((Qnode-Parameterp Q)                          ;; If Q is a parameter then
	   (return-Solution-Successor                    ;; It is known automatically so add it 
	    S :id 10 :inc 1 :pop-Soughts 1  :Push-Knowns Q))  ;; to the knowns of S and return.
	  
	  ((Qnode-has-mark? Q **Given**)  ;If the qnode is given then expand by
	   (Expand-for-given-Q S Q Eqns)) ;the given eqn only to prevent search.
	  ((null Eqns) nil)            ;If the search cannot expand return nil.
	  (t (expand-for-q-eqns S Q Eqns)))))     ;Else expand for each Eqn E.
  


;;; Given a Solution S with a quantity q to be solved for, the set E 
;;; of extension equations for the quantity is defined as the set of 
;;; all Enodes connected to the quantity Q s.t. 
;;;   1. It is algebraically independent of the other equations in
;;;       this solution.  This is done to prevent search looping.
;;;   2. It does not raise a nogood when combined with the other 
;;;       Nodes in this solution.  This prevents specialized errors
;;;       such as the choice of conflicting axes in the solution set.
;;;   3. They are not part of the optional forbidden set supplied by
;;;       the solution solver.
;;;
(defun get-extension-eqns (Solution &optional (Forbidden nil))
  "Get the set of valid extension connected to the sought."
  (cs-action 1 "Solution now: ~A" Solution)
  (let ((EqnSet (collect-Solution-Eqns Solution)))             
        
    (setq EqnSet
      (remove-nogood-eqns 
       (Solution-Assumptions Solution)
       (if EqnSet (get-indy-ext-eqns Solution EqnSet Forbidden)
	 (get-nonindy-ext-eqns Solution Forbidden))))
    		    
    (cs-result 1 "Result After Nogood removal:" )
    (cs-list 1 EqnSet)
    EqnSet))


;;; Given a set of equations return the proper subset
;;; of that set that do not cause a nogood to arise.
(defun remove-nogood-eqns (Assumptions Eqns)
  "Given a list of equations return the subset that do not generate nogoods."
  (loop for Eqn in Eqns
      unless (and (Enode-Assumptions Eqn)
		  (test-for-nogood 
		   (append Assumptions 
			   (Enode-Assumptions Eqn)
			   (loop for Q in (Enode-Qnodes Eqn)
			       append (Qnode-Assumptions Q)))))
      collect Eqn))



;;; Given the Solution, and the set of equations within it
;;; collect the proper subset of those eq
(defun get-indy-ext-eqns (Solution EqnSet Forbidden)
  "Get the indy eqns set."
  (cs-action 2 "Getting independent expansion equations.")
  (setup-indyset EqnSet)
  (let ((R) (Eqn))
    (dolist (E (get-nonindy-ext-eqns Solution Forbidden))
      (cs-result 3 "Testing for independence:")
      (cs-list 3 E)
      (setq Eqn (find (Enode-Algebra E) *Indy-Eqn-Index*
		      :test #'equal :key #'cadr))
      
      (cond ((not Eqn) (cs-result 3 "Unsolved"))
	    ((< *sgg-Indy-Threshold* (car (Solver-isIndependent 0 (car Eqn))))
	     (cs-result 3 "Not independent."))
	    (t (cs-result 3 "Accepted.")
	       (push E R))))
    R))


(defun setup-indyset (Eqns)
  "Setup the Indyset 0 for use by the independence checker."
  (when *indyset0-in-use*
      (Solver-indyKeepN 0 0)) ; Clear any preexisting set contents
  (dolist (E Eqns)
    (Solver-indyAddEq2Set 0 (car E)))
  (when Eqns
     (setf *indyset0-in-use* T)))	 




;;; Given a solution and an optional list of forbidden equations
;;; return the proper subset of the solution's equations that are
;;; neither unsolved nor forbidden.
(defun get-nonindy-ext-eqns (Solution Forbidden)
  (if *debug-cs* (cs-action 2 "Getting all expansion equations."))
  (let (R)
    (dolist (E (Qnode-Eqns (Car (Solution-Soughts Solution))))
      (cs-list 3 (list E))
      (cond ((not (Enode-Solvedp E)) (cs-result 4 "Unsolved therefore invalid"))
	    ((member E Forbidden) (cs-result 4 "Forbidden therefore invalid"))
	    ((member E (Solution-Knowns Solution)) 
	     (cs-result 4 "Already known therefore invalid."))
	    (t (cs-result 4 "Solved and acceptable therefore valid.")
	       (push E R))))
    R))




;;; When the Specified quantity is given then expand it if possible 
;;; using the given equation only.  This prevents the system from
;;; making any unnecessary search.  
(defun Expand-for-given-Q (S Q Eqns)
  "When the sought quantity Q is given, expand as appropriate for it."
  (let ((E (find-if #'Enode-Givenp Eqns)))                                     
    (cond (E (Return-Solution-Successor S :push-soughts E :push-Knowns Q))
	  ;; if a given value can be derived from existing equations in 
	  ;; this solution, then the given equation will not be in the 
	  ;; passed-in set of extension equations, because non-independent 
	  ;; equations are filtered from the set.  We can continue to collect
	  ;; a solution without it.  Here we just assume that is the reason, 
	  ;; with warning.
	  (t (format t "WARNING:  Partial solution set found no usable given eqn for ~A~%Assuming given value is not needed within this solution.~%" Q)
	      (Return-Solution-Successor S :push-Knowns Q)))))
	  

;;; When there are multiple ways of solving for a sought
;;; quantitiy then the system generates a set of solution 
;;; successors one for each alternate Eqn.  
(defun Expand-for-Q-eqns (S Q Eqns)
  "Given multiple ways of solving for Q expand for all."
  (let (r)
    (dotimes (N (length Eqns)) ;; I use this solely to support numbering. 
      (push (Solution-Successor 
	     S :id 10 :inc N
	     :Push-Soughts (nth N Eqns)
	     :Push-Knowns Q)
	    R))
    R))
				   





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expand-for-E
;;; Given a current solution S where the topmost sought node is an enode,
;;; This function generates a single search successor where:
;;;   The Enode E is pushed onto the knowns list.
;;;   The Quantity nodes that are connected to the enode are added to
;;;       the soughts list.  
;;; by adding all of the 
;;; unknown qnodes connected to the enode to the solution's soughts.  These
;;; nodes are then recur

(defun Expand-for-E (S &optional (Forbidden Nil))
  "Given a solution S Expand for its sought equation E."
  (let* ((E (car (Solution-Soughts S)))                ;Pop the top of Bs' Soughts into E.
	 (Qs (set-difference 
	      (Enode-Qnodes (car (Solution-Soughts S)))
	      Forbidden)))
	 
    (cs-action 1 "Expanding for Eqn ~A" (Solution-ID S))
    (cs-list 1 (Solution-soughts S) Qs)
    
    (cond ((null Qs)                                   ;If no unknown quantities remain in the eqn.
	   (Solution-Successor S :Push-Knowns E))           ;make it known and return the resulting solution.
	  
	  (t (Solution-Successor S :Push-Soughts Qs         ;Otherwize generate a new solution 
				 :Push-Knowns E)))))        ;with them sought and E Known.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution-successor
;; Give a solution and a set of modifications modify it accordingly.
;; 
;; Arguments: S:           The solution to be modified.
;;            Pop-Soughts: If t pop the top element from Bs' Soughts.
;;            Soughts:     push the specified element(s) onto Soughts.
;;            Knowns:      Push the specified element(s) onto Knowns.
;;            
;; Restuns: S following the specified modifications.
 
(defun Solution-Successor (S &key (id nil) (inc nil) (Pop-Soughts t) (Push-Soughts nil) 
				(Push-Knowns nil))
  "Generate a successor to the solution B."
  
  (let ((N (copy-Solution S)))
    
    (when id 
      (setf (Solution-id N) (* 10 (Solution-ID N)))) 
    
    (when inc 
      (setf (Solution-ID N) (+ inc (Solution-ID N))))
    
    (when Pop-Soughts                      ;;Pop the top element from soughts                                           
      (pop (Solution-Soughts N)))      
    
    (when Push-Soughts                          ;;Push onto soughts.
      (setf (Solution-Soughts N) 
	(append (force-to-list Push-Soughts) 
		(Solution-Soughts N)))
      
      (setf (Solution-Assumptions N)       ;;And add any assumptions.
	(append (collect-bgnodes->assumpts Push-Soughts) 
		(Solution-Assumptions N))))
    
    (when Push-Knowns                           ;;Push onto knowns.
      (setf (Solution-Knowns N)
	(append (force-to-list Push-Knowns) 
		(Solution-Knowns N))))
    N))

(defun return-Solution-Successor (S &key (id nil) (inc nil) (Pop-Soughts t) 
				(Push-Soughts nil) (Push-Knowns nil))
  "Generate a successor list to the solution B."
  (list (solution-Successor S :id id :inc inc 
			    :pop-Soughts Pop-Soughts
			    :Push-Soughts Push-Soughts 
			    :Push-knowns Push-Knowns)))

(defun collect-Solution-Eqns (Solution)
  "Collect the known equations from a solution."
  (loop for E in (Solution-Knowns Solution)
      when (Enode-P E)
      collect (find (Enode-Algebra E) *Indy-Eqn-Index*
		    :key #'cadr :test #'equal)))





;;;============================================================================
;;; Solution set collection.
;;; Once we have collected the set of solutions we can merge them into 
;;; solution sets according to the equations that they contain.  These sets 
;;; are what is then passed on to the help system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; collect-eqn-sets
;;; Given a set of solutions traversing through the graph convert them to a 
;;; set of equation sets for output.
;;;
;;; Arguments: Solutions: A set of solution traversals.
;;;
;;; Returns: A collated list of the equation sets containing the solutions.

(defun collect-Eqn-Sets (Solutions)
  "Collect a set of equation sets from the solutions provided."
  (let ((Esets) (E))
    (loop for S in Solutions		;For each solution B
	when (setq E (member-eset S Esets)) ;IF s is a member of an eset
	do (append-eset S E)		;add it into S
	else do (push (make-eset S) Esets)) ;otherwize make a new eset.
    Esets))				;Return the sets.


(defun member-eset (S Sets)
  (loop for E in sets			;for each set
      when (and (null (set-exclusive-or	;If all the equations in
		       (remove-if-not #'enode-p (Solution-Knowns S)) ; the solution are in the 
		       (EqnSet-Eqns E))) ; set and vice versa.
		(null (set-exclusive-or	; And if all the 
		       (Solution-Assumptions S) ; assumptions are equal.
		       (EqnSet-Assumpts E))))
      return E))			; return e or nil otherwize.


(defun append-eset (Solution Set)
  "Append the specified solution to the set."
  (setf (EqnSet-Nodes Set)
    (union (EqnSet-Nodes Set) (Solution-Knowns Solution)))
  (push Solution (EqnSet-Solutions Set)))


(defun make-eset (Solution)
  "Given a Solution generate a new Eset from B."
  (make-eqnset 
   :Eqns (remove-if-not #'Enode-P (Solution-Knowns Solution))
   :nodes (Solution-Knowns Solution)
   :Assumpts (Solution-Assumptions Solution)
   :Solutions (list Solution)))

							  
;;;--------------------------------------------------------
;;; Printing funcs.

(defun cs-action (x format &rest args)
  (when *Debug-cs* (format t "~A ~?~%" (print-outline-indent (+ x 1)) 
			   format args)))

(defun cs-result (x format &rest args)
  (when *Debug-cs* (format t "~A ~?~%" (print-outline-indent (+ x 1) nil) 
			   format args)))

(defun cs-list (x &rest args)
  (when *Debug-cs* (format t "~A ~A~%" (print-outline-indent (+ x 1) nil) 
			   args)))
			     

;;=======================================================================
;; Trace functions.

(defun trace-solutions ()
  (trace collect-solutions)
  (trace Expand-for-Q)
  (trace Expand-for-E)
  (trace collect-eqn-sets)
  (trace make-eset)
  (trace Solution-Successor)
  (trace get-extension-eqns)
  
  (trace prime-solution-indy)
  (trace solver-indyEmpty)
  (trace solver-indyAddVar)
  (trace solver-indyDoneAddVar)
  (trace solver-indyAddEquation)

  (trace solver-isindependent)

  (trace get-indy-ext-eqns)
  (trace setup-indyset)
  (trace get-nonindy-ext-eqns)
  (trace remove-nogood-eqns)
  
  (trace expand-for-q-eqns)
  )
			       
  	
