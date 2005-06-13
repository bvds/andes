#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;  Where S = the set of problem soughts and k = nil.
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

|#

;;================================================================================
;; Parameters

(defparameter *debug-cs* () "Controls debugging output for the collect-solutions loop.")

(defparameter *Indy-Var-Index* () "Var index (# Var Value Units) for the indy system.")
(defparameter *Indy-Eqn-Index* () "Eqn index (# Algebra) for the indy system.")

(defconstant *sgg-indy-threshold* 0 "Theshold for the is-indy test")

(defvar *indyset0-in-use* NIL "indyset 0 needs to be cleared before next use")   

;;================================================================================
;; Solution Collection
;;
;;------------------------------------------------------------------------------
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


;;===============================================================================
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
  (cs-bp "Collecting Solutions for ~A" Soughts)                ;; Produce debugging info.
  (do ((Que (list (make-solution :id 1 :Soughts Soughts)))     ;; Make the initial S and iterate.
       (Results) (S) (cache) (Local) (clocal))                 ;; Allocate other temp variables.
      
      ((null Que) Results)                                     ;; Set the end test for a Null que.
    
    (setq S (pop Que))                                         ;; Pop the top Solution from the que.

    (cond ((null (Solution-Soughts S))                         ;; If there are no more soughts in S
	   (cs-bp2 "Solution Completed ~A" (Solution-id S))    ;; The provide debugging info for the
	   (cs-db "~A~%" (Solution-Knowns S))                  ;; User if debugging is on.
	   (push S Results))                                   ;; Then add the solution to results.
	  
	  ((Qnode-P (car (Solution-Soughts S)))                ;; If the top sought in S is a quantity.
	   (setq Que (append (Expand-for-Q S Forbidden) Que))) ;; Expand as appropriate and append.

	  ((Enode-P (car (Solution-Soughts S)))                ;; If the top sought is an Enode then
	   (setf Local (Expand-for-E S Forbidden))             ;; Expand it via the Enode.
	   (setf clocal (find Local cache                      ;; Test to see if a matching solution
			      :test #'Solution-equalp))        ;; has been cached already.
	   	   
	   (cond (clocal                                       ;; If a matching solution has already
		  (cs-bp2 "Equal solutions found:")            ;; been cached in the system then inform
		  (cs-db "~A~%  ~A~%" Local clocal))           ;; the user if debugging is on.
		 
		 (t (push Local Que)                           ;; Else if no matching solution has been
		    (push Local cache)))))))                   ;; cached then add this to the que and continue.



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
  (cs-bp "Expanding for Quantity ~A" (Solution-ID S))    ;; Perform debugging output.
  (cs-bp "~A" (car (Solution-Soughts S)))                ;; If debugging is turned on.
  
  (let ((Q (car (Solution-Soughts S)))                   ;; Get the sought Qnode Q.
	(Eqns (get-extension-eqns S Forbidden)))         ;; Get the Enodes that can extend it Eqns. 

    (cond ((member Q (Solution-Knowns S))                ;; If the quantity is already known.
	   (return-Solution-Successor S :pop-Soughts 1)) ;; Pop the quantity and return S.
	  
	  ((Qnode-Parameterp Q)                          ;; If Q is a parameter then
	   (return-Solution-Successor                    ;; It is known automatically so add it 
	    S :id 10 :inc 1 :pop-Soughts 1  :Push-Knowns Q))  ;; to the knowns of S and return.
	  
	  ((Qnode-Givenp Q)                              ;; If the qnode is given then expand by
	   (Expand-for-given-Q S Q Eqns))                ;; The given eqn only to prevent search.
	  
	  ((null Eqns) nil)                              ;; If the search cannot expand return nil.

	  (t (expand-for-q-eqns S Q Eqns)))))            ;; Else expand for each Eqn E.
  


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
  (let ((EqnSet (collect-Solution-Eqns Solution)))             
    (cs-db "Solution Equations ~A~%" Solution)
        
    (setq EqnSet
      (remove-nogood-eqns 
       (Solution-Assumptions Solution)
       (if EqnSet (get-indy-ext-eqns Solution EqnSet Forbidden)
	 (get-nonindy-ext-eqns Solution Forbidden))))
    		    
    (cs-db "Resulting Set After Nogoods:~%  ~A~%" EqnSet)
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
  (cs-bp2 "Testing indy Equations.")
  (setup-indyset EqnSet)
  (let ((R) (Eqn))
    (dolist (E (get-nonindy-ext-eqns Solution Forbidden))
      (cs-db "Testing Nonindy eqns for indy:~%")
      (cs-db "Eqn: ~A " E)
      (setq Eqn (find (Enode-Algebra E) *Indy-Eqn-Index*
		      :test #'equalp :key #'cadr))
      
      (cond ((not Eqn) (cs-db "Unsolved.~%"))
	    ((< *sgg-Indy-Threshold* (car (Solver-isIndependent 0 (car Eqn))))
	     (cs-db "Not independent.~%"))
	    (t (cs-db "Accepted.~%")
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
  (if *debug-cs* (cs-bp2 "Testing Equations."))
  (let (R)
    (dolist (E (Qnode-Eqns (Car (Solution-Soughts Solution))))
      (cs-db "Eqn: ~A " E)
      (cond ((not (Enode-Solvedp E)) (cs-db " Unsolved therefore invalid.~%"))
	    ((member E Forbidden) (cs-db " Forbidden therefore invalid.~%"))
	    ((member E (Solution-Knowns Solution)) 
	     (cs-db " Already known therefore invalid.~%"))
	    (t (cs-db " Solved and acceptable therefore valid.~%")
	       (push E R))))
    R))




;;; When the Specified quantity is given then expand it if possible 
;;; using the given equation only.  This prevents the system from
;;; making any unnecessary search.  
(defun Expand-for-given-Q (S Q Eqns)
  "When the sought quantity Q is given expand as appropriate for it."
  (let ((E (find-if #'Enode-Givenp Eqns)))                                     
    (cond (E (Return-Solution-Successor S :push-soughts E :push-Knowns Q))
	  (t (Error "Given quantity returned no given eqn. ~A ~%~A" Q Eqns)))))
	  

;;; When there are multiple ways of solving for a sought
;;; quantitiy then the system generates a set of solution 
;;; successors one for each alternate Eqn.  
(defun Expand-for-Q-eqns (S Q Eqns)
  "Given multiple ways of solving for Q expand for all."
  (let (r)
    (dotimes (N (length Eqns)) ;; I use this soley to support numbering. 
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
  (let* ((E (car (Solution-Soughts S)))                ;;Pop the top of Bs' Soughts into E.
	 (Qs (set-difference 
	      (Enode-Qnodes (car (Solution-Soughts S)))
	      Forbidden)))
	 
    (cs-bp "Expanding for Eqn ~A" (Solution-ID S))
    (cs-db "~A~%  ~A~%" (Solution-soughts S) Qs)
    
    (cond ((null Qs)                                   ;;If no unknown quantities remain in the eqn.
	   (Solution-Successor S :Push-Knowns E))           ;;make it known and return the resulting solution.
	  
	  (t (Solution-Successor S :Push-Soughts Qs         ;;Otherwize generate a new solution 
				 :Push-Knowns E)))))        ;;with them sought and E Known.


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
		    :key #'cadr :test #'equalp)))





;;=====================================================================================
;; Solution set collection.
;; Once we have collected the set of solutions we can merge them into solution sets
;; according to the equations that they contain.  These sets are what is then passed
;; on to the help system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect-eqn-sets
;; Given a set of solutions traversing through the graph convert them to a 
;; set of equation sets for output.
;;
;; Arguments: Solutions: A set of solution traversals.
;;
;; Returns: A collated list of the equation sets containing the solutions.

(defun collect-Eqn-Sets (Solutions)
  "Collect a set of equation sets from the solutions provided."
  (let ((Esets) (E))
    (loop for S in Solutions                            ;;For each solution B
	when (setq E (member-eset S Esets))             ;; IF s is a member of an eset
	do (append-eset S E)                            ;; add it into S
	else do (push (make-eset S) Esets))             ;; otherwize make a new eset.
    Esets))                                             ;; Return the sets.


(defun member-eset (S Sets)
  (loop for E in sets                                                ;; for each set
      when (and (null (set-exclusive-or                               ;; If all the equations in
		       (remove-if-not #'enode-p (Solution-Knowns S))  ;; the solution are in the 
		       (EqnSet-Eqns E)))                              ;; set and vice versa.
		(null (set-exclusive-or                               ;; And if all the 
		       (Solution-Assumptions S)                       ;; assumptions are equal.
		       (EqnSet-Assumpts E))))
      return E))                                                      ;; return e or nil otherwize.


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

							  
;;--------------------------------------------------------
;; Printing funcs.

(defun cs-db (&rest form)
  (if *Debug-cs* (eval `(format t ,@(qlist form)))))

(defun cs-bp (&rest form)
  (if *debug-cs* (barprint "############" 1 form)))

(defun cs-bp2 (&rest form)
  (if *debug-cs* (barprint "#####" 0 form)))
			     



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
			       
  	
