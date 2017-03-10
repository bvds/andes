;; Solver.cl
;; Kurt VanLehn and Collin Lynch
;; 12/11/2000
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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file defines the core element of the Andes2 solver system including
;; the central solver struct and the access functions.  This file as well
;; as all associated files are located in the Turkey-Solver package.
;;
;; This system is intended to function as a subsystem of a larger solution
;; system and is used to solve given a specified knowledge base of operators
;; and other data.
;;



;;===================================
;; Portable struct.
(defstruct qsolres
  ID
  Algebra
  nodes
  path
  subeqns
  subvars
  assumpts
  wm)

(defparameter **wm** ())

;;==========================================================================
;; Public functions.

(defun solve-for-PSM-Quantity (Goal Givens)
  "Solve for PSMs on the specified Quantity with the specified givens."
  (merge-duplicate-PSMS
   (loop for State in (qsolve-for (list 
				   `(psm ,Goal ?eqn-id ?eqn-algebra 
					 ?unknowns)) Givens)
	 collect (let ((PSM (find 'psm (St-WM State)
				  :key #'car
				  :test #'equal)))
		   (setq **wm** (union (st-wm state) **wm** :test #'unify))
		   (make-qsolres
		    :ID (nth 2 PSM)
		    :algebra (nth 3 PSM)
		    :nodes (mapcar #'cdr
				   (remove-if-not 
				    #'(lambda (wme) 
					(and (eql (car wme) 
						  'variable)
					     (member (caddr wme) 
						     (nth 4 PSM))))
				    (St-WM State)))
		    :path (collect-path State)
		    :subeqns (collect-subeqns State)  ;Collect the subequations
		    ;; Collect variables.
		    :subvars (collect-subvars State (nth 4 PSM))
		    ;; Collect the assumptions.
		    :assumpts (collect-assumptions State)                   
		    ;; Collect the wm.
		    :wm (St-wm State))))))     


(defun solve-for-Constant-Quantity (Goal Givens)
  "Solve for the quantity iff it is a Constant."
  (let ((R (merge-duplicate-PSMS
	    (loop for State in (qsolve-for (list `(psm ,Goal (std-constant ?V) 
						       ?eqn-algebra ?unknowns))
					   Givens)
		  collect (let ((PSM (find 'psm (St-WM State)
					   :key #'car
					   :test #'equal)))
			    (setq **wm** (union (st-wm state) **wm** 
						:test #'unify))
			    (make-qsolres
			     :id (nth 2 PSM)
			     :algebra (nth 3 PSM)
			     :nodes (mapcar #'cdr
					  (remove-if-not 
					   #'(lambda (wme) (and (eql (car wme)
								     'variable)
								(member 
								 (caddr wme) 
								 (nth 4 PSM))))
					   (St-WM State)))
			     :path (collect-path State)
			     ;;Collect the subequations.
			     :subeqns (collect-subeqns State) 
			     ;;Collect the variables.
			     :subvars (collect-subvars State (nth 4 PSM))
			     ;;Collect the assumptions.
			     :assumpts (collect-assumptions State)
			     :wm (st-wm State)))))))
    
    (cond ((> (Length R) 1)
	   (error "Multiple Constant Results for ~S~%" Givens))
	  (t (car R)))))


(defun solve-for-given-eqn (Goal Givens)
  "Solve for the general goal expression given Givens."
  (merge-qsolver-results
   (loop for State in 
      ;; generate given-eqn
	(qsolve-for (list `(Given-eqn ?eqn ,Goal)) Givens)
      ;;For each returned state.
      collect (let ((Eqn-prop (find 'Given-eqn (st-wm State)             
			       :key #'car
			       :test #'unify))
		    (Given-prop (find `(given ,Goal . ?rest)
		                      (st-wm State) :test #'unify)))
		(setq **wm** 
		      (union (st-wm state) **wm** :test #'unify))
		(make-qsolres
		 ; id for a given eqn is the given proposition
		 :id      (or Given-prop 
		              (error "Given prop not found in wm for ~A~%" Eqn-prop))
		 :algebra (second Eqn-prop)
		 ;;Get the quantity variable.
		 :nodes (find-if #'(lambda (V) (and (eql (car V) 
							 'Variable)
						    (unify (caddr V) 
							   Goal)))
				 (st-wm State))
		 ;;collect the paths.
		 :path (collect-path State)
		 ;;Collect the subequations
		 :subeqns (collect-subeqns State)
		 ;;Collect the variables.
		 :subvars (collect-subvars State (list Goal))
		 ;;Collect the assumptions. 
		 :assumpts (collect-assumptions State)
		 :wm (st-wm State))))))


(defun solve-for-param-var (Param Givens)
  "Solve for a parameter variable corresponding to param."
  ;; Solves variable definition goal only for quantity Param
  (merge-qsolver-results
   (loop for State in (qsolve-for (list `(variable ?var ,Param)) Givens)
       collect (let ((Var (find-if #'(lambda (W) 
				       (unify W `(Variable ?Var ,Param))) 
				   (st-wm State))))
		 (setq **wm** (union (st-wm state) **wm** :test #'unify))
		 (make-qsolres
		  :id (cadr Var)
		  :path (collect-path State)
		  :wm (st-wm State))))))



;;; Fix to deal with single selection problem.			      
(defun solve-for-non-quantity (goals givens)
  "Solve for the specified non-quantity goal."
  (let (R P N)
    (loop for S in (qsolve-for goals givens)
	do (setq N (subst-bindings (st-bindings S) goals))
	   (setq P (find N R :key #'qsolres-id :test #'unify))
	when (or (null R) (null P)) 
	do (push (make-qsolres
		  :ID (subst-bindings (st-bindings S) Goals)
		  :subvars (collect-subvars S nil)
		  :subeqns (collect-subeqns S)
		  :path (collect-path S)
		  :assumpts (collect-assumptions S)
		  :wm (st-wm S))
		 R)
	else do (setf (qsolres-path P) 
		  (merge-paths (qsolres-path P) (collect-path S))))
    R))

    
(defun qsolve-for (goals givens)
  "Call the quantity solver for the goals with givens."
  (solution-sts (initial-st Goals Givens))) 
			       
			       


;;============================================================================
;; Internal functions.
;; The functions following this mark are all internal to the solver system 
;; and should not be called directly by the user.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect-path
;; Given a state cycle back though the preceeding states and collect
;; it's solution path.

(defun collect-Path (State)
  "Collect the path of actions from State and its preceeding states."
  (when State
    ;; Substitute any new bindings into the predecessors action lists
    ;; AW: taken out. Backpatching var values into actions after the fact 
    ;; can prevent path merge from detecting the common subgoal action 
    ;; before conflicting operator choices in case where the conflicting 
    ;; ops result with different values.
    (append (collect-path (St-Predecessor State))
	    (remove-if #'removable-actionp (st-actions State)))))

;; Some of the actions do not need to be recorded in the 
;; solution path.  Operators like 'not, 'in-wm, 'bind and
;; 'test simply get eliminated from the path as they are 
;; of no further use once the search is over.
;; this is somewhat redundant with sg-ignore-class
;; should make common list in HelpStructs/PsmGraph.cl
(defun removable-actionp (action)
  (and (listp action)
       (or (eq (car action) 'setof-result)
	   (and (cssg-p action)
		(member (car (cssg-goal action))  
			'(not bind test debug in-wm))))))


(defun collect-subvars (State BubbleVars)
  "Collect every variable statement in a state's WM not in the Bubble."
   (loop for V in (St-wm State)
       when (and (eql (car V) 'Variable)
		 (not (member (caddr V) 
			      BubbleVars
			     :test #'unify)))
       collect V))


(defun collect-subEqns (State)
  "Collect every equation in a state's WM."
   (loop for E in (St-wm State)
       when (and (or (eq (car E) 'Eqn)
		     (eq (car E) 'Given-Eqn)
		     (eq (car E) 'Derived-Eqn)
		     (eq (car E) 'Implicit-eqn)))
		 ;;(not (unify (nth 2 E) MainEqn)))
       collect E))


(defun collect-assumptions (State)
  "collect the set of (assume <assumption>) statements found in State."
  (loop for A in (St-wm State)
      when (eql (car A) 'Assume)
      collect (cdr A)))


;;===========================================================================
;; Merge-duplicates
;; In solving for psms, given, eqns, etc we occasionally end up with
;; duplicate values the functions here are used to merge those
;; duplicates to generate a single set as necessary.

;; The set of all results for a particular sought may contain several different
;; psms, but also multiple instances of the same psm with different paths.
;; This merges multiple instances for the same psm into a single instance.
(defun merge-duplicate-psms (PSMS)
  "Merge any duplicates found within given set of psm qsolver results"
  (let ((tmp) (R (list (car psms))))
    (dolist (P (cdr psms))
      (setq tmp (merge-psm P R))
      (if tmp (setq R tmp)
	(push P R)))
    R))
  
(defun merge-psm-check (P Set)
"check for inconsistent results when merging PSM result P into result Set"
 (let ((idmatch (find (qsolres-id P) Set :key #'qsolres-id :test #'equalp))
       (algmatch (find (qsolres-algebra P) Set :key #'qsolres-algebra :test #'equalp)))
     (when (and idmatch (not (equalp (qsolres-algebra idmatch) (qsolres-algebra P))))
           (error "Two PSM results with same id~%      ~A~%but different algebra:~%[1] ~A~%[2] ~A~%"
	        (qsolres-id P) (qsolres-algebra idmatch) (qsolres-algebra P)))
     (when (and algmatch (not (equalp (qsolres-id algmatch) (qsolres-id P))))
         (error "Two PSM results with same algebra~%     ~A~%but different psm ids:~%[1] ~A~%[2] ~A~%"
	        (qsolres-algebra P) (qsolres-id algmatch) (qsolres-id P)))))

(defun merge-psm (P Set)
  "return result of merging PSM result P into matching eqn result in Set of solutions; NIL if no match"
  ; Check results for conflicting results with same id or algebra. !!! Shouldn't be
  ; an error if this is lower-order result which doesn't get merged into final set.
  (merge-psm-check P Set)
  (loop for P2 in Set			;If it matches another PSM in equations
      when (and (equal (qsolres-algebra P) (qsolres-algebra P2))
		(equal-sets (qsolres-nodes P) (qsolres-nodes P2))) ;And quantities 
      do (merge-qsolver-result P2 P)
      and return Set))			;return the Set if we merged.

(defun merge-qsolver-result (P2 P)
"modify qsolres P2 by merging path from qsolres P into it, returning P2"
   (let ((merged-path (merge-paths (qsolres-path P2) (qsolres-path P))))
     (when merged-path  ; merge of paths succeeded
	 (setf (qsolres-path P2) merged-path)  ; use merged paths
	 (setf (qsolres-subeqns P2) 
	   (union (qsolres-subeqns P) (qsolres-subeqns P2))) ;Subeqns
	 (setf (qsolres-subvars P2) 
	   (union (qsolres-subvars P) (qsolres-subvars P2))) ;SubVars
	 (setf (qsolres-assumpts P2) 
	   (union (qsolres-assumpts P) (qsolres-assumpts P2) 
		  :test #'unify))	;Assumptions.
	 (setf (qsolres-wm P2)
	   (union (qsolres-wm P) (qsolres-wm p2) :test #'unify))))
   ; Always return first argument. Allows use of reduce to
   ; successively merge all elements in a set
   P2)

(defun merge-qsolver-results (results)
"merge all given qsolver results into the first one, returning it"
 (when results ; may be NIL
    (reduce #'merge-qsolver-result results)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The root of all the merge-duplicates functions is merge-paths.
;; this function locates the point at which the pair of paths diverge
;; and inserts a choose statement, merging the second argument into
;; the first.
;; 
;; merge (A B C X Y Z) and (A B C M N O) 
;;    => (A B C (CHOOSE (X Y Z) (M N O)))
;;
;; When adding a new alternative into a CHOOSE, the new tail is
;; merged recursively into the first choice with a matching initial 
;; element, if one exists:
;;
;; merge (A B C (CHOOSE (X Y Z) (M N O))) and (A B C X Y W)
;;    => (A B C (CHOOSE (X Y (CHOOSE (Z) (W)) (M N O)))
;;       
;; When paths diverge at choice of operators, paths using lower-ranked
;; operators at that point are NOT merged into a path using a higher-ranking 
;; ones. Hence alternative solutions using lower-ranked operators at
;; wind up discarded from the result tree being built.
;;
;; merge-paths returns the new composite path if built, or NIL
;; if the new path was discarded.
;;
;; The current implementation relies on the fact that higher-ranking
;; paths are found before lower ranking ones at every choice point.
;; So new alternatives to be merged into the result being accumulated may 
;; contain lower ranked operators, but never have to be checked for having 
;; higher rank than anything in the current accumulated result.

(defparameter *watch-merge* nil
  "enable trace message on merging paths")

(defun merge-paths (P1 P2)
  "return merge of path P2 into P1; NIL if not done"
  ;; must match without any bindings needed.
  (let ((Loc (mismatch P1 P2 :test #'unify-check-for-variables))
         new-choose) 
    (when (= Loc 0) ; no common prefix: shouldn't happen
	  (error "paths to merge lack common prefix ~%  (~A ...)~%  (~A ...)~%" 
		  (first P1) (first P2)))
    (when *watch-merge* ;; show point of divergence, plus preceding line for context
	(format t "merge-paths: at ~S [Loc=~A]~% add ~S~%  to ~A~%" 
		(nth (1- Loc) P1) Loc
		(nth Loc P2)
		(if (choose-p (nth Loc P1)) ; list choice path heads
		      (mapcar #'first (cdr (nth Loc P1)))
		  (nth Loc P1))))

    (cond 
      ((not (choose-p (nth Loc P1))) ; not already a choose at Loc
          (if (action< (nth Loc P2) (nth Loc P1)) ; don't insert P2 if lower-rank
              (progn (format t "merge: dropping ~A < ~A~%" 
	                     (caadr (nth Loc P2)) (caadr (nth Loc P1))) 
	              NIL) ; return value to signal merge failure
          ; else not lower rank: insert a choose
          (append (subseq P1 0 Loc)
		(list (list 'CHOOSE (subseq P1 Loc) (subseq P2 Loc))))))
      (T  ; else add or merge new choice into existing choose set. 
          ; Note may fail if it's dropped due to order
          (when (setq new-choose (add-to-choose (nth Loc P1) (subseq P2 Loc)))
               (append (subseq P1 0 Loc) (list new-choose)))))))

(defun add-to-choose (C S)   
  "Add or merge new subsequence S into choose C."
  (let ((R) (choices (cdr C)))
    (loop for I below (length choices) ; for each existing choice i
	  ;; if it matches new one at first element
	  when (unify-check-for-variables (car S) (car (nth I choices))) 
	  ;; modify choice i to be merge of old choice i and s, returning C
	  do (setq R C)	
	  (setf (nth I (cdr R)) (merge-paths (nth I choices) S))
	  and return t)
    
    (when (not R)   ; no match in existing choices: append whole new choice
       ; don't do if new choice head is lower ranked than any existing choice head
       (let ((better-choice (find-if #'(lambda (choice) 
                                         (action< (first S) (first choice)))
                                     choices )))
	(if better-choice 
	   (progn
	     (format T "merge: dropping ~a < ~a~%" 
	                (caadr (first S)) (caadr (first better-choice)))
	     (setq R NIL)) ; signal merge failure
	 ; else
	 (setq R (append C (list S))))))
     
    ; finally return result
    R))

(defun unify-check-for-variables (x y)
  (let ((bindings (unify x y)))
    (when (and bindings (not (equal bindings no-bindings)))
      (format t "WARNING:  two paths differ only by unbound variables:~%    ~A~%  Path:  ~S~%"
	      bindings x))
    (equal bindings no-bindings)))
	 
;;; ========================= top level ==========================
;;; This function solves the problem and then dumps the solution graph
;;; to a file.  The file has same name as the problem and "sg" as the
;;; extension.

;;; Producing a solution graph is done in two passes.  First we solve
;;; the problem, which generates a tree of states whose root is the
;;; initial state and whose leaves are the final states.  However, the
;;; links in this tree point backwards, from leaves back toward roots
;;; (see comment on dump-sg for explanation). Thus, a second pass
;;; (dump-sg) is done to turn reverse the links and print the tree
;;; like a normal tree, with links running from the root toward the
;;; leaves.

;;; This file is organized into three parts.  First come the
;;; definitions of data structures and globals, since the compiler
;;; gets confused if they appear after their usage.  The second part
;;; of the file is the solver itself.  The last part of the file is
;;; the code for dump-sg, which prints the solution graph.

;;; ===================== globals ===============================

;;; flag to control tracing (see also *debug* in executable2.cl)

(defparameter *actions* nil 
  "Controls whether note-action will trace.")

;;; markers used in action lists of states and in solution graphs

(defconstant +split+ 'split
  "Marks a non-deterministic split in the solution path")
(defconstant +next+ 'next
  "Marks a end of one parallel branch and the beginning of another")
(defconstant +join+ 'join
  "Marks the end of a set of parallel branches")

(defun choose-p (exp)
  "true if expression is a choose list"
  (and (listp exp) (eq (first exp) 'CHOOSE))) 

; For prioritizing actions by operator order. The only actions
; which have orders are OP actions recording operator selections
(defun action-order (action)
   "return operator order for operator selection actions, else NIL"
   (when (eq (first action) +goal-unified-with-effect+)
      (operator-order (get-operator-by-tag (second action)))))

(defun action< (a1 a2)
   "return T if action a1 has lower rank than a2"
  (alist< (action-order a1) (action-order a2)))

			      

;;; ======================= st ====================================
;;; The st (short for state) struct represents a problem solver's
;;; state.  The working memory (wm) is the set of facts (ground atomic
;;; propositions) that the solver believes at this time.  The stack is
;;; an ordered list of operator instances, except for the top item on
;;; the stack, which might be a goal or an executable form instead of
;;; an operator instance.  The stuff in the stack has lots of
;;; variables in it.  Rather than continually substituting values
;;; into the variable, the state keeps a list of bindings (the format
;;; is defined by unifier.cl) which indicate what values all the
;;; variables appearing in the stack have. The history slot is a list
;;; of operator instance identifiers, which should all have their
;;; variables bound.  It is used to prevent the same operator from
;;; being executed twice in the same state.  The action slot is not
;;; used by the interpreter.  It is kept for building the solution
;;; graph later.  Similarly, the precedessor slot, which points to the
;;; preceding state, is only used for building the solution graph.

(defstruct (st (:print-function print-st))
  level          ; The level in search tree (for debugging)
  branches       ; T if multiple fellow branches in search tree (for debugging)
  bindings       ; A binding list (format defined by unifier.cl)
  wm	         ; A set of ground atomic propositions
  stack		 ; a list of operator instances (mostly)
  history	 ; a list of operator instance identifiers
  predecessor    ; the preceding state (represented as a st struct)
  actions	 ; list of s-expression recording the actions taken here
  )

;;; This is called by the Lisp system whenever it wants to print a st
;;; struct.

(defun print-st (st stream depth)
  "Prints the given state onto the stream"
  (declare (ignore depth))
  (let ((*print-length* 100)
	(*print-level* 20))
  (format stream "<st ~S~%>" 
	  (list (st-wm st)
		(st-bindings st)
		(st-stack st)))))

(defun push-wm (item state)
  "Adds the given item to the given state's working memory"
  (if (not (groundp item))
      (cerror "Ignore" "Adding non-ground proposition ~S to wm" item))
  (pushnew item (st-wm state) :test #'unify))

(defun push-history (item state)
  "Adds the given item to the given states' history list"
  (if (not (groundp item))
      (cerror "Ignore" "Adding non-ground ~S to history" item))
  (pushnew item (st-history state) :test #'unify))

(defun push-binding (variable value state)
  "Adds the given variable-value pair to the state's bindings"
  (if (not (variable-p variable))
      (cerror "~S non-variable in push-bindings" variable))
  (setf (st-bindings state)
    (extend-bindings variable value (st-bindings state))))

(defun print-wm (st)
  "Prints the working memory in batches"
  (let ((predicates '(eqn vector axis-for variable)))
    (dolist (p predicates)
      (format t "~&Wm for ~S:~%" p)
      (dolist (wme (st-wm st))
	(if (eql p (first wme))
	    (format t "~S~%" wme))))
    (format t "The rest of wm:~%")
    (dolist (wme (st-wm st))
      (if (not (member (car wme) predicates))
	  (format t "~S~%" wme)))))

; For sorting states by operator rank at generation time. The
; only states that have orders are those resulting from OP actions
(defun state< (state1 state2)
  "return T if state1 uses higher-ranked operator than state2"
  (action< (first (st-actions state1)) 
           (first (st-actions state2))))

;;; =================== opinst =====================================
;;; The operator instance (opinst for short) struct represents an
;;; operator that is in the process of being satisfied.  It starts
;;; life as a sort of copy of the operator, with the variables
;;; standardized apart.  The subgoals slot holds a copy of the
;;; operator's preconditions.  The effects slot holds a copy of the
;;; operator's effects.  As the operator instance is repeatedly
;;; processed by the interpreter, the list of subgoals is popped.
;;; When all of them are gone, the effects are added to working
;;; memory.  The identifier slot is read-only.  It consists of the
;;; name of the operator (an atom) consed to the arguments of the
;;; operator (a list of variables).  If the identifiers of two
;;; operator instances are the same given the current bindings, then
;;; they would do exactly the same thing if executed.  The interpreter
;;; uses this to prevent two equivalent operators from being executed
;;; on the same state.

(defstruct (opinst (:print-function print-opinst))
  subgoals	     ;an ordered list of preconditions
  effects	     ;a set of effects (atomic propositions)
  identifier	     ;the name of the operator plus its arguments
  variables	     ;a list of the operator's help variables.
  )

;;; This is called by the Lisp system whenever it wants to print an
;;; operator instance.

(defun print-opinst (inst stream depth)
  "Prints the given operator instance on the given stream"
  (declare (ignore depth))
  (format stream "<opinst ~S>" (opinst-identifier inst)))


;;; ========================= solver ==============================
;;; The top level call is (solve2 <problem>) where the problem is an
;;; atom.  It looks up the problem to get the soughts and givens, then
;;; creates an initial state based on them.  It solves the problem,
;;; generating a possibly empty set of solution states, and stores it
;;; in the problem.  Macros in the Problem.cl file can be used to
;;; print the solutions of a problem in various ways).

;;; The problem might specify more than one goal, so the goal stack is
;;; initialized with an operator application for a dummy operator that
;;; has those goals as its preconditions.

(defun initial-st (Goals Givens)
  "Given atomic propositions representing a problem, 
   return an initial state for solving that problem"
  (Make-st 
   :level 2  ;start two levels from base of tree
   :branches T  ;start at base of tree
   :bindings no-bindings	; no-bindings is in unifier.cl
   :WM Givens
   :stack (list (final-operator Goals))
   :actions NIL))

(defun final-operator (Goals)
  "Returns a dummy operator instance whose goals are the given ones."
  (make-opinst :identifier '(dummy)
	       :subgoals Goals))

;;; The main loop is the algorithm used for all standard state-space
;;; searches, except that it keeps going when it finds final states
;;; until it has tried all possible solution paths.  The pseudo code
;;; is:

;;; Until queue is empty do
;;;  State <= (pop queue)
;;;  If state is done,
;;;  then put it in solutions
;;;  else queue <= (append (successors state) queue))

;;; This function is also called by execute-setof.

(defun solution-sts (initial-st)
  "Given an initial state, returns all solution states"
    (do ((queue (list initial-st))
	 (state)
	 (solutions)
	 (successors))
	((null queue)  solutions)
      (setq state (pop queue))
      (cond ((null (st-stack state))
	     (push state solutions)
	     (qs-debug-print-success state))
            ; Path merging code below assumes higher ranking paths precede lower in
	    ; the list of solutions returned.  We ensure this by exploring lower ranking 
	    ; operator choices first and collecting solutions by pushing, so returned set 
	    ; lists them in reverse of the order in which they were found.
	    ((null  (setq successors (stable-sort (successors state) #'state<)))
	     (qs-debug-print-failure state))
	    ((null (cdr successors))
	     ;; treat as same level:
	     (dolist (x successors) 
	       (setf (st-branches x) nil)
	       (setf (st-level x) (+ 0 (st-level state))))
	     (qs-debug-print-node state  (length successors))
	     ;; one successor, just put it back on queue
	     (push (car successors) queue))
	    (t
	     ;; next level deeper:
	     (dolist (x successors)
	       (setf (st-branches x) t)
	       (setf (st-level x) (+ 1 (st-level state))))
	     (qs-debug-print-node state (length successors))
	     (setq queue (append successors queue))))))
  


;;; There are many ways to generate successor states, depending on
;;; what is on the top of the stack.  However, they all start by
;;; copying the current state, popping its stack and setting its
;;; predecessor to the given state, so those actions are done here.
;;; The action is set to NIL in case any of the successors don't set
;;; it, because otherwise we get a copy of the preceeding state's
;;; action.

(defun successors (state)
  "Returns the successor states to the given state"
  (let* ((new-state (copy-st State))
	 (top (pop (st-stack new-state))))
    (setf (st-predecessor new-state) State)
    (setf (st-actions new-state) NIL)
    (cond ((opinst-p top)
	   (opinst-successors top new-state))
	  ((executable-p top)
	   (executable-successors top new-state))
	  (T
	   (goal-successors top new-state)))))

;;; When the top of the stack is an operator instance, there are four 
;;; Possible cases: 
;;; 1. The operator matches one above it in the stack and is therefore
;;;    (probably) going to cause a loop so we will kick it out at this
;;;    point.  (see below for criteria)
;;; 2. The operator has already been done in the history and therefore
;;;    is unnecessary so we will skip it.
;;; 3. The operator is done with no more subgoals and we can pop it off
;;;    of the stack.
;;; 4. The operator has some goals left so we move onto them.

(defun opinst-successors (inst state)
  "Given an operator instance, returns zero or more copies of the given state"
  (cond ((opinst-in-stack-p inst state) 
	 (cerror "Prune and continue." 
		 (strcat "opinst-successors:  Expression already in stack:~%"
			 "   ~A~%"
			 "   This is a symptom of an unwanted recursion.~%") 
		 inst)
	 NIL)
	((opinst-done-already-p inst state) NIL)
	((null (opinst-subgoals inst)) (opinst-done inst state))
	(T (opinst-next inst state))))


;;; If the operator matches one that is already in the stack then we 
;;; want to prune this branch in order to avoid a loop.  
;;; Generally, this should never happen if the rules are well-formed.
;;; This function tests for two cases:
;;; 1. Exact matches:  "foo(A)" and "foo(A)"
;;; 2. Variable matches: "foo(?x)" and "foo(?y)"
;;; BvdS: use unify so that keywords are handled correctly

(defun opinst-in-stack-p (inst state)
  "return t iff there exists a copy of inst in st-stack or one inst subsumes."
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Generate the ground id instance.
  (let ((ground-id (subst-bindings (st-bindings State) 
				   (opinst-identifier inst))))
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Search through the stack for a direct 
    ;; match or an expression subsumption. (unification.cl)
    (member-if 
     #'(lambda (Op) (unify
		     ground-id (subst-bindings (st-bindings State) 
					       (Opinst-identifier Op))))
     (st-stack State))))

;;; We never want to apply the same operator instance twice to the
;;; same state.  Thus, we keep a list of operator instances that have
;;; been executed in the states history.  We check for duplicates as
;;; soon as all the variables in the operator instance's identifier
;;; are bound.

(defun opinst-done-already-p (inst state)
  "True if the given operator instance has already been done in the 
   given state"
  (let ((Ground-id (ground-expression (opinst-identifier inst) 
				      (st-bindings state))))
    (and ground-id 
	 (member ground-id (st-history state) :test #'unify))))

;;; This is called to generate successor states when the top of the
;;; stack is an operator instance whose subgoals have all been
;;; accomplished.  It merely adds its literals to the state's working
;;; memory, checking to make sure that they are ground first.  It also
;;; records that this action has been done in the state.  The effects
;;; have to be include in the action list so that the help system can
;;; know what vectors, equations etc. were written just by reading the
;;; solution graph.

(defun opinst-done (inst state)
  "Given operator instance has all its subgoals achieved.
   Returns a list of a successor state."
  (let (ground-id ground-effects ground-act ground-vars var-bindings)
    (dolist (e (opinst-effects inst))
      (push (subst-bindings (st-bindings state) e) ground-effects)
      (push-wm (first ground-effects) state))
    (setf ground-id (subst-bindings (st-bindings state) 
				    (append (opinst-identifier inst))))
    (setf ground-act (subst-bindings (st-bindings state) 
				     (opinst-identifier inst)))
    (setf ground-vars (subst-bindings (st-bindings State)
				      (opinst-variables inst)))
    ;; Make a binding list of operator variables, to be used by help.
    (setf var-bindings 
	  (if ground-vars
	      (mapcar #'cons 
		      (operator-variables (get-operator-by-tag ground-act))
		      ground-vars)
	      no-bindings))
    
  (if (not (groundp ground-id))
	(cerror "Ignore" "The op instance identifier ~S is not ground" 
		ground-id))
    (note-action state 
		 (make-csdo :op ground-act
			    :effects ground-effects
			    :varvals var-bindings))
    (push-history ground-id state)
    (list state)))

;;; This is called when the top of the stack is an operator instance
;;; and it has some goals that are still not satisfied.  It pops the
;;; first subgoal.  If it is a macro, it expands it, pushes the
;;; resulting subgoals back onto the operator instance, and pops the
;;; new first subgoal.  When it finally has a subgoal that is not a
;;; macro, it puts a copy of the operator instance back on the stack.
;;; Then it pushes the subgoal onto the stack.  Thus, the subgoal will
;;; be processed first, followed by the operator instance again.  We
;;; can do all this to the given state, rather than a copy of it, because
;;; we know that this state was made up fresh in successors and passed
;;; to this function and no other.

(defun opinst-next (inst state)
  "Given operator instance wants its next subgoal satisfied.
   Returns a state with that goal on the top of the stack."
  (let* ((new-inst (copy-opinst inst))
	 (subgoal (pop (opinst-subgoals new-inst))))
    ;; AW: add loop for cases where expansion is a macro (rare),
    ;; or is empty and operator's next subgoal is also a macro. 
    (loop while (precond-macro-p subgoal) do
      (setf (opinst-subgoals new-inst)
	(append (expand-precond-macro subgoal state)
		(opinst-subgoals new-inst)))
         (setf subgoal (pop (opinst-subgoals new-inst))))
    (if (and nil (consp new-inst) (eql (car new-inst) 'dot-term))
        (progn (format t "*** opinst-next ~S~%" new-inst)
               (trace goal-successors-effects execute-bind))
        ;(untrace)
        )
    (push new-inst (st-stack state))
    ;; NB: macro may expand to NIL => no new subgoal.
    (when subgoal                                                          
      (if (and nil (consp subgoal) (eql (car subgoal) 'dot-term))
          (progn (format t "*** opinst-next subgoal ~S~%" subgoal)
                 (trace goal-successors-effects execute-bind))
                                        ;(untrace)
          )
      (push subgoal (st-stack state))                                      
      (note-action state 
		   (make-cssg 
		    :op (car (opinst-identifier inst))
		    :goal (subst-bindings (st-bindings state) subgoal))))
    (list state)))

;;; This is called to generate successor states when the top of the
;;; stack is a goal.  It returns one state for each unification of the
;;; goal with an element of working memory or with an effect of an
;;; operator.

(defun goal-successors (goal state)
  "Given a new state, returns a copy for each unification of the given goal"
  (if (and (consp goal) (eql (car goal) 'dot-term))
      (trace operator-var-copy goal-successors-effects)
      (untrace))
  (nconc (goal-successors-wmes goal state t)
	 (goal-successors-effects goal state)))

;;; This is also called by execute-in-wm.  If the action-flag is
;;; non-null, an action is added to each state returned.
;;; Execute-in-wm calls it with a null flag.

(defun goal-successors-wmes (goal state action-flag)
  "Returns a set of states, one for each unification with a wm element"
  (let ((bindings (st-bindings state))
	(new-bindings)
	(successors))
    (dolist (wme (st-wm state))
      (if (setq new-bindings (unify goal wme bindings))
	  (push (wm-unified new-bindings wme state action-flag) successors)))
    successors))

;;; This is called when the goal has unified with a working memory
;;; element.  Just notes that fact and does nothing else, because the
;;; bindings introduced by the unification are the main result.

(defun wm-unified (bindings wme state action-flag)
  "Returns a state that is a copy of the given state with the new bindings
   and the action noted."
  (let ((new-state (copy-st state)))
    (setf (st-bindings new-state) bindings)
    (when action-flag (note-action new-state 
				 (list +goal-unified-with-fact+ wme)))
    new-state))

;;; This is called by goal-successors to generate states via matching
;;; the goal to effects of operators.  The easy way to do this is to
;;; standardize the operator apart before unifying the goal with
;;; effects.  This standardization is necessary because this
;;; operator's variables are supposed to be unique to this instance.
;;; However, this wastes time copying operators whose effects don't
;;; unify with the goal.  Note that the binding list is almost useless
;;; during the unification because the operator's variables are new,
;;; so only the goal's variables can get values from the binding
;;; list. Thus, we first convert the goal to a ground proposition,
;;; standardize its remaining variables apart, and then unify it to the
;;; effects ignoring the binding list.  If this succeeds, then we do
;;; the expensive standardization (in effect-unified) which means that
;;; we have to keep track of which effect was the one that unified
;;; with the goal so that we can re-unify it with the standardized
;;; version of the operator.

(defun goal-successors-effects (goal state)
  "Returns a set of states, one for each unification of the given goal
   with an effect of an operator."
  (let ((bound-goal (rename-variables 
		     (subst-bindings (st-bindings state) goal)))
	(successors)
	(n))
    (dolist (op (get-operators-by-effect (car bound-goal)))
      (setq n 0)
      (dolist (effect (operator-effects op))
	(if (unify bound-goal effect no-bindings)
	    (push (effect-unified goal N op state) successors))
	(incf N)))
    successors))

;;; this is called when the goal has unified with the effect of an
;;; operator.  It creates a new operator instance, standardizes the
;;; variables apart, unifies the goal with the target effect in order
;;; to get bindings, and stores the effects and preconditions in the
;;; operator instance.  It creates an operator instance identifier by
;;; consing the operators name to its arguments.  All this is duely
;;; noted in the new state.

(defun effect-unified (goal effect-position op state)
  "Returns a new state with a new operator instance push onto its stack."
  (let* ((new-state (copy-st state))
	 (new-op (operator-var-copy op))
	 (effect (nth effect-position (operator-effects new-op)))
	 (bindings (unify goal effect (st-bindings state)))
	 (inst))
    (setf inst (make-opinst
		:subgoals (operator-preconditions new-op)
		:effects  (operator-effects new-op)
		:identifier (cons (operator-name op) 
				  (operator-arguments new-op))
		:variables (operator-variables new-op)))
    (if (and nil (consp inst) (eql (car inst) 'dot-term))
        (progn (format t "*** effect-unified ~S~%" inst)
               (trace goal-successors-effects execute-bind))
                                        ;(untrace)
        )
    (push inst (st-stack new-state))
    (setf (st-bindings new-state) bindings)
    (note-action new-state 
		 (list +goal-unified-with-effect+
		       (subst-bindings bindings (opinst-identifier inst))))
    new-state))



;;; ==================== solution graph ============================
;;; The solution is basically a tree of solution paths.  This code
;;; generates a printable version of the solution graph.  Other code
;;; can worry about reading it back in and making objects.  The
;;; printable solution graph consists of a lisp list.  The elements of
;;; the list are actions, SPLIT, NEXT, JOIN or (CHOOSE . <set of
;;; paths>).  An action is an s-expression passed note-action (see
;;; below for further description of actions).  The 3 atoms, SPLIT,
;;; JOIN and NEXT are used to represent and-parallellism caused by
;;; unordered operators.  The SPLIT indicates the begining of one
;;; parallel path, due to the first precondition of the operator.  The
;;; NEXT indicates the end of one path due to one precondition and the
;;; beginning of another path due to the next precondition.  JOIN
;;; indicates the end of the path due to the last precondition.
;;; (CHOOSE . <set>) indicats an or-branch.  The set of paths are
;;; alternatives.

;;; Actions taken by the interpreter are collected in the states in
;;; the actions slot.  Normally there is just one action per
;;; slot. However, if an unordered operator is just starting, we put
;;; SPLIT just after the action.  If an unordered operator is moving
;;; on to its next subgoal, we put NEXT before the action.  If an
;;; unordered operator is finishing up, we put JOIN after the action.
;;; The setof executable makes really huge list of actions.  The other
;;; executables do not put any actions in their state's actions slot.

(defun note-action (state action)
  "Sets the actions field of the state to the action plus split/next/join 
   markers.  Prints trace if *actions* is true.  Use for its side-effects."
  (setf (st-actions state)
	(cond ((and (csop-p action)
		    (unordered-op-id (first (second action))))
	       (list action +split+))
	      ((and (cssg-p action)
		    (unordered-op-id (cssg-op action)))
	       (list +next+ action))
	      ((and (csdo-p action)
		    (unordered-op-id (first (csdo-op action))))
	       (list +join+ action))
	      (T  (list action))))
  (qs-debug-print-action State))

(defun unordered-op-id (op-name)
  "Non-null if the given operator instance identifier comes from an 
   unordered operator"
  (let ((op (get-operator-by-name op-name)))
    (and op (member 'unordered (operator-features op)))))


;;; ============================ QS-Debug Printing ========================
;;; The qs-debug-print-* functions allow for debug printing
;;; of only those states that are at or below a desired 
;;; operator.  The three functions below print relevant
;;; information for the problem designer if the state is at
;;; or below a predicate being traced.  (definition of the trace
;;; code is below.)

(defparameter **Qs-Trace-Ops** ())

(defun qs-debug-printp (State)
  "Return t iff the state action or one of the preceeding 
state actions Is set to be traced."
 (or *actions* 
  (and (listp (car (st-actions State)))
        (or (and (cssg-p (car (st-actions State)))
		 (member (cssg-op (car (st-actions State))) **QS-Trace-Ops**))
	    (and (csop-p (car (st-actions State)))
		 (member (caadar (st-actions State)) **Qs-Trace-Ops**))
	    (and (st-predecessor State)
		 (qs-debug-printp (St-Predecessor State)))
	    ))))

;;  Characters for use in emacs outline minor-mode
;;  Allegro uses normal outline mode with a minimum of one asterisk
;;  sbcl uses lisp-mode outline mode with a minimum of 3 semicolons

(defun print-outline-indent (x &optional (charp T)) 
  (make-string (+ x #+sbcl 3 #+allegro 1)
  :initial-element (if charp #+sbcl #\; #+allegro #\* #\space)))

;;
;; Print outline header if state has multiple successors
;; or parent has multiple successors.
;;
;; The node is printed based on the content of st-actions, which is
;; defined in the routine note-action.  Currently, cases handled by 
;; executable-successors do not have actions defined. (May want to
;; do this later.)
;;
(defun qs-debug-print-success (state)
  "Prints a trace indicating that this state is a final state"
  (when (and (qs-debug-printp State) (st-actions state))
    (format t  "~A Success on ~S~%" 
	    (print-outline-indent (st-level state) (st-branches state)) 
	    (st-actions state))))

(defun qs-debug-print-failure (state)
  "Prints a trace indicating that this state had no successors"
  (when (and (qs-debug-printp State) (st-actions state))
    (format t   "~A Failed on ~S~%" 
	    (print-outline-indent (st-level state) (st-branches state)) 
	    (st-actions state)
	    ;;    (let ((top (first (st-stack state))))
	    ;;	(if (listp top)
	    ;;    (subst-bindings (st-bindings state) top)
	    ;;	  top))
	    )))

(defun qs-debug-print-node (state ns)
  "Prints a trace of a state that is a node"
  (when (and (qs-debug-printp State) (st-actions state))
    (format t "~A ~S~%"
	    (print-outline-indent (st-level state) 
				  (or (> ns 1) (st-branches state)))
	    (st-actions state))))

;; this is the old method for printing tree nodes; it does not
;; put the nodes in the right order however.  
;; Note that it is currently turned off.

(defun qs-debug-print-action (state)
  "Print action if state should be traced."
  (if (and (qs-debug-printp state) nil) ;turn on/off
      (format t "~2D> *** ~S~%" (st-level state) (st-actions state))))


;;; In order to add an operator to the debug tracing 
;;; it must be pushed onto the tracing stack by name.
(defun qs-traceop (&rest OpNames)
  "Trace the specified operator."
  (if (null OpNames)
      (setq **QS-Trace-Ops** nil)
    (setq **QS-Trace-Ops**
      (append **Qs-Trace-Ops** OpNames))))

;;; watch macro doesn't need to quote args.
;;; warn if name doesn't match any operator
;;; works entirely by side effects, printing qs-trace-ops
(defmacro watch (&rest OpNames)
"add named ops to the trace list; empty list if none"
  (if (null OpNames)
       (setq **QS-Trace-Ops** nil)
   (dolist (opName opNames)
      (if (not (get-operator-by-name opName))
         (format T "Warning: operator ~S not found~%" opName)
       (setq **qs-trace-ops** (reverse (adjoin OpName **qs-Trace-Ops**))))))
   ; macro function result form to be evaluated:
   '**QS-Trace-Ops**)

(defmacro unwatch (&rest OpNames)
"remove named ops from the trace list; empty list if none"
 (if (null OpNames)
       (setq **QS-Trace-Ops** nil)
   (dolist (opName opNames)
      (if (not (get-operator-by-name opName))
         (format T "Warning: operator ~S not found~%" opName)
       (setq **qs-trace-ops** (remove OpName **qs-Trace-Ops**)))))
   ; macro function result form to be evaluated:
   '**QS-Trace-Ops**)
