#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solver.cl
;; Kurt VanLehn and Collin Lynch
;; 12/11/2000
;;
;; This file defines the core element of the Andes2 solver system including
;; the central solver struct and the access functions.  This file as well
;; as all associated files are loacated in the Turkey-Solver package.
;;
;; This system is intended to function as a subsystem of a larger solution
;; system and is used to solve given a specified knowledge base of operators
;; and other data.
;;
|#


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
   (loop for State in (qsolve-for (list `(psm ,Goal ?eqn-id ?eqn-algebra ?unknowns)) Givens)
       collect (let ((PSM (find 'psm (St-WM State)
				:key #'car
				:test #'equalp)))
		 (setq **wm** (union (st-wm state) **wm** :test #'equalp))
		 (make-qsolres
		  :ID (nth 2 PSM)
		  :algebra (nth 3 PSM)
		  :nodes (mapcar #'cdr
				  (remove-if-not #'(lambda (wme) (and (equalp (car wme) 'variable)
								      (member (caddr wme) (nth 4 PSM))))
						 (St-WM State)))
		  :path (collect-path State)
		  :subeqns (collect-subeqns State)                              ;;Collect the subequations.
		  :subvars (collect-subvars State (nth 4 PSM))                  ;;Collect the variables.
		  :assumpts (collect-assumptions State)                          ;;Collect the assumptions.
		  :wm (St-wm State))))))                                        ;; Collect the wm.


(defun solve-for-Constant-Quantity (Goal Givens)
  "Solve for the quantity iff it is a Constant."
  (let ((R (merge-duplicate-PSMS
	    (loop for State in (qsolve-for (list `(psm ,Goal (STD-Constant ?V) ?eqn-algebra ?unknowns)) Givens)
		collect (let ((PSM (find 'psm (St-WM State)
					 :key #'car
					 :test #'equalp)))
			  (setq **wm** (union (st-wm state) **wm** :test #'equalp))
			  (make-qsolres
			   :id (nth 2 PSM)
			   :algebra (nth 3 PSM)
			   :nodes (mapcar #'cdr
					  (remove-if-not #'(lambda (wme) (and (equalp (car wme) 'variable)
									      (member (caddr wme) (nth 4 PSM))))
							 (St-WM State)))
			   :path (collect-path State)
			   :subeqns (collect-subeqns State)                              ;;Collect the subequations.
			   :subvars (collect-subvars State (nth 4 PSM))                  ;;Collect the variables.
			   :assumpts (collect-assumptions State)                         ;;Collect the assumptions.
			   :wm (st-wm State)))))))
    
    (cond ((> (Length R) 1)
	   (error "Multiple Constant Results for ~A~%" Givens))
	  (t (car R)))))


(defun solve-for-given-eqn (Goal Givens)
  "Solve for the general goal expression given Givens."

  (let ((R (loop for State in (qsolve-for (list `(Given-eqn ?eqn ,Goal)) Givens)
			      
	       collect (let ((Eqn (find 'Given-eqn (st-wm State)                    ;;For each returned state.
					:key #'car
					:test #'equalp)))
			 (setq **wm** (union (st-wm state) **wm** :test #'equalp))
			 (make-qsolres
			  :id (nth 1 Eqn)                                             ;;collect the eqn algebra
			  :nodes (find-if #'(lambda (V) (and (equalp (car V) 'Variable)  ;;Get the quantity variable.
							      (equalp (caddr V) Goal)))
					   (st-wm State))
			  :path (collect-path State)                                    ;;collect the paths.
			  :subeqns (collect-subeqns State)                                  ;;Collect the subequations.
			  :subvars (collect-subvars State (list Goal))                      ;;Collect the variables.
			  :assumpts (collect-assumptions State)                         ;;Collect the assumptions.
			  :wm (st-wm State))))))
    (cond ((eq 0 (length R))                                                        ;;If no values are returned.
	   (error "No return values specified for given: ~A ~%~A.~%" Goal Givens))  ;;Signal an error.
	  
	  ((< 1 (length R))                                                         ;;If more than one is returned
	   (merge-duplicate-givens R))                                              ;;merge the results.
	  
	  (t (car R)))))                                                                  ;;Otherwize return it.


(defun solve-for-param-var (Param Givens)
  "Solve for a parameter variable corresponding to param."
  (merge-duplicate-param-vars 
   (loop for State in (qsolve-for (list `(variable ?var ,Param)) Givens)
       collect (let ((Var (find-if #'(lambda (W) (unify W `(Variable ?Var ,Param))) (st-wm State))))
		 (setq **wm** (union (st-wm state) **wm** :test #'equalp))
		 (make-qsolres
		  :id (cadr Var)
		  :path (collect-path State)
		  :wm (st-wm State))))))



;;; Fix to deal with single selection problem.			      
(defun solve-for-non-quantity (goals givens)
  "Solve for the specified non-quantity goal."
  (let ((R) (P) (N))
    (loop for S in (qsolve-for goals givens)
	do (setq N (subst-bindings (st-bindings S) goals))
	   (setq P (find N R :key #'qsolres-id :test #'equalp))
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
  "Call the quantity solver for the gols with givens."
  
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
  "Collect the path of actions from State and it's preceeding states."
  (when (not (null State))
    ;;(pprint (st-actions State))
    (append (collect-path (St-Predecessor State))
	    (remove-if #'removable-actionp 
		       (St-actions State)))))

;; Some of the actions do not need to be recorded in the 
;; solution path.  These such as not. in-em, wm, bind and
;; test simply get eleiminated from the path as they are 
;; of not further use once the search is over.

(defun removable-actionp (action)
  (and (listp action)
       (and (eq (car action) 'sg)
	    (or (eq (car (cssg-goal action)) 'not)
		(eq (car (cssg-goal action)) 'bind)
		(eq (car (cssg-goal action)) 'test)
		(eq (car (cssg-goal action)) 'debug)
		(eq (car (cssg-goal action)) 'in-wm)))))
			

(defun collect-subvars (State BubbleVars)
  "Collect every variable statement in a state's WM, that is not in the Bubble."
   (loop for V in (St-wm State)
       when (and (equalp (car V) 'Variable)
		 (not (member (caddr V) 
			      BubbleVars
			     :test #'equalp)))
       collect V))


(defun collect-subEqns (State)
  "Collect every equation in a state's WM."
   (loop for E in (St-wm State)
       when (and (or (eq (car E) 'Eqn)
		     (eq (car E) 'Given-Eqn)
		     (eq (car E) 'Derived-Eqn)
		     (eq (car E) 'Implicit-eqn)))
		 ;;(not (equalp (nth 2 E) MainEqn)))
       collect E))


(defun collect-assumptions (State)
  "collect the set of (assume <assumption>) statements found in State."
  (loop for A in (St-wm State)
      when (equalp (car A) 'Assume)
      collect (cdr A)))


;;===========================================================================
;; Merge-duplicates
;; In solving for psms, given, eqns, etc we occasionally end up with
;; duplicate values the functions herew are used to merge those
;; suplicates top generate a single set as necessary.

(defun merge-duplicate-psms (PSMS)
  "Merge the identical psms into a single list.."
  (let ((tmp) (R (list (car psms))))
    (dolist (P (cdr psms))
      (setq tmp (merge-psm P R))
      (if tmp (setq R tmp)
	(push P R)))
    R))
  
(defun merge-psm (P Set)
  "Merge the specified PSM into the set of possible"
  (loop for P2 in Set                                     ;;If it matches another PSM
      when (and (equalp (qsolres-algebra P) (qsolres-algebra P2))                  ;;In equations
		(sets-equalp (qsolres-nodes P) (qsolres-nodes P2)))          ;;And quantities
	   
      do (when (or (sets-not-equalp (qsolres-subeqns P) (qsolres-subeqns P2)) 
                   (sets-not-equalp (qsolres-subvars P) (qsolres-subvars P2)) 
                   (sets-not-equalp (qsolres-assumpts P) (qsolres-assumpts P2)))
	   ; If paths differ in eqns, vars, or assumptions, it may indicate coding error (perhaps only one path
	   ; was intended) or it may be OK (most common case: where axes and body w/mass var are optional)
	   ; give warning trying to explain what the differences are here. Note: third path or higher will
	   ; almost always differ from the merge of 1 and 2. Maybe we ought to warn only when new path element
	   ; is not *subset* of existing path for paths beyond the 2nd to be merged (though we don't keep count)
	      (format t "WARNING: alternative way of generating ~A differs in~%" (qsolres-id P))
              (when (sets-not-equalp (qsolres-subeqns P) (qsolres-subeqns P2))   
	         (format t "subequations: ~A~%" (qsolres-subeqns P))          
		 (format t "        from: ~A~%" (qsolres-subeqns P2)))
	      (when (sets-not-equalp (qsolres-subvars P) (qsolres-subvars P2))   
	         (format t "variables:    ~A~%" (qsolres-subvars P))
		 (format t "       from:  ~A~%" (qsolres-subvars P2))) 
	      (when (sets-not-equalp (qsolres-assumpts P) (qsolres-assumpts P2))  
	         (format t "assumptions:  ~A~%" (qsolres-assumpts P))  
		 (format t "       from:  ~A~%" (qsolres-assumpts p2))) )
	 
	 (setf (qsolres-path P2) 
	   (merge-paths (qsolres-path P2) (qsolres-path P)))   ;;merge the two paths
	 (setf (qsolres-subeqns P2) 
	   (union (qsolres-subeqns P) (qsolres-subeqns P2)))   ;; Subeqns
	 (setf (qsolres-subvars P2) 
	   (union (qsolres-subvars P) (qsolres-subvars P2)))   ;; SubVars
	 (setf (qsolres-assumpts P2) 
	   (union (qsolres-assumpts P) (qsolres-assumpts P2) :test #'equalp))  ;; Assumptions.
	 (setf (qsolres-wm P2)
	   (union (qsolres-wm P) (qsolres-wm p2) :test #'equalp))
      and return Set))                                          ;;return the result.





(defun merge-duplicate-givens (Givens)
  "Given a list of equal givens merge those that can be merged."
  (let ((R (car Givens)))                               ;;The initial list of results.
    (dolist (G (cdr Givens))                         ;;For each G in the rest of givens.
      (if (or (sets-not-equalp (qsolres-subeqns R) 
			       (qsolres-subeqns G)) ;;Ensure that they match in
	      (sets-not-equalp (qsolres-subvars R) 
			       (qsolres-subvars G)) ;;Subeqns subvars and assumptions.
	      (sets-not-equalp (qsolres-assumpts R) 
			       (qsolres-assumpts G))) ;;Signalling a cerror if they do not.
	  (cerror "Accept merge and continue." "Difference in given psms ~A~%~A%" R G)
	(progn (setf (qsolres-path R) 
		 (merge-paths (qsolres-path R) 
			      (qsolres-path G)))   ;;Merge it into R
	       (setf (qsolres-wm R)
		 (union (qsolres-wm R) (qsolres-wm G)
			:test #'equalp)))))
    R))                                                 ;;Return R.




(defun merge-duplicate-param-vars (Params)
  "Merge identical param var values."
  (let ((R (car Params)))
    (dolist (P (cdr Params))
      (setf (qsolres-path R) 
	(merge-paths (qsolres-path R) 
		     (qsolres-path P)))
      (setf (qsolres-wm R)
	(union (Qsolres-wm R) (qsolres-wm P)
	       :test #'equalp)))
    R))
	      
      
		      
			      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The root of all the merge-duplicates functions is merge-paths.
;; this function locates the point at which the pair of paths diverge
;; and inserts a choose statement.

(defun merge-paths (P1 P2)
  "merge P2 path into P1."
  (let ((Loc (mismatch P1 P2 :test #'equalp)))
    
    (append (subseq P1 0 Loc)
	    
	    (if (or (not (listp (nth Loc P1)))
		    (not (equalp (car (nth Loc P1)) 'CHOOSE)))
		
		(list (list 'CHOOSE
			    (subseq P1 Loc)
			    (subseq P2 Loc)))
	      
	      (list (insert-choose (nth Loc P1) (subseq P2 Loc)))))))
	      
(defun insert-choose (C S)
  "Insert subsequence S into choose C."
  (let ((R)) 
    (loop for I below (length (cdr C))
	when (equalp (car S) (car (nth I (cdr C))))
	do (setq R C)
	   (setf (nth I (cdr R)) (merge-paths (nth I (cdr C)) S))
	and return t)
    
    (if (not R)
	(setq R (append C (list S))))
    
    R))
			       
			       
  
(defun solver-trace ()
  (trace solve-for)
  (trace solution-sts)
  (trace initial-st)
  (trace collect-path))

(defun trace-qsolver ()
  (trace solve-for-PSM-Quantity
	 solve-for-Constant-Quantity
	 solve-for-given-eqn
	 solve-for-param-var 
	 solve-for-non-quantity 
	 qsolve-for
	 
	 collect-Path
	 collect-subvars
	 collect-subEqns
	 collect-assumptions
	 
	 merge-duplicate-psms
	 merge-psm
	 merge-duplicate-givens
	 merge-duplicate-param-vars
	 merge-paths 
	 insert-choose))
	 
#|
  ;;; This file is the interpreter for Andes Script. Call (s2 sdd1) to
  ;;; try it out.  The following macros allows you to leave off the
  ;;; quote on the problem identifier.
  
  (defmacro s2 (Prob)
  `(solve2 (quote ,Prob)))
  
  (defmacro pwm (Prob)
  `(print-problem-wms (quote ,Prob)))
  
  (defmacro pa (Prob)
  `(print-problem-actions (quote ,Prob)))
  
  (defmacro psg (Prob)
  `(dump-sg (quote ,Prob)))
  
  (defmacro run (Prob)
  `(solve-dump-sg (quote ,Prob)))
  |#

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

#|(defun solve-dump-sg (Problem)
  (solve2 Problem)
  (with-open-file 
  (sg-stream (concatenate 'string *solver-path* (string Problem) ".sg")
  :direction :output :iff-exists :supersede)
  (dump-sg Problem sg-stream))
  t)
|#

;;; ===================== globals ===============================

;;; flags to control tracing (see also *debug* in executable2.cl and
;;; *lk-hack* in Physics-Funcs.cl)

(defparameter *actions* nil 
  "Controls whether note-action will trace.")

;;; action types used as part of actions that appear in action lists
;;; and solution graphs

(defconstant *do-operator* 'DO 
  "Action prefix meaning that an operator is executed")
(defconstant *next-operator* 'SG
  "Actin prefix meaning that a subgoal is being started for an operator")
(defconstant *goal-unified-with-fact* 'WM
  "Action prefix meaning that a goal unified with a working memory element")
(defconstant *goal-unified-with-effect* 'OP
  "Action prefix meaning that an operator was started")

;;; markers used in action lists of states and in solution graphs

(defconstant *split* 'split
  "Marks a non-deterministic split in the solution path")
(defconstant *next* 'next
  "Marks a end of one parallel branch and the beginning of another")
(defconstant *join* 'join
  "Marks the end of a set of parallel branches")
(defconstant *choose* 'choose
  "Marks a choice among alternative paths")


;;; ======================= st ====================================
;;; The st (short for state) struct represents a problem solver's
;;; state.  The working memory (wm) is the set of facts (ground atomic
;;; propositions) that the solver believes at this time.  The stack is
;;; an ordered list of operator instances, except for the top item on
;;; the stack, which might be a goal or an executable form instead of
;;; an operator instance.  The stuff in the stack has lots of
;;; variables in it.  Rather thatn continually substituting values
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
  bindings       ; A binding list (format defined by unifier.cl)
  wm	         ; A set of ground atomic propositions
  stack		 ; a list of operator instances (mostly)
  history	 ; a list of operator instance identifiers
  predecessor    ; the preceding state (represented as a st struct)
  actions	 ; list of s-expression recording the actions taken here
  successors	 ; states that come after this one (set by reverse-sg-links)
  )

;;; This is called by the Lisp system whenever it wants to print a st
;;; struct.

(defun print-st (st stream depth)
  "Prints the given state onto the stream"
  (declare (ignore depth))
  (let ((*print-length* 100)
	(*print-level* 20))
  (format stream "<st ~a~%>" 
	  (list (st-wm st)
		(st-bindings st)
		(st-stack st)))))

(defun push-wm (item state)
  "Adds the given item to the given state's working memory"
  (if (not (groundp item))
      (cerror "Ignore" "Adding non-ground proposition ~a to wm" item))
  (pushnew item (st-wm state) :test #'equalp))

(defun push-history (item state)
  "Adds the given item to the given states' history list"
  (if (not (groundp item))
      (cerror "Ignore" "Adding non-ground op inst identifier to history" item))
  (pushnew item (st-history state) :test #'equalp))

(defun push-binding (variable value state)
  "Adds the given variable-value pair to the state's bindings"
  (if (not (variable-p variable))
      (cerror "~a non-variable in push-bindings" variable))
  (setf (st-bindings state)
    (extend-bindings variable value (st-bindings state))))

;;; The following print functions are called from macros at the top level
#|
  (defun print-problem-wms (Prob)
  "Called from top level to print wms of all solutions to given problem"
  (let ((p (get-named-problem Prob))
  (N 1))
  (when p
  (loop for s in (problem-solutions p) do
  (format t "~&Working Memory for solution ~a.~%" N)
  (print-wm s)
  (incf N)))))
|#

(defun print-wm (st)
  "Prints the working memory in batches"
  (let ((predicates '(eqn vector axis-for variable)))
    (dolist (p predicates)
      (format t "~&Wm for ~a:~%" p)
      (dolist (wme (st-wm st))
	(if (eql p (first wme))
	    (format t "~a~%" wme))))
    (format t "The rest of wm:~%")
    (dolist (wme (st-wm st))
      (if (not (member (car wme) predicates))
	  (format t "~a~%" wme)))))

#|
  (defun print-problem-actions (Prob)
  "Called from top level to print actions of all solutions to given problem"
  (let ((p (get-named-problem Prob))
  (N 1))
  (when p
  (loop for s in (problem-solutions p) do
  (format t "~&Actions for solution ~a.~%" N)
  (print-acts s)
  (incf N)))))
  |#

(defun print-acts (st)
  "prints the actions of the given state"
  (cond ((null st) NIL)
	((st-p st)
	 (print-acts (st-predecessor st))
	 (print (st-actions st)))))

;;; =================== opinst =====================================
;;; The operator instance (opinst for short) struct represents an
;;; operator that is in the process of being satisfied.  It starts
;;; life as a sort of copy of the operator, with the variables
;;; standardized apart.  Teh subgoals slot holds a copy of the
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
  subgoals				; An ordered list of preconditions
  effects				; A set of effects (atomic propositions)
  identifier				; the name of the operator plus its arguments
  variables				; a list of the operats's help variables.
  )

;;; This is called by the Lisp system whenever it wants to print an
;;; operator instance.

(defun print-opinst (inst stream depth)
  "Prints the given operator instance on the given stream"
  (declare (ignore depth))
  (format stream "<opinst ~a>" (opinst-identifier inst)))


;;; ========================= solver ==============================
;;; The top level call is (solve2 <problem>) where the problem is an
;;; atom.  It looks up the problem to get the soughts and givens, then
;;; creates an initial state based on them.  It solves the problem,
;;; generating a possibly empty set of solution states, and stores it
;;; in the problem.  Macros in the Problem.cl file can be used to
;;; print the solutions of a problem in various ways).
#|
(defun solve2 (Problem)
  "Solves the given problem, stores the set of solution states 
  in it, and returns length of the set" 
  (let ((prob (get-problem Problem)))
    (when prob
      (print-problem-text Prob)
      (setf (problem-initial-state Prob)
	(initial-st (problem-soughts Prob) (problem-givens prob)))
      (setf (problem-graph Prob)
	(solution-sts (problem-initial-state prob)))
      (length (problem-solutions prob)))))
|#

;;; The problem might specify more than one goal, so the goal stack is
;;; initialized with an operator application for a dummy operator that
;;; has those goals as its preconditions.

(defun initial-st (Goals Givens)
  "Given atomic propositions representing a problem, 
   return an initial state for solving that problem"
  (Make-st :bindings no-bindings	; no-bindings is in unifier.cl
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
      ((null queue) solutions)
    ; in ACL IDE: pump input events each iter so kbd interrupts can get thru
    #+common-graphics (cg:process-pending-events)
    (setq state (pop queue))
    (cond ((null (st-stack state))
	   (push state solutions)
	   (qs-debug-print-success state))
	  ((null (setq successors (successors state)))
	   (qs-debug-print-failure state))
	  ((null (cdr successors))
	   (setq queue (cons (car successors) queue)))
	  (t
	   (setq queue (append successors queue))
	   (qs-debug-print-split state successors)))))



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
;;; 2. The opewrator has already been done in the history and therefore
;;;    is unnecessary so we will skip it.
;;; 3. The operator is done with no more subgoials and we can pop it off
;;;    of the stack.
;;; 4. The operator has some goals left so we move onto them.

(defun opinst-successors (inst state)
  "Given an operator instance, returns zero or more copies of the given state"
  (cond ((opinst-in-stack-p inst state) NIL)
	((opinst-done-already-p inst state) NIL)
	((null (opinst-subgoals inst)) (opinst-done inst state))
	(T (opinst-next inst state))))


;;; If the operator matches one that is already in the stack then we 
;;; want to prune this branch in order to avoid a loop.  This tests
;;; for two cases:
;;; 1. Exact matches:  "foo(A)" and "foo(A)"
;;; 2. Variable matches: "foo(?x)" and "foo(?y)"

(defun opinst-in-stack-p (inst state)
  "return t iff there exists a copy of inst in st-stack or one inst subsumes."
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Generate the ground id instance.
  (let ((ground-id (subst-bindings (st-bindings State) (opinst-identifier inst))))
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Search through the stack for a direct 
    ;; match or an expression subsumption. (unification.cl)
    (member-if 
     #'(lambda (Op) (weak-match-expressions 
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
	 (member ground-id (st-history state) :test #'equalp))))

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
  (let ((ground-id)(ground-effects)(ground-act)(ground-vars))
    (dolist (e (opinst-effects inst))
      (push (subst-bindings (st-bindings state) e) ground-effects)
      (push-wm (first ground-effects) state))
    (setf ground-id (subst-bindings (st-bindings state) 
				    (append (opinst-identifier inst))))
    (setf ground-act (subst-bindings (st-bindings state) 
				     (opinst-identifier inst)))
    (setf ground-vars (subst-bindings (st-bindings State)
				      (opinst-variables inst)))
    (if (not (groundp ground-id))
	(cerror "Ignore" "The op instance identifier ~a is not ground" 
		ground-id))
    (note-action state (list *do-operator* ground-act ground-effects ground-vars))
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
    (when (precond-macro-p subgoal)
      (setf (opinst-subgoals new-inst)
	(append (expand-precond-macro subgoal state)
		(opinst-subgoals new-inst)))
      (setf subgoal (pop (opinst-subgoals new-inst))))
    (push new-inst (st-stack state))
    (when subgoal                                                          ;;If a macro expands to nil
      (push subgoal (st-stack state))                                      ;;this prevents errors.
      (note-action state 
		   (list *next-operator*
			 (car (opinst-identifier inst))
			 (subst-bindings (st-bindings state) subgoal))))
    (list state)))

;;; This is called to generate successor states when the top of the
;;; stack is a goal.  It returns one state for each unification of the
;;; goal with an element of working memory or with an effect of an
;;; operator.

(defun goal-successors (goal state)
  "Given a new state, returns a copy of it for each unification of the given goal"
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
    (if action-flag (note-action new-state (list *goal-unified-with-fact* wme)))
    new-state))

;;; This is called by goal-successors to generate states via matching
;;; the goal to effects of operators.  The easy way to do this is to
;;; standardize the operator apart before unifying the goal with
;;; effects.  This standardization is necessary because this
;;; operator's variables are suppose to be unique to this instance.
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
  (let ((bound-goal (rename-variables (subst-bindings (st-bindings state) goal)))
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
		:identifier (cons (operator-name op) (operator-arguments new-op))
		:variables (operator-variables new-op)))
    (push inst (st-stack new-state))
    (setf (st-bindings new-state) bindings)
    (note-action new-state 
		 (list *goal-unified-with-effect*
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
  "Sets the actions field of the state to the action plus split/next/join markers.
   Prints trace if *actions* is true.  Use for its side-effects."
  (let ((act-type (car action)))
    (setf (st-actions state)
      (cond ((and (eql act-type *goal-unified-with-effect*)
		  (unordered-op-id (first (second action))))
	     (list action *split*))
	    ((and (eql act-type *next-operator*)
		  (unordered-op-id (second action)))
	     (list *next* action))
	    ((and (eql act-type *do-operator*)
		  (unordered-op-id (first (second action))))
	     (list *join* action))
	    (T  (list action))))
    (qs-debug-print-action State action)))

(defun unordered-op-id (op-name)
  "Non-null if the given operator instance identifier comes from an unordered operator"
  (let ((op (get-operator-by-name op-name)))
    (and op (member 'unordered (operator-features op)))))


;;; Although the printed version of the solution graph starts with the
;;; initial action as it root, the states are collected so that the
;;; final states are available to start with and we can use the
;;; predecessor slot to reach earlier ones.  If we collected the
;;; states so that they pointed to their successors, then we'd have to
;;; filter out failed paths, which would be a pain.  Thus, we collect
;;; the tree with the links going the wrong way, and then turn them
;;; around here.  After that, its just a matter of printing the sg.

#|
  (defun dump-sg (problem-id &optional (stream t))
  "Dumps a subgraph to the given stream"
  (let ((prob (get-named-problem problem-id)))
  (when prob
  (dolist (s (problem-solutions prob))
  (reverse-sg-links s))
  (print-sg (problem-initial-state prob)
  stream ""))))
  |#

(defun reverse-sg-links (state)
  "Given states with a precedessor link, fill in the successor links."
  (let ((p (st-predecessor state)))
    (when (and p (not (member state (st-successors p))))
      (push state (st-successors p))
      (reverse-sg-links p))))

(defun print-sg (root stream indent)
  "prints a tree of solution paths"
  (princ "(" stream)
  (print-path root stream indent)
  (princ ") " stream)
  (terpri stream))

(defun print-path (state stream indent)
  "Prints the indentation, then the actions in the state, then the rest of the path"
  (dolist (act (st-actions state)) (print-act act stream indent))
  (cond ((null (St-successors state)))
	((null (cdr (st-successors state)))
	 (print-path (car (st-successors state)) stream indent))
	(t (princ "(" stream)
	   (princ *choose* stream)
	   (terpri stream)
	   (dolist (alt (st-successors state))
	     (print-sg alt stream (concatenate 'string "   " indent)))
	   (princ ")" stream)
	   (terpri stream))))

(defun print-act (act stream indent)
  "Only prints the interesting actions"
  (when (interesting-action act)
    (princ indent stream)
    (prin1 act stream)
    (terpri stream)))

(defun interesting-action (act)
  "An action is interesting if it doesn't involve executables"
  (not (and (listp act)
	    (eql (car act) *next-operator*)
	    (executable-p (third act)))))


;;; ===================== utilities ===========================
;;; should be moved to utilities.cl eventually

(defun groundp (form)
  "True if the given form contains no variables"
  (cond ((variable-p form) NIL)
	((atom form) t)
	(t (and (groundp (car form))
		(groundp (cdr form))))))

(defun ground-expression (form bindings)
  "If all the variables in the given form are bound, 
   returns a ground instance of it. Otherwise, returns NIL"
  (and (all-boundp form bindings)
       (subst-bindings bindings form)))

(defun all-boundp (form bindings)
  "True if the given form's variables all have bindings"
  (cond ((variable-p form)
	 (get-binding form bindings))
	((atom form) t)
	(t (and (all-boundp (car form) bindings)
		(all-boundp (cdr form) bindings)))))


;;; ============================ QS-Debug Printing ========================
;;; The qs-debug-print-* functions allow for debug printing
;;; of only those states that are at or below a desired 
;;; operator.  The three functions below print relevant
;;; information for the problem designer if the stste is at
;;; or below a predicate being traced.  (definition of the trace
;;; code is below.)

(defparameter **Qs-Trace-Ops** ())

(defun qs-debug-printp (State)
  "Return t iff the state action or one of the preceeding 
state actions Is set to be traced."
 (or *actions* 
  (and (listp (car (st-actions State)))
        (or (and (eq 'sg (caar (st-actions State)))
		 (member (cadar (st-actions State)) **QS-Trace-Ops**))
	    (and (eq 'op (caar (st-actions State)))
		 (member (caadar (st-actions State)) **Qs-Trace-Ops**))
	   ;; (and (st-predecessor State)
		;; (qs-debug-printp (St-Predecessor State)))
	    ))))
  
(defun qs-debug-print-success (state)
  "Prints a trace indicating that this state is a final state"
  (if (qs-debug-printp State) (format t "Success~%")))


(defun qs-debug-print-failure (state)
  "Prints a trace indicating that this state had no successors"
  (if (qs-debug-printp State)
      (format t "Failed on ~a~%" 
	      (let ((top (first (st-stack state))))
		(if (listp top)
		    (subst-bindings (st-bindings state) top)
		  top)))))


(defun qs-debug-print-split (state successors)
  "Prints a trace indicating that the state has mulipled successors"
  (if (qs-debug-printp State)
      (format t "State has ~a successors.~%" (length successors))))


(defun qs-debug-print-action (state action)
  "Print action if state should be traced."
  (if (qs-debug-printp state)
      (format t "~a~%" action)))


;;; In order to add an operator to the debug tracing 
;;; it must be pushed onto the tracing stack by name.
(defun qs-traceop (&rest OpNames)
  "Trace the specified operator."
  (if (null OpNames)
      (setq **QS-Trace-Ops** nil)
    (setq **QS-Trace-Ops**
      (append **Qs-Trace-Ops** OpNames))))

; watch macro doesn't need to quote args.
; warn if name doesn't match any operator
; works entirely by side effects, printing qs-trace-ops
(defmacro watch (&rest OpNames)
"add named ops to the trace list; empty list if none"
  (if (null OpNames)
       (setq **QS-Trace-Ops** nil)
   (dolist (opName opNames)
      (if (not (get-operator-by-name opName))
         (format T "Warning: operator ~A not found~%" opName)
       (setq **qs-trace-ops** (reverse (adjoin OpName **qs-Trace-Ops**))))))
   ; macro function result form to be evaluated:
   '**QS-Trace-Ops**)

(defmacro unwatch (&rest OpNames)
"remove named ops from the trace list; empty list if none"
 (if (null OpNames)
       (setq **QS-Trace-Ops** nil)
   (dolist (opName opNames)
      (if (not (get-operator-by-name opName))
         (format T "Warning: operator ~A not found~%" opName)
       (setq **qs-trace-ops** (remove OpName **qs-Trace-Ops**)))))
   ; macro function result form to be evaluated:
   '**QS-Trace-Ops**)
