;;;;
;;;;            Problem Solving Methods, for both scalars and vectors
;;;;            Given and implicit equations
;;;;            Inheritance of quantities
;;;;            Optional steps
;;;;            Alternative problem types: drawing, multiple-choice, planning


;; ============== Top-level search support ===================
  
;; The top-level search for a solution is driven externally by the 
;; "Bubble Graph" generator written in Lisp.  It aims to build a network
;; of quantities and applicable equations containing them that may be 
;; searched to find sets of equations sufficient to solve for the problem 
;; sought(s).  In the course of this process the bubble graph generator 
;; repeatedly invokes the problem solver as a subroutine whenever it needs 
;; to generate an an equation containing a given quantity (top-level sought or
;; intermediate sought). The problem solver then uses the KB operators to 
;; generate an applicable equation containing the requested quantity.
;; 
;; Each invocation of the problem solver applies one problem solving method 
;; (PSM) to generate one result equation for a single sought quantity (although
;; subsidiary equations may be written as well).  No problem solver
;; state persists across invocations, so each invocation is entirely 
;; independent of any other. 
;; 
;; The Bubble-Graph generator invokes the problem-solver code to achieve a
;; top-level goal of applying a PSM to generate an equation for
;; a sought quantity. This goal is represented by a proposition of the 
;; form (PSM ?sought ?eqn-id ?eqn ?remaining-unknowns), with ?sought
;; bound to a quantity term coming in.  Thus this goal represents the
;; main top-level entry point into the problem solver.
;;
;; The operators for applying specific PSMs should result in a statement of
;; the form (PSM-applied ?sought ?eqn-id ?eqn-algebra) in wm.  These
;; operators do the real PSM-specific work of generating an equation.
;; The following operator encapsulates the common bookkeeping needed to
;; finish up the PSM application after some equation has been so generated.
;; It posts the final PSM statement the top-level driver keys on.
;; It does the following:
;;
;; 1. Sometimes a PSM might get applied and generate an equation which does not
;; actually contain the sought quantity. This can happen because the 
;; applicability tests for some PSMs check only that the equation *might* 
;; contain the sought quantity.  A test here fails in these cases. Since the 
;; final PSM statement is never posted, these blind alleys don't make it into 
;; the solution at all.
;;
;; 2. Here is common code to collect the residual unknowns from the equation 
;; for inclusion in the final "PSM" statement as a service to the bubble-graph
;; generator.
;;
;; 3. Some vars in the equation such may become known as a side effect of 
;; applying the PSM. For example, vector direction variables typically become 
;; known as the vectors are drawn by the relevant operators. In this case the
;; operator should add a "given" statement for them to wm since they are
;; now like the givens in not needing to be sought.  ("given" is being
;; used to mean "known").  We write equations for all these new knowns here 
;; since these equations are required for algebraic completeness.
;;
;; Some of these equations correspond to steps the student will have to write; 
;; others, in particular those for angles of drawn vectors, need not be
;; written by the student although they may be.  Issues pertaining to writing
;; equations for givens may have to be worked out later. For now we declare 
;; this operator unordered so that the given equation steps may occur in
;; any order with respect to the other steps.
;;
;; The hints pertaining to the top-level search for a solution must be
;; handled specially, because they should be questions: "What
;; quantity are you seeking?" and "What method should be used to find
;; it?"  These questions comprise the preamble discussed in the
;; proposal.  The specification document works out how the preamble
;; will be generated from the bubble graph.  Thus, there are no hints on 
;; operators that exist to support this top-level search.

(defoperator find-by-PSM (?sought ?eqn-id)
  :preconditions 
  (
   ;; save initially known quants for detecting changes below
   ;; This is initial givens plus quantities given as parameters
   (setof (in-wm (given ?quant . ?dont-care)) ?quant ?initial-givens)
   (setof (in-wm (parameter ?quant . ?dont-care)) ?quant ?parameters)
   (bind ?initial-knowns (union ?initial-givens ?parameters :test #'unify))
   ;; Main step: apply a PSM to generate an equation for sought.
   (PSM-applied ?sought ?eqn-id ?eqn-algebra)
   
   ;; collect list of quantities in the equation. 
   (map ?v (vars-in-eqn ?eqn-algebra)
	(in-wm (variable ?v ?q))
	?q ?quantities-in-eqn)
   ;; make sure sought quantity actually occurs in equation
   ;; Use "unify" to correctly handle keywords.
   (test (member ?sought ?quantities-in-eqn :test #'unify))
   
   ;; Some quantities in eqn may have become "given" -- known -- as side 
   ;; effects of applying the PSM. Here we call them the new-knowns.
   ;; We get what's known now and figure out what's changed.
   (setof (in-wm (given ?quant . ?dont-care))
	  ?quant ?given-now)
   (bind ?known-now (union ?given-now ?parameters :test #'unify))
   (bind ?new-knowns
	 (set-difference ?known-now ?initial-knowns :test #'unify))
   ;; (debug "Made known inside PSM: ~A~%" ?new-knowns)
   ;; Although the bubble driver won't have to seek them, for algebraic 
   ;; completeness we must put out equations giving values for 
   ;; the new-knowns.  Could also do this at all the points they become 
   ;; known but it's simpler to have one bit of code to do this.
   ;; we distinguish them as implicit equations since the student will
   ;; not have to write them. The algebra module still needs them.
   (foreach ?quant ?new-knowns
	    (implicit-eqn ?new-eqn ?quant))
   
   ;; Collect residual unknowns from eqn to include in the final PSM stmt 
   ;; This is a convenience to bubble-graph search driver, which will
   ;; see the equation and can easily get its variables but finds it 
   ;; inconvenient to map the variables to quantities.  Used to be:
   ;; (bind ?new-unknowns
   ;;      (set-difference ?quantities-in-eqn ?known-now :test #'unify))
   ;; But removing problem givens and parameters is no longer wanted. 
   ;; Driver now pre-enters problem givens and parameters into the graph 
   ;; before calling solver to find soughts, so now wants them included 
   ;; in order to know to link nodes for these quants to this equation. 
   ;; Since givens and parameters are flagged known the driver won't seek 
   ;; them.  We do exclude the quantities known by side effect since they 
   ;; shouldn't go into the bubble-graph. We get these by looking for
   ;; quantities for which implicit equations were written, either above
   ;; or in the operators -- for given vector dirs it happens in operators
   (setof (in-wm (implicit-eqn (= ?var (dnum . ?valunits)) ?imp-eq-quant))
	  ?imp-eq-quant ?imp-eq-quants)
   (bind ?new-unknowns 
	 (set-difference ?quantities-in-eqn ?imp-eq-quants :test #'unify))
   )
  :effects
  ((PSM ?sought ?eqn-id ?eqn-algebra ?new-unknowns)))


;;; =================== applying a scalar equation=================
;;; This is a generic "driver" operator for finding a problem solving 
;;; method (PSM) consisting of scalar equation containing the sought 
;;; quantity then writes the equation.  Whenever an author wants to define 
;;; a new scalar equation, such as speed=distance/duration, the author must 
;;; define operators for both the eqn-contains goal, which indicate which
;;; quantities are contained in the equation, and the eqn goal, which
;;; represents writing the equation on the Andes user interface.
;;; Because the reasoning done by this operator is covered by the
;;; preamble, it has no hints.

(defoperator apply-scalar-PSM (?sought ?eqn-id)
  :specifications "If the goal is to apply a PSM to find a quantity,
      and there is a scalar equation  that contains that quantity,
      then generate the equation."
  :preconditions 
  (
   (debug "~&Start apply-scalar-psm for ~S~%" ?sought)
   (inherit-or-quantity ?quant ?sought :and t) ;looking for children
   (debug "~&apply-scalar-psm quantity ~S~%" ?quant)
   (eqn-contains ?eqn-id ?quant)
    ;; make sure PSM name not on problem's ignore list:
   (test (not (member (first ?eqn-id) (problem-ignorePSMS *cp*))))
   (not (eqn ?dont-care ?eqn-id))
   (eqn ?eqn-algebra ?eqn-id)
   (debug "~&To find ~S~%    using ~S~%    via ~S,~%    wrote ~a~%"
	  ?sought ?quant ?eqn-id ?eqn-algebra)
   )
:effects
  ((PSM-applied ?sought ?eqn-id ?eqn-algebra)))

;;; ================== entering givens =============================
;;; This operator corresponds to the step of entering an "assignment
;;; statement" equation for given values.  Note there may be 
;;; prerequisite steps for defining the required variable; for example, 
;;; if the quantity is a vector magnitude the vector would have to be drawn.
;;;
;;; Since the main top-level driver never searches for ways to determine 
;;; givens, there is currently no problem-solving goal in the main search that 
;;; will cause this operator to be invoked. As a separate step, the top-level 
;;; driver can invoke the problem solver to achieve the goal of writing an 
;;; equation for each of the givens.  The top-level has to go through the
;;; solver for this simple step because the operators encode the knowledge 
;;; of how to define the variables.
;;;

(defoperator write-known-value-eqn (?quantity)
  :specifications "If a quantity's value is known, then define a variable 
    for it and write an equation giving its value"
  :preconditions 
  ;; right now, bubblegraph generator stops once it has found a given value
  ((wm-or-derive (given ?quantity ?value-expr 
			:hint (?given-loc ?more) 
			("in the problem statement" nil)))
   ;; Make sure expression is usable in equation, not special atom. 
   ;; Assume if a list its an algebraic expression
   (test (or (numberp ?value-expr) (listp ?value-expr)))
   (variable ?var-name ?quantity))
  :effects 
  ((given-eqn (= ?var-name ?value-expr) ?quantity))
  :hint
  ((point (string "You can find the value of ~A ~A."  
		  ?quantity (?given-loc identity)))
   (point (string "~@[~A  ~]The value of ~A is given as ~A." 
		  (?more identity) ?quantity (?value-expr algebra)))
   (bottom-out (string "Enter the equation ~A = ~A." 
		       (?var-name algebra) (?value-expr algebra)))
   ))

;; This variant handles the case where the known value is a z-axis direction
;; specified by special atom 'into or 'out-of 
;; For purposes of equation, we convert it to a phi angle with the z-axis 
;; of zero or 180 degrees. The phi angle is used in writing projections.
(defoperator write-known-zdir-eqn (?vector ?t)
  :preconditions
  (
   (in-wm (given (dir ?vector) ?dir . ?rest))
   (test (and (z-dir-spec ?dir)
	      (not (equal ?dir 'z-unknown))))
   (bind ?t (time-of ?vector))
   ;; ? use in-wm for variable here ?
   (variable ?var-name (dir ?vector))
   (bind ?dir-degrees (if (eql ?dir 'into) 180 0))
   )
  :effects 
  ((given-eqn (= ?var-name (dnum ?dir-degrees |deg|)) (dir ?vector)))
  :hint
  ((point (string "You know the numerical direction of ~A." ?vector))
   (point (string "The numerical direction of ~A is ~A deg." 
		  ?vector ?dir-degrees))
   (teach (string "For algebraic purposes, the direction of ~A is represented numerically as the angle it makes with the z-axis. This will be 0 deg if the vector points out of the plane and 180 deg if it points into the plane." ?vector))
   (bottom-out (string "Enter the equation ~A = ~A deg." 
		       (?var-name algebra) ?dir-degrees))
   ))

;; following variants write implicit equations for quantities like vector
;; directions or zero-magnitudes that become known as side-effects of 
;; other steps.  We don't attach hints to these since we don't want the 
;; help system to prompt the student to enter them explicitly. 
;; They have to be printed out as equations however for algebraic completeness 
;; of the set sent to algebra.
(defoperator write-implicit-eqn (?quantity)
  :specifications "If a quantity's value becomes known as a side effect of some other step, then define a variable for it and write an equation giving its value"
  :preconditions 
  ((in-wm (given ?quantity ?value-expr . ?rest))
   ;; Make sure expression is usable in equation, not special atom. 
   ;; Assume if a list its an algebraic expression
   (test (or (numberp ?value-expr) (listp ?value-expr)))
   ;; variable should already be defined 
   (in-wm (variable ?var-name ?quantity))) 
  :effects 
  ((implicit-eqn (= ?var-name ?value-expr) ?quantity)))

;; This variant handles the case where the known value is a z-axis direction
;; specified by special atom 'into or 'out-of 
;; For purposes of equation, we convert it to a phi angle with the z-axis 
;; of zero or 180 degrees. The phi angle is used in writing projections.
(defoperator write-implicit-zdir-eqn (?vector ?t)
   :preconditions (
     (in-wm (given (dir ?vector) ?dir . ?rest))
     (bind ?t (time-of ?vector))
     (test (and (z-dir-spec ?dir)
	        (not (equal ?dir 'z-unknown))))
     ;; variable should already be defined
     (in-wm (variable ?var-name (dir ?vector)))
     (bind ?dir-term (zdir-phi ?dir))
   )
   :effects (
     (implicit-eqn (= ?var-name ?dir-term) (dir ?vector))
   ))

;;; ================== applying vector equations ===================
;;; These operators are the second of the two main top level methods for
;;; finding a value for a variable.  They apply vector equations.
;;; The major steps are to draw a vector diagram, write the component
;;; equation, find expressions for each of the components, then write
;;; the final equation which is free of component variables.
;;; 
;;; The component equation is an equation where all the vector
;;; components are expressed in terms of component variables. 
;;; The author of a vector PSM, such as Newton's law or Kinematics, must 
;;; define operators for
;;; 
;;; *  eqn-family-contains -- indicates which quantities might be found 
;;;                           with this
;;; *  vector-diagram -- procedure for drawing the appropriate vectors
;;;                      and choosing the coordinate system
;;; *  compo-eqn-contains -- which quantities various component eqns find
;;; 
;;; This operator takes care of the rest of the job of applying
;;; the vector equation.  
;;; 
;;; Kinematics is treated as one vector PSM even though it has 5
;;; different component equations that can be used with it, all of
;;; which reference the same vector diagram.  This means that no
;;; matter which of the 5 equations the student will eventually write,
;;; the student must first draw all the kinematic vectors.  Hope this
;;; is OK with the instructors.
;;; 
;;; Because the reasoning done by these operators is covered by the
;;; preamble, they have no hints.

;;
;; Following applies vector PSMs writing component equations.
;; Note that compo-eqn-contains will be asserted by a vector PSM for
;; the *magnitude* of the relevant vector, so we have to temporarily
;; pretend we are seeking that when running through the generic equation
;; selection code. This avoids having to add new stuff to the existing
;; equation-contains stuff.
;;
(defoperator apply-vector-PSM (?sought ?eqn-id) 
  :specifications " If the goal is to apply a PSM to find a vector component,
      and there is a vector equation that contains the vector magnitude,
      then
      find a component equation that contains the quantity,
      and generate the component equation"
  :preconditions
  (
   (inherit-or-quantity ?quant ?sought :and t) ;looking for children
   ;; get any vector associated with ?quant
   (bind ?sought-vec (cond ((componentp ?quant) (compo-base-vector ?quant))
			   ((eq (first ?quant) 'mag) (second ?quant))
			   ((eq (first ?quant) 'dir) (second ?quant))
			   (t ?quant)))
   (eqn-family-contains ?vec-eqn-id ?sought-vec)
   ;; make sure PSM name not on problem's ignore list:
   (test (not (member (first ?vec-eqn-id) (problem-ignorePSMS *cp*))))
   (debug "~&To find ~S~%    using ~S,~%    drawing vectors ~S.~%" 
	  ?sought ?quant ?vec-eqn-id)
   (debug "Vectors drawn for ~a, axes ~A.~%" ?vec-eqn-id ?rot)
   ;; Different methods depending on type of quantity for ?sought
   (compo-eqn-selected ?vec-eqn-id ?quant ?eqn-id)
   ;;  write out equation itself
   (not (eqn ?whatever ?eqn-id))
   (eqn ?compo-eqn ?eqn-id)
   ;;
   (debug "Wrote compo eqn ~a.~%    ~a~%" ?compo-eqn ?eqn-id)
   )
  :effects ((PSM-applied ?sought ?eqn-id ?compo-eqn)))

;;;
;;; operators for applying vector PSM's
;;;
;;; The next three operators decide which component equation to
;;; generate and which axis to generate it along.  They get
;;; ?vec-eqn-id and a quantity as the first and second arguments of
;;; the effect.  They "return" an identifier for the selected compo
;;; equation.  They are smart enough to know that one should not
;;; select an axis that is perpendicular to the sought quantity.
;;; However, that only applies if that quantity is the magnitude or
;;; direction of a vector, which is why we need 3 versions of this
;;; operator: for magnitudes, for directions and for other quantities.
;;; Eventually, they should check that the time of the axis is
;;; compatible with the time of the sought quantity.  Currently that
;;; is not done because the semantics of times for axes is messed up.
;;; In particular, if we have two time points and seek the velocity at
;;; one of them, then we want to use linear kinematics and an axis
;;; with (during 1 2) as the time.  But time point 1 is not included
;;; in time interval (during 1 2) because intervals are considered
;;; open, not closed.  However, it would be odd to change the
;;; semantics of (during 1 2) so that it measn a closed interval when
;;; used on an axis and an open interval when used elsewhere.  Thus,
;;; we just ignore the times on axes until this can all be sorted out.

(defoperator select-compo-eqn-for-vector (?vec-eqn-id ?compo-eqn-name ?vector)
  :preconditions
  (
   (debug "start vector diagram for ~A at coord ~A~%" ?vec-eqn-id ?rot)
   ;; The sought vector component has a coordinate system associated
   ;; with it.  Force that coordinate system to be among the allowed 
   ;; coordinate choices.
   (add-to-wm (projection-axis ?rot))
   ;;
   (vector-diagram ?rot ?vec-eqn-id) ;draw vectors and axes first
   (wm-or-derive (compo-eqn-contains ?vec-eqn-id ?compo-eqn-name ?vector))
   (wm-or-derive (inherit-or-quantity ?vector ?parent))
   (in-wm (vector ?b ?parent ?dir))  ;get dir
   (test (non-zero-projectionp ?dir ?xyz ?rot)) ; = not known zero-projectionp
   (debug "finish vector diagram for ~A at coord ~A~%" ?vec-eqn-id ?rot)
   )
  :effects
  ((compo-eqn-selected ?vec-eqn-id 
		       (compo ?xyz ?rot ?vector) 
		       (compo-eqn ?compo-eqn-name ?xyz ?rot ?vec-eqn-id))
   (assume using-compo (?compo-eqn-name ?xyz ?rot ?vec-eqn-id))))

(defoperator select-projection-for-mag (?vec-eqn-id ?vector)
  :preconditions
  (
   (vector-diagram ?rot ?vec-eqn-id) ;draw vectors and axes.
   (wm-or-derive (inherit-or-quantity ?vector ?parent))
   (in-wm (vector ?b ?parent ?dir))  ;get dir
   ;; make sure this is not acheivable by the existing projection equations
   (test (not (member ?rot (cons 0 (minimal-x-rotations (list ?dir))))))
   (get-axis ?xyz ?rot)  ;iterate over directions
   (test (non-zero-projectionp ?dir ?xyz ?rot)) ;sanity test
   )
  :effects
  ((compo-eqn-selected ?vec-eqn-id (mag ?vector) 
		       (projection (compo ?xyz ?rot ?vector)))))

(defoperator select-vector-magnitude-for-mag (?vec-eqn-id ?vector)
  :preconditions
  (
   (vector-diagram ?rot ?vec-eqn-id) ;draw vectors and axes.
   (wm-or-derive (inherit-or-quantity ?vector ?parent))
   (in-wm (vector ?b ?parent ?dir))  ;get dir
   ;; verify this is not acheivable by the existing vector-magnitude equations
   (test (not (member ?rot (cons 0 (minimal-x-rotations (list ?dir))))))
   )
  :effects
  ((compo-eqn-selected ?vec-eqn-id (mag ?vector) 
		       (vector-magnitude ?vector ?rot))))


;;; This operator suggests applying a vector equation in order to find
;;; a scalar.  

(defoperator select-compo-eqn-for-scalar (?vec-eqn-id ?compo-eqn-name ?quantity)
  :specifications 
   "If the sought quantity is a scalar,
      and ?compo-eqn-name is a component equation for the given vector equation
        that could contain that quantity,
   then select the component equation along any axis." 
  :preconditions
   ((test (scalar-quantityp ?quantity))
    (vector-diagram ?rot ?vec-eqn-id) ;draw vectors and axes first
    (wm-or-derive (compo-eqn-contains ?vec-eqn-id ?compo-eqn-name ?quantity))
    (debug "choosing compo to apply ~A to find scalar ~A~%"   
	   ?compo-eqn-name ?quantity) 
    (get-axis ?xyz ?rot) ;iterate over ?xyz
    )
  :effects
  ((compo-eqn-selected ?vec-eqn-id
		       ?quantity 
		       (compo-eqn ?compo-eqn-name ?xyz ?rot ?vec-eqn-id))
   (assume using-compo (?compo-eqn-name ?xyz ?rot ?vec-eqn-id))))


;;; =================  Inheritance of quantities =============================
;;;
;;;  When a quantity is constant over a larger interval of time,
;;;  it should also be used over shorter time intervals.
;;;  Also, quantities are sometimes timeless.
;;;  Likewise for spatial homogeneity and for currents in a circuit.
;;;
;;;  Such inheritance is achieved via the (inherit-quantity ?child ?parent)
;;;  proposition.  There are two places where this is invoked.
;;;  First, in find-by-PSM and apply-vector-PSM, one looks for equations  
;;;  that contain the ?sought or contain any ?child of the sought.
;;;  Second, when a variable is defined or a vector is drawn, we
;;;  want the parent quantity to be used.
;;;  
;;;  Generally, PSM's involving only one quantity, like vector
;;;  component equations, should not use inheritance, since there 
;;;  should be only one instance of that PSM.  The vector drawing
;;;  or variable definition operator should determine if the quantity
;;;  can be defined on the user interface.

;;;  There is a subtlety with the SGG:  if (inherit-quantity ...) has 
;;;  already been calculated previously in the solution graph, with the
;;;  result in working memory, then this operator will fail.  Thus, we
;;;  have two versions:  one that unifies with working memory, and one
;;;  that calculates (inherit-quantity ...) anew.

(defoperator inherit-quantity-new (?child)
  :preconditions 
  (
   (test (or (and ?child (groundp ?child)) (and ?parent (groundp ?parent))
	     (error "inherit-quantity:  child ~A or parent ~A must be grounded"
		    ?child ?parent)))
   (not (inherit-quantity ?child ?parent :composite ?allowed))
  ; (test (or (format t "inherit-quantity-new 1 for ~s ~S~%" ?child ?parent) t))
   (inherit-quantity ?child ?parent :composite ?allowed)
  ; (test (or (format t "inherit-quantity-new 2 for ~s ~S~%" ?child ?parent) t))
   ;; Test that parent has no further ancestors
   (setof (inherit-quantity ?parent ?ancestor) ?ancestor ?ancestors)
   (test (null ?ancestors))
   )
  :effects ((inherit-or-quantity ?child ?parent :and ?flag)))

(defoperator inherit-quantity-wm (?child)
  :preconditions 
  (
   (test (or (and ?child (groundp ?child)) (and ?parent (groundp ?parent))
	     (error "inherit-quantity:  child ~A or parent ~A must be grounded"
		    ?child ?parent)))
   (in-wm (inherit-quantity ?child ?parent :composite ?allowed))
   ;; Test that parent has no further ancestors
   (setof (inherit-quantity ?parent ?ancestor) ?ancestor ?ancestors)
   (test (null ?ancestors))
   )
  :effects ((inherit-or-quantity ?child ?parent :and ?flag)))

;; Rule applies when ?quant has children.
(defoperator quantity-no-inherit (?quant ?flag)
  :preconditions 
  (
   (test (or (and ?quant (groundp ?quant))
	     (error "quantity-no-inherit:  ~A must be grounded" ?quant)))
   ;; Apply rule if ?flag or ?quant has no parent.
   (setof (inherit-quantity ?quant ?parent) ?parent ?list1)
   (test (or ?flag (null ?list1)))
   )
  :effects ((inherit-or-quantity ?quant ?quant :and ?flag)))

;; The method used here avoids an infinite recursion.
;; There may be more than one ?parent for a given ?child, ?grandparent pair
(defoperator composite-1 (?child ?parent ?grandparent)
  :preconditions 
  (
   (test (and ?child (groundp ?child)))
   (inherit-quantity ?child ?parent)
   (inherit-quantity ?parent ?grandparent :composite ?allowed)
   )
   :effects ((inherit-quantity ?child ?grandparent :composite 1)))

(defoperator composite-2 (?child ?parent ?grandparent)
  :preconditions 
  (
   (test (and ?grandparent (groundp ?grandparent)))
   (inherit-quantity ?parent ?grandparent)
   (inherit-quantity ?child ?parent :composite ?allowed)
   )
   :effects ((inherit-quantity ?child ?grandparent :composite 2)))

(defoperator inherit-proposition (?prop ?child)
  :preconditions (?prop ;may not be fully bound, must come first
		  (inherit-or-quantity ?child ?parent))
  :effects ((inherit-proposition ?child ?parent ?prop)))

(defoperator define-inherit-variable (?quant)
  :preconditions 
   ;; When defining a variable, ?quant is already fully bound.
  (
   (inherit-or-quantity ?quant ?parent)
   (variable ?q-var ?parent)
   )
  :effects ((inherit-variable ?q-var ?quant)))

(defoperator inherit-rate-of-change (?child)
  :preconditions ((inherit-quantity ?child ?parent))
  :effects ((inherit-quantity (rate-of-change ?child) 
			      (rate-of-change ?parent))))

(defoperator draw-inherit-vector (?vec)
  :preconditions ((inherit-or-quantity ?vec ?vec-parent)
		  (vector ?b ?vec-parent ?dir))
  :effects ((inherit-vector ?b ?vec ?dir)))

(defoperator inherit-vector-mag (?child)
  :preconditions ((inherit-quantity ?child ?parent))
  :effects ((inherit-quantity (mag ?child) (mag ?parent))))

(defoperator inherit-vector-dir (?child)
  :preconditions ((inherit-quantity ?child ?parent))
  :effects ((inherit-quantity (dir ?child) (dir ?parent))))

(defoperator inherit-vector-compo (?xyz ?rot ?child)
  :preconditions ((inherit-quantity ?child ?parent))
  :effects ((inherit-quantity (compo ?xyz ?rot ?child) 
			      (compo ?xyz ?rot ?parent))))

;; Inheritance is appropriate if a quantity is declared constant,
;; This may render inherit-constant-value obsolete; see Bug #1002.
(defoperator inherit-when-constant (?quant1)
  :preconditions 
  (
   ;; Here we assume ?t-constant is widest possible interval
   (constant ?quant ?t-constant :inclusive ?flag)
   (time ?t-constant)	  ; sanity test
   (time ?t1)
   (test (and (not (equal ?t1 ?t-constant))
	      (if ?flag
		  (tinsidep-include-endpoints ?t1 ?t-constant)
		  (tinsidep ?t1 ?t-constant))))
   (bind ?quant1 (set-time ?quant ?t1))
   (bind ?quant2 (set-time ?quant ?t-constant))
   )
:effects ((inherit-quantity ?quant1 ?quant2)))


;;; =================== Generic: Optional steps =============================
;;;
;;; A goal of form (optional ?goal-form) is one that can be skipped.
;;; The help system will likely want to deal with these subgoals specially
;;; when giving help.  Here we define two generic operators, do-optional-step 
;;; and skip-optional-step, to provide two ways of achieving one of these 
;;; forms: one by achieving the embedded goal, the other by just asserting it 
;;; achieved without doing it. 
;;;
;;; Note ?goal will be bound to some more determinate proposition by 
;;; unification with effects when these operators are invoked. It must
;;; be fully grounded so it can be added to wm in the skip case.

(defoperator do-optional-step (?goal)
  :specifications "If you have an optional step to do then do it"
  :preconditions ( 
      ?goal ; embedded goal proposition to be achieved
  )
  :effects ( (optional ?goal) ))

(defoperator skip-optional-step (?goal)
  :specifications "let's not and say we did"
   :effects ( (optional ?goal) ))

;;; =================== Generic: planning only problems =====================

; For Sandy Katz planning only preparatory problems, the only goal is
; to read the problem statement then click a check box when done to trigger
; a dialog about how to plan the solution. The following trivial operator just 
; achieves the dummy goal (read-problem) used as sought for these problems. 
; Note there are no entries in the solution to such a problem.
(defoperator read-problem ()
   :effects ((read-problem)))

;; define English for goal used as sought in planning-only problem 
(def-goalprop read-problem (read-problem)
  :english ("reading the problem statement then indicating you have done so."))

;;; =================== Generic: Multiple choice answer ======================
;;;
;;; This is just a hack to enable simple multiple choice questions within our 
;;; problem/solution graph format. Multiple choice questions coded this way
;;; will not have any help.  The problem soughts will just be the goals of
;;; picking the correct multiple choice answers, of the form
;;;      (choose-answer question-id correct-choice)
;;; Different questions can be distinguished by ordinal position within
;;; list of soughts.
;;; !!! Someday we may want to extend this to enable some helpful feedback on 
;;; some wrong answers, say.
(defoperator select-mc-answer (?question-id)
  ; no preconditions!
  :effects ((choose-answer ?question-id ?correct-choice)))

;;; ================= Generic: Draw each requested vector =====================
;;;
;;; A goal of form (draw-vectors . ?vector-list) gives a list of vector 
;;; quantities to draw. This is a convenience used for batching a list of 
;;; vectors into a single goal to be the sought for a non-quantititive problem.
;;; That is needed by our implementation so we can have a single "done" button 
;;; to mean that all parts are done, since our implementation associates 
;;; buttons with soughts.  May not be needed if we fix our implementation to 
;;; understand a button meaning "done-all-parts".
;;; Used on qualitative magnetism problems mag1a, mag1b
(defoperator draw-required-vectors (?vector-list)
   :preconditions (
		   ;; draw vectors, saving a list of distinct axis owners
		   (map ?vector ?vector-list
			(vector ?b ?vector ?dir) ?b ?bbb)
		   (bind ?bb (remove-duplicates ?bbb))
		   ;; Drawing axes allowed, see draw-standard-fbd
		   (foreach ?b ?bb (optional (fbd-axes-drawn ?b)))
		   ;; Allow drawing of any body in problem
		   (foreach ?b ?bb (optional (body ?b)))
		   )
   :effects ( (draw-vectors ?vector-list) ))

;;;;
;;;;   Do motion diagrams as described in the first Chapter of Knight
;;;;
;; this goal used as sought in vector-drawing-only problem (magtor*)
(def-goalprop motion-diagram (motion-diagram ?b . ?rest)
  :english ("drawing a motion diagram for ~A" (nlg ?b)))

(defoperator do-motion-diagram (?b)
  :preconditions 
  ( 
   (foreach ?time ?times (body ?b :time ?time)) 
   (bind ?intervals (mapcar #'(lambda (a b) `(during ,a ,b)) 
			  ?times (cdr ?times)))
   (foreach ?interval ?intervals 
	    (vector ?b (velocity ?b :time ?interval) ?dir))
   (bind ?atimes (butlast (cdr ?times)))
   (foreach ?atime ?atimes 
	    (vector ?b (accel ?b :time ?atime) ?dir))
   )
  :effects ( (motion-diagram ?b ?times) ))

;;; ========== Generic knowledge about equal and proportional quantities ======
;;;
;; Equal quantities: this is a generic operator that writes the
;; equality between scalar quantities in cases where it can be determined
;; that two differently-defined quantities are equal. In some problems
;; this may be given or derived by special rules from the problem situation, 
;; e.g.  in a round trip, distance travelled out = distance travelled back.
;; We use the generic "equals" rather than writing out the specific equality
;; first, so this info can be put in the givens; and second, so that it can
;; be used to mark cases where the substitution of equal quantities may be 
;; made by the student in the head. In cases where the equality is based on a 
;; fundamental principle like Newton's third law we label it as such and
;; do not use the (equals ...) proposition.
;; Although most hints for equations are hung off the operator whose
;; effects include (eqn...), in this case, the hints are hung off the operator
;; whose effects include (equals ...).
;;
;; On symmetry:  Andes does not really have a method to express symmetries
;; properly, since that would involve a coordinate transformation (for spatial
;; symmetries) or a transformation over objects for other symmetries.
;; Thus, we use (equals ...) to express symmetries, 
;; with the Keyword :hint to express any special hints and :opposite to express
;; quantities that are odd under a symmetry.

(defoperator equality-contains (?quant)
  :preconditions 
  (
   (equals ?quant1 ?quant2 :opposite ?flag . ?whatever)
   (any-member ?quant (?quant1 ?quant2))
   )
  :effects
  ((eqn-contains (equals ?quant1 ?quant2 :opposite ?flag) ?quant)))

(defoperator write-equality (?quant1 ?quant2)
  :preconditions 
  (
   ;; found in equality-contains above
   (in-wm (equals ?quant1 ?quant2 :opposite ?flag 
			       :hint ?hint "The two quantities are equal."))
   (variable ?v1 ?quant1)
   (variable ?v2 ?quant2)
   (bind ?v2-term (if ?flag `(- ,?v2) ?v2)))
  :effects 
  ((eqn (= ?v1 ?v2-term) (equals ?quant1 ?quant2 :opposite ?flag)))
  :hint
  ((point (string "How are ~A and ~A related to each other?" 
		  (?v1 algebra) (?v2 algebra)))
   (teach (string ?hint))
   (bottom-out (string "You can write the equation ~A = ~A." 
		       (?v1 algebra) (?v2-term algebra)))
  ))


;;; generic principle when given one quantity as a fraction of another
;;; Totally generic (like equals), can be used for any quantities
;;; Problem givens should specify
;;;     (fraction-of ?quant1 ?fraction ?quant2) 
;;; to mean quant1 = fraction*quant2

(def-PSMclass given-fraction (given-fraction ?q1 ?q2)
  :complexity connect ;just like (equals ...)
  :short-name "fraction of"
  :english ("one quantity as given fraction of another")
  :eqnFormat ("val1 = fraction*val2"))

(defoperator given-fraction-contains (?sought)
  :preconditions (
		  ;; typically, this means fraction-of must be in
		  ;; the givens when defining a problem
		  (in-wm (fraction-of ?q1 ?fraction ?q2))
		  (any-member ?sought (?q1 ?q2))
		  )
  :effects ( (eqn-contains (given-fraction ?q1 ?q2) ?sought)
	     ))

(defoperator given-fraction (?q1 ?q2)
  :preconditions (
		  (in-wm (fraction-of ?q1 ?fraction ?q2))
		  (variable ?v1 ?q1)
		  (variable ?v2 ?q2)
		  (bind ?fracmult (if (and (numberp ?fraction) (< ?fraction 1))
				       "fraction" "multiple"))
		  )
   :effects ( (eqn (= ?v1 (* ?fraction ?v2)) (given-fraction ?q1 ?q2)) )
   :hint (
	  (point (string "You can determine ~A as a ~A of ~A from the problem statement" ?q1 (?fracmult identity) ?q2)) ;bypass nlg
	  (bottom-out (string "Write the equation ~A" 
			      ((= ?v1 (* ?fraction ?v2)) algebra)))
	  ))

