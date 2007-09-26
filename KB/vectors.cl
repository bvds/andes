;;;;
;;;;       Axes, coordinate systems, projections
;;;;       vector functions
;;;;       geometric stuff like moment of inertia
;;;;


;;; =============== bodies =======================================


;;; This  models using the Andes body tool.  The body tool both draws a body
;;; which is represented by the "body" predicate, and it defines a variable
;;; for the body's mass, which is represented by the "variable" predicate.
;;; The operator can be validly applied to any object and time, so those are
;;; specified in the conditions.  However, it will usually be called by
;;; unifying the (body ?b) effect with a goal that provides bindings for
;;; the body and time.  Thus, most frequently, ?b will be bound at the
;;; conditions are subgoalled on. 
;;; 
;;; Because Andes won't let students define two variables for the same 
;;; quantity, this operator checks to make sure that a mass variable has not 
;;; already been defined for this body, either by the body tool or the 
;;; variable definition tool. 

;;; This assumes you can draw a body at *any* defined time, something which 
;;; might be done to achieve the goal of defining a (timeless) mass variable 
;;; for a body. But this is not right for compound bodies in momentum problems 
;;; with splits or joins, in which the compound doesn't really exist
;;; at all times in the problem. So we have a separate operator to draw 
;;; compound bodies that checks to make sure the time is not ruled out by 
;;; specificiation of a split or join collision (see linmom).

(defoperator inherit-timeless-body (?b ?t)
  :preconditions ((test (not (member 'body-time (problem-features *cp*))))
		  (time ?t))
  :effects ((inherit-quantity (body ?b :time ?t) (body ?b))))

(defoperator draw-body (?b ?t)
  :preconditions 
  (
   (object ?b)
   ;; only use time when allowed by feature body-time
   ;; This is a sanity test to ensure inherit-quantity is working OK.
   (test (or (eq (null ?t) 
	     (null (member 'body-time (problem-features *cp*))))
	     (error "time slot ~A not consistant with problem features" ?t)))
   )
  :effects ((body ?b :time ?t)) 	
  :hint
  ((point (string "It is a good idea to begin by choosing the body or system of bodies you are going to focus on."))
   (teach (string "First figure out which object you want to apply the principle to, and if necessary, what time or time interval to analyze.  Then use the body tool (looks like a dot) to indicate your selections."))
   (bottom-out (string "You should use the body tool to draw a body choosing ~a as the body." (?b at-time ?t)))
   ))


;;
;; Compound bodies = (compound orderless body1 body2 ... bodyn)
;;
;; Bodies given as moving together may be treated as a single unit.
;; This is specified in using (assume move-together orderless b1 b2 ...).
;; We don't make any other use of the move-together proposition beyond
;; forming compounds; we could use it directly in the future for things 
;; like carried objects but it is not currently used for that.
;;
;; The time of moving together is not used in forming compound bodies.
;; We could try saying that the compound only exists at certain times, e.g.
;; after merging in a collision, but our object statement doesn't have a 
;; time slot.  In effect we pretend that objects *exist* at all times. 
;; This is OK since it may be that their kinematic attributes may not be 
;; determinable at all times, e.g.  can't expect to find a velocity for
;; the resulting compound at times before the merge. 
;;
;; With compounds, motion attributes might in principle have to propagate 
;; from part to whole, from whole to part, or from part to other part.
;; 1. The compound object inferring operators require a motion spec for 
;; at least one of the parts and assert the qualitative motion of the 
;; compound to be the same. This motion spec is needed for drawing kinematic 
;; vectors for the compound. 
;; 2. There can also be cases where qualitative motion is given for a compound 
;; and must be propagated to the parts. This is natural in some problems with 
;; splits/joins of bodies, in which the givens describe motion of a 
;; compound before or after.  In this case the compound object should be 
;; asserted to exist in the problem statement, and another operator will 
;; propagate the motion description to the parts.
;; 3. Ought to be able to propagate motion properties including given values
;; from one part directly to another if they are given to move together, but
;; currently the only way to effect this is by an equation going through the 
;; compound.
;;
;; These operators make an entry drawing the compound body. This is necessary
;; on the workbench before any attributes of a compound may be defined. 
;; However, in some cases these operators are not used because we are either
;; given the existence of a compound in the problem statement or are given
;; some attribute, e.g. velocity, of a compound in the problem statement.

;; if two bodies are in contact and both are at rest they may be treated
;; as a compound. 
;; !!! Currently only applies to exactly two bodies
;; Could use function so search would aggregate existing bodies including
;; compounds into progressively larger compounds, but would be tricky. 
(defoperator form-compound-at-rest (?bodies)
  :preconditions (
   (in-wm (motion ?b1 at-rest :time ?t))
   (supports ?b1 ?b2 . ?dont-care) ;should only be written for atomic b1, b2 
   (in-wm (motion ?b2 at-rest :time ?t))
   (bind ?bodies (sort (list ?b1 ?b2) #'expr<))
   (not (object (compound orderless . ?bodies)))
  )
  :effects (
    (object (compound orderless . ?bodies))
    (motion (compound orderless . ?bodies) at-rest :time ?t)
    (body (compound orderless . ?bodies))
  )
  :hint
  ((point (string "Notice that ~A stay together as a unit ~A." 
		  (?bodies conjoined-defnp) (?t pp)))
   (teach 
      (kcd "draw_conpound_lk_body")
      (string "When two bodies move together you can often simplify your solution by treating the two of them as a single body. This is called a compound body."))
   (bottom-out (string "Use the body drawing tool and select a list of bodies to introduce the compound body consisting of ~a ~A." (?bodies conjoined-defnp) (?t pp)))
   ))


;;; =============================== axes =========================
;;; Although the axis-drawing code is only called from vector-diagram
;;; code, it is included here because it is general, and doesn't
;;; depend on the particular kind of vector diagram being drawn.
;;; Because the Andes axis drawing tool always produces both x and y
;;; axes, even if only one is needed, this code always produces both.

;;; This first operator models drawing standard, unrotated axes.  That
;;; is, the x-axis has 0 direction and the y-axis has 90 direction.
;;; It applies only when there are no vectors whose angles are known
;;; numerically.  If there are such vectors, then another operator
;;; applies and draws axes alligned with one of the vectors.  This
;;; prevents drawing standard axes when there are no vertical or
;;; horizontal vectors.
;;;
;;; Vectors along the z-axis are used in rotational problems. Althought there is
;;; only one possible setting for the z-axis, Andes currently requires the 
;;; axes to be drawn before component notation is involved. We use a special
;;; case operator for this in order to give a custom hint.
;;; !!! We might need a new operator for this if ever needed an axis in case 
;;; where rotated vectors in the x-y plane had been drawn in addition 
;;; to the z-axis vectors on the goal body.

;;; !!! Currently this operator *only* applies in following cases:
;;;   - cons-linmom, to achieve axis for many-body system
;;;   [- rotational problems, to achieve z-axis for z-compo eqn ] -- taken out!
;;  But both of these uses ought to have specialized hints.

;; iterate over axes in a given coordinate system
;; Right now, this doesn't do much, but it allows for generalization
;; of the coordinate system.
(defoperator get-axis-from-coordinate-system (?xyz ?rot)
   :preconditions ( (any-member ?xyz (x y z)) )
   :effects ( (get-axis ?xyz ?rot)))

(defoperator draw-unrotated-axes ()
  :specifications 
   "If  there are no vectors with numerical directions,
   then draw unrotated coordinate axes"
   :preconditions 
   ( ; we want different hints for standard axes in these cases:
     (not (component-form))
     (not (use-energy-axes))	  
     ;; don't draw axis for system part if system axis already chosen
     ;; use-system-axes will choose axes in this case.
     (not (axes-for ?sys . ?dontcare)
	  (part-of-sys ?b ?sys))
     (not (vector ?b ?v ?dir) (degree-specifierp ?dir))
     )
   :effects 
   (
    (draw-axes ?b 0) ; action proposition for help system gives x dir
    (axes-for ?b 0)
    (assume axes-for ?b 0)
    )
  :hint
  (;;(point (string "Although one usually rotates the x-y coordinate system to align axes with vectors, there are no vectors on the system of interest with known directions in the x-y plane."))
   ;;(teach (string "When you don't have any vectors in the x-y plane with which to align your coordinate system, you might as well draw a standard horizontal-vertical coordinate system."))
   (teach (string "In this problem, there is no strong reason not to use a standard horizontal-vertical coordinate system."))
   (bottom-out (string "Draw a standard horizontal-vertical coordinate system setting the positive x axis at 0 degrees."))
   ))

;; following draws standard axes when problem seeks horizontal or vertical 
;; components. We don't want to draw vector-aligned axes in this case. It
;; is possible that draw-unrotated-axes could apply in the same case if it
;; applies when no vector is drawn; however, in order to always get this hint, 
;; we suppress that with test in draw-unrotated-axes.
;;
;; !!! Following not done yet, causes problems with multiple choices:
;; Include body and time parameters so can be applied as often as needed to
;; achieve axis-for any body. This means there will be multiple
;; draw-axes entries -- perhaps within a single operator! -- but they 
;; will all be marked done by the same entry since body and time are removed 
;; in forming entry propositions for use at help time.
;;
(defoperator draw-compo-form-axes ()
  :specifications 
   "If the problem is seeking horizontal or vertical components then use standard axes"
  :preconditions (
    (component-form)
    ; don't draw new axis for system part if system axis already chosen
    ; use-system-axes will choose axes in this case.
    (not (axes-for ?sys . ?dontcare)
         (part-of-sys ?b ?sys))
    )
  :effects (
   (draw-axes ?b 0) ; action proposition for help system gives x dir
   (axes-for ?b 0) 
   (assume axes-for ?b 0)
  )
  :hint
  ((point (string "Although one usually rotates the x-y coordinate system to align axes with vectors, since this problem gives or seeks horizontal or vertical vector components, you should use a standard horizontal-vertical coordinate system."))
   (bottom-out (string "Draw a standard horizontal-vertical coordinate system setting the positive x axis at 0 degrees."))
   ))

;; reuse-other-body-axes only applies once to draw axes within a given call to
;; the problem solver, registering those as the "axes-for" vectors on some
;; original body. Following operator reports the standard axes as the 
;; "axes-for" vectors on *another* body in case draw-compo-form axis has 
;; already been called to draw the axes.  This is needed in at least one case 
;; where we give the equality between components of two vectors on different 
;; bodies, and need to define compo vars for each via draw-compo2, 
;; hence need an axis for each.

;; following applies for same purpose when not component-form
(defoperator reuse-other-body-axes (?b)
  :preconditions 
  (
   (in-wm (draw-axes ?drawn-axes-body ?rot))
   ;; don't need this in addition if already registered an axis for 
   ;; vectors on ?b 
   (not (axes-for ?b . ?dontcare1))
   ;; don't return axis for system part if system axis already chosen
   ;; use-system-axes will return axes in this case.
   (not (axes-for ?sys . ?dontcare2)
        (part-of-sys ?b ?sys))
   )
  :effects ( (axes-for ?b ?rot) ))


;;; This operator draws axes so that they are alligned with some
;;; vector.  It chooses a vector whose direction is known numerically.
;;; The rotation of the x-axis is between 0 and 90 degrees, and of
;;; course the y-axis rotation is 90 more than that, and that is
;;; guaranteed to be less than 360.  This only works when the
;;; numerical angle of the vector is known.
;;; 
;;; The operator collects up all the vector's directions, removes
;;; duplicates, then uses any-member to non-deterministically pick one
;;; from the set.  This avoids producing two branches of the solution
;;; graph that have the same axis rotation even though the axis was
;;; alligned with different vectors. 
;;;
;;; The conditions must access the vectors via in-wm, which is satisfied
;;; only when its argument is already in working memory.  If vectors are
;;; accessed without in-wm, then they can be drawn in order to satisfy the
;;; condition.
;;;
;;; Although these operators achieve the goal of defining axes for a given
;;; body, the workbench currently provides no way of 
;;; associating drawn axes with any particular body. For this reason
;;; the action propositions posted for use by the help system leave out
;;; body, and the operators do not have body in their
;;; parameter lists. That means the axis drawing operators can only apply
;;; once to draw an axis at any particular rotation. This should be OK within
;;; the context of a single PSM application. However, if we needed axes for
;;; multiple bodies within a single PSM application we could use variants
;;; that achieve the goal of choosing axes for a body by reusing existing 
;;; drawn axes for another body without drawing again.

(defoperator draw-vector-aligned-axes (?rot)
  :specifications 
   "If the goal is to draw coordinate axes for use on some body's vectors,
       and there are any vectors on that body drawn at known angles
   then draw coordinate axes so that one of them is alligned with ?vector,"
   :preconditions 
   (
    ;; don't rotate axes if components are sought:
    (not (component-form))
    (not (use-energy-axes))	  
    (not (projection-axes ?junk))
    ;; don't draw axis for system part if system axis already chosen
    ;; use-system-axes will choose axes in this case.
    (not (axes-for ?sys . ?dontcare)
	 (part-of-sys ?b ?sys))
    ;; (test (atom ?b))	; only for atomic bodies
    (setof (in-wm (vector ?b ?vector ?dir)) ?dir ?dirs)
    ;; add 0 so standard axes always an option:
    (bind ?min-dirs (adjoin 0 (minimal-x-rotations ?dirs)))
    (any-member ?rot ?min-dirs)
    (bind ?x-rotation (axis-dir 'x ?rot)) ;for help statement
    (debug "Setting axes for ~a at ~A~%" ?b ?rot)
    )
   :effects 
   (
    (draw-axes ?b ?rot)	   ;action proposition for helpsys gives x dir
    (axes-for ?b ?rot)
    (assume axes-for ?b ?rot)
    )
  :hint
  ((point (string "What would be a useful choice for the coordinate axes?"))
   (teach (minilesson "mini_choose_axes.htm")
          (kcd "draw-rotate-axes")
	  ;; Careful with wording: we can't be sure that the axis direction 
	  ;; being prompted actually conforms to the recommended heuristic. 
	  (string "Although you can choose any rotation for the axes and still get a correct answer, the solution is usually simpler if you rotate the axes so at least one vector is parallel to an axis.  Typically, the more vectors that come out parallel to the axes, the better, although which choice is most efficient will depend on the particular problem. "))
   (bottom-out (string "~:[Draw~;Rotate~] the coordinate axes setting the x axis at ~a degrees." (rotate-existing-axes) ?x-rotation))
  ))

(defun rotate-existing-axes ()
"TRUE at help time if should rotate axes rather than drawing new ones"
 (and (axes-drawnp)                   ; have already drawn at least one
      (not nsh-multi-axis-problemp))) ; only one is required for solution 

(defoperator use-system-axes (?b ?rot)
   :specifications 
   "If the goal is to choose an axis for a body b and we have already drawn an
   axis for a many-body system containing b, then use the system's axis
   as the axis for b."
  :preconditions (
    (in-wm (axes-for (system . ?bodies) ?rot))
    (test (part-of-sys ?b `(system ,@?bodies)))
  )
  :effects (
    (axes-for ?b ?rot)
    (assume axes-for ?b ?rot)
  ))

(defoperator use-body-axes-for-point (?point ?rot)
  :specifications 
  "Axes for points on a body inherit the axes used for the whole body."
  :preconditions (
		  (point-on-body ?point ?body)
		  (axes-for ?body ?rot)
		  )
  :effects (
    (axes-for ?point ?rot) 
    (assume axes-for ?point ?rot)
  ))

; this draws the axes requested by the projection psm.
(defoperator draw-projection-axes (?rot)
  :preconditions
  ( (in-wm (projection-axis ?rot)) ) 
   :effects (
    (draw-axes ?b ?rot)	   ;action proposition for helpsys gives x dir
    (axes-for ?b ?rot)
    (assume axes-for ?b ?rot)
   )
   :hint (
       ; !!! TODO
   ))

(defoperator draw-energy-axes ()
  :preconditions ( 
		  ;; only use this if have chosen energy method
		  (in-wm (use-energy-axes))
		  ;; component-form requires standard axes
		  (not (component-form))
		  )
  :effects (
	    (draw-axes ?b 0)  ;action proposition for help system gives x dir
	    (axes-for ?b 0)
	    (assume axes-for ?b 0)
	    (energy-axes ?b)
	    )
  :hint (
	 (point (string "To find an appropriate direction to set the coordinate axes, think about the sorts of quantities that will be needed for an energy solution."))
	 (teach (string "Gravitational potential energy depends on the height above the stipulated zero level. Because change in height is the vertical component of the displacement, and because energy solutions otherwise involve only scalar quantities, there is no advantage to using rotated coordinate axes. So energy solutions in Andes use only standard horizontal-vertical axes."))
	 (bottom-out (string "Draw standard horizontal-vertical coordinate axes by setting the x axis at 0 degrees." ))
	 ))


;;; ===================== projections ====================

;;; Following is the projection PSM applied at the bubble-graph level.
;;; For example, if given mag and dir of v0, need projection to link to 
;;; v0_x and v0_y which occur in component-form bubble-graph equations.
;;;

(defoperator projection-contains (?sought)
  :preconditions (
   (any-member ?sought ((mag ?vector) (dir ?vector)))
   ;; Projection does not use inheritance, since it involves only one quantity.
   (inherit-or-quantity ?vector ?vector) ;test ?vector has no parents
   (vector ?b ?vector ?whatever)    ;Find axis body associated with the vector
   ;; When sought is vector mag or dir, it's tricky to choose all reasonable
   ;; axes.  So, just find axes based on vectors that are already drawn.
   (axes-for ?b ?rot)
   (get-axis ?xy ?rot)
   )
  :effects (
   (eqn-contains (projection (compo ?xy ?rot ?vector)) ?sought)
  ))


(defoperator projection-contains-compo (?rot ?vector)
  ;; set flag to draw use these axes, even if not vector-aligned or standard.
  :preconditions 
  (   
   ;; Projection does not use inheritance, since it involves only one quantity.
   (inherit-or-quantity ?vector ?vector) ;test ?vector has no parents
   (add-to-wm (projection-axis ?rot))
   )
  :effects ( (eqn-contains (projection (compo ?xy ?rot ?vector)) 
			   (compo ?xy ?rot ?vector)) ))

;; Projection writing rules used within larger psms should not draw a body, 
;; since it is the psm that decides whether and which body should be drawn. 
;; However, on simple projection-only problems in our introductory vector set, 
;; we do want the body to be drawn.  We use a projection-body stmt in problem 
;; to turn this on. 
(defoperator draw-body-for-projection (?rot ?vector)
   :preconditions 
   (
    (in-wm (projection-body ?problem-body ?problem-time)) 
    (body ?problem-body)
    )
   :effects ((body-for-projection ?rot ?vector)))

(defoperator omit-body-for-projection (?rot ?vector)
   :preconditions ( (not (projection-body ?problem-body ?problem-time)) )
   :effects ((body-for-projection ?rot ?vector)))

;;; This operator represents writing a projection equation for a zero
;;; vector.  Obviously, the component along any axis of a zero length
;;; vector is zero.  The operator expects ?compo-var to be bound by
;;; unifying a goal with the effects.

(defoperator compo-zero-vector (?xyz ?rot ?vector)
  :specifications 
  "If the goal is to write a projection equation for a given component 
   variable ?v, and the vector is zero, then write ?v = 0."
  :preconditions
   ((body-for-projection ?rot ?vector)
    ;; This causes the vector to be drawn
    (variable ?compo-var (compo ?xyz ?rot ?vector))
    (in-wm (vector ?b ?vector zero))
    (bind ?t (time-of ?vector))  
    )
  :effects
   ((eqn (= ?compo-var 0)
	 (projection (compo ?xyz ?rot ?vector))))
  :hint
  ((point (string "Notice that the ~a has zero length." ?vector ))
   (teach (string "When a vector has zero length, then all its components are zero, too.")
	  (kcd "write_v_x=zero"))
   (bottom-out (string "Because ~a is a zero-length vector, write an equation stating that its component along the ~a axis is zero: ~A" ?vector 
   ((axis ?xyz ?rot) symbols-label) ((= ?compo-var 0) algebra)))
   ))


;;; special case for parallel/anti-parallel axis in xy-plane
;;; also enables us to put out implicit equations for orthogonal component, 
;;; so that it gets into the Andes solution, even if otherwise unused.
(defoperator compo-parallel-axis (?compo-var)
  :specifications "
   If ?compo-var is the variable for a component of a vector,
      and the vector is at a known angle parallel or antiparallel to the axis,
   then write ?compo-var = +/- ?mag-var as appropriate"
  :preconditions (
   (body-for-projection ?rot ?vector)
   (variable ?compo-var (compo ?xyz ?rot ?vector))
   (test (member ?xyz '(x y)))  ;restrict to xy-plane
   (in-wm (vector ?b ?vector ?dir))
   (in-wm (variable ?mag-var (mag ?vector)))
   ;; should fail if ?dir is 'unknown (this test does not work for z-axis)
   (test (parallel-or-antiparallelp (axis-dir ?xyz ?rot) ?dir))
   (bind ?sign (if (same-angle (axis-dir ?xyz ?rot) ?dir) '+ '-))
   )
  :effects
  ((eqn (= ?compo-var (?sign ?mag-var)) (projection (compo ?xyz ?rot ?vector)))
   )
  :hint
  ((point (string "Since ~A lies along the ~A axis, it has a non-zero component along that axis."  ?vector ((axis ?xyz ?rot) symbols-label)))
   (teach (string "You can use general {\\l projection equations}{\\v ProjectionEquations.html} that will work in all cases.  but when a vector V lies along an axis x, the component of the vector V_x along the axis is simply equal to plus or minus the magnitude of the vector: V_x = +/- V. The sign is positive if the vector points in the positive axis direction, and negative otherwise. "))
   (bottom-out (string "Since ~A lies along the ~a axis and points in the ~a ~a direction, write the equation ~A." 
		       ?vector  (?xyz adj) (?sign adj) (?xyz adj)
		       ((= ?compo-var (?sign ?mag-var)) algebra)))
   ))


;;; This operator represents writing a projection equation for a
;;; non-zero vector that is not perpendicular to the axis.  It
;;; expects ?compo-var to be bound by unifying a goal with the
;;; effects, and it produces an equation.  It looks up the definition
;;; of the component variable, then looks up the vector in order to
;;; get its direction.

;;; This uses a simple treatement of geometry, wherein the directions of
;;; vectors and axes are specified by reference to the horizonal, rightward 
;;; screen axis system.  Thus, the projection onto an X axis is always the 
;;; cosine of the difference in their directions. Because it is conventionally
;;; written this way, we write projections along the y-axis using the sin
;;; of the angle made with the X axis.

(defoperator compo-general-case (?xyz ?rot ?vector)
  :specifications "
   If ?compo-var is the variable for a component of a vector,
      and the vector is at a known angle not perpendicular to the axis,
   then the projection equation is ?compo-var = ?mag*cos(?dir - ?rot)
      where ?mag is the magnitude of the vector,
      ?dir is the direction in degrees of the vector,
      and ?rot is the rotation of the axes."
  :preconditions (
   (body-for-projection ?rot ?vector)
   (variable ?compo-var (compo ?xyz ?rot ?vector))
   ; use different special case op for z axis projections:
   (test (not (equal ?xyz 'z)))
   (in-wm (vector ?b ?vector ?dir))
   ; don't use this if lies along axis
   (test (not (parallel-or-antiparallelp (axis-dir ?xyz ?rot) ?dir))) 
   ; don't use this if known zero projection:
   (test (non-zero-projectionp ?dir ?xyz ?rot))
   ;; test is passed if dir is not known to be orthogonal to axis dir
   ;; following makes sure has a known dir so we can plug in the numerical
   ;; degree value (this is important for generating the right equation).
   ;; Another operator will handle unknowns in terms of angle variables
   (test (not (parameter-or-unknownp ?dir)))
   ;; Note ?dir may be a z-axis spec or a (dnum n deg)
   (bind ?degrees (second (if (z-dir-spec ?dir) (zdir-phi ?dir) 
			    ?dir)))
   (in-wm (variable ?mag-var (mag ?vector)))
   ;; write y-axis projection as mag * sin (dir - x-axis rotation)
   (bind ?cos-or-sin (if (equal ?xyz 'y) 'sin 'cos))
   (bind ?x-rot (axis-dir 'x ?rot))
   )
  :effects
  ((eqn (= ?compo-var (* ?mag-var (?cos-or-sin (- (dnum ?degrees |deg|) (dnum ?x-rot |deg|)))))
	(projection (compo ?xyz ?rot ?vector)))
   (assume using-compo (compo ?xyz ?rot ?vector)) ;projection xor pyth-theorem
   )
  :hint
  ((point (string "Since ~A is not perpendicular to the ~A axis, it has a non-zero component along that axis."  ?vector ((axis ?xyz ?rot) symbols-label)))
   (teach (string "In general, if a vector V is oriented at $qV and the positive x axis is oriented at $qx ccw from horizontal, the components of the vector along the axes are given by the {\\l projection equations}{\\v ProjectionEquations.html}\\n   V_x = V * cos($qV - $qx)\\n   V_y = V * sin($qv - $qx)" )
	  (kcd "write_x_trig_projection_equation"))
   (bottom-out (string "Since the direction of ~A is $q~A (~A deg) and the orientation of the x axis is $q~A (~A deg), you can write the general formula ~A = ~A * ~A($q~A - $q~A)."
	       ?vector (?mag-var algebra) (?degrees adj)
	       ;; symbols-label gets x axis label -- could be x, x1, x2
	       ((axis x ?rot) symbols-label) (?x-rot adjective)
	       (?compo-var algebra) (?mag-var algebra) (?cos-or-sin adjective) 
	       (?mag-var algebra) ((axis x ?rot) symbols-label) ))
   ))

(defoperator compo-general-case-unknown (?xyz ?rot ?vector)
  :specifications "
   If ?compo-var is the variable for a component of a vector,
      and the vector is drawn at an unknown direction,
   then the projection equation is ?compo-var = ?mag*cos((?dir - ?rot))
      where ?mag is the magnitude of the vector,
      ?dir is the variable for the direction of the vector,
      and ?rot is the rotation of the axes."
  :preconditions 
  (
   ;; use different special case op for z axis projections:
   (test (not (equal ?xyz 'z)))
   (body-for-projection ?rot ?vector)
   (variable ?compo-var (compo ?xyz ?rot ?vector))
   (in-wm (vector ?b ?vector ?dir))
   ;; z-direction vectors handled separately
   (test (not (z-dir-spec ?dir)))
   (test (parameter-or-unknownp ?dir))
   (in-wm (variable ?dir-var (dir ?vector)))
   (in-wm (variable ?mag-var (mag ?vector)))
   ; write y-axis projection as mag * sin (dir - x-axis rotation)
   (bind ?cos-or-sin (if (equal ?xyz 'y) 'sin 'cos))
   (bind ?x-rot  (axis-dir 'x ?rot))
   (bind ?dir-term (if (eq ?x-rot 0) ?dir-var 
		       `(- ,?dir-var (dnum ,?x-rot |deg|))))
   )
  :effects
  ((eqn (= ?compo-var (* ?mag-var (?cos-or-sin ?dir-term)))
	(projection (compo ?xyz ?rot ?vector)))
   (assume using-compo (compo ?xyz ?rot ?vector)) ;projection xor pyth-theorem
   )
  :hint
  ((point (string "Since ~a is not known to be perpendicular to the ~A axis, you should use a general formula for its component along that axis."  ?vector ((axis ?xyz ?rot) symbols-label)))
   (teach (string "In general, if a vector V is oriented at $qV and the positive x axis is oriented at $qx ccw from horizontal, the components of the vector along the axes are given by the {\\l projection equations}{\\v ProjectionEquations.html}\\n   V_x = V * cos($qV - $qx)\\n   V_y = V * sin($qv - $qx)" )
	  (kcd "write_x_trig_projection_equation"))
   (bottom-out (string "Since the direction of ~a is ~a, and the rotation of the x axis is $q~A (~a deg), you can write the general formula ~a = ~a * ~a(~a - $q~A)." 
		       ?vector (?dir-var algebra) 
		       ((axis ?x ?rot) symbols-label) (?x-rot adj)
		       (?compo-var algebra)
		       (?mag-var algebra)  (?cos-or-sin adj) 
		       (?dir-var algebra) ((axis x ?rot) symbols-label)))
   ))


;;; This operator represents writing a component equation for a vector whose
;;; angle is known numerically and it is perpendicular to the axis.  Thus,
;;; its component along the axis is zero.  We need this special case because
;;; it removes the vector's magnitude from the system of equations.

(defoperator compo-perpendicular (?xyz ?rot ?vector)
  :specifications "
   If a vector is perpendicular to an axis,
   then its component along that axis is zero."
  :preconditions (
    (body-for-projection ?rot ?vector)
    (variable ?compo-var (compo ?xyz ?rot ?vector))
    (in-wm (vector ?b ?vector ?dir))
    (in-wm (variable ?dir-var (dir ?vector)))
    ;; known to be orthogonal
    (test (not (non-zero-projectionp ?dir ?xyz ?rot)))
    )
  :effects
   ((eqn (= ?compo-var 0)
	 (projection (compo ?xyz ?rot ?vector))))
  :hint
  ((point (string  "Notice that ~a is perpendicular to the ~a axis."  ?vector ((axis ?xyz ?rot) symbols-label)))
   (teach (kcd "write_v_x=v_zero")
	  (string "When a vector is perpendicular to an axis, its component along that axis is zero."))
   (bottom-out (string "Because ~a is perpendicular to the ~a axis, write the equation ~a=0"
		       ?vector ((axis ?xyz ?rot) symbols-label) (?compo-var algebra)))
   ))

;; projection equations for z-axis vectors, which are guaranteed to
;; be parallel to the z axis. We also use phi angles for these rather 
;; than theta vars for these. When the phi var is known we put out the
;; value when vector is drawn, so there is really no need to use them 
;; in projections for known directions. We could use them with trig 
;; functions in case of unknown phi dirs, but currently the algebra system 
;; cannot solve for unknown phi variables (see apply-vector-PSM-compo-form). 
;; So in this case we just write equivalent of V = abs(V_z) so mag value
;; will be determined. 

(defoperator compo-z-axis (?vector)
  :preconditions (
   (body-for-projection ?rot ?vector)
   (variable ?compo-var (compo z ?rot ?vector))
   (in-wm (vector ?b ?vector ?dir))
   (test (parallel-or-antiparallelp (axis-dir 'z ?rot) ?dir))
   ;; will still pass above test if ?dir is 'z-unknown, thus:
   (test (known-z-dir-spec ?dir))
   (in-wm (variable ?mag-var (mag ?vector)))
   ;; rhs is plus or minus mag:
   (bind ?rhs (if (eq ?dir 'out-of) ?mag-var `(- ,?mag-var)))
   )
  :effects
  (
   (eqn (= ?compo-var ?rhs) (projection (compo z ?rot ?vector)))
   )
  :hint
  ((point (string "You should write an equation relating the ~A component of ~A to its magnitude."  
                  ((axis z ?rot) symbols-label) ?vector ))
   (teach (string "If a vector V lies entirely along an axis, its component along that axis will be + or - its magnitude, depending on whether the vector points in the positive or negative direction.  In a right-handed coordinate system, the positive z axis points out of the x-y plane of the diagram, and the negative z axis points into the plane.  Thus V_z = V if V points out of the plane and V_z = -V if into the plane." ))
  (bottom-out (string "Since ~A points ~A, write the equation ~A" 
                      ?vector (?dir adj) 
		      ((= ?compo-var ?rhs) algebra)))
  ))


;; determine direction from either given direction or given compos
;; generally should be applied after drawing the vector.

(defoperator use-compos-for-dir (?quant-in)
   :preconditions 
   (
    (inherit-or-quantity ?quant-in ?quant)
    (given (compo x ?rot ?quant) (dnum ?xc ?units))
    (given (compo y ?rot ?quant) (dnum ?yc ?units))
    (not (given (dir ?quant) ?dir-given))
    (bind ?dir (dir-from-compos ?xc ?yc))
    )
   ;; Note that the answer is rounded, so the result cannot
   ;; be used as part of a numerical solution.
   :effects ((dir-given-or-compos ?quant-in ?dir)))

(defoperator use-given-for-dir (?quant-in)
  :preconditions 
  ( 
   (inherit-or-quantity ?quant-in ?quant)
   ;; in-wm so that it is parallel to above
   (in-wm (given (dir ?quant) ?dir)) 
   )
   :effects ((dir-given-or-compos ?quant-in ?dir)))
   

;;; =================== defining component variables =============
;;; When writing a compo equation, the code will call (variable <var>
;;; <quantity>) with <quantity> bound in order to fetch a variable
;;; that denotes that quantity.  This operator defines such variables.
;;;
;;; On the Andes interface, component variables are automatically
;;; defined whenever both an axis and a vector are drawn. Thus we
;;; ensure axes and vectors are drawn in the preconditions here.
;;;
;;; Because the component variables are defined automatically by the
;;; workbench, no hints are necessary on this operator.
;;;
;;; Normally, the PSM that needs component variables will ensure that
;;; they have already been drawn before the component variable is needed,
;;; so these subgoals will usually be satisfied in working memory.
;;; But in component-form problems, some operator may have to be used 
;;; to enter the given components, and this will have to draw the vector 
;;; and axes to define the variables. To use this operator we would have
;;; to take out "in-wm" on these preconds. One issue is that it may
;;; is necessary to pick a time to associate with the axis if we have
;;; to draw it. [The time an axis is chosen "for" is not necessarily the
;;; time on the vector but more like the time on the main PSM -- e.g
;;; in [LK block (1 2) time on the axis will be (1 2) while we still
;;; draw instantaneous vector at 1.] For this reason we define another op
;;; to achieve axes for writing given compos for component form problems.
;;; !!! we should just have a common set of operators that draw axes as needed
;;; in all cases rather than making assumptions about when they will be 
;;; called.

(defoperator define-compo (?var)
  :specifications "
   If there is an axis defined
      and there is a vector defined
   then define a component variable for the vector along the axis"
  :preconditions
  (
;; causes kr4b to fail
  ;; (inherit-or-quantity ?vector ?vector) ;test ?vector has no parents
   (wm-or-derive (vector ?b ?vector ?dir))
   (wm-or-derive (axes-for ?b ?rot))  ;sanity check
   (wm-or-derive (get-axis ?xyz ?rot)) ;sanity check
   (not (variable ?dont-care (compo ?xyz ?rot ?vector)))
   ;; fetch vector's mag var for building compo var name only. 
   (in-wm (variable ?v-var (mag ?vector)))  
   (bind ?var (format-sym "~Ac_~A_~A" ?xyz ?v-var ?rot))
   (debug "define-compo:  defining var for (compo ~a ~a ~a).~%" 
	  ?xyz ?rot ?vector)
   )
  :effects
  ((variable ?var (compo ?xyz ?rot ?vector))))

;;; Following is a somewhat hairy attempt to allow zero-valued velocity 
;;; components to be mentioned in student equations, even if they are not 
;;; formally needed for a conservation of momentum solution. 
;;; The solution may need p or p_x, but draws p as zero-magnitude immediately 
;;; from fact that body is at rest, and projection rules put out p_x = 0. 
;;; Momentum definition p_x = m*v_x never comes out because driver rules don't 
;;; apply a vector equation for a vector attribute if vector does not have a 
;;; non-zero projection along axis. (That is desirable because our 
;;; bubble-collection method depends on counting variables, and we don't want 
;;; an equation containing m that can't be used to solve for m.)
;;;
;;; One way to include equations as optional in the solution is by generating 
;;; them as "implicit equations."  This was not the intent behind the implicit 
;;; equation designation, but can be used to achieve that effect.
;;;
;;; Another way is to include them on only one of several paths achieving a 
;;; PSM equation.  We try that here, since we are already splitting a path on 
;;; the optional v drawing in draw-momentum-at-rest.  This operator should only
;;; write entries along that path, though the include-zero-vcomps subgoal 
;;; must still succeed along the other path for the other path to be included 
;;; in the solution. This happens because on the other path, the setof 
;;; succeeds but with an empty set of component variables, and the subsequent 
;;; foreach's then expand to an empty set of new subgoals, so this turns out 
;;; to be a no-op.
(defoperator include-zero-vcomps (?xyz ?rot)
  :effects ((include-zero-vcomps ?xyz ?rot))
  :preconditions 
  (
   ;; Collect component terms from all the drawn zero velocity vectors. 
   ;; Zero velocities are drawn as an optional step in draw-momentum-at-rest.
   ;; Since that is optional, this will only find them when in the path 
   ;; through the containing PSM that actually draws them.
      (setof (in-wm (vector ?b (velocity . ?rest) zero))
              (compo ?xyz ?rot (velocity . ?rest))
	      ?zero-vcomps)
      ;; for each one, declare its component variable. This is needed because 
      ;; the  projection writing operators fetch the variables from wm.
      (foreach ?vcomp ?zero-vcomps
          (variable ?var ?vcomp))
      ;; for each one, emit its projection equation.
      (foreach ?vcomp ?zero-vcomps
           (eqn (= ?compo-var 0) (projection ?vcomp)))
  ))

;;; Pythagorean theorem for vector magnitudes: 
;;;        r^2 = r_x^2 + r_y^2
;;; This applies to any vector, but for now we define it for relative 
;;; position vectors only, so that we can limit its effects on other problems 
;;; until we decide to add it for all vectors.
;;; For now we also restrict to standard axes since that's where we need it, 
;;; although any orthogonal pair could do.
;;;
;;; !!! This seems only to work on component-form, due to problems getting 
;;; the axes drawn when fetching component variables.  This is the detail: 
;;; define-compo, intended for standard vector psms, fails because it uses 
;;; "in-wm", but no axis from vector psm diagram is already registered in 
;;; wm for vectors on body ?b (which may just be a point in space in this 
;;; case).
;;;
;;; The more liberal define-compo2 draws vectors and standard axes as needed. 
;;; That will work, but is defined only to apply when component-form is set.
;;; In general the association of all vectors with bodies for purposes of 
;;; finding axes to use on them, which works well for things like 
;;; the Atwoods machine problems, is not so appropriate for field problems 
;;; with position vectors and an origin. 
;;;
;;; The whole treatment of axes needs to be cleaned up and simplified to 
;;; avoid this hairiness.
;;;   
(def-psmclass vector-magnitude (vector-magnitude ?vector ?rot)
  :complexity minor 
  :short-name "vector magnitude"
  :english ("the magnitude of a vector")
  :ExpFormat ("calculating the magnitude of of ~a" (nlg ?vector))
  :EqnFormat ("A = sqrt(A_x^2 + A_y^2)"))

;; can't use this to get components, because of square.
(defoperator vector-magnitude-contains ((mag ?vector))
  ;; ensure axis drawn, to emulate a vector method diagram drawing step. 
  ;; Reason is hairy: this enables the define-compo to apply as it does for 
  ;; normal vector methods, since define-compo requires vectors and axes to
  ;; be in wm.  Since the change to alternative define-compo2, the latter no 
  ;; longer works if EITHER vector or axis is drawn. 
  :preconditions 
  (
   ;; vector-magnitude not use inheritance, since it involves only one quantity.
   (inherit-or-quantity ?vector ?vector) ;test ?vector has no parents
   (vector ?b ?vector ?dir) ;draw the vector
   (axes-for ?b ?rot)
   ;; make sure there can be more than one nonzero term in sum
   ;; else it is redundant with the projection equations
   ;; This test is not sufficient for problem coul3, Bug #1375.
   (setof (get-axis ?xyz ?rot) ?xyz ?xyz-list)
   (test (> (length (remove-if-not 
		   #'(lambda (xyz) (non-zero-projectionp ?dir xyz ?rot)) 
		   ?xyz-list)) 1))
   )
  :effects ( (eqn-contains (vector-magnitude ?vector ?rot) (mag ?vector)) ))

(defoperator write-vector-magnitude (?vector ?rot)
  :preconditions 
  (
   (variable ?r (mag ?vector))
   (dot ?dot ?vector ?vector ?rot :nonzero ?nonzero)
  )
  :effects (
    (eqn (= ?r (sqrt ?dot)) (vector-magnitude ?vector ?rot))
    ;; pyth-theorem xor projections
    (assume using-magnitude ?vector)
  )
  :hint (
   (point (string "The pythagorean theorem can be used to relate the magnitude of a vector to its components."))
   (bottom-out (string "Write the equation ~A." 
		       ((= ?r (sqrt ?nonzero)) algebra)))
  ))


;; Rather than make separate vector PSM's, we inherit (equals ...) for
;; components and magnitudes from (equals ...) of vectors.
(defoperator vector-component-equality-from-vector-equality (?v1 ?v2)
  :preconditions 
  (
   (equal-vectors ?v1 ?v2 :opposite ?flag :hint ?vector-hint)
   ;; Draw vectors
   (vector ?b1 ?v1 ?dir1)
   (vector ?b2 ?v2 ?dir2)
   (axes-for ?b1 ?rot) ;make sure both vectors have same coordinates
   (axes-for ?b2 ?rot)
   (get-axis ?xyz ?rot) 
   (bind ?compo-hint 
	 (format nil "~@[~A  ~]Since the two vectors are ~:[the same~;opposite and equal~], their components are ~:[the same~;opposite and equal~]." 
			     ?vector-hint ?flag ?flag))
   )
  :effects 
  ((equals (compo ?xyz ?rot ?v1) (compo ?xyz ?rot ?v2) 
	   :opposite ?flag :hint ?compo-hint)
   ;; The family-id doesn't match the regular equation id.
   ;; Since this is purely a side-effect, ?v1 and ?v2 are always bound
   (assume using-compo (nil ?xyz ?rot 
			    (equals orderless ?v1 ?v2))) ;mag xor compos
))

(defoperator vector-mag-equality-from-vector-equality (?v1 ?v2)
  :preconditions 
  (
   (equal-vectors ?v1 ?v2 :opposite ?flag :hint ?vector-hint)
   (bind ?mag-hint 
	 (format nil "~@[~A  ~]Since the two vectors are ~:[the same~;opposite and equal~], their magnitudes are equal." 
			     ?vector-hint ?flag))
   )
  :effects 
  (
   (equals (mag ?v1) (mag ?v2) :hint ?mag-hint)
   ;; Since this is purely a side-effect, ?v1 and ?v2 are always bound
   (assume using-magnitude (equals orderless ?v1 ?v2)) ;mag xor compos
   ))

#|
(defoperator draw-vector-given-compos (?vec)
  :preconditions 
  ( 
   (object ?b)
   (given (compo x 0 ?vec (dnum ?vx ?units)))
   (given (compo y 0 ?vec (dnum ?vy ?units)))
   (test (or (= ?vx 0) (= ?vy 0)))
   (bind ?dir-expr (dir-from-compos ?vx ?vy))
   (bind ?mag-var (format-sym "v_~A" (my-hast ?vec))) 
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?x-var (format-sym "Xc_~A" (my-hast ?vec))) 
   (bind ?y-var (format-sym "Yc_~A" (my-hast ?vec)))  
   (bind ?mag-expr `(dnum ,(sqrt (+ (* vx vx) (*vy vy)))))
  )
  :order ((relative-position . 1))
  :effects (
    (vector ?b ?vec ?dir-expr)
    (variable ?mag-var (mag ?vec))
    (variable ?dir-var (dir ?vec))
    (variable ?x-var (compo x 0 ?vec))
    (variable ?y-var (compo y 0 ?vec))
    ;; Because dir is problem given, find-by-PSM won't ensure implicit eqn
    ;; gets written. Given value may not be used elsewhere so ensure it here.
    (implicit-eqn (= ?dir-var ?dir-expr) (dir ?vec))
    (implicit-eqn (= ?mag-var ?mag-expr) (mag ?vec))
   )
  :hint (
    (point (string "You know the position of ~a relative to ~a." ?b1 ?b2))
    (bottom-out (string "Use the relative position drawing tool (labeled R) to draw the position of ~a with respect to ~a ~a at ~a."
	  ?b1 ?b2 (?t pp) ?dir-expr))
  ))
|#


;;;;===========================================================================
;;;;
;;;; construct component of a unit vector associated with some normal vector
;;;;
;;;;===========================================================================

;; This is a bit of a hack until we get real vectors working in Andes

;; This gives wrong results if ?vec does not lie entirely in the xy plane.
(defoperator hat-using-angle (?vec ?xy ?rot)
  :preconditions 
  ((variable ?vec-theta (dir ?vec))
   (bind ?axis-angle (axis-dir 'x ?rot))
   (test (numberp ?axis-angle))
   (bind ?trig (if (eq ?xy 'y) 'sin 'cos))
   (bind ?term (if (= ?axis-angle 0) 
		   ?vec-theta `(- ,?vec-theta (dnum ,?axis-angle |deg|)))))
  :effects ((hat (?trig ?term) ?vec ?xy ?rot nil)
	    (assume using-hat ?vec ?rot nil)
	    ))

(defoperator hat-using-compos (?vec ?xyz ?rot)
  :preconditions
  ((variable ?vec-compo (compo ?xyz ?rot ?vec))
   (variable ?vec-mag (mag ?vec)))
  :effects ((hat (/ ?vec-compo ?vec-mag) ?vec ?xyz ?rot t)
	    (assume using-hat ?vec ?rot t)
	    ))

;;;;===========================================================================
;;;;
;;;;   dot product of two vectors
;;;;
;;;;===========================================================================

;; dot product for two vectors
(defoperator dot-using-angle (?a ?b)
  :preconditions 
  ( (inherit-or-quantity ?a-in ?a)
    (inherit-or-quantity ?b-in ?b)
    (wm-or-derive (vector ?a-body ?a ?dir-a))
    (wm-or-derive (vector ?b-body ?b ?dir-b))
    (test (not (perpendicularp ?dir-a ?dir-b))) ;see dot-orthogonal
    (in-wm (variable ?a-var (mag ?a)))
    (in-wm (variable ?b-var (mag ?b)))
    (variable ?theta-var (angle-between orderless ?a ?b))
    (bind ?dot (if (exactly-equal ?a ?b)
		   `(^ ,?a-var 2) ;dot of a vector with itself
		 `(* ,?a-var ,?b-var (cos ,?theta-var))))
    )
  :effects ( (dot ?dot ?a-in ?b-in nil :nonzero ?dot) 
	     ;; nogood rule to so that only one form of dot is chosen
	     (assume using-dot ?a ?b nil) ))

(defoperator dot-term (?a ?b ?xyz ?rot)
  :preconditions 
  ( 
   (get-axis ?xyz ?rot)
   (variable ?a-xyz (compo ?xyz ?rot ?a))
   (variable ?b-xyz (compo ?xyz ?rot ?b))
   (bind ?dot (if (exactly-equal ?a ?b) `(^ ,?a-xyz 2) `(* ,?a-xyz ,?b-xyz)))
    )
  :effects ( (dot-term ?dot ?a ?b ?rot) ))

(defoperator dot-term-nonzero (?a ?b ?xyz ?rot)
  :preconditions 
  ( 
   (in-wm (vector ?a-body ?a ?dir-a)) ;now done in the eqn-contains
   (in-wm (vector ?b-body ?b ?dir-b)) ; ditto
   (test (not (perpendicularp ?dir-a ?dir-b))) ;see dot-orthogonal
   (get-axis ?xyz ?rot)
   ;; drop any zero term
   (test (non-zero-projectionp ?dir-a ?xyz ?rot))
   (test (non-zero-projectionp ?dir-b ?xyz ?rot))
   (variable ?a-xyz (compo ?xyz ?rot ?a))
   (variable ?b-xyz (compo ?xyz ?rot ?b))
   (bind ?dot (if (exactly-equal ?a ?b) `(^ ,?a-xyz 2) `(* ,?a-xyz ,?b-xyz)))
    )
  :effects ( (dot-term-nonzero ?dot ?a ?b ?rot) ))

(defoperator dot-using-components (?a ?b ?rot)
  :preconditions 
  ( 
   (inherit-or-quantity ?a-in ?a)
   (inherit-or-quantity ?b-in ?b)
   (test (not (null ?rot))) ; use component form
   ;; gather all terms of dot product
   (setof (dot-term ?dot ?a ?b ?rot) ?dot ?terms)
   ;; gather all terms not known to be zero.  This is used
   ;; to test for nonzero dot and for hints.
   (setof (dot-term-nonzero ?dot ?a ?b ?rot) ?dot ?nonzero-terms)
   (test (not (null ?nonzero-terms))) ;dot=0 handled by dot-orthogonal
   ;; write out as a sum when appropriate
   (bind ?dot (format-plus ?terms))
   ;; write out as a sum when appropriate
   (bind ?nonzero-dot (format-plus ?nonzero-terms))
   )
  :effects ( (dot ?dot ?a-in ?b-in ?rot :nonzero ?nonzero-dot)
	     ;; nogood rule to so that only one form of dot is chosen
	     (assume using-dot ?a ?b ?rot) ))

(defoperator dot-orthogonal (?a ?b ?angle-flag)
  :preconditions 
  (
   (inherit-or-quantity ?a-in ?a)
   (inherit-or-quantity ?b-in ?b)
   ;; Only use one form since equations are identical.
   ;; Angle form is preferred because axes are sometimes
   ;; not demanded in this case.
   (test (not ?angle-flag))
   (wm-or-derive (vector ?a-body ?a ?dir-a))
   (wm-or-derive (vector ?b-body ?b ?dir-b))
   (test (perpendicularp ?dir-a ?dir-b))
   )
  :effects ( (dot 0 ?a-in ?b-in ?angle-flag :nonzero 0) 
	     ;; nogood rule to so that only one form of dot is chosen
	     (assume using-dot ?a ?b ?angle-flag) ))


;;;;===========================================================================
;;;;
;;;;   cross product of two vectors
;;;;
;;;;===========================================================================


;; angle formulation:  only works when direction of cross product
;; is along an axis

(defoperator cross-using-angle (?a ?b ?compo ?rot)
  :preconditions 
  ( 
   (inherit-or-quantity ?a-in ?a)
   (inherit-or-quantity ?b-in ?b)
   (in-wm (vector ?a-body ?a ?dir-a))
   (in-wm (vector ?b-body ?b ?dir-b))
   (bind ?dir-cross (cross-product-dir ?dir-a ?dir-b))
   (bind ?dir-axis (axis-dir ?compo ?rot))
   ;; make sure that the direction is along the axis
   (test (parallel-or-antiparallelp ?dir-cross ?dir-axis))
   ;; cross-zero handles these cases:
   (test (not (parallel-or-antiparallelp ?dir-a ?dir-b)))
   (test (non-zero-projectionp ?dir-cross ?compo ?rot))
   ;;
   (in-wm (variable ?a-mag (mag ?a)))
   (in-wm (variable ?b-mag (mag ?b)))
   ;; Use angle-between because using the difference of
   ;; directions does not work outside the xy-plane.
    ;; Also matches method of finding the magnitude and then
   ;; using the right hand rule to find the correct direction (and sign).
   (variable ?theta-var (angle-between orderless ?a ?b))
   (any-member ?absin ((* ?a-mag ?b-mag (sin ?theta-var))))
   (bind ?cross (if (same-angle ?dir-cross ?dir-axis) ?absin `(- ,?absin)))
   )
  :effects ( (cross ?cross ?a-in ?b-in ?compo ?rot nil) 
	     ;; nogood rule to so that only one form of cross is chosen
	     (assume using-cross ?a ?b ?compo ?rot nil) ))

(defoperator cross-product-term (?a ?b ?i ?j ?rot)
  :preconditions 
  (
   (in-wm (vector ?a-body ?a ?dir-a)) ;now done in the eqn-contains
   (in-wm (vector ?b-body ?b ?dir-b)) ; ditto
   (variable ?ai (compo ?i ?rot ?a))
   (variable ?bj (compo ?j ?rot ?b))
   )
  :effects ((cross-term (* ?ai ?bj) ?a ?b ?i ?j ?rot)))

;; use vector-PSM
(defoperator cross-using-components (?a ?b ?compo ?rot)
  :preconditions 
  ( 
   (inherit-or-quantity ?a-in ?a)
   (inherit-or-quantity ?b-in ?b)
   (in-wm (vector ?a-body ?a ?dir-a)) ;now done in the eqn-contains
   (in-wm (vector ?b-body ?b ?dir-b)) ; ditto
   (bind ?dir-cross (cross-product-dir ?dir-a ?dir-b))
   ;; cross-zero handles these cases:
   (test (not (parallel-or-antiparallelp ?dir-a ?dir-b)))
   (test (non-zero-projectionp ?dir-cross ?compo ?rot))
   ;;
   (any-member (?compo ?i ?j) ((x y z) (y z x) (z x y)))
   (cross-term ?ai-bj ?a ?b ?i ?j ?rot)
   (cross-term ?aj-bi ?a ?b ?j ?i ?rot) 
   )
  :effects ( (cross (- ?ai-bj ?aj-bi) ?a-in ?b-in ?compo ?rot t)
	     ;; nogood rule to so that only one form of cross is chosen
	     (assume using-cross ?a ?b ?compo ?rot t) ))

(defoperator cross-zero (?a ?b ?compo ?rot ?angle-flag)
  :preconditions 
  (
   (inherit-or-quantity ?a-in ?a)
   (inherit-or-quantity ?b-in ?b)
   ;; Only use one form since equations are identical.
   ;; Angle form is preferred because axes are sometimes
   ;; not demanded in this case.
   (test (not ?angle-flag))
   (in-wm (vector ?a-body ?a ?dir-a))
   (in-wm (vector ?b-body ?b ?dir-b))
   (bind ?dir-cross (cross-product-dir ?dir-a ?dir-b))
   ;; negative of above test
   (test (or (parallel-or-antiparallelp ?dir-a ?dir-b)
	     (not (non-zero-projectionp ?dir-cross ?compo ?rot))))
   )
  :effects ( (cross 0 ?a-in ?b-in ?compo ?rot ?angle-flag) 
	     ;; nogood rule to so that only one form of cross is chosen
	     (assume using-cross ?a ?b ?compo ?rot ?angle-flag) ))

;; post processing to ensure unused given vector components
;; get into the variable index with values. They will be tagged
;; with the mark 'UNUSED-GIVEN in the index. There are no
;; qnodes for them and no equations stating their values in
;; the solution, but the solver will have a value for them.
(post-process add-unused-given-compos (problem)
  "add unused given vector components with values to variable index"
  ;; do for each given vector component
  (dolist (prop (Filter-expressions 
                       '(given (compo ?xyz ?rot ?vector) . ?rest)
		        (problem-givens problem))) 
   (let ((quant (second prop)))
     ;; if it's not already in the index
     (when (not (find quant (problem-varindex problem) 
                      :key #'qvar-exp :test #'unify))
       (let* ((axis (second quant))
	      (rot (third quant))
	      (vector (fourth quant))
	      ;; find vector mag's entry in var index for naming
	      ;; compo var. May not be there if mag unused also
	      (mag-qvar (match-exp->qvar `(mag ,vector)
	                    (problem-varindex problem)))
              (value (third prop)))
       ;; make sure we found a mag var
       (if (null mag-qvar) 
             (warn "Unused given ~A not added because mag unused also."
	              `(compo ,axis ,rot ,vector))
       ;;else:
       ;;  make qvar item for it and append to end of varindex
         (setf (problem-varindex problem)
           (append (problem-varindex problem)
	      (list (make-qvar 
                     ; need to form compo variable name
                     :var (format-sym "~Ac_~A_~A" axis (qvar-var mag-qvar) rot)
		     :exp quant
		     ; requiring value to be dnum, not plain 0:
		     :value (second value)
		     :units (third value)
		     :marks '(unused-given)
		     ; new element's index = length of old index
		     :index (length (problem-varindex problem))
		     :nodes NIL))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                    Angle between vectors or lines
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Following defines a variable for the angle between two vectors
;;; for the case where the angle of the two vectors is known.
;;; The angle between is always defined as the smaller of two possible angles.
;;;      theta12 = min(|theta1-theta2|,180-|theta1-theta2|)
;;; In this case, a side-effect of the definition is to make the 
;;; angle-between known as well so it won't be sought further.
;;;
;;; Vector quantities are identified in this quant by expressions of the form 
;;; 	(vec ?b ... :time ?t)  
;;; which include the times of each of the two vectors. These could in 
;;; principle be different if you were defining the angle between a vector at 
;;; one time and the same vector at another.
;;;
;;; So that angle between v1 and v2 gets the same representation as angle 
;;; between v2 and v1, we require the vector expressions in any angle-between 
;;; expression to be sorted. Any goal to be achieved by this operator will 
;;; know the major type of the vectors needed, so can sort the vector 
;;; expressions in the goal to be achieved.
;;;
;;; On the ANDES interface an angle can also be introduced on the diagram by 
;;; labelling the angle between the drawn vectors using the angle label tool.
;;; This used to be a distinguished sort of entry, with its own entry proposition
;;; but now we treat this as just a different gesture for defining an angle var
;;; (Same as treatment of revolution-radius, which may be drawn or defined)

(defoperator inherit-angle-between (?children)
  :preconditions 
  (
   ;; just look for one child-parent pair, using composite inheritance
   ;; if there is more than one
   (any-member ?children ((?child ?quant) (?quant ?child)))
   (any-member ?parents ((?parent ?quant) (?quant ?parent))) 
   (inherit-quantity ?child ?parent)
   )
  :effects ((inherit-quantity (angle-between orderless . ?children)
			      (angle-between orderless . ?parents))))

(defoperator define-angle-between-vectors (?vecs)
 :preconditions 
 (
  ;; assume that ?vec1 and ?vec2 are parent quantities
  (any-member (?vec1 ?vec2) (?vecs))
  ;; vectors must be drawn first, with known angles
  ;; note vector's axis owner bodies need not be the same
  (vector ?b1 ?vec1 ?dir1)
  (vector ?b2 ?vec2 ?dir2)
  ;; fetch vector mag vars for forming angle variable name only
  (in-wm (variable ?v1-var (mag ?vec1)))
  (in-wm (variable ?v2-var (mag ?vec2)))
  (bind ?theta-var (format-sym "theta_~A_~A" ?v1-var ?v2-var))
  (debug "angle between ~A and ~A = ~A~%" ?v1-var ?v2-var ?angle)
  )
 :effects (
	   (define-var (angle-between orderless . ?vecs))
	   (variable ?theta-var (angle-between orderless . ?vecs))
	   )
 :hint (
	(bottom-out (string "Define a variable for the angle between ~A and ~A by using the Add Variable command on the Variable menu and selecting Angle." 
			    (?v1-var algebra) (?v2-var algebra)))
	))



(defoperator define-angle-between-lines (?lines)
 :preconditions 
 (
  (any-member ?lines (((line ?r1) (line ?r2))))
  ;; lines must be drawn first, with known angles
  (draw-line (line ?r1) ?dir1)
  (draw-line (line ?r2) ?dir2)
  (test (not (equal ?r1 ?r2)))  ;?r1 & ?r2 distinct
  ;; define variable name
  (bind ?theta-var (format-sym "theta_~A_~A" (body-name ?r1) (body-name ?r2)))
 )
 :effects (
   (define-var (angle-between orderless . ?lines))
   (variable ?theta-var (angle-between orderless . ?lines))
 )
 :hint (
  (bottom-out (string "Define a variable for the angle between ~A and ~A by using the Add Variable command on the Variable menu and selecting Angle." 
   ?r1 ?r2))
 ))

(def-psmclass angle-direction (angle-direction orderless . ?things)
  :complexity definition
  :short-name "angle between"
  :english ("angle between")
  :ExpFormat ("finding the angle between ~A" (nlg ?things 'conjoined-defnp))
  :eqnFormat ("$q12 = $q2 - $q1 or 180-($q2-$q1)"))

(defoperator angle-direction-contains-angle (?things)
  :effects 
  ( (eqn-contains (angle-direction orderless . ?things)
		  (angle-between orderless . ?things)) ))

;; direction can't be sought for vectors, due to ambiguity
(defoperator angle-direction-contains-dir (?things)
  :preconditions 
  (
   (any-member ?line1 ((line . ?r))) ;this is for speed
   (draw-line ?line2 ?dir2) ;draw another line to allow angle
   (test (not (unify ?line1 ?line2)))   ;test that lines are distinct
   (bind ?things (sort (list ?line1 ?line2) #'expr<))
   )
  :effects ( (eqn-contains (angle-direction orderless . ?things) 
			   (dir (line . ?r))) ))

(defoperator relate-inequality (?greater ?lesser)
  :preconditions ((less-than ?lesser ?greater))
  :effects ((greater-than ?greater ?lesser)))

;; infer order from any given statements
(defoperator greater-than-given-directions (?greater ?lesser)
  :preconditions 
  ;; wm-or-derive is cruicial for speed
  ( (wm-or-derive (given (dir ?greater) ?dg . ?g-rest))
    (wm-or-derive (given (dir ?lesser) ?dl . ?l-rest))
    ;; only makes sense for vectors/lines in the xy plane
    ;; use estimated values as best guess
    (test (and (degree-specifierp ?dg :error t)
	       (degree-specifierp ?dl :error t)
	       (> (second ?dg) (second ?dl))))
    )
  :effects ((greater-than ?greater ?lesser)))

(defoperator write-angle-direction-line (?lines)
  :preconditions 
  (
   (any-member ?lines ((?greater ?lesser) (?lesser ?greater)))
   (test (eq (car ?greater) 'line)) ;test that these are lines
   ;; does not do inheritance, since inheritance is consistent for quantities
   (inherit-or-quantity ?lesser ?lesser)
   (inherit-or-quantity ?greater ?greater)
   ;; this only makes sense if both lines are in the plane
   (greater-than ?greater ?lesser)
   (variable ?dg (dir ?greater))
   (variable ?dl (dir ?lesser))
   (variable ?angle-var (angle-between orderless . ?lines))
   )
  :effects 
  ( (eqn (= ?angle-var (- ?dg ?dl)) (angle-direction orderless . ?lines))
    (assume using-angle-direction (angle-between orderless . ?lines)) )
  :hint (
	 (point (string "Express the angle between ~A and ~A in terms of directions."
			?greater ?lesser))
	 (teach (string "The angle between two lines is simply the difference of the directions of the two lines.")) 
	 (bottom-out (string "Write the equation ~A." 
			     ((= ?angle-var (- ?dg ?dl)) algebra)))
	 ))

(defoperator write-angle-direction-vector (?vectors)
  :preconditions 
  (
   ;; iterate over orders and do inheritance
   (any-member ?vectors ((?greater ?lesser) (?lesser ?greater)))
   ;; does not do inheritance, since inheritance is consistent for quantities
   (inherit-or-quantity ?lesser ?lesser)
   (inherit-or-quantity ?greater ?greater)
   ;; This will also restrict to x-y plane
   (greater-than ?greater ?lesser)
   ;; need to draw vectors before talking about angle between them
   (vector ?bg ?greater ?dir-g)
   (vector ?bl ?lesser ?dir-l)
   ;; Make sure angle is not known explicitly
   (test (not (get-angle-between ?dir-g ?dir-l)))
   ;; in working memory from drawing vector
   (in-wm (variable ?dg (dir ?greater)))
   (in-wm (variable ?dl (dir ?lesser)))
   (variable ?angle-var (angle-between orderless . ?vectors))
   (bind ?term `(- (dnum 180 |deg|) (abs (- (dnum 180 |deg|) (- ,?dg ,?dl)))))
   )
  :effects 
  ( (eqn (= ?angle-var ?term) (angle-direction orderless . ?vectors)) )
  :hint (
	 (point (string "Express the angle between ~A and ~A in terms of directions."
			?greater ?lesser))
	 (teach (string "The angle between two vectors is the difference between the directions of the two vectors.  If the result is larger than 180 degrees, take 360 degrees minus the difference.")) 
	 (bottom-out (string "An equation that will always work is ~A." 
			     ((= ?angle-var ?term) algebra)))
	 ))

(defoperator write-angle-direction-known (?vectors)
  :preconditions 
  (
   (any-member ?vectors ((?greater ?lesser)))
   ;; does not do inheritance, since it involves only one quantity
   (inherit-or-quantity ?lesser ?lesser)
   (inherit-or-quantity ?greater ?greater)
   ;; need to draw vectors before talking about angle between them
   (vector ?bg ?greater ?dir-g)
   (vector ?bl ?lesser ?dir-l)
   (bind ?angle (get-angle-between ?dir-g ?dir-l))
   (test ?angle)  ;make sure angle can be found.
   (variable ?angle-var (angle-between orderless . ?vectors))
   (bind ?dir-term (if (<= ?angle 180) (dir-to-term ?angle)
		       `(- (dnum 360 |deg|) ,(dir-to-term ?angle))))
   )
  :effects 
  ( (eqn (= ?angle-var ?dir-term) (angle-direction orderless . ?vectors)))
  :hint (
	 (point (string "What is the angle between ~A and ~A?"
			?greater ?lesser))
	 (bottom-out (string "Write the equation ~A." 
			     ((= ?angle-var ?dir-term) algebra)))
	 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                          Unit vector
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-qexp unit-vector (unit-vector ?orientation ?body :at ?loc :time ?time)
  :units nil  ;dimensionless
  :english ("a unit vector~@[ at ~A~] ~A ~A" ?loc 
	    (unit-vector-orientation-name ?orientation)
	    (nlg ?body 'at-time ?time)))

(defun unit-vector-orientation-name (orientation)
  (cond ((eq orientation 'normal-to) "normal to")
	((eq orientation 'towards) "pointing towards")
	((eq orientation 'away-from) "pointing away from")
	(t orientation)))

(defoperator unit-vector-given-relative-position1 (?body ?orientation ?loc ?t)
  :preconditions 
  (
   (given (dir (relative-position ?loc ?body :time ?t)) ?dir)
   (any-member ?orientation (towards away-from))
   (bind ?n-dir (if (eq ?orientation 'towards) (opposite ?dir) ?dir))
   )
  :effects 
  ((given (dir (unit-vector ?orientation ?body :at ?loc :time ?t)) ?n-dir)))

(defoperator unit-vector-given-relative-position2 (?body ?orientation ?loc ?t)
  :preconditions 
  (
   (given (dir (relative-position ?body ?loc :time ?t)) ?dir)
   (any-member ?orientation (towards away-from))
   (bind ?n-dir (if (eq ?orientation 'towards) ?dir (opposite ?dir)))
   )
  :effects 
  ((given (dir (unit-vector ?orientation ?body :at ?loc :time ?t)) ?n-dir)))

(defoperator draw-unit-vector-given-dir (?orientation ?body ?loc ?t)
  :preconditions 
  ((time ?t)
   (given (dir (unit-vector ?orientation ?body :at ?loc :time ?t)) ?dir)
   (not (vector ?whatever (unit-vector ?orientation ?body :at ?loc :time ?t) 
		?dir))
   (bind ?mag-var (format-sym "n~A_~A_~A~@[_~A~]"
			      (subseq (string ?orientation) 0 1) 
			      (body-name ?body) 
			      (body-name ?loc) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?angle-value (if (z-dir-spec ?dir) (zdir-phi ?dir) ?dir))
   )
  :effects 
  (
   (vector ?body (unit-vector ?orientation ?body :at ?loc :time ?t) ?dir)
   (variable ?mag-var (mag (unit-vector ?orientation ?body :at ?loc :time ?t)))
   (variable ?dir-var (dir (unit-vector ?orientation ?body :at ?loc :time ?t)))
   ;; Because dir is problem given, find-by-psm won't ensure implicit 
   ;; eqn gets written.  Given value may not be used elsewhere so 
   ;; ensure it here.
   (implicit-eqn (= ?dir-var ?angle-value) 
		 (dir (unit-vector ?orientation ?body :at ?loc :time ?t)))
   )  
  :hint 
  ( (point (string "You can draw ~A" 
		   ((unit-vector ?orientation ?body :at ?loc :time ?t) def-np)))
       (bottom-out (string "Use the unit vector drawing tool (labeled n) to draw a unit vector in the direction ~A." 
			   ?dir))))

;; It would be better for this to be an effect of the drawing rule itself
;; since drawing a unit vector entails it having unit length.
;; The Help/entry-API.cl handler for unit-vector sets n=1 as a side-effect of
;; drawing the vector.
(def-psmclass unit-vector-mag (unit-vector-mag . ?args)
  :complexity definition
  :short-name "length of unit vector"
  :english ("the length of a unit vector")
  :ExpFormat("introducing a unit vector")
  :EqnFormat("n = 1"))

(defoperator unit-vector-mag-contains (?sought)
  :preconditions ( (any-member ?sought ((mag (unit-vector . ?args)))) )
  :effects ( (eqn-contains (unit-vector-mag . ?args) ?sought) ))


(defoperator write-unit-vector-mag (?args)
  :preconditions ((variable ?n (mag (unit-vector . ?args))) )
  :effects ( (eqn (= ?n 1) (unit-vector-mag . ?args)) )
  ;; These should never come out:
  :hint ( (point (string "What is the length of a unit vector?"))
	 (bottom-out (string "Write the equation ~A" ((= ?n 1)  algebra) )) ))

;;;; Shape variables: We define special scalar variables for the appropriate
;;;; dimensions characterizing certain rigid bodies.

;;;; length: defines a variable for the length of a rigid body that has a 
;;;; dimension normally described as a length, i.e. rod or rectangular plate
;;;; or a string
(defoperator define-shape-length (?b)
  :preconditions (
		  (bind ?l-var (format-sym "length_~A" (body-name ?b)))
		  )
  :effects (
	    (define-var (length ?b))
	       (variable ?l-var (length ?b))
	       )
  :hint (
	 (bottom-out (string "Use the Add Variable command to define a variable for the length of ~A"  ?b))
	 ))


;;; mass per length

(defoperator mass-per-length (?rope)
  :preconditions(
		 (bind ?mu-var (format-sym "mu_~A" (body-name ?rope))))
  :effects (
	    (variable ?mu-var (mass-per-length ?rope))
	    (define-var (mass-per-length ?rope)))
  :hint ((bottom-out 
	  (string "Define a variable ~A by using the Add Variable command on the Variable menu and selecting mass per length."  
		  ((mass-per-length ?rope) def-np)))))

;;; mass per length = mass /length of a rod

(def-PSMclass mass-per-length-eqn (mass-per-length-equation ?b)
  :complexity minor  ;same as the moment of inertia formulas
  :doc "mass per length = mass/length"
  :short-name "mass per unit length"
  :english ("mass per length = mass/length")
  :expFormat ("using the mass per length of ~A" (nlg ?b))
  :EqnFormat ("$l = m/L"))

(defoperator mass-per-length-eqn-contains (?quantity)
  :preconditions (
		  (any-member ?quantity
			      ((mass-per-length ?b)
			       (length ?b)
			       (mass ?b))))
  :effects
  ((eqn-contains (mass-per-length-equation ?b) ?quantity)))

(defoperator mass-per-length-equation (?b)
  :preconditions (
		  (variable ?m (mass ?b))
		  (variable ?l (length ?b))
		  (variable ?mu (mass-per-length ?b)))
  :effects
  ((eqn (= ?mu (/ ?m ?l)) (mass-per-length-equation ?b)))
  :hint
  ((point (string "Use the mass per unit length of ~A." ?b))
   (teach (string "The mass per unit length is the total mass of ~a divided by the length of ~a" ?b ?b))
   (bottom-out (string "Because ~a is mass per length, write ~a=~a/~a"
		       ?mu ?mu ?m ?l))
   ))

;;; defines a variable for the "width" of a rigid body that has a dimension
;;; normally described as a width, i.e. second dimension of rectangle.
;;; Body dimensions are typically given; which one counts as "length" and 
;;; which one as "width" would have to be specified in the verbal problem 
;;; statement of the given dimensions. 
(defoperator define-width (?b)
  :preconditions (
     (shape ?b rectangle ?dontcare)
     (bind ?l-var (format-sym "width_~A" (body-name ?b)))
  )
  :effects (
    (define-var (width ?b))
    (variable ?l-var (width ?b))
  )
  :hint (
    (bottom-out (string "Use the Add Variable command to define a variable for the width of ~A" ?b))
  ))


;;;;-------------------------------------------------------------------------
;;;; Formulas for moment of inertia for objects of various shapes about
;;;; axes such as cm (for center of mass) or end. 
;;;;-------------------------------------------------------------------------

;; I for long thin rod rotating about cm = 1/12 m l^2, where l is length
(defoperator I-rod-cm-contains (?sought)
  :preconditions 
  ((shape ?b rod cm)
   ;; could be generalized to include time:
  (any-member ?sought ( (moment-of-inertia ?b)
		        (mass ?b)
		        (length ?b) )))
  :effects ( (eqn-contains (I-rod-cm ?b) ?sought)))

(defoperator write-I-rod-cm (?b)
  :preconditions (
    (variable ?I-var (moment-of-inertia ?b))
    (variable ?m-var (mass ?b))
    (variable ?l-var (length ?b) ))
  :effects 
    ((eqn (= ?I-var (* (/ 1 12) ?m-var (^ ?l-var 2))) (I-rod-cm ?b)))
   :hint
    ((point (string "You need the formula for the moment of inertia of a long thin rod rotating about its center of mass."))
     (bottom-out (string "Write the equation ~A"
            ((= ?I-var (* (/ 1 12) ?m-var (^ ?l-var 2))) algebra)))))

;; I for long thin rod about end = 1/3 M * L^2 where L is length
;; This is our only formula for rotation not about the center of mass.
;; It could be derived from the formula for I about cm plus the "parallel 
;; axis theorem", but we don't include that yet.
(defoperator I-rod-end-contains (?sought)
  :preconditions 
  ((shape ?b rod end)
   ;; this could be generalized to include time
  (any-member ?sought ( (moment-of-inertia ?b)
		        (mass ?b)
		        (length ?b) ))
  )
  :effects ( (eqn-contains (I-rod-end ?b) ?sought)))

(defoperator write-I-rod-end (?b)
  :preconditions (
    (variable ?I-var (moment-of-inertia ?b))
    (variable ?m-var (mass ?b))
    (variable ?l-var (length ?b) ))
  :effects 
    ((eqn (= ?I-var (* (/ 1 3) ?m-var (^ ?l-var 2))) (I-rod-end ?b)))
  :hint
    ((point (string "You need the formula for the moment of inertia of a long thin rod rotating about its end."))
     (bottom-out (string "Write the equation ~A"
            ((= ?I-var (* (/ 1 3) ?m-var (^ ?l-var 2))) algebra)))))


; I for hoop of given radius about center: I = MR^2 where R is radius
; !!! quick hack !!! The workbench offers a "radius" variable, but uses it
; to mean "radius of uniform circular motion" -- our "revolution-radius" --
; and has nothing to be radius of a rigid body shape. In order to allow
; this to be defined with the current workbench, we define I for a hoop in 
; terms of a revolution radius. This is not totally awful since our hoop is 
; likely rotating about it's center of mass, but it should be fixed in
; the workbench. 
(defoperator I-hoop-cm-contains (?sought)
  :preconditions 
  ((shape ?b hoop cm)
   ;; this could be generalized to include time
   (any-member ?sought ((moment-of-inertia ?b)
			(mass ?b)
			(radius-of-circle ?b)
		      ))
  )
  :effects 
    ((eqn-contains (I-hoop-cm ?b) ?sought)))

(defoperator write-I-hoop-cm (?b)
  :preconditions 
    ((variable ?I-var (moment-of-inertia ?b))
    (variable ?m-var (mass ?b))
    (variable ?r-var (radius-of-circle ?b)))
  :effects 
  ( (eqn (= ?I-var (* ?m-var (^ ?r-var 2))) (I-hoop-cm ?b)) )
   :hint
    ((point (string "You need the formula for the moment of inertia of a hoop rotating about its center of mass."))
     (bottom-out (string "Write the equation ~A"
            ((= ?I-var (* ?m-var (^ ?r-var 2))) algebra)))))

; I for disk or cylinder of given radius about center: I = 1/2 M R^2
(defoperator I-disk-cm-contains (?sought)
  :preconditions 
  ((shape ?b disk cm)
   ;; this could be generalized to include time
  (any-member ?sought ((moment-of-inertia ?b)
		       (mass ?b)
		       (radius-of-circle ?b))) )
  :effects 
    ( (eqn-contains (I-disk-cm ?b) ?sought) ))

(defoperator write-I-disk-cm (?b)
  :preconditions (
    (variable ?I-var (moment-of-inertia ?b))
    (variable ?m-var (mass ?b))
    (variable ?r-var (radius-of-circle ?b))
  )
  :effects 
    ( (eqn (= ?I-var (* 0.5 ?m-var (^ ?r-var 2))) (I-disk-cm ?b)) ))

; rectangular plate I = 1/12 M * (l^2 + w^2) where l = length, w = width
(defoperator I-rect-cm-contains (?sought)
  :preconditions 
  ((shape ?b rectangle cm)
   ;; this could be generalized to include time
  (any-member ?sought ( (moment-of-inertia ?b)
		        (mass ?b)
		        (length ?b) 
		        (width ?b) )))
  :effects ( (eqn-contains (I-rect-cm ?b) ?sought)))

(defoperator write-I-rect-cm (?b)
  :preconditions 
   ((variable ?I-var (moment-of-inertia ?b))
    (variable ?m-var (mass ?b))
    (variable ?l-var (length ?b)) 
    (variable ?w-var (width ?b)))
  :effects 
    ((eqn (= ?I-var (* (/ 1 12) ?m-var (+ (^ ?l-var 2) 
                                          (^ ?w-var 2)))) 
          (I-rect-cm ?b)))
   :hint
    ((point (string "You need the formula for the moment of inertia of a rectangle rotating about its center of mass."))
     (bottom-out (string "Write the equation ~A"
            ((= ?I-var (* (/ 1 12) ?m-var (+ (^ ?l-var 2) 
                                             (^ ?w-var 2))))  algebra)))))

;; moment of inertia of a compound body is sum of moments of inertia of 
;; its constituents
(defoperator I-compound-contains (?sought)
   :preconditions 
   (  ;; could be generalized to optionally include time
    (any-member ?sought ( (moment-of-inertia (compound orderless . ?bodies)) ))
     ; can also find I for component bodies from I of compound, see below
   )
   :effects (
     (eqn-contains (I-compound ?bodies) ?sought)
   ))

(defoperator I-compound-contains2 (?sought)
   :preconditions (
     (object (compound orderless . ?bodies))
     (any-member ?sought ( (moment-of-inertia ?b) ))
     (test (member ?b ?bodies :test #'equal))
   )
   :effects (
     (eqn-contains (I-compound ?bodies) ?sought)
   ))

(defoperator write-I-compound (?bodies)
  :preconditions (
		  ;; make sure compound body is drawn. This is the only place 
		  ;; the compound occurs as a "principle body" in a cons 
		  ;; ang-mom problem, for next-step-help to prompt
		  ;; to draw it at the beginning. 
		  ;; (This isn't needed for counterpart mass-compound,
      ;; since compound is drawn as one way of defining mass variable.)
      (body (compound orderless . ?bodies))
      (variable ?I-var (moment-of-inertia (compound orderless . ?bodies)))
      (map ?body ?bodies
         (variable ?Ipart-var (moment-of-inertia ?body))
	 ?Ipart-var ?Ipart-vars)
   )
   :effects (
      (eqn (= ?I-var (+ . ?Ipart-vars)) (I-compound ?bodies))
   )
   :hint (
     (point (string "Think about how the moment of inertia of a compound body relates to the moments of inertia of its parts"))
     (teach (string "The moment of inertia of a compound body is the sum of the moments of inertia of its parts"))
     (bottom-out (string "Write the equation ~A"
               ((= ?I-var (+ . ?Ipart-vars)) algebra)))
   ))


;; area of a shape
(def-qexp area (area ?shape)
  :symbol-base |A|     
  :short-name "area"	
  :dialog-text "of [body:bodies]"
     :units |m^2|
     :restrictions positive
     :english ("the area of ~A" (nlg ?shape))
     :fromworkbench `(area ,body)
)

(def-qexp area-change (rate-of-change (area ?shape))
  :symbol-base |dAdt|     
  :short-name "rate of change in area"	
  :dialog-text "of [body:bodies]"
     :units |m^2/s|
     :restrictions positive
     :english ("the rate of change of the area of ~A" (nlg ?shape))
     :fromworkbench `(rate-of-change (area ,body))
)

(defoperator define-area (?shape)
     :preconditions((bind ?Ac-var (format-sym "A_~A" (body-name ?shape))))
     :effects ((variable ?Ac-var (area ?shape))
               (define-var (area ?shape)))
     :hint (
          (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Area."  
			      ((area ?shape) def-np)))))

;; quantity to represent radius of a circular shape
(def-qexp radius-of-circle (radius-of-circle ?body)
  :symbol-base |r|     
  :short-name "radius"	
  :dialog-text "of [body:bodies]"
     :units |m|
     :restrictions positive
     :english ("the radius of ~A" (nlg ?body))
     :fromworkbench `(radius-of-circle ,body)
   )

(defoperator define-radius-of-circle (?body)
  :preconditions
  ((bind ?rc-var (format-sym "rc_~A" (body-name ?body))))
     :effects ((variable ?rc-var (radius-of-circle ?body))
               (define-var (radius-of-circle ?body))
   )
     :hint (
          (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting circle radius."  
			      ((radius-of-circle ?body) def-np)))))

;; quantity to represent diameter of a circular shape
(def-qexp diameter-of-circle (diameter-of-circle ?body)
  :symbol-base |d|     
  :short-name "diameter"	
  :dialog-text "of [body:bodies]"
  :units |m|
  :restrictions positive
  :english ("the diameter of ~A" (nlg ?body))
  :fromworkbench `(diameter-of-circle ,body)
  )

(defoperator define-diameter-of-circle (?body)
     :preconditions((bind ?dc-var (format-sym "dc_~A" (body-name ?body))))
     :effects ((variable ?dc-var (diameter-of-circle ?body))
               (define-var (diameter-of-circle ?body))
   )
     :hint (
          (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting circle diameter."  
			      ((diameter-of-circle ?body) def-np)))
          ))

;; quantity to represent circumference of a circular shape
(def-qexp circumference-of-circle (circumference-of-circle ?body)
  :symbol-base |c|     
  :short-name "circumference"	
  :dialog-text "of [body:bodies]"
     :units |m|
     :restrictions positive
     :english ("the circumference of ~A" (nlg ?body))
     :fromworkbench `(circumference-of-circle ,body)
   )

(defoperator define-circumference-of-circle (?body)
     :preconditions((bind ?cc-var (format-sym "cc_~A" (body-name ?body))))
     :effects ((variable ?cc-var (circumference-of-circle ?body))
               (define-var (circumference-of-circle ?body))
   )
     :hint (
          (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting circle circumference."  
			      ((circumference-of-circle ?body) def-np)))
          ))

;; equation of the circumference of a circle c = 2*pi*r
(def-psmclass circumference-of-circle-r (circumference-of-circle-r ?body)
  :complexity minor  
  :short-name "circumference of circle"
  :english ("the formula for circumference of a circle")
  :ExpFormat ("finding the circumference of a circle")
  :EqnFormat ("c = 2*$p*r")) 

 (defoperator circumference-of-circle-r-contains (?sought)
   :preconditions (
		   (in-wm (shape ?shape circle))
		   (any-member ?sought ( 
					(radius-of-circle ?shape)
					(circumference-of-circle ?shape)
					))  
   )
   :effects (
	     (eqn-contains (circumference-of-circle-r ?shape) ?sought)
   ))

(defoperator circumference-of-circle-r (?circle)
   :preconditions (
       (variable  ?rc  (radius-of-circle ?circle))
       (variable  ?Ac  (circumference-of-circle ?circle))
   )
   :effects (
    (eqn  (= ?Ac (* 2 $P ?rc)) 
                (circumference-of-circle-r ?circle))
   )
   :hint (
      (point (string "You can use the formula for the circumference of a circle"))
      (teach (string "The circumference of a circle is 2*$p times the radius."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?Ac (* 2 $P ?rc)) algebra)) ))
   )

;; equation of the circumference of a circle c = pi*d
(def-psmclass circumference-of-circle-d (circumference-of-circle-d ?body)
  :complexity minor  
  :short-name "circumference of circle"
  :english ("the formula for the circumference of a circle")
  :ExpFormat ("finding the circumference of a circle")
  :EqnFormat ("c = $p*d")) 

 (defoperator circumference-of-circle-d-contains (?sought)
   :preconditions (
		   (in-wm (shape ?shape circle))
		   (any-member ?sought ( 
					(diameter-of-circle ?shape)
					(circumference-of-circle ?shape)
					))  
   )
   :effects (
	     (eqn-contains (circumference-of-circle-d ?shape) ?sought)
   ))

(defoperator circumference-of-circle-d (?circle)
   :preconditions (
       (variable  ?dc  (diameter-of-circle ?circle))
       (variable  ?Ac  (circumference-of-circle ?circle))
   )
   :effects (
    (eqn  (= ?Ac (* $P ?dc)) 
                (circumference-of-circle-d ?circle))
   )
   :hint (
      (point (string "You can use the formula for the circumference of a circle"))
      (teach (string "The circumference of a circle is $p times the diameter."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?Ac (* $P ?dc)) algebra)) ))
   )

;;; equation of the area of a circle Ac = pi*rc^2
(def-psmclass area-of-circle (area-of-circle ?body)
  :complexity minor  
  :short-name "area of circle"
  :english ("the formula for the area of a circle")
  :ExpFormat ("finding the area of a circle")
  :EqnFormat ("A = $p*r^2")) 

 (defoperator area-of-circle-contains (?sought)
   :preconditions 
   (
    (in-wm (shape ?shape circle))
    (any-member ?sought ( (radius-of-circle ?shape)
			  (area ?shape) ))  
   )
   :effects ( (eqn-contains (area-of-circle ?shape) ?sought) ))

(defoperator area-of-circle (?circle)
   :preconditions (
       (variable  ?rc  (radius-of-circle ?circle))
       (variable  ?Ac  (area ?circle))
   )
   :effects (
    (eqn  (= ?Ac (* $P (^ ?rc 2))) 
                (area-of-circle ?circle))
   )
   :hint (
      (point (string "You can use the formula for the area of a circle"))
      (teach (string "The area of a circle is $p times the radius squared."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?Ac (* $P (^ ?rc 2))) algebra)) ))
   )

(def-psmclass area-of-rectangle (area-of-rectangle ?body :square ?flag)
  :complexity minor  
  :short-name "area of rectangle"
  :english ("the formula for area of a rectangle")
  :ExpFormat ("finding the area of a ~:[rectangle~;square~]" 
	      ?flag)
  :EqnFormat ("A = w*l")) 

(defoperator area-of-square-contains (?sought)
  :preconditions 
  (
    (shape ?shape square ?dontcare)
    (any-member ?sought ((length ?shape)
			 (area ?shape)))  
    )
   :effects ((eqn-contains (area-of-rectangle ?shape :square t) ?sought)) )

(defoperator write-area-of-square (?rectangle)
  :preconditions 
  (
   (variable  ?l  (length ?rectangle))
   (variable  ?A  (area ?rectangle))
   )
  :effects ( (eqn (= ?A (^ ?l 2)) (area-of-rectangle ?rectangle :square t)) )
  :hint (
	 (point (string "You can use the formula for the area of a square"))
	 (teach (string "The area of a square is the length of one side squared."))
	 (bottom-out (string "Write the equation ~A"  
			     ((= ?A (^ ?l 2)) algebra)) )) )

(defoperator square-is-kind-of-rectangle (?shape)
  :preconditions ((shape ?shape square ?dontcare))
  :effects ((shape ?shape rectangle ?dontcare))
)

(defoperator area-of-rectangle-contains (?sought)
   :preconditions 
   (
    (shape ?shape rectangle ?dontcare)
    (any-member ?sought ((width ?shape)
			 (length ?shape)
			 (area ?shape)))  
    )
   :effects ((eqn-contains (area-of-rectangle ?shape) ?sought)) )

(defoperator write-area-of-rectangle (?rectangle)
   :preconditions 
   (
    (variable  ?l  (length ?rectangle))
    (variable  ?w  (width ?rectangle))
    (variable  ?A  (area ?rectangle))
    )
   :effects ( (eqn (= ?A (* ?l ?w)) (area-of-rectangle ?rectangle)) )
   :hint (
      (point (string "You can use the formula for the area of a rectangle"))
      (teach (string "The area of a rectangle is length times width."))
      (bottom-out (string "Write the equation ~A"  
			  ((= ?A (* ?l ?w)) algebra)) )) )

(def-psmclass area-of-rectangle-change (area-of-rectangle-change ?body)
  :complexity definition  
  :short-name "derivative of area of rectangle (constant width)"
  :english ("the derivative of the area of a rectangle")
  :ExpFormat ("taking the derivative of the formula for area")
  :EqnFormat ("dA/dt = w*dl/dt")) 

 (defoperator area-of-rectangle-change-contains (?sought)
   :preconditions (
		   (in-wm (shape ?shape rectangle ?dontcare))
		   (any-member ?sought ((width ?shape)
					(rate-of-change (length ?shape))
					(rate-of-change (area ?shape)))  )
   )
   :effects ((eqn-contains (area-of-rectangle-change ?shape) ?sought)) )

(defoperator write-area-of-rectangle-change (?rectangle)
   :preconditions 
   (
    (variable  ?l  (rate-of-change (length ?rectangle)))
    (variable  ?w  (width ?rectangle))
    (variable  ?A  (rate-of-change (area ?rectangle)))
    )
   :effects ( (eqn (= ?A (* ?l ?w)) (area-of-rectangle-change ?rectangle)) )
   :hint (
      (point (string "You can take the time derivative of the formula for the area of a rectangle"))
      (teach (string "The area of a rectangle is length times width.  Take the time derivative, assuming the width is constant."))
      (bottom-out (string "Write the equation ~A"  
			  ((= ?A (* ?l ?w)) algebra)) )) )

;;; equation for the volume of a cylinder
(def-psmclass volume-of-cylinder (volume-of-cylinder ?body)
  :complexity minor  
  :short-name "volume of a cylinder"
  :english ("the formula for the volume of a cylinder")
  :ExpFormat ("finding the volume of a cylinder")
  :EqnFormat ("V = A*l")) 

 (defoperator volume-of-cylinder-contains (?sought)
   :preconditions 
   (
    (shape ?cylinder cylinder :base ?base)
    (any-member ?sought ( (length ?cylinder)
			  (volume ?cylinder)
			  (area ?base)))
   )
   :effects ( (eqn-contains (volume-of-cylinder ?cylinder ?base) ?sought) ))

(defoperator volume-of-cylinder (?cylinder ?base)
   :preconditions (
       (variable ?V (volume ?cylinder))
       (variable ?A (area ?base))
       (varible ?h (length ?cylinder))
   )
   :effects (
    (eqn  (= ?V (* ?A ?l)) (volume-of-cylinder ?cylinder ?base))
   )
   :hint 
   (
    (point (string "You apply the formula for the volume of a cylinder to ~A."
		   ?cylinder))
      (teach (string "The volume of a cylinder is the area times the length."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?V (* ?A ?l)) algebra)) ))
   )
