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
;;;;===========================================================================
;;;;                Definition of Linear Momentum p=m*v
;;;;===========================================================================

;;; Following writes p_x = m * v_x for a single body and time
;;; body may be a compound body in case of splits or joins.

;; definition of momentum in component form:
(def-psmclass momentum-compo (?eq-type definition ?axis ?rot 
				       (linear-momentum ?body ?time))
  :complexity definition ;so it can be substituted into momentum conservation
  :short-name ("momentum defined (~A component)" (axis-name ?axis))
  :english ("the definition of momentum (component form)")
  :expformat ("applying the definition of momentum to ~A" (nlg ?body))
  :EqnFormat ("p_~a = m*v_~a" (axis-name ?axis) (axis-name ?axis)))

(defoperator momentum-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ((momentum ?b :time ?t)
			(velocity ?b :time ?t)
			(mass ?b) 
			))
   (time ?t)
   (object ?b)
   )
  :effects (
  (eqn-family-contains (linear-momentum ?b ?t) ?sought)
  ;; since only one compo-eqn under this vector PSM, we can just
  ;; select it now, rather than requiring further operators to do so
  (compo-eqn-contains (linear-momentum ?b ?t) definition ?sought)
  ))

(defoperator draw-momentum-diagram (?rot ?b ?t)
  :preconditions 
  ( (body ?b)
    ;; ?dirv = ?dirm is set in drawing rules
    (inherit-vector ?b (velocity ?b :time ?t) ?dirv)
    (inherit-vector ?b (momentum ?b :time ?t) ?dirm)
    (axes-for ?b ?rot) ;maybe a problem for compounds?
  )
  :effects (
   (vector-diagram ?rot (linear-momentum ?b ?t))
  ))


(defoperator write-momentum-compo (?b ?t ?xyz ?rot)
  :preconditions (
    ;; for now, all these preconds satisfied from above
    (inherit-variable ?p_compo (compo ?xyz ?rot (momentum ?b :time ?t)))
    (inherit-variable ?v_compo (compo ?xyz ?rot (velocity ?b :time ?t)))
    (inherit-variable ?m (mass ?b :time ?t))
    ;; The magnitude equation can be put out as an optional equation. 
    ;; But this is now done by the projection equations 
    ;; (variable ?p-var (mag (momentum ?b :time ?t)))
    ;; (variable ?v-var (mag (velocity ?b :time ?t)))
  )
  :effects 
  (
   (eqn (= ?p_compo (* ?m ?v_compo)) 
	      (compo-eqn definition ?xyz ?rot (linear-momentum ?b ?t)))
   ;; associated dir equality is done in drawing rules
   ;; include as optional equation: but might be done by compo-equation
   ;; (implicit-eqn (= ?p-var (* ?m ?v-var)) (mag-momentum ?b ?t))
   )
  :hint (
    (point (string "In order to form an expression for the ~a component of total momentum ~a, you will need an expression for the ~a component of the momentum of ~A ~A."
     ((axis ?xyz ?rot) symbols-label) (?t pp)
     ((axis ?xyz ?rot) symbols-label)  ?b (?t pp)))
    (teach (string "The linear momentum of a body is a vector defined as its mass times the velocity vector. Therefore, the component of a body's momentum along an axis can be expressed as its mass times the component of the body's velocity along that axis."))
    (bottom-out (string "Write the equation ~A"  
                        ((= ?p_compo (* ?m ?v_compo)) algebra)))
  ))

;;;;===========================================================================
;;;;                 Draw Linear Momentum
;;;;===========================================================================
;;;
;;; operators for drawing momentum vectors on simple bodies
;;; these exactly parallel the velocity drawing operators

(defoperator draw-momentum-at-rest (?b ?t)
  :specifications 
   "If there is an object,
     and it is at rest at a certain time,
   then its momentum at that time is zero."
  :preconditions
   ((motion ?b at-rest :time ?t-motion)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (bind ?mag-var (format-sym "p_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    ;; allow student to draw velocity vector, which might not otherwise
    ;; be needed for the solution
    (optional (vector ?b (velocity ?b :time ?t) zero))
    )
  :effects
   ((vector ?b (momentum ?b :time ?t) zero)
    (variable ?mag-var (mag (momentum ?b :time ?t)))
    (given (mag (momentum ?b :time ?t)) (dnum 0 |kg.m/s|))
    (implicit-eqn (= ?mag-var (dnum 0 |kg.m/s|)) (mag (momentum ?b :time ?t)))
    )
  :hint
   ((point (string "Notice that ~a is at rest ~a." ?b (?t pp)))
    (teach (string "When an object is at rest, its velocity is zero. Since the momentum vector is defined as mass times the velocity vector, the momentum is also zero at that time."))
    ;; too simple for a kcd
    (bottom-out (string "Because ~a is at rest ~a, use the momentum tool to draw a zero-length momentum vector for it." ?b (?t pp)))))

;; we could get momentum direction from velocity direction, but these operators
;; get it from straight-line motion spec, so that it is not required that 
;; velocity be drawn first.
(defoperator draw-momentum-straight (?b ?t)
  :specifications 
   "If an object is moving in a straight line at a certain time,
   then its momentum at that time is non-zero and in the same direction
     as its motion."
  :preconditions
   ((motion ?b straight :dir ?dir :time ?t-motion . ?whatever)
    (test (not (equal ?dir 'unknown)))  ; until conditional effects 
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    ;; work-around for kgraph9, Bug #977
    (not (motion ?b at-rest :time ?t))
    (not (vector ?b (momentum ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "p_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
   ((vector ?b (momentum ?b :time ?t) ?dir)
    (variable ?mag-var (mag (momentum ?b :time ?t)))
    (variable ?dir-var (dir (momentum ?b :time ?t)))
    (given (dir (momentum ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (momentum ?b :time ?t))))
  :hint
   ((point (string "Notice that ~a is moving in a straight line ~a." ?b (?t pp)))
    (teach (string "Whenever an object is moving in a straight line, it has a velocity in the same direction as its motion. Since the momentum vector is defined as mass times the velocity vector, the momentum will have the same direction as the velocity.")
	   (kcd "draw_momentum"))
    (bottom-out (string "Because ~a is moving in a straight line ~a, draw a non-zero momentum vector in direction ~a." ?b (?t pp) ?dir))))

(defoperator draw-momentum-straight-unknown (?b ?t)
  :specifications 
   "If an object is moving in a straight line at a certain time,
   then its momentum at that time is non-zero and in the same direction
     as its motion."
  :preconditions
   ((motion ?b straight :dir unknown :time ?t-motion . ?whatever)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (momentum ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "p_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    ;; following is for implicit-eqn, assumes velocity vars are named this way
    (bind ?dir-vel (format-sym "Ov_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    )
  :effects
   ((vector ?b (momentum ?b :time ?t) unknown)
    (variable ?mag-var (mag (momentum ?b :time ?t)))
    (variable ?dir-var (dir (momentum ?b :time ?t)))
    ;; following is "optional equation" put out so solver will be able to 
    ;; determine a value for 0p in case student happens to use it. It isn't
    ;; needed for m*v form solution we teach, so student doesn't have to 
    ;; enter it.
    ;; using this, imp3b does not solve:
    ;; (implicit-eqn (= ?dir-var ?dir-vel) (dir-momentum ?b ?t))
    )
  :hint
   ((point (string "Notice that ~a is moving in a straight line ~a, although the exact direction is unknown." ?b (?t pp)))
    (teach (string "Whenever an object is moving in a straight line, it has a non-zero momentum in the same direction as its motion.")
	   (kcd "draw_nonzero_momentum"))
    (bottom-out (string "Because ~a is moving in a straight line ~a, draw a non-zero momentum vector for it in an approximately correct direction, then erase the number in the direction box to indicate that the exact direction is unknown." ?b (?t pp)))))

(defoperator draw-momentum-curved (?b ?t)
  :preconditions
   ((motion ?b (curved ?kind (?dir ?a-dir)) :time ?t-motion . ?whatever)
    (test (not (equal ?dir 'unknown)))  ; until conditional effects 
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (momentum ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "p_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
   ((vector ?b (momentum ?b :time ?t) ?dir)
    (variable ?mag-var (mag (momentum ?b :time ?t)))
    (variable ?dir-var (dir (momentum ?b :time ?t)))
    (given (dir (momentum ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (momentum ?b :time ?t))))
  :hint
   ((point (string "Notice that ~a is moving ~a." ?b (?t pp)))
    (teach (string "Although the the object is moving in a circular path, you can figure out the direction of motion at any given moment.  Since the momentum vector is defined as mass times the velocity vector, the momentum will have the same direction as the velocity."))
    (bottom-out (string "Because ~a is moving in direction ~A ~a, draw a non-zero momentum vector in direction ~a." ?b ?dir (?t pp) ?dir))))


;;;;===========================================================================
;;;;                 Conservation of Linear Momentum
;;;;===========================================================================
    
(defoperator cons-linmom-contains (?sought)
  :preconditions 
  (
   ;; for now only apply if there is a collision 
   (in-wm (collision (orderless . ?bodies) ?tt :type ?type))
   (any-member ?sought ((momentum ?b :time ?t)) )
   (test (tendpointp ?t ?tt))
   (test (subsetp (simple-parts ?b) ?bodies))
   )
  :effects (
  (eqn-family-contains (cons-linmom ?bodies ?tt) ?sought)
  ;; since only one compo-eqn under this vector PSM, we can just
  ;; select it now, rather than requiring further operators to do so
  (compo-eqn-contains (cons-linmom ?bodies ?tt) lm-compo ?sought)
  ))

(def-goalprop linmom-fbd (vector-diagram ?rot (cons-linmom ?bodies (during ?t1 ?t2)))
   :doc "diagram showing all momentum vectors and axes"
   :english ("drawing a diagram showing all of the needed kinematic vectors and coordinate axes" ))

(defoperator draw-linmom-diagram (?rot ?bodies ?tt)
  :preconditions (
   (not (vector-diagram ?rot (cons-linmom ?bodies ?tt)))
   ;; how much to draw? a lot of vectors at issue:
   ;; total system momentum before, total system momentum after
   ;; constituent momenta (normally 2 initial, 2 final)
   ;; and constituent velocities. 
   ;; Ideally would allow but not require both momenta and velocities. 
   ;; For now we include both so that both get defined.

   ;; For when we provide tool to allow drawing of many-body systems:
   ;;     draw system
   ;;     (body (system ?b1 ?b2))
   ;; draw initial constituent velocity and momentum
   (collision-momenta-drawn ?bodies nil ?tt)
   ;; draw final constitutent velocity and momentum
   (collision-momenta-drawn ?bodies t ?tt)
   ;; draw axis to use for many-body system. 
   ;; ! Because no vectors have been drawn on the system object, will always 
   ;; get standard horizontal-vertical axes since nothing to align with
   (axes-for (system . ?bodies) ?rot)
   ;; must also record axes to use for vectors on system's constituent bodies 
   ;; so they can be picked up from working wm by compo-eqn choosing operators.
   ;; Use-system-axis should apply to inherit from the main system axis. 
   ;; Could also try to do this when drawing axis for many-body system, if we 
   ;; had a special operator for that case. Note this doesn't register
   ;; an axis for compound bodies that are constituents of the system in case 
   ;; of split/join.
   (foreach ?b ?bodies
      (axes-for ?b ?rot))
  )
  :effects ( (vector-diagram ?rot (cons-linmom ?bodies ?tt)) ))

(defoperator draw-collision-momenta (?bodies ?after-flag ?tt)
  :preconditions (
     ;; use this if bodies don't split from initial compound
     ;; !!! code assumes there's only one collision in problem
     (in-wm (collision (orderless . ?bodies) ?tt :type ?type))
     ;; make sure this is a time without a compound body
     (test (not (if ?after-flag (eq ?type 'join) (eq ?type 'split))))
     (bind ?t (if ?after-flag (third ?tt) (second ?tt)))
     (foreach ?b ?bodies (body ?b))
     (foreach ?b ?bodies
   	(vector ?b (momentum ?b :time ?t) ?dirb))
  )
  :effects ( (collision-momenta-drawn ?bodies ?after-flag ?tt) ))

(defoperator draw-collision-momenta-inelastic (?bodies ?tt)
  :preconditions (
     ;; use this if collision involves split or join
     (in-wm (collision (orderless . ?bodies) ?tt :type ?type))
     ;; make sure this is a time with a compound body
     (test (if ?after-flag (eq ?type 'join) (eq ?type 'split)))
     (bind ?t (if ?after-flag (third ?tt) (second ?tt)))
     (bind ?c `(compound orderless ,@?bodies)) ;for shorthand
     (body ?c)
     (axes-for ?c ?rot)
     (vector ?c (momentum ?c :time ?t) ?dirc)
  )
  :effects ( (collision-momenta-drawn ?bodies ?after-flag ?tt) ))


;; following still restricted to two-body collisions, and
;; doesn't use compound bodies before split or after join
(defoperator write-cons-linmom-compo (?bodies ?t1 ?t2 ?xyz ?rot)
  :preconditions 
  (
   ;; use these steps if no split or join
   (in-wm (collision (orderless . ?bodies) (during ?t1 ?t2) :type ?type))
   (test (not (or (equal ?type 'join) (equal ?type 'split))))
   (map ?b ?bodies
	(variable ?p1_compo (compo ?xyz ?rot (momentum ?b :time ?t1)))
	?p1_compo ?p1_compos)
   (map ?b ?bodies
	(variable ?p2_compo (compo ?xyz ?rot (momentum ?b :time ?t2)))
	?p2_compo ?p2_compos)
  ;; allow any zero-valued velocity components to be mentioned, since they
  ;; might not be needed anywhere else in the solution
  (include-zero-vcomps ?xyz ?rot)
  )
  :effects ( 
	    (eqn (= (+ . ?p1_compos) (+ . ?p2_compos))
		 (compo-eqn lm-compo ?xyz ?rot 
			    (cons-linmom ?bodies (during ?t1 ?t2))))
  )
  :hint (
  (point (string "Can you write an equation relating the ~a components of total momentum before and after the collision?" ((axis ?xyz ?rot) symbols-label)))
  (teach (string "The law of conservation of momentum states that if no external force acts on a system, then the total momentum remains constant. Because the total momentum is the vector sum of the momenta of each body in the system, this law entails that the sum of the momentum components in any direction is the same before and
 after a collision."))
  (bottom-out (string "Write conservation of momentum along the ~A axis as ~A"  
			((axis ?xyz ?rot) symbols-label)
			((= (+ . ?p1_compos) (+ . ?p2_compos)) algebra)))
  ))



(defoperator write-cons-linmom-compo-join-split (?bodies ?t1 ?t2 ?xyz ?rot)
  :preconditions (
  ;; use these steps if collision involves split 
  (in-wm (collision (orderless . ?bodies) (during ?t1 ?t2) :type ?flag))
  (any-member ?flag (join split))
  (bind (?t ?tt) (if (eq ?flag 'join) (list ?t1 ?t2) (list ?t2 ?t1)))
  ;; write subsidiary equations for all needed momenta components along ?xyz
  (map ?b ?bodies
       (variable ?pf_compo (compo ?xyz ?rot (momentum ?b :time ?t)))
       ?pf_compo ?pf_compos)
  (bind ?c `(compound orderless ,@?bodies))
  (variable ?pc_compo (compo ?xyz ?rot (momentum ?c :time ?tt)))
  ;; allow any zero-valued velocity components to be mentioned, since they
  ;; might not be needed anywhere else in the solution
  (include-zero-vcomps ?xyz ?rot)
  )
  :effects 
  ( (eqn (= ?pc_compo (+ . ?pf_compos))
	 (compo-eqn lm-compo ?xyz ?rot 
		    (cons-linmom ?bodies (during ?t1 ?t2))))
    )
  :hint 
  (
   (point (string "Can you write an equation relating the ~a components of total momentum before and after the collision?" ((axis ?xyz ?rot) symbols-label)))
   (teach (string "The law of conservation of momentum states that if no external force acts on a system, then the total momentum remains constant. Because the total momentum is the vector sum of the momenta of each body in the system, this law entails that the sum of the momentum components in any direction is the same before and
 after a collision."))
   (bottom-out (string "Write conservation of momentum along the ~A axis as ~A"  
		       ((axis ?xyz ?rot) symbols-label)
		       ((= ?pc_compo (+ . ?pf_compos)) algebra)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Angular momentum and its conservation
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; magnitude of angular momentum: 
;; following draws the angular momentum direction based on the drawn
;; angular velocity vector.
;; !!! could put out equation linking dirL = dirOmega, even if unknown
(defoperator draw-ang-momentum-rotating (?b ?t)
   :preconditions (
    (object ?b)
    (time ?t)
    (not (vector ?b (ang-momentum ?b :time ?t)))
    ; draw the angular velocity
    (vector ?b (ang-velocity ?b :time ?t-rotating) ?dir-vel)
    ; Though the match to dir-vel is perfectly general, the implicit eqn effect 
    ; only makes sense if dir-vel has a known non-zero value
    (test (not (or (eq ?dir-vel 'zero) (eq ?dir-vel 'unknown))))
    (test (tinsidep ?t ?t-rotating))
    (bind ?mag-var (format-sym "L_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir-vel))
  )
  :effects (
    (vector ?b (ang-momentum ?b :time ?t) ?dir-vel)
    (variable ?mag-var (mag (ang-momentum ?b :time ?t)))
    (variable ?dir-var (dir (ang-momentum ?b :time ?t))) 
    (given (dir (ang-momentum ?b :time ?t)) ?dir-vel)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (ang-momentum ?b :time ?t)))
   )
  :hint 
  (
   (point (string "Notice that ~a is rotating ~A ~a.  Consequently, it has a non-zero angular momentum vector." 
		  ?b (?dir-vel rotation-name)  (?t pp) (?dir-vel adj)))
   (teach (string "In the case of a symmetrical rigid body rotating about a fixed axis, the angular momentum vector will be equal to the moment of inertia -- a scalar-- times the angular velocity vector. The angular momentum will therefore point along the z axis in the same direction as the angular velocity vector."))
   (bottom-out (string "Because ~a has an angular velocity pointing ~a ~A, use the momentum tool to draw a non-zero angular momentum vector with direction ~a ." ?b (?dir-vel adj) (?t pp) (?dir-vel adj)))
  ))

; following writes the equation for angular momentum 
; compo equation: L_z = I * omega_z
(def-psmclass ang-momentum (?eq-type definition ?xyz ?rot 
				     (ang-momentum ?body ?time))
  :complexity definition ;definition, but can be first "principle" for sought
  :short-name "angular momentum defined"
  :english ("definition of angular momentum")
  :expformat ("applying the definition of angular momentum on ~a"
	      (nlg ?body 'at-time ?time))
  :EqnFormat ("L_z = I*$w_z"))

(defoperator ang-momentum-contains (?sought)
   :preconditions (
      (any-member ?sought (
              (ang-momentum ?b :time ?t)
	      (ang-velocity ?b :time ?t)
	      (moment-of-inertia ?b :time ?t)
                          )) 
      (time ?t)
   )
   :effects 
   (
    (eqn-family-contains (ang-momentum ?b ?t) ?sought)
    ;; since only one compo-eqn under this vector PSM, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (ang-momentum ?b ?t) definition ?sought)
    ))

(def-goalprop angmom-fbd (vector-diagram ?rot (ang-momentum ?b ?t))
   :english ("drawing a diagram showing all of the needed kinematic vectors and coordinate axes"))

(defoperator draw-ang-momentum-vectors (?rot ?b ?t)
  :preconditions 
     ((vector ?b (ang-momentum ?b :time ?t) ?dir) 
       (axes-for ?b ?rot) )
  :effects 
     ( (vector-diagram ?rot (ang-momentum ?b ?t)) ))

(defoperator write-ang-momentum (?b ?t)
  :preconditions (
     (variable ?L_z (compo ?z ?rot (ang-momentum ?b :time ?t)))
     (variable ?omega_z (compo ?z ?rot (ang-velocity ?b :time ?t)))
     (inherit-variable ?I (moment-of-inertia ?b :time ?t))
  )
  :effects (
     (eqn (= ?L_z (* ?I ?omega_z)) 
                 (compo-eqn definition ?z ?rot (ang-momentum ?b ?t)))
  )
  :hint (
    (point (string "Can you write an equation for the z component of the angular momentum of ~A ~A?" ?b (?t pp)))
    (teach (string "For a body rotating about a fixed axis, the angular momentum vector will be equal to its moment of inertia (a scalar) times the its angular velocity vector. The angular momentum vector will therefore point in the same direction as the angular velocity vector. You can use this vector relation to write an equation for the z component of angular momentum in terms of the z component of angular velocity and the moment of inertia of the object."))
    (bottom-out (string "Write the equation ~A"
                         ((= ?L_z (* ?I ?omega_z)) algebra)))
   ))

;; conservation of angular momentum
;;
;; We use a (collision ... ) statement to indicate that there are no
;; external forces on the system during the specified time interval
;; In the special case that there are no torques about a specific
;; axis, then use the :axis keyword to specify that axis.
;; 

(def-psmclass cons-angmom (?eq-type angmom-id ?xyz ?rot 
				    (cons-angmom ?bodies ?ti))
  :complexity major
  :short-name "conservation of angular momentum"
  :english ("conservation of angular momentum")
  :expformat ("applying Conservation of Angular Momentum to ~a ~a"
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time 'time))
  :eqnformat ("L1i_z + L2i_z + ... = L1f_z + L2f_z + ..."))

(defoperator cons-angmom-contains (?sought)
  :preconditions 
  (
   ;; for now only apply if we are given some momentum conserving change:
   (collision (orderless . ?bodies) ?tt :axis ?axis :type ?split-join)
   (any-member ?sought ((ang-momentum ?b :time ?t)))
   (test (tendpointp ?t ?tt))   
   (test (subsetp (simple-parts ?b) ?bodies))
   )
  :effects (
  (eqn-family-contains (cons-angmom ?bodies ?tt) ?sought)
  ;; since only one compo-eqn under this vector PSM, we can just
  ;; select it now, rather than requiring further operators to do so
  (compo-eqn-contains (cons-angmom ?bodies ?tt) angmom-id ?sought)
    ))

(defoperator draw-cons-angmom-diagram (?rot ?bodies ?tt)
  :preconditions 
  (
   ;; This follows draw-linmom-diagram closely
   (not (vector-diagram ?rot (cons-angmom ?bodies ?tt)))
   (rotation-collision-momenta-drawn ?bodies nil ?tt)
   (rotation-collision-momenta-drawn ?bodies t ?tt)
   (axes-for (system . ?bodies) ?rot)
   (foreach ?b ?bodies
	    (axes-for ?b ?rot))
  )
  :effects (
   (vector-diagram ?rot (cons-angmom ?bodies ?tt))
  ))

(defoperator draw-rotation-collision-momenta (?bodies ?after-flag ?tt)
  :preconditions (
     ;; use this if bodies don't split from initial compound
     (in-wm (collision (orderless . ?bodies) ?tt :axis ?axis :type ?type))
     ;; make sure this is a time without a compound body
     (test (not (if ?after-flag (eq ?type 'join) (eq ?type 'split))))
     (bind ?t (if ?after-flag (third ?tt) (second ?tt)))
     (foreach ?b ?bodies (body ?b))
     (foreach ?b ?bodies
   	(vector ?b (ang-momentum ?b :time ?t) ?dir1))
  )
  :effects ( (rotation-collision-momenta-drawn ?bodies ?after-flag ?tt) ))

(defoperator draw-rotation-collision-momenta-inelastic (?bodies ?tt)
  :preconditions (
     ;; use this if collision involves split or join
     (in-wm (collision (orderless . ?bodies) ?tt :axis ?axis :type ?type))
     ;; make sure this is a time with a compound body
     (test (if ?after-flag (eq ?type 'join) (eq ?type 'split)))
     (bind ?t (if ?after-flag (third ?tt) (second ?tt)))
     (bind ?c `(compound orderless ,@?bodies)) ;for shorthand
     (body ?c)
     (axes-for ?c ?rot)
     (vector ?c (ang-momentum ?c :time ?t) ?dir1)
  )
  :effects ( (rotation-collision-momenta-drawn ?bodies ?after-flag ?tt) ))

(defoperator write-cons-angmom (?bodies ?t1 ?t2 ?z ?rot)
  :preconditions (
   ;; don't use this in case of a join
   (collision (orderless . ?bodies) (during ?t1 ?t2) :axis ?axis :type ?type)
   (test (not (or (equal ?type 'join) (equal ?type 'split))))
   ;; apply single-body ang-momentum method for each to draw vectors and 
   ;; generate compo equation for each body at initial and final times
   (map ?b ?bodies
	(variable ?L1_compo (compo ?z ?rot (ang-momentum ?b :time ?t1)))
	?L1_compo ?L1_compos)
   (map ?b ?bodies
	(variable ?L2_compo (compo ?z ?rot (ang-momentum ?b :time ?t2)))
	?L2_compo ?L2_compos)
  )
  :effects (
  (eqn (= (+ . ?L1_compos) (+ . ?L2_compos))
       (compo-eqn angmom-id ?z ?rot (cons-angmom ?bodies (during ?t1 ?t2))))
	   )
  :hint (
  (point (string "Can you write an equation relating the z components making up the total angular momentum before and after the change?"))
  (teach (string "The law of conservation of angular momentum states that if no external ~A acts on a system, then the total angular momentum in the system remains constant.  Because the total angular momentum is the vector sum of the angular momenta of each body in the system, this law entails that the sum of the z components of the angular momenta of each body is the same before and after any internal change such as change of shape, as long as there is no external ~A."
		 (nil moment-name) (nil moment-name)))
  (bottom-out (string "Write the equation ~A" 
                      ((= (+ . ?L1_compos) (+ . ?L2_compos)) algebra)))
  ))

;; same as above for case of bodies joining together into compound
(defoperator write-cons-angmom-join-split (?bodies ?t1 ?t2)
  :preconditions (
   ;; use this only in case of a join
  (collision (orderless . ?bodies) (during ?t1 ?t2) :axis ?axis :type ?flag)
  (any-member ?flag (join split))
  (bind (?t ?tt) (if (eq ?flag 'join) (list ?t1 ?t2) (list ?t2 ?t1)))
  ;; apply single-body ang-momentum method for each to draw vectors and 
  ;; generate compo equation for each body at initial and final times
  ;; initial time:
   (map ?b ?bodies
	(variable ?L1_compo (compo ?z ?rot (ang-momentum ?b :time ?t)))
	?L1_compo ?L1_compos)
  (bind ?c `(compound orderless ,@?bodies)) ; for shorthand
  (variable ?L2_z (compo ?z ?rot (ang-momentum ?c :time ?tt)))
  )
  :effects (
  (eqn (= (+ . ?L1_compos) ?L2_z)
       (compo-eqn angmom-id ?z ?rot (cons-angmom ?bodies (during ?t1 ?t2))))
	   )
   :hint (
 (point (string "Can you write an equation relating the z-components making up
 the total angular momentum before and after the change?"))
 (teach (string "The law of conservation of angular momentum states that if no external ~A acts on a system, then the total angular momentum in the system constant.  Because the total angular momentum is the vector sum of the angular momenta of each body in the system, this law entails that the sum of the angular momentum components in the z direction is the same before and after a collision."
		(nil moment-name)))
  (bottom-out (string "Write the equation ~A" 
                      ((= (+ . ?L1_compos) ?L2_z) algebra)))	  
	  ))


;;;;===========================================================================
;;;;
;;;;                            Impulse
;;;;
;;;;===========================================================================


;; Impulse is specified in problem statement by given impulse direction 
;; which may be unknown
(def-qexp impulse (impulse ?body ?agent :time ?time)
  :units |N.s|
  :english ("Impulse on ~A due to ~A" 
	    (nlg ?body 'at-time ?time) (nlg ?agent 'agent)))

;; Draw a "given" impulse at a certain direction. 
(defoperator draw-impulse-given-dir (?b ?agent ?t)
  :preconditions
   ((given (dir (impulse ?b ?agent :time ?t)) ?dir)
    (test (not (eq ?dir 'unknown)))
    (not (vector ?b (impulse ?b ?agent :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (debug "~&Drawing ~a impulse on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) ?dir)
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (impulse ?b ?agent :time ?t)))
   )
  :hint
   ((point (string "You were given that there is an impulse on ~a." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b (?agent agent) (?t pp) ?dir))
    ))

;;;;===========================================================================
;;;;
;;;;              Relation of impulse and force
;;;;
;;;;===========================================================================


(def-psmclass impulse-force (?eqn-type definition ?axis ?rot 
				 (impulse-force-vector ?body ?agent ?time))
    :complexity major    
    :Doc "Definition of impulse."
    :short-name "impulse and force"
    :english ("the definition of impulse") 
    :ExpFormat ("applying the definition of impulse on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("J_~A = F(avg)_~a*t" (axis-name ?axis) (axis-name ?axis)))

;; Draw an impulse if associated force and interval known 
(defoperator draw-impulse-given-force (?b ?agent ?t)
  :preconditions
  (
   (force ?b ?agent ?type ?t ?dir ?action)
   (test (time-intervalp ?t)) ;only impulse for intervals
   (test (not (eq ?dir 'unknown)))
   (not (vector ?b (impulse ?b ?agent :time ?t) ?dont-care)) ;not already drawn
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-var-value (dir-var-value ?dir))
    )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) ?dir)
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (impulse ?b ?agent :time ?t))))
  :hint
   ((point (string "There is a force acting on ~a." ?b))
    (teach (string "One can define an impulse associated with the force exerted by ~A ~A." ?agent (?t pp)))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b (?agent agent) (?t pp) ?dir))
    ))


(defoperator impulse-vector-contains (?sought)
  :preconditions 
  ((collision (orderless . ?bodies) ?t :type ?elastic-dont-care)
   (any-member ?sought
	       ((impulse ?b ?agent :time ?t)
		(force ?b ?agent ?type :time ?t)
		(duration ?t)))
   (object ?b) ;sanity test
   (time ?t) ;sanity test
   (any-member ?b ?bodies)
   (any-member ?agent ?bodies)
   (test (not (eq ?b ?agent)))
   (test (time-intervalp ?t)))
  :effects 
   ((eqn-family-contains (impulse-force-vector ?b ?agent ?t) ?sought)
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (impulse-force-vector ?b ?agent ?t) 
			definition ?sought)))

(defoperator draw-impulse-force-vector-diagram (?rot ?b ?agent ?type ?t)
  :preconditions 
  (
   ;; ?agent might not be a proper body
   (body ?b)
   (vector ?b (impulse ?b ?agent :time ?t) ?dir1)
   ;; assuming (without checking) only one force between the two bodies.
   (vector ?b (force ?b ?agent ?type :time ?t) ?dir2)
   (axes-for ?b ?rot)
   )
  :effects (
	    (vector-diagram ?rot (impulse-force-vector ?b ?agent ?t))
  ))

(defoperator write-impulse-compo (?b ?agent ?t1 ?t2 ?xy ?rot)
  :preconditions 
  (
   ;; drawn above, use this to bind ?type
   (in-wm (vector ?b (force ?b ?agent ?type 
				    :time (during ?t1 ?t2)) ?any-dir))
   (variable ?F12_x (compo ?xy ?rot (force ?b ?agent ?type
					   :time (during ?t1 ?t2))))
   (variable ?J12_x (compo ?xy ?rot (impulse ?b ?agent 
					     :time (during ?t1 ?t2))))
   (variable ?t12 (duration (during ?t1 ?t2))))
   :effects 
   (
    (eqn (= ?J12_x (* ?F12_x ?t12))
	 (compo-eqn definition ?xy ?rot 
		    (impulse-force-vector ?b ?agent (during ?t1 ?t2))))
    )
  :hint 
  ( (point (string "What is the relationship between average force, impulse and duration?"))
    (teach (string "The impulse vector is defined as the average force vector times the duration.  This can be applied component-wise."))
    (bottom-out (string "Write the equation ~a"
			((= ?J12_x (* ?F12_x ?t12)) algebra)))
  ))


;;; ================== Impulse and momentum ================================== 
;;;
;;; This is just F=m*a integrated over time.  
;;; The following is based on the "NSL-compo" rules.
;;;


(def-psmclass impulse-momentum 
    (?eq-type imp-momentum ?axis ?rot 
	      (impulse ?body  ?agent (during ?t1 ?t2))) 
     :complexity major
     :doc "equation relating impulse to change in momentum"
     :short-name "Newton's second law"
     :english ("Relation between impulse and change in momentum")
     :ExpFormat ("relating the impulse to the change in momentum of ~a"
		 (nlg ?body))
     :EqnFormat ("J1_~a + J2_~a + ... = pf_~a - pi_~a" 
                 (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) 
		 (axis-name ?axis)))

(defoperator draw-impulse-momentum-diagram (?rot ?b ?t1 ?t2)
  :preconditions 
  ( (body ?b)
    ;; ?dirv = ?dirm is set in drawing rules
    (vector ?b (impulse ?b ?agent :time (during ?t1 ?t2)) ?dirj)
    (vector ?b (momentum ?b :time ?t1) ?dirm1)
    (vector ?b (momentum ?b :time ?t2) ?dirm2)
    (axes-for ?b ?rot) ;maybe a problem for compounds?
  )
  :effects (
   (vector-diagram ?rot (impulse ?b ?agent (during ?t1 ?t2)))
  ))


;;; Draw an impulse if two momenta are known to be opposite 
(defoperator draw-impulse-given-momenta (?b ?agent ?t)
  :preconditions
  (
   (test (time-intervalp ?t))		;introduce ?t to save some typing
   (bind ?t1 (second ?t)) (bind ?t2 (third ?t)) ;get interval endpoints
   (collision (orderless . ?bodies) ?t :type ?elastic-dont-care)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   (motion ?b straight :dir ?dir1 :time ?t1 . ?whatever1)
   (test (not (eq ?dir1 'unknown)))	;known direction
   (motion ?b straight :dir ?dir2 :time ?t2 . ?whatever2)
   (test (not (eq ?dir2 'unknown)))	;known direction
   (test (same-angle ?dir2 (opposite ?dir1))) ;momenta in opposite directions
   (not (vector ?b (impulse ?b ?agent :time ) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-var-value (dir-var-value ?dir2))
   )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) ?dir2)
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
    (given (dir (impulse ?b ?agent :time ?t)) ?dir2)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (impulse ?b ?agent :time ?t)))
   )
  :hint
   ((point (string "The impulse on ~a causes its motion to change." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b (?agent agent) (?t pp) ?dir))
    ))

;; Draw an impulse if two momenta are not known to be equal or opposite
;; and direction of impulse is not given.
(defoperator draw-impulse-given-momenta-unknown-dir (?b ?agent ?t)
  :preconditions
  (
   (not (given (dir (impulse ?b ?agent :time ?t)) ?dir))
   (test (time-intervalp ?t))		;introduce ?t to save some typing
   (bind ?t1 (second ?t)) (bind ?t2 (third ?t)) ;get interval endpoints
   (collision (orderless . ?bodies) ?t :type ?elastic-dont-care)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   (motion ?b straight :dir ?dir1 :time ?t1 . ?whatever1)
   (motion ?b straight :dir ?dir2 :time ?t2 . ?whatever2)
   (test (or (eq ?dir1 'unknown) (eq ?dir2 'unknown) ;unknown direction
	     (not (same-angle ?dir2 (opposite ?dir1))))) ;momenta not opposite 
   (not (vector ?b (impulse ?b ?agent :time ?t) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) unknown)
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    ;; since it is unknown, no implicit-eqn
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
   )
  :hint
   ((point (string "The impulse on ~a causes its motion to change." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b (?agent agent) (?t pp) ?dir))
    ))

;;; This operator indicates when the impulse form of NSL is
;;; applicable.  
;;;
(defoperator impulse-momentum-contains (?sought)
  :preconditions 
  (
   (collision (orderless . ?bodies) (during ?t1 ?t2) :type ?elastic-dont-care)
   (any-member ?sought
	        ((impulse ?b ?agent :time (during ?t1 ?t2))
		 (momentum ?b :time ?t1)
		 (momentum ?b :time ?t2)
		))
   (object ?b) (object ?agent)
   (test (not (equal ?b ?agent)))
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   )
  :effects
  ((eqn-family-contains (impulse ?b ?agent (during ?t1 ?t2)) ?sought)
  ;; since only one compo-eqn under this vector psm, we can just
  ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (impulse ?b ?agent (during ?t1 ?t2)) 
		       imp-momentum ?sought))
 )

;;; It expects to get the body, time, axis label (?xyz) and axis rotation
;;; (?rot) via the equation identifier in the effects, and it fetches
;;; the relevant vectors from wm.  It just looks up the appropriate
;;; variables and writes the equation.  It also leaves behind a
;;; proposition recording the component variables that appear in the
;;; equation.  

(defoperator write-impulse-momentum-compo (?b ?t ?xyz ?rot)
  :preconditions
  ( ;; just to get things working, assume there is only 
   ;; one source of momentum.
   (test (time-intervalp ?t))		;introduce ?t to save some typing
   (bind ?t1 (second ?t)) (bind ?t2 (third ?t)) ;get interval endpoints
   (variable ?J-compo-var (compo ?xyz ?rot (impulse ?b ?agent :time ?t)))
   (variable ?pf-compo (compo ?xyz ?rot (momentum ?b :time ?t2)))
   (variable ?pi-compo (compo ?xyz ?rot (momentum ?b :time ?t1)))
   )
  :effects
  (
   (eqn (= ?J-compo-var (- ?pf-compo ?pi-compo))
	 (compo-eqn imp-momentum ?xyz ?rot (impulse ?b ?agent ?t)))
    (assume using-NL impulse ?b ?t)
    )
  :hint
  ((point (string "You can relate the change in momentum of ~A to the
impulse ~A." (?b def-np) (?t pp)))
    (bottom-out (string "Write the equation using component variables along the ~A axis as ~A" ((axis ?xyz ?rot) symbols-label) ((= ?J-compo-var (- ?pf-compo ?pi-compo)) algebra)))
    ))


;;; ========================== Impulse and Impulse ============================
;;;
;;; This is just NTL integrated over time.  
;;; The following is based on the "NTL-compo" rules.
;;;


(def-psmclass NTL-impulse (NTL-impulse (?Object0 ?Object1) ?time)
  :complexity major
  :short-name "Newton's third law (magnitude)"
  :english ("Newton's third law applied to impulse")
  :ExpFormat ("applying Newton's third law to impulse between ~a and ~a"
	      (nlg ?Object0) (nlg ?Object1 'at-time ?time))
  :EqnFormat ("J12 = J21"))

(def-psmclass NTL-impulse-vector (?eq-type NTL-impulse ?axis ?rot 
					   (NTL-impulse-vector 
					    (?Object0 ?Object1) ?time))
  :complexity major
  :short-name ("Newton's third law (~A component)" (axis-name ?axis))
  :english ("Newton's third law applied to impulse")
  :ExpFormat ("applying Newton's third law to impulse between ~a and ~a"
	      (nlg ?Object0) (nlg ?Object1 'at-time ?time))
  :EqnFormat ("J12_~a = -J21_~a" (axis-name ?axis) (axis-name ?axis)))

(defoperator NTL-impulse-contains (?quantity)
  :preconditions 
  (
   (any-member ?quantity (
			  (mag (impulse ?b1 ?b2 :time ?t))
                        ))
   (bind ?bodies (sort (list ?b1 ?b2) #'expr<))
  )
  :effects ( 
  	(eqn-contains (NTL-impulse ?bodies ?t) ?quantity) 
  ))

(defoperator NTL-impulse (?b1 ?b2 ?t)
  :preconditions (
  (variable ?mag1-var (mag (impulse ?b1 ?b2 :time ?t)))
  (variable ?mag2-var (mag (impulse ?b2 ?b1 :time ?t)))
  )
  :effects 
  (
   (eqn (= ?mag1-var ?mag2-var) (NTL-impulse (?b2 ?b1) ?t)) 
   (assume using-NTL-impulse (?b2 ?b1) ?t)
   (assume using-magnitude (NTL-impulse-vector (?b2 ?b1) ?t)) ;max xor compos
  )
  :hint
  ((point (string "What does Newton's third law tell you about the relation of ~A and ~A" (?mag1-var algebra) (?mag2-var algebra)))
  (teach (string "Newton's third law states that forces come in pairs: whenever A exerts a force of some type on B, B exerts a force of equal magnitude and opposite direction on A.  The same is true for impulse.  You can use that to equate the magnitudes of this pair of impulses."))
   (bottom-out (string "Write the equation ~A" ((= ?mag1-var ?mag2-var) algebra)))
  ))

;;
;; Vector form of NTL-impulse writes component equation J12_x = -J21_x
;;
;; Note the vector equation ID for this is incompatible with convention required
;; by select-compo-eqn-for-scalar, according to which vector args start with
;; body and time. Should be OK, since NTL-impulse doesn't contain any scalars.
;;

(defoperator NTL-impulse-vector-contains (?sought)
  :preconditions (
   (any-member ?sought ( (impulse ?b1 ?b2 :time ?t) ))
   (bind ?body-pair (sort (list ?b1 ?b2) #'expr<))
   )
   :effects (
   (eqn-family-contains (NTL-impulse-vector ?body-pair ?t) ?sought) 
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (NTL-impulse-vector ?body-pair ?t) NTL-impulse ?sought)
   ))

(defoperator draw-NTL-impulse-vector-diagram (?rot ?b1 ?b2 ?t)
  :preconditions (
    ;; Draw both bodies. 
    (body ?b1)
    (body ?b2)
    (vector ?b1 (impulse ?b1 ?b2 :time ?t) ?dir1)
    (vector ?b2 (impulse ?b2 ?b1 :time ?t) ?dir2)
    ;; we need axis-for each body, since component defining operators will 
    ;; lookup axis-for principal body of each vector. Our operators that
    ;; draw axes only apply once, so there is no danger of drawing two
    ;; axes. In order to reuse the axes drawn for body1 as axes used
    ;; for vectors on body2, we added reuse-other-body-axis in axes section.
    (axes-for ?b1 ?rot)
    (axes-for ?b2 ?rot)
    )
  :effects (
	    (vector-diagram ?rot (NTL-impulse-vector (?b1 ?b2) ?t))
  ))
  
(defoperator write-NTL-impulse-compo (?b1 ?b2 ?t ?xy ?rot)
   :preconditions (
      (variable ?J12_xy (compo ?xy ?rot (impulse ?b1 ?b2 :time ?t)))
      (variable ?J21_xy (compo ?xy ?rot (impulse ?b2 ?b1 :time ?t)))
   )
   :effects (
    (eqn (= ?J12_xy (- ?J21_xy)) (compo-eqn NTL-impulse ?xy ?rot (NTL-impulse-vector (?b1 ?b2) ?t)))
    (assume using-NTL-impulse (?b1 ?b2) ?t)
   )
   :hint (
     ;; !!! TODO
     (point (string "What does Newton's third law tell you about the relation of ~A and ~A" (?J12_xy algebra) (?J21_xy algebra)))
    (teach (string "Newton's third law states that the members of an action/reaction pair of forces are equal in magnitude and opposite in direction.  The same must be true for impulse.  This entails that the components of each impulse vector are the negations of the corresponding components of the other: J12_x = -J21_x and J12_y = -J21_y."))
     (bottom-out (string "Write the equation ~A" 
                         ((= ?J12_xy (- ?J21_xy)) algebra)))
   ))

;;;;===========================================================================
;;;;
;;;;                        Center of Mass
;;;;
;;;;===========================================================================

(def-psmclass center-of-mass-compo (?eq-type definition ?axis ?rot 
				       (center-of-mass ?com ?time))
  :short-name "center of mass"
  :english ("definition of center of mass")
  :complexity major ;we want the equation to be used explicitly
  :EqnFormat ("Rcm_~A = (m1*r1_~A + m2*r2_~A + ...)/(m1 + m2 + ...)" 
	      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))

(defoperator center-of-mass-contains (?sought)
  :preconditions 
  (
   (center-of-mass ?com ?bodies) ;define objects (and origin) for cm
   ;; This is yucky.  The studend should be able to specify
   ;; their own axis.
   (origin ?origin) ;explicitly specify origin point
   (any-member ?sought (
			(relative-position ?b ?origin :time ?t)
			(mass ?b) 
			))
   (test (or (member ?b ?bodies :test #'equal) (equal ?b ?com)))
   (time ?t)
   )
  :effects (
  (eqn-family-contains (center-of-mass ?com ?t) ?sought)
  ;; since only one compo-eqn under this vector psm, we can just
  ;; select it now, rather than requiring further operators to do so
  (compo-eqn-contains (center-of-mass ?com ?t) definition ?sought)
  ))

;;;
;;;  Currently, the center of mass is assumed to have an unknown direction
;;;  There are cases where a specific direction could be found from
;;;  the problem statements (all objects are collinear).
;;;  In that case, the use of the pythagorean theorem should be suppressed.
;;;  see cm3.
;;;

(defoperator draw-center-of-mass-diagram (?rot ?com ?t)
  :preconditions 
  ( 
   (in-wm (center-of-mass ?com ?bodies)) ;define objects for cm
   (origin ?origin) ;explicitly specify origin point
   (foreach ?b ?bodies
	    (body ?b))			;make object
   (foreach ?b ?bodies			;make position vector
	    (vector ?b (relative-position ?b ?origin :time ?t) ?dirb))
   (vector ?com (relative-position ?com ?origin :time ?t) ?dircom)
   ;; branch on possible axes
   (map ?b ?bodies (axes-for ?b ?rotb) ?rotb ?rots)
   (bind ?reduced (remove-duplicates ?rots))
   (any-member ?rot ?reduced)
   )
  :effects (
   (vector-diagram ?rot (center-of-mass ?com ?t))
  ))


(defoperator write-center-of-mass-compo (?com ?t ?xyz ?rot)
  :preconditions 
  ( (in-wm (center-of-mass ?com ?bodies)) ;define objects for cm
    (origin ?origin) ;explicitly specify origin
    (variable ?r-com-compo (compo ?xyz ?rot (relative-position ?com ?origin :time ?t)))
    ;; list of position variables
    (map ?b ?bodies
	(variable ?r-compo-var (compo ?xyz ?rot (relative-position ?b ?origin :time ?t)))
	?r-compo-var ?r-compo-vars)
    ;; list of mass variables
    (map ?b ?bodies
	(variable ?mass-var (mass ?b))
	?mass-var ?mass-vars)
    ;; compute list of products of mass and position for each ?b
    (bind ?rhs (mapcar #'(lambda(a b) (list '* a b)) ?mass-vars ?r-compo-vars))
  )
  :effects 
  (
   (eqn (= (* ?r-com-compo (+ . ?mass-vars)) (+ . ?rhs)) 
	      (compo-eqn definition ?xyz ?rot (center-of-mass ?com ?t)))
   )
  :hint 
  ( (point (string "Find the center of mass of ~A ~A"  
		   (?bodies conjoined-defnp) (?t pp)))
    (teach (string "Use the masses and the ~A component of the positions"
		   ((axis ?xyz ?rot) symbols-label)))
    (bottom-out (string "Write the equation ~A"  
                        ((= ?r-com-compo (/ (+ . ?rhs) (+ . ?mass-vars))) algebra)))
    ))
