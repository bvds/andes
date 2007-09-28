;;;;
;;;;                     Energy and work
;;;;
;;;;

;;;
;;; definition of potential energy in terms of work
;;;

(def-psmclass potential-energy-definition
  (potential-energy ?body ?agent ?type ?time)
  :complexity definition
  :short-name "definition of potential energy"
  :english ("the relation of potential energy to work done")
  :ExpFormat ("relating the ~A of ~A to the work done by ~A"
		 (nlg ?type 'adj) (nlg ?body) (nlg ?agent))
  :EqnFormat ("U2 - U1 = -W12" ))



(defoperator potential-energy-contains-energy (?sought)
  :preconditions 
  (
   (any-member ?sought ( (grav-energy ?body ?agent :time ?t)
			 (electric-energy ?body ?agent :time ?t)
			 (dipole-energy ?body ?agent :time ?t)
			 (spring-energy ?body ?agent :time ?t)
			 ))
   (bind ?type (car ?sought))
   (time (during ?t1 ?t2))
   (any-member ?t (?t1 ?t2))
  )
  :effects (
   (eqn-contains (potential-energy ?body ?agent ?type (during ?t1 ?t2)) ?sought)
  ))


(defoperator potential-energy-contains-work (?sought)
  :preconditions 
  (
   (any-member ?sought ((work ?body ?agent :time ?t)))
   (any-member ?type (grav-energy electric-energy dipole-energy
				    spring-energy))
  )
  :effects (
   (eqn-contains (potential-energy ?body ?agent ?type ?t) ?sought)
  ))

(defoperator write-potential-energy (?body ?agent ?type ?t1 ?t2)
  :preconditions 
  (
   (variable ?W-var (work ?body ?agent :time (during ?t1 ?t2)))
   (variable ?U1-var (?type ?body ?agent :time ?t1))
   (variable ?U2-var (?type ?body ?agent :time ?t2))
   )
  :effects (
    (eqn (= ?W-var (- ?U1-var ?U2-var))
	 (potential-energy ?body ?agent ?type (during ?t1 ?t2)))
    )
  :hint 
  (
   (point (string "How does ~A change ~A?" 
		  ((?type ?body ?agent) def-np) ((during ?t1 ?t2) pp)))
   (teach (string "The potential energy of a body is defined in terms of the work done on that body by a conservative force."))
   (bottom-out (string "Write the equation ~A" 
		       ( (= (- ?U2-var ?U1-var) (- ?W-var)) algebra) ))
  ))


;;--------------------------------------------------------------------------
;;               gravitational potential energy for point
;;--------------------------------------------------------------------------

(def-psmclass gravitational-energy-point
  (gravitational-energy-point ?body ?agent ?zerop ?time)
  :complexity major
  :short-name "gravitational potential energy, spherical source"
  :english ("the gravitational potential energy due to a spherical object")
  :ExpFormat ("calculating the gravitational potential of ~a due to ~a"
		 (nlg ?body) (nlg ?agent))
  :EqnFormat ("Ug = -G*m1*m2/r" ))

(defoperator gravitational-energy-point-contains (?sought)
  :preconditions 
  (
   (gravity (orderless . ?grav-bodies) :time ?t-grav)
   (any-member ?sought ( (grav-energy ?body ?agent :time ?t)
			 (mag (relative-position ?cm-body ?cm-agent :time ?t))
			 (mass ?body)
			 (mass ?agent)
			 ))
   ;; in case sought is relative position:
   (center-of-mass ?cm-body (?body))
   (center-of-mass ?cm-agent (?agent))
   (time ?t)
   (test (tinsidep ?t ?t-grav))
   ;; NB: have to make sure body is a gravitational, or else this will apply
   ;; for any relative positions in any problem.
   (any-member ?body ?grav-bodies)
   (any-member ?agent ?grav-bodies)
   (test (not (unify ?body ?agent))) ;no self-energy
  )
  :effects (
   (eqn-contains (gravitational-energy-point ?body ?agent t ?t) ?sought)
  ))

(defoperator write-gravitational-energy-point (?body ?agent ?t)
  :preconditions 
  (
   (body ?body)   ;this psm draws ?body
   (variable ?m1 (mass ?body))
   (variable ?m2 (mass ?agent))
   (variable ?Ug (grav-energy ?body ?agent :time ?t))
   ;; force is on b1 due to b2, so want relative position of center of
   ;; b1 wrt center of b2. 
   (center-of-mass ?cm-body (?body))
   (center-of-mass ?cm-agent (?agent))
   (inherit-variable ?r (mag (relative-position ?cm-body ?cm-agent :time ?t)))
   )
  :effects (
    (eqn (= ?Ug (- (/ (* |G| ?m1 ?m2) ?r))) 
	 (gravitational-energy-point ?body ?agent t ?t))
    )
  :hint (
    (teach (string "Newton's law of universal gravitation states that the magnitude of the gravitational force between two bodies, masses m1 and m2, is equal to G*m1*m2/r^2.  If we integrate this over r, with r going from r=infinity to r=r1, we get the potential energy -G*m1*m2/r.  This applies to either m1 or m2."))
    (bottom-out (string "Write the equation ~A" 
                         ((= ?Ug (- (/ (* G ?m1 ?m2) ?r))) algebra) ))
  ))

(defoperator gravitational-energy-zero-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ( (grav-energy ?body ?agent :time ?t) ))
   (time ?t) ;sanity test
   ;; make sure Newton's law of gravitation should be zero
   (not (gravity (orderless . ?grav-bodies) :time ?t-grav) 
	(and (member ?body ?grav-bodies) (member ?agent ?grav-bodies) 
	     (tinsidep ?t ?t-grav)))
   ;; make sure there is no constant gravitational field
   (not (near-planet ?agent :body ?body ?body))
   )
  :effects (
   (eqn-contains (gravitational-energy-point ?body ?agent nil ?t) ?sought)
  ))

(defoperator write-gravitational-energy-zero (?body ?agent ?t)
  :preconditions ((variable ?Ug (grav-energy ?body ?agent :time ?t)))
  :effects ((eqn (= ?Ug 0) (gravitational-energy-point ?body ?agent nil ?t)))
  :hint (
	(teach (string "We define the gravitational potential energy of widely separated bodies to be zero."))
    (bottom-out (string "Write the equation ~A" 
                         ((= ?Ug 0) algebra) ))
  ))


;;;===================== Conservation of Energy ===============================
;;;
;;; This method applies conservation of mechanical energy to a single body 
;;; at different times to find the sought quantity. It writes the top-level 
;;; equality of ; total mechanical energy, writes equations for all the 
;;; constituent terms (kinetic and potential) making up total mechanical 
;;; energy at each time, and plugs in the terms to get the final equation. 
;;;
;;; We look for gravitational potential energy due to nearby planet and 
;;; elastic potential energy stored in a compressed massless spring in contact 
;;; with the body at some time.  Properly speaking, the total energy is a 
;;; property of the body-planet-spring system, but that is implicit here. 
;;; For the future we should generalize this method to choose a system of 
;;; objects and sum the energies of all its constitutents, but this simple 
;;; method suffices to solve for the energy problems in Andes 1.
;;;
;;; Note: Many problems could in principle be solved either by kinematics or
;;; conservation of energy or a combination. However, our energy solution 
;;; requires use of the "height" quantity for gravitational pe, which specifies
;;; the height of the body with respect to a zero-level stipulated in the 
;;; problem givens.  Since the "height" of the body at various times is only 
;;; explicitly specified in the givens when we want energy methods to be used, 
;;; this functions to restrict the problems in which energy solutions will be 
;;; found.  We don't have fully general rules that relate "height" to any 
;;; other quantities such as displacement, distance travelled, or distance 
;;; between points, even though change in height is in fact related to these. 
;;; (Also, even if we could get the *change* in height from these, we still 
;;; wouldn't know which height was the zero-level.) 
;;; Moreover, if we *don't* specify displacement and time then kinematics 
;;; solutions cannot be found. 
;;; Specify both height and the kinematics quantities in the givens if you 
;;; want both kinematics and energy solutions to be found.
;;;
;;; These problems also rely on the expedient used in the CLIPS solutions of 
;;; simply stipulating the reference-point used as the zero of gravitational 
;;; pe, so that height is a one-argument quantity.  This should probably be
;;; changed in the future. The ANDES interface could change to provide some 
;;; way for the student to choose the zero level, and their equations would 
;;; have to be translated.
;;;
;;; Note we need time parameters in the operator because one of the times is
;;; going to be chosen, and might be chosen multiple ways. For example, if our
;;; sought is height at 3 (as in e1a) we want different op-apps for 
;;; cons-energy 2 3 and cons-energy 1 3 because these are different equations
;;; in the solution graph.
;;
;;  cons-energy would be redundant if we had principles relating 
;;  work done on A by B to the change in the associated potential energy of A.
;;

(defoperator cons-energy-contains (?sought)
  :preconditions 
  (
   ;; check sought is one of cons-energy quants at timepoint t
   (any-member ?sought ((total-energy ?b :time ?t)))
   (object ?b) ;sanity check
   ;; find time interval containing ?sought time
   ;; Here, we allow intervals that are not explicitly declared.
   (time ?t1) (time ?t2)
   (test (and (time-pointp ?t1) (time-pointp ?t2) (tearlierp ?t1 ?t2)))
   (any-member ?t (?t1 ?t2)) 
   ;; need to ensure all forces conservative so energy is in fact conserved.
   ;; Cheap way would be to assert it in problem statement. For now, test no 
   ;; friction or drag, external applied, thrust, or tension force on body. 
   ;; We test by testing for the situation descriptions that entail 
   ;; these forces.
   ;; An applied force might be conservative, but don't include since
  ;; there is no corresponding potential
   (not (given (dir (force ?b ?agent1 applied :time ?t-applied)) ?dir1)
	(or (null ?t-applied) (tintersect2 ?t-applied `(during ,?t1 ,?t2))))
   (not (given (dir (force ?b ?agent2 thrust :time ?t-thrust)) ?dir2)
	(or (null ?t-thrust) (tintersect2 ?t-thrust `(during ,?t1 ,?t2))))
   (not (tied-to ?string1 ?b :time ?t-tension :dir ?dir3)
	(tintersect2 ?t-tension `(during ,?t1 ,?t2)))
   (not (slides-against ?b ?surface1 :time ?t-friction)
	(tintersect2 ?t-friction `(during ,?t1 ,?t2)))
   (not (drag ?b ?medium :time ?t-drag)
	(tintersect2 ?t-drag `(during ,?t1 ,?t2)))
   ;; no collisions involving the body
   ;; cons-ke-elastic handles the case of elastic collisions separately
   (not (collision (orderless . ?objects) ?t-collision . ?whatever-type)
	(and (member ?b ?objects) 
	     (tintersect2 ?t-collision `(during ,?t1 ,?t2))))
  ;; Also not conserved if a (possibly unknown) external work source is given 
  ;; (the associated force may not be defined, but it is still doing work).
  (not (does-work-on ?agent ?b :time ?t-work)
       (tintersect2 ?t-work `(during ,?t1 ,?t2)))
  ;; make sure we can determine all forces:
  (not (unknown-forces :time (during ?t1 ?t2) (during ?t1 ?t2)))
  )
 :effects (
	   (eqn-contains (cons-energy ?b ?t1 ?t2) ?sought)
	   ))

;; generate equation TME_1 = TME_2
(defoperator write-energy-cons (?b ?t1 ?t2)
  :preconditions 
  (
   (body ?b)
   (variable ?te1-var (total-energy ?b :time ?t1))
   (variable ?te2-var (total-energy ?b :time ?t2))
  )
  :effects (
  (eqn (= ?te1-var ?te2-var) (cons-energy ?b ?t1 ?t2))
  (assume using-energy-conservation cons-energy ?b ?t1 ?t2)
  )
  :hint (
  (point (string "Think about what you can conclude about the total mechanical energy in the system throughout this problem."))
  (point (string "Notice that all forces doing work on ~a in this problem are conservative." ?b ))
  (teach (string "When the only forces doing work on a body are conservative, then the law of conservation of energy states that the total mechanical energy remains constant.  That is, the total mechanical energy at one time is equal to the total mechanical energy at another time, for any two time points."))
  (bottom-out (string "Write ~a" ((= ?te1-var ?te2-var) algebra)))
  ))


(defoperator total-energy-top-contains-total-energy (?sought)
  :preconditions 
  (
   (any-member ?sought ((total-energy ?body :time ?t)))
   )
  :effects ((eqn-contains (total-energy-top ?body ?t) ?sought)))

(defoperator total-energy-top-contains-ee-var (?sought)
  :preconditions ((ee-var ?body ?t ?sought))
  :effects ((eqn-contains (total-energy-top ?body ?t) ?sought)))

(defoperator define-energy-var (?b ?t ?quant)
  :preconditions 
  ( 
   (ee-var ?b ?t ?quant) ;test that quantity is allowed (appropriate)
   (bind ?ge-var (format-sym "U~A_~A~@[_~A~]" 
			      (subseq (string (car ?quant)) 0 2) 
			      (body-name ?b) (time-abbrev ?t))))
 :effects ( 
	   (define-var ?quant) 
	   (variable ?ge-var ?quant) 
  )
 :hint (
	 (bottom-out (string "Define a variable for ~A by selecting Energy from the Variables menu on the top menu bar."
			     (?quant def-np)))
       ))

;;; equation TME = Translational Kinetic Energy + Rotational KE
;;;                    + Grav PE + Spring PE + Electrostatic PE ...
;;  Note that any new kind of energy must be added to the list
;;  of energies in the hint below.
;; !!! spring PE term could just be omitted if spring not extended at t
(defoperator write-total-energy-top (?b ?t)
  :preconditions 
  (
   (variable ?te-var (total-energy ?b :time ?t))
   ;; can't collect potential energies if not all are defined
   (not (unknown-potentials))	  
   ;; collect energy quantities that can be defined
   (setof (ee-var ?b ?t ?quant) ?quant ?quants)
   (test ?quants)  ;make sure there is something
   ;; define a variable for each quantity
   (map ?quant ?quants
	(inherit-variable ?ee-var ?quant) ?ee-var ?ee-vars)
   (debug "Set of ee-vars = ~A~%" ?ee-vars)
  )
  :effects (
	    (eqn (= ?te-var (+ . ?ee-vars)) (total-energy-top ?b ?t))
  )
  :hint (
   (point (string "Try writing an equation defining the total mechanical energy of the system containing ~a ~a" (?b def-np) (?t pp)))
   (teach (string "The total mechanical energy is the sum of the kinetic energy and the potential energy. Kinetic energy consists of translational and rotational kinetic energy.  Potential energy consists of the gravitational potential energy, electric potential energy, the energy of any dipole, and the elastic potential energy in any spring in the system."))
   (bottom-out (string "Write ~a" 
		       ((= ?te-var (+ . ?ee-vars)) algebra)))
   ))

;;; these operators achieve (ee-var ?b ?t ?var) by defining a variable needed 
;;; for applicable constituents of the energy of body at t in this 
;;; problem
(defoperator define-grav-ee-var (?b ?t)
    :preconditions 
    (
     ;; use this for gravity near surface of a planet
     (near-planet ?planet :body ?b ?b)
     )
    :effects ( (ee-var ?b ?t (grav-energy ?b ?planet :time ?t)) ))

(defoperator define-grav-point-ee-var (?b ?agent ?t)
    :preconditions 
    (
     ;; use this for gravitational potential force defined at any time
     (gravity (orderless . ?bodies) :time ?t-grav)
     (any-member ?b ?bodies)
     (any-member ?agent ?bodies)
     (test (not (unify ?b ?agent))) ;no self-energy
     )
    :effects ( (ee-var ?b ?t (grav-energy ?b ?agent :time ?t)) ))

(defoperator define-spring-ee-var (?b ?spring ?t)
    :preconditions (
       ;; use this form if spring contact present anywhere in problem -- 
       ;; spring pe may be zero at some times but still use a term for it.
       (in-wm (spring-contact ?b ?spring . ?dontcare))
    )
    :effects ( (ee-var ?b ?t (spring-energy ?b ?spring :time ?t) ) ))

(defoperator define-kinetic-energy-ee-var (?b ?t)
  :preconditions 
  (
   (use-point-for-body ?b ?cm ?axis) ;always use axis of rotation
   ;; test for translational motion of any axis at any time
   ;; There may be more than one (motion ...) statement that matches
   (in-wm (motion ?axis ?kind . ?whatever))
   (test (or (eq ?kind 'straight) 
	     (and (consp ?kind) (eq (first ?kind) 'curved))))
    )
  :effects ( (ee-var ?b ?t (kinetic-energy ?b :time ?t)) ))

(defoperator define-rotational-energy-ee-var (?b ?t)
  :preconditions 
  (
   ;; include if it rotates at any time
   (in-wm (motion ?b rotating . ?whatever))
   )
  :effects ( (ee-var ?b ?t (rotational-energy ?b :time ?t)) ))

;;; For an object that is rotating, we define linear 
;;; kinematic quantities using the center of mass or the fixed axis.
;;; Nothing is done in the case of a rotating body rotating about
;;; an axis that neither fixed nor the center of mass.
;;;
;;; We assume this property if it is rotating about the center of 
;;; mass or a fixed point at any time.
;;;
(defoperator use-body-rotating-cm (?body)
  :preconditions 
  ( (center-of-mass ?cm (?body))
    (in-wm (object ?body)) ;sanity test
    (motion ?body rotating :axis ?cm . ?whatever)
    )
  :effects ( (use-point-for-body ?body ?cm ?cm) 
	     ;; ?cm is kinematic variable for translational motion
	     (object ?cm)))

;; fixed axis case
(defoperator use-body-rotating-fixed (?body)
  :preconditions 
  ( (object ?body)
    (motion ?body rotating :axis ?axis :time ?t-motion . ?whatever)
    (center-of-mass ?cm (?body))
    (motion ?axis ?rest :time ?t-axis)
    (test (or (eq ?rest 'at-rest) (eq ?rest 'momentarily-at-rest)))
    ;; some time where both are true
    (test (or (null ?t-motion) (null ?t-axis) (tintersect2 ?t-motion ?t-axis)))
    ) 
  :effects ( (use-point-for-body ?body ?cm ?axis)))

;; for non-rotating motion, we don't have to distinguish
(defoperator use-body-for-body (?body)
  :preconditions 
  ( (object ?body)
    (not (motion ?body rotating . ?whatever))
    ;; make sure this is really a body
    (not (point-on-body ?body ?another-body)))
  :effects ( (use-point-for-body ?body ?body ?body) ))

;;;
;;; equations for constituents of total energy:
;;;

;; equation KE = 1/2 * m * v^2

(defoperator kinetic-energy-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ((kinetic-energy ?body :time ?t)
			(mag (velocity ?body :time ?t))
			(mass ?body :time ?t)))
   (time ?t) ; choose t if sought is mass
   ;; if we did allow an interval, would need test for constant v
   (test (time-pointp ?t)) 
   )
  :effects (
    (eqn-contains (kinetic-energy ?body ?t) ?sought)
  ))

(defoperator write-kinetic-energy (?body ?t)
  :preconditions 
  (
   ;; test for translational motion of body axis at any time
   (motion ?axis ?kind . ?whatever)
   (test (or (eq ?kind 'straight) 
	     (and (consp ?kind) (eq (first ?kind) 'curved))))
   (use-point-for-body ?body ?cm ?axis)	;always use axis of rotation
   (variable ?ke-var (kinetic-energy ?body :time ?t))
   (inherit-variable ?m-var (mass ?body :time ?t))
   (variable ?v-var (mag (velocity ?axis :time ?t)))
  )
  :effects (
   (eqn (= ?ke-var (* 0.5 ?m-var (^ ?v-var 2))) 
	(kinetic-energy ?body ?t))
   )
  :hint (
  (point (string "Try writing the definition of translational kinetic energy of ~a ~a" (?body def-np) (?t pp)))
  (teach (string "The translational kinetic energy of an object is defined as one half its mass times its velocity squared.  That is, 0.5*m*v^2."))
  (bottom-out (string "Write the equation ~a" ((= ?ke-var (* 0.5 ?m-var (^ ?v-var 2))) algebra)))
  ))

(defoperator rotational-energy-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ((rotational-energy ?body :time ?t)
			(mag (ang-velocity ?body :time ?t))
			(moment-of-inertia ?body :time ?t)))
   (time ?t) ;sanity test
   ;; if we did allow an interval, would need test for constant omega
   (test (time-pointp ?t)) 
   )
  :effects (
    (eqn-contains (rotational-energy ?body ?t) ?sought)
  ))

(defoperator write-rotational-energy (?body ?t)
  :preconditions 
  (
   ;; if the object is rotating at any time, then this term should be 
   ;; included.
   (motion ?body rotating . ?whatever)
   (variable ?kr-var (rotational-energy ?body :time ?t))
   ;; definition of energy at a given moment is ok with changing mass...
   (inherit-variable ?m-var (moment-of-inertia ?body :time ?t))
   (variable ?v-var (mag (ang-velocity ?body :time ?t)))
  )
  :effects (
   (eqn (= ?kr-var (* 0.5 ?m-var (^ ?v-var 2)))
        (rotational-energy ?body ?t))
   )
  :hint (
  (point (string "Try writing the definition of rotational kinetic energy of ~a ~a" (?body def-np) (?t pp)))
  (teach (string "The rotational kinetic energy of an object is defined as one half its moment of inertia times its angular velocity squared.  That is, 0.5*I *$w^2."))
  (bottom-out (string "Write the equation ~a" ((= ?ke-var (* 0.5 ?m-var (^ ?v-var 2))) algebra)))
  ))

(defoperator grav-energy-contains (?sought)
  :preconditions (
    (any-member ?sought ((grav-energy ?body ?planet :time ?t)
                         (mass ?body :time ?t)
		         (gravitational-acceleration ?planet)
                         (height ?cm :time ?t)))
    (object ?body)	; must choose if sought is g 
    (time ?t)		; must choose if sought is g
    (near-planet ?planet :body ?body ?body)
    (use-point-for-body ?body ?cm ?axis) ;always use cm
  )
  :effects (
    (eqn-contains (grav-energy ?body ?planet ?t) ?sought)
    ;; set flag to choose standard axes because energy problem
    (use-energy-axes)
  ))

;; equation PE_grav = m * g * h
;; Note relies on problem statement stipulating zero level. 
(defoperator write-grav-energy (?body ?planet ?t)
  :preconditions 
  (
  (axes-for ?body 0) ;draw unrotated axes for gravitational energy
  (variable ?PE-var (grav-energy ?body ?planet :time ?t))
  (inherit-variable ?m-var  (mass ?body :time ?t))
  (use-point-for-body ?body ?cm ?axis) ;always use cm
  (variable ?h-var (height ?cm :time ?t))
  (variable ?g-var (gravitational-acceleration ?planet))
  )
  :effects ((eqn (= ?PE-var (* ?m-var ?g-var ?h-var)) 
		 (grav-energy ?body ?planet ?t))
	    )
  :hint (
   (point (string "Try writing an equation for gravitational potential energy of ~a ~a" (?body def-np) (?t pp)))
   (teach (string "The gravitational potential energy of a body near the surface of a planet is m*g*h, its mass times the gravitational acceleration times its height above the stipulated zero level."))
   (bottom-out (string "Write ~a" ((= ?PE-var (* ?m-var ?g-var ?h-var)) algebra)))
   ))

(defoperator spring-energy-contains (?sought)
  :preconditions (
    (any-member ?sought ((spring-energy ?body ?spring :time ?t)
                         (spring-constant ?spring)
                         (compression ?spring :time ?t)))
    (object ?body)	; must choose if sought is k or d
    (time ?t)		; must choose if sought is k
    (spring-contact ?body ?spring ?t-contact ?sforce-dir)
    ;; if we did allow an interval, would need test for constant compression
    (test (time-pointp ?t)) 
  )
  :effects (
    (eqn-contains (spring-energy ?body ?spring ?t) ?sought)
  ))

;;; equation PE_spring = 1/2 * k * d^2 
;;; where k = spring const, d = compression distance.
;;; This only applies if spring in contact with object with non-zero 
;;; compression, as given in the spring-contact statement. We allow 
;;; spring-contact to be asserted even when spring is uncompressed so that 
;;; the general equation for spring PE is used even when d=0 -- but could have 
;;; special case to just write Us=0 in this case.
;;; !!! PE-var should include slot for the spring in definition, but this
;;; would block use in write-null-spring-energy below.
(defoperator write-spring-energy (?body ?spring ?t)
  :preconditions (
  (in-wm (spring-contact ?body ?spring ?t-contact ?sforce-dir))
  (test (tinsidep ?t ?t-contact))
  (variable ?PE-var (spring-energy ?body ?spring :time ?t))
  (variable ?k-var  (spring-constant ?spring))
  (variable ?d-var  (compression ?spring :time ?t))
  )
  :effects (
  (eqn (= ?PE-var (* 0.5 ?k-var (^ ?d-var 2)))
       (spring-energy ?body ?spring ?t))
  )

  :hint (
  (point (string "Try writing an equation for the elastic potential energy due to the interaction between ~a and the spring ~a." (?body def-np) (?t pp)))
  (teach (string "The elastic potential energy due to the interaction of a body with a compressed spring is 0.5*k*x^2 where  k is the spring constant and x is the distance the spring is compressed or extended from its equilibrium length."))
  (bottom-out (string "Write ~a" ((= ?PE-var (* 0.5 ?k-var (^ ?d-var 2))) algebra)))
  ))
	 
;;; equation PE_spring = 0 for case where spring in problem but not in contact 
;;; !!! PE-var should include slot for spring, but then quantity can't be 
;;; introduced and stated to be zero if no spring exists or body not in contact
;;; with a spring. In fact this is true in Andes interface.
(defoperator write-null-spring-energy (?b ?spring ?t)
  :preconditions (
		  ;; must be spring-contact at some time in problem:
		  (in-wm (spring-contact ?body ?spring ?sometime ?dontcare))
		  ;; but must NOT be spring-contact at time we are called for
		  (not (spring-contact ?body ?spring ?t-contact ?s-force-dir) 
		       (tinsidep ?t ?t-contact))
		  (variable ?PE-var (spring-energy ?body ?spring :time ?t))
		  )
  :effects (
	    (eqn (= ?PE-var 0) (spring-energy ?b ?spring ?t))
	    )
  :hint (
	 (point (string "Notice that ~A is not in contact with a spring ~A 
that could transfer elastic potential energy to ~A." ?b (?t pp) ?b))
	 (bottom-out (string "Write ~A" ((= ?PE-var 0) algebra)))
	 ))

;;;
;;; NB: Electrostatic potential energy defined in electromagnetism.cl as electric-energy
;;;

;;; ops to define variables for energy quantities:
(defoperator define-total-energy (?b ?t)
  :preconditions (
		  (object ?b)
		  (bind ?TE-var (format-sym "TE_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
		  ) 
  :effects ( 
	    (define-var (total-energy ?b :time ?t))
	       (variable ?TE-var (total-energy ?b :time ?t))
	       )
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Energy."
			     ((total-energy ?b :time ?t) def-np)))
	 ))

(defoperator define-height (?body ?time)
  :preconditions 
  ( (bind ?h-var (format-sym "h_~A~@[_~A~]" (body-name ?body) (time-abbrev ?time))) )
  :effects ( (variable ?h-var (height ?body :time ?time))
	     (define-var (height ?body :time ?time)) )
  :hint (
	 (bottom-out (string "Define a height variable for ~A using the Variables menu on the top menu bar." ?body))
	 ))

(defoperator define-spring-constant (?spring)
  :preconditions ( (bind ?k-var (format-sym "k_~A" ?spring)) ) 
  :effects 
  ( (define-var (spring-constant ?spring))
      (variable ?k-var  (spring-constant ?spring)) )
  :hint (
	 (bottom-out (string "Define a spring constant variable using the Variables menu on the top menu bar."))
	 ))

(defoperator define-compression (?spring ?t)
  :preconditions 
  ( (bind ?d-var (format-sym "comp_~A~@[_~A~]" ?spring (time-abbrev ?t))) ) 
  :effects (
	    (define-var (compression ?spring :time ?t))
	       (variable ?d-var  (compression ?spring :time ?t))
	       )
  :hint (
	 (bottom-out (string "Define a variable for ~A using the Variables menu on the top menu bar."
			     ((compression ?spring :time ?t) def-np)))
	 ))


(defoperator define-extension (?spring ?t)
  :preconditions 
  ( (bind ?d-var (format-sym "comp_~A~@[_~A~]" ?spring (time-abbrev ?t))) ) 
  :effects 
  ( (define-var (extension ?spring :time ?t))
      (variable ?d-var (extension ?spring :time ?t)) )
 :hint (
	(bottom-out (string "Define a variable for ~A using the Variables menu on the top menu bar."
			    ((extension ?spring :time ?t) def-np)))
	))

;;; Change in height is y component of displacement 
;;; We code this as a vector equation so that the projection equation will 
;;; automatically be packed into the method. However, it is not really true
;;; that it is a vector equation which could be projected along x or y axes.
;;; Thus we do not handle this as a vector PSM.
;;;

(defoperator height-dy-contains (?quantity)
  :preconditions 
  (
   (any-member ?quantity ((compo y 0 (displacement ?b :time (during ?t1 ?t2)))
	       (height ?b :time ?t1)
	       (height ?b :time ?t2)
	       ))
   (time (during ?t1 ?t2))
   (time ?t1) (time ?t2) ;sanity test
   )
  :effects 
  (
   (eqn-contains (height-dy ?b (during ?t1 ?t2)) ?quantity)
   ;; post this to make sure we will use standard axes
   (use-energy-axes)
   ))

(defoperator draw-height-dy-diagram (?b ?t)
  :preconditions (
		  (body ?b)
		  (vector ?b (displacement ?b :time ?t) ?dir)
		  (axes-for ?b 0) ;Must use standard axes for this. 
		  )
  :effects ( (vector-diagram 0 (height-dy ?b ?t)) ))

(defoperator write-height-dy (?b ?t1 ?t2)
  :preconditions 
  (
   ;; Draw body and displacement vector
   (vector-diagram ?rot (height-dy ?b ?t))
   (variable ?h2 (height ?b :time ?t2))
   (variable ?h1 (height ?b :time ?t1))
   (variable ?d12_y  (compo y ?rot (displacement ?b :time (during ?t1 ?t2))))
   )
  :effects ( (eqn (= (- ?h2 ?h1) ?d12_y) (height-dy ?b (during ?t1 ?t2))) )
  :hint 
  (
   (point (string "You should relate the change in height of ~A ~A to the displacement during that period." 
		  ?b ((during ?t1 ?t2) pp)))
   (teach (string "The change in height will be equal to the vertical component of the displacement."))
   (bottom-out (string "Write the equation ~A" ((= (- ?h2 ?h1) ?d12_y) algebra)))
   ))


;;;============================================================================
;;; Work
;;;
;;; Note (use-work) is required in problem statement to enable work and
;;; work-energy principles to be applied. This is to suppress generating 
;;; these entries on earlier problems. 
;;; This should be replaced by use of a more general facility to specify which
;;; principles may be used when it is implemented at the bubble graph level
;;;============================================================================
;;; Work is defined in terms of a force, but following the idiom used in verbal
;;; problem descriptions, our quantity specifies work done by *agent* of force.
;;; Note: this requires there to be a unique force done by a given agent, so 
;;; couldn't handle work done by friction and normal force from floor

;;; The "work" scalar equation PSM computes work done by a single force over 
;;; a time interval as F * d * cos(theta) where theta is angle between F and d.
;;; We use a variant operator to write work = 0 for forces known to be 
;;; orthogonal to the displacement. 
;;;
;;; Note: if coordinate axes are drawn this quantity can also be computed 
;;; component-wise as F_x * d_x + F_y * d_y.  We will have to define another 
;;; PSM to calculate the work done by a single force in this way.
;;;
(defoperator work-contains (?sought)
 :preconditions (
    (in-wm (use-work))
    (any-member ?sought (
		  (work ?b ?agent :time ?t)
                  (mag (force ?b ?agent ?type :time ?t))
		  (angle-between orderless
				 (force ?b ?agent ?type :time ?t) 
				 (displacement ?b :time ?t))
		  ;; see inst-power-contains for the correct way to
		  ;; find the displacement and still get the ?agent
                  ;; (mag (displacement ?b :time ?t))
    			))
    (object ?b)
    (time ?t)
    (test (time-intervalp ?t))
    ;; will require that ?agent exerts force on ?body when writing equation
 )
 :effects (
    (eqn-contains (work ?b ?agent ?t NIL) ?sought)
 ))

(defoperator work-compo-contains (?sought)
  :preconditions 
  (
   (in-wm (use-work))	 
   (any-member ?sought ((work ?b ?agent :time ?t)
			(compo ?xyz ?rot (force ?b ?agent ?type :time ?t))
			;; see inst-power-contains for the correct way to
			;; find the displacement and still get the ?agent
			;; (mag (displacement ?b :time ?t))
			))
   ;; find axes now, before applying dot product:
   (vector ?b (force ?b ?agent ?type :time ?t) ?dir-f)
   (vector ?b (displacement ?b :time ?t) ?dir-d)
   (time ?t)
   ;; If ?rot is unbound, draw-rotate-axes or draw-standard-axes
   ;; etc. will choose the angle.  If it is bound from the ?sought,
   ;; operator will also succeed.
   (axes-for ?b ?rot) 
   (test (time-intervalp ?t))
   ;; will require that ?agent exerts force on ?body when writing equation
   )
  :effects (
	    (eqn-contains (work ?b ?agent ?t ?rot) ?sought)
	    (assume axes-for ?b ?rot)
 ))

;; This can write either the component or the angle form of the work equation,
;; depending on ?rot. Work is done in dot product operators.
(defoperator write-work (?b ?agent ?t ?rot)
 :preconditions (
    ;; !!! could be more than one force from agent, e.g. normal and friction
    ;; from floor.  This should be fixed by adding type slot to work argument.
    ;; Until then, just ignore normal force if there's more than one, since
    ;; it does not contribute to the work done by this agent. Leave it if it's
    ;; the only one in frictionless problems so we can write Wa = 0.
    (setof (force ?b ?agent ?type1 ?t ?dir1 ?action) 
	   ?type1 ?agent-force-types)
    (debug "write-work: agent ~a exerts forces of type ~A~%" 
	   ?agent ?agent-force-types)
    (bind ?type (first (if (not (cdr ?agent-force-types)) ?agent-force-types
                           (remove 'Normal ?agent-force-types))))
    (debug "write-work: choosing force of type ~A for work by ~A~%" 
	   ?type ?agent)
    ;; Formula for work only applies if the force is constant. 
    ;; That is true of almost all forces in Andes problems except the force 
    ;; from a compressed spring and gravitational (not weight) force.
    ;; 
    ;; We assume all other forces are constant over the interval.  
    ;; It would be better to test for this explicitly.
    (test (not (member ?type '(spring gravitation))))
    ;; must draw body, force and displacement vectors
    (body ?b)
    (dot ?dot (force ?b ?agent ?type :time ?t) (displacement ?b :time ?t) 
	 ?rot :nonzero ?nonzero)
    ;; It might make sense to have a seperate operator for the case
    ;; of zero work.  In that case, the displacement and the force can't
    ;; be soughts.  Also, the formula is valid for non-constant forces.
    ;; Different hints for orthogonal vectors
    (bind ?points (if (equal ?dot 0)  
		      "Notice that the force exerted on ~A by ~A ~A is perpendicular to the direction of its displacement."
		    "You need the value of the work done on ~a by ~a ~A"
		    ))
    ;; This is a copy of stuff in the work ontology...
    (bind ?teaches (if (equal ?dot 0)
		       "If a force has no component in the direction of the displacement of an object, then the force does no work on that object."
		     (strcat "The work done on a body by a constant force of magnitude F acting through a displacement of magnitude d is given by "
			     (if ?rot "F_x * d_x + F_y d_y." 
			       "F * d * cos ($q), where $q is the angle between the force and displacement vectors."))
		     ))
    (variable ?work-var (work ?b ?agent :time ?t))
 )
 :effects (
    (eqn (= ?work-var ?dot) (work ?b ?agent ?t ?rot))
    )
 :hint (
  (point (string ?points ?b ?agent (?t pp)))
  (teach (string ?teaches))
  (bottom-out (string "Write ~A"  ((= ?work-var ?nonzero) algebra)))
 ))


;;;
;;; Following defines a variable for the work done by a force agent
;;; over a time interval.
(defoperator define-work (?b ?agent ?t)
 :preconditions (
 (object ?b)
 (time ?t)
 (test (time-intervalp ?t))
 (bind ?work-var (format-sym "work_~A_~A_~A" (body-name ?b) (body-name ?agent) 
 					     (time-abbrev ?t)))
 )
 :effects (
   (define-var (work ?b ?agent :time ?t))
   (variable ?work-var (work ?b ?agent :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting work" ((work ?b ?agent :time ?t) def-np)))
 ))

;; following defines a variable for net-work done on an object due to
;; all forces over a time interval
(defoperator define-net-work (?b ?t)
 :preconditions (
 (object ?b)
 (time ?t)
 (test (time-intervalp ?t))
 (bind ?work-var (format-sym "net_work_~A_~A" (body-name ?b) (time-abbrev ?t)))
 )
 :effects (
   (define-var (net-work ?b :time ?t))
   (variable ?work-var (net-work ?b :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu, selecting work, then choosing work done by all forces." ((net-work ?b :time ?t) def-np)))
 ))


;;
;; Net-work PSM -- compute net work as the sum of the work done by
;; each force on the object. 
;;
;; The most natural method would be to write the equation 
;;           Work_net = workF1 + workF2 ... 
;; at the bubble-graph-level and rely on chaining to find the work done by each
;; of the work agents.  However, when we first tried this "chaining" version,
;; it gave rise to combinatorial explosion at the path traversal phase for 
;; *other* problems in which one was not expected to use work. Still, breaking
;; up net work into constituents at the bubble graph level in the manner of the
;; chaining version is necessary for some problems that require use of
;; work-energy to find work done by a single agent. 
;; 
;; We include alternate code for an "all-in-one" version that incorporates
;; subsidiary equations for the individual works done by each force. These 
;; terms are eliminated in the final equation, so the whole solution for 
;; net work occurs within a single PSM. This means we can't simply chain from 
;; net work to find work done by an individual force at the top-level, though 
;; we could find the force magnitude.
;;
;; Either way the net work PSM requires drawing a free body diagram showing 
;; all the forces on an object as for Newton's law problems (but w/o accel).
;; Forces orthogonal to the displacement contribute nothing to 
;; the net work done so could in principle be left out of the computation 
;; entirely. For now we leave the forces in the diagram to teach the general 
;; method. The work terms wind up set to zero for these forces; only
;; the zero values occur in the final equation for net work in the all-in-one 
;; version.
;;
;; One could also compute net work by first computing the net force vector
;; and then computing net force dot displacement.  This was not done in the
;; CLIPS solutions and we avoid that for now since we are discouraging 
;; introduction of net force terms where not needed.

;; begin chaining net-work method

(defoperator net-work-contains (?sought)
  :preconditions 
  ((any-member ?sought  ((net-work ?b :time ?t) 
	                 (work ?b ?agent :time ?t)))
   ;; Can't collect (does-work-on ..) if some work agents are unknown;
   ;; (unknown-forces) means that a work agent is neither specified
   ;; as a force, nor specified via (does-work-on ...).
  (not (unknown-forces :time ?t ?t)) ;also tested in draw-forces
  (test (time-consecutivep ?t)))  ;only apply to consecutive times
  :effects 
  ((eqn-contains (net-work ?b ?t) ?sought)))

(defoperator apply-net-work (?b ?t)
  :preconditions (
   ; !!! can't draw forces from agents specified only as power sources
   ; draw free-body diagram showing all forces on object
   (net-work-diagram ?b ?t) 
   ; introduce net-work variable
   (variable ?net-work-var (net-work ?b :time ?t))
   ; introduce variables for work done by each work source. 
   ; need to collect list of force *agents* to use in work quantities
   ; agent can be one we know exerts a force on body, but it can also be
   ; a power-source we are told is transferring energy to the body, where
   ; we might not have detailed information about the mechanism so can not
   ; find or draw a force.
   ; !!! this is bad if there are both friction and normal forces from a
   ; surface.  Cheap workaround is to suppress normal force in the problem if
   ; friction is present, but that is ugly. For now we list agents
   ; get the ind work operator to write expr for the non-orthogonal force 
   ; when it is asked to write work done by agent.
   (in-wm (any-forces ?b ?t ?forces))
   (setof (in-wm (does-work-on ?work-agent ?b :time ?t ?t))
          ?work-agent ?other-agents)
   (bind ?agents (remove-duplicates (append (mapcar #'third ?forces) 
                                            ?other-agents)))
   (map ?agent ?agents
      (variable ?work-var (work ?b ?agent :time ?t))
      ?work-var ?work-vars) 
  )
  :effects (
   (eqn (= ?net-work-var (+ . ?work-vars)) (net-work ?b ?t))
  )
  :hint (
    (teach (string "The net work done on an object is the sum of the work done on that object by each individual force or energy source acting on it."))
    (bottom-out (string "Write the equation ~A" ((= ?net-work-var (+ . ?work-vars)) algebra)))
  ))

;; end chaining version of net-work method

(defoperator draw-net-work-diagram (?b ?t)
  :specifications "choose body and draw all forces on it"
  :preconditions 
   ((body ?b)
    ;; make sure axis is allowed, even if unused
    (axes-for ?b 0) 
    (any-forces ?b ?t ?forces) 
    )
   :effects ( (net-work-diagram ?b ?t) ))

;;;
;;; work-energy PSM -- net-work = change in kinetic energy
;;;
;;; In almost all Andes problems where net work can be computed the forces are
;;; constant so an equivalent solution using Newton's law + constant 
;;; acceleration kinematics can also be found.  This principle will also 
;;; apply in cases where conservation of mechanical energy can be used, 
;;; although it is intended for use with a non-conservative force.
;;;
(defoperator work-energy-contains (?sought)
 :preconditions 
  ((in-wm (use-work))
   (any-member ?sought ( 
             (net-work ?b :time (during ?t1 ?t2)) 
	     (kinetic-energy ?b :time ?t1)
	     (kinetic-energy ?b :time ?t2)
	     ))
  (time (during ?t1 ?t2)))
 :effects 
  ((eqn-contains (work-energy ?b (during ?t1 ?t2)) ?sought)))

;;; Write work = delta ke without writing out values for the ke terms.
(defoperator write-work-energy (?b ?t1 ?t2)
  :preconditions 
   ( ; draw body and standard axes for principle
    (body ?b)
    (axes-for ?b 0)
    (variable ?Wnet-var (net-work ?b :time (during ?t1 ?t2)))
    (variable ?ke1-var (kinetic-energy ?b :time ?t1))
    (variable ?ke2-var (kinetic-energy ?b :time ?t2)))
  :effects 
  (
   (eqn (= ?Wnet-var (- ?ke2-var ?ke1-var)) (work-energy ?b (during ?t1 ?t2)))
   (assume using-energy-conservation work-energy ?b ?t1 ?t2)
   )
  :hint (
   (point (string "What do you know about the relation between net work done on an object and its kinetic energy?" ))
   (teach (string "The work-energy principle states that the net work done on an object by all forces over an interval is equal to the change in its kinetic energy over that interval"))
  (bottom-out (string "Write the equation ~A" ((= ?Wnet-var (- ?ke2-var ?ke1-var)) algebra)))  ))


;; Change in mechanical energy: Wnc = ME2 - ME1
;;
;; This is a generalization of conservation of mechanical energy that is
;; applicable when non-conservative forces act on the object. This form
;; therefore subsumes conservation of energy as a special case when there
;; are no non-conservative forces. For now, we allow either form to be used in
;; those cases.

(defoperator change-ME-contains (?sought)
  :preconditions (
     (in-wm (use-work))
     (any-member ?sought ((total-energy ?b :time ?t)
                           (work-nc ?b :time (during ?t1 ?t2)) ))
     (time ?t)
     ;; Need to declare interval in problem statement
     (time (during ?t1 ?t2))
     (any-member ?t (?t1 ?t2))
  )
  :effects (
    (eqn-contains (change-ME ?b ?t1 ?t2) ?sought)
  ))

;;; following writes the top-level change in ME equation,
;;;       Wnc = ME2 - ME1
(defoperator write-change-ME (?b ?t1 ?t2)
  :preconditions 
  (
   (body ?b)
   (variable ?Wnc (work-nc ?b :time (during ?t1 ?t2)))
   (variable ?ME1 (total-energy ?b :time ?t1))
   (variable ?ME2 (total-energy ?b :time ?t2))
   )
  :effects (
    (eqn (= ?Wnc (- ?ME2 ?ME1)) (change-ME ?b ?t1 ?t2))
    (assume using-energy-conservation change-me ?b ?t1 ?t2)
  )
  :hint (
   (point (string "Think about what you can conclude about the total mechanical energy in the system throughout this problem."))
  (teach (string "The most general form of conservation of energy states that the work done by non-conservative forces over an interval is equal to the change in total mechanical energy over that interval."))
   (bottom-out (string "Write the equation ~A" 
                       ((= ?Wnc (- ?ME2 ?ME1)) algebra)))
  ))

;;;
;;; Wnc = Wf1 + Wf2 + ... where f1, f2, ... are non-conservative
;;;
(defoperator Wnc-contains (?sought)
 :preconditions(
  (in-wm (use-work))
  ; !! this won't find Wnc at wider interval when sought is work
  ; during smaller interval.
  (any-member ?sought ( (work ?body ?agent :time ?t)
                        (work-nc ?body :time ?t) ))
  ;; Can't collect (nc-work-during ..) if some work agents are unknown;
  ;; (unknown-forces) means that a work agent is neither specified
  ;; as a force, nor specified via (does-work-on ...).
  ;; can't collect (nc-work-during ...) if unknown forces
  (not (unknown-forces :time ?t ?t))  
  )
 :effects (
  (eqn-contains (Wnc ?body ?t) ?sought)
 ))

(defoperator write-Wnc (?body ?t)
  :preconditions (
    ;; draw body and standard axes for principle
    (body ?body)
    (axes-for ?body 0)
    (variable ?Wnc (work-nc ?body :time ?t))
   ;; introduce variables for work done by each non-conservative work source. 
   ;; need to collect list of force *agents* to use in work quantities
   ;; agent can be one we know exerts a force on body, but it can also be
   ;; a power-source we are told is transferring energy to the body, where
   ;; we might not have detailed information about the mechanism so can not
   ;; find or draw a force.
   (setof (nc-work-during (work ?b ?agent :time ?t-work) ?t) 
          (work ?b ?agent :time ?t-work) ?work-quants)
   (debug "collect nc-work-during gives:  ~S~%" ?work-quants)
   (map ?work-quant ?work-quants
      (variable ?work-var ?work-quant)
      ?work-var ?work-vars)
   (bind ?rhs (format-plus ?work-vars))
  ) 
  :effects (
    (eqn (= ?Wnc ?rhs) (Wnc ?body ?t))
  ) 
  :hint (
   (point (string "You need to identify all the non-conservative forces that do work in this problem."))
   (teach (string "In Andes problems, the conservative or path-independent forces are gravity and spring forces; all other forces acting on a system are non-conservative and should be included in Wnc since they change the total mechanical energy of the system."))
   (bottom-out
    (string "Write the equation ~A" ( (= ?Wnc ?rhs) algebra)))
  ))


;;; Difficult to collect all work terms with different times inside a large 
;;; interval, because our (force...) proposition will find forces at any time 
;;; within a desired interval.  Eg: if tension force given to exist between 
;;; 1 and 3, then the force statement will be found for 1 to 2, 2 to 3, 
;;; and 1 to 3 (and time point 2).  We only want the largest interval we can 
;;; find.  Presumably, the problem givens specify the force with its largest 
;;; possible time. So we want to fetch force times as
;;; they are given, not derive a "force" proposition with unbound time.

;;; The following returns an agent of a non-conservative force on b during t
;;; via the nc-work-during proposition. Note that an agent like the floor 
;;; may exert both normal and friction forces on object; we need to ignore
;;; the normal force and use the friction force. 
(defoperator get-nc-force-agent (?b ?agent ?t-force) ;don't use twice if same (body agent force-time)
  :preconditions (
      (force-given-at ?b ?agent ?type ?t-force ?dir1 ?action)
      ;; force given at time NIL holds over whole problem. 
      ;; Translate to ?t-query here
      (bind ?t-overlap (if ?t-force (tintersect2 ?t-force ?t-query)
                          ?t-query))
      (test (time-intervalp ?t-overlap))
      (test (not (member ?type 
			 '(weight gravitational normal spring electric))))
  )
  :effects ( (nc-work-during (work ?b ?agent :time ?t-overlap) ?t-query) ))

;;; if an entity is declared as a power source transmitting energy to ?b,
;;; without details of the force, then it is also an agent of Wnc
(defoperator get-nc-force-agent2 (?b ?agent ?t-work)
  :preconditions 
  (
   (in-wm (does-work-on ?agent ?b :time ?t-work))
   (bind ?t-overlap (tintersect2 ?t-work ?t-query))
   (test (time-intervalp ?t-overlap))
   )
   :effects (
	     (nc-work-during (work ?b ?agent :time ?t-overlap) ?t-query)
	     ))

(defoperator define-Wnc (?b ?t)
  :preconditions (
    (bind ?work-var (format-sym "Wnc_~A_~A" (body-name ?b) (time-abbrev ?t)))
  )
 :effects (
   (define-var (work-nc ?b :time ?t))
   (variable ?work-var (work-nc ?b :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu, selecting work, then defining work done by all non-conservative forces." ((work-nc ?b :time ?t) def-np)))
))

;;;
;;; average power = work/time
;;;
;;; Power is the rate of work done by some agent on some body
;;; We can consider instantaneous power at a time point or average power over
;;; a time interval.
;;; We might also need net-power to correspond to net-work, though in all our
;;; power problems there is a single power source, so we don't have much
;;; need to sum up several.
;;; Note in some cases we may be given that the agent is a source of energy 
;;; transfer without being told the details of the mechanism, so we could not
;;; draw the force and compute the work done by the force, but might do it
;;; from the given power output.
(defoperator power-contains (?sought)
  :preconditions (
    (any-member ?sought ( (work ?b ?agent :time ?t)
			  ;; if sought is duration, need to bind ?body and 
			  ;; ?agent -- a nuisance, since agents aren't always 
			  ;; declared objects. For now, just don't allow it 
                          ;; (duration ?t)
			  (power ?b ?agent :time ?t)))
  )
  :effects (
    (eqn-contains (power ?b ?agent ?t) ?sought)
  ))

(defoperator write-power (?agent ?b ?t)
  :preconditions (
     (body ?b)
     (axes-for ?b 0)
     (variable ?P-var  (power ?b ?agent :time ?t))
     (variable ?W-var  (work ?b ?agent :time ?t))
     (variable ?t-var  (duration ?t))
  )
  :effects (
    (eqn (= ?W-var (* ?P-var ?t-var)) (power ?b ?agent ?t))
  )
  :hint (
   (teach (string "Power is the rate at which work is done. The average power supplied by a force over an interval is therefore the work done by that force over the interval divided by the time."))
   (bottom-out (string "Write the equation ~A" 
                       ((= ?P-var (/ ?W-var ?t-var)) algebra)))
  ))

(defoperator define-power-var (?b ?agent ?t)
 :preconditions (
 (bind ?power-var (format-sym "power_~A_~A~@[_~A~]" (body-name ?b) 
			      (body-name ?agent) (time-abbrev ?t)))
 )
 :effects (
   (define-var (power ?b ?agent :time ?t))
   (variable ?power-var (power ?b ?agent :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting power" ((power ?b ?agent :time ?t) def-np) ))
 ))

;;;
;;; same as above for net-power = Wnet/t
;;;
(defoperator net-power-contains (?sought)
  :preconditions (
    (any-member ?sought ( (net-power ?b :time ?t)
                          (net-work ?b :time ?t)
			  ;; for now don't use to find duration:
			  ;; (duration ?t)
			  ))
  )
  :effects (
    (eqn-contains (net-power ?b ?t) ?sought)
  ))

(defoperator write-net-power (?b ?t)
  :preconditions (
     (body ?b)
     (axes-for ?b 0)
     (variable ?P-var  (net-power ?b :time ?t))
     (variable ?W-var  (net-work ?b :time ?t))
     (variable ?t-var  (duration ?t))
  )
  :effects (
    (eqn (= ?P-var (/ ?W-var ?t-var)) (net-power ?b ?t))
  )
  :hint (
   (teach (string "Power is the rate at which work is done. The average net power supplied over an interval is therefore the net work done by all forces over the interval divided by the time."))
   (bottom-out (string "Write the equation ~A" 
                       ((= ?P-var (/ ?W-var ?t-var)) algebra)))
  ))

(defoperator define-net-power-var (?b ?t)
 :preconditions (
 (bind ?power-var (format-sym "Pnet_~A~@[_~A~]" (body-name ?b) 
			      (time-abbrev ?t)))
 )
 :effects (
   (define-var (net-power ?b :time ?t))
   (variable ?power-var (net-power ?b :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu, selecting power, and defining power supplied by all forces" ((net-power ?b :time ?t) def-np) ))
   ))

;;;
;;;  Net power produced by some source
;;;

(defoperator define-net-power-out (?source  ?t)
  :preconditions
  ((bind ?p-var (format-sym "power_~A~@[_~A~]" 
			      (body-name ?source) 
			      (time-abbrev ?t))))
  :effects ((variable ?p-var (net-power-out ?source  :time ?t))
	    (define-var (net-power-out ?source :time ?t)))
  :hint ((bottom-out 
	  (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting power."  
		  ((net-power-out ?source :time ?t) def-np)))))

;;;
;;; instantaneous power = F dot v = F*v*cos(theta)
;;;
;;; This operator exactly parallels work, with velocity instead of 
;;; displacement.
;;;

(defoperator inst-power-contains (?sought)
 :preconditions 
 (
  (in-wm (use-work))
  (any-member ?sought (
		       (power ?b ?agent :time ?t)
		       (mag (force ?b ?agent ?type :time ?t))
		       (mag (velocity ?b :time ?t))
		       (angle-between orderless 
				      (force ?b ?agent ?type :time ?t)
				      (velocity ?b :time ?t))
		       ))
  ;; can be timeless
  (test (or (null ?t) (time-pointp ?t)))
  ;; get list of force agents we can use
  (setof (force ?b ?agent1 ?type1 ?t ?dir1 ?action) 
	 ?agent1 ?force-agents)
  ;; select a force agent in case sought is velocity, else verify agent
  (any-member ?agent ?force-agents)
  )
 :effects (
	   (eqn-contains (inst-power ?b ?agent ?t NIL) ?sought)
	   ))

(defoperator inst-power-compo-contains (?sought)
  :preconditions 
  (
   (in-wm (use-work))	 
   (any-member ?sought (
			(power ?b ?agent :time ?t)
			(compo ?xyz ?rot (force ?b ?agent ?type :time ?t))
			(compo ?xyz ?rot (velocity ?b :time ?t))
			))
    ;; find axes now, before applying dot product:
    (vector ?b (force ?b ?agent ?type :time ?t) ?dir-f)
    (vector ?b (velocity ?b :time ?t) ?dir-v)
    ;; If ?rot is unbound, draw-rotate-axes or draw-standard-axes
    ;; etc. will choose the angle.  If it is bound from the ?sought,
    ;; operator will also succeed.
    (axes-for ?b ?rot) 
    ;; can be timeless
    (test (or (null ?t) (time-pointp ?t)))
    ;; will require that ?agent exerts force on ?body when writing equation
    ;; get list of force agents we can use
    (setof (force ?b ?agent1 ?type1 ?t ?dir1 ?action) 
	   ?agent1 ?force-agents)
    ;; select a force agent in case sought is velocity, else verify agent
    (any-member ?agent ?force-agents)
)
 :effects (
	   (eqn-contains (inst-power ?b ?agent ?t ?rot) ?sought)
           (assume axes-for ?b ?rot)
 ))

	  
(defoperator write-inst-power (?b ?agent ?t ?rot)
 :preconditions (
    ;; !!! could be more than one force from agent, e.g. normal and friction
    ;; from floor.  This should be fixed by adding type slot to work argument.
    ;; Until then, just ignore normal force if there's more than one, since
    ;; it does not contribute to the work done by this agent. Leave it if it's
    ;; the only one in frictionless problems so we can write Wa = 0.
    (setof (force ?b ?agent ?type1 ?t ?dir1 ?action) 
	   ?type1 ?agent-force-types)
    (bind ?type (first (if (not (cdr ?agent-force-types)) ?agent-force-types
                           (remove 'Normal ?agent-force-types))))
    ;; must draw body, force and velocity vectors
    ;; Without a proper dot product, there is no point in drawing axes.
    (body ?b)
    (dot ?dot (force ?b ?agent ?type :time ?t) (velocity ?b :time ?t) 
	 ?rot :nonzero ?nonzero)
    ;; Different hints for orthogonal vectors
    (bind ?teaches (if (equal ?dot 0)
		       "If a force has no component in the direction of the movement of an object, then the force does no work on that object."
		     (strcat "Power is the rate at which work is done.  The instantaneous power supplied from a force F to a body moving at velocity v is equal to " 
			     (if ?rot "F_x * v_x + F_y v_y." 
			       "F * v * cos ($q), where $q is the angle between the force and velocity vectors."))
		     ))
    
    (variable ?P-var (power ?b ?agent :time ?t))
 )
 :effects (
    (eqn (= ?P-var ?dot) (inst-power ?b ?agent ?t ?rot))
 )
 :hint (
  (teach (string ?teaches))
  (bottom-out (string "Write the equation ~A" ((= ?P-var ?nonzero) algebra)))
 ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; if we are given that a collision is perfectly elastic, we may have to use 
;;; the fact that kinetic energy is conserved in addition to momentum.
;;; The following operator applies conservation of kinetic energy to perfectly
;;; elastic collisions. This is slightly different than the more general 
;;; conservation of energy equation so we group it under linear momentum.
;;; It is a separate scalar equation PSM (not part of the cons linmom PSM)
;;; so it will only be applied if needed to determine some unknown and goes
;;; at the bubble-graph level.
;;;
(defoperator cons-ke-elastic-contains (?quantity)
  :preconditions 
  (
   ;; For elastic collision, we need need the time of the collision
   ;; to be contained within the time we measure the asymptotic states.
   (collision (orderless . ?bodies) (during ?t1 ?t2) :type elastic)
   (any-member ?quantity (
			  (kinetic-energy ?b :time ?t1)
			  (kinetic-energy ?b :time ?t2)
                	  ))
   (test (member ?b ?bodies :test #'equal))
   (time (during ?t1 ?t2)) ;sanity test
  )
  :effects (
    (eqn-contains (cons-ke-elastic ?bodies (during ?t1 ?t2)) ?quantity)
  ))

;; Since the solver doesn't know how to solve this type of system 
;; reliably.  In this case, supply an explicit equation to the solver
;; for the case of 1-dimensional 2-body elastic collisions.
;;
;; This also means that the solver probably will
;; fail when attempting to solve the student's equations.
;; Special help is given in that case; see Help/NextStepHelp.cl.
(defoperator solver-eqn-for-1-d-elastic-collision (?b1 ?b2 ?t1 ?t2)
  :preconditions (
		  (1-d-elastic ?b1 ?b2 (during ?t1 ?t2))
		  (test (member ?b1 ?bodies))
		  (test (member ?b2 ?bodies))
		  (variable ?m1 (mass ?b1))
		  (variable ?m2 (mass ?b2))
		  (variable ?vi (compo x 0 (velocity ?b1 :time ?t1)))
		  (variable ?vvi (compo x 0 (velocity ?b2 :time ?t1)))
		  (variable ?vf (compo x 0 (velocity ?b1 :time ?t2))))
  :effects ((solver-eqn (= ?vf (+ (* (/ (- ?m1 ?m2) (+ ?m1 ?m2)) ?vi)
				    (* (/ (* 2 ?m2) (+ ?m1 ?m2)) ?vvi)))
			  (elastic-collision ?bodies (during ?t1 ?t2)))))

;; Help solver with a two body elastic collision, equal masses, where
;; one body starts at rest.
(defoperator solver-eqn-for-stationary-equal-mass-elastic-collision 
  (?b1 ?b2 ?t1 ?t2)
  :preconditions (
		  ;; ?b2 is starting at rest
		  ;; ?b2-angle is the angle between the initial velocity
		  ;;     of ?b1 and the final velocity of ?b2
		  (stationary-equal-mass-elastic ?b1 ?b2 ?b2-angle 
						 (during ?t1 ?t2))
		  (test (member ?b1 ?bodies))
		  (test (member ?b2 ?bodies))
		  (variable ?magv1 (mag (velocity ?b1 :time ?t2)))
		  (variable ?magv2 (mag (velocity ?b2 :time ?t2)))
		  )
  :effects ((solver-eqn (= ?magv1 (* ?magv2 (tan ?b2-angle)))
			  (elastic-collision ?bodies (during ?t1 ?t2)))))

(defoperator write-cons-ke-elastic (?bodies ?t1 ?t2)
  :preconditions 
  (
   (map ?b ?bodies (variable ?ke1-var (kinetic-energy ?b :time ?t1))
	?ke1-var ?ke1-terms)
   (map ?b ?bodies (variable ?ke2-var (kinetic-energy ?b :time ?t2))
	?ke2-var ?ke2-terms)
   ;; When appropriate, send some extra equations along to help the solver.
   (setof (solver-eqn ?ans-eqn (elastic-collision ?bodies (during ?t1 ?t2))) 
	  nil ?dont-care)
  )
  :effects (
	    ;; final equation sets sum of ke's equal
	    (eqn (= (+ . ?ke1-terms) (+ . ?ke2-terms)) 
		 (cons-ke-elastic ?bodies (during ?t1 ?t2)))
	    )
  :hint (
	 (point (string "Notice that the collision is elastic."))
	 (teach (string "An elastic collision is one in which kinetic energy is conserved. You can use this fact to equate the kinetic energy in the system before and after the collision."))
	 (bottom-out (string "Write the equation ~A"  
			     ((= (+ . ?ke1-terms) (+ . ?ke2-terms)) algebra)))
	 ))

