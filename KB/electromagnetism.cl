;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                Electric/Magnetic Forces and Fields
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          Coulomb's law
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; naturally, this is basically a copy of ug (Newton's law of gravity)
;;; It is enabled by a statement
;;;   (coulomb ?body1 ?body2 ...)
;;; to mean there is a Coulomb interaction between any pair of bodies
;;; in the list.  We normally treat the r as the
;;; magnitude of the relative position vector from the center of the body 
;;; exerting the force to the center of the body experiencing the force.
;;;
(def-psmclass coulomb (coulomb ?body ?agent ?time)
  :complexity major 
  :english ("Coulomb's Law")
  :expformat ("applying Coulombs's Law for the force on ~a due to ~a" (nlg ?body) (nlg ?agent))
  :EqnFormat ("F = kelec*abs(q1)*abs(q2)/r^2"))


;;; The equation is scalar equation containing vector magnitudes only.
(defoperator coulomb-contains (?sought)
   :preconditions (
     ;; first make sure a coulomb interaction exists in problem
     (coulomb-bodies . ?coul-bodies)
     (any-member ?sought 
		 (
		  ;; because of the abs(q1)*abs(q2), don't include charge
		  (mag (force ?b1 ?b2 electric :time ?t))
		  (mag (relative-position ?b1 ?b2 :time ?t))
		  ))
     (object ?b1)
     (object ?b2)
     (time ?t)
     (test (member ?b1 ?coul-bodies))
     (test (member ?b2 ?coul-bodies))
   )
   :effects (
    (eqn-contains (coulomb ?b1 ?b2 ?t) ?sought)
   ))

(defoperator write-coulomb (?b1 ?b2 ?t) 
  :preconditions 
  (
   (body ?b1)
   (variable ?q1 (charge-on ?b1 :time ?t ?t))
   (variable ?q2 (charge-on ?b2 :time ?t ?t))
   (variable ?r  (mag (relative-position ?b1 ?b2 :time ?t)))
   (variable ?F  (mag (force ?b1 ?b2 electric :time ?t)))
   )
  :effects 
  ;; kelec is predefined, see file constants.cl
  ( (eqn (= ?F (/ (* |kelec| (abs ?q1) (abs ?q2)) (^ ?r 2)))
	 (coulomb ?b1 ?b2 ?t) ))
  :hint (
     (teach (string "Coulombs's Law states that electrostatic force between two charges is proportional to the charges of the bodies divided by the square of the distance between the bodies."))
     (bottom-out (string "Write the equation ~A" 
			 ((= ?F (/ (* |kelec| (abs ?q1) (abs ?q2))) 
			     (^ ?r 2))) algebra))
  ))

(def-psmclass coulomb-compo (?eqn-type coulomb-force ?axis ?rot 
				 (coulomb ?body ?agent ?time))
    :complexity major    
    :Doc "Definition of Coulomb's law, component form."
    :english ("the definition of Coulomb's law (component form)") 
    :ExpFormat ("applying Coulomb's law to ~a and ~A ~a"
		(nlg ?body) (nlg ?agent) (nlg ?time 'pp))
    :EqnFormat ("F_~A = kelec*q1*q2/r^2 r_~A/r" 
		(nlg ?axis 'adj) (nlg ?axis 'adj)))

(defoperator coulomb-vector-contains (?sought)
  :preconditions 
  (
   ;; first make sure a coulomb interaction exists in problem
   (coulomb-bodies . ?coul-bodies)
   (any-member ?sought
	       (;; if sought is charge, can use either equation for force
		;; on b1 from b2 or force on b2 from b1, so need both:
		(charge-on ?b1 :time ?t ?t)
		(charge-on ?b2 :time ?t ?t)
		(compo ?xy ?rot (relative-position ?b1 ?b2 :time ?t))
		(compo ?xy ?rot (force ?b1 ?b2 electric :time ?t)))
	       )
   (object ?b1)
   (object ?b2)
   (time ?t)
   (test (member ?b1 ?coul-bodies))
   (test (member ?b2 ?coul-bodies))
   )
  :effects 
   ((eqn-family-contains (coulomb ?b1 ?b2 ?t) ?sought)
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (coulomb ?b1 ?b2 ?t) coulomb-force ?sought)))

(defoperator write-coulomb-compo (?b ?agent ?t1 ?t2 ?xy ?rot)
  :preconditions 
   ((variable ?F12_x  (compo ?xy ?rot (force ?b ?agent ?dont-care)
			  :time (during ?t1 ?t2)))
    (variable ?J12_x  (compo ?xy ?rot (coulomb ?b ?agent)
			  :time (during ?t1 ?t2)))
    (variable ?t12    (duration (during ?t1 ?t2))))
  :effects (
   (eqn (= ?J12_x (* ?F12_x ?t12))
            (compo-eqn coulomb-force ?xy ?rot 
		       (coulomb ?b ?agent (during ?t1 ?t2))))
   (eqn-compos (compo-eqn coulomb-force ?xy ?rot 
		       (coulomb ?b ?agent (during ?t1 ?t2)))
             (?J12_x ?F12_x)))
  :hint 
  ( (point (string "What is the relationship between average force, coulomb and duration?"))
    (teach (string "The coulomb vector is defined as the average force vector times the duration.  This can be applied component-wise."))
    (bottom-out (string "Write the equation ~a"
			((= ?J12_x (* ?F12_x ?t12)) algebra)))
  ))



;;; We have two main principles for electric fields:
;;; charge-force-Efield: vector equation E_x = F_x/q   (equiv:  F_x = q*E_x)
;;; which is definition of E-field.  This is a vector principle written
;;; in component form. We also have scalar variants for magnitude only.
;;;
;;; point-charge-Efield: E-field at a distance r from a single point charge 
;;; derived from definition + Coulomb's Law:  E = k*q/r^2.  We have a vector
;;; form for components in terms of thetar, as well as a magnitude only 
;;; scalar form.
;;;
;;; TODO: K&M's original problems all had only a single body.  
;;; So the situations had to be either of only two kinds: 
;;;     (1) a "region" w/field of unspecified source plus a "test" charge 
;;;         feeling the force; OR, 
;;;     (2) a point-charge source creating a field at an unoccupied point. 
;;; Some rules exploit this dichotomy, testing (at-place ?b ?loc ?t) to 
;;; distinguish situation in a relevant way.  But this does not generalize, 
;;; so has to be changed. 
;;;
;;; The question of what bodies to draw, and which bodies to use in the 
;;; vector statement (as axis-owner) is still a nuisance.

;;; Drawing E-field and E-force vectors:
;;;
;;; Because E-field and E-force directions are related, we have several ways 
;;; we can draw an E-field vector, depending on what is given:
;;;  - From given E-field direction: draw-Efield-vector
;;;  - From given E-field components w/grid: draw-vector-given-compos 
;;;  - From E-force direction:
;;;     - If given dir E-force on charge and charge sign: 
;;;       draw-Efield-given-force-dir[-pos|-neg]
;;;     - If given compos E-force on charge, charge sign and grid: 
;;;       draw-Efield-from-force-compos[-pos|-neg]  
;;;  - From given of position wrt point-particle 
;;;  - From given components of position wrt point-particle & grid 
;;; If all else fails: 
;;;  Unknown -- if know that field exists but no direction determinable 
;;;             (e.g. it's sought)
;;;
;;; Exactly corresponding set applies to drawing E-force vector:  dir can 
;;; be given (directly or via compos) or can be derived from given E-field 
;;; direction (directly or via compos), can be derived
;;; from configuration wrt point particle, or else unknown.

(defoperator draw-Efield-vector (?b ?loc ?source ?t)
 :specifications " "
 :preconditions 
 ((rdebug "Using draw-Efield-vector  ~%")
  (time ?t)
  ;; ?b is "test charge" feeling force at loc
  ;; it is only used as axis owner for vector
  ;; !!! what if we're given field at point with no body?
  (at-place ?b ?loc ?t)
  (given (dir (field ?loc electric ?source :time ?t)) ?dir)  
  (not (vector ?b (field ?loc electric ?source :time ?t) ?dir))     
  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) 
			     (body-name ?source) (time-abbrev ?t)))
  (bind ?dir-var (format-sym "O~A" ?mag-var))
  (rdebug "fired draw-Efield-vector   ~%")
  )
 :effects (
	   (vector ?b (field ?loc electric ?source :time ?t) ?dir)
	   (variable ?mag-var (mag (field ?loc electric ?source :time ?t)))
	   (variable ?dir-var (dir (field ?loc electric ?source :time ?t)))
	   ;; Because dir is problem given, find-by-psm won't ensure implicit 
	   ;; eqn gets written.  Given value may not be used elsewhere so 
	   ;; ensure it here.
	   (implicit-eqn (= ?dir-var ?dir) (dir (field ?loc electric ?source :time ?t)))
            )  
 :hint (
	(point (string "You were given the direction of the electric field at ~a due to ~a." ?loc (?source agent)))
         (bottom-out (string "Use the electric field drawing tool (labeled E) to draw the electric field at ~a due to ~a in the given direction of ~A." 
			     ?loc (?source agent) ?dir))
         ))

;; pull out the sign of a given charge
(defoperator get-sign-given-charge (?b)
  :preconditions ((in-wm (given (charge-on ?b) (dnum ?val ?units)))
                  (bind ?pos-neg (if (> ?val 0) 'pos 'neg)))
  :effects ((sign-charge ?b ?pos-neg)))

;; Can draw field vector if E force dir is given directly
(defoperator draw-Efield-given-force-dir (?b ?t)
   :preconditions 
   ((rdebug "Using draw-Efield-given-force-dir ~%")
    ;; make sure direction of force on ?b is given
    (given (dir (force ?b ?source electric :time ?t)) ?F-dir)
    ;; make sure field direction at loc of b not given, directly or via components:
    (at-place ?b ?loc ?t)
    (not (given (dir (field ?loc electric ?source :time ?t)) ?dontcare1))
    (not (given (compo x 0 (field ?loc electric ?source :time ?t)) ?dontcare2))
    ;; require sign of charge to be given
    (sign-charge ?b ?pos-neg)
    (bind ?Field-dir (if (eq ?pos-neg 'pos) ?F-dir (opposite ?F-dir)))
    (bind ?same-or-opposite  (if (eq ?pos-neg 'pos) 'same 'opposite))
    (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?b) (body-name ?source)
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (rdebug "fired draw-Efield-given-force-dir  ~%")
    )
   :effects 
   (
    (vector ?b (field ?loc electric ?source :time ?t) ?Field-dir)
    (variable ?mag-var (mag (field ?loc electric ?source :time ?t))) 
    (variable ?dir-var (dir (field ?loc electric ?source :time ?t))) 
    (given (dir (field ?loc electric ?source :time ?t)) ?Field-dir)
    )
   :hint (
	  (point (string "Think about how the direction of the electric force at ~a due to ~a is related to the direction of the electric field vector at ~a" ?loc (?source agent) ?loc))
	  (teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
	  (bottom-out (string "Because the charge of ~a is ~a,  use the electric field drawing tool (labeled E) to draw the electric field vector at ~a due to ~a in the ~a direction as the electric force that ~A undergoes, namely ~A." 
			      ?b (?pos-neg adj) ?loc (?source agent) 
			      (?same-or-opposite adj) ?b (?field-dir adj)))
	  ))

;; Can draw Efield vector if E force dir is given by components, 
;; and grid is used
(defoperator draw-Efield-given-force-compos-pos (?b ?source ?t)
  :preconditions 
  (
   ;; only use for component form problems with drawing grid
   (component-form)
   (vector-grid)
   ;; require sign of charge to be positive
   (sign-charge ?b pos)
   ;; make sure E-force compos given
   (given (compo x 0  (force ?b ?source electric :time ?t)) (dnum ?xc ?units))
   (given (compo y 0 (force ?b ?source electric :time ?t)) (dnum ?yc ?units))
   ;; make sure field direction not given at loc of ?b, directly or via components:
   (at-place ?b ?loc ?t)
   (not (given (dir (field ?loc electric ?source :time ?t)) ?dontcare1))
   (not (given (compo x 0 (field ?loc electric ?source :time ?t)) ?dontcare2))
   ;; similar to draw-vector-given-compos
   (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?b) (body-name ?source) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir (dir-from-compos ?xc ?yc)) 
   )
  :effects (
            (vector ?b (field ?loc electric ?source :time ?t) ?dir)
            (variable ?mag-var (mag (field ?loc electric ?source :time ?t))) 
            (variable ?dir-var (dir (field ?loc electric ?source :time ?t))) 
            ;; Don't put out equation for thetaV since value is not exact, 
	    ;; could lead to errors if given to algebraic solver with other 
	    ;; equations.
            ;; (given (dir (field ?loc electric ?source :time ?t)) (dnum ?Field-dir |deg|))
            )
  :hint (
	 (point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
	(bottom-out (string "Because the charge of ~a is positive, and you were given the horizontal and vertical components of the force vector, draw the electric field at the location of ~a in the same direction as the force by choosing a scale, then counting ~a units horizontally and ~a vertically as you draw." ?b ?xc ?yc))
	))

(defoperator draw-Efield-given-force-compos-neg (?b ?source ?t)
   :preconditions 
   (
    ;; only use for component form problems with drawing grid
    (component-form)
    (vector-grid)
    ;; require sign of charge to be positive
    (sign-charge ?b neg)
    ;; make sure E-force compos given
    (given (compo x 0  (force ?b ?source electric :time ?t)) (dnum ?xc ?units))
    (given (compo y 0 (force ?b ?source electric :time ?t)) (dnum ?yc ?units))
    ;; make sure field direction not given at loc of ?b, directly or via components:
    (at-place ?b ?loc ?t)
    (not (given (dir (field ?loc electric ?source :time ?t)) ?dontcare1))
    (not (given (compo x 0 (field ?loc electric ?source :time ?t)) ?dontcare2))
    ;; similar to draw-vector-given-compos
    (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?b) (body-name ?source)
			       (time-abbrev ?t)))
    (bind ?dir (opposite (dir-from-compos ?xc ?yc)))
    )
   :effects 
   (
    (vector ?b (field ?loc electric ?source :time ?t) ?dir)
    (variable ?mag-var (mag (field ?loc electric ?source :time ?t))) 
    (variable ?dir-var (dir (field ?loc electric ?source :time ?t))) 
    ;; Don't put out equation for thetaV since value is not exact, could 
    ;; lead to errors if given to algebraic solver with other equations.
    ;;(given (dir (field ?loc electric ?source :time ?t)) (dnum ?Field-dir |deg|))
    )
   :hint (
(point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is negative, and you were given the horizontal and vertical components of the force vector, draw the electric field at the location of ~a in the opposite direction from the force by choosing a scale, then counting -~a units horizontally and -~a vertically as you draw." ?b ?xc ?yc))
         ))

;; Draw field vector if given that unknown E field exists
;;      given by (E-field source) in problem. 
;; Don't include this field dir determinable by other ways

;; NB: ?b is only needed as axis-owner of drawn vector. 
;; It is "test charge" experiencing force at ?loc
(defoperator draw-region-Efield-unknown (?b ?loc ?source ?t)
  :specifications " "
  :preconditions 
  ((rdebug "Using draw-region-Efield-unknown ~%")
   (time ?t)
   (E-field ?source) ; means that unknown E-field exists
   ;; Must be given there is a body at ?loc. ?? Seems to be used to
   ;; indicate this is region Efield type problem -- should change.
   ;; what if we're asked about field at an unoccupied point?
   (at-place ?b ?loc ?t)
   ;; make sure source not at loc of test charge
   ;; (not (at-place ?source ?loc ?t))
   (test (time-pointp ?t))
   (not (vector ?dontcare (field ?loc electric ?source :time ?t) ?dir))
   ;; make sure field direction not given, directly 
   (not (given (dir (field ?loc electric ?source :time ?t)) ?dontcare3))
   ;; take out following when we change from grid to drawing unknown
   ;; (not (given (compo x 0 (field ?loc electric ?source :time ?t)) ?dontcare4))
   ;; !!! Should also make sure direction of E-force not given, directly or via components.
   ;; Would be given as electric force on object for an object at-place loc.
   (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) (body-name ?source)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-region-Efield-unknown  ~%")
   )
  :effects (
            (vector ?b (field ?loc electric ?source :time ?t) unknown)
            (variable ?mag-var (mag (field ?loc electric ?source :time ?t)))
	    (variable ?dir-var (dir (field ?loc electric ?source :time ?t)))
            )
  :hint (
         (point (string "You know there is an electric field at ~A." ?loc))
         (teach (string "In this problem the exact direction of the electric field vector requires calculation to determine, so you can draw the vector at an approximately angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the electric field at ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?loc (?source agent)))
          ))


;; draw point charge Efield at loc if dir from source to loc is given
(defoperator draw-point-Efield-given-relpos-dir (?b ?loc ?t)
  :preconditions 
  (
   ;; Make sure source is point-charge
   (point-charge ?b)
   (test (time-pointp ?t))
   (not (given (dir (field ?loc electric ?b :time ?t)) ?dontcare3))
   (in-wm (given (dir (relative-position ?loc ?b :time ?t)) ?rdir))
   (sign-charge ?b ?pos-neg)
   (bind ?Field-dir (if (eq ?pos-neg 'pos) ?rdir (opposite ?rdir)))
   (bind ?same-or-opposite (if (eq ?pos-neg 'pos) 'same 'opposite))
   (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) (body-name ?b) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
            (vector ?b (field ?loc electric ?b :time ?t) ?Field-dir)
            (variable ?mag-var (mag (field ?loc electric ?b :time ?t))) 
            (variable ?dir-var (dir (field ?loc electric ?b :time ?t))) 
            (given (dir (field ?loc electric ?b :time ?t)) ?Field-dir)
  )
  :hint (
        (point (string "Because ~A is charged, it creates an electric field at ~A." ?b ?loc))
        (teach (string "The direction of the electric field due to a point charge is radial away from a positive charge and toward a negative charge."))
        (bottom-out (string "Because the charge of ~a is ~a and the line from ~a to ~a is oriented at ~a, draw the electric field at ~a due to ~a in the ~a direction, namely ~a." 
			    ?b (?pos-neg adj) ?b ?loc ?rdir ?loc (?b agent) 
			    (?same-or-opposite adj) ?Field-dir))
  ))


;; NB: ?b is only needed as axis-owner of drawn vector. 
;; It is normally charged particle that is source of field
(defoperator draw-point-Efield-unknown (?b ?loc ?t)
  :preconditions 
  ((rdebug "Using draw-point-Efield-unknown ~%")
   (time ?t)
   (E-field ?b)
   ;; Do we need this anymore?
   ;;(at-place ?b ?loc-source ?t)
   (test (time-pointp ?t))
   ;; Make sure source is point-charge
   (point-charge ?b)
   ;; make sure ?loc not equals ?loc-source?
   (not (vector ?dontcare (field ?loc electric ?b :time ?t) ?dir))
   (not (given (dir (field ?loc electric ?b :time ?t)) ?dontcare3))
   (not (given (dir (relative-position ?loc ?b :time ?t)) (dnum ?rdir |deg|)))
   (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) (body-name ?b)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "Fired draw-point-Efield-unknown  ~%")
   )
  :effects (
            (vector ?b (field ?loc electric ?b :time ?t) unknown)
            (variable ?mag-var (mag (field ?loc electric ?b :time ?t)))
            (variable ?dir-var (dir (field ?loc electric ?b :time ?t)))
            )
  :hint (
        (point (string "Because ~A is charged, it creates an electric field at ~A." ?b ?loc))
        (teach (string "The direction of the electric field due to a point charge is radial away from a positive charge and toward a negative charge.  In this problem the exact direction of the electric field vector requires calculation to determine, so you can draw the vector at an approximately correct angle and leave the exact angle unspecified."))
        (bottom-out (string "Draw the electric field at ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?loc (?b agent)))
          ))

;;
;; Drawing E force vector -- parallels drawing E field vector
;;
;; !!! TODO -- should factor out proposition that electric force exists that 
;; fires without drawing vector, as we do for other forces in Newton's 
;; Law problems.  (See force-finding rules in Newtons2.cl for examples). 
;; This won't matter until we have mechanics problems with electric forces
;; and have to reason about all forces in the problem (e.g. whether they are 
;; all conservative) without drawing vectors.

;; - if given E force vector dir --

;; make sure force exists apart from drawing
(defoperator find-electric-force (?b ?agent ?t)
  :preconditions (
		  
    (time ?t)
    (in-wm (given (dir (force ?b ?agent electric :time ?t-force)) ?dir-expr))
    (test (tinsidep ?t ?t-force))
    ;; check that something else hasn't defined this force.
    (not (force ?b ?agent electric ?t . ?dont-care)) 
  )
  :effects (
    (force ?b ?agent electric ?t ?dir-expr action)
    (force-given-at ?b ?agent electric ?t-force ?dir-expr action)
  ))


(defoperator find-electric-force-unknown (?b ?agent ?t)
  :preconditions (
	(coulomb-bodies . ?bodies)	  
	(time ?t)
	(object ?b)
	(object ?agent)
	(not (given (dir (force ?b1 ?b2 electric :time ?t-force)) ?dir-expr)
	     (and (member ?b1 (list ?b ?agent)) (member ?b2 (list ?b ?agent)) 
		  (tinsidep ?t ?t-force)))
	(test (member ?b ?bodies :test #'equal))
	(test (member ?agent ?bodies :test #'equal))
	;; check that something else hasn't defined this force.
	(not (force ?b ?agent electric ?t . ?dont-care)) 
  )
  :effects (
    (force ?b ?agent electric ?t unknown action)
    (force-given-at ?b ?agent electric ?t unknown action)
  ))

 
(defoperator draw-Eforce-given-dir (?b ?source ?t)
  :preconditions 
  ((rdebug "Using draw-Eforce-given-dir ~%")
   (force ?b ?source electric ?t ?dir ?dontcare)
   (test (not (equal ?dir 'unknown)))
   (not (vector ?b (force ?b ?source electric :time ?t) ?whatever))
   (bind ?mag-var (format-sym "Fe_~A_~A~@[_~A~]" (body-name ?b) 
			      (body-name ?source) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-Eforce-given-dir  ~%")
   )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) ?dir)
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
	    ;; Because dir is problem given, find-by-psm won't ensure implicit 
	    ;; eqn gets written.  Given value may not be used elsewhere so 
	    ;; ensure it here.
            (implicit-eqn (= ?dir-var ?dir) 
			  (dir (force ?b ?source electric :time ?t)))
            )
  :hint (
    (point (string "You were given that there is an electric force on ~a." ?b))
    (bottom-out (string "Use the force drawing tool to draw the electric force on ~a due to ~a ~a at ~a." ?b (?source agent) (?t pp) ?dir))
))


(defoperator draw-Eforce-unknown (?b ?source ?t)
  :preconditions 
  ((rdebug "Using draw-Eforce-given-dir ~%")
   (force ?b ?source electric ?t unknown ?dontcare)
   (not (vector ?b (force ?b ?source electric :time ?t) ?whatever))
   (bind ?mag-var (format-sym "Fe_~A_~A~@[_~A~]" (body-name ?b) 
			      (body-name ?source) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-Eforce-given-dir  ~%")
   )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) unknown)
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
            )
  :hint (
    (point (string "You were given that there is an electric force on ~a." ?b))
    (bottom-out (string "Use the force drawing tool to draw the electric force on ~a due to ~a ~a, direction unknown." ?b (?source agent) (?t pp)))
))

;; if given E field vector dir
;;    - directly
(defoperator draw-Eforce-given-field-dir (?b ?source ?t)
   :preconditions 
   ((rdebug "Using draw-Eforce-given-field-dir ~%")
    ;; make sure E-field direction given at loc of ?b
    (in-wm (given (dir (field ?loc electric ?source :time ?t)) (dnum ?field-dir |deg|)))
    ;; make sure force direction not given, directly or via components:
    (not (given (dir (force ?b ?source electric :time ?t)) ?dontcare1))
    (not (given (compo x 0 (force ?b ?source electric :time ?t)) ?dontcare2))
    ;; require sign of charge to be given
    (sign-charge ?b ?pos-neg)
    (bind ?F-dir (if (eq ?pos-neg 'pos) ?field-dir (opposite ?field-dir)))
    (bind ?same-or-opposite (if (eq ?pos-neg 'pos) 'same 'opposite))
    (bind ?mag-var (format-sym "Fe_~A_~A$~A" (body-name ?b) (body-name ?source)
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (rdebug "fired draw-Eforce-given-field-dir  ~%")
    )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) (dnum ?F-dir |deg|))
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
            (given (dir (force ?b ?source electric :time ?t)) (dnum ?F-dir |deg|))
            )
  :hint (
        (point (string "Think about how the direction of the electric force at ~a due to ~a is related to the direction of the electric field vector at ~a" ?loc (?source agent) ?loc))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is ~a, use the force drawing tool (labeled F) to draw the electric force on ~a due to ~a in the ~a direction as the electric field at that location, namely ~A." 
	 ?b (?pos-neg adj) ?b (?source agent) (?same-or-opposite adj) 
	 (?F-dir adj)))
         ))

;; if given E field vector dir
;;    - by components, with grid
;; Similar to generic draw-vector-given-compos, but requires sign to be taken 
;; into account.

(defoperator draw-Eforce-given-field-compos-pos (?b ?source ?t)
   :preconditions 
   ((rdebug "Using draw-Eforce-given-field-compos-pos ~%")
    ;; only use for component form problems with drawing grid
    (component-form)
    (vector-grid)
    ;; require sign of charge to be positive
    (sign-charge ?b pos)
    ;; make sure E-field compos given at loc of ?b
    (given (compo x 0 (field ?loc electric ?source :time ?t)) (dnum ?xc ?units))
    (given (compo y 0 (field ?loc electric ?source :time ?t)) (dnum ?yc ?units))
    ;; make sure force direction not given, directly or via components:
    (not (given (dir (force ?b ?source electric :time ?t)) ?dontcare1))
    (not (given (compo x 0 (force ?b ?source electric :time ?t)) ?dontcare2))
    ;; similar to draw-vector-given-compos
    (bind ?mag-var (format-sym "Fe_~A_~A$~A" (body-name ?b) (body-name ?source)
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir (dir-from-compos ?xc ?yc))
    (rdebug "fired draw-Eforce-given-field-compos-pos  ~%")
    )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) ?dir)
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
            ;; Don't put out equation for thetaV since value is not exact, could 
            ;; lead to errors if given to algebraic solver with other equations.
            ; (given (dir (force ?b ?source electric :time ?t)) ?dir)
            )
  :hint (
          (point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is positive, and you were given the horizontal and vertical components of the electric field due to ~a, draw the electric force on ~a in the same direction as the field at its location by choosing a scale, then counting ~a units horizontally and ~a vertically as you draw." ?b (?source agent) ?b ?xc ?yc))
         ))

(defoperator draw-Eforce-given-field-compos-neg (?b ?source ?t)
   :preconditions 
   ((rdebug "Using draw-Eforce-given-field-compos-pos ~%")
    ;; only use for component form problems with drawing grid
    (component-form)
    (vector-grid)
    ;; require sign of charge to be negative
    (sign-charge ?b neg)
    ;; make sure E-field compos given at loc of ?b
    (given (compo x 0 (field ?loc electric ?source :time ?t)) (dnum ?xc ?units))
    (given (compo y 0 (field ?loc electric ?source :time ?t)) (dnum ?yc ?units))
    ;; make sure force direction not given, directly or via components:
    (not (given (dir (force ?b ?source electric :time ?t)) ?dontcare1))
    (not (given (compo x 0 (force ?b ?source electric :time ?t)) ?dontcare2))
    ;; similar to draw-vector-given-compos
    (bind ?mag-var (format-sym "Fe_~A_~A$~A" (body-name ?b) (body-name ?source)
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir (opposite (dir-from-compos ?xc ?yc)))
    (rdebug "fired draw-Eforce-given-field-compos-pos  ~%")
    )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) ?dir)
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
            ;; Don't put out equation for thetaV since value is not exact, could 
            ;; lead to errors if given to algebraic solver with other equations.
            ; (given (dir (force ?b ?source electric :time ?t)) (dnum ?dir |deg|)))
            )
  :hint (
(point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is negative, and you were given the horizontal and vertical components of the electric field due to ~a, draw the electric force on ~a in the opposite direction from the field at its location by choosing a scale, then counting -~a units horizontally and -~a vertically as you draw." ?b (?source agent) ?b ?xc ?yc))
         ))

;;  -if given that unknown field exists 
;;      given by (E-field source) or (B-field source) in problem. 
;;      Don't use these if field direction given in other ways
(defoperator draw-Eforce-unknown (?b ?source ?t)
  :specifications " "
  :preconditions 
  ((rdebug "Using draw-Eforce-unknown ~%")
   (time ?t)
   (E-field ?source) 
   (test (time-pointp ?t))
   (not (vector ?b (force ?b ?source electric :time ?t) ?dir))
   ;; make sure force direction not given, directly or via components:
   (not (given (dir (force ?b ?source electric :time ?t)) ?dontcare1))
   ;; take out when we change to unknown:
   ;; (not (given (compo x 0 (force ?b ?source electric :time ?t)) ?dontcare2))
   ;; make sure E-field direction not given, directly or via components
   (at-place ?b ?loc ?t)
   (not (given (dir (field ?loc electric ?source :time ?t)) ?dontcare3))
   ;; (not (given (compo x 0 (field ?loc electric ?source :time ?t)) ?dontcare4))
   (bind ?mag-var (format-sym "Fe_~A_~A$~A" (body-name ?b) (body-name ?source)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-Eforce-unknown  ~%")
   )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) unknown)
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
            )
  :hint (
         (point (string "Since ~a is charged and in an electric field, it is subject to an electric force." ?b))
         (teach (string "In this problem the exact direction of the electric force vector requires calculation to determine, so you can draw the force vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the electric force on ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?b (?source agent)))
         ))

;;-------------------------------
;; Charge-force-Efield Vector PSM
;;-------------------------------

(def-psmclass charge-force-Efield 
             (?eq-type qfe ?axis ?rot (charge-force-Efield ?body ?source ?time)) 
  :complexity major
  :english ("the definition of electric field")
  :ExpFormat ("applying the definition of electric field on ~a ~a"
		 (nlg ?body) (nlg ?time 'pp) )
  :EqnFormat ("F_~a = q * E_~a" (nlg ?axis 'adj) (nlg ?axis 'adj)))

(defoperator charge-force-Efield-contains (?sought)
  :preconditions 
  ((rdebug "Using charge-force-Efield-contains  ~%")
   (any-member ?sought((mag (force ?b ?source electric :time ?t))
		       (dir (force ?b ?source electric :time ?t))
		       (mag (field ?loc electric ?source :time ?t))
		       (dir (field ?loc electric ?source :time ?t))
		       ))
   ;; make sure ?b (test-charge) is bound in case sought is field at loc
   (at-place ?b ?loc ?t)
   (rdebug "Firing charge-force-Efield-contains  ~%")
   )
  :effects (
            (eqn-family-contains (charge-force-Efield ?b ?source ?t) ?sought)
            ;; since only one compo-eqn under this vector psm, we can just
            ;; select it now, rather than requiring further operators to do so
            (compo-eqn-contains (charge-force-Efield ?b ?source ?t) qfe ?sought)))

;; special case when sought is charge: need to choose a field source to bind
(defoperator charge-force-Efield-contains-charge (?sought)
  :preconditions 
  ((rdebug "Using charge-force-Efield-contains-charge  ~%")
   (any-member ?sought ( (charge-on ?b :time ?t ?t) ))
   (at-place ?b ?loc ?t)
   ;; following will fetch the source of an E-field at loc if we are given
   ;; its direction or component value
   (source-of-Efield ?loc ?t ?source)
   (rdebug "Firing charge-force-Efield-contains-charge  ~%")
   )
  :effects 
  (
   (eqn-family-contains (charge-force-Efield ?b ?source ?t) ?sought)
   ;; since only one compo-eqn under this vector psm, we can just
   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (charge-force-Efield ?b ?source ?t) qfe ?sought)))

(defoperator get-source-from-given-field-dir (?loc ?t ?source)
  :preconditions 
  ((in-wm (given (dir (field ?loc electric ?source :time ?t)) ?value)))
  :effects ((source-of-Efield ?loc ?t ?source)))

(defoperator get-source-from-given-field-compo (?loc ?t ?source)
  :preconditions 
  ((in-wm (given (compo x 0 (field ?loc electric ?source :time ?t)) ?value)))
  :effects ((source-of-Efield ?loc ?t ?source)))

(defoperator draw-charge-force-Efield-diagram (?b ?source ?t)
  :preconditions 
  (
   (debug "Using draw-charge-force-Efield-diagram ~%")
   (not (vector-diagram (charge-force-Efield ?b ?source ?t)))
   ;; ?b is "test charge" feeling force at ?loc 
   (body ?b)
   (at-place ?b ?loc ?t)
   ;; need source of field
   (vector ?dontcare (field ?loc electric ?source :time ?t) ?dir1) 
   (vector ?b (force ?b ?source electric :time ?t) ?dir2)
   (axis-for ?b x ?rot)
   (rdebug "Fired draw-charge-force-Efield-diagram ~%")
   )
  :effects (
            (vector-diagram (charge-force-Efield ?b ?source ?t))
            ))

(defoperator write-charge-force-Efield-compo (?b ?t ?xy ?rot)
  :preconditions ((debug "Using write-charge-force-Efield-compo ~%")
                  (at-place ?b ?loc ?t)
                  (variable ?E_x  (compo ?xy ?rot (field ?loc electric ?source :time ?t)))
                  (variable ?F_x  (compo ?xy ?rot (force ?b ?source electric :time ?t)))
                  (variable ?q (charge-on ?b :time ?t ?t))
                  ;(sign-charge ?b ?pos-neg)
                  ;(bind ?sign (if (equal ?pos-neg 'neg) '- '+))
                  (rdebug "fired write-charge-force-Efield-compo  ~%")
                  )
  :effects (
            (eqn (= ?F_x (* ?q ?E_x))
                 (compo-eqn qfe ?xy ?rot (charge-force-Efield ?b ?source ?t)))
            (eqn-compos (compo-eqn qfe ?xy ?rot (charge-force-Efield ?b ?source ?t))
                        (?F_x ?E_x))
            )
  :hint (
         (point (string "What is the relationship between the force, the charge and the electric field?"))
         (teach ;(kcd "write-charge-force-Efield-compo")
                 (string "The electric field vector at a location is defined as electric force vector per unit charge. That is, the electric force vector that a test charge would undergo if placed there, divided by the size of the charge. This definition can be applied component-wise to relate force and field components"))
         (bottom-out (string "Write the equation ~a" ((= ?F_x (* ?q ?E_x)) algebra)))
         ))

;; Vector relation F = q*E also licences magnitude and direction scalar 
;; equations.  Simpler to find these quantities than using component equations 
;; plus projections (though should be able to use either method).
;; We have to write these out as separate scalar principles.
;; Similarly for B-field equations.

(def-psmclass charge-force-Efield-mag 
             (charge-force-Efield-mag ?body ?source ?time) 
  :complexity major
  :english ("the definition of electric field magnitude")
  :ExpFormat ("applying the definition of electric field magnitude at ~a ~a"
		 (nlg ?body) (nlg ?time 'pp) )
  :EqnFormat ("F = abs(q) * E" ))

(defoperator charge-force-Efield-mag-contains (?sought)
  :preconditions (
                  (not (component-form))
                  (any-member ?sought ((mag (force ?b ?source electric :time ?t))
                                       (mag (field ?loc electric ?source :time ?t))
				       ; need to choose a source if sought is charge:
				       ; ignore for now
                                       ;(charge-on ?b :time ?t ?t)
				       ))
		  (at-place ?b ?loc ?t)
                  (debug "Using & firing write-charge-force-Efield-mag-contains ~%")
                  )
  :effects(
           (eqn-contains (charge-force-Efield-mag ?b ?source ?t) ?sought)
           ))  

 ; !!! since making charge signed and writing equation with abs, this equation now can't be
 ; inverted to solve for q. Would need to add further equation for sign of q,
 ; e.g. q = -abs(q), or have a way of registering sign constraints with algebra module.  Even then,
 ; bubble-collection algorithm doesn't know a further equation is needed. Might try to put it out here
 ; as a sub-equation, but it is not always needed. Might try as an implicit equation but then it
 ; is optional, and won't be hinted for even when it is needed.
(defoperator write-charge-force-Efield-mag (?b ?t)
  :preconditions ((debug "Using write-charge-force-Efield-mag ~%")
                  (at-place ?b ?loc ?t)
		  ;; must draw body in diagram for this psm
		  (body ?b)
		  ; even though this is scalar equation, want axes to be allowed
		  (axis-for ?b x ?rot)
                  (variable ?magE (mag (field ?loc electric ?source :time ?t)))
                  (variable ?magF (mag (force ?b ?source electric :time ?t)))
                  (variable ?q (charge-on ?b :time ?t ?t))
                  (rdebug "fired write-charge-force-Efield-mag  ~%")
                  )
  :effects (
	    ; NB: need abs charge since it is now signed
            (eqn (= ?magF (* (abs ?q) ?magE)) (charge-force-Efield-mag ?b ?source ?t))
            )
  :hint (
         (point (string "What is the relationship between the force, the charge and the electric field?"))
         (teach ;(kcd "write-charge-force-Efield-mag")
                (string "The electric field vector at a location is defined as electric force vector per unit charge. That is, the electric force vector that a test charge would undergo if placed there, divided by the size of the charge. This definition can be applied to relate force and field magnitudes."))

         (bottom-out (string "Write the equation ~a" ((= ?magF (* (abs ?q) ?magE)) algebra)))
          ))

;;
;; direction equation must be different for positive and negative charge

#|
(def-psmclass charge-force-Efield-dir 
             (charge-force-Efield-dir ?body ?source ?time) 
  :complexity minor 
  :english ("the electric field field direction rule")
  :ExpFormat ("applying the electric field direction rule") 
  :EqnFormat ("$qF = $qE (pos) or $qF = $qE + 180 deg (neg)" ))

(defoperator charge-force-Efield-dir-contains (?sought)
  :preconditions (
                  (any-member ?sought ((dir (force ?b ?source electric :time ?t))
                                       (dir (field ?loc electric ?source :time ?t))
                                       (charge-on ?b :time ?t ?t)))
                  ;(not (component-form))
                  (at-place ?b ?loc ?t)
                  (rdebug "Using & firing write-charge-force-Efield-dir-contains ~%")
                  )
  :effects(
           (eqn-contains (charge-force-Efield-dir ?b ?source ?t) ?sought)
           ))
|#
(defoperator write-charge-force-Efield-dir-pos (?b ?t)
  :preconditions ((debug "Using write-charge-force-Efield-dir ~%")
                  (sign-charge ?b pos)
                  (at-place ?b ?loc ?t)
                  (variable ?dirE (dir (field ?loc electric ?source :time ?t)))
                  (variable ?dirF (dir (force ?b ?source electric :time ?t)))
                  (variable ?q (charge-on ?b :time ?t ?t))
                  (rdebug "fired write-charge-force-Efield-dir  ~%")
                  )
  :effects (
            (eqn (= ?dirF ?dirE) (charge-force-Efield-dir ?b ?source ?t))
            )
  :hint (
         (point (string "What is the relationship between the direction of the force vector and the direction of the electric field vector?"))
         (teach (kcd "write-charge-force-Efield-dir-pos")
                 (string "For a positive charge, the direction of the force vector is the same as the direction of the electric field vector."))
         (bottom-out (string "Write the equation ~a=~a."
			(?dirF algebra) (?dirE algebra)))
          ))

;;;;--------------------------------------------
;;;;                      point-charge-Efield Vector PSM 
;;;;--------------------------------------------

(def-psmclass point-charge-Efield (?eq-type 
				   qpe ?axis ?rot 
				   (point-charge-Efield ?body ?loc ?time)) 
  :complexity major
  :english ("the formula for electric field due to a point charge")
  :ExpFormat ("calculating for electric field at ~A due to ~a"
	      (nlg ?loc) (nlg ?body) )
  :EqnFormat ("E_~a = (kelec * q / r^2 ) * ~a $qr" (nlg ?axis 'adj) (axis-proj-func ?axis)))

(defun axis-proj-func (axis) 
  "map axis label to trig func used to project on that axis"
    (if (eq axis 'y) 'sin 'cos))

(defoperator point-charge-Efield-contains (?sought)
  :preconditions ((rdebug "Using point-charge-Efield-compo-contains  ~%")
		  (component-form)
                  (any-member ?sought((mag (field ?loc electric ?b :time ?t))
                                      (dir (field ?loc electric ?b :time ?t))
                                      (charge-on ?b :time ?t ?t) 
				      (mag (relative-position ?loc ?b :time ?t))
				      (dir (relative-position ?loc ?b :time ?t))
                                      ))
		  (point-charge ?b)
                  ;(at-place ?b ?loc-source ?t)
                  (rdebug "Firing point-charge-Efield-compo-contains  ~%")
                  )
  :effects (
            (eqn-family-contains (point-charge-Efield ?b ?loc ?t) ?sought)
             ;; since only one compo-eqn under this vector psm, we can just
             ;; select it now, her than requiring further operators to do so
            (compo-eqn-contains (point-charge-Efield ?b ?loc ?t) qpe ?sought)))

(defoperator draw-point-charge-Efield-diagram (?b ?loc ?t)
  :preconditions (
                  (rdebug "Using draw-point-charge-Efield-diagram ~%")
                  (not (vector-diagram (point-charge-Efield ?b ?loc ?t)))
                  ;; ?b is point charge source of field at ?loc
                  (body ?b)
		  ; do we need this?
                  ;(at-place ?b ?loc2 ?t)
                  (vector ?dontcare (field ?loc electric ?b :time ?t) ?dir1) 
                  (axis-for ?b x ?rot)
                  (rdebug "Fired draw-point-charge-Efield-diagram ~%")
                  )
  :effects (
            (vector-diagram (point-charge-Efield ?b ?loc ?t))
            )
  :hint (
         (point (string "Try drawing a diagram."))
         (teach (string "The diagram should show the electric field vector at ~a." ?loc))
         (bottom-out (string "Draw a diagram showing the electric field at point ~a due to the charge on the ~a." ?loc ?b))
          ))


; "macro" expansion: translate coords as given to relative position vector components, if needed.
; !!! This doesn't seem to work to allow the sgg to detect the components as givens in the
; problem, must investigate.
(defoperator coords-to-relpos ()
 :preconditions ( (given (pos ?loc :time ?t) (?value1 ?value2)) )
 :effects ( (given (compo x 0 (relative-position ?loc origin :time ?t)) ?value1)
            (given (compo y 0 (relative-position ?loc origin :time ?t)) ?value2) ))


; This is equation for the component of field, so includes a sort of projection.
(defoperator write-point-charge-Efield-compo (?b ?loc ?t ?xy ?rot)
   :preconditions (
       (rdebug "Using write-point-charge-Efield-compo ~%")
       ; b is point-charge source of field
       ;(at-place ?b ?loc-source ?t)
       (variable ?E_x  (compo ?xy ?rot (field ?loc electric ?b :time ?t)))
       (variable ?q    (charge-on ?b :time ?t ?t))
       (variable ?r    (mag (relative-position ?loc ?b :time ?t)))
       (variable ?theta_r (dir (relative-position ?loc ?b :time ?t)))
       (bind ?sin-or-cos (if (eq ?xy 'x) 'cos 'sin))
       ; how to handle k_e ?
       (rdebug "fired write-point-charge-Efield-compo  ~%")
    )
    :effects (
            (eqn (= ?E_x (* (/ (* |kelec| ?q) (^ ?r 2)) (?sin-or-cos ?theta_r)))
                 (compo-eqn qpe ?xy ?rot (point-charge-Efield ?b ?loc ?t)))
	    ; do we need this? do we use this only in component form?
            (eqn-compos (compo-eqn qpe ?xy ?rot (point-charge-Efield ?b ?loc ?t))
                        (?E_x))
    )
    :hint (
	(teach (string "The electric field due to a point charge is directly proportional to the charge and inversely proportional to the square of the distance from the point charge. The constant of proportionality can be written in Andes as kelec. To compute a component of the electric field, multiply the magnitude by the cos or sin of the angle of the relative position of the location from the point charge, using cos for x components and sin for y components."))
	(bottom-out (string "Write the equation ~A"  
	    ((= ?E_x (* (/ (* |kelec| ?q) (^ ?r 2)) (?sin-or-cos ?theta_r))) algebra)))
    ))

;; mag, dir forms of point-charge-Efield

(def-psmclass point-charge-Efield-mag
             (point-charge-Efield-mag ?body ?loc ?time) 
  :complexity major
  :english ("the formula for the magnitude of the electric field due to a point charge")
  :ExpFormat ("applying the formula for the magnitude of the electric field due to a point charge to ~a ~a"
		 (nlg ?loc) (nlg ?time 'pp) )
  :EqnFormat ("E = kelec * q / r^2" ))

(defoperator point-charge-Efield-mag-contains (?sought)
  :preconditions ((rdebug "Using point-charge-Efield-mag-contains ~%")
                  ;(not (component-form))
                  (any-member ?sought ((mag (field ?loc electric ?b :time ?t))
				       (mag (relative-position ?loc ?loc-source :time ?t))
                                       (charge-on ?b :time ?t ?t)))
                  ;(at-place ?b ?loc-source ?t)
		  (point-charge ?b)
                  (rdebug "Firing point-charge-Efield-mag-contains ~%")
                  )
  :effects(
           (eqn-contains (point-charge-Efield-mag ?b ?loc ?t) ?sought)
           ))

(defoperator write-point-charge-Efield-mag (?b ?loc ?t)
  :preconditions ((debug "Using write-point-charge-Efield-mag ~%")
                  ;;(at-place ?b ?loc-source ?t)
		  ;; need to draw body for this psm. 
		  ;; ?b is point-charge source of field
		  (body ?b)
		  ;; need to allow axes for this scalar psm. 
		  (axis-for ?b x 0) ; use standard axes only
                  (variable ?magE (mag (field ?loc electric ?b :time ?t)))
                  (variable ?q (charge-on ?b :time ?t ?t))
		  (variable ?r (mag (relative-position ?loc ?b :time ?t)))
                  (rdebug "fired write-point-charge-Efield-mag  ~%")
                  )
  :effects (
            (eqn (= ?magE (/ (* |kelec| (abs ?q)) (^ ?r 2) ))
               (point-charge-Efield-mag ?b ?loc ?t))
            )
  :hint (
         (point (string "What is the equation for the magnitude of the electric field due to a point charge?"))
         (teach (kcd "write-point-charge-force-Efield-mag")
                (string "Write the definition of the magnitude of the electric field due to a point charge in terms of the charge and the distance to the point."))
         (bottom-out (string "Write the equation ~A"
	                   ( (= ?magE (/ (* |kelec| (abs ?q)) (^ ?r 2) )) algebra)))
          ))

#| ; this rule really doesn't add anything to projection rules we have
(defoperator point-charge-Efield-dir-contains (?sought)
  :preconditions ((rdebug "Using point-charge-Efield-dir-contains ~%")
                  (any-member ?sought ((dir (field ?loc electric ?b :time ?t))
                                       (charge-on ?b :time ?t ?t)))
                  ;(not (component-form))
                  ;(at-place ?b ?loc2 ?t)
		  (point-charge ?b)
                  (rdebug "Firing point-charge-Efield-dir-contains ~%")
                  )
  :effects(
           (eqn-contains (point-charge-Efield-dir ?b ?loc ?t) ?sought)
           ))
|# ; if this commented out, rule will never be used

(defoperator write-point-charge-Efield-dir-pos (?b ?loc ?t)
  :preconditions ((debug "Using write-point-charge-Efield-dir-pos ~%")
		  ; following was used to distinguish point-charge problem:
		  ; location of source body mentioned in givens
                  ;(at-place ?b ?loc2 ?t)
		  (point-charge ?b)
                  (sign-charge ?b pos)
                  (variable ?dirE (dir (field ?loc electric ?b :time ?t)))
		  (variable ?xpos (compo x 0 (relative-position ?loc ?b :time ?t)))
		  (variable ?ypos (compo y 0 (relative-position ?loc ?b :time ?t)))
                  (rdebug "fired write-point-charge-Efield-dir-pos  ~%")
                  )
  :effects (
	    ; !! this equation doesn't determine quadrant. Would need atan2
            (eqn (= (tan ?dirE) (/ ?ypos ?xpos)) (point-charge-Efield-dir ?b ?loc ?t))
            )
  :hint (
         (point (string "What is the direction of the electric field vector due to a point charge?"))
         (teach (kcd "write-point-charge-Efield-dir-pos")
                 (string "For a positive charge, the electric field is directed radially outward from the point charge."))
         (bottom-out (string "Write the equation ~a= arctan (~a / ~a)."
			(?dirE algebra) (?ypos algebra) (?xpos algebra)))
          ))

(defoperator write-point-charge-Efield-dir-neg (?b ?loc ?t)
  :preconditions ((debug "Using write-point-charge-Efield-dir-pos ~%")
		  ; following was used to distinguish point-charge problem:
		  ; location of source body mentioned in givens
                  ;(at-place ?b ?loc2 ?t)
		  (point-charge ?b)
                  (sign-charge ?b neg)
                  (variable ?dirE (dir (field ?loc electric ?b :time ?t)))
		  (variable ?xpos (compo x 0 (relative-position ?loc ?b :time ?t)))
		  (variable ?ypos (compo y 0 (relative-position ?loc ?b :time ?t)))
                  (rdebug "fired write-point-charge-Efield-dir-neg  ~%")
                  )
  :effects (
	    ; !! this equation doesn't determine quadrant. Would need atan2
            (eqn (= (tan ?dirE) (/ ?ypos ?xpos)) (point-charge-Efield-dir ?b ?loc ?t))
            )
  :hint (
         (point (string "What is the direction of the electric field vector due to a point charge?"))
         (teach (kcd "write-point-charge-Efield-dir-neg")
                 (string "For a negative charge, the electric field is directed radially inward from the point charge."))
         (bottom-out (string "Write the equation tan ~a= (~a / ~a) ."
			(?dirE algebra) (?ypos algebra) (?xpos algebra)))
          ))


;; This derives sign on charge of ?b when Fe_x and E_x are given at location 
;; of ?b x-component can't be zero
(defoperator get-sign-charge-from-FE-compos (?b ?t ?xyz ?rot)
  :specifications " "
  :preconditions ((rdebug "sign-on-charge ~%")
                  ;(at-place ?b ?loc ?t)                    
                  (given (compo ?xyz ?rot (force ?b ?source electric :time ?t)) (dnum ?val1 |N|))
                  (given (compo ?xyz ?rot (field ?loc electric ?source :time ?t)) (dnum ?val2 |N/C|))
		  (test (not (or (= ?val1 0) (= ?val2 0))))
                  (bind ?sign (if (> (* ?val1 ?val2) 0) 'pos 'neg))
                  (rdebug "sign-on-charge~%")
                  )
  :effects (
            (sign-charge ?b ?sign) 
            ))


;; Scalar variable definitions:

(defoperator define-constant-charge-on-obj-var (?p)
  :preconditions 
  (
   (rdebug "Using define-charge-on-obj-var ~%")
   (not (changing-voltage))
   (object ?p)
   (bind ?q-var (format-sym "Q_~A" (body-name ?p)))
   (not (circuit-component ?p capacitor))
   (rdebug "fired define-charge-on-obj-var ~%")
   )
  :effects (
            (variable ?q-var (charge-on ?p))
	    (define-var (charge-on ?p))
            )
   :hint (
       (bottom-out (string "Define a variable for the charge on ~A by using the Add Variable command on the Variable menu and selecting Charge."  ?p ))
       ))

#|  ;; not used anywhere yet
(defoperator define-changing-charge-on-obj-var (?p ?t)
  :preconditions 
  (
   (rdebug "Using define-charge-on-obj-var ~%")
   (in-wm (changing-voltage))
   (object ?p)
   (time ?t)
   (bind ?q-var (format-sym "Q_~A~@[_~A~]" (body-name ?p) (time-abbrev ?t)))
   (not (circuit-component ?p capacitor))
   (rdebug "fired define-charge-on-obj-var ~%")
   )
  :effects (
            (variable ?q-var (charge-on ?p :time ?t))
	    (define-var (charge-on ?p :time ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for the charge on ~A by using the Add Variable command on the Variable menu and selecting Charge."  ?p ))
       ))
|#

(defoperator define-potential-var (?loc ?source ?t)
  :preconditions 
  (
   (bind ?V-var (format-sym "V_~A_~A_~A" ?loc ?source (time-abbrev ?t)))
   )
  :effects (
            (variable ?V-var (potential ?loc ?source :time ?t))
            (define-var (potential ?loc ?source :time ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for the potential at ~A due to ~a by using the Add Variable command on the Variable menu and selecting Potential."  ?loc (?source agent)))
       ))

(defoperator define-net-potential-var (?loc ?t)
  :preconditions (
                  (bind ?V-var (format-sym "Vnet_~A_~A" ?loc (time-abbrev ?t)))
                  )
  :effects (
            (variable ?V-var (net-potential ?loc :time ?t))
            (define-var (net-potential ?loc :time ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for the net potential at ~A from all sources by using the Add Variable command on the Variable menu and selecting Potential."  ?loc))
       ))

;;--------------------------------------------------
;; Superposition principle for electric fields:
;;   Enet_x = E1_x + E2_x + ...
;;--------------------------------------------------

; We might try writing a generic version that will work for both electric and 
; magnetic fields.  However, we might need a different way of enumerating all 
; sources of fields to consider for each. So for now, restrict to electric.

(def-psmclass net-Efield (?eq-type Enet ?axis ?rot (net-Efield ?loc ?time))
  :complexity major
  :english ("the definition of net electric field")
  :ExpFormat ("calculating the net electric field at ~a ~a" (nlg ?loc) (nlg ?time 'pp))
  :EqnFormat ("Enet_~a = E1_~a + E2_~a + E3_~a + ..." (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))

(defoperator net-Efield-contains (?sought)
 :preconditions (
  (any-member ?sought (
		 (mag (net-field ?loc electric :time ?t))
		 (dir (net-field ?loc electric :time ?t))
                 ; need to choose ?loc to apply at when sought is field due 
		 ; to some source.  Ignore this case for now.
		 ;(mag (field ?loc electric ?source :time ?t))
		 ;(dir (field ?loc electric ?source :time ?t))
		 ))
  ; Must make sure don't include source at loc. We will filter for this
  ; when we write the equation.
  )
  :effects (
   (eqn-family-contains (net-Efield ?loc ?t) ?sought)
  ; since only one compo-eqn under this vector psm, we can just
  ; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (net-Efield ?loc ?t) Enet ?sought)
  ))

#|
(defoperator get-Efield-sources (?loc ?t)
  :preconditions (
  )
  :effects (
  )
)
|#

(defoperator draw-net-Efield-diagram (?loc ?t)
 :preconditions (
    ; draw body? which? use the point?
    (Efield-sources ?loc ?t ?sources)
    (foreach ?source ?sources
       (vector ?b (field ?loc electric ?source :time ?t) ?dir)) 
    ; which body should own the axis to use for these vectors
    (axis-for ?loc ?xy ?rot)
 )
 :effects (
    (vector-diagram (net-Efield ?loc ?t))
 ))

(defoperator write-net-Efield-compo (?loc ?t ?xy ?rot)
 :preconditions (
   (variable ?Enet_x (compo ?xy ?rot (net-field ?loc electric :time ?t)))
   (in-wm (Efield-sources ?loc ?t ?sources))
   (map ?source ?sources 
   	(variable ?compo-var (compo ?xy ?rot (field ?loc electric ?source :time ?t)))
	?compo-var ?Ei_x)
  )
  :effects (
    (eqn (= ?Enet_x (+ . ?Ei_x))
                 (compo-eqn Enet ?xy ?rot (net-Efield ?loc ?t)))
     (eqn-compos (compo-eqn Enet ?xy ?rot (net-Efield ?loc ?t))
                        (?Enet_x . ?Ei_x))
  )
  :hint (
    (point (string "The net electric field at a point can be computed from the fields set up at that point by each of the field sources."))
    (teach (string "The principle of superposition states that the net electric field at a point is the vector sum of the electric fields due to each of the individual sources. This relation can be applied component-wise to calculate the components of the net electric field due to all sources."))
    (bottom-out (string "Write the equation ~A" 
			((= ?Enet_x (+ . ?Ei_x)) algebra) ))
  ))

; drawing net fields
(defoperator draw-net-Efield-unknown (?loc ?t)
 :preconditions (
  ; make sure field exists -- for now, test Efield-sources
  ; presume the direction is unknown -- not given.
  (not (given (dir (net-field ?loc electric :time ?t)) ?val))
  ; could make sure there is more than one source of an Efield.
   (in-wm (Efield-sources ?loc ?t ?sources))
   (test (cdr ?sources)) ; more than one in list
   (bind ?mag-var (format-sym "Enet_~A$~A" (body-name ?loc) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects (
            (vector ?loc (net-field ?loc electric :time ?t) unknown)
            (variable ?mag-var (mag (net-field ?loc electric :time ?t)))
	    (variable ?dir-var (dir (net-field ?loc electric :time ?t)))
            )
  :hint (
         (point (string "You know there is a net electric field at ~A." ?loc))
         (teach (string "In this problem the exact direction of the net electric field vector requires calculation to determine, so you can draw the vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the net electric field at ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?loc))
  ))

;;--------------------------------------------------------------------------
;; Electric potential
;;--------------------------------------------------------------------------

(def-psmclass point-charge-potential (point-charge-potential ?body ?loc ?time)
  :complexity major
  :english ("the formula for the electric potential due to a point charge")
  :ExpFormat ("calculating the electric potential at ~a due to ~a"
		 (nlg ?loc) (nlg ?body))
  :EqnFormat ("V = kelec * q/r" ))

(defoperator point-charge-potential-contains (?sought)
  :preconditions (
  (any-member ?sought ( (potential ?loc ?body :time ?t)
                        (mag (relative-position ?loc ?body :time ?t))
			; if sought is charge, have to choose a location
			; for now, just don't apply for charge
                        ;(charge-on ?body :time ?t ?t)
                      ))
  ; NB: have to make sure body is a point-charge, or else this will apply
  ; for any relative positions in any problem.
  (point-charge ?body)
  )
  :effects (
   (eqn-contains (point-charge-potential ?body ?loc ?t) ?sought)
  ))

(defoperator write-point-charge-potential (?body ?loc ?t)
  :preconditions (
     ;; this psm draws source charge as body:
     (body ?body)
     (variable ?V (potential ?loc ?body :time ?t))
     (variable ?q (charge-on ?body :time ?t ?t))
     (variable ?r (mag (relative-position ?loc ?body :time ?t)))
  )
  :effects (
    (eqn (= ?V (* |kelec| (/ ?q ?r))) (point-charge-potential ?body ?loc ?t))
  )
  :hint (
    ;(point (string " "  ))
    (teach (string "The electric potential at a distance r from a point charge is directly proportional to the charge and inversely proportional to the distance r. The constant of proportionality can be written as kelec in Andes." ))
    (bottom-out (string "Write the equation ~A" 
                         ((= ?V (* |kelec| (/ ?q ?r))) algebra) ))
  ))

#| ; this doesn't really solve the problem, since still need p1 and charge1 choices
; How to define r: If spatial point is occupied by a body, define r to body 
; Else define r to point. Call on r-var subgoal to get this choice
(defoperator use-r-to-body (?body ?loc ?t)
   :preconditions (
        (in-wm (at-place ?body2 ?loc ?t))
	(variable ?r (mag (relative-position ?body2 ?body :time ?t)))
   )
   :effects  ( (r-var ?r (mag (relative-position ?loc ?body :time ?t))) ))

(defoperator use-r-to-point (?body ?loc ?t)
   :preconditions (
        (not (at-place ?body2 ?loc ?t))
	(variable ?r (mag (relative-position ?loc ?body :time ?t)))
   )
   :effects  ( (r-var ?r (mag (relative-position ?loc ?body :time ?t))) ))
|#

;;
;; Addition principle for net electric potential
;;   Vnet = V1+ V2 + ...
;;
;; Note we only apply this if there is more than one source.
;;
(def-psmclass net-potential (net-potential ?loc ?time)
  :complexity major  ; want this in case it is the top psm on a problem
  :english ("the definition of net electric potential")
  :ExpFormat ("calculating the net electric potential from all sources at ~a~a"
		 (nlg ?loc) (nlg ?time 'pp) )
  :EqnFormat ("Vnet = V1 + V2 + ..." ))

(defoperator net-potential-contains (?sought)
  :preconditions (
    (any-member ?sought ((net-potential ?loc :time ?t)
                         (potential ?loc ?source :time ?t) ))
    ; change, always show this:
    ; make sure more than one source in this problem
    ; (in-wm (Efield-sources ?loc ?t ?sources))
    ;(test (cdr ?sources))
  )
  :effects (
    (eqn-contains (net-potential ?loc ?t) ?sought)
  ))

(defoperator write-net-potential (?loc ?t)
  :preconditions (
     (variable ?Vnet (net-potential ?loc :time ?t))
     (in-wm (Efield-sources ?loc ?t ?sources))
     (map ?source ?sources 
   	(variable ?V-var (potential ?loc ?source :time ?t))
	?V-var ?Vi)
  )
  :effects (
    (eqn (= ?Vnet (+ . ?Vi)) (net-potential ?loc ?t))
  )
  :hint (
    ;(point (string " "  ))
    (teach (string "The net electric potential at a point, a scalar, is the sum of the potentials at that point due to each of the sources." ))
    (bottom-out (string "Write the equation ~A" ((= ?Vnet (+ . ?Vi)) algebra) ))
  ))

;
; electric potential energy Ue = q*Vnet
;
(def-psmclass electric-energy (electric-energy ?body ?source ?time)
 :complexity major
  :english ("the formula for the electric potential energy")
  :ExpFormat ("calculating the electric potential energy of ~a"
		 (nlg ?body))
  :EqnFormat ("Ue = q*Vnet" ))

(defoperator electric-energy-contains (?sought)
  :preconditions (
    (any-member ?sought ((electric-energy ?body ?source :time ?t) 
                         (net-potential ?loc :time ?t)
			 (charge-on ?body :time ?t ?t)
			 ))
    ; if sought is net-potential, must bind body: 
    (in-wm (at-place ?body ?loc ?t))

    ; if sought is not energy, must bind source for energy quantity
    ; This will be single named source if known, else "electric_field" if more 
    ; than one source or unspecified.
    (in-wm (Efield-sources ?loc ?t ?sources))
    (bind ?source (cond (?source)    ; no change if already bound
			; if a single source and not = unspecified, use it
                        ((and (null (cdr ?sources))
			      (not (eq (car ?sources) 'unspecified))) (car ?sources))
			(T 'electric_field)))
  )
  :effects (
    (eqn-contains (electric-energy ?body ?source ?t) ?sought)
  ))

(defoperator write-electric-energy (?body ?source ?t)
  :preconditions (
     (in-wm (at-place ?body ?loc ?t))
     (variable ?Ue (electric-energy ?body ?source :time ?t))
     (variable ?q (charge-on ?body :time ?t ?t))
     (variable ?Vnet (net-potential ?loc :time ?t))
     ;; this psm may be the only one to draw body 
     ;; NB: this goal fails if part of cons-energy psm, since
     ;; body already drawn and body-drawing op tests for that.
     (optional (body ?body))
  )
  :effects (
    (eqn (= ?Ue (* ?q ?Vnet)) (electric-energy ?body ?source ?t))
  )
  :hint (
    ;(point (string " "  ))
    (teach (string "The electric potential energy of a body is equal to the the charge on that body times the electric potential at its location." ))
    (bottom-out (string "Write the equation ~A" ((= ?Ue (* q ?Vnet)) algebra) ))
  ))

(defoperator define-electric-energy (?b ?t)
  :preconditions 
  ( (object ?b)
    (bind ?ge-var (format-sym "Ue_~A~@[_~A~]" ?b (time-abbrev ?t))) ) 
 :effects ( 
	   (define-var (electric-energy ?b ?source :time ?t)) 
	   (variable ?ge-var (electric-energy ?b ?source :time ?t)) 
  )
 :hint (
	 (bottom-out (string "Define a variable for electrical potential energy by selecting Energy from the Variables menu on the top menu bar."))
       ))

; To interact with cons-energy psm: op to tell it that electric pe 
; exists in this problem by defining a variable when needed
(defoperator define-electric-ee-var (?b ?t)
  :preconditions 
  ( ;; need to know electric field exists in problem
   (at-place ?b ?loc ?t)
   (Efield-sources ?loc ?t ?sources)
   ;; need to bind source. See electric-energy-contains above
   (bind ?source (cond ((and (null (cdr ?sources))
			     (not (eq (car ?sources) 'unspecified))) 
			(car ?sources))
		       (T 'electric_field)))
   (variable ?var (electric-energy ?b ?source :time ?t))
   )
  :effects ( (ee-var ?b ?t ?var) ))

;;--------------------------------------------------
;;  Magnetic fields and forces 
;;--------------------------------------------------

; draw Bfield in given direction:
(defoperator draw-Bfield-vector (?b ?loc ?t)
  :preconditions ((rdebug "Using draw-Bfield-vector  ~%")
                  (time ?t)
                  (test (time-pointp ?t))
		  ; following requires ?loc to be occupied by body
                  (at-place ?b ?loc ?t)
                  (given (dir (field ?loc magnetic ?source :time ?t)) ?dir-B)  
                  (not (vector ?b (field ?loc magnetic ?source :time ?t) ?dir1))     
                  (bind ?mag-var (format-sym "B_~A$~A" (body-name ?loc) 
					     (time-abbrev ?t)))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
		  ; if dir is z-axis, implicit eqn should give phi angle value
		  (bind ?angle-value (if (z-dir-spec ?dir-B) (zdir-phi ?dir-B) 
		                      ?dir-B))
                  (rdebug "fired draw-Bfield-vector   ~%"))
  :effects (
            (vector ?b (field ?loc magnetic ?source :time ?t) ?dir-B)
            (variable ?mag-var (mag (field ?loc magnetic ?source :time ?t)))
            (variable ?dir-var (dir (field ?loc magnetic ?source :time ?t)))
            ;Because dir is problem given, find-by-psm won't ensure implicit eqn
            ;gets written.  Given value may not be used elsewhere so ensure it here.
            (implicit-eqn (= ?dir-var ?angle-value) (dir (field ?loc magnetic ?source :time ?t)))
            )
  :hint (
	(point (string "You can determine the direction of the magnetic field at ~a due to ~a from the problem statement"
	                ?loc (?source agent))) 
	(bottom-out (string "Use the magnetic field drawing tool (labeled B) to draw the magnetic field at ~a due to ~a in the given direction of ~A." 
		     ?loc (?source agent) (?dir-B adj)))
        )) 

; draw Bfield near a straight current-carrying wire
; problem should give dir of perpendicular distance from wire to ?loc
(defoperator draw-Bfield-straight-current (?loc ?wire ?t)
  :preconditions (
          (given (dir (current-length ?wire :time 1)) ?dir-l)
	  (given (dir (relative-position ?loc ?wire :time 1)) ?dir-r)
	  ; we require body at loc to be axis owner for vector
          (at-place ?b ?loc ?t)
	  (bind ?dir-B (cross-product-dir ?dir-l ?dir-r))
	  (test ?dir-B)
	  (test (not (eq ?B-dir 'zero)))
          (bind ?mag-var (format-sym "B_~A$~A" (body-name ?loc)
				     (time-abbrev ?t)))
          (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
           (vector ?b (field ?loc magnetic ?wire :time ?t) ?dir-B)
           (variable ?mag-var (mag (field ?loc magnetic ?wire :time ?t)))
           (variable ?dir-var (dir (field ?loc magnetic ?wire :time ?t)))
           (given (dir (field ?loc magnetic ?wire :time ?t)) ?dir-B)
  )
  :hint (
      (point (string "The direction of the magnetic field lines around a straight current-carrying wire can be determined by a use of the right-hand rule"))
      (teach (string "Magnetic field lines near a straight current-carrying wire take the form of concentric circles with the wire at their center. If you grasp the wire with your right hand with the thumb pointing in the direction of the current, your fingers curl around the wire in the direction of the magnetic field lines"))
      (bottom-out (string "Curling your right hand around the wire with the thumb in the direction of the current, ~a, your fingers at ~a point in the direction ~a.  Use the magnetic field drawing tool (labeled B) to draw the magnetic field at ~a due to ~a in that direction, ~A." 
           (?dir-l adj) ?loc (?dir-B adj) ?loc ?wire (?dir-B adj)))
  ))

; This draws the magnetic force vector on a positive charge by right-hand-rule
; given direction of B-field and v. Note dir of v may not be directly given,
; but can be derived in several ways, e.g. straight line or circular or other
; motion spec, by draw-velocity* operators. This may draw v as well to get
; the direction from givens.
(defoperator draw-Bforce-rhr-pos (?b ?t ?source)
 :preconditions (
                  (at-place ?b ?loc ?t)
                  (sign-charge ?b pos)
                  (given (dir (field ?loc magnetic ?source :time ?t)) ?dir-B)
		  ; this may require drawing the velocity vector: 
                  (given (dir (velocity ?b :time ?t)) ?dir-V)
		  ; following currently only works for dirs along axis
		  (bind ?F-dir (cross-product-dir ?dir-V ?dir-B))
		  ; make sure we have a non-null direction
		  (test ?F-dir) ; may be NIL on failure
		  (test (not (eq ?F-dir 'zero)))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?loc)
					     (time-abbrev ?t)))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
 )
 :effects (
            (vector ?b (force ?b ?source magnetic :time ?t) ?F-dir)
            (variable ?mag-var (mag (force ?b ?source magnetic :time ?t)))
            (variable ?dir-var (dir (force ?b ?source magnetic :time ?t)))
            (given (dir (force ?b ?source magnetic :time ?t)) ?F-dir)
 )
 :hint (
	(point (string "The magnetic force on a positively charged particle points in the direction of the cross product of its velocity vector and the magnetic field vector at its location.")) 
	(teach (string "The magnetic force vector on a moving charge points in a direction perpendicular to the plane formed by the velocity and magnetic field vectors, in a direction determined by the right hand rule: orient your right hand so that your outstretched fingers point in the direction of the the velocity and when you curl them in they point in the direction of the magnetic field. Your thumb will then point in the direction of the force."))
        (bottom-out (string "Because the velocity of ~a has direction ~a and the magnetic field direction is ~a, the right-hand rule determines the direction of force to be ~a. Use the force drawing tool (labeled F) to draw the magnetic force on ~a due to ~a in the direction of ~A." 
			    ?b (?dir-V adj) (?dir-B adj) (?F-dir adj) ?b 
			    (?source agent) (?F-dir adj)))
 ))

(defoperator draw-Bforce-rhr-neg (?b ?t ?source)
 :preconditions (
                  (at-place ?b ?loc ?t)
                  (sign-charge ?b neg)
                  (given (dir (field ?loc magnetic ?source :time ?t)) ?dir-B)
		  ; this may draw velocity vector -- OK.
                  (given (dir (velocity ?b :time ?t)) ?dir-V)
		  ; following currently only works for dirs along axis
		  ; we save the rhr direction for mentioning in the hints
		  (bind ?rhr-dir (cross-product-dir ?dir-V ?dir-B))
		  ; make sure we have a non-null direction
		  (test ?rhr-dir) ; may be NIL on failure
		  (test (not (eq ?rhr-dir 'zero)))
		  (bind ?F-dir (opposite ?rhr-dir))
		  (test ?F-dir) ; make sure this succeeded
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?loc)
					     (time-abbrev ?t)))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
 )
 :effects (
            (vector ?b (force ?b ?source magnetic :time ?t) ?F-dir)
            (variable ?mag-var (mag (force ?b ?source magnetic :time ?t)))
            (variable ?dir-var (dir (force ?b ?source magnetic :time ?t)))
            (given (dir (force ?b ?source magnetic :time ?t)) ?F-dir)
 )
 :hint (
  (point (string "The magnetic force on a negatively charged particle points in the opposite direction to the cross product of its velocity vector and the magnetic field vector at its location.")) 
	(teach (string "The magnetic force vector on a moving *positive* charge points in a direction perpendicular to the plane formed by the velocity and magnetic field vectors, as determined by the right hand rule: orient your right hand so that your outstretched fingers point in the direction of the the velocity and when you curl them in they point in the direction of the magnetic field. Your thumb will then point in the direction of the cross-product. In this case the charge is *negative*, so the force will be in the opposite direction."))
        (bottom-out (string "Because the velocity of ~a has direction ~a and the magnetic field direction is ~a, the right-hand rule determines the direction of the cross-product to be ~a. Because the charge is negative, the force is opposite that direction. Use the force drawing tool (labeled F) to draw the magnetic force on ~a due to ~a in the direction of ~A." 
			    ?b (?dir-V adj) (?dir-B adj) (?rhr-dir adj) ?b 
			    (?source agent) (?F-dir adj)))
 ))

; draw zero-length vector when V, B have null cross product. 
; Hints assume this is because they are parallel/anti-parallel.
; !!! Could also happen if one vector is zero-length 
(defoperator draw-Bforce-rhr-zero (?b ?t ?source)
 :preconditions (
                  (at-place ?b ?loc ?t)
                  (given (dir (field ?loc magnetic ?source :time ?t)) ?dir-B)
		  ; this may require drawing the velocity vector: 
                  (given (dir (velocity ?b :time ?t)) ?dir-V)
		  ; following currently only works for dirs along axis
		  (bind ?F-dir (cross-product-dir ?dir-V ?dir-B))
		  ; make sure we have a non-null direction
		  (test ?F-dir) ; may be NIL on failure
		  (test (eq ?F-dir 'zero))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?loc)
					     (time-abbrev ?t)))
 )
 :effects (
            (vector ?b (force ?b ?source magnetic :time ?t) ?F-dir)
            (variable ?mag-var (mag (force ?b ?source magnetic :time ?t)))
 )
 :hint (
	(point (string "The magnetic force on a positively charged particle points in the direction of the cross product of its velocity vector and the magnetic field vector at its location.")) 
	(teach (string "Remember the magnitude of a cross product of two vectors is proportional to the sine of the angle between them."))
	(teach (string "If two vectors are parallel or anti-parallel, the sine of the angle betwen them is zero, so their cross-product is a zero-length vector."))
        (bottom-out (string "Because the cross product of the velocity of ~a and the magnetic field is zero in this case, use the force drawing tool (labeled F) to draw a zero-length vector for the magnetic force on ~a due to ~a " 
			     ?b ?b (?source agent)))
 ))

; Draw Bforce on object in unknown direction when we know there's a B-field, but can't 
; determine both B AND V dir for right-hand rule from givens.  
; This might apply in cases where vectors are given by listing components so exact
; directions will be unknown.
;
; In theory, also should require that Fb not given directly -- but we have no problems 
; that just give Fb direction. 
; !!! Also need to test that body is charged and has non-zero velocity. 
; For now we are just assuming that if there's a B-field in the problem, any bodies in problem
; are moving charges, so subject to a force unless given directions determine zero force.
;
; !! Some geometry functions in PhysicsFuncs.cl take "unknown" to guarantee xy-planehood.
; This affects projection equations for unknown angles -- z-projections will always 
; come out equal to zero. Until that is fixed, problems must be designed so unknown
; Bforces come out in the x-y plane.

; This is a nuisance to code, since "not" can't negate a conjunction of conditions.
; Also, there are several ways V might be determinable from givens

; First form draws if we aren't given field dir:
(defoperator draw-Bforce-unknown-field (?b ?t)
  :specifications " "
  :preconditions ((rdebug "Using draw-Bforce-unknown ~%")
                  (B-field ?source) ; so know there is a Bfield in the problem
		  (object ?b)
                  (at-place ?b ?loc ?t)
                  (not (given (dir (field ?loc magnetic ?source :time ?t)) ?dir-B))
                  (not (vector ?b (force ?b ?source magnetic :time ?t) ?dir))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?b) 
					     (time-abbrev ?t)))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Bforce-unknown  ~%")
                  )
  :effects (
            (vector ?b (force ?b ?source magnetic :time ?t) unknown)
            (variable ?mag-var (mag (force ?b ?source magnetic :time ?t)))
            (variable ?dir-var (dir (force ?b ?source magnetic :time ?t)))
            )
  :hint (
         (point (string "Since ~a is charged and moving in a direction that is not parallel or antiparallel to the magnetic field, it will be subject to a magnetic force." ?b))
         (teach (string "In this problem the exact direction of the magnetic force vector requires calculation to determine, so you can draw the force vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the magnetic force on ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?b (?source agent))) 
  ))

; Need another in case we are given field dir but velocity can't be determined
; (might be given by x and y components for component-form calculation.)
; For now, we just require problem to tell us velocity is unknown.
(defoperator draw-Bforce-unknown-velocity (?b ?t)
  :preconditions ((rdebug "Using draw-Bforce-unknown ~%")
                  (B-field ?source) ;so know there is a Bfield
                  (object ?b)
                  (at-place ?b ?loc ?t)
		  ; Require motion explicitly specified as unknown
		  ; Not quite right to presume it is "straight", though
		  (motion ?b (straight ?dontcare unknown) :time ?t ?t)
                  (test (time-pointp ?t))
                  (not (vector ?b (force ?b ?source magnetic :time ?t) ?dir))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?b) 
					     (time-abbrev ?t)))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Bforce-unknown  ~%")
                  )
  :effects (
            (vector ?b (force ?b ?source magnetic :time ?t) unknown)
            (variable ?mag-var (mag (force ?b ?source magnetic :time ?t)))
            (variable ?dir-var (dir (force ?b ?source magnetic :time ?t)))
            )
  :hint (
    (point (string "Since ~a is charged and moving in a direction that is not parallel or antiparallel to the magnetic field, it will be subject to a magnetic force." ?b))
         (teach (string "In this problem the exact direction of the magnetic force vector requires calculation to determine, so you can draw the force vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the magnetic force on ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?b (?source agent))) 
  ))

;; torque vector on a current loop (really anything with a dipole moment!)
;; We draw this as a net-torque, so the assumption is there is no other source of torque.
(defoperator draw-torque-current-loop (?b ?t)
 :preconditions (
	   ; loc must be "region"
           (given (dir (field region magnetic ?source :time ?t)) ?dir-B)
           (given (dir (dipole-moment ?b :time ?t)) ?dir-mu)
	   ; following currently only works for dirs along axis
	   (bind ?tau-dir (cross-product-dir ?dir-mu ?dir-B))
	   ; make sure we have a non-null direction
	   (test (not (eq ?tau-dir 'zero)))  
           (bind ?mag-var (format-sym "NTOR_~A_~A_~A" (body-name ?b) ?axis 
				      (time-abbrev ?t)))
           (bind ?dir-var (format-sym "O~A" ?mag-var))
	   ; need rotation axis for torque definition
	   (rotation-axis ?b ?axis)
 )
 :effects (
            (vector ?b (net-torque ?b ?axis :time ?t) ?tau-dir)
            (variable ?mag-var (mag (net-torque ?b ?axis :time ?t)))
            (variable ?dir-var (dir (net-torque ?b ?axis :time ?t)))
            (given (dir (net-torque ?b ?axis :time ?t)) ?tau-dir)
 )
 :hint 
 (
  (point (string "The torque on a current loop points in the direction of the cross product of its magnetic dipole moment and the magnetic field vector at its location.")) 
  (teach (string "The torque vector on a current loop points in a direction perpendicular to the plane formed by the magnetic moment and magnetic field vectors, in a direction determined by the right hand rule: curl the fingers of your right hand from the dipole moment vector to the magnetic field vector, and your thumb will point in the direction of the torque."))
  (bottom-out (string "Because the magnetic moment has direction ~a and the magnetic field direction is ~a, the right-hand rule determines the direction of torque to be ~a. Use the torque drawing tool (labeled $t) to draw the net torque on ~a about ~a in the direction of ~A." 
		      (?dir-mu adj) (?dir-B adj) (?tau-dir adj) ?b ?axis 
		      (?tau-dir adj)))
  ))

(defoperator draw-torque-current-loop-zero (?b ?t)
 :preconditions (
	   ; loc must be "region"
           (given (dir (field region magnetic ?source :time ?t)) ?dir-B)
           (given (dir (dipole-moment ?b :time ?t)) ?dir-mu)
	   ; following currently only works for dirs along axis
	   (bind ?tau-dir (cross-product-dir ?dir-mu ?dir-B))
	   ; make sure we have a non-null direction
	   (test (eq ?tau-dir 'zero))  
           (bind ?mag-var (format-sym "NTOR_~A_~A_~A" (body-name ?b) ?axis 
				      (time-abbrev ?t)))
	   ; need rotation axis for torque definition
	   (rotation-axis ?b ?axis)
 )
 :effects (
            (vector ?b (net-torque ?b ?axis :time ?t) ?tau-dir)
            (variable ?mag-var (mag (net-torque ?b ?axis :time ?t)))
 )
 :hint (
	(point (string "The torque on a current loop points in the direction of the cross product of its magnetic dipole moment and the magnetic field vector at its location.")) 
	(teach (string "Remember the magnitude of a cross product of two vectors is proportional to the sine of the angle between them."))
	(teach (string "If two vectors are parallel or anti-parallel, the sine of the angle betwen them is zero, so their cross-product is a zero-length vector."))
        (bottom-out (string "Because the cross product of the magnetic moment and the magnetic field direction is zero in this case, use the torque drawing tool (labeled $t) to draw a zero-length vector for the net torque on ~a about ~a." 
			     ?b ?axis ))
        ))

;---------------------------------------------------------
; Bforce magnitude equation: F = abs(q)*V*B*sin(thetaVB)
;---------------------------------------------------------
(def-psmclass charge-force-Bfield-mag (charge-force-Bfield-mag ?body ?time)
  :complexity major
  :english ("force on charge moving in a magnetic field")
  :ExpFormat ("applying the formula for force on charge in a magnetic field")
  :EqnFormat ("Fb = abs(q)*v*B*sin($q)" ))

(defoperator charge-force-Bfield-mag-contains (?sought)
  :preconditions ((debug "Using write-charge-force-Bfield-mag-contains ~%")
                  (any-member ?sought ((mag (force ?b ?source magnetic :time ?t))
                                       (mag (field ?loc magnetic ?source :time ?t))
                                       (charge-on ?b :time ?t ?t)))
                  ;(not (component-form))
                  (at-place ?b ?loc ?t)
                  (rdebug "Firing write-charge-force-Bfield-mag-contains ~%")
                  )
  :effects(
           (eqn-contains (charge-force-Bfield-mag ?b ?t) ?sought)
           ))  

(defoperator write-charge-force-Bfield-mag (?b ?t)
  :preconditions ((debug "Using write-charge-force-Bfield-mag ~%")
                  (at-place ?b ?loc ?t)
		  ;; draw body for this psm
                  (body ?b)
		  ;; draw the vectors B, v, and F.
                  (vector ?dontcare1 (field ?loc magnetic ?source :time ?t) ?B-dir)
                  (vector ?dontcare2 (velocity ?b :time ?t) ?V-dir)
		  (vector ?dontcare3 (force ?b ?source magnetic :time ?t) ?F-dir)
		  ; retrieve vector variables for equation:
                  (in-wm (variable ?magB (mag (field ?loc magnetic ?source :time ?t))))
                  (in-wm (variable ?magV (mag (velocity ?b :time ?t))))
                  (in-wm (variable ?magF (mag (force ?b ?source magnetic :time ?t))))
		  ; define charge variable
                  (variable ?q (charge-on ?b :time ?t ?t))
		  ; calculate angle between. Put it directly into eqn w/o variable.
		  (bind ?theta `(dnum ,(get-angle-between ?V-dir ?B-dir) |deg|))
		  (test ?theta) ; make sure it was determinable
                  (debug "fired write-charge-force-Bfield-mag  ~%")
                  )
  :effects (
            (eqn (= ?magF (* (abs ?q) ?magV ?magB (sin ?theta))) (charge-force-Bfield-mag ?b ?t))
            )
  :hint (
         (point (string "You can calculate magnitude of the magnetic force on a particle from the magnitude of its charge, its velocity, the magnetic field, and the angle between the velocity and magnetic field vectors."))
         (teach (string "The magnitude of the magnetic force on a moving particle in a magnetic field is the product of the absolute value of the charge, the velocity, the magnetic field and the angle theta between the velocity and magnetic field vectors." ))
         (bottom-out (string "Write the equation ~a" ((= ?magF (* (abs ?q) ?magV ?magB (sin ?theta))) algebra)
		      ))
          ))

; Charge-force-Bfield vector PSM.
;
; This isn't written as a vector psm because it's not clear it can fit into our 
; general vector PSM framework: it's not a linear vector equation like Newton's Law
; that resolves into three identical component equations; rather there are three
; different component equations. Also, it relates components in 3 different
; directions, so can't directly use the idea of applying it along a given direction,
; and the test for non-zero-projection used when selecting axis along which
; to apply a vector psm is not appropriate. 

#|
(defoperator charge-force-Bfield-contains (?sought)
  :preconditions ((rdebug "Using charge-force-Bfield-compo-contains  ~%")
                  (any-member ?sought((mag (force ?b ?source magnetic :time ?t))
                                      (dir (force ?b ?source magnetic :time ?t))
                                      (mag (field ?loc magnetic ?source :time ?t))
                                      (dir (field ?loc magnetic ?source :time ?t))
                                      (charge-on ?b :time ?t ?t)
                                      ))
                  (at-place ?b region ?t)
                  (rdebug "Firing charge-force-Bfield-compo-contains  ~%")
                  )
  :effects (
            (eqn-family-contains (charge-force-Bfield ?b ?t) ?sought)
            ; since only one compo-eqn under this vector psm, we can just
            ; select it now, rather than requiring further operators to do so
            (compo-eqn-contains (charge-force-Bfield ?b ?t) qvb ?sought)))
|#

(defoperator draw-charge-force-Bfield-diagram (?b ?t)
  :preconditions (
                  (debug "Using draw-charge-force-Bfield-diagram ~%")
                  (not (vector-diagram (charge-force-Bfield ?b ?t)))
                  (body ?b)
                  (at-place ?b ?loc ?t)
                  (vector ?dontcare (field ?loc magnetic ?source :time ?t) ?dir1) 
                  (vector ?b (force ?b ?source magnetic :time ?t) ?dir2)
                  (vector ?b (velocity ?b :time ?t) ?dir3)
                  (axis-for ?b x ?rot)
                  (debug "Fired draw-charge-force-Bfield-diagram ~%")
                  )
  :effects (
            (vector-diagram (charge-force-Bfield ?b ?t))
            )
  :hint (
         (point (string "Try drawing a diagram."))
         (teach (string "The diagram should show the force vector and the magnetic field vector at ~a." ?b))
         (bottom-out (string "Draw a diagram showing the force vector on ~a due to the magnetic field at ~a." ?b ?loc))
          ))

; Component equations for F = q*(V X B). This is sometimes called the determinant
; form, since V X B =
;   |i  j  k |
;   |Vx Vy Vz| 
;   |Bx By Bz|
; = (Vx*Bz - Vz*Bx) i + (Vz*Bx - Vx*Bz) j + (Vx*By - Vy*Bx) k
; We treat these as three scalar equations in terms of vector components.
; These equations only enabled when "component-form" is in the problem,  
; which enables projection psm to be used, so components can appear at
; bubble-graph level. As coded, the rules assume use of standard axes, 
; which "component form" guarantees (could probably avoid this assumption by
; using variables for the tilt of x and y axes -- making sure they're orthogonal!)
;
; The formula works for arbritrary three-dimensional vectors, but we can't
; draw all of those in Andes.  We will usually be dealing with the following 
; cases only:
; 1) V, B in xy plane, which reduces to
;    Fx = 0 
;    Fy = 0
;    Fz = q(Vx*By - Vy*Bx)       
; 2) V in xy plane, B along z-axis:
;    Fx = q(Vy*Bz)  
;    Fy = q(-Vx*Bz)
;    Fz = 0             
; 3. V parallel/antiparallel to B => Fx=Fy=Fz=0
;
; Vectors will usually be drawn in known directions from the givens and
; the right hand rule, so projection equations will write equations for zero components.
; However, these equations write general form in all cases, and don't simplify form
; for cases where components vanish. !!! That might be a problem, since equation can 
; then contain a variable that it doesn't determine, which can fool the solution
; collection algorithm.

(def-psmclass charge-force-Bfield-x (charge-force-Bfield-x ?body ?time)
  :complexity major
  :english ("x component of magnetic force on moving charge")
  :ExpFormat ("applying the formula for x component of magnetic force")
  :EqnFormat ("F_x = q*(V_y*B_z - V_z*B_y" ))

(defoperator charge-force-Bfield-x-contains (?sought)
  :preconditions ((component-form)
                 (any-member ?sought((compo x 0 (force ?b ?source magnetic :time ?t))
				      (compo y 0 (velocity ?b :time ?t))
                                      (compo z 0 (field ?loc magnetic ?source :time ?t))
				      (compo z 0 (velocity ?b :time ?t))
                                      (compo y 0 (field ?loc magnetic ?source :time ?t))
                                      (charge-on ?b :time ?t ?t)
                                      ))
                  (at-place ?b ?loc ?t))
  :effects ((eqn-contains (charge-force-Bfield-x ?b ?t) ?sought)))

(defoperator charge-force-Bfield-x (?b ?t)
  :preconditions ((at-place ?b ?loc ?t)
                 (vector-diagram (charge-force-Bfield ?b ?t))
                 (variable ?Fx (compo x 0 (force ?b ?source magnetic :time ?t)))
		 (variable ?Vy (compo y 0 (velocity ?b :time ?t)))
                 (variable ?Bz (compo z 0 (field ?loc magnetic ?source :time ?t)))
		 (variable ?Vz (compo z 0 (velocity ?b :time ?t)))
		 (variable ?By (compo y 0 (field ?loc magnetic ?source :time ?t)))
                 (variable ?q (charge-on ?b :time ?t ?t)))
  :effects ( 
              (eqn (= ?Fx (* ?q (- (* ?Vy ?Bz) (* ?Vz ?By)))) (charge-force-Bfield-x ?b ?t)) 
           )
  :hint (
	  (point (string "The x component of a vector cross product can be computed from the y and z components of the vectors being multiplied. You can use this to compute the x component of the magnetic force."))
	  (teach (string "The x component of the cross product of two vectors V and B is equal to V_y*B_z - V_z*B_y. "))
          (bottom-out (string "Write the equation ~A"  
	                      ((= ?Fx (* ?q (- (* ?Vy ?Bz) (* ?Vz ?By)))) algebra) ))
        ))

(def-psmclass charge-force-Bfield-y (charge-force-Bfield-y ?body ?time)
  :complexity major
  :english ("y component of magnetic force on moving charge")
  :ExpFormat ("applying the formula for y component of magnetic force")
  :EqnFormat ("F_y = q*(V_z*B_x - V_x*B_z" ))

(defoperator charge-force-Bfield-y-contains (?sought)
  :preconditions ((component-form)
                  (any-member ?sought((compo y 0 (force ?b ?source magnetic :time ?t))
				      (compo z 0 (velocity ?b :time ?t))
                                      (compo x 0 (field ?loc magnetic ?source :time ?t))
				      (compo x 0 (velocity ?b :time ?t))
                                      (compo z 0 (field ?loc magnetic ?source :time ?t))
                                      (charge-on ?b :time ?t ?t)
				     ))
		  (at-place ?b ?loc ?t))
  :effects ((eqn-contains (charge-force-Bfield-y ?b ?t) ?sought)))

(defoperator charge-force-Bfield-y (?b ?t)
  :preconditions ( 
                 (at-place ?b ?loc ?t)
                 (vector-diagram (charge-force-Bfield ?b ?t))
                 (variable ?Fy (compo y 0 (force ?b ?source magnetic :time ?t)))
		 (variable ?Vz (compo z 0 (velocity ?b :time ?t)))
                 (variable ?Bx (compo x 0 (field ?loc magnetic ?source :time ?t)))
		 (variable ?Vx (compo x 0 (velocity ?b :time ?t)))
		 (variable ?Bz (compo z 0 (field ?loc magnetic ?source :time ?t)))
                 (variable ?q (charge-on ?b :time ?t ?t))
                 )
  :effects ( 
              (eqn (= ?Fy (* ?q (- (* ?Vz ?Bx) (* ?Vx ?Bz)))) (charge-force-Bfield-y ?b ?t)) 
           )
  :hint (
         (point (string "The y component of a vector cross product can be computed from the x and z components of the vectors being multiplied. You can use this to compute the y component of the magnetic force."))
	  (teach (string "The y component of the cross product of two vectors V and B is equal to V_z*B_x - V_x*B_z. "))
          (bottom-out (string "Write the equation ~A"  
	                      ((= ?Fy (* ?q (- (* ?Vz ?Bx) (* ?Vx ?Bz)))) algebra) ))
        ))

(def-psmclass charge-force-Bfield-z (charge-force-Bfield-z ?body ?time)
  :complexity major
  :english ("z component of magnetic force on moving charge")
  :ExpFormat ("applying the formula for z component of magnetic force")
  :EqnFormat ("F_z = q*(V_x*B_y - V_y*B_x" ))

(defoperator charge-force-Bfield-z-contains (?sought)
  :preconditions 
  ((component-form)
   (any-member ?sought ((compo z 0 (force ?b ?source magnetic :time ?t))
			(compo x 0 (velocity ?b :time ?t))
			(compo y 0 (field ?loc magnetic ?source :time ?t))
			(compo y 0 (velocity ?b :time ?t))
			(compo x 0 (field ?loc magnetic ?source :time ?t))
			(charge-on ?b :time ?t ?t)
			))
   (at-place ?b ?loc ?t))
  :effects ((eqn-contains (charge-force-Bfield-z ?b ?t) ?sought)))

(defoperator charge-force-Bfield-z (?b ?t)
  :preconditions 
  ( 
   (at-place ?b ?loc ?t)
   (variable ?Fz (compo z 0 (force ?b ?source magnetic :time ?t)))
   (variable ?Vx (compo x 0 (velocity ?b :time ?t)))
   (variable ?By (compo y 0 (field ?loc magnetic ?source :time ?t)))
   (variable ?Vy (compo y 0 (velocity ?b :time ?t)))
   (variable ?Bx (compo x 0 (field ?loc magnetic ?source :time ?t)))
   (variable ?q (charge-on ?b :time ?t ?t))
   )
  :effects ( 
	    (eqn (= ?Fz (* ?q (- (* ?Vx ?By) (* ?Vy ?Bx)))) 
		 (charge-force-Bfield-z ?b ?t)) 
           )
  :hint (
          (point (string "The z component of a vector cross product can be computed from the x and y components of the vectors being multiplied. You can use this formula to compute the z component of the magnetic force."))
	  (teach (string "The z component of the cross product of two vectors V and B is equal to V_x*B_y - V_y*B_x. "))
          (bottom-out (string "Write the equation ~A"  
	                      ((= ?Fz (* ?q (- (* ?Vx ?By) (* ?Vy ?Bx)))) algebra) ))
        ))


#| ; not using this, just show dir on diagram
(defoperator charge-force-Bfield-dir-contains (?sought)
  :preconditions 
  ((debug "Using write-charge-force-Bfield-dir-contains ~%")
   (any-member ?sought ((dir (force ?b ?source magnetic :time ?t))
			(dir (field ?loc magnetic ?source :time ?t))
			(charge-on ?b :time ?t ?t)))
   ;;(not (component-form))
   (body ?b)
   (rdebug "Firing write-charge-force-Bfield-dir-contains ~%")
   )
  :effects(
           (eqn-contains (charge-force-Bfield-dir ?b) ?sought)
           )) 

(defoperator write-force-dir-charge-Bfield-pos (?b ?t)
  :preconditions 
  ((debug "Using write-force-dir-charge-Bfield-pos ~%")
   (at-place ?b ?loc ?t)
   (variable ?q (charge-on ?b :time ?t ?t))
   (given (dir (field ?loc magnetic ?source :time ?t)) (dnum ?dir1 ?doncare1)) 
   (motion ?b (straight NIL (dnum ?dir2 ?dontcare1)) :time ?t ?t)
   (variable ?OF (dir (force ?b ?source magnetic :time ?t)))
   (bind ?in-out (if (>= ?dir1 ?dir2) '(dnum 0 |deg|) '(dnum 180 |deg|)))
   (sign-charge ?b pos)
   (debug "Fired write-force-dir-charge-Bfield-pos  ~%")
   )
  :effects (
            (eqn (= ?OF ?in-out) (charge-force-Bfield-dir ?b))
            )
  :hint (
         (point (string "What is the relationship between the force, the charge and the magnetic field?"))
         (teach (kcd "write-force-dir-charge-Bfield-pos")
                 (string "The force on the charged particle is perpendicular to both the velocity vector and the magnetic field vector."))
         (bottom-out (string "Use the right hand rule using the velocity vector and the magnetic field vector."))
         ))
         
(defoperator write-force-dir-charge-Bfield-neg (?b ?t)
  :preconditions 
  ((debug "Using write-force-dir-charge-Bfield-neg ~%")
   (at-place ?b ?loc ?t)
   (variable ?q (charge-on ?b :time ?t ?t))
   (given (dir (field ?loc magnetic ?source :time ?t)) (dnum ?dir1 ?doncare1)) 
   (motion ?b (straight NIL (dnum ?dir2 ?dontcare1)) :time ?t ?t)
   (variable ?OF (dir (force ?b ?source magnetic :time ?t)))
   (bind ?in-out (if (>= ?dir1 ?dir2) 180 0))
   (sign-charge ?b neg)
   (debug "Fired write-force-dir-charge-Bfield-neg  ~%")
   )
  :effects (
            (eqn (= ?OF ?in-out) (charge-force-Bfield-dir ?b))
            )
  :hint (
         (point (string "What is the relationship between the force, the charge and the magnetic field?"))
         (teach (kcd "write-force-dir-charge-Bfield-neg")
                 (string "The force on the charged particle is perpendicular to both the velocity vector and the magnetic field vector."))
         (bottom-out (string "Use the right hand rule using the velocity vector and the magnetic field vector."))
         ))
|#






#|
;; following draws the sought vector in one of these problems
(defoperator draw-sought-force-vector-unknown-with-multi-args (?b ?vectype ?args ?t)
   :preconditions ((rdebug "Using draw-sought-force-vector-unknown-with-multi-args  ~%")
                   (vector-grid)
                   (component-form) 
                   (test (not (equal ?args nil)))
                   (bind ?vector `(,?vectype ,?b . ?args :time ,?t))
                   (test (or (equal 'force (first (second ?vector)))
                             (equal 'field (first (second ?vector)))))
;;; we test whether xc of vector is a problem sought. HACK: This relies
;;; on *cp* as always holding the current problem, which is not guaranteed
;;; if problem solver is not invoked through our interface functions.
;;; But there should be some way to access this info from the environment.
                   ;; (bind ?xc `(compo x 0 ,?vector))
                   ;; (test (member ?xc (problem-soughts *cp*) :test #'equal))
                   (not (given (dir ?vector :time ?t) (dnum ?dir |deg|)))
                   (bind ?mag-var (format-sym "~A_~A_~A" ?vectype 
					      (body-name ?b) (time-abbrev ?t)))
                   (bind ?dir-var (format-sym "O~A" ?mag-var))
                   (rdebug "fired draw-sought-force-vector-unknown-with-multi-args ~%")
                   )
   :effects (
             (vector ?b (?vectype ?b . ?args :time ?t) unknown)
             (variable ?mag-var (mag (?vectype ?b . ?args :time ?t)))
             (variable ?dir-var (dir (?vectype ?b . ?args :time ?t)))
             )
  :hint (
         (point (string "Try drawing a diagram."))
         (teach (string "The diagram should show the vector you are seeking."))
         (bottom-out (string "Draw a diagram showing the vector you are seeking using the given components."))
        ))

|#

