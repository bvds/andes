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
  :short-name "Coulomb's law (magnitude)"
  :english ("Coulomb's law")
  :expformat ("applying Coulombs's law for the force on ~a due to ~a" (nlg ?body) (nlg ?agent))
  :EqnFormat ("F = kelec*abs(q1*q2)/r^2"))


;;; The equation is scalar equation containing vector magnitudes only.
(defoperator coulomb-contains (?sought)
  :preconditions 
  (
   (point-charge ?b1) (point-charge ?b2)
   (any-member ?sought 
		 (
		  ;; because of the abs(q1)*abs(q2), don't include charge
		  (mag (force ?b1 ?b2 electric :time ?t))
		  (mag (relative-position ?b1 ?b2 :time ?t))
		  ))
   (time ?t)
   )
  :effects ( (eqn-contains (coulomb ?b1 ?b2 ?t) ?sought) ))

(defoperator write-coulomb (?b1 ?b2 ?t) 
  :preconditions 
  (
   (body ?b1)
   (body ?b2) ; draw source as pot and E-field rules do
   (inherit-variable ?q1 (charge ?b1 :time ?t))
   (inherit-variable ?q2 (charge ?b2 :time ?t))
   (variable ?r  (mag (relative-position ?b1 ?b2 :time ?t)))
   (variable ?F  (mag (force ?b1 ?b2 electric :time ?t)))
   )
  :effects 
  ;; kelec is predefined, see file constants.cl
  ( (eqn (= ?F (/ (* |kelec| (abs ?q1) (abs ?q2)) (^ ?r 2)))
	 (coulomb ?b1 ?b2 ?t) )
    ;; match both forms of vector version of equation
    (assume using-magnitude (coulomb-vec ?b1 ?b2 ?t t))
    (assume using-magnitude (coulomb-vec ?b1 ?b2 ?t nil))
    )
  :hint (
	 (teach (string "Coulombs's law states that electrostatic force between two charges is proportional to the charges of the bodies divided by the square of the distance between the bodies."))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?F (/ (* |kelec| (abs ?q1) (abs ?q2)) 
				 (^ ?r 2))) algebra)))
	 ))

(def-psmclass coulomb-compo (?eqn-type coulomb-force ?axis ?rot 
				 (coulomb-vec ?body ?agent ?time ?form))
  :complexity major    
  :Doc "Definition of Coulomb's law, component form."
  :short-name ("Coulomb's law (~A component)" (axis-name ?axis))
  :english ("Coulomb's law (component form)") 
  :ExpFormat ("applying Coulomb's law to ~a and ~A ~a"
	      (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
  :EqnFormat ("F_~A = (kelec*q1*q2/r^2) * r_~A/r" 
	      (axis-name ?axis) (axis-name ?axis)))

(defoperator coulomb-vector-contains (?sought)
  :preconditions 
  (
   (point-charge ?b1)
   (point-charge ?b2)
   (any-member ?sought
	       (;; if sought is charge, can use either equation for force
		;; on b1 from b2 or force on b2 from b1, so need both:
		(charge ?b1 :time ?t)
		(charge ?b2 :time ?t)
		(relative-position ?b1 ?b2 :time ?t)
		(force ?b1 ?b2 electric :time ?t)
	       ))
   (time ?t)
   (any-member ?form (nil t)) ;switch between forms of r-hat
   )
  :effects 
   ((eqn-family-contains (coulomb-vec ?b1 ?b2 ?t ?form) ?sought)
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (coulomb-vec ?b1 ?b2 ?t ?form) coulomb-force ?sought)))

(defoperator draw-coulomb-vector-diagram (?rot ?b ?agent ?t)
  :preconditions 
  (
   (body ?b)
   (body ?agent) ; draw source as pot and E-field rules do
   (vector ?b (relative-position ?b ?agent :time ?t) ?dir1)
   ;; assuming (without checking) only one force between the two bodies.
   (vector ?b (force ?b ?agent electric :time ?t) ?dir2)
   (axes-for ?b ?rot)
   )
  :effects (
	    (vector-diagram ?rot (coulomb-vec ?b ?agent ?t ?form))
  ))

(defoperator write-coulomb-compo (?b1 ?b2 ?t ?xy ?rot ?form)
  :preconditions 
  (
   ;; make sure r-hat compo doesn't vanish
   (in-wm (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?r-dir))
   (test (non-zero-projectionp ?r-dir ?xy ?rot))
   (inherit-variable ?q1 (charge ?b1 :time ?t))
   (inherit-variable ?q2 (charge ?b2 :time ?t))
   (variable ?r  (mag (relative-position ?b1 ?b2 :time ?t)))
   (variable ?F_xy  (compo ?xy ?rot (force ?b1 ?b2 electric :time ?t)))
   (hat ?rhat-compo (relative-position ?b1 ?b2 :time ?t) ?xy ?rot ?form)
   )
  :effects (
   (eqn (= ?F_xy (* (/ (* |kelec| ?q1 ?q2) (^ ?r 2)) ?rhat-compo))
            (compo-eqn coulomb-force ?xy ?rot 
		       (coulomb-vec ?b1 ?b2 ?t ?form)))
   )
  :hint (
     (teach (string "Coulombs's law states that electrostatic force between two charges is proportional to the charges of the bodies divided by the square of the distance between the bodies."))
     (bottom-out (string "Write the equation ~A" 
			 ((= ?F_xy (* (/ (* |kelec| ?q1 ?q2) (^ ?r 2)) 
				      ?rhat-compo)) algebra)))
  ))


;;; We have two main principles for electric fields:
;;; charge-force-Efield: vector equation E_x = F_x/q   (equiv:  F_x = q*E_x)
;;; which is definition of E-field.  This is a vector principle written
;;; in component form. We also have scalar variants for magnitude only.
;;;
;;; point-charge-Efield: E-field at a distance r from a single point charge 
;;; derived from definition + Coulomb's law:  E = k*q/r^2.  We have a vector
;;; form for components in terms of thetar, as well as a magnitude only 
;;; scalar form.
;;;
;;; TODO: K&M's original problems all had only a single body.  
;;; So the situations had to be either of only two kinds: 
;;;     (1) a "region" w/field of unspecified source plus a "test" charge 
;;;         feeling the force; OR, 
;;;     (2) a point-charge source creating a field at an unoccupied point. 
;;; Some rules exploit this dichotomy, testing (at-place ?b ?loc :time ?t) to 
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
;;; be given (directly or via compos) or can be derived from the given field 
;;; direction (directly or via compos), can be derived
;;; from configuration wrt point particle, or else unknown.

(defoperator timeless-field (?loc ?type ?source ?t)
  :preconditions ((test (not (member 'changing-field (problem-features *cp*))))
		  (time ?t))
  :effects 
  ((inherit-quantity (field ?loc ?type ?source :time ?t)
		     (field ?loc ?type ?source))))

(defoperator homogeneous-field-is-given-field (?rest)
  :preconditions ((homogeneous-field . ?rest))
  :effects ((given-field . ?rest)))

(defoperator field-sources-is-given-field (?loc ?type ?source)
  :preconditions ((field-sources ?loc ?type ?sources)
		  (any-member ?source ?sources))
  :effects ((given-field ?loc ?type ?source)))


;; Draw homogeneous field in a specified direction.
(defoperator draw-field-given-dir (?loc ?type ?t)
  :preconditions 
  ((rdebug "Using draw-field-vector  ~%")
   (homogeneous-field ?loc ?type ?source :dir ?dir-f :time ?t)
   (test (definite-directionp ?dir-f))
   ;; only use time when allowed by feature changing-field
   ;; Sanity test for inherit-quantity working OK
   (test (or (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*))))
	     (error "draw-field-given-dir bad time slot ~A" ?t)))
   (not (vector ?any-body (field ?loc ?type ?source :time ?t) ?dir1))     
   (bind ?mag-var (format-sym "~A_~A_~A~@[_~A~]" (subseq (string ?type) 0 1)
			      (body-name ?loc) (body-name ?source) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-field-vector   ~%"))
  :effects (
            (vector ?loc (field ?loc ?type ?source :time ?t) ?dir-f)
            (variable ?mag-var (mag (field ?loc ?type ?source :time ?t)))
            (variable ?dir-var (dir (field ?loc ?type ?source :time ?t)))
	    (given (dir (field ?loc ?type ?source :time ?t)) ?dir-f)
            )
  :hint (
	(point (string "Notice that ~A contains a constant ~A field."
	               ?loc (?type adj) ?loc)) 
	(bottom-out (string "Use the ~A field drawing tool to draw the ~A field at ~a due to ~a in the given direction of ~A." 
		     (?type adj) (?type adj) ?loc (?source agent) (?dir-f adj)))
        )) 


(defoperator draw-efield-inside-conductor (?loc ?source ?t)
  :preconditions 
  (
   ;; Since the field is zero, the source is not really well-defined.
   ;; However, we assume that there is some other region that does have 
   ;; a given field source.
   (inside-conductor ?loc :electric-source ?source)
   ;; select a time in case it comes in unbound
   (time-or-timeless ?t)
   ;; only use time when allowed by feature changing-field
   (test (eq (null ?t) 
	     (null (member 'changing-field (problem-features *cp*)))))
   (not (vector ?any-body (field ?loc electric ?source :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "E_~A_~A~@[_~A~]" 
			      (body-name ?loc) (body-name ?source) 
			      (time-abbrev ?t)))
   )
  :effects 
  (
   (vector ?loc (field ?loc electric ?source :time ?t) zero)
   (variable ?mag-var (mag (field ?loc electric ?source :time ?t)))
   ;; Don't need this for flux
   ;; (given (mag (field ?loc electric :time ?t)) (dnum 0 |V/m|))
   )
  :hint (
	 (point (string "Not that ~A is inside a conductor." ?loc)) 
	 (teach (string "Inside a good conductor, the electric field is almost zero." ?loc)) 
	 (bottom-out (string "Use the E field drawing tool to draw a zero-length vector for the electric field at ~a ." 
			     ?loc))
	 ))

;; pull out the sign of a given charge
(defoperator get-sign-given-charge (?b)
  :preconditions ((in-wm (given (charge ?b) (dnum ?val ?units)))
                  (bind ?pos-neg (if (> ?val 0) 'pos 'neg)))
  :effects ((sign-charge ?b ?pos-neg)))

;; Can draw field vector if E force dir is given directly
;; Generally, this will only work when the feature changing-field is on
(defoperator draw-Efield-given-force-dir (?b ?t)
  :preconditions 
  ((rdebug "Using draw-Efield-given-force-dir ~%")
   ;; make sure there is a field but the direction at loc of b is not given, 
   ;; directly or via components:
   (given-field ?loc ?electric ?source :time ?t :dir ?dir)
   (test (null ?dir))
   (not (given (compo ?xyz ?rot 
		      (field ?loc electric ?source :time ?t)) ?dontcare2))
   ;; only use time when allowed by feature changing-field
   (test (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*)))))
   ;; ?b is "test charge" feeling force at loc at some time.
   (at-place ?b ?loc :time ?t-place)
   (test (tinsidep ?t ?p-place))
   ;; make sure direction of force on ?b is given
   ;; (given (dir (force ...)) ...) is a side-effect of several drawing rules;
   ;; need in-wm to prevent recursion with find-electric-force-given-field-dir
   (in-wm (given (dir (force ?b ?source electric :time ?t-force)) ?F-dir))
   (test (tinsidep ?t ?t-force))
   ;; require sign of charge to be given
   (sign-charge ?b ?pos-neg)
   (bind ?Field-dir (if (eq ?pos-neg 'pos) ?F-dir (opposite ?F-dir)))
   (bind ?same-or-opposite  (if (eq ?pos-neg 'pos) 'same 'opposite))
   (bind ?mag-var (format-sym "E_~A_~A~@[_~A~]" (body-name ?loc) 
			      (body-name ?source) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-Efield-given-force-dir  ~%")
   )
  :effects 
  (
   (vector ?loc (field ?loc electric ?source :time ?t) ?Field-dir)
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

(defoperator find-given-field-forces-in-region (?b ?loc ?source ?type ?t)
  :preconditions
  (
   (at-place ?b ?loc :time ?t-place)
   (test (tinisidep ?t ?t-place))
   (not (given (dir (force ?b ?source ?type :time ?t-force) ?any-dir))
	(or (tinsidep ?t ?t-force) (tinsidep ?t-force ?t)))
   (not (given (dir (force ?source ?b ?type :time ?t-force) ?any-dir)) 
	(or (tinsidep ?t ?t-force) (tinsidep ?t-force ?t)))
   )
:effects ((given-field-force ?b ?loc ?source ?type :time ?t)))

;; this is for drawing a homogeneous field in an un-specified direction
;; Many cases of this are ones where the componets are given.
(defoperator draw-field-unknown (?loc ?type ?source ?t)
  :preconditions 
  (
   (homogeneous-field ?loc ?type ?source :dir ?dir :time ?t)
   (test (or (null ?dir) (eq ?dir 'unknown)))
   ;; only use time when allowed by feature changing-field
   ;; Sanity test for inherit-quantity working OK
   (test (or (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*))))
	     (error "draw-field-unknown bad time slot ~A" ?t)))
   (not (vector ?dontcare (field ?loc ?type ?source :time ?t) ?any-dir))
   ;; test that the force direction is not given
   (setof (given-field-fore ?b ?loc ?source ?type ?t) ?b ?bb)
   (test (null ?bb))
   ;; inside a conductor is handled differently
   (not (inside-conductor ?loc) (eq ?type 'electric))
   (bind ?mag-var (format-sym "~A_~A_~A~@[_~A~]" (subseq (string ?type) 0 1) 
			      (body-name ?loc) (body-name ?source)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
            (vector ?loc (field ?loc ?type ?source :time ?t) unknown)
            (variable ?mag-var (mag (field ?loc ?type ?source :time ?t)))
	    (variable ?dir-var (dir (field ?loc ?type ?source :time ?t)))
            )
  :hint (
         (point (string "Note the constant ~A field at ~A." (?type adj) ?loc))
         (teach (string "In this problem, the exact direction of the ~A field vector is not given, so you can draw the vector at an approximately angle and leave the exact angle unspecified." (?type adj)))
         (bottom-out (string "Draw the ~A field at ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." (?type adj) ?loc (?source agent)))
          ))


;; draw point charge Efield at loc if dir from source to loc is given
;; Generally, this will only work when the feature changing-field is on
(defoperator draw-point-Efield-given-relpos-dir (?b ?loc ?t)
  :preconditions 
  (
   ;; Only include time when appropriate
   (test (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*)))))
   ;; Make sure source is point-charge
   (point-charge ?b)
   (test (or (null ?t) (time-pointp ?t)))
   (given-field ?loc ?electric ?b)
   (not (given (dir (field ?loc electric ?b :time ?t)) ?dontcare3))
   (given (dir (relative-position ?loc ?b :time ?t)) ?rdir)
   (sign-charge ?b ?pos-neg)
   (bind ?Field-dir (if (eq ?pos-neg 'pos) ?rdir (opposite ?rdir)))
   (bind ?same-or-opposite (if (eq ?pos-neg 'pos) 'same 'opposite))
   ;; make sure it has not already been drawn
   (not (vector ?any-loc (field ?loc electric ?b :time ?t) ?whatever))
   (bind ?mag-var (format-sym "E_~A_~A~@[_~A~]" (body-name ?loc) (body-name ?b) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
            (vector ?loc (field ?loc electric ?b :time ?t) ?Field-dir)
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
   ;; Make sure source is point-charge
   (point-charge ?b)
   ;; make sure it works at the given location
   (given-field ?loc electric ?b)
   ;; Sanity test for inherit-quantity working OK
   (test (or (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*))))
	     (error "draw-point-efield-unknown bad time slot ~A" ?t)))
   ;; make sure ?loc not equals ?loc-source?
   (not (vector ?dontcare (field ?loc electric ?b :time ?t1) ?any-dir))
   (not (given (dir (relative-position ?loc ?b :time ?t-any)) ?whatever1))
   (not (given (dir (relative-position ?b ?loc :time ?t-any)) ?whatever2))
   (bind ?mag-var (format-sym "E_~A_~A~@[_~A~]" (body-name ?loc) (body-name ?b)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "Fired draw-point-Efield-unknown  ~%")
   )
  :effects (
            (vector ?loc (field ?loc electric ?b :time ?t) unknown)
            (variable ?mag-var (mag (field ?loc electric ?b :time ?t)))
            (variable ?dir-var (dir (field ?loc electric ?b :time ?t)))
            )
  :hint (
        (point (string "Because ~A is charged, it creates an electric field at ~A." ?b ?loc))
        (teach (string "The direction of the electric field due to a point charge is radial away from a positive charge and toward a negative charge.  In this problem, the exact direction of the electric field vector requires calculation to determine, so you can draw the vector at an approximately correct angle and leave the exact angle unspecified."))
        (bottom-out (string "Draw the electric field at ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?loc (?b agent)))
          ))

;;
;; Drawing E force vector -- parallels drawing E field vector
;;

;; - if given E force vector dir --

;; make sure force exists apart from drawing
(defoperator find-field-force (?b ?agent ?type ?t)
  :preconditions (		  
    (time ?t)
    (any-member ?type (electric magnetic))
    (not (given (dir (field ?b ?type ?agent :time ?t-field)) ?field-dir)
	 (tinsidep ?t ?t-field))
    ;; The direction is a side-effect of draw-coulomb-force and
    ;; draw-electric-force-given-dir
    ;; Ideally, find-field-force would only apply when the others don't.
    ;; As a work-around, use (in-wm ...)
    (in-wm (given (dir (force ?b ?agent ?type :time ?t-force)) ?dir-expr))
    (test (tinsidep ?t ?t-force))
    ;; check that something else hasn't defined this force.
    (not (force ?b ?agent ?type ?t . ?dont-care)) 
  )
  :effects (
    (force ?b ?agent ?type ?t ?dir-expr action)
    (force-given-at ?b ?agent ?type ?t-force ?dir-expr action)
  ))

 
(defoperator draw-electric-force-given-dir (?b ?source ?t)
  :preconditions 
  ((rdebug "Using draw-Eforce-given-dir ~%")
   (force ?b ?source electric ?t ?dir action)
   ;; from find-electric-force above
   (in-wm (given (dir (force ?b ?source electric :time ?t-force)) ?dir-expr))
   (test (tinsidep ?t ?t-force))
   ;;
   (not (vector ?any-b (force ?b ?source electric :time ?t) ?whatever))
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

;; Force due to a Coulombs law interaction where the electric field
;; is not used.

(defoperator calculate-coulomb-force-dir (?b ?source ?t)
  :preconditions 
  (
   (given (dir (relative-position ?b ?source :time ?t)) ?rdir)
   ;; require sign of both charges to be known
   (sign-charge ?b ?pos-neg1)
   (sign-charge ?source ?pos-neg2)
   (bind ?F-dir (if (eq ?pos-neg1 ?pos-neg2) ?rdir (opposite ?rdir)))
   )	
  :effects ((coulomb-force-dir ?F-dir ?b ?source ?t)))

(defoperator find-coulomb-force (?b ?agent ?t)
  :preconditions 
  (
   (point-charge ?b)
   (point-charge ?agent)
   (time ?t)
   (not (given (dir (force ?b ?agent electric :time ?t-force)) ?dir1-expr)
	(tinsidep ?t ?t-force))
   (not (given (dir (force ?agent ?b electric :time ?t-force)) ?dir2-expr)
	(tinsidep ?t ?t-force))
   (test (not (equal ?b ?agent)))
   ;; Find any direction that can be calculated or 'unknown
   (setof (coulomb-force-dir ?F-dir ?b ?agent ?t) ?F-dir ?F-dirs)
   (bind ?final (if ?F-dirs (first ?F-dirs) 'unknown))
    ;; check that something else hasn't defined this force.
   (not (force ?b ?agent electric ?t . ?dont-care)) 
  )
  :effects (
	    ;; We don't want to invoke find-reaction force since the
	    ;; reaction can be gotten through opposite-relative-position
    (force ?b ?agent electric ?t ?final coulomb)
    (force-given-at ?b ?agent electric ?t ?final coulomb)
  ))

(defoperator draw-coulomb-force (?b ?source ?t)
  :preconditions 
  (
   ;; ensure the direction was found based on Coulomb's law
   (force ?b ?source electric ?t ?F-dir coulomb)
   (test (not (eq ?F-dir 'unknown)))
   (not (vector ?any-b (force ?b ?source electric :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "Fe_~A_~A~@[_~A~]" 
			      (body-name ?b) (body-name ?source)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
:effects (
	  (vector ?b (force ?b ?source electric :time ?t) ?F-dir)
	  (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
	  (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
	  (given (dir (force ?b ?source electric :time ?t)) ?F-dir)
	  )
:hint (
       (point (string "Think about how the direction of the electric force on ~A due to ~a is related to the relative position of the two bodies." ?b (?source agent)))
       (teach (string "Remember that opposite charges attract and like charges repel."))
       (bottom-out (string "Use the force drawing tool (labeled F) to draw the electric force on ~a due to ~a in the ~a direction." 
			   ?b (?source agent) (?F-dir adj)))
       ))

(defoperator draw-coulomb-force-unknown (?b ?source ?t)
  :preconditions 
  (
   (force ?b ?source electric ?t unknown coulomb)
   (not (vector ?b (force ?b ?source electric :time ?t) ?whatever))
   (bind ?mag-var (format-sym "Fe_~A_~A~@[_~A~]" (body-name ?b) 
			      (body-name ?source) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-electric-force-given-unknown  ~%")
   )
  :effects (
            (vector ?b (force ?b ?source electric :time ?t) unknown)
            (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
            (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
            )
  :hint 
  (
   (point (string "Note that ~A and ~A are both charged particles." ?b (?source agent)))
   (point (string "Charged particles experience a force due to other charged particles."))
   (bottom-out (string "Use the force drawing tool to draw the electric force on ~a due to ~a ~a, direction unknown." ?b (?source agent) (?t pp)))
   ))

;; if given E field vector dir
;;    - directly
(defoperator find-electric-force-given-field-dir (?b ?source ?t)
  :preconditions 
  ((rdebug "Using find-electric-force-given-field-dir~%")
   (time ?t)
   ;; make sure force direction not given, directly or via components:
   (not (given (dir (force ?b ?source electric :time ?t-force)) ?dontcare1) 
	(tinsidep ?t ?t-force))
   (not (given (compo ?xy ?rot (force ?b ?source electric :time ?t-force)) 
	       ?dontc2)
	(tinsidep ?t ?t-force))
   ;; make sure field is acting on the particle
   (at-place ?b ?loc :time ?t ?t)
   ;; determine the source of the field
   (given-field ?loc electric ?source :time ?t-given . ?rest)
   (test (tinsidep ?t ?t-given))
   ;; go ahead and draw field, since that is a prerequisite for
   ;; the student figuring out there is a force
   (inherit-vector ?ll (field ?loc electric ?source :time ?t) ?field-dir)
   ;; require sign of charge to be known
   (sign-charge ?b ?pos-neg)
   (bind ?F-dir (if (eq ?pos-neg 'pos) ?field-dir (opposite ?field-dir)))
   )
   :effects (
	     ;; no reaction force since field is not an object
	     (force ?b ?source electric ?t ?F-dir action)
	     ))

(defoperator draw-electric-force-given-field-dir (?b ?source ?t)
  :preconditions 
  (
   (force ?b ?source electric ?t ?F-dir action)
   (test (not (parameter-or-unknownp ?F-dir)))
   ;; found above, make sure this is right situation
   (in-wm (inherit-vector ?ao (field ?loc electric ?source :time ?t) 
			  ?field-dir))
   (in-wm (at-place ?b ?loc :time ?t ?t))
   ;; already found above, use in hints
   (in-wm (sign-charge ?b ?pos-neg))
   (bind ?same-or-opposite (if (eq ?pos-neg 'pos) 'same 'opposite))
   (not (vector ?any-body (force ?b ?source electric :time ?t) ?whatever))
   (bind ?mag-var (format-sym "Fe_~A_~A~@[_~A~]" 
			      (body-name ?b) (body-name ?source)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
:effects (
	  (vector ?b (force ?b ?source electric :time ?t) ?F-dir)
	  (variable ?mag-var (mag (force ?b ?source electric :time ?t)))
	  (variable ?dir-var (dir (force ?b ?source electric :time ?t)))
	  (given (dir (force ?b ?source electric :time ?t)) ?F-dir)
	  )
:hint (
       (point (string "Think about how the direction of the electric force on ~A due to ~a is related to the direction of the electric field vector." ?b (?source agent)))
       (teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
       (bottom-out (string "Because the charge of ~a is ~a, use the force drawing tool (labeled F) to draw the electric force on ~a due to ~a in the ~a direction as the electric field at that location, namely ~A." 
			   ?b (?pos-neg adj) ?b (?source agent) (?same-or-opposite adj) 
			   (?F-dir adj)))
       ))

;;  -if given that unknown field exists 
;;      given by (homogeneous-field ?loc ?type ?source ...) in problem. 
;;      Don't use these if field direction given in other ways
(defoperator find-electric-force-given-field-unknown (?b ?source ?t)
  :preconditions 
  ((rdebug "Using draw-Eforce-unknown ~%")
   (time ?t)
   (homogeneous-field ?loc electric ?source :dir ?dir-e :time ?t-given)
   (test (tinsidep ?t ?t-given)) 
   ;; make sure E-field direction not given, directly or via components
   (test (or (null ?dir-e) (eq ?dir-e 'unknown)))
   ;; make sure force direction not given, directly or via components:
   (not (given (dir (force ?b ?source electric :time ?t)) ?dontcare1))
   (at-place ?b ?loc :time ?t ?t)
   ;; check that something else hasn't defined this force.
   (not (force ?b ?source electric ?t . ?dont-care)) 
   (rdebug "fired draw-Eforce-unknown  ~%")
   )
  :effects (
	    ;; We don't want a reaction-force in this case.
	    (force ?b ?source electric ?t unknown from-field)
	    (force-given-at ?b ?source electric ?t unknown from-field)
         ))

(defoperator draw-electric-force-given-field-unknown (?b ?source ?t)
  :preconditions 
  (
   (force ?b ?source electric ?t unknown from-field)
   (not (vector ?any-b (force ?b ?source electric :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "F_~A_~A~@[_~A~]" (body-name ?b) 
			      (body-name ?source) (time-abbrev ?t)))
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
         (teach (string "In this problem, the exact direction of the electric force vector requires calculation to determine, so you can draw the force vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the electric force on ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?b (?source agent)))
         ))

;;;;---------------------------------------------------------------------------
;;;;
;;;;                   Charge-force-Efield Vector PSM
;;;;
;;;;---------------------------------------------------------------------------

(def-psmclass charge-force-Efield 
    (?eq-type qfe ?axis ?rot (charge-force-Efield ?body ?source ?time)) 
  :complexity major
  :short-name "electric field"
  :english ("the definition of electric field")
  :ExpFormat ("applying the definition of electric field on ~a ~a"
		 (nlg ?body) (nlg ?time 'pp) )
  :EqnFormat ("F_~a = q*E_~a" (axis-name ?axis) (axis-name ?axis)))

(defoperator charge-force-Efield-contains (?sought)
  :preconditions 
  ((rdebug "Using charge-force-Efield-contains  ~%")
   (any-member ?sought ((force ?b ?source electric :time ?t)
		       (field ?loc electric ?source :time ?t)))
   (time ?t)
   ;; make sure ?b (test-charge) is bound in case sought is field at loc
   (at-place ?b ?loc :time ?t ?t)
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
   (any-member ?sought ( (charge ?b :time ?t) ))
   (time ?t)
   (at-place ?b ?loc :time ?t ?t)
   ;; following will fetch the source of an E-field at loc if we are given
   ;; its direction or component value
   (given-field ?loc electric ?source :time ?t-given . ?rest)
   (test (tinsidep ?t ?t-given))
   (rdebug "Firing charge-force-Efield-contains-charge  ~%")
   )
  :effects 
  (
   (eqn-family-contains (charge-force-Efield ?b ?source ?t) ?sought)
   ;; since only one compo-eqn under this vector psm, we can just
   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (charge-force-Efield ?b ?source ?t) qfe ?sought)))


(defoperator draw-charge-force-Efield-diagram (?rot ?b ?source ?t)
  :preconditions 
  (
   (debug "Using draw-charge-force-Efield-diagram ~%")
   (not (vector-diagram ?rot (charge-force-Efield ?b ?source ?t)))
   ;; ?b is "test charge" feeling force at ?loc 
   (body ?b)
   (at-place ?b ?loc :time ?t ?t)
   ;; need source of field
   (inherit-vector ?dontcare (field ?loc electric ?source :time ?t) ?dir1)
   (vector ?b (force ?b ?source electric :time ?t) ?dir2)
   (axes-for ?b ?rot)
   (rdebug "Fired draw-charge-force-Efield-diagram ~%")
   )
  :effects (
            (vector-diagram ?rot (charge-force-Efield ?b ?source ?t))
            ))

(defoperator write-charge-force-Efield-compo (?b ?t ?xy ?rot)
  :preconditions 
  ((debug "Using write-charge-force-Efield-compo ~%")
   (at-place ?b ?loc :time ?t ?t)
   (inherit-variable ?E_x (compo ?xy ?rot (field ?loc electric ?source :time ?t)))
   (inherit-variable ?F_x (compo ?xy ?rot (force ?b ?source electric :time ?t)))
   (inherit-variable ?q (charge ?b :time ?t))
   (rdebug "fired write-charge-force-Efield-compo  ~%")
   )
  :effects (
            (eqn (= ?F_x (* ?q ?E_x))
                 (compo-eqn qfe ?xy ?rot (charge-force-Efield ?b ?source ?t)))
            )
  :hint (
         (point (string "What is the relationship between the force, the charge and the electric field?"))
         (teach ;(kcd "write-charge-force-Efield-compo")
                 (string "The electric field vector at a location is defined as electric force vector per unit charge. That is, the electric force vector that a test charge would undergo if placed there, divided by the size of the charge. This definition can be applied component-wise to relate force and field components."))
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
  :short-name "electric field (magnitude)"
  :english ("the definition of electric field magnitude")
  :ExpFormat ("applying the definition of electric field magnitude at ~a ~a"
		 (nlg ?body) (nlg ?time 'pp) )
  :EqnFormat ("F = abs(q)*E" ))

(defoperator charge-force-Efield-mag-contains (?sought)
  :preconditions 
  (
   ;; because of abs(Q), charge is not a sought
   (any-member ?sought ((mag (force ?b ?source electric :time ?t))
			(mag (field ?loc electric ?source :time ?t))
			))
   (time ?t)  ;in case ?t is not bound
   (at-place ?b ?loc :time ?t ?t)
   (debug "Using & firing write-charge-force-Efield-mag-contains ~%")
   )
  :effects (
           (eqn-contains (charge-force-Efield-mag ?b ?source ?t) ?sought)
           ))  

;; !!! since making charge signed and writing equation with abs, this equation 
;; now can't be inverted to solve for q.  Would need to add further equation 
;; for sign of q, e.g. q = -abs(q), or have a way of registering sign 
;; constraints with algebra module.  Even then, bubble-collection algorithm 
;; doesn't know a further equation is needed.  Might try to put it out here
;; as a sub-equation, but it is not always needed.  Might try as an implicit 
;; equation but then it is optional, and won't be hinted for even when it is 
;; needed.
(defoperator write-charge-force-Efield-mag (?b ?t)
  :preconditions 
  ((debug "Using write-charge-force-Efield-mag ~%")
   (at-place ?b ?loc :time ?t ?t)
   ;; must draw body in diagram for this psm
   (body ?b)
   ;; even though this is scalar equation, want axes to be drawn
   (axes-for ?b ?rot)
   (inherit-variable ?magE (mag (field ?loc electric ?source :time ?t)))
   (variable ?magF (mag (force ?b ?source electric :time ?t)))
   (inherit-variable ?q (charge ?b :time ?t))
   (rdebug "fired write-charge-force-Efield-mag  ~%")
   )
  :effects 
  (
   (eqn (= ?magF (* (abs ?q) ?magE)) (charge-force-Efield-mag ?b ?source ?t))
   (assume using-magnitude (charge-force-Efield ?b ?source ?t)) ;mag xor compos
   )
  :hint 
  (
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
  :preconditions 
  (
   (any-member ?sought ((dir (force ?b ?source electric :time ?t))
			(dir (field ?loc electric ?source :time ?t))
			(charge ?b :time ?t)))
   (time ?t) ;in case ?t is not bound
   (at-place ?b ?loc :time ?t ?t)
   (rdebug "Using & firing write-charge-force-Efield-dir-contains ~%")
   )
  :effects(
           (eqn-contains (charge-force-Efield-dir ?b ?source ?t) ?sought)
           ))

(defoperator write-charge-force-Efield-dir-pos (?b ?t)
  :preconditions 
  ((debug "Using write-charge-force-Efield-dir ~%")
   (sign-charge ?b pos)
   (at-place ?b ?loc :time ?t ?t)
   (inherit-variable ?dirE (dir (field ?loc electric ?source :time ?t)))
   (variable ?dirF (dir (force ?b ?source electric :time ?t)))
   (inherit-variable ?q (charge ?b :time ?t))
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
|#

;;;;--------------------------------------------
;;;;                      point-charge-Efield Vector PSM 
;;;;--------------------------------------------

(def-psmclass point-charge-Efield 
  (?eq-type qpe ?axis ?rot (point-charge-Efield ?body ?loc ?time ?form)) 
  :complexity major
  :short-name ("point charge field (~A component)" (axis-name ?axis))
  :english ("the formula for electric field due to a point charge")
  :ExpFormat ("calculating the electric field at ~A due to ~a"
	      (nlg ?loc) (nlg ?body) )
  :EqnFormat ("E_~a = (kelec*q/r^2) * r_~a/r" 
	      (axis-name ?axis) (axis-name ?axis)))

(defoperator point-charge-Efield-contains (?sought)
  :preconditions 
  ((rdebug "Using point-charge-Efield-compo-contains  ~%")
   (any-member ?sought ((field ?loc electric ?b :time ?t)
			(charge ?b :time ?t)
			(relative-position ?loc ?b :time ?t)
		       ))
   ;; only *needed* if ?loc is not bound (when charge is ?sought)
   (field-sources ?loc electric ?sources :time ?t ?t)
   (any-member ?b ?sources) 
   (time ?t)  ;sometimes ?t is not bound
   (any-member ?form (nil t)) ;switch between forms of r-hat
   (point-charge ?b)    
   (rdebug "Firing point-charge-Efield-compo-contains  ~%")
   )
  :effects 
  (
   (eqn-family-contains (point-charge-Efield ?b ?loc ?t ?form) ?sought)
   ;; since only one compo-eqn under this vector psm, we can just
   ;; select it now, her than requiring further operators to do so
   (compo-eqn-contains (point-charge-Efield ?b ?loc ?t ?form) qpe ?sought)))

(defoperator draw-point-charge-Efield-diagram (?rot ?b ?loc ?t)
  :preconditions 
  (
   (rdebug "Using draw-point-charge-Efield-diagram ~%")
   (not (vector-diagram ?rot (point-charge-Efield ?b ?loc ?t)))
   ;; ?b is point charge source of field at ?loc
   (body ?b)
   (inherit-vector ?dontcare1 (field ?loc electric ?b :time ?t) ?dir1) 
   (vector ?dontcare2 (relative-position ?loc ?b :time ?t) ?dir2)
   (axes-for ?b ?rot)
   (rdebug "Fired draw-point-charge-Efield-diagram ~%")
   )
  :effects (
            (vector-diagram ?rot (point-charge-Efield ?b ?loc ?t ?form))
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


(defoperator write-point-charge-Efield-compo (?b ?loc ?t ?xy ?rot ?form)
  :preconditions 
  (
   (rdebug "Using write-point-charge-Efield-compo ~%")
   ;; make sure r-hat compo doesn't vanish
   ;; For vanishing components, the projection equations should be used
   (in-wm (vector ?whatever (relative-position ?loc ?b :time ?t) ?r-dir))
   (test (non-zero-projectionp ?r-dir ?xy ?rot))
   ;; b is point-charge source of field
   (inherit-variable ?E_x (compo ?xy ?rot (field ?loc electric ?b :time ?t)))
   (inherit-variable ?q (charge ?b :time ?t))
   (variable ?r (mag (relative-position ?loc ?b :time ?t)))
   (hat ?rhat-compo (relative-position ?loc ?b :time ?t) ?xy ?rot ?form)
   (rdebug "fired write-point-charge-Efield-compo  ~%")
   )
  :effects 
  (
   (eqn (= ?E_x (* (/ (* |kelec| ?q) (^ ?r 2)) ?rhat-compo))
	(compo-eqn qpe ?xy ?rot (point-charge-Efield ?b ?loc ?t ?form)))
    )
  :hint 
  (
   (teach (string "The electric field due to a point charge is directly proportional to the charge and inversely proportional to the square of the distance from the point charge. The constant of proportionality can be written in Andes as kelec. To compute a component of the electric field, multiply the magnitude by the cos or sin of the angle of the relative position of the location from the point charge, using cos for x components and sin for y components."))
   (bottom-out (string "Write the equation ~A"  
		       ((= ?E_x (* (/ (* |kelec| ?q) (^ ?r 2)) ?rhat-compo)) 
			algebra)))
   ))

;; mag, dir forms of point-charge-Efield

(def-psmclass point-charge-Efield-mag
             (point-charge-Efield-mag ?body ?loc ?time) 
  :complexity major
  :short-name "point charge field (magnitude)"
  :english ("the formula for the magnitude of the electric field due to a point charge")
  :ExpFormat ("applying the formula for the magnitude of the electric field due to a point charge to ~a ~a"
		 (nlg ?loc) (nlg ?time 'pp) )
  :EqnFormat ("E = kelec*abs(q)/r^2" ))

(defoperator point-charge-Efield-mag-contains (?sought)
  :preconditions 
  ((rdebug "Using point-charge-Efield-mag-contains ~%")
   ;; charge can't be sought, since absolute value is taken
   (any-member ?sought ((mag (field ?loc electric ?b :time ?t))
			(mag (relative-position ?loc ?b :time ?t))))
   (time ?t)
   (point-charge ?b)
   (rdebug "Firing point-charge-Efield-mag-contains ~%")
   )
  :effects(
           (eqn-contains (point-charge-Efield-mag ?b ?loc ?t) ?sought)
           ))

(defoperator write-point-charge-Efield-mag (?b ?loc ?t)
  :preconditions 
  ((debug "Using write-point-charge-Efield-mag ~%")
   ;; need to draw body for this psm. 
   ;; ?b is point-charge source of field
   (body ?b)
   ;; need to allow axes for this scalar psm. 
   (axes-for ?b 0) ; use standard axes only
   (inherit-variable ?magE (mag (field ?loc electric ?b :time ?t)))
   (inherit-variable ?q (charge ?b :time ?t))
   (variable ?r (mag (relative-position ?loc ?b :time ?t)))
   (rdebug "fired write-point-charge-Efield-mag  ~%")
   )
  :effects 
  (
   (eqn (= ?magE (/ (* |kelec| (abs ?q)) (^ ?r 2) ))
	(point-charge-Efield-mag ?b ?loc ?t))
    ;; match both forms of vector version of equation
   (assume using-magnitude (point-charge-Efield ?b ?loc ?t t))
   (assume using-magnitude (point-charge-Efield ?b ?loc ?t nil))
   )
  :hint (
         (point (string "What is the equation for the magnitude of the electric field due to a point charge?"))
         (teach (kcd "write-point-charge-force-Efield-mag")
                (string "Write the definition of the magnitude of the electric field due to a point charge in terms of the charge and the distance to the point."))
         (bottom-out (string "Write the equation ~A"
	                   ( (= ?magE (/ (* |kelec| (abs ?q)) (^ ?r 2) )) algebra)))
          ))


(defoperator write-point-charge-Efield-dir-pos (?b ?loc ?t)
  :preconditions 
  ((debug "Using write-point-charge-Efield-dir-pos ~%")
   ;; following was used to distinguish point-charge problem:
   ;; location of source body mentioned in givens
   (point-charge ?b)
   (sign-charge ?b pos)
   (inherit-variable ?dirE (dir (field ?loc electric ?b :time ?t)))
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
		  (point-charge ?b)
                  (sign-charge ?b neg)
		  (inherit-variable ?dirE (dir (field ?loc electric ?b :time ?t)))
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
  :preconditions 
  ((rdebug "sign-on-charge ~%")                   
   (given (compo ?xyz ?rot (force ?b ?source electric :time ?t)) 
	  (dnum ?val1 ?units1))
   (given (compo ?xyz ?rot (field ?loc electric ?source :time ?t ?t)) 
	  (dnum ?val2 ?units2))
   (test (not (or (= ?val1 0) (= ?val2 0))))
   (bind ?sign (if (> (* ?val1 ?val2) 0) 'pos 'neg))
   (rdebug "sign-on-charge~%")
   )
  :effects (
            (sign-charge ?b ?sign) 
            ))


;; Scalar variable definitions:

(defoperator timeless-charge (?what ?t)
  :preconditions 
  ((test (not (member 'changing-voltage (problem-features *cp*))))
   (time ?t))
  :effects 
  ((inherit-quantity (charge ?what :time ?t) (charge ?what))))

(defoperator define-charge (?p ?t)
  :preconditions 
  (
   ;; Sanity test for inherit-quantity working OK
   (test (or (eq (null ?t) 
		 (null (member 'changing-voltage (problem-features *cp*))))
	     (error "define-charge bad time slot ~A" ?t)))
   (bind ?q-var (format-sym "Q~@[s~]_~A~@[_~A~]" ?surface-flag (body-name ?p) 
			    (time-abbrev ?t)))
   )
  :effects (
            (variable ?q-var (charge ?p :surface ?surface-flag :time ?t))
	    (define-var (charge ?p :surface ?surface-flag :time ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Charge."  
			   ((charge ?p :surface ?surface-flag :time ?t) def-np) ))
       ))

(defoperator define-number-of (?p)
  :preconditions ((bind ?q-var (format-sym "NN_~A" (body-name ?p))))
  :effects ((variable ?q-var (number-of ?p))
	    (define-var (number-of ?p)))
   :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Number."  
			   ((number-of ?p) def-np) ))
       ))

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
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Potential."  ((potential ?loc ?source :time ?t) def-np)))
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
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Potential."  
			   ((net-potential ?loc :time ?t) def-np)))
       ))

;;-----------------------------------------------------------
;; Superposition principle for electric or magnetic fields:
;;   Enet_x = E1_x + E2_x + ...
;;-----------------------------------------------------------

(def-psmclass net-field-electric 
  (?eq-type definition ?axis ?rot (net-field ?loc electric ?time))
  :complexity major
  :short-name ("net electric field (~A component)" (axis-name ?axis))
  :english ("the definition of net electric field")
  :ExpFormat ("calculating the net electric field at ~a ~a" 
	      (nlg ?loc) (nlg ?time 'pp))
  :EqnFormat ("Enet_~a = E1_~a + E2_~a + ..." (axis-name ?axis) 
	      (axis-name ?axis) (axis-name ?axis)))

(def-psmclass net-field-magnetic 
  (?eq-type definition ?axis ?rot (net-field ?loc magnetic ?time))
  :complexity major
  :short-name ("net magnetic field (~A component)" (axis-name ?axis))
  :english ("the definition of net magnetic field")
  :ExpFormat ("calculating the net magnetic field at ~a ~a" 
	      (nlg ?loc) (nlg ?time 'pp))
  :EqnFormat ("Bnet_~a = B1_~a + B2_~a + ..." (axis-name ?axis) 
	      (axis-name ?axis) (axis-name ?axis)))

(defoperator net-field-contains (?sought)
 :preconditions 
 (
  ;; this may end up timeless
  (any-member ?sought ((net-field ?loc ?type :time ?t)
		       ;; need to choose ?loc to apply at when sought is field
		       ;; due to some source.  Ignore this case for now.
		       (field ?loc ?type ?source :time ?t)
		 ))
  ;; Must make sure don't include source at loc. We will filter for this
  ;; when we write the equation.
  )
 :effects (
	   (eqn-family-contains (net-field ?loc ?type ?t) ?sought)
	   ;; since only one compo-eqn under this vector psm, we can just
	   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (net-field ?loc ?type ?t) definition ?sought)
  ))

(defoperator draw-net-field-diagram (?rot ?loc ?type ?t)
 :preconditions 
 (
  ;; list of sources from problem statement
  (in-wm (field-sources ?loc ?type ?sources :time ?t ?t))
  ;; draw field vectors
  (foreach ?source ?sources
	   (vector ?source-body (field ?loc ?type ?source :time ?t) ?dir))
  (vector ?field-axis-owner (net-field ?loc ?type :time ?t) ?dir-net)
  (axes-for ?field-axis-owner ?rot)
  )
 :effects (
    (vector-diagram ?rot (net-field ?loc ?type ?t))
 ))

(defoperator write-net-field-compo (?loc ?type ?t ?xy ?rot)
 :preconditions (
   (variable ?Fnet_x (compo ?xy ?rot (net-field ?loc ?type :time ?t)))
   (in-wm (field-sources ?loc ?type ?sources :time ?t ?t))
   (map ?source ?sources 
   	(variable ?compo-var (compo ?xy ?rot 
				    (field ?loc ?type ?source :time ?t)))
	?compo-var ?Fi_x)
  )
  :effects (
    (eqn (= ?Fnet_x (+ . ?Fi_x))
                 (compo-eqn definition ?xy ?rot (net-field ?loc ?type ?t)))
    )
  :hint (
    (point (string "The net ~A field at a point can be computed from the fields produced at that point by each of the field sources." 
		   (?type adj)))
    (teach (string "The principle of superposition states that the net ~A field at a point is the vector sum of the ~A fields due to each of the individual sources. This relation can be applied component-wise to calculate the components of the net ~A field due to all sources."
		   (?type adj) (?type adj) (?type adj)))
    (bottom-out (string "Write the equation ~A" 
			((= ?Fnet_x (+ . ?Fi_x)) algebra) ))
  ))

;; see calculate-net-force-dir-from-forces
(defoperator calculate-net-field-dir-from-fields (?loc ?type ?t)
  :preconditions 
  (
   (time-or-timeless ?t)
   ;; find all fields that are acting at ?loc, currently
   ;; this is done by hand in the problem definition
   ;; (field-sources ?loc ?type ?sources :time ?t ?t)
   ;; Instead, we use vector drawing to get the set of fields:
   (setof
	(vector ?body-axis (field ?loc ?type ?source :time ?t) ?dir) 
	  ?dir ?dirs)
   ;; if all fields have same direction, return direction
   (bind ?net-dir (cond
		   ((= (length ?dirs) 1) (first ?dirs))
		   ((every #'z-dir-spec ?dirs) 'z-unknown)
		   ((notany #'z-dir-spec ?dirs) 'unknown)
		   (t (error "can't have mixed vectors ~A~%" ?dirs))))
   ;; (test (progn (format t "calculate-net-field-dir-from-fields ~A net:  ~A~%" ?dirs ?net-dir) t))
   )
:effects ((net-field-dir ?loc ?type ?t ?net-dir)))


;;; drawing net fields

; draw net field in given direction:
(defoperator draw-net-field-given-dir (?loc ?type ?t)
  :preconditions 
  (
   ;; ?t may be timeless, but it does get bound
   (given (dir (net-field ?loc ?type :time ?t)) ?dir-f)  
   (test (not (eq ?dir-f 'zero)))
   (not (vector ?whatever (net-field ?loc ?type :time ?t) ?any-dir)) 
   (bind ?mag-var (format-sym "~A_~A~@[_~A~]" (subseq (string ?type) 0 1) 
			      (body-name ?loc) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   ;; if dir is z-axis, implicit eqn should give phi angle value
   (bind ?angle-value (if (z-dir-spec ?dir-f) (zdir-phi ?dir-f) 
			?dir-f)))
  :effects 
  (
   (vector ?loc (net-field ?loc ?type :time ?t) ?dir-f)
   (variable ?mag-var (mag (net-field ?loc ?type :time ?t)))
   (variable ?dir-var (dir (net-field ?loc ?type :time ?t)))
   ;; Because dir is problem given, find-by-psm won't ensure implicit eqn
   ;; gets written.  Given value may not be used elsewhere so ensure it here.
   (implicit-eqn (= ?dir-var ?angle-value) 
		 (dir (net-field ?loc ?type :time ?t)))
   )
  :hint (
	 (point (string "You can determine the direction of the net ~A field at ~a from the problem statement."
	                (?type adj) ?loc)) 
	 (bottom-out (string "Use the ~A field drawing tool to draw the net ~A field at ~a in the given direction of ~A." 
			     (?type adj) (?type adj) ?loc (?dir-f adj)))
	 )) 

(defoperator draw-net-field-given-zero (?type ?t)
  :preconditions 
  (
   (given (dir (net-field ?loc ?type :time ?t)) zero)  
   (not (vector ?any-body (net-field ?loc ?type :time ?t) ?dir1))     
   (bind ?mag-var (format-sym "B_~A~@[_~A~]" (body-name ?loc) 
			      (time-abbrev ?t)))
   )
  :effects 
  (
   (vector ?loc (net-field ?loc ?type :time ?t) zero)
   (variable ?mag-var (mag (net-field ?loc ?type :time ?t)))
   )
  :hint (
	 (point (string "At the point ~A, the ~A fields add up to zero."
	                ?loc (?type adj))) 
	 (bottom-out (string "Use the ~A field drawing tool to draw a zero-length vector for the net ~A field at ~a ." 
			     (?type adj) (?type adj) ?loc))
	 ))

(defoperator draw-net-field-from-fields (?loc ?type ?t)
  :preconditions 
  (
   ;; the direction is not explicitly given.
   (not (given (dir (net-field ?loc ?type :time ?t)) ?val))
   (net-field-dir ?loc ?type ?t ?net-dir)  ;calculate net field direction
   (test (not (member ?net-dir '(unknown z-unknown))))
   (bind ?z-axis (known-z-dir-spec ?net-dir))
   (not (vector ?whatever (net-field ?loc ?type :time ?t) ?any-dir)) 
   (bind ?mag-var (format-sym "B_~A~@[_~A~]" (body-name ?loc) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   ;; if dir is z-axis, implicit eqn should give phi angle value
   (bind ?angle-value (if (z-dir-spec ?net-dir) (zdir-phi ?net-dir) 
			?net-dir))
    ;; (test (progn (format t "draw-net-field-from-fields at ~A dir ~A~%" ?loc ?net-dir) t))
  )
  :effects 
  (
   (vector ?loc (net-field ?loc ?type :time ?t) ?net-dir)
   (variable ?mag-var (mag (net-field ?loc ?type :time ?t)))
   (variable ?dir-var (dir (net-field ?loc ?type :time ?t)))
   ;; Because dir is problem given, find-by-psm won't ensure implicit eqn
   ;; gets written.  Given value may not be used elsewhere so ensure it here.
   (implicit-eqn (= ?dir-var ?angle-value) 
		 (dir (net-field ?loc ?type :time ?t)))
   )
  :hint (
	 (point (string "Since the ~A field at ~A due to each source is pointing in the same direction, the direction of the net ~A field is known."
	                (?type adj) ?loc (?type adj))) 
	 (bottom-out (string "Use the ~A field drawing tool to draw the net ~A field at ~a in the direction ~A." 
			     (?type adj) (?type adj) ?loc (?net-dir adj)))
	 )) 

(defoperator draw-net-field-unknown (?loc ?type ?t)
 :preconditions 
 (
  ;; presume the direction is unknown -- not given.
  (not (given (dir (net-field ?loc ?type :time ?t)) ?val))
  (net-field-dir ?loc ?type ?t ?net-dir)  ;calculate net field direction
  ;; test that it is unknown
  (any-member ?net-dir (unknown z-unknown))
  (bind ?xy-plane (eq ?net-dir 'unknown))
  (bind ?mag-var (format-sym "~Anet_~A~@[_~A~]" 
			     (subseq (string ?type) 0 1)
			     (body-name ?loc) (time-abbrev ?t)))
  (bind ?dir-var (format-sym "O~A" ?mag-var))
  ;; (test (progn (format t "draw-net-field-unknown at ~A dir ~A~%" ?loc ?net-dir) t))
  )
  :effects 
  (
   (vector ?loc (net-field ?loc ?type :time ?t) ?net-dir)
   (variable ?mag-var (mag (net-field ?loc ?type :time ?t)))
   (variable ?dir-var (dir (net-field ?loc ?type :time ?t)))
   )
  :hint (
         (point (string "You know there is a net ~A field at ~A." 
			(?type adj) ?loc))
         (teach (string "In this problem, the exact direction of the net ~A field vector requires calculation to determine.  ~:[However, you do know that it lies along the z-axis.~;  Draw the vector at an approximately correct angle and leave the exact angle unspecified.~]"
			(?type adj) (?xy-plane identity)))
         (bottom-out (string "Draw the net ~A field at ~a, then ~:[select Unknown Z direction in the dialog box~;erase the number in the direction slot to indicate that the exact direction is not being specified~]." 
			     (?type adj) ?loc (?xy-plane identity)))
  ))

;;--------------------------------------------------------------------------
;; Electric potential
;;--------------------------------------------------------------------------

(def-psmclass point-charge-potential (point-charge-potential ?body ?loc ?time)
  :complexity major
  :short-name "point charge potential"
  :english ("the formula for the electric potential due to a point charge")
  :ExpFormat ("calculating the electric potential at ~a due to ~a"
		 (nlg ?loc) (nlg ?body))
  :EqnFormat ("V = kelec*q/r" ))

(defoperator point-charge-potential-contains (?sought)
  :preconditions (
  (any-member ?sought ( (potential ?loc ?body :time ?t)
                        (mag (relative-position ?loc ?body :time ?t))
			; if sought is charge, have to choose a location
			; for now, just don't apply for charge
                        ;(charge ?body :time ?t)
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
     (inherit-variable ?q (charge ?body :time ?t))
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


;;
;; Addition principle for net electric potential
;;   Vnet = V1+ V2 + ...
;;
;; Note we only apply this if there is more than one source.
;;
(def-psmclass net-potential (net-potential ?loc ?time)
  :complexity major  ; want this in case it is the top psm on a problem
  :short-name "net potential"
  :english ("the definition of net electric potential")
  :ExpFormat ("calculating the net electric potential from all sources at ~a~a"
		 (nlg ?loc) (nlg ?time 'pp) )
  :EqnFormat ("Vnet = V1 + V2 + ..." ))

(defoperator net-potential-contains (?sought)
  :preconditions ( (any-member ?sought ((net-potential ?loc :time ?t)
			 (potential ?loc ?source :time ?t)) ) )
  :effects ( (eqn-contains (net-potential ?loc ?t) ?sought) ))

(defoperator write-net-potential (?loc ?t)
  :preconditions (
     (variable ?Vnet (net-potential ?loc :time ?t))
     (in-wm (field-sources ?loc electric ?sources :time ?t ?t))
     (map ?source ?sources 
   	(variable ?V-var (potential ?loc ?source :time ?t))
	?V-var ?Vi)
  )
  :effects (
    (eqn (= ?Vnet (+ . ?Vi)) (net-potential ?loc ?t))
  )
  :hint (
    (teach (string "The net electric potential at a point, a scalar, is the sum of the potentials at that point due to each of the sources." ))
    (bottom-out (string "Write the equation ~A" ((= ?Vnet (+ . ?Vi)) algebra) ))
  ))

;
; electric potential energy Ue = q*Vnet
;
(def-psmclass electric-energy (electric-energy ?body ?source ?time)
  :complexity major
  :short-name "electric potential energy"
  :english ("the formula for the electric potential energy")
  :ExpFormat ("calculating the electric potential energy of ~a"
		 (nlg ?body))
  :EqnFormat ("U = q*Vnet" ))

(defoperator electric-energy-contains (?sought)
  :preconditions (
    (any-member ?sought ((electric-energy ?body ?source :time ?t) 
                         (net-potential ?loc :time ?t)
			 (charge ?body :time ?t)
			 ))
    (time ?t)
    ; if sought is net-potential, must bind body: 
    (in-wm (at-place ?body ?loc :time ?t ?t))
    ; if sought is not energy, must bind source for energy quantity
    ; This will be single named source if known, else "electric_field" if more 
    ; than one source or unspecified.
    (in-wm (field-sources ?loc electric ?sources :time ?t ?t))
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
     (in-wm (at-place ?body ?loc :time ?t ?t))
     (variable ?Ue (electric-energy ?body ?source :time ?t))
     (inherit-variable ?q (charge ?body :time ?t))
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
    (teach (string "The electric potential energy of a body is equal to the charge on that body times the electric potential at its location." ))
    (bottom-out (string "Write the equation ~A" ((= ?Ue (* q ?Vnet)) algebra) ))
  ))

;; To interact with cons-energy psm: op to tell it that electric pe 
;; exists in this problem by defining a variable when needed
(defoperator define-electric-ee-var (?b ?source ?t)
  :preconditions 
  ( ;; need to know electric field exists in problem
   (at-place ?b ?loc :time ?t ?t)
   (in-wm (field-sources ?loc electric ?sources :time ?t ?t))
   ;; need to bind source. See electric-energy-contains above
   (bind ?source (cond ((and (null (cdr ?sources))
			     (not (eq (car ?sources) 'unspecified))) 
			(car ?sources))
		       (T 'electric_field)))
   )
  :effects ( (ee-var ?b ?t (electric-energy ?b ?source :time ?t)) ))

;;;;---------------------------------------------------------------------------
;;;;
;;;;                   Electric & Magnetic dipole moment
;;;;
;;;;---------------------------------------------------------------------------

(def-qexp electric-dipole-moment (dipole-moment ?dipole electric :time ?time)
  :units |C.m|
  :english ("electric dipole moment of ~A~@[ ~A~]" 
	    (nlg ?dipole) (nlg ?time 'pp)))

(def-qexp magnetic-dipole-moment (dipole-moment ?dipole magnetic :time ?time)
  :units |C.m^2/s|
  :english ("magnetic dipole moment of ~A~@[ ~A~]" 
	    (nlg ?dipole) (nlg ?time 'pp)))

;; modification of draw-efield-vector
(defoperator draw-Dipole-Moment-given-dir (?dipole ?t)
  :preconditions 
  ((rdebug "Using draw-Dipole-Moment-vector  ~%")
   (time ?t)
   (given (dir (dipole-moment ?dipole ?type :time ?t-given)) ?dir)
   (test (tinsidep ?t ?t-given))  
   (not (vector ?dipole (dipole-moment ?dipole ?type :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "~A_~A~@[_~A~]" (if (eq ?type 'electric) 'P 'mu)
			      (body-name ?dipole) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?angle-value (if (z-dir-spec ?dir) (zdir-phi ?dir) ?dir))
   (rdebug "fired draw-Dipole-Moment-vector   ~%")
   )
  :effects (
	   (vector ?dipole (dipole-moment ?dipole ?type :time ?t) ?dir)
	   (variable ?mag-var (mag (dipole-moment ?dipole ?type :time ?t)))
	   (variable ?dir-var (dir (dipole-moment ?dipole ?type :time ?t)))
	   ;; Because dir is problem given, find-by-psm won't ensure implicit 
	   ;; eqn gets written.  Given value may not be used elsewhere so 
	   ;; ensure it here.
	   (implicit-eqn (= ?dir-var ?angle-value) 
			 (dir (dipole-moment ?dipole ?type :time ?t)))
	   )  
  :hint
  ((point (string "The problem specifies the direction of the ~A dipole moment of ~a ~a." 
		  (?type adj) ?dipole (?t pp)))
    (bottom-out (string "Use the vector drawing tool to draw the ~A dipole moment of ~a ~a oriented in the direction ~a." 
			(?type adj) ?b (?t pp) ?dir))
    ))

;; modification of draw-efield-vector
(defoperator draw-Electric-Dipole-Moment-given-relative-position (?dipole ?t)
  :preconditions 
  ((rdebug "Using draw-Electric-Dipole-Moment-vector  ~%")
   (time ?t)
   (electric-dipole ?dipole ?positive-charge ?negative-charge)
   (given (dir (relative-position ?positive-charge ?negative-charge 
				  :time ?t-given)) ?dir)
   (test (tinsidep ?t ?t-given))  
   (not (given (dir (dipole-moment ?dipole electric :time ?t-mom)) ?mom-dir)
	(tinsidep ?t ?t-mom))
   (not (vector ?dipole (dipole-moment ?dipole electric :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "P_~A~@[_~A~]" (body-name ?dipole) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?angle-value (if (z-dir-spec ?dir) (zdir-phi ?dir) ?dir))
   (rdebug "fired draw-Electric-Dipole-Moment-vector   ~%")
   )
  :effects (
	   (vector ?dipole (dipole-moment ?dipole electric :time ?t) ?dir)
	   (variable ?mag-var (mag (dipole-moment ?dipole electric :time ?t)))
	   (variable ?dir-var (dir (dipole-moment ?dipole electric :time ?t)))
	   ;; Because dir is problem given, find-by-psm won't ensure implicit 
	   ;; eqn gets written.  Given value may not be used elsewhere so 
	   ;; ensure it here.
	   (implicit-eqn (= ?dir-var ?angle-value) 
			 (dir (dipole-moment ?dipole electric :time ?t)))
	   (given (dir (dipole-moment ?dipole electric :time ?t)) ?dir)
	   ) 
  :hint (
	 (point (string "You were given the position of ~A relative to ~A.  What does this tell you about the electric dipole moment?" ?positive-charge ?negative-charge))
	 (teach (string "The dipole moment of a pair of charges is in the same direction as a vector starting at the negative charge and going to the positive charge."))
         (bottom-out (string "Use the electric dipole moment drawing tool (labeled P) to draw the electric dipole moment of ~a in the given direction of ~A." 
			     ?dipole ?dir))
         ))

(defoperator draw-Magnetic-Dipole-Moment-given-unit-vector (?current-loop ?t)
  :preconditions 
  ((rdebug "Using draw-Magnetic-Dipole-Moment-vector  ~%")
   (time ?t)
   (magnetic-dipole ?current-loop ?surface)
   (given (dir (unit-vector normal-to ?surface :time ?t-given)) ?dir)
   (test (tinsidep ?t ?t-given))  
   (not (given (dir (dipole-moment ?current-loop magnetic :time ?t-mom)) 
	       ?mom-dir) (tinsidep ?t ?t-mom))
   (not (vector ?surface (dipole-moment ?current-loop magnetic :time ?t) 
		?any-dir))
   (bind ?mag-var (format-sym "mu_~A~@[_~A~]" (body-name ?current-loop) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?angle-value (if (z-dir-spec ?dir) (zdir-phi ?dir) ?dir))
   (rdebug "fired draw-Magnetic-Dipole-Moment-vector   ~%")
   )
  :effects 
  (
   (vector ?surface (dipole-moment ?current-loop magnetic :time ?t) ?dir)
   (variable ?mag-var (mag (dipole-moment ?current-loop magnetic :time ?t)))
   (variable ?dir-var (dir (dipole-moment ?current-loop magnetic :time ?t)))
   ;; Because dir is problem given, find-by-psm won't ensure implicit 
   ;; eqn gets written.  Given value may not be used elsewhere so 
   ;; ensure it here.
   (implicit-eqn (= ?dir-var ?angle-value) 
		 (dir (dipole-moment ?current-loop magnetic :time ?t)))
   (given (dir (dipole-moment ?current-loop magnetic :time ?t)) ?dir)
   )  
  :hint 
  ( (point (string "What is the direction of ~A?  What does this tell you about the magnetic dipole moment?" 
		   ((unit-vector normal-to ?surface :time ?t) def-np)))
    (teach (string "The magnetic dipole moment vector for a loop of current is given by the following right hand rule:  the fingers curl around the loop in the direction of the current and the extended thumb points in the direction of $m."))
    (bottom-out (string "Use the magnetic dipole moment drawing tool (labeled $m) to draw the magnetic dipole moment of ~a in the given direction of ~A." 
			?current-loop ?dir))
    ))

;;;             The electric dipole moment of two charges

;; The following is a modification of charge-force-efield.

(def-psmclass electric-dipole-moment 
    (?eq-type definition ?axis ?rot (dipole-moment ?dipole electric ?time)) 
  :complexity major
  :short-name ("electric dipole moment (~A component)" (axis-name ?axis))
  :english ("the electric dipole moment of two charges")
  :ExpFormat ("finding electric dipole moment for ~a ~a"
		 (nlg ?dipole) (nlg ?time 'pp) )
  :EqnFormat ("p_~a = q*r_~a" (axis-name ?axis) (axis-name ?axis)))

(defoperator electric-dipole-moment-contains (?sought)
  :preconditions 
  ((rdebug "Using electric-dipole-moment-contains  ~%")
   (electric-dipole ?dipole ?positive-charge ?negative-charge)
   (time ?t)
   (any-member ?sought ((relative-position ?positive-charge
					       ?negative-charge :time ?t)
		       (dipole-moment ?dipole electric :time ?t)
		       (charge ?positive-charge) ;should be timeless
		       (charge ?negative-charge) ;should be timeless
		       ))
   (rdebug "Firing electric-dipole-moment-contains  ~%")
   )
  :effects 
  (
   (eqn-family-contains (dipole-moment ?dipole electric ?t) ?sought)
   ;; since only one compo-eqn under this vector psm, we can just
   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (dipole-moment ?dipole electric ?t) definition ?sought)))
(defoperator draw-electric-dipole-moment-diagram (?rot ?dipole ?t)
  :preconditions 
  (
   (debug "Using draw-electric-dipole-moment-diagram ~%")
   (not (vector-diagram ?rot (dipole-moment ?dipole electric ?t)))
   (electric-dipole ?dipole ?positive-charge ?negative-charge)
   ;; may draw charges in diagram for this psm
   (optional (body ?positive-charge))
   (optional (body ?negative-charge))
   (vector ?dipole (dipole-moment ?dipole electric :time ?t) ?dir1) 
   (vector ?positive-charge (relative-position 
			     ?positive-charge 
			     ?negative-charge :time ?t) ?dir1) 
   (axes-for ?dipole ?rot)
   (rdebug "Fired draw-electric-dipole-moment-diagram ~%")
   )
  :effects (
            (vector-diagram ?rot (dipole-moment ?dipole electric ?t))
            ))

(defoperator write-electric-dipole-moment-compo (?dipole ?t ?xy ?rot)
  :preconditions 
  ((debug "Using write-electric-dipole-moment-compo ~%")
   (electric-dipole ?dipole ?positive-charge ?negative-charge)
   (variable ?p_x  (compo ?xy ?rot (dipole-moment ?dipole electric :time ?t)))
   (variable ?d_x  (compo ?xy ?rot (relative-position 
				    ?positive-charge 
				    ?negative-charge :time ?t)))
   (variable ?qp (charge ?positive-charge))
   (variable ?qn (charge ?negative-charge))
   (rdebug "fired write-electric-dipole-moment-compo  ~%")
   )
  :effects 
  ( (eqn (= ?p_x (* ?qp ?d_x))
	 (compo-eqn definition ?xy ?rot (dipole-moment ?dipole electric ?t)))
    ;; allows (forces?) student to define both charges
    ;; note this equation is timeless
    (implicit-eqn (= (+ ?qp ?qn) 0) 
		  (electric-dipole-moment-balance ?dipole))
    )
  :hint 
  ( (point (string "What is the electric dipole moment for two charges?"))
    (teach ;(kcd "write-electric-dipole-moment-compo")
     (string "The electric dipole moment of a +q -q pair of charges is the charge q times a vector going from -q to +q."))
    (bottom-out (string "Write the equation ~a" ((= ?p_x (* ?qp ?d_x)) algebra)))
    ))

;;  Magnitude of the above equation

(def-psmclass electric-dipole-moment-mag 
             (electric-dipole-moment-mag ?dipole ?time) 
  :complexity major
  :short-name "electric dipole moment (magnitude)"
  :english ("the magnitude of the electric dipole moment of two charges")
  :ExpFormat ("finding the magnitude of the electric dipole moment of ~a ~a"
		 (nlg ?dipole) (nlg ?time 'pp) )
  :EqnFormat ("p = abs(q)*r" ))

(defoperator electric-dipole-moment-mag-contains (?sought)
  :preconditions 
  (
   (electric-dipole ?dipole ?positive-charge ?negative-charge)
   ;; because of abs(Q), charge is not a sought
   (any-member ?sought ((mag (relative-position ?positive-charge ?negative-charge :time ?t))
			(mag (dipole-moment ?dipole electric :time ?t))
			))
   (time ?t)
   (debug "Using & firing write-electric-dipole-moment-mag-contains ~%")
   )
  :effects (
           (eqn-contains (electric-dipole-moment-mag ?dipole ?t) ?sought)
           ))  

(defoperator write-electric-dipole-moment-mag (?dipole ?t)
  :preconditions 
  ((debug "Using write-electric-dipole-moment-mag ~%")
   (electric-dipole ?dipole ?positive-charge ?negative-charge)
   ;; may draw body in diagram for this psm
   (optional (body ?positive-charge))
   (optional (body ?negative-charge))
   ;; even though this is scalar equation, want axes to be allowed
   (axes-for ?dipole ?rot)
   (variable ?magP (mag (dipole-moment ?dipole electric :time ?t)))
   (variable ?magd (mag (relative-position ?positive-charge 
					   ?negative-charge :time ?t)))
   (inherit-variable ?qp (charge ?positive-charge :time ?t))
   (inherit-variable ?qn (charge ?negative-charge :time ?t))
   (rdebug "fired write-electric-dipole-moment-mag  ~%")
   )
  :effects 
  (
   (eqn (= ?magP (* (abs ?qp) ?magd)) 
	(electric-dipole-moment-mag ?dipole ?t))
   ;; allows (forces?) student to define both charges
   ;; this equation is timeless
   (implicit-eqn (= (+ ?qp ?qn) 0) (electric-dipole-moment-balance ?dipole))
   (assume using-magnitude (dipole-moment ?dipole electric ?t)) ;mag xor compos
   )
  :hint 
  (
   (point (string "What is the definition of the electric dipole moment?"))
   (teach ;(kcd "write-electric-dipole-moment-mag")
    (string "The electric dipole moment of a +q -q pair of charges  is defined as the charge q times a vector going from -q to +q."))
   (bottom-out (string "Write the equation ~a" ((= ?magP (* (abs ?qp) ?magd)) algebra)))
   ))

;;;                 The magnetic dipole moment of a loop of current

(def-psmclass magnetic-dipole-moment 
    (?eq-type definition ?axis ?rot (dipole-moment ?dipole magnetic ?time)) 
  :complexity major
  :short-name ("magnetic dipole moment (~A component)" (axis-name ?axis))
  :english ("the magnetic dipole moment of a flat current loop")
  :ExpFormat ("finding the magnetic dipole moment of ~a ~a"
		 (nlg ?dipole) (nlg ?time 'pp) )
  :EqnFormat ("$m_~a = N*I*A*n_~a" (axis-name ?axis) (axis-name ?axis)))

(defoperator magnetic-dipole-moment-contains (?sought)
  :preconditions 
  ((rdebug "Using magnetic-dipole-moment-contains  ~%")
   (magnetic-dipole ?current-loop ?surface)
   (time ?t)
   (any-member ?sought (
			(turns ?current-loop)
			(area ?current-loop)
			(unit-vector normal-to ?surface :time ?t)
			(dipole-moment ?current-loop magnetic :time ?t)
			))
   (rdebug "Firing magnetic-dipole-moment-contains  ~%")
   )
  :effects 
  (
   (eqn-family-contains (dipole-moment ?current-loop magnetic ?t) ?sought)
   ;; since only one compo-eqn under this vector psm, we can just
   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (dipole-moment ?current-loop magnetic ?t) 
		       definition ?sought)))

(defoperator draw-magnetic-dipole-moment-diagram (?rot ?dipole ?t)
  :preconditions 
  (
   (debug "Using draw-magnetic-dipole-moment-diagram ~%")
   (not (vector-diagram ?rot (dipole-moment ?dipole magnetic ?t)))
   (magnetic-dipole ?dipole ?surface)
   (vector ?dipole (dipole-moment ?dipole magnetic :time ?t) ?dir1) 
   (vector ?surface (unit-vector normal-to ?surface :time ?t) ?dir1) 
   (axes-for ?dipole ?rot)
   (rdebug "Fired draw-magnetic-dipole-moment-diagram ~%")
   )
  :effects (
            (vector-diagram ?rot (dipole-moment ?dipole magnetic ?t))
            ))

(defoperator write-magnetic-dipole-moment-compo (?dipole ?t ?xy ?rot)
  :preconditions 
  ((debug "Using write-magnetic-dipole-moment-compo ~%")
   (magnetic-dipole ?dipole ?surface)
   (variable ?mu_x (compo ?xy ?rot (dipole-moment ?dipole magnetic :time ?t)))
   (variable ?n_x (compo ?xy ?rot (unit-vector normal-to ?surface :time ?t)))
   (variable ?N (turns ?dipole))
   (inherit-variable ?I (current-thru ?dipole :time ?t))
   (variable ?A (area ?surface))
   (rdebug "fired write-magnetic-dipole-moment-compo  ~%")
   )
  :effects 
  ( (eqn (= ?mu_x (* ?N ?I ?A ?n_x))
	 (compo-eqn definition ?xy ?rot (dipole-moment ?dipole magnetic ?t)))
    )
  :hint 
  ( (point (string "What is the magnetic dipole moment for a current loop?"))
    (teach ;(kcd "write-magnetic-dipole-moment-compo")
     (string "The magnetic dipole moment of a current loop is the product of the number of turns, the current through the coil, the area of the surface, and a unit vector normal to the surface."))
    (bottom-out (string "Write the equation ~a" 
			((= ?mu_x (* ?N ?I ?A ?n_x)) algebra)))
    ))

;; Magnitude of the above equation

(def-psmclass magnetic-dipole-moment-mag 
             (magnetic-dipole-moment-mag ?body ?time) 
  :complexity major
  :short-name "magnetic dipole moment (magnitude)"
  :english ("the magnitude of the magnetic dipole moment of current loop")
  :ExpFormat ("finding the magnitude of the magnetic dipole moment of ~a ~a"
		 (nlg ?body) (nlg ?time 'pp) )
  :EqnFormat ("$m = N*I*A" ))

(defoperator magnetic-dipole-moment-mag-contains (?sought)
  :preconditions 
  (
   (magnetic-dipole ?dipole ?surface)
   ;; because of abs(Q), charge is not a sought
   (any-member ?sought(
		       (turns ?dipole)
		       (area ?surface)
		       (current-thru ?dipole :time ?t)
		       (mag (dipole-moment ?dipole magnetic :time ?t))
		       ))
   (time ?t)
   (debug "Using & firing write-magnetic-dipole-moment-mag-contains ~%")
   )
  :effects (
           (eqn-contains (magnetic-dipole-moment-mag ?dipole ?t) ?sought)
           ))  

(defoperator write-magnetic-dipole-moment-mag (?dipole ?t)
  :preconditions 
  ((debug "Using write-magnetic-dipole-moment-mag ~%")
   (magnetic-dipole ?dipole ?surface)
   (variable ?magmu (mag (dipole-moment ?dipole magnetic :time ?t)))
   (variable ?N (turns ?dipole))
   (inherit-variable ?I (current-thru ?dipole :time ?t))
   (variable ?A (area ?surface))
   (rdebug "fired write-magnetic-dipole-moment-mag  ~%")
   )
  :effects 
  (
   (eqn (= ?magmu (* ?N ?I ?A)) 
	(magnetic-dipole-moment-mag ?dipole ?t))
   (assume using-magnitude (dipole-moment ?dipole magnetic ?t)) ;mag xor compos
   )
  :hint 
  (
   (point (string "What is the magnetic dipole moment for a loop of current?"))
   (teach ;(kcd "write-magnetic-dipole-moment-mag")
    (string "The magnitude of the magnetic dipole moment of a current loop is the product of the number of turns, the current through the coil, the area of the surface."))
   (bottom-out (string "Write the equation ~a" ((= ?magmu (* ?N ?I ?A)) algebra)))
   ))


;;;              Torque from a dipole in an external field

;; Note that this is really a couple. 
;; The following is copied from mag-torque

(def-psmclass electric-dipole-torque-mag 
  (dipole-torque-mag ?dipole (field ?region electric ?source) ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "torque for electric dipole (magnitude)"
  :english ("the magnitude of the ~A on a dipole in an electric field" 
	    (moment-name))
  :expformat ((strcat "calculating the magnitude of the ~A "
		      "on ~a ~a due to the electric field in ~a")
	      (moment-name) (nlg ?dipole) (nlg ?time 'pp) (nlg ?region))
  :EqnFormat ((torque-switch "M = p*E*sin($q)" "$t = p*E*sin($q)")))

(def-psmclass magnetic-dipole-torque-mag 
  (dipole-torque-mag ?dipole (field ?region magnetic ?source) ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name ("~A for magnetic dipole (magnitude)" (moment-name))
  :english ("the magnitude of the ~A on a dipole in a magnetic field" 
	    (moment-name))
  :expformat ((strcat "calculating the magnitude of the ~A "
		      "on ~a ~a due to the magnetic field in ~a")
	      (moment-name) (nlg ?dipole) (nlg ?time 'pp) (nlg ?region))
  :EqnFormat ((torque-switch "M = $m*B*sin($q)" "$t = $m*B*sin($q)")))

(defoperator dipole-torque-mag-contains (?sought)
  :preconditions 
  (
   (any-member ?sought (
			(mag (torque ?dipole (field ?region ?type ?source)
				     :time ?t))
			(mag (field ?region ?type ?source :time ?t))
			(mag (dipole-moment ?dipole ?type :time ?t))
			(angle-between orderless 
				       (dipole-moment ?dipole ?type :time ?t)
				       (field ?region ?type ?source :time ?t))
			))
   ;; if dipole-moment is sought, need to bind ?source
   (given-field ?region ?type ?source :time ?t-given :dir ?any-dir)
   (at-place ?dipole ?region :time ?t ?t)
   )
   :effects 
   ((eqn-contains (dipole-torque-mag ?dipole (field ?region ?type ?source) ?t)
		  ?sought)))

(defoperator write-dipole-torque-mag (?dipole ?source ?t)
   :preconditions 
   (
    (inherit-variable ?tau-var 
		      (mag (torque ?dipole (field ?region ?type ?source)
				   :time ?t)))
    (inherit-variable ?p-var (mag (dipole-moment ?dipole ?type :time ?t)))
    (inherit-variable ?E-var (mag (field ?region ?type ?source :time ?t)))
    (inherit-variable ?theta-var 
		      (angle-between orderless 
				     (dipole-moment ?dipole ?type :time ?t)
				     (field ?region ?type ?source :time ?t)))
    )
   :effects (
      (eqn (= ?tau-var (* ?p-var ?E-var (sin ?theta-var))) 
             (dipole-torque-mag ?dipole (field ?region ?type ?source) ?t))
      (assume using-magnitude 
	      (dipole-torque ?dipole (field ?region ?type ?source) ?t))
   )
   :hint
   ( (point (string "What is the magnitude of the torque produced by ~A due to the ~A field?" ?dipole (?type adj)))
     (teach (string "When a a dipole is placed in a field, the field exerts a torque on the dipole."))
     (bottom-out (string "Write the equation ~A" 
			 ((= ?tau-var (* ?p-var ?E-var (sin ?theta-var))) 
			  algebra)))
  ))


;;; dipole-torque-zc: equation for torque z-component for a dipole

;;  tau_z = p*E*sin(thetaE - thetaP)

(def-psmclass electric-dipole-torque 
  (dipole-torque ?dipole (field ?region electric ?source) ?axis ?rot ?flag ?t) 
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "torque for electric dipole (z component)"
  :english ("the ~A on a dipole in an electric field" (moment-name))
  :expformat ((strcat "calculating the ~A component of the ~A "
		      "on ~a ~a due to the electric field in ~A")
	      (axis-name ?axis) (moment-name) (nlg ?dipole) (nlg ?t 'pp) 
	      (nlg ?region))
  :EqnFormat ((electric-dipole-equation ?axis)))

(defun electric-dipole-equation (xyz)
  (cond ((eq xyz 'x) (torque-switch "M_x = p_y*E_z - p_z*E_y"
				    "$t_x = p_y*E_z - p_z*E_y"))
	((eq xyz 'y) (torque-switch "M_y = p_z*E_x - p_x*E_z"
				    "$t_y = p_z*E_x - p_x*E_z"))
	((eq xyz 'z) 
	 (torque-switch 
	  "M_z = p*E*sin($qE-$qp) or M_z = p_x*E_y - p_y*E_x"
	  "$t_z = p*E*sin($qE-$qp)) or $t_z = p_x*E_y - p_y*E_x"))))

(def-psmclass magnetic-dipole-torque 
  (dipole-torque ?dipole (field ?region magnetic ?source) ?axis ?rot ?flag ?t) 
  :complexity major ; definition, but can be first "principle" for sought
  :short-name ("torque for magnetic dipole (~A component)" (axis-name ?axis))
  :english ("the ~A on a dipole in an magnetic field" (moment-name))
  :expformat ((strcat "calculating the ~A component of the ~A "
		      "on ~a ~a due to the magnetic field in ~A")
	      (axis-name ?axis) (moment-name) (nlg ?dipole) (nlg ?t 'pp) 
	      (nlg ?region))
  :EqnFormat ((magnetic-dipole-equation ?axis)))

(defun magnetic-dipole-equation (xyz)
  (cond ((eq xyz 'x) (torque-switch "M_x = $m_y*B_z - $m_z*B_y"
				    "$t_x = $m_y*B_z - $m_z*B_y"))
	((eq xyz 'y) (torque-switch "M_y = $m_z*B_x - $m_x*B_z"
				    "$t_y = $m_z*B_x - $m_x*B_z"))
	((eq xyz 'z) 
	 (torque-switch 
	  "M_z = $m*B*sin($qB-$q$m) or M_z = $m_x*B_y - $m_y*B_x"
	  "$t_z = $m*B*sin($qB-$q$m)) or $t_z = $m_x*B_y - $m_y*B_x"))))

(defoperator dipole-torque-contains-angle (?sought)
  :preconditions 
  (
   (any-member ?sought 
	       ( 
		(compo ?axis ?rot (torque ?dipole (field ?region ?type ?source)
					      :time ?t))
		(mag (field ?region ?type ?source :time ?t))
		(mag (dipole-moment ?dipole ?type :time ?t))
		(dir (field ?region ?type ?source :time ?t))
		(dir (dipole-moment ?dipole ?type :time ?t))
		))
   (time-or-timeless ?t)
   ;; if dipole-moment is sought, need to bind ?source
   (given-field ?region ?type ?source :time ?t-given :dir ?any-dir)
   (at-place ?dipole ?region :time ?t ?t)
   (axes-for ?dipole ?rot) ;in case ?rot is not bound
   (get-axis ?axis ?rot) ;in case ?axis is not bound
   )
  :effects 
  ( (eqn-contains 
     (dipole-torque ?dipole (field ?region ?type ?source) ?axis ?rot nil ?t)
		  ?sought) ))

(defoperator dipole-torque-contains-compo (?sought)
  :preconditions 
  (
   (any-member ?sought 
	       ( 
		(compo ?axis ?rot (torque ?dipole (field ?region ?type ?source)
					  :time ?t))
		(compo ?not-axis ?rot (field ?region ?type ?source 
					     :time ?t))
		(compo ?not-axis ?rot (dipole-moment ?dipole ?type :time ?t))
		))
   (time-or-timeless ?t)
   ;; if dipole-moment is sought, need to bind ?source
   (given-field ?region ?type ?source :time ?t-given :dir ?any-dir)
   (test (tinsidep ?t ?t-given))
   (at-place ?dipole ?region :time ?t ?t)
   (get-axis ?axis ?rot) ;in case ?axis is not bound
  )
 :effects 
 ( (eqn-contains (dipole-torque ?dipole 
				(field ?region ?type ?source) ?axis ?rot t ?t)
		 ?sought) ))

(defoperator write-dipole-torque (?dipole ?source ?axis ?rot ?flag ?t)
  :preconditions 
  ( 
   ;; draw vectors now, before applying cross product
   (vector ?dipole (dipole-moment ?dipole ?type :time ?t) ?dir-mom)
   (inherit-vector ?whatever (field ?region ?type ?source :time ?t) ?dir-field)
   (vector ?dipole (torque ?dipole (field ?region ?type ?source)
			   :time ?t) ?dir-torque)
   ;;
   (cross ?cross (dipole-moment ?dipole ?type :time ?t) 
	  (field ?region ?type ?source :time ?t) ?axis ?rot ?flag)
   (test (not (eq ?cross '0)))		; handled by write-dipole-torque-mag
   (variable ?tau-zc (compo ?axis ?rot 
			    (torque ?dipole (field ?region ?type ?source)
				    :time ?t)))
   (debug "write-dipole-torque firing with ?cross=~A, ?axis=~A~%" ?cross ?axis)
   )
  :effects 
  ( (eqn (= ?tau-zc ?cross)
	 (dipole-torque ?dipole (field ?region ?type ?source) 
			?axis ?rot ?flag ?t))
    ;; disallow both component-form and magnitude form in a solution
    (assume using-compo 
	    (compo ?axis ?rot (dipole-torque ?dipole 
					     (field ?region ?type ?source) ?t)))
    )
  :hint 
  ( (point (string "What is the torque produced by ~A due to the ~A field?" 
		   ?dipole (?type adj)))
    (teach (string "When a a dipole is placed in a field, the field exerts a torque on the dipole."))
    (bottom-out (string "Write the equation ~A" 
			((= ?tau-zc ?cross) 
			  algebra)))
    ))


;;; Draw torque acting on an electric or magnetic dipole

(defoperator draw-torque-dipole-given-dir (?dipole ?t)
  :preconditions 
  (
   (any-member ?field ((field ?region ?type ?source)))
   ;; right now, this must be specified in the problem statement
   ;; although the hints assume given dipole moment and given
   ;; field direction.
   (given (dir (dipole-moment ?dipole ?type :time ?t)) ?dir-d)
   ;; workbench requires field vector to be labelled as prerequisite for 
   ;; defining a dipole torque, so ensure it is drawn here.
   ;; also, draw vector so that direction can be determined below
   (inherit-vector ?own (field ?region ?type ?source :time ?t) ?dir-drawn)
   ;; vector direction is not determined if compos are given, use this
   ;; to determine direction (and bind ?region and ?source)
   (dir-given-or-compos (field ?region ?type ?source :time ?t) ?dir-f)
   (bind ?tau-dir (cross-product-dir ?dir-d ?dir-f))
   (test (not (eq ?tau-dir 'zero)))
   (bind ?mag-var (format-sym "TOR_~A_~A_~A" (body-name ?dipole) 
			      (body-name ?source) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
 :effects 
 (
  (vector ?dipole (torque ?dipole ?field :time ?t) ?tau-dir)
  (variable ?mag-var (mag (torque ?dipole ?field :time ?t)))
  (variable ?dir-var (dir (torque ?dipole ?field :time ?t)))
  )
 :hint 
 (
  (point (string "The torque on a dipole is the cross product of its ~A dipole moment and the ~A field vector at the same location." (?type adj) (?type adj))) 
  (teach (string "The torque vector on a dipole points in a direction perpendicular to the plane formed by the dipole moment and ~A field vectors, in a direction determined by the right hand rule:  curl the fingers of your right hand from the dipole moment vector to the ~A field vector, and your thumb will point in the direction of the torque." (?type adj) (?type adj)))
  (bottom-out (string "Because the ~A moment has direction ~a and the ~A field direction is ~a, the right-hand rule determines the direction of torque to be ~a. Use the torque drawing tool (labeled $t) to draw the torque on ~a due to ~a in the direction of ~A." 
		      (?type adj) (?dir-d adj) 
		      (?type adj) (?dir-f adj) (?tau-dir adj) 
		      ?dipole ?field (?tau-dir adj)))
  ))

(defoperator draw-torque-dipole-zero (?dipole ?t)
  :preconditions 
  (
   (any-member ?field ((field ?region ?type ?source))) ;save typing
   ;; right now, this must be specified in the problem statement
   ;; although the hints assume given dipole moment and given
   ;; field direction.
   (given (dir (dipole-moment ?dipole ?type :time ?t)) ?dir-d)
   ;; workbench requires field vector to be labelled as prerequisite for 
   ;; defining a dipole torque, so ensure it is drawn here.
   ;; also need to draw befor determining direction below
   (inherit-vector ?dk (field ?region ?type ?source :time ?t) ?dir-drawn)
   ;; AW: for magtor1d, changed t-field to match draw-torque-dipole-given-dir
   ;; vector direction is 'unknown if compos are given, use this
   ;; to determine direction (and bind ?region and ?source)
   (dir-given-or-compos (field ?region ?type ?source :time ?t) ?dir-f)
   (bind ?tau-dir (cross-product-dir ?dir-d ?dir-f))
   (test (eq ?tau-dir 'zero))
   (bind ?mag-var (format-sym "TOR_~A_~A_~A" (body-name ?dipole) 
			      (body-name ?source) (time-abbrev ?t)))
   )
 :effects 
 (
  (vector ?dipole (torque ?dipole ?field :time ?t) ?tau-dir)
  (variable ?mag-var (mag (torque ?dipole ?field :time ?t)))
  )
 :hint 
 (
  (point (string "The torque on a dipole is the cross product of its ~A dipole moment and the ~A field vector at the same location." 
		 (?type adj) (?type adj))) 
  (teach (string "Remember the magnitude of a cross product of two vectors is proportional to the sine of the angle between them."))
  (teach (string "If two vectors are parallel or anti-parallel, the sine of the angle betwen them is zero, so their cross-product is a zero-length vector."))
  (bottom-out (string "Because the cross product of the dipole moment and the ~A field direction is zero in this case, use the torque drawing tool (labeled $t) to draw a zero-length vector for torque on ~a due to ~a." 
			     (?type adj) ?dipole ?field))
  ))


;;; Potential energy of an electric dipole

;; can be either electric or magnetic
;; this was borrowed from work
(def-qexp dipole-energy (dipole-energy ?dipole ?field :time ?time)
  ;; custom dialog box "energy"
  :units |J|
  :english ("the potential energy of ~A in ~A" 
	    (nlg ?dipole) (nlg ?field 'at-time ?time)))

(defoperator define-dipole-energy (?dipole ?args ?t)
 :preconditions 
 ( (object ?dipole)
   (time ?t)
   (bind ?source (third ?args))
   (bind ?de-var (format-sym "Ud~A_~A_~A~@[_~A~]" 
			     (subseq (string (second ?args)) 0 1)
			     (body-name ?dipole) 
			     (body-name ?source) (time-abbrev ?t))) )
 :effects (
   (define-var (dipole-energy ?dipole (field . ?args) :time ?t))
   (variable ?de-var (dipole-energy ?dipole (field . ?args) :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting dipole energy" ((dipole-energy ?dipole (field . ?args) :time ?t) def-np)))
 ))
  
(defoperator define-dipole-energy-ee-var (?dipole ?source ?t)
  :preconditions 
  ( ;; Test for electric field acting on object
   (given-field ?region ?type ?source :time ?t-given :dir ?any-dir)
   (test (tinsidep ?t ?t-given))
   (at-place ?dipole ?region :time ?t ?t)
   )
  :effects ( (ee-var ?dipole ?t (dipole-energy 
		   ?dipole (field ?region ?type ?source) :time ?t)) ))

(def-psmclass electric-dipole-energy 
  (dipole-energy ?dipole (field ?region electric . ?rest) ?time ?dot-type)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "potential energy of electric dipole"
  :english ("the definition of the energy of a dipole in an electric field")
  :expformat ("calculating the energy of ~a in ~A" 
	      (nlg ?dipole) 
	      (nlg (set-time (append (list 'field ?region 'electric) ?rest) ?time)))
  :EqnFormat ("U = -p*E*cos($qp - $qE) or U = -(p_x*E_x + p_y*E_y)"))

(def-psmclass magnetic-dipole-energy 
  (dipole-energy ?dipole (field ?region magnetic . ?rest) ?time ?dot-type)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "potential energy of magnetic dipole"
  :english ("the definition of the energy of a dipole in a magnetic field")
  :expformat ("calculating the energy of ~a in ~A" 
	      (nlg ?dipole) 
	      (nlg (set-time (append (list 'field ?region 'magnetic) ?rest) ?time)))
  :EqnFormat ("U = -$m*B*cos($q$m - $qB) or U = -($m_x*B_x + $m_y*B_y)"))

(defoperator dipole-energy-contains (?sought)
  :preconditions 
  ((any-member ?sought 
	       ( (dipole-energy ?dipole (field ?region ?type ?source) 
					  :time ?t)
		 (mag (field ?region ?type ?source :time ?t))
		 (mag (dipole-moment ?dipole ?type :time ?t))
		 (angle-between orderless
				(dipole-moment ?dipole ?type :time ?t) 
				(field ?region ?type ?source :time ?t))))
   ;; bind source if dipole-moment is sought
   (given-field ?region ?type ?source :time ?t-given :dir ?any-dir)
   (test (tinsidep ?t ?t-given))
   (at-place ?dipole ?region :time ?t ?t) ;bind ?dipole if field is sought
   )
 :effects 
 ((eqn-contains (dipole-energy ?dipole (field ?region ?type ?source) 
			       ?t NIL) ?sought)
  ))

(defoperator dipole-energy-compo-contains (?sought)
  :preconditions 
  ((any-member ?sought 
	       ( (dipole-energy ?dipole (field ?region ?type ?source) 
				:time ?t)
		 (compo ?xyz ?rot (field ?region ?type ?source :time ?t))
		 (compo ?xyz ?rot (dipole-moment ?dipole ?type :time ?t))
		 ))
   (at-place ?dipole ?region :time ?t ?t)
   ;; bind ?source in case of dipole-moment
   (given-field ?region ?type ?source :time ?t-given :dir ?dir)
   (test (tinsidep ?t ?t-given))
   ;; find axes now, before applying dot product:
   (vector ?dipole (dipole-moment ?dipole ?type :time ?t) ?dir-d)
   ;; If ?rot is unbound, draw-rotate-axes or draw-standard-axes
   ;; etc. will choose the angle.  If it is bound from the ?sought,
   ;; operator will also succeed.
   (axes-for ?dipole ?rot) 
   )
  :effects 
  ((eqn-contains (dipole-energy ?dipole (field ?region ?type ?source) 
				?t ?rot) ?sought)
   (assume axes-for ?dipole ?rot)
 ))

;; This can write either the component or the angle form of the 
;; electric dipole energy equation, depending on ?rot.  
(defoperator write-dipole-energy (?dipole ?t ?rot)
  :preconditions 
  (
   (variable ?u-var (dipole-energy ?dipole (field ?region ?type ?source) 
				   :time ?t))
   (dot ?dot (dipole-moment ?dipole ?type :time ?t)
	(field ?region ?type ?source :time ?t)
	?rot :nonzero ?nonzero)
   ;; It might make sense to have a seperate operator for the case
   ;; of zero energy.  In that case, the displacement and the force can't
   ;; be soughts. 
   (bind ?teaches 
	 (if (eq ?type 'electric)
	     (strcat "The electric dipole energy of a dipole P in an electric field E is given by "
		     (if ?rot "- (p_x * E_x + p_y * E_y)." 
			 "- p * E * cos ($q), where $q is the angle between the dipole and electric field vectors."))
	     (strcat "The magnetic dipole energy of a dipole $m in a magnetic field B is given by "
		     (if ?rot "- ($m_x * B_x + $m_y * B_y)." 
			 "- $m * B * cos ($q), where $q is the angle between the dipole and magnetic field vectors."))))
   )
  :effects 
 ((eqn (= ?u-var (- ?dot))
       (dipole-energy ?dipole (field ?region ?type ?source) ?t ?rot))
  )
 :hint (
	(point (string "You need the value of the ~A dipole energy of ~a ~A" 
		       (?type adj) ?dipole (?t pp)))
	(teach (string ?teaches))
	(bottom-out (string "Write the equation ~A"  
			    ((= ?u-var (- ?nonzero)) algebra)))
	))


;;--------------------------------------------------
;;  Magnetic fields and forces 
;;--------------------------------------------------

(defoperator draw-Bfield-point-particle (?loc ?b ?t)
  :preconditions 
  (
   (point-charge ?b) ;Make sure source is point-charge
   (given (dir (velocity ?b :time ?t)) ?dir-v)
   (given (dir (relative-position ?loc ?b :time ?t)) ?dir-r)
   (bind ?cross-dir (cross-product-dir ?dir-v ?dir-r))
   (test ?cross-dir) ;make sure direction can be determined
   (test (not (eq ?cross-dir 'zero)))
   (sign-charge ?b ?pos-neg)    ;require sign of charge to be given
   (bind ?same-or-opposite  (if (eq ?pos-neg 'pos) 'same 'opposite))
   (bind ?dir-B (if (eq ?pos-neg 'pos) ?cross-dir (opposite ?cross-dir)))
   (bind ?mag-var (format-sym "B_~A_~A~@[_~A~]" (body-name ?loc) (body-name ?b)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   ;; direction not explicitly given
   (not (given (dir (field ?loc magnetic ?b :time ?t)) ?any-dir))
   )
  :effects (
           (vector ?loc (field ?loc magnetic ?wire :time ?t) ?dir-B)
           (variable ?mag-var (mag (field ?loc magnetic ?wire :time ?t)))
           (variable ?dir-var (dir (field ?loc magnetic ?wire :time ?t)))
           (given (dir (field ?loc magnetic ?wire :time ?t)) ?dir-B)
  )
  :hint (
      (point (string "The direction of the magnetic field of a moving point charge can be determined using the Biot-Savart law."))
      ;; This should be replaced by a tutorial.  It is rather cumbersome
      ;; to explain in word form.
      (teach (string "Look up the Biot-Savart law in your textbook."))
      (bottom-out (string "The velocity vector points in the direction ~A.  The relative position of ~A is in the direction ~A.  Thus, the cross product is in the direction ~A.  Since the charge is ~A, the resulting magnetic field is in the ~A direction.  Use the magnetic field drawing tool (labeled B) to draw the magnetic field at ~a due to ~a in the direction, ~A." 
           (?dir-v adj) ?loc (?dir-r adj) (?cross-dir adj) (?pos-neg adj)
	   (?same-or-opposite adj) ?loc ?b (?dir-B adj)))
  ))

(defoperator draw-Bfield-point-particle-zero (?loc ?b ?t)
  :preconditions 
  (
   (point-charge ?b) ;Make sure source is point-charge
   (given (dir (velocity ?b :time ?t)) ?dir-v)
   (given (dir (relative-position ?loc ?b :time ?t)) ?dir-r)
   (bind ?cross-dir (cross-product-dir ?dir-v ?dir-r))
   (test (eq ?cross-dir 'zero))
   (bind ?mag-var (format-sym "B_~A_~A~@[_~A~]" (body-name ?loc) (body-name ?b)
			      (time-abbrev ?t)))
   )
  :effects (
           (vector ?loc (field ?loc magnetic ?wire :time ?t) zero)
           (variable ?mag-var (mag (field ?loc magnetic ?wire :time ?t)))
  )
  :hint (
      (point (string "The direction of the magnetic field of a moving point charge can be determined using the Biot-Savart law."))
      ;; This should be replaced by a tutorial.  It is rather cumbersome
      ;; to explain in word form.
      (teach (string "Look up the Biot-Savart law in your textbook."))
      (bottom-out (string "The velocity vector points in the direction ~A.  The relative position of ~A is in the direction ~A.  Since these two vectors point in the same (or opposite) direction, the cross product is zero.  Use the magnetic field drawing tool (labeled B) to draw a zero magnetic field at ~a due to ~a." 
           (?dir-v adj) ?loc (?dir-r adj) ?loc ?b))
  ))

;; draw Bfield near a straight current-carrying wire
;; problem should give dir of perpendicular distance from wire to ?loc
;; This will probably only work with the feature changing-field
(defoperator draw-Bfield-straight-current (?loc ?wire ?t)
  :preconditions 
  (
   (given (dir (current-length ?wire :time ?t)) ?dir-l)
   (given (dir (relative-position ?loc ?wire :time ?t)) ?dir-r)
   ;; Sanity test for inherit-quantity working OK
   (test (or (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*))))
	     (error "draw-bfield-straight-current bad time slot ~A" ?t)))
   (bind ?dir-B (cross-product-dir ?dir-l ?dir-r))
   (test ?dir-B)
   (test (not (eq ?B-dir 'zero)))
   (bind ?mag-var (format-sym "B_~A_~A~@[_~A~]" 
			      (body-name ?loc) (body-name ?wire) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
           (vector ?loc (field ?loc magnetic ?wire :time ?t) ?dir-B)
           (variable ?mag-var (mag (field ?loc magnetic ?wire :time ?t)))
           (variable ?dir-var (dir (field ?loc magnetic ?wire :time ?t)))
           (given (dir (field ?loc magnetic ?wire :time ?t)) ?dir-B)
  )
  :hint (
      (point (string "The direction of the magnetic field lines around a straight current-carrying wire can be determined by a use of the right-hand rule."))
      (teach (string "Magnetic field lines near a straight current-carrying wire take the form of concentric circles with the wire at their center. If you grasp the wire with your right hand with the thumb pointing in the direction of the current, your fingers curl around the wire in the direction of the magnetic field lines."))
      (bottom-out (string "Curling your right hand around the wire with the thumb in the direction of the current, ~a, your fingers at ~a point in the direction ~a.  Use the magnetic field drawing tool (labeled B) to draw the magnetic field at ~a due to ~a in that direction, ~A." 
           (?dir-l adj) ?loc (?dir-B adj) ?loc ?wire (?dir-B adj)))
  ))

(defoperator draw-Bfield-current-loop (?loc ?wire ?t)
  :preconditions 
  (
   (center-of-coil ?loc ?wire :current-axis ?dir-B)
   ;; only use time when allowed by feature changing-field
   (time-or-timeless ?t)
   ;; if time is bound above, make sure it is consistant with
   ;; what student can define
   (test (eq (null ?t) 
		 (null (member 'changing-field (problem-features *cp*)))))
   (bind ?mag-var (format-sym "B_~A_~A~@[_~A~]" 
			      (body-name ?loc) (body-name ?wire) 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (test (or ?dir-B (error "must supply value for :current-axis")))
   (bind ?dir-i (rotation-name ?dir-B))
   )
  :effects (
           (vector ?loc (field ?loc magnetic ?wire :time ?t) ?dir-B)
           (variable ?mag-var (mag (field ?loc magnetic ?wire :time ?t)))
           (variable ?dir-var (dir (field ?loc magnetic ?wire :time ?t)))
           (given (dir (field ?loc magnetic ?wire :time ?t)) ?dir-B)
  )
  :hint (
      (point (string "The direction of the magnetic field at the center of a current-carrying coil can be determined by a use of the right-hand rule."))
      (teach (string "If you grasp the wire with your right hand with the thumb pointing in the direction of the current, your fingers curl around the wire in the direction of the magnetic field lines."))
      (bottom-out (string "Curling your right hand around ~A with the thumb in the direction of the current, ~a.  At ~A, your fingers point in the direction ~a.  Use the magnetic field drawing tool (labeled B) to draw the magnetic field at ~a due to ~a in that direction, ~A." 
           ?wire (?dir-i adj) ?loc (?dir-B adj) ?loc ?wire (?dir-B adj)))
  ))

;; This draws the magnetic force vector on a charge by right-hand-rule
;; given direction of B-field and v. Note dir of v may not be directly given,
;; but can be derived in several ways, e.g. straight line or circular or other
;; motion spec, by draw-velocity* operators. This may draw v as well to get
;; the direction from givens.
(defoperator find-magnetic-force-charge (?b ?t ?source)
  :preconditions 
  (
   (at-place ?b ?loc :time ?t ?t)
   (sign-charge ?b ?pos-or-neg)
   ;; in principle, the field need not be uniform, but we would
   ;; need to test ?loc is a point
   (homogeneous-field ?loc magnetic ?source :time ?t ?t :dir ?dir-b)
   ;; this may require drawing the velocity vector: 
   (given (dir (velocity ?b :time ?t)) ?dir-V)
   ;; following currently only works for dirs along axis
   (bind ?cross-dir (cross-product-dir ?dir-V ?dir-B))
   (test ?cross-dir) ; may be NIL on failure
   (test (not (eq ?cross-dir 'zero)))
   (bind ?F-dir (if (eq ?pos-or-neg 'pos) ?cross-dir (opposite ?cross-dir)))
   ;; make sure we have a non-null direction
   (test ?F-dir) ; may be NIL on failure
   (add-to-wm (magnetic-force-charge ?b ?loc ?t ?source))
   )
  :effects (
	    (force ?b ?source magnetic ?t ?F-dir action)
	    (force-given-at ?b ?source magnetic ?t ?F-dir action)
	    ))


(defoperator draw-Bforce-charge (?b ?t ?source)
  :preconditions 
  (
   (force ?b ?source magnetic ?t ?F-dir action)
   ;; bind ?loc, make sure we are connected to right find
   (in-wm (magnetic-force-charge ?b ?loc ?t ?source))
   ;; from find-magnetic-force-charge above
   (in-wm (homogeneous-field ?loc magnetic ?source :time ?t ?t :dir ?dir-b))
   (in-wm (sign-charge ?b ?pos-or-neg))
   ;; this may require drawing the velocity vector: 
   (in-wm (given (dir (velocity ?b :time ?t)) ?dir-V))
   ;;
   (bind ?mag-var (format-sym "Fb_~A~@[_~A~]" (body-name ?loc)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?porn (if (eq ?pos-or-neg 'pos) "positive" "negative"))
   (bind ?saop (if (eq ?pos-or-neg 'pos) "same" "opposite"))
   )
  :effects (
	    (vector ?b (force ?b ?source magnetic :time ?t) ?F-dir)
            (variable ?mag-var (mag (force ?b ?source magnetic :time ?t)))
            (variable ?dir-var (dir (force ?b ?source magnetic :time ?t)))
            (given (dir (force ?b ?source magnetic :time ?t)) ?F-dir)
	    )
  :hint 
  (
   (point (string "The magnetic force on a ~Aly charged particle points in the ~A direction as the cross product of the velocity and magnetic field vectors." (?porn adj) (?saop adj))) 
   (teach (string "The magnetic force vector on a moving charge points in a direction perpendicular to the plane formed by the velocity and magnetic field vectors, in a direction determined by the right hand rule:  orient your right hand so that your outstretched fingers point in the direction of the velocity and when you curl them in they point in the direction of the magnetic field.  Your thumb will then point in the direction of the cross product.  For a ~A charge, the force is in the ~A direction." (?porn adj) (?saop adj)))
   (bottom-out (string "Because the velocity of ~a has direction ~a, the magnetic field direction is ~a, and the charge is ~A, the right-hand rule determines the direction of force to be ~a. Use the force drawing tool (labeled F) to draw the magnetic force on ~a due to ~a in the direction of ~A." 
		       ?b (?dir-V adj) (?dir-B adj) (?porn adj) (?F-dir adj) 
		       ?b (?source agent) (?F-dir adj)))
   ))


(defoperator draw-Bforce-rhr-zero (?b ?t ?source)
 :preconditions 
 (
  (at-place ?b ?loc :time ?t ?t)
  (given (dir (field ?loc magnetic ?source :time ?t ?t)) ?dir-B)
  ;; this may require drawing the velocity vector: 
  (given (dir (velocity ?b :time ?t)) ?dir-V)
  ;; following currently only works for dirs along axis
  (bind ?F-dir (cross-product-dir ?dir-V ?dir-B))
  ;; make sure we have a non-null direction
  (test ?F-dir) ; may be NIL on failure
  (test (eq ?F-dir 'zero))
  (bind ?mag-var (format-sym "Fb_~A~@[_~A~]" (body-name ?loc)
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

#|   ;this is not used in any problems
;; Draw Bforce on object in unknown direction when we know there's a B-field, 
;; but can't determine both B AND V dir for right-hand rule from givens.  
;; This might apply in cases where vectors are given by listing components so 
;; exact directions will be unknown.

;; In theory, also should require that Fb not given directly -- but we have 
;; no problems that just give Fb direction. 
;; !!! Also need to test that body is charged and has non-zero velocity. 
;; For now we are just assuming that if there's a B-field in the problem, 
;; any bodies in problem are moving charges, so subject to a force unless 
;; given directions determine zero force.

;; !! Some geometry functions in PhysicsFuncs.cl take "unknown" to guarantee 
;; xy-planehood.  This affects projection equations for unknown angles 
;; -- z-projections will always come out equal to zero. Until that is fixed, 
;; problems must be designed so unknown Bforces come out in the x-y plane.

;; This is a nuisance to code, since "not" can't negate a conjunction of 
;; conditions.  Also, there are several ways V might be determinable from 
;; givens

;; First form draws if we aren't given field dir:
(defoperator draw-Bforce-unknown-field (?b ?t)
  :specifications " "
  :preconditions 
  ((rdebug "Using draw-Bforce-unknown ~%")
   (given-field ?loc magnetic ?source :dir ?dir :time ?t)
   (test (or (null ?dir) (eq ?dir 'unknown)))
   (object ?b)
   (at-place ?b ?loc :time ?t ?t)
   (not (given (dir (field ?loc magnetic ?source :time ?t ?t)) ?dir-B))
   (not (vector ?b (force ?b ?source magnetic :time ?t) ?dir))
   (bind ?mag-var (format-sym "Fb_~A~@[_~A~]" (body-name ?b) 
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
         (teach (string "In this problem, the exact direction of the magnetic force vector requires calculation to determine, so you can draw the force vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the magnetic force on ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?b (?source agent))) 
  ))
|#

;; Given field dir but velocity can't be determined
;; (might be given by x and y components for component-form calculation.)
;; For now, we just require problem to tell us velocity is unknown.
(defoperator draw-Bforce-unknown-velocity (?b ?t)
  :preconditions 
  (
   (rdebug "Using draw-Bforce-unknown ~%")
   (homogeneous-field ?loc magnetic ?source :dir ?any-dir-b :time ?t-given)
   (test (tinsidep ?t ?t-given))
   (object ?b)
   (at-place ?b ?loc :time ?t ?t)
   ;; Require motion explicitly specified as unknown
   ;; Not quite right to presume it is "straight", though
   (motion ?b straight :dir unknown :time ?t ?t . ?whatever)
   (test (time-pointp ?t))
   (not (vector ?b (force ?b ?source magnetic :time ?t) ?dir))
   (bind ?mag-var (format-sym "Fb_~A~@[_~A~]" (body-name ?b) 
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
         (teach (string "In this problem, the exact direction of the magnetic force vector requires calculation to determine, so you can draw the force vector at an approximately correct angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the magnetic force on ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?b (?source agent))) 
	 ))


(defoperator find-magnetic-force-current (?b ?t ?source)
  :preconditions 
  (
  (given (dir (current-length ?b :time ?t ?t)) ?dir-i)
  (at-place ?b ?loc :time ?t ?t)
  (homogeneous-field ?loc magnetic ?source :time ?t ?t :dir ?dir-B)
  ;; following currently only works for dirs along axis
  (bind ?F-dir (cross-product-dir ?dir-i ?dir-B))
  ;; make sure we have a non-null direction
  (test ?F-dir) ; may be NIL on failure
  (test (not (eq ?F-dir 'zero)))
  (add-to-wm (magnetic-force-current ?b ?loc ?t ?source))
   )
  :effects (
	    (force ?b ?source magnetic ?t ?F-dir action)
	    (force-given-at ?b ?source magnetic ?t ?F-dir action)
	    ))

(defoperator draw-Bforce-current (?b ?t ?source)
 :preconditions 
 (
   (force ?b ?source magnetic ?t ?F-dir action)
   ;; bind ?loc, make sure we are connected to right find
   (in-wm (magnetic-force-current ?b ?loc ?t ?source))
   ;; This is found in find-magnetic-force-current above
   (in-wm (homogeneous-field ?loc magnetic ?source :time ?t ?t :dir ?dir-B))
   (in-wm (given (dir (current-length ?b :time ?t)) ?dir-i))
   ;;
   (bind ?mag-var (format-sym "Fb_~A~@[_~A~]" (body-name ?loc)
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
	(point (string "Let n_w to be a unit vector pointing in the direction of the current flow.  The magnetic force on ~A points is equal to the current times the cross product of n_w and the magnetic field." ?b)) 
	(teach (string "The magnetic force vector on a current carrying wire points in a direction perpendicular to the plane formed by the wire and the magnetic field vector, in a direction determined by the right hand rule:  orient your right hand so that your outstretched fingers point in the direction of the current and when you curl them in they point in the direction of the magnetic field.  Your thumb will then point in the direction of the force."))
        (bottom-out (string "Because the current in ~a has direction ~a and the magnetic field direction is ~a, the right-hand rule determines the direction of force to be ~a. Use the force drawing tool (labeled F) to draw the magnetic force on ~a due to ~a in the direction of ~A." 
			    ?b (?dir-i adj) (?dir-B adj) (?F-dir adj) ?b 
			    (?source agent) (?F-dir adj)))
 ))


;;;---------------------------------------------------------
;;; Bforce magnitude equation: F = abs(q)*V*B*sin(thetaVB)
;;;---------------------------------------------------------

(def-psmclass charge-force-Bfield-mag (charge-force-Bfield-mag ?body ?time)
  :complexity major
  :short-name "magnetic force (magnitude)"
  :english ("force on charge moving in a magnetic field")
  :ExpFormat ("applying the formula for force on charge in a magnetic field")
  :EqnFormat ("F = abs(q)*v*B*sin($q)" ))

(defoperator charge-force-Bfield-mag-contains (?sought)
  :preconditions 
  ((debug "Using write-charge-force-Bfield-mag-contains ~%")
   (any-member ?sought ((mag (force ?b ?source magnetic :time ?t))
			;; can't have charge as sought, due to absolute value
			(mag (field ?loc magnetic ?source :time ?t))
			(mag (velocity ?b :time ?t))))
   (time ?t) ;in case ?t is not bound
   (at-place ?b ?loc :time ?t ?t)
   (rdebug "Firing write-charge-force-Bfield-mag-contains ~%")
   )
  :effects(
           (eqn-contains (charge-force-Bfield-mag ?b ?t) ?sought)
           ))  

(defoperator write-charge-force-Bfield-mag (?b ?t)
  :preconditions 
  (
   (debug "Using write-charge-force-Bfield-mag ~%")
   (at-place ?b ?loc :time ?t ?t)
   ;; draw body for this psm
   (body ?b)
   ;; draw the vectors B, v, and F.
   (vector ?dontcare1 (field ?loc magnetic ?source :time ?t-field) ?B-dir)
   (inherit-or-quantity (field ?loc magnetic ?source :time ?t)
			(field ?loc magnetic ?source :time ?t-field))
   (vector ?dontcare2 (velocity ?b :time ?t) ?V-dir)
   (vector ?dontcare3 (force ?b ?source magnetic :time ?t) ?F-dir)
   ;; retrieve vector variables for equation:
   (in-wm (variable ?magB (mag (field ?loc magnetic ?source :time ?t-field))))
   (in-wm (variable ?magV (mag (velocity ?b :time ?t))))
   (in-wm (variable ?magF (mag (force ?b ?source magnetic :time ?t))))
   ;; define charge variable
   (inherit-variable ?q (charge ?b :time ?t))
   ;; calculate angle between. Put it directly into eqn w/o variable.
   (bind ?theta `(dnum ,(get-angle-between ?V-dir ?B-dir) |deg|))
   (test ?theta) ; make sure it was determinable
   (debug "fired write-charge-force-Bfield-mag  ~%")
   )
  :effects (
            (eqn (= ?magF (* (abs ?q) ?magV ?magB (sin ?theta))) (charge-force-Bfield-mag ?b ?t))
            )
  :hint (
         (point (string "You can find the magnitude of the magnetic force on ~A due to ~A" ?b ((field ?loc magnetic ?source :time ?t) def-np)))
         (teach (string "The magnitude of the magnetic force on a particle moving in a magnetic field is the product of the absolute value of the charge, the velocity, the magnetic field and the angle theta between the velocity and magnetic field vectors." ))
         (bottom-out (string "Write the equation ~a" ((= ?magF (* (abs ?q) ?magV ?magB (sin ?theta))) algebra)
		      ))
          ))

;; Charge-force-Bfield vector PSM.
;;
;; This isn't written as a vector psm because it's not clear it can fit into 
;; our general vector PSM framework: it's not a linear vector equation like 
;; Newton's law that resolves into three identical component equations; 
;; rather there are three different component equations.  Also,it relates 
;; components in 3 different directions, so can't directly use the idea of 
;; applying it along a given direction, and the test for non-zero-projection 
;; used when selecting axis along which to apply a vector psm is not 
;; appropriate. 


(def-psmclass charge-force-Bfield 
  (charge-force-Bfield ?axis ?rot ?body ?flag ?time)
  :complexity major
  :short-name ("magnetic force (~A component)" (axis-name ?axis))
  :english ("the force on a moving charge due to a magnetic field")
  :ExpFormat ("finding the force (~A component) on ~A due to the magnetic field"
	      (axis-name ?axis) (nlg ?body))
  :EqnFormat ((charge-force-Bfield-equation ?axis) ))

(defun charge-force-Bfield-equation (axis)
  (cond ((eq axis 'x) "F_x = q*(v_y*B_z - v_z*B_y)")
	((eq axis 'y) "F_y = q*(v_z*B_x - v_x*B_z)")
	((eq axis 'z) 
	 "F_z = q*v*B*sin($qB-$qv) or F_z = q*(v_x*B_y - v_y*B_x)")))

(defoperator charge-force-Bfield-contains (?sought)
  :preconditions 
  ((any-member ?sought ((compo ?not-axis ?rot (velocity ?b :time ?t))
			(compo ?not-axis ?rot 
			       (field ?loc magnetic ?source :time ?t))
			(mag (velocity ?b :time ?t))
			(mag (field ?loc magnetic ?source :time ?t))
			(charge ?b :time ?t)
		       ))
   (at-place ?b ?loc :time ?t ?t)  ;?b is not bound if field is sourght
   (axes-for ?b ?rot) ;?rot not bound if charge is sought
   (time ?t) ;sanity test
   (get-axis ?axis ?rot)  ;iterate over components
   (test (not (exactly-equal ?axis ?not-axis)))
   (any-member ?flag (t nil))
   (test (if ?flag (not (eq (car ?sought) 'mag))
	   (not (eq (car ?sought) 'compo))))
   )
  :effects 
  ((eqn-contains (charge-force-Bfield ?axis ?rot ?b ?flag ?t) ?sought)))

(defoperator charge-force-Bfield-contains-force (?sought)
  :preconditions 
  ;; the angle is not a valid sought because of the sine.
  ((any-member ?sought 
	       ((compo ?axis ?rot (force ?b ?source magnetic :time ?t))))
   (any-member ?flag (t nil))
   )
  :effects 
  ((eqn-contains (charge-force-Bfield ?axis ?rot ?b ?flag ?t) ?sought)))

(defoperator draw-charge-force-Bfield-diagram (?rot ?b ?t)
  :preconditions 
  (
   (debug "Using draw-charge-force-Bfield-diagram ~%")
   (not (vector-diagram ?rot (charge-force-Bfield ?b ?t)))
   (body ?b)
   (at-place ?b ?loc :time ?t ?t)
   (vector ?b (force ?b ?source magnetic :time ?t) ?dir2) ;this binds ?source
   ;; ?source needs to be bound for this to work
   (inherit-vector ?dontcare (field ?loc magnetic ?source :time ?t) ?dir1) 
   (vector ?b (velocity ?b :time ?t) ?dir3)
   (axes-for ?b ?rot)
   (debug "Fired draw-charge-force-Bfield-diagram ~%")
   )
  :effects ( (vector-diagram ?rot (charge-force-Bfield ?b ?t)) )
  :hint (
         (point (string "Try drawing a diagram."))
         (teach (string "The diagram should show the force vector and the magnetic field vector at ~a." ?b))
         (bottom-out (string "Draw a diagram showing the force vector on ~a due to the magnetic field at ~a." ?b ?loc))
          ))


(defoperator write-charge-force-Bfield (?axis ?rot ?b ?t )
  :preconditions 
  (
   (at-place ?b ?loc :time ?t ?t)
   (vector-diagram ?rot (charge-force-Bfield ?b ?t))
   ;; already drawn in vector-diagram, this binds ?source
   (in-wm  (vector ?b (force ?b ?source magnetic :time ?t) ?any-dir))
   (variable ?F (compo ?axis ?rot (force ?b ?source magnetic :time ?t)))
   (cross ?cross (velocity ?b :time ?t) 
	  (field ?loc magnetic ?source :time ?t) ?axis ?rot ?flag)
   (inherit-variable ?q (charge ?b :time ?t))
   )
  :effects 
  ( (eqn (= ?F (* ?q ?cross)) (charge-force-Bfield ?axis ?rot ?b ?flag ?t)) )
  :hint 
  (
   (point (string "There is a force acting on ~A due to ~A."
		  ?b ((field ?loc magnetic ?source :time ?t) def-np)))
   (teach (string "The ~A component of the magnetic force equation is ~A."
		  (?axis axis-name) 
		  (?axis charge-force-Bfield-equation)))
   (bottom-out (string "Write the equation ~A"  
		       ((= ?F (* ?q ?cross)) algebra) ))
   ))


;;;       Magnetic force on a wire

;; mag7 needs just the magnitude, still need to add components
(def-psmclass current-force-Bfield-mag (current-force-Bfield-mag ?body ?source ?time)
  :complexity major
  :short-name "magnetic force on a wire (magnitude)"
  :english ("force on a current carrying wire in a magnetic field")
  :ExpFormat ("finding the force on ~A in a magnetic field" (nlg ?body))
  :EqnFormat ("F = I*L*B*sin($q)" ))

(defoperator current-force-Bfield-mag-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ((mag (force ?b ?source magnetic :time ?t))
			(mag (field ?loc magnetic ?source :time ?t))
			(length ?b)
			(current-thru ?b :time ?t)))
   (time ?t)
   (object ?source)  ;this is not bound by most ?sought
   (at-place ?b ?loc :time ?t ?t)  ;so that ?b is bound
   )
  :effects(
           (eqn-contains (current-force-Bfield-mag ?b ?source ?t) ?sought)
           ))  

(defoperator write-current-force-Bfield-mag (?b ?t)
  :preconditions 
  (
   (at-place ?b ?loc :time ?t ?t)
   ;; draw body for this psm
   (body ?b)
   ;; draw the vectors B, and F.
   (vector ?dontcare1 (field ?loc magnetic ?source :time ?t-field) ?B-dir)
   (test (tinsidep ?t ?t-field))
   (vector ?dontcare3 (force ?b ?source magnetic :time ?t) ?F-dir)
   ;; retrieve vector variables for equation:
   (in-wm (variable ?magB (mag (field ?loc magnetic ?source :time ?t-field))))
   (in-wm (variable ?magF (mag (force ?b ?source magnetic :time ?t))))
   ;; direction of current (problem given)
   (given (dir (current-length ?b :time ?t)) ?dir-i)
   ;; define current variable
   (inherit-variable ?i (current-thru ?b :time ?t))
   (variable ?l (length ?b))
   ;; calculate angle between. Put it directly into eqn w/o variable.
   (bind ?theta `(dnum ,(get-angle-between ?dir-i ?B-dir) |deg|))
   (test ?theta) ; make sure it was determinable
   )
  :effects ((eqn (= ?magF (* ?i ?l ?magB (sin ?theta))) 
		 (current-force-Bfield-mag ?b ?source ?t)))
  :hint 
  (
   (point (string "You can calculate magnitude of the magnetic force on ~A due to ~A." 
		  ?b ((field ?loc magnetic ?source :time ?t) def-np)))
   (teach (string "The magnitude of the magnetic force on a wire in a magnetic field is the product of the current, the length of the wire, the magnetic field and the angle between the direction of the current and the magnetic field vector." ))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?magF (* ?i ?l ?magB (sin ?theta))) algebra)))
   ))


#| ; not using this, just show dir on diagram
(defoperator charge-force-Bfield-dir-contains (?sought)
  :preconditions 
  ((debug "Using write-charge-force-Bfield-dir-contains ~%")
   (any-member ?sought ((dir (force ?b ?source magnetic :time ?t))
			(dir (field ?loc magnetic ?source :time ?t))
			(charge ?b :time ?t)))
   (body ?b)
   (rdebug "Firing write-charge-force-Bfield-dir-contains ~%")
   )
  :effects(
           (eqn-contains (charge-force-Bfield-dir ?b) ?sought)
           )) 

(defoperator write-force-dir-charge-Bfield-pos (?b ?t)
  :preconditions 
  ((debug "Using write-force-dir-charge-Bfield-pos ~%")
   (at-place ?b ?loc :time ?t ?t)
   (inherit-variable ?q (charge ?b :time ?t))
   (given (dir (field ?loc magnetic ?source :time ?t ?t)) (dnum ?dir1 ?doncare1)) 
   (motion ?b straight :dir (dnum ?dir2 ?dontcare1) :time ?t ?t . ?rest-motion)
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
   (at-place ?b ?loc :time ?t ?t)
   (inherit-variable ?q (charge ?b :time ?t))
   (given (dir (field ?loc magnetic ?source :time ?t ?t)) 
	  (dnum ?dir1 ?whever)) 
   (motion ?b straight :dir (dnum ?dir2 ?dontcare1) :time ?t ?t . ?rest) 
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


;;;
;;;                    Biot-Savart law for a point particle
;;;

(def-psmclass biot-savert-point-particle-mag 
  (biot-savert-point-particle-mag ?loc ?b ?dir-vec ?time)
  :complexity major 
  :short-name "Biot-Savart law for a point charge (magnitude)"
  :english ("the magnetic field due to a moving point charge")
  :expformat ((strcat "using the Biot-Savart law to find the magnetic field "
		      "at ~A due to ~A")
	      (nlg ?loc) (nlg ?b 'at-time ?time))
  :EqnFormat "B = $m0*abs(q)*v*sin($q)/(4*$p*r^2)")

(defoperator biot-savert-point-particle-mag-contains (?sought)
   :preconditions 
   (
   (any-member ?sought (
			 ;; can't find charge as sought.
			 (mag (field ?loc magnetic ?b :time ?t))
			 (mag (velocity ?b :time ?t))
			 (mag (relative-position ?loc ?b :time ?t))
			 ))
   (point-charge ?b)
   (time ?t) ;sanity check
   (at-place ?loc ?region :time ?t ?t)  ;sometimes ?loc is not bound
   (any-member ?dir-vec (
			 (unit-vector towards ?loc :at ?b :time ?t)
			 (relative-position ?loc ?b :time ?t)))
   )
   :effects 
   ((eqn-contains (biot-savert-point-particle-mag ?loc ?b ?dir-vec ?t) 
		  ?sought)))

(defoperator write-biot-savert-point-particle-mag (?loc ?b ?dir-vec ?t)
  :preconditions 
  (
   (variable ?B-var (mag (field ?loc magnetic ?b :time ?t)))
   (variable ?v-var (mag (velocity ?b :time ?t)))
   (variable ?r-var (mag (relative-position ?loc ?b :time ?t)))
   (inherit-variable ?q-var (charge ?b :time ?t))
   (variable ?theta-var (angle-between orderless
				       (velocity ?b :time ?t) ?dir-vec))
   )
  :effects (
	    (eqn (= ?B-var (/ (* |mu0| (abs ?q-var) ?v-var (sin ?theta-var)) 
			      (* 4 $p (^ ?r-var 2)))) 
		 (biot-savert-point-particle-mag ?loc ?b ?dir-vec ?t))
	    (assume using-magnitude 
		    (biot-savert-point-particle ?loc ?b ?t))
   )
   :hint
   ( (point (string "What is the magnitude of the magnetic field at ~A due to the moving charge ~A?" ?loc ?b))
     ;; We really need a tutorial for this
     (teach (string "The magnetic field produced by a moving point charge is given by the Biot-Savart law.  Read about the Biot-Savart law in your textbook."))
     (bottom-out (string "Write the equation ~A" 
			 ((= ?B-var (/ (* |mu0| (abs ?q-var) ?v-var (sin ?theta-var)) 
				       (* 4 $p (^ ?r-var 2)))) algebra)))
     ))

;; The vector psm is not really set up to handle cross products...

(def-psmclass biot-savert-point-particle 
  (biot-savert-point-particle ?loc ?b ?axis ?rot ?flag ?t) 
  :complexity major
  :short-name ("Biot-Savart law for a point charge (~A component)" 
	       (axis-name ?axis))
  :english ("the magnetic field due to a moving charge")
  :expformat ((strcat "using the Biot-Savart law to find the ~A component "
		      "of the magnetic field at ~A due to ~A")
	      (axis-name ?axis) (nlg ?loc) (nlg ?b 'at-time ?time))
  :EqnFormat ((biot-savert-law-equation ?axis)))

(defun biot-savert-law-equation (xyz)
  (cond ((eq xyz 'x) "B_x = $m0*q*(v_y*n_z - v_z*n_y)/(4*$p*r^2)")
	((eq xyz 'y) "B_y = $m0*q*(v_z*n_x - v_x*n_z)/(4*$p*r^2)")
	((eq xyz 'z) "B_z = $m0*q*v*sin($qn-$qv)/(4*$p*r^2) or B_z = $m0*q*(v_x*n_y - v_y*n_x)/(4*$p*r^2)")))

(defoperator biot-savert-point-particle-contains-angle (?sought)
  :preconditions 
  (
   (any-member ?sought 
	       ( 
		(compo ?axis ?rot (field ?loc magnetic ?b :time ?t))
		(charge ?b :time ?t)
		(mag (relative-position ?loc ?b :time ?t))
		(mag (velocity ?b :time ?t))
		(dir (velocity ?b :time ?t))
		(dir (unit-vector towards ?loc :at ?b :time ?t))
		))
   (time ?t)
   (point-charge ?b)
   (axes-for ?b ?rot) ;in case ?rot is not bound
   (get-axis ?axis ?rot) ;in case ?axis is not bound
   (at-place ?loc ?region :time ?t ?t)  ;sometimes ?loc is not bound
   )
  :effects 
  ( (eqn-contains 
     (biot-savert-point-particle ?loc ?b ?axis ?rot nil ?t) ?sought) ))

(defoperator biot-savert-point-particle-contains-compo (?sought)
  :preconditions 
  (
   (any-member ?sought 
	       ( 
		(compo ?axis ?rot (field ?loc magnetic ?b :time ?t))
		(charge ?b :time ?t)
		(mag (velocity ?b :time ?t))
		(compo ?not-axis ?rot (velocity ?b :time ?t))
		(compo ?not-axis ?rot (unit-vector towards ?loc :at ?b :time ?t))
		(mag (relative-position ?loc ?b :time ?t))
		))
   (time ?t)
   (point-charge ?b)
   (axes-for ?b ?rot) ;in case ?rot is not bound
   (get-axis ?axis ?rot) ;in case ?axis is not bound
   (at-place ?loc ?region :time ?t ?t)  ;sometimes ?loc is not bound
  )
 :effects 
 ( (eqn-contains (biot-savert-point-particle ?loc ?b ?axis ?rot t ?t)
		 ?sought) ))

(defoperator write-biot-savert-point-particle (?loc ?b ?axis ?rot ?flag ?t)
  :preconditions 
  ( 
   ;; draw vectors now, before applying cross product
   (vector ?any (field ?loc magnetic ?b :time ?t) ?dir-B)
   (vector ?whatever (field ?loc magnetic ?b :time ?t) ?dir-field)
   (vector ?b (velocity ?b :time ?t) ?dir-v)
   (vector ?what (unit-vector towards ?loc :at ?b :time ?t) ?dir-r)
   (optional (body ?b))
   ;;
   (variable ?B-var (compo ?axis ?rot (field ?loc magnetic ?b :time ?t)))
   (inherit-variable ?q-var (charge ?b :time ?t))
   (cross ?cross (velocity ?b :time ?t) 
	  (unit-vector towards ?loc :at ?b :time ?t) ?axis ?rot ?flag)
   (test (not (eq ?cross '0)))	   ; handled by write-biot-savert-point-particle-mag
   (variable ?r-var (mag (relative-position ?loc ?b :time ?t)))
   )
  :effects 
  ( (eqn (= ?B-var (/ (* |mu0| ?q-var ?cross) (* 4 $p (^ ?r-var 2))))
	 (biot-savert-point-particle ?loc ?b ?axis ?rot ?flag ?t))
    ;; disallow both component-form and magnitude form in a solution
    (assume using-compo 
	    (compo ?axis ?rot (biot-savert-point-particle ?loc ?b ?t)))
    )
   :hint
   ( (point (string "What is the magnetic field at ~A due to the moving charge ~A?" ?loc ?b))
     ;; We really need a tutorial for this
     (teach (string "The magnetic field produced by a moving point charge is given by the Biot-Savart law.  Read about the Biot-Savart law in your textbook."))
    (bottom-out (string "Write the equation ~A" 
			((= ?B-var (/ (* |mu0| ?q-var ?cross) (* 4 $p (^ ?r-var 2))))
			  algebra)))
    ))


;;;              Magnetic field of a straight wire

(def-psmclass straight-wire-Bfield (straight-wire-Bfield ?point ?wire ?t)
  :complexity major
  :short-name "magnetic field of a straight wire"
  :english ("the magnetic field from current flowing through a straight wire")
  :ExpFormat ("finding the magnetic field due to a current flowing through ~A" (nlg ?wire))
  :EqnFormat ("B = $m0*I/(2*$p*r)" ))

(defoperator straight-wire-bfield-contains (?sought)
  :preconditions 
  (
   ;; relative-position must be perpendicular to the wire
   (given (dir (current-length ?wire :time ?t)) ?dir-i)
   (given (dir (relative-position ?point ?wire :time ?t)) ?dir-r)
   (test (perpendicularp ?dir-i ?dir-r))
   (any-member ?sought (
			(current-thru ?wire :time ?t)
			(mag (relative-position ?point ?wire :time ?t))
			(mag (field ?point magnetic ?wire :time ?t))
			))
   (time ?t) ;sanity test
   )
  :effects ((eqn-contains (straight-wire-Bfield ?point ?wire ?t) ?sought)))

(defoperator write-straight-wire-Bfield (?point ?wire ?t)
  :preconditions 
  ( 
   (inherit-variable ?I (current-thru ?wire :time ?t))
   (variable ?r (mag (relative-position ?point ?wire :time ?t)))
   (inherit-variable ?B (mag (field ?point magnetic ?wire :time ?t)))
   )
  :effects ( 
	    (eqn (= (* 2 $p ?r ?B) (* |mu0| ?I))
		 (straight-wire-Bfield ?point ?wire ?t))
	    )
  :hint (
	 (point (string "What is the magnetic field at ~A due to the current flowing in ~A?" ?point ?wire))
	 (teach (string "Find the formula for the magnetic field due to the current flowing through a straight wire."))
	 (bottom-out (string "Write the equation ~A"  
			     ((= ?B (/ (* |mu0| ?I) (* 2 $p ?r))) algebra) ))
	 ))

;;;              Magnetic field at the center of a coil

(def-psmclass center-coil-Bfield 
  (center-coil-Bfield ?center ?coil ?t :loop ?flag)
  :complexity major
  :short-name "magnetic field at center of a thin coil"
  :english ("the magnetic field at the center of a ~:[coil of N turns~;single loop~]" 
	    ?flag)
  :ExpFormat ("finding the magnetic field at the center of ~A" (nlg ?coil))
  :EqnFormat ("B = $m0*N*I/(2*r)" ))

(defoperator center-coil-Bfield-contains (?sought)
  :preconditions 
  (
   (center-of-coil ?point ?coil . ?rest)  ;given that there is a coil
   (any-member ?sought (
			(current-thru ?coil :time ?t)
			(turns ?coil)
			(radius-of-circle ?coil)
			(mag (field ?center magnetic ?coil :time ?t))
			))
   ;; determine whether coil is a single loop
   (setof (given (turns ?coil) 1) 1 ?flag)
   (time ?t) ;not bound by some ?sought
   )
  :effects ((eqn-contains (center-coil-Bfield ?point ?coil ?t :loop ?flag) ?sought)))

(defoperator use-turns-for-turns (?coil)
  :preconditions ((variable ?N (turns ?coil)))
  :effects ((use-for-turns ?N ?coil :loop nil)))

(defoperator use-no-turns (?coil)
  :preconditions ((test ?flag))
  :effects ((use-for-turns nil ?coil :loop ?flag)))

(defoperator write-center-coil-Bfield (?point ?coil ?t ?flag)
  :preconditions 
  ( 
   (inherit-variable ?I (current-thru ?coil :time ?t))
   (use-for-turns ?N ?coil :loop ?flag)
   (variable ?r	(radius-of-circle ?coil))
   (inherit-variable ?B (mag (field ?point magnetic ?coil :time ?t)))
   (bind ?rhs (if ?N `(* |mu0| ,?N ,?I) `(* |mu0| ,?I)))
   )
  :effects ( 
	    (eqn (= (* 2 ?r ?B) ?rhs)
		 (center-coil-Bfield ?point ?coil ?t :loop ?flag))
	    )
  :hint (
	 (point (string "What is the magnetic field at ~A due to the current flowing in ~A?" ?point ?coil))
	 (teach (string "Find the formula for the magnetic field at the center of a ~:[coil of N turns~;loop of current~]." (?flag identity)))
	 (bottom-out (string "Write the equation ~A"  
			     ((= ?B (/ ?rhs (* 2 ?r))) algebra) ))
	 ))

;;;              Magnetic field inside a long solenoid

(def-psmclass inside-solenoid-Bfield (inside-solenoid-Bfield ?center ?solenoid ?t)
  :complexity major
  :short-name "magnetic field inside a long solenoid"
  :english ("the magnetic field inside a long solenoid")
  :ExpFormat ("finding the magnetic field inside ~A" (nlg ?solenoid))
  :EqnFormat ("B = $m0*n*I" ))

(defoperator inside-solenoid-Bfield-contains (?sought)
  :preconditions 
  (
   (inside-solenoid ?point ?solenoid)  ;given that there is a solenoid
   (any-member ?sought (
			(current-thru ?solenoid :time ?t ?t)
			(turns-per-length ?solenoid)
			(mag (field ?center magnetic ?solenoid :time ?t ?t))
			))
   (time ?t)  ;should be made timeless
   )
  :effects ((eqn-contains (inside-solenoid-Bfield ?point ?solenoid ?t) ?sought)))

(defoperator write-inside-solenoid-Bfield (?point ?solenoid ?t)
  :preconditions 
  ( 
   (inherit-variable ?I (current-thru ?solenoid :time ?t))
   (variable ?n (turns-per-length ?solenoid))
   (inherit-variable ?B (mag (field ?point magnetic ?solenoid :time ?t)))
   )
  :effects ( 
	    (eqn (= ?B (* |mu0| ?n ?I))
		 (inside-solenoid-Bfield ?point ?solenoid ?t))
	    )
  :hint (
	 (point (string "What is the magnetic field at ~A due to ~A?" ?point ?solenoid))
	 (teach (string "Find the formula for the magnetic field inside a long solenoid."))
	 (bottom-out (string "Write the equation ~A"  
			     ((= ?B (* |mu0| ?n ?I)) algebra) ))
	 ))


;;;  definition of turns and turns per unit length

(def-qexp turns (turns ?body)
  :symbol-base |N|     
  :short-name "turns" 
  :pre-dialog-text "number of turns" 
  :dialog-text "wrapped around [body:bodies]"
  :units NIL  ;dimensionless
     :restrictions positive
     :english ("the number of turns wrapping around ~A" (nlg ?body))
     :fromworkbench `(turns ,body)
   )

(defoperator define-turns (?body)
     :preconditions((bind ?N-var (format-sym "Nt_~A" (body-name ?body))))
     :effects ((variable ?N-var (turns ?body))
               (define-var (turns ?body))
   )
     :hint 
     ((bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting turns."  
			      ((turns ?body) def-np))) ))


(def-qexp turns-per-length (turns-per-length ?body)
  :symbol-base |n|     
  :short-name "turns per unit length" 
  :pre-dialog-text "number of turns per unit length" 
  :dialog-text "wrapped around [body:bodies]"
     :units |m^-1|
     :restrictions positive
     :english ("the number of turns per length wrapping around ~A" (nlg ?body))
     :fromworkbench `(turns-per-length ,body)
   )

(defoperator define-turns-per-length (?body)
     :preconditions((bind ?N-var (format-sym "ntl_~A" (body-name ?body))))
     :effects ((variable ?N-var (turns-per-length ?body))
               (define-var (turns-per-length ?body))
   )
     :hint 
     ((bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting turns per unit length."  
			      ((turns-per-length ?body) def-np))) ))

;;; turns per length = turns/length

(def-PSMclass turns-per-length-definition (turns-per-length ?b)
  :complexity definition
  :doc "turns per length = turns/length"
  :short-name "turns per unit length"
  :english ("turns per length = turns/length")
  :expFormat ("using the turns per length of ~A" (nlg ?b))
  :EqnFormat ("n = N/l"))

(defoperator turns-per-length-contains (?quantity)
  :preconditions (
		  (any-member ?quantity
			      ((turns-per-length ?b)
			       (length ?b)
			       (turns ?b))))
  :effects
  ((eqn-contains (turns-per-length ?b) ?quantity)))

(defoperator write-turns-per-length (?b)
  :preconditions (
		  (variable ?m (turns ?b))
		  (variable ?l (length ?b))
		  (variable ?mu (turns-per-length ?b)))
  :effects
  ((eqn (= ?mu (/ ?m ?l)) (turns-per-length ?b)))
  :hint
  ((point (string "Use the turns per unit length of ~A." ?b))
   (teach (string "The turns per unit length is the total turns wrapped around ~a divided by the length of ~a" ?b ?b))
   (bottom-out (string "Write the equation ~A" ((= ?mu (/ ?m ?l)) algebra)))
   ))


;;;      Flux through a surface for a field that is constant over
;;;      the surface in the direction orthogonal to the surface.

(def-qexp electric-flux (flux ?surface electric :time ?t)
  :symbol-base |$F|     
  :short-name "electric flux"	
  :dialog-text "through [body:bodies] at time [time:times]"
     :units |V.m|
     :fromworkbench `(flux ,body electric :time ,time)
     :english ("electric flux through ~A~@[ ~A~]" 
	       (nlg ?surface) (nlg ?t 'pp)))

(def-qexp electric-flux-change (rate-of-change 
				(flux ?surface electric :time ?t))
  :symbol-base |d$Fdt|     
  :short-name "rate of change in electric flux"	
  :dialog-text "through [body:bodies] at time [time:times]"
     :units |V.m/s|
     :fromworkbench `(rate-of-change (flux ,body electric :time ,time))
     :english ("rate of change in electric flux through ~A~@[ ~A~]" 
	       (nlg ?surface) (nlg ?t 'pp)))

(def-qexp magnetic-flux (flux ?surface magnetic :time ?t)
  :symbol-base |$F|     
  :short-name "magnetic flux"	
  :dialog-text "through [body:bodies] at time [time:times]"
     :units |T.m^2|
     :fromworkbench `(flux ,body magnetic :time ,time)
     :english ("magnetic flux through ~A~@[ ~A~]" 
	       (nlg ?surface) (nlg ?t 'pp)))

(def-qexp magnetic-flux-change (rate-of-change 
				(flux ?surface magnetic :time ?t))
  :symbol-base |d$Fdt|     
  :short-name "rate of change in magnetic flux"	
  :dialog-text "through [body:bodies] at time [time:times]"
     :units |T.m^2/s|
     :fromworkbench `(rate-of-change (flux ,body magnetic :time ,time))
     :english ("rate of change in magnetic flux through ~A~@[ ~A~]" 
	       (nlg ?surface) (nlg ?t 'pp)))

(defoperator define-flux (?surface ?type ?t)
 :preconditions 
 ((bind ?flux-var (format-sym "Phi~A_~A~@[_~A~]" 
			     (subseq (string ?type) 0 1)
			     (body-name ?surface) (time-abbrev ?t))) )
 :effects (
   (define-var (flux ?surface ?type :time ?t))
   (variable ?flux-var (flux ?surface ?type :time ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting ~A flux" 
		       ((flux ?surface ?type :time ?t) def-np) (?type adj)))
 ))

(def-psmclass electric-flux-constant-field
  (flux-constant-field ?surface electric ?time ?rot)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "electric flux (uniform field)"
  :english ("the definition of electric flux through a surface")
  :expformat ("calculating the ~A" 
	      (nlg (list 'flux ?surface 'electric :time ?time)))
  :EqnFormat ("$Fe = A*E*cos($qE - $qn) or $Fe = A*(E_x*n_x + E_y*n_y)"))

(def-psmclass magnetic-flux-constant-field
  (flux-constant-field ?surface magnetic ?time ?rot)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "magnetic flux (constant field)"
  :english ("the definition of magnetic flux through a surface")
  :expformat ("calculating the ~A" 
	      (nlg (list 'flux ?surface 'magnetic :time ?time)))
  :EqnFormat ("$Fb = A*B*cos($qB - $qn) or $Fb = A*(B_x*n_x + B_y*n_y)"))

(defoperator flux-constant-field-angle-contains (?sought)
  :preconditions 
  ((any-member ?sought 
	       ( (flux ?surface ?type :time ?t)
		 (mag (field ?region ?type ?source :time ?t))
		 (area ?surface)
		 (angle-between orderless (field ?region ?type ?source :time ?t)
				(unit-vector normal-to ?surface :time ?t))
		 ))
   ;; this tests validity of law
   (homogeneous-field ?region ?type ?source :time ?t-given :dir ?dir) 
   (at-place ?surface ?region)
   (time-or-timeless ?t) ;time not bound by area
   (test (tinsidep ?t ?t-given))
   )
  :effects ((eqn-contains (flux-constant-field ?surface ?type ?t NIL) ?sought)
  ))


(defoperator flux-constant-field-compo-contains (?sought)
  :preconditions 
  ((any-member ?sought 
	       ( (flux ?surface ?type :time ?t)
		 (area ?surface)
		 (compo ?xyz ?rot (field ?region ?type ?source :time ?t))
		 (compo ?xyz ?rot (unit-vector normal-to ?surface :time ?t))
		 ))
   ;; this tests validity of law
   (homogeneous-field ?region ?type ?source :time ?t-given :dir ?dir) 
   (time-or-timeless ?t)
   (test (tinsidep ?t ?t-given))
   (at-place ?surface ?region)
      ;; find axes now, before applying dot product:
   (vector ?any-body (field ?region ?type ?source :time ?tot) ?dir-d)
   (inherit-or-quantity (field ?region ?type ?source :time ?t)
			(field ?region ?type ?source :time ?tot))
   (vector ?surface (unit-vector normal-to ?surface :time ?t) ?dir-e)
   ;; If ?rot is unbound, draw-rotate-axes or draw-standard-axes
   ;; etc. will choose the angle.  If it is bound from the ?sought,
   ;; operator will also succeed.
   (axes-for ?surface ?rot) 
   )
  :effects 
  ((eqn-contains (flux-constant-field ?surface ?type ?t ?rot) ?sought)
   (assume axes-for ?surface ?rot)
 ))

;; make sure there is only one field defined in region
;; if we have multiple fields, the associated law probably should be expressed
;; in terms of the net field.
(defoperator collect-one-field (?region ?type ?source ?t)
  :preconditions 
  (
   (setof (inherit-proposition (field ?region ?type ?any-source :time ?t)
			       (field ?region ?type . ?rest)
			       (vector ?dontcare (field ?region ?type . ?rest) 
				       ?any-dir))
	  ?any-source ?sources)
   (test (= 1 (length ?sources))) ;exactly one field at surface
   (bind ?source (first ?sources))
   )
  :effects 
  ((collect-one-source (field ?region ?type ?source :time ?t))))

;; This can write either the component or the angle form of the 
;; electric dipole energy equation, depending on ?rot.  
(defoperator write-flux-constant-field (?surface ?type ?t ?rot)
  :preconditions 
  (
   (at-place ?surface ?region)
   (collect-one-source (field ?region ?type ?source :time ?t))
   ;;
   (dot ?dot (field ?region ?type ?source :time ?t)
	(unit-vector normal-to ?surface :time ?t)
	?rot :nonzero ?nonzero)
   (test (not (equal ?dot 0))) ;zero handled separately
   (variable ?Phi-var (flux ?surface ?type :time ?t))
   (variable ?A (area ?surface))
   )
  :effects ( (eqn (= ?Phi-var (* ?A ?dot))
		  (flux-constant-field ?surface ?type ?t ?rot)) )
  :hint (
	 (point (string "Note that the ~A field is uniform over ~A." 
			(?type adj) ?surface))
	 (teach (string "For a field that is uniform over a surface, the ~A flux through the surface is the area times the dot product of the ~A field and the unit normal to the surface."
			(?type adj) (?type adj)))
	 (bottom-out (string "Write the equation ~A."  
			     ((= ?Phi-var (* ?A ?nonzero)) algebra)))
	 ))

(defoperator write-flux-zero (?surface ?type ?t ?rot)
  :preconditions 
  (
   (at-place ?surface ?region)
   (collect-one-source (field ?region ?type ?source :time ?t))
   ;;
   (dot 0 (field ?region ?type ?source :time ?t)
	(unit-vector normal-to ?surface :time ?t)
	?rot . ?whatever)
   (variable ?Phi-var (flux ?surface ?type :time ?t))
   )
  :effects ( (eqn (= ?Phi-var 0)
		  (flux-constant-field ?surface ?type ?t ?rot)) )
  :hint (
	(point (string "Note that the ~A field is parallel to ~A or is zero." 
		       (?type adj) ?surface))
	(teach (string "If the component of the ~A field perpendicular to a surface is zero, then the ~A flux going through the surface is zero." 
		       (?type adj) (?type adj)))
	(bottom-out (string "Write the equation ~A."  
			    ((= ?Phi-var 0) algebra)))
	))

;;;; This is really clunky, it is just the time derivative of
;;;; flux-constant-field above

(def-psmclass electric-flux-constant-field-change
  (flux-constant-field-change ?surface electric ?field ?time ?rot)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "derivative of electric flux, uniform field"
  :english ("the time derivative of the definition of electric flux through a surface")
  :expformat ("calculating the ~A" 
	      (nlg (list 'rate-of-change 
			 (list 'flux ?surface 'electric :time ?time))))
  :EqnFormat ("d$Fe/dt = E.n*dA/dt"))

(def-psmclass magnetic-flux-constant-field-change
  (flux-constant-field-change ?surface magnetic ?time ?rot)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "rate of change in magnetic flux (constant field)"
  :english ("the time derivative of the definition of magnetic flux through a surface")
  :expformat ("calculating the ~A" 
	      (nlg (list 'rate-of-change 
			 (list 'flux ?surface 'magnetic :time ?time))))
  :EqnFormat ("d$Fb/dt = B.n*dA/dt"))

(defoperator flux-constant-field-change-angle-contains (?sought)
  :preconditions 
  ((any-member ?sought 
	       ( (rate-of-change (flux ?surface ?type :time ?t))
		 (mag (field ?region ?type ?source :time ?t))
		 (rate-of-change (area ?surface))
		 ))
   ;; this tests validity of law
   (homogeneous-field ?region ?type ?source :time ?t-given :dir ?dir) 
   (time-or-timeless ?t)
   (test (tinsidep ?t ?t-given))
   (at-place ?surface ?region)
   )
  :effects ((eqn-contains (flux-constant-field-change ?surface ?type ?t NIL) ?sought)
  ))


(defoperator flux-constant-field-change-compo-contains (?sought)
  :preconditions 
  ((any-member ?sought 
	       ( (rate-of-change (flux ?surface ?type :time ?t))
		 (rate-of-change (area ?surface))
		 (compo ?xyz ?rot (field ?region ?type ?source :time ?t))
		 (compo ?xyz ?rot (unit-vector normal-to ?surface :time ?t))
		 ))
   ;; this tests validity of law
   (homogeneous-field ?region ?type ?source :time ?t-given :dir ?dir) 
   (time-or-timeless ?t)
   (test (tinsidep ?t ?t-given))
   (at-place ?surface ?region)
   ;; find axes now, before applying dot product:
   ;; (draw vectors before finding axis)
   (vector ?any-body (field ?region ?type ?source :time ?tot) ?dir-d)
   (inherit-or-quantity (field ?region ?type ?source :time ?t)
			(field ?region ?type ?source :time ?tot))
   (vector ?surface (unit-vector normal-to ?surface :time ?t) ?dir-e)
   ;; If ?rot is unbound, draw-rotate-axes or draw-standard-axes
   ;; etc. will choose the angle.  If it is bound from the ?sought,
   ;; operator will also succeed.
   (axes-for ?surface ?rot) 
   )
  :effects 
  ((eqn-contains (flux-constant-field-change ?surface ?type ?t ?rot) ?sought)
   (assume axes-for ?surface ?rot)
 ))

;; This can write either the component or the angle form of the 
;; electric dipole energy equation, depending on ?rot.  
(defoperator write-flux-constant-field-change (?surface ?type ?t ?rot)
 :preconditions 
 (
  (at-place ?surface ?region)
  (collect-one-source (field ?region ?type ?source :time ?t))
  ;;
  (dot ?dot (field ?surface ?type ?source :time ?t)
       (unit-vector normal-to ?surface :time ?t)
       ?rot :nonzero ?nonzero)
  (variable ?Phi-var (rate-of-change (flux ?surface ?type :time ?t)))
  (variable ?A (rate-of-change (area ?surface)))
  )
 :effects 
 ((eqn (= ?Phi-var (* ?A ?dot))
       (flux-constant-field-change ?surface ?type ?t ?rot))
  )
 :hint (
	(point (string "Note that the ~A flux through ~A is changing." 
		       ?type ?surface))
	(teach (string "For a constant, uniform field, the time derivative of the flux through a surface is the time derivative of the area times the dot product of the field and the unit normal to the surface." ?type))
	(bottom-out (string "Write the equation ~A."  
			    ((= ?Phi-var (* ?A ?nonzero)) algebra)))
	))


;;;
;;;                         Gauss' law
;;;

(def-psmclass gauss-law (gauss-law ?surface :sum ?flag :time ?t)
  :complexity major                   ; must explicitly use
  :short-name "Gauss' law"
  :english ("Gauss' law")
  :ExpFormat ("using Gauss' law")
  :EqnFormat ("$Fe = Q/$e0"))

(defoperator gauss-law-contains (?sought)
  :preconditions
  (
   (closed-surface ?surface . ?rest)
   (any-member ?sought ((charge ?surface :surface t)
			(flux ?surface electric :time ?t)))
   (time ?t) ;in case t is not bound
   )
  :effects
  ( (eqn-contains (gauss-law ?surface :time ?t) ?sought)))

(defoperator gauss-law-sum-contains (?sought)
  :preconditions
  (
   (closed-surface ?surface . ?rest)
   (at-place ?loc ?surface)
   (any-member ?sought ((charge ?loc)
			(flux ?surface electric :time ?t)))
   (time ?t) ;in case t is not bound
   )
  :effects
  ( (eqn-contains (gauss-law ?surface :sum t :time ?t) ?sought)))

(defoperator write-charges-as-net (?surface ?t)
  :preconditions ((variable  ?Q (charge ?surface :surface t)))
  :effects ((total-charge-term ?Q ?surface ?t nil)))

(defoperator write-charges-as-sum (?surface ?t)
  :preconditions 
  (
   (setof (at-place ?loc ?surface) ?loc ?locs)
   (map ?loc ?locs (variable ?Q-var (charge ?loc)) ?Q-var ?Q-vars)
   (bind ?Q-term (format-plus ?Q-vars))
   )
  :effects ((total-charge-term ?Q-term ?surface ?t t)))

(defoperator write-gauss-law (?surface ?t)
  :preconditions
  (
   (variable  ?phi (flux ?surface electric :time ?t)) 
   (total-charge-term ?Q-term ?surface ?t ?flag)
   )
  :effects
  ( (eqn  (= ?phi (/ ?Q-term |eps0|))
	  (gauss-law ?surface :sum ?flag :time ?t)) )
  :hint
  ( (point (string "Note that you can relate the flux through ~A to the charge inside ~A."
		   ?surface ?surface))
    (teach (string "Gauss law states that the electric flux through a closed surface is equal to the total charge inside divided by $e0."))
    (bottom-out (string "Write the equation ~A ."
			((= ?phi (/ ?Q-term |eps0|)) algebra) )) ))

(def-psmclass sum-fluxes (sum-fluxes ?b ?parts ?type ?t)
  :complexity minor
  :short-name "adding fluxes"
  :english ("add ~A flux going through different surfaces" (nlg ?type 'adj))
  :ExpFormat ("finding the total ~A flux through ~A" (nlg ?type 'adj) (nlg ?b))
  :EqnFormat ("$F = $F1 + $F2 + ..."))

(defoperator sum-fluxes-contains (?sought)
  :preconditions 
  (
   (composite-surface ?s ?parts)
   (any-member ?sought ( (flux ?b ?type :time ?t)))
   (test (or (equal ?b ?s) (member ?b ?parts)))
   (time ?t) ;sanity test
   )
  :effects 
  ((eqn-contains (sum-fluxes ?s ?parts ?type ?t) ?sought)))

;; only handles writing as sum of atomic sub-intervals
(defoperator write-sum-fluxes (?s ?type ?t)
  :preconditions 
  ((variable ?tt-var (flux ?s ?type :time ?t))
   (map ?part ?parts
      (variable ?t-var (flux ?part ?type :time ?t))
      ?t-var ?t-vars))
  :effects ( (eqn (= ?tt-var (+ . ?t-vars)) (sum-fluxes ?s ?parts ?type ?t)) )
  :hint
  ((point (string "Note that surface ~A is made by joining several surfaces together." ?s))
   (teach (string "The total flux going through a surface is equal to the sum of the flux going through each part of the surface."))
   (bottom-out (string "Write the equation ~a."
		        ((= ?tt-var (+ . ?t-vars)) algebra)))
   ))

;;;
;;;              Faraday's law
;;;

(def-psmclass faradays-law (faradays-law ?surface ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "Faraday's law"
  :english ("Faraday's law")
  :expformat ("applying Faradays law to ~A" (nlg ?surface)) 
  :EqnFormat ("V = -N*d$Fb/dt"))


(defoperator faradays-law-contains (?sought)
  :preconditions 
  (
   ;; assuming EMF induces voltage across ?R
   ;; specify by hand whether formula is valid
   (faraday-loop ?surface ?R :turns ?turn-flag) 
   (any-member ?sought 
	       ( (rate-of-change (flux ?surface magnetic :time ?t))
		 (turns ?R)
		 (voltage-across ?R :time ?t)))
   (time ?t)
   (test (if (eq (first ?sought) 'turns) ?turn-flag t))
   )
  :effects ((eqn-contains (faradays-law ?surface ?t) ?sought)
  ))

(defoperator write-faradays-law (?surface ?r ?t)
 :preconditions 
 (
  (in-wm (faraday-loop ?surface ?R :turns ?turn-flag))
  (inherit-variable ?V (voltage-across ?R :time ?t))
  (variable ?Phi-var (rate-of-change (flux ?surface magnetic :time ?t)))
  (variable ?turn-var (turns ?R))
  (bind ?phi-term (if ?turn-flag `(* ,?turn-var ,?Phi-var) ?Phi-var))
 )
 :effects 
 ( (eqn (= ?Phi-term (- ?V)) (faradays-law ?surface ?t)) )
 :hint (
	(point (string "Note that the magnetic flux through ~A is changing." 
		        ?surface))
	(teach (string "Faraday's law states that changing magnetic flux through a surface induces a voltage around the edge of that surface."))
 	(bottom-out (string "Write the equation ~A."  
			    ((= ?Phi-term (- ?V)) algebra)))
	))


;;;
;;;    Ampere's law
;;; 

;; This is a rather primitive first attempt.
;; ideally, we should have the path and field as separate arguments
(def-qexp magnetic-line-integral (line-integral 
				  (net-field ?path magnetic :time ?t))
  :symbol-base |intB|     
  :short-name "line integral of B"
  :pre-dialog-text "line integral of the net magnetic field"
  :dialog-text "along path [body:positions]"
  :units |T.m|
  :fromworkbench `(line-integral (net-field ,body magnetic))
  :english ("line integral of the net magnetic field along path ~A~@[ ~A~]" 
	       (nlg ?path) (nlg ?t 'pp)))

(defoperator define-line-integral (?path ?type ?t)
  :preconditions 
  ((bind ?int-var (format-sym "int~A_~A~@[_~A~]" 
			      (subseq (string ?type) 0 1)
			      (body-name ?path) (time-abbrev ?t))) )
  :effects 
  (
   (define-var (line-integral (net-field ?path ?type :time ?t)))
   (variable ?int-var (line-integral (net-field ?path ?type :time ?t)))
   )
  :hint 
  (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting the ~A line integral" 
		       ((line-integral 
			 (net-field ?surface ?type :time ?t)) def-np) 
		       (?type adj)))
   ))

(def-psmclass amperes-law (amperes-law :surface ?S)
  :complexity major			;must explicitly use
  ;; Allegro lisp character encoding has trouble reading in "`e"
  :short-name ("Amp~Cre's law" (code-char 232))
  :english ("Amp~Cre's law" (code-char 232))
  :ExpFormat ("applying Amp~Cre's law to ~A"  
	      (code-char 232) (nlg ?S))
  ;; use implicit format args to insert the plus-minus character code into 
  ;; the EqnFormat string using only standard characters in our source text
  :EqnFormat ("int B = $m0*(~CI1 ~C I2 ~C ...)" 
	      (code-char 177) (code-char 177) (code-char 177)))

(defoperator amperes-law-contains (?sought)
  :preconditions
  (
   (amperes-law ?line-integral :surface ?S :currents ?wires . ?rest)
   (any-member ?sought ((current-thru ?wire)
			?line-intgral))
   (test (member ?wire ?wires))
   )
  :effects
  ( (eqn-contains (amperes-law :surface ?S) ?sought)))

(defoperator write-amperes-law (?S)
  :preconditions
  ( 
   (amperes-law ?line-integral :surface ?S :currents ?wires 
		:directions ?directions)
   (variable  ?lint-var ?line-integral)
   (map ?wire ?wires
	   (variable ?current-var (current-thru ?wire))
	   ?current-var ?current-vars)
   ;; include any signs, if specified
   (bind ?current-terms 
	 (if ?directions 
	     (mapcar #'(lambda (x y) (if (eq '+ x) y (list '- y))) 
		     ?directions ?current-vars)
	   ?current-vars))
   ;; format the sum
   (bind ?current-sum (format-plus ?current-terms))
   )
  :effects
  ( (eqn  (= ?lint-var (* |mu0| ?current-sum))
	  (amperes-law :surface ?S)) )
  :hint
  ;; Allegro lisp character encoding has trouble reading in "`e"
  ( (point (string "You can apply Amp~Cre's law to surface ~A."
		   (#\LATIN_SMALL_LETTER_E_WITH_GRAVE identity) ?s))
    (teach (string "Amp~Cre's law states that the line integral of the magnetic field around the boundary of a surface S is equal to the
total current flowing through S times $m0.  The direction of positive current flow is given by the following right hand rule:  wrap you fingers around the boundary of the surface in the direction of the line integral; your thumb will be pointing in the direction of positive current flow." 
		   (#\LATIN_SMALL_LETTER_E_WITH_GRAVE identity)))
    (bottom-out (string "Write the equation ~A ."
			((= ?lint-var (* |mu0| ?current-sum)) 
			 algebra)))))
