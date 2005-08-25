;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Electric/Magnetic Forces and Fields
;;; 

; We have two main principles for electric fields:
; charge-force-Efield: vector equation E_x = F_x/q   (equiv:  F_x = q*E_x)
; which is definition of E-field.  This is a vector principle written
; in component form. We also have scalar variants for magnitude only.
;
; point-charge-Efield: E-field at a distance r from a single point charge 
; derived from definition + Coulumb's Law:  E = k*q/r^2.  We have a vector
; form for components in terms of thetar, as well as a magnitude only scalar form.
;
; TODO: K&M's original problems all had only a single body. So the situations had to be either 
; of only two kinds: (1) a "region" w/field of unspecified source plus a "test" charge feeling 
; the force; OR, (2) a point-charge source creating a field at an unoccupied point. Some rules
; exploit this dichotomy, testing (at-place ?b ?loc ?t) to distinguish situation in
; a relevant way. But this does not generalize, so has to be changed. 
; The question of what bodies to draw, and which bodies to use in the vector statement
; (as axis-owner) is still a nuisance.

; Drawing E-field and E-force vectors:
;
; Because E-field and E-force directions are related, we have several ways we can draw
; an E-field vector, depending on what is given:
;  - From given E-field direction: draw-Efield-vector
;  - From given E-field components w/grid: draw-vector-given-compos 
;  - From E-force direction:
;     - If given dir E-force on charge and charge sign: draw-Efield-given-force-dir[-pos|-neg]
;     - If given compos E-force on charge, charge sign and grid: 
;                  draw-Efield-from-force-compos[-pos|-neg]  
;  - From given of position wrt point-particle [TODO - no problems like this yet]
;  - From given components of position wrt point-particle & grid [TODO -- no problems yet]
; If all else fails: 
;  Unknown -- if know that field exists but no direction determinable (e.g. it's sought)
;
; Exactly corresponding set applies to drawing E-force vector: dir can be given (directly or via compos)
; or can be derived from given E-field direction (directly or via compos), can be derived
; from configuration wrt point particle, or else unknown.

(defoperator draw-Efield-vector (?b ?loc ?source ?t)
 :specifications " "
  :preconditions ((rdebug "Using draw-Efield-vector  ~%")
                  (time ?t)
                  (test (time-pointp ?t))
		  ; ?b is "test charge" feeling force at loc
		  ; it is only used as axis owner for vector
		  ; !!! what if we're given field at point with no body?
                  (at-place ?b ?loc ?t)
                  (given (at (dir (field ?loc electric ?source)) ?t) ?dir)  
                  (not (vector ?b (at (field ?loc electric ?source) ?t) ?dir))     
                  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) 
					     (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Efield-vector   ~%")
                  )
  :effects (
            (vector ?b (at (field ?loc electric ?source) ?t) ?dir)
            (variable ?mag-var (at (mag (field ?loc electric ?source)) ?t))
            (variable ?dir-var (at (dir (field ?loc electric ?source)) ?t))
	    ;; Because dir is problem given, find-by-psm won't ensure implicit 
	    ;; eqn gets written.  Given value may not be used elsewhere so 
	    ;; ensure it here.
            (implicit-eqn (= ?dir-var ?dir) (at (dir (field ?loc electric ?source)) ?t))
            )  
  :hint (
         (point (string "You were given the direction of the electric field at ~a due to ~a." ?loc (?source agent)))
         (bottom-out (string "Use the electric field drawing tool (labeled E) to draw the electric field at ~a due to ~a in the given direction of ~A." 
			     ?loc (?source agent) ?dir))
         ))

;; pull out the sign of a given charge
(defoperator get-sign-given-charge (?b ?t)
  :preconditions ((in-wm (given (at (charge-on ?b) ?t) (dnum ?val ?units)))
                  (bind ?pos-neg (if (> ?val 0) 'pos 'neg)))
  :effects ((sign-charge ?b ?pos-neg)))

; Can draw field vector if E force dir is given directly
(defoperator draw-Efield-given-force-dir (?b ?t)
   :preconditions ((rdebug "Using draw-Efield-given-force-dir ~%")
		  ; make sure direction of force on ?b is given
                  (given (at (dir (force ?b ?source electric)) ?t) (dnum ?F-dir |deg|))
		  ; make sure field direction at loc of b not given, directly or via components:
                  (at-place ?b ?loc ?t)
                  (not (given (at (dir (field ?loc electric ?source)) ?t) ?dontcare1))
                  (not (given (at (compo x 0 (field ?loc electric ?source)) ?t) ?dontcare2))
		  ; require sign of charge to be given
                  (sign-charge ?b ?pos-neg)
                  (bind ?field-dir (if (eq ?pos-neg 'pos) ?F-dir (mod (+ 180 ?F-dir) 360)))
		  (bind ?same-or-opposite  (if (eq ?pos-neg 'pos) 'same 'opposite))
                  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Efield-given-force-dir  ~%")
                  )
  :effects (
            (vector ?b (at (field ?loc electric ?source) ?t) (dnum ?Field-dir |deg|))
            (variable ?mag-var (at (mag (field ?loc electric ?source)) ?t)) 
            (variable ?dir-var (at (dir (field ?loc electric ?source)) ?t)) 
            (given (at (dir (field ?loc electric ?source)) ?t) (dnum ?Field-dir |deg|))
            )
  :hint (
        (point (string "Think about how the direction of the electric force at ~a due to ~a is related to the direction of the electric field vector at ~a" ?loc (?source agent) ?loc))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is ~a,  use the electric field drawing tool (labeled E) to draw the electric field vector at ~a due to ~a in the ~a direction as the electric force that ~A undergoes, namely ~A degrees" ?b (?pos-neg adj) ?loc (?source agent) (?same-or-opposite adj) ?b ?field-dir))
         ))

; Can draw Efield vector if E force dir is given by components, and grid is used
(defoperator draw-Efield-given-force-compos-pos (?b ?source ?t)
   :preconditions (
		  ; only use for component form problems with drawing grid
		  (component-form)
		  (vector-grid)
		  ; require sign of charge to be positive
                  (sign-charge ?b pos)
		  ; make sure E-force compos given
                  (given (at (compo x 0  (force ?b ?source electric)) ?t) (dnum ?xc ?units))
                  (given (at (compo y 90 (force ?b ?source electric)) ?t) (dnum ?yc ?units))
		  ; make sure field direction not given at loc of ?b, directly or via components:
		  (at-place ?b ?loc ?t)
		  (not (given (at (dir (field ?loc electric ?source)) ?t) ?dontcare1))
		  (not (given (at (compo x 0 (field ?loc electric ?source)) ?t) ?dontcare2))
		  ; similar to draw-vector-given-compos
		  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (bind ?dir (dir-from-compos ?xc ?yc)) 
                  )
  :effects (
            (vector ?b (at (field ?loc electric ?source) ?t) (dnum ?dir |deg|))
            (variable ?mag-var (at (mag (field ?loc electric ?source)) ?t)) 
            (variable ?dir-var (at (dir (field ?loc electric ?source)) ?t)) 
            ;; Don't put out equation for thetaV since value is not exact, could 
            ;; lead to errors if given to algebraic solver with other equations.
            ;(given (at (dir (field ?loc electric ?source)) ?t) (dnum ?Field-dir |deg|))
            )
  :hint (
     (point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is positive, and you were given the horizontal and vertical components of the force vector, draw the electric field at the location of ~a in the same direction as the force by choosing a scale, then counting ~a units horizontally and ~a vertically as you draw." ?b ?xc ?yc))
         ))

(defoperator draw-Efield-given-force-compos-neg (?b ?source ?t)
   :preconditions (
		  ; only use for component form problems with drawing grid
		  (component-form)
		  (vector-grid)
		  ; require sign of charge to be positive
                  (sign-charge ?b neg)
		  ; make sure E-force compos given
                  (given (at (compo x 0  (force ?b ?source electric)) ?t) (dnum ?xc ?units))
                  (given (at (compo y 90 (force ?b ?source electric)) ?t) (dnum ?yc ?units))
		  ; make sure field direction not given at loc of ?b, directly or via components:
		  (at-place ?b ?loc ?t)
		  (not (given (at (dir (field ?loc electric ?source)) ?t) ?dontcare1))
		  (not (given (at (compo x 0 (field ?loc electric ?source)) ?t) ?dontcare2))
		  ; similar to draw-vector-given-compos
		  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (bind ?opp-dir (dir-from-compos ?xc ?yc)) 
                  (bind ?dir (mod (+ 180 ?opp-dir) 360))
                  )
  :effects (
            (vector ?b (at (field ?loc electric ?source) ?t) (dnum ?dir |deg|))
            (variable ?mag-var (at (mag (field ?loc electric ?source)) ?t)) 
            (variable ?dir-var (at (dir (field ?loc electric ?source)) ?t)) 
            ;; Don't put out equation for thetaV since value is not exact, could 
            ;; lead to errors if given to algebraic solver with other equations.
            ;(given (at (dir (field ?loc electric ?source)) ?t) (dnum ?Field-dir |deg|))
            )
  :hint (
(point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is negative, and you were given the horizontal and vertical components of the force vector, draw the electric field at the location of ~a in the opposite direction from the force by choosing a scale, then counting -~a units horizontally and -~a vertically as you draw." ?b ?xc ?yc))
         ))

; Draw field vector if given that unknown E field exists
;      given by (E-field source) in problem. Don't include this field dir determinable by other ways

; NB: ?b is only needed as axis-owner of drawn vector. 
; It is "test charge" experiencing force at ?loc
(defoperator draw-region-Efield-unknown (?b ?loc ?source ?t)
  :specifications " "
  :preconditions ((rdebug "Using draw-region-Efield-unknown ~%")
                  (time ?t)
                  (E-field ?source) ; means that unknown E-field exists
		  ; Must be given there is a body at ?loc. ?? Seems to be used to
		  ; indicate this is region Efield type problem -- should change.
		  ; what if we're asked about field at an unoccupied point?
                  (at-place ?b ?loc ?t)
		  ; make sure source not at loc of test charge
		  ; (not (at-place ?source ?loc ?t))
                  (test (time-pointp ?t))
                  (not (vector ?dontcare (at (field ?loc electric ?source) ?t) ?dir))
		  ; make sure field direction not given, directly 
                  (not (given (at (dir (field ?loc electric ?source)) ?t) ?dontcare3))
		  ; take out following when we change from grid to drawing unknown
		  ; (not (given (at (compo x 0 (field ?loc electric ?source)) ?t) ?dontcare4))
		  ; !!! Should also make sure direction of E-force not given, directly or via components.
		  ; Would be given as electric force on object for an object at-place loc.
                  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-region-Efield-unknown  ~%")
                  )
  :effects (
            (vector ?b (at (field ?loc electric ?source) ?t) unknown)
            (variable ?mag-var (at (mag (field ?loc electric ?source)) ?t))
	    (variable ?dir-var (at (dir (field ?loc electric ?source)) ?t))
            )
  :hint (
         (point (string "You know there is an electric field at ~A." ?loc))
         (teach (string "In this problem the exact direction of the electric field vector requires calculation to determine, so you can draw the vector at an approximately angle and leave the exact angle unspecified."))
         (bottom-out (string "Draw the electric field at ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?loc (?source agent)))
          ))


; draw point charge Efield at loc if dir from source to loc is given
(defoperator draw-point-Efield-given-relpos-dir (?b ?loc ?t)
  :preconditions (
	   ; Make sure source is point-charge
	   (point-charge ?b)
           (test (time-pointp ?t))
           (not (given (at (dir (field ?loc electric ?b)) ?t) ?dontcare3))
	   (in-wm (given (at (dir(relative-position ?loc ?b)) ?t) (dnum ?rdir |deg|)))
	   (sign-charge ?b ?pos-neg)
           (bind ?Field-dir (if (eq ?pos-neg 'pos) ?rdir (mod (+ 180 ?rdir) 360)))
	   (bind ?same-or-opposite (if (eq ?pos-neg 'pos) 'same 'opposite))
           (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) (body-name ?b) ?t))
           (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects (
            (vector ?b (at (field ?loc electric ?b) ?t) (dnum ?Field-dir |deg|))
            (variable ?mag-var (at (mag (field ?loc electric ?b)) ?t)) 
            (variable ?dir-var (at (dir (field ?loc electric ?b)) ?t)) 
            (given (at (dir (field ?loc electric ?b)) ?t) (dnum ?Field-dir |deg|))
  )
  :hint (
        (point (string "Because ~A is charged, it creates an electric field at ~A." ?b ?loc))
        (teach (string "The direction of the electric field due to a point charge is radial away from a positive charge and toward a negative charge."))
        (bottom-out (string "Because the charge of ~a is ~a and the line from ~a to ~a is oriented at ~a, draw the electric field at ~a due to ~a in the ~a direction, namely ~a deg." 
			    ?b (?pos-neg adj) ?b ?loc ?rdir ?loc (?b agent) 
			    (?same-or-opposite adj) ?Field-dir))
  ))


; NB: ?b is only needed as axis-owner of drawn vector. 
; It is normally charged particle that is source of field
(defoperator draw-point-Efield-unknown (?b ?loc ?t)
  :preconditions ((rdebug "Using draw-point-Efield-unknown ~%")
                  (time ?t)
                  (E-field ?b)
		  ; Do we need this anymore?
                  ;(at-place ?b ?loc-source ?t)
                  (test (time-pointp ?t))
		  ; Make sure source is point-charge
		  (point-charge ?b)
		  ; make sure ?loc not equals ?loc-source?
                  (not (vector ?dontcare (at (field ?loc electric ?b) ?t) ?dir))
                  (not (given (at (dir (field ?loc electric ?b)) ?t) ?dontcare3))
	          (not (given (at (dir(relative-position ?loc ?b)) ?t) (dnum ?rdir |deg|)))
                  (bind ?mag-var (format-sym "E_~A_~A$~A" (body-name ?loc) (body-name ?b) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "Fired draw-point-Efield-unknown  ~%")
                  )
  :effects (
            (vector ?b (at (field ?loc electric ?b) ?t) unknown)
            (variable ?mag-var (at (mag (field ?loc electric ?b)) ?t))
            (variable ?dir-var (at (dir (field ?loc electric ?b)) ?t))
            )
  :hint (
        (point (string "Because ~A is charged, it creates an electric field at ~A." ?b ?loc))
        (teach (string "The direction of the electric field due to a point charge is radial away from a positive charge and toward a negative charge.  In this problem the exact direction of the electric field vector requires calculation to determine, so you can draw the vector at an approximately correct angle and leave the exact angle unspecified."))
        (bottom-out (string "Draw the electric field at ~a due to ~a, then erase the number in the direction slot to indicate that the exact direction is not being specified." ?loc (?b agent)))
          ))

;
; Drawing E force vector -- parallels drawing E field vector
;
; !!! TODO -- should factor out proposition that electric force exists that fires without drawing vector,
; as we do for other forces in Newton's Law problems.  (See force-finding rules in Newtons2.cl for examples). 
; This won't matter until we have mechanics problems with electric forces and have to reason about 
; all forces in the problem (e.g. whether they are all conservative) without drawing vectors.

; - if given E force vector dir -- 
(defoperator draw-Eforce-given-dir (?b ?source ?t)
  :preconditions ((rdebug "Using draw-Eforce-given-dir ~%")
                  (given (at (dir (force ?b ?source electric)) ?t) ?dir)
                  (bind ?mag-var (format-sym "F_~A_~A$~A" (body-name ?b) (body-name ?source)?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Eforce-given-dir  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source electric) ?t) ?dir)
            (variable ?mag-var (at (mag (force ?b ?source electric)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source electric)) ?t))
            ;Because dir is problem given, find-by-psm won't ensure implicit eqn
            ;gets written.  Given value may not be used elsewhere so ensure it here.
            (implicit-eqn (= ?dir-var ?dir) (at (dir (force ?b ?source electric)) ?t))
            )
  :hint (
    (point (string "You were given that there is an electric force on ~a." ?b))
    (bottom-out (string "Use the force drawing tool to draw the electric force on ~a due to ~a ~a at ~a." ?b (?source agent) (?t pp) ?dir))
))

; if given E field vector dir
;    - directly
(defoperator draw-Eforce-given-field-dir (?b ?source ?t)
   :preconditions ((rdebug "Using draw-Eforce-given-field-dir ~%")
		  ; make sure E-field direction given at loc of ?b
                  (in-wm (given (at (dir (field ?loc electric ?source)) ?t) (dnum ?field-dir |deg|)))
		  ; make sure force direction not given, directly or via components:
                  (not (given (at (dir (force ?b ?source electric)) ?t) ?dontcare1))
                  (not (given (at (compo x 0 (force ?b ?source electric)) ?t) ?dontcare2))
		  ; require sign of charge to be given
                  (sign-charge ?b ?pos-neg)
                  (bind ?F-dir (if (eq ?pos-neg 'pos) ?field-dir (mod (+ 180 ?field-dir) 360)))
		  (bind ?same-or-opposite (if (eq ?pos-neg 'pos) 'same 'opposite))
                  (bind ?mag-var (format-sym "F_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Eforce-given-field-dir  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source electric) ?t) (dnum ?F-dir |deg|))
            (variable ?mag-var (at (mag (force ?b ?source electric)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source electric)) ?t))
            (given (at (dir (force ?b ?source electric)) ?t) (dnum ?F-dir |deg|))
            )
  :hint (
        (point (string "Think about how the direction of the electric force at ~a due to ~a is related to the direction of the electric field vector at ~a" ?loc (?source agent) ?loc))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is ~a, use the force drawing tool (labeled F) to draw the electric force on ~a due to ~a in the ~a direction as the electric field at that location, namely ~A degrees" 
	 ?b (?pos-neg adj) ?b (?source agent) (?same-or-opposite adj) ?F-dir))
         ))

; if given E field vector dir
;    - by components, with grid
; Similar to generic draw-vector-given-compos, but requires sign to be taken into account.

(defoperator draw-Eforce-given-field-compos-pos (?b ?source ?t)
   :preconditions ((rdebug "Using draw-Eforce-given-field-compos-pos ~%")
		  ; only use for component form problems with drawing grid
		  (component-form)
		  (vector-grid)
		  ; require sign of charge to be positive
                  (sign-charge ?b pos)
		  ; make sure E-field compos given at loc of ?b
                  (given (at (compo x 0 (field ?loc electric ?source)) ?t) (dnum ?xc ?units))
                  (given (at (compo y 90 (field ?loc electric ?source)) ?t) (dnum ?yc ?units))
		  ; make sure force direction not given, directly or via components:
                  (not (given (at (dir (force ?b ?source electric)) ?t) ?dontcare1))
                  (not (given (at (compo x 0 (force ?b ?source electric)) ?t) ?dontcare2))
		  ; similar to draw-vector-given-compos
                  (bind ?mag-var (format-sym "F_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (bind ?dir `(dnum ,(dir-from-compos ?xc ?yc) |deg|))
                  (rdebug "fired draw-Eforce-given-field-compos-pos  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source electric) ?t) ?dir)
            (variable ?mag-var (at (mag (force ?b ?source electric)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source electric)) ?t))
            ;; Don't put out equation for thetaV since value is not exact, could 
            ;; lead to errors if given to algebraic solver with other equations.
            ; (given (at (dir (force ?b ?source electric)) ?t) ?dir)
            )
  :hint (
          (point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is positive, and you were given the horizontal and vertical components of the electric field due to ~a, draw the electric force on ~a in the same direction as the field at its location by choosing a scale, then counting ~a units horizontally and ~a vertically as you draw." ?b (?source agent) ?b ?xc ?yc))
         ))

(defoperator draw-Eforce-given-field-compos-neg (?b ?source ?t)
   :preconditions ((rdebug "Using draw-Eforce-given-field-compos-pos ~%")
		  ; only use for component form problems with drawing grid
		  (component-form)
		  (vector-grid)
		  ; require sign of charge to be negative
                  (sign-charge ?b neg)
		  ; make sure E-field compos given at loc of ?b
                  (given (at (compo x 0 (field ?loc electric ?source)) ?t) (dnum ?xc ?units))
                  (given (at (compo y 90 (field ?loc electric ?source)) ?t) (dnum ?yc ?units))
		  ; make sure force direction not given, directly or via components:
                  (not (given (at (dir (force ?b ?source electric)) ?t) ?dontcare1))
                  (not (given (at (compo x 0 (force ?b ?source electric)) ?t) ?dontcare2))
		  ; similar to draw-vector-given-compos
                  (bind ?mag-var (format-sym "F_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
		  (bind ?opp-dir (dir-from-compos ?xc ?yc))
		  (bind ?dir (mod (+ 180 ?opp-dir) 360))
                  (rdebug "fired draw-Eforce-given-field-compos-pos  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source electric) ?t) (dnum ?dir |deg|))
            (variable ?mag-var (at (mag (force ?b ?source electric)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source electric)) ?t))
            ;; Don't put out equation for thetaV since value is not exact, could 
            ;; lead to errors if given to algebraic solver with other equations.
            ; (given (at (dir (force ?b ?source electric)) ?t) (dnum ?dir |deg|)))
            )
  :hint (
(point (string "Think about how the direction of the electric force on ~a due to ~a is related to the direction of the electric field vector at its location." ?b (?source agent)))
	(teach (string "The electric field vector points in the same direction as the electric force experienced by a positive charge, or in the opposite direction for a negative charge."))
         (bottom-out (string "Because the charge of ~a is negative, and you were given the horizontal and vertical components of the electric field due to ~a, draw the electric force on ~a in the opposite direction from the field at its location by choosing a scale, then counting -~a units horizontally and -~a vertically as you draw." ?b (?source agent) ?b ?xc ?yc))
         ))

;  -if given that unknown field exists 
;      given by (E-field source) or (B-field source) in problem. 
;      Don't use these if field direction given in other ways
(defoperator draw-Eforce-unknown (?b ?source ?t)
  :specifications " "
  :preconditions ((rdebug "Using draw-Eforce-unknown ~%")
                  (time ?t)
                  (E-field ?source) 
                  (test (time-pointp ?t))
                  (not (vector ?b (at (force ?b ?source electric) ?t) ?dir))
		  ; make sure force direction not given, directly or via components:
                  (not (given (at (dir (force ?b ?source electric)) ?t) ?dontcare1))
		  ; take out when we change to unknown:
                  ; (not (given (at (compo x 0 (force ?b ?source electric)) ?t) ?dontcare2))
		  ; make sure E-field direction not given, directly or via components
		  (at-place ?b ?loc ?t)
		  (not (given (at (dir (field ?loc electric ?source)) ?t) ?dontcare3))
		  ;(not (given (at (compo x 0 (field ?loc electric ?source)) ?t) ?dontcare4))
                  (bind ?mag-var (format-sym "F_~A_~A$~A" (body-name ?b) (body-name ?source) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Eforce-unknown  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source electric) ?t) unknown)
            (variable ?mag-var (at (mag (force ?b ?source electric)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source electric)) ?t))
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
  :preconditions ((rdebug "Using charge-force-Efield-contains  ~%")
                  (any-member ?sought((at (mag (force ?b ?source electric)) ?t)
                                      (at (dir (force ?b ?source electric)) ?t)
                                      (at (mag (field ?loc electric ?source)) ?t)
                                      (at (dir (field ?loc electric ?source)) ?t)
                                      ))
		  ; make sure ?b (test-charge) is bound in case sought is field at loc
                  (at-place ?b ?loc ?t)
                  (rdebug "Firing charge-force-Efield-contains  ~%")
                  )
  :effects (
            (eqn-family-contains (charge-force-Efield ?b ?source ?t) ?sought)
            ; since only one compo-eqn under this vector psm, we can just
            ; select it now, rather than requiring further operators to do so
            (compo-eqn-contains (charge-force-Efield ?b ?source ?t) qfe ?sought)))

; special case when sought is charge: need to choose a field source to bind
(defoperator charge-force-Efield-contains-charge (?sought)
  :preconditions ((rdebug "Using charge-force-Efield-contains-charge  ~%")
                  (any-member ?sought ( (at (charge-on ?b) ?t) ))
                  (at-place ?b ?loc ?t)
		  ; following will fetch the source of an E-field at loc if we are given
		  ; its direction or component value
		  (source-of-Efield ?loc ?t ?source)
                  (rdebug "Firing charge-force-Efield-contains-charge  ~%")
                  )
  :effects (
            (eqn-family-contains (charge-force-Efield ?b ?source ?t) ?sought)
            ; since only one compo-eqn under this vector psm, we can just
            ; select it now, rather than requiring further operators to do so
            (compo-eqn-contains (charge-force-Efield ?b ?source ?t) qfe ?sought)))

(defoperator get-source-from-given-field-dir (?loc ?t ?source)
  :preconditions ((in-wm (given (at (dir (field ?loc electric ?source)) ?t) ?value)))
  :effects ((source-of-Efield ?loc ?t ?source)))

(defoperator get-source-from-given-field-compo (?loc ?t ?source)
  :preconditions ((in-wm (given (at (compo x 0 (field ?loc electric ?source)) ?t) ?value)))
  :effects ((source-of-Efield ?loc ?t ?source)))

(defoperator draw-charge-force-Efield-diagram (?b ?source ?t)
  :preconditions (
                  (debug "Using draw-charge-force-Efield-diagram ~%")
                  (not (vector-diagram (charge-force-Efield ?b ?source ?t)))
                  ;; ?b is "test charge" feeling force at ?loc 
                  (body ?b)
                  (at-place ?b ?loc ?t)
		  ; need source of field
                  (vector ?dontcare (at (field ?loc electric ?source) ?t) ?dir1) 
                  (vector ?b (at (force ?b ?source electric) ?t) ?dir2)
                  (axis-for ?b x ?rot)
                  (rdebug "Fired draw-charge-force-Efield-diagram ~%")
                  )
  :effects (
            (vector-diagram (charge-force-Efield ?b ?source ?t))
            ))

(defoperator write-charge-force-Efield-compo (?b ?t ?xy ?rot)
  :preconditions ((debug "Using write-charge-force-Efield-compo ~%")
                  (at-place ?b ?loc ?t)
                  (variable ?E_x  (at (compo ?xy ?rot (field ?loc electric ?source)) ?t))
                  (variable ?F_x  (at (compo ?xy ?rot (force ?b ?source electric)) ?t))
                  (variable ?q (at (charge-on ?b) ?t))
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

; Vector relation F = q*E also licences magnitude and direction scalar equations.
; Simpler to find these quantities than using component equations plus projections
; (though should be able to use either method)>
; We have to write these out as separate scalar principles.
; Similarly for B-field equations.

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
                  (any-member ?sought ((at (mag (force ?b ?source electric)) ?t)
                                       (at (mag (field ?loc electric ?source)) ?t)
				       ; need to choose a source if sought is charge:
				       ; ignore for now
                                       ;(at (charge-on ?b) ?t)
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
                  (variable ?magE (at (mag (field ?loc electric ?source)) ?t))
                  (variable ?magF (at (mag (force ?b ?source electric)) ?t))
                  (variable ?q (at (charge-on ?b) ?t))
                  (rdebug "fired write-charge-force-Efield-mag  ~%")
                  )
  :effects (
	    ; NB: need abs charge since it is now signed
            (eqn (= ?magF (* (abs ?q) ?magE))(charge-force-Efield-mag ?b ?source ?t))
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
                  (any-member ?sought ((at (dir (force ?b ?source electric)) ?t)
                                       (at (dir (field ?loc electric ?source)) ?t)
                                       (at (charge-on ?b) ?t)))
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
                  (variable ?dirE (at (dir (field ?loc electric ?source)) ?t))
                  (variable ?dirF (at (dir (force ?b ?source electric)) ?t))
                  (variable ?q (at (charge-on ?b) ?t))
                  (rdebug "fired write-charge-force-Efield-dir  ~%")
                  )
  :effects (
            (eqn (= ?dirF ?dirE)(charge-force-Efield-dir ?b ?source ?t))
            )
  :hint (
         (point (string "What is the relationship between the direction of the force vector and the direction of the electric field vector?"))
         (teach (kcd "write-charge-force-Efield-dir-pos")
                 (string "For a positive charge, the direction of the force vector is the same as the direction of the electric field vector."))
         (bottom-out (string "Write the equation ~a=~a."
			(?dirF algebra) (?dirE algebra)))
          ))
#| ; this was broken, so unused, in Andes 8.0.5
(defoperator write-charge-force-Efield-dir-neg (?b ?t)
  :preconditions ((debug "Using write-charge-force-Efield-dir ~%")
                  (sign-charge ?b neg)
                  (at-place ?b ?loc ?t)
                  (variable ?dirE (at (dir (field ?loc electric ?source)) ?t))
                  (variable ?dirF (at (dir (force ?b ?source electric)) ?t))
                  (variable ?q (at (charge-on ?b) ?t))
                  (rdebug "fired write-charge-force-Efield-dir  ~%")
                  )
  :effects (
	    ; using degrees here caused solver to report numerical discrepancies, perhaps because 
	    ; of round-off error introduced in converting to radians.
	    ; So change to use pi radians instead -- though students may find this odd
            ;(eqn (= ?dirF (+ ?dirE (dnum 180 |deg|)))(charge-force-Efield-dir ?b ?source ?t))
            (eqn (= ?dirF (+ ?dirE $P)) (charge-force-Efield-dir ?b ?source ?t))
            )
  :hint (
         (point (string "What is the relationship between the direction of the force vector and the direction of the electric field vector?"))
         (teach (kcd "write-charge-force-Efield-dir-neg")
                 (string "For a negative charge, the direction of the force vector is opposite the direction of the electric field vector."))
         (bottom-out (string "Write the equation ~a = ~a + 180 degrees."
			(?dirF algebra) (?dirE algebra)))
          ))
|#

;;--------------------------------------------
;; point-charge-Efield Vector PSM 
;;--------------------------------------------
(def-psmclass point-charge-Efield
             (?eq-type qpe ?axis ?rot (point-charge-Efield ?body ?loc ?time)) 
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
                  (any-member ?sought((at (mag (field ?loc electric ?b)) ?t)
                                      (at (dir (field ?loc electric ?b)) ?t)
                                      (at (charge-on ?b) ?t) 
				      (at (mag (relative-position ?loc ?b)) ?t)
				      (at (dir (relative-position ?loc ?b)) ?t)
                                      ))
		  (point-charge ?b)
                  ;(at-place ?b ?loc-source ?t)
                  (rdebug "Firing point-charge-Efield-compo-contains  ~%")
                  )
  :effects (
            (eqn-family-contains (point-charge-Efield ?b ?loc ?t) ?sought)
             ; since only one compo-eqn under this vector psm, we can just
             ; select it now, her than requiring further operators to do so
            (compo-eqn-contains (point-charge-Efield ?b ?loc ?t) qpe ?sought)))

(defoperator draw-point-charge-Efield-diagram (?b ?loc ?t)
  :preconditions (
                  (rdebug "Using draw-point-charge-Efield-diagram ~%")
                  (not (vector-diagram (point-charge-Efield ?b ?loc ?t)))
                  ;; ?b is point charge source of field at ?loc
                  (body ?b)
		  ; do we need this?
                  ;(at-place ?b ?loc2 ?t)
                  (vector ?dontcare (at (field ?loc electric ?b) ?t) ?dir1) 
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
 :preconditions ( (given (at (pos ?loc) ?t) (?value1 ?value2)) )
 :effects ( (given (at (compo x 0 (relative-position ?loc origin)) ?t) ?value1)
            (given (at (compo y 90 (relative-position ?loc origin)) ?t) ?value2) ))


; This is equation for the component of field, so includes a sort of projection.
(defoperator write-point-charge-Efield-compo (?b ?loc ?t ?xy ?rot)
   :preconditions (
       (rdebug "Using write-point-charge-Efield-compo ~%")
       ; b is point-charge source of field
       ;(at-place ?b ?loc-source ?t)
       (variable ?E_x  (at (compo ?xy ?rot (field ?loc electric ?b)) ?t))
       (variable ?q    (at (charge-on ?b) ?t))
       (variable ?r    (at (mag (relative-position ?loc ?b)) ?t))
       (variable ?theta_r (at (dir (relative-position ?loc ?b)) ?t))
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
                  (any-member ?sought ((at (mag (field ?loc electric ?b)) ?t)
				       (at (mag (relative-position ?loc ?loc-source)) ?t)
                                       (at (charge-on ?b) ?t)))
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
                  (variable ?magE (at (mag (field ?loc electric ?b)) ?t))
                  (variable ?q (at (charge-on ?b) ?t))
		  (variable ?r (at (mag (relative-position ?loc ?b)) ?t))
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
                  (any-member ?sought ((at (dir (field ?loc electric ?b)) ?t)
                                       (at (charge-on ?b) ?t)))
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
                  (variable ?dirE (at (dir (field ?loc electric ?b)) ?t))
		  (variable ?xpos (at (compo x 0 (relative-position ?loc ?b)) ?t))
		  (variable ?ypos (at (compo y 90 (relative-position ?loc ?b)) ?t))
                  (rdebug "fired write-point-charge-Efield-dir-pos  ~%")
                  )
  :effects (
	    ; !! this equation doesn't determine quadrant. Would need atan2
            (eqn (= (tan ?dirE) (/ ?ypos ?xpos))(point-charge-Efield-dir ?b ?loc ?t))
            )
  :hint (
         (point (string "What is the direction of the electric field vector due to a point charge?"))
         (teach (kcd "write-point-charge-Efield-dir-pos")
                 (string "For a positive charge, the electric field is directed radially outward from the point charge."))
         (bottom-out (string "Write the equation ~a= arctan (~a / ~a)."
			(?dirE algebra) (?ypos algebra)(?xpos algebra)))
          ))

(defoperator write-point-charge-Efield-dir-neg (?b ?loc ?t)
  :preconditions ((debug "Using write-point-charge-Efield-dir-pos ~%")
		  ; following was used to distinguish point-charge problem:
		  ; location of source body mentioned in givens
                  ;(at-place ?b ?loc2 ?t)
		  (point-charge ?b)
                  (sign-charge ?b neg)
                  (variable ?dirE (at (dir (field ?loc electric ?b)) ?t))
		  (variable ?xpos (at (compo x 0 (relative-position ?loc ?b)) ?t))
		  (variable ?ypos (at (compo y 90 (relative-position ?loc ?b)) ?t))
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
			(?dirE algebra) (?ypos algebra)(?xpos algebra)))
          ))


; This derives sign on charge of ?b when Fe_x and E_x are given at location of ?b
; x-component can't be zero
(defoperator get-sign-charge-from-FE-compos (?b)
  :specifications " "
  :preconditions ((rdebug "sign-on-charge ~%")
                  (time ?t)
                  (test (time-pointp ?t))
                  ;(at-place ?b ?loc ?t)                    
                  (given (at (compo x 0 (force ?b ?source electric)) 1) (dnum ?val1 |N|))
                  (given (at (compo x 0 (field ?loc electric ?source)) 1) (dnum ?val2 |N/C|))
                  (bind ?sign (if (> (/ ?val1 ?val2) 0) 'pos 'neg))
                  (rdebug "sign-on-charge~%")
                  )
  :effects (
            (sign-charge ?b ?sign)
            ))


;;; Pythagorean theorem for vector magnitudes: 
;;;        r^2 = r_x^2 + r_y^2
; This applies to any vector, but for now we define it for relative position vectors only, so that
; we can limit its effects on other problems until we decide to add it for all vectors
; For now we also restrict to standard axes since that's where we need it, although any 
; orthogonal pair could do.
;
; !!! This seems only to work on component-form, due to problems getting the axes drawn when
; fetching component variables. This is the detail: define-compo, intended for standard vector psms, 
; fails because it uses "in-wm", but no axis from vector psm diagram is already registered in wm for 
; vectors on body ?b (which may just be a point in space in this case).
; The more liberal define-compo2 draws vectors and standard axes as needed. That will work, but is defined 
; only to apply when component-form is set.
; In general the association of all vectors with bodies for purposes of finding axes to use on them,
; which works well for things like the inclined atwoods problem, is not so appropriate 
; for field problems with position vectors and an origin. The whole treatment of axes needs to be 
;;; cleaned up and simplified to avoid this hairiness.
(def-psmclass rmag-pyth (rmag-pyth ?body ?origin ?time)
  :complexity minor 
  :english ("the pythagorean theorem for position magnitudes")
  :ExpFormat ("calculating the magnitude of the position of ~a from its components" (nlg ?body))
  :EqnFormat ("r = sqrt(r_x^2 + r_y^2)"))

(defoperator rmag-pyth-contains (?sought)
   :preconditions (
     (any-member ?sought (
       (at (mag (relative-position ?b ?o)) ?t)
       ;; only standard axes (doesn't work for tilted)
       (at (compo x 0 (relative-position ?b ?o)) ?t) 
       (at (compo y 90 (relative-position ?b ?o)) ?t)
                         ))
   )
   :effects ( 
     (eqn-contains (rmag-pyth ?b ?o ?t) ?sought) 
   ))

(defoperator write-rmag-pyth (?b ?o ?t)
  :preconditions (
    (variable ?r (at (mag(relative-position ?b ?o)) ?t))
    ; ensure axis drawn, to emulate a vector method diagram drawing step. Reason is hairy: this enables
    ; the define-compo to apply as it does for normal vector methods, since define-compo requires vectors 
    ; and axes to have been drawn in wm. Since change to alternative define-compo2, the latter no longer 
    ; works if EITHER vector or axis is drawn. 
    (axis-for ?b x 0)
    ; don't apply this rule if vector lies along an axis: it won't be needed
    ; to calculate magnitude from compos and just multiplies solutions.
    ; Note: this doesn't test for vector along z-axis (unlikely to occur).
    (in-wm (vector ?dontcare (at (relative-position ?b ?o) ?t) ?dir-r))
    (test (not (eq ?dir-r 'zero))) ;don't apply to zero length
    (test (not (horizontal-or-vertical ?dir-r)))
    (variable ?r_x (at (compo x 0  (relative-position ?b ?o)) ?t)) 
    (variable ?r_y (at (compo y 90 (relative-position ?b ?o)) ?t)) 
  )
  :effects (
    (eqn (= ?r (sqrt (+ (^ ?r_x 2) (^ ?r_y 2)))) (rmag-pyth ?b ?o ?t))
  )
  :hint (
   (point (string "The pythagorean theorem can be used to relate the magnitude of a relative position vector to its components."))
   (bottom-out (string "Write the equation ~A." 
		       ((= ?r (sqrt (+ (^ ?r_x 2) (^ ?r_y 2)))) algebra)))
  ))

;
; "rdiff": vector psm for calculating components of r21 from given
; coordinates of points 1 and 2. Coordinates are given values of 
; relative-positions wrt the specially named point 'origin.
; The equation is:
;   r21_x = r2o_x - r1o_x 
; However, to keep the solutions simple, we plug the numerical
; values of the given coordinates directly into the equation, rather 
; than using variables for the given positions, which would then have
; to be drawn.  Might have to change this eventually, or add variant
; that allows them to be drawn.

(def-psmclass rdiff
             (?eq-type rdiff ?axis ?rot (rdiff ?p1 ?p2 ?time)) 
  :complexity minor
  :english ("the relative position definition")
  :ExpFormat ("computing the ~a component of the relative position of ~a with respect to ~a"
		 (nlg ?axis 'adj) (nlg ?p2) (nlg ?p1) )
  :EqnFormat ("r21_~a = ~a2 - ~a1" (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))

(defoperator rdiff-contains (?sought)
  :preconditions (
    ; only applies in component-form
    (component-form)
    (any-member ?sought (
		 (at (mag (relative-position ?p2 ?p1)) ?t)
		 (at (dir (relative-position ?p2 ?p1)) ?t)
		 ; no other variables in this equation
		 ))
    (in-wm (given (at (compo x 0  (relative-position ?p1 origin)) ?t) ?p1_x))
    (in-wm (given (at (compo y 90 (relative-position ?p1 origin)) ?t) ?p1_y))
    (in-wm (given (at (compo x 0  (relative-position ?p2 origin)) ?t) ?p2_x))
    (test (not (eq ?p2 ?p1))) ; make sure two different points
    (in-wm (given (at (compo y 90 (relative-position ?p2 origin)) ?t) ?p2_y))
    ; should still work if p1 or p2 = origin, but would need to be 
    ; told that coords of origin are (0,0) in givens
    )
  :effects 
  ((eqn-family-contains (rdiff ?p1 ?p2 ?t) ?sought)
  ; since only one compo-eqn under this vector psm, we can just
  ; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (rdiff ?p1 ?p2 ?t) rdiff ?sought)))

(defoperator draw-rdiff-diagram (?p1 ?p2 ?t)
  :preconditions 
  ((not (vector-diagram (rdiff ?p1 ?p2 ?t)))
   ;; do we draw a body for this? What body do we call this
   (vector ?p2 (at (relative-position ?p2 ?p1) ?t) ?dir1)
   ;; have to make sure we have an axis for this vector
   (axis-for ?p2 x 0))
  :effects 
  ((vector-diagram (rdiff ?p1 ?p2 ?t))))

(defoperator write-rdiff-compo (?p1 ?p2 ?t ?xy ?rot)
  :preconditions (
    (variable ?r21_xy (at (compo ?xy ?rot (relative-position ?p2 ?p1)) ?t))
    ; just fetch the coordinate values to plug in
    (given (at (compo ?xy ?rot  (relative-position ?p1 origin)) ?t) ?r1o_xy_val)
    (given (at (compo ?xy ?rot  (relative-position ?p2 origin)) ?t) ?r2o_xy_val)
   )
  :effects (
   (eqn (= ?r21_xy (- ?r2o_xy_val ?r1o_xy_val))
         (compo-eqn rdiff ?xy ?rot (rdiff ?p1 ?p2 ?t)) )
   (eqn-compos 
         (compo-eqn rdiff ?xy ?rot (rdiff ?p1 ?p2 ?t))
         (?r21_xy))
  ) 
  :hint (
    (point (string "The components of the relative position of one point wrt another can be computed from the coordinates of the two points"))
    (teach (string "The relative position vector r21 of a point p2 wrt p1 is the vector difference r2o - r1o of the positions of p2 and p1 with respect to the origin. You can apply this relation component-wise to compute the components of the needed relative position vector from the given coordinates of the two points."))
    (bottom-out (string "Write the equation ~A"
                ((= ?r21_xy (- ?r2o_xy_val ?r1o_xy_val)) algebra)))
  ))


; Following would enable the equality rba = rpa to be exploited when body b is at p
; However, it doesn't really help if rpa is sought and rpa compos given, since
; then nothing allows rba to be calculated, so gets purged as dead-path quant.
; Might be useable in some problems.

(defoperator same-relpos(?body ?loc ?origin ?time)
  :preconditions (
      (in-wm (at-place ?body ?loc ?t-at-loc))
      (test (tinsidep ?time ?t-at-loc))
      ; ?origin should be bound from sought coming in
  )
  :effects (
     ; Assert equality. Equation will be written by generic write-equality 
     ; operator without much of a hint, as if equality is given or obvious.
     (equals (at (mag(relative-position ?body ?origin)) ?time)
             (at (mag(relative-position ?loc ?origin)) ?time))
  ))

;; Scalar variable definitions:


; Charge has a time argument, mainly to handle varying charge on capacitors.
; Charge on a particle will not change over time in Andes electrostatic problems.
; Most of our simple problems involve only an instant, so this is not an issue,
; but it does come up when conservation of energy is applied over an interval.
; If problem involves more than one time point, we want to use charge defined
; over the longest possible interval. We want this in all equations mentioning charge.
; Therefore, in all equation-writing operators with a precondition of a charge variable, 
; the precond should be 
;  (charge-var ?var (at (charge-on ?body) ?time-needed) 
; This may be realized by actually definining a charge-variable for a different time.
;
; (This method might be generalized for other variables that can be constant over time)
(defoperator define-charge-on-obj-var (?p ?t)
  :preconditions (
                  (rdebug "Using define-charge-on-obj-var ~%")
		  ; use this form if haven't declared constant charge over problem interval
		  (not (constant (charge-on ?b) ?t-constant inclusive))
                  (bind ?q-var (format-sym "Q_~A$~A" (body-name ?p) (time-abbrev ?t)))
                  (not (circuit-component ?p capacitor))
                  (rdebug "fired define-charge-on-obj-var ~%")
                  )
  :effects (
            (variable ?q-var (at (charge-on ?p) ?t))
	    (define-var (at (charge-on ?p) ?t))
	    (charge-var ?q-var (at (charge-on ?p) ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for the charge on ~A by using the Add Variable command on the Variable menu and selecting Charge."  ?p ))
       ))

(defoperator define-charge-on-obj-var2 (?p ?t)
  :preconditions (
		  ; If there's more than one time, charge should be declared constant
		  (constant (charge-on ?b) ?t-constant inclusive)
                  (bind ?q-var (format-sym "Q_~A$~A" (body-name ?p) (time-abbrev ?t-constant)))
                  (not (circuit-component ?p capacitor))
		  ; hairy: must ensure ?t is bound, since we may be called by generic variable 
		  ; declaring precond to enter given by write-known-value-eqn 
		  (bind ?t (if (null ?t) ?t-constant ?t))
                  )
  :effects (
            (variable ?q-var (at (charge-on ?p) ?t-constant))
	    (define-var (at (charge-on ?p) ?t-constant))
	    (charge-var ?q-var (at (charge-on ?p) ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for the charge on ~A by using the Add Variable command on the Variable menu and selecting Charge. Because the charge is constant throughout the problem, define the charge ~A"  ?p ?t-constant))
       ))

(defoperator define-potential-var (?loc ?source ?t)
  :preconditions (
                  (bind ?V-var (format-sym "V_~A_~A_~A" ?loc ?source ?t))
                  )
  :effects (
            (variable ?V-var (at (potential ?loc ?source) ?t))
            (define-var (at (potential ?loc ?source) ?t))
            )
   :hint (
       (bottom-out (string "Define a variable for the potential at ~A due to ~a by using the Add Variable command on the Variable menu and selecting Potential."  ?loc (?source agent)))
       ))

(defoperator define-net-potential-var (?loc ?t)
  :preconditions (
                  (bind ?V-var (format-sym "Vnet_~A_~A" ?loc ?t))
                  )
  :effects (
            (variable ?V-var (at (net-potential ?loc) ?t))
            (define-var (at (net-potential ?loc) ?t))
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
		 (at (mag (net-field ?loc electric)) ?t)
		 (at (dir (net-field ?loc electric)) ?t)
                 ; need to choose ?loc to apply at when sought is field due 
		 ; to some source.  Ignore this case for now.
		 ;(at (mag (field ?loc electric ?source)) ?t)
		 ;(at (dir (field ?loc electric ?source)) ?t)
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
       (vector ?b (at (field ?loc electric ?source) ?t) ?dir)) 
    ; which body should own the axis to use for these vectors
    (axis-for ?loc ?xy ?rot)
 )
 :effects (
    (vector-diagram (net-Efield ?loc ?t))
 ))
(defoperator write-net-Efield-compo (?loc ?t ?xy ?rot)
 :preconditions (
		 ;(test (setf *actions* T))
   (variable ?Enet_x (at (compo ?xy ?rot (net-field ?loc electric)) ?t))
   (in-wm (Efield-sources ?loc ?t ?sources))
   (map ?source ?sources 
   	(variable ?compo-var (at (compo ?xy ?rot (field ?loc electric ?source)) ?t))
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
  (not (given (at (dir (net-field ?loc electric)) ?t) ?val))
  ; could make sure there is more than one source of an Efield.
   (in-wm (Efield-sources ?loc ?t ?sources))
   (test (cdr ?sources)) ; more than one in list
   (bind ?mag-var (format-sym "Enet_~A$~A" (body-name ?loc) ?t))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects (
            (vector ?loc (at (net-field ?loc electric) ?t) unknown)
            (variable ?mag-var (at (mag (net-field ?loc electric)) ?t))
	    (variable ?dir-var (at (dir (net-field ?loc electric)) ?t))
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
  (any-member ?sought ( (at (potential ?loc ?body) ?t)
                        (at (mag (relative-position ?loc ?body)) ?t)
			; if sought is charge, have to choose a location
			; for now, just don't apply for charge
                        ;(at (charge-on ?body) ?t)
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
     (variable ?V (at (potential ?loc ?body) ?t))
     (charge-var ?q (at (charge-on ?body) ?t))
     (variable ?r (at (mag (relative-position ?loc ?body)) ?t))
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
	(variable ?r (at (mag (relative-position ?body2 ?body)) ?t))
   )
   :effects  ( (r-var ?r (at (mag (relative-position ?loc ?body)) ?t)) ))

(defoperator use-r-to-point (?body ?loc ?t)
   :preconditions (
        (not (at-place ?body2 ?loc ?t))
	(variable ?r (at (mag (relative-position ?loc ?body)) ?t))
   )
   :effects  ( (r-var ?r (at (mag (relative-position ?loc ?body)) ?t)) ))
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
    (any-member ?sought ((at (net-potential ?loc) ?t)
                         (at (potential ?loc ?source) ?t) ))
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
     (variable ?Vnet (at (net-potential ?loc) ?t))
     (in-wm (Efield-sources ?loc ?t ?sources))
     (map ?source ?sources 
   	(variable ?V-var (at (potential ?loc ?source) ?t))
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
    (any-member ?sought ((at (electric-energy ?body ?source) ?t) 
                         (at (net-potential ?loc) ?t)
			 (at (charge-on ?body) ?t)
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
     (variable ?Ue (at (electric-energy ?body ?source) ?t))
     (charge-var ?q (at (charge-on ?body) ?t))
     (variable ?Vnet (at (net-potential ?loc) ?t))
     ; this psm may be the only one to draw body 
     ; NB: this goal fails if part of cons-energy psm, since
     ; body already drawn and body-drawing op tests for that.
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
 :preconditions (
 (object ?b)
 (bind ?ge-var (format-sym "Ue_~A_~A" ?b (time-abbrev ?t)))
 ) 
 :effects ( 
 (define-var (at (electric-energy ?b ?source) ?t)) 
 (variable ?ge-var (at (electric-energy ?b ?source) ?t)) 
  )
 :hint (
	 (bottom-out (string "Define a variable for electrical potential energy by selecting Energy from the Variables menu on the top menu bar."))
       ))

; To interact with cons-energy psm: op to tell it that electric pe 
; exists in this problem by defining a variable when needed
(defoperator define-electric-pe-var (?b ?t)
    :preconditions (
          ; need to know electric field exists in problem
	  ; for now look for this: 
	  (at-place ?b ?loc ?t)
          (Efield-sources ?loc ?t ?sources)
	  ; need to bind source. See electric-energy-contains above
          (bind ?source (cond ((and (null (cdr ?sources))
			        (not (eq (car ?sources) 'unspecified))) (car ?sources))
			       (T 'electric_field)))
	  (variable ?var (at (electric-energy ?b ?source) ?t))
    )
    :effects ( (pe-var ?b ?t ?var) ))

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
                  (given (at (dir (field ?loc magnetic ?source)) ?t) ?dir-B)  
                  (not (vector ?b (at (field ?loc magnetic ?source) ?t) ?dir1))     
                  (bind ?mag-var (format-sym "B_~A$~A" (body-name ?loc) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
		  ; if dir is z-axis, implicit eqn should give phi angle value
		  (bind ?angle-value (if (z-dir-spec ?dir-B) (zdir-phi ?dir-B) 
		                      ?dir-B))
                  (rdebug "fired draw-Bfield-vector   ~%"))
  :effects (
            (vector ?b (at (field ?loc magnetic ?source) ?t) ?dir-B)
            (variable ?mag-var (at (mag (field ?loc magnetic ?source)) ?t))
            (variable ?dir-var (at (dir (field ?loc magnetic ?source)) ?t))
            ;Because dir is problem given, find-by-psm won't ensure implicit eqn
            ;gets written.  Given value may not be used elsewhere so ensure it here.
            (implicit-eqn (= ?dir-var ?angle-value) (at (dir (field ?loc magnetic ?source)) ?t))
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
          (given (at (dir (current-length ?wire)) 1) ?dir-l)
	  (given (at (dir (relative-position ?loc ?wire)) 1) ?dir-r)
	  ; we require body at loc to be axis owner for vector
          (at-place ?b ?loc ?t)
	  (bind ?dir-B (cross-product-dir ?dir-l ?dir-r))
	  (test ?dir-B)
	  (test (not (eq ?B-dir 'zero)))
          (bind ?mag-var (format-sym "B_~A$~A" (body-name ?loc) ?t))
          (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
           (vector ?b (at (field ?loc magnetic ?wire) ?t) ?dir-B)
           (variable ?mag-var (at (mag (field ?loc magnetic ?wire)) ?t))
           (variable ?dir-var (at (dir (field ?loc magnetic ?wire)) ?t))
           (given (at (dir (field ?loc magnetic ?wire)) ?t) ?dir-B)
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
                  (given (at (dir (field ?loc magnetic ?source)) ?t) ?dir-B)
		  ; this may require drawing the velocity vector: 
                  (given (at (dir (velocity ?b)) ?t) ?dir-V)
		  ; following currently only works for dirs along axis
		  (bind ?F-dir (cross-product-dir ?dir-V ?dir-B))
		  ; make sure we have a non-null direction
		  (test ?F-dir) ; may be NIL on failure
		  (test (not (eq ?F-dir 'zero)))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?loc) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
 )
 :effects (
            (vector ?b (at (force ?b ?source magnetic) ?t) ?F-dir)
            (variable ?mag-var (at (mag (force ?b ?source magnetic)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source magnetic)) ?t))
            (given (at (dir (force ?b ?source magnetic)) ?t) ?F-dir)
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
                  (given (at (dir (field ?loc magnetic ?source)) ?t) ?dir-B)
		  ; this may draw velocity vector -- OK.
                  (given (at (dir (velocity ?b)) ?t) ?dir-V)
		  ; following currently only works for dirs along axis
		  ; we save the rhr direction for mentioning in the hints
		  (bind ?rhr-dir (cross-product-dir ?dir-V ?dir-B))
		  ; make sure we have a non-null direction
		  (test ?rhr-dir) ; may be NIL on failure
		  (test (not (eq ?rhr-dir 'zero)))
		  (bind ?F-dir (opposite ?rhr-dir))
		  (test ?F-dir) ; make sure this succeeded
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?loc) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
 )
 :effects (
            (vector ?b (at (force ?b ?source magnetic) ?t) ?F-dir)
            (variable ?mag-var (at (mag (force ?b ?source magnetic)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source magnetic)) ?t))
            (given (at (dir (force ?b ?source magnetic)) ?t) ?F-dir)
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
                  (given (at (dir (field ?loc magnetic ?source)) ?t) ?dir-B)
		  ; this may require drawing the velocity vector: 
                  (given (at (dir (velocity ?b)) ?t) ?dir-V)
		  ; following currently only works for dirs along axis
		  (bind ?F-dir (cross-product-dir ?dir-V ?dir-B))
		  ; make sure we have a non-null direction
		  (test ?F-dir) ; may be NIL on failure
		  (test (eq ?F-dir 'zero))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?loc) ?t))
 )
 :effects (
            (vector ?b (at (force ?b ?source magnetic) ?t) ?F-dir)
            (variable ?mag-var (at (mag (force ?b ?source magnetic)) ?t))
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
                  (not (given (at (dir (field ?loc magnetic ?source)) ?t) ?dir-B))
                  (not (vector ?b (at (force ?b ?source magnetic) ?t) ?dir))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?b) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Bforce-unknown  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source magnetic) ?t) unknown)
            (variable ?mag-var (at (mag (force ?b ?source magnetic)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source magnetic)) ?t))
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
		  (motion ?b ?t (straight ?dontcare unknown))
                  (test (time-pointp ?t))
                  (not (vector ?b (at (force ?b ?source magnetic) ?t) ?dir))
                  (bind ?mag-var (format-sym "Fb_~A$~A" (body-name ?b) ?t))
                  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (rdebug "fired draw-Bforce-unknown  ~%")
                  )
  :effects (
            (vector ?b (at (force ?b ?source magnetic) ?t) unknown)
            (variable ?mag-var (at (mag (force ?b ?source magnetic)) ?t))
            (variable ?dir-var (at (dir (force ?b ?source magnetic)) ?t))
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
           (given (at (dir (field region magnetic ?source)) ?t) ?dir-B)
           (given (at (dir (dipole-moment ?b)) ?t) ?dir-mu)
	   ; following currently only works for dirs along axis
	   (bind ?tau-dir (cross-product-dir ?dir-mu ?dir-B))
	   ; make sure we have a non-null direction
	   (test (not (eq ?tau-dir 'zero)))  
           (bind ?mag-var (format-sym "NTOR_~A_~A_~A" (body-name ?b) ?axis (time-abbrev ?t)))
           (bind ?dir-var (format-sym "O~A" ?mag-var))
	   ; need rotation axis for torque definition
	   (rotation-axis ?b ?axis)
 )
 :effects (
            (vector ?b (at (net-torque ?b ?axis) ?t) ?tau-dir)
            (variable ?mag-var (at (mag (net-torque ?b ?axis)) ?t))
            (variable ?dir-var (at (dir (net-torque ?b ?axis)) ?t))
            (given (at (dir (net-torque ?b ?axis)) ?t) ?tau-dir)
 )
 :hint (
	(point (string "The torque on a current loop points in the direction of the cross product of its magnetic dipole moment and the magnetic field vector at its location.")) 
	(teach (string "The torque vector on a current loop points in a direction perpendicular to the plane formed by the magnetic moment and magnetic field vectors, in a direction determined by the right hand rule: curl the fingers of your right hand from the dipole moment vector to the magnetic field vector, and your thumb will point in the direction of the torque."))
        (bottom-out (string "Because the magnetic moment has direction ~a and the magnetic field direction is ~a, the right-hand rule determines the direction of torque to be ~a. Use the torque drawing tool (labeled $t) to draw the net torque on ~a about ~a in the direction of ~A." 
			     (?dir-mu adj) (?dir-B adj) (?tau-dir adj) ?b ?axis (?tau-dir adj)))
        ))

(defoperator draw-torque-current-loop-zero (?b ?t)
 :preconditions (
	   ; loc must be "region"
           (given (at (dir (field region magnetic ?source)) ?t) ?dir-B)
           (given (at (dir (dipole-moment ?b)) ?t) ?dir-mu)
	   ; following currently only works for dirs along axis
	   (bind ?tau-dir (cross-product-dir ?dir-mu ?dir-B))
	   ; make sure we have a non-null direction
	   (test (eq ?tau-dir 'zero))  
           (bind ?mag-var (format-sym "NTOR_~A_~A_~A" (body-name ?b) ?axis (time-abbrev ?t)))
	   ; need rotation axis for torque definition
	   (rotation-axis ?b ?axis)
 )
 :effects (
            (vector ?b (at (net-torque ?b ?axis) ?t) ?tau-dir)
            (variable ?mag-var (at (mag (net-torque ?b ?axis)) ?t))
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
                  (any-member ?sought ((at (mag (force ?b ?source magnetic)) ?t)
                                       (at (mag (field ?loc magnetic ?source)) ?t)
                                       (at (charge-on ?b) ?t)))
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
                  (vector ?dontcare1 (at (field ?loc magnetic ?source) ?t) ?B-dir)
                  (vector ?dontcare2 (at (velocity ?b) ?t) ?V-dir)
		  (vector ?dontcare3 (at (force ?b ?source magnetic) ?t) ?F-dir)
		  ; retrieve vector variables for equation:
                  (in-wm (variable ?magB (at (mag (field ?loc magnetic ?source)) ?t)))
                  (in-wm (variable ?magV (at (mag (velocity ?b)) ?t)))
                  (in-wm (variable ?magF (at (mag (force ?b ?source magnetic)) ?t)))
		  ; define charge variable
                  (variable ?q (at (charge-on ?b) ?t))
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
                  (any-member ?sought((at (mag (force ?b ?source magnetic)) ?t)
                                      (at (dir (force ?b ?source magnetic)) ?t)
                                      (at (mag (field ?loc magnetic ?source)) ?t)
                                      (at (dir (field ?loc magnetic ?source)) ?t)
                                      (at (charge-on ?b) ?t)
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
                  (vector ?dontcare (at (field ?loc magnetic ?source) ?t) ?dir1) 
                  (vector ?b (at (force ?b ?source magnetic) ?t) ?dir2)
                  (vector ?b (at (velocity ?b) ?t) ?dir3)
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
                 (any-member ?sought((at (compo x 0 (force ?b ?source magnetic)) ?t)
				      (at (compo y 90 (velocity ?b)) ?t)
                                      (at (compo z 0 (field ?loc magnetic ?source)) ?t)
				      (at (compo z 0 (velocity ?b)) ?t)
                                      (at (compo y 90 (field ?loc magnetic ?source)) ?t)
                                      (at (charge-on ?b) ?t)
                                      ))
                  (at-place ?b ?loc ?t))
  :effects ((eqn-contains (charge-force-Bfield-x ?b ?t) ?sought)))

(defoperator charge-force-Bfield-x (?b ?t)
  :preconditions ((at-place ?b ?loc ?t)
                 (vector-diagram (charge-force-Bfield ?b ?t))
                 (variable ?Fx (at (compo x 0 (force ?b ?source magnetic)) ?t))
		 (variable ?Vy (at (compo y 90 (velocity ?b)) ?t))
                 (variable ?Bz (at (compo z 0 (field ?loc magnetic ?source)) ?t))
		 (variable ?Vz (at (compo z 0 (velocity ?b)) ?t))
		 (variable ?By (at (compo y 90 (field ?loc magnetic ?source)) ?t))
                 (variable ?q (at (charge-on ?b) ?t)))
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
                  (any-member ?sought((at (compo y 90 (force ?b ?source magnetic)) ?t)
				      (at (compo z 0 (velocity ?b)) ?t)
                                      (at (compo x 0 (field ?loc magnetic ?source)) ?t)
				      (at (compo x 0 (velocity ?b)) ?t)
                                      (at (compo z 0 (field ?loc magnetic ?source)) ?t)
                                      (at (charge-on ?b) ?t)
				     ))
		  (at-place ?b ?loc ?t))
  :effects ((eqn-contains (charge-force-Bfield-y ?b ?t) ?sought)))

(defoperator charge-force-Bfield-y (?b ?t)
  :preconditions ( 
                 (at-place ?b ?loc ?t)
                 (vector-diagram (charge-force-Bfield ?b ?t))
                 (variable ?Fy (at (compo y 90 (force ?b ?source magnetic)) ?t))
		 (variable ?Vz (at (compo z 0 (velocity ?b)) ?t))
                 (variable ?Bx (at (compo x 0 (field ?loc magnetic ?source)) ?t))
		 (variable ?Vx (at (compo x 0 (velocity ?b)) ?t))
		 (variable ?Bz (at (compo z 0 (field ?loc magnetic ?source)) ?t))
                 (variable ?q (at (charge-on ?b) ?t))
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
  :preconditions ((component-form)
                  (any-member ?sought ((at (compo z 0 (force ?b ?source magnetic)) ?t)
				      (at (compo x 0 (velocity ?b)) ?t)
                                      (at (compo y 90 (field ?loc magnetic ?source)) ?t)
				      (at (compo y 90 (velocity ?b)) ?t)
                                      (at (compo x 0 (field ?loc magnetic ?source)) ?t)
                                      (at (charge-on ?b) ?t)
				      ))
		  (at-place ?b ?loc ?t))
  :effects ((eqn-contains (charge-force-Bfield-z ?b ?t) ?sought)))

(defoperator charge-force-Bfield-z (?b ?t)
  :preconditions ( 
                 (at-place ?b ?loc ?t)
                 (variable ?Fz (at (compo z 0 (force ?b ?source magnetic)) ?t))
		 (variable ?Vx (at (compo x 0 (velocity ?b)) ?t))
                 (variable ?By (at (compo y 90 (field ?loc magnetic ?source)) ?t))
		 (variable ?Vy (at (compo y 90 (velocity ?b)) ?t))
		 (variable ?Bx (at (compo x 0 (field ?loc magnetic ?source)) ?t))
                 (variable ?q (at (charge-on ?b) ?t))
                 )
  :effects ( 
              (eqn (= ?Fz (* ?q (- (* ?Vx ?By) (* ?Vy ?Bx)))) (charge-force-Bfield-z ?b ?t)) 
           )
  :hint (
          (point (string "The z component of a vector cross product can be computed from the x and y components of the vectors being multiplied. You can use this formula to compute the z component of the magnetic force."))
	  (teach (string "The z component of the cross product of two vectors V and B is equal to V_x*B_y - V_y*B_x. "))
          (bottom-out (string "Write the equation ~A"  
	                      ((= ?Fz (* ?q (- (* ?Vx ?By) (* ?Vy ?Bx)))) algebra) ))
        ))


#| ; not using this, just show dir on diagram
(defoperator charge-force-Bfield-dir-contains (?sought)
  :preconditions ((debug "Using write-charge-force-Bfield-dir-contains ~%")
                  (any-member ?sought ((at (dir (force ?b ?source magnetic)) ?t)
                                       (at (dir (field ?loc magnetic ?source) ?t))
                                       (at (charge-on ?b) ?t)))
                  ;;(not (component-form))
                  (body ?b)
                  (rdebug "Firing write-charge-force-Bfield-dir-contains ~%")
                  )
  :effects(
           (eqn-contains (charge-force-Bfield-dir ?b) ?sought)
           )) 

(defoperator write-force-dir-charge-Bfield-pos (?b ?t)
  :preconditions ((debug "Using write-force-dir-charge-Bfield-pos ~%")
                  (at-place ?b ?loc ?t)
                  (variable ?q (at (charge-on ?b) ?t))
                  (given (at (dir (field ?loc magnetic ?source)) ?t) (dnum ?dir1 ?doncare1)) 
                  (motion ?b ?t (straight NIL (dnum ?dir2 ?dontcare1)))
                  (variable ?OF (at (dir (force ?b ?source magnetic)) ?t))
                  (bind ?in-out (if (>= ?dir1 ?dir2) '(dnum 0 |deg|) '(dnum 180 |deg|)))
                  (sign-charge ?b pos)
                  (debug "Fired write-force-dir-charge-Bfield-pos  ~%")
                  )
  :effects (
            (eqn (= ?OF ?in-out)(charge-force-Bfield-dir ?b))
            )
  :hint (
         (point (string "What is the relationship between the force, the charge and the magnetic field?"))
         (teach (kcd "write-force-dir-charge-Bfield-pos")
                 (string "The force on the charged particle is perpendicular to both the velocity vector and the magnetic field vector."))
         (bottom-out (string "Use the right hand rule using the velocity vector and the magnetic field vector."))
         ))
         
(defoperator write-force-dir-charge-Bfield-neg (?b ?t)
  :preconditions ((debug "Using write-force-dir-charge-Bfield-neg ~%")
                  (at-place ?b ?loc ?t)
                  (variable ?q (at (charge-on ?b) ?t))
                  (given (at (dir (field ?loc magnetic ?source)) ?t) (dnum ?dir1 ?doncare1)) 
                  (motion ?b ?t (straight NIL (dnum ?dir2 ?dontcare1)))
                  (variable ?OF (at (dir (force ?b ?source magnetic)) ?t))
                  (bind ?in-out (if (>= ?dir1 ?dir2) 180 0))
                  (sign-charge ?b neg)
                  (debug "Fired write-force-dir-charge-Bfield-neg  ~%")
                  )
  :effects (
            (eqn (= ?OF ?in-out)(charge-force-Bfield-dir ?b))
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
                   (bind ?vector `(at (,?vectype ,?b . ?args) ,?t))
                   (test (or (equal 'force (first (second ?vector)))
                             (equal 'field (first (second ?vector)))))
                   (bind ?xc (vector-xc ?vector))
    ;; we test whether xc of vector is a problem sought. HACK: This relies
    ;; on *cp* as always holding the current problem, which is not guaranteed
    ;; if problem solver is not invoked through our interface functions.
    ;; But there shoudl be some way to access this info from the environment.
                   ; (test (member ?xc (problem-soughts *cp*) :test #'equal))
                   (not (given (at (dir ?vector) ?t) (dnum ?dir |deg|)))
                   (bind ?mag-var (format-sym "~A_~A_~A" ?vectype (body-name ?b) (time-abbrev ?t)))                                                         
                   (bind ?dir-var (format-sym "O~A" ?mag-var))
                   (rdebug "fired draw-sought-force-vector-unknown-with-multi-args ~%")
                   )
   :effects (
             (vector ?b (at (?vectype ?b . ?args) ?t) unknown)
             (variable ?mag-var (at (mag (?vectype ?b . ?args)) ?t))
             (variable ?dir-var (at (dir (?vectype ?b . ?args)) ?t))
             )
  :hint (
         (point (string "Try drawing a diagram."))
         (teach (string "The diagram should show the vector you are seeking."))
         (bottom-out (string "Draw a diagram showing the vector you are seeking using the given components."))
        ))

|#

