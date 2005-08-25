;;;;
;;;; Fluids
;;;;

;;; Fluids variables. 
;;;  Use height h to define depth with h = positive if higher than reference 
;;;  and h = negative if lower than reference.
 

;; Define the quantity mass density. The density defined here is a vouume density.
(def-qexp mass-density (mass-density ?material)
   :units |kg/m^3|
   :restrictions nonnegative   
   :english ("the mass density of ~A" (nlg ?material))
   :fromWorkbench `(mass-density ,body)
)

(defoperator define-mass-density (?material)
  :preconditions ( (bind ?$rm-var (format-sym "$rm_~A" (body-name ?material))) )
  :effects ( (variable ?$rm-var (mass-density ?material))
             (define-var (mass-density ?material)))
  :hint (
       (bottom-out (string "Define a variable for the mass density of ~A by using the Add Variable command on the Variable menu and selecting Mass Density."  ?material))
       ))

;;; Definition of mass density: rho = m/V
;;;
;;; Currently mass-density is a property of a body.  It might be preferable 
;;; to use terms for kinds of materials, ; e.g. water, copper, bronze, that 
;;; could be distinct from terms for particular bodies such as block. 
;;; Then density could be a property of the material.
;;; We would need something like (made-of ?body ?material) in the givens to 
;;; do that.  We stick to the simpler method for now of treating density of 
;;; objects as given.
(def-psmclass density (density ?body ?t)
  :complexity major
  :english ("the definition of mass density")
  :expformat ("applying the definition of mass density")
  :EqnFormat ("$r = m/V"))

(defoperator density-contains (?sought)
   :preconditions (
     (any-member ?sought ( (mass-density ?body)
		           (mass ?body)
		           (at (volume ?body) ?t) ))
    (time ?t)
   )
   :effects (
       (eqn-contains (density ?body ?t) ?sought)
   ))

(defoperator write-density (?body ?time)
   :preconditions (
      (variable ?rho  (mass-density ?body))
      (variable ?m    (mass ?body))
      (variable ?V    (at (volume ?body) ?time))
   )
   :effects (
      (eqn (= ?rho (/ ?m ?V)) (density ?body ?time))
   )
   :hint (
      (point (string "The definition of mass density relates mass and volume of an object."))
      (teach (string "The mass density of a body is defined as mass per its volume."))
      (bottom-out (string "Write the equation ~A" ((= ?rho (/ ?m ?V)) algebra) ))
   ))


;;Define the quantity pressure.  The variable of choice is "P".
(def-qexp pressure (pressure ?position)
   :units |Pa|
   :english ("the pressure at point ~a in the fluid" (nlg ?position))
   :fromWorkbench `(at (pressure ,body) ,time)
)

(defoperator define-pressure (?point ?time)
  :preconditions ( (bind ?Pr-var (format-sym "Pr_~a" (body-name ?point))) )
  :effects ( (variable ?Pr-var (at (pressure ?point) ?time))
             (define-var (at (pressure ?point) ?time)) )
  :hint (
       (bottom-out (string "Define a variable for the pressure at ~a by using the Add Variable command on the Variable menu and selecting Pressure."  ?point))
       ))


;; Define the pressure of a standard atmosphere.  
;; The symbol for atmospheric pressure at standard conditions is Pr0.  
;; We should remember to label the points in our drawings
;;  for problems in the workbench starting with 1 vice 0.  
;; The value of Pr0 is 1.013E5 Pa.  
;; See constants.cl, function enter-predefs

(def-qexp atmosphere (atmosphere)
   :units |Pa|
   :english ("the pressure of one standard atmosphere")
   :fromWorkbench `(atmosphere)
)

; should normally be predefined in fluids problems:
(defoperator define-atmosphere-var ()
 :effects ( (variable |Pr0| (atmosphere)) ))

(def-psmclass std-constant-Pr0 (std-constant atmosphere)
  :complexity simple 
  :english ("value of the atmospheric pressure Pr0 ")
  :expformat ("defining the value of the atmospheric pressure Pr0")
  :EqnFormat ("Pr0 = 1.013E5 Pa"))

(defoperator atmosphere-contains()
  :effects ( (eqn-contains (std-constant atmosphere) (atmosphere)) ))

(defoperator write-value-of-atmosphere ()
  :preconditions 
    ( (variable ?Pr0-var (atmosphere)) )
  :effects ( 
    (eqn (= ?Pr0-var (dnum 1.013E5 |Pa|)) (std-constant atmosphere)) 
   )
  :hint
  ((point (string "You can find the value of a standard atmosphere in your textbook. The value to four significant figures should be used in Andes problems."))
   (teach (string "You can use 1.013E5 |Pa| as the value of Pr0."))
   (bottom-out (string "Write the equation ~A" ((= ?Pr0-var (dnum 1.013E5 |Pa|)) algebra)))
    ))

;;Assign a value to the pressure that is listed in the givens as "open-to-atmosphere"
;;in a problem.  We would like Andes to recognize the pressure at that point is Pr0 Pa.
;;Write the expression (open-to-atmosphere ?point) in the givens.
(def-psmclass pressure-at-open-to-atmosphere (pressure-at-open-to-atmosphere ?point ?time)
  :complexity minor
  :english ("the formula for pressure at a point open to the atmosphere")
  :ExpFormat ("setting the pressure at a point open to the atmosphere")
  :EqnFormat ("P = Pr0")) 

(defoperator pressure-at-open-to-atmosphere-contains (?quantity)
   :preconditions (
     (open-to-atmosphere ?point ?time)  
     (any-member ?quantity ( (at (pressure ?point) ?time)
   ))
)
   :effects (
     (eqn-contains (pressure-at-open-to-atmosphere ?point ?time) ?quantity)
   ))

(defoperator pressure-at-open-to-atmosphere (?point ?time)
  :preconditions (
     (open-to-atmosphere ?point ?time) 
     (variable  ?Pr-var (at (pressure ?point) ?time))
     ; should be predefined in Andes:
     (variable ?Pr0-var (atmosphere))
)
  :effects ( 
    (eqn (= ?Pr-var ?Pr0-var ) (pressure-at-open-to-atmosphere ?point ?time)) 
   )
  :hint
  ((point (string "Where a fluid is open to the atmosphere, the pressure in the fluid must be one standard atmosphere. The atmospheric pressure is denoted in Andes by the predefined constant ~a." (?Pr0-var algebra) ))
   ;(teach (string "You can use 1.013E5 |Pa| as the value of one standard atmosphere."))
   (bottom-out (string "Write the equation ~A" ((= ?Pr-var ?Pr0-var) algebra)))
    ))


;  Fluids Equations

;pressure at depth Pr2 - Pr1 = rho_m*g*(h1-h2)
(def-psmclass pressure-height-fluid (pressure-height-fluid ?point ?time)
  :complexity major  
  :english ("the pressure at a height in a fluid")
  :ExpFormat ("finding the pressure at a level in a fluid")
  :EqnFormat ("P2 - P1 = $r*g*(h1-h2)")) 

(defoperator pressure-height-fluid-contains (?sought)
   :preconditions (
     ; problem givens must specify which fluid point is in:
     (in-wm (in-fluid ?point-bottom ?material ?point-top ?time))
     (bind ?fluid-at-top  (format-sym "FLUID_AT_~a" ?point-top))
     (bind ?fluid-at-bottom  (format-sym "FLUID_AT_~a" ?point-bottom))
     (any-member ?sought ( (at (pressure ?point-bottom) ?time)
                           (at (height ?fluid-at-top) ?time)
			   (at (height ?fluid-at-bottom) ?time)
			   (mass-density ?fluid))  )
   )
   :effects (
     (eqn-contains (pressure-height-fluid ?point-bottom ?time) ?sought)
   ))

; TODO: add eqn-contains for when sought is pressure at point-top as well

(defoperator pressure-height-fluid (?point-bottom ?time)
   :preconditions (
       (in-wm (in-fluid ?point-bottom ?material ?point-top ?time))
       (bind ?fluid-at-top  (format-sym "FLUID_AT_~a" ?point-top))
       (bind ?fluid-at-bottom  (format-sym "FLUID_AT_~a" ?point-bottom))
       (variable  ?Pr2 (at (pressure ?point-bottom) ?time))
       (variable  ?Pr1 (at (pressure ?point-top) ?time))
       (variable  ?rho (mass-density ?material))
       (in-wm (near-planet ?planet))
       (variable  ?g   (gravitational-acceleration ?planet))
       (variable  ?h1  (at (height ?fluid-at-top) ?time))
       (variable  ?h2  (at (height ?fluid-at-bottom) ?time))
   )
   :effects (
    (eqn  (= (- ?Pr2 ?Pr1) (* ?rho ?g (- ?h1 ?h2))) (pressure-height-fluid ?point-bottom ?time))
   )
   :hint (
      (point (string "Remember the pressure at some depth in a fluid must be greater than the pressure at a higher point by an amount equal to the weight per unit area of the column of additional fluid above the lower point."))
      (teach (string "The weight per unit area of a column of fluid of uniform density will be equal to the mass density $r times g times the positive height of the column, h_top - h_bottom. This will be the difference between the pressure at the bottom  and the pressure at the top.")) 
      (bottom-out (string "Write the equation ~A" 
                     ((= (- ?Pr2 ?Pr1) (* ?rho ?g (- ?h1 ?h2))) algebra) ))
   ))

; Bernoulli's principle: 
(def-psmclass bernoulli (bernoulli ?point1 ?point2 ?time)
  :complexity major  
  :english ("Bernoulli's principle")
  :ExpFormat ("applying Bernoulli's principle")
  :EqnFormat ("0.5*$r*v1^2 + $r*g*h1 + P1 = 0.5*$r*v2^2 + $r*g*h2 + P2")) 

(defoperator bernoulli-contains (?sought)
   :preconditions (
     ; problem givens must specify that bernoulli's applies:
     (in-wm (bernoulli ?point1 ?fluid ?point2))
     (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
     (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
     (any-member ?sought ( (at (pressure ?point1) ?time)
                           (at (pressure ?point2) ?time)
                           (at (height ?fluid-at-p1) ?time)
			   (at (height ?fluid-at-p2) ?time)
			   (mass-density ?fluid)
                           (at (mag (velocity ?fluid-at-p1)) ?time)
                           (at (mag (velocity ?fluid-at-p2)) ?time)
                         ))
   )
   :effects (
     (eqn-contains (bernoulli ?point1 ?point2 ?time) ?sought)
   ))

(defoperator bernoulli (?point1 ?point2 ?time)
   :preconditions (
      (in-wm (bernoulli ?point1 ?fluid ?point2))
      (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
      (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
      (variable  ?Pr1 (at (pressure ?point1) ?time))
      (variable  ?Pr2 (at (pressure ?point2) ?time))
       (variable  ?rho (mass-density ?fluid))
       (in-wm (near-planet ?planet))
       (variable  ?g   (gravitational-acceleration ?planet))
       (variable  ?h2  (at (height ?fluid-at-p2) ?time))
       (variable  ?h1  (at (height ?fluid-at-p1) ?time))
       (variable  ?v1  (at (mag (velocity ?fluid-at-p1)) ?time))
       (variable  ?v2  (at (mag (velocity ?fluid-at-p2)) ?time))
   )
   :effects (
    (eqn  (= (+ (* 0.5 ?rho (^ ?v1 2)) (* ?rho ?g ?h1) ?Pr1) 
             (+ (* 0.5 ?rho (^ ?v2 2)) (* ?rho ?g ?h2) ?Pr2)) 
                (bernoulli ?point1 ?point2 ?time))
   )
   :hint (
      (point (string "Bernoulli's principle is a conservation principle which has the consequence that where velocity is high, pressure is low, and where velocity is high, pressure is low."))
      (teach (string "Bernoulli's principle says that the sum (P + 0.5$rv^2 + $rgh) is conserved along a streamline."))
      (bottom-out (string "Write the equation ~A" 
                     ((= (+ (* 0.5 ?rho (^ ?v1 2)) (* ?rho ?g ?h1) ?Pr1) 
                         (+ (* 0.5 ?rho (^ ?v2 2)) (* ?rho ?g ?h2) ?Pr2))  
                       algebra) ))
   ))


;equation of continuity A1*v1 = A2*v2
(def-psmclass equation-of-continuity (equation-of-continuity ?point1 ?point2 ?time)
  :complexity major  
  :english ("the equation of continuity for incompressible fluid")
  :ExpFormat ("Applying the equation of continuity")
  :EqnFormat ("A1*v1 = A2*v2")) 

(defoperator continuity-contains (?sought)
   :preconditions (
     ; problem givens must specify that bernoulli's applies:
     (in-wm (bernoulli ?point1 ?fluid ?point2))
     (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
     (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
     (any-member ?sought ( 
                           (at (mag (velocity ?fluid-at-p1)) ?time)
                           (at (mag (velocity ?fluid-at-p2)) ?time)
                           (at (area-at ?point1) ?time)
                           (at (area-at ?point2) ?time)
                         ))
   )
   :effects (
     (eqn-contains (equation-of-continuity ?point1 ?point2 ?time) ?sought)
   ))

(defoperator continuity (?point1 ?point2 ?time)
   :preconditions (
       (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
       (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
       (variable  ?v1  (at (mag (velocity ?fluid_at_p1)) ?time))
       (variable  ?v2  (at (mag (velocity ?fluid_at_p2)) ?time))
       (variable ?A1 (at (area-at ?point1) ?time))
       (variable ?A2 (at (area-at ?point2) ?time))
   )
   :effects (
    (eqn  (= (* ?A1 ?v1) (* ?A2 ?v2)) 
                (equation-of-continuity ?point1 ?point2 ?time))
   )
   :hint (
      (point (string "You can use equation-of-continuity"))
      ;(teach (string "The equation-of-continuity states that the volume rate of flow (Area*speed) of an incompressible fluid in a stream is a constant for that stream."))
      (bottom-out (string "Write the equation ~A" 
                     ((= (* ?A1 ?v1) (* ?A2 ?v2)) 
                       algebra) ))
   ))


;;Define the quantity cross-sectional area at a point.  The variable choice is "A".
(def-qexp area-at (area-at ?position)
     :units |m^2|
     :restrictions positive
     :english ("the cross-sectional area at ~A" (nlg ?position))
     :fromworkbench `(at (area-at ,body) ,time)
)

(defoperator define-area-at (?point ?time)
     :preconditions((bind ?A-var (format-sym "Ac_~A" (body-name ?point))))
     :effects ((variable ?A-var (at (area-at ?point) ?time))
               (define-var (at (area-at ?point) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the cross sectional area at ~A by using the Add Variable command on the Variable menu and selecting Area."  ?body))
          ))

;; area of a shape
(def-qexp area (area ?shape)
     :units |m^2|
     :restrictions positive
     :english ("the area of ~A" (nlg ?shape))
     :fromworkbench `(area ,body)
)

(defoperator define-area (?shape)
     :preconditions((bind ?Ac-var (format-sym "A_~A" (body-name ?shape))))
     :effects ((variable ?Ac-var (area ?shape))
               (define-var (area ?shape)))
     :hint (
          (bottom-out (string "Define a variable for the area of ~A by using the Add Variable command on the Variable menu and selecting Area."  ?shape))
          ))

;; quantity to represent radius of a circular shape
(def-qexp radius-of-circle (radius-of-circle ?body)
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
          (bottom-out (string "Define a variable for the radius of ~A by using the Add Variable command on the Variable menu and selecting circle radius."  ?body))
          ))

;; quantity to represent diameter of a circular shape
(def-qexp diameter-of-circle (diameter-of-circle ?body)
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
          (bottom-out (string "Define a variable for the diameter of ~A by using the Add Variable command on the Variable menu and selecting circle diameter."  ?body))
          ))

;; quantity to represent circumference of a circular shape
(def-qexp circumference-of-circle (circumference-of-circle ?body)
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
          (bottom-out (string "Define a variable for the circumference of ~A by using the Add Variable command on the Variable menu and selecting circle circumference."  ?body))
          ))

;; equation of the area of a circle Ac = pi*rc^2
(def-psmclass area-of-circle (area-of-circle ?body)
  :complexity minor  
  :english ("the formula for the area of a circle")
  :ExpFormat ("Applying the formula for the area of a circle")
  :EqnFormat ("Ac = $p*r^2")) 

 (defoperator area-of-circle-contains (?sought)
   :preconditions (
		   (in-wm (shape ?shape circle))
		   (any-member ?sought ( 
					(radius-of-circle ?shape)
					(area ?shape)
					))  
   )
   :effects (
	     (eqn-contains (area-of-circle ?shape) ?sought)
   ))

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
                     (= ?Ac (* $P (^ ?rc 2))) algebra) ))
   )

;; equation of the circumference of a circle c = 2*pi*r
(def-psmclass circumference-of-circle-r (circumference-of-circle-r ?body)
  :complexity minor  
  :english ("the formula for the circumference of a circle")
  :ExpFormat ("Applying the formula for the circumference of a circle")
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
                     (= ?Ac (* 2 $P ?rc)) algebra) ))
   )

;; equation of the circumference of a circle c = pi*d
(def-psmclass circumference-of-circle-d (circumference-of-circle-d ?body)
  :complexity minor  
  :english ("the formula for the circumference of a circle")
  :ExpFormat ("Applying the formula for the circumference of a circle")
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
                     (= ?Ac (* $P ?dc)) algebra) ))
   )

;;;
;;; Pressure forces: 
;;;

;;; Problems involving pressure force require a statement of the form
;;;   (fluid-contact ?body ?surface ?fluid ?point ?time ?direction)
;;;
;;; surface names the surface of the body at which the pressure force acts.
;;; direction gives the direction of the force.
;;; point names the spatial point at which the fluid pressure is defined. 
;;; Possibly it can be the same as the surface name.
;;;
;;; There can be more than one surface on a body. If we need multiple 
;;; pressure forces from the same fluid, we have to introduce separate 
;;; agents such as "fluid_at_left" and "fluid_at_right" to make unique force 
;;; terms.

(defoperator find-pressure-force (?body ?surface ?fluid ?t)
   :preconditions (
    (in-wm (fluid-contact ?body ?surface ?fluid ?point ?t-pressure ?dir))
    (test (tinsidep ?t ?t-pressure))
  ) 
  :effects (
    (force ?body ?fluid pressure ?t ?dir action)
  ))

(defoperator draw-pressure (?b ?fluid ?t)
  :preconditions
   ((force ?b ?fluid pressure ?t ?dir action)
    ; need to get body containing surface for vector statement
    (in-wm (fluid-contact ?b ?surface ?fluid ?point ?t-pressure ?dir))
    (not (vector ?b (at (force ?b ?fluid pressure) ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fp_~A_~A_~A" (body-name ?b) ?fluid
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a pressure on ~a due to ~a at ~a.~%" ?dir ?b ?fluid ?t)
    )
  :effects
   ((vector ?b (at (force ?b ?fluid pressure) ?t) ?dir)
    (variable ?mag-var (at (mag (force ?b ?fluid pressure)) ?t))
    (variable ?dir-var (at (dir (force ?b ?fluid pressure)) ?t))
    (given (at (dir (force ?b ?fluid pressure)) ?t) ?dir))
  :hint
   ((point (string "Notice that ~a is in contact with ~A." ?b (?fluid agent)))
    (teach (string "When a body in contact with a fluid, the fluid exerts a pressure force on it, acting perpendicular to the surface of contact."))
    (bottom-out (string "Because ~a presses against ~a at ~a, draw a pressure force on ~a due to ~a at an angle of ~a degrees." 
			(?fluid agent) ?b ?surface ?b (?fluid agent) ?dir))
    ))

;;;
;;; Scalar equation for pressure force magnitude:
;;;    Fp = P*A (definition of pressure)
;;;
(def-psmclass pressure-force (pressure-force ?body ?time ?fluid)
  :complexity major  
  :english ("the definition of pressure")
  :ExpFormat ("applying the definition of pressure")
  :EqnFormat ("Fp = P*A"))

(defoperator pressure-force-contains (?sought)
  :preconditions(
    (any-member ?sought ( (at (mag (force ?body ?fluid pressure)) ?time)
                          (at (pressure ?point) ?time)
			  (area ?surface)  ))
    (time ?time) ;because area is timeless
    (in-wm (fluid-contact ?body ?surface ?fluid ?point ?t-pressure ?dir))
    (test (tinsidep ?time ?t-pressure))
  )
  :effects (
    (eqn-contains (pressure-force ?body ?time ?fluid) ?sought)
  ))

(defoperator pressure-force (?body ?t ?fluid)
  :preconditions(
    (in-wm (fluid-contact ?body ?surface ?fluid ?point ?t-pressure ?dir))
    (test (tinsidep ?t ?t-pressure))
    (body ?body)
    (variable ?Fp (at (mag (force ?body ?fluid pressure)) ?t))
    (variable ?P  (at (pressure ?point) ?t))
    (variable ?A  (area ?surface))
  )
  :effects (
     (eqn (= ?Fp (* ?P ?A)) (pressure-force ?body ?t ?fluid))
  )
  :hint (
    (bottom-out (string "Write the equation ~A" ((= ?Fp (* ?P ?A)) algebra)))
  ))


;
; Buoyant force. Represents net effect of pressure forces on immersed object, so
; we shouldn't use in combination with pressure forces. Will indicate by
; proposition of form (buoyant-force ?body ?fluid ?t-buoyant ?dir), instead
; of fluid-contact form used for pressure forces.
;
(defoperator find-buoyant-force (?body ?fluid ?t)
   :preconditions (
    (in-wm (buoyant-force ?body ?fluid ?t-buoyant ?dir))
    (test (tinsidep ?t ?t-buoyant))
  ) 
  :effects (
    (force ?body ?fluid buoyant ?t ?dir action)
  ))

(defoperator draw-buoyant-force (?b ?fluid ?t)
  :preconditions
   ((force ?b ?fluid buoyant ?t ?dir action)
    (not (vector ?b (at (force ?b ?fluid buoyant) ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fb_~A_~A_~A" (body-name ?b) ?fluid
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a buoyant force on ~a due to ~a at ~a.~%" 
	   ?dir ?b (?fluid agent) ?t)
    )
  :effects
   ((vector ?b (at (force ?b ?fluid buoyant) ?t) ?dir)
    (variable ?mag-var (at (mag (force ?b ?fluid buoyant)) ?t))
    (variable ?dir-var (at (dir (force ?b ?fluid buoyant)) ?t))
    (given (at (dir (force ?b ?fluid buoyant)) ?t) ?dir))
  :hint
   ((point (string "Notice that ~a is submerged in ~A." ?b (?fluid agent)))
    (teach (string "When a body is submerged in a fluid, the upward fluid pressure on its bottom is greater than the downward pressure on its top. The net effect can be represented by an upward buoyant force on the object."))
    (bottom-out (string "Because ~a is submerged in ~a, draw a buoyant force on ~a due to ~a at an angle of ~a." 
			?b (?fluid agent) ?b (?fluid agent) ?dir))
    ))

;;Quantity: The volume of a body
(def-qexp volume (volume ?body)
     :units |m^3|
     :restrictions nonnegative ; we allow zero-volume for negligible parts of compound bodies
     :english ("the volume of ~A" (nlg ?body))
     :fromworkbench `(at (volume ,body) ,time)
   )

(defoperator define-volume (?body ?time)
     :preconditions((bind ?Vol-var (format-sym "Vol_~A" (body-name ?body))))
     :effects ((variable ?Vol-var (at (volume ?body) ?time))
               (define-var (at (volume ?body) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the volume of ~A by using the Add Variable command on the Variable menu and selecting Volume."  ?body))
	   ))

; TODO: add equations for volumes of various shapes


; Volume of compound body is sum of volumes of its parts. This parallels mass-compound
; in Newtons2.cl

(defoperator volume-compound-contains (?b-sought ?t)
  :preconditions (
   ; compound must exist
   (object (compound . ?bodies))
   ; applies if sought is volume of compound or one of its parts
   (test (or (member ?b-sought ?bodies)
             (equal ?b-sought `(compound ,@?bodies))))
  )
  :effects (
    (eqn-contains (volume-compound ?bodies ?t) (at (volume ?b-sought) ?t))
  ))

(defoperator write-volume-compound (?bodies ?t)
  :preconditions (
    (variable ?Vwhole-var (at (volume (compound . ?bodies)) ?t))
    (map ?body ?bodies
         (variable ?Vpart-var (at (volume ?body) ?t)) 
	 ?Vpart-var ?Vpart-vars) 
  )
  :effects (
     (eqn (= ?Vwhole-var (+ . ?Vpart-vars)) (volume-compound ?bodies ?t))
  )
  :hint
  ((point (string "How does the volume of a compound body relate to the volumes of its parts?"))
   (teach (string "The volume of a compound body is equal to the sum of the volumes of its parts."))
   (bottom-out (string "Write the equation ~A" ((= ?Vwhole-var (+ . ?Vpart-vars)) algebra)))
   ))
 
;
; Archimedes principle: magnitude of buoyant force = weight of fluid displaced,
;   Fb = rho*V*g 

(def-psmclass archimedes (archimedes ?body ?fluid ?t)
  :complexity major  
  :english ("Archimedes' principle")
  :ExpFormat ("applying Archimedes' principle")
  :EqnFormat ("Fb = $rf*V*g"))

(defoperator archimedes-contains (?sought)
  :preconditions (
    (in-wm (buoyant-force ?body ?fluid ?t-buoyant ?dir))
    (any-member ?sought ( (at (mag(force ?b ?fluid ?buoyant)) ?t)
                          (mass-density ?fluid) 
                          (at (volume ?b) ?t) ))
    (test (tinsidep ?t ?t-buoyant))
  )
  :effects (
    (eqn-contains (archimedes ?body ?fluid ?t) ?sought)
  ))

(defoperator write-archimedes (?b ?fluid ?t)
   :preconditions (
       (in-wm (near-planet ?planet))
       (variable ?Fb  (at (mag(force ?b ?fluid ?buoyant)) ?t))
       (variable ?rho (mass-density ?fluid))
       (variable ?g   (gravitational-acceleration ?planet))
       (variable ?V   (at (volume ?b) ?t))
   )
   :effects (
       (eqn (= ?Fb (* ?rho ?V ?g)) (archimedes ?b ?fluid ?t))
   )
   :hint (
       (point (string "Archimedes principle says that the magnitude of the buoyant force on an object submerged in a fluid is equal to the weight of the fluid displaced by the object."))
       (teach (string "The total mass of displaced fluid will be given by the fluid's mass density times the volume displaced. The weight of displaced fluid will be this total mass times g."))
       (bottom-out (string "Write the equation ~A" ((= ?Fb (* ?rho ?V ?g)) algebra) ))
   ))


