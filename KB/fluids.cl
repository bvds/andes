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
;;;;
;;;; Fluids
;;;;

;;; Fluids variables. 
;;;  Use height h to define depth with h = positive if higher than reference 
;;;  and h = negative if lower than reference.
 

;; Define the quantity mass density.
(def-qexp mass-density (mass-density ?material)
  :symbol-base |$r|     
  :short-name "mass density"	
   :units |kg/m^3|
   :restrictions nonnegative   
   :nlg-english ("the mass density of ~A" (nlg ?material))
)

(defoperator define-mass-density (?material)
  :preconditions ( (bind ?$rm-var (format-sym "$rm_~A" (body-name ?material))) )
  :effects ( (variable ?$rm-var (mass-density ?material))
             (define-var (mass-density ?material)))
  :hint 
  ((bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Mass Density." 
			   ((mass-density ?material) def-np)))))

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
  :short-name "mass density defined"
  :nlg-english ("the definition of mass density")
  :expformat ("applying the definition of mass density")
  :EqnFormat ("&rho; = m/V"))

(defoperator density-contains (?sought)
   :preconditions (
     (any-member ?sought ( (mass-density ?body)
		           (mass ?body)
		           (volume ?body :time ?t) ))
    (time ?t)
   )
   :effects (
       (eqn-contains (density ?body ?t) ?sought)
   ))

(defoperator write-density (?body ?time)
   :preconditions (
      (variable ?rho  (mass-density ?body))
      (variable ?m    (mass ?body))
      (variable ?V    (volume ?body :time ?time))
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
(def-qexp pressure (pressure ?position :time ?time)
  :symbol-base |P|     
  :short-name "pressure"	
   :units |Pa|
   :nlg-english ("the pressure at ~a" 
	     (nlg ?position 'at-time ?time))
)

(defoperator define-pressure (?point ?time)
  :preconditions ( (bind ?Pr-var (format-sym "Pr_~a" (body-name ?point))) )
  :effects ( (variable ?Pr-var (pressure ?point :time ?time))
             (define-var (pressure ?point :time ?time)) )
  :hint (
       (bottom-out (string "Define a variable for ~a by using the Add Variable command on the Variable menu and selecting Pressure."  
			   ((pressure ?point :time ?time) def-np)))
       ))


;; Define the pressure of a standard atmosphere.  
;; The symbol for atmospheric pressure at standard conditions is Pr0.  
;; We should remember to label the points in our drawings
;;  for problems in the workbench starting with 1 vice 0.  
;; The value of Pr0 is 1.013E5 Pa.  
;; See constants.cl, function enter-predefs

(def-qexp atmosphere (atmosphere)
  :units |Pa|
  :restrictions positive
  :short-name "standard atmosphere"
  :nlg-english ("the pressure of one standard atmosphere")
)

; should normally be predefined in fluids problems:
(defoperator define-atmosphere-var ()
  :effects ( (variable |Pr0| (atmosphere))
	     (define-var (atmosphere)) )
  :hint 
  ((bottom-out 
    (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting standard atmosphere." 
	    ((atmosphere) def-np)))))

   

(post-process add-standard-atmosphere (problem)
  "In fluids problems, add Pr0 to list of pre-defined scalars"
  ;; for fluids problems, predefine atmospheric pressure constant
  ;; in principle, should test it is not already present
  ;; and test for the next available Var-number
  (when (and (member 'fluids (problem-features problem))
	     ;; test whether it has been done already
	     (notany #'(lambda (x) (search "atmosphere" x))
		     (problem-predefs problem)))
   (push '((EQN (= |Pr0| (DNUM 101300.0 |Pa|))) . 
	    ((:action . "new-object") (:id . "peq") (:type . "equation")
	     (:text . "Pr0=1.013E5 Pa") (:width . 300)
	     (:mode . "unknown") (:x . 450) (:y . 70)))
	  (problem-predefs problem))
    (push '((define-var (atmosphere)) . 
	    ((:action . "new-object") (:id . "pvar") (:type . "statement")
	     (:text . "Pr0 is the pressure of one standard atmosphere")
	     (:width . 300)
	     (:symbol . "Pr0") (:mode . "unknown") (:x . 450) (:y . 55)))
	  (problem-predefs problem))))

(def-psmclass std-constant-Pr0 (std-constant (atmosphere))
  :complexity simple 
  :short-name "atmospheric pressure"
  :nlg-english ("value of the atmospheric pressure Pr0 ")
  :expformat ("defining the value of the atmospheric pressure Pr0")
  :EqnFormat ("Pr0 = 1.013E5 Pa"))

(defoperator atmosphere-contains()
  :effects ( (eqn-contains (std-constant (atmosphere)) (atmosphere)) ))

(defoperator write-value-of-atmosphere ()
  :preconditions 
    ( (variable ?Pr0-var (atmosphere)) )
  :effects ( 
    (eqn (= ?Pr0-var (dnum 1.013E5 |Pa|)) (std-constant (atmosphere))) 
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
  :complexity definition
  :short-name "point 1 open to atmosphere"
  :nlg-english ("the formula for pressure at a point open to the atmosphere")
  :ExpFormat ("setting the pressure at a point open to the atmosphere")
  :EqnFormat ("P = Pr0")) 

(defoperator pressure-at-open-to-atmosphere-contains (?quantity)
   :preconditions (
     (open-to-atmosphere ?point ?time)  
     (any-member ?quantity ( (pressure ?point :time ?time)
   ))
)
   :effects (
     (eqn-contains (pressure-at-open-to-atmosphere ?point ?time) ?quantity)
   ))

(defoperator pressure-at-open-to-atmosphere (?point ?time)
  :preconditions (
     (open-to-atmosphere ?point ?time) 
     (variable  ?Pr-var (pressure ?point :time ?time))
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
  :short-name "pressure in fluid"
  :nlg-english ("the formula for pressure at a height in a fluid")
  :ExpFormat ("finding the pressure at a level in a fluid")
  :EqnFormat ("P2 - P1 = &rho; g (h1-h2)")) 

(defoperator pressure-height-fluid-contains (?sought)
   :preconditions (
     ; problem givens must specify which fluid point is in:
     (in-wm (in-fluid ?point-bottom ?material ?point-top ?time))
     (bind ?fluid-at-top  (format-sym "FLUID_AT_~a" ?point-top))
     (bind ?fluid-at-bottom  (format-sym "FLUID_AT_~a" ?point-bottom))
     (any-member ?sought ( (pressure ?point-bottom :time ?time)
                           (height ?fluid-at-top :time ?time)
			   (height ?fluid-at-bottom :time ?time)
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
       (variable  ?Pr2 (pressure ?point-bottom :time ?time))
       (variable  ?Pr1 (pressure ?point-top :time ?time))
       (variable  ?rho (mass-density ?material))
       (in-wm (near-planet ?planet))
       (variable  ?g   (gravitational-acceleration ?planet))
       (inherit-variable ?h1 (height ?fluid-at-top :time ?time))
       (inherit-variable ?h2 (height ?fluid-at-bottom :time ?time))
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
  :short-name "Bernoulli equation"
  :nlg-english ("Bernoulli's principle")
  :ExpFormat ("applying Bernoulli's principle")
  :EqnFormat ("0.5 &rho; v1<sup>2</sup> + &rho; g h1 + P1 = 0.5 &rho; v2<sup>2</sup> + &rho; g h2 + P2")) 

(defoperator bernoulli-contains (?sought)
   :preconditions (
     ; problem givens must specify that bernoulli's applies:
     (in-wm (bernoulli ?point1 ?fluid ?point2))
     (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
     (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
     (any-member ?sought ( (pressure ?point1 :time ?time)
                           (pressure ?point2 :time ?time)
                           (height ?fluid-at-p1 :time ?time)
			   (height ?fluid-at-p2 :time ?time)
			   (mass-density ?fluid)
                           (mag (velocity ?fluid-at-p1 :time ?time))
                           (mag (velocity ?fluid-at-p2 :time ?time))
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
      (variable  ?Pr1 (pressure ?point1 :time ?time))
      (variable  ?Pr2 (pressure ?point2 :time ?time))
       (variable  ?rho (mass-density ?fluid))
       (in-wm (near-planet ?planet))
       (variable  ?g   (gravitational-acceleration ?planet))
       (inherit-variable ?h2 (height ?fluid-at-p2 :time ?time))
       (inherit-variable ?h1 (height ?fluid-at-p1 :time ?time))
       (variable  ?v1  (mag (velocity ?fluid-at-p1 :time ?time)))
       (variable  ?v2  (mag (velocity ?fluid-at-p2 :time ?time)))
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
  :short-name "equation of continuity"
  :nlg-english ("the equation of continuity for incompressible fluid")
  :ExpFormat ("applying the equation of continuity")
  :EqnFormat ("A1 v1 = A2 v2")) 

(defoperator continuity-contains (?sought)
   :preconditions (
     ; problem givens must specify that bernoulli's applies:
     (in-wm (bernoulli ?point1 ?fluid ?point2))
     (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
     (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
     (any-member ?sought ( 
                           (mag (velocity ?fluid-at-p1 :time ?time))
                           (mag (velocity ?fluid-at-p2 :time ?time))
                           (area-at ?point1 :time ?time)
                           (area-at ?point2 :time ?time)
                         ))
   )
   :effects (
     (eqn-contains (equation-of-continuity ?point1 ?point2 ?time) ?sought)
   ))

(defoperator continuity (?point1 ?point2 ?time)
   :preconditions (
       (bind ?fluid-at-p1  (format-sym "FLUID_AT_~a" ?point1))
       (bind ?fluid-at-p2  (format-sym "FLUID_AT_~a" ?point2))
       (variable  ?v1  (mag (velocity ?fluid-at-p1 :time ?time)))
       (variable  ?v2  (mag (velocity ?fluid-at-p2 :time ?time)))
       (variable ?A1 (area-at ?point1 :time ?time))
       (variable ?A2 (area-at ?point2 :time ?time))
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
(def-qexp area-at (area-at ?position :time ?time)
  :symbol-base |A|     
  :short-name "cross-sectional area"	
     :units |m^2|
     :restrictions positive
     :nlg-english ("the cross-sectional area at ~A" (nlg ?position 'at-time ?time))
)

(defoperator define-area-at (?point ?time)
     :preconditions((bind ?A-var (format-sym "Ac_~A" (body-name ?point))))
     :effects ((variable ?A-var (area-at ?point :time ?time))
               (define-var (area-at ?point :time ?time)))
     :hint (
          (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Area."  
			      ((area-at ?point :time ?time) def-np)))))


;;;
;;; Pressure forces: 
;;;

;;; Problems involving pressure force require a statement of the form
;;;   (exerts-pressure ?body ?surface ?fluid ?point ?direction :type ?type 
;;;                    :time ?time)
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
   :preconditions 
   ((time ?t)
    (in-wm (exerts-pressure ?body ?surface ?fluid ?point ?dir 
			    :type ?type :time ?t-pressure))
    (test (tinsidep ?t ?t-pressure))) 
  :effects ( (force ?body ?fluid pressure ?t ?dir action) ))

;; The hints need to be generalized to handle radiation pressure.
(defoperator draw-pressure (?b ?fluid ?t)
  :preconditions
   ((force ?b ?fluid pressure ?t ?dir action)
    (test (not (eq ?dir 'unknown)))
    ;; need to get body containing surface for vector statement
    (in-wm (exerts-pressure ?b ?surface ?fluid ?point ?dir 
			    :type ?type :time ?t-pressure))
    (not (vector ?b (force ?b ?fluid pressure :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fp_~A_~A_~A" (body-name ?b) ?fluid
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (bind ?hint-point (if (eq ?type 'radiation)
		     "A force is exerted on ~A by ~A" 
		   "Notice that ~a is in contact with ~A."))
    (bind ?hint-teach (if (eq ?type 'radiation)
		 "When radiation is absorbed by (or is reflected off) a body, it exerts a pressure force on that body which is proportional to the intensity of the radiation."
		 "When a body is in contact with a fluid, the fluid exerts a pressure force on it, acting perpendicular to the surface of contact."))
    (debug "~&Drawing ~a pressure on ~a due to ~a at ~a.~%" ?dir ?b ?fluid ?t)
    )
  :effects
   ((vector ?b (force ?b ?fluid pressure :time ?t) ?dir)
    (variable ?mag-var (mag (force ?b ?fluid pressure :time ?t)))
    (variable ?dir-var (dir (force ?b ?fluid pressure :time ?t)))
    (given (dir (force ?b ?fluid pressure :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (force ?b ?fluid pressure :time ?t))))
  :hint
   ((point (string ?hint-point ?b (?fluid agent)))
    (teach (string ?hint-teach))
    (bottom-out (string "Because ~a exerts a force on ~a, draw a pressure force on ~a due to ~a at an angle of ~a." 
			(?fluid agent) ?surface ?b (?fluid agent) ?dir))
    ))

(defoperator draw-pressure-unknown (?b ?fluid ?t)
  :preconditions
   ((force ?b ?fluid pressure ?t unknown action)
    ;; need to get body containing surface for vector statement
    (in-wm (exerts-pressure ?b ?surface ?fluid ?point unknown 
			    :type ?type :time ?t-pressure))
    (not (vector ?b (force ?b ?fluid pressure :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fp_~A_~A_~A" (body-name ?b) ?fluid
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?hint-point (if (eq ?type 'radiation)
		     "A force is exerted on ~A by ~A" 
		   "Notice that ~a is in contact with ~A."))
    (bind ?hint-teach (if (eq ?type 'radiation)
		 "When radiation is absorbed by (or is reflected off) a body, it exerts a pressure force on that body which is proportional to the intensity of the radiation."
		 "When a body is in contact with a fluid, the fluid exerts a pressure force on it, acting perpendicular to the surface of contact."))
    )
  :effects
   ((vector ?b (force ?b ?fluid pressure :time ?t) unknown)
    (variable ?mag-var (mag (force ?b ?fluid pressure :time ?t)))
    (variable ?dir-var (dir (force ?b ?fluid pressure :time ?t))))
  :hint
   ((point (string ?hint-point ?b (?fluid agent)))
    (teach (string ?hint-teach))
    (bottom-out (string "Because ~a exerts a pressure against ~a, draw a pressure force on ~a due to ~a acting at an unknown angle." 
			(?fluid agent) ?b ?b (?fluid agent)))
    ))

;;;
;;; Scalar equation for pressure force magnitude:
;;;    Fp = P*A (definition of pressure)
;;;
(def-psmclass pressure-force (pressure-force ?body ?time ?fluid)
  :complexity major  
  :short-name "pressure defined"
  :nlg-english ("the definition of pressure")
  :ExpFormat ("finding the pressure")
  :EqnFormat ("Fp = P A"))

(defoperator pressure-force-contains (?sought)
  :preconditions(
    (any-member ?sought ( (mag (force ?body ?fluid pressure :time ?time))
                          (pressure ?point :time ?time)
			  (area ?surface)  ))
    (time ?time) ;because area is timeless
    (in-wm (exerts-pressure ?body ?surface ?fluid ?point ?dir 
			    :type ?type :time ?t-pressure))
    (test (tinsidep ?time ?t-pressure))
  )
  :effects (
    (eqn-contains (pressure-force ?body ?time ?fluid) ?sought)
  ))

(defoperator pressure-force (?body ?t ?fluid)
  :preconditions(
    (in-wm (exerts-pressure ?body ?surface ?fluid ?point ?dir 
			    :type ?type :time ?t-pressure))
    (test (tinsidep ?t ?t-pressure))
    (body ?body)
    (variable ?Fp (mag (force ?body ?fluid pressure :time ?t)))
    (variable ?P  (pressure ?point :time ?t))
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
; of exerts-pressure form used for pressure forces.
;
(defoperator find-buoyant-force (?body ?fluid ?t)
   :preconditions (
    (in-wm (buoyant-force ?body ?fluid ?t-buoyant ?dir))
    (time ?t)
    (test (tinsidep ?t ?t-buoyant))
  ) 
  :effects (
    (force ?body ?fluid buoyant ?t ?dir action)
  ))

(defoperator draw-buoyant-force (?b ?fluid ?t)
  :preconditions
   ((force ?b ?fluid buoyant ?t ?dir action)
    (not (vector ?b (force ?b ?fluid buoyant :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fb_~A_~A_~A" (body-name ?b) ?fluid
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (debug "~&Drawing ~a buoyant force on ~a due to ~a at ~a.~%" 
	   ?dir ?b ?fluid ?t)
    )
  :effects
   ((vector ?b (force ?b ?fluid buoyant :time ?t) ?dir)
    (variable ?mag-var (mag (force ?b ?fluid buoyant :time ?t)))
    (variable ?dir-var (dir (force ?b ?fluid buoyant :time ?t)))
    (given (dir (force ?b ?fluid buoyant :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (force ?b ?fluid buoyant :time ?t))))
  :hint
   ((point (string "Notice that ~a is submerged in ~A." ?b (?fluid agent)))
    (teach (string "When a body is submerged in a fluid, the upward fluid pressure on its bottom is greater than the downward pressure on its top. The net effect can be represented by an upward buoyant force on the object."))
    (bottom-out (string "Because ~a is submerged in ~a, draw a buoyant force on ~a due to ~a at an angle of ~a." 
			?b (?fluid agent) ?b (?fluid agent) ?dir))
    ))

;;Quantity: The volume of a body
(def-qexp volume (volume ?body :time ?time)
  :symbol-base |V|     
  :short-name "volume"	
     :units |m^3|
     :restrictions nonnegative ; we allow zero-volume for negligible parts of compound bodies
     :nlg-english ("the volume of ~A" (nlg ?body 'at-time ?time))
   )

(defoperator define-volume (?body ?time)
     :preconditions((bind ?Vol-var (format-sym "Vol_~A~@[_~A~]" 
					       (body-name ?body) 
					       (time-abbrev ?time))))
     :effects ((variable ?Vol-var (volume ?body :time ?time))
               (define-var (volume ?body :time ?time)))
     :hint (
          (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Volume."  
			      ((volume ?body :time ?time) def-np)))
	   ))

;; TODO: add equations for volumes of various shapes


;; Volume of compound body is sum of volumes of its parts. 
;; This parallels mass-compound in Newtons2.cl

(def-psmclass volume-compound (volume-compound ?compound ?t) 
  :complexity connect
  :short-name "volume of compound"
  :nlg-english ("volume of a compound body is sum of volumes of parts")
  :expformat ((strcat "using the fact that the volume of ~a "
		      "is the sum of the volumes of its parts") 
	      (nlg ?compound))
  :EqnFormat ("V = V1 + V2 + ..."))


(defoperator volume-compound-contains (?b-sought ?t)
  :preconditions (
   ; compound must exist
   (object (compound orderless . ?bodies))
   ; applies if sought is volume of compound or one of its parts
   (test (or (member ?b-sought ?bodies)
             (equal ?b-sought `(compound orderless ,@?bodies))))
  )
  :effects (
    (eqn-contains (volume-compound (compound orderless . ?bodies) ?t) 
		  (volume ?b-sought :time ?t))
  ))

(defoperator write-volume-compound (?bodies ?t)
  :preconditions (
    (variable ?Vwhole-var (volume ?compound :time ?t))
    (bind ?bodies (cddr ?compound))
    (map ?body ?bodies
         (variable ?Vpart-var (volume ?body :time ?t)) 
	 ?Vpart-var ?Vpart-vars) 
  )
  :effects (
     (eqn (= ?Vwhole-var (+ . ?Vpart-vars)) (volume-compound ?compound ?t))
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
  :short-name "Archimedes principle (buoyant force)"
  :nlg-english ("Archimedes' principle")
  :ExpFormat ("applying Archimedes' principle")
  :EqnFormat ("Fb = &rho;f V g"))

(defoperator archimedes-contains (?sought)
  :preconditions (
    (in-wm (buoyant-force ?body ?fluid ?t-buoyant ?dir))
    (any-member ?sought ( (mag (force ?b ?fluid ?buoyant :time ?t))
                          (mass-density ?fluid) 
                          (volume ?b :time ?t) ))
    (test (tinsidep ?t ?t-buoyant))
  )
  :effects (
    (eqn-contains (archimedes ?body ?fluid ?t) ?sought)
  ))

(defoperator write-archimedes (?b ?fluid ?t)
   :preconditions (
       (in-wm (near-planet ?planet))
       (variable ?Fb  (mag (force ?b ?fluid ?buoyant :time ?t)))
       (variable ?rho (mass-density ?fluid))
       (variable ?g   (gravitational-acceleration ?planet))
       (variable ?V   (volume ?b :time ?t))
   )
   :effects (
       (eqn (= ?Fb (* ?rho ?V ?g)) (archimedes ?b ?fluid ?t))
   )
   :hint (
       (point (string "Archimedes principle says that the magnitude of the buoyant force on an object submerged in a fluid is equal to the weight of the fluid displaced by the object."))
       (teach (string "The total mass of displaced fluid will be given by the fluid's mass density times the volume displaced. The weight of displaced fluid will be this total mass times g."))
       (bottom-out (string "Write the equation ~A" ((= ?Fb (* ?rho ?V ?g)) algebra) ))
   ))


