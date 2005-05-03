
;;;;===========================================================================
;;;;
;;;;                            Impulse
;;;;
;;;;===========================================================================


;; Impulse is specified in problem statement by given impulse direction 
;; which may be unknown
(def-qexp impulse (impulse ?body ?agent)
  :units |N.s|
  :english ("Impulse on ~A due to ~A" (nlg ?body) (nlg ?agent)))

;; Draw a "given" impulse at a certain direction. 
(defoperator draw-impulse-given-dir (?b ?agent ?t)
  :specifications 
  "if you are given that there is an impulse on an object at a time
   in a certain direction, then draw the impulse in that direction"
  :preconditions
   ((impulse ?b ?agent ?t ?dir)
    (test (not (equal ?dir 'unknown)))
    (not (vector ?b (at (impulse ?b ?agent) ?t) ?dont-care))
    (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a impulse on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "You were given that there is an impulse on ~a." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;;
;; Relation of impulse and force
;;
;; Following operators find average velocity as a vector PSM.
;; These are mainly for problems that test understanding of definition
;; of average velocity. Still it could be used to find components of
;; average velocity if we want.
;;
(def-psmclass impulse (?eqn-type impulse ?axis ?rot 
				 (impulse ?body ?agent ?time ?dir))
    ;; :group Dynamics  :BvdS:  what to choose?
    :complexity major    
    :Doc "Definition of impulse."
    :english ("the definition of impulse") 
    :ExpFormat ("applying the definition of impulse on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("J_~A = F(avg)_~a*t" (nlg ?axis 'adj) (nlg ?axis 'adj)))

;; Draw an impulse if associated force and interval known 
(defoperator draw-impulse-given-force (?b ?agent ?t)
  :specifications 
  "if you are given that there is an impulse on an object at a time
   in a certain direction, then draw the impulse in that direction"
  :preconditions
   ((impulse ?b ?agent ?t ?dir)
    (test (not (equal ?dir 'unknown)))
    (not (vector ?b (at (impulse ?b ?agent) ?t) ?dont-care))
    (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a impulse on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "You were given that there is an impulse on ~a." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

#|  ;old version?
(defoperator draw-impulse-diagram (?b ?t1 ?t2)
  :preconditions 
  ((not (vector-diagram (impulse ?b ?agent (during ?t1 ?t2) ?dir)))
   (body ?b)
   (vector ?b (at (displacement ?b) (during ?t1 ?t2)) ?dir2)
   (vector ?b (at (velocity ?b) (during ?t1 ?t2)) ?dir1)
   (axis-for ?b x ?rot))
  :effects 
  ((vector-diagram (impulse ?b ?agent (during ?t1 ?t2) ?dir))))
|#

(defoperator impulse-vector-contains (?sought)
  :preconditions 
  ((any-member ?sought
	       ((at (mag (impulse ?b ?agent)) ?t)
		(at (dir (impulse ?b ?agent)) ?t)
		(at (mag (force ?b ?agent ?type)) ?t)
		(at (dir (force ?b ?agent ?type)) ?t)
		(duration ?t)))
   (object ?b)
   (time ?t)
   (test (time-intervalp ?t)))
  :effects 
   ((vector-psm-contains (impulse ?b ?agent ?t) ?sought)
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (impulse ?b ?agent ?t) imp-force ?sought)))

;; This is the impulse from a particular force
(defoperator write-impulse-compo (?b ?agent ?t1 ?t2 ?xy ?rot)
  :preconditions 
   ((variable ?F12_x  (at (compo ?xy ?rot (force ?b ?agent ?dont-care)) 
			  (during ?t1 ?t2)))
    (variable ?J12_x  (at (compo ?xy ?rot (velocity ?b)) (during ?t1 ?t2)))
    (variable ?t12    (duration (during ?t1 ?t2))))
  :effects (
   (eqn (= ?J12_x (* ?F12_x ?t12))
            (compo-eqn imp-force ?xy ?rot 
		       (impulse ?b ?agent (?during ?t1 ?t2))))
   (eqn-compos (compo-eqn imp-force ?xy ?rot 
		       (impulse ?b ?agent (?during ?t1 ?t2)))
             (?J12_x ?F12_x)))
  :hint 
  ( (point (string "What is the relationship between average force, impulse and duration?"))
    (teach (string "The impulse vector is defined as the average force vector time the duration.  This can be applied component-wise to relate the components of average velocity to the components of displacement."))
    (bottom-out (string "Write the equation ~a"
			((= ?J12_x (* ?F12_x ?t12)) algebra)))
  ))

;;; ================== Impulse and velocity ================================== 
;;;
;;; This is just F=m*a integrated over time.  It is more natural
;;; to express this in terms of momentum, but current problems do
;;; not require that form (it would be an extra step).  This is
;;; based on the "NSL-compo" rules.
;;;

(def-psmclass impulse-velocity 
    (?eq-type ?compo-eqn-id ?axis ?rot 
	      (impulse ?body  ?agent (during ?t1 ?t2) ?dir)) 
     :complexity major
     :doc "equation relating impulse to change in velocity"
     :english ("Relation between impulse and change in velocity")
     :ExpFormat ("relating the impulse to the change in velocity of ~a"
		 (nlg ?body))
     :EqnFormat ("J1_~a + J2_~a + J3_~a + ...= m*v_~a(~A) - m*v_~a(~A)" 
                 (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj) 
		 (nlg ?axis 'adj) (nlg ?t2 'nlg-time) 
		 (nlg ?axis 'adj)(nlg ?t1 'nlg-time)))

;; Draw an impulse if two velocities are given and collinear 
(defoperator draw-impulse-given-velocities (?b ?agent ?t)
  :preconditions
   ((impulse ?b ?agent ?t ?dir)
    (test (not (equal ?dir 'unknown)))
    (not (vector ?b (at (impulse ?b ?agent) ?t) ?dont-care))
    (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a impulse on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "You were given that there is an impulse on ~a." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;; Draw an impulse if two velocities are given and not collinear 
(defoperator draw-impulse-given-velocities-direction-unknown (?b ?agent ?t)
  :preconditions
   ((impulse ?b ?agent ?t ?dir)
    (test (not (equal ?dir 'unknown)))
    (not (vector ?b (at (impulse ?b ?agent) ?t) ?dont-care))
    (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a impulse on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "You were given that there is an impulse on ~a." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;;; This operator indicates when the impulse form of NSL is
;;; applicable.  
;;;
(defoperator impulse-velocity-contains (?quantity)
  :preconditions 
  ((any-member ?quantity
	        ((at (mag (impulse ?b ?agent)) (during ?t1 ?t2))
		 (at (dir (impulse ?b ?agent)) (during ?t1 ?t2))
		 (mass ?b)
		 (at (mag (velocity ?b)) ?t1)
		 (at (dir (velocity ?b)) ?t1)
		 (at (mag (velocity ?b)) ?t2)
		 (at (dir (velocity ?b)) ?t2))
		)
   (object ?b)
   (time ?t1)
   (time ?t2)
   (time (during ?t1 ?t2))
   )
  :effects
  ((vector-psm-contains (impulse ?b ?agent (during ?t1 ?t2)) ?sought)
  ;; since only one compo-eqn under this vector psm, we can just
  ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (impulse ?b ?agent (during ?t1 ?t2)) imp-velocity ?quantity))
 )

;;; It expects to get the body, time, axis label (?xyz) and axis rotation
;;; (?rot) via the equation identifier in the effects, and it fetches
;;; the relevant vectors from wm.  It just looks up the appropriate
;;; variables and writes the equation.  It also leaves behind a
;;; proposition recording the component variables that appear in the
;;; equation.  

(defoperator write-impulse-velocity-compo (?b ?t ?xyz ?rot)
  :preconditions
  ((in-wm (impulses ?b ?t ?impulses))
   ;; for each impulse on b at t, define a component variable, 
   ;; collecting variable names into ?J-compo-vars
   (map ?J ?impulses 
    (variable ?J-compo-var (at (compo ?xyz ?rot ?J) ?t))
   	?J-compo-var ?J-compo-vars)
   (variable ?vf-compo (at (compo ?xyz ?rot (velocity ?b)) ?t2))
   (variable ?vi-compo (at (compo ?xyz ?rot (velocity ?b)) ?t1))
   ;; Add velocity components to list of variables.
   (bind ?eqn-compo-vars (cons ?vi-compo (cons ?vf-compo ?J-compo-vars)))
   (variable ?m (mass ?b)))
  :effects
   ((eqn (= (+ . ?J-compo-vars) (- (* ?m ?vf-compo) (* ?m ?vi-compo)))
	 (compo-eqn imp-velocity ?xyz ?rot 
		    (impulse ?b ?agent (during ?t1 ?t2))))
    (eqn-compos (compo-eqn imp-velocity ?xyz ?rot 
			   (impulse ?b ?agent (during ?t1 t2))) 
		?eqn-compo-vars))
  :hint
  ((point (string "You can relate the change in velocity of ~A to the
impulse ~A." (?b def-np) (?t pp)))
    (bottom-out (string "Write the equation using component variables along the ~A axis as ~A" ((axis ?xyz ?rot) symbols-label) ((= (+ . ?f-compo-vars) (* ?m ?a-compo)) algebra)))
    ))
