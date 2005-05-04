
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
  :preconditions
  (
   ;; BvdS:  why not this form for forces
   ;;(in-wm (at (dir (force ?b ?agent ?type)) ?t))
   (force ?b ?agent ?type ?t ?dir ?action)
   (test (intervalp ?t)) ;only impulse for intervals
   (test (not (equal ?dir 'unknown)))
   (not (vector ?b (at (impulse ?b ?agent) ?t) ?dont-care)) ;not already drawn
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-val (convert-dnum-to-number ?dir))
    )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is from force
    ;; BvdS:  why implicit?
    (implicit-eqn (= ?dir-var ?dir-val) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "There is a force acting on ~a." ?b))
    (teach (string "One can define an impulse associated with the force exerted by ~A ~A." ?agent (?t pp)))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))


(defoperator impulse-vector-contains (?sought)
  :preconditions 
  ((collision ?bodies ?t ?elastic-dont-care)
   (any-member ?sought
	       ((at (mag (impulse ?b ?agent)) ?t)
		(at (dir (impulse ?b ?agent)) ?t)
		(at (mag (force ?b ?agent ?type)) ?t)
		(at (dir (force ?b ?agent ?type)) ?t)
		(duration ?t)))
   (object ?b)
   (time ?t)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
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
;;; not require that form (it would be an extra step).  
;;; Generally, for these rules to apply, one must specify the 
;;; agent explicitly using (impulse-agent ?body ?agent)
;;;
;;;This is based on the "NSL-compo" rules.
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

;; Draw an impulse if two velocities are known to be opposite 
(defoperator draw-impulse-given-velocities (?b ?agent ?t1 ?t2)
  :preconditions
  (
   (collision ?bodies ?t ?elastic-dont-care)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   (motion ?b ?t1 (straight ?dontcare1 ?dir1))
   (test (not (equal ?dir1 'unknown)))	;known direction
   (motion ?b ?t2 (straight ?dontcare2 ?dir2))
   (test (not (equal ?dir2 'unknown)))	;known direction
   (test (equal ?dir2 (opposite ?dir1))) ;velocities in opposite directions
   (not (vector ?b (at (impulse ?b ?agent) ?t) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-val (convert-dnum-to-number ?dir2))
   )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir2)
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dirval) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "The impulse on ~a causes its motion to change." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;; Draw an impulse if two velocities are not known to be opposite
(defoperator draw-impulse-given-velocities-unknown-dir (?b ?agent ?t1 ?t2)
  :preconditions
  (
   (collision ?bodies ?t ?elastic-dont-care)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   (motion ?b ?t1 (straight ?dontcare1 ?dir1))
   (motion ?b ?t2 (straight ?dontcare2 ?dir2))
   (test (or (equal ?dir1 'unknown) (equal ?dir2 'unknown) ;unknown direction
	     (not (equal ?dir2 (opposite ?dir1))))) ;velocities not opposite 
   (not (vector ?b (at (impulse ?b ?agent) ?t) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-val (convert-dnum-to-number ?dir2))
   )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) unknown)
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir-val) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "The impulse on ~a causes its motion to change." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;;; This operator indicates when the impulse form of NSL is
;;; applicable.  
;;;
(defoperator impulse-velocity-contains (?quantity)
  :preconditions 
  (
   (collision ?bodies ?t ?elastic-dont-care)
   (any-member ?quantity
	        ((at (mag (impulse ?b ?agent)) (during ?t1 ?t2))
		 (at (dir (impulse ?b ?agent)) (during ?t1 ?t2))
		 (mass ?b)
		 (at (mag (velocity ?b)) ?t1)
		 (at (dir (velocity ?b)) ?t1)
		 (at (mag (velocity ?b)) ?t2)
		 (at (dir (velocity ?b)) ?t2))
		)
   (object ?b)
   (object ?agent)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
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
