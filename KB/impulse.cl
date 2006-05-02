
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
    (debug "~&Drawing ~a impulse on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir) (dir (impulse ?b ?agent :time ?t)))
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
    )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
    ;; Ensure implicit eqn is written because dir is from force
    (implicit-eqn (= ?dir-var ?dir) (dir (impulse ?b ?agent :time ?t)))
   )
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
   (object ?b)
   (time ?t)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
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

;; This is the impulse from a particular force
(defoperator write-impulse-compo (?b ?agent ?t1 ?t2 ?xy ?rot)
  :preconditions 
  ((variable ?F12_x (compo ?xy ?rot (force ?b ?agent ?dont-care
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
   (motion ?b (straight ?dontcare1 ?dir1) :time ?t1)
   (test (not (eq ?dir1 'unknown)))	;known direction
   (motion ?b (straight ?dontcare2 ?dir2) :time ?t2)
   (test (not (eq ?dir2 'unknown)))	;known direction
   (test (same-angle ?dir2 (opposite ?dir1))) ;momenta in opposite directions
   (not (vector ?b (impulse ?b ?agent :time ) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects
   ((vector ?b (impulse ?b ?agent :time ?t) ?dir2)
    (variable ?mag-var (mag (impulse ?b ?agent :time ?t)))
    (variable ?dir-var (dir (impulse ?b ?agent :time ?t)))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir2) (dir (impulse ?b ?agent :time ?t)))
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
   (motion ?b (straight ?dontcare1 ?dir1) :time ?t1)
   (motion ?b (straight ?dontcare2 ?dir2) :time ?t2)
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
    (assume using-NSL impulse ?b ?t)
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
  :english ("Newton's Third Law applied to impulse")
  :ExpFormat ("applying Newton's Third Law to impulse between ~a and ~a ~a"
	      (nlg ?Object0) (nlg ?Object1) (nlg ?time 'nlg-time))
  :EqnFormat ("J12 = J21"))

(def-psmclass NTL-impulse-vector (?eq-type NTL-impulse ?axis ?rot 
					   (NTL-impulse-vector 
					    (?Object0 ?Object1) ?time))
  :complexity major
  :short-name ("Newton's third law (~A component)" (axis-name ?axis))
  :english ("Newton's Third Law applied to impulse")
  :ExpFormat ("applying Newton's Third Law to impulse between ~a and ~a ~a"
	      (nlg ?Object0) (nlg ?Object1) (nlg ?time 'nlg-time))
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
  ((point (string "What does Newton's Third Law tell you about the relation of ~A and ~A" (?mag1-var algebra) (?mag2-var algebra)))
  (teach (string "Newton's Third Law states that forces come in pairs: whenever A exerts a force of some type on B, B exerts a force of equal magnitude and opposite direction on A.  The same is true for impulse.  You can use that to equate the magnitudes of this pair of impulses."))
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
     (point (string "What does Newton's Third Law tell you about the relation of ~A and ~A" (?J12_xy algebra) (?J21_xy algebra)))
    (teach (string "Newton's Third Law states that the members of an action/reaction pair of forces are equal in magnitude and opposite in direction.  The same must be true for impulse.  This entails that the components of each impulse vector are the negations of the corresponding components of the other: J12_x = -J21_x and J12_y = -J21_y."))
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
