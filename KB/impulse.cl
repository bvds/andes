
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
   ((given (at (dir (impulse ?b ?agent)) ?t) ?dir)
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

;;;
;;; Relation of impulse and force
;;;
;;;


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
   (test (time-intervalp ?t)) ;only impulse for intervals
   (test (not (equal ?dir 'unknown)))
   (not (vector ?b (at (impulse ?b ?agent) ?t) ?dont-care)) ;not already drawn
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) ?agent 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
    )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is from force
    (implicit-eqn (= ?dir-var ?dir) (at (dir (impulse ?b ?agent)) ?t))
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
   ((eqn-family-contains (impulse ?b ?agent ?t) ?sought)
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (impulse ?b ?agent ?t) imp-force ?sought)))

;; This is the impulse from a particular force
(defoperator write-impulse-compo (?b ?agent ?t1 ?t2 ?xy ?rot)
  :preconditions 
   ((variable ?F12_x  (at (compo ?xy ?rot (force ?b ?agent ?dont-care)) 
			  (during ?t1 ?t2)))
    (variable ?J12_x  (at (compo ?xy ?rot (impulse ?b ?agent)) 
			  (during ?t1 ?t2)))
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
    (teach (string "The impulse vector is defined as the average force vector time the duration.  This can be applied component-wise.."))
    (bottom-out (string "Write the equation ~a"
			((= ?J12_x (* ?F12_x ?t12)) algebra)))
  ))


;;; ================== Impulse and momentum ================================== 
;;;
;;; This is just F=m*a integrated over time.  
;;; The following is based on the "NSL-compo" rules.
;;;


(def-psmclass impulse-momentum 
    (?eq-type ?compo-eqn-id ?axis ?rot 
	      (impulse ?body  ?agent (during ?t1 ?t2) ?dir)) 
     :complexity major
     :doc "equation relating impulse to change in momentum"
     :english ("Relation between impulse and change in momentum")
     :ExpFormat ("relating the impulse to the change in momentum of ~a"
		 (nlg ?body))
     :EqnFormat ("J1_~a + J2_~a + J3_~a + ...= m*v_~a(~A) - m*v_~a(~A)" 
                 (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj) 
		 (nlg ?axis 'adj) (nlg ?t2 'nlg-time) 
		 (nlg ?axis 'adj)(nlg ?t1 'nlg-time)))

(defoperator draw-impulse-momentum-diagram (?b ?t1 ?t2)
  :preconditions 
  ( (body ?b)
    ;; ?dirv = ?dirm is set in drawing rules
    (vector ?b (at (impulse ?b ?agent) (during ?t1 ?t2)) ?dirj)
    (vector ?b (at (momentum ?b) ?t1) ?dirm1)
    (vector ?b (at (momentum ?b) ?t2) ?dirm2)
    (axis-for ?b ?xyz ?rot) ;maybe a problem for compounds?
  )
  :effects (
   (vector-diagram  (impulse ?b ?agent (during ?t1 ?t2)))
  ))


;; Draw an impulse if two momenta are known to be opposite 
(defoperator draw-impulse-given-momenta (?b ?agent ?t)
  :preconditions
  (
   (test (time-intervalp ?t))		;introduce ?t to save some typing
   (bind ?t1 (second ?t)) (bind ?t2 (third ?t)) ;get interval endpoints
   (collision ?bodies ?t ?elastic-dont-care)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   (motion ?b ?t1 (straight ?dontcare1 ?dir1))
   (test (not (equal ?dir1 'unknown)))	;known direction
   (motion ?b ?t2 (straight ?dontcare2 ?dir2))
   (test (not (equal ?dir2 'unknown)))	;known direction
   (test (equal ?dir2 (opposite ?dir1))) ;momenta in opposite directions
   (not (vector ?b (at (impulse ?b ?agent) ) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) ?dir2)
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir2) (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "The impulse on ~a causes its motion to change." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;; Draw an impulse if two momenta are not known to be equal or opposite
;; and direction of impulse is not given.
(defoperator draw-impulse-given-momenta-unknown-dir (?b ?agent ?t)
  :preconditions
  (
   (not (given (at (dir (impulse ?b ?agent)) ?t) ?dir))
   (test (time-intervalp ?t))		;introduce ?t to save some typing
   (bind ?t1 (second ?t)) (bind ?t2 (third ?t)) ;get interval endpoints
   (collision ?bodies ?t ?elastic-dont-care)
   (test (member ?b ?bodies :test #'equal)) 
   (test (member ?agent ?bodies :test #'equal)) 
   (motion ?b ?t1 (straight ?dontcare1 ?dir1))
   (motion ?b ?t2 (straight ?dontcare2 ?dir2))
   (test (or (equal ?dir1 'unknown) (equal ?dir2 'unknown) ;unknown direction
	     (not (equal ?dir2 (opposite ?dir1))))) ;momenta not opposite 
   (not (vector ?b (at (impulse ?b ?agent) ?t) ?dontcare3)) ;not already done 
   (bind ?mag-var (format-sym "J_~A_~A_~A" (body-name ?b) (body-name ?agent)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects
   ((vector ?b (at (impulse ?b ?agent) ?t) unknown)
    (variable ?mag-var (at (mag (impulse ?b ?agent)) ?t))
    ;; since it is unknown, no implicit-eqn
    (variable ?dir-var (at (dir (impulse ?b ?agent)) ?t))
   )
  :hint
   ((point (string "The impulse on ~a causes its motion to change." ?b))
    (bottom-out (string "Use the impulse drawing tool to draw the impulse on ~a due to ~a ~a at ~a." ?b ?agent (?t pp) ?dir))
    ))

;;; This operator indicates when the impulse form of NSL is
;;; applicable.  
;;;
(defoperator impulse-momentum-contains (?sought)
  :preconditions 
  (
   (collision ?bodies (during ?t1 ?t2) ?elastic-dont-care)
   (any-member ?sought
	        ((at (mag (impulse ?b ?agent)) (during ?t1 ?t2))
		 (at (dir (impulse ?b ?agent)) (during ?t1 ?t2))
		 (at (mag (momentum ?b)) ?t1)
		 (at (dir (momentum ?b)) ?t1)
		 (at (mag (momentum ?b)) ?t2)
		 (at (dir (momentum ?b)) ?t2))
		)
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
   (variable ?J-compo-var (at (compo ?xyz ?rot (impulse ?b ?agent)) ?t))
   (variable ?pf-compo (at (compo ?xyz ?rot (momentum ?b)) ?t2))
   (variable ?pi-compo (at (compo ?xyz ?rot (momentum ?b)) ?t1))
   ;; Add momentum components to list of variables.
   (bind ?eqn-compo-vars (list ?pi-compo ?pf-compo ?J-compo-var))
   )
  :effects
   ((eqn (= ?J-compo-var (- ?pf-compo ?pi-compo))
	 (compo-eqn imp-momentum ?xyz ?rot 
		    (impulse ?b ?agent ?t)))
    (eqn-compos (compo-eqn imp-momentum ?xyz ?rot 
			   (impulse ?b ?agent ?t)) 
		?eqn-compo-vars))
  :hint
  ((point (string "You can relate the change in momentum of ~A to the
impulse ~A." (?b def-np) (?t pp)))
    (bottom-out (string "Write the equation using component variables along the ~A axis as ~A" ((axis ?xyz ?rot) symbols-label) ((= ?f-compo-var (- ?pf-compo ?pi-compo)) algebra)))
    ))


;;; ================== Impulse and Impulse ================================== 
;;;
;;; This is just NTL integrated over time.  
;;; The following is based on the "NTL-compo" rules.
;;;


(def-psmclass NTL-impulse (NTL-impulse (?Object0 ?Object1) ?time)
  :complexity major
  :english ("Newton's Third Law applied to impulse")
  :ExpFormat ("applying Newton's Third Law to impulse between ~a and ~a ~a"
	      (nlg ?Object0) (nlg ?Object1) (nlg ?time 'nlg-time))
  :EqnFormat ("Jof1on2 = Jof2on1"))

(def-psmclass NTL-impulse-vector (?eq-type NTL-impulse ?axis ?rot 
					   (NTL-impulse-vector 
					    (?Object0 ?Object1) ?time))
  :complexity major
  :english ("Newton's Third Law applied to impulse")
  :ExpFormat ("applying Newton's Third Law to impulse between ~a and ~a ~a"
	      (nlg ?Object0) (nlg ?Object1) (nlg ?time 'nlg-time))
  :EqnFormat ("J12_~a = - J21_~a" (nlg ?axis 'adj) (nlg ?axis adj)))

(defoperator NTL-impulse-contains (?quantity)
  :preconditions 
  (
   (collision ?bodies ?t ?elastic-dont-care)
   (any-member ?quantity (
			  (at (mag (impulse ?b1 ?b2)) ?t)
                        ))
   (test (member ?b1 ?bodies :test #'equal)) 
   (test (member ?b2 ?bodies :test #'equal)) 
   (bind ?body-pair (sort (list ?b1 ?b2) #'expr<))
  )
  :effects ( 
  	(eqn-contains (NTL-impulse ?body-pair ?t) ?quantity) 
  ))

(defoperator NTL-impulse (?b1 ?b2 ?t)
  :preconditions (
  (variable ?mag1-var (at (mag (impulse ?b1 ?b2)) ?t))
  (variable ?mag2-var (at (mag (impulse ?b2 ?b1)) ?t))
  )
  :effects (
    	(eqn (= ?mag1-var ?mag2-var) (NTL-impulse (?b2 ?b1) ?t)) 
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
   (any-member ?sought ( (at (mag(impulse ?b1 ?b2)) ?t)
  		         (at (dir(impulse ?b1 ?b2)) ?t) ))
   (bind ?body-pair (sort (list ?b1 ?b2) #'expr<))
   )
   :effects (
   (eqn-family-contains (NTL-impulse-vector ?body-pair ?t) ?sought) 
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (NTL-impulse-vector ?body-pair ?t) NTL-impulse ?sought)
   ))

(defoperator draw-NTL-impulse-vector-diagram (?bodies ?t)
  :preconditions (
    ;; Draw both bodies. 
    (body ?b1)
    (body ?b2)
    (vector ?b1 (at (impulse ?b1 ?b2) ?t) ?dir1)
    (vector ?b2 (at (impulse ?b2 ?b1) ?t) ?dir2)
    ;; we need axis-for each body, since component defining operators will 
    ;; lookup axis-for principal body of each vector. Our operators that
    ;; draw axes only apply once, so there is no danger of drawing two
    ;; axes. In order to reuse the axes drawn for body1 as axes used
    ;; for vectors on body2, we added reuse-other-body-axis in axes section.
    (axis-for ?b1 x ?x-rot)
    (axis-for ?b2 y ?y-rot)
    (bind ?bodies (sort (list ?b1 ?b2)  #'expr<))
    )
  :effects (
	    (vector-diagram (NTL-impulse-vector ?bodies ?t))
  ))
  
(defoperator write-NTL-impulse-vector (?b1 ?b2 ?t ?xy ?rot)
   :preconditions (
      (variable ?J12_xy (at (compo ?xy ?rot (impulse ?b1 ?b2)) ?t))
      (variable ?J21_xy (at (compo ?xy ?rot (impulse ?b2 ?b1)) ?t))
   )
   :effects (
    (eqn (= ?J12_xy (- ?J21_xy)) (compo-eqn NTL-impulse ?xy ?rot (NTL-impulse-vector (?b1 ?b2) ?t)))
    (eqn-compos (compo-eqn NTL-impulse ?xy ?rot (NTL-impulse-vector (?b1 ?b2) ?t))
          (?J12_xy ?J21_xy))
   )
   :hint (
     ;; !!! TODO
     (point (string "What does Newton's Third Law tell you about the relation of ~A and ~A" (?J12_xy algebra) (?J21_xy algebra)))
    (teach (string "Newton's Third Law states that the members of an action/reaction pair of forces are equal in magnitude and opposite in direction.  The same must be true for impulse.  This entails that the components of each impulse vector are the negations of the corresponding components of the other: J12_x = -J21_x and J12_y = -J21_y."))
     (bottom-out (string "Write the equation ~A" 
                         ((= ?J12_xy (- ?J21_xy)) algebra)))
   ))

