;;;
;;; This file now only deals with relative velocity addition problems, 
;;; included in our vector set as an application of vector addition.
;;;

;;; Relative velocity relation V13 = V12 + V23
;;;
;;; In our simple relvel* problems, the velocity triples to which it applies 
;;; are identified by the following statement in the givens:
;;;   (relvel-triangle body1 body2 body3)
;;; body1 should be the moving object of interest.
;;;
;;; These is done to tell us exactly which system of relative velocities we 
;;; can relate. Otherwise, whenever the sought were a relative velocity Vab, 
;;; we would have to try many possibilities: for each possible third 
;;; object c we could choose, we would have three candidate equations, 
;;; one with Vab in each of the three possible roles. Most of
;;; these would turn out to be blind alleys. 
;;; It simplifies things just to tell us which we should use. 

;;; Note: need var named ?body* for nsh to recognize this arg as 
;;; a principle's important body (to prompt to draw first).

(def-psmclass relative-vel (?eq-type relvel ?xy ?rot (relative-vel ?body1 ?b2 ?b3 ?t))
  :complexity major
  :english ("relative velocity equation")
  :expformat ((strcat "applying the relative velocity equation to ~a "
                      "in relation to ~a and ~a")
              (nlg ?body1) (nlg ?b2) (nlg ?b3))
  :EqnFormat ("Vac_~a = Vab_~a + Vbc_~a" 
             (nlg ?axis 'adj)(nlg ?axis 'adj) (nlg ?axis 'adj)))

(defoperator relative-vel-contains (?sought)
  :preconditions (
		  (in-wm (relvel-triangle ?b1 ?b2 ?b3))
		  (any-member ?sought ((at (mag (relative-vel ?b1 ?b3)) ?t)
				       (at (dir (relative-vel ?b1 ?b3)) ?t)
				       (at (mag (relative-vel ?b1 ?b2)) ?t)
				       (at (dir (relative-vel ?b1 ?b2)) ?t)
				       (at (mag (relative-vel ?b2 ?b3)) ?t)
				       (at (dir (relative-vel ?b2 ?b3)) ?t)))
		  )
  :effects (
	    (vector-psm-contains (relative-vel ?b1 ?b2 ?b3 ?t) ?sought)
	    (compo-eqn-contains (relative-vel ?b1 ?b2 ?b3 ?t) relvel ?sought)))


;;; following attaches a hint to the subgoal of drawing this diagram,
;;; see ontology.cl for examples
(def-goalprop rel-vel-fbd (vector-diagram (relative-vel ?b1 ?b2 ?b3 ?t))
   :english ("drawing a diagram showing all of the needed relative velocity vectors and coordinate axes" ))
   
(defoperator draw-rel-vel-diagram (?b1 ?t)
  :preconditions (
		  (rdebug "Using draw-rel-vel-diagram-no-axes-problem ~%")
		  (not (vector-diagram (relative-vel ?b1 ?b2 ?t)))
		  (body ?b1)		; choose b1 as our body to draw:
		  (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir1)    
		  (vector ?b1 (at (relative-vel ?b1 ?b3) ?t) ?dir3)   
		  (vector ?dontcare (at (relative-vel ?b2 ?b3) ?t) ?dir2)
		  (axis-for ?b1 x ?rot)
		  (rdebug "Fired draw-rel-vel-diagram-no-axes-problem ~%")
		  )
  :effects (
	    (vector-diagram (relative-vel ?b1 ?b2 ?b3 ?t))
	    ))

(defoperator draw-relative-vel-given-dir (?b1 ?b2 ?t)
  :specifications "If the relative velocity vector of a body wrt to
                   something else is needed & the direction is given, 
                   then draw it at the given direction"
  :preconditions ((rdebug "Using draw-rel-vel-vector-given-dir ~%")
		  ;; this means sub-intervals must be given explicitly
		  (given (at (dir(relative-vel ?b1 ?b2)) ?t) ?dir)
		  (not (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir))
		  (bind ?mag-var (format-sym "V_~A_~A_~A" 
					     (body-name ?b1) 
					     (body-name ?b2) 
					     (time-abbrev ?t)))
		  (bind ?dir-var (format-sym "O~A" ?mag-var))
		  (rdebug "fired draw-rel-vel-vector-given-dir  ~%")
		  )
  :effects (
	    (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir)
	    (variable ?mag-var (at (mag (relative-vel ?b1 ?b2)) ?t))
	    (variable ?dir-var (at (dir (relative-vel ?b1 ?b2)) ?t))
	    ;; Because dir is problem given, find-by-psm won't ensure 
	    ;; implicit eqn gets written.  Given value may not be used 
	    ;; elsewhere so ensure it here.
	    (implicit-eqn (= ?dir-var ?dir) (at (dir (relative-vel ?b1 ?b2)) ?t))
	    )
  :hint (
	 (point (string "The problem gives you the direction of the relative velocity of ~a with respect to ~a ~a." ?b1 ?b2 (?t pp)))
	 (bottom-out (string "The problem specifies that the relative velocity of ~a with respect to ~a ~a is at ~a, so use the velocity tool to draw that vector at ~a." ?b1 ?b2 (?t pp) ?dir ?dir))
	 ))


(defoperator draw-rel-vel-vector-unknown (?b1 ?b2 ?t)
  :specifications "If the relative velocity vector of a body wrt to
                   something else is needed & the direction is not given, 
                   then draw it with an unknown direction"
  :preconditions (
		  (rdebug "Using draw-rel-vel-vector-unknown ~%")
		  (not (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dontcare1))
		  (not (given (at (dir (relative-vel ?b1 ?b2)) ?t) ?dontcare))
		  (not (given (at (mag (relative-vel ?b1 ?b2)) ?t) (dnum 0 ?units)))
		  (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1) (body-name ?b2)(time-abbrev ?t)))
		  (bind ?dir-var (format-sym "O~A" ?mag-var))
		  (rdebug "fired draw-rel-vel-vector-unknown  ~%")
		  )
  :effects (
	    (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) unknown)
	    (variable ?mag-var (at (mag (relative-vel ?b1 ?b2)) ?t))
	    (variable ?dir-var (at (dir (relative-vel ?b1 ?b2)) ?t))
	    )
  :hint (
	 (point (string "You need to introduce a term for the relative velocity of ~A with respect to ~A ~A" ?b1 ?b2 (?t pp)))
	 (teach (string "If a vector angle requires calculation to determine, you should draw the vector at an unspecified angle. You do this by drawing the vector making an approximation to the correct angle, then erasing the number in the direction slot of the subsequent dialog box to indicate that the exact angle is unspecified."))
	 (bottom-out (string "Use the velocity tool to draw the relative velocity of ~a with respect to ~a ~A at an approximately correct angle, then erase the number in the direction slot to indicate that the exact direction is not specified."
			     ?b1 ?b2 (?t pp)))
	 ))


(defoperator write-relative-vel-compo (?b1 ?b2 ?b3 ?t ?xy ?rot)
  :features (unordered)
  :preconditions ((rdebug "Using write-relative-vel-compo ~%")
		  (variable ?v12  (at (compo ?xy ?rot (relative-vel ?b1 ?b2)) ?t))
		  (variable ?v23  (at (compo ?xy ?rot (relative-vel ?b2 ?b3)) ?t))
		  (variable ?v13  (at (compo ?xy ?rot (relative-vel ?b1 ?b3)) ?t))
		  (rdebug "fired write-relative-vel-compo  ~%")
                  )
  :effects (
            (eqn (= ?v13 (+ ?v12 ?v23))
                 (compo-eqn relvel ?xy ?rot (relative-vel ?b1 ?b2 ?b3 ?t)))
            (eqn-compos (compo-eqn relvel ?xy ?rot (relative-vel ?b1 ?b2 ?b3 ?t))
                        (?v12 ?v23 ?v13))
            )
  :hint (
	 (point (string 
		 "Can you relate the following 3 vectors: the relative velocity of ~A wrt ~A, the relative velocity of ~a wrt ~a, and the relative velocity of ~a wrt ~A?"
		 ?b1 ?b3  ?b1 ?b2  ?b2 ?b3))
	 (teach (string "The relative velocity of a wrt c is equal to the vector sum of the relative velocity of a wrt b plus the relative velocity of b wrt c. In terms of components:\\n   Vac_x = Vab_x + Vbc_x\\n   Vac_y = Vab_y + Vbc_y."))
	 (bottom-out (string "Write the equation ~A"
			     ((= ?v13 (+ ?v12 ?v23)) algebra)))
	 ))

;;; This is from draw-zero-displacement.
(defoperator draw-zero-relative-vel (?b1 ?b2 ?t)
  :preconditions
  ;; this means sub-intervals must be given explicitly
  ((in-wm (given (at (mag(relative-vel ?b1 ?b2)) ?t) (dnum 0 ?units)))
   (not (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dontcare))
   (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1)
			      (body-name ?b2) (time-abbrev ?t)))
   )
  :effects
  ((vector ?b1 (at (relative-vel ?b1 ?b2) ?t) zero)
   (variable ?mag-var (at (mag (relative-vel ?b1 ?b2)) ?t))
   ;; Because mag is problem given, find-by-psm won't ensure 
   ;; implicit eqn gets written.  Given value may not be used 
   ;; elsewhere so ensure it here.
   ;; see draw-rel-vel-vector-given-dir
   (implicit-eqn (= ?mag-var (dnum 0 ?units)) 
   		 (at (mag (relative-vel ?b1 ?b2)) ?t))
   )
  :hint
  ((bottom-out (string "Since the problem specifies that the velocity of ~a relative to ~A is zero, just draw a zero-length vector for it." ?b1 ?b2))
   ))

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

;; BvdS:  Why doesn't displacement have "find-displacement" ?
(defoperator find-impulse (?b ?agent ?t)
  :preconditions (
    (object ?b)
    (time ?t)
    (test (time-intervalp ?t))
    (in-wm (given (at (dir (impulse ?b ?agent)) ?t) ?dir))
  )
  :effects (
    (impulse ?b ?agent ?t ?dir)
  ))

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
    :group Dynamics
    :complexity major    
    :Doc "Definition of impulse."
    :english ("the definition of impulse") 
    :ExpFormat ("applying the definition of impulse on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("J_~A = F(avg)_~a*t" (nlg ?axis 'adj) (nlg ?axis 'adj)))


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
    (test (time-intervalp ?t))
  :effects 
  ((vector-psm-contains (impulse ?b ?agent ?t ?dir) ?sought)
  ; since only one compo-eqn under this vector psm, we can just
  ; select it now, rather than requiring further operators to do so
  (compo-eqn-contains (impulse ?b ?agent ?t ?dir) 
		      impulse ?sought)))

(defoperator draw-impulse-diagram (?b ?t1 ?t2)
  :preconditions 
  ((not (vector-diagram (impulse ?b ?agent (during ?t1 ?t2) ?dir)))
   (body ?b)
   (vector ?b (at (displacement ?b) (during ?t1 ?t2)) ?dir2)
   (vector ?b (at (velocity ?b) (during ?t1 ?t2)) ?dir1)
   (axis-for ?b x ?rot))
  :effects 
  ((vector-diagram (impulse ?b ?agent (during ?t1 ?t2) ?dir))))

(defoperator write-impulse-compo (?b ?t1 ?t2 ?xy ?rot)
  :preconditions 
   ((variable ?F12_x  (at (compo ?xy ?rot (displacement ?b)) (during ?t1 ?t2)))
    (variable ?J12_x  (at (compo ?xy ?rot (velocity ?b)) (during ?t1 ?t2)))
    (variable ?t12    (duration (during ?t1 ?t2))))
  :effects (
   (eqn (= ?J12_x (* ?F12_x ?t12))
            (compo-eqn impulse ?xy ?rot (impulse ?b ?agent (?during ?t1 ?t2) ?dir)))
   (eqn-compos 
            (compo-eqn impulse ?xy ?rot (impulse ?b ?agent (?during ?t1 ?t2) ?dir))
             (?J12_x ?F12_x)))
  :hint (
   (point (string "What is the relationship between average force, impulse and duration?"))
    (teach (kcd "write_average_velocity_eqn")
	   (string "The impulse vector is defined as the average force vector time the duration.  This can be applied component-wise to relate the components of average velocity to the components of displacement."))
    (bottom-out (string "Write the equation ~a"
			((= ?J12_x (* ?F12_x ?t12)) algebra)))
  ))
