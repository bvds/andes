
;; following attaches a hint to the subgoal of drawing this diagram, 
;; see ontology.cl for examples
(def-goalprop avg-vel-fbd (vector-diagram (relative-vel ?b1 ?b2 ?b3 ?t))
   :english ("drawing a diagram showing all of the needed relative velocity vectors and coordinate axes" ))
   
(defoperator draw-rel-vel-diagram (?b1 ?t)
  :features (unordered)
             :preconditions (
                             (rdebug "Using draw-rel-vel-diagram ~%")
                             (not (vector-diagram (relative-vel ?b1 ?b2 ?t)))
                             (body ?b1)
                             (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir1)    
                             (vector ?b1 (at (relative-vel ?b1 ?b3) ?t) ?dir3)   
                             (reference-object ?b3)
                             (test (not (equal ?b2 ?b3)))
                             (vector ?dontcare (at (relative-vel ?b2 ?b3) ?t) ?dir2)
                             (axis-for ?b1 ?t x ?rot)
                             (register-axes ?b1 ?t)
                             (register-axes ?b2 ?t)
                             (rdebug "Fired draw-rel-vel-diagram ~%")
                            )
             :effects (
                       (vector-diagram (relative-vel ?b1 ?b2 ?b3 ?t))
                       ))

(defoperator draw-rel-vel-diagram-no-axes-problem (?b1 ?t)
  :features (unordered)
             :preconditions (
                             (rdebug "Using draw-rel-vel-diagram-no-axes-problem ~%")
                             (not (vector-diagram (relative-vel ?b1 ?b2 ?t)))
                             (body ?b1)
                             (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir1)    
                             (vector ?b1 (at (relative-vel ?b1 ?b3) ?t) ?dir3)   
                             (reference-object ?b3)
                             (test (not (equal ?b2 ?b3)))
                             (vector ?dontcare (at (relative-vel ?b2 ?b3) ?t) ?dir2)
                             (axis-for ?b1 ?t x ?rot)
                             (rdebug "Fired draw-rel-vel-diagram-no-axes-problem ~%")
                            )
             :effects (
                       (vector-diagram (relative-vel ?b1 ?b2 ?b3 ?t))
                       ))


(defoperator draw-rel-vel-vector-unknown (?b1 ?b2 ?t)
             :specifications "If the relative velocity vector of a body wrt to
                              something else is sought, then draw it."
             :preconditions (
                             (rdebug "Using draw-rel-vel-vector-unknown ~%")
                             (time ?t)
                             (body ?b1)
                             (not (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dontcare1))
                             (not (given (at (dir (relative-vel ?b1 ?b2)) ?t) ?dontcare))
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

(defoperator draw-rel-vel-vector-unknown-no-body (?b1 ?b2 ?t)
             :specifications "If the relative velocity vector of a body wrt to
                              something else is sought, then draw it."
             :preconditions (
                             (rdebug "Using draw-rel-vel-vector-unknown-no-body ~%")
                             (time ?t)
                             (reference-object ?b2)
                             (body ?b3)
                             (not (vector ?b3 (at (relative-vel ?b1 ?b2) ?t) ?dontcare1))
                             (not (given (at (dir (relative-vel ?b1 ?b2)) ?t) ?dontcare))
                             (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1) (body-name ?b2)(time-abbrev ?t)))
                             (bind ?dir-var (format-sym "O~A" ?mag-var))
                             (rdebug "fired draw-rel-vel-vector-unknown-no-body  ~%")
                             )
             :effects (
                       (vector ?b3 (at (relative-vel ?b1 ?b2) ?t) unknown)
                       (variable ?mag-var (at (mag (relative-vel ?b1 ?b2)) ?t))
                       (variable ?dir-var (at (dir (relative-vel ?b1 ?b2)) ?t))
                        )
             :hint (
	     (point (string "You need to introduce a term for the relative velocity of ~A with respect to ~A ~A" ?b1 ?b2 (?t pp)))
             (teach (string "If a vector angle requires calculation to determine, you should draw the vector at an unspecified angle. You do this by drawing the vector making an approximation to the correct angle, then erasing the number in the direction slot of the subsequent dialog box to indicate that the exact angle is unspecified."))
    (bottom-out (string "Use the velocity tool to draw the relative velocity of ~a with respect to ~a ~A at an approximately correct angle, then erase the number in the direction slot to indicate that the exact direction is not specified."
			?b1 ?b2 (?t pp)))
		   ))


(defoperator draw-rel-vel-vector-given-dir (?b1 ?b2 ?t)
             :specifications "If the relative velocity vector of a body wrt to
                              something else is needed & the direction is known, 
                              then draw it."
             :preconditions ((rdebug "Using draw-rel-vel-vector-given-dir ~%")
                             (time ?t)
                             (body ?b1)
                             (given (at (dir(relative-vel ?b1 ?b2)) ?t1) ?dir)
                             (test (numberp (second ?dir)))
                             (not (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir))
                             (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1) (body-name ?b2)(time-abbrev ?t)))
                             (bind ?dir-var (format-sym "O~A" ?mag-var))
                             (rdebug "fired draw-rel-vel-vector-given-dir  ~%")
                             )
             :effects (
                       (vector ?b1 (at (relative-vel ?b1 ?b2) ?t) ?dir)
                       (variable ?mag-var (at (mag (relative-vel ?b1 ?b2)) ?t))
                       (variable ?dir-var (at (dir (relative-vel ?b1 ?b2)) ?t))
                       ;Because dir is problem given, find-by-psm won't ensure implicit eqn
                       ;gets written.  Given value may not be used elsewhere so ensure it here.
                       (implicit-eqn (= ?dir-var ?dir) (at (dir (relative-vel ?b1 ?b2)) ?t))
                       )
             :hint (
                  (point (string "The problem gives you the direction of the relative velocity of ~a with respect to ~a ~a." ?b1 ?b2 (?t pp)))
                  (bottom-out (string "The problem specifies that the relative velocity of ~a with respect to ~a ~a is at ~a, so use the velocity tool to draw that vector at ~a." ?b1 ?b2 (?t pp) ?dir ?dir))
	            ))

(defoperator draw-rel-vel-vector-given-dir-body-not-in-rel-vel (?b2 ?b3 ?t)
             :specifications "If the relative velocity vector of a x wrt to
                              y is needed and neither x or y are the body but
                              the direction is known, then draw it."

             :preconditions ((rdebug "Using draw-rel-vel-vector-given-dir-body-not-in-rel-vel ~%")
                             (time ?t)
                             (body ?b1)
                             (given (at (dir(relative-vel ?b2 ?b3)) ?t) ?dir)
                             (test (not (equal ?b2 ?b3))) 
                             (test (numberp (second ?dir)))                  
                             (not (vector ?b (at (relative-vel ?b2 ?b3) ?t) 
					  ?dir))
                             (bind ?mag-var (format-sym "V_~A_~A_~A" 
							(body-name ?b2) 
							(body-name ?b3)
							(time-abbrev ?t)))
                             (bind ?dir-var (format-sym "O~A" ?mag-var))
                             (rdebug "fired draw-rel-vel-vector-given-dir-body-not-in-rel-vel  ~%")
                             )
             :effects (
                       (vector ?b1 (at (relative-vel ?b2 ?b3) ?t) ?dir)
                       (variable ?mag-var (at (mag (relative-vel ?b2 ?b3)) ?t))
                       (variable ?dir-var (at (dir (relative-vel ?b2 ?b3)) ?t))
                       (implicit-eqn (= ?dir-var ?dir) (at (dir (relative-vel ?b2 ?b3)) ?t))
                        )
             :hint (
                  (point (string "The problem gives you the direction of the relative velocity of ~a with respect to ~a ~a." ?b2 ?b3 (?t pp)))
                  (bottom-out (string "The problem specifies that the relative velocity of ~a with respect to ~a ~a is at ~a, so use the velocity tool to draw that vector at ~a." ?b2 ?b3 (?t pp) ?dir ?dir))
	            ))

(defoperator relative-vel-contains-ref-obj-not-in-sought (?sought)
             :specifications "The reference object is not involved in the sought and
                             is the last body passed in the effects."
             :preconditions ((any-member ?sought((at (mag (relative-vel ?b1 ?b2)) ?t)
                                                 (at (dir (relative-vel ?b1 ?b2)) ?t)                                                                           
                                                 ))
                             (reference-object ?b3)
                             (rdebug "Using & firing relative-vel-contains-with-ref-obj  ~%")

                             )
             :effects (
                       (vector-psm-contains (relative-vel ?b1 ?b2 ?b3 ?t) ?sought)
                       (compo-eqn-contains (relative-vel ?b1 ?b2 ?b3 ?t) relvel ?sought)))

(defoperator relative-vel-contains-ref-obj-in-sought (?sought)
             :specifications "The reference object and the body is involved in the sought and
                             is the last body passed in the effects."
             :preconditions ((any-member ?sought((at (mag (relative-vel ?b1 ?b2)) ?t)
                                                 (at (dir (relative-vel ?b1 ?b2)) ?t)                                                                           
                                                 ))
                             (rdebug "Using relative-vel-contains-no-ref-obj  ~%")
                             (reference-object ?b2)
                             (object ?b1)
                             (given (at (mag (relative-vel ?b1 ?b3)) ?t) ?dontcare)
                             (test (not (equal ?b2 ?b3)))
                             (rdebug "Firing relative-vel-contains-no-ref-obj  ~%")
                             )
             :effects (
                       (vector-psm-contains (relative-vel ?b1 ?b3 ?b2 ?t) ?sought)
                       (compo-eqn-contains (relative-vel ?b1 ?b3 ?b2 ?t) relvel ?sought)))

;;;
;;;  Apply the relative velocity rule for 3 bodies
;;;

;;; need var named ?body* for nsh to recognize this arg as a principle's important body (to prompt to draw first).
(def-psmclass relative-vel (?eq-type relvel ?xy ?rot 
				     (relative-vel ?body1 ?b2 ?b3 ?t))
  :complexity major
  :english ("relative velocity equation")
  :expformat ((strcat "applying the relative velocity equation to ~a "
                      "in relation to ~a and ~a")
              (nlg ?body1) (nlg ?b2) (nlg ?b3))
  :EqnFormat ("Vac_~a = Vab_~a + Vbc_~a" 
             (nlg ?axis 'adj)(nlg ?axis 'adj) (nlg ?axis 'adj)))


(defoperator relative-vel-contains-body-not-in-sought (?sought)
  :specifications "The reference object is involved in the sought and the body is not and the reference obj is the last body passed in the effects."
  :preconditions ((any-member ?sought((at (mag (relative-vel ?b3 ?b2)) ?t)
				      (at (dir (relative-vel ?b3 ?b2)) ?t) 
				      ))
		  (rdebug "Using relative-vel-contains-no-body  ~%")
		  (reference-object ?b2)
		  (object ?b1)
		  (given (at (mag (relative-vel ?b1 ?b3)) ?t) ?dontcare)
		  (given (at (dir (relative-vel ?b1 ?b3)) ?t) ?dontcare2)
		  (test (not (equal ?b2 ?b3)))
		  (rdebug "Firing relative-vel-contains-no-body  ~%")
		  )
  :effects (
	    (vector-psm-contains (relative-vel ?b1 ?b3 ?b2 ?t) ?sought)
	    (compo-eqn-contains (relative-vel ?b1 ?b3 ?b2 ?t) relvel ?sought)))

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
            (eqn-compos (compo-eqn relvel ?xy ?rot 
				   (relative-vel ?b1 ?b2 ?b3 ?t))
                        (?v12 ?v23 ?v13)) )
  :hint (
	 (point (string 
		 "Can you relate the following 3 vectors: the relative velocity of ~A wrt ~A, the relative velocity of ~a wrt ~a, and the relative velocity of ~a wrt ~A?"
		 ?b1 ?b3  ?b1 ?b2  ?b2 ?b3))
	 (teach (string "The relative velocity of a wrt c is equal to the vector sum of the relative velocity of a wrt b plus the relative velocity of b wrt c. In terms of components:\\n   Vac_x = Vab_x + Vbc_x\\n   Vac_y = Vab_y + Vbc_y."))
	 (bottom-out (string "Write the equation ~A"
			     ((= ?v13 (+ ?v12 ?v23)) algebra)))
	 ))

