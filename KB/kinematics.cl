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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;      time and kinematic quantities
;;;;
;;;;

;;
;; Summing of time intervals. 
;; duration (t02) = duration(t01) + duration (t12), etc.
;;
(defoperator sum-times-contains (?quant)
  :preconditions (
  (any-member ?quant ((duration ?t)))
  (time ?tt)
  (test (proper-subintervalp ?t ?tt)) ;?t equals ?tt or is atomic subinterval
  )
  :effects (
    (eqn-contains (sum-times ?tt) ?quant)
  ))

;; handles writing as sum of atomic sub-intervals
(defoperator write-sum-times (?tt)
  :preconditions 
  ((variable ?tt-var (duration ?tt))
   (bind ?intervals (successive-intervals ?tt))
   (map ?interval ?intervals
      (variable ?t-var (duration ?interval))
      ?t-var ?t-vars))
  :effects ( (eqn (= ?tt-var (+ . ?t-vars)) (sum-times ?tt)) )
  :hint
  ((point (string "Time intervals can be added together."))
   (teach (kcd "write-sum-times")
          (string "Since the time interval ~A is equal to the union of its subintervals, its duration must be the sum of their durations." 
		  ?tt))
   (bottom-out (string "Write the equation ~a."
		        ((= ?tt-var (+ . ?t-vars)) algebra)))
   ))

(defoperator sum-times-contains2 (?quant)
  :preconditions 
  (
   (any-member ?quant ((duration ?t)))
   ;; This is what keeps the number of solutions from blowing up:
   ;; only consider intervals declared in the problem declaration
   (time (during ?t1 ?t2))
   (time (during ?t1 ?ti))
   (time (during ?ti ?t2))
   (any-member ?t ((during ?t1 ?ti) (during ?ti ?t2) (during ?t1 ?t2)))
   ;; so we don't overlap the other version of the rule
   (test (not (and (time-consecutivep '(during ?t1 ?ti))
		   (time-consecutivep '(during ?ti ?t2)))))
   )
  :effects (
    (eqn-contains (sum-times (during ?t1 ?t2) :middle ?ti) ?quant)
  ))

;; sum of two sub-intervals
(defoperator write-sum-times2 (?tt ?ti)
  :preconditions 
  ((variable ?tt-var (duration ?tt))
   (any-member ?tt ((during ?t1 ?t2)))
   (variable ?ta-var (duration (during ?t1 ?ti)))
   (variable ?tb-var (duration (during ?ti ?t2)))
      )
  :effects ( (eqn (= ?tt-var (+ ?ta-var ?tb-var)) 
		  (sum-times ?tt :middle ?ti)) )
  :hint
  ((point (string "Time intervals can be added together."))
   (teach (kcd "write-sum-times")
          (string "Since the time interval ~A is equal to the union of its subintervals, its duration must be the sum of their durations." 
		  ?tt))
   (bottom-out (string "Write the equation ~a."
		        ((= ?tt-var (+ ?ta-var ?tb-var)) algebra)))
   ))


;;; Equation asserts the value at any contained time is equal to the value 
;;; over the interval declared constant.  Generally, this is taken
;;; care of by the inherit-quantity rules.  However, sometimes a child
;;; quantity is a sought.  In that case, we need to explicitly allow both
;;; child and parent quantities and the equality between them.
;;;
;;; We borrow the (equals ...) PSM class for this, but with no overlap
;;; in operators, since we want a different set of hints.  
;;;
;;; BvdS:  In this convention, the generic case for a quantitity
;;; set to a constant over an interval is an average value.  
;;; This is different from standard notation where 
;;;           f(x)=c, x \in {a,b} 
;;; implies f(x) constant.  Also, if one extends to calculus, one would
;;; want average value defined in terms of an integral.
;;; Currently, there is nothing in the user interface corresponding
;;; to setting a quantity to something constant.
;;;

(defoperator constant-value-contains (?quant1)
  :preconditions 
  (
   ;; Generally, this rule is meant to connect a sought child
   ;; quantity with its parent.  Thus, we only consider ?quant1 
   ;; as a possible sought.
   (bind ?quant (remove-time ?quant1))
   (constant ?quant ?t-constant :inclusive ?flag . ?whatever)
   (bind ?t1 (time-of ?quant1))
   (test (and (not (equal ?t1 ?t-constant))
	      (if ?flag
		  (tinsidep-include-endpoints ?t1 ?t-constant)
		(tinsidep ?t1 ?t-constant))))
   (bind ?quant2 (set-time ?quant ?t-constant))
   )
  :effects
  ((eqn-contains (equals ?quant1 ?quant2 :constant t) ?quant1)))


(defoperator write-constant-value (?quant1) 
  :preconditions 
  (
   (variable ?v1 ?quant1)
   (variable ?v2 ?quant2)
   (bind ?t1 (time-of ?quant1))  ;for hints
   (bind ?t-constant (time-of ?quant2))  ;for hints
  )
  :effects (
	    (eqn (= ?v1 ?v2) (equals ?quant1 ?quant2 :constant t))
	    )
  :hint
  ((point (string "Notice that ~a is constant." ?quant2))
   (teach (string "If a quantity is constant over a time interval, then its value at any time inside the interval is equal to its value over the whole interval.")
	  (kcd "inherit-constant-value"))
   (bottom-out (string "Since ~a is constant, you can write the equation ~a=~a." 
		       (?v2 algebra) (?v1 algebra) (?v2 algebra)))
   ))


;; A vector being constant implies that its components are also constant
;; Used to have a version for magnitude and direction but these were
;; never used and tended to cause trouble.
;;
;; This operator is redundant with inherit-vector-compo
;; There is a test in inherit-when-constant to ensure that
;; constant for a compo was not generated by this rule.
(defoperator constant-vector-components (?quant ?t-constant)
  :preconditions 
  (
   (constant ?quant ?t-constant :inclusive ?flag :from-vector nil)
   (bind ?b (second ?quant))
   (axes-for ?b ?rot)
   (get-axis ?xy ?rot)
   )
  :effects ((constant (compo ?xy ?rot ?quant) ?t-constant 
		      :inclusive ?flag :from-vector t)))

;;
;;  Return any times plus timeless
;;  This is better than adding the effect (time nil) which 
;;  would break backwards compatability


(defoperator time-is-time (?t)
  :preconditions ((time ?t))
  :effects ((time-or-timeless ?t)))

(defoperator time-is-timeless ()
  :preconditions ()
  :effects ((time-or-timeless nil)))

;;; ================= speed distance duration ===================
;;; These operators represent knowledge of the speed=distance/duration
;;; equation, which is often abbreviated sdd.  The first few operators
;; define the quantities and the other define the equations.

;;; this operator defines a variable for the duration of an interval.
;;; It expects to be given the interval via unification with its
;;; effect.  The author controls which times are relevant by including
;;; only the relevant ones in a 'time' proposition.

(defoperator define-duration (?interval)
  :description "
   If there is a time interval defined,
   then define a duration variable for it."
  :preconditions
   ((time ?interval)
    (test (time-intervalp ?interval))
    (not (variable ?dont-care (duration ?interval)))
    (bind ?var (format-sym "t_~A" (time-abbrev ?interval))))
  :effects (
    (variable ?var (duration ?interval))
    (define-var (duration ?interval))
  )
  :hint
  ((bottom-out (string "~A and define a variable for ~A."
		       ((begin-sentence *text-tool-action*) eval)
		       ((duration ?interval) def-np)))
   ))

(post-process add-duration (problem)
  "Add predef for duration"
  ;; test whether other planets are involved in problem
  (when ;; test whether any duration has been done as predef already
	     (not (member '(define-var 
			    (duration . ?rest))
			  (problem-predefs problem) :key #'car :test #'unify))
    (dolist (prop (mapcar #'qvar-exp (problem-varindex problem)))
      (let ((vars (unify prop '(duration (during ?t ?tt)))))
	(when vars
	  (push `((define-var ,prop) . 
		  ((:type . "statement")
		   (:symbol . ,(format nil "t~A~A" 
				       (- (cdr (assoc '?t vars)) 1)
				       (- (cdr (assoc '?tt vars)) 1)))))
		(problem-predefs problem)))))))

;;; This operator defines a speed variable.  Its only restriction is
;;; that there be an object and and an interval for it.  It expects to
;;; get these given to it by unification of a goal with its effects.
;;; Thus, it only produces a speed variable when there is a goal to
;;; have one.  

(defoperator define-speed (?b ?t)
  :description "
   If there is time interval and an object,
   then you can define a speed of the object"
  :preconditions
   (
    (time ?t)
    (test (time-intervalp ?t))
    (object ?b)
    (bind ?var (format-sym "sp_~A_~A" (body-name ?b) (time-abbrev ?t))))
  :effects
  ((variable ?var (speed ?b :time ?t))
   (define-var (speed ?b :time ?t)))
  :hint
  ;((bottom-out (string "~A and define a variable for the speed of ~a ~a." 
  ;		       ((begin-sentence *text-tool-action*) eval)
  ; 		       ?b (?t pp))))
  ((bottom-out (string "~A and define a variable for ~a." 
  		       ((begin-sentence *text-tool-action*) eval)
   		       ((speed ?b :time ?t) def-np))))
)

;; This operator defines a distance-traveled variable.  Same comments
;; as for the speed variable.

(defoperator define-distance (?b ?interval)
  :description "
   If there is a time interval and an object,
   then you can define a distance variable for the object."
  :preconditions
  ((time ?interval)
   (test (time-intervalp ?interval))
   (object ?b)
   (not (variable ?dont-care (distance ?b :time ?interval)))
   (bind ?var (format-sym "dist_~A_~A" (body-name ?b) (time-abbrev ?interval))))
  :effects
  ((variable ?var (distance ?b :time ?interval))
   (define-var (distance ?b :time ?interval)))
  :hint
  ;((bottom-out (string "~A and define a variable for the distance traveled by ~a ~a." 
  ;		       ((begin-sentence *text-tool-action*) eval)
  ;		       ?b ?interval))
  ((bottom-out (string "~A and define a variable for ~a." 
  		       ((begin-sentence *text-tool-action*) eval)
  		       ((distance ?b :time ?interval) def-np)))))

;;; This operator represents knowing what kinds of quantities occur in
;;; the speed-distance-duration (sdd) equation.  Like all equations,
;;; this knowledge needs to be represented explicitly so that the seek
;;; operator can choose an equation that might contain the quantity it
;;; is seeking.

(defoperator sdd-contains (?quantity)
  :description "
   the speed-distance-duration equation (sdd) contains
      the speed of ?b and the distance it traveled during ?t,
      and the duration of ?t,
   where ?b is an object and ?t is a time interval."
  :preconditions
  ((any-member ?quantity 
	       ((speed ?b :time ?t)
		(distance ?b :time ?t)
		(duration ?t)))
   (object ?b)
   (time ?t)
   (test (time-intervalp ?t)))
  :effects
  ((eqn-contains (sdd ?b ?t) ?quantity)))


;;; This operator represents the procedure for writing the sdd
;;; equation.  The procedure is simply to define the 3 variables
;;; needed then write the equation. The body of interest may optionally be 
;;; drawn even though no vector diagram is being drawn on it for
;;; consistency with standard Andes problem-solving procedure.

(defoperator write-sdd (?b ?t)
  :description "
   If the goal is to write the sdd equations,
   then the subgoals are to define variables for speed, distance and duration,
   then write speed = distance / duration."
  :preconditions
  ((inherit-variable ?s-var (speed ?b :time ?t))
   (variable ?d-var (distance ?b :time ?t))
   (variable ?t-var (duration ?t))
   ;; nsh now requires body and axes if you ask for help, so there's 
   ;; little point making these 'optional' any more. 
   ;; At end so PSM graph branches at end only.
   (optional (body ?b))
   (optional (axes-for ?b 0))
   )
  :effects
  ((eqn (= ?s-var (/ ?d-var ?t-var)) (sdd ?b ?t)))
  :hint
  ((point (string "Can you write an equation in terms of speed, distance traveled and duration?"))
   (point (string "You need to write an equation for the speed of ~a ~a in terms of the distance traveled by ~a ~a and the duration of the interval ~a." ?b (?t pp) ?b (?t pp) ?t))
   (teach (string "The speed of an object is defined to be the distance traveled by the object divided by the duration of its trip.")
	  (kcd "speed"))
   (bottom-out (string "Because ~a is the speed of ~a ~a, and ~a is the distance traveled, and ~a is the duration of the trip, write ~a=~a/~a." 
		       (?s-var algebra) ?b (?t pp) (?d-var algebra) (?t-var algebra)
		       (?s-var algebra) (?d-var algebra) (?t-var algebra)))
   ))

;;; 
;;; Relate the distance traveled to the magnitude of displacement
;;; in the case of straight line motion.
;;; 

(defoperator displacement-distance-contains (?quantity)
  :preconditions
  ((any-member ?quantity 
	       ((mag (displacement ?b :time ?t))
		(distance ?b :time ?t)))
   ;; make sure we are moving in a straight line.
   (motion ?b straight :accel ?a-dir :dir ?v-dir :time ?t-motion . ?whatever)
   ;; this test does not work for the case of slowing down.
   ;; Also, in the case that :accel is unspecified, we assume
   ;; that it does not change direction.
   (test (or (eq ?a-dir 'zero) (equal ?a-dir ?v-dir) (null ?a-dir)))
   (test (tinsidep ?t ?t-motion))
   (time ?t)
   (test (time-intervalp ?t)) ;sanity check
   )
  :effects
  ((eqn-contains (displacement-distance ?b ?t) ?quantity)))


(defoperator write-displacement-distance (?b ?t)
  :preconditions
  ((variable ?s-var (distance ?b :time ?t))
   (variable ?d-var (mag (displacement ?b :time ?t)))
   )
  :effects
  ((eqn (= ?s-var ?d-var) (displacement-distance ?b ?t)))
  :hint
  ((point (string "How is distance traveled related to displacement?"))
   (teach (string "If ~A is moving in a straight line, the distance traveled ~A is equal to the magnitude of the displacement." ?b (?t pp)))
   (bottom-out (string "Write the equation ~A = ~A" (?s-var algebra) (?d-var algebra)))))

;;;
;;; add up distances
;;;
(defoperator sum-distances-contains (?sought)
  :preconditions 
  ((any-member ?sought ( (distance ?b :time ?t)))
   (time ?tt)
   (test (proper-subintervalp ?t ?tt)) ;?t equals ?tt or is atomic subinterval
   (object ?b) ;sanity check
   )
  :effects 
  ((eqn-contains (sum-distances ?b ?tt) ?sought)))

;; only handles writing as sum of atomic sub-intervals
(defoperator write-sum-distances (?b ?tt)
  :preconditions
  ((variable ?tt-var (distance ?b :time ?tt))
   (bind ?intervals (successive-intervals ?tt))
   (map ?interval ?intervals
      (variable ?t-var (distance ?b :time ?interval))
      ?t-var ?t-vars))
  :effects ( (eqn (= ?tt-var (+ . ?t-vars)) (sum-distances ?b ?tt)) )
  :hint
  ((point (string "Distances can be added together."))
   (teach (string "The distance ~A has traveled ~A is equal to the sum of distances traveled during each sub-interval." 
		  ?b (?tt pp)))
   (bottom-out (string "Write the equation ~a."
		        ((= ?tt-var (+ . ?t-vars)) algebra)))
   ))


;; Pythagorean theorem for distance, currently only used in kt5a. 
;; We use relative positions to specify the givens. 
;; We recognize it for displacement of b from points whose relative 
;; positions from some origin o are given and rbo1 and sb12 form a 
;; right angle so |rbo2|^2 = |rbo1|^2 + |db12|^2
;; That is not the most general form we could try but I'm not going to worry 
;; about it until we get a second problem that needs it.
(defoperator pyth-thm-contains (?sought)
   :preconditions (
     (any-member ?sought (
       (mag (relative-position ?b ?o :time ?t1))
       (mag (relative-position ?b ?o :time ?t2)) 
       (mag (displacement ?b :time (during ?t1 ?t2)))
                         ))
     ;; angle r1 must be given, and must be able to determine angle d12
     (given (dir (relative-position ?b ?o :time ?t1)) ?dir-r1 . ?rest)
     (displacement-dir ?b (during ?t1 ?t2) ?dir-d12)
     (test (perpendicularp ?dir-r1 ?dir-d12))
   )
   :effects ( 
     (eqn-contains (pyth-thm ?b ?o ?t1 ?t2) ?sought) 
   ))

(defoperator get-displacement-dir-from-given (?b ?t1 ?t2)
    :effects ( (displacement-dir ?b (during ?t1 ?t2) ?dir-d12) )
    :preconditions ( (given (dir (displacement ?b :time (during ?t1 ?t2))) 
			    ?dir-d12 . ?rest) ))

(defoperator get-displacement-dir-from-motion (?b ?t1 ?t2)
    :effects ( (displacement-dir ?b (during ?t1 ?t2) ?dir-d12) )
    :preconditions 
    (  (motion ?b straight :dir ?dir-d12 :time (during ?t1 ?t2) . ?whatever) ))

(defoperator write-pyth-thm (?b ?o ?t1 ?t2)
  
  :preconditions (
    (variable ?r1 (mag (relative-position ?b ?o :time ?t1)))
    (variable ?r2 (mag (relative-position ?b ?o :time ?t2))) 
    (variable ?d12 (mag (displacement ?b :time (during ?t1 ?t2))))
  )
  :effects (
    (eqn (= (^ ?r2 2) (+ (^ ?r1 2) (^ ?d12 2))) (pyth-thm ?b ?o ?t1 ?t2))
  )
  :hint
  ((point (string "Notice that there is a right triangle you can make use of."))
   (bottom-out (string "Write the equation ~A." 
		       ((= (^ ?r2 2) (+ (^ ?r1 2) (^ ?d12 2))) algebra)))
  ))

;;; Sum of two distances making up a line segment:
;;;
;;; If have collinear points A,B,C such that B is between A and C, then 
;;; distAC = distAB + distBC.  Since we now give spatial layout information
;;; in terms of relative position vectors, this equation is written in terms
;;; of magnitudes of relative position vectors.
;;;
;;; Right now this is only used in one problem (pot4), so rather then
;;; spend time on geometry rules, we cheat and rely on a given proposition 
;;;       (distance-sum (?b3 ?b1) (?b2 ?b1) (?b2 ?b3))
;;; We write the equation using the following relative positions:
;;;          b1--------->b3
;;;          b1--->b2<---b3
;;; because those are what we need for the Earth-probe-moon problem. 
;;; We can spend time on a general operator when we encounter a need for it.
(defoperator sum-distance-contains (?sought)
  :preconditions (
     (in-wm (distance-sum (?b3 ?b1) (?b2 ?b1) (?b2 ?b3)))
     (any-member ?sought (
		      (mag (relative-position ?b3 ?b1 :time ?t))
                      (mag (relative-position ?b2 ?b1 :time ?t))
		      (mag (relative-position ?b2 ?b3 :time ?t))
                  ))
  )
  :effects ( (eqn-contains (sum-distance ?b1 ?b2 ?b3 ?t) ?sought) )
)

(defoperator write-sum-distance (?b1 ?b2 ?b3 ?t)
  :preconditions (
    (variable ?r21 (mag (relative-position ?b2 ?b1 :time ?t)))
    (variable ?r23 (mag (relative-position ?b2 ?b3 :time ?t)))
    (variable ?r31 (mag (relative-position ?b3 ?b1 :time ?t)))
  )
  :effects (
    (eqn (= ?r31 (+ ?r21 ?r23)) (sum-distance ?b1 ?b2 ?b3 ?t))
  )
  :hint (
    (point (string "Because ~a, ~a and ~a lie along a line, you can use the fact that the total distance from ~a to ~a is the sum of the distance from ~a to ~a and the distance from ~a to ~a."
          ?b1 ?b2 ?b3   ?b1 ?b3    ?b1 ?b2     ?b3 ?b2))
    (bottom-out (string "Write the equation ~A"
                        ((= ?r31 (+ ?r21 ?r23)) algebra)))
  ))


;; "rdiff": vector psm for calculating components of r21 from given
;; coordinates of points 1 and 2. Coordinates are given values of 
;; relative-positions wrt the specially named point 'origin.
;; The equation is:
;;   r21_x = r2o_x - r1o_x 
;; However, to keep the solutions simple, we plug the numerical
;; values of the given coordinates directly into the equation, rather 
;; than using variables for the given positions, which would then have
;; to be drawn.  Might have to change this eventually, or add variant
;; that allows them to be drawn.

;; See Bug #1510

(def-psmclass rdiff
             (?eq-type rdiff ?axis ?rot (rdiff ?p1 ?p2 ?time)) 
  :complexity minor
  :short-name "relative position from coordinates"
  :nlg-english ("the relative position definition")
  :ExpFormat ("computing the ~a component of the position of ~a relative to ~a"
		 (axis-name ?axis) (nlg ?p2) (nlg ?p1) )
  :EqnFormat ("r21<sub>~a</sub> = ~a2 - ~a1" (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))

(defoperator rdiff-contains (?sought)
  :preconditions (
    ;; only applies in component-form
    (component-form)
    (any-member ?sought ((relative-position ?p2 ?p1 :time ?t)
			 ;; no other variables in this equation
			 ))
    (in-wm (given (compo x 0  (relative-position ?p1 origin :time ?t)) ?p1_x))
    (in-wm (given (compo y 0 (relative-position ?p1 origin :time ?t)) ?p1_y))
    (in-wm (given (compo x 0  (relative-position ?p2 origin :time ?t)) ?p2_x))
    (test (not (eq ?p2 ?p1))) ; make sure two different points
    (in-wm (given (compo y 0 (relative-position ?p2 origin :time ?t)) ?p2_y))
    ;; should still work if p1 or p2 = origin, but would need to be 
    ;; told that coords of origin are (0,0) in givens
    )
  :effects 
  ((eqn-family-contains (rdiff ?p1 ?p2 ?t) ?sought)
  ; since only one compo-eqn under this vector psm, we can just
  ; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (rdiff ?p1 ?p2 ?t) rdiff ?sought)))

(defoperator draw-rdiff-diagram (?rot ?p1 ?p2 ?t)
  :preconditions 
  ((not (vector-diagram ?rot (rdiff ?p1 ?p2 ?t)))
   ;; do we draw a body for this? What body do we call this
   (vector ?p2 (relative-position ?p2 ?p1 :time ?t) ?dir1)
   ;; have to make sure we have an axis for this vector
   (axes-for ?p2 ?rot))
  :effects 
  ((vector-diagram ?rot (rdiff ?p1 ?p2 ?t))))

(defoperator write-rdiff-compo (?p1 ?p2 ?t ?xy ?rot)
  :preconditions (
    (variable ?r21_xy (compo ?xy ?rot (relative-position ?p2 ?p1 :time ?t)))
    ;; just fetch the coordinate values to plug in
    (given (compo ?xy ?rot  (relative-position ?p1 origin :time ?t)) ?r1o_xy_val)
    (given (compo ?xy ?rot  (relative-position ?p2 origin :time ?t)) ?r2o_xy_val)
   )
  :effects (
   (eqn (= ?r21_xy (- ?r2o_xy_val ?r1o_xy_val))
         (compo-eqn rdiff ?xy ?rot (rdiff ?p1 ?p2 ?t)) )
  ) 
  :hint (
    (point (string "The components of the relative position of one point wrt another can be computed from the coordinates of the two points"))
    (teach (string "The relative position vector r21 of a point p2 wrt p1 is equal to the vector difference r2o - r1o of the positions of p2 and p1 with respect to the origin.  You can compute the components of the needed relative position vector from the given coordinates of the two points."))
    (bottom-out (string "Write the equation ~A"
                ((= ?r21_xy (- ?r2o_xy_val ?r1o_xy_val)) algebra)))
  ))


;; Following would enable the equality rba = rpa to be exploited when body 
;; b is at p.  However, it doesn't really help if rpa is sought and rpa 
;; compos given, since then nothing allows rba to be calculated, 
;; so gets purged as dead-path quant.  Might be useable in some problems.
;; Causes error in dip1a.
#|
(defoperator same-relpos(?body ?loc ?origin ?time)
  :preconditions 
  ( (time ?t)
    (any-member ?time (nil ?t)) ;try either declared time or constant.
    (in-wm (at-place ?body ?loc :time ?t-at-loc))
    (test (tinsidep ?time ?t-at-loc))
    ;; ?origin should be bound from sought coming in
  )
  :effects (
     ; Assert equality. Equation will be written by generic write-equality 
     ; operator without much of a hint, as if equality is given or obvious.
     (equals (mag (relative-position ?body ?origin :time ?time))
             (mag (relative-position ?loc ?origin :time ?time)))
  ))
|#


;;; ========================= Displacement ====================================
;;; The operator draws displacement in the case where the object is
;;; moving in a straight line during a time that includes the desired
;;; time.  The desired time, ?t, is passed in via unification with the
;;; effects.  It must be a time interval. 
;;; 
;;; As per the email discussion during the week of 10/16/2000, all vector 
;;; drawing tools will write an equation setting the direction variable to 
;;; the value given in the box on the vector drawing tool.  This value 
;;; should be either a number of degrees, a parameter or the constant unknown.  
;;; That constant unknown stands for the case where the student erases 
;;; the number in the vector drawing dialog box and leaves it blank.
;;; 
;;; Because this should only write the direction variable equation when the 
;;; value is not unknown, we'd need either two versions of this operator or 
;;; conditional effects.  For now, only one version of the operator is 
;;; supplied, and it  will only work when the direction value is known.
;;; 


(defoperator draw-displacement-straight (?b ?t)
  :description "
   If an object is moving in a straight line over a time interval
   then draw a displacement vector for it in the direction of its motion."
  :preconditions
   (
    (motion ?b straight :may-stop ?sflag :dir ?dir :time ?t-motion . ?rest)
    (time ?t)
    (test (and (time-intervalp ?t) (tinsidep ?t ?t-motion)))
    ;; choose smallest interval for which there is a motion statement
    ;; Work around for kgraph9, Bug #977
    (not (motion ?b at-rest :time ?t))
    (test (not (equal ?dir 'unknown)))	;until conditional effects are implemented
    (not (vector ?b (displacement ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (bind ?any-motion (if ?sflag "any motion of ~A is" "~A is moving"))
    )
  :effects
   ((vector ?b (displacement ?b :time ?t) ?dir)
    (variable ?mag-var (mag (displacement ?b :time ?t)))
    (variable ?dir-var (dir (displacement ?b :time ?t)))
    (given (dir (displacement ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (displacement ?b :time ?t)))
    ) 
  :hint
  ((point (string "Notice that ~A, ~@? along a straight line." 
		  (?t pp) (?any-motion identity) ?b))
   (teach (string "Whenever an object moves in a straight line, the displacement vector is parallel to the direction of motion.")
	  (kcd "draw_displacement"))
   (bottom-out (string "Because ~@? in the direction ~A, use ~A to draw a displacement vector in the direction ~a ~A" 
		       (?any-motion identity) ?b ?dir
		       (*vector-tool* eval) ?dir (?t pp)))
   ))


(defoperator draw-displacement-straight-unknown (?b ?t)
  :description "
   If an object is moving in a straight line over a time interval in an unknown direction,
   then draw a displacement vector for it in the direction of its motion."
  :preconditions
   ((motion ?b straight :dir unknown :time ?t-motion . ?whatever)
    (time ?t)
    (test (time-intervalp ?t))
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (displacement ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var)))
  :effects
   ((vector ?b (displacement ?b :time ?t) unknown)
    (variable ?mag-var (mag (displacement ?b :time ?t)))
    (variable ?dir-var (dir (displacement ?b :time ?t))))
  :hint
  ((point (string
	 "Notice that ~a is moving in a straight line ~a." ?b (?t pp)))
   (teach (string
	 "Whenever an object is moving in a straight line over a time interval, it has a displacement which is parallel to the direction of motion.  In this problem, the exact direction of the displacement vector requires calculation to determine, so you can draw the vector at an approximately correct angle."))
   ;(bottom-out (string
   ;		 "Draw the displacement of ~a ~a at an approximately correct angle." 
   ;		 ?b (?t pp)))
   (bottom-out (string
   		 "Draw ~a at an approximately correct angle." 
   		 ((displacement ?b :time ?t) def-np)))
    ))

;;; Might want rule to put out equation thetaD = thetaV for unknown 
;;; directions  if needed.

;; This operator draws a zero-length displacement vector for an object
;; that is at rest over an interval.  This would seldom be useful in practice.

(defoperator draw-displacement-at-rest (?b ?t)
  :description "If an object is at rest,
   then draw a zero displacement vector."
  :preconditions
   ((motion ?b at-rest :time ?t-motion)
    (time ?t)
    (test (time-intervalp ?t))
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (displacement ?b :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t))))
  :effects
   ((vector ?b (displacement ?b :time ?t) zero)
    (variable ?mag-var (mag (displacement ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is at rest ~a." ?b (?t pp)))
   (teach (string "Whenever an object is at rest during a time interval, it has a displacement of zero.")
	  (kcd "draw_zero_displacement"))
   (bottom-out (string "Because ~a is at rest ~a, use ~A to draw a zero length displacement vector for it." 
		       ?b (?t pp) (*vector-tool* eval)))
   ))

;; Following draws a zero-mag displacement vector for case where it is
;; given to be zero. This is given in cases of a round-trip where the body
;; returns to its original position. We don't have any special motion
;; specifier to entail this, it is just specified by zero displacement.

(defoperator draw-displacement-zero (?b ?t)
  :description 
   "If an object has no net change of position over an interval, then
   draw a zero displacement vector"
  :preconditions
   ((in-wm (given (mag (displacement ?b :time ?t)) (dnum 0 ?units) 
		  :hint (?wherefrom ?motion) 
		  (nil "Look at the problem statement.")))
    (not (vector ?b (displacement ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t))))
  :effects
   ((vector ?b (displacement ?b :time ?t) zero)
    (variable ?mag-var (mag (displacement ?b :time ?t))))
   :hint
   ((bottom-out (string "~@[~A  ~]The displacement of ~a is zero, just draw a zero-length vector for it."
			(?motion identity) ?b))
    ))

;; This operator draws displacement at a given direction. This is needed
;; for problems like the bumblebee where the trajectory over the interval
;; is irregular but the net displacement direction is known.

;; See bug #1899
(defoperator draw-displacement-given-dir (?b ?t) 
  :description 
   "If you are given the direction of a net displacement over an interval
   then draw a displacement vector for it in the direction of its motion."
  :preconditions
   ((in-wm (given (dir (displacement ?b :time ?t)) ?dir))
    (test (time-intervalp ?t))
    (not (vector ?b (displacement ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    )
  :effects
   ((vector ?b (displacement ?b :time ?t) ?dir)
    (variable ?mag-var (mag (displacement ?b :time ?t)))
    (variable ?dir-var (dir (displacement ?b :time ?t)))
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (displacement ?b :time ?t)))
    ) 
   :hint
   ((point (string "The problem specifies the displacement of ~a ~a." 
		   ?b (?t pp)))
    (teach (kcd "draw_displacement")
	   (string "The displacement of an object is a vector from its starting point to its ending point.  It doesn't matter what path the object took.  Only the two points matter.  The problem gives you that information."))
    ;(bottom-out (string "The problem specifies that the displacement of ~a ~a is at ~a, so just draw a displacement vector oriented at ~a." 
    ;			?b (?t pp) ?dir ?dir))
    (bottom-out (string "The problem specifies that ~a is at ~a, so just draw a displacement vector oriented at ~a." 
    			((displacement ?b :time ?t) def-np) ?dir ?dir))
    ))

;; This operator draws net displacement at an unknown angle for a 2D 
;; projectile trajectory if the direction of net displacement is 
;; not given. 

(defoperator draw-displacement-projectile (?b ?t)
  :description 
   "If you don't know the direction of a net displacement over an interval
   then draw a displacement vector for it at an unspecified direction"
  :preconditions
   ((motion ?b curved :type projectile :time ?t . ?dontcare)
    (not (given (dir (displacement ?b :time ?t)) ?dir-given))
    (test (time-intervalp ?t))
    (not (vector ?b (displacement ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var)))
  :effects
   ((vector ?b (displacement ?b :time ?t) unknown)
    (variable ?mag-var (mag (displacement ?b :time ?t)))
    (variable ?dir-var (dir (displacement ?b :time ?t))))
   :hint
   ((point (string "You need to introduce a term for the displacement of ~a ~a." ?b (?t pp)))
    (teach (string "The displacement of an object is a vector from its starting point to its ending point.  It doesn't matter what path the object took.  Only the two points matter. In this problem, the exact direction of the net displacement vector requires calculation to determine, so you can draw the vector at an approximately correct angle."))
    ;(bottom-out (string "Draw the displacement of ~a ~a at an approximately correct angle."
    ;			?b (?t pp)))
    (bottom-out (string "Draw ~a at an approximately correct angle."
    			((displacement ?b :time ?t) def-np)))
    ))

;;; This operator draws a displacement vector at an unknown angle when 
;;; we have no other information about it -- no given displacement 
;;; direction and no motion spec that we can use to apply a more specific 
;;; operator. Needed if net displacement is the sought as in vec1a, vec3a.
;;;
;;; No simple way to make sure this applies when nothing else does short 
;;; of negating all other's preconditions.  For now, just trigger by 
;;; complete absence of motion spec for object.
;;;
;;; !!! We could try to use the same operator for this case as the 
;;; projectile case.
;;
(defoperator draw-displacement-unknown (?b ?t)
  :preconditions 
  ( (time ?t)
    (test (time-intervalp ?t))
    (object ?b) ;needs to be kinematics-able body
    ;; motion with unknown direction not handled correctly:
    (not (motion ?b ?motion-spec :time ?t-motion . ?whatever) 
	 (tinsidep ?t ?t-motion))
    ;; dir=unknown not handled correctly:
    (not (given (dir (displacement ?b :time ?t)) ?dir))
    ;; BvdS:  hack to get kt13a and kgraph9 to work
    (not (given (mag (displacement ?b :time ?t)) (dnum 0 ?units) . ?rest-hint))
    (not (vector ?b (displacement ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "s_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?is-net (if (> (third ?t) (+ 1 (second ?t))) 
		      "In this problem, the net displacement vector must be calculated by summing individual displacements.  " "In this problem, the direction of the net displacement is not given.")))
  :effects
  ((vector ?b (displacement ?b :time ?t) unknown)
   (variable ?mag-var (mag (displacement ?b :time ?t)))
   (variable ?dir-var (dir (displacement ?b :time ?t))))
  :hint
  ((point (string "You need to introduce a term for the displacement of ~a ~a." ?b (?t pp)))
   (teach (string "The displacement of an object is a vector from its starting point to its ending point.  It doesn't matter what path the object took.  Only the two points matter.~A  Draw the vector at an approximately correct angle." 
		  (?is-net identity)))
    ;(bottom-out (string "Draw the displacement of ~a ~a at an approximately correct angle."
    ;			?b (?t pp)))
    (bottom-out (string "Draw ~a at an approximately correct angle."
    			((displacement ?b :time ?t) def-np)))
   ))


;;; ================================= Velocity ================================
;;; These operators translate the motion of the object, which is given
;;; in the problem statement, into a velocity vector.

;; This operator draws a zero velocity vector because the object is at
;; rest during a time period that incloses the desired time period.  This
;; assumes that the desired time period and body are passed in via
;; unification with an effect. 

(defoperator draw-velocity-at-rest (?b ?t)
  :description 
  "If there is an object,
     and it is at rest at a certain time,
   then its velocity at that time is zero."
  :preconditions
  ((use-point-for-body ?body ?cm ?b)	;else ?b is sometimes not bound
   (motion ?b at-rest :time ?t-motion)
   (time ?t)
   (test (tinsidep ?t ?t-motion))
   (bind ?mag-var (format-sym "v_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t))))
  :effects
  ((vector ?b (velocity ?b :time ?t) zero)
   (variable ?mag-var (mag (velocity ?b :time ?t)))
   (given (mag (velocity ?b :time ?t)) (dnum 0 |m/s|))
   (implicit-eqn (= ?mag-var (dnum 0 |m/s|)) (mag (velocity ?b :time ?t)))
   )
  :hint
  ((point (string "Notice that ~a is at rest ~a." ?b (?t pp)))
   (teach (kcd "draw_zero_velocity")
	  (string "When an object is at rest, its velocity is zero.")) ; too simple for a kcd
   (bottom-out (string "Because ~a is at rest ~a, use ~A to draw a zero-length velocity vector for it." 
		       ?b (?t pp)
		       (*vector-tool* eval)
		       ))))

;; might now be redundant with draw-velocity-at-rest
(defoperator draw-velocity-rotating-fixed (?b ?t)
  :preconditions
   ((use-point-for-body ?b ?cm ?axis) ;else ?b is sometimes not bound
    (motion ?b rotating :axis ?axis :time ?t-body . ?whatever)
    (time ?t)
    (test (tinsidep ?t ?t-body))
    (motion ?axis ?at-rest :time ?t-axis)
    (test (tinsidep ?t ?t-axis))
    (test (or (eq ?at-rest 'at-rest) (eq ?at-rest 'momentarily-at-rest)))
    (bind ?mag-var (format-sym "v_~A_~A" ?axis (time-abbrev ?t))))
  :effects
   ((vector ?b (velocity ?axis :time ?t) zero)
    (variable ?mag-var (mag (velocity ?axis :time ?t)))
    (given (mag (velocity ?axis :time ?t)) (dnum 0 |m/s|))
    (implicit-eqn (= ?mag-var (dnum 0 |m/s|)) (mag (velocity ?axis :time ?t)))
    )
  :hint
  ((point (string "Although ?b is rotating, the axis of rotation ~A is fixed." 
		  ?b ?axis))
    (teach (string "For an object that is rotating around a fixed axis, its motion is defined to be rotational.  Thus, its translational velocity is zero.")) ; too simple for a kcd
    (bottom-out (string "Use ~A to draw a zero-length velocity vector for ~A ~A." 
			(*vector-tool* eval)
			?b (?t pp)))
   ))

;;
;; This draws zero velocity for object momentarily at rest at an instant
;; This is a weaker statement than "at-rest" since it doesn't entail
;; anything about the acceleration at the instant. We use this for
;; objects like tossed objects at the apex which have zero velocity
;; are accelerating, so that we don't derive accel zero for them.
;; In the future might want projectile motion rule to handle this case.
;;
(defoperator draw-velocity-momentarily-at-rest (?body ?t)
  :description 
  "If there is an object,
     and it is momentarily at rest at a certain instant,
   then its velocity at that time is zero."
  :preconditions
  ((use-point-for-body ?body ?cm ?b)	;else ?b is sometimes not bound
   (motion ?b momentarily-at-rest :time ?t-motion)
   (time ?t)
   (test (and ?t-motion (tinsidep ?t ?t-motion)))
   (bind ?mag-var (format-sym "v_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t))))
  :effects
  ((vector ?body (velocity ?b :time ?t) zero)
   (variable ?mag-var (mag (velocity ?b :time ?t)))
   (given (mag (velocity ?b :time ?t)) (dnum 0 |m/s|))
   (implicit-eqn (= ?mag-var (dnum 0 |m/s|)) (mag (velocity ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is momentarily at rest ~a." ?b (?t pp)))
   (teach (string "When an object is at rest even momentarily, its velocity at that moment is zero.")
	   (kcd "draw_zero_velocity"))
   (bottom-out (string "Because ~a is at rest ~a, use ~A to draw a zero-length velocity vector for it." 
		       (*vector-tool* eval)
		       ?b (?t pp)))
   ))


;; This operator draws a non-zero velocity vector along the line of
;; motion because the object is moving in a straight line during a time
;; period that includes the desired time, which was passed in via the
;; effects. 

(defoperator draw-velocity-straight (?b ?t)
  :description 
  "If an object is moving in a straight line at a certain time,
   then its velocity at that time is non-zero and in the same direction
     as its motion."
  :preconditions
  ((use-point-for-body ?body ?cm ?b) ;else ?b is sometimes not bound
   (motion ?b straight :dir ?dir :time ?t-motion . ?whatever)
   (test (and ?dir (not (equal ?dir 'unknown))))
   (time ?t)
   (test (tinsidep ?t ?t-motion))
   (not (vector ?body (velocity ?b :time ?t) ?dir))
   (bind ?mag-var (format-sym "v_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
  ((vector ?body (velocity ?b :time ?t) ?dir)
   (variable ?mag-var (mag (velocity ?b :time ?t)))
   (variable ?dir-var (dir (velocity ?b :time ?t)))
   (given (dir (velocity ?b :time ?t)) ?dir)
   (implicit-eqn (= ?dir-var ?dir-var-value) (dir (velocity ?b :time ?t)))
   )
  :hint
  ((point (string "Notice that ~a is moving in a straight line ~a." ?b (?t pp)))
   (teach (string "Whenever an object is moving in a straight line, it has a velocity in the same direction as its motion.")
	  (kcd "draw_nonzero_velocity"))
   (bottom-out (string "Because ~a is moving in a straight line ~a, use the ~A draw a non-zero vector in direction ~a." 
		       ?b (?t pp) (*vector-tool* eval) (?dir adj)))
   ))

(defoperator draw-velocity-straight-unknown (?b ?t)
  :description 
  "If an object is moving in a straight line at a certain time,
   then its velocity at that time is non-zero and in the same direction
     as its motion."
  :preconditions
  ((use-point-for-body ?body ?cm ?b)	;else ?b is sometimes not bound
   (motion ?b straight :dir unknown :time ?t-motion . ?whatever)
   (time ?t)
   (test (tinsidep ?t ?t-motion))
   (not (vector ?body (velocity ?b :time ?t) ?dir))
   (bind ?mag-var (format-sym "v_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var)))
  :effects
  ((vector ?body (velocity ?b :time ?t) unknown)
   (variable ?mag-var (mag (velocity ?b :time ?t)))
   (variable ?dir-var (dir (velocity ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is moving in a straight line ~a, although the exact direction of motion is unknown." ?b (?t pp)))
   (teach (string "Whenever an object is moving in a straight line, it has a non-zero velocity in the same direction as its motion.")
	  (kcd "draw_nonzero_velocity"))
   (bottom-out (string
		"Because ~a is moving in a straight line ~a in an unknown direction, draw a non-zero velocity vector for it in an approximately correct direction." ?b (?t pp)))
   ))

;;; Using the motion statement:
;;;    (motion ?body curved :type ?type :dir ?dir-velocity 
;;;                 :accel ?dir-acceleration :time ?t)
;;; This operator draws velocities for curved motion, where curved
;;; motion ?type is projectile, circular and other kinds of curves.
;;; ?dir-accleration is used only for ?type=projectile or circular.
;;; General motion along a curve can be described by:
;;;     (motion ?body curved :type nil :dir ?dir-velocity :time ?t . ?rest)
;;;

(defoperator draw-velocity-curved (?b ?t)
  :description 
   "If an object is moving along a curved at a certain time point,
   then its velocity is tangent to the curve at that time."
  :preconditions
   ((motion ?b curved :dir ?dir :time ?t . ?dontcare)
    (test (and ?dir (not (eq ?dir 'unknown))))
    (time ?t) ;sanity test
    (test (time-pointp ?t))
    (not (vector ?b (velocity ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "v_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
   ((vector ?b (velocity ?b :time ?t) ?dir)
    (variable ?mag-var (mag (velocity ?b :time ?t)))
    (variable ?dir-var (dir (velocity ?b :time ?t)))
    (given (dir (velocity ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (velocity ?b :time ?t))))
  :hint
  ((teach (string "When an object is moving in a curve, its velocity at an instant of time is tangent to the curve.")
	  (kcd "draw-velocity-curved"))
   (bottom-out (string "Because ~a is moving in a curve ~a, and the tangent to the curve at that point is ~a, draw a non-zero velocity in direction ~a." 
		       ?b (?t pp) (?dir adj) (?dir adj)))
   ))


;;; This draws velocity for a curved path at a point for which 
;;; velocity direction is not given.
(defoperator draw-velocity-curved-unknown (?b ?t)
  :preconditions
  (;; don't use this to draw average velocity over an interval.
   (motion ?b curved :dir ?dir-spec :time ?t-motion . ?dontcare)
   (time ?t)
   (test (time-pointp ?t))
   (test (tinsidep ?t ?t-motion))
   ;; Test for unknown
   (test (or (null ?dir-spec) (eq ?dir-spec 'unknown)))
   ;; direction of displacement is not known
   ;; This is handled by draw-avg-vel-from-displacement
   (not (given (dir (displacement ?b :time ?t)) ?disp-dir))
   (not (vector ?b (velocity ?b :time ?t) ?dir))
   (bind ?mag-var (format-sym "v_~A_~A" (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var)))
  :effects
  ((vector ?b (velocity ?b :time ?t) unknown)
   (variable ?mag-var (mag (velocity ?b :time ?t)))
    (variable ?dir-var (dir (velocity ?b :time ?t))))
  :hint
   ((point (string "You need to draw a vector for the velocity of ~a ~a." ?b (?t pp)))
    (teach (string "In this problem, the exact direction of the velocity vector is not given, so you should draw the vector at any angle."))
    ;(bottom-out (string "Draw the velocity of ~a ~a." ?b (?t pp)))
    (bottom-out (string "Draw ~a." ((velocity ?b :time ?t) def-np)))
    ))

; Following draws horizontal velocity for a 2D projectile at its maximum height.
; !!! Note we assume the curve is going from left to right. Should have this 
; in motion specs somehow but don't currently.
(defoperator draw-velocity-apex(?b ?t)
  :description "if a projectile is at the apex of parabolic flight then its velocity is horizontal at that point"
  :preconditions (
     (apex ?b ?t)  ;only use if there is horizontal motion
     (not (vector ?b (velocity ?b :time ?t) ?dir))
     (bind ?mag-var (format-sym "v_~A_~A" (body-name ?b) (time-abbrev ?t)))
     (bind ?dir-var (format-sym "O~A" ?mag-var))
     (bind ?dir '(dnum 0 |deg|))
  )
  :effects (
    (vector ?b (velocity ?b :time ?t) ?dir)
    (variable ?mag-var (mag (velocity ?b :time ?t)))
    (variable ?dir-var (dir (velocity ?b :time ?t)))
    (given (dir (velocity ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir) (dir (velocity ?b :time ?t)))
  )
  :hint (
    (point (string "Notice that ~A is at its maximum height ~A" ?b (?t pp)))
    (teach (string "When the height of a projectile is at its maximum, the vertical component of its velocity will be zero. Therefore it's velocity must lie entirely in the horizontal direction"))
    ;(bottom-out (string "Use ~a to draw the velocity of ~a ~a at ~A" 
    ;			(*vector-tool* eval)
    ;			?b (?t pp) ?dir))
    (bottom-out (string "Use ~a to draw ~a at ~A" 
    			(*vector-tool* eval)
    			((velocity ?b :time ?t) def-np) ?dir))
    ))

;;; Special to average velocity vector = displacement / t
;;;
;;; Average velocity is little used in our problems, it's mainly defined for 
;;; basic problems like the bumblebee problem that test understanding the
;;; difference between speed and avg. velocity.  In linear kinematics with 
;;; constant acceleration, v_avg = (v0 + vf)/2 could be used with 
;;; s = v_avg * t, but our solutions don't use this form of that equation 
;;; so don't introduce this term.

;; Following operator draws the average velocity vector based on known 
;; direction of net displacement over the interval.  This is needed for case 
;; of irregular non-straight-line motion as in the bumblebee problem. 

(defoperator draw-avg-vel-from-displacement (?b ?t)
  :preconditions 
  (
   (in-wm (given (dir (displacement ?b :time ?t)) ?dir))
   (not (vector ?b (velocity ?b :time ?t) ?dontcare))
   (bind ?mag-var (format-sym "v_~A_~A" (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
  ((vector ?b (velocity ?b :time ?t) ?dir)
    (variable ?mag-var (mag (velocity ?b :time ?t)))
    (variable ?dir-var (dir (velocity ?b :time ?t)))
    (given (dir (velocity ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (velocity ?b :time ?t)))
    )
  :hint
  ((teach (kcd "average_velocity_drawn")
	  (string "The average velocity during a time interval is just the displacement of the body over that time interval divided by the duration of the time interval.  Since displacement is a vector, so is average velocity, and they have the same direction."))
   (bottom-out (string "Draw an ~a average velocity vector for ~a ~a."
		       ?dir ?b (?t pp)))
   ))

(defoperator draw-avg-vel-unknown (?b ?tt)
  :preconditions 
  (
   ;; displacement is unknown from draw-displacement-unknown
   (vector ?b (displacement ?b :time ?tt) unknown)
   ;; This doesn't handle ?dir=unknown correctly:
   (not (given (dir (velocity ?b :time ?tt)) ?dir))
   (not (vector ?b (velocity ?b :time ?tt) ?dontcare))
   (bind ?mag-var (format-sym "v_~A~@[_~A~]" (body-name ?b) 
                                (time-abbrev ?tt)))
   (bind ?dir-var (format-sym "O~A" ?mag-var)))
  :effects
   ((vector ?b (velocity ?b :time ?tt) unknown)
    (variable ?mag-var (mag (velocity ?b :time ?tt)))
    (variable ?dir-var (dir (velocity ?b :time ?tt)))
    )
  :hint
  ((teach (kcd "average_velocity_drawn")
	  (string "The average velocity during a time interval is equal to the displacement of the body over that time interval divided by the duration of the time interval.  In this case, the direction of the velocity vector is not clear from the problem statement."))
   ;(bottom-out (string "Draw an ~a average velocity vector for ~a ~a with unknown direction."
   ;		       ?dir ?b (?tt pp)))
   (bottom-out (string "Draw ~a with unknown direction."
   		       ((velocity ?b :time ?tt) def-np) ))
   ))


;;
;; Average Velocity vector = Displacement over time.
;; Following operators find average velocity as a vector PSM.
;; These are mainly for problems that test understanding of definition
;; of average velocity. Still it could be used to find components of
;; average velocity if we want.

(defoperator avg-vel-vector-contains (?sought)
  :preconditions 
    ((any-member ?sought ((velocity ?b :time ?t)
			  (displacement ?b :time ?t)
			  (duration ?t)))
    (object ?b)
    (time ?t))
  :effects 
  ((eqn-family-contains (avg-velocity ?b ?t) ?sought)
  ;; since only one compo-eqn under this vector PSM, we can just
  ;; select it now, rather than requiring further operators to do so
  (compo-eqn-contains (avg-velocity ?b ?t) avg-vel ?sought)))

(defoperator draw-avg-vel-diagram (?rot ?b ?t)
  :preconditions 
  ((not (vector-diagram ?rot (avg-velocity ?b ?t)))
   (body ?b)
   (inherit-vector ?b (displacement ?b :time ?t) ?s-dir)
   (inherit-vector ?b (velocity ?b :time ?t) ?v-dir)
   (axes-for ?b ?rot))
  :effects 
  ((vector-diagram ?rot (avg-velocity ?b ?t))))

(defoperator write-avg-vel-compo (?b ?t ?xy ?rot)  
  :preconditions 
   (
    ;; test that there is possibly some motion in this direction:
    ;; else this overlaps with the projection equations
    ;; vector drawn in drawing step above
    (in-wm (inherit-vector ?b (velocity ?b :time ?t) ?v-dir))
    (test (non-zero-projectionp ?v-dir ?xy ?rot))
    (inherit-variable ?d12_x (compo ?xy ?rot (displacement ?b :time ?t)))
    (inherit-variable ?v12_x (compo ?xy ?rot (velocity ?b :time ?t)))
    (inherit-variable ?t12 (duration ?t)))
  :effects (
	    (eqn (= ?d12_x (* ?v12_x ?t12))
		 (compo-eqn avg-vel ?xy ?rot (avg-velocity ?b ?t)))
	    )
  :hint (
   (point (string "What is the relationship between average velocity, displacement and duration?"))
    (teach (kcd "write_average_velocity_eqn")
	   (string "The average velocity vector is defined as the displacement vector divided by the duration. This can be applied component-wise to relate the components of average velocity to the components of displacement."))
    (bottom-out (string "Write the equation ~a"
			((= ?v12_x (/ ?d12_x ?t12)) algebra)))
  ))


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
  :short-name "relative velocity"
  :nlg-english ("relative velocity equation")
  :tutorial "RelativeVelocity.html"
  :expformat ((strcat "applying the relative velocity equation to ~a "
                      "in relation to ~a and ~a")
              (nlg ?body1) (nlg ?b2) (nlg ?b3))
  :EqnFormat ("Vac<sub>~a</sub> = Vab<sub>~a</sub> + Vbc<sub>~a</sub>" 
             (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))

(defoperator relative-vel-contains (?sought)
  :preconditions (
		  (in-wm (relvel-triangle ?b1 ?b2 ?b3))
		  (any-member ?sought ((relative-vel ?b1 ?b3 :time ?t)
				       (relative-vel ?b1 ?b2 :time ?t)
				       (relative-vel ?b2 ?b3 :time ?t)))
		  )
  :effects ((eqn-family-contains (relative-vel ?b1 ?b2 ?b3 ?t) ?sought)
	    (compo-eqn-contains (relative-vel ?b1 ?b2 ?b3 ?t) relvel ?sought)))


;;; following attaches a hint to the subgoal of drawing this diagram,
;;; see ontology.cl for examples
(def-goalprop rel-vel-fbd (vector-diagram ?rot (relative-vel ?b1 ?b2 ?b3 ?t))
   :nlg-english ("drawing a diagram showing all of the needed relative velocity vectors and coordinate axes" ))
   
(defoperator draw-rel-vel-diagram (?rot ?b1 ?t)
  :preconditions (
		  (rdebug "Using draw-rel-vel-diagram-no-axes-problem ~%")
		  (not (vector-diagram ?rot (relative-vel ?b1 ?b2 ?t)))
		  (body ?b1)		; choose b1 as our body to draw:
		  (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dir1)    
		  (vector ?b1 (relative-vel ?b1 ?b3 :time ?t) ?dir3)   
		  (vector ?dontcare (relative-vel ?b2 ?b3 :time ?t) ?dir2)
		  (axes-for ?b1 ?rot)
		  (rdebug "Fired draw-rel-vel-diagram-no-axes-problem ~%")
		  )
  :effects (
	    (vector-diagram ?rot (relative-vel ?b1 ?b2 ?b3 ?t))
	    ))

;; See bug #1899
(defoperator draw-relative-vel-given-dir (?b1 ?b2 ?t)
  :description "If the relative velocity vector of a body wrt to
                   something else is needed & the direction is given, 
                   then draw it at the given direction"
  :preconditions ((rdebug "Using draw-rel-vel-vector-given-dir ~%")
		  (given (dir (relative-vel ?b1 ?b2 :time ?t-given)) ?dir)
		  (time ?t) ;explicit time
		  (test (tinsidep ?t ?t-given))
		  (not (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dir))
		  (bind ?mag-var (format-sym "V_~A_~A_~A" 
					     (body-name ?b1) 
					     (body-name ?b2) 
					     (time-abbrev ?t)))
		  (bind ?dir-var (format-sym "O~A" ?mag-var))
                  (bind ?dir-var-value (dir-var-value ?dir))
		  (rdebug "fired draw-rel-vel-vector-given-dir  ~%")
		  )
  :effects (
	    (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dir)
	    (variable ?mag-var (mag (relative-vel ?b1 ?b2 :time ?t)))
	    (variable ?dir-var (dir (relative-vel ?b1 ?b2 :time ?t)))
	    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (relative-vel ?b1 ?b2 :time ?t)))
	    )
  :hint (
	 (point (string "The problem gives you the direction of the relative velocity of ~a with respect to ~a ~a." ?b1 ?b2 (?t pp)))
	 ;(bottom-out (string "The problem specifies that the relative velocity of ~a with respect to ~a ~a is at ~a, so use ~A to draw that vector at ~a." 
	 ;		(*vector-tool* eval)
	 ;		?b1 ?b2 (?t pp) ?dir ?dir))
	 (bottom-out (string "The problem specifies that ~a is at ~a, so use ~A to draw that vector at ~a." 
			((relative-vel ?b1 ?b2 :time ?t) def-np) ?dir 
			(*vector-tool* eval) ?dir))
	 ))


(defoperator draw-rel-vel-vector-unknown (?b1 ?b2 ?t)
  :description "If the relative velocity vector of a body wrt to
                   something else is needed & the direction is not given, 
                   then draw it with an unknown direction"
  :preconditions 
  (
   (rdebug "Using draw-rel-vel-vector-unknown ~%")
   (time ?t)
   ;; relative velocity can be inferred from thrust force.
   ;; presence of thrust is indicated by specifying direction:
   (not (given (dir (force ?b2 ?b1 thrust :time ?t)) ?dir-force) 
	(not (equal ?dir-force 'unknown)))
   (not (given (dir (force ?b1 ?b2 thrust :time ?t)) ?dir-force) 
	(not (equal ?dir-force 'unknown)))
   (not (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dontcare1))
   (not (given (dir (relative-vel ?b1 ?b2 :time ?t)) ?dontcare))
   (not (given (mag (relative-vel ?b1 ?b2 :time ?t)) (dnum 0 ?units)))
   (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1) (body-name ?b2) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (rdebug "fired draw-rel-vel-vector-unknown  ~%")
   )
  :effects (
	    (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) unknown)
	    (variable ?mag-var (mag (relative-vel ?b1 ?b2 :time ?t)))
	    (variable ?dir-var (dir (relative-vel ?b1 ?b2 :time ?t)))
	    )
  :hint (
	 (point (string "You need to introduce a term for the relative velocity of ~A with respect to ~A ~A" ?b1 ?b2 (?t pp)))
	 (teach (string "If a vector angle requires calculation to determine, you should draw the vector in roughly the correct direction."))
	 ;(bottom-out (string "Use ~A to draw the relative velocity of ~a with respect to ~a ~A at an approximately correct angle."
	 ;		     (*vector-tool* eval)
	 ;		     ?b1 ?b2 (?t pp)))
	 (bottom-out (string "Use ~A to draw ~a at an approximately correct angle."
	 		     (*vector-tool* eval)
	 		     ((relative-vel ?b1 ?b2 :time ?t) def-np)))
	 ))

;; Draw relative velocity if associated thrust force is known 
(defoperator draw-relative-vel-given-thrust-force (?b1 ?b2 ?t)
  :preconditions
  (
   ;; BvdS:  why not this form for forces?
   ;;(in-wm (dir (force ?b1 ?b2 thrust :time ?t)))
   (force ?b2 ?b1 thrust ?t ?dir-force ?action)
   (test (not (eq ?dir-force 'unknown)))
   (not (given (dir (relative-vel ?b1 ?b2 :time ?t-given)) ?whatever)
	(tinsidep ?t ?t-given))
   (not (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dont-care)) ;not already drawn
   (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1) (body-name ?b2)
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir (opposite ?dir-force))
   (bind ?dir-var-value (dir-var-value ?dir))
    )
  :effects
   ((vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dir)
    ;; BvdS:  Why no equation for this?
    (variable ?mag-var (mag (relative-vel ?b1 ?b2 :time ?t)))
    (variable ?dir-var (dir (relative-vel ?b1 ?b2 :time ?t)))
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (relative-vel ?b1 ?b2 :time ?t)))
   )
  :hint
  ((point (string "Notice that ~a is moving relative to ~a." ?b1 ?b2))
   (teach (string "If ~A induces a thrust force on ~A, it must have a non-zero velocity relative to ~A." 
		 ?b1 ?b2 ?b2))
    ;(bottom-out (string "Use ~a to draw the relative velocity of ~a with respect to ~a ~a at ~a." 
    ;			(*vector-tool* eval)
    ;			?b1 ?b2 (?t pp) ?dir))
    (bottom-out (string "Use ~a to draw ~a at ~a." 
    			(*vector-tool* eval)
    			((relative-vel ?b1 ?b2 :time ?t) def-np) ?dir))
   ))

(defoperator write-relative-vel-compo (?b1 ?b2 ?b3 ?t ?xy ?rot)
  :preconditions 
  ((rdebug "Using write-relative-vel-compo ~%")
   (variable ?v12  (compo ?xy ?rot (relative-vel ?b1 ?b2 :time ?t)))
   (variable ?v23  (compo ?xy ?rot (relative-vel ?b2 ?b3 :time ?t)))
   (variable ?v13  (compo ?xy ?rot (relative-vel ?b1 ?b3 :time ?t)))
   (rdebug "fired write-relative-vel-compo  ~%")
   )
  :effects (
            (eqn (= ?v13 (+ ?v12 ?v23))
                 (compo-eqn relvel ?xy ?rot (relative-vel ?b1 ?b2 ?b3 ?t)))
            )
  :hint (
	 (point (string 
		 "Can you relate the following 3 vectors: the relative velocity of ~A wrt ~A, the relative velocity of ~a wrt ~a, and the relative velocity of ~a wrt ~A?"
		 ?b1 ?b3  ?b1 ?b2  ?b2 ?b3))
	 (teach (string "The relative velocity of a wrt c is equal to the vector sum of the relative velocity of a wrt b plus the relative velocity of b wrt c. In terms of components:<br>   Vac_x = Vab_x + Vbc_x<br>   Vac_y = Vab_y + Vbc_y."))
	 (bottom-out (string "Write the equation ~A"
			     ((= ?v13 (+ ?v12 ?v23)) algebra)))
	 ))

;;; This is from draw-zero-displacement.
(defoperator draw-zero-relative-vel (?b1 ?b2 ?t)
  :preconditions
  ((given (mag (relative-vel ?b1 ?b2 :time ?t-given)) (dnum 0 ?units))
   (time ?t)
   (test (tinsidep ?t ?t-given))
   (not (vector ?b1 (relative-vel ?b1 ?b2 :time ?t) ?dontcare))
   (bind ?mag-var (format-sym "V_~A_~A_~A" (body-name ?b1)
			      (body-name ?b2) (time-abbrev ?t)))
   )
  :effects
  ((vector ?b1 (relative-vel ?b1 ?b2 :time ?t) zero)
   (variable ?mag-var (mag (relative-vel ?b1 ?b2 :time ?t)))
   ;; Because mag is problem given, find-by-psm won't ensure 
   ;; implicit eqn gets written.  Given value may not be used 
   ;; elsewhere so ensure it here.
   ;; see draw-rel-vel-vector-given-dir
   (implicit-eqn (= ?mag-var (dnum 0 ?units)) 
   		 (mag (relative-vel ?b1 ?b2 :time ?t)))
   )
  :hint
  ((bottom-out (string "Since the problem specifies that the velocity of ~a relative to ~A is zero, just draw a zero-length vector for it." ?b1 ?b2))
   ))


;;; ============================ acceleration =================================
;;; This section contains operators that determine whether acceleration is zero
;;; or non-zero, and what direction the non-zero accelerations are.

;; This operator draws a zero acceleration vector for a body that is
;; at rest during a time period that includes the desired time.  Because
;; the vector has zero length, no direction variable is defined for it.
;; Note this means "at-rest" should only be used for a time instant if
;; the acceleration is zero at that instant.

(defoperator accel-at-rest (?b ?t)
  :description 
   "If a body is a rest, then it has zero acceleration."
  :preconditions
   ((motion ?b at-rest :time ?t-motion)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) zero))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (debug "~&Drawing zero accel for at-rest ~a at ~a.~%" ?b ?t)
    )
  :effects
  ((vector ?b (accel ?b :time ?t) zero)        
   (variable ?mag-var (mag (accel ?b :time ?t)))
   (given (mag (accel ?b :time ?t)) (dnum 0 |m/s^2|))
   (implicit-eqn (= ?mag-var (dnum 0 |m/s^2|)) (mag (accel ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is at rest ~a." ?b (?t pp)))
   (teach (kcd "draw_accel_when_at_rest")
          (string "If a body is at rest throughout some time interval, its average acceleration during that interval is zero."))
   (bottom-out (string "Because ~a is at rest ~a, use ~A to draw a zero-length acceleration vector for it." 
		       ?b (?t pp)
		       (*vector-tool* eval)))
   ))

(defoperator draw-accel-given-zero-net-force (?b ?t)
  :preconditions 
  (
   (net-force-zero ?b :time ?t-given)
   (time ?t)
   (test (tinsidep ?t ?t-given))
   ;; test for absence of applicable motion statement
   (not (motion ?b ?type :accel ?accel-motion :time ?t-motion . ?rest-motion) 
	(and (tinsidep ?t ?t-motion) 
	     (or (eq ?type 'at-rest) (eq ?accel-motion 'zero))))
   (bind ?mag-var (format-sym "a_~A~@[_~A~]" 
			      (body-name ?b) (time-abbrev ?t)))
    )
  :effects ((vector ?b (accel ?b :time ?t) zero)
	    (variable ?mag-var (mag (accel ?b :time ?t)))
	    (given (mag (accel ?b :time ?t)) (dnum 0 |m/s^2|))
            (implicit-eqn (= ?mag-var (dnum 0 |m/s^2|)) (mag (accel ?b :time ?t))))
  :hint 
  ((point (string "What do you know about the acceleration of ~A ~A?" 
		  ?b (?t pp)))
   (teach (string "If the total (net) force acting on an object is zero, according to Newton's second law, the acceleration of that object must also be zero."))
   (bottom-out (string "Draw a zero-length acceleration vector for ~A ~A." 
		       ?b (?t pp)))
   ))


;; This operator draws a zero acceleration vector for a body that is
;; moving in a straight line at constant speed during a time period that
;; includes the desired time.  

(defoperator accel-constant-speed (?b ?t)
  :description 
   "If ?body is moving in a straight line with constant speed during ?time,
   then its acceleration during ?time is zero."
  :preconditions
   ((motion ?b straight :accel zero :time ?t-motion . ?whatever)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) zero))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (debug "Drawing zero accel vector for constant-speed ~A at ~A.~%" ?b ?t)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) zero)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (given (mag (accel ?b :time ?t)) (dnum 0 |m/s^2|))
    (implicit-eqn (= ?mag-var (dnum 0 |m/s^2|)) (mag (accel ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is moving in a straight line at constant speed ~A."
		  ?b (?t pp)))
   (teach (minilesson "mini_zero_accel.htm")
          (kcd "draw_accel_straight_constant_speed")
	  (string "When a body is moving in a straight line at constant speed, then it has constant velocity and it has zero acceleration."))
   (bottom-out (string "Because ~a has constant velocity ~a, use ~A to draw a zero-length acceleration vector for it." 
		       (*vector-tool* eval)
		       ?b (?t pp)))
   ))

(defoperator draw-accel-potential-zero (?b ?t)
  :description 
   "If a body is a rest, then it has zero acceleration."
  :preconditions
   ((potential ?b :force-dir zero :time ?t-motion)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (at-place ?b ?loc :time ?t)
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (debug "~&Drawing zero accel for potential ~a at ~a.~%" ?b ?t)
    )
  :effects
  ((vector ?b (accel ?b :time ?t) zero)        
   (variable ?mag-var (mag (accel ?b :time ?t)))
   (given (mag (accel ?b :time ?t)) (dnum 0 |m/s^2|))
   (implicit-eqn (= ?mag-var (dnum 0 |m/s^2|)) (mag (accel ?b :time ?t))))
  :hint
  ((point (string "Notice that the potential has zero slope at ~A." ?loc))
   (teach (string "If a potential has zero slope, then it exerts no force."))
   (bottom-out (string "Use ~A to draw a zero-length acceleration vector for ~A ~A."
		       (*vector-tool* eval)
			?b (?t pp)))
   ))

(defoperator draw-accelerating (?b ?t)
  :preconditions
   ((motion ?b straight :accel ?dir :dir ?v-dir :time ?t-motion . ?whatever) 
    (test (degree-specifierp ?dir)) 
    ;; cases handled by draw-accel-speed-up and draw-accel-slow-down
    (test (not (degree-specifierp ?v-dir)))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) ?any-dir))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
   :hint
   ((point (string "Notice that the velocity of ~A is changing ~A." 
		   ?b (?t pp)))
    (teach (string "When a body is moving in a straight line and accelerating, its acceleration is either parallel or opposite to the line of motion."))
    (bottom-out (string "Because ~a is accelerating in direction ~a, you should use ~A to draw an acceleration for it ~a at direction ~a." 
			?b ?dir 
			(*vector-tool* eval)
			(?t pp) ?dir))
    ))


;;; This operator draws an non-zero acceleration vector for a body that 
;;; is moving in a straight line and speeding up.  The motion descriptor's 
;;; third argument is the direction of the object's velocity. 

(defoperator draw-accel-speed-up (?b ?t)
  :description 
   "If ?body is moving in a straight line during ?time,
      and it is speeding up,
      and the direction of motion ?direction,
   then draw a non-zero acceleration in ?direction during ?time."
  :preconditions
   ((motion ?b straight :accel ?dir :dir ?dir :time ?t-motion . ?whatever)
    (test (not (equal ?dir 'unknown)))  ; until conditional effects are implemented
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (debug "~&Drawing ~a accel for ~a at ~a.~%" ?dir ?b ?t)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
   :hint
   ((point (string "Notice that ~a is moving in a straight line and speeding up ~a" ?b (?t pp)))
    (teach (minilesson "mini_speedup_accel.htm")
           (kcd "draw_accel_straight_speeding_up")
	   (string "When a body is moving in a straight line and speeding up, its acceleration is parallel to the line of motion."))
    (bottom-out (string "Because ~a is speeding up while moving in a straight line with direction ~a, you should use ~A to draw an acceleration for it ~a at direction ~a." 
			?b ?dir
			(*vector-tool* eval)
			(?t pp) ?dir))
    ))

(defoperator draw-accel-potential (?b ?t)
  :preconditions
   (
    (potential ?b :force-dir ?dir :time ?t-motion)
    (test (not (equal ?dir 'zero))) 
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (at-place ?b ?loc :time ?t)
    (bind ?slope (if (< (get-angle-between '(dnum 0 |deg|) ?dir) '90) 
		     'decreasing 'increasing))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (debug "~&Drawing ~a accel for ~a at ~a.~%" ?dir ?b ?t)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
   :hint
   ((point (string "Notice that the potential at ~A is ~A as x increases."
		   ?loc (?slope adj)))
    (teach (string "The force is minus the derivative of the potential energy.  Thus, if the potential energy increases in a given direction, then the associated force vector points in the opposite direction.  "))
    ;(bottom-out (string "You should ~A to draw an acceleration for ~A ~a in the direction ~a." 
    ;			(*vector-tool* eval)
    ;			?b (?t pp) ?dir))
    (bottom-out (string "You should use ~A to draw ~a in the direction ~a." 
    			(*vector-tool* eval)
    			((accel ?b :time ?t) def-np) ?dir))
    ))

;;; This draws an acceleration vector at an unknown direction for an object 
;;; when we are given that there are at least two forces acting on it 
;;; (dt12b), but the exact direction of net force will not be known 
;;; until calculated.  The exact direction is unknown until components are 
;;; calculated. 
;;; !!! Might want operator and hint specific to case where existence of 
;;; more than one force in different directions is given. As it is it
;;; will appear you are just given that there is acceleration in some 
;;; straight line.

(defoperator draw-accel-unknown (?b ?t)
  :description 
   "If ?body is moving in a straight line during ?time,
      and it is subject to more than one given force,
   then draw a non-zero acceleration in ?direction during ?time."
  :preconditions
   (;; following tells us forces are not balanced, else would have at-rest
    (motion ?b straight :accel unknown :time ?t-motion . ?whatever)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    ;; find all forces that are acting on ?b (without drawing them)
    ;; and collect all distinct directions
    ;; This is really redundant with the motion statement above
    (setof (force ?b ?agent ?type ?t ?dir ?action) ?dir ?dirs)
    ;; forces with different directions are acting on ?b 
    (test (or (member 'unknown ?dirs) (> (length ?dirs) 1)))
    ;;
    (not (vector ?b (accel ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a accel for ~a at ~a.~%" ?dir ?b ?t)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) unknown)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t))))
   :hint
   ((point (string "Can you tell whether the acceleration of ~a will be zero or non-zero?" ?b))
    (teach (string "When a body is subject to a net force it will have an acceleration parallel to the vector sum of all forces. In this problem you should be able to see that there will be a net force on ~A so it will have a non-zero acceleration. But not all of the forces are known, so you should draw the acceleration at an approximate angle." ?b))
    ;(bottom-out (string "Use ~A to draw the acceleration for ~a ~A an an approximately correct direction." 
    ;			(*vector-tool* eval)
    ;			?b (?t pp)))
    (bottom-out (string "Use ~A to draw ~a in an approximately correct direction." 
    			(*vector-tool* eval)
    			((accel ?b :time ?t) def-np)))
    ))


;; draw acceleration when all we are given is its direction, and have no
;; other specification about the motion. Used in simple vector problems.
;; See bug #1899
(defoperator draw-accel-given-dir (?b ?t)
  :description 
   "If you are given the direction of acceleration at some time
   then draw an acceleration vector for it in the given direction."
  :preconditions
   ((in-wm (given (dir (accel ?b :time ?t-given)) ?dir))
    (test (not (equal ?dir 'unknown)))  
    (time ?t)
    (test (tinsidep ?t ?t-given))
    ;; make sure no other motion specification in problem for time
    ;; !! Too strict, some motion specs leave accel dir out.
    (not (motion ?b ?dontcare :time ?t-motion . ?whatever)
         (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
   ((vector ?b (accel ?b :time ?t) ?dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t)))
    ) 
   :hint
   ((point (string "The problem specifies the direction of the acceleration of ~a ~a." ?b (?t pp)))
    ;(bottom-out (string "The problem specifies that the acceleration of ~a ~a is at ~a, so just draw an acceleration vector oriented at ~a." ?b (?t pp) ?dir ?dir))
    (bottom-out (string "The problem specifies that ~a is at ~a, so just draw an acceleration vector oriented at ~a." ((accel ?b :time ?t) def-np) ?dir ?dir))
    ))

;; draw acceleration when all we are given is its direction, and have no
;; other specification about the motion. Used in simple vector problems.
;; See bug #1899
(defoperator draw-accel-given-dir-with-hint (?b ?t)
  :description 
   "If you are given the direction of acceleration at some time
   then draw an acceleration vector for it in the given direction."
  :preconditions
   ((in-wm (given (dir (accel ?b :time ?t-given)) ?dir :hint ?hints))
    (test ?hints)
    (test (not (equal ?dir 'unknown)))  
    (time ?t)
    (test (tinsidep ?t ?t-given))
    ;; make sure no other motion specification in problem for time
    ;; !! Too strict, some motion specs leave accel dir out.
    (not (motion ?b ?dontcare :time ?t-motion . ?whatever)
         (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
    (bind ?hint1 (car ?hints))
    (bind ?hint2 (second ?hints))
    (bind ?hint3 (third ?hints))
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t)))
    ) 
   :hint
   ((point (string "~a" (?hint1 identity)))
    (teach (string "~a" (?hint2 identity)))
    (bottom-out (string "~a" (?hint3 identity))))
   )

;; draw average acceleration when we know that the motion is 
;; curved (from a motion statement), but no direction is given.
(defoperator draw-accel-curved-unknown (?b ?t)
  :preconditions
   (
    (time ?t)
    ;; this is redundant with excluding curved :type projectile
    (not (free-fall ?b :time ?tfree) (tinsidep ?t ?tfree))		
    (motion ?b curved :type ?type :dir ?dir-spec :time ?t-motion . ?rest)
    (test (tinsidep ?t ?t-motion))
    ;; However, the direction is known for projectile motion.
    ;; This should also be clear from the ?dir-spec but the 
    ;; ?dir-spec is not specified consistantly on all problems.
    (test (not (eq ?type 'projectile)))
    ;; the acceleration direction is nil or 'unknown
    ;; may conflict for cases where nil != unknown; see problem kt12a
    (test (or (null ?dir-spec) (eq ?dir-spec 'unknown)))
    (object ?b) ;sanity test
    (not (vector ?b (accel ?b :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "a_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var)))
  :effects
   ((vector ?b (accel ?b :time ?t) unknown)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    ) 
   :hint
   ((point (string "Note that ~A is not moving in a straight line ~A." 
		   ?b (?t pp)))
    (teach (string "If an object is moving in a curved path, then its velocity is changing.  Thus, it has a non-zero acceleration." ?b))
    (bottom-out (string "Define an acceleration vector for ~a ~A.  Since its direction may not be clear from the problem statement, treat the direction as unknown." ?b (?t pp)))
    ))


;;; This operator draws an non-zero acceleration vector for a body that is 
;;; moving in a straight line and slowing down.  The motion descriptor's third 
;;; argument is the direction of the object's velocity.  We reverse it here 
;;; because the object acceleration is in the opposite direction from its 
;;; motion.

;; Note: we write out structured direction term in effects and to prevent
;; effect from unifying with NTL precond (vector ?b (accel ?b :time ?t) zero). 
;; When we had (vector ... ?dir) this operator would be tried
;; with ?accel-dir bound to 'zero coming in, causing error when we 
;; attempt to bind ?accel-dir.
(defoperator draw-accel-slow-down (?b ?t)
 :description 
   "If ?body is moving in a straight line and slowing down during ?time,
   then its acceleration is opposite its direction of motion."
  :preconditions
   ((motion ?b straight :accel ?accel-dir :dir ?motion-dir :time ?t-motion . ?rest)
    (test (degree-specifierp ?motion-dir)) 
    (test (equal ?accel-dir (opposite ?motion-dir)))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?accel-dir))
    (debug "~&Drawing ~a vector for accel of ~a at ~a.~%" ?accel-dir ?b ?t)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?accel-dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?accel-dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is slowing down as it moves in a straight line ~a" ?b (?t pp)))
   (teach (minilesson "mini_slowdown_accel.htm")
          (kcd "draw_accel_straight_slowing_down")
	  (string "When a body is slowing down as it moves in a straight line, it is decelerating, which means that its acceleration is in the opposite direction from its motion."))
   (bottom-out (string "Because ~a is slowing down as it moves in a straight line, draw an acceleration vector for it at ~a.  It should have a direction of ~a because that is the opposite direction from its motion." 
		       ?b (?t pp) (?accel-dir adj)))
   ))
  

;; This operator draws the acceleration vector for a freely falling body. 
;; This must be given in the problem statement as (free-fall body :time time)
;; The acceleration is non-zero straight down, on the assumption that
;; the relevant planet is always straight down in the diagram.
;; The free-fall law will specify an equation for the magnitude of the 
;; acceleration
;;
(defoperator draw-accel-free-fall (?b ?t)
  :description 
   "If ?body is in free-fall during ?time,
   then draw a non-zero acceleration straight down during ?time."
  :preconditions
   ((free-fall ?b :time ?t-motion)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (accel ?b :time ?t) ?dir))
    (bind ?mag-var (format-sym "a_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing free-fall accel at 270 for ~a at ~a.~%" ?b ?t)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) (dnum 270 |deg|))
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) (dnum 270 |deg|))
    (implicit-eqn (= ?dir-var (dnum 270 |deg|)) (dir (accel ?b :time ?t)))
    (constant (accel ?b) ?t)
   )
   :hint
   ((point (string "Notice that ~a is a freely falling body ~a" ?b (?t pp)))
    (teach (kcd "draw_accel_freefall")
	   (string "When a body is in free fall, it undergoes acceleration due to gravity directed toward the center of the planet exerting the gravitational force on it. This will be straight down in the diagrams for Andes problems."))
    (bottom-out (string "Because ~a is accelerating due to gravity, you should use ~A to draw an acceleration for it ~a in the direction 270 degrees." 
			?b (*vector-tool* eval)
			(?t pp)))
    ))

;;; right now (motion ?body curved :type projectile ...) means all 
;;; non-circular curved motion.  So automatically entailing 
;;; (free-fall ...) is inappropriate
;;; perhaps we should rename projectile to "nil" or "unknown"
#| 
;; projectile motion entails (free-fall ...)
;; we might want to get rid of (free-fall ...) entirely 
(defoperator projectile-motion-is-free-fall (?b ?t)
  :preconditions ((motion ?b curved :type projectile :time ?t . ?whatever)
		  (test (time-intervalp ?t)))
  :effects
   ((free-fall ?b :time ?t)))
|#

;;;
;;; free-fall equation: acceleration = g
;;;

(defoperator free-fall-accel-contains (?quantity)
  :description 
  "if an object is in free-fall near a planet during an interval, 
  then the equation for free-fall acceleration may be used to relate
  the body's acceleration and the gravitational acceleration for the planet"
  :preconditions
  ((any-member ?quantity
	       ((mag (accel ?b :time ?t))
		(gravitational-acceleration ?planet)))
   (free-fall ?b :time ?t-free)
   (time ?t)
   (test (tinsidep ?t ?t-free))
   (near-planet ?planet :body ?b ?b))
  :effects
  ((eqn-contains (free-fall-accel ?b ?t) ?quantity)))

;;; This operator writes the equation a = g, where a is the magnitude of the 
;;; acceleration of the body. g is a variable for the gravitational accel
;;; of the relevant planet.
(defoperator write-free-fall-accel (?b ?t)
  :description 
  "if an object is in free-fall near a planet during an interval, 
  then for any interior time period,
     the magnitude of its acceleration equals the gravitational acceleration for
     that planet"
  :preconditions
  ((variable ?accel-var (mag (accel ?b :time ?t)))
   (variable ?g-var (gravitational-acceleration ?planet))
   )
  :effects
  ((eqn (= ?accel-var ?g-var) (free-fall-accel ?b ?t)))
  :hint
  ((teach (string "If an object is in free-fall near a planet, its acceleration equals the acceleration due to gravity for that planet. The variable <var>g</var> is predefined in Andes to denote the magnitude of the gravitational acceleration, so you don't have to define <var>g</var> before you use it. However, you will have to enter an equation giving the value of <var>g</var>."))
   (bottom-out (string "Write the equation ~A." ((= ?accel-var ?g-var) algebra)))
   ))


;;; This operator draws the instantaneous acceleration vector for a body in 
;;; uniform circular motion.  This must be given in the problem statement as 
;;; (motion ?body curved :type circular :dir ?dir :accel ?accel-dir :time ?t)
;;; where tangent-dir is the direction of the velocity at that time.
;;; It time is an interval, then the angle spec may be ignored, since it
;;; is no longer well-defined.
;;;
;;; The acceleration is orthogonal to the velocity towards the center of the
;;; circle; we include this direction in the motion spec because velocity
;;; direction alone doesn't suffice to fully characterize the motion.
;;; The centripetal acceleration law will specify an equation for the 
;;; magnitude of the acceleration.
(defoperator draw-centripetal-acceleration (?B ?t)
  :description 
   "If ?body is in uniform circular motion during ?time,
   then draw a non-zero acceleration perpendicular to the velocity at ?time."
  :preconditions
   ((motion ?b curved :type circular :accel ?accel-dir :time ?t-motion . ?rest)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (test (time-pointp ?t))
    (test (degree-specifierp ?accel-dir))  ;direction is known
    (not (vector ?b (accel ?b :time ?t) ?dontcare))
    (bind ?mag-var (format-sym "a_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?accel-dir))
    (debug "~&Drawing centripetal accel at ~A for ~a at ~a.~%" ?b ?t ?accel-dir)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?accel-dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?accel-dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
   :hint
   ((point (string "Notice that ~a is in uniform circular motion ~a" ?b (?t pp)))
    (teach (kcd "draw_accel_circular_constant_speed")
	   (string "When a body is in uniform circular motion, its acceleration is directed towards the center of the circle."))
    (bottom-out (string "Because ~a is in uniform circular motion you should use ~A to draw an acceleration for it ~a at direction ~A." 
			?b (*vector-tool* eval)
    			(?t pp) (?accel-dir adj)))
    ))

;;; for Pyrenees missle problem
;;; draw acceleration for a curved projectile trajectory when we are given 
;;; its direction.
;;; This differs from draw-accel-given-dir since the dir is in 
;;; the projectile motion spec
;;; Like draw-centripetal-accel in pulling dir from curved motion spec, 
;;; differing only in that it does not assume uniform circular motion.
;; See bug #1899
(defoperator draw-accel-projectile-given-dir (?b ?t)
   :preconditions 
   (
    (motion ?b curved :type projectile :accel ?accel-dir :time ?t-motion . ?rest)
    (test (degree-specifierp ?accel-dir))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    ;; should we test that free-fall is not specified? Assume we won't
    ;; have this motion spec in that case.
    (not (vector ?b (accel ?b :time ?t) ?dontcare))
    (bind ?mag-var (format-sym "a_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?accel-dir))
    (debug "~&Drawing projectile accel at ~A for ~a at ~a.~%" ?b ?t ?accel-dir)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?accel-dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?accel-dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
   :hint
    ((point (string "The problem specifies the direction of the acceleration of ~a ~a." ?b (?t pp)))
    ;(bottom-out (string "The problem specifies that the acceleration of ~a ~a is at ~a, so just draw an acceleration vector oriented at ~a." 
    ;                  ?b (?t pp) (?accel-dir adj) (?accel-dir adj)))
    (bottom-out (string "The problem specifies that ~a is at ~a, so just draw an acceleration vector oriented at ~a." 
                      ((accel ?b :time ?t) def-np) (?accel-dir adj) (?accel-dir adj)))
    ))

;; If all the forces are in the same direction then we can infer the 
;; direction of acceleration.
(defoperator draw-accel-given-parallel-forces (?b ?t)
   :preconditions 
   (
    ;; we have special rules to handle straight line and circular motion
    ;; This motion statement implies all other possibilities
    (motion ?b curved :type projectile :time ?t-motion . ?dirstuff)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    ;; we have special rules for the case of gravitational acceleration
    (not (free-fall ?b :time ?t-freefall) (tinsidep ?t ?t-freefall))
    ;; find all forces that are acting on ?b and collect distinct directions
    (setof (force ?b ?agent ?type ?t ?dir ?action) ?dir ?dirs)
    ;; test that all the directions are the same
    (test (null (rest ?dirs))) ;setof only gives distinct matches to ?dir
    (bind ?accel-dir (first ?dirs))
    (test (degree-specifierp ?accel-dir)) ;exclude 'zero and 'unknown
    (not (unknown-forces :body ?b ?b :time ?t ?t)) ;only valid if all forces are specified
    (not (vector ?b (accel ?b :time ?t) ?dontcare))
    (bind ?mag-var (format-sym "a_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?accel-dir))
    (debug "~&Drawing projectile accel at ~A for ~a at ~a.~%" ?b ?t ?accel-dir)
    )
  :effects
   ((vector ?b (accel ?b :time ?t) ?accel-dir)
    (variable ?mag-var (mag (accel ?b :time ?t)))
    (variable ?dir-var (dir (accel ?b :time ?t)))
    (given (dir (accel ?b :time ?t)) ?accel-dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (accel ?b :time ?t))))
   :hint
    ((point (string "The force(s) acting on ~A ~A point(s) in the direction ~A." ?b (?t pp) (?accel-dir adj)))
    (teach (string "Newton's second law F=ma relates the net force and acceleration vectors.  If you know the direction of the net force, then you can find the direction of the acceleration.")) 
    ;(bottom-out (string "Draw the acceleration of ~a ~a at an angle of ~a." 
    ;                  ?b (?t pp) (?accel-dir adj)))
    (bottom-out (string "Draw ~a at an angle of ~a." 
                      ((accel ?b :time ?t) def-np) (?accel-dir adj)))
    ))

;;;
;;; centripetal acceleration law: acceleration = v^2/r
;;;
(defoperator centripetal-accel-contains (?quantity)
  :description 
  "if an object is in uniform circular motion during an interval, 
  then the equation for centripetal acceleration may be used to relate
  the body's acceleration and its velocity and radius of circular motion"
  :preconditions
  (
   (any-member ?quantity
	       ((mag (accel ?b :time ?t))
		(mag (velocity ?b :time ?t))
		(revolution-radius ?b :time ?t)))
   (motion ?body curved :type circular :time ?t-motion . ?dontcare)
   (test (tinsidep ?t ?t-motion))
   )
  :effects
  ((eqn-contains (centripetal-accel ?b ?t) ?quantity)))

;;; This operator writes the equation a = v^2/r, where a is the magnitude 
;;; of the acceleration of the body. 
(defoperator write-centripetal-accel (?b ?t)
  
  :description 
  "if an object is in uniform circular motion during an interval, 
  then for any interior time instant,
     the magnitude of its acceleration equals the velocity squared 
     divided by the radius of circular motion"
  :preconditions
   (
   (variable ?accel-var (mag (accel ?b :time ?t)))
   (variable ?vel-var (mag (velocity ?b :time ?t)))
   (variable ?radius-var (revolution-radius ?b :time ?t))
   )
  :effects
  ((eqn (= ?accel-var (/ (^ ?vel-var 2) ?radius-var)) 
	(centripetal-accel ?b ?t)))
  :hint
  ((point (string "Notice that ~a is moving in uniform circular motion." ?b))
   (teach (kcd "centripetal_PSM")
	  (string "If an object is in uniform circular motion, its acceleration equals the velocity squared divided by the radius of circular motion."))
   (bottom-out (string "Because ~a is moving in a circle of radius ~a with velocity ~a, its acceleration is ~a."  ?b (?radius-var algebra) (?vel-var algebra) ((= ?accel-var (/ (^ ?vel-var 2) ?radius-var)) algebra)))
   ))

(defoperator centripetal-accel-vector-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ((accel ?b :time ?t)
			(velocity ?b :time ?t)
			(revolution-radius ?b :time ?t)))
   (motion ?body curved :type circular :time ?t-motion . ?dontcare)
   (test (tinsidep ?t ?t-motion))
   )
  :effects 
   ((eqn-family-contains (centripetal-accel-vec ?b ?t) ?sought)
    ;; since only one compo-eqn under this vector psm, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (centripetal-accel-vec ?b ?t) definition ?sought)))

(defoperator draw-centripetal-accel-vector-diagram (?rot ?b ?t)
  :preconditions 
  (
   (body ?b)
   (inherit-vector ?b (accel ?b :time ?t) ?dir1)
   (axes-for ?b ?rot)
   )
  :effects ( (vector-diagram ?rot (centripetal-accel-vec ?b ?t)) ))

(defoperator write-centripetal-accel-compo (?b ?t ?xy ?rot)
  :preconditions 
  (
   ;; make sure r-hat compo doesn't vanish
   (in-wm (inherit-vector ?b (accel ?b :time ?t) ?a-dir))
   (test (non-zero-projectionp ?a-dir ?xy ?rot))
   (inherit-variable ?a_xy (compo ?xy ?rot (accel ?b :time ?t)))
   (variable ?vel-var (mag (velocity ?b :time ?t)))
   (variable ?radius-var (revolution-radius ?b :time ?t))
   ;; Ideally, the radius direction would be given by relative-position
   ;; insead, we have revolution-radius.  
   ;; Thus, we are forced to use the direction of the acceleration itself:
   (bind ?r-dir (opposite ?a-dir))
   (test (degree-specifierp ?r-dir)) ;is a numerical value
   (bind ?trig (if (eq ?xy 'y) 'sin 'cos))
   (bind ?axis-angle (axis-dir 'x ?rot))
   (bind ?term `(dnum ,(- (second ?r-dir) ?axis-angle) |deg|))
   )
  :effects 
  ((eqn (= ?a_xy (* (- (/ (^ ?vel-var 2) ?radius-var)) (?trig ?term)))
	(compo-eqn definition ?xy ?rot (centripetal-accel-vec ?b ?t)))
   )
  :hint
  ((point (string "Notice that ~a is moving in uniform circular motion." ?b))
   (teach (kcd "centripetal_PSM")
	  (string "If an object is in uniform circular motion, its acceleration equals the velocity squared divided by the radius of circular motion.  The acceleration is always pointing towards the center of the circle."))
   (bottom-out (string "Write the equation ~A." 
		       ((= ?a_xy (* (- (/ (^ ?vel-var 2) ?radius-var)) 
				    (?trig ?term))) algebra)))
   ))

;;; define a variable for the revolution radius = radius of uniform circular
;;; motion. Note no time on this quantity in the workbench; OK, all our
;;; problems use the default time instant.
(defoperator define-revolution-radius (?b ?t)
  :preconditions 
  ((object ?b)
   (time ?t)
   (bind ?radius-var (format-sym "r_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))) 
  :effects 
  ((variable ?radius-var (revolution-radius ?b :time ?t))
   (define-var (revolution-radius ?b :time ?t))) 
  :hint 
  ((bottom-out (string "Use ~A to define ~A." 
			(*text-tool* eval)
		       ((revolution-radius ?b :time ?t) def-np)))))

; Can optionally introduce variable for revolution radius by using a tool
; to put a radius graphic on the diagram. This is a special purpose tool, 
; not one of the vector drawing tools.  No properties of the drawing are 
; checkable.  It's just a graphic way of introducing the radius variable.
; Since we don't care about the difference, we just assimilate a drawn
; radius and a defined radius variable as the same entry. 

;;
;; period of uniform circular motion = 2*pi*r/v  
;;
;; Period is time to make one revolution. This equation is just a 
;; special case of sdd: time = distance/speed. But to use the sdd rule 
;; for this we would need to define a starting and ending time, and represent
;; the fact that the object makes one revolution in this interval, then add 
;; a rule to deduce that the distance traveled in this interval = 2*pi*r.
;; We would also need a rule that avg speed = mag velocity at any time for 
;; an object in circular motion.
;;

(defoperator period-circular-contains (?sought)
  :preconditions (
     (motion ?b curved :type circular :time ?t-circular . ?dontcare)
     (any-member ?sought (
                       (period ?b)
		       (revolution-radius ?b :time ?t)
		       (mag (velocity ?b :time ?t))
                         ))
    (time ?t)
    (test (tinsidep ?t ?t-circular))
  )
  :effects (
   (eqn-contains (period ?b ?t circular) ?sought)
  ))

(defoperator write-period-circular (?b ?t)
   :preconditions (
      ;; make sure body is drawn if it hasn't been drawn for something else
      (body ?b) 
      (variable ?T-var    (period ?b))
      (variable ?r    (revolution-radius ?b :time ?t))
      (variable	?v    (mag (velocity ?b :time ?t)))
   )
   :effects (
     (eqn (= ?T-var (/ (* 2 |\\pi| ?r) ?v)) (period ?b ?t circular))
   )
   :hint (
      (teach (string "The period of an object in circular motion is the time to make one complete revolution. This time is equal to the distance traveled, which is 2 &pi; times the radius of the circle, divided by the speed."))
      (bottom-out (string "Write the equation ~A" 
                          ((= ?T-var (/ (* 2 |\\pi| ?r) ?v)) algebra)))
   )
)


;;; =============== linear kinematics compo equations ==============
;;; The physicist do not want Andes to hint s=vf*t-0.5*a*t^2 (leaves
;;; out vi), so that equation is left out.  The other four are here,
;;; expressed with two operators each.  One lists the quantities
;;; contained in the equation, and the other writes the equation.

(def-psmgroup lk	
    :form (?eq-type ?Eqn-ID ?axis ?rot (lk ?body ?time))
    :supergroup Kinematics
    :doc "Equations for one dimensional motion with constant acceleration."
    :nlg-english ("a constant acceleration equation"))

(def-goalprop lk-eqn-chosen
      (compo-eqn-selected (LK ?body ?time) ?quantity (compo-eqn . ?eq-args))
   :nlg-english ("choosing a particular kinematic equation containing ~A" 
             (nlg ?quantity)))

;;; This operator writes vf=vi+a*t.  That is, it leaves out displacement (s).

;;; Acceleration over an interval is interpreted as average acceleration.
;;; This is consistent with the labels in the Andes dialog boxes.
;;; We use the proposition (constant (accel ?b) (during ?t1 ?t2)) to 
;;; assert that the acceleration is constant over each instant 
;;; in an interval.  In the case of free-fall, this is inferred. 
;;; In other cases it must be given. 
;;;

(def-psmclass lk-no-s (?eq-type lk-no-s ?axis ?rot (lk ?body (during ?time0 ?time1))) 
  :group lk
  :complexity major
  :nlg-english ("the definition of average acceleration")
  :tutorial "AverageAcceleration.html"
  :ExpFormat ("applying the definition of average acceleration on ~a from ~a to ~a"
		 (nlg ?body) (nlg ?time0 'moment) (nlg ?time1 'moment))
  ;; alternative form in principles.cl
  :EqnFormat ("vf<sub>~A</sub> = vi<sub>~A</sub> + a(avg)<sub>~A</sub> t" (axis-name ?axis) (axis-name ?axis)
	      (axis-name ?axis)))

(def-goalprop lk-no-s-eqn (eqn ?algebra (compo-eqn lk-no-s ?axis ?rot (lk ?body ?time)))
  :nlg-english ((strcat "writing the definition of average acceleration equation "
		    "in terms of vector components along the ~A axis") 
	    (axis-name ?axis)))

(defoperator LK-no-s-contains (?quantity)
  :description 
   "Lists the quantities contained in vf = vi + a * t"
  :preconditions
  (
   ;; vector PSM uses wm-or-derive for compo-eqn-contains so this operator
   ;; will only be called once
   (any-member ?quantity  ((velocity ?b :time ?t1)
			   (velocity ?b :time ?t2)
			   (accel ?b :time (during ?t1 ?t2))
			   (duration (during ?t1 ?t2))
			   ))
   (object ?b) ;in case ?b is not bound
   (time (during ?t1 ?t2))	; ensure both endpoints to try bound
   )
  :effects
  ((eqn-family-contains (lk ?b (during ?t1 ?t2)) ?quantity)
    (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) lk-no-s ?quantity)))

(defoperator draw-lk-no-s-fbd (?b ?t1 ?t2 ?rot)
  :description "
   If the goal is to draw a lk fbd for lk-no-s
   then draw the body, the initial and final velocity, 
      the acceleration, and axes"
  :preconditions
  ((in-wm (compo-eqn-contains (lk ?b (during ?t1 ?t2)) lk-no-s ?quantity))
   (body ?b)
   (inherit-vector ?b (velocity ?b :time ?t1) ?dir1)
   (inherit-vector ?b (velocity ?b :time ?t2) ?dir2)
   (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?dir3)
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (lk ?b (during ?t1 ?t2))))
)

(defoperator write-lk-no-s-compo (?b ?t1 ?t2 ?xyz ?rot)
  :description "
   writes vf=vi+a*t.  That is, it leaves out displacement (s)."
  :preconditions
   (;; make sure accel compo doesn't vanish
    (in-wm (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?accel-dir))
    (test (non-zero-projectionp ?accel-dir ?xyz ?rot))
    (inherit-variable ?vi-compo (compo ?xyz ?rot (velocity ?b :time ?t1)))
    (inherit-variable ?vf-compo (compo ?xyz ?rot (velocity ?b :time ?t2)))
    (inherit-variable ?a-compo  (compo ?xyz ?rot 
				       (accel ?b :time (during ?t1 ?t2))))
    (variable ?t (duration (during ?t1 ?t2))))
  :effects
  (
   (eqn (= ?vf-compo (+ ?vi-compo (* ?a-compo ?t)))
	(compo-eqn lk-no-s ?xyz ?rot (lk ?b (during ?t1 ?t2))))
   )
  :hint
   ((point (string "Can you think of an equation that relates the components of average acceleration to those of the initial velocity, final velocity, and duration?"))
    (teach (string "Acceleration is the rate of change of velocity.  The average acceleration vector over some time is defined as the difference between initial and final velocity vectors divided by the duration."))
    (bottom-out (string "Write the equation ~a = ~a" 
			(?vf-compo algebra) 
			((+ ?vi-compo (* ?a-compo ?t)) algebra)))
    ))

;;; zero component of non-zero acceleration
;;
;; This is given a separate psmclass from lk-no-s to make it
;; parallel with sdd-constvel.  It might be better to group this
;; with lk-no-s as part of a common eqn-family (like NFL and NSL
;; are parts of NL). 
(def-psmclass const-v (?eq-type const-v ?axis ?rot 
				(lk ?body (during ?time0 ?time1))) 
  :group lk
  :complexity minor
  :nlg-english ("constant velocity")
  :tutorial "ConstantVelocityComponent.html"
  :ExpFormat ("noting that the accelaration ~A has no ~A component between ~a to ~a, so the ~A component of velocity remains constant"
	      (nlg ?body) (axis-name ?axis) (nlg ?time0 'moment) 
	      (nlg ?time1 'moment) (axis-name ?axis))
  :short-name ("[a<sub>~A</sub>=0]" (axis-name ?axis))
  ;; alternative form in principles.cl
  :EqnFormat ("vf<sub>~A</sub> = vi<sub>~A</sub>" (axis-name ?axis) (axis-name ?axis)))

(defoperator const-v-contains (?quantity)
  :preconditions
  (
   ;; vector PSM uses wm-or-derive for compo-eqn-contains so this operator
   ;; will only be called once
   (any-member ?quantity  ((velocity ?b :time ?t1)
			   (velocity ?b :time ?t2)
			   ))
   (time (during ?t1 ?t2))	; ensure both endpoints to try bound
   )
  :effects
  ((eqn-family-contains (lk ?b (during ?t1 ?t2)) ?quantity)
    (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) const-v ?quantity)))

(defoperator draw-const-v-fbd (?b ?t1 ?t2 ?rot)
  :description "
   If the goal is to draw a lk fbd for lk-no-s
   then draw the body, the initial and final velocity, 
      the acceleration, and axes"
  :preconditions
  ((in-wm (compo-eqn-contains (lk ?b (during ?t1 ?t2)) const-v ?quantity))
   (body ?b)
   (inherit-vector ?b (velocity ?b :time ?t1) ?dir1)
   (inherit-vector ?b (velocity ?b :time ?t2) ?dir2)
   (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?dir-a)
   ;; the case of zero acceleration is better handled by inheritance
   (test (not (eq ?dir-a 'zero)))
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (lk ?b (during ?t1 ?t2))))
)

(defoperator write-const-v (?b ?t1 ?t2 ?xyz ?rot)
  :preconditions 
  (
   (in-wm (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?accel-dir))
   (test (not (non-zero-projectionp ?accel-dir ?xyz ?rot)))
   (inherit-variable ?v1-compo (compo ?xyz ?rot (velocity ?b :time ?t1)))
   (inherit-variable ?v2-compo (compo ?xyz ?rot (velocity ?b :time ?t2)))
   )
  :effects
  ((eqn (= ?v1-compo ?v2-compo) 
               (compo-eqn const-v ?xyz ?rot (lk ?b (during ?t1 ?t2))))
   )
  :hint
  ((point (string "What happens to the ~A-component of the velocity of ~A ~A?"
		  ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		  ?b ((during ?t1 ?t2) pp)))
   (teach (string "Because the acceleration of ~A ~A is perpendicular to the ~A axis, is has no component in the ~A direction.  Therefore, the ~A component of velocity remains constant. You can use this to relate ~A to ~A." 
		  ?b ((during ?t1 ?t2) pp)  
		  ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		  ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		  ((axis ?xyz ?rot) symbols-label :namespace :objects)
		  ((during ?t1 ?t2) pp) 
		  (?v1-compo algebra) (?v2-compo algebra)))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?v1-compo ?v2-compo) algebra)))
   ))


;;; Writes the equation vf^2 = vi^2 + 2*a*s, which is lacking a duration.


(def-psmclass lk-no-t (?eq-type lk-no-t ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :short-name ("[a<sub>~A</sub> is constant]" (axis-name ?axis))
  :tutorial "ConstantAcceleration.html"
  :doc "Linear kinematics eqn w/o time."
  :nlg-english ("the constant acceleration equation v<sub>~a</sub><sup>2</sup> = v0<sub>~a</sub><sup>2</sup> + 2 a<sub>~a</sub> d<sub>~a</sub>" 
               (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) (axis-name ?axis))
  :ExpFormat ((strcat "writing the constant acceleration equation "
                      "v<sub>~a</sub><sup>2</sup> = v0<sub>~a</sub><sup>2</sup> + 2 a<sub>~a</sub> d<sub>~a</sub> "
		      "for ~a from ~a to ~a") 
                      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)
		      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("v<sub>~a</sub><sup>2</sup> = v0<sub>~a</sub><sup>2</sup> + 2 a<sub>~a</sub> d<sub>~a</sub>" (axis-name ?axis) (axis-name ?axis)
					       (axis-name ?axis) (axis-name ?axis)))

(def-goalprop lk-no-t-eqn 
   (eqn ?algebra (compo-eqn lk-no-t ?axis ?rot (lk ?body ?time)))
   :nlg-english ("writing a constant acceleration equation in terms of vector components along the ~A axis" ?axis))

(defoperator LK-no-t-contains (?quantity)
  :description "
   Lists the quantities contained in vf^2 = vi^2+2*a*s"
  :preconditions
  (
   ;; vector PSM uses wm-or-derive for compo-eqn-contains so this operator
   ;; will only be called once
   (any-member ?quantity 
	       ((velocity ?b :time ?t1)
		 (velocity ?b :time ?t2)
		 (accel ?b :time (during ?t1 ?t2))
		 (displacement ?b :time (during ?t1 ?t2))
		 ))
   ;; only applies if accel is constant within interval we are using
   ;; sought may not bind t1 & t2, so choose endpoints of interval to try
   (constant (accel ?b) ?t-constant . ?whatever)
   (time (during ?t1 ?t2))	; ensure both endpoints to try bound
   (test (tinsidep '(during ?t1 ?t2) ?t-constant))
   )
  :effects
   ((eqn-family-contains (lk ?b (during ?t1 ?t2)) ?quantity)
    (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) lk-no-t ?quantity)))

(defoperator draw-lk-no-t-fbd (?b ?t1 ?t2 ?rot)
  :description "
   If the goal is to draw a lk fbd for lk-no-s
   then draw the body, the initial and final velocity, 
      the acceleration, and axes"
  :preconditions
  ((in-wm (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) lk-no-t ?quantity))
   (body ?b)
   (inherit-vector ?b (velocity ?b :time ?t1) ?dir1)
   (inherit-vector ?b (velocity ?b :time ?t2) ?dir2)
   (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?dir3)
   (vector ?b (displacement ?b :time (during ?t1 ?t2)) ?dir4)
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (lk ?b (during ?t1 ?t2))))
)

(defoperator write-lk-no-t-compo (?b ?t1 ?t2 ?xyz ?rot)
  :description "
   Writes the equation vf^2 = vi^2 + 2*a*s, which is lacking a duration."
  :preconditions
   (
    ;; make sure accel compo doesn't vanish
    (in-wm (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?accel-dir))
    (test (non-zero-projectionp ?accel-dir ?xyz ?rot))
    (inherit-variable ?vi-compo (compo ?xyz ?rot (velocity ?b :time ?t1)))
    (inherit-variable ?vf-compo (compo ?xyz ?rot (velocity ?b :time ?t2)))
    (inherit-variable ?a-compo  (compo ?xyz ?rot (accel ?b :time (during ?t1 ?t2))))
    (variable ?s-compo  (compo ?xyz ?rot (displacement ?b :time (during ?t1 ?t2))))
    )
  :effects
  ((eqn (= (^ ?vf-compo 2) (+ (^ ?vi-compo 2) (* 2 ?a-compo ?s-compo)))
	        (compo-eqn lk-no-t ?xyz ?rot (lk ?b (during ?t1 ?t2))))
   )
  :hint (
    (point (string "Do you know an equation relating the components of initial velocity, final velocity, acceleration, and displacement when acceleration is constant?"))
    (bottom-out (string "Write the equation ~A" 
			((= (^ ?vf-compo 2) (+ (^ ?vi-compo 2) 
					       (* 2 ?a-compo ?s-compo))) algebra)))
  ))

;;; Writes the equation s = vi*t + 0.5*a*t^2, which lacks vf

(def-psmclass lk-no-vf (?eq-type lk-no-vf ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :short-name ("[a<sub>~A</sub> is constant]" (axis-name ?axis))
  :tutorial "ConstantAcceleration.html"
  :Doc "Linear kinematics eqn sans final velocity."
  :nlg-english ("the constant acceleration equation d<sub>~a</sub> = v0<sub>~a</sub> t + 0.5 a<sub>~a</sub> t<sup>2</sup>"
                            (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) )
  :ExpFormat ((strcat "writing the constant acceleration equation "
                      "d<sub>~a</sub> = v0<sub>~a</sub> t + 0.5 a<sub>~a</sub> t<sup>2</sup> "
		      "for ~a from ~a to ~a") 
                      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) 
		      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("d<sub>~a</sub> = v0<sub>~a</sub> t + 0.5 a<sub>~a</sub> t<sup>2</sup>" (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))

(def-goalprop lk-no-vf-eqn 
   (eqn ?algebra (compo-eqn lk-no-vf ?axis ?rot (lk ?body ?time)))
  :nlg-english ("writing a constant acceleration equation in terms of vector components along the ~A axis" ?axis))

(defoperator LK-no-vf-contains (?quantity)
  :description "
   Lists the quantities contained in s = vi*t + 0.5*a*t^2"
  :preconditions
  (
   ;; vector PSM uses wm-or-derive for compo-eqn-contains so this operator
   ;; will only be called once
   (any-member ?quantity 
	        ((velocity ?b :time ?t1)
		 (accel ?b :time (during ?t1 ?t2))
		 (displacement ?b :time (during ?t1 ?t2))
		 (duration (during ?t1 ?t2))
		 ))
   ;; only applies if accel is constant within interval we are using
   ;; sought may not bind both times, so must choose endpoints of interval 
   ;; to try
   (constant (accel ?b) ?t-constant . ?whatever)
   (time (during ?t1 ?t2))	;ensure both endpoints to try bound
   (test (tinsidep '(during ?t1 ?t2) ?t-constant))
   )
  :effects
   ((eqn-family-contains (lk ?b (during ?t1 ?t2)) ?quantity)
    (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) lk-no-vf ?quantity)))

(defoperator draw-lk-no-vf-fbd (?b ?t1 ?t2 ?rot)
  :description "
   If the goal is to draw a lk fbd for lk-no-s
   then draw the body, the initial and final velocity, 
      the acceleration, and axes"
  :preconditions
  ((in-wm (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) lk-no-vf ?quantity))
   (body ?b)
   (inherit-vector ?b (velocity ?b :time ?t1) ?dir1)
   (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?dir3)
   (vector ?b (displacement ?b :time (during ?t1 ?t2)) ?dir4)
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (lk ?b (during ?t1 ?t2))))
)

(defoperator write-lk-no-vf-compo (?b ?t1 ?t2 ?xyz ?rot)
  :description 
  "Writes the equation s = vi*t + 0.5*a*t^2 (which lacks vf)"
  :preconditions
   (;; make sure accel compo doesn't vanish
    (in-wm (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?accel-dir))
    (test (non-zero-projectionp ?accel-dir ?xyz ?rot))
    (inherit-variable ?vi-compo (compo ?xyz ?rot (velocity ?b :time ?t1)))
    (inherit-variable ?a-compo (compo ?xyz ?rot (accel ?b :time (during ?t1 ?t2))))
    (variable ?s-compo (compo ?xyz ?rot (displacement ?b :time (during ?t1 ?t2))))
    (variable ?t-var (duration (during ?t1 ?t2)))
   ;; When appropriate, send some extra equations along to help the solver.
   (setof (solver-eqn ?ans-eqn 
		      (horizontal-projectile ?xyz ?rot ?b (during ?t1 ?t2))) 
	  nil ?dont-care)
    )
  :effects
  ((eqn (= ?s-compo (+ (* ?vi-compo ?t-var) (* 0.5 ?a-compo (^ ?t-var 2))))
	 (compo-eqn lk-no-vf ?xyz ?rot (lk ?b (during ?t1 ?t2))))
   )
  :hint (
    (point (string "Do you know an equation relating the components of displacement to those of initial velocity, time, and acceleration when acceleration is constant?"))
    (bottom-out (string "Write the equation ~A" 
			((= ?s-compo (+ (* ?vi-compo ?t-var)
					(* 0.5 ?a-compo (^ ?t-var 2))))
						 algebra)))
  ))

;; The solver doesn't know how to solve this type of system reliably.  
;; In this case, supply an explicit equation to the solver
;; for the case of horizontal projectile motion.
;;
;; This also means that the solver probably will
;; fail when attempting to solve the student's equations.
;; Special help is given in that case; see Help/NextStepHelp.cl.
(defoperator solver-eqn-for-horizontal-projectile-motion (?b ?t1 ?t2)
  :preconditions 
  (
   ;; this should have a more general test that displacement
   ;; in this direction is zero, or solve the general quadratic.  Bug #1509
   ;; it doesn't really have to be problem-specific.  Currently used for elec10
   (horizontal-projectile-motion ?b (during ?t1 ?t2))
   (inherit-variable ?vi (compo y 0 (velocity ?b :time ?t1)))
   (inherit-variable ?a (compo y 0 (accel ?b :time (during ?t1 ?t2))))
   (inherit-variable ?t-var (duration (during ?t1 ?t2)))
   )
  :effects ((solver-eqn (= ?t-var (/ (* -2.0 ?vi) ?a))
			  (horizontal-projectile ?xyz ?rot ?b (during ?t1 ?t2)))))

;;
;; LK equations special to projectile motion
;;

;; Following two write component equations for components with constant 
;; velocity motion.  This is used for horizontal motion of a projectile.
;; Note they apply only to one component, since v_y need not be constant, 
;; so can't be derived as instances of a general vector equation. 

;; Following writes s_x = v0_x * t when a_x is zero so v_x is constant.
;; (This is a special case of lk-no-vf, so could possibly use same eq id
;; to treat it as a special case --  not clear if this would be useful.)
;; V0 is most commonly given, but other constant velocity rule should permit 
;; equating v0_x to v1_x, v2_x if needed. Vavg_x could also be used but we 
;; don't introduce Vavg ;; all unless the problem asks for it. 
;; The test for vanishing acceleration compo must be deferred until we actually 
;; write the equation since the axis is not chosen at the time of trying 
;; compo-eqn-contains.
;;
;; Because this is defined as a subsdiary compo-eqn under the lk method
;; writing it will require drawing all the lk vectors over the interval. This 
;; could be a nuisance if you wish to apply it over a sub-interval of 
;; projectile motion but for now it suffices. 

(def-psmclass sdd-constvel (compo-eqn sdd-constvel ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :short-name ("[a<sub>~A</sub>=0; v<sub>~A</sub> is constant]" (axis-name ?axis) (axis-name ?axis))
  :tutorial "ConstantVelocityComponent.html"
  :nlg-english ("displacement component = constant velocity component * time")
  :ExpFormat ((strcat "calculating the ~a component of displacement as constant "
		      "velocity component * time for ~a from ~a to ~a") 
	      (axis-name ?axis) (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("d<sub>~a</sub> = v<sub>~a</sub>   t" (axis-name ?axis) (axis-name ?axis)))

(defoperator sdd-constvel-compo-contains (?quantity)
  :description 
   "Lists the quantities contained in s_x = v0_x*t when a_x = 0" 
  :preconditions
  (
   ;; vector PSM uses wm-or-derive for compo-eqn-contains so this operator
   ;; will only be called once
   (any-member ?quantity 
	        ((velocity ?b :time ?t1)
		 (displacement ?b :time (during ?t1 ?t2))
		 (duration (during ?t1 ?t2))
		 ))
   (object ?b) ;in case ?b is not bound
   ;; only applies if accel is constant so child of lk.
   (constant (accel ?b) ?t-constant . ?whatever)
   (time (during ?t1 ?t2))	; ensure both endpoints to try bound
   (test (tinsidep '(during ?t1 ?t2) ?t-constant))
   )
  :effects
   ((eqn-family-contains (lk ?b (during ?t1 ?t2)) ?quantity)
    (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) sdd-constvel ?quantity)))

(defoperator draw-sdd-constvel-fbd (?b ?t1 ?t2 ?rot)
  :preconditions
  ((in-wm (compo-eqn-contains  (lk ?b (during ?t1 ?t2)) sdd-constvel ?quantity))
   (body ?b)
   (inherit-vector ?b (velocity ?b :time ?t1) ?dir1)
   (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?dir3)
   (vector ?b (displacement ?b :time (during ?t1 ?t2)) ?dir4)
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (lk ?b (during ?t1 ?t2))))
)

(defoperator sdd-constvel-compo (?b ?t1 ?t2 ?xyz ?rot)
  :description 
  "Writes the component equation s_x = vi_x*t when a_x = 0"
  :preconditions
  ( ;; make sure accel compo vanishes
   (in-wm (inherit-vector ?b (accel ?b :time (during ?t1 ?t2)) ?accel-dir))
   (test (not (non-zero-projectionp ?accel-dir ?xyz ?rot)))
   ;; and veclocity doesn't vanish, else this overlaps with projection
   (in-wm (inherit-vector ?b (velocity ?b :time ?t1) ?vel-dir))
   (test (non-zero-projectionp ?vel-dir ?xyz ?rot))
   ;; and write it 
   (inherit-variable ?vi-compo (compo ?xyz ?rot (velocity ?b :time ?t1)))
   (variable ?s-compo (compo ?xyz ?rot (displacement ?b :time (during ?t1 ?t2))))
   (variable ?t-var (duration (during ?t1 ?t2)))
   ;; following only used for implicit eqn so a_x can be accepted if used
   (inherit-or-quantity (compo ?xyz ?rot (accel ?b :time (during ?t1 ?t2)))
			?a-compo)
   (inherit-variable ?a_x ?a-compo)
   )
  :effects
  ((eqn (= ?s-compo (* ?vi-compo ?t-var))
	 (compo-eqn sdd-constvel ?xyz ?rot (lk ?b (during ?t1 ?t2))))
    (implicit-eqn (= ?a_x 0) (projection ?a-compo)))
  :hint (
    (point (string "Can you think of an equation relating the components of displacement to those of initial velocity and time?"))
    (point (string "What do you know about the ~A component of the velocity of ~A ~A?" 
		   ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		   ?b ((during ?t1 ?t2) pp)))
    (teach (string "Because the acceleration of ~A ~A is perpendicular to the ~A axis, is has no component in the ~A direction. Therefore, the ~A component of velocity remains constant ~A. You can use this to relate ~A to ~A and ~A." 
		   ?b ((during ?t1 ?t2) pp) 
		   ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		   ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		   ((axis ?xyz ?rot) symbols-label :namespace :objects) 
		   ((during ?t1 ?t2) pp) 
		   (?s-compo algebra) (?vi-compo algebra) (?t-var algebra)))
    (bottom-out (string  "Write the equation ~A"
		   ((= ?s-compo (* ?vi-compo ?t-var)) algebra)))
  ))

 
;;; Following draws a relative position vector of ?b1 from ?b2 at ?t
;;; using a direction given in the problem statement. 
;;;
;;; taking ?b2 as "origin" we tag this as a vector property of b1. This
;;; association is used only by axis-drawing and component writing operators,
;;; when looking for vectors on an object, but shouldn't matter for rotational
;;; problems.

(defoperator draw-relative-position (?b1 ?b2 ?t)
  :description 
  "if you are given that one body is at a certain direction with respect to another,
  then draw the relative position vector from one to the other in that direction"
  :preconditions 
  ( 
   (given (dir (relative-position ?b1 ?b2 :time ?t-given)) ?dir-expr . ?rest)
   (time ?t) ;explicit time
   (test (tinsidep-include-endpoints ?t ?t-given))
   ;; make sure this is an exact value (no :error term)
   (test (definite-directionp ?dir-expr))
   ;; make sure this vector not already drawn
   (not (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "r_~A_~A~@[_~A~]" (body-name ?b1) 
			      (body-name ?b2) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?dir-var-value (dir-var-value ?dir-expr))
   )
  :effects (
    (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?dir-expr)
    (variable ?mag-var (mag (relative-position ?b1 ?b2 :time ?t)))
    (variable ?dir-var (dir (relative-position ?b1 ?b2 :time ?t)))
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (relative-position ?b1 ?b2 :time ?t)))
   )
  :hint (
    (point (string "You know the direction of the position of ~a relative to ~a." ?b1 ?b2))
    ;(bottom-out (string "Use ~A to draw the position of ~a with respect to ~a ~a at ~a."
    ;			(*vector-tool* eval)
    ;			?b1 ?b2 (?t pp) ?dir-expr))
    (bottom-out (string "Use ~A to draw ~a at ~a."
    			(*vector-tool* eval)
    			((relative-position ?b1 ?b2 :time ?t) def-np) ?dir-expr))
  ))

(defoperator draw-relative-position-approximate (?b1 ?b2 ?t)
  :preconditions 
  ( 
   (given (dir (relative-position ?b1 ?b2 :time ?t-given)) ?dir-expr
	  :hint (?loc ?more) ("based upon the problem statement" nil))
   (time ?t) ;explicit time
   (test (tinsidep-include-endpoints ?t ?t-given))
   ;; make sure there is an error term
   (test (and (parameter-or-unknownp ?dir-expr) (member ':error ?dir-expr)))
   ;; make sure this vector not already drawn
   (not (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "r_~A_~A~@[_~A~]" (body-name ?b1) 
			      (body-name ?b2) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
  :effects (
    (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?dir-expr)
    (variable ?mag-var (mag (relative-position ?b1 ?b2 :time ?t)))
    (variable ?dir-var (dir (relative-position ?b1 ?b2 :time ?t)))
   )
  :hint (
    (point (string "Draw the position of ~a relative to ~a in an approximate direction ~A." 
		   ?b1 ?b2 (?loc identity)))
    ;(bottom-out (string "Use ~A to draw the position of ~a with respect to ~a ~A at an approximate angle of ~A."
    ;			(*vector-tool* eval)
    ;			?b1 ?b2 (?t pp) ?dir-expr))
    (bottom-out (string "Use ~A to draw ~a at an approximate angle of ~A."
    			(*vector-tool* eval)
    			((relative-position ?b1 ?b2 :time ?t) def-np) ?dir-expr))
  ))

;; draw rba at direction opposite given dir of rab
(defoperator draw-opposite-relative-position (?b1 ?b2 ?t)
  :description 
  "if you are given that one body is at a certain direction with respect to another,
  then draw the relative position vector from one to the other in that direction"
  :preconditions ( 
    (given (dir (relative-position ?b2 ?b1 :time ?t-given)) ?opp-dir-expr
	   . ?whatever)
    ;; make sure that the direction is known, without error
    (test (definite-directionp ?opp-dir-expr))
    (time ?t)
    (test (tinsidep-include-endpoints ?t ?t-given))
    ;; make sure this vector not already drawn
    (not (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "r_~A_~A~@[_~A~]" (body-name ?b1) 
			       (body-name ?b2) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-expr (opposite ?opp-dir-expr))
    (bind ?dir-var-value (dir-var-value ?dir-expr))
    (debug "~&Drawing ~a relative position from ~a to ~a at ~a.~%" ?dir-expr ?b1 ?b2 ?t)
    )
  :effects (
    (vector ?b1 (relative-position ?b1 ?b2 :time ?t) ?dir-expr)
    (variable ?mag-var (mag (relative-position ?b1 ?b2 :time ?t)))
    (variable ?dir-var (dir (relative-position ?b1 ?b2 :time ?t)))
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (relative-position ?b1 ?b2 :time ?t)))
   )
  :hint (
    (point (string "You know the direction of the relative position of ~a with respect to ~a." ?b1 ?b2))
    ;(bottom-out (string "Use ~A to draw the position of ~a relative to ~a ~a at ~a."
    ;			(*vector-tool* eval)
    ;			?b1 ?b2 (?t pp) ?dir-expr))
    (bottom-out (string "Use ~A to draw ~a at ~a."
    			(*vector-tool* eval)
    			((relative-position ?b1 ?b2 :time ?t) def-np) ?dir-expr))
    ))

(def-PSMclass opposite-relative-position (opposite-relative-position (?Object0 ?Object1) ?time)
  :complexity minor
  :short-name "relative position equality"
  :nlg-english ("relative position equality")
  :ExpFormat ("applying relative position equality to ~a and ~a"
	      (nlg ?Object0) (nlg ?Object1 'at-time ?time))
  :EqnFormat ("R12 = R21"))

(defoperator opposite-relative-position-contains (?quantity)
  :preconditions (
  (any-member ?quantity (
  		(mag (relative-position ?b1 ?b2 :time ?t))
                        ))
  ;; sort body names in id so opposite-relative-position(b1, b2) 
  ;; gets same id as opposite-relative-position(b2, b1)
  (bind ?body-pair (sort (list ?b1 ?b2) #'expr<))
  )
  :effects ( 
  	(eqn-contains (opposite-relative-position ?body-pair ?t) ?quantity) 
  ))

(defoperator opposite-relative-position (?b1 ?b2 ?t)
  :preconditions (
  (variable ?mag1-var (mag (relative-position ?b1 ?b2 :time ?t)))
  (variable ?mag2-var (mag (relative-position ?b2 ?b1 :time ?t)))
  )
  :effects 
  (
   (eqn (= ?mag1-var ?mag2-var) (opposite-relative-position (?b2 ?b1) ?t))
   ;; don't use when rdiff is being used
   (assume using-magnitude (rdiff ?b1 ?b2 ?t))
   (assume using-magnitude (rdiff ?b2 ?b1 ?t))
   )
  :hint
  ((point (string "Note that ~A and ~A are equal" (?mag1-var algebra) (?mag2-var algebra)))
   (bottom-out (string "Write the equation ~A" ((= ?mag1-var ?mag2-var) algebra)))
  ))


(defoperator draw-relative-position-unknown (?b1 ?b2 ?t)
  :description 
  "if the direction of the relative position of one body with respect to 
  another is not given, you can introduce the relative position vector by drawing it with an unknown direction"
  :preconditions 
  ( 
   (time ?t)  ;explicit time
   ;; Make sure we are not given the direction, or the opposite direction,
   ;; when other drawing rules would apply
   (not (given (dir (relative-position ?b1 ?b2 :time ?t-at)) . ?whatever) 
	(tinsidep-include-endpoints ?t ?t-at))
   (not (given (dir (relative-position ?b2 ?b1 :time ?t-at)) . ?whatever) 
	(tinsidep-include-endpoints ?t ?t-at))
   ;; test must be after the (not ...) or an error if ?b1 is unbound.
   (test (not (equal ?b1 ?b2))) ;make sure the objects are distinct.
   ;; make sure this is not known to be zero-length from at-place stmt.
   (not (at-place ?b1 ?b2 :time ?t-at) (tinsidep-include-endpoints ?t ?t-at))
   (not (at-place ?b2 ?b1 :time ?t-at) (tinsidep-include-endpoints ?t ?t-at))
   (not (same-place (orderless ?b1 ?b2) :time ?t-at ) (tinsidep ?t ?t-at))
   ;; make sure this vector not already drawn
   (not (vector ?b2 (relative-position ?b1 ?b2 :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "r_~A_~A~@[_~A~]" (body-name ?b1) 
			      (body-name ?b2) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (debug "~&Drawing the relative position ~A with respect to ~a ~a at an unknown angle.~%" ?b1 ?b2 (?t pp))
   )
  :effects (
    (vector ?b1 (relative-position ?b1 ?b2 :time ?t) unknown)
    (variable ?mag-var (mag (relative-position ?b1 ?b2 :time ?t)))
    (variable ?dir-var (dir (relative-position ?b1 ?b2 :time ?t)))
   )
  :hint (
    ;(bottom-out (string "Use ~A to draw the position of ~a with respect to ~a ~a, at an approximately correct angle, since its exact direction is unknown."
    ;			(*vector-tool* eval)
    ;			?b1 ?b2 (?t pp)))
    (bottom-out (string "Use ~A to draw ~a, at an approximately correct angle, since its exact direction is unknown."
    			(*vector-tool* eval)
    			((relative-position ?b1 ?b2 :time ?t) def-np)))
  ))

;;; draw zero-length relative position if body is at location
(defoperator draw-zero-relative-position (?b ?loc ?t)
  :preconditions
  ((in-wm (at-place ?b ?loc :time ?t-at-place))
   (time ?t)
   (test (tinsidep-include-endpoints ?t ?t-at-place))
   (not (vector ?b (relative-position ?b ?loc :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "r_~A_~A~@[_~A~]" (body-name ?b) ?loc 
			      (time-abbrev ?t)))
   (debug "~&Drawing zero-length relative position of ~a wrt ~a at ~a.~%" ?b ?loc ?t))
  :effects 
  ((vector ?b (relative-position ?b ?loc :time ?t) zero)
   (variable ?mag-var (mag (relative-position ?b ?loc :time ?t)))
   (given (mag (relative-position ?b ?loc :time ?t)) (dnum 0 |m|))
   (implicit-eqn (= ?mag-var (dnum 0 |m|)) (mag (relative-position ?b ?loc :time ?t))))
  :hint 
  ( (point (string "Note that ~A is at ~A." ?b ?loc))
    (teach (string "What is the relative position of ~A with respect to ~A?" 
		   ?b ?loc))    
    ;(bottom-out (string "Use ~A to draw a zero length vector representing the position of ~a relative to ~a ~a."
    ;			(*vector-tool* eval)
    ;			?b ?loc (?t pp)))
    (bottom-out (string "Use ~A to draw a zero length vector representing ~a."
    			(*vector-tool* eval)
    			((relative-position ?b ?loc :time ?t) def-np)))
  ))

;;; draw zero-length relative position for two bodies
(defoperator draw-zero-relative-position-bodies (?a ?b  ?t)
  :preconditions
  ((in-wm (same-place (orderless . ?bodies) :time ?t-at-place))
   (time ?t)
   (test (tinsidep-include-endpoints ?t ?t-at-place))
   (any-member ?bodies ((?a ?b) (?b ?a)))
   (not (vector ?b (relative-position ?a ?b :time ?t) ?dont-care))
   (not (vector ?b (relative-position ?b ?a :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "r_~A_~A~@[_~A~]" (body-name ?a) (body-name ?b)
			      (time-abbrev ?t)))
   (debug "~&Drawing zero-length relative position of ~a wrt ~a at ~a.~%" ?a ?b ?t))
  :effects 
  ((vector ?a (relative-position ?a ?b :time ?t) zero)
   (variable ?mag-var (mag (relative-position ?a ?b :time ?t)))
   (given (mag (relative-position ?a ?b :time ?t)) (dnum 0 |m|))
   (implicit-eqn (= ?mag-var (dnum 0 |m|)) (mag (relative-position ?a ?b :time ?t))))
  :hint 
  ( (point (string "Note that ~A and ~A are at the same place ~." ?a ?b (?t pp)))
    (teach (string "What is the relative position of ~A with respect to ~A ~A?" 
		   ?a ?b (?t pp)))    
    ;(bottom-out (string "Use ~A to draw a zero length vector representing the position of ~a relative to ~a ~a."
    ;			(*vector-tool* eval)
    ;			?a ?b (?t pp)))
    (bottom-out (string "Use ~A to draw a zero length vector representing ~a."
    			(*vector-tool* eval)
    			((relative-position ?a ?b :time ?t) def-np)))
    ))

;;;
;;; Vector sum of displacements
;;; We compute displacement over several times as sum of succesive 
;;; displacements over all contained times. Doesn't handle other 
;;; decompositions of time interval to avoid multiplying possibilities.
;;; 
(defoperator sum-disp-vector-contains (?sought)
  :preconditions 
  ((any-member ?sought ( (displacement ?b :time ?t) ))
   (object ?b)
   (time ?tt)
   (test (proper-subintervalp ?t ?tt)) ;?t equals ?tt or is atomic subinterval
   )
  :effects 
  ((eqn-family-contains (sum-disp ?b ?tt) ?sought)
   ;; since only one compo-eqn under this vector PSM, we can just
   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (sum-disp ?b ?tt) sum-disp ?sought)))

(defoperator draw-sum-disp-diagram (?rot ?b ?tt)
  :preconditions 
  (;; 1. draw body.
   (body ?b)
   ;; 2. draw each constituent displacement. Note we want to do this before
   ;; drawing the net displacement, so have some cue to drawing an accurate
   ;; net displacement.
   (bind ?intervals (successive-intervals ?tt))
   (foreach ?interval ?intervals
      (vector ?b (displacement ?b :time ?interval) ?dir-di))
   ;; then draw the net displacement
   (vector ?b (displacement ?b :time ?tt) ?dir-dnet)
   (axes-for ?b ?rot))
  :effects 
  ((vector-diagram ?rot (sum-disp ?b ?tt))))

(defoperator write-sum-disp-compo (?b ?tt ?xy ?rot)
  :preconditions 
   ((variable ?dnet-xy (compo ?xy ?rot (displacement ?b :time ?tt)))
   (bind ?intervals (successive-intervals ?tt))
   (map ?interval ?intervals
      (variable ?di-xy (compo ?xy ?rot (displacement ?b :time ?interval)))
      ?di-xy ?di-compos))
  :effects 
   ((eqn (= ?dnet-xy (+ . ?di-compos))
               (compo-eqn sum-disp ?xy ?rot (sum-disp ?b ?tt)))
    )
   :hint
   ((point (string "Think about the relationship between the net displacement of ~A ~A and the individual displacements over each of the times making up the interval." ?b (?tt pp)))
    (point (string "The net displacement vector over a time interval represents the net change in position over that interval. This will be the vector sum of the individual displacements making up the net change. This can be applied component-wise to write an equation for the components of the net displacement in terms of the components of the individual displacements."))
    (bottom-out (string "Write the equation ~A" ((= ?dnet-xy (+ . ?di-compos)) algebra)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Relate relative positions of two objects to their displacements
;;; 

(def-psmclass relative-position-displacement 
    (?eq-type relative-position-displacement ?axis ?rot (relative-position-displacement ?a ?b ?time))
  :complexity minor
  :short-name "relative position and displacement"
  :nlg-english ("relative positon and displacement for two bodies")
  :ExpFormat ("calculating change in relative position between ~a and ~a ~a" (nlg ?a) (nlg ?b) (nlg ?time))
  :EqnFormat ("Rab2~a = da12<sub>~a</sub> - db12<sub>~a</sub> Rab1<sub>~A</sub>" (axis-name ?axis)  
	      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))


(defoperator relative-position-displacement-vector-contains (?sought)
  :preconditions 
  ((any-member ?sought ( (displacement ?b :time ?tt)
			 (displacement ?a :time ?tt)
			 (relative-position ?a ?b :time ?t)))
   (object ?a) (object ?b)
   (time ?tt) (time ?t)
   (test (tendpointp ?t ?tt)) ;?t is endpoint of ?tt
   )
  :effects 
  ((eqn-family-contains (relative-position-displacement ?a ?b ?tt) ?sought)
   ;; since only one compo-eqn under this vector PSM, we can just
   ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (relative-position-displacement ?a ?b ?tt) 
		       relative-position-displacement ?sought)))

(defoperator draw-relative-position-displacement-diagram (?rot ?a ?b ?t1 ?t2)
  :preconditions 
  (
   (vector ?a (relative-position ?a ?b :time ?t1) ?dir-abi)
   (vector ?a (relative-position ?a ?b :time ?t2) ?dir-abf)
   (vector ?a (displacement ?a :time (during ?t1 ?t2)) ?dir-a)
   (vector ?b (displacement ?b :time (during ?t1 ?t2)) ?dir-b)
   ;; make sure that same coordinates being used for both bodies.
   ;; Note that both bodies have equal footing under this PSM.
   (axes-for ?a ?rot)
   (axes-for ?b ?rot))
  :effects 
  ((vector-diagram ?rot (relative-position-displacement ?a ?b 
							(during ?t1 ?t2)))))

(defoperator write-relative-position-displacement-compo (?a ?b ?t1 ?t2 ?xy ?rot)
  :preconditions 
   ((variable ?da (compo ?xy ?rot (displacement ?a :time (during ?t1 ?t2))))
    (variable ?db (compo ?xy ?rot (displacement ?b :time (during ?t1 ?t2))))
    (variable ?rabi (compo ?xy ?rot (relative-position ?a ?b :time ?t1)))
    (variable ?rabf (compo ?xy ?rot (relative-position ?a ?b :time ?t2)))
    )
  :effects 
   ((eqn (= (+ ?rabf ?db) (+ ?rabi ?da))
               (compo-eqn relative-position-displacement ?xy ?rot 
			  (relative-position-displacement ?a ?b 
							  (during ?t1 ?t2))))
    (assume using-relative-position-displacement ?a ?b (during ?t1 ?t2))
    )
   :hint
   ((point (string "The change in relative position of ~A and ~B is related to their individual displacements ~A." 
		   ?a ?b ((during ?t1 ?t2) pp)))
    (point (string "The relative position of two objects over a time interval is determined by each of ther displacements during that time. This will be the vector sum of the initial relative position plus the displacement of the first object minus the displacement of the second object.  This can be applied component-wise."))
    (bottom-out (string "Write the equation ~A" 
			((= ?rabf (+ ?rabi (- ?da ?db))) algebra)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; motion of a part is same as that of a compound
;; this is useful if we are given the motion of a compound in the problem
;; It also enables motion description to propagate from one part up through
;; compound back down to other part so only needed for one part in givens.
;; !!! currently only works if compound body motion is given -- doesn't
;; search to derive compound body motion from move-together statement.
(defoperator get-part-motion-from-compound (?b ?bodies)
  :preconditions (
     (in-wm (motion (compound orderless . ?bodies) . ?rest))
     (any-member ?b ?bodies)
     (not (motion ?b . ?whatever))
  )
  :effects (
     (motion ?b . ?rest)
  ))


;; mag of kinematic vector (displacement, velocity, acceleration) of compound 
;; is same as vector on parts. mag equal ; equation
;; This also applies to rotational vectors
(defoperator kine-compound-contains (?sought)
   :preconditions (
   (any-member ?sought ( 
                        (mag (velocity ?b1 :time ?t))
			(mag (velocity (compound . ?obodies)) :time ?t)
                        (mag (accel ?b1 :time ?t))
			(mag (accel (compound . ?obodies)) :time ?t)
                        (mag (displacement ?b1 :time ?t))
			(mag (displacement (compound . ?obodies)) :time ?t)
			(mag (ang-velocity ?b1 :time ?t))
			(mag (ang-velocity (compound . ?obodies)) :time ?t)
                        (mag (ang-accel ?b1 :time ?t))
			(mag (ang-accel (compound . ?obodies)) :time ?t)
                        (mag (ang-displacement ?b1 :time ?t))
			(mag (ang-displacement (compound . ?obodies)) :time ?t)
                        ))
   (object (compound . ?obodies))
   (object ?b1)
   (test (orderless-p ?obodies)) ;sanity test
   (test (member ?b1 (cdr ?obodies) :test #'equal))
   (bind ?vec-type (first (second ?sought)))
   )
   :effects (
      (eqn-contains (kine-compound ?vec-type ?b1 ?obodies ?t) ?sought)
   ))

(defoperator write-kine-compound (?vec-type ?b1 ?obodies ?t)
   :preconditions (
      (variable ?c-var (mag (?vec-type (compound . ?obodies)) :time ?t))
      (variable ?b-var (mag (?vec-type ?b1) :time ?t))
   )
   :effects (
      (eqn (= ?c-var ?b-var) (kine-compound ?vec-type ?b1 ?obodies ?t))
   )
   :hint
   ((teach (string "If an object is part of a compound body, then ~a of the object is the same as ~a of the compound body." ?vec-type ?vec-type))
    (bottom-out (string "Write the equation ~A" ((= ?c-var ?b-var) algebra)))
   ))


;;; gravitational acceleration
;;; This represents entering the known constant value 9.8 m/s^2 for the 
;;; gravitational acceleration near the surface of the Earth.
;;; g is not expected to be known for other planets so will have to be 
;;; given in the problem statement or treated as a parameter.
;;;
;;; The "std-constant" equation ID is used so the bubble-graph driver
;;; can give this special treatment: if a quantity is determined because
;;; it is a standard constant, there is no need to try to seek it by any
;;; other equations.
(defoperator g-on-earth-contains (?quantity)
  :preconditions 
    ( (any-member ?quantity ((gravitational-acceleration earth))) )
    :effects ( (eqn-contains (std-constant (gravitational-acceleration earth))
			     ?quantity) ))

(defoperator write-g-on-earth ()
  :preconditions 
    ( (variable ?g-var (gravitational-acceleration earth)) )
  :effects ( (eqn (= ?g-var (dnum 9.8 |m/s^2|)) 
		  (std-constant (gravitational-acceleration earth))) )
  :hint
  ((point (string "You should know the value of g for the Earth"))
   (teach (string "You can use 9.8 m/s<sup>2</sup> for the value of g near the surface of the Earth"))
   (bottom-out (string "Write the equation ~A" ((= ?g-var (dnum 9.8 |m/s^2|)) algebra))) ))

(defoperator define-grav-accel (?planet)
  :short-name "define g"
  :description "Define the acceleration due to gravity, assuming it is constant."
  :preconditions 
  (
   (near-planet ?planet :body ?whatever)
   (bind ?g-var (format-sym "g_~A" ?planet)) 
   (test (or (member ?planet (cadr (find 'bodies (problem-choices *cp*) 
					:key #'car)))
	     (error "Body ~A missing from choices." ?planet)))
   )
  :effects ( (variable ?g-var (gravitational-acceleration ?planet)) 
	     (define-var (gravitational-acceleration ?planet)) )
  :hint 
  ((bottom-out 
    (string "Define a variable for ~A by using ~a." 
	    ((gravitational-acceleration ?planet) def-np)
	    (*text-tool* eval)))))


;;; ======================== connected bodies =====================
;;; Two bodies are said to be connected if the magnitudes of their
;;; kinematic variables are all equal.  This occurs when they are
;;; connected by a taut string, by one pushing on the other, by one
;;; carrying the other, etc.  Thus, we write the equation-producing
;;; operators to depend on a proposition (connected ?b1 ?b2 ?t) that
;;; is inferred from propositions describing taut strings, pushing,
;;; etc.
;;;
;;; Note: we ensure arguments to "connected" are sorted so only assert
;;; and use connections in a canonical order. This blocks writing both 
;;; vb1 = vb2  and vb2 = vb1.

(defoperator string-connects (?string ?b1 ?b2 ?t)
  :description "
  If a string connects two objects,
  then they are connected."
  :preconditions
  (
   (time ?t)
   ;;(debug "Trying connections ~a ~a ~a~%" ?b1 ?b2 ?t)
   (tied-to ?string ?b1 :time ?t1 :dir ?dir1)
   (tied-to ?string ?b2 :time ?t2 :dir ?dir2)
   (test (tinsidep ?t ?t1))
   (test (tinsidep ?t ?t2))
   ;; Only apply to bodies in canonical order. 
   ;; Note this test ensures they are distinct as well.
   (test (expr< ?b1 ?b2))   
   (not (connected ?b1 ?b2 ?t))
   (debug "found connected ~A %" ?bodies)
   )
  :effects
  ((connected ?b1 ?b2 ?t))
  )


(defoperator connected-accels-contains (?quantity)
  :description "
  If two objects are connected over a time period
  then for any interior time period,
     you can infer that magnitudes of their accelerations are equal,
     which is an equation that mentions the mags of the accels of the bodies."
  :preconditions (
   (any-member ?quantity
	        ((mag (accel ?b1 :time ?t))
		 (mag (accel ?b2 :time ?t))))
   (connected ?b1 ?b2 ?t-connected)
   (time ?t)
   (test (tinsidep ?t ?t-connected))
   ; this rule doesn't apply to connected points on rotating objects
   ; so make sure both bodies in straight line motion during sought time
   (in-wm (motion ?b1 straight :time ?t-straight1 . ?what1))
   (test (tinsidep ?t ?t-straight1))
   (in-wm (motion ?b2 straight :time ?t-straight2 . ?what2))
   (test (tinsidep ?t ?t-straight2))
  )
  :effects
  ((eqn-contains (connected-accels ?b1 ?b2 ?t) ?quantity)))

;;; This operator writes the equation a1=a2, where a1 and a2 are the
;;; magnitudes of the accelerations of two bodies.  This operator
;;; should not use in-wm to fetch the variables, because the need to
;;; define a second bodies acceleration variable causes that
;;; acceleration to be drawn.

(defoperator write-connected-accels (?b1 ?b2 ?t)
  
  :description "
  If two objects are connected over a time period
  then for any interior time period,
     their magnitudes are equal."
  :preconditions
  ((connected ?b1 ?b2 ?t)
   (variable ?a1-var (mag (accel ?b1 :time ?t)))
   (variable ?a2-var (mag (accel ?b2 :time ?t))))
  :effects
  ((eqn (= ?a1-var ?a2-var) (connected-accels ?b1 ?b2 ?t)))
  :hint
  ((teach 
     (kcd "draw_compound_lk_body")
     (string "When two objects are connected, their accelerations have equal magnitude."))
   (bottom-out (string "Write ~a" ((= ?a1-var ?a2-var) algebra)))))

;;
;; Following uses the equality of velocities of connected objects. 
;; This applies in same conditions as connected-accels but also represents
;; a crucial condition needed for linked rotating objects such as the
;; two pulleys linked by a belt in Exkr6a that the linear velocities of
;; the points on the rims are equal. We do *not* want connected-accels
;; to apply in this case, since the centripetal acceleration of the two
;; points need not be the same, owing to the differing radii of the pulleys.
;; We could use a different proposition than "connected" for the case
;; of chained rotations; but currently we block this by including a restriction
;; to straight line motion in connected-accels.
;;
(defoperator connected-velocities-contains (?quantity)
  :description "
  If two objects are connected over a time period
  then for any interior time period,
     you can infer that magnitudes of their velocities are equal,
     which is an equation that mentions the mags of the velocities of the bodies."
  :preconditions
  ((any-member ?quantity
	        ((mag (velocity ?b1 :time ?t))
		 (mag (velocity ?b2 :time ?t))))
   (debug "trying connected b1 b2 for mag v b1=~A b2=~A~%" ?b1 ?b2)
   (connected ?b1 ?b2 ?t))
  :effects
  ((eqn-contains (connected-velocities ?b1 ?b2 ?t) ?quantity)))

;;; This operator writes the equation v1=v2, where v1 and v2 are the
;;; magnitudes of the velocities of two bodies.  This operator
;;; should not use in-wm to fetch the variables, because the need to
;;; define a second body's velocity variable causes that
;;; velocity to be drawn.

(defoperator write-connected-velocities (?b1 ?b2 ?t)
  
  :description "
  If two objects are connected over a time period
  then for any interior time period,
     their magnitudes are equal."
  :preconditions
  ((connected ?b1 ?b2 ?t)
   (variable ?v1-var (mag (velocity ?b1 :time ?t)))
   (variable ?v2-var (mag (velocity ?b2 :time ?t))))
  :effects
  ((eqn (= ?v1-var ?v2-var) (connected-velocities ?b1 ?b2 ?t)))
  :hint
  ((teach 
      (kcd "draw_compound_lk_body")
      (string "When two objects are connected, their velocities have equal magnitude."))
   (bottom-out (string "Write ~a" ((= ?v1-var ?v2-var) algebra)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rotational Kinematics
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Objects rotating in the x-y plane of the screen are described by 
;; 	(motion ?obj rotating :dir ?rotate-dir :accel ?accel-spec 
;;                                         :axis ?axis-pt :time ?t)
;; ?axis-pt: name of pt about which rotation occurs 
;;           most often 'cm for center of mass
;; ?rotate-dir: 'into 'out-of 'z-unknown
;; ?accel-spec: 'into 'out-of 'zero 'z-unknown
;;
;; Rotational vectors point along the z-axis perpendicular to the plane
;; of the screen. Directions along the z axis are represented by 
;; the special atoms 'into and 'out-of in direction slots and are computed
;; using the right-hand-rule. We also use 'unknown-zdir for rotational 
;; vectors of unknown orientation to indicate they are known to be along the 
;; z-axis.  This applies to net torque, for example, before the sum of the
;; individual torques is computed, so you don't yet know which way the object 
;; will be torqued.
;;
;; In equations the direction variable OV denotes direction with respect
;; to the z-axis so is always equal to either 0 or 180. We nevertheless
;; write out projection equations as V_z = V * cos (OV). 
;;
;; Using the apparatus of angles and z-dir projections is overkill
;; for these problems since all the vectors will have only two possible 
;; directions and the projections will always just give a sign. The 
;; quantities could be treated as signed scalar magnitudes instead.  
;; We use this method because: we want a step in any case for moving from 
;; given rotation directions to choose the signs to use when plugging in 
;; terms into a standard equation. I.e. we don't want given values
;; to include the signs, else mistake in sign just looks like transcription 
;; error, when it could indicate uncertainty about right hand rule.  Also,
;; this notation is currently used on the ANDES interface; it gives us a 
;; number the student can enter as an answer for a sought z-axis direction 
;; and an equation we can use algebraically to specify it; and, it may help 
;; to teach general vector concepts.
;;

; draw angular velocity of an object rotating in a known direction 
; Direction is given as cw or ccw in a motion description statement.
; May be used for instantaneous ang-vel at an instant or
; average ang-vel over an interval
; corresponds to draw-velocity-straight
(defoperator draw-ang-velocity-rotating (?b ?t)
   :preconditions (
    (motion ?b rotating :dir ?dir 
	    :axis ?axis :time ?t-motion . ?whatever)
    (test (definite-directionp ?dir)) ;match dir-var-value
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-velocity ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "omega_~A~@[_~A~]" (body-name ?b) 
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
  )
  :effects (
    (variable ?mag-var (mag (ang-velocity ?b :time ?t)))
    (variable ?dir-var (dir (ang-velocity ?b :time ?t)))
    (given (dir (ang-velocity ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (ang-velocity ?b :time ?t)))
    (vector ?b (ang-velocity ?b :time ?t) ?dir) 
  )
  :hint 
  ((point (string "Notice that ~a is rotating ~a about ~A ~A." ?b 
		  (?dir rotation-name) ?axis (?t pp)))
   (teach (string "The angular velocity vector represents the rate of change of a rotating object's angular position. The angular velocity vector lies along the z-axis in Andes problems. By the right hand rule it points out of the x-y plane of the diagram for counter-clockwise rotation and into the x-y plane for clockwise rotation."))
   (bottom-out (string "Because ~a is rotating ~a ~A, use ~A to draw a non-zero angular velocity vector with direction ~a." 
		       ?b (?dir rotation-name) (?t pp) 
		       (*vector-tool* eval)
		       (?dir adj)))
   ))

;; Draw zero angular velocity for an object that is not rotating.
;; This is specified using the ang-at-rest or 
;; ang-momentarily-at-rest motion specifier. 
(defoperator draw-ang-velocity-at-rest (?b ?t)
   :preconditions (
    (motion ?b ?ar :time ?t-motion)
    (test (or (equal ?ar 'ang-at-rest) (equal ?ar 'ang-momentarily-at-rest)))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-velocity ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "omega_~A~@[_~A~]" (body-name ?b) 
			       (time-abbrev ?t)))
  )
  :effects (
    (vector ?b (ang-velocity ?b :time ?t) zero) 
    (variable ?mag-var (mag (ang-velocity ?b :time ?t)))
    (given (mag (ang-velocity ?b :time ?t)) (dnum 0 |rad/s|))
    (implicit-eqn (= ?mag-var (dnum 0 |rad/s|)) (mag (ang-velocity ?b :time ?t)))
  )
  :hint (
    (point (string "Notice that ~a is in rotational equilibrium ~a." 
		   ?b (?t pp)))
    (teach (string "The angular velocity vector represents the rate of change of a rotating object's angular position. If an object is at rest, its angular position is not changing and its angular velocity is zero.")) 
    (bottom-out (string "Because ~a is not rotating ~a, use ~A to draw a zero-length angular velocity vector for it." 
			?b (?t pp)
			(*vector-tool* eval)
			))))

;; draw angular displacement of an object rotating in a known direction
;; over an interval. 
(defoperator draw-ang-displacement-rotating (?b ?t)
  :preconditions (
    (motion ?b rotating :dir ?dir :time ?t-motion . ?whatever)
    (test (not (equal ?dir 'z-unknown)))  
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-displacement ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "theta_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
  )
  :effects (
    (vector ?b (ang-displacement ?b :time ?t) ?dir) 
    (variable ?mag-var (mag (ang-displacement ?b :time ?t)))
    (variable ?dir-var (dir (ang-displacement ?b :time ?t)))
    (given (dir (ang-displacement ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (ang-displacement ?b :time ?t)))
  )
  :hint
  ((point (string "Notice that ~a is rotating ~a ~a." 
		  ?b (?dir rotation-name) (?t pp)))
   (teach (string "The angular displacement of an object over an interval represents its net change in angular position as it rotates during that interval.  This vector is defined to lie along the z-axis in Andes problems. By the right hand rule, the angular displacement vector points out of the x-y plane of the diagram for net counter-clockwise rotation and into the x-y plane for net clockwise rotation."))
   (bottom-out (string "Because it is rotating ~A ~a, use ~A to draw a non-zero displacement vector for ~A in the direction ~a." 
		       (?dir rotation-name) (?t pp)
		       (*vector-tool* eval)
		       ?b (?dir adj)))
   ))

(defoperator draw-ang-displacement-unknown (?b ?t)
  :preconditions (
    (motion ?b rotating :dir z-unknown :time ?t-motion . ?whatever)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-displacement ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "theta_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects (
    (vector ?b (ang-displacement ?b :time ?t) z-unknown) 
    (variable ?mag-var (mag (ang-displacement ?b :time ?t)))
    (variable ?dir-var (dir (ang-displacement ?b :time ?t)))
  )
  :hint 
  ((point (string "You need to introduce a term for the angular displacement of ~A." ?b))
   (teach (string "The net rotation of the object is not given in the problem statement.  Whether the angular displacement points into or out of the plane requires calculation to determine.  Since it must lie along the z axis, you should draw it but specify an unknown Z direction."))
    (bottom-out (string "Use ~A to draw a non-zero angular displacement for ~a ~A and ~A."
			(*vector-tool* eval)
			?b (?t pp) (*unknown-z-direction-action* eval)))
   ))

(defoperator ang-accel-at-rest (?b ?t)
  :description 
   "If a body is a rest, then it has zero acceleration."
  :preconditions
   ((motion ?b ang-at-rest :time ?t-motion)
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-accel ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "alpha_~A~@[_~A~]" (body-name ?b) 
			       (time-abbrev ?t)))
    )
  :effects
  ((vector ?b (ang-accel ?b :time ?t) zero)        
   (variable ?mag-var (mag (ang-accel ?b :time ?t)))
   (given (mag (ang-accel ?b :time ?t)) (dnum 0 |rad/s^2|))
   (implicit-eqn (= ?mag-var (dnum 0 |rad/s^2|)) (mag (ang-accel ?b :time ?t))))
  :hint
  ((point (string "Notice that ~a is not rotating ~a." ?b (?t pp)))
   (teach (kcd "draw_accel_when_at_rest")
          (string "If a body is not rotating during some time interval, its angular acceleration during that interval is zero."))
   (bottom-out (string "Because ~a is not rotating ~a, use ~A to draw a zero-length angular acceleration vector for it." 
		       ?b (?t pp)
			(*vector-tool* eval)
			))
   ))

;;; draw angular acceleration of an object where the direction of rotation
;;; is indefinite.
(defoperator draw-ang-accel (?b ?t)
  :preconditions 
   ((motion ?b rotating :dir ?vel-dir :accel ?dir :time ?t-motion . ?whatever)
    (test (not (known-z-dir-spec ?vel-dir)))
    (test (known-z-dir-spec ?dir))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-accel ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "alpha_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir)))
  :effects 
   ((vector ?b (ang-accel ?b :time ?t) ?dir) 
    (variable ?mag-var (mag (ang-accel ?b :time ?t)))
    (variable ?dir-var (dir (ang-accel ?b :time ?t)))
    (given (dir (ang-accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (ang-accel ?b :time ?t))))
  :hint
   ((point (string "Notice that the rate at which ~a is rotating is changing ~A." ?b (?t pp)))
    (teach (string "The angular acceleration vector represents the rate of change of a rotating object's angular velocity."))
    ;(bottom-out (string "Because it is accelerating in a ~a direction ~a, you should use ~A to draw an angular acceleration for ~A pointing ~A." 
    ;(?dir rotation-name) (?t pp) (?dir adj)
    ;(*vector-tool* eval)
    ;?b (?dir adj)))
    (bottom-out (string "Because it is accelerating in a ~a direction, you should use ~A to draw ~A pointing ~A." 
	    (?dir rotation-name)
	    (*vector-tool* eval)
	    ((ang-accel ?b :time ?t) def-np) (?dir adj)))
    ))

(defoperator draw-ang-accel-speed-up (?b ?t)
  :preconditions 
   ((motion ?b rotating :dir ?dir :accel ?dir :time ?t-motion . ?whatever)
    (test (known-z-dir-spec ?dir))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-accel ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "alpha_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir)))
  :effects
   ((vector ?b (ang-accel ?b :time ?t) ?dir) 
    (variable ?mag-var (mag (ang-accel ?b :time ?t)))
    (variable ?dir-var (dir (ang-accel ?b :time ?t)))
    (given (dir (ang-accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (ang-accel ?b :time ?t))))
  :hint
   ((point (string "Notice that the rate at which ~a is rotating is increasing ~a" ?b (?t pp)))
    (teach (string "The angular acceleration vector represents the rate of change of a rotating object's angular velocity.  If an object's rate of rotation is speeding up then its angular velocity vector is increasing in magnitude over time, so the angular acceleration will point in the same direction as the angular velocity.  By the right-hand rule that will be out of the x-y plane for ccw rotation and into the plane for cw rotation."))
    (bottom-out (string "Because ~a is rotating ~a ~a so its angular velocity points ~A, and it's angular velocity is increasing, you should use ~a to draw an angular acceleration for it pointing ~a." 
    ?b (?dir rotation-name) (?t pp) (?dir adj)
    (*vector-tool* eval)
    (?dir adj)))
    ))

;; draw angular acceleration of an object rotating in a known direction
;; and slowing down
(defoperator draw-ang-accel-slow-down(?b ?t)
  :preconditions (
    (motion ?b rotating :dir ?vel-dir :accel ?dir :time ?t-motion . ?whatever)
    (test (known-z-dir-spec ?dir))
    (test (equal ?vel-dir (opposite ?dir)))
    (time ?t)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-accel ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "alpha_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (bind ?dir-var-value (dir-var-value ?dir))
  )
  :effects 
   ((vector ?b (ang-accel ?b :time ?t) ?dir) 
    (variable ?mag-var (mag (ang-accel ?b :time ?t)))
    (variable ?dir-var (dir (ang-accel ?b :time ?t)))
    (given (dir (ang-accel ?b :time ?t)) ?dir)
    (implicit-eqn (= ?dir-var ?dir-var-value) (dir (ang-accel ?b :time ?t))))
  :hint
   ((point (string "Notice that the rate at which ~a is rotating is decreasing ~a" ?b (?t pp)))
    (teach (string "The angular acceleration vector represents the rate of change of a rotating object's angular velocity. If an object's rate of rotation is slowing down then its angular velocity vector is decreasing in magnitude over time, so the angular acceleration will point in the opposite direction from the angular velocity, as determined by the right-hand rule."))
    ;(bottom-out (string "Because the angular acceleration of ~a ~a opposes the angular velocity for ~A rotation, which points ~A, you should use ~A to draw an angular acceleration for it pointing ~a." 
    ;?b  (?t pp) (?vel-dir rotation-name) (?vel-dir adj)
    ;(*vector-tool* eval)
    ;(?dir adj)))
    (bottom-out (string "Because ~a opposes the angular velocity for ~A rotation, which points ~A, you should use ~A to draw an angular acceleration for it pointing ~a." 
    	((ang-accel ?b :time ?t) def-np) (?vel-dir rotation-name) (?vel-dir adj)
	    (*vector-tool* eval)
	    (?dir adj)))
   ))

;;; draw angular acceleration of an objection rotating in an unknown direction
;;; but known to be accelerating. This arises in torque problems which seek
;;; angular acceleration at an instant from given forces. 
;;; Since we need not be given the initial angular velocity we don't know if 
;;; it is speeding up or slowing down, but we do know it may be changing 
;;; (i.e. it is not known constant).  Strictly, zero acceleration is a 
;;; possible answer, but that is a bad choice for Andes problems since we 
;;; expect any zero vectors to be determinable from the givens. 
;;; By drawing it unknown we should allow that it could turn out to be zero. 
(defoperator draw-ang-accel-unknown-dir (?b ?t)
  :preconditions (
    (time ?t)
    (motion ?b rotating :accel z-unknown :time ?t-motion . ?whatever)
    (test (tinsidep ?t ?t-motion))
    (not (vector ?b (ang-accel ?b :time ?t) ?dir-drawn))
    (bind ?mag-var (format-sym "alpha_~A_~A" (body-name ?b) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects 
   ((vector ?b (ang-accel ?b :time ?t) z-unknown) 
    (variable ?mag-var (mag (ang-accel ?b :time ?t)))
    (variable ?dir-var (dir (ang-accel ?b :time ?t))))
  :hint 
  ((point (string "You need to introduce a term for the angular acceleration of ~A." ?b))
   (teach (string "When a body is subject to a non-zero net ~A it will have an angular acceleration in the direction of the net ~A.  In this problem you can assume that the forces will result in a net ~A so the body will have a non-zero angular acceleration.  Whether the angular acceleration points into or out of the plane requires calculation to determine.  Since it must lie along the z axis, you should draw it but specify an unknown Z direction." 
		  (nil moment-name) (nil moment-name) 
		  (nil moment-name)))
    (bottom-out (string "Use ~A to draw a non-zero angular acceleration for ~a ~A and ~A." 
			(*vector-tool* eval)
			?b (?t pp) (*unknown-z-direction-action* eval)))
   ))
 
;;; angular sdd:
;;; angular displacement = avg angular velocity * duration
;;; Because this uses average angular velocity it does not require
;;; that the velocity be constant. It could also be called the definition
;;; of average angular velocity, as scalar sdd is definition of average speed.

(def-psmclass ang-sdd (?eq-type ang-sdd-constvel ?xyz ?rot 
				(rk ?body ?time))
  :complexity major
  :short-name "average angular velocity"
  :nlg-english ("the definition of average angular velocity")
  :tutorial "AngularVelocity.html"
  :expformat ((strcat "applying the definition of Average Angular Velocity "
		  "to ~a ~a") (nlg ?body) (nlg ?time 'time))
  :EqnFormat ("&omega;<sub>z</sub> = &theta;<sub>z</sub>/t"))

(defoperator ang-sdd-contains (?quantity)
  :preconditions (
   (any-member ?quantity (
                (ang-velocity ?b :time ?t)
		(ang-displacement ?b :time ?t)
		(duration ?t)
		))
   (object ?b)
   (time ?t)
   (test (time-intervalp ?t)))
  :effects 
  ((eqn-family-contains (rk ?b ?t) ?quantity)
   (compo-eqn-contains (rk ?b ?t) ang-sdd-constvel ?quantity)))

(defoperator draw-ang-sdd-vectors (?b ?t ?rot)
  :preconditions 
  (
   (in-wm (compo-eqn-contains (rk ?b ?t) ang-sdd-constvel ?quantity))
   (body ?b)
   (inherit-vector ?b (ang-velocity ?b :time ?t) ?dir-v)
   (vector ?b (ang-displacement ?b :time ?t) ?dir-d)
   (axes-for ?b ?rot)
  )
  :effects (
    (vector-diagram ?rot (rk ?b ?t))
  )
)

; this writes the compo equation; we use generic projection operators
; to produce the compo-free equation.
(defoperator write-ang-sdd (?b ?t)
  :preconditions
   ((variable ?theta_z  (compo ?z ?rot (ang-displacement ?b :time ?t)))
    (inherit-variable ?omega_z  (compo ?z ?rot (ang-velocity ?b :time ?t)))
    (variable ?t-var (duration ?t)))
  :effects 
   ((eqn (= ?theta_z (* ?omega_z ?t-var)) 
	 (compo-eqn ang-sdd-constvel ?z ?rot (rk ?b ?t)))
    )
  :hint (
  (point (string "Can you write an equation in terms of components relating average angular velocity to angular displacement and duration?"))
   (teach (string "The average angular velocity of a rotating object over an interval is defined to be the angular displacement divided by the duration of the interval. This gives the rotational counterpart of distance = average speed * time. Writing vector relations like this in terms of the vector components is recommended to avoid sign errors."))
   (bottom-out (string "Write the equation ~a=~a." 
		       (?theta_z algebra) ((* ?omega_z ?t-var) algebra)))
  ))

;;; ================ Rotational kinematics equations ========================

(def-psmgroup rk	
    :form (?eq-type ?Eqn-ID ?axis ?rot (rk ?body ?time))
    :supergroup Kinematics
    :doc "Equations for one dimensional motion with constant acceleration."
    :nlg-english ("a constant acceleration equation"))

;; angular version of lk-no-s: omega_f = omega_i + alpha_avg * t
;; Because this uses average angular acceleration, it doesn't require
;; acceleration to be constant

(def-psmclass rk-no-s (?eq-type rk-no-s ?xy ?rot (rk ?body (during ?t0 ?t1)))
  :group rk
  :complexity major
  :short-name "average anglular acceleration"
  :tutorial nil ;seems to be missing
  :nlg-english ("constant angular acceleration kinematics")
  :EqnFormat ("&omega;f<sub>z</sub> = &omega;i<sub>z</sub> + &alpha;(avg)<sub>z</sub> t")) ;alternative form in principles.cl

(defoperator rk-no-s-contains (?sought)
 :preconditions (
  (any-member ?sought
	       ( (ang-velocity ?b :time ?t1)
		 (ang-velocity ?b :time ?t2)
		 (ang-accel ?b :time (during ?t1 ?t2))
		 (duration (during ?t1 ?t2))
		 ))
   (object ?b)
   (time (during ?t1 ?t2))
  )
 :effects 
 (
  (eqn-family-contains (rk ?b (during ?t1 ?t2)) ?sought)
    (compo-eqn-contains  (rk ?b (during ?t1 ?t2)) rk-no-s ?sought)
))

(defoperator draw-rk-no-s-vectors (?b ?t1 ?t2 ?rot)
  :preconditions  
  (
   (in-wm (compo-eqn-contains (rk ?b (during ?t1 ?t2)) rk-no-s ?sought))
   (body ?b)
   (inherit-vector ?b (ang-velocity ?b :time ?t2) ?dir-v2)
   (inherit-vector ?b (ang-accel ?b :time (during ?t1 ?t2)) ?dir-d)
   (inherit-vector ?b (ang-velocity ?b :time ?t1) ?dir-v1)
   (variable ?var (duration (during ?t1 ?t2)))
   (axes-for ?b ?rot)
  )
  :effects ( (vector-diagram ?rot (rk ?b (during ?t1 ?t2))) )
)

(defoperator write-rk-no-s (?b ?t1 ?t2)
 :preconditions
  ((inherit-variable ?omega2_z (compo ?z ?rot (ang-velocity ?b :time ?t2)))
   (inherit-variable ?omega1_z (compo ?z ?rot (ang-velocity ?b :time ?t1)))
   (inherit-variable ?alpha_z  (compo ?z ?rot (ang-accel ?b :time (during ?t1 ?t2))))
   (variable ?t-var (duration (during ?t1 ?t2))))
  :effects 
  ((eqn (= ?omega2_z (+ ?omega1_z (* ?alpha_z ?t-var))) 
               (compo-eqn rk-no-s ?z ?rot (rk ?b (during ?t1 ?t2))))
   )
    :hint
   ((point (string "Can you think of an equation that relates the z component of average angular acceleration to that of the initial angular velocity, final angular velocity, and duration?"))
    (teach (string "Acceleration is the rate of change of velocity. The average acceleration vector over some time is defined as the difference between initial and final velocity vectors divided by the duration. This definition can be be applied in component form to relate ~A, ~A, ~A and ~A" (?omega2_z algebra) (?omega1_z algebra) (?alpha_z algebra) (?t-var algebra)))
    (bottom-out (string "Write the equation ~a = ~a" 
                        (?omega2_z algebra) ((+ ?omega1_z (* ?alpha_z ?t-var)) algebra)))))

;; angular version of lk-no-vf: 
;;        theta12 = omega1 * t + 0.5 * alpha12 * t12^2

(def-psmclass rk-no-vf (?eq-type rk-no-vf ?xyz ?rot (rk ?body (during ?t0 ?t1)))
     :group rk
     :complexity major
  :short-name "[&alpha;<sub>z</sub> is constant]"
     :nlg-english ("constant angular acceleration kinematics")
     :tutorial "ConstantAngularAcceleration.html"
     :EqnFormat ("&theta;<sub>z</sub> = &omega;0<sub>z</sub> t + 0.5 &alpha;<sub>z</sub> t<sup>2</sup>"))

(defoperator rk-no-vf-contains (?sought)
 :preconditions (
  (any-member ?sought
	       ( (ang-displacement ?b :time (during ?t1 ?t2))
 		 (ang-velocity ?b :time ?t1)
		 (ang-accel ?b :time (during ?t1 ?t2))
		 (duration (during ?t1 ?t2))
		 ))
   ; only applies if accel is constant within interval we are using
   (time (during ?t1 ?t2))  ; ensure both endpoints bound
   (constant (ang-accel ?b) ?t-constant . ?whatever)
   (test (tinsidep '(during ?t1 ?t2) ?t-constant))
  )
 :effects 
 ((eqn-family-contains (rk ?b (during ?t1 ?t2)) ?sought)
  (compo-eqn-contains (rk ?b (during ?t1 ?t2)) rk-no-vf ?sought)
  ))

(defoperator draw-rk-no-vf-vectors (?rot ?b ?t1 ?t2)
  :preconditions  
  (
   (in-wm (compo-eqn-contains (rk ?b (during ?t1 ?t2)) rk-no-vf ?sought))
   (not (vector-diagram ?rot (rk-no-vf ?b (during ?t1 ?t2))))
   (body ?b)
   (vector ?b (ang-displacement ?b :time (during ?t1 ?t2)) ?dir-d)
   (inherit-vector ?b (ang-accel ?b :time (during ?t1 ?t2)) ?dir-a)
   (inherit-vector ?b (ang-velocity ?b :time ?t1) ?dir-v1)
   (axes-for ?b ?rot)
  )
  :effects ( (vector-diagram ?rot (rk ?b (during ?t1 ?t2))) )
)

(defoperator write-rk-no-vf (?b ?t1 ?t2)
 :preconditions(
   (variable ?theta_z  (compo ?z ?rot (ang-displacement ?b :time (during ?t1 ?t2))))
   (inherit-variable ?omega1_z (compo ?z ?rot (ang-velocity ?b :time ?t1)))
   (inherit-variable ?alpha_z  (compo ?z ?rot (ang-accel ?b :time (during ?t1 ?t2))))
   (variable ?t-var    (duration (during ?t1 ?t2)))
  )
  :effects (
   (eqn (= ?theta_z (+ (* ?omega1_z ?t-var)
                       (* 0.5 ?alpha_z (^ ?t-var 2))) )
               (compo-eqn rk-no-vf ?z ?rot (rk ?b (during ?t1 ?t2))))
   )
  :hint (
    (point (string "Do you know an equation relating the z component of angular displacement to that of initial angular velocity, time, and angular acceleration when angular acceleration is constant?"))
    (bottom-out (string "Write the equation ~A" 
                ((= ?theta_z (+ (* ?omega1_z ?t-var)
                               (* 0.5 ?alpha_z (^ ?t-var 2)))) algebra) ))
  ))

;; angular version of lk-no-t 
;;        omega2^2 = omega1^2 + 2 * alpha12 * theta12

(def-psmclass rk-no-t (?eq-type rk-no-t ?xyz ?rot (rk ?body (during ?t0 ?t1)))
     :group rk
     :complexity major
  :short-name "[&alpha;<sub>z</sub> is constant]"
     :nlg-english ("constant angular acceleration kinematics")
     :tutorial "ConstantAngularAcceleration.html"
     :EqnFormat ("&omega;<sub>z</sub><sup>2</sup> = &omega;0<sub>z</sub><sup>2</sup> + 2 &alpha;<sub>z</sub> &theta;<sub>z</sub>"))

(defoperator rk-no-t-contains (?sought)
 :preconditions (
  (any-member ?sought
	       ( (ang-velocity ?b :time ?t1)
	         (ang-velocity ?b :time ?t2)
	         (ang-displacement ?b :time (during ?t1 ?t2))
		 (ang-accel ?b :time (during ?t1 ?t2))
		 ))
   ; only applies if accel is constant within interval we are using
   (time (during ?t1 ?t2))  ; ensure both endpoints bound
   (constant (ang-accel ?b) ?t-constant . ?whatever)
   (test (tinsidep '(during ?t1 ?t2) ?t-constant))
  )
 :effects 
 (
 (eqn-family-contains (rk ?b (during ?t1 ?t2)) ?sought)
    (compo-eqn-contains (rk ?b (during ?t1 ?t2)) rk-no-t ?sought)
 ))

(defoperator draw-rk-no-t-vectors (?rot ?b ?t1 ?t2)
  :preconditions  
  (
   (in-wm  (compo-eqn-contains (rk ?b (during ?t1 ?t2)) rk-no-t ?sought))
   (not (vector-diagram ?rot (rk-no-t ?b (during ?t1 ?t2))))
   (body ?b)
   (inherit-vector ?b (ang-velocity ?b :time ?t2) ?dir-v2)
   (inherit-vector ?b (ang-velocity ?b :time ?t1) ?dir-v1)
   (inherit-vector ?b (ang-accel ?b :time (during ?t1 ?t2)) ?dir-a)
   (vector ?b (ang-displacement ?b :time (during ?t1 ?t2)) ?dir-d)
   (axes-for ?b ?rot)
  )
  :effects ( (vector-diagram ?rot (rk ?b (during ?t1 ?t2))) )
)

(defoperator write-rk-no-t (?b ?t1 ?t2)
 :preconditions(
   (inherit-variable ?omega2_z (compo ?z ?rot (ang-velocity ?b :time ?t2)))
   (inherit-variable ?omega1_z (compo ?z ?rot (ang-velocity ?b :time ?t1)))
   (inherit-variable ?alpha_z  (compo ?z ?rot (ang-accel ?b :time (during ?t1 ?t2))))
   (variable ?theta_z  (compo ?z ?rot (ang-displacement ?b :time (during ?t1 ?t2))))
  )
  :effects (
   (eqn (= (^ ?omega2_z 2) (+ (^ ?omega1_z 2)
                              (* 2 ?alpha_z ?theta_z)))
               (compo-eqn rk-no-t ?z ?rot (rk ?b (during ?t1 ?t2))))
   )
  :hint (
    (point (string "Do you know an equation relating the z components of initial angular velocity, final angular velocity, angular acceleration, and angular displacement when acceleration is constant?"))
    (bottom-out 
      (string "Write the equation ~A" 
               ((= (^ ?omega2_z 2) (+ (^ ?omega1_z 2)
                                   (* 2 ?alpha_z ?theta_z))) algebra)))
  ))


;; Counterpart to lk-no-a would be 
;;          theta12 = 0.5 * (omega_i + omega_f) * t12
;; This formula wasn't used in the CLIPS solution so it's not included here.
;; It would be straightforward to add it if desired.


;; LINEAR-VEL: Linear velocity of point on rotating object 
;;    v_pt = omega * r 	where r is the radial distance from axis to point.
;;
;; We need an operator to draw the linear velocity of the rotating point with 
;; an appropriate direction, even though direction doesn't matter for the
;; answer. For now, we derive a motion statement for it from
;; a description of the point's relative position from the center at the
;; sample time shown in the problem diagram together
;; with the rotation direction; draw-velocity-curved will then draw it.
;; We could also add another special-purpose velocity drawing operator.
;; Note CLIPS solutions didn't pick any particular location of the point at
;; the time in question so left the velocity direction completely unspecified. 
;;
;; We want the radius in this case to be a vector in order to 
;; match with the cross product form of the equation.
;; However, this makes the revolution-radius superfluous.
;; Perhaps, we should replace revolution-radius with 
;; (mag (relative-position ...))
;;
;; Another possibility is to try to split the method graph inside 
;; the PSM to use both methods of specifying this quantity, but that involves
;; coding two completely different versions of the operator.

;; Following derives linear motion description from given relative position
;; of point on rim and rotation direction, for use by curved velocity drawing 
;; operator.
(defoperator describe-linear-motion (?pt ?t)
   :preconditions (
   (point-on-body ?pt ?whole-body)
   (time ?t)
   (motion ?whole-body rotating :dir ?rotate-dir
	   :axis ?axis-pt :time ?t-rotating . ?whatever)
   (test (not (equal ?rotate-dir 'z-unknown)))
   (test (tinsidep ?t ?t-rotating))
   (given (dir (relative-position ?pt ?axis-pt :time ?t)) ?r-dir . ?any-hint)
   (test (degree-specifierp ?r-dir))
   (bind ?v-dir (cross-product-dir ?rotate-dir ?r-dir))
   (bind ?a-dir (opposite ?r-dir))  
   (debug "linear motion of ~A: vel dir ~A, accel dir ~A~%" ?pt ?v-dir ?a-dir)
   )
   :effects ((motion ?pt curved :type circular :dir ?v-dir :accel ?a-dir :time ?t))
)

(defoperator linear-vel-contains (?sought)
   :preconditions (
   (any-member ?sought (
		  (mag (velocity ?pt :time ?t))
                  (mag (ang-velocity ?whole-body :time ?t))
		  (mag (relative-position ?pt ?axis :time ?t))
		))
   (point-on-body ?pt ?whole-body)
   (time ?t)
   (motion ?whole-body rotating :axis ?axis :time ?t-rotating . ?whatever)
   (test (tinsidep ?t ?t-rotating))
   )
   :effects (
     (eqn-contains (linear-vel ?pt ?t ?axis) ?sought)
   ))

(defoperator write-linear-vel (?pt ?t)
   :preconditions (
      (point-on-body ?pt ?whole-body)
      ;; Problems that use only this rule should draw a body for
      ;; consistency.  We choose the whole rotating object as our 
      ;; body, as suggested by Bob 
      (body ?whole-body)
      (variable ?v-var (mag (velocity ?pt :time ?t)))
      (variable ?omega-var (mag (ang-velocity ?whole-body :time ?t)))
      (variable ?r-var (mag (relative-position ?pt ?axis :time ?t)))
   )
   :effects (
    (eqn  (= ?v-var (* ?omega-var ?r-var)) (linear-vel ?pt ?t ?axis))
   )
   :hint (
    (point (string "Do you know the relation between the linear velocity of a point on a rotating object and the angular velocity of the rotation?"))
    (teach (string "The linear velocity of a point on a rotating object is equal to the angular velocity of the rotation times the radius of the point's circular motion.  For the radius, use the magnitude of the position of the point relative to the axis of rotation."))
    (bottom-out (string "Write the equation ~A"
                         ((= ?v-var (* ?omega-var ?r-var)) algebra)))
   )
)

(defoperator rolling-vel-contains (?sought)
  :preconditions 
  (
   (any-member ?sought (
		(radius-of-circle ?body)		 
		(mag (velocity ?axis :time ?t))
		(mag (ang-velocity ?body :time ?t))
		))
   (time ?t)				;radius-of-circle does not bind ?t
   (object ?body)			;velocity does not bind ?body
   (rolling ?body)  ;only apply when specified
   (motion ?body rotating :axis ?axis :time ?t-motion . ?whatever)
   (test (tinsidep ?t ?t-motion))
   )
   :effects (
     (eqn-contains (rolling-vel ?body ?axis ?t) ?sought)
   ))

(defoperator write-rolling-vel (?body ?t)
   :preconditions (
      (variable ?v-var (mag (velocity ?axis :time ?t)))
      (variable ?omega-var (mag (ang-velocity ?body :time ?t)))
      (variable ?r-var (radius-of-circle ?body))
   )
   :effects (
    (eqn  (= ?v-var (* ?omega-var ?r-var)) (rolling-vel ?body ?axis ?t))
   )
   :hint (
	  (point (string "Since ~A is rolling without slipping, what is the relation between the linear velocity of ~A and the rotational velocity of ~A?" 
			 ?body ?axis ?body))
    (teach (string "The linear velocity of a rolling body, measured at the axis of rotation, is equal to the angular velocity of the rotation times the radius of the body."))
    (bottom-out (string "Write the equation ~A"
                         ((= ?v-var (* ?omega-var ?r-var)) algebra)))
   )
)


;; following asserts the equality r = magR, so that we can describe the 
;; motion in terms of relative position vectors in the problem statement, 
;; while the formula uses the revolution-radius quantity.
(defoperator radius-equals-relpos (?pt ?t)
   :description "the radius of revolution of a point on a rotating object is equal
   to the magnitude of its relative position from the axis of rotation" 
   :preconditions (
   (point-on-body ?pt ?whole-body)
   (motion ?whole-body rotating :axis ?axis-pt :time ?t-rotating . ?whatever)
   (time ?t)
   (test (tinsidep-include-endpoints ?t ?t-rotating))
   (given (mag (relative-position ?pt ?axis-pt :time ?t)) ?value)
   )
   :effects (
    (equals (revolution-radius ?pt :time ?t) 
            (mag (relative-position ?pt ?axis-pt :time ?t)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                  Special Relativity
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

