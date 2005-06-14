;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KB/ontology: defines the expressions used in the Andes Knowledge Base
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter unit-english
    '(
      (|m| . "meters")
      (|kg| . "kilograms")
      (|s| . "seconds")
      (|C| . "degrees centigrade")
      (|K| . "degrees kelvin")
      (|g| . "grams")
      (|N| . "newtons")
      (|J| . "joules")
      (|V| . "volts")
      (|A| . "amperes")
      (|T| . "teslas")
      (|Wb| . "webers")
      (|ohm| . "ohms")
      (|Hz| . "hertz")
      (|Pa| . "pascals")
      (|F| . "farads")
      (|H| . "henries")
      (|W| . "watts")
      (|m/s| . "m/s")
      (|m/s^2| . "m/s^2")
      (|N.m| . "N.m")
      (|J.s| . "J.s")
      (|kg.m^2| . "kg.m^2")
      (|kg.m/s| . "kg.m/s")
      (|N/m| . "N/m")
      (|N.s/m^2| . "N.s/m^2")
      (|N/m^2| . "N/m^2")
      (|deg| . "degrees")
      (|rad| . "radians")
      (|rev| . "revolutions")
      (|lb| . "pounds")
      (|day| . "days")
      (|hr| . "hours")
      (|h| . "hours")
      (|min| . "minutes")
      (|yr| . "years")
      (|liter| . "liters")
      (|ft| . "feet")
      (|in| . "inches")
      (|mi| . "miles")
      (|slug| . "slugs")
      (|gal| . "gallons")
      (|u| . "")
      (|eV| . "electon volts")
      (|dyne| . "dynes")
      (|erg| . "ergs")
      (|cal| . "calories")
      (|lbs| . "pounds")
      (|ozW| . "ounces")
      (|ozVUS| . "ounces")
      (|knot| . "knots")
      (|dB| . "decibels")
      ))

;;;             Quantity Terms:

(defun translate-units (x)
  (let ((result (assoc x unit-english)))
    (Tell :test "Got ~W" result)
    ; leave untranslated if no name in table:
    (if result (cdr result) (format NIL "~A" x))))

; may wrap any time varying quant:
(def-qexp at (at ?quant ?time)
  :units ?quant
  :restrictions ?quant
  :english ("~A ~A" (nlg ?quant) (nlg ?time 'pp)))
(def-qexp dnum (dnum ?value ?unit)
  :english ("~A ~A" (identity ?value) (translate-units ?unit)))
; vector quantities:
(def-qexp relative-position (relative-position ?to-pt ?from-pt)
  :units |m|
  :english ("the relative position of ~A with respect to ~A" (nlg ?to-pt) (nlg ?from-pt)))
(def-qexp displacement (displacement ?body)
  :units |m|
  :english ("the displacement of ~A" (nlg ?body)))
(def-qexp velocity (velocity ?body)
  :units |m/s|
  :english ("the velocity of ~A" (nlg ?body)))
(def-qexp relative-vel (relative-vel ?to-pt ?from-pt)
  :units |m/s|
  :english ("the relative velocity of ~A with respect to ~A" (nlg ?to-pt) (nlg ?from-pt)))
(def-qexp accel	(accel ?body)
  :units |m/s^2|
  :english ("the acceleration of ~A" (nlg ?body)))
(def-qexp momentum (momentum ?body)
  :units |kg.m/s|
  :english ("the momentum of ~A" (nlg ?body)))
(def-qexp force (force ?body ?agent ?type)
  :units N
  :english ("~A force on ~A due to ~A" (nlg ?type) (nlg ?body) (nlg ?agent)))

(def-qexp net-force (net-force ?body)
  :units N
  :english ("the net force on ~A" (nlg ?body)))
(def-qexp ang-displacement (ang-displacement ?body)
  :units |rad|
  :english ("the angular displacement of ~A" (nlg ?body)))
(def-qexp ang-velocity (ang-velocity ?body)
  :units |rad/s|
  :english ("the angular velocity of ~A" (nlg ?body)))
(def-qexp ang-accel (ang-accel ?body)
  :units |rad/s^2|
  :english ("the angular acceleration of ~A" (nlg ?body)))
(def-qexp ang-momentum (ang-momentum ?body)
  :units |kg.m^2/s|
  :english ("the angular momentum of ~A" (nlg ?body)))
(def-qexp torque (torque ?body ?axis ?force)
  :units |N.m|
  :english ("the torque on ~A about ~A due to ~A" (nlg ?body) (nlg ?axis) (nlg ?force)))
(def-qexp net-torque (net-torque ?body ?axis)
  :units |N.m|
  :english ("the net torque on ~A about ~A" (nlg ?body) (nlg ?axis)))
;; attributes of vectors:
(def-qexp compo	(compo ?xyz ?rot ?vector)
  :units ?vector
  :english ("the ~A component of ~A" (nlg ?xyz adj) (nlg ?vector)))
(def-qexp mag	(mag ?vector)
  :units ?vector
  :restrictions nonnegative
  :english ("the magnitude of ~A" (nlg ?vector)))
(def-qexp dir	(dir ?vector)
  :units |deg|
  :english ("the direction of ~A" (nlg ?vector)))
  
;; scalar quantities
(def-qexp mass	(mass ?body)
  :units |kg|
  :restrictions positive
  :fromWorkbench (if time `(at (mass ,body) ,time) `(mass ,body))
  :english ("the mass of ~A" (nlg ?body)))
(def-qexp distance (distance ?body)
  :units |m|
  :fromWorkbench `(at (distance ,body) ,time)
  :english ("the distance travelled by ~A" (nlg ?body)))
(def-qexp distance-between (distance-between ?body ?body2)
  :units |m|
  :fromWorkbench `(at (distance-between ,body ,body2) ,time)
  :english ("the distance between ~A and ~a" (nlg ?body) (nlg ?body2)))

(def-qexp duration (duration (during ?t1 ?t2))
  :units |s|
  :restrictions positive
  :fromWorkbench  `(duration ,time)
  :english ("the duration of the interval from ~A to ~A" 
            (nlg ?t1 'moment) (nlg ?t2 'moment)))
(def-qexp speed    (speed ?body)
  :units |m/s|
  :fromWorkbench `(at (speed ,body) ,time)
  :english ("the speed of ~A" (nlg ?body)))
(def-qexp coef-friction (coef-friction ?body1 ?body2 ?static-or-kinetic)
  :units NIL ;; dimensionless
  :english ("coefficient of ~(~A~) friction between ~A and ~A" 
            (nlg ?static-or-kinetic NIL) (nlg ?body1) (nlg ?body2))) 

;; see constants.cl, function enter-predefs
(def-qexp gravitational-acceleration (gravitational-acceleration ?planet)
  :units |m/s^2|
  :restrictions positive
  :fromWorkbench `(gravitational-acceleration ,body)
  :english ("the gravitational acceleration due to ~A" (nlg ?planet nil)))

(def-qexp num-forces (num-forces ?body)
  :english ("the number of forces on ~A" (nlg ?body)))
(def-qexp revolution-radius (revolution-radius ?body)
  :units |m|
  :restrictions positive
  :fromWorkbench `(at (revolution-radius ,body) ,time)
  :english ("the radius of the circular motion of ~A" (nlg ?body)))
(def-qexp work (work ?b ?agent)
  :units J
  :english ("the work done on ~A by ~A" (nlg ?b) (nlg ?agent)))
(def-qexp net-work (net-work ?b)
  :units J
  :english ("the net work done on ~A" (nlg ?b)))
(def-qexp work-nc (work-nc ?b)
  :units J
  :english ("the work done by non-conservative forces on ~A" (nlg ?b)))
(def-qexp power (power ?b ?agent)
  :units W
  :english ("the power supplied to ~a from ~a" (nlg ?b) (nlg ?agent)))
(def-qexp net-power (net-power ?b)
  :units W
  :english ("the net power supplied to ~a" (nlg ?b)))
(def-qexp angle-between (angle-between ?vec1 ?vec2)
  :units |deg|
  :english ("the angle between ~A and ~A" (nlg ?vec1) (nlg ?vec2)))
(def-qexp total-energy (total-energy ?system) 
  :units J
  :english ("the total mechanical energy of ~A" (nlg ?system)))
(def-qexp kinetic-energy (kinetic-energy ?body)
  :units J
  :english ("the kinetic energy of ~A" (nlg ?body)))
(def-qexp grav-energy (grav-energy ?body ?agent)
  :units J
  :english ("the gravitational potential energy of ~A" (nlg ?body)))
(def-qexp spring-energy (spring-energy ?body ?agent) 
  :units J
  :english ("the elastic potential energy transmittable to ~A" (nlg ?body)))
(def-qexp compression (compression ?spring)
  :units |m|
  :fromWorkbench `(at (compression ,body) ,time)
  :english ("the compression distance of ~A" (nlg ?spring)))
(def-qexp spring-constant (spring-constant ?spring)
  :units |N/m|
  :restrictions positive
  :fromWorkbench `(spring-constant ,body)
  :english ("the spring constant of ~A" (nlg ?spring)))
(def-qexp height (height ?body)
  :units |m|
  :fromWorkbench `(at (height ,body) ,time)
  :english ("the height of ~A above the zero level" (nlg ?body)))
(def-qexp moment-of-inertia (moment-of-inertia ?body)
  :units |kg.m^2|
  :fromWorkbench `(at (moment-of-inertia ,body) ,time)
  :english ("the moment of inertia of ~A" (nlg ?body)))
;; for dimensions of certain rigid bodies:
(def-qexp length (length ?body)
  :units |m|
  :fromWorkbench `(length ,body)
  :english ("the length of ~A" (nlg ?body)))
(def-qexp width  (width ?body)
  :units |m|
  :fromWorkbench `(width ,body) 
  :english ("the width of ~A" (nlg ?body)))
(def-qexp radius (radius ?body)
  :units |m|
  :fromWorkbench `(at (radius ,body) ,time)
  :english ("the radius of ~A" (nlg ?body)))
(def-qexp num-torques (num-torques ?body ?axis)
  :english ("the number of torques on ~A about ~A" (nlg ?body) (nlg ?axis)))

; for now assume only two bodies:
(def-qexp compound (compound ?body1 ?body2)
  :english ("the compound of ~A and ~A" (nlg ?body1) (nlg ?body2)))
(def-qexp system (system ?body1 ?body2)
  :english ("the system of ~A and ~A" (nlg ?body1) (nlg ?body2)))

(def-qexp during (during ?t0 ?t1) 
  :english ("from ~A to ~A" (nlg ?t0 'moment) (nlg ?t1 'moment)))

;; Note when nlg'ing other time arguments in format strings: 
;; 'pp         forms a prepositional phrase: "at T0" or "during T0 to T1"
;; 'nlg-time   does the same as 'pp for times.
;; 'moment     just prings a time point name "T0", no preposition
;; 'time       does the same as moment.
;; Omit temporal prepositions in format string if nlg func supplies them.

;;====================================================
;; Entry Propositions.
(def-entryprop body (body ?body)
  :Doc "The body tool."
  :English ("the body for the ~a" ?body)) 


;;; Nlging the vector is complex.  The dir term can be an angle
;;; dnum in which case the nlg is easy, or it can be one of a 
;;; set of special atoms indicating the direction and (possibly)
;;; magnitude of the vector.  
;;; 
;;; The results of the formatting are:
;;;  A dnum velue:
;;;    "a vector for FOO rotated to Bar degrees."
;;;  unknown:
;;;   "a vector for FOO in an unknown direction."
;;;  into | out-of:
;;;   "a vector for FOO directed (Into the plane | Out of the plane)"
;;;  z-unknown:
;;;   "a vector for foo the direction is unknown but either into or out of the plane."
;;;  zero:
;;;   " A zero length vector for FOO.

(def-entryprop vector (vector ?body ?quantity ?direction)
  :helpform (vector ?quantity ?direction)
  :Doc "The generic vector entry tool."
  :English ("a ~a" (ont-vector-entryprop-format-func ?body ?quantity ?direction)))

(defun ont-vector-entryprop-format-func (Body Quantity Direction)
  "Format the vector entry."
  (declare (ignore body))
  (let ((Quant (nlg Quantity 'nlg-exp))
	(Dir (if (listp Direction) (nlg Direction)
	       (nlg Direction 'adjective))))
    (if (listp Direction) 
	(format Nil "vector for ~a rotated to ~a" Quant Dir)
      (case Direction
	(unknown (format Nil "vector for ~a in an unknown direction." Quant))
	(into (format Nil "vector for ~a directed ~a." Quant Dir))
	(out-of (format Nil "vector for ~a directed ~a." Quant Dir))
	(z-unknown (format Nil "vector for ~a the direction is ~a." Quant Dir))
	(zero (format nil "zero length vector for ~a." Quant))))))
		       
	
;;; The direction term for a vector can be a special atom indicating
;;; that the magnitude is zero or 
;;(defun ont-vec-rotformat (Direction)
;;  "A formatting function used only in the nlging of vectors."
;;  (if (equalp Direction 'Unknown)
;;      "in an unknown direction."
;;    (format Nil "rotated to ~a" (nlg Direction 'nlg-exp))))



;; Note: Fie on my inabilty to funcall special forms Fie I Say!
(def-entryprop draw-axes (draw-axes ?body ?x-direction)
  ;; until workbench associates bodies and times w/axes, just
  ;; leave them out of the entry proposition for axes
  :helpform (draw-axes ?x-direction)
  :Doc "The Axes drawing tool."
  :English ("a pair of axes on ~a rotated to ~a" 
	    ?body  (nlg ?x-direction)))
  
; TEMPORARY: enables loading .prb files with old form axes prop
; can be removed when all .prbs are regenerated axes prop
(def-entryprop draw-axes-old (draw-axes ?body ?time ?x-direction)
  ;; until workbench associates bodies and times w/axes, just
  ;; leave them out of the entry proposition for axes
  :helpform (draw-axes ?x-direction)
  :Doc "The Axes drawing tool."
  :English ("a pair of axes on ~a rotated to ~a" 
	    ?body  (nlg ?x-direction)))  

; no longer used - AW
;(def-entryprop radius (radius ?body ?time))

;; Note:: can't funcall if feh!
(def-entryprop angle (angle ?vec1 ?vec2)
  :Doc "Definition of the angle between two vectors."
  :English ("the angle between ~a and ~a" ?Vec1 ?Vec2))
	       
#|	       (or (and (atom ?Vec1) ?Vec1) 
		   (format Nil "the vector for ~a" (nlg ?Vec1)))
	     (or (and (atom ?Vec2) ?Vec2) 
	       (format Nil "the vector for ~a" (nlg ?Vec2)))))
	       |#

(def-entryprop define-var (define-var ?quantity)
  :Doc "Defining a variable for a specific quantity"
  :English ("a variable for ~a" (nlg ?Quantity)))

(defun choose-answer-ontology-fixfunc (ID)
  (let ((D (format Nil "~a" ID)))
    (if (string-equal (subseq D 0 2) "MC")
	(subseq D 3)
      D)))

(def-entryprop choose-answer (choose-answer ?question-id ?answer-num)
  :doc "Select multiple choice question answer"
  :English ("answer for multiple-choice question number ~A" 
	    (choose-answer-ontology-fixfunc ?question-id)))

(def-eqn-entryprop eqn (eqn ?equation ?eqn-id) 
  :helpform (eqn ?equation)
  :Doc "Entering an equation with the specified id."
  :English ("the equation ~a in slot ~a" ?Equation ?Eqn-ID))

(def-eqn-entryprop given-eqn (given-eqn ?equation ?quantity) 
  :helpform (eqn ?equation)
  :Doc "A given equation entry."
  :English ("the given equation ~a for: ~a" 
	       ?Equation (nlg ?Quantity 'def-np)))
  
(def-eqn-entryprop implicit-eqn (implicit-eqn ?equation ?quantity) 
  :helpform (implicit-eqn ?equation)
  :Doc "An implicit equation entry."
  :English ("the implicit equation ~a for: ~a"
	     ?Equation (nlg ?quantity 'def-np)))

(def-eqntype 'derived-eqn)

;;; Equation type merges specify what to do when two equations with the same
;;; expression and algebra but different type are called to be merged.
(def-eqntype-merge 'Implicit-eqn 'Given-Eqn 'Implicit-Eqn)  ;; Implicit + Given -> Implicit




;;========================================================
;; goal proposition forms
;;
;; These are hints to be given on important subgoals 
;; when giving nextstep help. They should be phrased so as
;; to fit as the argument to "you should be ...".
;; It is not necessary to include a hint for subgoals whose
;; satisfaction involves making an entry, since in this case
;; the operator hints on the operator that make the entry will
;; be applicable as well, however, it is possible if the
;; subgoal hint adds further benefit.
;;
;; Subgoal propositions will often contain variables at the
;; point at which they are encountered. Typically this will
;; be predictable from their position in the operator.
;;========================================================

; helper to use in format strings when args may be variables
; needed because "if" is special form, makes problems for our apply.
; uses var text if arg is var, else nlg's arg using nlg-type.
(defun if-var (form var-text &optional (nlg-type 'def-np))
  (if (variable-p form) var-text
    (nlg form nlg-type))) 

(def-goalprop lk-fbd (vector-diagram (lk ?body ?time))
   :doc "diagram showing all lk vectors and axes"
   :english ("drawing a diagram showing all of the needed kinematic vectors of ~A ~A and coordinate axes" 
              (nlg ?body ) (nlg ?time 'pp)))

;#| ;; Experimental, maybe following 2 are better handled by operator hints on entries.
; Subgoal hint for goal of drawing a vector:
; We assume first argument is relevant body to mention
(def-goalprop vec-drawn (vector ?axis-body (at (?vec-type . (?body . ?args)) ?time) ?direction)
   :english ("drawing a ~a vector for ~a." (nlg ?vec-type 'adjective) (nlg ?body 'def-np)))

; Subgoal hint for goal of defining a variable: We use one hint for vector [mag] variables,
; another for all others, to show that vector vars are introduced by drawing.  
; Note this depends crucially on order of entries and fact that earlier entries are matched first.
; Note the second form also gets used for vector component variables (which may be givens)
; -- maybe should insert another variant for that case.
(def-goalprop vector-var (variable ?var (at (mag (?vec-type . (?body . ?args))) ?t))
   :english ("drawing a ~a vector for ~a." (nlg ?vec-type 'adjective) (nlg ?body 'def-np)))

(def-goalprop other-var (variable ?var ?quantity)
   :english ("introducing a variable for ~A" (nlg ?quantity)))
;|# ;; end experimental

(def-goalprop lk-eqn-chosen
      (compo-eqn-selected (LK ?body ?time) ?quantity (compo-eqn . ?eq-args))
   :english ("choosing a particular kinematic equation containing ~A" 
             (nlg ?quantity)))

(def-goalprop axes-chosen (axis-for ?body x ?rotation)
  ;; !! this goal can be achieved in some cases without drawing 
  ;; by re-using existing axes.
   :english ("setting coordinate axes"))

(def-goalprop write-projections
      (projections ?component-variables ?component-expressions)
    :english ("writing projection equations for all the component variables in the equation you are using"))

(def-goalprop avg-vel-fbd (vector-diagram (avg-velocity ?body ?time))
   :english ("drawing a diagram showing all of the needed vectors for ~A ~A and coordinate axes" 
             (nlg ?body) (nlg ?time 'pp)))
   
(def-goalprop avg-accel-fbd (vector-diagram (avg-accel ?body ?time))
   :english ("drawing a diagram showing all of the needed kinematic vectors for ~A ~A and coordinate axes" 
              (nlg ?body) (nlg ?time 'pp)))

(def-goalprop net-disp-fbd (vector-diagram (sum-disp ?body ?time))
   :english ("drawing a diagram showing all of the needed displacements of ~A ~A and coordinate axes" 
              (nlg ?body) (nlg ?time 'pp)))

(def-goalprop nl-fbd (vector-diagram (nl ?body ?time))
  :doc "free-body-diagram for applying Newton's Law"
  :english ("drawing a free-body diagram for ~A ~A"
            (nlg ?body) (nlg ?time 'pp))) ; time may be interval or instant

; next goal used as sought in fbd-only problem:
(def-goalprop standard-fbd (fbd ?body ?time)
  :doc "free-body-diagram on its own."
  :english ("drawing a free-body diagram for ~A ~A"
            (nlg ?body) (nlg ?time 'pp))) ; time may be interval or instant

(def-goalprop all-forces (forces ?body ?time ?all-forces)
   :english ("drawing all the forces acting on ~A ~A" (nlg ?body) (nlg ?time 'pp)))

(def-goalprop linmom-fbd (vector-diagram (cons-linmom ?bodies (during ?t1 ?t2)))
   :doc "diagram showing all momentum vectors and axes"
   :english ("drawing a diagram showing all of the needed kinematic vectors and coordinate axes" ))

(def-goalprop angmom-fbd (vector-diagram (ang-momentum ?b ?t))
   :english ("drawing a diagram showing all of the needed kinematic vectors and coordinate axes"))

(def-goalprop all-torques (torques ?b ?axis ?time ?torques)
   :english ("showing the torques due to each force acting on ~A ~A" (nlg ?b) (nlg ?time 'pp)))

(def-goalprop nl-rot-fbd  (vector-diagram (NL-rot ?b ?axis ?t))
  :doc "diagram for applying Newton's Law for rotation"
  :english ("drawing a diagram showing all the torques on ~A ~A, the angular acceleration, and coordinate axes"
            (nlg ?b) (nlg ?t 'pp))) 

; this goal used as sought in vector-drawing-only problem (magtor*)
(def-goalprop draw-vectors (draw-vectors ?vector-list)
  :english ("drawing the vectors asked for in the problem statement"))

; this goal used as sought in multiple-choice question problem
(def-goalprop choose-mc-answer (choose-answer ?question-id ?answer)
   :english ("answering question ~A" (str-after ?question-id)))

(defun str-after (id &optional (sep #\-))
"return the part of string or symbol after separator ch (default hyphen)"
 (let ((pos-sep (position sep (string id))))
   (if pos-sep (subseq (string id) (1+ pos-sep))
      ""))) ; empty string if no separator
     

;;========================================================
;; psm groups and classes.
;; 
;; goal propositions for writing psm equations are included here as well.
;; ??? do we need these ? should be able to get from equation-writing ops.
;;
;; ?eq-type may be 'compo-free or 'compo-eqn

;;;; NOTE: For the purposes of the NSH code I am imposing some naming conventions here.
;;;;       All axes names should be of the form ?axis or (?axis0 ... ?axisn) if multiple axes.
;;;;       All body variables should be of the form ?body or [?body0 ... ?bodyn]
;;;;       Times should be ?time or [?time0, ... ?timen]

(def-psmclass given (given ?quantity ?value)
  :complexity simple
  :doc "enter given value"
  :english ("the given value.")
  :ExpFormat ("entering the given value of ~A" (nlg ?quantity))
  :EqnFormat ("Var = N units"))

(def-psmclass proj (proj (at (compo ?axis ?rot ?vector) ?time))
  :complexity connect
  :doc "projection equations"
  :english ("projection equations")
  :ExpFormat ("writing the projection equation for ~a onto the ~a axis." 
	      (nlg ?vector) (nlg ?axis 'adj))
  :EqnFormat ("~a_~a = ~a*~a($q~a - $q~a)"
	      (nlg ?vector 'nlg-vector-var-pref)
	      (nlg ?axis 'adj) (nlg ?vector 'nlg-vector-var-pref)
	      (nlg ?vector 'nlg-vector-var-pref)
	      (nlg ?vector 'nlg-vector-var-pref) (nlg ?axis 'adj)))
   
   
;;-------------------------------------------------------
;; Kinematics top-level group.
(def-psmgroup Kinematics 
    ;;:form (?class ?body ?duration)
    :doc "Kinematical principles.")

(def-psmclass sdd (sdd ?body ?time)
  :group Kinematics
  :complexity major
  :doc "Distance = rate * time."
  :english ("distance = average speed * time")
  :expFormat ((strcat "applying the \"distance = average speed "
		      "* time\" principle with ~a as the body ~a")
	      (nlg ?body) (nlg ?time))
  :EqnFormat ("vavg = s / t"))
 
(def-goalprop sdd-eqn (eqn ?algebra (sdd ?body ?time))
  :english ("writing an equation involving the speed of ~A ~A, and distance it travelled, and the duration of the trip" 
	    (nlg ?body) (nlg ?time 'pp)))

(def-psmclass avg-velocity
    (?eqn-type avg-vel ?axis ?rot (avg-velocity ?body ?time))
    :group Kinematics
    :complexity major    
    :Doc "Definition of average velocity."
    :english ("the definition of average velocity") 
    :ExpFormat ("applying the definition of average velocity on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("v(avg)_~a = d_~a/t" (nlg ?axis 'adj) (nlg ?axis 'adj)))

(def-goalprop avg-vel-eqn 
 (eqn ?algebra (compo-eqn avg-vel ?axis ?rot (avg-velocity ?body ?time)))
 :english ((strcat "writing an equation for the average velocity of ~A "
		   "~A in terms of components along the ~A axis")
	   (nlg ?body) (nlg ?time 'pp) ?axis))
 

(def-psmclass pyth-thm (pyth-thm ?body ?origin ?time0 ?time1)
  :complexity connect
  :english ("the pythagorean theorem" )
  :eqnFormat ("c^2 = a^2 + b^2"))

(def-psmclass sum-distance (sum-distance ?b1 ?b2 ?b3 ?t)
   :complexity simple
   :english ("the relationship among the distances between ~a, ~a and ~a" 
              (nlg ?b1) (nlg ?b2) (nlg ?b3))
   :ExpFormat("relating the total distance between ~a and ~a to the distances from these points to ~a" (nlg ?b1) (nlg ?b3) (nlg ?b2))
   :EqnFormat ("rAC = rAB + rBC"))

(def-psmclass net-disp (?eq-type sum-disp ?axis ?rot (sum-disp ?body ?time))
  :complexity minor 
  :english ("net displacement")
  :ExpFormat ("calculating the net displacement of ~a ~a" (nlg ?body) (nlg ?time))
  :EqnFormat ("dnet_~a = d1_~a + d2_~a + d3_~a + ..." (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))


(def-goalprop net-disp-eqn 
 (eqn ?algebra (compo-eqn sum-disp ?axis ?rot (sum-disp ?body ?time)))
 :english ((strcat "writing an equation for the component of net "
		   "displacement of ~A ~A along the ~A axis") 
	   (nlg ?body) (nlg ?time 'pp) ?axis))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Linear kinematic family of equations.
;; For a vector psm with subsidiary compo-eqns, we use major
;; vector psm id as group id
(def-psmgroup lk	
    :form (?eq-type ?Eqn-ID ?axis ?rot (lk ?body ?time))
    :supergroup Kinematics
    :doc "Equations for one dimensional motion with constant acceleration."
    :english ("a constant acceleration equation"))

;; lk-no-s = average acceleration def-- can occur under different methods as 
;; lk/lk-no-s if accel is constant or avg-accel/lk-no-s if it is not.
;; if student selects specific "average accel" equation as opposed to "constant
;; acceleration" group we want to match whichever instance is in use
;;
;; In order to deal with that and to meet the single-line parents that we 
;; require for the ontology we have defined two lk-no-s* psms one is 
;; 'lk-no-s-lk' which is part of the linear kinematics group.  The other is 
;; 'lk-no-s-avg-accel' which is part of the average accel form.
;; lk-no-s matches either

(def-psmclass lk-no-s-lk (?eq-type lk-no-s ?axis ?rot (lk ?body (during ?time0 ?time1))) 
  :group lk
  :complexity major
  :english ("the definition of average acceleration")
  :ExpFormat ("applying the definition of average acceleration on ~a from ~a to ~a"
		 (nlg ?body) (nlg ?time0 'pp) (nlg ?time1 'pp)))

(def-psmclass lk-no-s-avg-accel (?eq-type lk-no-s ?axis ?rot (avg-accel ?body (during ?time0 ?time1))) 
  :complexity major
  :english ("the definition of average acceleration")
  :ExpFormat ("applying the definition of average acceleration on ~a from ~a to ~a"
		 (nlg ?body) (nlg ?time0 'pp) (nlg ?time1 'pp)))

; generic form to match either version of lk-no-s.
(def-psmclass lk-no-s (?eq-type lk-no-s ?axis ?rot (?parent-psm ?body (during ?time0 ?time1))) 
  :group Kinematics
  :complexity major
  :english ("the definition of average acceleration")
  :ExpFormat ("applying the definition of average acceleration on ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'pp) (nlg ?time1 'pp))
  :EqnFormat ("a(avg)_~a = (vf_~a - vi_~a)/t" (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))

(def-goalprop lk-no-s-eqn (eqn ?algebra (compo-eqn lk-no-s ?axis ?rot (lk ?body ?time)))
  :english ((strcat "writing an constant acceleration equation in "
		    "terms of vector components along the ~A axis") ?axis))


(def-psmclass lk-no-t (?eq-type lk-no-t ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :doc "Linear kinematics eqn w/o time."
  :english ("the constant acceleration equation v_~a^2 = v0_~a^2 + 2*a_~a*d_~a" 
               (adj ?axis) (adj ?axis) (adj ?axis) (adj ?axis))
  :ExpFormat ((strcat "writing the constant acceleration equation "
                      "v_~a^2 = v0_~a^2 + 2*a_~a*d_~a "
		      "for ~a from ~a to ~a") 
                      (adj ?axis) (adj ?axis) (adj ?axis) (adj ?axis)
		      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("v_~a^2 = v0_~a^2 + 2*a_~a*d_~a" (nlg ?axis 'adj) (nlg ?axis 'adj)
					       (nlg ?axis 'adj) (nlg ?axis 'adj)))

(def-goalprop lk-no-t-eqn 
   (eqn ?algebra (compo-eqn lk-no-t ?axis ?rot (lk ?body ?time)))
   :english ("writing a constant acceleration equation in terms of vector components along the ~A axis" ?axis))


(def-psmclass lk-no-vf (?eq-type lk-no-vf ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :Doc "Linear kinematics eqn sans final velocity."
  :english ("the constant acceleration equation d_~a = v0_~a*t + 0.5*a_~a*t^2"
                            (adj ?axis) (adj ?axis) (adj ?axis) )
  :ExpFormat ((strcat "writing the constant acceleration equation "
                      "d_~a = v0_~a*t + 0.5*a_~a*t^2 "
		      "for ~a from ~a to ~a") 
                      (adj ?axis) (adj ?axis) (adj ?axis) 
		      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("d_~a = v0_~a*t + 0.5*a_~a*t^2" (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))

(def-goalprop lk-no-vf-eqn 
   (eqn ?algebra (compo-eqn lk-no-vf ?axis ?rot (lk ?body ?time)))
  :english ("writing a constant acceleration equation in terms of vector components along the ~A axis" ?axis))


(def-psmclass lk-no-a (?eq-type lk-no-a ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :doc "Linear Kinematics sans acceleration."
  :english ("the constant acceleration equation d_~a = 0.5*(vi_~a + vf_~a)*t" 
             (adj ?axis) (adj ?axis) (adj ?axis) )
  :ExpFormat ((strcat "writing the constant acceleration equation "
                      "d_~a = 0.5*(vi_~a + vf_~a)*t "
		      "for ~a from ~a to ~a") 
                      (adj ?axis) (adj ?axis) (adj ?axis) 
		      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time)))


(def-goalprop lk-no-a-eqn 
   (eqn ?algebra (compo-eqn lk-no-a ?axis ?rot (lk ?body ?time)))
   :english ("writing a constant acceleration equation in terms of vector components along the ~A axis" ?axis))


; special-case constant-velocity equations for x component of 2D projectile:
; We treat these as members of const accel group (0 accel is constant!), 
; though it might not be obvious to the students what to choose.
; Might also use a constant-velocity group, though only component is constant
(def-psmclass const-vx (compo-eqn (const-vx ?time0 ?time1) x 0 (lk ?body ?t-lk))
   :group lk
   :complexity simple
   :english ("constant velocity component")
   :ExpFormat ("using the constancy of the x-component of the velocity of ~a from ~a to ~a"
		  (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
   :EqnFormat ("vf_x = vi_x"))

(def-psmclass sdd-constvel (compo-eqn sdd-constvel ?axis ?rot (lk ?body (during ?time0 ?time1)))
  :group lk
  :complexity major
  :english ("displacement component = constant velocity component * time")
  :ExpFormat ((strcat "calculating the ~a component of displacement as constant "
		      "velocity component * time for ~a from ~a to ~a") 
	      (nlg ?axis 'adj) (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("d_~a = v_~a * t" (nlg ?axis 'adj) (nlg ?axis 'adj)))


; FREELY FALLING BODIES
(def-psmclass free-fall-accel (free-fall-accel ?body ?time)
  :complexity simple
  :english ("free fall acceleration")
  :ExpFormat ("determining free fall acceleration for ~a ~a"
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("a = g"))

(def-psmclass std-constant-g (std-constant g)
  :complexity simple 
  :english ("value of g on Earth")
  :expformat ("defining the value of g on Earth")
  :EqnFormat ("g = 9.8 m/s^2"))


; UNIFORM CIRCULAR MOTION
(def-psmclass centripetal-accel (centripetal-accel ?body ?time)
  :complexity major
  :english ("centripetal acceleration equation: a = v^2/r")
  :ExpFormat ((strcat "writing the centripetal acceleration "
		      "equation: a = v^2/r for ~a ~a")
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("ac = v^2/r"))

(def-psmclass period-circle (period ?body ?time circular)
   :complexity minor
   :english ("the formula for the period of uniform circular motion")
   :ExpFormat ("calculating the period of the motion of ~A" (nlg ?body))
   :EqnFormat("T = 2*$p*r/v"))

; MISCELLANEOUS 
(def-psmclass equals (equals ?quant1 ?quant2)
  :complexity connect
  :english ("find by equivalent quantity")
  :ExpFormat ("applying the fact that ~a is equivalent to ~a"
	      (nlg ?quant1) (nlg ?quant2))
  :EqnFormat ("s1 = s2"))


(def-psmclass sum-times (sum-times ?time0 ?time1 ?time2)
  :complexity connect
  :english ("t02 = t01 + t12")
  :ExpFormat ("calculating the sum of times \"~a = ~a + ~a\"" 
	      (nlg ?time0 'time) (nlg ?time1 'time) (nlg ?time2 'time))
  :EqnFormat ("t02 = t01 + t12"))


;; NEWTONS LAW family of equations.
(def-psmgroup NL
    :form (?eq-type ?compo-eqn-id ?axis ?rot (NL ?body ?time))
    :english ("Newton's second law"))

;; NSL now stands for all versions, same as group id:
(def-psmclass NSL (?eq-type ?compo-eqn-id ?axis ?rot (NL ?body ?time)) 
     :group NL
     :complexity major
     :doc "Newton's Second Law when accel is non-zero"
     :english ("Newton's Second Law")
     :ExpFormat ("applying Newton's Second Law on ~a ~a"
		 (nlg ?body) (nlg ?time 'nlg-time))
     :EqnFormat ("F1_~a + F2_~a + F3_~a + ...= m*a_~a" 
                 (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))

; following more specific choices no longer offered to students:
(def-psmclass NFL (?eq-type NFL ?axis ?rot (NL ?body ?time)) 
     :group NL
     :complexity major
     :doc "Newton's Second Law when accel is zero"
     :english ("Newton's first law")
     :ExpFormat ("applying Newton's First Law on ~a ~a"
		 (nlg ?body) (nlg ?time 'nlg-time)))

(def-psmclass NSL-net (?eq-type NSL-net ?axis ?rot (NL ?body ?time))
     :group NL
     :complexity major
     :english ("Newton's Second law, net force version")
     :ExpFormat ("applying Newton's Second Law for net forces on ~a ~a."
		 (nlg ?body) (nlg ?time 'nlg-time)))


;;; NOTE: Because this principle is applied to objects such as "the Air Bag"
;;; and "the Table" that we do not necessarily want to have the students 
;;; draw I use the ?Object variables.  ?Body variables imply drawn bodies.
(def-psmclass NTL (NTL (?Object0 ?Object1) ?force-type ?time)
  :complexity major
  :english ("Newton's Third Law")
  :ExpFormat ("applying Newton's Third Law to ~a and ~a ~a"
	      (nlg ?Object0) (nlg ?Object1) (nlg ?time 'nlg-time))
  :EqnFormat ("Fof1on2 = Fof2on1"))

(def-psmclass NTL-vector (?eq-type NTL ?axis ?rot (NTL-vector (?Object0 ?Object1) ?force-type ?time))
  :complexity major
  :english ("Newton's Third Law")
  :ExpFormat ("applying Newton's Third Law to ~a and ~a ~a"
	      (nlg ?Object0) (nlg ?Object1) (nlg ?time 'nlg-time))
  :EqnFormat ("F12_~a = - F21_~a" (nlg ?axis 'adj) (nlg ?axis adj)))

; FORCE LAWS
(def-psmclass wt-law (wt-law ?body ?time)
  :complexity minor
  :english ("Weight law")
  :ExpFormat ("applying the Weight Law on ~a" (nlg ?body))
  :EqnFormat ("Fw = m * g"))

(def-psmclass kinetic-friction (kinetic-friction ?body ?surface ?time)
  :complexity simple
  :english ("Kinetic Friction Law")
  :ExpFormat ("applying the Kinetic friction law for ~a and ~a ~a"
	      (nlg ?body) (nlg ?surface) (nlg ?time 'nlg-time))
  :EqnFormat ("Ffk = $u * Fn"))

(def-psmclass static-friction (static-friction ?body ?surface ?time) 
  :complexity simple
  :english ("Static Friction at maximum")
  :expformat ("applying the Definition of Static friction for ~a and ~a ~t"
	      (nlg ?body) (nlg ?surface) (nlg ?time 'nlg-time))
  :EqnFormat ("Ffs = $u * Fn"))


(def-psmclass num-forces (num-forces ?body ?time)		 ; silly, num-forces = <count of the forces>
  :complexity simple
  :english ("count forces")
  :expformat ("counting the total number of forces on ~a ~a"
		 (nlg ?body) (nlg ?time 'nlg-time)))

(def-psmclass ug (ug ?body ?agent ?time ?distance-quant)
  :complexity major 
  :english ("Newton's Law of Universal Gravitation")
  :expformat ("applying Newton's Law of Universal Gravitation for the force on ~a due to ~a" (nlg ?body) (nlg ?agent))
  :EqnFormat ("Fg = G*m1*m2/r^2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONNECTED BODIES
(def-psmclass connected-accels (connected-accels ?body0 ?body1 ?time)
  :complexity connect
  :english ("connected bodies have same acceleration")
  :expformat ((strcat "using the fact that ~a and ~a are connected and "
		      "therefore have the same acceleration")
	      (nlg ?body0) (nlg ?body1) (nlg ?time 'nlg-time))
  :EqnFormat ("a1 = a2"))
    
(def-psmclass connected-velocities (connected-velocities ?body0 ?body1 ?time)
  :complexity connect
  :english ("connected bodies have same velocity")
  :expformat ((strcat "using the fact that ~a and ~a are connected and "
		      "therefore have the same velocity")
	      (nlg ?body0) (nlg ?body1) (nlg ?time 'nlg-time))
  :EqnFormat ("v1 = v2"))

(def-psmclass tensions-equal (tensions-equal ?string (?body0 ?body1) ?time)
  :complexity connect
  :english ("tension equal throughout string")
  :expformat ((strcat "using the fact that the tension forces are always "
		      "equal at both ends of a string with ~a") (nlg ?string))
  :EqnFormat ("Ft1 = Ft2"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPOUND BODIES 
(def-psmclass mass-compound (mass-compound . ?compound)   ; b_i = bodies making up compound
  :complexity connect
  :english ("mass of a compound body is sum of masses of parts")
  :expformat ((strcat "using the fact that the mass of ~a "
		      "is the sum of the masses of its parts.") 
	      (nlg (cons 'compound (car ?compound))))
  :EqnFormat ("m_compound = m_part1 + m_part2"))

(def-psmclass kine-compound (kine-compound ?vec-type ?bi ?compound ?time) ; part, whole same kinematics
  :complexity connect
  :english ("~A of compound same as part" (nlg ?vec-type))
  :expformat ("applying the fact that the ~A of a compound is same as that of its parts" (nlg ?vec-type))
  :EqnFormat ("v_compound = v_part"))

					  
(def-psmclass force-compound(force-compound ?type ?agent ?compound ?time)
  :complexity connect
  :english ("external force on a compound")
  :expformat ((strcat "applying the fact that there is an external force "
		      "on the compound body consisting of ~a due to ~a ~a"
		      "of type ~a")
	      (nlg ?compund) (nlg ?agent) (nlg ?time) (nlg ?type))
  :EqnFormat ("F_on_part = F_on_compound"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORK and ENERGY
(def-psmclass work (work ?body ?agent (during ?time0 ?time1))     ; by a single force
  :complexity major ; definition, but can be first "principle" for sought
  :english ("the definition of work")
  :expformat ("calculating the work done on ~a by ~a from ~a to ~a" 
	      (nlg ?body) (nlg ?agent) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("W = F*d*cos($qF - $qd)"))

(def-psmclass net-work (net-work ?body (during ?time0 ?time1)) ; by all forces
 :complexity major ; definition, but can be first "principle" for sought
 :english ("the definition of net work")
  :expformat ("calculating the net work done by all forces on ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("Wnet = WF1 + WF2 + WF3 + ..."))

(def-psmclass work-nc (Wnc ?body (during ?time0 ?time1))
  :complexity minor  ; definition, but *can't* be first "principle" for sought
  :english ("the definition of Wnc")
  :expformat ("representing the work done on ~A by non-conservative forces"
              (nlg ?body))
  :EqnFormat ("Wnc = Wncf1 + Wncf2 + ..."))

(def-psmclass work-energy (work-energy ?body (during ?time0 ?time1))
  :complexity major
  :english ("the work-kinetic energy theorem")
  :expformat ("applying the work-kinetic energy theorem to ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("Wnet = Kf - Ki"))

(def-psmclass cons-energy (cons-energy ?body ?time0 ?time1)
  :complexity major
  :english ("conservation of mechanical energy")
  :ExpFormat ((strcat "applying conservation of mechanical energy to ~a "
		      "from ~a to ~a") (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("ME1 = ME2"))

(def-psmclass change-ME  (change-ME ?body ?time0 ?time1)
  :complexity major
  :english ("change in mechanical energy")
  :ExpFormat ((strcat "applying change in mechanical energy to ~a "
		      "from ~a to ~a") (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat("Wnc = ME2 - ME1"))

(def-psmclass height-dy (?eq-type height-dy ?y ?rot (height-dy ?body ?time))
  :complexity connect
  :english ("the height change-displacement relationship")
  :expformat ((strcat "relating the change in height of ~a ~a to the y "
		      "component of its displacement")
	      (nlg ?body) (nlg ?time 'pp))
  :EqnFormat ("h2 - h1 = d12_y"))

(def-psmclass power (power ?body ?agent ?time)
   :complexity major ; definition, but can be first "principle" for sought
   :english ("the definition of average power")
   :expformat ("applying the definition of average power supplied to ~A by ~A ~A"
               (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
   :EqnFormat("Pavg = W/t"))

(def-psmclass net-power (net-power ?body ?time)
   :complexity major ; definition, but can be first "principle" for sought
   :english ("the definition of net power")
   :expformat ("applying the definition of net power supplied to ~A ~A"
               (nlg ?body) (nlg ?time 'pp))
   :EqnFormat("Pnet = Wnet/t"))

(def-psmclass inst-power (inst-power ?body ?agent ?time)
   :complexity major ; definition, but can be first "principle" for sought
   :english ("the instantaneous power principle")
   :expformat ("calculating the instantaneous power supplied to ~A by ~A ~A"
               (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
   :EqnFormat("P = F*v*cos($q)"))

; Subsidiary equations used inside energy psms:
; complexity = major => must be explicit in solution for full credit

; !! "total-energy" label used in kb is misleading, it's total *mechanical* energy only

; Students don't need the sub-equation cons-energy choice since whole cons-energy psm shows 
; eqn as ME2=ME1. Selection of cons-energy in workbench will match the psmclass, not the sub-equation.
(def-equation tme-cons (total-energy-cons ?b ?t1 ?t2)
   :english ("the conservation of mechanical energy")
   :complexity major
   :EqnFormat ("ME1 = ME2"))

(def-equation mechanical-energy  (total-energy-top ?body ?time) 
   :english ("the definition of mechanical energy")
   :complexity definition
   :EqnFormat ("ME = K + $S Ui"))

(def-equation kinetic-energy (kinetic-energy ?body ?time)
  :english ("the definition of kinetic energy")
  :complexity definition
  :EqnFormat ("KE = 0.5*m*v^2"))

(def-equation grav-energy (grav-energy ?body ?time)
   :english ("gravitational potential energy")
   :complexity definition
   :EqnFormat ("Ug = m*g*h"))

(def-equation spring-energy (spring-energy ?body ?time)
   :english ("spring potential energy")
   :complexity definition
   :EqnFormat ("Us = 0.5*k*d^2"))


;; LINEAR MOMENTUM
(def-psmclass cons-linmom (?eq-type lm-compo ?axis ?rot (cons-linmom ?bodies (during ?time0 ?time1)))
  :complexity major
  :english ("conservation of momentum")
  :expformat ((strcat "applying Conservation of Linear Momentum "
		      "to ~a from ~a to ~a")
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("m1*v1i_~a + m2*v2i_~a = m1*v1f_~a + m2*v2f_~a" (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj) (nlg ?axis 'adj)))

(def-psmclass cons-ke-elastic (cons-ke-elastic ?bodies (during ?time0 ?time1))
  :complexity major
  :english ("conservation of kinetic energy in elastic collisions")
  :expformat ((strcat "Applying Conservation of Kinetic Energy in "
		      "elastic collisions to ~a from ~a to ~a")
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("K1 = K2"))

;; ROTATIONAL KINEMATICS
(def-psmclass ang-sdd (?eq-type z 0 (ang-sdd ?body ?time))
  :complexity major
  :english ("the definition of average angular velocity")
  :expformat ((strcat "applying the definition of Average Angular Velocity "
		  "to ~a ~a") (nlg ?body) (nlg ?time 'time))
  :EqnFormat ("$w(avg) = $q/t"))

(def-psmclass linear-vel (linear-vel ?pt ?time ?axis)
  :complexity major
  :english ("linear velocity of rotating point")
  :ExpFormat ("calculating the linear velocity of the rotating point ~a ~a"
	      (nlg ?pt) (nlg ?time 'nlg-time))
  :EqnFormat ("v = $w*r"))


;; ROTATIONAL KINEMATICS: CONSTANT ANGULAR ACCELERATION EQUATIONS
(def-psmgroup rk	
    ; !!! following matches any angular vector psm equation id, including, e.g.
    ; ang-momentum. Currently the rk equations proper lack any common pattern
    ; -- should fix that
    :form (?eq-type z 0 (?eqn-name ?body ?time)) 
    :supergroup Kinematics
    :doc "Equations for rotational motion with constant angular acceleration."
    :english ("a constant angular acceleration equation" )
    :expformat ("writing a constant angular acceleration equation for ~a ~a"
		(nlg ?body) (nlg ?time 'nlg-time)))
(def-psmclass rk-no-s (?eq-type z 0 (rk-no-s ?body (during ?time0 ?time1)))
     :group rk
     :complexity major
     :english ("constant angular acceleration kinematics")
     :EqnFormat ("$w_z = $w0_z + $a_z*t"))
(def-psmclass rk-no-vf (?eq-type z 0 (rk-no-vf ?body (during ?time0 ?time1)))
     :group rk
     :complexity major
     :english ("constant angular acceleration kinematics")
     :EqnFormat ("$q_z = $w0_z*t + 0.5*$a_z*t^2"))
(def-psmclass rk-no-t (?eq-type z 0 (rk-no-t ?body (during ?time0 ?time1)))
     :group rk
     :complexity major
     :english ("constant angular acceleration kinematics")
     :EqnFormat ("$w_z^2 = $w0_z^2 + 2*$a_z*$q_z"))
;; MOMENT OF INERTIA
(def-psmclass I-rod-cm (I-rod-cm ?body ?time)
  :complexity minor
  :english ("moment of inertia of a rod about its center")
  :expformat ("calculating the moment of inertia of ~a about its cm ~a"
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("I = (1/12) m*L^2"))

(def-psmclass I-rod-end (I-rod-end ?body ?time)
  :complexity minor
  :english ("moment of inertia of a rod about its end")
  :expformat ("moment of inertia of the rod ~a about its end ~a"
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("I = (1/3) m*L^2"))
(def-psmclass I-hoop-cm (I-hoop-cm ?body ?time)
  :complexity minor
  :english ("moment of inertia for a hoop about its center")
  :expformat ("moment of inertia for the hoop ~a about its cm ~a"
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("I = m*r^2"))

(def-psmclass I-disk-cm (I-disk-cm ?body ?time)
  :complexity minor
  :english ("moment of inertia of a disk about its center")
  :expformat ("moment of inertia of the disk ~a about its cm ~a"
	      (nlg ?body) (nlg ?time 'nlg-time)))

(def-psmclass I-rect-cm (I-rect-cm ?body ?time)
  :complexity minor
  :english ("moment of inertia of a rectangle about its center")
  :expformat ("moment of inertia of the rectangle ~a about its cm ~a"
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("I = (1/12) m*(L^2 + W^2)"))

(def-psmclass I-compound (I-compound ?compound ?time)
  :complexity minor
  :english ("moment of inertia of a compound body")
  :expformat ("calculating the moment of inertia of the compound body ~a ~a"
	      (nlg ?compound) (nlg ?time 'nlg-time))
  :EqnFormat ("I12 = I1 + I2"))


;; ANGULAR MOMENTUM
(def-psmclass ang-momentum (?eq-type z 0 (ang-momentum ?body ?time))
  :complexity major ; definition, but can be first "principle" for sought
  :english ("definition of angular momentum")
  :expformat ("applying the definition of angular momentum on ~a ~a"
	      (nlg ?body) (nlg ?time 'nlg-time))
  :EqnFormat ("L_z = I * $w_z"))

(def-psmclass cons-angmom (?eq-type z 0 (cons-angmom ?bodies (during ?time0 ?time1)))
  :complexity major
  :english ("conservation of angular momentum")
  :expformat ((strcat "applying Conservation of Angular Momentum "
		      "to ~a from ~a to ~a")
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time0 'time) (nlg ?time1 'time))
  :eqnformat ("Li_z = Lf_z"))

; ROTATIONAL DYNAMICS (TORQUE)

; We use two ways of writing net-torque, distinguished by presence or absence
; of the zc flag in id.  Following form is designed to match either one:
(def-psmclass net-torque-zc (?eq-type z 0 (net-torque ?body ?pivot ?time . ?opt-zc-flag))
  :complexity major ; definition, but can be first "principle" for sought
  :english ("the definition of net torque")
  :expformat ("applying the definition of Net Torque on ~a about ~a ~A"
	      (nlg ?body) (nlg ?pivot) (nlg ?time 'nlg-time))
  :eqnformat ("$tnet_z = $t1_z + $t2_z + ..."))

(def-psmclass torque-zc (torque-zc ?body ?pivot (force ?pt ?agent ?type) ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :english ("the definition of torque")
  :expformat ((strcat "calculating the z component of the torque "
		      "on ~a ~a due to the force acting at ~a")
	      (nlg ?body) (nlg ?time 'pp) (nlg ?pt))
  :EqnFormat ("$t_z = r*F*sin($qF-$qr)"))

(def-psmclass mag-torque (mag-torque ?body ?pivot (force ?pt ?agent ?type) ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :english ("the definition of torque magnitude")
  :expformat ((strcat "calculating the magnitude of torque "
		      "on ~a ~a due to the force acting at ~a")
	      (nlg ?body) (nlg ?time 'pp) (nlg ?pt))
  :EqnFormat ("$t = r*F*sin($q)"))
  

(def-psmclass NL-rot (?eq-type z 0 (NL-rot ?body ?pivot ?time))
  :complexity major
  :english ("Newton's Law for rotation")
  :expformat ("appling Newton's Law for rotation to ~a about ~a ~a"
	      (nlg ?body) (nlg ?pivot) (nlg ?time 'nlg-time))
  :eqnFormat ("$tnet_z = I*$a_z"))


;;
;; Subsidiary equations that occur inside top-level psm nodes:
;; In the future, we may be able to rewrite so that some of these
;; can become top-level psms.
;;

; Used as sub-equation inside standard vector psms: 
; also used inside the top-level psm version ("proj" above) which
; is used in "component-form" solutions
(def-equation projection (projection (at (compo ?axis ?rot ?vector) ?time))
  :doc "projection equation inside psm"
  :complexity connect
  :english ("projection equation")
  
  ;:ExpFormat ("writing the projection equation for ~a onto the ~a axis." 
  ;	      (nlg ?vector) (nlg ?axis 'adj))
  :EqnFormat ("~a_~a = ~a*~a($q~a - $q~a)"
	      (nlg ?vector 'nlg-vector-var-pref)
	      (nlg ?axis 'adj) 
	      (nlg ?vector 'nlg-vector-var-pref)
	      (format nil "~:[sin~;cos~]" (equal ?axis X))
	      (nlg ?vector 'nlg-vector-var-pref) (nlg ?axis 'adj)))


;; definition of momentum in component form:
(def-psmclass momentum-compo (?eq-type definition ?axis ?rot 
				       (linear-momentum ?body ?time))
  :english ("definition of momentum component")
  :complexity definition ;so it can be substituted into momentum conservation
  :EqnFormat ("pi_~a = m * v_~a" (nlg ?axis 'adj) (nlg ?axis 'adj)))
