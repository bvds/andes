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
;;;;  KB/ontology: defines the expressions used in the Andes Knowledge Base
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; should match entries in Algebra/src/units.h
(defparameter unit-english
    '(
      (|m| . "meters")
      (|kg| . "kilograms")
      (|s| . "seconds")
      (|C| . "coulombs")
      (|K| . "degrees kelvin")
      (|g| . "grams")
      (|N| . "newtons")
      (|J| . "joules")
      (|V| . "volts")
      (|A| . "amperes")
      (|T| . "teslas")
      (|G| . "gauss")
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

;;;;
;;;;  Engineers like to use the term "moment" instead of "torque"
;;;;  This is turned on via the 'engineering-names problem feature.

(defun torque-switch (x y)
  (if (and *cp* (member 'engineering-names (problem-features *cp*))) x y))   
(defun moment-symbol (&optional junk) ;optional arg for use with nlg
  (declare (ignore junk))
  (torque-switch "M" "&tau;"))
(defun moment-name (&optional junk) ;optional arg for use with nlg
  (declare (ignore junk))
 (torque-switch "moment" "torque"))


;;;             Quantity Terms:

(defun translate-units (x)
  (let ((result (assoc x unit-english)))
    ;; leave untranslated if no name in table:
    (if result (cdr result) (format NIL "~A" x))))

(def-qexp dnum (dnum ?value ?unit :error ?err)
  :nlg-english ("~A~:[~2*~;~A~A~] ~A" (identity ?value) ?err (code-char 177) 
	    ?err (translate-units ?unit)))

;;;; vector quantities:

(def-qexp relative-position (relative-position ?to-pt ?from-pt :time ?time)
  :units |m|
  :nlg-english ("the relative position of ~A with respect to ~A" 
	    (nlg ?to-pt) (nlg ?from-pt 'at-time ?time)))
(def-qexp displacement (displacement ?body :time ?time)
  :units |m|
  :new-english ((preferred "the") (or "displacment" "disp")
		 (and (preferred (property ?body))
		      (preferred (time ?time)))))

(def-qexp velocity (velocity ?body :time ?time)
  :units |m/s|
  :nlg-english ("the velocity of ~A" (nlg ?body 'at-time ?time)))
(def-qexp relative-vel (relative-vel ?to-pt ?from-pt :time ?time)
  :units |m/s|
  :nlg-english ("the relative velocity of ~A with respect to ~A" 
	    (nlg ?to-pt) (nlg ?from-pt 'at-time ?time)))

(def-qexp accel	(accel ?body :time ?time)
  :units |m/s^2|
  :new-english ((preferred "the") (or "acceleration" "accel." "accel")
		(and (preferred (property ?body))
		     (preferred (time ?time)))))

(def-qexp momentum (momentum ?body :time ?time)
  :units |kg.m/s|
  :nlg-english ("the momentum of ~A" (nlg ?body 'at-time ?time)))

(def-qexp force (force ?body ?agent ?type :time ?time)
  :units N
  :new-english ((preferred "the") (eval (force-types ?type))
		(and (preferred (object ?body))
		     (preferred (agent ?agent))
		     (preferred (time ?time)))))

(defun force-types (type)
  (case type 
    (weight '(or "force of gravity"
	      ((or "weight" "gravitational" "grav." "grav") "force")))
    (gravitational '(or "force of gravity"
		     ((or "gravitational" "weight" "grav." "grav") "force")))
    (normal "normal force")
    (tension '((or "tension" "pulling") "force"))
    (applied '((allowed "applied") "force")) ;catch-all force
    (kinetic-friction '(((preferred "kinetic") (or "friction" "frictional"))
			"force"))
    (static-friction  '(((preferred "static") (or "friction" "frictional")) 
			"force"))
    (electric '((or "electric" "E" "coulomb") "force"))
    (magnetic '((or "magnetic" "B") "force"))
    (buoyant '((or "buoyant" "buoyancy") "force"))
    (thrust "thrust force")
    (spring "spring force")
    (pressure "pressure force")
    (drag '((or "drag" "friction" "frictional") "force"))
    (t (warn "unknown force type ~A" type) (format nil "~(~A~) force" type))))

(def-qexp net-force (net-force ?body :time ?time)
  :units N
  :new-english ((preferred "the") (or "net" "total") 
		"force" (and (preferred (object ?body))
			     (preferred (time ?time)))))

(def-qexp ang-displacement (ang-displacement ?body :time ?time)
  :units |rad|
  :nlg-english ("the angular displacement of ~A" (nlg ?body 'at-time ?time)))
(def-qexp ang-velocity (ang-velocity ?body :time ?time)
  :units |rad/s|
  :nlg-english ("the angular velocity of ~A" (nlg ?body 'at-time ?time)))
(def-qexp ang-accel (ang-accel ?body :time ?time)
  :units |rad/s^2|
  :nlg-english ("the angular acceleration of ~A" (nlg ?body 'at-time ?time)))
(def-qexp ang-momentum (ang-momentum ?body :time ?time)
  :units |kg.m^2/s|
  :nlg-english ("the angular momentum of ~A" (nlg ?body 'at-time ?time)))
(def-qexp torque (torque ?body ?agent :axis ?axis :time ?time)
  :units |N.m|
  :nlg-english ("the ~A on ~A~@[ about ~A~] due to ~A" 
	       (moment-name) (nlg ?body) (nlg ?axis)
	       (nlg ?agent 'at-time ?time)))
(def-qexp net-torque (net-torque ?body ?axis :time ?time)
  :units |N.m|
  :nlg-english ("the net ~A on ~A about ~A" 
	       (moment-name) (nlg ?body) (nlg ?axis 'at-time ?time)))
(def-qexp couple (couple orderless . ?bodies)
  :nlg-english ("the couple between ~A"
	       (nlg ?bodies 'conjoined-defnp)))
;; attributes of vectors:
(def-qexp compo	(compo ?xyz ?rot ?vector)
  :units ?vector
  :nlg-english ("the ~A component of ~A" (nlg ?xyz 'adj) (nlg ?vector)))
(def-qexp mag (mag ?vector)
  :units ?vector
  :restrictions nonnegative
  :nlg-english ("the magnitude of ~A" (nlg ?vector)))
(def-qexp dir	(dir ?vector)
  :units |deg|
  :nlg-english ("the direction of ~A" (nlg ?vector)))

;; this is only used by implicit-eqns, so it should never be visible
;; to the user
(def-qexp test-var (test-var . ?angle)
  :units nil
  :restrictions nonnegative)

;; Special axis terms entered into the symbol table. These are not
;; used as quantities, but may need to be Englished.
(def-qexp axis (axis ?xyz ?angle)
   :nlg-english ("the axis at ~A degrees" ?angle))

;;;;         General phrases

(def-qexp property (property ?body)
  :new-english ("of" (or (var (body ?body)) ?body)))

(def-qexp time (time ?time)
  :new-english (eval (pp ?time)))

(def-qexp object (object ?body)
  :new-english ((or "acting on" "on") 
		(or (var (body ?body)) ?body)))

(def-qexp agent (agent ?body)
  :new-english ((or "due to" "by" "from" "caused by") 
		(or (var (body ?body)) ?body)))

;;;; scalar quantities

;;; in the workbench, the time slot is added if feature changing-mass
;;; is included.
(def-qexp mass	(mass ?body :time ?time)
  :symbol-base |m|
  :short-name "mass"	
  :units |kg|
  :restrictions positive
  :new-english ((preferred "the") "mass" (and (preferred (property ?body))
					   (preferred (time ?time)))))

(def-qexp mass-change-magnitude	(mass-change-magnitude ?body ?agent :time ?t)
  :symbol-base |dmdt|     
  :short-name "magnitude of mass change per unit time"	
  :units |kg/s|
  :restrictions nonnegative
  :nlg-english ("the magnitude of the change of mass of ~A per unit time due to ~A~@[ ~A~]" 
	       (nlg ?body) (nlg ?agent 'agent) (nlg ?t 'pp)))
(def-qexp mass-per-length (mass-per-length ?rope)
  :symbol-base |$l|     
  :short-name "mass per length"	
  :units |kg/m|
  :restrictions nonnegative 
  :nlg-english ("the mass-per-length of ~A" (nlg ?rope))
)

(def-qexp distance (distance ?body :time ?time)
  :symbol-base |s|     
  :short-name "distance traveled"	
  :units |m|
  :nlg-english ("the distance traveled by ~A" (nlg ?body 'at-time ?time)))

(def-qexp duration (duration (during ?t1 ?t2))
  :symbol-base |t|     
  :short-name "duration of time"	
  :units |s|
  :restrictions positive
  :nlg-english ("the duration of time between ~A and ~A" 
            (nlg ?t1 'moment) (nlg ?t2 'moment)))
(def-qexp speed (speed ?body :time ?time)
  :symbol-base |v|     
  :short-name "speed"	
  :units |m/s|
  :nlg-english ("the speed of ~A" (nlg ?body 'at-time ?time)))

(def-qexp coef-friction (coef-friction ?body1 ?body2 ?static-or-kinetic :time ?time)
  :symbol-base |$m|     
  :short-name "coef. of friction"	
  :units NIL ;; dimensionless
  :nlg-english ("coefficient of ~(~A~) friction between ~A and ~A" 
            (nlg ?static-or-kinetic NIL) (nlg ?body1) 
	    (nlg ?body2 'at-time ?time))) 

(def-qexp coef-drag-turbulent (coef-drag ?b ?medium :type turbulent :time ?time)
  :symbol-base |K|     
  :short-name "coef. of drag"	
  :units |kg/m|
  :nlg-english ("coefficient of drag for ~A moving through ~A" 
            (nlg ?b) (nlg ?medium 'at-time ?time))) 

;; see constants.cl, function enter-predefs
(def-qexp gravitational-acceleration (gravitational-acceleration ?planet)
  :symbol-base |g|     
  :short-name "gravitational acceleration"	
  :units |m/s^2|
  :restrictions positive
  ;; Gianocolli: "acceleration due to gravity [on the Earth]"
  ;; Cummings .. "gravitational acceleration constant"
  ;;             "the earth's local gravitational strength"
  ;; Hudson & Nelson "acceleration due to gravity at the surface of the earth"
  ;;              "the acceleration of a freely falling object"
  ;; Walker "the acceleration produced by gravity on the Earth's surface"
  ;;        "... gravitational strength ..."
  ;;        "the acceleration of gravity [on [the surface of] the Earth]"
  ;;        "free-fall acceleration"
  ;;        
  :new-english ((preferred "the")
		(or ((or "gravitational" "grav." "grav")
		     (or "acceleration" "accel." "accel") (allowed "constant"))
		    ((or "acceleration" "accel." "accel")
		     (or ((or "due to" "caused by" "of") "gravity")
			 "of a freely falling object")))
		(preferred ((or "at" "on") (preferred "the surface") 
				 (property ?planet)))))

;; Add Earth to Ontology as a universal name
;; Alternatively, it could be added to all problem ontologies
;; involving the earth.
(def-qexp the-Earth earth 
  :new-english ((preferred "the") "Earth"))

(post-process add-gravitational-acceleration (problem)
  "if only the earth's gravity is used, add gravitational acceleration"
  ;; test whether other planets are involved in problem
  (when (and (every #'(lambda (x) (eq (second x) 'earth)) 
		    (Filter-expressions '(near-planet ?p :body ?b)
						 (problem-givens problem))) 
	     ;; test whether acceleration for earth is needed
	     (member '(gravitational-acceleration earth) 
		     (problem-varindex problem) :test #'unify :key #'qvar-exp)
	     ;; test whether it has been done as predef already
	     (not (member '(define-var 
			    (gravitational-acceleration ?place))
			  (problem-predefs problem) :key #'car :test #'unify)))
    ;; Add definition plus equation
    ;; The help system will give these an appropriate color
    ;; when the problem is loaded.
    ;; Not a good idea to have the positions hard-coded like this.
    ;; need to work out a scheme for positioning.
    (push '((EQN (= |g_EARTH| (DNUM 9.8 |m/s^2|))) . 
	    ((:type . "equation") (:text . "g=9.8 m/s^2")))
	  (problem-predefs problem))
    (push '((define-var (gravitational-acceleration earth)) . 
	    ((:type . "statement")
	     (:text . "g is the acceleration of gravity on earth")
	     (:symbol . "g")))
	  (problem-predefs problem))))

(def-qexp num-forces (num-forces ?body :time ?time)
  :nlg-english ("the number of forces on ~A" (nlg ?body 'at-time ?time)))
(def-qexp revolution-radius (revolution-radius ?body :time ?time)
  :symbol-base |r|     
  :short-name "radius of circular motion"	
  :units |m|
  :restrictions positive
  :nlg-english ("the radius of the circular motion of ~A" 
	    (nlg ?body 'at-time ?time)))

;; Halliday and Resnick talk about work done by a force
;; "work done by the spring force"
;; Giancoli
;; "work done by a constant force ..."
;; "the work done by gravity ..."
;; "the work done by each force ..."

(def-qexp work (work ?b ?agent :time ?time)
  :symbol-base |W|     
  :short-name "work"	
  :units |J|
  :new-english ((preferred "the") "work" (preferred "done")
		(and (preferred (object ?b))
		     (preferred (agent ?agent))
		     (preferred (time ?time)))))

;; Halliday & Resnick
;; "work done on ... by all forces"
;; "total work done on ... by all forces that act on it"
;; Giancoli
;; "net work done on ..."

(def-qexp net-work (net-work ?body :time ?time)
  :units |J|
  :new-english (((preferred "the") (preferred (or "total" "net"))
		 "work" (preferred "done")
		 (and (preferred (object ?body))
		      (allowed (agent "all forces"))
		      (preferred (time ?time))))))

;; Giancoli
;; "work done by non-conservative forces ..."

(def-qexp work-nc (work-nc ?body :time ?time)
  :units |J|
  :new-english (((preferred "the") (allowed (or "total" "net"))
		 "work" (preferred "done") 
		 (and (preferred (object ?body))
		      (preferred (agent "non-conservative forces"))
		      (preferred (time ?time))))))

(def-qexp power (power ?b ?agent :time ?time)
  :symbol-base |P|     
  :short-name "power"	
  :units |W|
  :nlg-english ("the power supplied to ~a from ~a" 
	    (nlg ?b) (nlg ?agent 'at-time ?time)))
(def-qexp net-power (net-power ?b :time ?time)
  :units |W|
  :nlg-english ("the net power supplied to ~a" (nlg ?b 'at-time ?time)))
(def-qexp net-power-out (net-power-out ?source :time ?time)
  :symbol-base |P|     
  :short-name "power output" 
  :units |W|
  :nlg-english ("the total power produced by ~A" 
	       (nlg ?source 'at-time ?time)))
(def-qexp angle-between (angle-between orderless . ?vecs)
  :units |deg|
  :restrictions nonnegative 
  :nlg-english ("the angle between ~A" (nlg ?vecs 'conjoined-defnp)))

(def-qexp total-energy (total-energy ?system :time ?time) 
  :units |J|
  :new-english (((preferred "the") 
		 (or ((or "total" "net") "mechanical energy") "TME")
		 (and (preferred (property ?system)) 
		      (preferred (time ?time))))))

(def-qexp kinetic-energy (kinetic-energy ?body :time ?time)
  :units |J|
  :new-english ((preferred "the") (allowed "translational")
		(or "kinetic energy" "KE")
		(and (preferred (property ?body)) 
		     (preferred (time ?time)))))

(def-qexp rotational-energy (rotational-energy ?body :time ?time)
  :units |J|
  :new-english ((preferred "the") (or "rotational" "rot")
		(or "kinetic energy" "KE")
		(and (preferred (property ?body)) 
		     (preferred (time ?time)))))

(def-qexp grav-energy (grav-energy ?body ?agent :time ?time)
  :units |J|
  :new-english ((preferred "the") (or "gravitational" "grav") 
		(or ((preferred (or "potential" "pot")) "energy") "PE")
		(and (preferred (property ?body))
		     (preferred (agent ?agent)) 
		     (preferred (time ?time)))))

;; see bug 1463
(def-qexp spring-energy (spring-energy ?body ?spring :time ?time) 
  :units |J|
  :new-english ((preferred "the") (allowed "elastic") 
		(or ((or "potential" "pot" "spring") "energy") "PE")
		(and (preferred (or (property ?body) ("transmittable to" ?body)))
		     ;; always include spring, since that defines force type.
		     (agent ?spring)
		     (preferred (time ?time)))))

(def-qexp compression (compression ?spring :time ?time)
  :symbol-base |d|     
  :short-name "compression distance"	
  :units |m|
  :new-english ((preferred "the") "compression" (allowed "distance")
		 (and (property ?spring) (preferred (time ?time)))))

(def-qexp spring-constant (spring-constant ?spring)
  :symbol-base |k|     
  :short-name "spring constant"	
  :units |N/m|
  :restrictions positive
  :new-english ((preferred "the") "spring constant" 
		(preferred (property ?spring))))

(def-qexp height (height ?body ?zero-height :time ?time)
  :symbol-base |h|     
  :short-name "height"	
  :units |m|
  :new-english ((preferred "the") "height" 
		(and (property ?body)
		     ;; Assume there is no user defined variable for zero-height
		     (allowed ((or "above" "relative to") ?zero-height))
		     (preferred (time ?time)))))

;; default phrase, in absence of something sensible.
(def-qexp zero-height zero-height
  :new-english (or ((preferred "the") 
		    (or "zero level" "axis" "horizontal axis" "origin"))
		   "zero"))

(def-qexp moment-of-inertia (moment-of-inertia ?body :axis ?axis :time ?time)
  :symbol-base |I|     
  :short-name "moment of inertia"	
  :units |kg.m^2|
  :restrictions positive
  :nlg-english ("the moment of inertia of ~A about ~A" 
	    (nlg ?body) (nlg ?axis 'at-time ?time)))
;; for dimensions of certain rigid bodies:
(def-qexp length (length ?body)
  :symbol-base ||     
  :short-name "length"	
  :units |m|
  :nlg-english ("the length of ~A" (nlg ?body)))
(def-qexp length-change (rate-of-change (length ?body))
  :symbol-base ||     
  :short-name "rate of change in length"	
  :units |m/s|
  :nlg-english ("the rate of change of the length of ~A" (nlg ?body)))
(def-qexp width  (width ?body)
  :symbol-base ||     
  :short-name "width"	  
  :units |m|
  :nlg-english ("the width of ~A" (nlg ?body)))
(def-qexp num-torques (num-torques ?body ?axis :time ?time)
  :nlg-english ("the number of ~As on ~A about ~A" 
	     (moment-name) (nlg ?body) (nlg ?axis 'at-time ?time)))

(def-qexp compound (compound orderless . ?bodies)
  :new-english ((allowed "a compound of") 
		(conjoin (or "and" "&") . ?bodies)))


(def-qexp system (system . ?bodies)
  :new-english ((preferred "a system of") 
		(conjoin (or "and" "&") . ?bodies)))

(def-qexp during (during ?t0 ?t1)
  :new-english (or ("from" (time ?t0) (or "to" "until") (time ?t1))
		   ("between" (time ?t0) (or "and" "&") (time ?t1))))


;; Note when nlg'ing other time arguments in format strings: 
;; 'pp         forms a prepositional phrase: "at T0" or "during T0 to T1"
;; 'moment     just prings a time point name "T0", no preposition
;; 'time       does the same as moment.
;; Omit temporal prepositions in format string if nlg func supplies them.

;;====================================================
;; Entry Propositions.
(def-entryprop body (body ?body :time ?t)
  :Doc "The body tool."
  :nlg-english ("the body for ~a" (nlg ?body 'at-time ?t)))


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
  :nlg-english ("a ~a" (ont-vector-entryprop-format-func ?body ?quantity ?direction)))

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

(def-entryprop draw-line (draw-line ?line ?dir)
  :Doc "The line drawing tool"
  :nlg-english ("a line ~A representing ~A" 
	    (draw-line-entryprop ?dir) (nlg ?line)))

(defun draw-line-entryprop (dir)
  (if (eq dir 'unknown) "in an unknown direction" 
    (format nil "at an angle of ~A" (nlg dir))))
	
(def-entryprop draw-axes (draw-axes ?body ?x-direction)
  ;; until workbench associates bodies and times w/axes, just
  ;; leave them out of the entry proposition for axes
  :helpform (draw-axes ?x-direction)
  :Doc "The Axes drawing tool."
  :nlg-english ("a pair of axes on ~a rotated to ~a" 
	    ?body  (nlg ?x-direction)))
  

(def-entryprop define-var (define-var ?quantity)
  :Doc "Defining a variable for a specific quantity"
  :nlg-english ("a variable for ~a" (nlg ?Quantity)))

(defun choose-answer-ontology-fixfunc (ID)
  (let ((D (format Nil "~a" ID)))
    (if (string-equal (subseq D 0 2) "MC")
	(subseq D 3)
      D)))

(def-entryprop choose-answer (choose-answer ?question-id ?answer-num)
  :doc "Select multiple choice question answer"
  :nlg-english ("answer for multiple-choice question number ~A" 
	    (choose-answer-ontology-fixfunc ?question-id)))

(def-eqn-entryprop eqn (eqn ?equation ?eqn-id) 
  :helpform (eqn ?equation)
  :Doc "Entering an equation with the specified id."
  :nlg-english ("the equation ~a in slot ~a" ?Equation ?Eqn-ID))

(def-eqn-entryprop given-eqn (given-eqn ?equation ?quantity) 
  :helpform (eqn ?equation)
  :Doc "A given equation entry."
  :nlg-english ("the given equation ~a for: ~a" 
	       ?Equation (nlg ?Quantity 'def-np)))
  
(def-eqn-entryprop implicit-eqn (implicit-eqn ?equation ?quantity) 
  :helpform (implicit-eqn ?equation)
  :Doc "An implicit equation entry."
  :nlg-english ("the implicit equation ~a for: ~a"
	     ?Equation (nlg ?quantity 'def-np)))

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

(def-goalprop lk-fbd (vector-diagram ?rot (lk ?body ?time))
   :doc "diagram showing all lk vectors and axes"
   :nlg-english ("drawing a diagram showing all of the needed kinematic vectors of ~A ~A and coordinate axes" 
              (nlg ?body ) (nlg ?time 'pp)))

#| ;; Experimental, maybe following 2 are better handled by operator hints on entries.
;; BvdS:  These also use the internal Andes name as the name of the vector.
;; 
;; Subgoal hint for goal of drawing a vector:
;; We assume first argument is relevant body to mention
(def-goalprop vec-drawn (vector ?axis-body (?vec-type . (?body . ?args)) 
				?direction)
   :nlg-english ("drawing a ~a vector for ~a." (nlg ?vec-type 'adjective) (nlg ?body 'def-np)))

;; Subgoal hint for goal of defining a variable: We use one hint for vector 
;; [mag] variables, another for all others, to show that vector vars are 
;; introduced by drawing.  Note this depends crucially on order of entries and
;; the fact that earlier entries are matched first.
;; Note the second form also gets used for vector component variables 
;; (which may be given)
;; -- maybe should insert another variant for that case.
(def-goalprop vector-var (variable ?var (mag (?vec-type . (?body . ?args) )))
   :nlg-english ("drawing a ~a vector for ~a." (nlg ?vec-type 'adjective) 
					   (nlg ?body 'def-np)))
|# ;; end experimental

(def-goalprop other-var (variable ?var ?quantity)
   :nlg-english ("introducing a variable for ~A" (nlg ?quantity)))

(def-goalprop axes-chosen (axes-for ?body ?rotation)
  ;; !! this goal can be achieved in some cases without drawing 
  ;; by re-using existing axes.
   :nlg-english ("setting coordinate axes"))

(def-goalprop write-projections
      (projections ?component-variables ?component-expressions)
    :nlg-english ("writing projection equations for all the component variables in the equation you are using"))

(def-goalprop avg-vel-fbd (vector-diagram ?rot (avg-velocity ?body ?time))
   :nlg-english ("drawing a diagram showing all of the needed vectors for ~A ~A and coordinate axes" 
             (nlg ?body) (nlg ?time 'pp)))
   
(def-goalprop net-disp-fbd (vector-diagram ?rot (sum-disp ?body ?time))
   :nlg-english ("drawing a diagram showing all of the needed displacements of ~A ~A and coordinate axes" 
              (nlg ?body) (nlg ?time 'pp)))

;; next goal used as sought in fbd-only problem:
(def-goalprop standard-fbd (fbd ?body ?time)
  :doc "free-body-diagram on its own."
  :nlg-english ("drawing a free-body diagram for ~A ~A"
            (nlg ?body) (nlg ?time 'pp))) ; time may be interval or instant

(def-goalprop all-forces (forces ?body ?time ?all-forces)
   :nlg-english ("drawing all the forces acting on ~A ~A" (nlg ?body) (nlg ?time 'pp)))


(def-goalprop all-torques (torques ?b ?axis ?time ?torques)
  :nlg-english ("showing the ~A due to each force acting on ~A ~A" 
	     (moment-name) (nlg ?b) (nlg ?time 'pp)))


;; this goal used as sought in vector-drawing-only problem (magtor*)
(def-goalprop draw-vectors (draw-vectors ?vector-list)
  :nlg-english ("drawing the vectors asked for in the problem statement"))

;; this goal used as sought in variable-defining-only problems (q1)
(def-goalprop define-scalar-variable (define-scalar-variable ?quant)
  :nlg-english ("defining the variable asked for"))

;; this goal used as sought in general qualitative problems (q5)
(def-goalprop do-all-steps (do-all-steps . ?steps)
  :nlg-english ("making the entries asked for in the problem statement"))

;; this goal used as subgoal in general qualitative problems (q5)
(def-goalprop write-eqn (write-eqn ?eqn-name)
  :nlg-english ("writing the equation asked for"))

;; this goal used as sought in multiple-choice question problem
(def-goalprop choose-mc-answer (choose-answer ?question-id ?answer)
   :nlg-english ("answering question ~A" (str-after ?question-id)))

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
;;;; NOTE: For the purposes of the NSH code I am imposing some naming conventions here.
;;;;       All axes names should be of the form ?axis or (?axis0 ... ?axisn) if multiple axes.
;;;;       All body variables should be of the form ?body or [?body0 ... ?bodyn]
;;;;       Times should be ?time or [?time0, ... ?timen]

(def-psmclass given (given ?quantity ?value :hint ?hint)
  :complexity simple
  :doc "enter given value"
  :nlg-english ("the given value")
  :ExpFormat ("~:[entering the given value of~;finding~] ~A" 
	      ?hint (nlg ?quantity))
  :EqnFormat ("Var = N units"))

(def-psmclass projection (projection (compo ?axis ?rot ?vector))
  :complexity connect
  :doc "projection equations"
  :short-name ("on ~A-axis" (axis-name ?axis))
  :nlg-english ("projection equations")
  :ExpFormat ("writing the projection equation for ~a onto the ~a axis" 
	      (nlg ?vector) (axis-name ?axis))
  :EqnFormat ("~a<sub>~a</sub> = ~a ~:[sin~;cos~](&theta;~a - &theta;~a)"
	      ;; vector-var-pref expecting a list which may 
	      ;; only be partially bound
	      (vector-var-pref '?vector) (axis-name ?axis) 
	      (vector-var-pref '?vector) (eq ?axis 'x)
	      (vector-var-pref '?vector) (axis-name ?axis)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given a partially bound vector definition convert it to a type for the
;; nlg of the equation form.  This type will be one of 
;; Displacement d
;; Velocity v
;; accelleration a
;; force F
;; relative position r
;; momentum p
(defun vector-var-pref (x)
  (if (and (consp x) (groundp (car x)))
      (case (car x)
	(displacement "d")
	(velocity "v")
	(acceleration "a")
	(force "F")
	(relative-position "r")
	(momentum "p")
	(t (format nil "~a" (car x))))
      (error "vector-var-pref with ~A" x)))

   
;;-------------------------------------------------------
;; Kinematics top-level group.
(def-psmgroup Kinematics 
    ;;:form (?class ?body ?duration)
    :doc "Kinematical principles.")

(def-psmclass sdd (sdd ?body ?time)
  :group Kinematics
  :complexity major
  :short-name "average speed"
  :doc "Distance = rate * time."
  :nlg-english ("distance = average speed * time")
  :expFormat ((strcat "applying the \"distance = average speed "
		      "* time\" principle with ~a as the body ~a")
	      (nlg ?body) (nlg ?time))
  :EqnFormat ("v(avg) = s/t"))

(def-psmclass displacement-distance (displacement-distance ?body ?time)
  :complexity connect  ;since this is like (equals ...)
  :doc "Distance = Displacement."
  :short-name "distance &amp; displacement"
  :nlg-english ("distance = magnitude of displacment")
  :expFormat ("noting that distance is the magnitude of the displacment")
  :EqnFormat ("|d| = s"))
 
(def-goalprop sdd-eqn (eqn ?algebra (sdd ?body ?time))
  :nlg-english ("writing an equation involving the speed of ~A ~A, and distance it traveled, and the duration of the trip" 
	    (nlg ?body) (nlg ?time 'pp)))

(def-psmclass avg-velocity
    (?eqn-type avg-vel ?axis ?rot (avg-velocity ?body ?time))
    :group Kinematics
    :complexity major    
    :Doc "Definition of average velocity."
    :short-name "average velocity"
    :nlg-english ("the definition of average velocity") 
    :ExpFormat ("applying the definition of average velocity on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("v(avg)<sub>~a</sub> = d<sub>~a</sub>/t" (axis-name ?axis) (axis-name ?axis)))

(def-goalprop avg-vel-eqn 
 (eqn ?algebra (compo-eqn avg-vel ?axis ?rot (avg-velocity ?body ?time)))
 :nlg-english ((strcat "writing an equation for the average velocity of ~A "
		   "~A in terms of components along the ~A axis")
	   (nlg ?body) (nlg ?time 'pp) ?axis))
 

(def-psmclass pyth-thm (pyth-thm ?body ?origin ?time0 ?time1)
  :complexity connect
  :short-name "Pythagorean Thm"
  :nlg-english ("the Pythagorean theorem" )
  :eqnFormat ("c<sup>2</sup> = a<sup>2</sup> + b<sup>2</sup>"))

(def-psmclass sum-distance (sum-distance ?b1 ?b2 ?b3 ?t)
   :complexity definition
   :short-name "sum collinear distances"
   :nlg-english ("the relationship among the distances between ~a, ~a and ~a" 
              (nlg ?b1) (nlg ?b2) (nlg ?b3))
   :ExpFormat ("relating the total distance between ~a and ~a to the distances from these points to ~a" (nlg ?b1) (nlg ?b3) (nlg ?b2))
   :EqnFormat ("rAC = rAB + rBC"))


(def-psmclass net-disp (?eq-type sum-disp ?axis ?rot (sum-disp ?body ?time))
  :complexity minor   ;See Bug #1144
  :short-name "net displacement"
  :nlg-english ("net displacement")
  :ExpFormat ("calculating the net displacement of ~a ~a" (nlg ?body) (nlg ?time))
  :EqnFormat ("dnet<sub>~a</sub> = d1<sub>~a</sub> + d2<sub>~a</sub> + ..." (axis-name ?axis) 
	      (axis-name ?axis) (axis-name ?axis)))

(def-goalprop net-disp-eqn 
 (eqn ?algebra (compo-eqn sum-disp ?axis ?rot (sum-disp ?body ?time)))
 :nlg-english ((strcat "writing an equation for the component of net "
		   "displacement of ~A ~A along the ~A axis") 
	   (nlg ?body) (nlg ?time 'pp) ?axis))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 	Linear kinematic family of equations.
;;
;; special-case constant-velocity equations for x component of 2D projectile:
;; We treat these as members of const accel group (0 accel is constant!), 
;; though it might not be obvious to the students what to choose.
;; Might also use a constant-velocity group, though only component is constant



; FREELY FALLING BODIES
(def-psmclass free-fall-accel (free-fall-accel ?body ?time)
  :complexity simple
  :short-name "accel in free-fall"
  :nlg-english ("free fall acceleration")
  :ExpFormat ("determining the free fall acceleration for ~a"
	      (nlg ?body 'at-time ?time))
  :EqnFormat ("a = g"))

(def-psmclass std-constant-g (std-constant (gravitational-acceleration earth))
  :complexity simple 
  :short-name "g near Earth"
  :nlg-english ("value of g on Earth")
  :expformat ("defining the value of g on Earth")
  :EqnFormat ("g = 9.8 m/s<sup>2</sup>"))


; UNIFORM CIRCULAR MOTION
(def-psmclass centripetal-accel (centripetal-accel ?body ?time)
  :complexity major
  :short-name "centripetal acceleration (instantaneous)"
  :nlg-english ("centripetal acceleration equation: <var>a</var> = <var>v</var><sup>2</sup>/<var>r</var>")
  :ExpFormat ((strcat "writing the centripetal acceleration "
		      "equation: <var>a</var> = <var>v</var><sup>2</sup>/<var>r</var> for ~a")
	      (nlg ?body 'at-time ?time))
  :EqnFormat ("ac = v<sup>2</sup>/r"))
(def-psmclass centripetal-accel-compo (?eqn-type definition ?axis ?rot 
				 (centripetal-accel-vec ?body ?time))
  :complexity major    
  :Doc "Definition of Coulomb's law, component form."
  :short-name ("centripetal acceleration (~A component)" (axis-name ?axis))
  :nlg-english ("centripetal acceleration (component form)") 
  :ExpFormat ("finding the centripetal acceleration of ~a ~a (component form)"
	      (nlg ?body) (nlg ?time 'pp))
  :EqnFormat ("ac<sub>~A</sub> = -v<sup>2</sup>/r r<sub>~A</sub>/r" 
	      (axis-name ?axis) (axis-name ?axis)))


(def-psmclass period-circle (period ?body ?time circular)
   :complexity minor
  :short-name "period of uniform circular motion"
   :nlg-english ("the formula for the period of uniform circular motion")
   :ExpFormat ("calculating the period of the motion of ~A" (nlg ?body))
   :EqnFormat ("T = 2 &pi; r/v"))

;; MISCELLANEOUS 

(def-psmclass equals (equals ?quant1 ?quant2 :opposite ?flag . ?rest)
  :complexity connect
  :short-name "equivalent quantities"
  :nlg-english ("find by equivalent quantity")
  :ExpFormat ("applying the fact that ~a is the ~:[same as~;opposite of~] ~A"
              (nlg ?quant1) ?flag (nlg ?quant2))
  :EqnFormat ("val1 = ~Aval2" (code-char 177)))   ;plus/minus


(def-psmclass sum-times (sum-times ?tt :middle ?ti)
  :complexity connect
  :short-name "sum of times"
  :nlg-english ("tac = tab + tbc")
  :ExpFormat ("calculating the sum of times ~A" (nlg ?tt 'pp)) 
  :EqnFormat ("tac = tab + tbc"))


(def-psmclass sum-distances (sum-distances ?b ?tt)
  :complexity connect
  :short-name "sum distance traveled"
  :nlg-english ("sac = sab + sbc")
  :ExpFormat ("calculating the sum of distances traveled by ~A ~A" 
	      (nlg ?b) (nlg ?tt 'pp))
  :EqnFormat ("sac = sab + sbc"))


;; NEWTONS LAW family of equations.
(def-psmgroup NL
    :form (?eq-type ?compo-eqn-id ?axis ?rot (NL ?body ?time :net ?netp))
    :nlg-english ("Newton's second law"))

;; NSL now stands for all versions, same as group id:
(def-psmclass NSL (?eq-type ?c-eqn-id ?axis ?rot (NL ?body ?time :net ?netp)) 
     :group NL
     :complexity major
     :doc "Newton's second law when accel is non-zero"
  :short-name "Newton's second law"
     :nlg-english ("Newton's second law")
     :ExpFormat ("applying Newton's second law to ~a"
		 (nlg ?body 'at-time ?time))
     :EqnFormat ("F1<sub>~a</sub> + F2<sub>~a</sub> + ... = m a<sub>~a</sub>" 
                 (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))

(def-psmclass net-force (?eq-type definition ?axis ?rot (net-force ?body ?t))
  :complexity minor ;See Bug #1144
  :short-name ("net force (~A component)" (axis-name ?axis))
  :nlg-english ("net force")
  :ExpFormat ("calculating the net force acting on ~a ~a" (nlg ?body) (nlg ?t))
  :EqnFormat ("Fnet<sub>~a</sub> = F1<sub>~a</sub> + F2<sub>~a</sub> + ..." 
	      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))


;;; NOTE: Because this principle is applied to objects such as "the Air Bag"
;;; and "the Table" that we do not necessarily want to have the students 
;;; draw I use the ?Object variables.  ?Body variables imply drawn bodies.
(def-psmclass NTL (NTL (?Object0 ?Object1) ?force-type ?time)
  :complexity major
  :short-name "Newton's Third law (magnitudes)"
  :nlg-english ("Newton's Third law")
  :ExpFormat ("applying Newton's Third law to ~a and ~a"
	      (nlg ?Object0) (nlg ?Object1 'at-time ?time))
  :EqnFormat ("Fof1on2 = Fof2on1"))

(def-psmclass NTL-vector (?eq-type NTL ?axis ?rot (NTL-vector (?Object0 ?Object1) ?force-type ?time))
  :complexity major
  :short-name "Newton's Third law (components)"
  :nlg-english ("Newton's Third law")
  :ExpFormat ("applying Newton's Third law to ~a and ~a"
	      (nlg ?Object0) (nlg ?Object1 'at-time ?time))
  :EqnFormat ("F12<sub>~a</sub> = - F21<sub>~a</sub>" (axis-name ?axis) (axis-name ?axis)))

; FORCE LAWS
(def-psmclass wt-law (wt-law ?body ?time)
  :complexity minor
  :short-name "weight law"
  :nlg-english ("Weight law")
  :ExpFormat ("applying the Weight law on ~a" (nlg ?body))
  :EqnFormat ("Fw = m g"))

(def-psmclass kinetic-friction (kinetic-friction ?body ?surface ?time)
  :complexity simple
  :short-name "kinetic friction"
  :nlg-english ("Kinetic Friction law")
  :ExpFormat ("applying the Kinetic friction law for ~a and ~a"
	      (nlg ?body) (nlg ?surface 'at-time ?time))
  :EqnFormat ("Ff = &mu;k Fn"))

(def-psmclass static-friction (static-friction ?body ?surface ?time) 
  :complexity simple
  :short-name "static friction (at max)"
  :nlg-english ("Static Friction at maximum")
  :expformat ("applying the Definition of Static friction for ~a and ~a"
	      (nlg ?body) (nlg ?surface 'at-time ?time))
  :EqnFormat ("Ff = &mu;s Fn"))

(def-psmclass drag-force-turbulent (drag-force ?body ?medium turbulent ?time)
  :complexity simple
  :short-name "drag force"
  :nlg-english ("drag force")
  :ExpFormat ("finding the drag force on ~a moving at high speed through ~A"
	      (nlg ?body) (nlg ?medium 'at-time ?time))
  :EqnFormat ("F = K v<sup>2</sup>"))

;; silly, num-forces = <count of the forces>
(def-psmclass num-forces (num-forces ?body ?time) 
  :complexity simple
  :short-name "number of forces" 
  :nlg-english ("count forces")
  :expformat ("counting the total number of forces acting on ~a"
		 (nlg ?body 'at-time ?time))
  :EqnFormat "nf = (count forces)")

(def-psmclass ug (ug ?body ?agent ?time ?distance-quant)
  :complexity major 
  :short-name "universal gravitation"
  :nlg-english ("Newton's law of Universal Gravitation")
  :expformat ("applying Newton's law of Universal Gravitation for the force on ~a due to ~a" (nlg ?body) (nlg ?agent))
  :EqnFormat ("Fg = G m1 m2/r<sup>2</sup>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONNECTED BODIES
(def-psmclass connected-accels (connected-accels ?body0 ?body1 ?time)
  :complexity connect
  :short-name "connected accelerations"
  :nlg-english ("connected bodies have same acceleration")
  :expformat ((strcat "using the fact that ~a and ~a are connected and "
		      "therefore have the same acceleration")
	      (nlg ?body0) (nlg ?body1))
  :EqnFormat ("a1 = a2"))
    
(def-psmclass connected-velocities (connected-velocities ?body0 ?body1 ?time)
  :complexity connect
  :short-name "connected velocities"
  :nlg-english ("connected bodies have same velocity")
  :expformat ((strcat "using the fact that ~a and ~a are connected and "
		      "therefore have the same velocity")
	      (nlg ?body0) (nlg ?body1))
  :EqnFormat ("v1 = v2"))

(def-psmclass tensions-equal (tensions-equal ?string (?body0 ?body1) ?time)
  :complexity connect
  :short-name "equal tensions at both ends"
  :nlg-english ("tension equal throughout string")
  :expformat ((strcat "using the fact that the tension forces are always "
		      "equal at both ends of a string with ~a") (nlg ?string))
  :EqnFormat ("Ft1 = Ft2"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPOUND BODIES 
(def-psmclass mass-compound (mass-compound ?compound)  
  :complexity connect
  :short-name "mass of compound"
  :nlg-english ("mass of a compound body is sum of masses of parts")
  :expformat ((strcat "using the fact that the mass of ~a "
		      "is the sum of the masses of its parts") 
	      (nlg `(compound orderless . ,?compound)))
  :EqnFormat ("M = m1 + m2 + ..."))

(def-psmclass kine-compound (kine-compound ?vec-type ?bi ?compound ?time) ; part, whole same kinematics
  :complexity connect
  :short-name "velocity of compound"
  :nlg-english ("~A of compound same as part" (nlg ?vec-type))
  :expformat ("applying the fact that the ~A of a compound is same as that of its parts" (nlg ?vec-type))
  :EqnFormat ("v_compound = v_part"))

					  
(def-psmclass force-compound (force-compound ?type ?agent ?compound ?time)
  :complexity connect
  :short-name "force on compound"
  :nlg-english ("external force on a compound")
  :expformat ((strcat "applying the fact that there is an external force "
		      "on ~a due to ~a ~a of type ~a")
	      (nlg `(compound orderless . ,?compound)) 
	      (nlg ?agent 'agent) (nlg ?time 'pp) (nlg ?type))
  :EqnFormat ("F_on_part = F_on_compound"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WORK and ENERGY

;; the form tagged "work" matches either form of the work equation
(def-psmclass work (work ?body ?agent (during ?time0 ?time1) ?rot)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "work defined"
  :nlg-english ("the definition of work")
  :expformat ("calculating the work done on ~a by ~a from ~a to ~a" 
	      (nlg ?body) (nlg ?agent) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("W = F d cos(&theta;) or W = F<sub>x</sub> d<sub>x</sub> + F<sub>y</sub> d<sub>y</sub>"))

;; No pattern to select only the component form of the work equation, 
;; since a variable will match NIL for the angle form as well as the 
;; rotation arg for component form

(def-psmclass net-work (net-work ?body (during ?time0 ?time1)) ; by all forces
  :complexity major   ;See Bug #1144
  :short-name "net work defined"
  :nlg-english ("the definition of net work")
  :expformat ("calculating the net work done by all forces on ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("Wnet = WF1 + WF2 + ..."))

(def-psmclass work-nc (Wnc ?body (during ?time0 ?time1))
  :complexity minor  ; definition, but *can't* be first "principle" for sought
  :short-name "work by non-conservative"
  :nlg-english ("the definition of Wnc")
  :expformat ("representing the work done on ~A by non-conservative forces"
              (nlg ?body))
  :EqnFormat ("Wnc = Wncf1 + Wncf2 + ..."))

(def-psmclass work-energy (work-energy ?body (during ?time0 ?time1))
  :complexity major
  :short-name "work-energy theorem"
  :nlg-english ("the work-kinetic energy theorem")
  :expformat ("applying the work-kinetic energy theorem to ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("Wnet = Kf - Ki"))

(def-psmclass cons-energy (cons-energy ?body ?t1 ?t2)
  :complexity major
  :short-name "[Wnc=0] conservation of mechanical energy"
  :nlg-english ("conservation of mechanical energy")
  :ExpFormat ((strcat "applying conservation of mechanical energy to ~a "
		      "from ~a to ~a") 
	      (nlg ?body) (nlg ?t1 'time) (nlg ?t2 'time))
  :EqnFormat ("ME1 = ME2"))

(def-psmclass change-ME (change-ME ?body ?t1 ?t2)
  :complexity major
  :short-name "change in mechanical energy"
  :nlg-english ("change in mechanical energy")
  :ExpFormat ((strcat "applying change in mechanical energy to ~a "
		      "from ~a to ~a") 
	      (nlg ?body) (nlg ?t1 'time) (nlg ?t2 'time))
  :EqnFormat("Wnc = ME2 - ME1"))

(def-psmclass height-dy (height-dy ?body ?zero-height ?time)
  :complexity connect
  :short-name "change in height"
  :nlg-english ("the height change-displacement relationship")
  :expformat ((strcat "relating the change in height of ~a ~a to the y "
		      "component of its displacement")
	      (nlg ?body) (nlg ?time 'pp))
  :EqnFormat ("h2 - h1 = d12<sub>y</sub>"))

(def-psmclass power (power ?body ?agent ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "average power defined"
  :nlg-english ("the definition of average power")
  :expformat ("applying the definition of average power supplied to ~A by ~A ~A"
	      (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
  :EqnFormat("P(avg) = W/t"))

(def-psmclass net-power (net-power ?body ?time)
   :complexity major  ;See Bug #1144
  :short-name "average net power defined"
   :nlg-english ("the definition of net power")
   :expformat ("applying the definition of net power supplied to ~A ~A"
               (nlg ?body) (nlg ?time 'pp))
   :EqnFormat("Pnet = Wnet/t"))

(def-psmclass inst-power (inst-power ?body ?agent ?time ?rot)
   :complexity major ; definition, but can be first "principle" for sought
  :short-name "instantaneous power"
   :nlg-english ("the instantaneous power principle")
   :expformat ("calculating the instantaneous power supplied to ~A by ~A ~A"
               (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
   :EqnFormat("P = F v cos(&theta;) or P = F<sub>x</sub> v<sub>x</sub> + F<sub>y</sub> v<sub>y</sub>"))

;; !! "total-energy" label used in kb is misleading, it is, in fact,
;; total *mechanical* energy.

(def-psmclass mechanical-energy  (total-energy-top ?body ?time) 
  :short-name "mechanical energy defined"
  :nlg-english ("the definition of mechanical energy")
  :complexity definition
  :EqnFormat ("ME = KE + &Sigma Ui"))

; These are now top level psms, not subequations:
(def-psmclass kinetic-energy (kinetic-energy ?body ?time)
  :short-name "kinetic energy defined"
  :nlg-english ("the definition of kinetic energy")
  :complexity definition
  :EqnFormat ("KE = 0.5 m v<sup>2</sup>"))

(def-psmclass rotational-energy (rotational-energy ?body ?time)
  :short-name "rotational kinetic energy defined"
  :nlg-english ("the definition of rotational kinetic energy")
  :complexity definition
  :EqnFormat ("KE = 0.5 I &omega;<sup>2</sup>"))

(def-psmclass grav-energy (grav-energy ?body ?planet ?zero-height ?time)
  :short-name "gravitational potential energy"
   :nlg-english ("gravitational potential energy")
   :complexity definition
   :EqnFormat ("Ug = m g h"))

(def-psmclass spring-energy (spring-energy ?body ?spring ?time)
  :short-name "spring potential energy"
  :nlg-english ("spring potential energy")
  :complexity definition
  :EqnFormat ("Us = 0.5 k d<sup>2</sup>"))


;; LINEAR MOMENTUM
(def-psmclass cons-linmom (?eq-type lm-compo ?axis ?rot (cons-linmom ?bodies ?time))
  :complexity major
  :short-name "conservation of momentum"
  :nlg-english ("conservation of momentum")
  :expformat ("applying Conservation of Linear Momentum to ~a ~a"
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time 'time))
  :EqnFormat ("p1i<sub>~a</sub> + p2i<sub>~a</sub> + ... = p1f<sub>~a</sub> + p2f<sub>~a</sub> + ..." 
	      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) 
	      (axis-name ?axis)))

(def-psmclass cons-ke-elastic (cons-ke-elastic ?bodies (during ?time0 ?time1))
  :complexity major
  :short-name "elastic collision defined"
  :nlg-english ("conservation of kinetic energy in elastic collisions")
  :expformat ((strcat "applying Conservation of Kinetic Energy to "
		      "elastic collisions of ~a from ~a to ~a")
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("KEf = KEi"))

;; ROTATIONAL KINEMATICS

(def-psmclass linear-vel (linear-vel ?pt ?time ?axis)
  :complexity major
  :short-name "linear velocity at a certain radius"
  :nlg-english ("linear velocity of rotating point")
  :ExpFormat ("calculating the linear velocity of the rotating point ~a"
	      (nlg ?pt 'at-time ?time))
  :EqnFormat ("v = &omega; r"))

(def-psmclass rolling-vel (rolling-vel ?body ?axis ?time)
  :complexity major
  :short-name "linear velocity of rolling object"
  :nlg-english ("linear velocity of rolling object")
  :ExpFormat ("finding the relation between linear motion and rotational motion of ~A"
	      (nlg ?body 'at-time ?time))
  :EqnFormat ("v = &omega; r"))

;;;; ROTATIONAL DYNAMICS (TORQUE)

(def-psmclass mag-torque (mag-torque ?body ?pivot (force ?pt ?agent ?type) ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name ("~A defined (magnitude)" (moment-name))
  :nlg-english ("the definition of ~A magnitude" (moment-name))
  :expformat ((strcat "calculating the magnitude of the ~A "
		      "on ~a ~a due to the force acting at ~a")
	      (moment-name) (nlg ?body) (nlg ?time 'pp) (nlg ?pt))
  :EqnFormat ((torque-switch "M = r F sin(&theta;)" "&tau; = r F sin(&theta;)")))

(def-psmclass torque 
  (torque ?xyz ?rot ?body ?pivot (force ?pt ?agent ?type) ?angle-flag ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name ("~A defined (~A component)" (moment-name) (axis-name ?xyz))
  :nlg-english ("the definition of ~A" (moment-name))
  :expformat ((strcat "calculating the ~A component of the ~A "
		      "on ~a ~a due to the force acting at ~a")
	      (axis-name ?xyz)
	      (moment-name) (nlg ?body) (nlg ?time 'pp) (nlg ?pt))
  :EqnFormat ((torque-equation ?xyz)))

(defun torque-equation (xyz)
  (cond ((eq xyz 'x) (torque-switch "M<sub>x</sub> = r<sub>y</sub> F<sub>z</sub> - r<sub>z</sub> F<sub>y</sub>"
				    "&tau;<sub>x</sub> = r<sub>y</sub> F<sub>z</sub> - r<sub>z</sub> F<sub>y</sub>"))
	((eq xyz 'y) (torque-switch "M<sub>y</sub> = r<sub>z</sub> F<sub>x</sub> - r<sub>x</sub> F<sub>z</sub>"
				    "&tau;<sub>y</sub> = r<sub>z</sub> F<sub>x</sub> - r<sub>x</sub> F<sub>z</sub>"))
	((eq xyz 'z) 
	 (torque-switch 
	  "M<sub>z</sub> = r F sin(&theta;F-&theta;r) or M<sub>z</sub> = r<sub>x</sub> F<sub>y</sub> - r<sub>y</sub> F<sub>x</sub>"
	  "&tau;<sub>z</sub> = r F sin(&theta;F-&theta;r)) or &tau;<sub>z</sub> = r<sub>x</sub> F<sub>y</sub> - r<sub>y</sub> F<sub>x</sub>"))))


;;
;; required-identities -- identity equations that must always be written out
;;
;; This is used by the constraints in Help/interpret-equation.cl that 
;; check whether fundamental equations are written explicitly. Those constraints
;; allow variable identity equations to be exploited when formulating principles,
;; because they usually are trivial artifacts of choice of a quantity. But
;; a few identities express important principles that we want to see written
;; explicitly, so we enumerate them here.
;;
;; !!! Keeping exception list here is fragile wrt changes in kb. Maybe could 
;; just exclude any identity that is marked as "major"? Though that wouldn't block
;; projection, which is minor.
;;
;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 *required-identities* 
 '( NTL					;Newton's Third law
   cons-energy 			        ;conservation of energy ME1 = ME2
   projection    			;projection psm: block using v for v_x when equal
   charge-same-caps-in-branch		;want students to show they know this
   ) #+sbcl #'equalp)

(defun required-identity-p (eqn)
"true if eqn is on the list of required identities"
    (member (first (eqn-exp eqn)) *required-identities*))


;; Detecting net quant variant forms
;;
;; A few error handlers need to know the name of the variant net form of a 
;; particular quantity, in order to avoid saying, e.g, no work variables 
;; are used in the solution when in fact a net-work variable is used. 
;; Following function maps a quantity name symbol -- the car of some quantity 
;; expression -- to that of its net form, if one exists.
;; Returns NIL if no net form. 
;; Note error handlers may wind up looking up e.g. (define-var (NIL ...))
;; in this case, so that should never be used as a quantity name.
(defun get-net-quant-name (quant-name)
  (when (member quant-name '(work power potential intensity db-intensity
	                   force torque field))
      (format-sym "NET-~A" quant-name)))

