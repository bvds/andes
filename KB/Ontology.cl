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


(defun problem-axes (problem)
  "return list of possible problem axes propositions."
  (problem-atoms problem))

(defun problem-atoms (problem)
  "return list of problem atomic propositions."
  ;; Start of with problem-specific ontology, then go
  ;; through list of bodies, etc.
  (let ((results 
	 (remove-if #'consp 
		    (mapcar #'car (problem-english problem)))))
    (dolist (prop 
	      (second (find 'bodies (problem-choices problem) :key #'car)))
	(pushnew prop results))
    (dolist (prop (second 
		   (find 'locations (problem-choices problem) :key #'car)))
	(pushnew prop results))
    (dolist (prop (second 
		   (find 'positions (problem-choices problem) :key #'car)))
	(pushnew prop results))
    results))

(defun problem-vectors (problem)
  "Return a list of vectors defined in a problem."
  (if (problem-wm problem)
      (mapcar #'third (remove '(vector . ?rest) 
			      (problem-wm problem)
			      :test-not #'unify))
      ;; Only needed for dra1a-old and magtor1a (as of Oct. 2010).
      ;; These are vector drawing problems involving torques.
      (mapcar #'second (remove '(vector . ?rest)
			       (loop for x in (second (problem-graph problem))
				     append (collect-psmgraph-csdo-effects
					     (enode-path x)))
			       :test-not #'unify))))

(defun problem-couples (problem)
  "Return a list of couples defined in a problem."
  ;; As of Oct. 2010, this only applies to problem static1t
  (remove '(couple . ?rest)
	    (problem-givens problem)
	    :test-not #'unify))

(defun generate-subsets (set)
  "List of all subsets of the problem atoms."
  (let ((atoms (sort (copy-list set) #'string>)) 
	result)
    (dotimes (x (ash 1 (length atoms)))
      (let (this)
	(dotimes (i (length atoms))
	  (unless (= (logand x (ash 1 i)) 0)
	    (push (elt atoms i) this)))
	(push this result)))
    (remove nil result)))

(defun problem-bodies-and-compounds (problem)
  (if t
      ;; Only use compound bodies that are explicitly declared.
      ;; This covers all solution quantities, but may be too
      ;; restrictive if student has trouble with defining 
      ;; compounds.
      (append
       (mapcar #'second
	       (remove '(object . ?rest)
		       (problem-givens problem)
		       :test-not #'unify))
       (problem-atoms problem))
      ;; generate compound from all problem atoms
      (mapcar #'(lambda (x) (if (cdr x) 
				(append '(compound orderless) x) 
				(car x)))
	      (generate-subsets (problem-atoms problem)))))


;;;             Quantity Terms:
(def-qexp dnum (dnum ?value ?unit :error ?err)
  :new-english ((eval (format nil
			      "~A~:[~2*~;~A~A~]~@[ ~A~]" 
			      (algebra ?value) ?err  #\PLUS-MINUS_SIGN 
			      ?err (when ?unit (algebra ?unit))))))

;;;; vector quantities:

;; ex) "the position of the ball relative to the observer"
;;     "the ball's relative position to the observer"
;;     "the ball's position with respect to the observer"
(def-qexp relative-position (relative-position ?to-pt ?from-pt :time ?time)
  :rank vector
  :units |m|
  :short-name "relative position"
  ;; see relative-vel
  :new-english (or ((property-object (key "position") ?to-pt)
		    (preferred ((or "relative to" "with respect to" "from") 
				?from-pt))
		    (time ?time))
		   ((property-object ("relative" (key "position")) ?to-pt)
		    (preferred ((or "from" "with respect to") ?from-pt))
		    (time ?time))))

;; "the car's displacement between T0 and T1"
(def-qexp displacement (displacement ?body :time ?time)
  :rank vector
  :units |m|
  :short-name "displacement"
  ;; BvdS:  is there a common abbreviation?
  ;; Can't use property-object, because "average" doesn't make sense.
  :new-english ((the) (key "displacement")
		(and (preferred (property ?body)) (time ?time))))

;; ex) "the average velocity of the car between T0 and T1"
;;     "the car's average velocity between T0 and T1" (to do)
(def-qexp velocity (velocity ?body :time ?time)
  :rank vector
  :units |m/s|
  :short-name "velocity"
  :new-english (property-object (key (or "velocity" "vel." "vel")) ?body 
				:time ?time))

;; ex) "the velocity of the ball relative to the observer"
(def-qexp relative-vel (relative-vel ?to-pt ?from-pt :time ?time)
  :rank vector
  :units |m/s|
  :short-name "relative velocity"
  ;; see relative-position
  :new-english ((the) 
		(time-type ?time)
		(or ((key (or "velocity" "vel." "vel")) (property ?to-pt) 
		     (preferred ("relative to" ?from-pt)))
		    ((allowed "relative") 
		     (key (or "velocity" "vel." "vel")) (property ?to-pt) 
		     (preferred ("with respect to" ?from-pt))))
		(time ?time)))

(def-qexp accel	(accel ?body :time ?time)
  :rank vector
  :units |m/s^2|
  :short-name "acceleration"
  :new-english (property-object (key (or "acceleration" "accel." "accel")) 
				?body :time ?time))

(def-qexp momentum (momentum ?body :time ?time)
  :rank vector
  :units |kg.m/s|
  :short-name "momentum"
  :new-english (property-object (key (or "momentum" "mom." "mom")) 
				?body :time ?time))

;; ex) "the constant normal force that the man acts on the crate"
;;     "the constant normal force of the man acting on the crate"
;;     "the average force exerted on the pier between T0 and T1" in dt3b 
;;     "the tension in the wire" in s13 ("wire" is not a defined object in s13)
;; "the frictional force on the aircraft"
;; "the frictional force against the aircraft"
;; "force due to gravity" Raj experiment subject Nov 2010.

;; For tension, need to distinguish scalar and vector
;; forms (see Bug #1719):
;; "tension in the string" (scalar case)
;; "tension force on the brick due to the string" (vector case)
;; 
(def-qexp force (force ?body ?agent ?type :time ?time)
  :rank vector
  :units N
  :short-name "force"
  :new-english 
  ((the)
   (eval (when (time-intervalp ?time)
	   '(allowed (or "constant" "const." "const" "steady" 
		"average" "avg."))))
   (eval (force-types ?type)
	 (?type . (mapcar #'car *force-types*)))
   (or 
    ;; This should handle "force of the man acting on the crate"
    (and (preferred (object ?body)) 
	 (preferred (agent ?agent)) 
	 (time ?time))
    ;; case "the force that the man exerts on the crate"
    (and ((or "that" "with which") ?agent (or "exerts on" "acts on") ?body) 
	 (time ?time)))))

(defparameter *force-types*
  ;; Generally, we demand that the student specifies
  ;; the kind of force, if known.  This might be a little problematic
  ;; for odd forces like "spring" or "thrust".
  '((weight . (or ("force" (or "of" "due to") "gravity")
	       ((or "weight" "gravitational" "grav." "grav") "force")))
    (gravitational . (or ("force" (or "of" "due to") "gravity")
		      ((or "gravitational" "weight" "grav." "grav") "force")))
    ;; "normal force exerted on a body by the surface" from Y&F 
    (normal . ("normal" (key "force")))
    (tension . (or ((key "tension") (preferred "force")) 
		("pulling" (key "force")) 
		((key "force") "of" "tension")))
    ;; catch-all force.  In this case, the force type is allowed.
    (applied . ((allowed (or "applied" "external")) (key "force")))
    (kinetic-friction . (or (((preferred "kinetic") 
			      (or "friction" "frictional")) "force")
			 ("force" "of" (preferred "kinetic") "friction")))
    (static-friction . (or (((preferred "static") 
			     (or "friction" "frictional")) "force")
			("force" "of" (preferred "static") "friction")))
    (electric . ((or "electric" "E" "coulomb") (key "force")))
    (magnetic . ((or "magnetic" "B") (key "force")))
    (buoyant . ((or "buoyant" "buoyancy") (key "force")))
    (thrust . ("thrust" (key "force")))
    (spring . ("spring" (key "force")))
    (pressure . (or ((key "pressure") (allowed "force"))
		 ((key "force") "of pressure")))
    (drag . ((or "drag" "friction" "frictional") (key "force")))))

(defun force-types (type)
  (or (cdr (assoc type *force-types*))
      (warn "unknown force type ~A" type)
      (format nil "~(~A~) force" type)))

(def-qexp net-force (net-force ?body :time ?time)
  :rank vector
  :units N
  :short-name "net force"
  :new-english ((the) (or "net" "total") 
		(key "force") (and (preferred (object ?body))
				   (time ?time))))

(def-qexp rotation-adj (rotation-adj)
  :new-english (or "angular" "rotational" "orbital" 
		   "rot." "ang." "orb." "rot" "ang") )

(def-qexp ang-displacement (ang-displacement ?body :time ?time)
  :rank vector
  :units |rad|
  :short-name "angular displacement"
  :new-english (property-object 
		((rotation-adj) (or "displacement" "disp." "disp"))
		?body :time ?time))

(def-qexp ang-velocity (ang-velocity ?body :time ?time)
  :rank vector
  :units |rad/s|
  :short-name "angular velocity"
  :new-english (property-object
		((rotation-adj) (key (or "velocity" "vel." "vel")))
		?body :time ?time))

(def-qexp ang-accel (ang-accel ?body :time ?time)
  :rank vector
  :units |rad/s^2|
  :short-name "rotational acceleration"
  :new-english (property-object 
		((rotation-adj) (key (or "acceleration" "accel." "accel")))
		?body :time ?time))

(def-qexp ang-momentum (ang-momentum ?body :time ?time)
  :rank vector
  :units |kg.m^2/s|
  :short-name "angular momentum"
  :new-english (property-object
		((rotation-adj) (key (or "momentum" "mom." "mom")))
		?body :time ?time))

(def-qexp torque (torque ?body ?agent :axis ?axis :time ?time)
  :rank vector
  :units |N.m|
  :short-name "torque"
  :new-english 
  ((the) (key (eval (moment-name)))
   (and (preferred (object ?body))
	;; eval is a wrapper simply to specify agent
	(eval '(preferred (agent ?agent))
	      (?agent . (mapcar #'remove-time 
				;; torques are due to forces and couples
				(append (remove '(force . ?rest)
						(problem-vectors *cp*)
						:test-not #'unify)
					(problem-couples *cp*)))))
	(preferred (eval (when ?axis '("about" ?axis))
			 ;; include case where axis is omitted
			 (?axis . (cons nil (problem-axes *cp*)))))
	(time ?time))))

(def-qexp net-torque (net-torque ?body ?axis :time ?time)
  :rank vector
  :units |N.m|
  :short-name "net torque"
  :new-english ((the) (or "net" "total") (key (eval (moment-name)))
		(and (preferred (object ?body))
		     (preferred (eval (when ?axis '("about" ?axis))
				      (?axis . (problem-axes *cp*))))
		     (time ?time))))

(def-qexp couple (couple orderless . ?bodies)
  :new-english ((the) "couple between" 
		(conjoin (or "and" "&") . ?bodies)))

;; attributes of vectors:
(def-qexp compo	(compo ?xyz ?rot ?vector)
  :rank scalar
  :units ?vector
  :short-name "vector component"
  :new-english ((or ((the) ?xyz (or "component" "compo." "compo"))
		    ((the) (eval (format nil "~A-component" ?xyz)))
		    ;; possessive is not allowed. ex) "the driver's 
		    ;; x-component of force on the driver"
		)
		(property ?vector))
)

(def-qexp mag (mag ?vector)
  :rank scalar
  :units ?vector
  :restrictions nonnegative
  :short-name "vector magnitude"
  :new-english ((allowed "the value of") 
		;; "length"
		(the) (or "magnitude" "mag." "mag" (allowed "strength")) 
		(property ?vector)))

(def-qexp dir (dir ?vector)
  :rank scalar
  :units |deg|
  :short-name "vector direction"
  :new-english ((the) (or "direction" "dir." "dir" (allowed "angle"))
		(property ?vector)))

;; this is only used by implicit-eqns, so it should never be visible
;; to the user
(def-qexp test-var (test-var . ?angle)
  :units nil
  :restrictions nonnegative)

;; Special axis terms entered into the symbol table. These are not
;; used as quantities, but may need to be Englished.
;; See Bug #1685
(def-qexp axis (axis ?xyz ?angle)
  :new-english ((the) (eval (nlg ?xyz 'adj)) "axis"
		(eval (list (if (= ?angle 0) 'allowed 'preferred) 
			    (format nil "at ~A degrees" ?angle)))))

;;;;         General phrases

(def-qexp var-wrapper (variable . ?rest)
  :new-English (case-sensitive (var . ?rest)))

(def-qexp property (property ?body)
  ;; "for" is exceptionally used
  :new-english ("of" (or (variable ?body :namespace :objects) 
			 ;; eval as wrapper to set possible values
			 (eval ?body
			       (?body . (problem-bodies-and-compounds *cp*)))
			 ))) 


(def-qexp time-derivative (time-derivative ?property :agent ?agent :time ?time)
  ;; should work when agent or time are omitted.
  :new-english ((the)
		(or "change" "rate of change" "difference" "diff.") 
		(or "in" "of") 
		?property
		(preferred "per unit time")
		(and (preferred (agent ?agent)) (time ?time))))

(def-qexp time-type (time-type ?time)
  ;; These are redundant with time arguments, which are usually "preferred;"
  ;; make these "allowed" so that both don't show up in default form.
  :new-english (eval (cond
		       ((time-intervalp ?time)
			   ;; to allow "constant", would have to look at problem
			 '(allowed (or "average" "avg." "avg")))
		       ((time-pointp ?time)
			 ;; to do "initial" or "final", would have to adjust
			 ;; time ontology and look at problem times.
			 '(allowed (or "instantaneous" "instant." "instant")))
		       ((null ?time) nil)
		       (t (warn "time-type:  Bad time ~A" ?time)))))

;; ex) "the average speed of the car at time T1"
;;    "the car's average speed at time T1"  (to do!)
;;    "the mass of the crate"
;;    "the crate's mass" (to do!)
;;    "the average velocity of the car between T0 and T1"
;;    "the car's average velocity between T0 and T1"  (to do)
;;    "the car's displacement between T0 and T1"  (to do)
;; Exclude "value of", without special help.
(def-qexp property-object (property-object ?property ?body
					   :modifier ?mod :time ?time)
  ;; Handles timeless case properly.
  :new-english (or ((the) (time-type ?time)
		    ?property  ;the quantity
		    (and (preferred (property ?body)) ?mod (time ?time)))
		   ;; See if "initial" or "final" is applicable.
		   (eval (let ((tp (collect-extremal-time-points *cp*)))
			   (when (member ?time tp)
			     `((the) ,(if (equal ?time (car tp))
					  "initial" "final")
			       ,?property 
			       (and ,?mod (preferred (property ,?body)))))))))
  
(defun collect-extremal-time-points (Problem)
  "Collect any distinct min or max time points."
  (let (min max)
    (dolist (item (problem-wm problem))
      (when (eql (car item) 'time)
	(when (or (null min) (tbeforep (cadr item) min)) ;has timepointp test
	  (setf min (cadr item)))
	(when (or (null max) (tbeforep max (cadr item))) ;has timepointp test
	  (setf max (cadr item)))))
    (when (tbeforep min max) (list min max))))

(def-qexp preferred-the (the)
  :new-english (preferred "the"))

(def-qexp object (object ?body)
  ;; eval wrapper to specify possible values for body.
  :new-english (eval '((or "on" "acting on" "exerted on" "that acts on" 
			"applied on" "applied to" "against") 
		       (or (variable ?body :namespace :objects) ?body))
		     ;; include case where body is omitted
		     (?body . (problem-bodies-and-compounds *cp*))))

(def-qexp agent (agent ?body)
  ;;+syjung
  ;; checking the content of ?body by (expand-new-english ..) is 
  ;; important for the case that it is missing (in elec4b, see Bug #1676)
  :new-english (eval (when (expand-new-english ?body)
			'((or "due to" "by" "from" "caused by" "exerted by" "of") 
			  (or (variable ?body :namespace :objects) ?body)))
		     ;; include case with no agent
		     (?body . (cons nil (problem-atoms *cp*)))))

(def-qexp agent-prep (agent-prep ?preposition ?body)
  :new-english (eval (when (expand-new-english ?body)
		       '(?preposition
			 (or (variable ?body :namespace :objects) ?body)))
		     ;; include case with no agent
		     (?body . (cons nil (problem-atoms *cp*)))))


(def-qexp time (time ?time)
  :new-english (eval (when ?time
			'(preferred (time-not-omittable ?time)))
		     ;; add timeless as possibility.
                    (?time . (cons nil (get-problem-times)))))

(defun get-problem-times ()
  (mapcar #'second
         (remove '(time . ?rest) (problem-givens *cp*) :test-not #'unify)))

(def-qexp time-not-omittable (time-not-omittable ?time)
  :new-english (eval (if (time-pointp ?time) (pp ?time)
			 ;; else go back to Ontology
			 (new-english-find ?time))))

(def-qexp during (during ?ta ?tb)
  :new-english (or ((or "between" "during") 
			(allowed ((the) (allowed "time") "interval") )
			(eval (moment ?ta)) (or "and" "&") 
			(eval (moment ?tb)))
		   ((or "from" "during") (eval (moment ?ta)) 
		    (or "to" "until") (eval (moment ?tb)))))

;;;; scalar quantities

;;; Only use time slot if feature changing-mass is included.
(def-qexp mass	(mass ?body :time ?time)
  :rank scalar
  :symbol-base |m|
  :short-name "mass"	
  :units |kg|
  :restrictions positive
  ;; "ball's mass" problem s2e (to do)
  :new-english (property-object (key "mass") ?body :time ?time))

;; the magnitude of the change of mass of ~A per unit time due to ~A~@[ ~A~]" 
;;	       (nlg ?body) (nlg ?agent 'agent) (nlg ?time 'pp)
(def-qexp mass-change-magnitude	(mass-change-magnitude ?body ?agent :time ?time)
  :rank scalar
  :symbol-base |dmdt|     
  :short-name "magnitude of mass change per unit time"	
  :units |kg/s|
  :restrictions nonnegative
  :new-english ((allowed ((the) "magnitude of"))
		(time-derivative (mass ?body) :agent ?agent :time ?time)))

(def-qexp mass-per-length (mass-per-length ?rope)
  :rank scalar
  :symbol-base |\\lambda|     
  :short-name "mass per length"	
  :units |kg/m|
  :restrictions nonnegative 
  :new-english (property-object (or "mass per length" "mass-per-unit-length" 
			  "mass-per-length") ?rope)
)

;; Sophia:  "the distance traveled"
;;          "the distance of the aircraft"  (not very good english)
;;          "the distance"
(def-qexp distance (distance ?body :time ?time)
  :rank scalar
  :symbol-base |s|     
  :short-name "distance traveled"	
  :units |m|
  :new-english (or 
		((the) (key (or "distance" "dist." "dist"))
		 (preferred (or "traveled" "travelled" 
				"travels" "moves" "moved"))
		 (and (preferred ("by" (or (variable ?body :namespace :objects)
					   ?body)))
		      (time ?time)))
		((property-object (key (or "distance" "dist." "dist")) ?body)
		 (and (allowed (or "traveled" "travelled" "travels" 
				   "moves" "moved"))
		      (allowed "from the origin")
		      (time ?time)))))

;; ex) "the duration of time between T0 and T1"
;;     "the time duration between T0 and T1"
;;     "the duration between T0 and T1"
;; KVL "the duration of the blackbird flight" Bug #1815
(def-qexp duration (duration ?time)
  :rank scalar
  :symbol-base |t|     
  :short-name "duration of time"	
  :units |s|
  :restrictions positive
  :new-english ((the) 
		(or ((preferred "duration of") "time")
		    ((preferred "time") "duration")
		    ("duration"))
		(time ?time)))

;; ex) "the value of the average speed of the aircraft between T0 and T1"
;; ex) "the value of average speed of the car at time T1"
;;    "the car's average speed at time T1" 
(def-qexp speed (speed ?body :time ?time)
  :rank scalar
  :symbol-base |v|     
  :short-name "speed"	
  :units |m/s|
  :new-english (property-object (key  "speed") ?body :time ?time)
)

;;ex) "the coeffienct of kinetic friction between the crate and the plain"
(def-qexp coef-friction 
    (coef-friction ?body1 ?body2 ?static-or-kinetic :time ?time)
  :rank scalar
  :symbol-base |\\mu|     
  :short-name "coef. of friction"	
  :units NIL ;; dimensionless
  :new-english ((the) "coefficient of" 
		(preferred (eval (if (eql ?static-or-kinetic 'static) "static"
				     "kinetic")
				 (?static-or-kinetic . '(static kinetic))))
		"friction"
		(and (preferred 
		      ("between" 
		       (conjoin (or "and" "&") 
				(or (variable ?body1 :namespace :objects) 
				    ?body1)
				(or (variable ?body2 :namespace :objects) 
				    ?body2))))
		     (time ?time))))

;; "coefficient of drag for ~A moving through ~A" 
;; (nlg ?b) (nlg ?medium 'at-time ?time)
(def-qexp coef-drag-turbulent (coef-drag ?b ?medium :type turbulent :time ?time)
  :rank scalar
  :symbol-base |K|     
  :short-name "coef. of drag"	
  :units |kg/m|
  :new-english ("coefficient of drag for" ?b
		(and (preferred ("through" ?medium))
		     (time ?time)))) 

;; see constants.cl, function enter-predefs
(def-qexp gravitational-acceleration (gravitational-acceleration ?planet)
  :rank scalar
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
  ;; "the constant gravitational acceleration near the surface of the planet" 
  ;; (kt10a)
  :new-english ((allowed "the magnitude of")
		(or ((the) (or "gravitational" "grav." "grav")
		     (key (or "acceleration" "accel." "accel")) 
		     (allowed "constant"))
		    ((the) (key (or "acceleration" "accel." "accel"))
		     (or ((or "due to" "caused by" "of" "produced by") 
			  "gravity") "of a freely falling object"))
		    ((the) "constant" (or "gravitational" "grav." "grav")
		     (key (or "acceleration" "accel." "accel")))
		    (property-object ("local gravitational" (key "strength")) 
				     ?planet)	
		    ("free-fall" (key "acceleration"))
		    )
		(preferred ((or "at" "on" "near") 
			    (property-object "surface" ?planet))))
  )

;; Add Earth to Ontology as a universal name
;; Alternatively, it could be added to all problem ontologies
;; involving the earth.
(def-qexp the-Earth earth 
  :new-english ((the) "Earth"))

;(def-qexp unspecified unspecified 
;  :new-english (nil))

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
  :rank scalar
  ;:nlg-english ("the number of forces on ~A" (nlg ?body 'at-time ?time)))
  :short-name "number of forces"
  :new-english ((the) "number of forces"
		(and (preferred ("on" ?body))
		     (time ?time))))

;; ex) "radius of the circular path"
;;     "radius of the circular motion of the particle"
(def-qexp revolution-radius (revolution-radius ?body :time ?time)
  :rank scalar
  :symbol-base |r|     
  :short-name "radius of circular motion"	
  :units |m|
  :restrictions positive
  :new-english ((the) "radius" (preferred "of")
		(preferred ((the) (preferred "circular") 
			    (or "motion" "path")))
		(preferred (property ?body))
		(time ?time)))

;; Halliday and Resnick talk about work done by a force
;; "work done by the spring force"
;; Giancoli
;; "work done by a constant force ..."
;; "the work done by gravity ..."
;; "the work done by each force ..."

(def-qexp work (work ?b ?agent :time ?time)
  :rank scalar
  :symbol-base |W|     
  :short-name "work"	
  :units |J|
  :new-english ((the) (allowed "average") (key "work") (preferred "done")
		(and (preferred (object ?b))
		     (preferred (agent ?agent))
		     (time ?time))))

;; Halliday & Resnick
;; "work done on ... by all forces"
;; "total work done on ... by all forces that act on it"
;; Giancoli
;; "net work done on ..."

(def-qexp net-work (net-work ?body :time ?time)
  :rank scalar
  :units |J|
  :short-name "net work"
  :new-english (((the) (or "net" "total net" "total") (key "work")
		 (preferred "done")
		 (and (preferred (object ?body))
		      (allowed (agent "all forces"))
		      (time ?time)))))

;; Giancoli
;; "work done by non-conservative forces ..."

(def-qexp work-nc (work-nc ?body :time ?time)
  :rank scalar
  :units |J|
  :short-name "work by non-conservative forces"
  :new-english (((the) (allowed (or "total" "net")) (key "work")
		 (preferred "done") 
		 (and (preferred (object ?body))
		      (preferred (agent "non-conservative forces"))
		      (time ?time)))))

;; (Young&Freeman's textbook): 
;;	"the power developed by the engine" 
;;      "the power generated by the engine"
;;      "the power output of the engine applied to pushing the ship"
;;	"the power supplied by the net force acting on a particle"			
;; (by googling "power supplied by")
;;      "the power supplied by the engine"
(def-qexp power (power ?b ?agent :time ?time)
  :rank scalar
  :symbol-base |P|     
  :short-name "power"	
  :units |W|
  :new-english ((the) (allowed "instantaneous") "power" 
		(or ((preferred (or "developed" "generated" "supplied"))
		     (and (preferred (agent-prep (or "by" "due to" "from" "caused by" "exerted by" "of") ?agent)) 
			  ;; (agent (or "by" "due to" "from" "caused by" "exerted by" "of") ?agent)) 
		          (preferred (object ?b))
		          (time ?time)))
		    ("output" (preferred (agent-prep (or "of" "by" "due to" "from" "caused by" "exerted by") ?agent))
			      ;;(agent (or "of" "by" "due to" "from" "caused by" "exerted by") ?agent))
		     (or "applied" "supplied") 
			 (preferred (object ?b))
		     (time ?time)))))

(def-qexp net-power (net-power ?b :time ?time)
  :rank scalar
  :units |W|
  :short-name "net power"
  :new-english ((the) "net power"
		(and (preferred ("supplied to" ?b)) 
		     (time ?time))))

(def-qexp net-power-out (net-power-out ?source :time ?time)
  :rank scalar
  :symbol-base |P|     
  :short-name "power output" 
  :units |W|
  :new-english ((the) "total power produced" 
		(and (preferred ("by" ?source))
		     (time ?time))))

;;by syjung:
;; In the tutor hint in elec6b:
;;      "the angle between the displacement of e between T0 and T1 and 
;;      the electric force on e by the unspecified between T0 and T1"
;; The hint can be: 
;;      (when two are in the same time interval):
;;      "the angle between the displacement of e, and the electric force on e 
;;       between T0 and T1" 
;;      (when two are not in the same time interval):
;;	"the angle between the displacement of e between T1 and T2, and the 
;;       electric force on e between T0 and T1" 
;; It means that the time information can be displayed depending on the contents of ?vecs
;; This hint was generated by (conjoin (or "and" "&") . ?bodies)
;;
;; However, The hint should be shorter when each vector was defined with a variable name:
;;      "the angle between d and f" 
;;
(def-qexp angle-between (angle-between orderless . ?vecs)
  :rank scalar
  :units |deg|
  :short-name "angle"
  :restrictions nonnegative 
  ;; To print the contents of ?vecs:
  ;; :new-english ( (eval (dolist (vec ?vecs) (format t "~A~%" vec) ?vecs))))
  ;;
  ;; ?vecs = '((DISPLACEMENT ELECTRON :TIME (DURING 1 2)) 
  ;;	      (FORCE ELECTRON EARTH WEIGHT :TIME (DURING 1 2)))
  ;;     =>  '((DISPLACEMENT ELECTRON) 
  ;;	      (FORCE ELECTRON EARTH WEIGHT :TIME (DURING 1 2)))
  ;; ?vecs=  '((DISPLACEMENT ELECTRON TIME (DURING 1 2))
  ;;	      (FORCE ELECTRON UNSPECIFIED ELECTRIC :TIME (DURING 1 2)))
  ;;     =>  '((DISPLACEMENT ELECTRON)
  ;;	      (FORCE ELECTRON UNSPECIFIED ELECTRIC :TIME (DURING 1 2)))
  ;;
  ;;ex) ?vecs=(a b c)
  ;;    (mapcar .. ?vecs) = ((or (variable a) a) (or (variable b) b) 
  ;;                         (or (variable c) c))
  ;;
  ;; Generally, we expect the objects to be already defined before
  ;; angle-between is defined.  Thus, the variable names will be available.
  :new-english ((the) (key "angle") "between" 
		(conjoin 
		 (or "and" "&") . 
		 (eval (mapcar 
			#'(lambda (x) `(or (variable ,x :namespace :objects) 
					   ,x))
			?vecs)))))

(def-qexp total-energy (total-energy ?system :time ?time) 
  :rank scalar
  :units |J|
  :short-name "net mechanical energy"
  :new-english (property-object 
		(or ((preferred (or "total" "net")) "mechanical" 
		     (key "energy")) 
		    (key "TME"))
		?system
		:time ?time))

(def-qexp kinetic-energy (kinetic-energy ?body :time ?time)
  :rank scalar
  :units |J|
  :short-name "kinetic energy"
  :new-english (property-object ((allowed (or "total" "net")) 
				 (allowed (or "translational" "linear"))
				 (or ("kinetic" (key "energy")) (key "KE")))
				?body :time ?time))

(def-qexp rotational-energy (rotational-energy ?body :time ?time)
  :rank scalar
  :units |J|
  :short-name "kinetic energy"
  :new-english (property-object ((allowed (or "total" "net")) 
				 (preferred (rotation-adj))
				 (or ("kinetic" (key "energy")) "KE"))
				?body :time ?time))

(def-qexp grav-energy (grav-energy ?body ?agent :time ?time)
  :rank scalar
  :units |J|
  :short-name "potential energy"
  :new-english ((the) (allowed "total") (or "gravitational" "grav")
		(or ((preferred (or "potential" "pot." "pot")) (key "energy"))
		    "PE")
		(and (preferred (property ?body))
		     (preferred (agent ?agent)) 
		     (time ?time))))

;; see bug 1463
(def-qexp spring-energy (spring-energy ?body ?spring :time ?time) 
  :rank scalar
  :units |J|
  :short-name "potential energy"
  :new-english ((the) (allowed "elastic") (or ((or "potential" "pot" "spring") 
					       (key "energy")) "PE")
		(and (preferred (or (property ?body) 
				    ("transmittable to" ?body)))
		     ;; always include spring, since that defines force type.
		     (agent ?spring)
		     (time ?time))))

;; "the extension (or compression) of the spring from its equilibrium position"
;; "the elongation of the spring" in Young&Freeman's textbook in page 221
(def-qexp compression (compression ?spring :time ?time)
  :rank scalar
  :symbol-base |d|     
  :short-name "compression distance"	
  :units |m|
  :new-english ((the) (key (or "compression" "extension" "elongation" 
			       "stretch") )
		(allowed (or "distance" "displacement"))
		(and (property ?spring) 
		     (preferred ("from" 
				 (or "its" "the" "her") 
				 (or "equilibrium" "unstretched") 
				 (preferred (or "position" "point" "length"))))
		     (time ?time))))

;; "sprint constant" or "force constant" in Young&Freeman's text book in page 221
(def-qexp spring-constant (spring-constant ?spring)
  :rank scalar
  :symbol-base |k|     
  :short-name "spring constant"	
  :units |N/m|
  :restrictions positive
  :new-english (property-object ((or "spring" "force") (key "constant"))
				?spring))

(def-qexp height (height ?body ?zero-height :time ?time)
  :rank scalar
  :symbol-base |h|     
  :short-name "height"	
  :units |m|
  :new-english 
  (property-object (key "height") ?body 
   ;; Case with the zero height included.
   ;; Generally, we only have one zero height defined
   ;; in a problem, so we can default to not using it.
   :modifier (allowed 
	      ((or "above" "relative to")
	       ;; eval wrapper to include zero-height as possible
	       ;; value
	       (or (eval ?zero-height
			 (?zero-height . (cons 'zero-height 
					       (problem-atoms *cp*))))
		   ((the) (allowed "level of") 
		    "origin"))))
   :time ?time))

;; default phrase, in absence of something sensible.
(def-qexp zero-height zero-height
  :new-english (or ((the) (or "zero level" "axis" "horizontal axis" "origin"))
		   "zero"))

;; "the moment of inertia of ~A about ~A" (nlg ?body) (nlg ?axis 'at-time ?time)
(def-qexp moment-of-inertia (moment-of-inertia ?body :axis ?axis :time ?time)
  :rank scalar   ;; This is a particular component of the tensor
  :symbol-base |I|     
  :short-name "moment of inertia"	
  :units |kg.m^2|
  :restrictions positive
  :new-english (property-object 
		 "moment of inertia" 
		 ?body 
		 :modifier (eval (when ?axis '(preferred ("about" ?axis)))
				 (?axis . (problem-axes *cp*)))
		 :time ?time))

;; for dimensions of certain rigid bodies:
;;    from Bob: "the length of the beam"
(def-qexp length (length ?body)
  :rank scalar
  :symbol-base ||     
  :short-name "length"	
  :units |m|
  :new-english (property-object (key (or "length" "len" "len.")) ?body))

(def-qexp length-change (rate-of-change (length ?body))
  :rank scalar
  :symbol-base ||     
  :short-name "rate of change in length"	
  :units |m/s|
  :new-english (time-derivative (property-object 
				 (key (or "length" "len" "len."))
				 ?body)))

(def-qexp width  (width ?body)
  :rank scalar
  :symbol-base ||     
  :short-name "width"	  
  :units |m|
  :new-english ((property-object "width" ?body)))

(def-qexp num-torques (num-torques ?body ?axis :time ?time)
  :rank scalar
  :short-name "number of torques"
  :new-english ((the) "number of" (eval (moment-name)) "on" ?body 
		(preferred (eval (when ?axis '("about" ?axis))
				 (?axis . (problem-axes *cp*))))
		(time ?time)))

(def-qexp compound (compound orderless . ?bodies)
  :new-english (
		(allowed "a compound of") 
		(conjoin (or "and" "&") . ?bodies)
		))


(def-qexp system (system . ?bodies)
  :new-english ((preferred "a system of") 
		(conjoin (or "and" "&") . ?bodies)))


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
;;;   "a vector for foo the direction is unknown but either into or out of the 
;;;    plane."
;;;  zero:
;;;   " A zero length vector for FOO.

(def-entryprop vector (vector ?body ?quantity ?direction)
  :helpform (vector ?quantity ?direction)
  :Doc "The generic vector entry tool."
  :nlg-english ("a ~a" (ont-vector-entryprop-format-func ?body ?quantity 
							 ?direction)))

(defun ont-vector-entryprop-format-func (Body Quantity Direction)
  "Format the vector entry."
  (declare (ignore body))
  (let ((Quant (get-default-phrase Quantity))
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
  :short-name ("projecton on ~A-axis" (axis-name ?axis))
  :nlg-english ("projection equations")
  :tutorial "ProjectionEquations.html"
  :ExpFormat ("writing the projection equation for ~a onto the ~a axis" 
	      (nlg ?vector) (axis-name ?axis))
  :EqnFormat ("V<sub>~a</sub> = V ~:[sin~;cos~](&theta;V - &theta;~a)"
	      (axis-name ?axis) 
	      (eq ?axis 'x)
	      (axis-name 'x)))

   
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
  :tutorial "AverageSpeed.html"
  :nlg-english ("distance = average speed * time")
  :expFormat ((strcat "applying the \"distance = average speed "
		      "* time\" principle with ~a as the body ~a")
	      (nlg ?body) (nlg ?time))
  :EqnFormat ("v(avg) = s/t"))

(def-psmclass displacement-distance (displacement-distance ?body ?time)
  :complexity connect  ;since this is like (equals ...)
  :doc "Distance = Displacement."
  :short-name "distance &amp; displacement"
  :nlg-english ("distance = magnitude of displacement")
  :ExpFormat ("noting that distance is the magnitude of the displacement")
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
    :tutorial "AverageVelocity.html"
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
  :tutorial "PythagoreanTheorem.html"
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
  :tutorial "NetDisplacement.html"
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
  :tutorial "FreeFallAcceleration.html"
  :ExpFormat ("determining the free fall acceleration for ~a"
	      (nlg ?body 'at-time ?time))
  :EqnFormat ("a = g"))

(def-psmclass std-constant-g (std-constant (gravitational-acceleration earth))
  :complexity simple 
  :short-name "g near Earth"
  :nlg-english ("value of g on Earth")
  :expformat ("defining the value of g on Earth")
  :tutorial "ValueOfGNearEarth.html"
  :EqnFormat ("g = 9.8 m/s<sup>2</sup>"))


; UNIFORM CIRCULAR MOTION
(def-psmclass centripetal-accel (centripetal-accel ?body ?time)
  :complexity major
  :short-name "centripetal acceleration (instantaneous)"
  :tutorial "CentripetalAcceleration.html"
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
   :tutorial "PeriodCircular.html"
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
  :tutorial "SumOfTimes.html"
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
  :tutorial "NewtonsSecondLaw.html"
     :nlg-english ("Newton's second law")
     :ExpFormat ("applying Newton's second law to ~a"
		 (nlg ?body 'at-time ?time))
     :EqnFormat ("F1<sub>~a</sub> + F2<sub>~a</sub> + ... = m a<sub>~a</sub>" 
                 (axis-name ?axis) (axis-name ?axis) (axis-name ?axis)))

(def-psmclass net-force (?eq-type definition ?axis ?rot (net-force ?body ?t))
  :complexity minor ;See Bug #1144
  :short-name ("net force (~A component)" (axis-name ?axis))
  :nlg-english ("net force")
  :tutorial "NewtonsSecondLaw.html"
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
  :tutorial "NewtonsThirdLaw.html"
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
  :nlg-english ("weight law")
  :tutorial "WeightLaw.html"
  :ExpFormat ("applying the weight law to ~a" (nlg ?body))
  :EqnFormat ("Fw = m g"))

(def-psmclass kinetic-friction (kinetic-friction ?body ?surface ?time)
  :complexity simple
  :short-name "kinetic friction"
  :nlg-english ("Kinetic Friction law")
  :tutorial "KineticFriction.html"
  :ExpFormat ("applying the Kinetic friction law for ~a and ~a"
	      (nlg ?body) (nlg ?surface 'at-time ?time))
  :EqnFormat ("Ff = &mu;k Fn"))

(def-psmclass static-friction (static-friction ?body ?surface ?time) 
  :complexity simple
  :short-name "static friction (at max)"
  :nlg-english ("Static Friction at maximum")
  :tutorial "StaticFrictionMax.html"
  :expformat ("applying the Definition of Static friction for ~a and ~a"
	      (nlg ?body) (nlg ?surface 'at-time ?time))
  :EqnFormat ("Ff = &mu;s Fn"))

(def-psmclass drag-force-turbulent (drag-force ?body ?medium turbulent ?time)
  :complexity simple
  :short-name "drag force"
  :nlg-english ("drag force")
  :tutorial "DragForce.html"
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
  :tutorial "UniversalGravitation.html"
  :nlg-english ("Newton's law of Universal Gravitation")
  :expformat ("applying Newton's law of Universal Gravitation for the force on ~a due to ~a" (nlg ?body) (nlg ?agent))
  :EqnFormat ("Fg = G m1 m2/r<sup>2</sup>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONNECTED BODIES
(def-psmclass connected-accels (connected-accels ?body0 ?body1 ?time)
  :complexity connect
  :short-name "connected accelerations"
  :nlg-english ("connected bodies have same acceleration")
  :tutorial "EqualAccelerations.html"
  :expformat ((strcat "using the fact that ~a and ~a are connected and "
		      "therefore have the same acceleration")
	      (nlg ?body0) (nlg ?body1))
  :EqnFormat ("a1 = a2"))
    
(def-psmclass connected-velocities (connected-velocities ?body0 ?body1 ?time)
  :complexity connect
  :short-name "connected velocities"
  :nlg-english ("connected bodies have same velocity")
  :tutorial "EqualVelocities.html"
  :expformat ((strcat "using the fact that ~a and ~a are connected and "
		      "therefore have the same velocity")
	      (nlg ?body0) (nlg ?body1))
  :EqnFormat ("v1 = v2"))

(def-psmclass tensions-equal (tensions-equal ?string (?body0 ?body1) ?time)
  :complexity connect
  :short-name "equal tensions at both ends"
  :nlg-english ("tension equal throughout string")
  :tutorial "EqualTensionsAtBothEnds.html"
  :expformat ((strcat "using the fact that the tension forces are always "
		      "equal at both ends of a string with ~a") (nlg ?string))
  :EqnFormat ("Ft1 = Ft2"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPOUND BODIES 
(def-psmclass mass-compound (mass-compound ?compound)  
  :complexity connect
  :short-name "mass of compound"
  :nlg-english ("mass of a compound body is sum of masses of parts")
  :tutorial "MassOfACompoundBody.html"
  :expformat ((strcat "using the fact that the mass of ~a "
		      "is the sum of the masses of its parts") 
	      (nlg `(compound orderless . ,?compound)))
  :EqnFormat ("M = m1 + m2 + ..."))

(def-psmclass kine-compound (kine-compound ?vec-type ?bi ?compound ?time) ; part, whole same kinematics
  :complexity connect
  :short-name "velocity of compound"
  :nlg-english ("~A of compound same as part" (nlg ?vec-type))
  :tutorial "KinematicsOfCompoundSameAsPart.html"
  :expformat ("applying the fact that the ~A of a compound is same as that of its parts" (nlg ?vec-type))
  :EqnFormat ("v_compound = v_part"))

					  
(def-psmclass force-compound (force-compound ?type ?agent ?compound ?time)
  :complexity connect
  :short-name "force on compound"
  :nlg-english ("external force on a compound")
  :tutorial "ForceOnACompoundBody.html"
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
  :tutorial "WorkDoneByAForce.html"
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
  :tutorial "NetWork.html"
  :expformat ("calculating the net work done by all forces on ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("Wnet = WF1 + WF2 + ..."))

(def-psmclass work-nc (Wnc ?body (during ?time0 ?time1))
  :complexity minor  ; definition, but *can't* be first "principle" for sought
  :short-name "work by non-conservative"
  :nlg-english ("the definition of Wnc")
  :tutorial "ConservationOfEnergy.html"
  :expformat ("representing the work done on ~A by non-conservative forces"
              (nlg ?body))
  :EqnFormat ("Wnc = Wncf1 + Wncf2 + ..."))

(def-psmclass work-energy (work-energy ?body (during ?time0 ?time1))
  :complexity major
  :short-name "work-energy theorem"
  :nlg-english ("the work-kinetic energy theorem")
  :tutorial "Work-Energy.html"
  :expformat ("applying the work-kinetic energy theorem to ~a from ~a to ~a"
	      (nlg ?body) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("Wnet = Kf - Ki"))

(def-psmclass cons-energy (cons-energy ?body ?t1 ?t2)
  :complexity major
  :short-name "[Wnc=0] conservation of mechanical energy"
  :nlg-english ("conservation of mechanical energy")
  :tutorial "ConservationOfEnergy.html"
  :ExpFormat ((strcat "applying conservation of mechanical energy to ~a "
		      "from ~a to ~a") 
	      (nlg ?body) (nlg ?t1 'time) (nlg ?t2 'time))
  :EqnFormat ("ME1 = ME2"))

(def-psmclass change-ME (change-ME ?body ?t1 ?t2)
  :complexity major
  :short-name "change in mechanical energy"
  :nlg-english ("change in mechanical energy")
  :tutorial "ConservationOfEnergy.html"
  :ExpFormat ((strcat "applying change in mechanical energy to ~a "
		      "from ~a to ~a") 
	      (nlg ?body) (nlg ?t1 'time) (nlg ?t2 'time))
  :EqnFormat ("Wnc = ME2 - ME1"))

(def-psmclass height-dy (height-dy ?body ?zero-height ?time)
  :complexity connect
  :short-name "change in height"
  :nlg-english ("the height change-displacement relationship")
  :tutorial "HeightAndDisplacement.html"
  :expformat ((strcat "relating the change in height of ~a ~a to the y "
		      "component of its displacement")
	      (nlg ?body) (nlg ?time 'pp))
  :EqnFormat ("h2 - h1 = d12<sub>y</sub>"))

(def-psmclass power (power ?body ?agent ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "average power defined"
  :nlg-english ("the definition of average power")
  :tutorial "Power.html"
  :expformat ("applying the definition of average power supplied to ~A by ~A ~A"
	      (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
  :EqnFormat ("P(avg) = W/t"))

(def-psmclass net-power (net-power ?body ?time)
  :complexity major  ;See Bug #1144
  :short-name "average net power defined"
  :nlg-english ("the definition of net power")
  :tutorial "Power.html"
  :expformat ("applying the definition of net power supplied to ~A ~A"
	      (nlg ?body) (nlg ?time 'pp))
  :EqnFormat ("Pnet = Wnet/t"))

(def-psmclass inst-power (inst-power ?body ?agent ?time ?rot)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name "instantaneous power"
  :nlg-english ("the instantaneous power principle")
  :tutorial "Power.html"
  :expformat ("calculating the instantaneous power supplied to ~A by ~A ~A"
	      (nlg ?body) (nlg ?agent) (nlg ?time 'pp))
  :EqnFormat ("P = F v cos(&theta;) or P = F<sub>x</sub> v<sub>x</sub> + F<sub>y</sub> v<sub>y</sub>"))

;; !! "total-energy" label used in kb is misleading, it is, in fact,
;; total *mechanical* energy.

(def-psmclass mechanical-energy  (total-energy-top ?body ?time) 
  :short-name "mechanical energy defined"
  :nlg-english ("the definition of mechanical energy")
  :complexity definition
  :tutorial "ConservationOfEnergy.html"
  :EqnFormat ("ME = KE + &Sigma Ui"))

;; These are now top level psms, not subequations:
(def-psmclass kinetic-energy (kinetic-energy ?body ?time)
  :short-name "kinetic energy defined"
  :nlg-english ("the definition of kinetic energy")
  :tutorial "ConservationOfEnergy.html"
  :complexity definition
  :EqnFormat ("KE = 0.5 m v<sup>2</sup>"))

(def-psmclass rotational-energy (rotational-energy ?body ?time)
  :short-name "rotational kinetic energy defined"
  :nlg-english ("the definition of rotational kinetic energy")
  :tutorial "ConservationOfEnergy.html"
  :complexity definition
  :EqnFormat ("KE = 0.5 I &omega;<sup>2</sup>"))

(def-psmclass grav-energy (grav-energy ?body ?planet ?zero-height ?time)
  :short-name "gravitational potential energy"
  :nlg-english ("gravitational potential energy")
  :tutorial "ConservationOfEnergy.html"
  :complexity definition
  :EqnFormat ("Ug = m g h"))

(def-psmclass spring-energy (spring-energy ?body ?spring ?time)
  :short-name "spring potential energy"
  :nlg-english ("spring potential energy")
  :tutorial "ConservationOfEnergy.html"
  :complexity definition
  :EqnFormat ("Us = 0.5 k d<sup>2</sup>"))


;; LINEAR MOMENTUM
(def-psmclass cons-linmom (?eq-type lm-compo ?axis ?rot (cons-linmom ?bodies ?time))
  :complexity major
  :short-name "conservation of momentum"
  :nlg-english ("conservation of momentum")
  :tutorial "ConservationOfMomentum.html"
  :expformat ("applying Conservation of Linear Momentum to ~a ~a"
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time 'time))
  :EqnFormat ("p1i<sub>~a</sub> + p2i<sub>~a</sub> + ... = p1f<sub>~a</sub> + p2f<sub>~a</sub> + ..." 
	      (axis-name ?axis) (axis-name ?axis) (axis-name ?axis) 
	      (axis-name ?axis)))

(def-psmclass cons-ke-elastic (cons-ke-elastic ?bodies (during ?time0 ?time1))
  :complexity major
  :short-name "elastic collision defined"
  :nlg-english ("conservation of kinetic energy in elastic collisions")
  :tutorial "ElasticCollisions.html"
  :expformat ((strcat "applying Conservation of Kinetic Energy to "
		      "elastic collisions of ~a from ~a to ~a")
	      (nlg ?bodies 'conjoined-defnp) (nlg ?time0 'time) (nlg ?time1 'time))
  :EqnFormat ("KEf = KEi"))

;; ROTATIONAL KINEMATICS

(def-psmclass linear-vel (linear-vel ?pt ?time ?axis)
  :complexity major
  :short-name "linear velocity at a certain radius"
  :nlg-english ("linear velocity of rotating point")
  :tutorial "LinearVelocity.html"
  :ExpFormat ("calculating the linear velocity of the rotating point ~a"
	      (nlg ?pt 'at-time ?time))
  :EqnFormat ("v = &omega; r"))

(def-psmclass rolling-vel (rolling-vel ?body ?axis ?time)
  :complexity major
  :short-name "linear velocity of rolling object"
  :nlg-english ("linear velocity of rolling object")
  :tutorial "LinearVelocity.html"
  :ExpFormat ("finding the relation between linear motion and rotational motion of ~A"
	      (nlg ?body 'at-time ?time))
  :EqnFormat ("v = &omega; r"))

;;;; ROTATIONAL DYNAMICS (TORQUE)

(def-psmclass mag-torque (mag-torque ?body ?pivot (force ?pt ?agent ?type) ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name ("~A defined (magnitude)" (moment-name))
  :nlg-english ("the definition of ~A magnitude" (moment-name))
  :tutorial "IndividualTorqueMagnitude.html"
  :expformat ((strcat "calculating the magnitude of the ~A "
		      "on ~a ~a due to the force acting at ~a")
	      (moment-name) (nlg ?body) (nlg ?time 'pp) (nlg ?pt))
  :EqnFormat ((torque-switch "M = r F sin(&theta;)" "&tau; = r F sin(&theta;)")))

(def-psmclass torque 
  (torque ?xyz ?rot ?body ?pivot (force ?pt ?agent ?type) ?angle-flag ?time)
  :complexity major ; definition, but can be first "principle" for sought
  :short-name ("~A defined (~A component)" (moment-name) (axis-name ?xyz))
  :nlg-english ("the definition of ~A" (moment-name))
  :tutorial "IndividualTorqueMagnitude.html"
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
	+required-identities+
	'(NTL		      ;Newton's Third law
	  cons-energy 	      ;conservation of energy ME1 = ME2
	  projection          ;projection psm: block using v for v_x when equal
	  charge-same-caps-in-branch	 ;want students to show they know this
	  ) #+sbcl #'equalp)

(defun required-identity-p (eqn)
"true if eqn is on the list of required identities"
    (member (first (eqn-exp eqn)) +required-identities+))


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

