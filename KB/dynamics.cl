;;;;
;;;;          Mass, forces, Newton's laws
;;;;
;;;;


;;; ========================== mass  ======================

;;; Andes has mass on the variables menu even though mass is also
;;; defined as a side-effect of drawing a body.  This means that
;;; whenever a student just wants a variable for mass and doesn't want
;;; to draw a body, there are two ways to do it.  Thus, the solution
;;; graph builder must construct both solutions.
;;;
;;; mass can have time in rocket problems, but is left timeless in others
;;; Which one gets defined is controlled by presence of
;;; in the problem features.

(defoperator inherit-timeless-mass (?b ?t)
   :preconditions ((test (not (member 'changing-mass (problem-features *cp*))))
		   (time ?t))
  :effects ((inherit-quantity (mass ?b :time ?t) (mass ?b))))

(defoperator define-mass (?b ?t)
  :preconditions
  (
   ;; only use time when allowed by feature changing-mass
   ;; This is a sanity test to ensure inherit-quantity is working OK.
   (test (or (eq (null ?t) 
		 (null (member 'changing-mass (problem-features *cp*))))
	     (error "define-mass time slot ~A not consistant with problem features" ?t)))
   (object ?b)
   (not (variable ?dont-care (mass ?b :time ?t)))
   (bind ?var (format-sym "m_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
   )
  :effects
  ((variable ?var (mass ?b :time ?t))
   (define-var (mass ?b :time ?t)))
  :hint
  ((bottom-out (string "You can use the variable definition tools, which are under the variables menu, in order to define a variable for mass."))
   ))

;;; Magnitude of derivative of mass with respect to time
;;; due to agent
;;; We use the magnitude form because students in a non-calculus
;;; based course don't have a strong concept of "slope."

(defoperator define-mass-change-magnitude (?b ?agent ?t)
  :preconditions
  (
   (test (member 'changing-mass (problem-features *cp*)))
   (object ?b)
   (time ?t)
   (bind ?var (format-sym "dmdt_~A_~A~@[_~A~]" (body-name ?b) (body-name ?agent) 
			  (time-abbrev ?t))))
  :effects
  ((variable ?var (mass-change-magnitude ?b ?agent :time ?t))
   (define-var (mass-change-magnitude ?b ?agent :time ?t)))
  :hint
  ((bottom-out (string "You can use the variable definition tools, which are under the variables menu, in order to define a variable for the magnitude of the change in mass."))
   ))

(defoperator mass-compound-contains (?sought)
  :preconditions (
   (any-member ?sought ((mass ?b-sought)))
   ;; compound must exist
   (object (compound orderless . ?bodies))
   ;; applies if sought is mass of compound or one of its parts
   (test (or (member ?b-sought ?bodies :test #'equal)
             (equal ?b-sought `(compound orderless ,@?bodies))))
  )
  :effects (
    (eqn-contains (mass-compound (compound orderless . ?bodies)) ?sought)
  ))

(defoperator write-mass-compound (?bodies)
  :preconditions 
  (
   ;; make sure compound body is drawn, else it can't be defined.
   (body ?compound)	
   (variable ?mwhole-var (mass ?compound))
   (bind ?bodies (cddr ?compound))
   (map ?body ?bodies
	(variable ?mpart-var (mass ?body)) 
	?mpart-var ?mpart-vars) 
   )
  :effects ((eqn (= ?mwhole-var (+ . ?mpart-vars)) (mass-compound ?compound)))
  :hint
  ((point (string "How does the mass of a compound body relate to the masses of its parts?"))
   (teach (string "The mass of a compound body is equal to the sum of the masses of its parts."))
   (bottom-out (string "Write the equation ~A" ((= ?mwhole-var (+ . ?mpart-vars)) algebra)))
   ))

;;; ============================== forces =====================================

;;; For each basic force type we have an operator called find-TYPE-force
;;; which derives a proposition of the form
;;;      (force ?body ?agent ?type ?time ?direction ?action-or-reaction)
;;; This means that the relevant force exists.  These "force-exists" rules
;;; are broken out because there are cases in which we need to know whether
;;; certain forces exist but don't necessarily want the student to draw them.  
;;; There are also operators for drawing force vectors of the different types.  
;;; The hints for the force types are actually associated with these drawing 
;;; operators, because that is where we will have to give help if the 
;;; student fails to draw a needed force. 
;;; The 'action or 'reaction tag is for heuristic purposes: 'reaction forces
;;; are those the explanation of which should appeal to Newton's third law.
;;; We have a single rule that derives the existence of a "reaction" force 
;;; from any "action" force, so this tag also limits the search to find action
;;; forces and so prevents looping which caused problems with other ways of
;;; writing Newton's third law.
;;; Forces on a compound body are also derived from these basic forces on
;;; the simple bodies without having to draw the basic forces.


;;; This operator models drawing a weight force using the Andes force
;;; tool.  The tool not only draws the vector but also defines
;;; variables for the magnitude and the direction of the force.  The
;;; vector predicate contains the information that the student can see
;;; in the vector drawing tool.  In particular, it contains the
;;; direction of the weight vector, 270 degrees.  This is information
;;; is needed by the axis drawing operators in order to rotated the
;;; axes to match the vectors.  All dimensioned numbers are
;;; represented by (dnum <value> <unit>), so the weight force's
;;; direction is (dnum 270 |deg|).

(defoperator find-weight-force (?b ?t ?planet)
  :preconditions 
   ((object ?b)
    ;; We can apply this to a point or a whole rigid body, if center
    ;; of mass is not specified.  Don't want to apply it to a part
    ;; of a rigid body.
    (not (point-on-body ?b ?rigid-body))
    (not (center-of-mass ?b (?bb)))  ;center of mass handled below
    (not (center-of-mass ?cm (?b)))  ;center of mass handled below
    (time ?t)
    (not (massless ?b))
    (near-planet ?planet :body ?b ?b)
    (not (force ?b ?planet weight ?t . ?dont-care))
    (add-to-wm (non-cm-mass ?b ?planet ?t)))
  :effects (
     (force ?b ?planet weight ?t (dnum 270 |deg|) action)
     ;; NIL time here should be translated into sought time interval
     (force-given-at ?b ?planet weight NIL (dnum 270 |deg|) action)
  ))

;; Ideally, weight force should be timeless, but it is not
;; clear how to represent this on the user interface.
;; Instead, we use the largest defined interval.
(defoperator inherit-weight (?b ?t ?planet)
  :preconditions 
  (
   (time ?t-big)
   ;; collect set of all intervals
   (setof (time (during ?t1 ?t2)) (during ?t1 ?t2) ?tlist)	
   ;; make sure ?t-big is not inside any interval
   (test (notany #'(lambda (x) (tinsidep-include-endpoints ?t-big x))
		 (remove ?t-big ?tlist :test #'equal)))
   (time ?t)
   (test (not (unify ?t ?t-big))) ;make sure times are distinct
   (test (tinsidep-include-endpoints ?t ?t-big))
   ;; test force exists and bind ?planet, if needed
   (force ?b ?planet weight ?t-big . ?rest)
   )
  :effects 
  ((inherit-quantity (force ?b ?planet weight :time ?t) 
		     (force ?b ?planet weight :time ?t-big))))
 
(defoperator draw-weight (?b ?t ?planet)
  :specifications "
    If ?body is not massless, and
       it is near a ?planet,
    then draw a weight vector for it pointing straight down,
       define a magnitude variable and an direction variable for it."
  :preconditions
   ((force ?b ?planet weight ?t ?dir action)
    (in-wm (non-cm-mass ?b ?planet ?t))  ; select correct force
    (not (vector ?b (force ?b ?planet weight :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fw_~A_~A~@[_~A~]" (body-name ?b) ?planet 
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing weight of ~a~@[ at ~a~].~%" ?b ?t))
  :effects
   ((vector ?b (force ?b ?planet weight :time ?t) ?dir)
    (variable ?mag-var (mag (force ?b ?planet weight :time ?t)))
    (variable ?dir-var (dir (force ?b ?planet weight :time ?t)))
    (given (dir (force ?b ?planet weight :time ?t)) ?dir))
  :hint
  ((point (string "Notice that ~a is near ~a." ?b ?planet))
   (teach (string "When an object is near a planet, the planet exerts a weight force on the object."))
   (bottom-out (string "Because ~a is near the planet ~a, the planet exerts a weight force on it, so use the force drawing tool to draw a force on ~a due to ~a of type weight ~a pointing straight down (270 deg)." ?b ?planet ?b (?planet agent) (?t pp)))
   ))

;; For rigid body problems: treat weight of body as force acting at body's
;; center of mass

(defoperator find-weight-cm (?cm ?planet ?t)
  :preconditions 
   (;; We don't want to apply this rule to parts of 
    ;; a larger rigid body, or to the whole rigid body.  Rather an alt op 
    ;; will treat weight of whole body as force acting at cm
    (center-of-mass ?cm (?rigid-body))
    (not (point-on-body ?part ?cm))
    (time ?t)
    (not (massless ?cm))
    (near-planet ?planet :body ?cm ?cm)
    (not (force ?cm ?planet weight ?t . ?dont-care))
    ;; activate associated drawing rule.
    (add-to-wm (use-cm-mass ?cm ?rigid-body ?planet ?t)))
  :effects (
     (force ?cm ?planet weight ?t (dnum 270 |deg|) action)
     ;; NIL time here should be translated into sought time interval
     (force-given-at ?cm ?planet weight NIL (dnum 270 |deg|) action)
  ))

(defoperator draw-weight-at-cm (?cm ?planet ?t)
  :specifications "
    If rigid body is not massless, and it is near a planet,
    then draw a weight force vector acting at the center of mass, pointing straight down,
       define a magnitude variable and an direction variable for it."
  :preconditions
   ( 
    (force ?cm ?planet weight ?t ?dir action)
    (in-wm (use-cm-mass ?cm ?b ?planet ?t))
    (bind ?mag-var (format-sym "Fw_~A_~A~@[_~A~]" ?cm ?planet 
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing weight of ~a~@[ at ~a~] acting at cm.~%" ?cm ?t))
  :effects
   ((vector ?cm (force ?cm ?planet weight :time ?t) (dnum 270 |deg|))
    (variable ?mag-var (mag (force ?cm ?planet weight :time ?t)))
    (variable ?dir-var (dir (force ?cm ?planet weight :time ?t)))
    (given (dir (force ?cm ?planet weight :time ?t)) (dnum 270 |deg|)))
  :hint
  ((point (string "Notice that ~a is near ~a." ?b ?planet))
   (teach (string "When a rigid body is near a planet, each portion of the body is acted on by the force of gravity. The net effect of all these forces is equivalent to that of a single weight force of magnitude m * g acting at a single point called the center of gravity, which normally is the same as the center of mass."))
   (bottom-out (string "Because ~a is near the planet ~a, ~a exerts a weight force on it which can be treated as acting at the center of mass, so use the force drawing tool to draw a weight force vector acting at ~a due to ~a ~a pointing straight down (270 deg)." ?b ?planet ?planet ?cm (?planet agent) (?t pp)))
   ))


;;; draw-tension This op models draws a tension force.  It draws the
;;; vector and defines magnitude and direction vars.  It only draws
;;; vectors for the time and body specified in a body predicate.
;;; 
;;; The direction of the tension force is given in the problem
;;; statement by the 4th argument of the tied-to predicate.  It can
;;; either be "unknown" if the problem says nothing about the
;;; orientation of the string, or (dnum ?x |deg|) if the direction
;;; of the string is given numerically, or (parameter ?name) if it is
;;; given as a parameter like theta.
;;; 
;;; In the future, the code should let the 4th argument of tied-to be
;;; an algebraic expression of numbers and parameters.  For instance,
;;; it might be theta+90 to indicate that one vector is orthogonal to
;;; another, without specifying the direction of either numerically.
;;; 
;;; Parameters are different from variables.  Variables are the things
;;; in the Andes variable window, so only students can define them.
;;; However, parameters can appear in the problem statement and the
;;; equations. 

(defoperator find-tension-force (?b ?string ?t)
  :preconditions (
    (tied-to ?string ?b :time ?t-tied-to :dir ?dir-expr)
    (time ?t)
    (test (tinsidep ?t ?t-tied-to))
    (not (force ?b ?string tension ?t . ?dont-care))
  )
  :effects (
    (force ?b ?string tension ?t ?dir-expr action)
    (force-given-at ?b ?string tension ?t-tied-to ?dir-expr action)
  ))

(defoperator draw-tension (?b ?string ?t)
  :specifications "
   If there is a string tied to a body with the direction known,
   then draw a tension force on the body due to the string,
     with its direction being the given string-body connection,
     and define vectors for the magnitude and direction of the force."
  :preconditions
   ((force ?b ?string tension ?t ?dir-expr action)
    (test (not (equal ?dir-expr 'unknown)))
    (not (vector ?b (force ?b ?string tension :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Ft_~A_~A~@[_~A~]" (body-name ?b) 
			       ?string (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a tension on ~a due to ~a at ~a.~%" 
	   ?dir-expr ?b ?string ?t)
    )
  :effects
   ((vector ?b (force ?b ?string tension :time ?t) ?dir-expr)
    (variable ?mag-var (mag (force ?b ?string tension :time ?t)))
    (variable ?dir-var (dir (force ?b ?string tension :time ?t)))
    (given (dir (force ?b ?string tension :time ?t)) ?dir-expr))
  :hint
   ((point (string "Notice that ~a is tied to ~a." ?string ?b))
    (teach (string "Whenever something has a taut string, or something like a string, attached to it, then the string exerts a tension force on it."))
    (bottom-out (string "Because ~a is tied to ~a, you should use the force drawing tool to draw a force on ~a due to ~a of type tension ~a." (?string agent)
			?b ?b (?string agent) (?t pp)))
    ))

;;; Need a second version of the operator for drawing tension forces
;;; to cover the case where the direction is unknown.  This should be
;;; done with a conditional effect, but we don't have those yet.

(defoperator draw-tension-unknown-dir (?b ?string ?t)
  :specifications "
   If there is a string tied to a body with the direction unknown,
   then draw a tension force on the body due to the string,
     with its direction being the given string-body connection,
     and define vectors for the magnitude and direction of the force."
  :preconditions
   ((force ?b ?string tension ?t unknown action)
    (not (vector ?b (force ?b ?string tension :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Ft_~A_~A~@[_~A~]" (body-name ?b) ?string                                               (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing tension on ~a due to ~a at ~a of unknown direction.~%" ?b ?string ?t)
    )
  :effects
   ((vector ?b (force ?b ?string tension :time ?t) unknown)
    (variable ?mag-var (mag (force ?b ?string tension :time ?t)))
    (variable ?dir-var (dir (force ?b ?string tension :time ?t))))
  :hint
   ((point (string "Notice that ~a is tied to ~a." ?string ?b))
    (teach (string "Whenever something has a string, or something like a string, tied to it, then the string exerts a tension force on it."))
    (bottom-out (string "Because ~a is tied to ~a, you should use the force drawing tool to draw a force on ~a due to ~a of type tension ~a." 
			(?string agent) ?b ?b (?string agent) (pp ?t)))
    ))

;;; This operator models drawing a normal force.  The problem
;;; statement should contain a proposition of the form (supports
;;; <surface> <obj> <time> <direction>) where the direction is chosen
;;; so that <direction>+90 points away from the surface.  Currently,
;;; this only works when the surface's direction is numerical, so we
;;; simply don't handle normal vectors for surfaces whose direction is
;;; parametric or unknown.  Need to fix that someday.  Perhaps it
;;; would be easly to make it work with algebraic expresson that
;;; include parameters.

(defoperator find-normal-force (?b ?surface ?t)
   :preconditions (
    (supports ?surface ?b :time ?t-supports :dir (dnum ?dir |deg|))
    (time ?t)
    (test (tinsidep ?t ?t-supports))
    (not (force ?b ?surface normal ?t . ?dont-care))
    (bind ?normal-dir (mod (+ ?dir 90) 360))
  ) 
  :effects (
    (force ?b ?surface normal ?t (dnum ?normal-dir |deg|) action)
    (force-given-at ?b ?surface normal ?t-supports (dnum ?normal-dir |deg|) action)
  ))

(defoperator draw-normal (?b ?surface ?t)
  :specifications 
  "If an object slides along a plane,
      and the plane has a known direction ?dir,
   then there is normal force on the object due to the plane,
      and it is perpendicular to the plane"
  :preconditions
   ((force ?b ?surface normal ?t ?normal-dir action)
    (not (vector ?b (force ?b ?surface normal :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fn_~A_~A~@[_~A~]" (body-name ?b) ?surface 
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a normal on ~a due to ~a at ~a.~%" 
	   ?normal-dir ?b ?surface ?t)
    )
  :effects
   ((vector ?b (force ?b ?surface normal :time ?t) ?normal-dir)
    (variable ?mag-var (mag (force ?b ?surface normal :time ?t)))
    (variable ?dir-var (dir (force ?b ?surface normal :time ?t)))
    (given (dir (force ?b ?surface normal :time ?t)) ?normal-dir))
  :hint
   ((point (string "Notice that ~a is supported by a surface: ~a." ?b ?surface))
    (teach (minilesson "mini_normal_force.htm")
           (kcd "normal_force_direction")
	   (string "When an object is supported by a surface, the surface exerts a normal force on it.  The normal force is perpendicular to the surface."))
    (bottom-out (string "Because ~a supports ~a, draw a normal force on ~a due to ~a at an angle of ~a degrees." 
			(?surface agent) ?b ?b (?surface agent) 
			(?normal-dir adj)))
    ))

;; Applied force is specified in problem statement by given force direction 
;; which may be unknown

(defoperator find-applied-force (?b ?agent ?t)
  :preconditions 
  (
   ;; energy conservation law also checks for this to not exist:
   (in-wm (given (dir (force ?b ?agent applied :time ?t-force)) ?dir-expr))
   (time ?t)
   (test (tinsidep ?t ?t-force))
   ;; check that something else hasn't defined this force.
   (not (force ?b ?agent applied ?t . ?dont-care)) 
   )
  :effects (
	    (force ?b ?agent applied ?t ?dir-expr action)
	    (force-given-at ?b ?agent applied ?t-force ?dir-expr action)
  ))

;; Draw a applied ("given") force at a certain direction. 
(defoperator draw-applied-force (?b ?agent ?t)
  :specifications 
  "if you are given that there is an applied force on an object at a time
   at a certain direction,
  then draw the force at that direction"
  :preconditions
  ( (force ?b ?agent applied ?t ?dir-expr action)
    (test (not (equal ?dir-expr 'unknown)))
    (not (vector ?b (force ?b ?agent applied :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fa_~A_~A~@[_~A~]" 
			       (body-name ?b) ?agent (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a applied force on ~a due to ~a~@[ at ~a~].~%" 
	   ?dir-expr ?b ?agent ?t)
    )
  :effects
   ((vector ?b (force ?b ?agent applied :time ?t) ?dir-expr)
    (variable ?mag-var (mag (force ?b ?agent applied :time ?t)))
    (variable ?dir-var (dir (force ?b ?agent applied :time ?t)))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir-expr) (dir (force ?b ?agent applied :time ?t)))
   )
  :hint
   ((point (string "You were given that there is an applied force on ~a." ?b))
    (bottom-out (string "Use the force drawing tool to draw the applied force on ~a due to ~a ~a at ~a." ?b (?agent agent) (?t pp) ?dir-expr))
    ))

(defoperator find-applied-force-unknown-dir (?b ?agent ?t)
  :preconditions 
  (
   ;; energy conservation law also checks for this to not exist:
   (in-wm (unknown-applied-force ?b ?agent :time ?t-force))
   (time ?t)
   (test (tinsidep ?t ?t-force))
   ;; check that something else hasn't defined this force.
   (not (force ?b ?agent applied ?t . ?dont-care)) 
   )
  :effects (
	    (force ?b ?agent applied ?t unknown action)
	    (force-given-at ?b ?agent applied ?t-force unknown action)
  ))

(defoperator draw-applied-force-unknown (?b ?agent ?t)
  :specifications 
  "if you are given that there is an applied force on an object at a time
   at a certain direction,
  then draw the force at that direction"
  :preconditions
  ( (force ?b ?agent applied ?t unknown action)
    (not (vector ?b (force ?b ?agent applied :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fa_~A_~A~@[_~A~]" 
			       (body-name ?b) ?agent (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    )
  :effects
   ((vector ?b (force ?b ?agent applied :time ?t) unknown)
    (variable ?mag-var (mag (force ?b ?agent applied :time ?t)))
    (variable ?dir-var (dir (force ?b ?agent applied :time ?t)))
   )
  :hint
   ((point (string "You were given that there is an applied force on ~a." ?b))
    (bottom-out (string "Use the force drawing tool to draw the applied force on ~a due to ~a ~a at an approximately correct angle, then erase the number in the direction box to indicate that its exact direction is unknown." 
			?b (?agent agent) (?t pp)))
    ))

;; draw kinetic friction force on ?b due to ?surface
;; requires a (slides-against ?surface ?b :time ?t) statement in the problem 
;; This form is only to be used where there is a frictional interaction, 
;; just leave it out for frictionless contact.
;; ! might still want to include "frictionless/frictional" tag somewhere
;; to explicitly indicate when friction is to be neglected (helpsys might use).
;; This only handles friction for objects given to be in straight
;; line motion in known direction. 
;; We could add another version for object given in curved motion. 
;; We could also change to derive direction from the velocity 
;; vector direction instead. This would be a more general rule; however, as 
;; operators are structured now this would require a step of drawing the 
;; velocity vector before you could draw the friction direction.
(defoperator find-kinetic-friction-force (?b ?surface ?t)
  :preconditions (
    (slides-against ?b ?surface :time ?t-slides)
    (motion ?b straight :dir ?motion-dir :time ?t-motion ?t . ?whatever)
    (time ?t)
    (object ?b)
    (not (force ?b ?surface kinetic-friction ?t . ?dont-care))
    (test (tinsidep ?t (tintersect2 ?t-slides ?t-motion)))
    (bind ?friction-dir (opposite ?motion-dir))
   )
  :effects (
    (force ?b ?surface kinetic-friction ?t ?friction-dir action)
    (force-given-at ?b ?surface kinetic-friction ?t-slides 
		    ?friction-dir action)
  ))

(defoperator draw-kinetic-friction (?b ?surface ?t)
  :specifications 
   "If an object slides along a surface in direction ?dir,
   then there is a kinetic friction force on the object due to the surface,
      and it is opposite the direction of motion"
  :preconditions
   ((force ?b ?surface kinetic-friction ?t ?friction-dir action)
    (test (not (eq ?friction-dir 'unknown))) ;only explicit angles
    (not (vector ?b (force ?b ?surface kinetic-friction :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Ff_~A_~A~@[_~A~]" (body-name ?b) ?surface 
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a kinetic-friction for ~a due to ~a at ~a.~%" ?friction-dir ?b ?surface ?t)
    )
  :effects
   ((vector ?b (force ?b ?surface kinetic-friction :time ?t) ?friction-dir)
    (variable ?mag-var (mag (force ?b ?surface kinetic-friction :time ?t)))
    (variable ?dir-var (dir (force ?b ?surface kinetic-friction :time ?t)))
    (given (dir (force ?b ?surface kinetic-friction :time ?t)) ?friction-dir))
  :hint
   ((point (string "Notice that ~a is sliding across ~a." ?b ?surface))
    (teach (minilesson "Mini_kinetic_friction.HTM")
           (kcd "dynamic_friction_force_direction")
	   (string "When an object is moving in contact with a surface and the surface exerts a kinetic friction force on it.  The friction force is opposite to the direction of motion."))
    (bottom-out (string "Because ~a is moving in contact with ~a, draw a kinetic friction force on ~a due to ~a at an angle of ~a." 
			?b (?surface agent) ?b (?surface agent) 
			(?friction-dir adj)))
    ))

(defoperator kinetic-friction-law-contains (?quantity)
  :preconditions(
    (any-member ?quantity (
	           (mag (force ?b ?surface kinetic-friction :time ?t))
		   (mag (force ?b ?surface normal :time ?t))
		   (coef-friction ?b ?surface kinetic)
                 	  ))
    (slides-against ?b ?surface :time ?t-slides)
    (time ?t)
    (test (tinsidep ?t ?t-slides))
  )
  :effects(
    (eqn-contains (kinetic-friction ?b ?surface ?t) ?quantity)
  ))

(defoperator kinetic-friction-law (?b ?surface ?t)
  
  :preconditions (
    (variable ?ff-var (mag (force ?b ?surface kinetic-friction :time ?t)))
    (variable ?N-var (mag (force ?b ?surface normal :time ?t)))
    (variable ?mu-var (coef-friction ?b ?surface kinetic))
  )
  :effects (
    (eqn (= ?ff-var (* ?mu-var ?N-var)) (kinetic-friction ?b ?surface ?t))
  ))

; draw a static friction force
; The direction of static friction opposes the motion that would occur if there 
; were no static friction. The magnitude can take on any value up to a maximum 
; determined by the coefficient of static friction.  For now, the existence of 
; static friction must just be given in the problem statement by a 
; "static-friction" statement which also gives its direction.  
; Some problems may make use of the 'max tag in the given static-friction 
; statement to apply the static friction law Fsf_max = mu_sf * N.
(defoperator find-static-friction-force (?b ?surface ?t)
   :preconditions
   ((object ?b)
    (time ?t)
    (static-friction ?b ?surface :time ?t-friction :dir ?friction-dir . ?any)
    (test (tinsidep ?t ?t-friction))
    (not (force ?b ?surface static-friction ?t . ?dont-care)))
   :effects (
    (force ?b ?surface static-friction ?t ?friction-dir action)
    (force-given-at ?b ?surface static-friction ?t-friction 
		    ?friction-dir action)
   ))

(defoperator draw-static-friction (?b ?surface ?t)
  :specifications 
   "If it is known that there is a static friction force in a given direction, draw it"
  :preconditions
   ((force ?b ?surface static-friction ?t ?friction-dir action)
    (not (vector ?b (force ?b ?surface static-friction :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fsf_~A_~A~@[_~A~]" (body-name ?b) ?surface 
                                              (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a static-friction for ~a due to ~a at ~a.~%" ?friction-dir ?b ?surface ?t)
    )
  :effects
   ((vector ?b (force ?b ?surface static-friction :time ?t) ?friction-dir)
    (variable ?mag-var (mag (force ?b ?surface static-friction :time ?t)))
    (variable ?dir-var (dir (force ?b ?surface static-friction :time ?t)))
    (given (dir (force ?b ?surface static-friction :time ?t)) ?friction-dir))
  :hint
   ((point (string "Notice that ~a is not moving with respect to ~a." ?b ?surface))
    (teach (string "If an object is in contact with a surface and not moving with respect to it, the surface exerts a static friction force on it.  The friction force is opposite to the direction of incipient motion."))
    (bottom-out (string "Because ~a is in contact with but not moving with respect to ~a, draw a static friction force on ~a due to ~a at an angle of ~a." 
			?b (?surface agent) ?b (?surface agent) ?friction-dir))
    ))

; requires we are given that static friction takes on its max value
(defoperator static-friction-law-contains (?quantity)
  :preconditions(
    (static-friction ?b ?surface :time ?t-friction :max ?max . ?rest)
    (test ?max)
    (time ?t)
    (test (tinsidep ?t ?t-friction))
    (any-member ?quantity (
	           (mag (force ?b ?surface static-friction :time ?t))
		   (mag (force ?b ?surface normal :time ?t))
		   (coef-friction ?b ?surface static)
                 	  ))
  )
  :effects(
    (eqn-contains (static-friction ?b ?surface ?t) ?quantity)
  ))

(defoperator static-friction-law (?b ?surface ?t)
  
  :preconditions (
    (variable ?ff-var (mag (force ?b ?surface static-friction :time ?t)))
    (variable ?N-var   (mag (force ?b ?surface normal :time ?t)))
    (variable ?mu-var (coef-friction ?b ?surface static))
  )
  :effects (
    (eqn (= ?ff-var (* ?mu-var ?N-var)) (static-friction ?b ?surface ?t))
  )
  :hint (
    (point (string "You know that the static friction takes on its maximum value in this problem"))
    (teach 
        (kcd "dynamic_friction_PSM")
        (string "When static friction is at its maximum, the magnitude of the static friction force a surface exerts on an object is equal to the coefficient of static friction times the normal force on the object from the surface"))
    (bottom-out (string "Write the equation ~A" ((= ?ff-var (* ?mu-var ?N-var)) algebra)))
  ))

;; Define a variable for coefficient of friction, either static or kinetic
;; Here assuming it doesn't vary with time.
;; Expect all variables will be bound coming in.
;; !!! Note args are ordered. Body must come first, then supporting surface.
;;; could be confusing if between two bodies.
(defoperator define-coef-friction (?b ?surface ?type)
  :preconditions (
   (bind ?mu-var (format-sym "mu~A_~A_~A" (if (equal ?type 'static) "s" "k") 
                                  ?b ?surface ))
  )
  :effects (
    (define-var (coef-friction ?b ?surface ?type))
    (variable ?mu-var (coef-friction ?b ?surface ?type))
  ))

; draw drag force on ?b due to ?medium
; requires a (drag ?b ?medium :time ?t) statement in the problem 
; Drawing rules essentially similar to kinetic-friction force. 
; Drag force opposes straight-line motion direction.
(defoperator find-drag-force (?b ?medium ?t)
  :preconditions (
    (object ?b)
    (time ?t)
    (drag ?b ?medium :time ?t-slides)
    (test (tinsidep ?t ?t-slides))
    (not (force ?b ?medium drag ?t . ?dont-care))
    (motion ?b straight :dir ?motion-dir :time ?t-motion . ?whatever)
    (test (tinsidep ?t ?t-motion))
    (bind ?drag-dir (opposite ?motion-dir))
   )
  :effects (
    (force ?b ?medium drag ?t ?drag-dir action)
    (force-given-at ?b ?medium drag ?t-slides ?drag-dir action)
  ))

(defoperator draw-drag (?b ?medium ?t)
  :specifications 
   "If an object moves in a fluid medium in direction ?dir,
   then there is a drag force on the object due to the medium,
      and it is opposite the direction of motion"
  :preconditions
   ((force ?b ?medium drag ?t ?drag-dir action)
    (not (vector ?b (force ?b ?medium drag :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fd_~A_~A~@[_~A~]" (body-name ?b) ?medium 
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a drag for ~a due to ~a at ~a.~%" ?drag-dir ?b ?medium ?t)
    )
  :effects
   ((vector ?b (force ?b ?medium drag :time ?t) ?drag-dir)
    (variable ?mag-var (mag (force ?b ?medium drag :time ?t)))
    (variable ?dir-var (dir (force ?b ?medium drag :time ?t)))
    (given (dir (force ?b ?medium drag :time ?t)) ?drag-dir))
  :hint
   ((point (string "Notice that ~a is moving in a fluid medium ~a." ?b ?medium))
    (teach (string "When an object is moving in a fluid medium, the fluid offers resistance to the motion of the object.  This is represented by a drag force directed opposite to the direction of motion."))
    (bottom-out (string "Because ~a is moving in fluid medium ~a, draw a drag force on ~a due to ~a at an angle of ~a." 
			?b (?medium agent) ?b (?medium agent) (?drag-dir adj)))
    ))

(defoperator drag-force-turbulent-contains (?quantity)
  :preconditions
  (
   (drag ?b ?medium :time ?t-drag)
   (any-member ?quantity (
	           (mag (force ?b ?medium drag :time ?t))
		   (mag (velocity ?b :time ?t))
		   (coef-drag ?b ?medium :type turbulent :time ?t)
		   ))
    (time ?t)
    (test (tinsidep ?t ?t-drag))
  )
  :effects(
    (eqn-contains (drag-force ?b ?medium turbulent ?t) ?quantity)
  ))

(defoperator write-drag-force-turbulent (?b ?medium ?t)
  
  :preconditions (
    (variable ?f-var (mag (force ?b ?medium drag :time ?t)))
    (variable ?v-var (mag (velocity ?b :time ?t)))
    (variable ?k-var (coef-drag ?b ?medium :type turbulent :time ?t))
  )
  :effects (
    (eqn (= ?f-var (* ?k-var (^ ?v-var 2))) 
	 (drag-force ?b ?medium turbulent ?t))
    )
  :hint 
  (
   (point (string "~A produces a drag force on ~A ~A" ?medium ?b (?t pp)))
   (teach (string "When a body is moving at high speed through a fluid medium (like air), the {\\l drag force is proportional to the velocity squared}{\\v DragForce.html}."))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?f-var (* ?k-var (^ ?v-var 2))) algebra)))
  ))

(defoperator define-coef-drag-force (?b ?medium ?type ?t)
  :preconditions (
   (bind ?K-var (format-sym "K~A_~A_~A~@[_~A~]" 
			     (if (equal ?type 'turbulent) "2" "1") ;power
                                  ?b ?medium (time-abbrev ?t)))
  )
  :effects (
    (define-var (coef-drag ?b ?medium :type ?type :time ?t))
    (variable ?K-var (coef-drag ?b ?medium :type ?type :time ?t))
  )
  :hint 
  (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Coef. drag."
		       ((coef-drag ?b ?medium :type ?type :time ?t) def-np)))
   ))


;; Spring force
;;
;; Spring forces by Hooke's law not fully implemented in Andes (no deep
;; reason for this). We currently only use springs in energy problems.
;; We need to know that a spring force exists so that we can include it
;; in the net work done on an object. Can add Hooke's law problems later
;;
;; spring-contact statement includes direction of the force. Note we may
;; include this statement even for times at which compression is zero,
;; so that zero-valued term for spring energy is included in total energy
(defoperator find-spring-force (?b ?spring ?t)
  :preconditions(
     (object ?b)
     (time ?t)
     ;; make sure in contact with spring at t and dir is not zero
     (spring-contact ?b ?spring ?t-contact ?force-dir)
     (test (not (eq ?force-dir 'zero)))
     (test  (tinsidep ?t ?t-contact))
  )
  :effects (
    (force ?b ?spring spring ?t ?force-dir action)
    (force-given-at ?b ?spring spring ?t-contact ?force-dir action)
  ))

(defoperator draw-spring-force (?b ?spring ?t)
  :preconditions 
   ((force ?b ?spring spring ?t ?force-dir action)
    (not (vector ?b (force ?b ?spring spring :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fs_~A_~A~@[_~A~]" (body-name ?b) ?spring (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a spring force on ~a due to ~a at ~a.~%" ?force-dir ?b ?spring ?t)
    )
  :effects
   ((vector ?b (force ?b ?spring spring :time ?t) ?force-dir)
    (variable ?mag-var (mag (force ?b ?spring spring :time ?t)))
    (variable ?dir-var (dir (force ?b ?spring spring :time ?t)))
    (given (dir (force ?b ?spring spring :time ?t)) ?force-dir))
   :hint
   ((point (string "Notice that ~a is in contact with a compressed spring ~a." ?b (?t pp)))
    (teach (string "A compressed spring exerts a restorative force on an object in contact with it.  The spring force opposes the compression of the spring from its equilibrium length."))
    (bottom-out (string "Because ~a is in contact with compressed ~a, draw a spring force on ~a due to ~a at an angle of ~a." 
			?b (?spring agent) ?b (?spring agent) 
			(?force-dir adj)))
    ))

;;; UG -- Newton's law of Universal Gravitation
;;;
;;; We use "near-planet" statements when problem takes place within a region 
;;; near the surface of a planet throughout which gravitational force can be 
;;; treated as m*g.  For greater distances we need the general law of UG in 
;;; terms of the gravitational constant G.  This is enabled by a statement
;;;   (gravity (orderless ?body1 ?body2 ...) :time ?t)
;;; to mean there is a gravitational interaction between any pair of bodies
;;; in the list.  
;;;
;;; We normally treat the r in the law of universal gravitation as the
;;; magnitude of the relative position vector from the center of the body 
;;; exerting the force to the center of the body experiencing the force.
;;; Referring to centers of bodies: though the kb can employ
;;; functional terms like center-of(b1), there is currently no way to specify
;;; complex terms like this in the workbench. So we need to use
;;; atomic names like center_of_Earth and include propositions like
;;;   (center-of-mass center_of_Earth (Earth))
;;; to enable the rules to map object names to names of their center points. 
;;; Note if an object is treated as a point we have to include the statement
;;;   (center-of-mass person1 (person1))  
;;; for these rules to work.
;;;
;;; The equation is scalar equation containing vector magnitudes only.
(defoperator ug-contains (?sought)
   :preconditions (
     ;; first make sure a gravitational interaction exists in problem
     (gravity (orderless . ?grav-bodies) :time ?t-grav)
     (any-member ?sought (
		    ;; if sought is a mass, can use either equation for force
		    ;; on b1 from b2 or force on b2 from b1, so need both:
                    (mass ?b1) (mass ?b2)  ;only timeless mass 
		    (mag (force ?b1 ?b2 gravitational :time ?t))
		    (mag (relative-position ?c1 ?c2 :time ?t))
                         ))
     (any-member ?b1 ?grav-bodies)
     (any-member ?b2 ?grav-bodies)
     ;; in case sought is relative position:
     (center-of-mass ?c1 (?b1))
     (center-of-mass ?c2 (?b2))
     (time ?t)
     (test (tinsidep ?t ?t-grav))
   )
   :effects (
    (eqn-contains (ug ?b1 ?b2 ?t rel-pos) ?sought)
   ))

(defoperator write-ug (?b1 ?t ?b2) 
  :preconditions (
      (body ?b1)
      (variable ?m1 (mass ?b1))
      (variable ?m2 (mass ?b2))
      ;; force is on b1 due to b2, so want relative position of center of
      ;; b1 wrt center of b2. 
      (center-of-mass ?c1 (?b1))
      (center-of-mass ?c2 (?b2))
      (variable ?r (mag (relative-position ?c1 ?c2 :time ?t)))
      (variable ?F (mag (force ?b1 ?b2 gravitational :time ?t)))
  )
  :effects 
  ;; G is predefined, see file constants.cl
  ( (eqn (= ?F (/ (* |G| ?m1 ?m2) (^ ?r 2))) (ug ?b1 ?b2 ?t rel-pos)) )
  :hint (
     (teach (string "Newton's law of universal gravitation states that the magnitude of the gravitational force between two bodies is equal to the gravitational constant G times the masses of the bodies divided by the square of the distance between the bodies."))
     (bottom-out (string "Write the equation ~A" 
                          ((= ?F (/ (* G ?m1 ?m2) (^ ?r 2))) algebra)))
  ))

;; When gravity is applied to an object in circular motion, it is
;; much more natural to use the radius of motion for the "r" in the law of
;; UG. So we have a variant form to apply in that case, which uses radius
;; instead of rel-pos. The last argument in the eqn id is 'rel-pos or 'radius
;; depending on which quantity is used to represent the r.

(defoperator ug-circular-contains (?sought)
   :preconditions (
     ; first make sure gravitational interaction exists in problem
     (gravity (orderless . ?grav-bodies) :time ?t-grav)
     ; make sure body1 is in circular motion for this form
     (motion ?b1 (curved circular ?dontcare) :time ?t-circular)
     (any-member ?sought (
                    (mass ?b1) (mass ?b2)
		    (mag (force ?b1 ?b2 gravitational :time ?t))
		    (revolution-radius ?b1 :time ?t)
			 ))
     (any-member ?b1 ?grav-bodies)
     (any-member ?b2 ?grav-bodies)
     (time ?t)
     (test (tinsidep ?t ?t-circular))
     (test (tinsidep ?t ?t-grav))
   )
   :effects (
    (eqn-contains (ug ?b1 ?b2 ?t radius) ?sought)
   ))

(defoperator write-ug-circular (?b1 ?t ?b2) 
  :preconditions (
      (body ?b1)
      (variable ?m1 (mass ?b1)) ;only timeless mass
      (variable ?m2 (mass ?b2)) ;only timeless mass
      ; force is on b1 due to b2, so want relative position of center of
      ; b1 wrt center of b2. Implicit for now that positions are wrt centers.
      (variable ?r  (revolution-radius ?b1 :time ?t))
      (variable ?F  (mag (force ?b1 ?b2 gravitational :time ?t)))
  )
  :effects (
      (eqn (= ?F (/ (* G ?m1 ?m2) (^ ?r 2))) (ug ?b1 ?b2 ?t radius))
  )
  :hint (
     (teach (string "Newton's law of universal gravitation states that the magnitude of the gravitational force between two bodies is equal to the gravitational constant G times the masses of the bodies divided by the square of the distance between the bodies."))
     (bottom-out (string "Write the equation ~A" 
                          ((= ?F (/ (* G ?m1 ?m2) (^ ?r 2))) algebra)))
  ))

;; need an operator to draw the gravitational force
;; In order for the force to be drawn, the givens must contain the
;; direction of the relative position vector from the center of
;; b2 to the center to b1, or its inverse. Note that a center-of
;; statement specifying the center of an object is required for
;; all objects subject to a gravitational force, even if the object
;; is treated as a particle. It is permitted to say
;;      (center-of-mass astronaut (astronaut))
;; in which case the required relative position will wind up 
;; specified as that of astronaut with respect to center_of_Earth, say.
;;
;; In keeping with other force-detecting rules, we need a "force" statement
;; to report the existence of a force without drawing it. Because the
;; force-inferring rule for gravity is symmetrical, we don't declare either
;; force the "action" force for an action-reaction pair, but just put NIL
;; in that slot in the force proposition. Thus Newton's third law will never
;; be cited in inferring the existence of a gravitational force. 

(defoperator find-grav-force (?b1 ?b2 ?t)
  :preconditions 
  (
   (gravity (orderless . ?grav-bodies) :time ?t-grav)
   ;; ?b1 probably bound coming in if finding all forces on it,
   ;; but agent ?b2 is probably not bound:
   (any-member ?b1 ?grav-bodies)
   (any-member ?b2 ?grav-bodies)
   ;; We get force direction as oppposite of relative position direction. 
   ;; Don't require r to be drawn -- ug-circular form doesn't use it.
   (grav-direction ?b1 ?b2 ?t ?dir)
   (test (not (unify ?b1 ?b2))) ;forbid self-action of force (sanity test)
   (time ?t)
   (test (tinsidep ?t ?t-grav))
   )
  :effects ((force ?b1 ?b2 gravitational ?t ?dir NIL)
	    ;; NIL given time should be translated to sought interval
	    (force-given-at ?b1 ?b2 gravitational NIL ?dir NIL)
	    ))

(defoperator grav-dir-from-rel-pos (?b1 ?b2 ?t)
  :preconditions 
  (
   (in-wm (center-of-mass ?c1 (?b1)))
   (in-wm (center-of-mass ?c2 (?b2)))
   (in-wm (given (dir (relative-position ?c1 ?c2 :time ?t-given)) ?r-dir 
		 . ?rest))
   (time ?t)
   (test (tinsidep ?t ?t-given))
    (bind ?grav-dir (opposite ?r-dir))
    )
  :effects ((grav-direction ?b1 ?b2 ?t ?grav-dir)))
     
(defoperator grav-dir-from-inverse-rel-pos (?b1 ?b2 ?t)
  :preconditions 
  (
   (in-wm (center-of-mass ?c1 (?b1)))
   (in-wm (center-of-mass ?c2 (?b2)))
   (in-wm (given (dir (relative-position ?c2 ?c1 :time ?t-given)) ?r-dir
		 . ?rest))
   (time ?t) 
   (test (tinsidep ?t ?t-given))
  )
  :effects ((grav-direction ?b1 ?b2 ?t ?r-dir)))

;; In grav3, this is needed to make the net-work PSM to fail at (during 1 2).
;; This makes a gravitational force, but no work can be calculated from it. 
(defoperator grav-dir-unknown (?b1 ?b2 ?t)
  :preconditions 
  (
   (in-wm (center-of-mass ?c1 (?b1)))
   (in-wm (center-of-mass ?c2 (?b2)))
   (test (not (unify ?c1 ?c2))) ;forbid self-action of force (needed test)
   (time ?t) 
   (not (given (dir (relative-position ?c1 ?c2 :time ?t-given)) . ?r-dir1)
	(tinsidep ?t ?t-given))
   (not (given (dir (relative-position ?c2 ?c1 :time ?t-given)) . ?r-dir2)
	(tinsidep ?t ?t-given))
  )
  :effects ((grav-direction ?b1 ?b2 ?t unknown)))

(defoperator draw-grav-force (?b1 ?b2 ?t)
  :preconditions 
  (
    (force ?b1 ?b2 gravitational ?t ?dir NIL)
    (bind ?mag-var (format-sym "Fg_~A_~A~@[_~A~]" (body-name ?b1) (body-name ?b2)
                                             (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects (
    (vector ?b1 (force ?b1 ?b2 gravitational :time ?t) ?dir)
    (variable ?mag-var (mag (force ?b1 ?b2 gravitational :time ?t)))
    (variable ?dir-var (dir (force ?b1 ?b2 gravitational :time ?t)))
    (given (dir (force ?b1 ?b2 gravitational :time ?t)) ?dir)
  )
  :hint (
    (point (string "Notice that ~a is subject to a gravitational force due to ~A." 
                   ?b1 (?b2 agent)))
    (teach (string "Every massive particle in the universe attracts every other massive particle with a gravitational force acting along a line joining the two particles. In the case of uniform spheres, the gravitational force acts along a line joining the centers of the two spheres."))
    (bottom-out (string "Draw the gravitational force on ~a due to ~a at a direction of ~a" ?b1 (?b2 agent) ?dir))
  )
)

;;;;===========================================================================
;;;;
;;;;                       Thrust force
;;;;
;;;;===========================================================================


;; Right now, we assume the direction of the force
;; is one of the givens in the problem statement.
(defoperator find-thrust-force (?body ?agent ?t)
  :preconditions
  ( ;; energy conservation law also checks for this:
    (in-wm (given (dir (force ?body ?agent thrust :time ?t-force)) ?dir))
    (object ?body)
    (time ?t)
    (test (tinsidep ?t ?t-force))
    ;; check that something else hasn't defined this force.
    (not (force ?b ?agent thrust ?t . ?dont-care)) ) 
  :effects ( 
	    (force ?body ?agent thrust ?t ?dir action) 
	    (force-given-at ?body ?agent thrust ?t-force ?dir action)
	    ))

;; draw thrust force in a known direction
(defoperator draw-thrust-force (?b ?agent ?t)
  :preconditions
  ( (force ?b ?agent thrust ?t ?dir action)
    (test (not (equal ?dir-expr 'unknown)))
    (not (vector ?b (force ?b ?agent thrust :time ?t) ?dont-care))
    (bind ?mag-var (format-sym "Fth_~A_~A~@[_~A~]" (body-name ?b) ?agent
			       (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "~&Drawing ~a thrust force on ~a due to ~a at ~a.~%" ?dir ?b ?agent ?t)
    )
  :effects
  ( (vector ?b (force ?b ?agent thrust :time ?t) ?dir)
    (variable ?mag-var (mag (force ?b ?agent thrust :time ?t)))
    (variable ?dir-var (dir (force ?b ?agent thrust :time ?t)))
    ;; Ensure implicit eqn is written because dir is problem given
    (implicit-eqn (= ?dir-var ?dir) (dir (force ?b ?agent thrust :time ?t))))
  :hint
( (point (string "Notice that ~a causes a force on ~A." ?agent ?b))
    (teach (string "When ~A escapes from ~A, a thrust force is exerted on ~A."
		   ?agent ?b ?b))
    (bottom-out (string "Draw a thrust force acting on ~a due to ~a at an angle of ~a." 
			?b (?agent agent) ?dir))
    ))


;;;
;;;          Define thrust force
;;;

;;; scalar version of definition:

(def-PSMclass thrust-force (thrust-definition ?body ?agent ?time)
    :complexity minor  ;same as definitions of other forces
    :Doc "Definition of thrust force."
  :short-name "thrust force (magnitudes)"
    :english ("the definition of thrust force") 
    :ExpFormat ("applying the definition of thrust force on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("F = v*abs(dmdt)"))

(defoperator thrust-force-contains (?sought)
  :preconditions 
  ( (any-member ?sought
		((mag (force ?b ?agent thrust :time ?t))
		 (mag (relative-vel ?agent ?b :time ?t))
		 (mass-change-magnitude ?b ?agent :time ?t)))
    (object ?b)
    (time ?t)
   )
  :effects 
   ((eqn-contains (thrust-definition ?b ?agent ?t) ?sought)))

;; This is the thrust-force from a particular force
(defoperator write-thrust-force (?b ?agent ?t)
  :preconditions 
   ((variable ?Fth (mag (force ?b ?agent thrust :time ?t)))
    (variable ?vr (mag (relative-vel ?agent ?b :time ?t)))
    (variable ?dmdt  (mass-change-magnitude ?b ?agent :time ?t)))
  :effects (
   (eqn (= ?Fth (* ?vr ?dmdt)) (thrust-definition ?b ?agent ?t))
   (assume using-magnitude (thrust ?b ?agent ?t)) ;magnitude xor components
   )
  :hint 
  ( (point (string "What is the relationship between thrust force, the velocity of ~A, and the magnitude of the mass change rate of ~A?"
?b ?agent ?b))
    (teach (string "The magnitude of the thrust force is defined the speed of ~A relative to ~A times the magnitude of the mass change rate of ~A." ?agent ?b ?b))
    (bottom-out (string "Write the equation ~a"
			((= ?Fth (* ?vr ?dmdt)) algebra)))
  ))

;;; vector version of thrust force
(def-PSMclass thrust-force-vector (?eqn-type definition ?axis ?rot 
				 (thrust ?body ?agent ?time))
    :complexity minor    ;same as definitions of other forces
    :Doc "Definition of thrust force."
  :short-name ("thrust force (~A components)" (axis-name ?axis))
    :english ("the definition of thrust force") 
    :ExpFormat ("applying the definition of thrust-force on ~a ~a"
		(nlg ?body) (nlg ?time 'pp))
    :EqnFormat ("F_~A = -v_~a*dmdt" (axis-name ?axis) (axis-name ?axis)))


(defoperator thrust-force-vector-diagram (?rot ?b ?agent ?t)
  :preconditions (
    ;; Draw only one body 
    (body ?b)
    (vector ?b (force ?b ?agent thrust :time ?t) ?dir1)
    (vector ?agent (relative-vel ?agent ?b :time ?t) ?dir2)
    ;; we need axis-for each body, since component defining operators will 
    ;; lookup axis-for principal body of each vector. Our operators that
    ;; draw axes only apply once, so there is no danger of drawing two
    ;; axes. In order to reuse the axes drawn for body1 as axes used
    ;; for vectors on body2, we added reuse-other-body-axis in axes section.
    (axes-for ?b ?rot)
  )
  :effects (
    (vector-diagram ?rot (thrust ?b ?agent ?t))
  ))

(defoperator thrust-force-vector-contains (?sought)
  :preconditions 
  ( (any-member ?sought
		((force ?b ?agent thrust :time ?t)
		 (relative-vel ?agent ?b :time ?t)
		 (mass-change-magnitude ?b ?agent :time ?t)))
    (object ?b)
   (time ?t)
   )
  :effects 
   ((eqn-family-contains (thrust ?b ?agent ?t) ?sought)
    ;; since only one compo-eqn under this vector PSM, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (thrust ?b ?agent ?t) definition ?sought)))

;; This is the thrust-force from a particular force
(defoperator write-thrust-force-compo (?b ?agent ?t ?xy ?rot)
  :preconditions 
   ((variable ?Fth_x  (compo ?xy ?rot (force ?b ?agent thrust :time ?t)))
    (variable ?vr_x  (compo ?xy ?rot (relative-vel ?agent ?b :time ?t)))
    (variable ?dmdt  (mass-change-magnitude ?b ?agent :time ?t)))
  :effects (
   (eqn (= ?Fth_x (* -1 ?vr_x ?dmdt))
            (compo-eqn definition ?xy ?rot (thrust ?b ?agent ?t)))
   )
  :hint 
  ( (point (string "What is the relationship between thrust force, the velocity of ~A, and the magnitude of the mass change rate of ~A?"
?b ?agent ?b))
    (teach (string "The thrust force vector is defined as minus the velocity of ~A relative to ~A times the magnitude of the mass change rate of ~A.  This can be applied component-wise." ?agent ?b ?b))
    (bottom-out (string "Write the equation ~a"
			((= ?Fth_x (* -1 ?vr_x ?dmdt)) algebra)))
  ))


;;;============================================================================
;;;
;;; Other basic force types in current ANDES: 
;;; 	"contact force" -- meaning unclear, trying to do without it.
;;; draw-reaction-force -- draws equal and opposite force of same type
;;;
;;; Our "primary" force finding rules are usually asymmetrical -- they derive 
;;; only one side of an action-reaction pair of forces from a force-determining
;;; proposition: 
;;;        supports s b => Normal force on b from s 
;;;        tied-to str b => tension force on b from str
;;;        slides-against b s => kinetic friction on b from s
;;;        static-friction b s => static friction on b from s
;;;        given (dir (force b a applied)) => applied force on b from a
;;; This operator can draw the reaction to any of these "action" forces on 
;;; the other body when needed.  We also have an NTL PSM to put out the 
;;; equation equating the magnitudes.  Currently we only use NTL for forces 
;;; at known directions, so the direction of the reaction force becomes known 
;;; ("given") here.
;;;
;;; A conceptually purer alternative would be to make all our force-inferring 
;;; rules symmetrical, so an interaction statement would directly entail 
;;; *both* forces in the action-reaction pair.  The asymmetrical method used 
;;; here implicates Newton's third law in one direction only.  This seems to 
;;; correspond more to the reasoning we want to tutor on the forces: If a 
;;; table supports a block, we probably want a different hint or dialog for 
;;; the Normal force the table exerts on the block than for the Normal force 
;;; the block exerts on the table, with NTL only mentioned in the second.
;;;
;;; Another way to achieve this heuristic goal would be to write a pair of 
;;; rules for each force, one for each direction, but the current method is 
;;; more economical in treating Newton's third law with a single rule.
;;;
;;; A question is whether the "action" force ought to be drawn before 
;;; drawing the "reaction" force. If we are analyzing the forces on block1 
;;; with block2 on top of it, we might want to draw the downward normal force 
;;; before drawing the upward one.  So here we consult the "force" statement to
;;; test for existence of the action force but don't require drawing it.
;;;
;;; We prevent this operator from applying to interactions involving compound 
;;; bodies to prevent the following problem: two blocks are stacked on
;;; table. The table exerts a normal force on the lower block and also on
;;; the compound. Therefore two reaction forces will be found on the table:
;;; one from the lower block and one from the table.  If we were interested in
;;; all forces on the table for applying Newton's law, this would give us 
;;; the wrong set of forces on the table -- we don't want to count both 
;;; the force from the compound and the one from its part.  This will have to 
;;; be fixed, but for now we just don't apply NTL to compound bodies.  
;;; That is OK in our problems since we are usually interested in the dynamics 
;;; of the compound body and the external forces on it.


(defoperator find-reaction-force (?b1 ?b2 ?type ?t)
  :preconditions 
  (
   ;; We look for "action" force exerted *on* object b2 from b1.
   (force ?b2 ?b1 ?type ?t ?f1-dir action)

   ;; make sure that this force cannot be generated by other means
   ;; it is not sufficient to test working memory
;;; It is probably better not to make this test.  Rather, 
;;; an error will occur when there is a conflict.
   ;; (setof (force ?b1 ?b2 ?type ?t ?f-dir action) ?f-dir ?f-dirs)
   ;; (debug "find-reaction-force test list ~A~%" ?f-dirs)
   ;; (test (null ?f-dirs))
   
   ;; We have been allowing some force agents to be implicitly defined by 
   ;; occurrence of their names in arguments of forms like tied-to or supports.
   ;; Following imposes the requirement that these must be declared in an 
   ;; object proposition if a reaction force is to be found. 
   (object ?b1)
   (test (not (compound-bodyp ?b1)))	; ignore compound bodies here
   (object ?b2)
   (test (not (compound-bodyp ?b2)))	; ignore compound bodies here
   (test (not (equal ?b1 ?b2)))
   (bind ?opposite-dir (opposite ?f1-dir))
   )
  :effects (
	    (force ?b1 ?b2 ?type ?t ?opposite-dir reaction)
	    ))

(defoperator draw-reaction-force (?b1 ?b2 ?type ?t)
  :preconditions(
    (force ?b2 ?b1 ?type ?t ?dir reaction)
    (not (vector ?b2 (force ?b2 ?b1 ?type :time ?t) ?whatever-dir))
    ;; unique symbol for a reaction force.  Thus, if the force is also
    ;; drawn through other means, an error should occur.
    (bind ?mag-var (format-sym "rF~A_~A_~A~@[_~A~]" 
			       ?type (body-name ?b2) 
			       (body-name ?b1) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
  )
  :effects (
    (vector ?b2 (force ?b2 ?b1 ?type :time ?t) ?dir) 
    (variable ?mag-var (mag (force ?b2 ?b1 ?type :time ?t)))
    (variable ?dir-var (dir (force ?b2 ?b1 ?type :time ?t)))
;; BvdS:  are these really needed?
;;    (given (dir (force ?b2 ?b1 ?type :time ?t)) ?dir)
;;    (implicit-eqn (= ?dir-var ?dir) (dir (force ?b2 ?b1 ?type :time ?t)))
  )
  :hint (
    (point (string "Notice that ~a and ~a are exerting forces on each other." ?b1 ?b2))
    (teach 
        (kcd "third_law_PSM")
        (string "Newton's third law states that forces always come in pairs: whenever one body exerts a force on a second body, the second body exerts a force of the same type back on the first body. The members of these action/reaction pairs are equal in magnitude and opposite in direction"))
    (bottom-out (string "Because there is a ~A force on ~A due to ~a, draw the reaction force, namely, a ~A force on ~A due to ~A at ~A" 
			(?type adjective) (?b1 agent) ?b2 (?type adjective) 
			?b2 (?b1 agent) (?dir adj)))
    ))


;;; compound body force rules
;;; if there is a forceful interaction between a compound body part and
;;; an object outside the compound (i.e. an external force) then there is
;;; an interaction of the same type between the compound and the object.
;;; The general rule is that the magnitude of a force of type T on the compound
;;; from a is the sum of the magnitudes of the forces of type T from that agent
;;; on the parts.  This sum rule is needed to handle cases where the same agent
;;; may have an interaction of the same type with several of the bodies.  
;;; For ex, if two blocks are pushed side by side along a plane, the net normal
;;; force from the plane on the compound is the sum of the normal forces from 
;;; the plane on each of the blocks; similarly for kinetic friction on the 
;;; compound. However:
;;; -In the case of weight, Wc = m1 * g + m2 * g simplifies to the simple 
;;; weight law applied to the compound Wc = mc * g where mc = m1 + m2
;;; so we can also get it this way (this is one advantage of compounding.)
;;; -Applied or tension force will typically act at a point on only one of 
;;; the parts so there can only be one such force on the compound. 
;;; Thus the sum rule will really only be needed for normal and friction forces
;;; from a surface.  But if there is more than one unknown force of this type
;;; then the problem is not much simplified by treating the objects as a 
;;; compound body anyway.  So we could just ignore this case and only write 
;;; a rule to handle the case where there is a single force of the given type. 
;;; Still it doesn't seem to hurt to write the general rule.
;;;
;;; Note this means there are now two operators to draw the weight force on 
;;; a compound, one using the weight operator and the other using the
;;; force on compound operator . There are also two corresponding 
;;; equations for the magnitude of the weight on the compound. We could
;;; filter in the force-compound rule to exclude weight, but since this is 
;;; physically correct, we leave it.

;; draw a force on a compound body
(defoperator draw-force-compound (?bodies ?agent ?type ?t)
  ;; if a force on a part of the compound due to a exists,
  ;; force in same direction exists on compound from a.
  :preconditions (
    (in-wm (object (compound orderless . ?bodies)))
    (bind ?c `(compound orderless ,@?bodies)) ; just shorthand
    ;; pick any body in compound
    (any-member ?b ?bodies)
    ;; find an external force on the body = one with agent not in compound.
    (force ?b ?agent ?type ?t ?dir ?whatever-action)
    (test (not (member ?agent ?bodies)))
    ;; make sure this force cannot be defined in another manner
    (setof (force ?c ?agent ?type ?t ?dir ?action) ?action ?action-list)
    (test (null ?action-list))
    ;;
    (bind ?mag-var (format-sym "F~A_~A_~A~@[_~A~]" ?type (body-name ?c) 
			       (body-name ?agent) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "drawing ~A ~A force on ~A due to ~A~%" ?dir ?type ?c ?agent)
  )
  :effects 
  (
   (vector (compound orderless . ?bodies) 
	   (force (compound orderless . ?bodies) ?agent ?type :time ?t) ?dir)
   (variable ?mag-var (mag (force ?c ?agent ?type :time ?t)))
   (variable ?dir-var (dir (force ?c ?agent ?type :time ?t)))
   (given (dir (force ?c ?agent ?type :time ?t)) ?dir)
   )
  :hint ;; already received the hint "try drawing a force for the compound ..."
  ((point (string "Notice that ~a, which is a part of the compound body, has a ~a force on it."
		  (?b def-np) (?type adjective)))
   (teach (string "When a force exists on a part of a compound and the force is due to an object outside the compound, then a similar force acts on the compound body itself."))
   (bottom-out (string "Draw ~a at ~a." 
		       ((force ?c ?agent ?type :time ?t) indef-np) (?dir adj)))
   ))


(defoperator force-compound-contains (?sought)
   :preconditions (
    (any-member ?sought ( (mag (force ?b ?agent ?type :time ?t)) ))
    (object (compound orderless . ?bodies))
    (test (or (member ?b ?bodies :test #'equal)
	      (equal ?b `(compound orderless ,@?bodies))))
    (test (not (member ?agent ?bodies)))
   )
   :effects (
    (eqn-contains (force-compound ?type ?agent ?bodies ?t) ?sought)
   ))

(defoperator write-force-compound (?type ?agent ?bodies ?t)
   :preconditions (
     (bind ?c `(compound orderless ,@?bodies)) ; for shorthand
     ; draw net force of given type on compound, will use draw-force-compound
     (debug "write-force-compound: drawing ~A force on compound~%" ?type)
     (vector ?c (force ?c ?agent ?type :time ?t) ?dir) 
     ;; find set of atomic parts subject to this type force from agent.
     ;; tricky: we use setof inside the map since force subgoal can fail
     ;; body-set from each step of map is a singleton (b) or NIL 
     ;; body-sets is then a list of these results which must be flattened
     (debug "write-force-compound: finding parts subject to ~A force from ~A~%" ?type ?agent)
     (map ?b ?bodies
         (setof (force ?b ?agent ?type ?t . ?dont-care) ?b ?body-set)
	?body-set ?body-sets)
     (bind ?parts-affected (remove NIL (flatten1 ?body-sets)))
     (debug "write-force-compound: parts contributing: ~A~%" ?parts-affected)
     ;; define variables for each of the force parts making up the net
     ;; unfortunately this can only be done by drawing them, although we
     ;; avoided drawing them when finding the force.
     (map ?b1 ?parts-affected
         (variable ?f-part (mag (force ?b1 ?agent ?type :time ?t)))
	 ?f-part ?f-parts)
     (variable ?f-compound (mag (force (compound orderless . ?bodies) ?agent ?type 
				       :time ?t)))
   )
   :effects ( (eqn (= ?f-compound (+ . ?f-parts))
		  (force-compound ?type ?agent ?bodies ?t)) )
   :hint
   ((point (string "What is the relationship between the magnitudes of the ~a force on the compound body and the ~a force(s) on ~a?" (?type adjective) (?type adjective) (?parts-affected conjoined-defnp)))
    (teach (string "Even though a force on a compound body corresponds directly with the force(s) on its part(s), you need to write an equation relating the magnitudes of the variables, because the variables refer to different bodies and thus denote different quantities."))
    (bottom-out (string "Write ~a" ((= ?f-compound (+ . ?f-parts)) algebra)))
    ))

;; collect directions of forces acting on compound
(defoperator calculate-net-force-dirs-compound (?b ?t)
  :preconditions 
  (
   (any-member ?b ((compound orderless . ?bodies)))
   (map ?body ?bodies
	;; For a compound body, we want to exclude any internal forces.
	;; Thus, we exclude any force whose agent is also in the compound.
	(net-force-dir ?body ?t ?dir :no-agent ?bodies) ?dir ?dirs)
   (bind ?net-dir (if (= (length ?dirs) 1) (first ?dirs) 'unknown))
   )
  :effects ((net-force-dir ?b ?t ?net-dir)))


;; find all forces that are acting on ?b (without drawing them)
;; and collect all distinct directions.
(defoperator calculate-net-force-dir-from-forces (?b ?t)
  :preconditions 
  (
   (test (not (compound-bodyp ?b)))
   ;; find list distinct of agent-direction pairs
   (setof (force ?b ?agent ?type ?t ?dir ?action) (?agent ?dir) ?ad)
   (not (unknown-forces :time ?t ?t))
   ;; remove any pair where the agent is on the list of excluded agents
   (bind ?ad-ok (remove-duplicates
		 (remove-if #'(lambda (a) (member a ?agents)) 
			    ?ad :key #'car)
		 :key #'second :test #'exactly-equal))
   ;; if all forces acting on ?b have same direction, return direction
   (bind ?net-dir (if (= (length ?ad-ok) 1) (second (car ?ad-ok)) 'unknown))
   )
:effects ((net-force-dir ?b ?t ?net-dir :no-agent ?agents)))

(defoperator calculate-net-force-dir-unknown (?b ?t)
  :preconditions ((unknown-forces :time ?t ?t))
  :effects ((net-force-dir ?b ?t unknown)))

;; Here we draw the net force in the same direction as the known acceleration
;; direction. 
;;
;; A net-force form problem might in principle give the net force mag and 
;; direction and ask for some kinematic property, but we don't currently 
;; have any problems of this form; this would require another operator and
;; possibly something to determine accel dir as well if not given by motion.
(defoperator draw-net-force-from-accel (?b ?t)
  :preconditions 
  (
   ;; non-zero acceleration vector drawn with known direction
   (vector ?b (accel ?b :time ?t) ?dir-accel)
   (test (not (or (eq ?dir-accel 'zero) (eq ?dir-accel 'unknown))))
   ;; can't determine direction from forces
   (net-force-dir ?b ?t unknown)
   ;;
   (not (vector ?b (net-force ?b :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "Fnet_~A~@[_~A~]" 
			      (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (debug "~&Drawing ~a net force for ~a at ~a.~%" ?dir-accel ?b ?t)
    )
  :effects (
    (vector ?b (net-force ?b :time ?t) ?dir-accel)
    (variable ?mag-var (mag (net-force ?b :time ?t)))
    (variable ?dir-var (dir (net-force ?b :time ?t)))
    (given (dir (net-force ?b :time ?t)) ?dir-accel)
  )
  :hint (
    (bottom-out (string "Draw the net force in the same direction as the acceleration."))
  ))

(defoperator draw-net-force-zero (?b ?t)
  :preconditions 
  (
   ;; directly from motion statements so we don't *have* to also draw
   ;; the acceleration vector
   (motion ?b ?type :accel ?adir :time ?t-motion . ?whatever)
   (test (or (eq ?type 'at-rest) (eq ?adir 'zero)))
   (time ?t)
   (test (tinsidep ?t ?t-motion))
   ;; Sanity test:  if a definite direction has been found, then 
   ;; it is likely that a force is missing.
   (net-force-dir ?b ?t ?net-force-dir)
   (test (or (eq ?net-force-dir 'unknown) 
	     (error "draw-net-force-zero error:  also found definite direction ~A for net force." 
		    ?net-force-dir)))
   ;;  make sure it is not given directly
   (not (net-force-zero ?b :time ?t-given) (tinsidep ?t ?t-given))
   ;;
   (not (vector ?b (net-force ?b :time ?t) ?dont-care))
   (bind ?mag-var (format-sym "Fnet_~A~@[_~A~]" 
			      (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (bind ?rest-string (if (eq ?type 'at-rest) "at rest" "not accelerating"))
   (debug "~&Drawing zero net force for ~a at ~a.~%" ?b ?t)
    )
  :effects (
    (vector ?b (net-force ?b :time ?t) zero)
    (variable ?mag-var (mag (net-force ?b :time ?t)))
    (given (mag (net-force ?b :time ?t)) (dnum 0 |N|))
  )
  :hint 
  ((point (string "Notice that ~a is ~A ~a.  What does this imply about the net force acting on ~A?" 
		  ?b (?rest-string identity) (?t pp) ?b))
   (bottom-out (string "Draw a zero-length net force vector acting on ~A ~A." 
		       ?b (?t pp)))
   ))

(defoperator draw-net-force-given-zero (?b ?t)
  :preconditions 
  (
   (net-force-zero ?b :time ?t-given)
   (time ?t)
   (test (tinsidep ?t ?t-given))
   (bind ?mag-var (format-sym "Fnet_~A~@[_~A~]" 
			      (body-name ?b) (time-abbrev ?t)))
    )
  :effects (
    (vector ?b (net-force ?b :time ?t) zero)
    (variable ?mag-var (mag (net-force ?b :time ?t)))
    (given (mag (net-force ?b :time ?t)) (dnum 0 |N|))
  )
  :hint 
  ((point (string "What do you know about the total force acting on ~A ~A?" 
		  ?b (?t pp)))
   (teach (string "If all of the forces acting on an object balance out, the total (net) force acting on that object is zero."))
   (bottom-out (string "Draw a zero-length net force vector acting on ~A ~A." 
		       ?b (?t pp)))
   ))

(defoperator draw-net-force-from-forces (?b ?t)
  :preconditions 
  (
   (object ?b)
   (time ?t)
   (net-force-dir ?b ?t ?net-dir)
   (test (not (eq ?net-dir 'unknown)))
   ;; make sure net-force has not already been drawn
   (not (vector ?b (net-force ?b :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "Fnet_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
     (debug "~&Drawing ~a net force for ~a at ~a.~%" ?net-dir ?b ?t)
     )
  :effects 
  (
   (vector ?b (net-force ?b :time ?t) ?net-dir)
   (variable ?mag-var (mag (net-force ?b :time ?t)))
   (variable ?dir-var (dir (net-force ?b :time ?t)))
   (given (dir (net-force ?b :time ?t)) ?net-dir)
   )
  :hint
  ((point (string "Since the forces acting on ~a ~a are all pointing in the same direction, the direction of the total force is known." 
		  ?b (?t pp)))
   (bottom-out (string "Draw a non-zero net force vector for ~A ~A at ~A." 
		       ?b (?t pp) (?net-dir adj)))))

(defoperator draw-net-force-unknown (?b ?t)
  :preconditions
  (
   (net-force-dir ?b ?t unknown)
   ;; make sure it is not given in the acceleration
   (setof (vector ?b (accel ?b :time ?t) ?a-dir) ?a-dir ?a-dirs)
   (test (or (null ?a-dirs) (member 'unknown ?a-dirs)))
   ;; make sure net-force has not already been drawn
   (not (vector ?b (net-force ?b :time ?t) ?any-dir))
   (bind ?mag-var (format-sym "Fnet_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (debug "~&Drawing unknown net force for ~a at ~a.~%" ?b ?t)
   )
  :effects
  ((vector ?b (net-force ?b :time ?t) unknown)
   (variable ?mag-var (mag (net-force ?b :time ?t)))
   (variable ?dir-var (dir (net-force ?b :time ?t))))
  :hint
  ((point (string "Notice that there are forces acting on ~a ~a, although the exact direction of the total force is unknown." ?b (?t pp)))
   (bottom-out (string "Draw a non-zero net force vector for ~A ~A in an approximately correct direction, then erase the number in the direction box to indicate that the exact direction is unknown." ?b (?t pp)))))

 
;;; ========================  draw all forces  ================================
;;;
;;; This operator represents the procedure for drawing all forces on a
;;; given body.  The procedure is simply to draw each of them.  Duh.
;;; Because (forces ?b ?t ?forces) can be a top level goal, it must
;;; first get the time and body selected.  Can't put these inside
;;; (in-wm ...).

;;; Because this operator is so simple, I'm not sure what the hints
;;; should be for it.  After Andes has hinted the goal with e.g., "You
;;; should draw forces", then its next hint should be for a specific
;;; force.  So I guess this operator shouldn't have any hints.


(defoperator draw-any-forces (?b ?t)
   :specifications 
    "If there are some forces on ?body at ?time,
     then make them the set of forces on ?body at ?time"
   :preconditions
   ( 
    (time ?t)
    ;; If the time covers more than two consecutive points, then we
    ;; cannot be assured that there are no forces defined for a subset. 
    (test (or (time-pointp ?t) (time-consecutivep ?t)))
    (inherit-or-quantity (body ?b :time ?t) (body ?b :time ?tt))
    (body ?b :time ?tt)
    (not (unknown-forces :time ?t ?t)) ;can't collect forces if some are unknown
    ;; list of forces acting on entire body (particle)
    (setof (inherit-proposition (force ?b ?agent ?type :time ?t)
				 (force ?b . ?rest) 
				 (vector ?b (force ?b . ?rest) ?dir))
	   (force ?b . ?rest)
	   ?body-forces)
     ;; list of forces acting on points on extended body
    (setof (force-on-point ?b ?force) ?force ?point-forces)
    ;; only one case should apply
     (test (or (null ?body-forces) (null ?point-forces)
	       (error "draw-forces error: can't act both on entire body and on point of body:~%    ~S~%    ~S~%"
		      ?body-forces ?point-forces)))
     (bind ?forces (or ?body-forces ?point-forces))
     (debug "Finish draw-forces for ?b=~A ?t=~A:~%~{     ~S~%~}"
	    ?b ?t ?forces)
     )
   :effects
   ((any-forces ?b ?t ?forces))) 

;; wrapper for draw-any-forces to make sure no special 
;; work agents have been defined

(defoperator draw-forces (?b ?t)
  :preconditions 
  ( (not (does-work-on ?work-agent ?b :time ?t-work) (tinsidep ?t ?t-work))
    (any-forces ?b ?t ?forces))
  :effects ((forces ?b ?t ?forces)))

;; this is used by draw-forces to collect all forces acting on
;; points on body, draws each force.
(defoperator collect-forces-on-points (?pt ?agent ?type ?t)
  :preconditions (
		  (point-on-body ?pt ?b)
		  ;; in some cases the axis owner is ?pt and others ?b
		  (vector ?pt-or-b (force ?pt ?agent ?type :time ?t) ?dir)
		  )
  :effects ((force-on-point ?b (force ?pt ?agent ?type :time ?t))))

;;; The "num-forces" scalar equation exists to put out an equation answering 
;;; the question "how many forces on ?b at ?t". This will likely be one part of
;;; a larger Newton's law problem though it could be asked all alone.

(defoperator num-forces-contains (?b ?t)
  :preconditions ()
  :effects ( (eqn-contains (num-forces ?b ?t) (num-forces ?b :time ?t)) ))

(defoperator write-num-forces (?b ?t)
  :preconditions 
   ((forces ?b ?t ?forces)
    (bind ?count (length ?forces))
    (variable ?n-var (num-forces ?b :time ?t)) )
  :effects 
  ( (eqn (= ?n-var ?count) (num-forces ?b ?t)) ))
 
(defoperator define-num-forces (?b ?t)
   :preconditions 
   ((bind ?n-var (format-sym "nforces_~A~@[_~A~]" (body-name ?b) 
			     (time-abbrev ?t))))
   :effects ( 
       (define-var (num-forces ?b :time ?t)) 
       (variable ?n-var (num-forces ?b :time ?t)) 
   ))



;;; The following draws a standard free-body-diagram, for qualititative 
;;; problems that ask for fbd's only.  This shows a body and all forces on it.
;;; It differs from the "NL" vector diagram preparatory to applying 
;;; Newton's laws in that NL diagram also includes acceleration and axes.
;;; we make axes optional here.
(defoperator draw-standard-fbd (?b ?t)
  :preconditions 
  (
   (inherit-or-quantity (body ?b :time ?t) (body ?b :time ?tt))
   (body ?b :time ?tt)
   (forces ?b ?t ?forces)
   ;; axes are optional, but still want to allow any valid rotation
   ;; Our 'optional' statement requires a fully bound goal forms, for
   ;; case where step is skipped and goal is just posted to wm.
   ;; So we can't use unbound axis rotation inside optional; instead, 
   ;; have to break out call to another operator.
   (optional (fbd-axes-drawn ?b)))
  :effects ((fbd ?b ?t)))

(defoperator choose-axes-for-fbd (?b)
  :effects ( (fbd-axes-drawn ?b) )
  ;; choose any legal rotation:
  :preconditions ( (axes-for ?b ?x-rot) ))


;;;                  Net Force

(defoperator net-force-vector-contains (?sought)
  :preconditions 
    ((any-member ?sought (
		 (net-force ?b :time ?t)
		 (force ?b ?agent ?type :time ?t)))
     (object ?b)
     (not (unknown-forces :time ?t ?t)) ;also checked in draw-forces
    )  
  :effects 
  ((eqn-family-contains (net-force ?b ?t) ?sought)
  ;; since only one compo-eqn under this vector PSM, we can just
  ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (net-force ?b ?t) definition ?sought)))

(defoperator draw-net-force-diagram (?rot ?b ?t)
  :preconditions
  ((debug "start draw-net-force-diagram at ~A~%" ?rot)
   (not (vector-diagram ?rot (net-force ?b ?t)))
   (forces ?b ?t ?forces)
   (test ?forces)	; fail if no forces could be found
   (vector ?b (net-force ?b :time ?t) ?net-force-dir)
   (axes-for ?b ?rot)
   (debug "finish draw-net-force-diagram at ~A for ?b=~A ?t=~A:~%~{     ~S~%~}" 
	  ?rot ?b ?t ?forces))
  :effects ((vector-diagram ?rot (net-force ?b ?t))))

(defoperator write-net-force-compo (?b ?t ?xyz ?rot)
  :preconditions 
  (
   (debug "start write-net-force-compo at ~A~%" ?rot)
   (in-wm (forces ?b ?t ?forces))	;found in vector-diagram
   ;; for each force on b at t, define a component variable, 
   ;; collecting variable names into ?f-compo-vars
   (map ?f ?forces 
	(inherit-variable ?f-compo-var (compo ?xyz ?rot ?f))
	?f-compo-var ?f-compo-vars)
   (variable ?fnet_xy (compo ?xyz ?rot (net-force ?b :time ?t)))
   (debug "finish write-net-force-compo~%")
   )
  :effects 
  ((eqn (= (+ . ?f-compo-vars) ?fnet_xy)
	(compo-eqn definition ?xyz ?rot (net-force ?b ?t)))
   ;; Also, don't use net force definition and explicit forces version of NL
   (assume using-NL net ?b ?t)
   )
  :hint
  ((point (string "What is the total force acting on ~A ~A." 
		  (?b def-np) (?t pp)))
   (teach (string "The net force on an object is the vector sum of all forces acting on the object."))
   (bottom-out (string "Write the equation for net force along the ~A axis as ~A" ((axis ?xyz ?rot) symbols-label) ((= (+ . ?f-compo-vars) ?fnet_xy) algebra)))
   ))

;;; ==================== The gravitational force ==============================

;;; This operator and the next represent writing W=m*g.  Because
;;; weight is treated as a magnitude in this equation, it is a scalar
;;; equation and not a vector equation.  Thus, students are not required
;;; to draw some kind of vector diagram.  However, because the Andes tools
;;; currently force students to draw a weight vector in order to define a
;;; variable for the magnitude of a weight, the students will still be
;;; required to draw at least that one vector.
;;; 
;;; Like all scalar equations,
;;; we use one operator (this one) to represent knowledge of what quantities 
;;; might be in the equation if it is written.  In this case, all three 
;;; quantities (w, m and g) are certain to be in the equation, but things 
;;; are not so certain for other equations.  At any rate, the second operator 
;;; does the actual process of preparing to write the equation then writing it.
   
(defoperator wt-law-contains (?quantity)
  :specifications "
   If a body is near a planet,
   then the weight law for the body potentially contains
     the magnitude of the weight force,
     the mass of the body, and
     the gravitational constant for the planet."
  :preconditions
  (   
   (time ?t)
   (object ?b)
   (any-member ?quantity
	       ((mag (force ?b ?planet weight :time ?t))
		(mass ?b :time ?t) 
		(gravitational-acceleration ?planet)
		))
   ;; make sure this is not case where ?b is cm of rigid body. For that
   ;; we need the mass of the whole body, plus special hint.
   (not (point-on-body ?b ?rigid-body))
   (near-planet ?planet :body ?b ?b)
   (not (massless ?b))
   ;; Determine what time to use when labeling equation.
   ;; otherwise, one gets several identical equations, with different labels.
   (inherit-or-quantity (force ?b ?planet weight :time ?t) 
			(force ?b ?planet weight :time ?t))
   ) 
  :effects
  ((eqn-contains (wt-law ?b ?t) ?quantity)))
  
;;; This operator models writing the W=m*g equation.  The variable for
;;; the relevant quantities will be either defined either via
;;; subgoaling or recalled if they have already been defined.
;;; 
;;; Current Andes predefines g in problems that use it but does not provide 
;;; a way for the student to define a variable for gravitational acceleration.  
;;; One possibility would be to represent g as a parameter.  Parameters are 
;;; symbolic quantities that are given in the problem statement.  If a 
;;; parameter appears in an equation, the code in the top level section knows 
;;; not to create a subgoal for it.  In fact, it will not be returned by 
;;; vars-in-eqns because a parameter is not a variable.
;;;
;;; However, for consistency we now treat g as any other variable and will 
;;; have to add a means to the interface to define it or to specify predefined
;;; variables in the problem statement somehow.

;; Ideally, weight force would be timeless; as a work-around,
;; choose largest possible time interval Bug #999.

(defoperator largest-time-interval (?t)
  :preconditions 
  (
   ;; get list of all time intervals
   (setof (time (during ?t1 ?t2)) (during ?t1 ?t2) ?tlist)
   (time ?t)  ;select a time
   ;; make sure the selected time is not included in any time interval
   (test (notany #'(lambda (tt) (tinsidep-include-endpoints ?t tt)) 
		(remove ?t ?tlist :test #'equal)))
   )
  :effects ((largest-time ?t)))

(defoperator inherit-weight-force (?b ?t-child ?planet)
  :preconditions 
  (
   (largest-time ?t)
   (test (not (member 'changing-mass (problem-features *cp*))))
   (time ?t-child)
   (test (tinsidep ?t-child ?t))
   (test (not (equal ?t-child ?t)))
   )
  :effects ((inherit-quantity (force ?b ?planet weight :time ?t-child)
				(force ?b ?planet weight :time ?t))))

(defoperator wt-law (?b ?t)
  :specifications "
   If a body is near a planet,
     and it is not massless,
     and you can find the appropriate variables,
   then write W=m*g where W is the magnitude of the weight force
     on the body, m is the body's mass and g is the gravitational
     acceleration of the planet."
  :preconditions
   ((in-wm (near-planet ?planet :body ?b ?b))
    (inherit-variable ?m-var (mass ?b :time ?t))
    (inherit-variable ?w-var (mag (force ?b ?planet weight :time ?t)))
    (variable ?g-var (gravitational-acceleration ?planet))
    )
  :effects
   ((eqn (= ?w-var (* ?m-var ?g-var)) (wt-law ?b ?t)))
  :hint
  ((point (string "Try applying the weight law."))
   (teach 
       (kcd "write_w_is_mg")
       (string "The weight law for a body is W = m*g, where W is the magnitude of the weight force acting on the body, m is the body's mass and g is the gravitational acceleration at the surface of the planet."))
   (bottom-out (string "Write ~a = ~a*~a" (?w-var algebra) 
		       (?m-var algebra) (?g-var algebra)))))

;; variant applies weight to a rigid body. In this case the quantity is
;; specified as a force acting on the cm, not on the whole body.
(defoperator wt-law-cm-contains (?quantity)
  :specifications "
   If a rigid body is near a planet,
   then the weight law for the body potentially contains
     the magnitude of the weight force acting at the cm,
     the mass of the body, and
     the gravitational constant for the planet."
  :preconditions
  (
   ;; only apply this in case where ?cm is declared cm of a single rigid body. 
   (in-wm (center-of-mass ?cm (?rigid-body)))
   (any-member ?quantity
	        ((mag (force ?cm ?planet weight :time ?t))
		 (mass ?rigid-body)
		 (gravitational-acceleration ?planet)))
   (time ?t)
   (near-planet ?planet :body ?rigid-body ?rigid-body)
   (not (massless ?rigid-body))) 
  :effects
  ((eqn-contains (wt-law ?rigid-body ?t) ?quantity)))

(defoperator wt-law-cm (?b ?t)
  :preconditions
   ((in-wm (center-of-mass ?cm (?b)))
    (variable ?m-var (mass ?b))
    (variable ?w-var (mag (force ?cm ?planet weight :time ?t)))
    (variable ?g-var (gravitational-acceleration ?planet))
    )
  :effects
   ((eqn (= ?w-var (* ?m-var ?g-var)) (wt-law ?b ?t)))
  :hint
  ((point (string "Try applying the weight law."))
   (teach 
       (string "The weight law for a body is W=m*g, where W is the magnitude of the weight force acting on the body, m is the body's mass and g is the gravitational acceleration of earth or whatever planet is nearby."))
   (bottom-out (string "Write ~a=~a*~a" (?w-var algebra) (?m-var algebra) (?g-var algebra)))))

;;; This operator models writing the Fs = k * compression/extension equation.  
;;; Selectively enabled by (uses-k-and-d) in the problem statement. ??? Unnecessary ? -AW
	       
(def-PSMclass spring-law (spring-law ?body ?time)
  :complexity minor
  :short-name "Hooke's law"
  :english ("Hooke's law")
  :expformat ("applying Hooke's law to ~a " (nlg ?body))
  :EqnFormat ("F = k*d" ))

(defoperator spring-law-contains (?quantity)
  :specifications "
   the spring law for the body potentially contains
     the magnitude of the spring force,the spring constant, and
     the compression/extension."
  :preconditions (
		  (any-member ?quantity 
			      ((mag (force ?b ?spring spring :time ?t))
			       (spring-constant ?spring)
			       (compression ?spring :time ?t)
			       ))
		  (object ?b)
		  (time ?t)
  		  (uses-k-and-d)	;Forces rule to fire only if k and d 
					; are needed to solve problem	
		  ) 
  :effects (
	    (eqn-contains (spring-law ?b ?t) ?quantity))
  )

(defoperator spring-law-compression (?b ?t)
  :specifications "
     and you can find the appropriate variables,
     then write Fs=k*d where Fs is the magnitude of the spring force
     on the body, k is the spring constant and d is the compression or
     extension distance of the spring."
  :preconditions(
                 (variable ?s-var (mag (force ?b ?spring spring :time ?t)))
                 (variable ?k-var (spring-constant ?spring))
                 (variable ?d-var (compression ?spring :time ?t))
                 )
  :effects (
            (eqn (= ?s-var (* ?k-var ?d-var)) (spring-law ?b ?t)))
  :hint
  ( (point (string "Apply Hooke's law to ~A." ?b))
    (teach 
     (string "Use ~A as the spring constant and ~A as the compression (or stretch) of the spring." ?k-var ?d-var))
    (bottom-out (string "Write the equation ~A" 
			((= ?s-var (* ?k-var ?d-var)) algebra) )) 
    ))


;;;; ========================== Newton's law ================================ 

;;; NL is newton's second law.  It is represented by several operators.  
;;; 
;;; The first one indicates what quantities might be contained in the
;;; equation if it should be written.  Note that the quantities are
;;; not guaranteed to be contained in the ultimate equation.  For
;;; instance, if the body is not accelerating, then the variables for
;;; mass and acceleration will not be in the equation.  When the
;;; solver unifies with one of the quantities in the list, some
;;; variables in the vector equation's identifier are bound.  For NL,
;;; the body and time are always bound.
;;; 
;;; The other operators expects to get a vector equation identifier
;;; that has some arguments bound, and they do all the work to write
;;; the equation.  So the vector equation identifier is a way of
;;; passing info (e.g., the body and the time) from the sought
;;; quantity to the hard working operator.
;;; 
;;; I'm not sure if hints on this operator will ever be used, but if
;;; so, it would be better to have the sought quantity available, as
;;; that would make the hint message nicer.
;;;


(defoperator NL-vector-contains (?quantity)
  :specifications 
  "Newton's law potentially contains the body's mass, 
     the magnitude of its acceleration, and
     the direction of its acceleration"
  :preconditions 
  ((any-member ?quantity
	       ;; assume any mass change described by thrust force
	       ((mass ?b :time ?t) 
		(accel ?b :time ?t)
		(force ?b ?agent ?type :time ?t)))
   (object ?b) ;; sanity check
   (time ?t))
  :effects ((eqn-family-contains (NL ?b ?t) ?quantity)))

;; version for rigid bodies
(defoperator NL-vector-point-contains (?Quantity)
  :preconditions 
  ( (any-member ?quantity
		;; in principle, we could also have net force here?
		((force ?point ?agent ?type :time ?t)))
    (point-on-body ?point ?b)
    (object ?b) ;sanity check (not really needed)
    (time ?t))
  :effects ((eqn-family-contains (NL ?b ?t) ?quantity)))

(defoperator NL-net-vector-contains (?quantity)
  :specifications 
  "Newton's law potentially contains the body's mass, 
     the magnitude of its acceleration, and
     the direction of its acceleration"
  :preconditions 
  ((any-member ?quantity ((mass ?b :time ?t) 
			  (accel ?b :time ?t)
			  (net-force ?b :time ?t)
			  ))
   (not (point-on-body ?b ?bb)) ;make sure this is not just a part
   (object ?b) ;sanity check
   (time ?t))
  :effects ((eqn-family-contains (NL ?b ?t :net t) ?quantity)))

;; This operator draws a free-body diagram consisting of the forces,
;;; acceleration and axes. Unlike draw-fbd-lk (linear kinematics), it
;;; doesn't draw velocity and displacement.  This is an unordered And
;;; operator.

;;; Unfortunately, the Andes vector drawing tools will add component
;;; variables if the axes are already drawn.  The operators for vector
;;; drawing don't do this.  Only the axis drawing operators define
;;; component variables.  Thus, we must insure that axis goal is posed
;;; *after* all the vectors are drawn.  Even though this operator is
;;; unordered in that it doesn't force the student to do the
;;; conditions in the specified order, the interpreter must achieve
;;; those conditions in the specified order for the code to work.

;;; In the last condition, only the x axis is requested. Drawing it
;;; causes the other axes to be draw as well.

;;; there are two mutually exclusive operators depending on whether
;;; net force is to be shown or not. We only show net force if the
;;; problem explicitly mentions it (i.e. seeks it.)

(def-goalprop nl-fbd (vector-diagram ?rot (nl ?body ?time))
  :doc "free-body-diagram for applying Newton's law"
  :english ("drawing a free-body diagram for ~A ~A"
            (nlg ?body) (nlg ?time 'pp))) ; time may be interval or instant

(defoperator draw-nl-fbd (?rot ?b ?t)
  :specifications 
   "If the goal is to draw a fbd for newton's law,
   then draw a body, draw the forces, the acceleration and the axes,
   in any order."
  :preconditions
  (
   (forces ?b ?t ?forces) 
   (test ?forces)	;fail if no forces could be found
   (vector ?b (accel ?b :time ?t) ?accel-dir)
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (NL ?b ?t)))
  :hint
   ((bottom-out (string "In order to draw a free-body diagram, which is the first step to applying Newton's law, draw (1) a body, (2) the forces on the body, (3) the acceleration of the body, and (4) coordinate axes."))))

;; 
;; Following draws a free-body diagram for the net-force variant of NL
;;
(defoperator draw-NL-net-fbd (?rot ?b ?t)
  :specifications 
   "If the goal is to draw a fbd for newton's law in terms of net force,
   then draw a body, draw the acceleration, draw the net force vector and the axes,
   in any order."
  :preconditions
  (
   (debug "start draw-NL-net-fbd~%")
   (test ?netp)
   (body ?b)
   (vector ?b (accel ?b :time ?t) ?accel-dir)
   (vector ?b (net-force ?b :time ?t) ?force-dir) 
   (axes-for ?b ?rot)
   (debug "finish draw-NL-net-fbd~%")
   )
  :effects
   ((vector-diagram ?rot (NL ?b ?t :net ?netp)))
  :hint
   ((bottom-out (string "In order to draw a free-body diagram when working in terms of Net force, draw (1) a body, (2) the acceleration of the body (3) the net force on the body, and (4) coordinate axes."))))

;;; The work of writing NL is divided into drawing the fbd, selecting
;;; an NL equation and writing the NL in component form.  An operator
;;; defined earlier draws the fbd.  These operators just select a
;;; component equation.  There are just two to select from: Newton's
;;; second law for zero (NFL) and nonzero (NSL) acceleration.  
  
(defoperator NFL-zero-accel (?quantity)
  :specifications "
   If the object has zero acceleration over a time period,
      and that time period includes the one we're useing for NL,
   then NFL applies and it potentially contains
     the magnitude and direction of any force acting on the body"
  :preconditions 
  ((any-member ?quantity ((force ?b ?agent ?type :time ?t)))
   (object ?b)
   (in-wm (vector ?b (accel ?b :time ?t-accel) zero)) ;done in drawing step
   (test (tinsidep ?t ?t-accel)))
  :effects ((compo-eqn-contains (NL ?b ?t) NFL ?quantity)))

;; variation for points on body
(defoperator NFL-zero-accel-point (?quantity)
  :preconditions 
  ((any-member ?quantity ((force ?pt ?agent ?type :time ?t)))
   (point-on-body ?pt ?b)
   (object ?b)  ;sanity check (not really needed)
   (in-wm (vector ?b (accel ?b :time ?t-accel) zero)) ;done in drawing step
   (test (tinsidep ?t ?t-accel)))
  :effects ((compo-eqn-contains (NL ?b ?t) NFL ?quantity)))

;; The case NFL-net, or Fnet=0, never occurs.  The choice NFL vs. NSL is made by
;; testing if the acceleration vector is known to be zero.  If the acceleration
;; is known to be zero, then the drawing rules for net force should 
;; also draw the net force as a zero-length vector.  The vector PSM routine 
;; SELECT-COMPO-EQN-FOR-VECTOR finds that the equation Fnet=0 has no non-zero 
;; component and quits.

;;; This operator indicates when Newton's second law (NSL) is
;;; applicable.  It should be applicable exactly when NFL is not applicable.
;;; That could be easily expressed if we could have goals with priorities,
;;; but we don't right now.  So we write out the condition: If an object is 
;;; massless or has zero acceleration during a time period containing 
;;; the target time, then NSL is prevented from applying. This makes use
;;; of the optional second test clause in the "not" condition, in case
;;; the time of zero acceleration might be wider than the target time.
;;;
;;; Also, to handle cases where a problem describes an acceleration 
;;; but does not determine the forces causing it, we test for the
;;; statement "unknown-forces" to block the attempt to apply Newton's law.
;;;
;;; Time may be an interval or an instant. In case of interval, we
;;; make sure endpoints are consecutive, to avoid applying this accross
;;; two sub-segments with different forces. [necessary? Could also 
;;; fail to determine forces applying over this composite segment]

(defoperator NSL (?quantity)
  :specifications "
   If the acceleration is not zero during the target time period
      and the body is not massless during the target time period,
   then NSL applies and it potentially contains
     the mass of the body,
     the magnitude and direction of its acceleration"
  :preconditions 
  (
   (debug "start  NSL~%")
   (not (vector ?b (accel ?b :time ?t) zero))
   (not (massless ?b))
   (debug "finish  NSL~%")
   )
  :effects
   ((compo-eqn-contains (NL ?b ?t :net ?netp) NSL ?quantity))
 )
 
;;; This operator writes newton's first law in component form for all
;;; forces.  This operator expects to get the body, time, axis label,
;;; axis rotation via the equation identifier in the effects.  It
;;; fetches the set of forces from working memory and the component
;;; variables for all forces.  It writes the equations and also leaves
;;; behind a proposition indicating which component variables are in
;;; the equation.  For now, the hints repeat ones given by the
;;; equation selection operator, as I don't know which one will be
;;; targetted by the help system yet.

(defoperator write-NFL-compo (?b ?t ?xyz ?rot)
  :specifications 
   "If the goal is to write newton's first law in component form 
      for ?body at ?time along ?axis at ?rot,
      ensure there are component variables ?compo-vars for the components 
      of each of the forces on ?b at ?t,
   then write ?f1-compo + ?f2-compo + ... = 0, where 
     ?fi-compo are the forces' component variables."
  :preconditions
  ((in-wm (forces ?b ?t ?forces))
   (map ?f ?forces 
   	(inherit-variable ?compo-var (compo ?xyz ?rot ?f))
	?compo-var ?f-compo-vars)
   ;; we want Fi = m * a to be accepted if it is written. But also
   ;; need to write Sum Fi = 0 as final eqn so won't appear to contain m, a
   ;; so we make sure we have a compo var and put implicit eqn in effects.
    (variable ?a-compo (compo ?xyz ?rot (accel ?b :time ?t)))
  )
  :effects
   ((eqn (= (+ . ?f-compo-vars) 0)
	 (compo-eqn NFL ?xyz ?rot (NL ?b ?t)))
    (assume using-NL no-net ?b ?t)
    (implicit-eqn (= ?a-compo 0) 
		  (projection (compo ?xyz ?rot (accel ?b :time ?t)))))
  :hint
   ((point (string "You can apply Newton's second law to ~A.  Note that ~A is not accelerating ~A." 
		   ?b ?b (?t pp)))
    (teach (string 
    "Newton's second law F = m*a states that the net force on an object = the object's mass times its acceleration.  In this case the acceleration is zero so you know the sum of all forces on the object must be zero.  This vector principle can be applied component-wise to require that the force components in any direction sum to zero."
    ))
    (bottom-out (string "Because ~a is not accelerating ~a, write Newton's second law as ~A" 
			?b (?t pp) ((= (+ . ?f-compo-vars) 0) algebra)))))


;;; This operator writes Newton's second law in component form.  It
;;; expects to get the body, time, axis label (?xyz) and axis rotation
;;; (?rot) via the equation identifier in the effects, and it fetches
;;; the relevant vectors from wm.  It just looks up the appropriate
;;; variables and writes the equation.  It also leaves behind a
;;; proposition recording the component variables that appear in the
;;; equation.  

(defoperator write-NSL-compo (?b ?t ?xyz ?rot)
  :specifications 
   "If the goal is to write newton's second law in component form,
      ensure there are component variables ?compo-vars for the components 
      of each of the forces on ?b at ?t,
   then write ?f1c + ?f2c + ... = ?m * ?ac, where ?fic and ?ac
      are the appropriate component variables for ?fi and ?a,
      respectively."
  :preconditions
  (
   (in-wm (forces ?b ?t ?forces)) ;done in drawing step
   ;; for each force on b at t, define a component variable, 
   ;; collecting variable names into ?f-compo-vars
   (debug "write-NSL-compo(~A ~A ~A): defining force compo vars~%" ?b ?xyz ?rot)
   (map ?f ?forces 
    (inherit-variable ?f-compo-var (compo ?xyz ?rot ?f))
   	?f-compo-var ?f-compo-vars)
   (debug "write-NSL-compo: set of force compo-vars = ~A~%" ?force-compo-vars)
   (variable ?a-compo (compo ?xyz ?rot (accel ?b :time ?t)))
   ;; assume any mass change is described by thrust force
   (inherit-variable ?m (mass ?b :time ?t))
   ;; see if acceleration compo doesn't vanish
   ;; if it does, we still write equation to give sum of forces = 0
   (in-wm (vector ?b (accel ?b :time ?t) ?dir-a))
   (bind ?ma-term (if (non-zero-projectionp ?dir-a ?xyz ?rot)
		      `(* ,?m ,?a-compo) 0))
   (debug "write-NSL-compo: eqn-compo-vars = ~A~%" ?eqn-compo-vars)
   )
  :effects
   ((eqn (= (+ . ?f-compo-vars) ?ma-term)
	 (compo-eqn NSL ?xyz ?rot (NL ?b ?t)))
    (assume using-NL no-net ?b ?t)
    )
  :hint
   ((point (string "You can apply Newton's second law to ~A.  Note that ~A is accelerating ~A." 
		   ?b ?b (?t pp)))
    (teach (string "Newton's second law F = m*a states that the net force on an object = the object's mass times its acceleration.  Because the net force is the vector sum of all forces on the object, this can be applied component-wise to relate the sum of the force components in any direction to the mass times the component of acceleration in that direction."))
    (bottom-out (string "Write Newton's second law in terms of component variables along the ~A axis as ~A" 
			((axis ?xyz ?rot) symbols-label) ((= (+ . ?f-compo-vars) ?ma-term) algebra)))
    ))

;;; 
;;; This operator writes the net force version of Newton's second law in 
;;; component form.  
;;;

(defoperator write-NSL-net-compo (?b ?t ?xyz ?rot)
  :specifications 
   "If the goal is to use newton's second law for net force in component form,
      define component variables for the net force and acceleration,
   then write ?fnet_c = ?m * ?ac"
  :preconditions
  (
   (debug "start  write-NSL-net-compo~%")
   (test ?netp)
   (variable ?fnet-compo-var (compo ?xyz ?rot (net-force ?b :time ?t)))
   (variable ?a-compo (compo ?xyz ?rot (accel ?b :time ?t)))
   ;; assume any mass change is described by thrust force
   (inherit-variable ?m (mass ?b :time ?t))
    ;; see if acceleration compo doesn't vanish
   (in-wm (vector ?b (accel ?b :time ?t) ?dir-a)) ;done in drawing step
   (bind ?ma-term (if (non-zero-projectionp ?dir-a ?xyz ?rot)
		      `(* ,?m ,?a-compo) 0))
   (debug "finish write-NSL-net-compo~%")
    )
  :effects (
    (eqn (= ?fnet-compo-var ?ma-term)
	 (compo-eqn NSL ?xyz ?rot (NL ?b ?t :net ?netp)))
    (assume using-NL net ?b ?t)
  )
  :hint (
    (point (string "You can apply Newton's second law to ~A.  Note that the acceleration of ~a is non-zero ~A." 
		   ?b ?b (?t pp)))
    (teach (string "Newton's second law F = m*a states that the net force on an object = the object's mass times its acceleration. This can be applied component-wise to relate the net force in any direction to the mass times the component of acceleration in that direction."))
    (bottom-out (string "Write Newton's second law along the ~a axis in terms of component variables, namely, ~a" 
			((axis ?xyz ?rot) symbols-label) 
			((= ?fnet-compo-var ?ma-term) algebra)))
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                  Newton's third law (NTL)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In theory NTL should apply to any force at all. However, we don't declare
;;; every force agent an "object" (particle with kinematic properties), E.g. 
;;; planes or ground or Earth are not usually declared objects, so can't 
;;; actually use introduce reaction forces on these. This may have to change
;;; but then it will multiply solutions since then could draw them and write 
;;; NSL in terms of reaction force magnitudes. That would be odd but correct.
(defoperator NTL-contains (?quantity)
  :preconditions (
  (any-member ?quantity (
  		(mag (force ?b1 ?b2 ?type :time ?t))
                        ))
  ;; no need to test if action/reaction pair definable; fail later if not
  ;; (force ?b1 ?b2 ?type ?t ?dir1 action) 
  ;; (force ?b2 ?b1 ?type ?t ?dir2 reaction) 
  ;; sort body names in id so NTL(b1, b2) gets same id as NTL(b2, b1)
  (bind ?body-pair (sort (list ?b1 ?b2) #'expr<))
  )
  :effects ( 
  	(eqn-contains (NTL ?body-pair ?type ?t) ?quantity) 
  ))

(defoperator NTL (?b1 ?b2 ?type ?t)
  :preconditions (
  (variable ?mag1-var (mag (force ?b1 ?b2 ?type :time ?t)))
  (variable ?mag2-var (mag (force ?b2 ?b1 ?type :time ?t)))
  )
  :effects 
  (
   (eqn (= ?mag1-var ?mag2-var) (NTL (?b2 ?b1) ?type ?t)) 
   (assume using-NTL (?b2 ?b1) ?type ?t)
   (assume using-magnitude (NTL-vector (?b2 ?b1) ?type ?t)) ;mag xor compos
   )
  :hint
  ((point (string "What does Newton's third law tell you about the relation of ~A and ~A" 
		  (?mag1-var algebra) (?mag2-var algebra)))
   (teach 
      (kcd "third_law_PSM")
      (string "Newton's third law states that forces come in pairs: whenever A exerts a force of some type on B, B exerts a force of equal magnitude and opposite direction on A. You can use that to equate the magnitudes of this pair of forces."))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?mag1-var ?mag2-var) algebra)))
  ))


;;;
;;; Vector form of NTL writes component equation F12_x = -F21_x
;;;
;;; Note the vector equation ID for this is incompatible with convention 
;;; required by select-compo-eqn-for-scalar, according to which vector args 
;;; start with body and time.  Should be OK, since NTL doesn't contain any 
;;; scalars.
;;;

(defoperator NTL-vector-contains (?sought)
  :preconditions (
   (any-member ?sought ( (force ?b1 ?b2 ?type :time ?t)))
   (bind ?body-pair (sort (list ?b1 ?b2) #'expr<))
   )
   :effects (
   (eqn-family-contains (NTL-vector ?body-pair ?type ?t) ?sought) 
    ;; since only one compo-eqn under this vector PSM, we can just
    ;; select it now, rather than requiring further operators to do so
    (compo-eqn-contains (NTL-vector ?body-pair ?type ?t) NTL ?sought)
   ))

(defoperator draw-NTL-vector-diagram (?rot ?b1 ?b2 ?type ?t)
  :preconditions (
    ;; Draw both bodies. 
    (body ?b1)
    (body ?b2)
    (vector ?b1 (force ?b1 ?b2 ?type :time ?t) ?dir1)
    (vector ?b2 (force ?b2 ?b1 ?type :time ?t) ?dir2)
    ;; we need axis-for each body, since component defining operators will 
    ;; lookup axis-for principal body of each vector. Our operators that
    ;; draw axes only apply once, so there is no danger of drawing two
    ;; axes. In order to reuse the axes drawn for body1 as axes used
    ;; for vectors on body2, we added reuse-other-body-axis in axes section.
    (axes-for ?b1 ?rot)
    (axes-for ?b2 ?rot)
  )
  :effects (
    (vector-diagram ?rot (NTL-vector (?b1 ?b2) ?type ?t))
  ))
  
(defoperator write-NTL-vector (?b1 ?b2 ?type ?t ?xy ?rot)
   :preconditions (
      (variable ?F12_xy (compo ?xy ?rot (force ?b1 ?b2 ?type :time ?t)))
      (variable ?F21_xy (compo ?xy ?rot (force ?b2 ?b1 ?type :time ?t)))
   )
   :effects (
    (eqn (= ?F12_xy (- ?F21_xy)) (compo-eqn NTL ?xy ?rot (NTL-vector (?b1 ?b2) ?type ?t)))
    (assume using-NTL (?b1 ?b2) ?type ?t)
   )
   :hint (
     ;; !!! TODO
     (point (string "What does Newton's third law tell you about the relation of ~A and ~A" (?F12_xy algebra) (?F21_xy algebra)))
   (teach 
    (kcd "third_law_PSM")
    (string "Newton's third law states that the members of an action/reaction pair of forces are equal in magnitude and opposite in direction. This entails that the components of each force vector are the negations of the corresponding components of the other: F12_x = -F21_x and F12_y = -F21_y."))
     (bottom-out (string "Write the equation ~A" 
                         ((= ?F12_xy (- ?F21_xy)) algebra)))
   ))


;;; ====================== tensions equal  =================

;;; These two operators are for pulley systems, wherein a string connects two
;;; objects.  They assert that the magnitude fo the tension forces on
;;; the ends of a string are equal.  I'm not sure what kind of hint to
;;; give for these equation-contains operators.

(defoperator tensions-equal-contains (?quantity)
  :specifications "
   If the same string is attached to two objects,
   then the tensions-equal law can be applied,
     and it mentions the magnitudes of the tension forces on the objects."
  :preconditions
  ((any-member ?quantity
	        ((mag (force ?b1 ?string tension :time ?t))))
   ;; can apply if string is connected to another body
   (tied-to ?string ?b2 :time ?t-other :dir ?dir2)
   (test (tinsidep ?t ?t-other))
   (test (not (equal ?b2 ?b1)))
   ; sort bodies in id so we don't generate both Tb = Tb2 and Tb2 = Tb
   (bind ?bodies (sort (list ?b1 ?b2) #'expr<))
   )
  :effects
  ((eqn-contains (tensions-equal ?string ?bodies ?t) ?quantity))
  )

;;; This operator writes the simple equation t1=t2, where t1 and t2
;;; are the magnitudes of the tension forces on the two bodies
;;; connected by the string. 
;;; ?bodies should have been set in tensions-equal-contains

(defoperator write-tensions-equal (?string ?bodies ?t)
  
  :specifications 
   "If a string is tied to two blocks,
   then the tension forces on them are equal."
  :preconditions
  ((bind ?b1 (first ?bodies))
   (bind ?b2 (second ?bodies))
   (variable ?t1-var (mag (force ?b1 ?string tension :time ?t)))
   (variable ?t2-var (mag (force ?b2 ?string tension :time ?t))))
  :effects
  ((eqn (= ?t1-var ?t2-var) (tensions-equal ?string ?bodies ?t)))
  :hint
  ((teach 
      (kcd "tension=magnitude_of_tension_force")
      (string "When a string connects two objects, then the tension forces that it exerts on them have the same magnitude."))
   (bottom-out (string "Because ~a connects ~a and ~a, the tension forces it exerts on them have the same magnitude, so write ~a=~a." ?string ?b1 ?b2 (?t1-var algebra) (?t2-var algebra)))))


;;; Moment of inertia:
;;; This is the rotational analog of mass. 
;;; Like mass, it is timeless unless feature changing-mass is specified.
;;;
;;; The moment of inertia of a body also depends on the axis of rotation being
;;; considered, e.g. it is different for a stick rotating about its center 
;;; than about its end. The "shape" statement which specifies the rigid body
;;; shape also includes a third argument giving the axis of rotation relevant 
;;; to the problem, so the relevant axis can be derived. We don't have 
;;; problems where rotation about more than one axis is considered.

(defoperator inherit-timeless-moment-of-inertia (?b ?t)
   :preconditions ((test (not (member 'changing-mass (problem-features *cp*))))
		   (time ?t))
  :effects 
  ((inherit-quantity (moment-of-inertia ?b :time ?t) (moment-of-inertia ?b))))


(defoperator define-moment-of-inertia (?b)
  :preconditions 
  (
   ;; only use time when allowed by feature changing-mass
    ;; This is a sanity test to ensure inherit-quantity is working OK.
   (test (or (eq (null ?t) 
		 (null (member 'changing-mass (problem-features *cp*))))
	     (error "time slot ~A not consistant with problem features" ?t)))
   (object ?b)
   (bind ?I-var (format-sym "I_~A~@[_~A~]" (body-name ?b) (time-abbrev ?t)))
  )
  :effects (
    (define-var (moment-of-inertia ?b :time ?t))
    (variable ?I-var (moment-of-inertia ?b :time ?t))
  )
  :hint 
  (
   (bottom-out (string "Use the Add Variable command to define a variable for ~A."
		       ((moment-of-inertia ?b :time ?t) def-np)))
   ))




;;===================== Torque and Net Torque =========================


;;; draw an individual torque due to a force with at known direction
;;; acting on a point with a known relative position
(defoperator draw-torque (?b ?axis ?pt ?agent ?type ?t)
   :preconditions (
     (in-wm (point-on-body ?pt ?b))
     ;;(force ?pt ?agent ?type ?t (dnum ?dir-f |deg|) action) 
     ;; draw the force on the point of application
     ;; sometimes the axis owner is ?b and sometimes ?pt
     (vector ?pt-or-b (force ?pt ?agent ?type :time ?t) ?f-dir)
     ;; fetch symbol name for force mag
     (in-wm (variable ?force-mag-var (mag (force ?pt ?agent ?type :time ?t))))
     ;; fetch the relative position vector and calculate torque direction
     (in-wm (given (dir (relative-position ?pt ?axis :time ?t)) ?r-dir . ?rest))
     (bind ?torque-dir (cross-product-dir ?r-dir ?f-dir))
     (bind ?mag-var (format-sym "TOR_~A_~A" ?axis ?force-mag-var))
     (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
   :effects (
     (vector ?b (torque ?b (force ?pt ?agent ?type) :axis ?axis :time ?t) 
	     ?torque-dir)
     (variable ?mag-var (mag (torque ?b (force ?pt ?agent ?type) 
				     :axis ?axis :time ?t)))
     (variable ?dir-var (dir (torque ?b (force ?pt ?agent ?type)
			     :axis ?axis :time ?t)))
     (given (dir (torque ?b (force ?pt ?agent ?type) :axis ?axis :time ?t)) 
	    ?torque-dir)
   )
   :hint 
   (
    (point (string "Notice that there is a[n] ~A force acting at ~a ~A which might have a tendency to cause ~a to rotate about ~A."
                   (?type adj) ?pt (?t pp) ?b ?axis))
    (teach (string "A ~A vector represents the tendency of a force acting on a rigid body to rotate the body about some axis.  In Andes problems a ~A vector will lie in the z axis, pointing in the positive direction (out of the plane of the diagram) for ~As that tend to cause ccw rotations, and in the negative direction (into the plane) for ~As that tend to cause cw rotations."
		   (nil moment-name) (nil moment-name) 
		   (nil moment-name)  (nil moment-name)))
    (bottom-out (string "Use the ~A vector drawing tool (labelled ~A) to draw the ~A about ~a due to the force acting at ~A ~A and set the direction to point ~A"  
			(nil moment-name) (nil moment-symbol)  
			(nil moment-name)
			?axis ?pt (?t pp) (?torque-dir adj)))
    ))



;;; draw an individual torque due to a couple
(defoperator draw-torque-couple (?pt ?agent ?t)
   :preconditions 
   (
    (couple orderless . ?points)
    (any-member ?points ((?pt ?agent) (?agent ?pt)))
    ;; var name identifies force by point of application and agent alone
    (bind ?mag-var (format-sym "TOR_~A_~A~@[_~A~]" (body-name ?pt) 
			       (body-name ?agent) (time-abbrev ?t)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (given (dir (torque ?pt (couple orderless . ?points) :time ?t)) 
	   ?torque-dir) 
    )
   :effects 
   (
    (vector ?pt (torque ?pt (couple orderless . ?points) :time ?t) ?torque-dir)
    (variable ?mag-var 
	      (mag (torque ?pt (couple orderless . ?points) :time ?t)))
    (variable ?dir-var 
	      (dir (torque ?pt (couple orderless . ?points) :time ?t)))
   )
   :hint 
   (
    (point (string "Notice that there is a couple between ~A."
		   (?points conjoined-defnp)))
    (teach (string "A couple is a way of expressing the rotational part of the forces between two bodies."))
    (bottom-out (string "Use the ~A vector drawing tool (labelled ~A) to draw the ~A  due to the couple from ~A ~A and set the direction to point ~A"  
			(nil moment-name) (nil moment-symbol)  
			(nil moment-name) ?agent
			(?t pp) (?torque-dir adj)))
    ))

;; For drawing net torque. Direction usually must be calculated, but first
;; two operators apply for special cases:

; draw net torque if we have been given its direction
(defoperator draw-net-torque-known-dir (?b ?axis ?t)
 :preconditions (
     (in-wm (given (dir (net-torque ?b ?axis :time ?t)) ?dir))
     ; var name identifies force by point of application and agent alone
     (bind ?mag-var (format-sym "NTOR_~A_~A~@[_~A~]" (body-name ?b) ?axis 
                                                 (time-abbrev ?t)))
     (bind ?dir-var (format-sym "O~A" ?mag-var))
     (not (vector ?b (net-torque ?b ?axis :time ?t) ?dontcare))
     (bind ?phi-value (zdir-phi ?dir))
   )
   :effects (
     (vector ?b (net-torque ?b ?axis :time ?t) ?dir)
     (variable ?mag-var (mag (net-torque ?b ?axis :time ?t)))
     (variable ?dir-var (dir (net-torque ?b ?axis :time ?t))) 
     (given (dir (net-torque ?b ?axis :time ?t)) ?dir)
     ; Because dir is problem given, find-by-PSM won't ensure implicit eqn
     ; gets written. Given value may not be used elsewhere so ensure it here.
     (implicit-eqn (= ?dir-var ?phi-value) (dir (net-torque ?b :time ?t)))
   )
   :hint 
   (
    (point (string "You were given the direction of the net ~A on ~a about ~a ~a in this situation." 
		   (nil moment-name) ?b ?axis (?t pp)))
    (bottom-out (string "Use the ~A vector drawing tool (labelled ~A) to draw the net ~A on ~a about ~a ~A and set the direction to point ~A" 
			(nil moment-name) (nil moment-symbol) 
			(nil moment-name) 
			?b ?axis (?time pp) (?dir adj))) 
    ))

;;; draw net torque if direction of angular acceleration known
;;; may be directly given or derivable from motion spec
;;; In some cases this is a cheat for where the direction is obvious because
;;; there is only one force, so we have given that the object's rotation is
;;; speeding up.
(defoperator draw-net-torque-from-motion (?b ?axis ?t)
  :preconditions 
  (
   (not (vector ?b (net-torque ?b ?axis :time ?t) ?dontcare))
   ;; we need to bind ?axis
   (motion ?b rotating :accel ?dir :axis ?axis :time ?t-motion . ?whatever)
   (time ?t)
   (test (tinsidep ?t ?t-motion))
   (test (known-z-dir-spec ?dir))
   ;; var name identifies force by point of application and agent alone
   (bind ?mag-var (format-sym "NTOR_~A_~A_~A" (body-name ?b) ?axis 
			      (time-abbrev ?t)))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   )
   :effects (
     (vector ?b (net-torque ?b ?axis :time ?t) ?dir)
     (variable ?mag-var (mag (net-torque ?b ?axis :time ?t)))
     (variable ?dir-var (dir (net-torque ?b ?axis :time ?t))) 
     (given (dir (net-torque ?b ?axis :time ?t)) ?dir)
   )
   :hint 
   (
    (point (string "You should be able to determine the direction of the angular acceleration of ~a ~a from the problem description.  You can use that to determine the direction of the net ~A." ?b (?t pp) (nil moment-name)))
    (teach (string "Newton's second law for rotation says that the net ~A on an object is proportional to its angular acceleration.  This is a vector relation, therefore the net ~A will point in the same direction as the angular acceleration vector." 
		   (nil moment-name) (nil moment-name)))
    (bottom-out (string "Since the angular acceleration is known to be directed ~A, use the ~A vector drawing tool (labelled ~A) to draw the net ~A on ~a about ~a ~A and set the direction to point ~A" 
			(?dir adj) (nil moment-name) (nil moment-symbol)
			(nil moment-name) ?b ?axis (?time pp) (?dir adj))) 
    ))

;;; draw zero net torque if object given is not rotating.
(defoperator draw-net-torque-non-rotating (?b ?axis ?t)
  :preconditions 
  (
   (time ?t)
   (not (vector ?b (net-torque ?b ?axis :time ?t) ?dontcare))
   (motion ?b ang-at-rest :time ?t-motion)
   (test (tinsidep ?t ?t-motion))
   (bind ?mag-var (format-sym "NTOR_~A_~A~@[_~A~]" (body-name ?b) ?axis 
			      (time-abbrev ?t)))
   )
   :effects (
     (vector ?b (net-torque ?b ?axis :time ?t) zero)
     (variable ?mag-var (mag (net-torque ?b ?axis :time ?t)))
     ;; put out implicit equation for given
     (implicit-eqn (= ?mag-var 0) (given (mag (net-torque ?b ?axis :time ?t))))
   )
   :hint (
	  (point (string "Notice that ~A is in rotational equilibrium ~A. That should tell you something about the net ~A." 
			 ?b (?t pp) (nil moment-name)))
	  (teach (string "A rigid object is said to be in rotational equilibrium if the net ~A acting on it is zero, so it has no tendency to rotate." 
			 (nil moment-name)))
     (bottom-out (string "Since the object is in rotational equilibrium, use the ~A vector drawing tool (labelled ~A) to draw a zero length vector representing the net ~A on ~a about ~a ~A." 
			 (nil moment-name) (nil moment-symbol) 
			 (nil moment-name) ?b ?axis (?t pp) (?dir adj))) 
   ))

;;; following draws the net torque vector on a body at an unknown direction
;;; We presume it is unknown until torques from all forces are computed and 
;;; summed.  Later we will add operators to draw it at a determinate direction 
;;; if that can be determined easily from the givens, e.g. if all given forces 
;;; torque in same direction.
(defoperator draw-net-torque-unknown-dir (?b ?axis ?t)
  :preconditions (
     ;; apply when none of above known dir ops apply:
     ;; not just given dir:
     (not (given (dir (net-torque ?b ?axis :time ?t)) ?given-dir))
     ;; not given ang-accel dir
     (not (given (dir (ang-accel ?b :time ?t)) ?dir))
     ;;     and dir(ang-accel) not in motion statement
     (not (motion ?b rotating :accel ?a-dir
		  :axis ?axis :time ?t-motion . ?whatever)
          (and (tinsidep ?t ?t-motion) (known-z-dir-spec ?a-dir)))
     ;; not known in rotational equilibrium:
     (not (motion ?b ang-at-rest :time ?t-motion) (tinsidep ?t ?t-motion))
      ;; var name identifies force by point of application and agent alone
     (bind ?mag-var (format-sym "NTOR_~A_~A~@[_~A~]" (body-name ?b) ?axis 
                                                 (time-abbrev ?t)))
     (bind ?dir-var (format-sym "O~A" ?mag-var))
     (not (vector ?b (net-torque ?b ?axis :time ?t) ?dontcare))
   )
   :effects (
     (vector ?b (net-torque ?b ?axis :time ?t) z-unknown)
     (variable ?mag-var (mag (net-torque ?b ?axis :time ?t)))
     (variable ?dir-var (dir (net-torque ?b ?axis :time ?t))) 
   )
   :hint (
	  (point (string "You need to introduce a term for the net ~A on ~a ~a"  
			 (nil moment-name) ?b (?t pp)))
     (teach (string "The net ~A on a rigid body will represent the tendency of the body to rotate cw or ccw by a vector along the z axis in accordance with the right hand rule.  Although you know the net ~A vector lies along the z axis, it requires calculation to determine whether it points into or out of the plane. Therefore you should specify its direction as Unknown Z direction in the dialog box after drawing it." (nil moment-name) (nil moment-name)))
     (bottom-out (string "Use the ~A vector drawing tool (labelled ~A) to draw a non-zero net ~A vector on ~A about ~a ~A, selecting \"Unknown Z direction\" from the direction menu in the dialog box." 
			 (nil moment-name) (nil moment-symbol) 
			 (nil moment-name) ?b ?axis (?t pp)))
   ))


(defoperator draw-torques (?b ?axis ?t)
   :specifications 
    "If there are any torques on ?body at ?time,
     then make them the set of torques on ?body at ?time"
   :preconditions
   ((body ?b)
    ;; draw all individual torques and couples we can find
    ;; Look in *.prb file to see if this turns out OK
    (setof (vector ?b (torque ?b ?force :axis ?axis :time ?t) ?dir)
	   (torque ?b ?force :axis ?axis :time ?t) 
	   ?torques)
    ;; Because of the way setof works (it substitutes bindings gotten
    ;; from unifying the Goal into the Term) this must be separate:
    (setof (vector ?b (torque ?b ?force :time ?t) ?dir)
	   (torque ?b ?force :time ?t) 
	   ?couples)
    (bind ?all-torques (union ?torques ?couples :test #'unify))
    (debug "Draw-torques for ?b=~A ?axis=~A ?t=~A:~%~{     ~S~%~}" 
	   ?b ?axis ?t ?all-torques)
    )
   :effects
    ((torques ?b ?axis ?t ?all-torques)))


;; following draws torque diagram for qualititative problems that ask for 
;; all torques only.  This shows a body and all torques on it. 
;; We make axes optional here. Counterpart to draw-standard-fbd
(defoperator draw-torque-fbd (?b ?axis ?t)
   :preconditions 
           ((body ?b)
	    (torques ?b ?axis ?t ?torques)
	    (optional (axes-for ?b 0)))
   :effects ((torque-fbd ?b ?axis ?t)))

;; Net torque PSM -- for computing net torque on an object as sum of
;; torques produced by individual forces on parts of object.
;;

(def-psmclass net-torque-zc (?eq-type definition ?xyz ?rot 
				      (net-torque ?body ?pivot ?time))
  :complexity major ;See Bug #1144
  :short-name ("net ~A defined" (moment-name))
  :english ("the definition of net ~A" (moment-name))
  :expformat ("applying the definition of net ~A on ~a about ~A"
	      (moment-name) (nlg ?body) (nlg ?pivot 'at-time ?time))
  :eqnformat ((torque-switch "Mnet_z = M1_z + M2_z + ..."
			     "$tnet_z = $t1_z + $t2_z + ...")))

(defoperator net-torque-contains (?sought)
  :preconditions (
    (any-member ?sought (
                     (net-torque ?b ?axis :time ?t)
		     (torque ?b ?force :axis ?axis ?axis :time ?t)
                        ))
    ;; should be able to do any point-on-body, 
    ;; but choose rotation axis when specified
    (rotation-axis ?b ?axis) ;?axis is not bound by couples
    ;; make sure there aren't unknown forces, as in basic rotational kinematics
    ;; problems with unexplained accelerations
   (not (unknown-forces :time ?t ?t))
   )
   :effects (
   (eqn-family-contains (net-torque ?b ?axis ?t) ?sought)
  ;; since only one compo-eqn under this vector PSM, we can just
  ;; select it now, rather than requiring further operators to do so
   (compo-eqn-contains (net-torque ?b ?axis ?t) definition ?sought)
   ))

(defoperator draw-net-torque-diagram (?rot ?b ?axis ?t)
  :preconditions (
   ;; draw all torques due to each force
   (torques ?b ?axis ?t ?torques)
   ;; draw net torque on object 
   (vector ?b (net-torque ?b ?axis :time ?t) ?dir)
   (axes-for ?b ?rot)
  )
  :effects (
    (vector-diagram ?rot (net-torque ?b ?axis ?t))
  ))

;; generate equation for net torque:
;; Tnet_z = Tau1_z + Tau2_z + Tau3_z
;;
;; The generic projection operator plugs in
;;    magTau * cos (0Tau_z) for Tau_z, where cos (0Tau_z) = +/- 1
;; We might like to use torque mag formula
;;    magR * magF * sin (ThetaRF)  for magTau1
;; and plug these terms in final equation for net torque. But that would 
;; preclude using the generic operators for writing a component equation then
;; for moving from component eqns to compo-free eqns by plugging in 
;; projections.  We would have to plug in projections first then plug in 
;; magnitude expressions.  For now, we just enter equations for mag of 
;; individual torques as separate in bubble graph. 

;; Following asks for projection for net torque. For use when dir is known. 
;; This can now be used with generic component-form flag as well.
;;
(defoperator write-net-torque (?b ?axis ?t ?z ?rot)
  :preconditions (
   ;; fetch list of individual torques
   (in-wm (torques ?b ?axis ?t ?torques))
   ;; define component variables for each of the contributing torques
   (map ?torque ?torques
      (variable ?ti_z (compo ?z ?rot ?torque))
      ?ti_z ?torque-compos) 
   (debug "net torque components: ~A~%" ?torque-compos)
   ;; define zc net torque variable 
   (variable ?tnet_z (compo ?z ?rot (net-torque ?b ?axis :time ?t)))
  )
  :effects (
   (eqn (= ?tnet_z (+ . ?torque-compos)) 
               (compo-eqn definition ?z ?rot (net-torque ?b ?axis ?t)))
   (assume using-NL net ?b ?t)
  )
  :hint (
	 (point (string "Can you write an equation for the z component of the net ~A in terms of the z components of ~As due to each force?" 
			(nil moment-name) (nil moment-name)))
   (teach (string "The net ~A on a rigid body is the vector sum of the individual ~As due to each force acting on that body. Therefore the z component of the net ~A is the sum of the z components of the ~A due to each force."
		  (nil moment-name) (nil moment-name) 
		  (nil moment-name) (nil moment-name)))
    (bottom-out (string "Write the equation ~A" 
          ((= ?tnet_z (+ . ?torque-compos)) algebra)))
  ))

;;
;; mag-torque: scalar equation for magnitude of an individual torque
;;

(defoperator mag-torque-contains (?sought)
   :preconditions (
   (any-member ?sought (
                  (mag (torque ?b (force ?pt ?agent ?type) 
			       :axis ?axis :time ?t))
		  (mag (force ?pt ?agent ?type :time ?t))
		  (mag (relative-position ?pt ?axis :time ?t))
                       ))
   ;; So far this will apply in any problem where any force is sought. 
   ;; Require pt of application to be part of larger rigid body, so that
   ;; won't apply if dealing only with particles. 
   (point-on-body ?pt ?b)
   ;; !!! if sought is not torque, e.g. a force on body part, have to choose 
   ;; an axis on body about which to consider torque.  In theory the torque 
   ;; about an axis is defined for any point on the object, but if it's fixed 
   ;; at a pivot or known to be rotating we should pick that axis.
   ;; So might want rotation-axis statement ala CLIPs to tell this. 
   (rotation-axis ?b ?axis)
   ;; find force without drawing it, to get ?agent and ?type
   (force ?pt ?agent ?type ?t . ?rest)
   )
   :effects 
   ((eqn-contains (mag-torque ?b ?axis (force ?pt ?agent ?type) ?t) ?sought)))

(defoperator mag-torque-contains-angle (?sought)
   :preconditions 
   (
    ;; doesn't explicitly contain directions of relative position
    ;; and force, only difference between these
   (any-member ?sought ((angle-between orderless . ?vecs)))
   (any-member ?vecs 
	       ;; These must be in lexical order:
	       (((force ?pt ?agent ?type :time ?t)
		 (relative-position ?pt ?axis :time ?t))))
   (point-on-body ?pt ?b)
   (rotation-axis ?b ?axis)
   ;; find force without drawing it, to get ?agent and ?type
   (force ?pt ?agent ?type ?t . ?rest)
   )
   :effects 
   ((eqn-contains (mag-torque ?b ?axis (force ?pt ?agent ?type) ?t) ?sought)))

(defoperator write-mag-torque (?b ?axis ?pt ?agent ?type ?t)
   :preconditions 
   (
    (variable ?tau-var (mag (torque ?b (force ?pt ?agent ?type)
				    :axis ?axis :time ?t)))
    (variable ?f-var   (mag (force ?pt ?agent ?type :time ?t)))
    (variable ?r-var   (mag (relative-position ?pt ?axis :time ?t)))
    (variable ?theta-var (angle-between orderless 
					(force ?pt ?agent ?type :time ?t) 
					(relative-position ?pt ?axis :time ?t)))
    )
   :effects (
      (eqn (= ?tau-var (* ?r-var ?f-var (sin ?theta-var))) 
             (mag-torque ?b ?axis (force ?pt ?agent ?type) ?t))
   )
   :hint (
   (point (string "You need an expression for the magnitude of the ~A due to the ~A force acting at ~A" (nil moment-name) (?type adj) ?pt))
   (teach (string "The magnitude of the ~A ~A resulting from a force of magnitude F acting at a point of perpendicular distance r from the axis is given by ~A = r * F * sin ($q), where $q is the smaller of two angles between the vectors r and F." (nil moment-name) (nil moment-symbol) (nil moment-symbol)))
   (bottom-out (string "Write the equation ~A" 
               ((= ?tau-var (* ?r-var ?f-var (sin ?theta-var))) algebra)))
   ))
   

;;
;; torque-zc: equation for individual torque z-component
;;  tau_z = F*r*sin(thetaF - thetaR)
;;

(defoperator torque-contains (?sought)
  :preconditions 
  (
   ;; don't list angle as sought since it is the sine
   (any-member ?sought ( 
			(compo ?xy ?rot (force ?pt ?agent ?type :time ?t))
			(compo ?xy ?rot (relative-position ?pt ?axis :time ?t))
			(mag (force ?pt ?agent ?type :time ?t))
			(mag (relative-position ?pt ?axis :time ?t))
	                ))
   ;; So far this will apply in any problem where any force is sought. 
   ;; Require pt of application to be part of any larger rigid body. 
   (point-on-body ?pt ?b)
   ;; Choose an axis on body about which to consider torque.  
   (rotation-axis ?b ?axis)
   (axes-for ?b ?rot)  ;in case ?rot is not bound
   (get-axis ?xyz ?rot) ;iterate over directions
   ;; find force without drawing it, to get ?agent
   (force ?pt ?agent ?type ?t . ?rest)
   ;;
   (any-member ?angle-flag (t nil))
   (test (if ?angle-flag (not (eq (car ?sought) 'compo))
	   (not (eq (car ?sought) 'mag))))
   )
 :effects 
 ( (eqn-contains (torque ?xyz ?rot ?b ?axis 
			 (force ?pt ?agent ?type) ?angle-flag ?t) ?sought)
   ))

(defoperator torque-contains-torque (?sought)
  :preconditions 
  (
   (any-member ?sought ((compo ?xyz ?rot (torque ?b (force ?pt ?agent ?type) 
						 :axis ?axis :time ?t))))
   ;; So far this will apply in any problem where any force is sought. 
   ;; Require pt of application to be part of any larger rigid body.
   (point-on-body ?pt ?b)
   (any-member ?angle-flag (t nil)) ;iterate over form
   )
  :effects 
  ( (eqn-contains (torque ?xyz ?rot ?b ?axis 
			  (force ?pt ?agent ?type) ?angle-flag ?t) ?sought)
    ))

(defoperator write-torque (?xyz ?rot ?b ?axis ?pt ?agent ?type ?t ?angle-flag)
  :preconditions 
  (
   ;; draw vectors before doing cross product
   (vector ?pt (relative-position ?pt ?axis :time ?t) ?dir-v)
   (vector ?pt (force ?pt ?agent ?type :time ?t) ?dir-f)
   (vector ?b (torque ?b (force ?pt ?agent ?type) :axis ?axis :time ?t) ?dir-t)
   (variable ?tau (compo ?xyz ?rot (torque ?b (force ?pt ?agent ?type)
				     :axis ?axis :time ?t)))
   (cross ?cross (relative-position ?pt ?axis :time ?t)
	  (force ?pt ?agent ?type :time ?t) ?xyz ?rot ?angle-flag)
   )
  :effects 
  ( (eqn (= ?tau ?cross) 
	(torque ?xyz ?rot ?b ?axis (force ?pt ?agent ?type) ?angle-flag ?t)) )
  :hint 
  (
   (point (string "You need an expression for the ~A component of the ~A due to the ~A force acting at ~A"
		  (?xyz axis-name)  
		  (nil moment-name) (?type adj) ?pt))
   (teach (string "The ~A component of the ~A ~A resulting from a force of magnitude F acting at a point of perpendicular distance r from the axis can be calculated as ~A."
		  (?xyz axis-name)
		  (nil moment-name) (nil moment-symbol) 
		  (?xyz torque-equation)))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?tau ?cross) algebra)))
   ))

;; num-torques mainly for test problems:
(defoperator num-torques-contains (?b ?t)
  :preconditions ()
  :effects ( (eqn-contains (num-torques ?b ?axis ?t) (num-torques ?b ?axis :time ?t)) ))

(defoperator write-num-torques (?b ?axis ?t)
  :preconditions 
   ((torques ?b ?axis ?t ?torques)
    (bind ?count (length ?torques))
    (variable ?n-var (num-torques ?b ?axis :time ?t)) )
  :effects 
  ( (eqn (= ?n-var ?count) (num-torques ?b ?axis ?t)) ))
 
(defoperator define-num-torques (?b ?axis ?t)
  :preconditions 
  ((bind ?n-var (format-sym "ntorques_~A~@[_~A~]" 
			    (body-name ?b) (time-abbrev ?t))))
  :effects ( (variable ?n-var (num-torques ?b ?axis :time ?t)) ))

;;;;===========================================================================
;;;;
;;;;                  Rotational version of NL
;;;;
;;;;===========================================================================
;;;
;;;   This should be a copy of the translational version of Newton's laws
;;;   In principle, the rotational and translational versions should be merged.
;;;

(defoperator NL-rot-contains (?quantity)
  :specifications 
  "Newton's law potentially contains the body's mass, 
     the magnitude of its acceleration, and
     the direction of its acceleration"
  :preconditions 
  (
   (rotation-axis ?b ?axis)
   (any-member ?quantity
	       ((moment-of-inertiam ?b :time ?t) 
		(ang-accel ?b :time ?t)
		(torque ?b ?force :axis ?axis :time ?t)))
   (object ?b) ;; sanity check
   (time ?t))
  :effects ((eqn-family-contains (NL-rot ?b ?axis ?t) ?quantity)))

(defoperator NL-rot-net-vector-contains (?quantity)
  :preconditions 
  (
   (rotation-axis ?b ?axis)
   (any-member ?quantity ((moment-of-inertia ?b :time ?t) 
			  (ang-accel ?b :time ?t)
			  (net-torque ?b ?axis :time ?t)
			  ))
   (not (point-on-body ?b ?bb))
   (object ?b) ;sanity check
   (time ?t))
  :effects ((eqn-family-contains (NL-rot ?b ?axis ?t :net t) ?quantity)))

(def-goalprop NL-rot-fbd  (vector-diagram ?rot (NL-rot ?b ?axis ?t))
  :doc "diagram for applying the rotational version of Newton's second law"
  :english ("drawing a diagram showing all the ~As on ~A ~A, the angular acceleration, and coordinate axes"
	    (moment-name) (nlg ?b) (nlg ?t 'pp))) 

(defoperator draw-nl-rot-fbd (?rot ?b ?t)
  :preconditions
  (
   (torques ?b ?axis ?t ?forces)   ;; this also draws the torques
   (test ?forces)	;fail if no forces could be found
   (inherit-vector ?b (ang-accel ?b :time ?t) ?accel-dir)
   (axes-for ?b ?rot))
  :effects
   ((vector-diagram ?rot (NL-rot ?b ?axis ?t))))

;; 
;; Following draws a free-body diagram for the net-force variant of NL
;;
(defoperator draw-NL-rot-net-fbd (?rot ?b ?t)
  :preconditions
  (
   (debug "start draw-NL-rot-net-fbd~%")
   (test ?netp)
   (inherit-vector ?b (ang-accel ?b :time ?t) ?accel-dir)
   (vector ?b (net-torque ?b ?axis :time ?t) ?force-dir) 
   (axes-for ?b ?rot)
   (debug "finish draw-NL-rot-net-fbd~%")
   )
  :effects
   ((vector-diagram ?rot (NL-rot ?b ?axis ?t :net ?netp))))
  
(def-psmclass NFL-rot (?eqn-type NFL ?xyz ?rot (NL-rot ?body ?pivot ?time))
  :complexity major
  :short-name "rotational form of Newton's 2nd law ($a =0)"
  :english ("rotational version of Newton's second law ($a=0)")
  :expformat ("applying rotational version of Newton's second law to ~a about ~A"
	      (nlg ?body) (nlg ?pivot 'at-time ?time))
  :eqnFormat ((torque-switch "0 = M1_z + M2_z + ..." 
			    "0 = $t1_z + $t2_z + ...")))
  
(defoperator NFL-rot-zero-accel (?quantity)
  :preconditions 
  ((any-member ?quantity ((torque ?b ?force :axis ?axis :time ?t)))
   (object ?b)
   ;;done in drawing step
   (in-wm (inherit-vector ?b (ang-accel ?b :time ?t) zero)) )
  :effects ((compo-eqn-contains (NL-rot ?b ?axis ?t) NFL ?quantity)))

(def-psmclass NSL-rot (?eqn-type NSL ?xyz ?rot 
				 (NL-rot ?body ?pivot ?time :net ?netp))
  :complexity major
  :short-name "rotational form of Newton's 2nd law"
  :english ("rotational version of Newton's second law")
  :expformat ("applying rotational version of Newton's second law to ~a about ~A"
	      (nlg ?body) (nlg ?pivot 'at-time ?time))
  :eqnFormat ((torque-switch "Mnet_z = I*$a_z" "$tnet_z = I*$a_z" )))

(defoperator NSL-rot (?quantity)
  :preconditions 
  (
   (debug "start  NSL~%")
   (not (vector ?b (ang-accel ?b :time ?t) zero))
   (not (massless ?b))
   (debug "finish  NSL~%")
   )
  :effects
   ((compo-eqn-contains (NL-rot ?b ?axis ?t :net ?netp) NSL ?quantity))
 )

(defoperator write-NFL-rot (?b ?t ?xyz ?rot)
  :preconditions
  ((in-wm (torques ?b ?axis ?t ?forces)) ;from drawing step
   (map ?f ?forces 
   	(inherit-variable ?compo-var (compo ?xyz ?rot ?f))
	?compo-var ?f-compo-vars)
   ;; we want Fi = m * a to be accepted if it is written. But also
   ;; need to write Sum Fi = 0 as final eqn so won't appear to contain m, a
   ;; so we make sure we have a compo var and put implicit eqn in effects.
    (variable ?a-compo (compo ?xyz ?rot (ang-accel ?b :time ?t)))
  )
  :effects
   ((eqn (= (+ . ?f-compo-vars) 0)
	 (compo-eqn NFL ?xyz ?rot (NL-rot ?b ?axis ?t)))
    (assume using-NL no-net ?b ?t)
    (implicit-eqn (= ?a-compo 0) 
		  (projection (compo ?xyz ?rot (accel ?b :time ?t)))))
  :hint
   ((point (string "You can apply the rotational version Newton's second law to ~A.  Note that ~A has no angular acceleration ~A." 
		   ?b ?b (?t pp)))
    (teach (string 
    "The rotational version of Newton's second law $t = I*$a states that the net torque on an object = the object's moment of inertia times its angular acceleration.  In this case the angular acceleration is zero so you know the sum of all torques on the object must be zero."
    ))
    (bottom-out (string "Because ~a is not accelerating ~a, write Newton's second law as ~A" 
			?b (?t pp) ((= (+ . ?f-compo-vars) 0) algebra)))))


(defoperator write-NSL-rot-compo (?b ?axis ?t ?xyz ?rot)
  :specifications 
   "If the goal is to write newton's second law in component form,
      ensure there are component variables ?compo-vars for the components 
      of each of the forces on ?b at ?t,
   then write ?f1c + ?f2c + ... = ?m * ?ac, where ?fic and ?ac
      are the appropriate component variables for ?fi and ?a,
      respectively."
  :preconditions
  (
   (in-wm (torques ?b ?axis ?t ?forces)) ;done in drawing step
   ;; for each force on b at t, define a component variable, 
   ;; collecting variable names into ?f-compo-vars
   (debug "write-NSL-compo(~A ~A ~A): defining force compo vars~%" ?b ?xyz ?rot)
   (map ?f ?forces 
    (inherit-variable ?f-compo-var (compo ?xyz ?rot ?f))
   	?f-compo-var ?f-compo-vars)
   (debug "write-NSL-compo: set of force compo-vars = ~A~%" ?force-compo-vars)
   (variable ?a-compo (compo ?xyz ?rot (ang-accel ?b :time ?t)))
   ;; assume any mass change is described by thrust force
   (inherit-variable ?m (moment-of-inertia ?b :time ?t))
   ;; see if acceleration compo doesn't vanish
   ;; if it does, we still write equation to give sum of forces = 0
   (in-wm (vector ?b (ang-accel ?b :time ?t) ?dir-a))
   (bind ?ma-term (if (non-zero-projectionp ?dir-a ?xyz ?rot)
		      `(* ,?m ,?a-compo) 0))
   (debug "write-NSL-compo: eqn-compo-vars = ~A~%" ?eqn-compo-vars)
   )
  :effects
   ((eqn (= (+ . ?f-compo-vars) ?ma-term)
	 (compo-eqn NSL ?xyz ?rot (NL-rot ?b ?axis ?t)))
    (assume using-NL no-net ?b ?t)
    )
  :hint
   ((point (string "You can apply the rotational version of Newton's second law to ~A.  Note that ~A has angular acceleration ~A." 
		   ?b ?b (?t pp)))
    (teach (string "The rotation version of Newton's second law is $t = m*$a.  The net torque $t is the vector sum of all torques acting on the object.  This can be applied component-wise."))
    (bottom-out (string "Write the rotational version of Newton's second law in terms of component variables along the ~A axis as ~A" 
			((axis ?xyz ?rot) symbols-label) ((= (+ . ?f-compo-vars) ?ma-term) algebra)))
    ))

(defoperator write-NSL-rot-net (?b ?axis ?t ?z ?rot)
   
   :preconditions 
   (
    (test ?netp)
    (variable ?tau_z  (compo ?z ?rot (net-torque ?b ?axis :time ?t)))
    (inherit-variable ?I (moment-of-inertia ?b :time ?t))
    (variable ?alpha_z (compo ?z ?rot (ang-accel ?b :time ?t)))
    ;; fetch mag variable for implicit equation (defined when drawn)
    (in-wm (variable ?mag-var (mag (ang-accel ?b :time ?t))))
    )
   :effects (
     (eqn (= ?tau_z (* ?I ?alpha_z)) 
                 (compo-eqn NSL ?z ?rot (NL-rot ?b ?axis ?t :net ?netp)))
     ;; Don't do this because it can pre-empt projection if it is in fact used
     ;; on a component-form problem for magnitude (tor5a)
     ;; for algebraic completeness: put out equation for mag ang-accel
     ;; in terms of component, so gets determined from alpha_z if dir is unknown
     ;; (implicit-eqn (= ?mag-var (abs (?alpha_z))) (mag (ang-accel ?b :time ?t)))
     (assume using-NL net ?b ?t)
     )
   :hint (
	  (point (string "Can you relate the z components of the net ~A and angular acceleration?"
			  (nil moment-name)))
	  (teach (string "Just as Newton's second law says that Fnet = m*a, Newton's law for rotation states that the net ~A on an object equals the object's moment of inertia times its angular acceleration.  This relation can be applied along the z axis to relate the z-components of net ~A and angular acceleration." 
			 (nil moment-name) (nil moment-name)))
    (bottom-out (string "Write Newton's law for rotation in terms of component variables along the z axis, namely ~A." 
                        ((= ?tau_z (* ?I ?alpha_z)) algebra)))
   ))
