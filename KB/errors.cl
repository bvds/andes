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
;;
;;                 Table of contents
;;
;; HOWTO
;; Runtime Parameters.
;; Non equation entries
;;    Defining variables
;;       all variables
;;       mass
;;       distance between (not implemented yet)
;;       distance traveled
;;       speed
;;       duration
;;       energy
;;       compression distance
;;       spring constant
;;       height
;;       moment of inertia
;;       radius of circular motion
;;       radius of an object (not tested yet)
;;       length and width (not tested yet)
;;       work
;;       net work
;;    body tool
;;    axis tool
;;    Vectors
;;       all vectors
;;       velocity
;;       acceleration
;;       displacement
;;       individual forces
;;       net force
;;       momentum (not implemented yet)
;;       individual torque
;;       net torque
;;       relative position
;;       electric field
;; Equation entries
;;    Substitute similar variables
;;    Trig errors
;;    Projection equation errors
;;    Sign errors
;;    Assignment statements
;; Answer box entries
;; Miscellaneous
;; Catchall to IEA
;;

;;;; =========================== HOWTO =============================
;;; Writing error handlers (known as error classes) is not a task
;;; for the faint of heart or weak of sanity but it can be done, 
;;; one may lose much of oneself in the process as the horrors burn
;;; away many things about oneself.  One must be prepared to watch
;;; the id burn away like so much loose clothing caught in a 
;;; firestorm, leaving the soul naked and cold but stronger for the
;;; pursuit.  If you accept this task follow on and learn what
;;; horrors do await.
;;; 
;;; In order to define them you must use the def-error-class function
;;; (def-Error-Class <name> <arguments> <conditions> 
;;;          	     &key <Probability Def=0.1> <Utility Def=1.0>)
;;; 
;;; This will generate a class and store it in the **entry-tests**
;;; parameter.  The contents of each field are as follows:
;;;
;;;   Name:  An identifier (that must be unique) which will be used
;;;          to tag the companion function later on.
;;;
;;;   Arguments: A list of variables (?foo...) that will be bound by
;;;     the Conditions as they are run.  They will be passed on to 
;;;     the function when it is called.
;;;
;;;   Conditions:  A set of error conditions (see below) that will 
;;;     be tested by what's wrong help.  If these conditions are
;;;     satisfied then the error is valid.
;;;
;;;   Probability:  A lisp-expression that returns a floating point 
;;;     number describing the probability that this entry-test is
;;;     the correct one.  This will be used along with the utility
;;;     to select the appropriate entry-test in the event that
;;;     several satisfy.
;;;
;;;   Utility:  A second lisp-expression reporting the estimated
;;;     "value" of this entry-test iff it satisfies.  This too
;;;     is used for selection later on.
;;;
;;;
;;; --------------------- Use of Error Classes ------------------------
;;; When Whats-Wrong-Help Andes2/Help/whatswrong.cl is called it will
;;; iterate over each error handler in the set of **Entry-Tests**
;;; and testing the conditions on each one.  As the conditions are 
;;; tested a set of bindings will be generated.  If the conditions are
;;; all satisfied then a error-interpretation will be generated based
;;; upon the entry-test and using the bindings that were generated.
;;;
;;; The set of possible conditions are:
;;;
;;; (Student <pattern>)
;;;  If the <pattern> of a (student <pattern>) condition unifies with
;;;  the entry proposition of the student's entry, then continue
;;;  checking conditions with the new bindings caused by unification.
;;;  If not, then fail by returning NIL.
;;;
;;; (old-student <pattern>)
;;;  If the <pattern> of a (old-student <pattern>) condition unifies with
;;;  the entry proposition of an existing correct student entry, then continue
;;;  checking conditions with the new bindings caused by unification.
;;;  If not, then fail by returning NIL.
;;;
;;; (no-student <pattern>)
;;;  If the <pattern> of a (no-student <pattern>) condition never unifies with
;;;  the entry proposition of a an existing correct student entry, 
;;;  then succeed.
;;;
;;; (correct <pattern>)
;;;  If the <pattern> of a (correct <pattern>) condition unifies with
;;;  the entry proposition of a system entry, then continue checking
;;;  conditions with the new bindings caused by the unification.  If
;;;  not, then skip this system entry proposition and try the next one.
;;;
;;;  Note, if this is satisfied then it will define the intended entry
;;;  for the resulting error-interpretation unless later conditions
;;;  supersede it.  
;;;
;;; (no-correct <pattern>)
;;;  If the <pattern> of a (no-correct <pattern>) condition never unifies with
;;;  the entry proposition of a system entry, then succeed.
;;;
;;; (test <form>)
;;;  if the form in a (test <form>) condition is non-NIL after bindings
;;;  have been substituted for the ?variables, then continue.
;;;
;;; (bind <var> <form>) 
;;;  Augment the running bindings by binding var to form and recursing.
;;;
;;; (problem <pattern>)
;;;  If the <pattern> of a (problem <pattern>) condition unifies with a
;;;  proposition in the problem's working memory, then continue checking with
;;;  the new bindings caused by the unificaiton.  If not, then try the
;;;  next proposition.
;;;
;;; (any-member <var> <lisp form>)
;;;  If the condition is (any-member <var> <lisp form>), then evaluate
;;;  the lisp form, and try binding <var> to each member of the
;;;  resulting set.
;;;
;;; (eqn <string>)
;;;  True if the student's entry is an equation.  The Entry prop in this
;;;  case is (eqn <string>) which is useless.  The ParsedEqn slot of
;;;  the student holds the students entry converted to system variables
;;;  and prefix form. (unused at present but availible)
;;;
;;; (expr-loc <loc-var> <pattern>)
;;;  If the condition is (expr-loc <loc-var> <pattern>) then
;;;  find all locations in the student's equation that unify with <pattern> and
;;;  bind them to <loc-var>
;;;
;;; (var-loc <?loc-var> <?variable-var> <pattern>)
;;;  If the condition is (var-loc <?loc-var> <?variable-var> <pattern>), 
;;;  then find a variable in the student's equation whose
;;;  definition unifies with <pattern> and bind its location to
;;;  <?loc-var> and the variable itself to <?variable-pattern>
;;;
;;; (correct-var <?var> <pattern>)
;;;  if the condition is (correct-var <?var> <pattern>), then find a
;;;  system variable whose definition unifies with the <pattern> and
;;;  bind it to the ?var.
;;;
;;; (var-defn <?var> <pattern>)
;;;  if the condition is (var-defn <?var> <pattern>), then the variable
;;;  should be bound to a system variable and the variable's definition
;;;  should match the pattern.
;;;
;;; (fix-eqn-by-replacing <old> <new>)
;;;  if the condition is (fix-eqn-by-replacing <old> <new>) and
;;;  substituting the <new> expression for the <old> expression in the
;;;  student's equation creates an equation that is correct (i.e., has
;;;  a non-null set of interpretations, then find the best
;;;  interpretation and treat that as the system entry.  That
;;;  interpretation will consist of a state cons'ed to a set of
;;;  system entries.
;;;
;;;  Note that this too will set the intended entry for the error-interp
;;;  unless it is overridden.
;;;
;;; 
;;; These conditions are tested by the check-err-conditions function.  It
;;; will iteratively match each one given the existing bindings.  For 
;;; each successful match the system will augment the bindings and continue
;;; searching.  
;;;
;;; Once all of the conditions are satisfied the system will generate an 
;;; error-interpretation based upon it.  The diagnosis field of this error
;;; interp will be the name of the error class coupled with the arguments
;;; (bound).  The bindings will be the resulting list of bindings, the 
;;; class will be the entry-test itself.  The state will be determined
;;; by the error handler.  
;;;
;;; The remediation will be the list of tutor turns generated by the 
;;; companion function.  Lastly, the expected utility will be a floating
;;; point number based upon the state, the intended entry, the entry-test
;;; utility and the entry-test probability.  This number will be used to 
;;; select which among the set of possible entries will be used.  
;;;
;;; The Intended entry will be set based upon the conditions.  If either
;;; a correct, or fix-eqn-by-replacing, condition was satisified then the 
;;; intended entry will be the last one of those that succeeded.  Therefore
;;; these should be used only when an interpetation is expected.


;;;; ==================== Runtime Parameters =========================

;; See Bug #1612 for a discussion of what to do for this hint.
(defparameter *dyi*
    (strcat "If you click on 'explain more', I'll tell you a correct "
	    "entry to make.  However, you'd learn more if you figure "
	    "it out yourself.")
  "This is a canned hint that essentially says, 'do it yourself'")


(defun agent-phrase (agent)
  (when agent (def-np `(agent ,agent))))


;;;; ============== Defining variables =============================
;;;; This section covers non-equation entries under the variables menu
;;;;=============================================================

;;; If the student defines a variable and the quantity is not used in this
;;; problem, then just say so.
;; First form matches time-dependent quantities:
(def-error-class non-existent-variable (?type)
  ((student (define-var (?type . ?sargs)))
   (no-correct (define-var (?type . ?cargs)))
   ; make sure net variant also unused, so message below makes sense. 
   ; If no net quant, get-net-quant-name should return some unused
   ; quantity name.   Note: won't detect work-nc as a type of work.
   (bind ?net-quant (get-net-quant-name ?type)) ; may be NIL if none
   (no-correct (define-var (?net-quant . ?args)))))

(defun non-existent-variable (type)
  (list (format nil (strcat "Although you can always define a "
			    "~a variable, none are needed for "
			    "any solution I know of this problem.")
		(nlg type 'adj))
	'(function next-step-help)))

(def-error-class should-be-net-variable (?type)
  ((student (define-var (?type . ?sargs)))
   (no-correct (define-var (?type . ?cargs)))
   (bind ?net-quant (get-net-quant-name ?type)) ; may be NIL if none
   (correct (define-var (?net-quant . ?args))))
   )

(defun should-be-net-variable (type)
   ;; Several net quants are defined as quant "due to all sources" 
   ;; on interface, so we include that phrase in the message. 
   ;; Work and power actually use "all forces", but message
   ;; should still be understood in this case.
   (list (format nil (strcat "In this problem you should use a "
			     "variable to represent the " 
			     "net ~a resulting from all sources.")
		 (nlg type 'adj) (nlg type 'adj))
	 '(function next-step-help)))

;;; If the student defines a variable but not for a relevant time,
;;; then give a hint then bottom out.
(def-error-class variable-with-wrong-time (?descr ?stime ?ctime)
  ((student (define-var (?descr . ?sargs)))
   (correct (define-var (?descr . ?cargs)))
   (test (unify (remove-time ?sargs) (remove-time ?cargs)))
   ; rebuild whole quant form in case angle-between
   ; !!! better to have a separate handler for this case
   (bind ?stime (time-of (cons ?descr ?sargs))) 
   (bind ?ctime (time-of (cons ?descr ?cargs)))
   (test (not (equal ?stime ?ctime))))
  :utility .1)

(defun variable-with-wrong-time (descr stime ctime)
  (setq descr (nlg descr 'def-np))
  (setq stime (nlg stime 'pp))
  (setq ctime (nlg ctime 'pp))
   (list (format nil "Are you sure you want the variable defined ~a?" stime)
	 (format nil (strcat "Although you need a variable for ~a, no "
			     "solution I know of needs it ~a.")
		 descr stime)
	 *dyi*
	 `(bottom-out (string ,(format nil "Define a variable for ~a ~a instead of ~a."
		 descr ctime stime)))))

;; generic error for the wrong body (second argument)
(def-error-class only-body-wrong (?sbody ?cbody) 
  ((student (define-var (?quant ?sbody . ?rest)))
   (no-correct (define-var (?quant ?sbody . ?other)))
   (correct (define-var (?quant ?cbody . ?rest))))
  ;; small probability because we want quantity-specific hints first
  :probability .0001)

(defun only-body-wrong (sbody cbody)
   (list (format nil "Are you sure you want to choose ~A?" (nlg sbody 'def-np))
	 `(bottom-out (string ,(format nil "Perhaps you should choose ~A instead." 
		 (nlg cbody 'def-np))))))

;;; ============= defining a mass variable ==========================
;;; The mass variable has only one slot: the body.  Thus, he only
;;; mistake one can make is to choose an irrelevant object as the
;;; body.  Unfortunately, we can't suggest a better body choice
;;; because we dont' know the best solution plan.  The no-correct
;;; condition prevents this from applying when mass is defined via the
;;; body tool.
;;;
;;; Does not handle time varying mass.
;;;
(def-error-class irrelevant-mass (?wrong-body)
  ((student (define-var (mass ?wrong-body)))
   (no-correct (body ?wrong-body))))

(defun irrelevant-mass (wrong-body)
  (setq wrong-body (nlg wrong-body 'def-np))
   (list (format nil "Are you sure you need the mass of ~a?" wrong-body)
	 (format nil (strcat "Although there is nothing really wrong "
			     "with defining a variable for the mass of "
			     "~a, that variable does not participate in "
			     "any solution that I know of to this problem."
			     ) wrong-body)
	 '(function next-step-help)))


;;; =========== defining a distance-traveled variable ==============
;;; The distance-traveled variable has two slots: body and time.
;;; Thus, there are 3 default cases: (a) wrong body (ignoring time),
;;; (b) wrong time (handled by variable-with-wrong-time) and (c) no
;;; need for distance on this problem (handled by
;;; non-existent-variable).  There is a special case: Using distance
;;; when displacement is appropriate.

;;; Default case: The student defines a distance-traveled variable
;;; for the wrong body.  We ignore time.  In particular, we don't
;;; favor interpretations where the student's time matches the correct
;;; time because the student is probably pretty confused.
(def-error-class distance-traveled-wrong-body (?wrong-body 
						?correct-body 
						?time3)
  ((student (define-var (distance ?wrong-body :time ?time1)))
   (no-correct (define-var (distance ?wrong-body :time ?time2)))
   (correct (define-var (distance ?correct-body :time ?time3)))))

(defun distance-traveled-wrong-body (wrong-body correct-body correct-time)
   (list (format nil (strcat "Are you sure you want to define the distance "
			     "traveled by ~a?") 
		 (nlg wrong-body 'def-np))
	 (format nil (strcat "For solving this problem, you need to define "
			     "the distance traveled by ~a ~a.")
		 (nlg correct-body 'def-np) (nlg correct-time 'pp))
	 '(function next-step-help)))

;;; Special case: The student defines a distance-traveled when
;;; displacement of that same body is correct.  We ignore the time.
;;; Test this on kt9a.
(def-error-class distance-traveled-should-be-displacement (?body ?correct-time)
  ((student (define-var (distance ?body :time ?time1)))
   (no-correct (define-var (distance ?body2 :time ?time2)))
   (correct (vector (displacement ?body :time ?correct-time) ?dir)))
  :utility 100)

(defun distance-traveled-should-be-displacement (body correct-time)
  (setq body (nlg body 'def-np))
  (setq correct-time (nlg correct-time 'pp))
   (list (format nil (strcat "Although it is certainly correct to define "
			     "a variable for the distance traveled by ~a, "
			     "it is only used in the equation <speed> = "
			     "<distance> / <duration>, which isn't useful "
			     "for solving this problem.  Can you think of a "
			     "different princple?") body)
	 (strcat "How about using displacement instead of distance?  Do you "
		 "know any principles that involve displacement?")
	 (format nil (strcat "You should draw the displacement of ~a ~a.") 
		 body correct-time)
	 '(function next-step-help)))


;;; =========================== speeds ============================
;;; There are two slots: body and time.  Thus, there are 3 mutually
;;; exclusive default cases: (a) wrong body (ignoring time), (b) wrong
;;; time (handled by wrong-time-variable), (c) no speed needed
;;; (handled by non-existent-variable).  There is a special cases: (1)
;;; defining a speed when a velocity is appropriate.

;;; Default case: If the student gets the body wrong, then its
;;; probably unintentional so just point it out.  Favor
;;; interpretations where the student's time matches the correct time
;;; because this is probably a slip.
(def-error-class wrong-body-speed (?sbody ?cbody)
  ((student (define-var (speed ?sbody :time ?stime)))
   (no-correct (define-var (speed ?sbody :time ?time2)))
   (correct (define-var (speed ?cbody :time ?ctime))))
  :probability
  (+ 0.1 (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-body-speed (sbody cbody)
  (setq sbody (nlg sbody 'def-np))
   (list (format nil "You'll need a speed variable, but maybe not for ~a." 
		 sbody)
	 *dyi*
	 `(bottom-out (string ,(format nil "Define the speed variable for ~a instead of ~a."
		 (nlg cbody 'def-np) sbody)))))

;;; Special case: On a problem where velocities are appropriate, the
;;; student uses speed instead.  Since a velocity problem will never
;;; use speeds, we can ignore the body and time arguments.  However,
;;; an interpretatin where the body and time matches the student's
;;; body and time match is more probable than one where they do not
;;; match.  Test this on kt9a.
(def-error-class velocity-not-speed (?cbody ?ctime )
  ((student (define-var (speed ?sbody :time ?stime)))
   (correct (vector (velocity ?cbody :time ?ctime) ?dir)))
  :utility 100
  :probability
  (+ 0.1
     (if (equal ?sbody ?cbody) 0.2 0.0)
     (if (equal ?stime ?ctime) 0.3 0.0)))

(defun velocity-not-speed (body time)
   (list "On this problem, you should use velocity instead of speed."
	 `(bottom-out (string ,(format nil (strcat "Delete the speed variable and draw a "
			     "velocity vector for ~a ~a.") 
		 (nlg body 'def-np) (nlg time 'pp))))))

;;
;; On problems where wave-speed is defined...
;;
(def-error-class wave-speed-not-speed (?medium)
  ((student (define-var (speed ?sbody :time ?stime)))
   (correct (define-var (wave-speed ?medium))))
  :utility 100
  :probability 0.3			;the body choice doesn't matter much
  )

(defun wave-speed-not-speed (medium)
   (list (format nil "To define the speed of waves moving in ~A, use \"Speed of wave\" instead of \"speed.\""
	 (nlg medium 'def-np))))

;;; ============================== durations ==================================
;;; The duration tool has two slot: both for time points.  There are 2
;;; default case, namely that (a) the times don't match a useful
;;; duration and (b) duration is not needed at all.  There is a
;;; special case for time points in reverse order.

;;; Default case: If the student specifies wrong time points, then
;;; point and bottom out.
(def-entry-test default-duration (?st1 ?st2 ?ct1 ?ct2)
  :preconditions
  ((student (define-var (duration (during ?st1 ?st2))))
   (correct (define-var (duration (during ?ct1 ?ct2))))
   (test (not (> ?st1 ?st2))))
  :apply no-match
  :state +incorrect+
  :hint (default-duration `(during ,?st1 ,?st2) `(during ,?ct1 ,?ct2))
  :order ((expected-utility . 0.1)))

(defun default-duration (st ct)
   (list (format nil (strcat "Although you do need to define a duration "
			     "variable, no solution I know needs a variable "
			     "for the duration ~a.") (nlg st 'pp))
	 *dyi*
	 (format nil "Define a variable for the duration ~a"
		 (nlg ct 'pp))))


;;; If duration is not needed on this problem, then say so.  Not
;;; handled by non-existent-variable because it doesn't have the (at
;;; ?descr ?time) format.  I can't find a problem to test this with.
(def-error-class non-existent-duration ()
  ((student (define-var (duration ?st)))
   (no-correct (define-var (duration ?ct)))))

(defun non-existent-duration ()
   (list "No solution I know of requires a duration variable."
	 '(function next-step-help)))


;;; Special case: I suppose some clown will try to define a duration
;;; with the time points reversed, but its rather unlikely.  Maybe a
;;; user interface confusion.
(def-entry-test reverse-duration (?t1 ?t2)
  :preconditions
  ((student (define-var (duration (during ?t1 ?t2))))
   (test (> ?t1 ?t2)))
  :apply no-match
  :state +incorrect+
  :hint (reverse-duration `(during ,?t1 ,?t2) `(during ,?t2 ,?t1))
  :order ((expected-utility . 0.1)))

(defun reverse-duration (st ct)
   (list (format nil (strcat "You've defined a variable ~a.  That "
			     "is, you've got the time points reversed.")
		 (nlg st 'pp))
	 (format nil "Try defining a duration variable ~a." 
		 (nlg ct 'pp))))

;;; ======== ===== coefficient of friction ========================
;;; student can get the wrong type (kinetic vs static)
(def-error-class wrong-type-coef-friction (?stype ?ctype)
  ((student (define-var (coef-friction ?body ?agent ?stype)))
   (correct (define-var (coef-friction ?body ?agent ?ctype)))))

(defun wrong-type-coef-friction (stype ctype)
   (list  "Think about what type of friction occurs in this problem."
         (strcat "Kinetic friction acts on an object as it moves with "
                 "respect to the surface exerting the friction force. "
                 "Static friction keeps an object at rest with respect to "
		 "a surface when it would otherwise move against it.")
	`(bottom-out (string ,(format NIL "Change the coefficient of friction type from ~a to ~a."
	        (adj stype) (adj ctype))))))

;; can also get order of arguments wrong

;;; ======== ===== defining energy variables ======================
;;; The kinetic and potential energies have a body slot and a time
;;; slot, so the only default cases for them are (a) wrong body
;;; (ignoring time), (b) wrong time (handled by wrong-time-variable)
;;; and (c) non-existent quantity (handled by non-existent-variable).
;;; For gravitational and spring potential energy, there are two body
;;; slots and a time slot, so the default cases for them, besides the
;;; ones handled by general code, are (a) potential energy but neither
;;; body is a planet nor a spring, (b) grav-energy or spring-energy
;;; but the body isn't one we need that kind of energy for.

;;; If the student defines a variable for a body for which we don't
;;; need an energy variable of any time, then point and bottom out.
;;; Favor interpretations where the times match.
(def-error-class wrong-body-energy (?energy-type ?sbody ?cbody)
  ((student (define-var (?energy-type ?sbody :time ?stime)))
   (test (member ?energy-type '(total-energy kinetic-energy)))
   (no-correct (define-var (?energy-type ?sbody :time ?time2)))
   (correct (define-var (?energy-type ?cbody :time ?ctime))))
  :probability
  (+ 0.1
     (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-body-energy (energy-type sbody cbody)
  (setq energy-type (nlg energy-type 'adj))
   (list (format nil "Are you sure it's ~a that you want ~a of?"
		 (nlg sbody 'def-np) energy-type)
	 (strcat "No solution I know needs that energy.")
	 *dyi*
	 `(bottom-out (string ,(format nil "Define ~a for ~a instead of ~a."
		 energy-type (nlg cbody 'def-np) (nlg sbody 'def-np))))))

;; if the order of the body and agent wrong, according to Andes convention.
(def-error-class wrong-order-potential-energy (?energy-type ?sbody ?sagent)
  ((student (define-var (?energy-type ?sbody ?sagent :time ?stime)))
   (test (member ?energy-type '(grav-energy spring-energy electric-energy)))
   (correct (define-var (?energy-type ?sagent ?sbody :time ?ctime))))
  :utility 10   ; higher, we want students to learn this
  :probability
  (+ 0.1 (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-order-potential-energy (energy-type sbody sagent)
  (setq energy-type (nlg energy-type 'adj))
   (list (format nil "Although interactions are symmetrical, Andes uses the convention that the body argument in the potential energy definition should be the primary body whose motion you are considering.   ")
	 `(bottom-out (string ,(format nil "Re-order the arguments in your definition to define the ~a due to the interaction of ~a and ~a."
		 energy-type (nlg sagent 'def-np) (nlg sbody 'def-np))))))

;;; If they have a wild body (ignoring agent and time).
;;; default utility, so wrong order should take precedence
;;; Also want to remind them of our definition convention.
(def-error-class wrong-body-potential-energy (?energy-type ?sbody ?cbody)
  ((student (define-var (?energy-type ?sbody ?agent :time ?stime)))
   (test (member ?energy-type '(grav-energy spring-energy)))
   (test (not (listp ?sbody)))  ; obsolete given WB/entry-API change
   (no-correct (define-var (?energy-type ?sbody ?agent2 :time ?stime2)))
   (correct (define-var (?energy-type ?cbody ?cagent :time ?ctime))))
  :probability
  (+ 0.1 (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-body-potential-energy (energy-type sbody cbody)
  (setq energy-type (nlg energy-type 'adj))
   (list (format nil "Are you sure ~a is the primary body for which to consider ~a in this problem?"
		 (nlg sbody 'def-np) (nlg energy-type 'adj))
	 (format nil "Andes uses the convention that the body argument in the potential energy definition should be the primary body whose motion you are considering. No solution I know chooses ~a as the primary body.  " 
			 (nlg sbody 'def-np))
	 *dyi*
	 `(bottom-out (string ,(format nil "You could define ~a using ~a instead of ~a in the body slot."
		 energy-type (nlg cbody 'def-np) (nlg sbody 'def-np))))))

(def-error-class wrong-agent-for-energy (?energy-type ?sagent ?cagent ?body)
  ((student    (define-var (?energy-type ?body ?sagent :time ?stime)))
   (test (member ?energy-type '(grav-energy spring-energy electric-energy)))
   (no-correct (define-var (?energy-type ?body ?sagent :time ?time2)))
   (correct    (define-var (?energy-type ?body ?cagent :time ?ctime)))))

(defun wrong-agent-for-energy (energy-type sagent cagent body)
   (list (format nil "Is ~a the agent of a conservative force on ~a that gives rise to ~a?"
		 (nlg sagent 'def-np) (nlg body 'def-np) (nlg energy-type 'adj))
	 `(bottom-out (string ,(format nil (strcat "Instead of defining ~a as due to interaction with ~a, you "
			     "should define it as due to ~a instead.")
	    (nlg energy-type 'adj) (nlg sagent 'def-np) (nlg cagent 'def-np))))))


;;; =============== defining compression distance =================
;;; The compression distance variable has two slots: the body and the
;;; time.  The only default cases, besides those handled by
;;; non-existent-variable and wrong-time-variable, is wrong object for
;;; the spring.  This is probably a user interface confusion, so its
;;; handled pretty bluntly.

(def-error-class wrong-spring-compression-distance (?sspring ?cspring)
  ((student (define-var (compression ?sspring :time ?stime)))
   (no-correct (define-var (compression ?sspring :time ?time2)))
   (correct (define-var (compression ?cspring :time ?ctime))))
  :probability
  (+ 0.1 (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-spring-compression-distance (sspring cspring)
   (list (strcat "The compression distance should be specified "
		 "for the object that is being compressed, such "
		 "as a spring.")
	 `(bottom-out (string ,(format nil (strcat "You should define the compression "
			     "distance of ~a instead ~a.")
		 (nlg cspring 'def-np) (nlg sspring 'def-np))))))

	    
;;; =============== defining spring constant ====================
;;; Either the student defines the spring constant for the wrong body,
;;; or the spring constant isn't needed on this problem.  This is
;;; probably a user interface confusion, so its handled pretty
;;; bluntly.

(def-error-class wrong-spring-for-spring-constant (?sspring ?cspring)
  ((student (define-var (spring-constant ?sspring)))
   (correct (define-var (spring-constant ?cspring)))
   (test (not (equal ?sspring ?cspring)))))

(defun wrong-spring-for-spring-constant (sspring cspring)
   (list (strcat "The spring constant should be specified for "
		 "the object that is being compressed, such as a spring.")
	 `(bottom-out (string ,(format nil "You should define the spring constant of ~a instead ~a."
		 (nlg cspring 'def-np) (nlg sspring 'def-np))))))


;;; I can't find a problem to test this.  Spring constant appears on
;;; the menu only when its relevant.
(def-error-class non-existent-spring-constant ()
  ((student (define-var (spring-constant ?ss)))
   (no-correct (define-var (spring-constant ?cs)))))

(defun non-existent-spring-constant ()
   '("No solution I know of requires using a spring constant."
     (function next-step-help)))


;;; =================== defining height ==========================
;;; There are only two slots, body and time, so in addition to the
;;; default cases handled by non-existent-variable and
;;; wrong-time-variable, we need only one default case, which is for
;;; the wrong body.  There is a special case for using a time interval
;;; rather than a time point. This variable is easily confused with
;;; displacement and the other spatial measures, so there are special
;;; cases in displacement and distance traveled to redirect the
;;; student to use height.  Here there could be special case to
;;; redirect them to use the others, but since height is available on
;;; the menu only when it is relevant, there is no way to trigger
;;; those special cases.

;;; default case of wrong body
(def-error-class wrong-body-for-height (?sbody ?cbody) 
  ((student (define-var (height ?sbody ?zero-height :time ?stime)))
   (no-correct (define-var (height ?sbody ?zero-height :time ?time2)))
   (correct (define-var (height ?cbody ?zero-height :time ?ctime))))
  :probability
  (+ 0.1
     (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-body-for-height (sbody cbody)
   (list (strcat "You should specify the height of the "
		 "body whose motion is being analyzed.")
	 `(bottom-out (string ,(format nil "Define the height of ~a instead of ~a."
		 (nlg cbody 'def-np) (nlg sbody 'def-np))))))

;;; special case: Students might think that height is relative, so
;;; they could define it using a time interval.  Andes doesn't use
;;; height like that, and should say so.  In order to get the best
;;; bottom out hint, it tries to figure out which time point the
;;; student was trying to refer to by assuming that the other time
;;; point in the interval must be the zero point in from the problem
;;; statement.  This is a bit risky, because the problem statement
;;; might not specify a zero point, in which case this entry-test
;;; will fail.
(def-entry-test height-over-a-time-interval (?body ?t1 ?t2 ?ctime)
  :preconditions
  ((student (define-var (height ?body ?zero-height :time (during ?t1 ?t2))))
   (correct (define-var (height ?body ?zero-height :time ?ctime)))
   (test (time-pointp ?ctime))
   (problem (given (height ?body ?zero-height :time ?t-zero) (dnum 0 ?unit))))
  :apply no-match
  :state +incorrect+
  :hint (height-over-a-time-interval ?body `(during ,?t1 ,?t2) ?ctime)
  :order ((expected-utility . (* 100 (+ 0.1
					(if (and (equal ?t1 ?t-zero)
						 (equal ?t2 ?ctime))
					    0.2 0.0)
					(if (and (equal ?t2 ?t-zero)
						 (equal ?t1 ?ctime))
					    0.2 0.0))))))


(defun height-over-a-time-interval (body stime ctime)
  (list   ;; height-at-points
   (strcat "Height is defined relative to the zero of "
	   "gravitational potential energy, which is usually "
	   "specified in the problem statement (if not, you "
	   "should specify it).  Thus, you always define height "
	   "at a time point, and that refers to its vertical "
	   "distance above or below the zero point.  Thus, you "
	   "should modify your variable definition to use a time "
	   "point rather than a time interval.")
   (format nil "Define the height of ~a ~a instead of ~a."
	   (nlg body 'def-np) (nlg ctime 'pp) (nlg stime 'pp))))


;;; ============= moment of inertia ==============================
;;; The slots on the moment of inertia tool are: body and, optinally, time.
;;; Thus, the default cases, other than those handled by
;;; non-existent-variable and wrong-time-variable, is just picking the
;;; wrong body (ignoring time).

;;; default case: picking the wrong body.  Test this with momr4a.
(def-error-class wrong-body-moment-of-inertia (?sbody ?cbody)
  ((student (define-var (moment-of-inertia ?sbody :axis ?axis :time ?t)))
   (no-correct (define-var (moment-of-inertia ?sbody :axis ?any-axis :time ?t)))
   (correct (define-var (moment-of-inertia ?cbody :axis ?axis :time ?t))))
  :probability
  (+ 0.3))

(defun wrong-body-moment-of-inertia (sbody cbody)
   (list (format nil "Are you sure you need the moment of inertia of ~a?"
		 (nlg sbody 'def-np))
	 *dyi*
	 `(bottom-out (string ,(format nil "Define the moment-of-inertia for ~a instead of ~a."
		 (nlg cbody 'def-np) (nlg sbody 'def-np))))))

;;; ==================== radius of revolution ============================
;;; There are two slots on the radius variable tool: body and time, except 
;;; that none of our problems have time points so the time slot never
;;; appears.  Thus, the default cases, other than those handled by
;;; non-existent-variable and wrong-time-variable, is just picking the
;;; wrong body (ignoring time)

;;; default case: picking the wrong body.  This error probably only
;;; occurs when there is another circular object, such as a race
;;; track, that has the desired radius.  Thus, the hint indicates
;;; Andes' convention, which is to use the object that is moving.
;;; Test this with Exkr7a for laughs.
(def-error-class wrong-body-radius-revolution (?sbody ?cbody ?ctime)
  ((student (define-var (revolution-radius ?sbody :time ?stime)))
   (no-correct (define-var (revolution-radius ?sbody :time ?time2)))
   (correct (define-var (revolution-radius ?cbody :time ?ctime))))
  :probability
  (+ 0.1 (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-body-radius-revolution (sbody cbody ctime)
  (declare (ignore ctime))    ; until problems augmented to include time
  (setq sbody (nlg sbody 'def-np))
   (list (format nil "Are you sure you need the radius of ~a?" sbody)
	 (format nil (strcat "Choose an object that is moving in a circle.  "
			     "Even if ~a has the same radius, it's really the"
			     " radius of the motion that matters.") sbody)
	 `(bottom-out (string ,(format nil (strcat "Because ~a is moving in a circle, define the "
			     "radius of its motion instead of ~a.")
		 (nlg cbody 'def-np) sbody)))))

;;; =============== defining the radius of an object =================
;;; The radius drawing tool and the define variable > radius both
;;; end up at the same dialog box, which defines either a revolution
;;; radius (see above) of the radius of an object (this case).  There
;;; is just the body slot.  

(def-error-class wrong-body-radius-of-circle (?sbody ?cbody)
  ((student    (define-var (radius-of-circle ?sbody)))
   (no-correct (define-var (radius-of-circle ?sbody)))
   (correct    (define-var (radius-of-circle ?cbody))))
  :probability 0.303  )

(defun wrong-body-radius-of-circle (sbody cbody)
   (list (format nil "Are you sure it's ~a that you want the radius of?"
		 (nlg sbody 'def-np))
	 `(bottom-out (string ,(format nil "Define a variable for the radius of ~a."
		 (nlg cbody 'def-np))))))

(def-error-class wrong-kind-of-radius (?body)
  ((student    (define-var (radius-of-circle ?body)))
   (no-correct (define-var (radius-of-circle ?body)))
   (correct    (define-var (revolution-radius ?body :time ?time))))
  :probability 0.3141  )

(defun wrong-kind-of-radius (body)
   (list (format nil "Since we are describing the motion of ~A, use the radius of motion."
		 (nlg body 'def-np))))

;;; =============== length  and width ========================
;;; There should be two slots on the length and width variable tools: body and
;;; time, except that none of our problems have time points so the
;;; time slot never appears.  Thus, the default cases, other than
;;; those handled by non-existent-variable and wrong-time-variable, is
;;; just picking the wrong body (ignoring time).  There is one special
;;; case, namely, using the length where relative position is the
;;; correct quantity to use.

;;; default case: picking the wrong body.  I haven't found a problem
;;; yet that requires length or width, so this is untested.  Moreover, I am not
;;; sure why the student might choose the wrong body, so the
;;; remediation here is rather generic.
(def-error-class wrong-body-length-width (?type ?sbody ?cbody ?ctime)
  ((student (define-var (?type ?sbody :time ?stime)))
   (test (or (equal ?type 'length) (equal ?type 'width)))
   (no-correct (define-var (?type ?sbody :time ?time2)))
   (correct (define-var (?type ?cbody :time ?ctime))))
  :probability
  (+ 0.1 (if (equal ?stime ?ctime) 0.2 0.0)))

(defun wrong-body-length-width (type sbody cbody ctime)
  (declare (ignore ctime))	; until problems augmented to include time
  (setq sbody (nlg sbody 'def-np))
  (setq type (nlg type 'lower-case))
   (list (format nil "Are you sure you need the ~a of ~a?" type sbody)
	 `(bottom-out (string ,(format nil "Define the ~a of ~a instead of ~a." 
		 type (nlg cbody 'def-np) sbody)))))

;;; On torque problems with bars, if the pivot is at one end of the
;;; bar, then the bar's length has the same value as the position of
;;; free end of the bar relative to the fixed end of the bar. It's an
;;; Andes convention to choose just the relative position in that
;;; case.  Although it is possible to add a (problem (rotation-axis
;;; ?object ?pivot))) condition and then boost probability if (equal
;;; ?object ?sbody), this would make the error class apply only to
;;; rotational problems.  I've generalized this to cover width,
;;; although that may never occur.
(def-error-class length-should-be-rel-pos (?type ?point ?ref-point)
  ((student (define-var (?type ?sbody :time ?stime)))
   (test (or (equal ?type 'length) (equal ?type 'width)))
   (correct (vector (relative-position ?point ?ref-point :time ?ctime) ?cdir)))
  :utility 100
  :probability
  (+ 0.1
     (if (equal ?stime ?ctime) 0.1 0.0)))


(defun length-should-be-rel-pos (type point ref-point)
   (list
    (format nil (strcat "This problem should be solved by "
			"drawing relative position vectors.  "
			"Although the ~a you've defined might "
			"have the same value as the magnitude "
			"of a relative position, it is an Andes "
			"convention to use quantities that appear "
			"in the principles whenever possible.")
	    (nlg type 'lower-case))
    (format nil (strcat "Try drawing, for instance, the relative "
			"position of ~a with respect to ~b.")
	    (nlg point 'def-np) (nlg ref-point 'def-np))))

;;; ===================== defining work ============================
;;; The work variable tool has slots for the body, the agent and the
;;; time.  The general code handles wrong times.  Thus, the default
;;; causes here are (a) wrong body (ignoring agent) and (b) wrong
;;; agent.

;;; If the student gets the body wrong, then they'd better get next
;;; step help.  This should also handle the case where they get the
;;; body and agent switched.
(def-error-class wrong-body-for-work (?sbody ?cbody ?cagent)
  ((student    (define-var (work ?sbody ?sagent :time ?stime)))
   (no-correct (define-var (work ?sbody ?agent2 :time ?time2)))
   (correct    (define-var (work ?cbody ?cagent :time ?ctime)))))

(defun wrong-body-for-work (sbody cbody cagent)
   (list (format nil (strcat "You can define a variable for the work "
			     "on any object, but only some of these "
			     "variables are useful in solving the problem.  "
			     "Are you sure you need ~a to have a work "
			     "variable defined for it?") (nlg sbody 'def-np))
	 *dyi*
	 `(bottom-out (string ,(format nil "Try defining the work on ~a done by ~a."
		 (nlg cbody 'def-np) (nlg cagent 'def-np))))))

;;; If the student gets the body right but the agent wrong, then 
;;; teach when work gets done.
(def-error-class wrong-agent-for-work (?sagent ?cagent ?body)
  ((student    (define-var (work ?body ?sagent :time ?stime)))
   (no-correct (define-var (work ?body ?sagent :time ?time2)))
   (correct    (define-var (work ?body ?cagent :time ?ctime)))))

(defun wrong-agent-for-work (sagent cagent body)
   (list (format nil "Are you sure that ~a does work on ~a?"
		 (nlg sagent 'def-np) (nlg body 'def-np))
	 ;; when-is-work-done
	 (strcat "An object does work on a moving body when it exerts a "
		 "force on the body and the force is not perpendicular "
		 "to the object's motion.")
	 `(bottom-out (string ,(format nil (strcat "Instead of defining the work done by ~a, you "
			     "could try defining the work done by ~a instead.")
	    (nlg sagent 'def-np) (nlg cagent 'def-np))))))

;;; ======================= net work ==============================
;;; There are two slots: the body and the time.  The general code
;;; handles wrong time, so the only default case is wrong body.  

;;; Default case of wrong body -- just like wrong body for individual
;;; work variables.  Test with Exwork3a or Exwork3b.
(def-error-class wrong-body-net-work (?sbody ?cbody)
  ((student    (define-var (net-work ?sbody :time ?stime)))
   (no-correct (define-var (net-work ?sbody :time ?time2)))
   (correct    (define-var (net-work ?cbody :time ?ctime)))))

(defun wrong-body-net-work (sbody cbody)
   (list (strcat "You can define a variable for the net work (the "
		 "work done by all the forces) on any body.  However, "
		 "when solving the problem, the net work of only some "
		 "bodies is useful.")
	 *dyi*
	 `(bottom-out (string ,(format nil "Try defining the net work done on ~a instead of on ~a."
		 (nlg cbody 'def-np) (nlg sbody 'def-np))))))


;;; ===================== optics variables ============================
;;;
;;; object-distance and image distance can't be used when object or image
;;; is at infinity. 
;;;
(def-error-class object-distance-infinite (?lens)
  ((student    (define-var (object-distance ?lens)))
   (problem    (infinite (object-distance ?lens))))
  :utility 50 
  )
(defun object-distance-infinite (lens)
  (declare (ignore lens))
    (list (strcat "When the object is at infinity (very far away), you "
                  "cannot use a value for object distance in your equations.  "
		  "In this case you can write the equation in a simpler form "
		  "by dropping the 1/do term, which is zero.")
	 '(function next-step-help)))

(def-error-class image-distance-infinite (?lens)
  ((student    (define-var (image-distance ?lens)))
   (problem    (infinite (image-distance ?lens))))
  :utility 50 
   )

(defun image-distance-infinite (lens)
  (declare (ignore lens))
    (list (strcat "When the outgoing rays are parallel, the image is 'at infinity' "
                  "and you cannot use a value for image distance in your equations. "
		  "In this case you can write the equation in a simpler form "
		  "by dropping the 1/di term, which is zero.")
	 '(function next-step-help)))

;;; ============= drawing a body ==================================
;;; There are two special case: (1) handling ambiguity in the choice
;;; of body, and (2) teaching about massless bodies.  There are 4
;;; mutually exclusive and exhausive default error classes: (a) time
;;; wrong but object right (b) object wrong but time right and (c)
;;; both body and time wrong, and (d) no body needed (can case d
;;; really occur?).

;;; If there are multiple correct bodies, then we usually can't tell
;;; which body the student should enter.  Thus, this error class
;;; handles the general case of multiple bodies, and has a higher
;;; utility than others that assume they know what correct body the
;;; student should enter.  However, if the contextualizer were smarter
;;; (and the use of two 'correct' conditions set the interpretation to
;;; indicate ambiguity), then it might decide that
;;; the other guy's interpretations were much more likely than this
;;; one, and thus given them higher expected utility.  For now, the
;;; high utility gives this error class priority over the others when
;;; it applies.
(def-error-class body-tool-multiple-correct ()
  ((student (body ?wrong-body))
   (correct (body ?body1))
   (no-student (body ?body1))
   (correct (body ?body2))
   (no-student (body ?body2))
   (test (not (equal ?body1 ?body2))))
  :utility 100)

(defun body-tool-multiple-correct ()
   (list (strcat "There are several useful choices of body, and it's not "
		 "clear which one you intended, because that depends on your "
		 "overall strategy for solving the problem.")
	 '(function next-step-help)))


;;; If the student chooses a body that has mass, but the correct body
;;; is massless, then suggest that bodies don't have to have mass,
;;; then teach, then bottom out.  This has higher utility than the
;;; less specific error classes for bodies.  Currently, there is no
;;; problem where a massless object is the only correct body, so this
;;; error handler has not been tested.
(def-error-class massless-body (?correct-body ?wrong-body)
  ((student (body ?wrong-body))
   (no-correct (body ?wrong-body))
   (correct (body ?correct-body))
   (problem (massless ?correct-body))
   (problem (given (mass ?wrong-body) ?dontcare)))
  :Utility 50)

(defun massless-body (correct-body wrong-body)
  (setq correct-body (nlg correct-body 'def-np))
  (setq wrong-body (nlg wrong-body 'def-np))
   (list "It is okay to choose a massless object as the body."
	 (strcat "Choose a body that fits your strategy even if it doesn't "
		 "have mass.")
	 `(bottom-out (string ,(format nil (strcat "Although ~a has mass, it is not a good choice "
			     "for the body.  Analyze the motion of ~a "
			     "instead.") wrong-body correct-body)))))

;; the student draws a body for an object but another object needs defining
(def-error-class wrong-object-for-body-tool (?correct-body)
  ((student (body ?wrong-body . ?wrest))
   (correct (body ?correct-body . ?crest))
   (no-student (body ?correct-body . ?crest)))
  :probability 0.5)

(defun wrong-object-for-body-tool (correct-body)
  (setf correct-body (nlg correct-body 'def-np))
   (list (strcat "What other bodies can you define?")
	 (format nil (strcat "Analyzing ~a is more important for solving " 
			     "this problem.") correct-body)
	 `(bottom-out (string ,(format nil "Choose ~a as the body." correct-body)))))

;; the student draws a body, but it doesn't need defining.
(def-error-class extra-object-for-body-tool (?wrong-body)
  ((student (body ?wrong-body . ?wrest))
   (correct (body ?correct-body . ?crest)))
  :probability 0.1  ;more general than above
  )

(defun extra-object-for-body-tool (wrong-body)
  (setf wrong-body (nlg wrong-body 'def-np))
   (list (format nil (strcat 
		      "You don't need to draw ~A to solve this problem.  "
		      "You should focus your attention on the other body "
		      "(or bodies).") wrong-body)))

;;; I suppose it might someday be possible to have a solution that
;;; doesn't include a body, such as a W=mg problem, but I think none
;;; exist right now.  If so, this error class handles the case of
;;; drawing a body when none are needed.
(def-error-class non-existent-body ()
  ((student (body ?sbody . ?wrest))
   (no-correct (body ?cbody . ?crest)))
  :probability 0.01)

(defun non-existent-body ()
   (list (strcat "This problem doesn't require a body to be drawn.")
	 '(function next-step-help)))


;;; ================ coordinate axes ============================
;;; Although there are currently some extra slots on the axis tool,
;;; the only one that matters is the rotation.  There are 2 default
;;; cases: (a) wrong rotation, and (b) no axes needed.  There are two
;;; special cases: (1) drawing an unrotated axis when only rotated
;;; ones are correct, and (2) drawing rotated axes for projectile
;;; motion.

;;; The default case is just an axis that is rotated but not
;;; correctly.  Although it could be a slip, such as forgetting to
;;; adjust the angle box after drawing the axis, it could also be that
;;; the student needs to be taught the heuristics for axis rotation.
;;; It is not wise to suggest an axis rotation, because we don't know
;;; th emost efficient path, so we just end up with next step help.
(def-error-class wrong-axis-rotation (?srot)
  ((student (draw-axes ?srot))
   (correct (draw-axes ?crot))
   (test (not (equal ?srot ?crot)))
   (test (not (zerop ?srot)))))

(defun wrong-axis-rotation (srot)
  (declare (ignore srot))
    (append
      (list (strcat "Although you could in principle solve the problem with any "
                     "rotation of the axes, that is not a useful choice "
		     "for this problem. "))
	 ;; delegate to next-step-help hints on the operator, which can say something
	 ;; about why this axis was chosen
         (sg-map-systemEntry->hints *correct-entry*)))
#|
   (list (format nil "Are you sure you want the axes rotated by ~a degrees?" srot)

	 (strcat "Although you could in principle solve the problem with any "
		 "rotation of the axes, the math is much simpler if you follow "
		 "two general rules: (1) If the problem seeks a vector quantity, "
		 "such as acceleration, then rotate the axes so one axis is "
		 "parallel to the sought vector. (2) Otherwise, rotate the axes "
		 "so that as many vectors as possible are parallel to axes.")
	 '(function next-step-help))
|#

;;; This error class handles cases, such as the sdd problems, where
;;; the student tries to draw axes when none are needed.
(def-error-class non-existent-axes ()
  ((student (draw-axes ?srot))
   (no-correct (draw-axes ?crot))))

(defun non-existent-axes ()
   (list (strcat "Although it is always okay to draw a coordinate system, "
		 "none are needed for this problem.")
	 '(function next-step-help)))

;;; Special case: If the student draws unrotated axes when rotated ones are 
;;; correct, then teach about rotated axes.
;;; AW -- this probably never occurs now, std axes should always be OK
(def-error-class need-to-rotate-axes (?rot)
  ((student (draw-axes 0))
   (correct (draw-axes ?rot))
   (test (not (equal ?rot 0))))
  :utility 100)

(defun need-to-rotate-axes (rot)
   (list "Try a rotated coordinate system."
	 ;; Rotated-axes
	 (strcat "Rotating the coordinate system can simplify the math.  "
		 "If you are seeking a vector quantity, then you should "
		 "rotate the coordinate axes so that one of them is parallel "
		 "to the sought vector.  If  you are seeking a scalar quantity, "
		 "then you should rotate the axes to maximize the number of "
		 "vectors that are parallel to axes.")
	 `(bottom-out (string ,(format nil (strcat "You should rotate the axes by ~a degrees.&nbsp; To "
			     "do so, double click on the axes and change the angle.") 
				       rot rot)))))


;; ================================================================
;;  This section covers all vector-drawing entries
;; ================================================================

;;; =================== default cases for vectors ==================
;;; There are 3 slots common to all vectors: body, time and direction.
;;; The error classes below handle defaults for all 8 combinations.
;;; The combinations are grouped as follows (1) wrong direction, (2)
;;; wrong time, (2) wrong time and direction, (3) wrong body 
;;; (4) wrong body ignoring time and direction [wild body case only] 
;;; (5) no such vector.  
;;; The wrong-time default has a special case.  There are lots of special 
;;; case for all the defaults covered in the sections on specific vector 
;;; types (e.g., if the vector is a normal force drawn straight up when 
;;; it should be tilted...)

;;; (1) mismatches in direction:
;;; NB!! Here and throughout must remember that vector direction slot
;;; can contain 'zero, 'unknown, 'into, 'out-of, 'z-unknown, not just dnum. 
;;; So direction slot mistmatch can come from zero/non-zero error or 
;;; known/unknown error. Need several defaults under (1) to give special 
;;; messages for these.

;;; If the student draws a zero length vector when it should be non-zero,
;;; or vice versa, prompt then bottom out.  Tests should ensure that 
;;; default-wrong-dir doesn't apply on zero/non-zero error so leave with 
;;; default utility, lower than any special case zero/non-zero errors.
(def-error-class default-should-be-zero ()
  ((student (vector ?descr ?dir))
   (correct (vector ?descr zero))
   (test (not (equal ?dir 'zero))))
  ;; increase so hints for d01 & d06 in kgraph9 come out right.
  :probability 0.5  
  :utility 10
  )

(defun default-should-be-zero ()
   (append
      (list "Are you sure that vector has a non-zero magnitude?")
      ;; no bottom-out hint yet.
      (sg-map-systemEntry->hints *correct-entry*)
	 ))

(def-error-class default-should-be-non-zero ()
  ((student (vector ?descr zero))
   (correct (vector ?descr ?dir))
   (test (not (equal ?dir 'zero)))))

(defun default-should-be-non-zero ()
   (append (list "Do you really want that vector to have a zero magnitude?"
		 ;; don't give bottom-out hint prematurely
	         ;; "It should have a non-zero magnitude." 
		 )
           ;; delegate to operator hints to explain correct magnitude.
	   (sg-map-systemEntry->hints *correct-entry*)
	 ))

;;; (ref Pitt non-eqn 4-17) If the student's vector is correct except
;;; for the angle, then just point that out.  This is the default case.
(def-error-class default-wrong-dir-vector (?wrong-dir ?correct-dir)
  ((student (vector ?descr ?wrong-dir))
   (correct (vector ?descr ?correct-dir))
   (test (not (equal ?wrong-dir 'zero))) ; either zero handled above
   (test (not (equal ?correct-dir 'zero)))
   (test (not (equal ?correct-dir 'unknown))) ; should-be-unknown above
   (test (not (equal ?wrong-dir ?correct-dir))))
  :utility 50
  ;; Low probability since we want any quantity-specific rules to act first
  :probability 0.08)

(defun default-wrong-dir-vector (wrong-dir correct-dir)
  (default-wrong-dir "vector" wrong-dir correct-dir))

(defun default-wrong-dir-line (wrong-dir correct-dir)
  (default-wrong-dir "line" wrong-dir correct-dir))

(defun default-wrong-dir (object wrong-dir correct-dir)
  (declare (ignore correct-dir))
   (append
     (list (format nil "Do you really want the direction of that ~A to be ~A?"
		 object (nlg wrong-dir 'adj))
	   ;; Don't give this bottom-out hint prematurely
	   ;; (format nil "It should be ~a." (nlg correct-dir 'adj))
	   )
     ;; delegate to the operator hints to explain correct direction.
     (sg-map-systemEntry->hints *correct-entry*)))

;;; !!! want special case message if drawn in the plane when should 
;; be into/out-of the plane, explaining this and how to do it.
;; Converse error should be rare since they know how to draw vectors in
;; the plane by the time they get to problems that utilize the z axis.

;;; There have been lots of cases where students supply the wrong time
;;; specification for a vector, and they all turned out to be due to
;;; confusion on what the overall problem solving plan should be.
;;; This is not so unreasonable, as the students may not have a plan
;;; early on when vectors are being drawn.  So the WW help is just to
;;; point, then bottom out.
(def-error-class vector-time (?descr ?bad-time ?good-time)
  ((student (vector (?descr . ?b1) ?dir))
   (correct (vector (?descr . ?b2) ?dir))
   (bind ?bad-time (time-of ?b1))
   (bind ?good-time (time-of ?b2))
   (test (equal (remove-time ?b1) (remove-time ?b2)))
   (test (not (equal ?bad-time ?good-time))))
  :utility 25 ; made half as big for good hint for d06 in kgraph9
  ;; Low probability since we want any quantity-specific rules to act first
  :probability 0.085)

(defun vector-time (descr bad-time good-time)
  (setf descr (nlg descr 'def-np))
  (setf bad-time (nlg bad-time 'pp))
  (setf good-time (nlg good-time 'pp))
   (list (format nil (strcat "You don't need to draw ~a ~a.  You need to draw "
			     "it for a different time. ") 
			     descr bad-time)
	 `(bottom-out (string ,(format nil "You should draw ~a ~a." 
				       descr good-time)))))

;;; (ref Pitt non-eqn 2-29-27) If the student defines a vector only
;;; for a time inside the time interval of the correct system entry,
;;; then explain that the time interval of the vectors should match
;;; the time intervals of the whole analysis.  This has somewhat
;;; higher utility than the default wrong-time error class.
(def-error-class vector-time-inside-correct-time (?descr ?wrong-time ?correct-time)
  ((student (vector (?descr . ?b1) ?dir))
   (correct (vector (?descr . ?b2) ?dir))
   (bind ?wrong-time (time-of ?b1))
   (bind ?correct-time (time-of ?b2))
   (test (equal (remove-time ?b1) (remove-time ?b2)))
   (test (and (tinsidep-include-endpoints  ?wrong-time ?correct-time)
	      (not (equal ?wrong-time ?correct-time)))))
  :utility 10)

(defun vector-time-inside-correct-time (descr wrong-time correct-time)
  (setf descr (nlg descr 'def-np))
  (setq wrong-time (nlg wrong-time 'pp))
  (setq correct-time (nlg correct-time 'pp))
   (list (format nil (strcat "Although ~a certainly exists ~a, you want to "
			     "analyze its motion ~a.")
		 descr wrong-time correct-time correct-time)
	 `(bottom-out (string ,(format nil "Define ~a ~a instead of ~a."
		 descr correct-time wrong-time )))))

;;; default for wrong time and direction.  This is less likely than
;;; just one of them being wrong.  The low probability is not
;;; necessary for the logic, as the no-correct conditions will prevent
;;; this from firing when only one of the two slots is wrong.
(def-error-class vector-wrong-time-and-direction (?descr ?stime ?ctime ?sdir ?cdir)
  ((student (vector (?descr . ?b1) ?sdir))
   (bind ?stime (time-of ?b1))
   (correct (vector (?descr . ?b2) ?cdir))
   (bind ?ctime (time-of ?b2))
   (test (unify (remove-time ?b1) (remove-time ?b2)))
   (no-correct (vector (?descr . ?b1) ?dir2))
   (bind ?b4 (set-time ?b1 '?anytime))  ;substitute in variable
   (no-correct (vector (?descr . ?b4) ?sdir)))
  :probability 0.01)

(defun vector-wrong-time-and-direction (descr stime ctime sdir cdir)
   (list (strcat "Both the time specification and the direction of "
		 "that vector look odd to me.")
	 (format nil (strcat "None of the solutions that I know of "
			     "contain a vector for ~a at that "
			     "time or in that direction.") (nlg descr))
	 (strcat "If you are unsure of what to do you can call "
		 "Next-Step-Help and it will guide you in "
		 "defining a solution.")
	 *dyi*
	 `(bottom-out (string ,(format nil (strcat "You should draw ~a ~a with a ~a direction, "
			     "rather than ~a with a ~a direction.")
		 (nlg descr 'def-np) (nlg ctime 'pp) (nlg cdir 'adj) 
		 (nlg stime 'pp) (nlg sdir 'adj))))))

;;; default in case everything but body matches some correct vector
;;; high probability since close match
(def-error-class default-wrong-body (?cbody ?sbody)
 ((student (vector (?vector-type ?sbody . ?sargs) ?sdir))
   (correct (vector (?vector-type ?cbody . ?sargs) ?sdir))
   (test (not (equal ?sbody ?cbody)))
   ; first arg not a body for following vector types
   (test (not (eq ?vector-type 'field)))
   (test (not (eq ?vector-type 'unit-vector))))
  :probability 0.5)

(defun default-wrong-body (correct-body wrong-body)
 (setf correct-body (nlg correct-body 'def-np))
  (setf wrong-body (nlg wrong-body 'def-np))
   (list (format nil "Are you sure you want the body on that vector to be ~a?" 
		 wrong-body)
	 `(bottom-out (string ,(format nil "Maybe the body should be ~a." correct-body)))))

;;; default error handler for wrong body choice, such that no correct
;;; vector on that body exists in the solution. Ignores time.  Ignores the
;;; other arguments if any.  Ignores the direction.  However, in order
;;; to better select a correct body, boosts the probability when the
;;; ignored slots match. Note this only handles case of "wild" body
;;; choice such that no correct vector with this body. 
(def-error-class default-vector-body (?cbody ?sbody)
  ((student (vector (?vector-type ?sbody . ?sargs) ?sdir))
   (no-correct (vector (?vector-type ?sbody . ?args2) ?dir2))
   (correct (vector (?vector-type ?cbody . ?cargs) ?cdir))
   ; first arg not a body for the following vector types:
   (test (not (eq ?vector-type 'field)))
   (test (not (eq ?vector-type 'unit-vector))))
  :probability
  (+ 0.1 
     (if (equal ?sargs ?cargs) 0.2 0.0)
     (if (equal ?sdir ?cdir)   0.1 0.0)))

(defun default-vector-body (correct-body wrong-body)
  (setf correct-body (nlg correct-body 'def-np))
  (setf wrong-body (nlg wrong-body 'def-np))
   (list (format nil "Are you sure you want to define a vector for ~a?" 
		 (def-np wrong-body))
	 `(bottom-out (string ,(format nil (strcat "A better choice of body (but maybe not the "
			     "only one) would be ~a.") 
		 (def-np correct-body))))))


;;; On some problems, the student's vector just doesn't appear in any solution.
;;; There are two special cases for this below.
(def-error-class default-non-existent-vector (?vector-type)
  ((student (vector (?vector-type . ?sargs) ?sdir))
   (no-correct (vector (?vector-type . ?cargs) ?cdir))
   ; make sure net variant also unused, so message below makes sense. 
   ; cf. non-existent-variable
   (bind ?net-vector-type (get-net-quant-name ?vector-type)) ; may be NIL if none 
   (no-correct (vector (?net-vector-type . ?cargs2) ?cdir2)))
  :probability 0.001)

(defun default-non-existent-vector (type)
   (list (format nil "None of the solutions that I know include a ~a vector." 
		 (nlg type 'adj))
	 '(function next-step-help)))


;; We don't use a generic should-be-net-vector ala should-be-net-variable, because:
;;   1. There are already special handlers for net-force/individual force errors.
;;   2. Torque and fields define net forms in different ways, so message has to 
;;      be different

;;; Although the KB describes net-force and single force vectors differently
;;; they are both force vectors as far as the student is concerned.  Therefore
;;; it would be inappropriate to tell the student that there are no force 
;;; vectors in the problem solution.  
(def-error-class default-non-existent-force-vector (?type)
  ((student (vector (force ?body ?agent ?type . ?s-args) ?sdir))
   (no-correct (vector (force ?body . ?n-args) ?ndir))
   (correct (vector (net-force . ?net-args) ?net-dir)))
  :probability 0.002)

(defun default-non-existent-force-vector (type)
   (list (format nil (strcat "None of the solutions that I know include an ~a "
			     "vector.  They do, however, include a net force "
			     "vector.  ") (nlg type 'adj))
	 '(function next-step-help)))





;;; ==================== line drawing ===============================



;;; The student's line doesn't appear at any angle
(def-error-class default-non-existent-line (?sline)
  ((student (draw-line ?sline ?sdir))
   (no-correct (draw-line ?cline ?cdir)))
  :probability 0.01)

(defun default-non-existent-line (wrong-line)
  (setf wrong-line (nlg wrong-line 'def-np))
   (list (format nil "You don't need to draw any lines to solve this problem.")
	 ))

;;; If the student's line is correct except
;;; for the angle, then just point that out.  This is the default case.
(def-error-class default-wrong-dir-line (?wrong-dir ?correct-dir)
  ((student (draw-line ?descr ?wrong-dir))
   (correct (draw-line ?descr ?correct-dir))
   (test (not (equal ?correct-dir 'unknown))) ; should-be-unknown above
   (test (not (equal ?wrong-dir ?correct-dir))))
  :probability 0.5)

;;; default in case everything but body matches some correct line
;;; high probability since close match
(def-error-class default-wrong-line (?cline ?sline)
 ((student (draw-line ?sline ?sdir))
   (correct (draw-line ?cline ?cdir))
   (test (not (equal ?sline ?cline))))
  :probability 0.1)

(defun default-wrong-line (correct-line wrong-line)
 (setf correct-line (nlg correct-line 'def-np))
  (setf wrong-line (nlg wrong-line 'def-np))
   (list (format nil "Are you sure you want to draw ~a?" 
		 wrong-line)
	 `(bottom-out (string ,(format nil "You should draw ~a." correct-line)))))


;;; ==================== velocity drawing ===========================
;;; The default cases (wrong body, wrong time, wrong direction) are
;;; handled by the general vector code, so this is all special cases.

(def-error-class speed-not-velocity (?body)
  ((student    (vector (velocity ?body :time ?wrong-time) ?dir))
   (no-correct (vector (velocity ?body :time ?time2)      ?dir2))
   (correct (define-var (speed ?body :time ?time))))
  :utility 100)

(defun speed-not-velocity (body)
  (setf body (nlg body 'def-np))
   (list "On this problem, you should use speed instead of velocity."
	 (format nil "Delete the velocity vector and define a speed variable for ~a." 
		 body)))

;;; If the student draws a zero velocity when it should be non-zero,
;;; then just prompt and bottom out.  This has slightly higher utility
;;; than the default wrong-direction error class.
(def-error-class velocity-should-be-non-zero (?body ?time)
  ((student (vector (velocity ?body :time ?time) zero))
   (correct (vector (velocity ?body :time ?time) ?dir))
   (test (not (equal ?dir 'zero))))
  :utility 10)

(defun velocity-should-be-non-zero (body time)
  (setf body (nlg body 'def-np))
  (setf time (nlg time 'pp))
  (list (format nil "Is ~a at rest ~a?" body time)
	 `(bottom-out (string ,(format nil (strcat "Since ~a is not at rest ~a, the "
			     "velocity vector needs to be non-zero.") 
		 body time)))))


;;; If the student draws a non-zero velocity vector when it should be
;;; zero, then first prompt, then teach that its okay to assume
;;; at-rest by default, then do a bottom out, then instruct them on
;;; how to draw a zero-length vector.  This has higher utility than
;;; the default wrong-direction error class.
(def-error-class velocity-should-be-zero (?body ?time)
    ((student (vector (velocity ?body :time ?time) ?dir))
     (test (not (equal ?dir 'zero)))
     (correct (vector (velocity ?body :time ?time) zero)))
    :utility 25)

(defun velocity-should-be-zero (body time)
  (setf body (nlg body 'def-np))
  (setf time (nlg time 'pp))
   (list (format nil "Are you sure the velocity of ~a is non-zero ~a?" body time)
	 (format nil "Because ~a is at rest ~a, its velocity ~a is zero." 
		 body time time)
	 `(bottom-out (string ,(strcat "Click on this vector and change its length to be zero.&nbsp; "
		 "The vector will be drawn as a circle to indicate it has zero length.")))))

;;; ====================== drawing acceleration ====================
;;; the default cases are handled by the general vector error classes,
;;; so these are all special cases.

;;; If the body is moving in a straight line and slowing down, and the
;;; student draws the acceleration in the opposite direction, then the
;;; need a lesson on deceleration.  Common misconception.
(def-error-class deceleration-bug (?body ?sdir ?cdir)
  ((student (vector (accel ?body :time ?time) ?sdir))
   (test (degree-specifierp ?sdir)) ; not 'zero 'unknown or other atom 
   (bind ?cdir (opposite ?sdir))
   (correct (vector (accel ?body :time ?time) ?cdir))
   (problem (motion ?body straight :dir ?sdir :accel ?cdir 
		    :time ?time)))
  :utility 100)

(defun deceleration-bug (body sdir cdir)
  (setq body (nlg body 'def-np))
   (list (format nil "Notice that ~a is slowing down." body)
	 ;; Deceleration
	 (strcat "When an body is moving in a straight line and slowing "
		 "down, its acceleration is in the opposite direction from "
		 "its velocity.  That is, it opposes the velocity, reducing "
		 "it, and thus slowing the body down.")
	 `(bottom-out (string ,(format nil "Make the direction of the acceleration be ~a instead of ~a."
		 (nlg cdir 'adj) (nlg sdir 'adj))))))


;;; =================== drawing displacement ===================== 
;;; Displacement describes change in location over time.  It is used
;;; in many of the problems but is often replaced in special cases
;;; such as Potential Energy cases by the height variables and other
;;; values.  Typical vector errors are handled by the vector 
;;; error-handlers.  The handlers below are special cases.

;;; On a few problems, one must use distance instead of displacement.
;;; Since the student is probably pretty sophisticated (they know
;;; about displacement), it is probably okay to use their time and body
;;; specification in the bottom out hint, as long as it is correct.
(def-error-class use-distance-instead-of-displacement (?cbody ?ctime)
  ((student (vector (displacement ?sbody :time ?stime) ?dir))
   (no-correct (vector (displacement ?body2 :time ?time2) ?dir2))
   (correct (define-var (distance ?cbody :time ?ctime))))
  :utility 40
  :probability
  (+ 0.1
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (equal ?sbody ?cbody) 0.2 0.0)))

(defun use-distance-instead-of-displacement (cbody ctime)
   (list "On this problem, you need to use distance rather than displacement."
	 (format nil "Define a variable for the distance traveled by ~a ~a."
		 (nlg cbody 'def-np) (nlg ctime 'pp))))
      
;;; On a few problems, one must use height instead of displacement.
;;; Unfortunately, there are usually alternative solutions involving
;;; kinematics that do use displacement.  Thus, we have to use a
;;; rather specific condition here, which looks for use of
;;; displacement over a time interval when there is no displacement
;;; being used over that time interval.  If so, it prefers a correct
;;; time that matches one of the times in the displacement definition.
(def-error-class use-height-instead-of-displacement (?body ?ctime)
  ((student (vector (displacement ?body :time (during ?st1 ?st2)) ?dir))
   (no-correct (vector (displacement ?body :time (during ?st1 ?st2)) ?dir2))
   (correct (define-var (height ?body ?zero-height :time ?ctime))))
  :utility 40
  :probability
  (+ 0.1
     (if (equal ?st1 ?ctime) 0.2 0.0)
     (if (equal ?st2 ?ctime) 0.2 0.0)))

(defun use-height-instead-of-displacement (body ctime)
   (list "On this problem, you need to use height rather than displacement."
	 (format nil "Define a variable for the height of ~a ~a."
		 (nlg body 'def-np) (nlg ctime 'pp))))


;;; As the logs have shown the students can be confused by the
;;; relationship between displacement and height.  On any problem
;;; that uses potential energy we are interested in using hieght
;;; not in using the displacement.  however in a problem such as
;;; Exe1a the block on a spring:
;;;   _____     Given a block with mass m on a compressed spring
;;;   |   |     determine how high it will go once the spring is
;;;   |   |     released?
;;;   -----     
;;;    =\=      Height is defined as a scalar variable while 
;;;    =/=      displacement is defined as a vector.  A rational
;;;             response to the students might be to define a two
;;;  hieght variables and then to define a displacement vector to
;;;  describe the motion between them.  This, sadly is not what
;;;  we want them to do as the hieght/displacement distinction is
;;;  either or.  However this confusion is significant enough that 
;;;  it probably deserves its own error handler (this one).  In 
;;; In other instances where the student has defined a single hieght
;;; vector then the system will respond with the stock (height not
;;; displacement hint).  This is a specific response to the 
;;; displacement as change in height confusion.

(def-error-class displacement-as-height-change-bug (?body ?stime ?etime)
  ((student (vector (displacement ?body :time (during ?stime ?etime)) ?dir))
   (no-correct (vector (displacement ?body :time (during ?stime ?etime)) ?dir2))
   (old-student (define-var (height ?body ?zero-height :time ?stime)))
   (old-student (define-var (height ?body ?zero-height :time ?etime))))
  :utility 80)

(defun displacement-as-height-change-bug (body starttime endtime)
  ;; Since they are unused this should suppress warnings and speed the sysytem.
  (declare (ignore Body StartTime EndTime))  
  "Something intelligent is said about the displacement as change in height bug."
   (list (strcat "Although it is always correct to use a displacement vector to "
		 "represent a change in position, that vector is not needed "
		 "to solve this problem.")
	 (strcat "The scalar height variables that you have defined at time ~a "
		 "and at time ~a are sufficient to solve this problem.  "
		 "All that you need is the difference between these scalars.")
	 '(function next-step-help)))

;;; =================== drawing a force ============================
;;; The default cases involving the body, direction and time are
;;; handled by the general vector error classes, with one special case
;;; defining force on whole rather than part.  Three other default cases
;;; are handled here: (1) wrong agent (2) wrong force type and (3)
;;; wrong agent and force type.  These ignore the time and direction
;;; in the conditions, but boost the probability if they they match.
;;; There lots of special cases, too.

; Special case wrong body on force (for torque problems):
; Andes convention is that force must be defined on point of application, 
; not on whole rigid body, if point of application matters. Applies if
; everything else is correct, so want very high EU.
(def-error-class force-on-whole-not-pt (?cbody ?sbody)
 ( (student (vector (force ?sbody ?agent ?type :time ?time) ?dir))
   (correct (vector (force ?cbody ?agent ?type :time ?time) ?dir))
   (problem (point-on-body ?cbody ?sbody)))
  :probability 0.9
  :utility 100)

(defun force-on-whole-not-pt (correct-body wrong-body)
  (setf correct-body (nlg correct-body 'def-np))
  (setf wrong-body (nlg wrong-body 'def-np))
   (list (format nil "Although that force does act on ~a, in problems where the precise point of application of a force matters, you should define the force as acting on the point of application." 
		 wrong-body)
	 `(bottom-out (string ,(format nil "Define the force to act on ~a instead of ~A." 
	         correct-body wrong-body)))))

;;; If the student gets the body and type right but the agent wrong,
;;; then teach them the force-is-an-interaction concept.
(def-error-class force-wrong-agent (?body ?sagent ?cagent ?ctime ?type)
  ((student (vector (force ?body ?sagent ?type :time ?stime) ?sdir))
   (no-correct (vector (force ?body ?sagent ?type :time ?time2) ?dir2))
   (correct (vector (force ?body ?cagent ?type :time ?ctime) ?cdir)))
  :probability
  (+ 0.1
	(if (equal ?stime ?ctime) 0.2 0.0)
	(if (equal ?sdir ?cdir) 0.1 0.0))
  :utility 5)

(defun force-wrong-agent (body sagent cagent ctime type)
  (declare (ignore ctime))
   (list `(point (string ,(if sagent (format nil "Is the force really ~A?" 
			    (agent-phrase sagent))
	     "What is causing the force?")))
	 ;; contact-and-field-forces
	 `(teach (string ,(strcat "Forces are caused by interacting objects.&nbsp; "
		 "The force can either be a field force, such as weight, "
		 "in which case the "
		 "objects do not have to touch.&nbsp; Or the force is a "
		 "contact "
		 "force, such as tension or friction, in which case the "
		 "objects must touch.")))
	 `(bottom-out (string ,(format nil "Draw a ~a force on ~a~@[ ~A~]~@[ instead of ~a~]."
		 (nlg type 'adj) (nlg body 'def-np)
		 (agent-phrase cagent)
		 (def-np sagent))))))



;;; If the student defines a force vector for a body that has no force 
;;; vectors on it then we should say something specific to that I.E. 
;;; superfluity.  However it is necessary to respond to the two 
;;; possible alternatives.  I.E. No single forces on body AND no net
;;; force.  And the situation where ther are no single forces but there
;;; is a net force.  These handlers might be set at a probability just
;;; above the no vectors on object rule.
(def-error-class no-forces-on-body (?body)
  ((student (vector (force ?body ?agent ?type :time ?time) ?dir))
   (no-correct (vector (force ?body ?ncagent ?nctype :time ?nctime) ?ncdir))
   (no-correct (vector (net-force ?body :time ?ntime) ?ndir)))
  :probability 0.005)

(defun no-forces-on-body (body)
   (list (format nil (strcat "None of the solutions that I know of include"
			     " force vectors on ~a.") (nlg body 'def-np))))

;;; In the situation (such as dt5a) where no single forces are 
;;; defined on a body but the net force is.
(def-error-class net-force-only-on-body (?body ?net-time ?net-dir)
  ((student (vector (force ?body ?agent ?type :time ?time) ?dir))
   (no-correct (vector (force ?body ?nagent ?ntype :time ?ntime) ?ndir))
   (correct (vector (net-force ?body :time ?net-time) ?net-dir)))
  :probability 0.01)

(defun net-force-only-on-body (body net-time net-dir)
  (let ((b (nlg body 'def-np)) (nt (nlg net-time 'pp)) 
	(nd (nlg net-dir 'adj)))
     (list (format nil (strcat "None of the solutions that I know of "
			       "include a single-force vector on ~a.") b)
	   (format nil (strcat "For this problem you only need to define "
			       "a net-force vector on ~a.") b)
	   *dyi*
	   `(bottom-out (string ,(format nil (strcat "You need to define a net-force on ~a "
			       "~a at ~a.") b nt nd))))))
		 

;;; ------------------ Right Body and Agent, wrong Type ------------------
;;; If the student defines a force vector and gets the body and agent right 
;;; but not the type then there are four possibilities:  
;;;   0) Student does not specify type explicitly when type is known.
;;;   1) There is a single force on that body by that 
;;;      agent but it is of a different type.
;;;   2) There are two foces for that body and agent 
;;;      (one friction and one applied or normal) and 
;;;      the student has done neither one.  
;;;   3) There are two forces (see above) and one of 
;;;      them has been completed.  
;;;
;;; Below are four errors handlers one for each possibility. 

(def-error-class force-type-unspecified (?body ?agent ?ctype)
  ;; Applied is the catch-all.  If type is not supplied, then
  ;; matching defaults to applied.
  ((student (vector (force ?body ?agent applied :time ?stime) ?sdir))
   (no-correct (vector (force ?body ?agent applied :time ?time2) ?dir2))
   (correct (vector (force ?body ?agent ?ctype :time ?ctime) ?cdir)))
  :probability
  (+ 0.35 ;; this should have higher weight than other three
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

(defun force-type-unspecified (body agent ctype)
  (declare (ignore agent))
   (list (strcat "You need to specify the <em>kind</em> of "
			     (quantity-html-link 
			      (lookup-exptype-struct 'force)) 
			     " acting on " (nlg body 'def-np) ".")
	 `(bottom-out (string ,(format nil "It is a ~a force." (nlg ctype 'adj))))))

;;; Typically there is only one force between the two objects and so all 
;;; that we need to do is tell them the right one to use.  This 
;;; interpretation often competes with the wrong-agent one.  It has higher 
;;; probability.
(def-error-class wrong-force-type-single (?body ?agent ?stype ?ctype)
  ((student (vector (force ?body ?agent ?stype :time ?stime) ?sdir))
   (no-correct (vector (force ?body ?agent ?stype :time ?time2) ?dir2))
   (correct (vector (force ?body ?agent ?ctype :time ?ctime) ?cdir)))
  :probability
  (+ 0.3
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

(defun wrong-force-type-single (body agent stype ctype)
   (list (format nil (strcat "There is indeed a force on ~a~@[~a~], "
			     "but it is not a ~a force.")
		 (nlg body 'def-np) (agent-phrase agent) (nlg stype 'adj))
	 `(bottom-out (string ,(format nil "It is a ~a force." (nlg ctype 'adj))))))


;;; When there are two forces on same body given a specific time and
;;; the student has done neither of them then we want to hint that 
;;; explicitly.  This is more specific than the wrong-force-type-single
;;; and therefore has a higher utility.
;;;
;;; If the student has drawn a force vector of type ?stype
;;; and if there is no correct vector of that type at any time
;;; and there are correct vetors of ?ctypes 1 and 2 at ?ctime
;;; and if ?ctype1 is either a kinetic or static friction vector.
;;; and if ?ctype2 is either normal or applied. 
;;;
;;; AW -- if direction is parallel to correct friction direction,
;;; would be natural to assume friction was intended. Maybe
;;; add error handler for wrong friction force type for this 
;;; competing case, presumably a common error.
(def-error-class wrong-force-type-dual-undone (?body ?agent ?stype ?frict-type ?force-type)
  ((student (vector (force ?body ?agent ?stype :time ?stime) ?sdir))    
   (no-correct (vector (force ?body ?agent ?stype :time ?time2) ?dir2))
   (any-member ?frict-type '(kinetic-friction static-friction))
   (correct-nointent (vector (force ?body ?agent ?frict-type :time ?ctime) ?frict-dir))
   (any-member ?force-type '(applied normal))
   (correct-nointent (vector (force ?body ?agent ?force-type :time ?ctime) ?force-dir)))
  :probability
  (+ 0.3
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (or (equal ?sdir ?frict-dir)
             (equal ?sdir ?force-dir)) 0.1 0.0))
  :utility 10)

(defun wrong-force-type-dual-undone (body agent stype frict-type force-type)
   (list (format nil (strcat "There are indeed forces on ~a~@[ ~a~], "
			     "but a ~a force is not one of them.")
		 (nlg body 'def-np) (agent-phrase agent) (nlg stype 'adj))
	 (format nil (strcat "There are two forces on ~a~@[ ~A~], "
			     "a ~a force and a ~a force.  You should "
			     "define one of them.")
		 (nlg body 'def-np) (agent-phrase agent) 
		 (nlg frict-type 'adj) (nlg force-type 'adj))
	 `(bottom-out (string ,(format nil (strcat "Change the force type to either ~a or ~a, "
	                     "adjusting the direction if necessary to "
			     "match your choice.")
		 (nlg frict-type 'adj) (nlg force-type 'adj))))))


;;; When there are two forces on the body from the agent (one friction 
;;; the other applied or normal) and one of them has been entered by the
;;; student then we want to hint them on the other one.
;;;
;;; There are two possible situations here.  Either the student has 
;;; done the friction force and the applied or normal force remains
;;; or they have done the applied/normal and friction remains.  The two 
;;; error classes below handle the possibilities.
(def-error-class wrong-force-type-dual-frict-done 
    (?body ?agent ?stype ?ctime ?frict-type ?force-type ?force-dir)
  ((student (vector (force ?body ?agent ?stype :time ?stime) ?sdir))    
   (no-correct (vector (force ?body ?agent ?stype :time ?time2) ?dir2))
   (any-member ?frict-type '(kinetic-friction static-friction))
   (old-student (vector (force ?body ?agent ?frict-type :time ?ctime) ?frict-dir))
   (any-member ?force-type '(applied normal))
   (correct (vector (force ?body ?agent ?force-type :time ?ctime) ?force-dir)))
  :probability
  (+ 0.3
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (or (equal ?sdir ?frict-dir)
             (equal ?sdir ?force-dir)) 0.1 0.0))
  :utility 20)

(defun wrong-force-type-dual-frict-done (body agent stype ctime 
					 frict-type force-type force-dir)
   (list (format nil (strcat "There are indeed forces on ~a~@[ ~A~], "
			     "but the ~a force is not one of them.")
		 (nlg body 'def-np) (agent-phrase agent) (nlg stype 'adj))
	 (format nil (strcat "There are two forces on ~a~@[ ~A~], "
			     "a ~a force and a ~a force.  You have "
			     "already completed the ~a force.")
		 (nlg body 'def-np) (agent-phrase agent) 
		 (nlg frict-type 'adj) (nlg force-type 'adj) 
		 (nlg frict-type 'adj))
	 *dyi*
	 `(bottom-out (string ,(format nil (strcat "You should draw a ~a force on ~a due to "
			     "~a ~a at ~a")
		 (nlg force-type 'adj) (nlg body 'def-np) (nlg agent 'def-np) 
		 (nlg ctime 'pp) (nlg force-dir 'def-np))))))



(def-error-class wrong-force-type-dual-other-done 
    (?body ?agent ?stype ?ctime ?frict-type ?frict-dir ?force-type)
  ((student (vector (force ?body ?agent ?stype :time ?stime) ?sdir))    
   (no-correct (vector (force ?body ?agent ?stype :time ?time2) ?dir2))
   (any-member ?frict-type '(kinetic-friction static-friction))
   (correct (vector (force ?body ?agent ?frict-type :time ?ctime) ?frict-dir))
   (any-member ?force-type '(applied normal))
   (old-student (vector (force ?body ?agent ?force-type :time ?ctime) ?force-dir)))
  :probability
  (+ 0.3
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (or (equal ?sdir ?frict-dir)
             (equal ?sdir ?force-dir)) 0.1 0.0))
  :utility 20)

(defun wrong-force-type-dual-other-done (body agent stype ctime 
					 frict-type frict-dir force-type)
   (list (format nil (strcat "There are indeed forces on ~a~@[ ~A~], "
			     "but the ~a force is not one of them.")
		 (nlg body 'def-np) (agent-phrase agent) (nlg stype 'adj))
	 (format nil (strcat "There are two forces on ~a~@[ ~A~], "
			     "a ~a force and a ~a force.  You have "
			     "already completed the ~a force.")
		 (nlg body 'def-np) (agent-phrase agent) 
		 (nlg frict-type 'adj) (nlg force-type 'adj) 
		 (nlg force-type 'adj))
	 *dyi*
	 `(bottom-out (string ,(format nil (strcat "You should draw a ~a force on ~a due to "
			     "~a ~a at ~a")
		 (nlg frict-type 'adj) (nlg body 'def-np) (nlg agent 'def-np) 
		 (nlg ctime 'pp) (nlg frict-dir 'def-np))))))


;;; If the student gets the body right but both the agent and type
;;; wrong, then there could be some significant confusion here.
;;; Calling next-step help isn't useful, as this is a conceptual not a
;;; procedural issue.  Hopefully, select-error-interpretation (in
;;; whatswrong.cl) will pick an interpretation where the correct force
;;; has not yet been drawn so that the bottom out hint will at least
;;; make sense.  This is pretty lousy help, so it gets low utility.
(def-error-class wrong-force-agent-and-type (?body ?sagent ?cagent ?stype ?ctype)
  ((student (vector (force ?body ?sagent ?stype :time ?stime) ?sdir))
   ;; no force with student's agent
   (no-correct (vector (force ?body ?sagent ?type2 :time ?time2) ?dir2)) 
   ;; no force with student's type
   (no-correct (vector (force ?body ?agent3 ?stype :time ?time3) ?dir3)) 
   (correct (vector (force ?body ?cagent ?ctype :time ?ctime) ?cdir)))
  :utility 0.1 
  :probability
  (+ 0.3
     (if (equal ?stime ?ctime) 0.2 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

;;; Note in individual work problems, only the needed force is drawn
;;; in the Andes solution, but other forces exist. Unless solutions changed,
;;; message can't correctly assert that forces don't exist, only that they are
;;; not part of solution. -- AW
(defun wrong-force-agent-and-type (body sagent cagent stype ctype)
  (setq body (nlg body 'def-np))
   (list (format nil 
		 (strcat "Andes' solution does not mention any ~A forces "
			 "on ~a~@[ ~A~].&nbsp; Either this force does not "
			 "exist or "
			 "it is not needed for solving this problem.")
		  (nlg stype 'adj) body (agent-phrase sagent))

	 `(bottom-out (string ,(format nil (strcat "You could try drawing a ~a "
			     "force on ~a~@[ ~A~].")
		 (nlg ctype 'adj) body (agent-phrase cagent))))))


;;; Special case: If the student defines a weight force for a body and
;;; uses the body as the agent as well, then they have a misconception
;;; about weight.  This error interpretation will not have a correct
;;; system entry.
(def-error-class weight-due-to-object (?object ?planet)
  ((student (vector (force ?object ?object weight :time ?stime) ?dir))
   (correct (vector (force ?body ?planet weight :time ?ctime) ?cdir)))
  :utility 200)

(defun weight-due-to-object (object planet)
  (setq object (nlg object 'def-np))
   (list 
    (format nil (strcat "Although there is a weight force acting on ~a, "
			"it is not~@[ ~A~] itself, because all forces, "
			"even the weight force, are due to interactions "
			"between two DIFFERENT objects.") object object)

    ;;weight-force-due-to-planet
    (strcat "The weight force is due to the interaction between the body "
	    "and a nearby planet.  Indeed, if the planet vanished, so would "
	    "the force.")

    `(bottom-out (string ,(format nil (strcat "Change the definition of the force so it is due "
			"to ~a instead of ~a.") (nlg planet 'def-np) object)))))


;;; It is never correct to select the same object as both the body and
;;; agent when one is defining a force.  This may be due to a typo, 
;;; confusion regarding the concept of forces, or it may be because the
;;; student is trying to define a net force but does not know how.  
;;; Initially there was one error here (commented out below)  However
;;; the mention of net force in problems where net-force does not exist
;;; appeared to confuse one of the students.  Therefore it has been 
;;; split into two error handlers the net-force-ok and no-net-force
;;; versions.  This was based upon the agreement that we probably 
;;; shouldn't mention net force at all unless there is the possibility
;;; that we might want the students to use it.


(def-error-class same-body-and-agent-of-a-force-no-net (?object)
   ((student (vector (force ?object ?object ?type :time ?time) ?dir))
    (correct (vector (force ?cbody ?cagent ?ctype :time ?ctime) ?cdir))
    (no-correct (vector (net-force ?net-body :time ?net-time) ?net-accel)))
   :utility 100)

(defun same-body-and-agent-of-a-force-no-net (object)
  (setq object (nlg object 'def-np))
   (list (format nil (strcat "A force arise from the interaction of two "
			     "DIFFERENT objects, but you have used ~a "
			     "for both.") object)
	 '(function next-step-help)))
   

(def-error-class same-body-and-agent-of-a-force-net-ok (?object)
  ((student (vector (force ?object ?object ?type :time ?time) ?dir))
   (correct-nointent (vector (net-force ?object :time ?net-time) ?net-accel)))
  :utility 150)

(defun same-body-and-agent-of-a-force-net-ok (object)
  (setq object (nlg object 'def-np))
   (list (format nil (strcat "A force arise from the interaction of two "
			     "DIFFERENT objects, but you have used ~a "
			     "for both.") object)
	 (format nil (strcat "If you were trying to draw the net force on "
			     "~a, then use the term \"net\" when defining "
			     "the force.") object)))


;;; Special case: If there is a normal force on a body, and if the student draws a
;;; force that has the right direction for a normal force but doesn't
;;; give it the normal type, then first hint, then explain about
;;; normals, then bottom out.
(def-error-class normal-force-mistyped (?badtype)
  ((student (vector (force ?body ?surface ?badtype :time ?time) ?dir))
   (test (not (equal ?badtype 'normal)))
   (correct (vector (force ?body ?surface normal :time ?time) ?dir)))
  :utility 100)

(defun normal-force-mistyped (badtype)
  (setf badtype (nlg badtype 'adjective))
   (list
    (format nil "Is \"~a\" really the type of force you meant?" badtype)

    ;;teach normal-force-exists
    (strcat "When a surface pushes on an object, the force it "
	    "exerts is called a 'normal' force.")

    `(bottom-out (string ,(format nil (strcat "Double click on the force and change "
			"the type from ~a to normal.") badtype)))))


;;; Special case: (ref Pitt 1-56-19) If the student draws a force that
;;; is correct except that the body and agent are switched, then they
;;; may be confused about the third law or have just make a slip.
(def-error-class switched-objects-for-force (?sbody ?sagent)
  ((student (vector (force ?sbody ?sagent ?type :time ?stime) ?sdir))
   (correct (vector (force ?sagent ?sbody ?type :time ?stime) ?sdir)))
  :utility 100)

(defun switched-objects-for-force (sbody sagent)
  (setq sbody (nlg sbody 'def-np))
  (setq sagent (nlg sagent 'def-np))
   (list
    (strcat "Judging from the direction and type of force you "
	    "drew, I'd guess you got the two objects switched.")

    `(bottom-out (string ,(format nil (strcat "Try defining the force on ~a~@[ ~A~], "
			"rather than the force on ~a~@[ ~A~].")
	    sagent sbody sbody sagent)))))


;;; Special case: If the student draws a normal force straight up when its direction
;;; should be tilted, then teach them about normal force directions.
(def-error-class normal-force-direction (?body ?surface ?dir)
  ((student (vector (force ?body ?surface normal :time ?time) (dnum 90 |deg|)))
   (correct (vector (force ?body ?surface normal :time ?time) ?dir))
   (test (not (equal ?dir '(dnum 90 |deg|)))))
  :utility 100)

(defun normal-force-direction (body surface dir)
   (list
    "Are you sure the normal force is straight up?"
    ;;teach normal-force-dir
    (strcat "The normal force is perpendicular (normal) to the surface "
	    "that causes it.  Of course the normal force is a 'pushing' "
	    "force rather than a 'pulling' force, so it points away from "
	    "the surface, not into it.")
    `(bottom-out (string ,(format nil (strcat "Because the normal force on ~a is perpendicular "
			"to ~a, make its direction be ~a.")
	    (nlg body 'def-np) (nlg surface 'def-np) (nlg dir 'adj))))))


;;; Special case: (ref pitt 1-35-23) Friction forces should
;;; always be parallel to the surface.
(def-error-class friction-force-not-parallel (?sdir ?cdir)
  ((student (vector (force ?body ?surface ?type :time ?time) ?sdir))
   (test (member ?type '(static-friction kinetic-friction)))
   (correct (vector (force ?body ?surface ?type :time ?time) ?cdir))
   (test (not (parallel-or-antiparallelp ?sdir ?cdir))))
  :utility 100)

(defun friction-force-not-parallel (sdir cdir)
  (list
    "Think about the direction of the force."
    ;; teach friction-is-parallel
    "Friction forces are always parallel to the surface causing them."
    `(bottom-out (string ,(format nil "The direction of the force should be ~a instead of ~a."
	    (nlg cdir 'adj) (nlg sdir 'adj))))))


;;; Special case: Static friction opposes the direction of relative motion
(def-error-class static-friction-force-sense (?sdir ?cdir)
  ((student (vector (force ?body ?surface static-friction :time ?time) ?sdir))
   (correct (vector (force ?body ?surface static-friction :time ?time) ?cdir))
   (test (parallel-or-antiparallelp ?sdir ?cdir))
   (test (not (equal ?sdir ?cdir))))
  :utility 100)

(defun static-friction-force-sense (sdir cdir)
   (list
    "Think about the direction of the force."
    ;; teach static-friction-sense
    (strcat "To figure out which way static friction points, imagine "
	    "that the surface is frictionless, and see which direction "
	    "the body would move relative to the surface.  The static "
	    "friction opposes this imaginary relative motion.")
    `(bottom-out (string ,(format nil "The direction of the force should be ~a instead of ~a."
	    (nlg cdir 'adj) (nlg sdir 'adj))))))

;;; Special case: Kinetic friction opposes the direction of relative motion
(def-error-class kinetic-friction-force-sense (?sdir ?cdir)
  ((student (vector (force ?body ?surface kinetic-friction :time ?time) ?sdir))
   (correct (vector (force ?body ?surface kinetic-friction :time ?time) ?cdir))
   (test (parallel-or-antiparallelp ?sdir ?cdir))
   (test (not (equal ?sdir ?cdir))))
  :utility 100)

(defun kinetic-friction-force-sense (sdir cdir)
   (list
    "Think about the direction of the force."
    ;; teach kinetic-friction-sense
    (strcat "Kinetic friction opposes the direction of relative motion.  "
	    "For example, if an object is moving leftward across a surface, "
	    "the kinetic friction points rightward.")
    `(bottom-out (string ,(format nil "The direction of the force should be ~a instead of ~a."
	    (nlg cdir 'adj) (nlg sdir 'adj))))))


;;; ==================== drawing net force ========================
;;; The default cases are handled by the general vector code, so this
;;; is all special cases.

;;; Handle case where student defines net-force in a problem that uses
;;; only explicit forces.
(def-error-class net-force-not-used (?sbody)
  ((student (vector (net-force ?sbody :time ?stime) ?sdir))
   (no-correct (vector (net-force ?sbody . ?rest) ?cdir))
   (correct (vector (force ?sbody . ?rest) ?ccdir)))
  :utility 10
  :probability 0.1)

(defun net-force-not-used (body)
   (list 
    (format nil (strcat "Although you can always define a net force vector, "
			"this problem can be solved without defining the "
			"net force acting on ~A.")
	    (nlg body 'def-np))
    (format nil (strcat "Delete the net force vector and instead draw all the "
			"individual forces acting on ~a.") 
	    (nlg body 'def-np))))


;;; If the student draws a zero net force when the real net force is
;;; non-zero, and the body is moving in a straight line, then point,
;;; teach and bottom out.
(def-error-class net-force-straight (?body ?time ?speed-up-or-slow-down)
  ((student (vector (net-force ?body :time ?time) zero))
   (correct (vector (net-force ?body :time ?time) ?dir1))
   (test (not (equal ?dir1 'zero)))
   (problem (motion ?body straight :accel ?accel-dir :dir ?motion-dir
		    :time ?time))
   (test (degree-specifierp ?motion-dir)) 
   (bind ?speed-up-or-slow-down (if (equal ?accel-dir ?motion-dir)
				    "speeding up" "slowing down")))
  :utility 100)

(defun net-force-straight (body time speed-up-or-slow-down)
  (setq body (nlg body 'def-np))
  (setq time (nlg time 'pp))
   (list
    (format nil "Notice that ~a is ~a while it moves in a straight line ~a."
	    body speed-up-or-slow-down time)
    (format nil (strcat "Whenever a body is moving in a straight line and "
			"is ~a, it is accelerating.  And whenever a body has "
			"a non-zero acceleration, the net force acting on "
			"it must be non-zero.  This follows directly from "
			"Newton's law, <var>F</var>=<var>m</var> <var>a</var>.") speed-up-or-slow-down)
    `(bottom-out (string ,(format nil (strcat "Because ~a is ~a ~a, it has non-zero acceleration.  "
			"You should change the net force to make it "
			"a non-zero vector.") 
	    body speed-up-or-slow-down time )))))


;;; ========================= drawing momentum =======================
;;; I can't find a problem that uses these, so they'll have to be put
;;; in later.


;;; ============== drawing an individual torque ====================
;;; An individual torque has slots for "torque due to force at _______
;;; about axis as ________" and the time and the direction.  The
;;; default cases for the times and directions are handled by the
;;; general vector error classes.  However, the general body error
;;; class default sounds funny, because the first argument of a torque
;;; descriptor is the point where the force is applied.  So it must be
;;; over-ridden here.  Thus, there are two default cases: (a) wrong
;;; applied point (ignoring axis point), and (b) wrong axis point but
;;; correct applied point.

;;; default error class for wrong applied point (ignoring axis point,
;;; time and direction).  Has to have a high enough utility to
;;; over-ride the default wrong-body for general vectors.
(def-error-class wrong-applied-pt-torque (?spt ?cpt)
  ((student    (vector (torque ?sbody (force ?spt . ?junk1)
			       :axis ?spivot :time ?stime) ?sdir))
   ;; no torque at ?spt
   (no-correct (vector (torque ?body2 (force ?spt . ?junk2)
			       :axis ?pivot2 :time ?time2) ?dir2)) 
   (correct    (vector (torque ?cbody (force ?cpt . ?junk3)
			       :axis ?cpivot :time ?ctime) ?cdir)))
  :utility 10
  :probability
  (+ 0.1 
     (if (equal ?spt ?cbody) 0.2 0.0)
     (if (equal ?stime ?ctime) 0.1 0.0)))

(defun wrong-applied-pt-torque (spt cpt)
   (list
    (format nil (strcat "You specified that the point where the force is "
			"applied is ~a.  Is that what you really want?")
	    (nlg spt 'def-np))
    
    (format nil (strcat "Instead of ~a, you should specify a point along "
			"the body whose rotation you want to analyze.")
	    (nlg spt 'def-np))
    
    `(bottom-out (string ,(format nil "For instance, define the ~A due to a force applied to ~a."
	  (moment-name)  (nlg cpt 'def-np))))))

;;; default case for a correct applied point but a wrong axis point.
;;; Probably a user-interface confusion, so handled it bluntly.
(def-error-class wrong-pivot-pt-torque (?spt ?cpt ?body)
  ((student    (vector (torque ?body (force ?pt ?agt ?type) 
			       :axis ?spt :time ?stime) ?sdir))
  ;; no torque for student's 2 pts
  (no-correct (vector (torque ?body (force ?pt ?ag2 ?typ2)
			      :axis ?spt :time ?time2) ?dir2)) 
  (correct    (vector (torque ?body ?cpt (force ?pt ?agt ?type3)
			      :axis ?cpt :time ?ctime) ?cdir)))
  :utility 10
  :probability
  (+ 0.1
     (if (equal ?stime ?ctime) 0.1 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

(defun wrong-pivot-pt-torque (spt cpt body)
   (list
    (format nil (strcat "You've picked ~a as the point that ~a rotates about."
			"  Is that what you really want?") 
	    (nlg spt 'def-np) (nlg body 'def-np))

    `(bottom-out (string ,(format nil "Because ~a rotates about ~a, choose it as the axis point."
	    (nlg body 'def-np) (nlg cpt 'def-np))))))

;; instead of non-existent-vector when need net-torque
;; Note we don't check for matches on net-torque args, just prompt them to
;; change to net torque and work from there
(def-error-class should-be-net-torque () 
  ((student    (vector (torque . ?sargs) ?sdir))
   (no-correct (vector (torque . ?args) ?dir))
   (correct    (vector (net-torque . ?cargs) ?cdir))))

(defun should-be-net-torque ()
   (list (format nil (strcat "No solution I know of includes an individual torque vector. "
			     "They do, however, include a net torque vector. "))
	 '(function next-step-help)))

;;; =============== Sums of torque =================================
;;; In the case of some torque problems *cough* tor7a *cough* it
;;; is necessary or at least useful for the studets to define a sum 
;;; of torque forces such as "\tau_l_end + \tau_r_end + \tau_cm = 0" if 
;;; they leave out one of the torque forces then the system should
;;; inform them of that in the same way that it does for other forces
;;; the error handler for forces is missing-forces in sums are the
;;; missing-forces-in-?Axis-sum where ?Axis is a member of [ x | y ]
;;; Torques will always be part of the z axis so we need to define a 
;;; new specific error handler along that axis.

(def-error-class missing-torques-in-z-axis-sum (?missing-non-zero)
  ((student-eqn ?dontcarewhatitis)	; ensure that the student's entry is an equation.
   (expr-loc ?loc ?sum)			; Get a sum expression if one is to be found.
   (test (sum-p ?sum))			; Ensure that the expression is a sum.
   (problem (torques ?body ?axis ?time ?correct-torques)) ; ensure that the problem has torques.
   
   ;; Get the torqes from the eqn
   (bind ?student-torques (torques-in-sum ?sum ?body ?time)) 
   (test (not (null ?student-torques)))	; ensure that there are torques.
   
   (bind ?missing-torques		; Then determine what torques are missing. 
	 (set-difference ?correct-torques ?student-torques :test #'equal))
   (test (not (null ?missing-torques)))	; and ensure that they are not null.
   (correct (draw-axes ?rot))   
   (bind ?missing-non-zero		; get nonzero missing torques 
	 (torques-non-zero ?missing-torques))
   (test (not (null ?missing-non-zero))) ; Ensure that it isn't null
   

   (bind ?missing-compo-vars		; collect the compos that are missing. 
	 (vectors-to-compo-sysvars 'z ?rot ?missing-non-zero))
   
   (bind ?new-sum (cons '+ (cons ?sum ?missing-compo-vars)))
   (fix-eqn-by-replacing ?loc ?new-sum))
  
  
  :probability 0.1
  :utility 102)

;;; If the student has written a sum that is missing one or more
;;; of the z-axis torques  then there are several possibilities.
;;; If they have drawn all of the torques but left one out of 
;;; the sum then we hint them to include those forces.  If they
;;; have failed to draw one of the torques then we hint them to
;;; draw it.  Else we hint them to draw the set of torques that
;;; they have forgotten.
(defun missing-torques-in-z-axis-sum (missing-torques)
  (let ((undrawn-torques (undrawn-vectors missing-torques)))
    (cond ((null undrawn-torques)	; only hint drawn forces if there are no undrawn ones
	   (hint-drawn-quants 
	    (moment-name) (set-difference missing-torques undrawn-torques 
		      :test #'equal)))
	  ((null (cdr undrawn-torques))	; only one undrawn force to hint
	   (hint-undrawn-vector 
	    (strcat "There is a " (moment-name) " acting on ~a ~a that you have not yet drawn.")
	    (car undrawn-torques)))
	  (t (hint-undrawn-vector	; Hint all of the undrawn ones.
	      (strcat "Two or more " (moment-name) "s acting on ~a ~a have not yet been drawn.")
	      (car undrawn-torques))))))
	   
;;; Given a set of torque vectors such as 
;;; (TORQUE BEAM PIVOT (FORCE R_END UNSPECIFIED APPLIED))
;;; and a time point collect the values of their magnitudes
;;; at that time from the problem's quantity index.
(defun torques-non-zero (torques)
  "Get the values for the TORQUES at TIME." 
  (loop for v in torques
      with at
      do (setq at (match-exp->qvar `(mag ,v) (problem-varindex *cp*)))
      ;(format t "!!! torques-non-zero ~a ~A~%" at (qvar-value at))
      unless (or (null at) 
		 (null (qvar-value at))
		 (= 0 (qvar-value at)))
      collect v))


;;; given a sum, which can be a tree of sums e.g, (+ (- (+
;;; ...)...)...), returns the torque descriptors mentioned by variables
;;; within it either at the top level of the sum or embedded in a
;;; multiplication.  Only torques consistent with the given body and
;;; time are returned.  Unless non-torques-ok is true, it will return NIL 
;;; if it finds an expression that does not contain a torque variable.
;;; this is based upon forces-in-sum by KVL
;;;
;;; This does so using a recursive call.  It calls torques-in-sum1 inside
;;; of a catch in case a non-torque is found and non-torques-ok is nil  
;;; in that instance it will return the nil that it gains.
(defun torques-in-sum (sum &optional (body '?body) (time '?time) (non-torques-ok nil))
  (let ((comp-pattern `(compo ?xyz ?rot (torque ,body ?force 
					  :axis ?axis :time ,time)))
	(mag-pattern `(mag (torque ,body ?force :axis ?axis :time ,time)))
	(torque-pattern `(torque ,body ?force :axis ?axis :time ,time)))
    (catch 'non-torque-found-err
      (torques-in-sum1 sum comp-pattern mag-pattern torque-pattern body time non-torques-ok))))


;;; Torques-in-sum1 takes a sum and three patterns, a torque-patten, a pattern 
;;; of a component of the same torque, a magnitude of that torque, and then 
;;; the body time and non-torques-ok.  It then tests the elements as described 
;;; below.  The  result value will be a list of torques, nil if there are none,
;;; or the non-torque-found-error condition if non-torques-ok is nil.
(defun torques-in-sum1 (expr comp-patt mag-patt torque-patt body time non-torques-ok)
  "Return the torques in the sum or nil if none are found or 
   non-torques are found and non-torques-ok is nil."
  (let (B) ;; to avoid pointless warnings.
    (cond 
     ;; If this is a sum then iterate over the cdr of the sum
     ;; recursively call torques-in-sum1 on each element in the list.
     ((sum-p expr)
      (loop for addend in (cdr expr) nconc
	    (torques-in-sum1 addend comp-patt mag-patt 
			     torque-patt body time non-torques-ok)))
     
     ;; If this is a sysvar and we can get a quant for it then return it
     ((and (sysvar-p expr)
	   (setq b (or (unify (sysvar-to-quant expr) comp-patt)
		       (unify (sysvar-to-quant expr) mag-patt))))	 
      (list  (subst-bindings b torque-patt)))
     
     ;; Multipication that includes a mag var can also be treated in the 
     ;; same way as the sum.  This cycles through the list and returns 
     ;; the result of substituting the values in as necessary.
     ((and (listp expr)			
	   (equal (car expr) '*)
	   (loop for factor in (cdr expr) with b2
	       when (and (sysvar-p factor) 
			 (setq b2 (unify (sysvar-to-quant factor) mag-patt)))
	       collect (subst-bindings b2 torque-patt))))
     
     ;; Unary minuses also do not effect the utility of the sum.  
     ;; Therefore the system will strip off the minus and keep going.
     ((and (listp expr)			
	   (null (cddr expr)) 
	   (equal (car expr) '-)
	   (sysvar-p (second expr))
	   (setq b (unify (sysvar-to-quant (second expr)) mag-patt)))
      (list (subst-bindings b torque-patt)))
     
     ;; If non-forces are ok then return nil if we reach this point as
     ;; no forces have been found.  
     (non-torques-ok NIL)
     
     ;; If a non-force is found and non-torques-ok is nil then we want 
     ;; to stop the recursive call and that is done by throwing the 
     ;; non-torque-error condition to the surrounding catch.
     (t (throw 'non-torque-found-err NIL)))))


;;; =============== drawing net torque =============================
;;; The slots are the body, the pivot, the direction and the time.
;;; Default case for the body, time and direction are handled by the
;;; general vector code.  The remaining default case is just wrong
;;; pivot but right body.

;;; If the student gets the pivot wrong but the body right, then its
;;; probably a user interface confusion, so just give a blunt hint.
(def-error-class wrong-pivot-net-torque (?spt ?cpt ?body)
  ((student    (vector (net-torque ?body ?spt :time ?stime) ?sdir))
   (no-correct (vector (net-torque ?body ?spt :time ?time2) ?dir2))
   (correct    (vector (net-torque ?body ?cpt :time ?ctime) ?cdir)))
  :utility 10
  :probability
  (+ 0.1
     (if (equal ?stime ?ctime) 0.1 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

(defun wrong-pivot-net-torque (spt cpt body)
   (list
    (format nil (strcat "You've picked ~a as the point that ~a rotates "
			"about.  Is that what you really want?")
	    (nlg spt 'def-np) (nlg body 'def-np))
    
    `(bottom-out (string ,(format nil "Because ~a rotates about ~a, choose it as the axis point."
	    (nlg body 'def-np) (nlg cpt 'def-np))))))


;;; ============= drawing relative position vectors ================
;;; The tool has slots for the point, the reference point, the
;;; direction and the time.  The general vector error classes handle
;;; the default cases for direction and time, but the default error
;;; class for body handles the default wrong-point case with funny
;;; wording, so it needs to be over-ridden here.  Thus, the default
;;; cases are (a) wrong point (ignoring everything else) and (b) wrong
;;; reference point (ignoring time and direction).

;;; default error class for wrong  point (ignoring reference point,
;;; time and direction).  Has to have a high enough utility to
;;; over-ride the default wrong-body for general vectors.
(def-error-class wrong-pt-relative-position (?spt ?cpt ?cpivot)
  ((student    (vector (relative-position ?spt ?spivot :time ?stime) ?sdir))
   (no-correct (vector (relative-position ?spt ?pivot2 :time ?time2) ?dir2))
   (correct    (vector (relative-position ?cpt ?cpivot :time ?ctime) ?cdir)))
  :utility 10
  :probability
  (+ 0.1 
     (if (equal ?sdir ?cdir) 0.2 0.0)
     (if (equal ?stime ?ctime) 0.1 0.0)))

(defun wrong-pt-relative-position (spt cpt cpivot)
   (list
    (format nil "Do you really want the relative position of ~a?"
	    (nlg spt 'def-np))
    `(bottom-out (string ,(format nil (strcat "Perhaps you should define the relative position "
			"from ~a to ~a instead of to ~a.")
	    (nlg cpivot 'def-np) (nlg cpt 'def-np) (nlg spt 'def-np))))))

;;; default case for a correct point but a wrong refernce point.
;;; Probably a user-interface confusion, so handled it bluntly.
;;; competes with wrong-pt-relative-position if multiple points are
;;; being measured, so utility and probability must be higher.
(def-error-class wrong-ref-pt-relative-position (?sref-pt ?cref-pt)
  ((student    (vector (relative-position ?pt ?sref-pt :time ?stime) ?sdir))
   (no-correct (vector (relative-position ?pt ?sref-pt :time ?time2) ?dir2))
   (correct    (vector (relative-position ?pt ?cref-pt :time ?ctime) ?cdir)))
  :utility 50
  :probability
  (+ 0.2
     (if (equal ?stime ?ctime) 0.1 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

(defun wrong-ref-pt-relative-position (spt cpt)
   (list
    (format nil (strcat "You've picked ~a as the reference point.  "
			"Is that what you really want?") (nlg spt 'def-np))
    `(bottom-out (string ,(format nil (strcat "You probably want to define the relative position "
			"from ~a instead of from ~a.")
	    (nlg cpt 'def-np) (nlg spt 'def-np))))))


;;; Case where defined opposite relative position than one we want. Exact 
;;; reason we want direction rather than another depends on equation being 
;;; used, but we don't know that here.  We don't check direction of drawing
;;; so it could also be wrong; after fixing orientation, may get further
;;; direction error.
;;; Could try to separate case where drew vector in correct direction, but mixed up 
;;; from and to points in spec, from case where drew correctly for opposite
;;; rel-pos. But many will be unknown, so don't worry about it now.
(def-error-class opposite-relative-position (?cobj ?cref)
  ((student (vector (relative-position ?cref ?cobj :time ?time) ?dir))
   ;; In general, the psmclass opposite-relative-position ensures 
   ;; the existance of relative-position vectors containing either order
   ;; of ?cref and ?cobj in a solution.
   (correct (vector (relative-position ?cobj ?cref :time ?time) ?dir-opp))
   ;; make sure student hasn't done it already
   (no-student (vector (relative-position ?cobj ?cref :time ?time) ?dir-opp))
   )
  :utility 55
  :probability
;; unless one direction matches, it is better handled by 
;; generic default-wrong-direction help.
  (+ 0.001 ;less than default-wrong-direction
     (if (equal ?dir ?dir-opp) 0.22 0.0)
     (if (equal ?dir (opposite ?dir-opp)) 0.18 0.0)))

(defun opposite-relative-position (cpt cref-pt)
   (list 
    (format nil (strcat "The relative position you defined will point FROM ~a TO ~a. "
                        "Is that the direction you want?") (nlg cpt) (nlg cref-pt))
    `(bottom-out (string ,(format nil (strcat "For this problem you need to use the relative position "
                        "of ~a with respect to ~a, instead of the other way around.")
			 (nlg cpt) (nlg cref-pt))))))


;;; ===================== relative-vel ============================

;;; Relative-vel is defined pairwise:  give a generic statement about
;;; the pair.
;;; We don't check that that the direction of the student vector
;;; is indeed opposite of the correct vector.

(def-error-class opposite-relative-vel (?cpt ?cref-pt)
  ((student    (vector (relative-vel ?cref-pt ?cpt :time ?stime) ?sdir))
   ; Don't pre-empt direction error like should-be-unknown: make sure there
   ; is no possible relative velocity vector with this sense in the solution
   (no-correct (vector (relative-vel ?cref-pt ?cpt :time ?stime) ?cdir))
   (correct    (vector (relative-vel ?cpt ?cref-pt :time ?ctime) ?cdir-opp)))
  :utility 55
  :probability
  (+ 0.2
     (if (equal ?stime ?ctime) 0.1 0.0)
     (if (equal ?sdir (opposite ?cdir-opp)) 0.1 0.0)
     (if (equal ?sdir ?cdir-opp) 0.1 0.0)))

(defun opposite-relative-vel (cpt cref-pt)
   (list 
    (format nil (strcat "For this problem you need to use the relative velocity "
                        "of ~a with respect to ~a, instead of the other way around.")
			 (nlg cpt) (nlg cref-pt))))

;;;
;;;  In doppler problems, velocity is specified relative to the medium.
;;;
(def-error-class wrong-ref-pt-relative-vel (?cref-pt ?pt)
  ((student (vector (relative-vel ?pt ?sref-pt :time ?stime) ?sdir))
   (correct (vector (relative-vel ?pt ?cref-pt :time ?ctime) ?cdir))
   (problem (doppler-system ?source ?cref-pt ?observer))
   (test (member ?pt (list ?source ?observer))))
  :utility 50
  :probability
  (+ 0.25
     (if (equal ?stime ?ctime) 0.1 0.0)
     (if (equal ?sdir ?cdir) 0.1 0.0)))

(defun wrong-ref-pt-relative-vel (cpt pt)
   (list
    (format nil (strcat "For doppler problems, you should define velocities  "
			"with respect to the wave medium (~A).") 
	    (nlg cpt 'def-np))
    `(bottom-out (string ,(format nil (strcat "Define the velocity of ~A with respect to ~A.")
	    (nlg pt 'def-np) (nlg cpt 'def-np))))))


;;; ============= drawing electric/magnetic field vectors ================
;;; quantity has location, agent and time.
;;; location looks like "body" to default rules, but is not a body.
;;; so must override default-vector-body, which doesn't fit
;;; Same rules work for both electric and magnetic fields
(def-error-class field-wrong-loc (?cloc ?sloc ?type)
  ((student    (vector (field ?type :location ?sloc :source ?sagent :time ?stime) ?sdir))
   (no-correct (vector (field ?type :location ?sloc :source ?agent2 :time ?time2) ?dir2))
   (correct    (vector (field ?type :location ?cloc :source ?cagent :time ?ctime) ?cdir)))
  :utility 50
  :probability 0.15) ;; This tends to be a weak/misleading hint.

(defun field-wrong-loc (correct-loc wrong-loc fieldtype)
  (setf correct-loc (nlg correct-loc 'def-np))
  (setf wrong-loc (nlg wrong-loc 'def-np))
   (list (format nil "Are you sure you want to consider the ~a field at ~a?" 
		      (nlg fieldtype 'adj) wrong-loc)
	 `(bottom-out (string ,(format nil (strcat "A better choice of location (but maybe not the "
			     "only one) would be ~a.") correct-loc)))))

(def-error-class field-loc-too-specific (?cloc ?sloc ?type)
  ((student (vector (field ?type :location ?sloc :source ?sagent :time ?stime) ?sdir))
   (no-correct (vector (field ?type :location ?sloc :source ?agent2 :time ?time2) ?dir2))
   (correct (vector (field ?type :location ?cloc :source ?cagent :time ?ctime) ?cdir))
   (problem (at-place ?sloc ?cloc))
   )
  :utility 75
  :probability 0.35)

(defun field-loc-too-specific (correct-loc wrong-loc fieldtype)
  (setf correct-loc (nlg correct-loc 'def-np))
  (setf wrong-loc (nlg wrong-loc 'def-np))
   (list (format nil "Yes, an ~a field exists at ~a.&nbsp;  However, you can define it for a more general region." 
		      (nlg fieldtype 'adj) wrong-loc)
	 `(bottom-out (string ,(format nil (strcat "A more general choice of location "
			     "would be ~a.") correct-loc)))))

(def-error-class field-wrong-agent (?sagent ?cagent ?loc ?type)
  ((student    (vector (field ?type :location ?loc :source ?sagent :time ?stime) ?sdir))
   (no-correct (vector (field ?type :location ?loc :source ?sagent :time ?time2) ?dir2))
   (correct    (vector (field ?type :location ?loc :source ?cagent :time ?ctime) ?cdir)))
  :utility 50)

(defun field-wrong-agent (sagent cagent loc fieldtype)
 (declare (ignore loc))    
   (list
    (if sagent
	(format nil "Is ~a the source of the ~a field?"
                        (def-np sagent) (nlg fieldtype 'adj))
	(format nil "What is source of the ~a field?"
		(nlg fieldtype 'adj)))
    `(bottom-out (string ,(format nil (strcat "A better choice (but maybe not the only one) "
                         "would be the field~@[ ~A~]~@[, not ~a~].") 
                          (agent-phrase cagent) 
			  (def-np sagent))))))

;; instead of non-existent-vector when net-field is used:
(def-error-class should-be-net-field (?type) 
  ((student    (vector (field ?type :location ?loc . ?sargs) ?sdir))
   (no-correct (vector (field ?type :location ?loc . ?args) ?dir))
   (correct    (vector (net-field ?cloc ?type . ?cargs) ?cdir))))

(defun should-be-net-field (type)
   (list (format nil (strcat "In this problem you should define the net "
			     "~A field due to all sources. ")
		     (nlg type 'adj))
	 '(function next-step-help)))


;;; --------------------- Confusing electric and magnetic ---------------------

(def-error-class field-wrong-type (?stype ?ctype ?loc)
  ((student    (vector (field ?stype :location ?loc :source ?sagent :time ?stime) ?sdir))
   (no-correct (vector (field ?stype :location ?loc :source ?agent2 :time ?time2) ?dir2))
   (correct    (vector (field ?ctype :location ?loc :source ?cagent :time ?ctime) ?cdir)))
  :utility 22) ;low utility since time, direction, and agent are not matched

(defun field-wrong-type (stype ctype loc)
 (declare (ignore loc))    
   (list
    (format nil (strcat "Is the field really supposed to be ~A?") 
                        (nlg stype 'adj) )
    `(bottom-out (string ,(format nil (strcat "You probably want the ~A field.")
                          (nlg ctype 'adj))))))


(def-error-class flux-wrong-type (?stype ?ctype ?loc)
  ((student    (define-var (flux ?loc ?stype :time ?stime)))
   (no-correct (define-var (flux ?loc ?stype :time ?time2)))
   (correct    (define-var (flux ?loc ?ctype :time ?ctime))))
  :utility 27) ;low utility since time is not matched

(defun flux-wrong-type (stype ctype loc)
 (declare (ignore loc))    
   (list
    (format nil (strcat "Is the flux really supposed to be ~A?") 
                        (nlg stype 'adj) )
    `(bottom-out (string ,(format nil (strcat "You probably want the ~A flux.")
                          (nlg ctype 'adj))))))


(def-error-class dipole-moment-wrong-type (?stype ?ctype ?loc)
  ((student    (vector (dipole-moment ?loc ?stype :time ?stime) ?sdir))
   (no-correct (vector (dipole-moment ?loc ?stype :time ?time2) ?dir2))
   (correct    (vector (dipole-moment ?loc ?ctype :time ?ctime) ?cdir)))
  :utility 21) ;low utility since time and direction are not matched

(defun dipole-moment-wrong-type (stype ctype loc)
 (declare (ignore loc))    
   (list
    (format nil (strcat "Is the dipole moment really supposed to be ~A?") 
                        (nlg stype 'adj) )
    `(bottom-out (string ,(format nil (strcat "You probably want the ~A dipole moment.")
                          (nlg ctype 'adj))))))


;;; -------------------------- equation error handlers ------------------------

;;; ================ substitute similar variable ==============================
;;; These error classes all result from the student using one variable
;;; when similar variable would make the equation correct.


;;; (ref brody eq-usna 49-11) The student used one force magnitude
;;; where another force would be correct.  This has been generalized
;;; to any vector magnitude.  It would not be wise to generalize it
;;; further.  If two variables have the same value in the solution
;;; (most commonly, both are zero), then if substituting one into the
;;; student equation fixes the equation,then substituting the other
;;; will also fix the equation.  If we're luckly, the contextualizer
;;; will pick the right interpretation, but...  Thus, it is better to
;;; avoid variables that might have a zero value.  Magnitudes of
;;; non-zero vectors are of course not zero, so hopefully this won't
;;; fail much.  The utility is set low because just about anything is
;;; better than this.
(def-error-class substitute-mag-vars (?svar ?cvar)
  ((student-eqn ?dontcare0)
   (var-loc ?sloc ?svar (mag ?svector))
   (bind ?stime (time-of ?svector))
   (correct-var ?cvar (mag ?cvector))
   (test (equal (time-of ?svector) (time-of ?cvector)))
   (problem (vector ?body ?pvector ?dir))
   (bind ?vtime (time-of ?pvector))
   (test (equal (remove-time ?pvector) (remove-time ?cvector)))
   (test (not (equal ?dir 'zero)))
   (test (tinsidep-include-endpoints ?stime ?vtime))
   (fix-eqn-by-replacing ?sloc ?cvar))
  :utility 0.1
  :probability
  (+ 0.1 (* 0.5 (structural-similarity ?svector ?cvector))))

(defun substitute-mag-vars (svar cvar)
 (let ((squant   (sysvar-to-quant svar))
       (cquant   (sysvar-to-quant cvar)))
   (list
    ;; include definition in msg, since student might be confused on that. 
    ;; (Easy to do with default most-recently drawn body if you intend to 
    ;; draw a vector on another body.)  It would be nice if we could emphasize 
    ;; differing slot(s) in the definition. 
    (format nil "Did you really mean to use ~a in this equation?  ~a is defined as ~a." 
                (nlg svar 'algebra) (nlg svar 'algebra) (nlg squant))
    ; use var-or-quant for cvar since might not be any student var for it yet
    `(bottom-out (string ,(format nil (strcat "I am not sure this is what you intended, but "
                        "replacing one occurrence of ~a with ~a "
			"would make this equation numerically correct. ")
	    (nlg svar 'algebra) (nlg cquant 'var-or-quant)))))))

;;; Used as a rough means for picking correct variables whose
;;; definitions are "similar" to the student's variable's definition.
(defun structural-similarity (form1 form2)
  "Returns a number between 0 and 1 indicating how similar two cons-trees are."
  (let ((hit 0) (miss 0))
    (declare (special hit miss))
    (structural-sim form1 form2)
    (/ hit (max 1 (+ hit miss)))))

(defun structural-sim (f1 f2)
  "Counts as hits those atoms that occur in the same places structurally
   Counts as misses structural differences."
  (declare (special hit miss))
  (cond ((and (null f1) (null f2)))
	((and (atom f1) (equal f1 f2))
	 (setq hit (+ hit 1)))
	((and (consp f1) f1 (consp f2) f2)
	 (structural-sim (car f1) (car f2))
	 (structural-sim (cdr f1) (cdr f2)))
	(t (setq miss (+ miss 1)))))


;;; The student is using a variable with the wrong time specifier.
;;; For instance, the student might use a final velocity instead of an
;;; initial velocity in an equation.
(def-error-class var-has-wrong-time-specifier (?wrong-var ?wrong-time ?right-time )
  ((student-eqn ?dontcare0)
   (student (eqn ?this-eqn))  ;don't apply to answer boxes.
   (var-loc ?wrong-var-loc ?wrong-var ?specifier)
   (bind ?wrong-time (time-of ?specifier))
   (correct-var ?right-var ?specifier2)
   (bind ?right-time (time-of ?specifier2))
   (test (equal (remove-time ?specifier) (remove-time ?specifier2)))
   (fix-eqn-by-replacing ?wrong-var-loc ?right-var)))

(defun var-has-wrong-time-specifier (wrong-var wrong-time right-time)
  (setq wrong-var (nlg wrong-var 'algebra))
  (setf wrong-time (nlg wrong-time 'pp))
  (setf right-time (nlg right-time 'pp))
   (list (format nil "Perhaps the variable ~a has the wrong time specification for this equation." wrong-var)
	 (format nil "~a is defined for a quantity ~a. I noticed that if you replaced it with a variable for the same quantity ~a you would get a correct equation. But that is just one possibility and may not be the true cause of the error. Select \"Principles\" in the \"Physics\" menu to view correct general forms of equations." 
		 wrong-var wrong-time right-time)))


;;; (ref Pitt A4 1-09-30; student entered Fn_y=0 but meant Fn_x=0).
;;; If the student's equation is correct when a component variables
;;; axis label is switch from x to y or vice versa, it is probably
;;; just a slip.  (ref seth USNA 8-13) does this because he rotates
;;; the axes by 90 degrees.  Currently, Andes won't allow this, but
;;; when it does, it might be good to add the special case in.
;;; Probability is lowered when the vector is at an angle of 45
;;; degrees with respect to either axis, because in that case it could
;;; be that V_x=-V_y, and a sign error is more likely.
(def-error-class switched-x-and-y-subscript (?svar ?cvar)
  ((student-eqn ?dontcare0)
   (student (eqn ?this-eqn)) ;don't apply to answer boxes.
   (var-loc ?svar-loc ?svar (compo ?s-xyz ?rot ?s-vector))
   (test (member ?s-xyz '(x y)))
   (bind ?c-xyz (if (equal ?s-xyz 'x) 'y 'x))
   (correct-var ?cvar (compo ?c-xyz ?rot ?s-vector))
   (fix-eqn-by-replacing ?svar-loc ?cvar)
   ; !!! fetching vector dir this way means this can't apply
   ; if vector drawn unknown, even though slip is just as
   ; likely on unknown angle vector
   (problem (vector ?body ?s-vector (dnum ?v-dir |deg|))))
  :probability (if (equalp 45 (mod (- ?v-dir ?rot) 90))
		   0.001
		 0.1))

(defun switched-x-and-y-subscript (svar cvar)
   (list
    (format nil "Did you mean to use ~a instead of ~a?"
	    (nlg cvar 'algebra) (nlg svar 'algebra))))

;;; (ref eq-Pitt A8 2-21-14) If the student used a magnitude where a
;;; component variable would be appropriate, it may be just a slip.
;;; Since we don't know if the axes have been drawn or drawn in such a
;;; what that the correct component variable is defined, we can't give
;;; a bottom out hint.
(def-error-class used-magnitude-instead-of-component (?svar ?vector)
  ((student-eqn ?dontcare0)
   (var-loc ?svar-loc ?svar (mag ?vector))
   (correct-var ?cvar (compo ?xyz ?rot ?vector))
   (fix-eqn-by-replacing ?svar-loc ?cvar)))

(defun used-magnitude-instead-of-component (svar vector)
   (list
    (format nil "You used ~a, which is the MAGNITUDE of ~a.  Is that what you intended?"
	    (nlg svar 'algebra) (nlg vector 'def-np))
    `(bottom-out (string ,(format nil  "Instead of the magnitude, perhaps you should use the component along an axis."
	   )))))


;;; ============== trig & projection errors ====================================

(defparameter *trig-functions* '(sin cos tan) 
  "The trig functions found in equations: sin, cos, tan")
(defparameter *proj-trig-functions* '(sin cos)
  "The trig functions used in projection equations: sin and cos")

;;; The student has used the wrong trig function and correct is one of those
;;; used in projection equations.
(def-entry-test wrong-trig-function (?wrong-trig-fn ?right-trig-fn ?argument)
  :preconditions
    ((student-eqn ?dontcare0)
     (expr-loc ?wrong-loc (?wrong-trig-fn ?argument))
     (test (member ?wrong-trig-fn *trig-functions*))
     (any-member ?right-trig-fn *proj-trig-functions*)
     (test (not (equal ?wrong-trig-fn ?right-trig-fn)))
     (fix-eqn-by-replacing ?wrong-loc (?right-trig-fn ?argument)))
  :apply no-match
  :state +incorrect+
  :hint (wrong-trig-function (list ?wrong-trig-fn ?argument) 
			      (list ?right-trig-fn ?argument))
  :order ((expected-utility . 0.1)))

(defun wrong-trig-function (wrong right)
  (list "Check your trigonometry."
	;; teach projection 
	;; !!! may not be projection they are trying to write, 
	;; but dot or cross product.
	(strcat "If you are trying to calculate the component of a vector along an axis, "
		"here is a general formula that will always work: "
		"Let &theta;V be the angle as you move counterclockwise from the horizontal to "
		"the vector.  Let &theta;x be the rotation of the x-axis from the horizontal. "
		"Then: V<sub>x</sub> = V cos(&theta;V-&theta;x) and V<sub>y</sub> = V sin(&theta;V-&theta;x).")
	(format nil "Replace ~a with ~a." 
		(nlg wrong 'algebra) (nlg right 'algebra))))


;;; The student has used the wrong trig function where tangent is right
;;; don't even try to guess what to teach here
(def-entry-test trig-function-should-be-tan (?wrong-trig-fn ?right-trig-fn ?argument)
  :preconditions
  ((student-eqn ?dontcare0)
   (expr-loc ?wrong-loc (?wrong-trig-fn ?argument))
   (test (equal ?wrong-trig-fn 'tan))
   (any-member ?right-trig-fn *trig-functions*)
   (test (not (equal ?wrong-trig-fn ?right-trig-fn)))
   (fix-eqn-by-replacing ?wrong-loc (?right-trig-fn ?argument)))
  :apply no-match
  :state +incorrect+
  :hint (trig-function-should-be-tan (list ?wrong-trig-fn ?argument) 
				     (list ?right-trig-fn ?argument))
  :order ((expected-utility . 0.1)))

(defun trig-function-should-be-tan (wrong right)
  (list "Check your trigonometry."
	(format nil "Replace ~a with ~a." 
		(nlg wrong 'algebra) (nlg right 'algebra))))

;;; Student has left off the "deg" unit but intended the number to be
;;; interpreted as degrees
(def-error-class trig-argument-units (?trig-fn ?arg)
    ((student-eqn ?dontcare0)
     (expr-loc ?wrong-loc (?trig-fn ?arg))
     (test (member ?trig-fn *trig-functions*))
     (test (numberp ?arg))
     (fix-eqn-by-replacing ?wrong-loc (?trig-fn (* ?arg (dnum 1 |deg|))))))

(defun trig-argument-units (trig-fn arg)
  (setf trig-fn (nlg trig-fn 'lower-case))
   (list
    "Radians are the default unit for angles."
    (format nil (strcat "The ~a(~a) means ~a(~a rad).  You need to write ~a(~a deg) "
			"in order to get the ~a interpreted as degrees.") 
	    trig-fn arg trig-fn arg trig-fn arg arg)
    `(bottom-out (string ,(format nil "Replace ~a(~a) with ~a(~a deg) in your equation."
	    trig-fn arg trig-fn arg)))))

;;; (ref eq-Pitt A8  1-17-51) If the student enters "V_y = 0" when the
;;; correct equation is "V_y=V", then they have got the two special
;;; cases of projection confused.
(def-error-class  used-perpendicular-instead-of-parallel-projection (?xyz ?vector ?scompo ?mag)
  ((student-eqn (= ?scompo 0))
   (correct-var ?ccompo (compo ?xyz ?rot (?vector . ?args)))
   (test (equal ?scompo ?ccompo))
   (correct-var ?mag (mag (?vector . ?args)))
   (student-eqn ?eqn)
   (bind ?rhs (cddr ?eqn))
   (fix-eqn-by-replacing ?rhs ?mag)))

(defun used-perpendicular-instead-of-parallel-projection (xyz vector compo mag)
  (setq vector (nlg vector 'def-np))
   (list
    (format nil (strcat "Looks like you are trying to calculate the value of "
			"the ~a component of ~a.  However, ~a=0 only when the "
			"vector is PERPENDICULAR to the ~a axis.")
	    xyz vector (nlg compo 'algebra) xyz)
    (format nil "Notice that the vector is PARALLEL to the ~a axis."
	    xyz)
    (format nil (strcat "Hence, its component along the ~a axis is just its "
			"magnitude.  That is, ~a=~a.")
	    xyz (nlg compo 'algebra) (nlg mag 'algebra))))
    

;; if forgot axis tilt term in a projection equation
(def-error-class missing-axis-tilt (?trig-fn ?arg ?xyz ?rot)
    ((student-eqn (= ?scompo ?rhs))                
     (expr-loc ?wrong-loc (?trig-fn ?arg))
     (test (member ?trig-fn *proj-trig-functions*))
     (correct-var ?ccompo (compo ?xyz ?rot ?vector))
     (test (equal ?scompo ?ccompo))
     (test (not (= ?rot 0))) 
     ;; This can conflict with sin/cos error (wrong-trig-function).
     ;; Don't apply if trig-fn arg already appears to subtract axis tilt
     (test (not (expr-subtracts-tilt ?arg ?rot)))
     (fix-eqn-by-replacing ?wrong-loc (?trig-fn (- ?arg (dnum ?rot |deg|)))))
)

(defun expr-subtracts-tilt (expr rot)
"true if expr (presumably trig fn arg) subtracts axis tilt value rot"
    (unify expr `(- ?something (dnum ,rot |deg|)))) 

(defun missing-axis-tilt (trig-fn arg xyz rot)
  (declare (ignore xyz))
  (let ((bad-expr `(,trig-fn ,arg)) 
       (good-expr `(,trig-fn (- ,arg (dnum ,rot |deg|)))))
   (list
    (format nil (strcat "Remember that the axes are rotated by &theta;x = ~A degrees "
                        "from the horizontal-vertical system used for vector " 
			"orientations. You need to take this into account when calculating the projection of a vector along the axis.")
	    rot)
    ;; !!! general formula and link to minilesson here
    `(bottom-out (string ,(format nil (strcat "Because the positive x axis is rotated by &theta;x = ~A deg from the horizontal, change ~A to ~A in your equation.")
            (algebra rot) (algebra bad-expr) (algebra good-expr))
   )))))


;; TODO: if used wrong angle value (var or numerical)
;;   Maybe special case if off by 180 degrees?  Especially common
;;   when wrong angle is direction of given plane, e.g.
;;
;; Also need: wrong angle variable, and different cases for
;; both standard axis and tilted axis form.

;;; ========================== sign errors ===========================

;;; Default sign error has low utility because it just encourages
;;; students to flip the sign without thinking about why the sign is
;;; wrong.  But Don and Bob want this (email 8/2/01).
(def-error-class default-sign-error (?var)
    ((student-eqn ?dontcare0)
     (var-loc ?loc ?var ?defn)
     (fix-eqn-by-replacing ?loc (- ?var)))
    :utility 0.1
    :probability 0.2)

(defun default-sign-error (var)
   (list "Check your signs."
	 `(bottom-out (string ,(format nil "Perhaps the sign of the ~a term should be changed." 
		 (nlg var 'algebra))))))

(def-error-class abs-sign-error ()
  ;; student equation does not contain an absolute value
    ((student-eqn ?seqn)
     (test (not (recursive-member 'abs ?seqn)))
     (bind ?s-vars (vars-in-eqn ?seqn))
     ;; student has made a sign error
     (var-loc ?loc ?var ?defn)
     (fix-eqn-by-replacing ?loc (- ?var))
     ;; correct equation does contain an absolute value
     (correct (eqn ?ceqn))
     (test (recursive-member 'abs ?ceqn))
     (bind ?c-vars (vars-in-eqn ?ceqn))
     ;; This is a naive test to see if the student
     ;; equation actually matches this correct equation.
     (test (equal-sets ?c-vars ?s-vars))
     )
    :utility 1
    :probability 0.3)

(defun abs-sign-error ()
   (list "Normally, this equation is written using an absolute value."
	 "Writing the equation using an absolute value will lessen the chance that you make a sign error."))
      

;;; (ref Bob's email of 7/19/01 8:56 am) If the student has makes a
;;; sign error in the definition of average acceleration, then teach
;;; them about CHANGE in velocity.  
;;;
;;; BvdS:  This is way too specific.  It only covers a very specific form for 
;;; the equation and only certain errors.
;;; There should also be a similar rule for definition of average velocity.
;;;
(def-entry-test avg-accel-is-change-in-velocity (?xyz ?a ?vf ?vi ?dur)
  :preconditions 
  ((student-eqn (= ?a (/ (?op ?v1 ?v2) ?dur)))
   (var-defn ?a (compo ?xyz ?rot (accel ?body :time (during ?ti ?tf))))
   (var-defn ?dur (duration (during ?ti ?tf)))
   (correct-var ?vi (compo ?xyz ?rot (velocity ?body :time ?ti)))
   (correct-var ?vf (compo ?xyz ?rot (velocity ?body :time ?tf)))
   (test (equal (sort (list ?v1 ?v2) #'expr<) (sort (list ?vi ?vf) #'expr<))))
  :apply no-match
  :state +incorrect+
  :hint (avg-accel-is-change-in-velocity ?xyz `(= ,?a (/ (- ,?vf ,?vi) ,?dur)))
  :order ((expected-utility . 5)))

(defun avg-accel-is-change-in-velocity (xyz eqn)
  (list
   (format nil (strcat "The average acceleration in the ~a direction in a "
		       "time interval is equal to the change in the ~a "
		       "component of velocity divided by the time.  Change "
		       "is the final value minus the initial value.") xyz xyz)
   "See the formulae for constant acceleration."  ;; teach avg-accel-defn
   (format nil "Replace your equation with ~a." (nlg eqn 'algebra))))

;;; (ref eq-Pitt A4 3-03-23) If the student left out the negative sign on
;;; a vector component, and the vector is parallel to the axis, then
;;; the student has probably confused the component and the magnitude.
(def-error-class missing-negation-on-vector-component (?vector ?xyz ?compo-var ?mag-var)
    ((student-eqn ?dontcare0)
     (var-loc ?loc ?compo-var (compo ?xyz ?rot ?vector))
     (fix-eqn-by-replacing ?loc (- ?compo-var))
     (correct-var ?mag-var (mag ?vector))
     (problem (vector ?body ?vector ?dir))
     (test (and (parallel-or-antiparallelp (axis-dir ?xyz ?rot) ?dir) 
		(not (same-angle (axis-dir ?xyz ?rot) ?dir))))))

(defun missing-negation-on-vector-component (vector xyz compo-var mag-var)
  (setq vector (nlg vector 'def-np))
     (list
      (format nil "Think about the direction of ~a." vector)
      (format nil (strcat "Because the vector is parallel to the ~a "
			  "axis but in the negative direction, the "
			  "projection equation is ~a so "
			  "~a stands for a negative value.")
	      xyz (nlg `(= ,compo-var (- ,mag-var)) 'algebra) 
	           (nlg compo-var 'algebra))
      #| ; this advice is strange in most equation contexts:
        (format nil (strcat "Because the vector is parallel to the ~a axis "
			  "and in the negative direction, replace ~a "
			  "with either -~a or with ~a")
	      xyz (nlg compo-var 'algebra)
	      (nlg compo-var 'algebra) (nlg mag-var 'algebra))|#
	`(bottom-out (string ,(format nil (strcat "Changing the sign on ~a "
			    "would make this a correct equation.")
	      (nlg compo-var 'algebra))))))


;;; (ref eq-Pitt A9 47-23) The student has left out the minus sign on
;;; a magnitude variable.  The vector will always be anti-parallel to
;;; some axis, and if the student has used that axis, it is likely
;;; that they confused component and magnitude.  But if they have not
;;; drawn an axis, then it is not clear whether this interpretation is
;;; correct.  Minus signs can come from many errors.  So the
;;; conditions here explicitly check that the student has drawn an
;;; axis and that the vector is anti-parallel to it.  On a simple equation
;;; like Fw_y=Fw, this error class competes with
;;; missing-negation-on-vector-component, but it is more likely.
(def-error-class missing-negation-on-vector-magnitude (?vector ?xyz ?compo-var ?mag-var)
    ((student-eqn ?dontcare0)
     (var-loc ?loc ?mag-var (mag ?vector))
     (fix-eqn-by-replacing ?loc (- ?mag-var))
     (problem (vector ?body ?vector ?dir))
     (old-student (draw-axes ?rot))  
     ; Test that vector is antiparallel to *some* axis in the xy[z] system.
     ; then get that axis and its rotation for looking up compo var.
     (bind ?xyz (find-antiparallel-axis ?rot ?dir))
     (test ?xyz) ; make sure 
     (correct-var ?compo-var (compo ?xyz ?rot ?vector)))
    :probability 0.2)

;; given x axis rotation and vector dir, return label 'x or 'y or 'z
;; specifying the axis in the system to which vdir is antiparallel;
;; NIL if none
(defun find-antiparallel-axis (rot vdir)
  (cond ((same-angle (axis-dir 'z rot) (opposite vdir)) 'z)
	((same-angle (axis-dir 'x rot) (opposite vdir)) 'x)
	((same-angle (axis-dir 'y rot) (opposite vdir)) 'y)	
	(T NIL)))

(defun missing-negation-on-vector-magnitude (vector xyz compo-var mag-var)
  (setq vector (nlg vector 'def-np))
     (list
      (format nil "Think about the direction of ~a." vector)
      (format nil (strcat "Perhaps you are confusing the MAGNITUDE of ~a with "
			  "its COMPONENT along the ~a axis.  Because the vector "
			  "is parallel to the ~a axis but in the negative "
			  "direction, the projection equation is ~a.") 
	      vector xyz xyz (nlg `(= ,compo-var (- ,mag-var)) 'algebra))
      `(bottom-out (string ,(format nil (strcat "Because the vector is parallel to the ~a axis and in "
			  "the negative direction, replace ~a with either -~a or ~a")
	      xyz (nlg mag-var 'algebra) (nlg mag-var 'algebra) (nlg compo-var 'algebra))))))

;;; (ref eq-Pitt A8 2-25-02) Students sometimes think that because the
;;; accel due to gravity is downward, there should be a negative sign
;;; in Fw=m*g.  This error classes teaches against that misconception.
;;; We match the exact equation here: Fg = m*(-g).  This avoids firing
;;; the rule when other sign rules would be more appropriate.
(def-entry-test no-negation-in-weight-law1 (?smag ?smass ?g)
  :preconditions
  ((student-eqn (= ?smag (* ?smass (- ?g))))
   (var-defn ?g (gravitational-acceleration ?planet))
   (var-defn ?smag (mag (force ?body ?planet weight :time ?time)))
   (var-defn ?smass (mass ?body)))
  :apply no-match
  :state +incorrect+
  :hint (no-negation-in-weight-law `(= ,?smag (* ,?smass ,?g)))
  :order ((expected-utility . 10)))

;;; this handles the case where the student entered Fw=-m*g.
(def-entry-test no-negation-in-weight-law2 (?smag ?smass ?g)
  :preconditions
  ((student-eqn (= ?smag (* (- ?smass) ?g)))
   (var-defn ?g (gravitational-acceleration ?planet))
   (var-defn ?smag (mag (force ?body ?planet weight :time ?time)))
   (var-defn ?smass (mass ?body)))
  :apply no-match
  :state +incorrect+
  :hint (no-negation-in-weight-law `(= ,?smag (* ,?smass ,?g)))
  :order ((expected-utility . 10)))

(defun no-negation-in-weight-law (eqn)
  (list
   ;; teach wt-law-sign
   (strcat "You probably recall the vector equation <var>Fg</var> = <var>m</var> <var>g</var>' "
	   "where <var>Fg</var> and <var>g</var> are vectors, namely the weight "
	   "and gravitational acceleration.  This vector equation implies "
	   "that the two vectors have the same direction (both are downward).  "
	   "It also implies that their magnitudes are proportional, that is, "
	   "that <var>W</var>=<var>m</var> <var>g</var> where <var>W</var> and <var>g</var> stand for the MAGNITUDES of the weight "
	   "and gravitational acceleration.  Your version of this scalar "
	   "equation has an unnecessary minus sign in it.  Just remember "
	   "that in the scalar equation <var>Fg</var> = <var>m</var> <var>g</var>, everything is positive: mass "
	   "is a positive number, and because <var>Fg</var> and <var>g</var> stand for magnitudes, "
	   "they are positive numbers, too.")
    (format nil "Change your equation to ~a." (nlg eqn 'algebra))))

;;; ==================== sum of forces ============================

;;; if the students equation contains a sum of force components, where
;;; the components are either component variables or multiplications
;;; including a magnitude variable, and the forces mentioned are a
;;; proper subset of the forces on a body at a time, and inserting the
;;; missing force component variables into the sum makes the equation
;;; correct, then the student's equation is missing some forces.  On
;;; Exdt10a, it competes with substitute-mag-vars on the equation Fn -
;;; Fa * cos(45 deg) = 0.  Substitute-mag-vars suggests replacing Fn
;;; with Fk.  Thus, the utility is boosted here.

(def-error-class missing-forces-in-x-axis-sum (?missing-non-zero)
    ((student-eqn ?dontcare0)
     (expr-loc ?loc ?sum)
     (test (sum-p ?sum))
     (problem (any-forces ?body ?time ?correct-forces)) ; generates bodies and times
     (bind ?student-forces (forces-in-sum ?sum ?body ?time T))
     (test (not (null ?student-forces))) ; nil means a sum of something other than forces
     (bind ?missing-forces (set-difference ?correct-forces ?student-forces :test #'equal))
     (test (not (null ?missing-forces)))
     (correct (draw-axes ?rot))  ; generate rotations for possible x-axes
     (bind ?missing-non-zero (vectors-non-zero ?missing-forces 
					       (axis-dir 'x ?rot)))
     (test (not (null ?missing-non-zero)))
     (bind ?missing-compo-vars (vectors-to-compo-sysvars 'x ?rot ?missing-non-zero))
     (test (every #'identity ?missing-compo-vars))
     (bind ?new-sum (cons '+ (cons ?sum  ?missing-compo-vars)))
     (fix-eqn-by-replacing ?loc ?new-sum))
    :probability
    (+ 0.1 (if (unify-with-student-entries (list 'draw-axes ?rot)) 0.3 0.0))
    :utility 100)

(defun missing-forces-in-x-axis-sum (missing-forces)
  (missing-forces-in-sum missing-forces))


(def-error-class missing-forces-in-y-axis-sum (?missing-non-zero)
    ((student-eqn ?dontcare0)
     (expr-loc ?loc ?sum)
     (test (sum-p ?sum))
     (problem (any-forces ?body ?time ?correct-forces)) ; generates bodies and times
     (bind ?student-forces (forces-in-sum ?sum ?body ?time T))
     (test ?student-forces) ; nil means a sum of something other than forces
     (bind ?missing-forces (set-difference ?correct-forces ?student-forces :test #'equal))
     (test ?missing-forces)
     (correct (draw-axes ?rot))		; generate rotations for the possible x-axes
     (bind ?missing-non-zero (vectors-non-zero ?missing-forces 
					       (axis-dir 'y ?rot)))
     (test ?missing-non-zero)
     (bind ?missing-compo-vars (vectors-to-compo-sysvars 'y ?rot ?missing-non-zero))
     (test (every #'identity ?missing-compo-vars))
     (bind ?new-sum (cons '+ (cons ?sum ?missing-compo-vars)))
     (fix-eqn-by-replacing ?loc ?new-sum))
    :probability
    (+ 0.1 (if (unify-with-student-entries (list 'draw-axes ?rot)) 0.3 0.0))
    :utility 100)

(defun missing-forces-in-y-axis-sum (missing-forces)
  (missing-forces-in-sum missing-forces))



;;; If there are missing forces in a sum then there are three possible 
;;; situations firstly the student has failed to draw all of the necessary 
;;; forces.  In that case they will be hinted to draw them.  Else, if they 
;;; have left out only one of the necessary forces then they will be told to 
;;; add those.  Else they will be instructed to draw add the first of 
;;; the missing forces.  In time this will be changed.    
(defun missing-forces-in-sum (missing-forces)
  (let ((undrawn-forces (undrawn-vectors missing-forces)))
    (cond ((null undrawn-forces) ; only hint drawn forces if there are no undrawn ones
	   (hint-drawn-quants
	    "force" (set-difference missing-forces undrawn-forces 
		     :test #'equal)))
	  ((null (cdr undrawn-forces)) ; only one undrawn force to hint
	   (hint-undrawn-vector
	    "There is a force acting on ~a ~a that you have not yet drawn."
	    (car undrawn-forces)))
	  (t (hint-undrawn-vector
	      "Two or more forces acting on ~a ~a have not yet been drawn."
	      (car undrawn-forces))))))

(defun undrawn-vectors (vectors)
  "Given a list of vector descriptors, returns the subset
   of the vectors that has not been drawn by the student"
  (loop for v in vectors 
      unless (unify-with-student-entries `(vector ,v ?dir)) 
      collect v))

;;; 
(defun hint-drawn-quants (quant forces)
  "Return a hint sequence for a set of forces that have already
   been drawn by the student"
  (if (null (cdr forces))
       (list (format nil "Did you leave a ~A out of your sum of ~As?"
		     quant quant) ;no nlg here
	     `(bottom-out (string ,(format nil "You left ~a out of your sum of ~As" 
		     (nlg (car forces) 'def-np) quant))))
     (list (format nil "Some ~As may be missing from your sum of ~As."
		   quant quant) ;no nlg here
	   `(bottom-out (string ,(format nil "You left several ~As out: ~a"
		   quant (nlg forces 'conjoined-defnp)))))))

(defun hint-undrawn-vector (msg vector-quant)
  "Return a hint sequence for a vector that has not yet been drawn by
    the student.  Msg is a format string with two ~a in it for the
    body and time.  It indicates whether one or mulple vectors are
    missing from the diagram."
  (let ((body (second vector-quant))
	(time (time-of vector-quant)))
     (cons (format nil msg (nlg body 'def-np) (nlg  time 'pp))
	   ;defer to operator hints.
           (sg-map-systemEntry->hints (sg-find-vector-entry vector-quant)))))


;; Given an arbitrary expression (defined as a list of atoms and functions)
;; return t iff it is a function (defined recursively) as a list starting
;; with + or - whose cdr is an atom or expression.
(defun sum-p (expr)
  "True if the given expression is a sum, that is, starts with + or -."
  (and (listp expr)
       (or (eql (car expr) '+)
	   (eql (car expr) '-))))


;;; given a sum, which can be a tree of sums e.g, (+ (- (+
;;; ...)...)...), returns the force descriptors mentioned by variables
;;; either at the top level of the sum or embedded in a
;;; multiplication.  Only forces consistent with the given body and
;;; time are returned.  If unless non-forces-ok is true, it will
;;; return NIL if it find an expression that does not contain a force
;;; variable.
(defun forces-in-sum (sum &optional (body '?body) (time '?time) (non-forces-ok nil))
  (let ((comp-pattern `(compo ?xyz ?rot (force ,body ?agent ?type :time ,time)))
	(mag-pattern `(mag (force ,body ?agent ?type :time ,time)))
	(force-pattern `(force ,body ?agent ?type :time ,time)))
    (catch 'non-force-found
      (forces-in-sum1 sum comp-pattern mag-pattern force-pattern non-forces-ok))))

(defun forces-in-sum1 (expr comp-pattern mag-pattern force-pattern non-forces-ok)
  (let (B)
    (cond ((sum-p expr)
	   (loop for addend in (cdr expr) nconc
		 (forces-in-sum1 addend comp-pattern mag-pattern force-pattern non-forces-ok)))
	  ((and (sysvar-p expr)		; compo or mag variable for a force
		(setq b (or (unify (sysvar-to-quant expr) comp-pattern)
			    (unify (sysvar-to-quant expr) mag-pattern))))
	   (list  (subst-bindings b force-pattern)))
	  ((and (listp expr)		; multiplication that includes magnitude variables
		(equal (car expr) '*)
		(loop for factor in (cdr expr) with b2
		    when (and (sysvar-p factor) 
			      (setq b2 (unify (sysvar-to-quant factor) mag-pattern)))
		    collect (subst-bindings b2 force-pattern))))
	  ((and (listp expr)		; unary minus wrapped around a magnitude variable
		(null (cddr expr)) 
		(equal (car expr) '-)
		(sysvar-p (second expr))
		(setq b (unify (sysvar-to-quant (second expr))
			       mag-pattern)))
	   (list (subst-bindings b force-pattern)))
	  (non-forces-ok NIL)
	  (t (throw 'non-force-found NIL)))))
  
(defun vectors-non-zero (vectors direction-angle)
  "Given a set of vector descriptions, returns those that are not perpendicular to the given rotation and not zero"
  (loop for v in vectors 
      with dir
      do (setq dir (vector-direction v))
      unless (or (equal dir 'zero)
		 (perpendicularp direction-angle (vector-direction v)))
      collect v))

(defun vector-direction (v)
  "Given a vector description, return its direction from working memory.
   Assumes the proposition in wm is (vector <body> <vector descr> <dir>)"
  (loop for p in (problem-wm *cp*) 
      when (and (listp p)
		(equal (car p) 'vector)
		(equal (third p) v))
      do (return (fourth p))))

(defun vectors-to-compo-sysvars (xyz rot vectors)
  "Given a list of vector descriptors, return the list of system
   variables for the vector components along the given axis label, rotation and time"
  (loop for v in vectors collect
	(or (quant-to-sysvar `(compo ,xyz ,rot ,v))
	    (warn 'log-condition:log-warn
		  :tag (list 'vectors-to-compo-sysvars xyz rot v)
		  :text "Missing sysvar for given vector compo."))))

;;; (ref eq-Pitt A10 47-02) Students sometimes try to write Newton's
;;; law as <sum of left forces> = <sum of right forces>, where the
;;; left and right forces point in different directions along some
;;; axis.  Their equation works if the forces are all parallel and one
;;; uses their magnitudes in the sums.  Trying to adjust it for other
;;; cases usually leads to errors such as incorrect signs.  This error
;;; class tries to catch such cases and teach a better method.  This
;;; is intended to work even if the student leaves out some forces.
;;; Thus, we don't try to find the corresponding correct equation, nor
;;; do we pass in the correct forces in order to give a bottom out
;;; hint.  If we can get the student to put all the forces on one side
;;; of the equation, then the missing forces error classes will
;;; trigger if necessary.  Moreover, we can't construct a correct
;;; version of NFL as a bottom out hint because the student might not have defined
;;; variables for all the forces yet or drawn the axes.
(def-error-class two-force-sums (?Lhs ?Rhs ?ldir ?rdir)
  ((student-eqn (= ?lhs ?rhs))
   (problem (vector ?body (accel ?body :time ?time) zero)) ; NFL applies
   (bind ?Lforces (forces-in-sum ?lhs ?body ?time))
   (test ?Lforces)
   (bind ?Rforces (forces-in-sum ?rhs ?body ?time))
   (test ?Rforces)
   (bind ?ldir (directed-forces (force-directions ?Lforces)))
   (test ?ldir)
   (bind ?rdir (directed-forces (force-directions ?Rforces)))
   (test ?rdir)
   (test (not (equal ?ldir ?rdir))) ; AW: make sure dirs differ, bug 1329
   )
  :utility 200)
(defun two-force-sums (lhs rhs ldir rdir)
     (list
      ;; teach NLF-as-2-force-sums
      (format nil 
	      (strcat "You have written an equation of the form "
		      "<~Award forces>=<~Award forces>.  This is not "
		      "a good strategy for applying Newton's second law.  "
		      "When the acceleration is zero, you should always write "
		      "Newton's second law in the form <all-forces>=0.") 
	      ldir rdir)
      `(bottom-out (string ,(format nil (strcat "You can rewrite your equation as ~a=0.  There may "
			  "be other errors in your equation, but this form will be "
			  "an improvement.")
	      (nlg (cons '+ (append (if (and (consp lhs) (eq (first lhs) '+)) 
				      (cdr lhs) (list lhs)) 
				    (if (and (consp rhs) (eq (first rhs) '+)) 
					(cdr rhs) (list rhs)))) 'algebra))))))

(defun directed-forces (dirs)
  "Are all the forces pointing in the same direction?"
  (cond ((null dirs) nil)
	((every #'(lambda (x) (< 90 x 270)) dirs) '|left|)
	((every #'(lambda (x) (or (< x 90) (< 270 x))) dirs) '|right|)
	((every #'(lambda (x) (< 0 x 180)) dirs) '|up|)
	((every #'(lambda (x) (< 180 x 360)) dirs) '|down|)
	(t nil)))

(defun force-directions (forces)
  "Given a list of force descriptions, returns their directions (numbers between 0 and 360).  
   If any force lacks a numerical direction, return nil"
  (loop for f in forces with b with dir
      do (setq b (unify-in-wm `(vector ?body ,f ?dir)))
	 (when (null b) (return nil))
	 (setq dir (cdr (get-binding '?dir b)))
	 (when (not (degree-specifierp dir)) (return nil))
     collect (mod (second dir) 360)))
	       

;======================Assignment statements ===========================

;; An "assignment statement" is an equation of the form 
;;	var = [-] number [units]
;; that assigns a value to a variable
;;
;; Note rhs may be DNUM term OR dimensionless number, most commonly 0.
;;
;; Handling initial minus sign is a pain, because "dnum mangling"
;; by parser can produce value expressions of the following form:
;;       (* (- N) (dnum 1 units))    from v = -N units
;; The form (* <constant-expr> (dnum 1 units) was used to translate
;; complex constant expressions with units like "3*pi/4 rad" into a 
;; form that algebra parser will accept.
;;
;; Can also get simple
;;       (- (dnum N units))    from v = -(N units)
;;       (- N)                 from v = - N

(defun negative-numvalp (exp)
"true if exp is negation of a numvalp"
   (and (listp exp)
        (eq (first exp) '-)
	(= (length exp) 2)
	(numvalp (second exp))))

;; Following takes sides of legal prefix equation in systemese.
(defun assignmentp (lhs rhs)
"true if lhs=rhs is assignment of numerical value to variable"
  (and (sysvar-p lhs) 
       (or (numvalp rhs)
           (negative-numvalp rhs))))

;; functions for detecting "givens":
;; Note these may not detect givens in the problem statement
;; that don't wind up used in the solution
(defun given-quant-p (quant)
"true if quantity is tagged as a problem given"
 (let ((eqn (find quant (Problem-eqnindex *cp*) 
		    :key #'eqn-exp :test #'unify)))
     (and eqn (given-eqn-p eqn))))

(defun given-var-p (var)
"true if system var has a given value in current problem"
 (given-quant-p (sysvar-to-quant var)))

(defun unused-given-var-p (var)
"true if system var is for an unused given included in the var index"
  (let ((qvar (find var (problem-varIndex *cp*) :key #'qvar-var)))
    (when qvar 
      (member 'UNUSED-GIVEN (qvar-marks qvar)))))

;; For vectors along axis, if vector mag is given then instructors
;; think of components as given. This detects such component vars
(defun given-component-p (var)
"true if component var is parallel to vector with given magnitude"
  (let ((compo-expr (sysvar-to-quant var)))
    (when (componentp compo-expr)
      (let* ((rot    (compo-axis-rot compo-expr)) 
	     (xyz    (compo-axis-xyz compo-expr))
	     (vector (compo-base-vector compo-expr))
	     (vector-dir (get-quant-value `(dir ,vector))))
        (and (given-quant-p `(mag ,vector))
	     (parallel-or-antiparallelp vector-dir (axis-dir xyz rot)))))))

;; For given zero-length vectors, magnitude usually not tagged as "given"
;; since value comes from implicit equation generated by operators.
;; Thus wrong value for it won't appear to us as wrong value for given.
;; Since zero mag is required for vector to be drawn correctly, entering
;; a non-zero value for mag would be inconsistent with equation from drawing, 
;; so we don't worry about it. Could include special message for that 
;; inconsistency, though.
(defun given-p (var)	; more liberal than given-var-p
"true if var should be treated as a given"
  (or (given-var-p var) 
      (unused-given-var-p var)
      (given-component-p var)))

(defun optionally-given-p (var)
  "true if var should be treated as optionally given"
  (and (or (given-var-p var) (given-component-p var))
       (member `(optionally-given ,(sysvar-to-quant var)) 
	       (Problem-wm *cp*) :test #'unify)))

;; following could be helpers in solution graph module:
(defun get-var-value (var)
"return DNUM term giving system variable's value and units"
 (let ((qvar (find var (Problem-VarIndex *cp*) :key #'qvar-var 
                                               :test #'equal)))
     (when qvar 
         `(dnum ,(qvar-value qvar) ,(qvar-units qvar)))))
    
(defun get-quant-value (quant)
"return DNUM term or number giving quantity's value"
   (get-var-value (quant-to-sysvar quant)))


(defun sysvar-parameter-p (sysvar)
"true if sysvar is a parameter in current problem"
 (or (canonical-var-cancelling-var-p sysvar)
     (canonical-var-answer-var-p sysvar)))
      
;; Classification of possible assignment statement errors: 
;;+[Mixed up var (Value is correct for similar var)] competes similar var rules
;;+[Sign errors]
;;   [wrong sign on component] competes missing-neg-on-vector-component
;;   [neg sign on magnitude var] competes missing-negation-on-vector-magnitude
;;   [Other sign error] competes default sign error
;;+Value error*
;;   [non-zero value for zero-mag vector]
;;   [non-zero value for orthogonal component]
;;   Var is given
;;   Var not given
;;        Var is a parameter
;;            cancels
;;            answer-var
;;        Var is not a parameter
;;            value imprecise
;;             value way off
;; [] = not done yet.
;;
;; !! *right now, value errors compete with sign error handlers if
;; only error is in sign.


; 1. Assigned wrong value for a given.
; !! Will apply for mere sign error in value. 
; Need to compare to other sign error handlers.
(def-error-class wrong-given-value (?var ?wrongval)
  ((student-eqn (= ?var ?wrongval)) ;so far matches any eqn entry
   (student (eqn ?whatever)) ;not an answer box
   (test (assignmentp ?var ?wrongval))
   (test (given-p ?var))))

(defun wrong-given-value (var wrongval) 
 (let* ((studvar (nlg var 'algebra))
	(studval (nlg wrongval 'algebra))
        (quant   (sysvar-to-quant var))
	;; BvdS:  I am not sure that this is the most appropriate
	;; way of accessing the hints associated with a given.
	;; Find any (given ...) associated with the quantity
	(given-enode (find `(given ,quant . ?rest)
			(second (problem-graph *cp*))
			:key #'enode-id :test #'unify))
	;; if the (given ...) has custom hints, grab the hints
	(bindings (when given-enode 
			(unify (enode-id given-enode) 
			       '(given ?qant ?val-expr :hint ?hint-arg))))
	;; optional ?hint-arg has form (?loc ?more)
	(given-loc (or (first (cdr (assoc '?hint-arg bindings)))
	               "from the problem statement"))
	(more      (second (cdr (assoc '?hint-arg bindings))))  ; may be NIL
	(rightval (nlg (or (cdr (assoc '?val-expr bindings))
	                   (get-var-value var)) 'algebra)))
   (list 
	;; Be sure to include full quantity def in message, in case problem
	;; is that student thinks var denotes some other quantity.
        (format nil "~A is not the correct value for ~A, ~A.&nbsp;  ~A can be determined ~A." 
	            studval studvar (nlg quant) studvar given-loc)
	;; ?? should we tell them correct given value?
	`(bottom-out (string ,(format nil "~@[~A  ~]The correct value for ~A is ~A." more studvar rightval)))
   )))

;; Wrong value for a non-given (i.e. calculated) quantity: 
;;  1. Parameter?
;;   -"cancelling-var", a parameter not needing to be solved-for
;;   -"answer-var", a parameter to solve in terms of.
;;  2. Non-parameter:
;;   -special message if they are close but imprecise
;;   -if just plain wrong.
;; Note that some things we think are "non-given" might actually be obvious 
;; from givens, e.g equal and opposite components.
;; Also, among non-givens, might want different message for problem soughts.

(def-error-class value-for-cancelling-var (?var)
  ((student-eqn (= ?var ?wrongval)) ; so far matches any eqn entry
   (test (assignmentp ?var ?wrongval))
   (test (not (given-p ?var)))
   (test (canonical-var-cancelling-var-p ?var))
   ))

(defun value-for-cancelling-var (var) 
 (let* ((studvar (nlg var 'algebra))
        (quant   (sysvar-to-quant var)))
   (list 
        (format nil "The numerical value of ~A, ~A, is not defined in this problem.&nbsp; This value is not needed for the solution.&nbsp; You should be able to enter enough equations so that terms containing ~A will cancel out, and you can ~A the final answer." 
		studvar (nlg quant) studvar *solve-for-quantity*) 
  '(function next-step-help))))

; special case for height if no zero-level defined
(def-error-class value-for-height-no-zero-level (?var)
  ((student-eqn (= ?var ?wrongval)) ; so far matches any eqn entry
   (test (assignmentp ?var ?wrongval))
   (test (not (given-p ?var)))
   ; don't check if parameter: h1 may be a parameter but h2 not 
   ;(test (canonical-var-cancelling-var-p ?var))
   (var-defn ?var (height ?body ?zero-height :time ?time))
   ;; make sure no zero-level stated in givens: RISKY! it's possible a body 
   ;; is never at the zero level (e5a).
   ;; Currently heights happen to be given when this is true, so its OK, 
   ;; but test could fail in future.
   ;; We need to add some other way of identifying the zero level in 
   ;; the problem givens.
   (test (not (find '(given (height ?body ?zero-height :time ?time) (dnum 0 |m|)) 
                     (problem-givens *cp*) :test #'unify)))
   )
  :utility 100)
(defun value-for-height-no-zero-level (var) 
 (let* ((studvar (nlg var 'algebra))
        (quant   (sysvar-to-quant var)))
   (list 
        (format nil "Because this problem does not stipulate a zero-level, the numerical value of ~A, ~A, is not defined.&nbsp; Since only differences in height are relevant to changes in energy, this value is not needed for the solution.&nbsp; Include an equation determining the difference between heights (h2-h1 = ...) and then ~A the final answer." 
		studvar (nlg quant) *solve-for-quantity*) 
   '(function next-step-help))))


(def-error-class value-for-answer-var (?var)
  ((student-eqn (= ?var ?wrongval)) ; so far matches any eqn entry
   (test (assignmentp ?var ?wrongval))
   (test (not (given-p ?var)))
   (test (canonical-var-answer-var-p ?var))
   ))

(defun value-for-answer-var (var) 
 (let* ((studvar (nlg var 'algebra))
        (quant   (sysvar-to-quant var)))
   (list 
        (format nil "The numerical value of  ~A, ~A, is not defined in this problem.  This problem asks for an answer as a function of ~A. Leave the symbol ~A in your solution equations and derive an expression for the sought quantity in terms of ~A and any other requested parameters." studvar (nlg quant) studvar studvar studvar) 
   )))


;; Wrong value for a non-given (i.e. calculated) quantity (cont'd):
;;  2. Non-parameter:
;;   -special message if they are close but imprecise
;;   -if just plain wrong.
;; Note that some things we think are "non-given" might actually be obvious 
;; from givens, e.g equal and opposite components.

(defun inaccuratep (eqn)
"true if solver reports equation as inaccurate."
  (eq (solver-equation-redp eqn) 'inaccurate))
 
(def-error-class imprecise-value-non-given (?var ?wrongval)
 ( (student-eqn (= ?var ?wrongval)) ; so far matches any eqn entry
   (test (assignmentp ?var ?wrongval))
   (test (not (given-p ?var)))
   (test (not (sysvar-parameter-p ?var)))
   (test (inaccuratep `(= ,?var ,?wrongval)))
 ))
(defun imprecise-value-non-given (var wrongval)
 (let* ((studvar (nlg var 'algebra))
	(studval (nlg wrongval 'algebra))
        (quant   (sysvar-to-quant var)))
   (list 
        (format nil "~A does not match the value Andes calculated for ~A, ~A." 
	             studval studvar (nlg quant))
	(format nil 
	      (strcat "Although final answers may be rounded off, Andes requires "
	              "values in equations to agree to within one part in 10<sup>11</sup>. "
		      "It will be easier to stick to the symbol ~A throughout your "
		      "solution equations. You can ~A the final answer when you have entered enough equations "
		      "to determine it." )
	       studvar *solve-for-quantity*) 
   )))

(def-error-class wrong-value-non-given (?var ?wrongval)
  ((student-eqn (= ?var ?wrongval)) ;so far matches any eqn entry
   (student (eqn ?whatever)) ;not an answer box
   (test (assignmentp ?var ?wrongval))
   (test (not (given-p ?var)))
   (test (not (sysvar-parameter-p ?var)))
   ; make sure it's not close:
   (test (not (inaccuratep `(= ,?var ,?wrongval))))
   ;; our advice to use the symbol throughout is a little odd for problem
   ;; soughts since they do have to enter an equation for the answer somewhere.
   ;; Could check:
   ;; (test (not (problem-sought-var-p ?var)))
   ))

(defun wrong-value-non-given (var wrongval) 
  (let ((studval (nlg wrongval 'algebra))
        (quant (sysvar-to-quant var)))
   (list 
    (format nil "~A is not the correct value for ~A.&nbsp;  It will usually be easier to leave the symbol ~A in your solution equations.&nbsp;  You can ~A the final answer when you have entered enough equations to determine it." 
	    studval (var-or-quant quant) (var-or-quant quant)
	    *solve-for-quantity*) 
   )))


;;; ========================== Answer box entries ===========================

;; default wrong-answer is kind of vacuous, but we want something to say:
(def-error-class default-wrong-answer (?quant ?wrongval)
  ((student (answer ?quant))
   (student-eqn ?eqn)
   (bind ?wrongval (third ?eqn))))
(defun default-wrong-answer (quant wrongval)
   (list (format nil "~A is not the correct value for ~A.&nbsp; When you have entered enough equations, you can ~A ~A, then transfer the result to this answer box."
            (nlg wrongval 'algebra) (nlg quant) *solve-for-quantity* 
	    ;; Hack to ignore dummy answer variable defined in 
	    ;; function check-answer.
	    (let ((var (symbols-label quant)))
	      (if (or (null var) (equalp var "Answer"))
		  "this quantity"
		  (var-or-quant quant))))))


;;; ================== Multiple choice entries

;;;
;;; Associate multiple-choice answer with correct entry.


(def-entry-test multiple-choice (?question ?sa ?ca) 
  :preconditions ((student (choose-answer ?question ?sa))
		  (correct (choose-answer ?question ?ca))
		  (test (not (equal ?ca ?sa)))
		  )
  :state +incorrect+
  :hint (list "Incorrect.&nbsp;  Please try again.")
  :order ((correct . 3))
)


;;; ================= Undiagnosed equation errors ==============

;;; If can't diagnose an error, then find out what they intended.
;;; Intended as a catchall that should fire if nothing else does.
;;; We achieve by assigning what should be a lower EU than any 
;;; other equation handler.

(def-error-class undiagnosed-eqn-error (?eqn)
 ((student-eqn ?eqn))
 :utility .001      ; default is 1
 :probability .001  ; default is .1
)

(defun undiagnosed-eqn-error (eqn)
   ;; following routine shows query and does all the work from there
   (list `(function IEA-Main ,eqn))
)

;;;
;;;   general matching for student entries
;;;
(def-entry-test exact-match (?quant)
  :preconditions ((student ?quant)
	       (correct ?quant))
  :state +correct+
  :order ((correct . 1))
  )


(def-entry-test match-with-error (?quant)
  :preconditions ((student (given ?quant ?val1)) ; this is probably wrong?
		  (correct (given ?quant ?val2))
		  ;; compare-dnums does not properly handle disparate units.
		  (test (compare-dnums ?val1 ?val2)))
  :state +correct+
  :order ((correct . 2))
  )

;; This test allows possibility of tolerance in direction via :error in
;; the correct direction dnum. Note non-dnum dirs including unknown must 
;; pass exact-match text so this test doesn't have to handle them.
(def-entry-test match-vector (?quant) 
  :preconditions ((student (vector ?quant ?dir1))
		  (correct (vector ?quant ?dir2)) ; may have :error
		  (test (and (dimensioned-numberp ?dir1) 
		             (dimensioned-numberp ?dir2)
			     (compare-dnums ?dir1 ?dir2))))
  :state +correct+
  :order ((correct . 2))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;              Tests for vector of unknown direction
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First, see if the student has guessed 
;; the correct direction, as determined by the solver.
(def-entry-test match-unknown-vector-to-solver (?quant ?dir1) 
  :preconditions ((student (vector ?quant ?dir1))
		  (correct (vector ?quant unknown))
		  (test (match-direction-to-solver ?dir1 ?quant))
		  )
  :state +correct+
  :order ((correct . 2) (unknown-vector . 4))
  )

(defun match-direction-to-solver (dir quant &key (epsilon 2.5))
  "Determine if dir matches value determined by the solver."
  (let ((at (match-exp->qvar `(dir ,quant) (problem-varindex *cp*))))
    (and at (qvar-value at)
	 (equal (qvar-units at) '|deg|) ;sanity check.
	 (dimensioned-numberp dir)
	 (< (mod (- (qvar-value at) (convert-dnum-to-number dir)) 360) 
	    ;; error bound
	    epsilon))))

;; Then test for vectors drawn out of the plane 
;; "unknown" infers that it lies in the plane
(def-entry-test z-axis-unknown (?quant ?dir1) 
  :preconditions ((student (vector ?quant ?dir1))
		  (correct (vector ?quant unknown))
		  (test (or (eql ?dir1 'into) (eql ?dir1 'out-of))))
  :state +incorrect+
  :hint (list
	 (format nil "Does ~a lie along the z-axis or in the plane?"
		 (nlg ?quant))
	 (strcat "Use " *vector-tool* " to modify the vector so that it lies in the plane."))
  :order ((correct . 2) (unknown-vector . 3))
  )



(defun solver-has-vector-direction (quant)
  "Determine if quantity direction has been determined by the solver."
  (let ((at (match-exp->qvar `(dir ,quant) (problem-varindex *cp*))))
    (and at (qvar-value at))))


;; Then, make sure it doesn't align with any
;; known vector directions.

(def-entry-test align-unknown-vector-with-vector (?other-quant ?dir1) 
  :preconditions ((student (vector ?quant ?dir1))
		  (correct (vector ?quant unknown))
		  ;; See if the direction can eventually be calculated.
		  ;; If not, there is no reason to complain if the
		  ;; student aligns it with another vector.
		  (test (solver-has-vector-direction ?quant))
		  (correct (vector ?other-quant ?dir2))
		  (test (parallel-or-antiparallelp ?dir1 ?dir2))
		  )
  :state +incorrect+
  :hint (list
	 (format nil "Drawing the vector in the direction ~A suggests that it is aligned with ~A." 
		 (nlg ?dir1 'adj) (nlg ?other-quant))
	 "However, the direction of this vector is not given.&nbsp;  Please choose another direction.")
  :order ((correct . 2) (unknown-vector . 2))
  )

;; Then, make sure it doesn't align with any drawn axes.
(def-entry-test align-unknown-vector-with-axes (?quant ?dir1) 
  :preconditions ((student (vector ?quant ?dir1))
		  (correct (vector ?quant unknown))
		  ;; See if the direction can eventually be calculated.
		  ;; If not, there is no reason to complain if the
		  ;; student aligns it with the axes.
		  (test (solver-has-vector-direction ?quant))
		  (old-student (draw-axes ?dir2))
		  (test (and (degrees-or-num ?dir1)
			     (degrees-or-num ?dir2)
			     (< (mod (- (convert-dnum-to-number ?dir1)
					(convert-dnum-to-number ?dir2)) 90) 2)))
		  )
  :state +incorrect+
  :hint (list
	 (format nil "Drawing the vector in the direction ~A suggests that it is aligned with the axes you have drawn." (nlg ?dir1 'adj))
	 "However, the direction of this vector is not given.&nbsp;  Please choose another direction.")
  :order ((correct . 2) (unknown-vector . 1))
  )

;; Finally, accept vector, since it passed the other tests.
(def-entry-test match-unknown-vector-default (?quant ?dir1) 
  :preconditions ((student (vector ?quant ?dir1))
		  (correct (vector ?quant unknown))
		  )
  :state +correct+
  :order ((correct . 2) (unknown-vector . 0))
  )
