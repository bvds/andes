;;;;
;;;;                   Waves and oscillations
;;;;

;;;;
;;;;  The wavelength and wavenumber of a wave
;;;;

(def-qexp wavelength (wavelength ?wave ?medium)
  :units |m|
  :restrictions positive  ;needed for harmonics problems to work
  :english ("the wavelength of ~A in ~A" (nlg ?wave) (nlg ?medium))
  :fromworkbench `(wavelength ,body ,body2))

(defoperator define-wavelength (?wave ?medium)
  :preconditions
  ((bind ?lambda-var (format-sym "lamb_~A_~A" 
				 (body-name ?wave)
				 (body-name ?medium))))
  :effects ((variable ?lambda-var (wavelength ?wave ?medium))
	    (define-var (wavelength ?wave ?medium)))
  :hint 
  ((bottom-out (string "Define a variable for the wavelength of ~A moving in ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave ?medium))))

(def-qexp wavenumber (wavenumber ?wave ?medium)
  :units |rad/m|
  :restrictions nonnegative  
  :english ("the wavenumber of ~A in ~A" (nlg ?wave) (nlg ?medium))
   :fromWorkbench `(wavenumber ,body ,body2))

(defoperator define-wavenumber (?wave ?medium)
  :preconditions 
  ( (bind ?wn-var (format-sym "kwave_~A_~A" 
			      (body-name ?wave)
			      (body-name ?medium))) )
  :effects ( (variable ?wn-var (wavenumber ?wave ?medium))
             (define-var (wavenumber ?wave ?medium)))
  :hint 
  ((bottom-out (string "Define a variable for the wave number of ~A moving in ~A by using the Add Variable command on the Variable menu and selecting wave number."  ?wave ?medium))))

;;; Equation of the wavenumber of the wave, wavenumber*lambda = 2*pi

(def-psmclass wavenumber-lambda-wave (wavenumber-lambda-wave ?wave ?medium)
  :complexity definition  ;substitute implicitly into major equation
  :english ("relation between wavelength and wavenumber")
  :ExpFormat 
  ("applying the equation relating wavenumber and wavelength of ~A in ~A"
	      (nlg ?wave) (nlg ?medium))
  :EqnFormat ("wavenumber*wavelength = 2*pi")) 


 (defoperator wavenumber-lambda-wave-contains (?sought)
   :preconditions (
		   (any-member ?sought ((wavelength ?wave ?medium)
					(wavenumber ?wave ?medium))))
   :effects (
     (eqn-contains (wavenumber-lambda-wave ?wave ?medium) ?sought)))


(defoperator wavenumber-lambda-wave (?wave ?medium)
   :preconditions (
       (variable  ?lamb  (wavelength ?wave ?medium))
       (variable  ?kwave  (wavenumber ?wave ?medium))
   )
   :effects (
    (eqn  (= (* ?lamb ?kwave) (* 2 $p))  ;For pi, must use $p
                (wavenumber-lambda-wave ?wave ?medium))
   )
   :hint (
      (point (string "You can use the definition of wavenumber."))
      (bottom-out (string "Write the equation ~A" 
			  ((= (* ?kwave   ?lamb) (*2 $p)) algebra) ))
      ))

;;;
;;; The frequency, period, and angular frequency of a wave
;;;

(def-qexp frequency (frequency ?wave)
  :units |Hz|
  :restrictions nonnegative 
  :english ("the frequency of ~A" (nlg ?wave))
  :fromWorkbench `(frequency ,body))

(defoperator define-frequency (?wave)
  :preconditions((bind ?freq-var (format-sym "freq_~A" (body-name ?wave))))
  :effects ((variable ?freq-var (frequency ?wave))
	    (define-var (frequency ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the frequency of ~A by using the Add Variable command on the Variable menu and selecting frequency."  ?wave))))

;;;
;;  For doppler problems, we have to introduce a frequency as
;;  observed by someone over some time interval.  We assume
;;  (but do not check) that the time interval is large enough
;;  for the frequency to be well-defined.
;;;

(def-qexp observed-frequency (observed-frequency ?wave ?me :time ?time)
  :units |Hz|
  :restrictions nonnegative 
  :english ("the frequency of ~A as observed by ~A" 
	       (nlg ?wave) (nlg ?me 'at-time ?time))
  :fromWorkbench `(observed-frequency ,body ,body2 :time ,time))

(defoperator define-observed-frequency (?wave ?me ?t)
  :preconditions
  (
   (test (time-intervalp ?t)) ;only defined over intervals
   (bind ?freq-var (format-sym "freq_~A_~A_~A" 
			       (body-name ?wave) (body-name ?me) 
			       (time-abbrev ?t))))
  :effects ((variable ?freq-var (observed-frequency ?wave ?me :time ?t))
	    (define-var (observed-frequency ?wave ?me :time ?t)))
  :hint ((bottom-out 
	  (string "Define a variable for the frequency of ~A as observed by ~A by using the Add Variable command on the Variable menu and selecting observed frequency."  ?wave ?me))))

;;
;; period is used in some circular motion rules:
;;
;; We don't have time on a period. The definition in terms of velocity 
;; allows it to be an instantaneous quantity -- time it *would* take object to 
;; make a complete revolution at its instantaneous speed at t. So period 
;; could change over time as speed does and object never needs to actually 
;; make a complete revolution in its period at a time.  In uniform circular
;; motion velocity is constant, so period could be defined for the interval 
;; of uniform circular motion.  However, in our circular motion problems we 
;; usually represent this constant state by analyzing a representative instant 
;; which is usually the only instant in the problem.  So we just assume that 
;; and leave out time.
;;

(def-qexp period (period ?body)
   :units |s|
   :restrictions positive
   :fromWorkbench `(period ,body)
   :english ("the period of the motion of ~A" (nlg ?body)))

(defoperator define-period-var (?b)
  :preconditions ( 
        (bind ?T-var (format-sym "T_~A" (body-name ?b)))
  )
  :effects (
      (variable ?T-var (period ?b))
      (define-var (period ?b))
   )
 :hint
  ((bottom-out (string "Use the Add Variable command located under 'variable' on the top menu bar and select Period to define a variable for the period of the motion of ~A." ?b))
   ))

(def-qexp angular-frequency (angular-frequency ?wave)
  :units |rad/s|
  :restrictions nonnegative 
  :english ("the angular-frequency of ~A" (nlg ?wave))
  :fromworkbench `(angular-frequency ,body))

(defoperator define-angular-frequency (?wave)
  :preconditions((bind ?omega-var (format-sym "omega_~A" (body-name ?wave))))
  :effects ((variable ?omega-var (angular-frequency ?wave))
	    (define-var (angular-frequency ?wave)))
  :hint (
	 (bottom-out (string "Define a variable for the angular-frequency of ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave))))

;;equation of the frequency of the wave, frequency = angular-frequency/2*pi
(def-psmclass frequency-of-wave (frequency-of-wave ?object)
  :complexity definition  ;substitute implicitly into major equation
  :english ("the equation for the frequency of a wave")
  :ExpFormat ("applying the definition angular frequency to ~A"
	      (nlg ?object))
  :EqnFormat ("frequency = angular-frequency/(2*$p)")) 

(defoperator frequency-of-wave-contains (?sought)
  :preconditions (
		  (any-member ?sought ( 
				       (angular-frequency ?object)
				       (frequency ?object))))
  :effects ((eqn-contains (frequency-of-wave ?object) ?sought)))

(defoperator frequency-of-wave (?object)
  :preconditions (
		  (variable  ?omega  (angular-frequency ?object))
		  (variable  ?freq  (frequency ?object))
		  )
  :effects (
	    (eqn  (= ?omega (* 2 $p ?freq)) ;pi must be $p
	    (frequency-of-wave ?object)))
  :hint (
	 (point (string "You can use equation for the frequency of a wave"))
	 ;;(teach (string "The equation-of-frequency-of-wave states that the frequency of a wave is angular-frequency/(2*pi)"))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?omega (*2 $p ?freq))  algebra) ))
	 ))

;; There are two versions for beat frequency: one for
;; the "timeless" intrinsic frequency and the other for
;; observed-frequency.  As much as possible, these should 
;; appear to the user as the same equation.

;;equation beat frequency for two waves
(def-psmclass beat-frequency (beat-frequency ?wbeat ?w1 ?w2 ?me ?t)
  :complexity major 
  :english ("the beat frequency of two waves")
  :ExpFormat ("finding the beat frequency of waves ~A and ~A"
	      (nlg ?w1) (nlg ?w2))
  :EqnFormat ("fbeat = (f1-f2)/2")) 

(defoperator beat-frequency-contains (?sought)
  :preconditions 
  ( (sinusoidal ?w1)			;only valid for sine waves
    (sinusoidal ?w2)
    (beat-frequency ?wbeat ?w1 ?w2)
    (any-member ?sought ((observed-frequency ?wbeat ?me :time ?t) 
			 (observed-frequency ?w1 ?me :time ?t)
			 (observed-frequency ?w2 ?me :time ?t)
			 ))
    (test (time-intervalp ?t)) )
  :effects 
  ( (eqn-contains (beat-frequency ?wbeat ?w1 ?w2 ?me ?t) ?sought) ))

(defoperator timeless-beat-frequency-contains (?sought)
  :preconditions 
  ( (sinusoidal ?w1)  ;only valid for sine waves
    (sinusoidal ?w2)
    (beat-frequency ?wbeat ?w1 ?w2)
    (any-member ?sought ((frequency ?wbeat) 
			 (frequency ?w1)
			 (frequency ?w2))) )
  :effects 
  ( (eqn-contains (beat-frequency ?wbeat ?w1 ?w2 nil nil) ?sought) ))

(defoperator write-beat-frequency (?wbeat ?w1 ?w2 ?me ?t)
  :preconditions 
  ( (variable  ?f1 (observed-frequency ?w1 ?me :time ?t))
    (variable  ?f2 (observed-frequency ?w2 ?me :time ?t))
    (variable  ?fbeat (observed-frequency ?wbeat ?me :time ?t)) )
  :effects 
  ( (eqn  (= ?fbeat (* 0.5 (abs (- ?f1 ?f2)))) 
	  (beat-frequency ?wbeat ?w1 ?w2 ?me ?t)) )
  :hint 
  ( (point (string "You can use equation for the beat frequency"))
    (teach (string "The beat frequency for two waves is one half the difference in frequency."))
    (bottom-out (string "Write the equation ~A" 
			((= ?fbeat (* 0.5 (abs (- ?f1 ?f2))))  algebra) )) ))

;; it would be nice to somehow combine this with the above rule...
(defoperator write-timeless-beat-frequency (?wbeat ?w1 ?w2 ?me ?t)
  :preconditions 
  ( (test (and (equal ?me 'nil) (equal ?t 'nil)))
    (variable  ?f1 (frequency ?w1))
    (variable  ?f2 (frequency ?w2))
    (variable  ?fbeat (frequency ?wbeat)) )
  :effects 
  ( (eqn  (= ?fbeat (* 0.5 (abs (- ?f1 ?f2)))) 
	  (beat-frequency ?wbeat ?w1 ?w2 ?me ?t)) )
  :hint 
  ( (point (string "You can use equation for the beat frequency"))
    (teach (string "The beat frequency for two waves is one half the difference in frequency."))
    (bottom-out (string "Write the equation ~A" 
			((= ?fbeat (* 0.5 (abs (- ?f1 ?f2))))  algebra) )) ))

;;;
;;;  Relate frequency and period of a wave

;;equation of the period of the wave, period = 1/frequency
(def-psmclass period-of-wave (period-of-wave ?object)
  :complexity definition      ;substitute implicitly into major equation
  :english ("the equation for the period")
  :ExpFormat ("applying the definition of period to ~A"
	      (nlg ?object))
  :EqnFormat ("period = 1/frequency")) 

(defoperator period-of-wave-contains (?sought)
  :preconditions (
		  (any-member ?sought ( 
				       (period ?object)
				       (frequency ?object))))
  :effects (
	    (eqn-contains (period-of-wave ?object) ?sought)))

(defoperator period-of-wave (?object)
  :preconditions (
		  (variable  ?period (period ?object))
		  (variable  ?freq  (frequency ?object)))
  :effects (
	    (eqn  (= (* ?freq ?period) 1) 
		  (period-of-wave ?object)))
  :hint (
	 (point (string "You can use equation for the period of a wave"))
	 ;;(teach (string "The equation-of-period-of-wave states that the frequency of a wave is 1/period"))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?freq (/ 1 ?period))  algebra) ))
	 ))

;;;
;;;  Hamonics of standing waves

;;  The allows one to do things in terms of either frequency or wavelength
;;
(def-psmclass harmonic-of (harmonic-of ?waven ?wave1 ?form)
  :complexity minor
  :english ("harmonic of a standing wave")
  :ExpFormat ("using the fact that ~A is a harmonic of ~A"
	      (nlg ?waven) (nlg ?wave1))
  :eqnFormat ("fn = n*f1 or $ln = $l1/n)"))

(defoperator harmonic-of-contains (?sought)
  :preconditions 
  ( (harmonic-of ?waven ?mult ?wave1)
    (any-member ?sought (
			 (wavelength ?waven ?medium)
			 (frequency ?waven)
			 (wavelength ?wave1 ?medium)
			 (frequency ?wave1)))
    ;; ?form is nil for frequency and ?medium for wavelength
    (bind ?form (if (eq (first ?sought) 'wavelength) ?medium))
    )
  :effects ( (eqn-contains (harmonic-of ?waven ?wave1 ?form) ?sought)
	     ))

(defoperator write-harmonic-of (?waven ?wave1)
  :preconditions 
  ((harmonic-of ?waven ?mult ?wave1)	;get ?mult
   (bind ?quantw (if ?form 'wavelength 'frequency))
   (bind ?q1 (if ?form  (list 'wavelength ?wave1 ?form) 
	       (list 'frequency ?wave1)))
   (bind ?qn (if ?form  (list 'wavelength ?waven ?form) 
	       (list 'frequency ?waven)))
   (variable ?v1 ?q1)
   (variable ?vn ?qn)
   (bind ?rhs (if ?form `(/ ,?v1 ,?mult) `(* ,?v1 ,?mult) )) )
  :effects ( (eqn (= ?vn ?rhs) 
		  (harmonic-of ?waven ?wave1 ?form))
	    (assume use-harmonic-of ?wave1 ?form) )
  :hint (
	 (point (string "~A is the ~:R harmonic of ~A" ;prints as ordinal
			?waven (?mult identity) ;so nlg just returns ?mult  
			?wave1)) 
	 (teach (string "You can determine ~A of ~A from ~A of ~A" 
			?quantw ?waven ?quantw ?wave1)) 
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?vn ?rhs) algebra)))
	 ))

;; only allow one form of the harmonic equation to be
;; used on a given system.
(defnogood harmonic-of-strategy
    ((use-harmonic-of ?wave1 ?form1)
     (use-harmonic-of ?wave1 ?form2)
     (test (not (equal ?form1 ?form2))))
  :Specs ("Prevent one from using both forms of the harmonic equation a given system.")
  :message (Should only use one form of harmonic equation for a given system.))

;;;
;;;   Wave speed, this is |phase velocity|

(def-qexp wave-speed (wave-speed ?medium)
  :units |m/s|
  :restrictions nonnegative
  :english ("the speed of waves in ~A" (nlg ?medium)) ;see entry in errors.cl
  :fromworkbench `(wave-speed ,body))

(defoperator define-wave-speed (?medium)
  :preconditions
  ( (wave-medium ?medium) ;must be object waves can move through
    (bind ?wv-var (format-sym "wv_~A" (body-name ?medium))))
  :effects ((variable ?wv-var (wave-speed ?medium))
	    (define-var (wave-speed ?medium)))
  :hint ((bottom-out 
	  (string "Define a variable for the speed of waves in ~A by using the Add Variable command on the Variable menu and selecting speed of wave."  ?medium))))

;;; equation of the speed of the wave, speed = freq* wavelength

;; Only for sinusoidal waves (where freq & wavelength are well-defined)

(def-psmclass speed-of-wave (speed-of-wave ?object ?medium ?form)
  :complexity major ; must use explicitly 
  :english ("the equation of the speed of a wave")
  :ExpFormat ("relating wavelength and frequency to the speed of wave ~A"
	      (nlg ?object))
  :EqnFormat ("v = $l*f or v = $w/k")) 

;; usual form in terms of wavelength and frequency
(defoperator speed-of-wave-contains (?sought)
  :preconditions 
  ( (sinusoidal ?object)		;so wavelength is defined
    (wave-medium ?medium)		;object waves can move through
    (any-member ?sought ((wave-speed ?medium)
			 (frequency ?object)
			 (wavelength ?object ?medium)))  )
  :effects 
  ( (eqn-contains (speed-of-wave ?object ?medium t) ?sought)))

;; alternative form in terms of wavenumber and omega
(defoperator alternative-speed-of-wave-contains (?sought)
  :preconditions 
  ( (not (suppress-alternative-speed-of-wave))
    (sinusoidal ?object)		;so wavelength is defined
    (wave-medium ?medium)		;object waves can move through
    (any-member ?sought ((wave-speed ?medium)
			 (angular-frequency ?object)
			 (wavenumber ?object ?medium)))  )
  :effects 
  ( (eqn-contains (speed-of-wave ?object ?medium nil) ?sought)))

;; this follows the approach used in harmonic-of
(defoperator write-speed-of-wave (?object ?medium)
  :preconditions 
  ( (variable  ?vw  (wave-speed ?medium))
    (bind ?x1 (if ?form (list 'frequency ?object) 
		(list 'angular-frequency ?object)))
    (bind ?x2 (if ?form (list 'wavelength ?object ?medium)
		(list 'wavenumber ?object ?medium)))
    (variable  ?y1 ?x1)
    (variable  ?y2 ?x2)
    ;; right hand side of equation: lambda*f or omega/k
    (bind ?rhs (if ?form `(* ,?y1 ,?y2) `(/ ,?y1 ,?y2)))
  )
  :effects 
  ((eqn  (= ?vw ?rhs) (speed-of-wave ?object ?medium ?form)))
  :hint 
  ( (point (string "You can apply the equation for the speed of a wave." 
		   ?medium))
     (teach (string "~A moves in ~A at a constant speed." 
		   ?object ?medium))
    (bottom-out (string "Write the equation ~A" 
			((= ?vw ?rhs) algebra) )) ))

;; speed of object is wave speed
(def-psmclass speed-equals-wave-speed (speed-equals-wave-speed ?object ?rope ?t)
  :complexity minor ; used implicitly 
  :english ("the speed of any wave is the same")
  :ExpFormat ("noting the speed of ~A is the same as the wave speed of ~A"
	      (nlg ?object) (nlg ?rope))
  ) 

(defoperator speed-equals-wave-speed-contains (?sought)
  :preconditions (
		  ;; only if defined this way in the problem
		  (in-wm (wave-speeds-equal ?object ?rope))
		  (any-member ?sought ( 
				       (wave-speed ?rope)
				       (speed ?object :time ?t)
				       (duration ?t)))
		  (time ?t))
  :effects (
	    (eqn-contains (speed-equals-wave-speed ?object ?rope ?t) 
			  ?sought)))

(defoperator speed-equals-wave-speed (?object ?rope ?t)
  :preconditions (
		  (variable  ?v1  (wave-speed ?rope))
		  (variable  ?v2  (speed ?object :time ?t)))
  :effects (
	    (eqn  (= ?v1 ?v2) 
		  (speed-equals-wave-speed ?object ?rope ?t) ))
  :hint 
  ( (point (string "The velocity of any wave on a given string, rope, et cetera is the same."))
    (point (string "If there are two waves on a string, then they have equal speeds."))
    (bottom-out (string "Write the equation ~A" 
			((= ?v1 ?v2) algebra) )) ))



;;;         Index of refraction

;; in principle, this should be a function of frequency, 
;; but we don't have any problems with this yet

(def-qexp index-of-refraction (index-of-refraction ?medium)
  :units NIL  ;dimensionless
  :restrictions nonnegative
  :english ("the index of refraction of ~A" (nlg ?medium))
  :fromworkbench `(index-of-refraction ,body))

(defoperator define-index-of-refraction (?medium)
  :preconditions
  ( (wave-medium ?medium) ;must be object waves can move through
    (bind ?n-var (format-sym "n_~A" (body-name ?medium))))
  :effects ((variable ?n-var (index-of-refraction ?medium))
	    (define-var (index-of-refraction ?medium)))
  :hint ((bottom-out 
	  (string "Define a variable for the index of refraction of ~A by using the Add Variable command on the Variable menu and selecting index of refraction."  ?medium))))

;;;; Relate index of refraction to wave-speed

(def-psmclass wave-speed-refraction (wave-speed-refraction ?type . ?media)
  :complexity major			; must explicitly use
  :english ("the definition of index of refraction")
  :ExpFormat ("relating the speed of waves in ~A to the index of refraction" 
	      (nlg ?media 'conjoined-defnp))
  :EqnFormat ("vw1*n1 = vw2*n2")) 

(defoperator wave-speed-refraction-contains (?sought)
  :preconditions 
  ( (any-member ?sought ((wave-speed ?medium1)
			 (index-of-refraction ?medium1)))
    (wave-medium ?medium1)
    (not (vacuum ?medium1))
    (wave-medium ?medium2)
    (not (vacuum ?medium2))
    (test (not (eq ?medium1 ?medium2)))
    (bind ?media (sort (list ?medium1 ?medium2) #'expr<)) )
  :effects ( (eqn-contains (wave-speed-refraction t . ?media) ?sought) ))

(defoperator wave-speed-refraction-vacuum-contains (?sought)
  :preconditions 
  ( (any-member ?sought ((wave-speed ?medium1)
			 (index-of-refraction ?medium1)))
    (wave-medium ?medium1)
    (wave-medium ?medium2)
    (in-wm (vacuum ?medium2)) ;thus rhs will have n=1, simplify equation
    (test (not (eq ?medium1 ?medium2))) )
  :effects 
  ( (eqn-contains (wave-speed-refraction nil ?medium1 ?medium2) ?sought) )
  )

(defoperator write-wave-speed-refraction (?medium1 ?medium2)
  :preconditions 
  ( (variable  ?v1 (wave-speed ?medium1))
    (variable  ?n1 (index-of-refraction ?medium1))
    (variable  ?v2 (wave-speed ?medium2))
    (variable  ?n2 (index-of-refraction ?medium2)) 
    (bind ?rhs (if ?type `(* ,?v2 ,?n2) ?v2)) )
  :effects ( (eqn  (= (* ?v1 ?n1) ?rhs)
		   (wave-speed-refraction ?type ?medium1 ?medium2)) )
  :hint (
	 (hint (string "Relate the speed of waves in ~A to those in ~A." ?medium1 ?medium2))
	 (pont (string "The index of refraction relates the speed of waves in a medium to the speed of waves in another medium."))
	 (bottom-out (string "Write the equation ~A" 
			     ((= (* ?v1 ?n1) ?rhs) algebra) ))
	 ))


;;;  Wave speed for various objects   

;; If medium is "light" then set its wave-speed to c
(def-psmclass wave-speed-light (wave-speed-light ?medium)
  :complexity definition
  :english ("the speed of a light or radio wave")
  :ExpFormat("setting wave speed to c")
  :EqnFormat("vw=c"))

(defoperator wave-speed-light-contains (?sought)
  :preconditions (
		  (vacuum ?medium)
		  (any-member ?sought ((wave-speed ?medium))))
  :effects (
	    (eqn-contains (wave-speed-light ?medium) ?sought)))

(defoperator write-wave-speed-light (?medium)
  :preconditions ((variable  ?v (wave-speed ?medium)))
  :effects
  ;; c is predefined, see file constants.cl
  ( (eqn  (= ?v |c|) (wave-speed-light ?medium)) )
  :hint (
	 (point (string "Light waves and radio waves have a special speed"))
	 (teach (string "c is defined to be the speed of light."))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?v |c|)  algebra) ))
	 ))

;;; set the index of refraction of vacuum to 1

(def-psmclass refraction-vacuum (refraction-vacuum ?medium)
  :complexity definition
  :english ("the index of refraction of a vacuum")
  :ExpFormat("setting the index of refraction to 1")
  :EqnFormat("n=1"))

(defoperator vacuum-refraction-contains (?sought)
  :preconditions (
		  (vacuum ?medium)
		  (any-member ?sought ((index-of-refraction ?medium))))
  :effects (
	    (eqn-contains (refraction-vacuum ?medium) ?sought)))

(defoperator write-vacuum-refraction (?medium)
  :preconditions ((variable  ?n (index-of-refraction ?medium)))
  :effects ( (eqn  (= ?n 1.0) (refraction-vacuum ?medium)) )
  :hint (
	 (hint (string "What is the index of refraction of ~A?" ?medium))
	 (teach (string "The index of refraction of a vacuum is 1.  Some materials (like air) have an index of refraction that is very close to 1."))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?n 1.0)  algebra) ))
	 ))

;;; Speed of transverse wave on a string

;;; define tension of a rope.

;; In principle, this should be connected with the
;; tension force applied to an object...

(def-qexp string-tension (string-tension ?rope)
  :units |N|
  :restrictions nonnegative 
  :english ("the string-tension of ~A" (nlg ?rope))
  :fromworkbench `(string-tension ,body))

(defoperator define-string-tension (?rope)
  :preconditions(
		 (string ?rope)
		 (bind ?t-var (format-sym "Ft_~A" (body-name ?rope))))
  :effects (
	    (variable ?t-var (string-tension ?rope))
	    (define-var (string-tension ?rope)))
  :hint ((bottom-out 
	  (string "Define a variable for the tension of ~A by using the Add Variable command on the Variable menu and selecting tension."  ?rope))))

;;; speed of transverse waves on a string
(def-psmclass wave-speed-string (wave-speed-string ?wave)
  :complexity major			; must explicitly use
  :english ("Transverse wave velocity of a string")
  :ExpFormat ("using formula for transverse wave speed on a string")
  :EqnFormat ("v_wave = sqrt(F_T/mu)")) 


(defoperator wave-speed-string-contains (?sought)
  :preconditions (
		  (string ?wave)
		  (any-member ?sought ((mass-per-length ?wave)
				       (string-tension ?wave)
				       (wave-speed ?wave))))
  :effects (
	    (eqn-contains (wave-speed-string ?wave ) ?sought)))

(defoperator wave-speed-string (?string)
  :preconditions (
		  (variable  ?mu-var  (mass-per-length ?string))
		  (variable  ?vw  (wave-speed ?string))
		  (variable ?tension (string-tension ?string))
		  )
  :effects (
	    (eqn  (= (^ ?vw 2) (/ ?tension ?mu-var))
		  (wave-speed-string ?string))
	    )
  :hint (
	 (point (string "Find a formula for the speed of transverse waves on a string."))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?vw (sqrt (/ ?tension ?mu-var))) algebra) ))
	 ))


;;;   The amplitude of a wave and associated quantities
;;   (usually, the amplitude is just given in a problem
;;    and the student must identify the quantity.

(def-qexp amplitude (amplitude ?wave)
  :units |m|
  :restrictions nonnegative 
  :english ("the amplitude of ~A" (nlg ?wave))
  :fromworkbench `(amplitude ,body))

(defoperator define-amplitude (?wave)
  :preconditions((bind ?lambda-var (format-sym "amp_~A" (body-name ?wave))))
  :effects ((variable ?lambda-var (amplitude ?wave))
	    (define-var (amplitude ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the amplitude of ~A by using the Add Variable command on the Variable menu and selecting amplitude."  ?wave))))

;;; define maximum speed of transverse motion
(def-qexp amplitude-max-speed (amplitude-max-speed ?wave)
  :units |m/s|
  :restrictions nonnegative 
  :english ("the maximum speed of ~A" (nlg ?wave))
  :fromworkbench `(amplitude-max-speed ,body))

(defoperator define-amplitude-max-speed (?wave)
  :preconditions((bind ?lambda-var (format-sym "vmax_~A" (body-name ?wave))))
  :effects ((variable ?lambda-var (amplitude-max-speed ?wave))
	    (define-var (amplitude-max-speed ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the maximum speed of ~A by using the Add Variable command on the Variable menu and selecting maximum speed."  ?wave))))
;; Yuck!  In the real world, one would derive this...
(def-psmclass max-transverse-speed-wave (max-transverse-speed-wave ?wave)
  :complexity major  ; must explicitly use
  :english ("Formula for maximum speed of an oscillation")
  :ExpFormat ("applying the formula for maximum speed of an oscillation")
  :EqnFormat ("v_max=A $w")) 


 (defoperator max-transverse-speed-wave-contains (?sought)
   :preconditions (
		   (sinusoidal ?wave)
		   (any-member ?sought ((amplitude-max-speed ?wave)
					(amplitude ?wave)
					(angular-frequency ?wave))))
   :effects (
     (eqn-contains (max-transverse-speed-wave ?wave ) ?sought)))


(defoperator max-transverse-speed-wave (?wave)
   :preconditions (
       (variable  ?vmax  (amplitude-max-speed ?wave))
       (variable  ?a  (amplitude ?wave))
       (variable  ?w  (angular-frequency ?wave))
   )
   :effects (
    (eqn  (= ?vmax (* ?a ?w))  
                (max-transverse-speed-wave ?wave))
   )
   :hint (
      (point (string "Find a formula for the maximum speed of an oscillation"))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?vmax (* ?a ?w)) algebra) ))
      ))


;;; define |maximum transverse accleration|
(def-qexp amplitude-max-abs-acceleration (amplitude-max-abs-acceleration ?wave)
  :units |m/s^2|
  :restrictions nonnegative 
  :english ("the |maximum acceleration of ~A|" (nlg ?wave))
  :fromworkbench `(amplitude-max-abs-acceleration ,body))

(defoperator define-amplitude-max-abs-acceleration (?wave)
  :preconditions((bind ?lambda-var (format-sym "amax_~A" (body-name ?wave))))
  :effects ((variable ?lambda-var (amplitude-max-abs-acceleration ?wave))
	    (define-var (amplitude-max-abs-acceleration ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for |maximum acceleration of ~A| by using the Add Variable command on the Variable menu and selecting |maximum acceleration|."  ?wave))))

;; Yuck!  In the real world, one would derive this...
(def-psmclass max-transverse-abs-acceleration-wave (max-transverse-abs-acceleration-wave ?wave)
  :complexity major  ; must explicitly use
  :english ("Formula for |maximum acceleration| of an oscillation")
  :ExpFormat ("applying the formula for |maximum acceleration| of an oscillation")
  :EqnFormat ("v_max=A $w")) 

 (defoperator max-transverse-abs-acceleration-wave-contains (?sought)
   :preconditions (
		   (sinusoidal ?wave)
		   (any-member ?sought ((amplitude-max-abs-acceleration ?wave)
					(amplitude ?wave)
					(angular-frequency ?wave))))
   :effects (
     (eqn-contains (max-transverse-abs-acceleration-wave ?wave ) ?sought)))


(defoperator max-transverse-abs-acceleration-wave (?wave)
   :preconditions (
       (variable  ?amax  (amplitude-max-abs-acceleration ?wave))
       (variable  ?a  (amplitude ?wave))
       (variable  ?w  (angular-frequency ?wave))
   )
   :effects (
    (eqn  (= ?amax (* ?a (^ ?w 2)))  
                (max-transverse-abs-acceleration-wave ?wave))
   )
   :hint (
      (point (string "Find a formula for the maximum acceleration of an oscillation"))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?amax (* ?a (^ ?w 2))) algebra) ))
      ))

;;;  Frequency for mass and spring.

;;  This is kind of lousy:  only works for one spring acting
;;  on a block and does not check any other forces that
;;  might be acting on the block.

(def-psmclass spring-mass-oscillation (spring-mass-oscillation ?block ?spring)
  :complexity major			; must explicitly use
  :english ("Formula for period of mass and spring")
  :ExpFormat ("using formula for period of oscillations of a mass and spring")
  :EqnFormat ("T = 2*$p*sqrt(m/k)")) 

(defoperator spring-mass-oscillation-contains (?sought)
  :preconditions (
		  (sinusoidal ?block)
		  (spring-contact ?block ?spring . ?dontcare)
		  (any-member ?sought ((period ?block)
				       (mass ?block)
				       (spring-constant ?spring)))
		  )
  :effects (
	    (eqn-contains (spring-mass-oscillation ?block ?spring) ?sought)))

(defoperator spring-mass-oscillation (?block ?spring)
  :preconditions (
		  (variable  ?t (period ?block))
		  (variable  ?m  (mass ?block))
		  (variable ?k (spring-constant ?spring))
		  )
  :effects (
	    ;; BvdS:  No solution found if this is in sqrt form.
	    (eqn (= (* ?t ?t ?k) (* 4 $p $p ?m)) ;must use $p for pi
		 (spring-mass-oscillation ?block ?spring))
	    )
  :hint (
	 (point (string "Find a formula for the period of oscillation of a mass and spring"))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?t (* 2 $p (sqrt (/ ?m ?k)))) algebra) ))
	 ))

;;;  Frequency for simple pendulum

;;  This is kind of lousy:  
;; Since it is not done as a true F=ma problem, it does not
;; properly check for other forces on the mass.
(def-psmclass pendulum-oscillation (pendulum-oscillation ?block ?rod ?planet)
  :complexity major			; must explicitly use
  :english ("Formula for period of a simple pendulum")
  :ExpFormat ("using formula for period of a pendulum")
  :EqnFormat ("T = 2*$p*sqrt(l/g)")) 

(defoperator pendulum-oscillation-contains (?sought)
  :preconditions (
		  (sinusoidal ?block)
		  (massless ?rod)
		  (pendulum ?block ?rod)
		  (near-planet ?planet :body ?block ?block)
		  (any-member ?sought ((period ?block)
				       (length ?rod)
				       (gravitational-acceleration ?planet)
				       ))
		  )
  :effects (
	    (eqn-contains (pendulum-oscillation ?block ?rod ?planet
						) ?sought)))

(defoperator pendulum-oscillation (?block ?rod ?planet
				   )
  :preconditions (
		  (variable  ?t (period ?block))
		  (variable  ?g-var (gravitational-acceleration ?planet))
		  (variable ?l (length ?rod))
		  )
  :effects (
	    ;; BvdS:  I couldn't get this to work in sqrt form.
	    (eqn  (= (* ?t ?t ?g-var) (* 4 $p $p ?l)) ;must use $p for pi
		  (pendulum-oscillation ?block ?rod ?planet))
	    )
  :hint (
	 (point (string "Find a formula for the period of oscillation of a pendulum"))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?t (* 2 $p (sqrt (/ ?l ?g-var)))) algebra) ))
	 ))


;;; Doppler shift in two dimensions.  

;; (The two-dimensional nature should be transparent to the student since 
;; all the problems are one-dimensional.)

(def-psmclass doppler-frequency (doppler-frequency ?source ?wave ?observer 
						   ?t-s ?t-o)
  :complexity major			; must explicitly use
  :english ("Formula for doppler frequency shift")
  :ExpFormat ("using formula for doppler frequency")
  ;; use implicit format args to insert the plus-minus character code into 
  ;; the EqnFormat string using only standard characters in our source text
  :EqnFormat ("fo=fs*(vw~Avo)/(vw~Avs)" (code-char 177) (code-char 177)))

;; velocities should be constant over the interval.  
;; We should demand (constant ?quant ?t-s) and (constant ?quant ?t-o) 
;; but hopefully that construct will eventually be replaced.

(defoperator doppler-frequency-contains (?sought)
  :preconditions 
  (
   (doppler-system ?source ?medium ?observer) ;so the ?medium is identified
   (time ?t-s) (test (time-intervalp ?t-s))
   (time ?t-o) (test (time-intervalp ?t-o))
   ;;  In principle, the beginning of the ?t-o interval must be 
   ;;  after the beginning of ?t-s interval and the end of the ?t-o 
   ;;  interval should not come to much after the end of the end of 
   ;;  the ?t-s interval.
   ;;  But we won't check these things since we don't know the distance
   ;;  between the source and observer.
   ;;  Note that tsomewhat-earlierp allows ?t-s = ?t-o
   (test (tsomewhat-earlierp ?t-s ?t-o)) 
   (any-member ?sought ((frequency ?source)
			(wave-speed ?medium)
			(observed-frequency ?source ?observer :time ?t-o)
			;; we *assume* the interval is big enough
			(mag (relative-vel ?source ?medium :time ?t-s)) 
			(mag (relative-vel ?observer ?medium :time ?t-o))
			(dir (relative-vel ?source ?medium :time ?t-s)) 
			(dir (relative-vel ?observer ?medium :time ?t-o))
			(dir (relative-position ?source ?observer :time ?t-o))
			))
   (object ?source)
   (object ?observer)
   (sinusoidal ?source)
   (not (vacuum ?medium))			;light uses a different formula
   )
  :effects 
  ( (eqn-contains (doppler-frequency ?source ?medium ?observer 
				     ?t-s ?t-o) ?sought)))

(defoperator make-doppler-frequency (?source ?medium ?observer ?t-s ?t-o)
  :preconditions 
  ((variable ?vs (mag (relative-vel ?source ?medium :time ?t-s)))
   (variable ?vo (mag (relative-vel ?observer ?medium :time ?t-o)))
   (variable ?vw (wave-speed ?medium))		  
   (variable ?fs (frequency ?source))		  
   (variable ?fo (observed-frequency ?source ?observer :time ?t-o))
   ;; vector from observer to source
   ;; BvdS:  this is 180 deg off from phi in my notes
   ;; BvdS:  maybe want to specify that this is as a given so
   ;; that students don't have to draw the vector
   ;;
   ;; (variable ?phi (dir (relative-position ?source ?observer :time ?t-o)))
   (given (dir (relative-position ?source ?observer :time ?t-o)) ?phi)
   ;; Doesn't work:
   ;; (vector ?observer (relative-position ?source ?observer :time ?t-o) ?phi)
   
   ;; use vector statements so that zero velocity can be handled correctly.  
   ;; This might prevent relative-vel from being the sought.
   ;; This is in working memory, because the magnitudes were found above.
   (in-wm (vector ?source (relative-vel ?source ?medium :time ?t-s) ?sdir))
   (in-wm (vector ?observer (relative-vel ?observer ?medium :time ?t-o) ?odir))
   (bind ?scos (cos (* (get-angle-between ?phi ?sdir) 
		       (/ pi 180))))	;degrees to radians
   (bind ?ocos (cos (* (get-angle-between ?phi ?odir) 
		       (/ pi 180))))	;degrees to radians
   ;; If we had a real smp doing the algebraic simplifications, 
   ;; this nonsense would not be needed:
   (bind ?sterm (cond ((eql ?sdir 'zero) ?vw)
		      ((equal ?scos 1.0) `(+ ,?vw ,?vs))
		      ((equal ?scos -1.0) `(- ,?vw ,?vs))
		      (t `(+ ,?vw (* ,?scos ,?vs)))))
   (bind ?oterm (cond ((eql ?odir 'zero) ?vw)
		      ((equal ?ocos 1.0) `(+ ,?vw ,?vo))
		      ((equal ?ocos -1.0) `(- ,?vw ,?vo))
		      (t `(+ ,?vw (* ,?ocos ,?vo)))))
   (optional (body ?source))		;allow draw source and observers  
   (optional (body ?observer))		
   (optional (axis-for ?source x 0)) ;allow draw standard axes 
   ;; motion descriptions for fancy hints:
   (bind ?stea (if (eql ?sdir 'zero) "not moving~*"
		 (if (< ?scos 0)  "moving towards ~A" "moving away from ~A")))
   (bind ?otea (if (eql ?odir 'zero) "not moving~*"
		 (if (> ?ocos 0) "moving towards ~A" "moving away from ~A")))
   )
  :effects 
  ( (eqn  (= (* ?fo ?sterm) (* ?fs ?oterm))
	  (doppler-frequency ?source ?medium ?observer ?t-s ?t-o)) )
  :hint 
  ( (point (string "Use the formula for doppler frequency shift."))
    (teach (string "Note that ~A is ~@?" 
		   ?source (?stea identity) ?observer)) ;no nlg for ?stea
    (teach (string "Note that ~A is ~@?" 
		   ?observer (?otea identity) ?source)) ;no nlg for ?otea
    (bottom-out (string "Write the equation ~A" 
			((=  ?fo (/  (* ?fs ?oterm) ?sterm)) algebra) ))
    ))


;;;;   Decibels and intensity

;;   These quantities can be functions of time,
;;   but none of the problems so far require it.

(def-qexp intensity (intensity ?wave ?agent :time ?time)
  :units |W/m^2|
  :restrictions positive
  :english ("the intensity supplied to ~A due to ~A" 
	       (nlg ?wave 'at-time ?time) (nlg ?agent 'agent))
   :fromWorkbench (if (string-equal body2 '|all sources|)
                     `(net-intensity ,body :time ,time)
                  `(intensity ,body ,body2 :time ,time)))

(defoperator define-intensity (?wave ?agent ?t)
  :preconditions
  ((bind ?intense-var (format-sym "int_~A_~A_~A" 
				 (body-name ?wave) (body-name ?agent)
				 (time-abbrev ?t))))
  :effects ((variable ?intense-var (intensity ?wave ?agent :time ?t))
	    (define-var (intensity ?wave ?agent :time ?t)))
  :hint ((bottom-out 
	  (string "Define a variable for the intensity of ~A due to ~A by using the Add Variable command on the Variable menu and selecting intensity."  
		  ?wave (?agent agent)))))

;;; Net intensity

;; Net intensity is sum of all power acting on object.
;; BvdS:  Maybe combine with intensity, with ?agent set to nil

(def-qexp net-intensity (net-intensity ?wave :time ?time)
  :units |W/m^2|
  :restrictions positive  
  :english ("the net intensity supplied to ~A" (nlg ?wave 'at-time ?time)))

;; based on define-net-work
(defoperator define-net-intensity (?wave ?t)
  :preconditions
  ((bind ?intense-var (format-sym "netint_~A_~A" 
				  (body-name ?wave) (time-abbrev ?t))))
  :effects ((variable ?intense-var (net-intensity ?wave :time ?t))
	    (define-var (net-intensity ?wave :time ?t)))
  :hint ((bottom-out 
	  (string "Define a variable for the total intensity of ~A by using the Add Variable command on the Variable menu and selecting intensity."  
		  ?wave))))

;; based on net-work-contains
(defoperator net-intensity-contains (?sought)
  :preconditions 
  ((any-member ?sought  ((net-intensity ?b :time ?t) 
	                 (power ?b ?agent :time ?t)))
   ;; make sure we can determine all agents providing power
   (not (unknown-intensity-agents)))
  :effects 
  ((eqn-contains (net-intensity ?b ?t) ?sought)))

  
;;;  Add up all intensities acting on ?b

;; based on apply-net-work
(defoperator write-net-intensity (?b ?t)
  :preconditions 
  (;; introduce net-intensity variable
   (variable ?net-intensity-var (net-intensity ?b :time ?t))
   ;; introduce variables for power from each source. 
   ;; need to collect list of power *agents* to use in quantities.
   (setof (in-wm (power-source ?source ?b)) ?source ?sources)
   (map ?source ?sources
	(variable ?intensity-var (intensity ?b ?source :time ?t))
	?intensity-var ?intensity-vars) 
   )
  :effects (
	    (eqn (= ?net-intensity-var (+ . ?intensity-vars)) 
		 (net-intensity ?b ?t))
	    )
  :hint (
	 (teach (string "The net power acting on ~A is the sum of the various individual powers." ?b))
	 (bottom-out (string "Write the equation ~A" ((= ?net-intensity-var (+ . ?intensity-vars)) algebra)))
	 ))

;;;  Intensity in decibels.

(def-qexp db-intensity (db-intensity ?wave ?agent :time ?time)
  :units |dB|
  :english ("the intensity supplied to ~A due to ~A in decibels" 
	       (nlg ?wave 'at-time ?time) (nlg ?agent 'agent))
  :fromWorkbench (if (string-equal body2 '|all sources|)
                     `(net-db-intensity ,body :time ,time)
                  `(db-intensity ,body ,body2 :time ,time)))

(defoperator define-db-intensity (?wave ?agent ?t)
  :preconditions
  ((bind ?dbi-var (format-sym "dbint_~A_~A_~A" 
			      (body-name ?wave) (body-name ?agent) 
			      (time-abbrev ?t))))
  :effects ((variable ?dbi-var (db-intensity ?wave ?agent :time ?t))
	    (define-var (db-intensity ?wave ?agent :time ?t)))
  :hint ((bottom-out 
	  (string "Define a variable for the intensity of ~A in decibels 
due to ~A by using the Add Variable command on the Variable menu and selecting decibel-intensity."  ?wave (?agent agent)))))


;;; net version of db-intensity

;; BvdS:  Maybe we should combine this with db-intensity, with
;; nil specified as the ?agent

;; see def-qexp for db-intensity
(def-qexp net-db-intensity (net-db-intensity ?wave :time ?time)
  :units |dB|
  :english ("the total intensity supplied to ~A, in decibels" 
	       (nlg ?wave 'at-time ?time)))

(defoperator define-net-db-intensity (?wave ?t)
  :preconditions
  ((bind ?net-dbi-var (format-sym "dbint_~A_~A" 
			      (body-name ?wave) 
			      (time-abbrev ?t))))
  :effects ((variable ?net-dbi-var (net-db-intensity ?wave :time ?t))
	    (define-var (net-db-intensity ?wave :time ?t)))
  :hint ((bottom-out 
	  (string "Define a variable for the total intensity of ~A in decibels 
using the Add Variable command on the Variable menu and selecting decibel-intensity."  ?wave))))

;; Relate intensity to intensity in decibels

;; ?agent=nil marks net-intensity and net-db-intensity
(def-psmclass intensity-to-decibels (intensity-to-decibels ?wave ?agent ?t)
  :complexity major			;must explicitly use
  :english ("express intensity in decibels")
  :ExpFormat ("expressing the intensity in decibels")
  :EqnFormat ("$b = 10*log10(I/Iref) or I=Iref*10^($b/10)")) 

(defoperator intensity-to-decibels-contains (?sought)
  :preconditions 
  ( (time ?t)
    (any-member ?sought ((intensity ?wave ?agent :time ?t)
			 (db-intensity ?wave ?agent :time ?t))))
  :effects ( (eqn-contains (intensity-to-decibels ?wave ?agent ?t) ?sought) ))

(defoperator net-intensity-to-decibels-contains (?sought)
  :preconditions (
		   (time ?t)
		   (any-member ?sought ((net-intensity ?wave :time ?t)
					(net-db-intensity ?wave :time ?t))))
   :effects ( (eqn-contains (intensity-to-decibels ?wave nil ?t) ?sought)))

(defoperator write-intensity-to-decibels (?wave ?agent ?t)
  :preconditions 				
  ;; switch between net and ordinary intensity.
  ( (bind ?int1 (if ?agent `(intensity ,?wave ,?agent :time ,?t) 
		  `(net-intensity ,?wave :time ,?t)))
    (bind ?intdb1 (if ?agent `(db-intensity ,?wave ,?agent :time ,?t) 
		    `(net-db-intensity ,?wave :time ,?t)))
    (variable  ?int  ?int1)
    (variable  ?intdb ?intdb1) )
  :effects 
  ;; this is not the lisp function (log x 10)
  ;; Iref is predefined, see file constants.cl
  ((eqn  (= ?intdb (* 10 (log10 (/ ?int |Iref|) )))
	 (intensity-to-decibels ?wave ?agent ?t)))
  :hint 
  ( (point (string "You can express intensity in decibels"))
    (bottom-out (string "Write the equation ~A or ~A" 
			((= ?intdb (* 10 (log10 (/ ?int |Iref|) 
						))) algebra)
			((= ?int (* |Iref| (^ 10 (/ ?intdb 10)))) algebra) ))
    ))
 
;;; Relate intensity to net power output in a spherical geometry.  

(def-psmclass intensity-to-power (intensity-to-power ?wave ?source ?t ?b1 ?b2)
  :complexity major  ;must explicitly use
  :english ("relate intensity to power in a spherical geometry")
  :ExpFormat ("relating the intensity to power (spherical symmetry)")
  :EqnFormat ("P = 4*$p*r^2")) 


(defoperator intensity-to-power-contains (?sought)
  :preconditions 
  ( (spherical-emitting ?wave ?source)	;need spherical symmetry
    (time ?t)
    (any-member ?sought ((intensity ?wave ?source :time ?t)
			 (net-power-out ?source :time ?t)
			 (mag (relative-position ?source ?wave :time ?t))
			 ))
    ) 
  :effects 
  ( (eqn-contains (intensity-to-power ?wave ?source ?t ?source ?wave) ?sought)))

(defoperator intensity-to-power-contains2 (?sought)
  :preconditions 
  ( (spherical-emitting ?wave ?source)	;need spherical symmetry
    (time ?t)
    (any-member ?sought ((intensity ?wave ?source :time ?t)
			 (net-power-out ?source :time ?t)
			 (mag (relative-position ?wave ?source :time ?t))
			 ))
    ) 
  :effects 
  ( (eqn-contains (intensity-to-power ?wave ?source ?t ?wave ?source) ?sought)))

(defoperator write-intensity-to-power (?wave ?source ?t ?b1 ?b2)
  :preconditions 
  ( (variable  ?int  (intensity ?wave ?source :time ?t))
    (variable  ?power  (net-power-out ?source :time ?t))
    (variable  ?r (mag (relative-position ?b1 ?b2 :time ?t)))
    (optional (body ?source)) ;allow draw bodies
    (optional (body ?wave))
    (optional (axis-for ?source x 0)) ;allow draw axes
    )
  :effects 
  ( (eqn  (= ?power (* 4 $p (^ ?r 2) ?int))
		  (intensity-to-power ?wave ?source ?t ?b1 ?b2)) )
  :hint 
  ( (point (string "If the power goes out in all directions, the intensity ~A is the power divided by the surface area of the sphere." 
		   (?wave pp)))
    (teach (string "Imagine a sphere centered at ~A and extending to ~A; all of the power goes out through this sphere." 
		   (?source def-np) (?wave def-np)))
    (bottom-out (string "Write the equation ~A" 
			     ((= ?power (* 4 $p (^ ?r 2) ?int)) algebra) ))
	 ))

;;; Energy dissipation in an oscillator.

(def-psmclass energy-decay (energy-decay ?components ?time) 
  :complexity major
  :english ("Energy in a damped system")
  :eqnFormat ("E = Ei*exp(-2*t/$t)"))

(defoperator energy-decay-contains (?sought)
  :preconditions
  (
   (damping . ?system)  ; system appropriate for formula
   (any-member ?sought ((stored-energy (compound . ?system) :time ?t1)
			(stored-energy (compound . ?system) :time ?t2)
			(duration (during ?t1 ?t2))
			(time-constant . ?system)
			))
   ;; make sure we have a time interval:
   (time (during ?t1 ?t2))
   )
  :effects(
	   (eqn-contains (energy-decay ?system (during ?t1 ?t2)) ?sought)
	   ))

(defoperator write-energy-decay (?system ?t1 ?t2)
  :preconditions 
  (
   (variable ?E1-var (stored-energy (compound . ?system) :time ?t1))
   (variable ?E2-var (stored-energy (compound . ?system) :time ?t2))
   (variable ?t-var (duration (during ?t1 ?t2)))
   (variable ?tau-var (time-constant . ?system))
   )
  :effects 
  ((eqn (= ?E2-var (* ?E1-var (exp (/ (- (* 2 ?t-var)) ?tau-var))))
	(energy-decay ?system (during ?t1 ?t2))))
  :hint
  (
   (point (string "Write the equation for exponential decay of energy for ~a."
		  (compound . ?system)))
   (bottom-out (string "Write the equation ~a"
		       ((= ?E2-var (* ?E1-var 
				      (exp (/ (- (* 2 ?t-var)) ?tau-var)))) 
			algebra) ))
   ))
