;;;;
;;;; Waves
;;;;


;;;;
;;;;  The wavelength and wavenumber of a wave
;;;;

(def-qexp wavelength (wavelength ?wave)
  :units |m|
  :restrictions nonnegative 
  :english ("the wavelength of ~A" (nlg ?wave))
  :fromworkbench `(wavelength ,body))

(defoperator define-wavelength (?wave)
  :preconditions((bind ?lambda-var (format-sym "lamb_~A" (body-name ?wave))))
  :effects ((variable ?lambda-var (wavelength ?wave))
	    (define-var (wavelength ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the wavelength of ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave))))

(def-qexp wavenumber (wavenumber ?wave)
  :units |rad/m|
  :restrictions positive 
  :english ("the wavenumber of ~A" (nlg ?wave))
   :fromWorkbench `(wavenumber ,body))

(defoperator define-wavenumber (?wave)
  :preconditions ( (bind ?wn-var (format-sym "kwave_~A" (body-name ?wave))) )
  :effects ( (variable ?wn-var (wavenumber ?wave))
             (define-var (wavenumber ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the wave number of the ~A by using the Add Variable command on the Variable menu and selecting wave number."  ?wave))))

;;; Equation of the wavenumber of the wave, wavenumber*lambda = 2*pi
(def-psmclass wavenumber-lambda-wave (wavenumber-lambda-wave ?body)
  :complexity major  ; must explicitly use
  :english ("relation between wavelength and wavenumber")
  :ExpFormat ("Applying the equation relating wavenumber and wavelength")
  :EqnFormat ("wavenumber*wavelength = 2*pi")) 


 (defoperator wavenumber-lambda-wave-contains (?sought)
   :preconditions (
		   (wave ?wave)
		   (any-member ?sought ((wavelength ?wave)
					(wavenumber ?wave))))
   :effects (
     (eqn-contains (wavenumber-lambda-wave ?wave ) ?sought)))


(defoperator wavenumber-lambda-wave (?wave)
   :preconditions (
       (variable  ?lamb  (wavelength ?wave))
       (variable  ?kwave  (wavenumber ?wave))
   )
   :effects (
    (eqn  (= (* ?lamb ?kwave) (* 2 $p))  ;For pi, must use $p
                (wavenumber-lambda-wave ?wave))
   )
   :hint (
      (point (string "You can use equation for the wavenumber of a wave"))
      ;(teach (string "The equation-of-wavenumber-of-wave states that the wavenumber of a wave is (2*pi)/wavelength"))
      (bottom-out (string "Write the equation ~A" 
                     ((= (* ?kwave   ?lamb) (*2 $p)) 
                       algebra)))))

;;;
;;; The frequency, period, and angular frequency of a wave
;;;

(def-qexp frequency (frequency ?wave)
  :units |Hz|
  :restrictions nonnegative 
  :english ("the frequency of ~A" (nlg ?wave))
  :fromworkbench `(frequency ,body))

(defoperator define-frequency (?wave)
  :preconditions((bind ?freq-var (format-sym "freq_~A" (body-name ?wave))))
  :effects ((variable ?freq-var (frequency ?wave))
	    (define-var (frequency ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the frequency of ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave))))

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

(def-qexp wave-period (wave-period ?wave)
  :units |s|
  :restrictions positive
  :english ("the period of ~A" (nlg ?wave))
  :fromworkbench `(wave-period ,body))

(defoperator define-wave-period (?wave)
  :preconditions((bind ?wave-period-var (format-sym "wave-period_~A" (body-name ?wave))))
  :effects ((variable ?wave-period-var (wave-period ?wave))
	    (define-var (wave-period ?wave)))
  :hint (
	 (bottom-out (string "Define a variable for the period of ~A by using the Add Variable command on the Variable menu and selecting wave-period."  ?wave))
	 ))

;;equation of the frequency of the wave, frequency = angular-frequency/2*pi
(def-psmclass frequency-of-wave (frequency-of-wave ?body)
  :complexity minor  
  :english ("the equation for the frequency of a wave")
  :ExpFormat ("Applying the equation for the frequency of a wave")
  :EqnFormat ("frequency = angular-frequency/(2*$p)")) 

(defoperator frequency-of-wave-contains (?sought)
  :preconditions ((any-member ?sought ( 
				       (angular-frequency ?object)
				       (frequency ?object))))
  :effects ((eqn-contains (frequency-of-wave ?object) ?sought)))

(defoperator frequency-of-wave (?object)
  :preconditions (
		  (variable  ?omega  (angular-frequency ?object))
		  (variable  ?freq  (frequency ?object))
		  )
  :effects (
	    (eqn  (= ?omega (* 2 $p ?freq)) ; pi must be $p
	    (frequency-of-wave ?object)))
  :hint (
	 (point (string "You can use equation for the frequency of a wave"))
	 ;;(teach (string "The equation-of-frequency-of-wave states that the frequency of a wave is angular-frequency/(2*pi)"))
	 (bottom-out (string "Write the equation ~A" 
			     (= ?omega (*2 $p ?freq)) 
			     algebra) )))

;;equation of the period of the wave, period = 1/frequency
(def-psmclass period-of-wave (period-of-wave ?body)
  :complexity major 
  :english ("the equation for the period of a wave")
  :ExpFormat ("Applying the equation for the period of a wave")
  :EqnFormat ("period = 1/frequency")) 

(defoperator period-of-wave-contains (?sought)
  :preconditions (
		  (any-member ?sought ( 
				       (wave-period ?object)
				       (frequency ?object))))
  :effects (
	    (eqn-contains (period-of-wave ?object) ?sought)))

(defoperator period-of-wave (?object)
  :preconditions (
		  (variable  ?wave-period (wave-period ?object))
		  (variable  ?freq  (frequency ?object)))
  :effects (
	    (eqn  (= (* ?freq ?wave-period) 1) 
		  (period-of-wave ?object)))
  :hint (
	 (point (string "You can use equation for the period of a wave"))
	 ;;(teach (string "The equation-of-period-of-wave states that the frequency of a wave is 1/period"))
	 (bottom-out (string "Write the equation ~A" 
			     (= ?freq (/ 1 ?wave-period)) 
			     algebra) )))

;;;
;;;   Wave speed, this as |phase velocity|
;;;

(def-qexp wave-speed (wave-speed ?wave)
  :units |m/s|
  :restrictions nonnegative 
  :english ("|the wave velocity of ~A|" (nlg ?wave))
  :fromworkbench `(wave-speed ,body))

(defoperator define-wave-speed (?wave)
  :preconditions((bind ?wv-var (format-sym "wv_~A" (body-name ?wave))))
  :effects ((variable ?wv-var (wave-speed ?wave))
	    (define-var (wave-speed ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the |velocity| of ~A by using the Add Variable command on the Variable menu and selecting amplitude."  ?wave))))

;;equation of the speed of the wave, speed = freq* wavelength
(def-psmclass speed-of-wave (speed-of-wave ?body)
  :complexity major ; must use explicitly 
  :english ("the equation of the speed of a wave")
  :ExpFormat ("Applying the equation of the speed of a wave")
  :EqnFormat ("v = lambda*freq")) 

(defoperator speed-of-wave-contains (?sought)
  :preconditions (
		  (any-member ?sought ( 
				       (wave-speed ?object)
				       (wavelength ?object)
				       (frequency ?object)))  )
  :effects (
	    (eqn-contains (speed-of-wave ?object) ?sought)))

(defoperator speed-of-wave (?object)
  :preconditions (
		  (variable  ?v  (wave-speed ?object))
		  (variable  ?lam  (wavelength ?object))
		  (variable  ?freq  (frequency ?object))
		  )
  :effects (
	    (eqn  (= ?v (* ?lam ?freq)) 
		  (speed-of-wave ?object)))
  :hint (
	 (point (string "You can use equation for the speed of a wave"))
	 ;;(teach (string "The equation-of-speed-of-wave states that the speed of a wave is wavelength*frequency"))
	 (bottom-out (string "Write the equation ~A" 
			     (= ?v (* ?lambda ?freq)) algebra) )))

;;;
;;;   The amplitude of a wave
;;;

(def-qexp wave-amplitude (wave-amplitude ?wave)
  :units |m|
  :restrictions nonnegative 
  :english ("the amplitude of ~A" (nlg ?wave))
  :fromworkbench `(wave-amplitude ,body))

(defoperator define-wave-amplitude (?wave)
  :preconditions((bind ?Am-var (format-sym "Amp_~A" (body-name ?wave))))
  :effects ((variable ?Am-var (wave-amplitude ?wave))
	    (define-var (wave-amplitude ?wave)))
  :hint ((bottom-out 
	  (string "Define a variable for the amplitude of ~A by using the Add Variable command on the Variable menu and selecting amplitude."  ?wave))))

