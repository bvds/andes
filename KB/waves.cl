;;
;; Waves
;;



;;Waves Variables


;;The wavelength of a wave
(def-qexp wavelength (wavelength ?wave)
     :units |m|
     :restrictions nonnegative 
     :english ("the wavelength of ~A" (nlg ?wave))
     :fromworkbench `(at (wavelength ,wave) ,time)
   )

(defoperator define-wavelength (?wave ?time)
     :preconditions((bind ?lamda-var (format-sym "lamda_~A" (body-name ?wave))))
     :effects ((variable ?lamda-var (at (wavelength ?wave) ?time))
               (define-var (at (wavelength ?wave) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the wavelength of ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave))
          ))


;;The frequency (linear frequency) of a wave
(def-qexp frequency (frequency ?wave)
     :units |Hz|
     :restrictions nonnegative 
     :english ("the frequency of ~A" (nlg ?wave))
     :fromworkbench `(at (frequency ,wave) ,time)
   )

(defoperator define-frequency (?wave ?time)
     :preconditions((bind ?freq-var (format-sym "freq_~A" (body-name ?wave))))
     :effects ((variable ?freq-var (at (frequency ?wave) ?time))
               (define-var (at (frequency ?wave) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the frequency of ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave))
          ))


;;The angular frequency of a wave
(def-qexp angular-frequency (angular-frequency ?wave)
     :units |rad/s|
     :restrictions nonnegative 
     :english ("the angular-frequency of ~A" (nlg ?wave))
     :fromworkbench `(at (angular-frequency ,wave) ,time)
   )

(defoperator define-angular-frequency (?wave ?time)
     :preconditions((bind ?omega-var (format-sym "omega_~A" (body-name ?wave))))
     :effects ((variable ?omega-var (at (angular-frequency ?wave) ?time))
               (define-var (at (angular-frequency ?wave) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the angular-frequency of ~A by using the Add Variable command on the Variable menu and selecting wavelength."  ?wave))
          ))

;;The period of a wave
(def-qexp wave-period (wave-period ?wave)
     :units |s|
     :restrictions nonnegative 
     :english ("the period of ~A" (nlg ?wave))
     :fromworkbench `(at (wave-period ,wave) ,time)
   )

(defoperator define-wave-period (?wave ?time)
     :preconditions((bind ?wave-period-var (format-sym "wave-period_~A" (body-name ?wave))))
     :effects ((variable ?wave-period-var (at (wave-period ?wave) ?time))
               (define-var (at (wave-period ?wave) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the period of ~A by using the Add Variable command on the Variable menu and selecting wave-period."  ?wave))
          ))

;;The wavenumber of a wave
(def-qexp wavenumber (wavenumber ?wave)
     :units |rad/m|
     :restrictions nonnegative 
     :english ("the wavenumber of ~A" (nlg ?wave))
     :fromworkbench `(at (wavenumber ,wave) ,time)
   )

(defoperator define-wavenumber (?wave ?time)
     :preconditions((bind ?kwave-var (format-sym "kwave_~A" (body-name ?wave))))
     :effects ((variable ?kwave-var (at (wavenumber ?wave) ?time))
               (define-var (at (wavenumber ?wave) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the wavenumber of ~A by using the Add Variable command on the Variable menu and selecting wavenumber."  ?wave))
          ))

;;The amplitude of a wave
(def-qexp amplitude (amplitude ?wave)
     :units |m|
     :restrictions nonnegative 
     :english ("the amplitude of ~A" (nlg ?wave))
     :fromworkbench `(at (amplitude ,wave) ,time)
   )

(defoperator define-amplitude (?wave ?time)
     :preconditions((bind ?Am-var (format-sym "Am_~A" (body-name ?wave))))
     :effects ((variable ?Am-var (at (amplitude ?wave) ?time))
               (define-var (at (amplitude ?wave) ?time)))
     :hint (
          (bottom-out (string "Define a variable for the amplitude of ~A by using the Add Variable command on the Variable menu and selecting amplitude."  ?wave))
          ))



;;Wave Equations

;;equation of the speed of the wave, speed = freq* wavelength
(def-psmclass speed-of-wave (speed-of-wave ?body ?time)
  :complexity minor  
  :english ("the equation of the speed of a wave")
  :ExpFormat ("Applying the equation of the speed of a wave")
  :EqnFormat ("v = lambda*freq")) 


 (defoperator speed-of-wave-contains (?sought)
   :preconditions (
   ;(in-wm (shape ?shape circle))
   (any-member ?sought ( 
                           (at (speed ?object) ?time)
                           (at (wavelength ?object) ?time)
                           (at (frequency ?object) ?time)
                         ))  
   )
   :effects (
     (eqn-contains (speed-of-wave ?object ?time) ?sought)
   ))


(defoperator speed-of-wave (?object ?time)
   :preconditions (
       (variable  ?v  (at (speed ?object) ?time))
       (variable  ?lamda  (at (wavelength ?object) ?time))
       (variable  ?freq  (at (frequency ?object) ?time))
   )
   :effects (
    (eqn  (= ?v (* ?lamda ?freq)) 
                (speed-of-wave ?object ?time))
   )
   :hint (
      (point (string "You can use equation for the speed of a wave"))
      ;(teach (string "The equation-of-speed-of-wave states that the speed of a wave is wavelength*frequency"))
      (bottom-out (string "Write the equation ~A" 
                     (= ?v (* ?lamda ?freq)) 
                       algebra) ))
   )

;;equation of the frequency of the wave, frequency = angular-frequency/2*pi
(def-psmclass frequency-of-wave (frequency-of-wave ?body ?time)
  :complexity minor  
  :english ("the equation for the frequency of a wave")
  :ExpFormat ("Applying the equation for the frequency of a wave")
  :EqnFormat ("frequency = angular-frequency/(2*$p)")) 


 (defoperator frequency-of-wave-contains (?sought)
   :preconditions (
   (any-member ?sought ( 
                           (at (angular-frequency ?object) ?time)
                           (at (frequency ?object) ?time)
                         ))  
   )
   :effects (
     (eqn-contains (frequency-of-wave ?object ?time) ?sought)
   ))


(defoperator frequency-of-wave (?object ?time)
   :preconditions (
       (variable  ?omega  (at (angular-frequency ?object) ?time))
       (variable  ?freq  (at (frequency ?object) ?time))
   )
   :effects (
    (eqn  (= ?freq (/ ?omega (* 2 $p))) 
                (frequency-of-wave ?object ?time))
   )
   :hint (
      (point (string "You can use equation for the frequency of a wave"))
      ;(teach (string "The equation-of-frequency-of-wave states that the frequency of a wave is angular-frequency/(2*pi)"))
      (bottom-out (string "Write the equation ~A" 
                     (= ?freq (/ ?omega (*2 $p))) 
                       algebra) ))
   )


;;equation of the period of the wave, period = 1/frequency
(def-psmclass period-of-wave (period-of-wave ?body ?time)
  :complexity major 
  :english ("the equation for the period of a wave")
  :ExpFormat ("Applying the equation for the period of a wave")
  :EqnFormat ("period = 1/frequency")) 


 (defoperator period-of-wave-contains (?sought)
   :preconditions (
   (any-member ?sought ( 
                           (at (wave-period ?object) ?time)
                           (at (frequency ?object) ?time)
                         ))  
   )
   :effects (
     (eqn-contains (period-of-wave ?object ?time) ?sought)
   ))


(defoperator period-of-wave (?object ?time)
   :preconditions (
       (variable  ?wave-period (at (wave-period ?object) ?time))
       (variable  ?freq  (at (frequency ?object) ?time))
   )
   :effects (
    (eqn  (= ?freq (/ 1 ?wave-period)) 
                (period-of-wave ?object ?time))
   )
   :hint (
      (point (string "You can use equation for the period of a wave"))
      ;(teach (string "The equation-of-period-of-wave states that the frequency of a wave is 1/period"))
      (bottom-out (string "Write the equation ~A" 
                     (= ?freq (/ 1 ?wave-period)) 
                       algebra) ))
   )


;;equation of the wavenumber of the wave, wavenumber = 2*pi/wavelength
(def-psmclass wavenumber-of-wave (wavenumber-of-wave ?body ?time)
  :complexity major  
  :english ("the equation for the wavenumber of a wave")
  :ExpFormat ("Applying the equation for the wavenumber of a wave")
  :EqnFormat ("wavenumber = 2*pi/wavelength")) 


 (defoperator wavenumber-of-wave-contains (?sought)
   :preconditions (
   (any-member ?sought ( 
                           (at (wavelength ?object) ?time)
                           (at (wavenumber ?object) ?time)
                         ))  
   )
   :effects (
     (eqn-contains (wavenumber-of-wave ?object ?time) ?sought)
   ))


(defoperator wavenumber-of-wave (?object ?time)
   :preconditions (
       (variable  ?lamda  (at (wavelength ?object) ?time))
       (variable  ?kwave  (at (wavenumber ?object) ?time))
   )
   :effects (
    (eqn  (= ?kwave (/ (* 2 $p) ?lamda)) 
                (wavenumber-of-wave ?object ?time))
   )
   :hint (
      (point (string "You can use equation for the wavenumber of a wave"))
      ;(teach (string "The equation-of-wavenumber-of-wave states that the wavenumber of a wave is (2*pi)/wavelength"))
      (bottom-out (string "Write the equation ~A" 
                     (= ?wavenumber (/ (*2 $p) ?wavelength)) 
                       algebra) ))
   )


