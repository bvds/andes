
(defproblem elec1
	:statement (
		"An electron (qe = -1.6E-19 C; me = 9.1E-31 kg) is placed"
		"in an electric field.  Determine the magnitude of the electric field"
		"that the electron is in so that it will exactly cancel"
		"its weight near the Earth's surface."
		"    Answer:  [XXXXXXXXXXXX]"
	)
	;:graphic "elec1.gif"
	:features (E&M E-field andes2 working dynamics)
	:choices ((bodies (electron Earth))
	       (positions (region)))
	:givens (
	  (time 1)
	  (object electron )
	  (near-planet earth)
	  (E-field unspecified)
	  (at-place electron region 1)
	  (given (charge-on electron) (dnum -1.6E-19 |C|)) 
	  (given (mass electron) (dnum 9.1E-31 |kg|))
	  (given (dir (field region electric unspecified :time 1)) (dnum 270 |deg|)) 
	  (motion electron at-rest :time 1)
	  )
	:soughts (
	  (mag (field region electric unspecified :time 1))
     ))

(defproblem elec2
	:statement (
		"A particle having a net charge of 52.0 $mC is placed in a"
		"uniform electric field of 120 N/C directed vertically. What is"
		"the mass of the object if it `floats' in the field?"
		"    Answer:  [XXXXXXXXXXXX]"
	)
	;:graphic "elec2.gif"
	:features (E&M E-field andes2 working dynamics)
	:choices ((bodies (particle Earth))
	       (positions (region)))
	:givens (
	  (time 1)
	  (object particle)
	  (near-planet earth)
	  (E-field unspecified)
	  (at-place particle region 1)
	  (given (charge-on particle) (dnum 52.0 |$mC|))
	  (given (mag (field region electric unspecified :time 1)) (dnum 120 |N/C|)) 
	  (given (dir (field region electric unspecified :time 1)) (dnum 90 |deg|)) 
	  (motion particle at-rest :time 1)
	 )
	:soughts (
	  (answer (mass particle))
     ))

(defproblem elec3
  :statement (
	      "An electron (charge -1.6E-19 C, mass 9.1E-31 kg) is accelerated in"
	      "a uniform electric field (E = 2.0E+4 N/C) between two parallel"
	      "charged plates. The separation between the plates is 1.5 cm. The"
	      "electron undergoes a constant acceleration from rest near the"
	      "negative plate and passes through a tiny hole in the positive plate"
	      "(see Figure below). What is the magnitude of the velocity as"
	      "the electron leaves the hole?"
	      "    Answer:  [XXXXXXXXXXXX]"
	      "In this problem, gravity can be ignored."
	      )
  ;; :graphic "elec3.gif"
  :features (E&M E-field andes2 working dynamics)
  :choices ((bodies (electron plates))
	    (positions (region)))
  :times ((1 "at rest") (2 "leaves hole") (during 1 2))
  :givens 
  (
   (time 1) (time 2) (time (during 1 2))
   (object electron)
   (given (mass electron)      (dnum 9.1E-31 |kg|))
   ;; Electrostatics
   (at-place electron region (during 1 2))
   (given (charge-on electron) (dnum -1.6E-19 |C|)) 
   (given (mag (field region electric plates :time (during 1 2))) 
	  (dnum 2.0E4 |N/C|))
   (given (dir (field region electric plates :time (during 1 2))) 
	  (dnum 270 |deg|))
   ;; Kinematics (rectilinear)
   (motion electron at-rest :time 1)
   (motion electron (straight speed-up (dnum 90 |deg|)) :time (during 1 2))
   (motion electron (straight unknown (dnum 90 |deg|)) :time 2)      
   (given (mag (displacement electron :time (during 1 2))) 
	  (dnum 0.015 |m|))
   (constant (accel electron) (during 1 2))
   )
  :ignorePSMS (cons-energy total-energy-cons linear-momentum momentum total-energy-top kinetic-energy total-energy)
  :soughts (
	    (mag (velocity electron :time 2))
	    ))

(defproblem elec4
	:statement (
		"A proton (qp = 1.6E-19 C; mp = 1.7E-27 kg) accelerates from"
		"rest in a uniform electric field of 320 N/C. After some later"
		"time, its velocity is 1.20E+5 m/s moving to the right."
		"How long does it take the proton to reach this speed?"
		"    Answer:  [XXXXXXXXXXXX]"
		"In this problem, gravity can be ignored."
	)
	;:graphic "elec4.gif"
	:features (E&M E-field andes2 working dynamics)
	:choices ((bodies (proton))
	      (positions (region)))
	:times ((1 "at rest") (2 "at 1.20E+5 m/s") (during 1 2))
	:givens (
        (time 1) (time 2) (time (during 1 2))
        (object proton)
        (given (mass proton)      (dnum 1.7E-27 |kg|))
	;; Electrostatics
        (at-place proton region (during 1 2))
        (given (charge-on proton) (dnum 1.6E-19 |C|)) 
	(given (mag (field region electric unspecified :time (during 1 2))) 
	(dnum 320 |N/C|))
        (given (dir (field region electric unspecified :time (during 1 2))) 
	(dnum 0 |deg|))
        (E-field unspecified)

	  ;; Kinematics (rectilinear)
	(motion proton momentarily-at-rest :time 1)
        (motion proton (straight speed-up (dnum 0 |deg|)) :time (during 1 2))
        (motion proton (straight constant (dnum 0 |deg|)) :time 2)      
	(given (mag (velocity proton :time 2)) (dnum 1.20E+5 |m/s|))
        (constant (accel proton) (during 1 2))
      )
  :ignorePSMS (cons-energy total-energy-cons linear-momentum momentum total-energy-top kinetic-energy total-energy)
	:soughts (
	  (duration (during 1 2))
	))

(defproblem elec5
	:statement (
		"A proton (charge 1.6E-19 C, mass 1.7E-27 kg) accelerates from"
		"rest in a uniform electric field of 920 N/C pointing to"
		"the left.  After some time, its speed is 7.2E+3 m/s."
		"How far does the proton travel during this duration?"
		"    Answer:  [XXXXXXXXXXXX]"
		"In this problem, gravity can be ignored."
	)
	;:graphic "elec5.gif"
	:features (E&M E-field andes2 working dynamics)
	:choices ((bodies (proton))
	      (positions (region)))
	:times ((1 "at rest") (2 "at 7.2E+3 m/s") (during 1 2))
	:givens (
        (time 1) (time 2) (time (during 1 2))
        (object proton)
        (given (mass proton)      (dnum 1.7E-27 |kg|))
	  ;; Electrostatics
        (at-place proton region (during 1 2))
        (given (charge-on proton) (dnum 1.6E-19 |C|)) 
        (given (mag (field region electric unspecified :time (during 1 2))) 
	       (dnum 920 |N/C|))
        (given (dir (field region electric unspecified :time (during 1 2))) 
	       (dnum 180 |deg|))
        (E-field unspecified)
	;; Kinematics (rectilinear)
	(motion proton momentarily-at-rest :time 1)
        (motion proton (straight speed-up (dnum 180 |deg|)) :time (during 1 2))
        (motion proton (straight constant (dnum 180 |deg|)) :time 2)      
 	  (given (mag (velocity proton :time 2)) (dnum 7.20E+3 |m/s|))
        (constant (accel proton) (during 1 2))
      )
  :ignorePSMS (cons-energy total-energy-cons linear-momentum momentum total-energy-top kinetic-energy total-energy)
	:soughts (
	  (mag (displacement proton :time (during 1 2)))
	))

(defproblem elec6
  :statement (
	      "An electron (charge -1.6e-19 C, mass 9.1e-31 kg) is moving in"
	      "the upwards in an electric field of magnitude 4.0e-12 N/C"
	      "at an initial velocity of 4.3E+6 m/s.  How far will the electron"
	      "travel before it comes to rest? In this problem, please include"
	      "gravity."
	      "    Answer:  [XXXXXXXXXXXX]"
	      )
  ;; :graphic "elec6.gif"
  :features (E&M E-field andes2 working dynamics)
  :choices ((bodies (electron Earth))
	    (positions (region)))
  :times ((1 "at 4.3E+6 m/s") (2 "at rest") (during 1 2))
  :givens 
  (
   (time 1) (time 2) (time (during 1 2))
   (object electron)
   (near-planet earth)
   ;; Electrostatics
   (at-place electron region (during 1 2))
   (given (charge-on electron) (dnum -1.6E-19 |C|)) 
   (given (mass electron)      (dnum 9.1E-31 |kg|)) 
   (given (mag (field region electric unspecified :time (during 1 2))) 
	  (dnum 4.0E-12 |N/C|)) 
   (given (dir (field region electric unspecified :time (during 1 2))) 
	  (dnum 270 |deg|))
   (E-field unspecified)
   ;; Kinematics (rectilinear)
   (given (mag (velocity electron :time 1)) (dnum 4.3E+6 |m/s|))
   (motion electron (straight constant (dnum 90 |deg|)) :time 1)
   (motion electron (straight slow-down (dnum 90 |deg|)) :time (during 1 2))
   (motion electron momentarily-at-rest :time 2)
   (constant (accel electron) (during 1 2))
   )
  :ignorePSMS (cons-energy total-energy-cons momentum linear-momentum total-energy-top kinetic-energy total-energy)
  :soughts (
	    (mag (displacement electron :time (during 1 2)))
	    ))

