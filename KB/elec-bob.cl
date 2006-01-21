
(defproblem elec1a
  :statement (
    "An electron (qe = -1.60e-19 C; me = 9.11e-31 kg) is in a region" 
    "where there is a uniform electric field E. The force on the electron"
    "due to the electric field exactly cancels its weight near the Earth's"
    "surface. If the y-component of the net force on the particle near"
    "Earth due to the electric field and gravity is zero, what is the"
    "magnitude (and show the direction) of the electric field E?"
    "    Answer:  [XXXXXXXXXXXX]"
  )
  ;; :graphic "elec1.gif"
  :features (E&M E-field andes2 working kinematics dynamics)
  :choices (
    (bodies (electron Earth))
    (positions (region))
  )
  :givens (
    (time 1)
    (object electron )
    (near-planet earth)
    (E-field unspecified)
    (at-place electron region 1)
    (given (charge-on electron) (dnum -1.60E-19 |C|)) 
    (given (mass electron) (dnum 9.11E-31 |kg|))
    (given (dir (field region electric unspecified :time 1)) (dnum 270 |deg|)) 
    (motion electron at-rest :time 1)
  )
  :soughts (
    (mag (field region electric unspecified :time 1))
  )
)

(defproblem elec1b ;not working for charge as sought
  :statement (
    "A charged particle is in a region where there is an"
    "electric field E of magnitude 16.2 N/C at an angle of"
    "15 degrees above the positive x-axis. The particle"
    "experiences a force, F of magnitude 60.4 N in the opposite"
    "direction. Find the charge on the particle."
    "    Answer:  [XXXXXXXXXXXXXXX]"
  )
  :features (E&M E-field andes2 kinematics dynamics)
  :choices (
    (bodies (particle))
    (positions (region))
  )
  :givens(
    (time 1)
    (object particle)
    (at-place particle region 1)
    (sign-charge particle neg)
    (given (mag (force particle unspecified electric :time 1))
      (dnum 60.4 |N|))
    (given (dir (force particle unspecified electric :time 1))
      (dnum 195 |deg|))
    (given (mag (field region electric unspecified :time 1))
      (dnum 16.2 |N/C|))
    (given (dir (field region electric unspecified :time 1))
      (dnum 15 |deg|)) 
  )
  :soughts (
    (charge-on particle)
  )
)

(defproblem for4c  ;this is a variant of for4
  :statement ("A charged particle is in a region where there is an"
    "electric field E of magnitude 24.4 V/m at an angle of"
    "17 degrees above the positive x-axis. If the charge"
    "on the particle is 8.4 C, find the magnitude of the"
    "force on the particle P due to the electric field E."
    "    Answer:  [XXXXXXXXXXXXXXX]"
  )
  :features (E&M E-field andes2 working)
  :choices (
    (bodies (particle))
    (positions (region))
  )
  :givens(
    (time 1)
    (object particle)
    (at-place particle region 1)
    (given (charge-on particle) (dnum 8.4 |C|))
    (given (mag (field region electric unspecified :time 1)) (dnum 24.4 |V/m|))
    (given (dir (field region electric unspecified :time 1)) (dnum 17 |deg|)) 
  )
  :soughts (
    (mag (force particle unspecified electric :time 1))
    ; Problem with seeking directions of drawn vectors:
    ;(dir (force particle field electric :time 1))
  )
)

(defproblem elec2
  :statement (
    "A charged particle (q = 52.0e-6 C) is in a region where there is a"
    "uniform electric field E of magnitude 120 N/C at an angle of 90 degrees"
    "above the positive x-axis. If the y-component of the net force on the"
    "particle near Earth due to the electric field and gravity is zero, what"
    "is the mass of the particle?"
    "    Answer:  [XXXXXXXXXXXX]"
  )
  ;; :graphic "elec2.gif"
  :features (E&M E-field andes2 working kinematics dynamics)
  :choices (
    (bodies (particle Earth))
    (positions (region))
  )
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
  )
)

(defproblem elec3a ;version for bob's experiment
  :statement (
    "An electron (qe = -1.60e-19 C; me = 9.11e-31 kg) is in a region,"
    "between two parallel charged plates, that produce a uniform electric"
    "field E of magnitude 2.0e+4 N/C. The separation between the plates" 
    "is 1.5 cm. The electron undergoes a constant acceleration from rest"
    "near the negative plate and passes through a tiny hole in the positive"
    "plate (see Figure below). Find the velocity of the electron as it leaves"
    "the hole."
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, gravity can be ignored."
  )
  :graphic "elec3.gif"
  :features (E&M E-field andes2 working kinematics dynamics)
  :choices (
    (bodies (electron plates))
    (positions (region))
  )
  :times ((1 "at rest") (2 "leaves hole") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object electron)
    (given (mass electron) (dnum 9.11E-31 |kg|))
    ;; Electrostatics
    (at-place electron region (during 1 2))
    (given (charge-on electron) (dnum -1.60E-19 |C|)) 
    (given (mag (field region electric plates :time (during 1 2))) 
      (dnum 2.0E4 |N/C|))
    (given (dir (field region electric plates :time (during 1 2))) 
      (dnum 270 |deg|))
    ;; Kinematics (rectilinear)
    (motion electron at-rest :time 1)
    (motion electron (straight speed-up (dnum 90 |deg|)) :time (during 1 2) )
    (motion electron (straight unknown (dnum 90 |deg|)) :time 2)      
    (given (mag (displacement electron :time (during 1 2))) 
      (dnum 0.015 |m|))
    (constant (accel electron) (during 1 2))
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
  )
  :soughts (
    (mag (velocity electron :time 2))
  )
)

(defproblem elec3b ;version for general use, includes work solution
  :statement (
    "An electron (qe = -1.60e-19 C; me = 9.11e-31 kg) is in a region,"
    "between two parallel charged plates, that produce a uniform electric"
    "field E of magnitude 2.0e+4 N/C. The separation between the plates" 
    "is 1.5 cm. The electron undergoes a constant acceleration from rest"
    "near the negative plate and passes through a tiny hole in the positive"
    "plate (see Figure below). Find the velocity of the electron as it leaves"
    "the hole."
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, gravity can be ignored."
  )
  :graphic "elec3.gif"
  :features (E&M E-field andes2 working kinematics dynamics energy)
  :choices (
    (bodies (electron plates))
    (positions (region))
  )
  :times ((1 "at rest") (2 "leaves hole") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object electron)
    (given (mass electron) (dnum 9.11E-31 |kg|))
    ;; Electrostatics
    (at-place electron region (during 1 2))
    (given (charge-on electron) (dnum -1.60E-19 |C|)) 
    (given (mag (field region electric plates :time (during 1 2))) 
      (dnum 2.0E4 |N/C|))
    (given (dir (field region electric plates :time (during 1 2))) 
      (dnum 270 |deg|))
    ;; Kinematics (rectilinear)
    (motion electron at-rest :time 1)
    (motion electron (straight speed-up (dnum 90 |deg|)) :time (during 1 2) )
    (motion electron (straight unknown (dnum 90 |deg|)) :time 2)      
    (given (mag (displacement electron :time (during 1 2))) 
      (dnum 0.015 |m|))
    (constant (accel electron) (during 1 2))
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
    (use-work) 
  )
  :soughts (
    (mag (velocity electron :time 2))
  )
)

(defproblem elec4a ;version for bob's experiment
  :statement (
    "A proton (qp = 1.60e-19 C; mp = 1.67e-27 kg) is in a region where"
    "there is a uniform electric field E of magnitude 320 N/C, directed" 
    "along the positive x-axis. The proton accelerates from rest and" 
    "reaches a speed of 1.20e+5 m/s. How long does it take the proton to"
    "reach this speed?"
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, gravity can be ignored."
  )
  ;; :graphic "elec4.gif"
  :features (E&M E-field andes2 working kinematics dynamics)
  :choices (
    (bodies (proton))
    (positions (region))
  )
  :times ((1 "at rest") (2 "at 1.20E+5 m/s") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object proton)
    (given (mass proton) (dnum 1.7E-27 |kg|))
    ;; Electrostatics
    (at-place proton region (during 1 2))
    (given (charge-on proton) (dnum 1.60E-19 |C|)) 
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
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
  )
  :soughts (
    (duration (during 1 2))
  )
)

(defproblem elec4b ;version for general use, includes work solution
  :statement (
    "A proton (qp = 1.60e-19 C; mp = 1.67e-27 kg) is in a region where"
    "there is a uniform electric field E of magnitude 320 N/C, directed" 
    "along the positive x-axis. The proton accelerates from rest and" 
    "reaches a speed of 1.20e+5 m/s. How long does it take the proton to"
    "reach this speed?"
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, gravity can be ignored."
  )
  ;; :graphic "elec4.gif"
  :features (E&M E-field andes2 working kinematics dynamics energy)
  :choices (
    (bodies (proton))
    (positions (region))
  )
  :times ((1 "at rest") (2 "at 1.20E+5 m/s") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object proton)
    (given (mass proton) (dnum 1.7E-27 |kg|))
    ;; Electrostatics
    (at-place proton region (during 1 2))
    (given (charge-on proton) (dnum 1.60E-19 |C|)) 
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
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
    (use-work)
  )
  :soughts (
    (duration (during 1 2))
  )
)

(defproblem elec5a ;version for bob's experiment
  :statement (
    "A proton (qp = 1.60e-19 C; mp = 1.67e-27 kg) is in a region where" 
    "there is a uniform electric field E of magnitude 920 N/C, directed" 
    "along the negative x-axis. The proton accelerates from rest and" 
    "reaches a speed of 7.2e+3 m/s. How far does the proton travel" 
    "during this duration?"
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, gravity can be ignored."
  )
  ;; :graphic "elec5.gif"
  :features (E&M E-field andes2 working kinematics dynamics)
  :choices (
    (bodies (proton))
    (positions (region))
  )
  :times ((1 "at rest") (2 "at 7.2E+3 m/s") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object proton)
    (given (mass proton)      (dnum 1.7E-27 |kg|))
    ;; Electrostatics
    (at-place proton region (during 1 2))
    (given (charge-on proton) (dnum 1.60E-19 |C|)) 
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
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
  )
  :soughts (
    (mag (displacement proton :time (during 1 2)))
  )
)


(defproblem elec5b ;version for general use, includes work solution
  :statement (
    "A proton (qp = 1.60e-19 C; mp = 1.67e-27 kg) is in a region where" 
    "there is a uniform electric field E of magnitude 920 N/C, directed" 
    "along the negative x-axis. The proton accelerates from rest and" 
    "reaches a speed of 7.2e+3 m/s. How far does the proton travel" 
    "during this duration?"
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, gravity can be ignored."
  )
  ;; :graphic "elec5.gif"
  :features (E&M E-field andes2 working kinematics dynamics energy)
  :choices (
    (bodies (proton))
    (positions (region))
  )
  :times ((1 "at rest") (2 "at 7.2E+3 m/s") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object proton)
    (given (mass proton)      (dnum 1.7E-27 |kg|))
    ;; Electrostatics
    (at-place proton region (during 1 2))
    (given (charge-on proton) (dnum 1.60E-19 |C|)) 
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
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
    (use-work)
  )
  :soughts (
    (mag (displacement proton :time (during 1 2)))
  )
)

(defproblem elec6a ;version for bob's experiment
  :statement (
    "An electron (qe = -1.60e-19 C; me = 9.11e-31 kg) is in a region near" 
    "the surface of the earth where there is a uniform electric field E"
    "of magnitude 4.0e-12 N/C, directed along the negative y-axis."
    "The electron is moving in the positive y-direction at an initial"
    "velocity of 4.3 m/s. How far will the electron travel before it"
    "momentarily comes to rest?"
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, please include gravity."
  )
  ;; :graphic "elec6.gif"
  :features (E&M E-field andes2 working kinematics dynamics)
  :choices ((bodies 
    (electron Earth))
    (positions (region)))
  :times ((1 "at 4.3 m/s") (2 "at rest") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object electron)
    (near-planet earth)
    ;; Electrostatics
    (at-place electron region (during 1 2))
    (given (charge-on electron) (dnum -1.60E-19 |C|)) 
    (given (mass electron)      (dnum 9.11E-31 |kg|)) 
    (given (mag (field region electric unspecified :time (during 1 2))) 
      (dnum 4.0E-12 |N/C|)) 
    (given (dir (field region electric unspecified :time (during 1 2))) 
      (dnum 270 |deg|))
    (E-field unspecified)
    ;; Kinematics (rectilinear)
    (given (mag (velocity electron :time 1)) (dnum 4.3 |m/s|))
    (motion electron (straight nil (dnum 90 |deg|)) :time 1)
    (motion electron (straight slow-down (dnum 90 |deg|)) :time (during 1 2))
    (motion electron momentarily-at-rest :time 2)
    (constant (accel electron) (during 1 2))
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
  )
  :soughts (
    (mag (displacement electron :time (during 1 2)))
  )
)

(defproblem elec6b  ;version for general use, includes work solution
  :statement (
    "An electron (qe = -1.60e-19 C; me = 9.11e-31 kg) is in a region near" 
    "the surface of the earth where there is a uniform electric field E"
    "of magnitude 4.0e-12 N/C, directed along the negative y-axis."
    "The electron is moving in the positive y-direction at an initial"
    "velocity of 4.3 m/s. How far will the electron travel before it"
    "momentarily comes to rest?"
    "    Answer:  [XXXXXXXXXXXX]"
    "In this problem, please include gravity."
  )
  ;; :graphic "elec6.gif"
  :features (E&M E-field andes2 working kinematics dynamics energy)
  :choices ((bodies 
    (electron Earth))
    (positions (region)))
  :times ((1 "at 4.3 m/s") (2 "at rest") (during 1 2))
  :givens (
    (time 1) (time 2) (time (during 1 2))
    (object electron)
    (near-planet earth)
    ;; Electrostatics
    (at-place electron region (during 1 2))
    (given (charge-on electron) (dnum -1.60E-19 |C|)) 
    (given (mass electron)      (dnum 9.11E-31 |kg|)) 
    (given (mag (field region electric unspecified :time (during 1 2))) 
      (dnum 4.0E-12 |N/C|)) 
    (given (dir (field region electric unspecified :time (during 1 2))) 
      (dnum 270 |deg|))
    (E-field unspecified)
    ;; Kinematics (rectilinear)
    (given (mag (velocity electron :time 1)) (dnum 4.3 |m/s|))
    (motion electron (straight nil (dnum 90 |deg|)) :time 1)
    (motion electron (straight slow-down (dnum 90 |deg|)) :time (during 1 2))
    (motion electron momentarily-at-rest :time 2)
    (constant (accel electron) (during 1 2))
     ;; Although there is an electrostatic potential, we don't 
     ;; have any rules to derive it.
    (unknown-potentials)
    (use-work)
  )
  :soughts (
    (mag (displacement electron :time (during 1 2)))
  )
)
