;;;
;;;    List of quantities associated with various "features"
;;;
;;;

(def-feature-set Andes2 (mag dir compo))
(def-feature-set dipole (dipole-energy))

(def-feature-set kinematics (distance speed duration angle))
(def-feature-set statics (mass angle coef-friction duration))
(def-feature-set dynamics (mass angle coef-friction duration))
(def-feature-set changing-mass (mass mass-change-magnitude))
(def-feature-set circular 
  (mass radius revolution-radius period duration angle))
(def-feature-set energy 
  (mass duration work power energy compression spring-constant height))
(def-feature-set work (work power duration angle))
(def-feature-set work-quants 
  (work power duration angle intensity db-intensity db-intensity-zero))
(def-feature-set work-quants-out 
  (work net-power-out duration angle intensity 
	db-intensity db-intensity-zero))
(def-feature-set linmom (energy duration))
(def-feature-set rotkin 
  (mass radius revolution-radius period duration 
	angle moment-of-inertia length width))
(def-feature-set angmom 
  (mass radius period duration angle moment-of-inertia length width))
(def-feature-set torque 
  (mass radius period duration angle moment-of-inertia length width))
(def-feature-set circuits 
  (electric-power current voltage resistance capacitance charge-on 
		  stored-energy inductance mutual-inductance current-change 
		  time-constant duration 
		  ;; BvdS:  had to add with checking enabled
		  voltage-across current-thru))
(def-feature-set E&M 
  (charge-on current potential duration length turns turns-per-length 
	     angle electric-flux magnetic-flux electric-flux-change 
	     magnetic-flux-change))
(def-feature-set optics 
  (object-distance image-distance focal-length magnification 
		   radius-of-curvature lens-distance index-of-refraction angle))
(def-feature-set fluids 
  (mass duration height mass-density pressure area-at area volume))
(def-feature-set rectangle-geometry 
  (area area-change length width length-change))
(def-feature-set circle-geometry 
  (area radius-of-circle diameter-of-circle circumference-of-circle))
(def-feature-set waves 
  (frequency distance speed duration mass length mass-per-length wavelength 
	     wavenumber period angular-frequency wave-speed 
	     index-of-refraction speed-of-light refraction-vacuum 
	     string-tension))
(def-feature-set observed-frequency (observed-frequency))
(def-feature-set oscillations 
  (mass length spring-constant compression wavelength wavenumber period 
	frequency angular-frequency amplitude amplitude-max-speed 
	amplitude-max-abs-acceleration))
(def-feature-set probability (probability))
(def-feature-set multiple-planets (gravitational-acceleration))

