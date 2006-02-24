;;;
;;;    List of quantities associated with various "features"
;;;
;;;
;;; The following features enable custom dialog boxes on the workbench:
;;;   speed, angle, energy, current, voltage, resistance, capacitance,
;;;   duration, probablity, time-constant

;; global features to match all vector quantities
(def-feature-set Andes2 (mag dir compo)) 

(def-feature-set dipole (dipole-energy))
(def-feature-set kinematics (distance speed duration angle 
				      ;; quantity associated with angle
				      angle-between))
(def-feature-set statics (mass angle coef-friction duration
			       ;; quantity associated with angle
			       angle-between))
(def-feature-set dynamics (mass angle coef-friction duration
				;; quantity associated with angle
				angle-between))
(def-feature-set changing-mass (mass mass-change-magnitude))
(def-feature-set circular 
  (mass revolution-radius period duration angle 
	;; quantity associated with angle
	angle-between))
(def-feature-set energy 
  (mass duration work power energy compression spring-constant height))
(def-feature-set work (work power duration angle
			    ;; quantity associated with angle
			    angle-between))
(def-feature-set work-quants 
  (work power duration angle intensity db-intensity
			    ;; quantity associated with angle
			    angle-between))
(def-feature-set work-quants-out 
  (work net-power-out duration angle intensity db-intensity
	;; quantity associated with angle
	angle-between))
(def-feature-set linmom (energy duration))
(def-feature-set rotkin 
  (mass revolution-radius period duration angle moment-of-inertia length width
	;; quantity associated with angle
	angle-between))
(def-feature-set angmom 
  (mass period duration angle moment-of-inertia length width
	;; quantity associated with angle
	angle-between))
(def-feature-set torque 
  (mass period duration angle moment-of-inertia 
	length width
	;; quantity associated with angle
	angle-between))
(def-feature-set circuits 
  (electric-power current voltage resistance capacitance charge-on 
		  stored-energy inductance mutual-inductance current-change 
		  time-constant duration 
		  ;; quantities associated with voltage and current
		  voltage-across current-in current-thru))
(def-feature-set E&M 
  (charge-on current potential duration length turns turns-per-length angle 
	     electric-flux magnetic-flux electric-flux-change 
	     magnetic-flux-change
	     ;; quantity associated with angle
	     angle-between))
(def-feature-set optics 
  (object-distance image-distance focal-length magnification 
		   radius-of-curvature lens-distance index-of-refraction angle 
	;; quantity associated with angle
	angle-between))
(def-feature-set fluids 
  (mass duration height mass-density pressure area-at area volume))
(def-feature-set rectangle-geometry 
  (area area-change length width length-change))
(def-feature-set circle-geometry 
  (area radius-of-circle diameter-of-circle circumference-of-circle))
(def-feature-set waves 
  (frequency distance speed duration mass length mass-per-length wavelength 
	     wavenumber period angular-frequency wave-speed 
	     index-of-refraction
	     string-tension))
(def-feature-set observed-frequency (observed-frequency))
(def-feature-set oscillations 
  (mass length spring-constant compression wavelength wavenumber period 
	frequency angular-frequency amplitude amplitude-max-speed 
	amplitude-max-abs-acceleration))
(def-feature-set probability (probability))
(def-feature-set multiple-planets (gravitational-acceleration))

;;;             Utilities for constructing features file

(defun print-features (str)
 "express features in the format needed for KB/features.tsv"
  (dolist (f *Ontology-features*)
  (format str "~A~C~{~A;~}~%" 
	  (first f) #\tab (second f))))

(defun features-file ()
 "construct file KB/features.tsv"
    (let ((str (open (merge-pathnames  "KB/features.tsv" *Andes-Path*)
		     :direction :output :if-exists :supersede)))
		   (print-features str) (close str)))
