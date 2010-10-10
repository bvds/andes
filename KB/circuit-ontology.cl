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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuits quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.
(def-qexp voltage-across (voltage-across ?comp :time ?time)
  :rank scalar
  :units |V|
  :short-name "voltage"
  :new-english ((the) "voltage across" (component ?comp) (time ?time)))

;; BobShelby (epow2):  "resistance of R1"
;;                     "resistance of resistor R2"
(def-qexp resistance (resistance ?name)
  :rank scalar
  :symbol-base |R| 
  :short-name "resistance"
  :units |ohm| ;also \Omega
  :new-english ((the) "resistance of" (allowed "resistor") (component ?name)))

(def-qexp current (current-thru ?component :time ?time)
  :rank scalar
  :symbol-base |I| 
  :short-name "current"
  :units |A|
  ;; No sign restriction on this quantity. A few prbs (LR1b,1c,2b) restrict 
  ;; currents to be positive on a per-problem basis with :VariableMarks
  :new-english ((the) "current" (allowed "flowing") "through" 
		(or (component ?component) 
		    ;; If this is a string of components, the user
		    ;; can just as well use any of the components.
		    ;; There should be a knowledge component associated with
		    ;; the fact that components in series all have the same 
		    ;; current, but we will treat this as just an different 
		    ;; way of saying things.
		    (eval (when (consp ?component) (cons 'or ?component))))
		    (time ?time)))

(def-qexp capacitance (capacitance ?name)
  :rank scalar
  :symbol-base |C|     
  :short-name "capacitance"
  :units |F|
  :new-english ((the) "capacitance of" (allowed "capacitor") 
		(component ?name)))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp charge-on (charge ?name :time ?time)
  :rank scalar
  :symbol-base |q|     
  :short-name "charge"	
  :units |C|
  :new-english ((the) "charge on" (component ?name) (time ?time)))

(def-qexp circuit-component (component ?a)
  ;; Generally components are a single symbol or a list for
  ;; a compound component.
  ;; Sometimes the component can be defined with the body tool.
  :new-english (or (var ?a :namespace :objects)
		   (eval (if (consp ?a) '(conjoin (or "and" "&") . ?a) ?a)
			 ;; Remove list for single components
			 (?a . (append (problem-atoms *cp*)
				       (problem-circuit-compounds *cp*)
				       (problem-circuit-branches *cp*))))))

(defun problem-circuit-branches (problem)
  "Collect branches of the circuit."
  ;; The raw branches in the givens also contain junctions.
  ;; This is mostly useful for current quantities.
  (mapcar #'second (remove '(path-to-branch . ?rest)
			   (problem-wm problem)
			   :test-not #'unify)))

(defparameter *circuit-compounds* 
  '(series-resistors parallel-resistors series-capacitors parallel-capacitors))

(defun problem-circuit-compounds (problem)
  "Collect explicitly declared compound objects"
  ;; Equivalent resistance & capacitance done
  ;; with explicit declarations in problem givens.
  ;;
  ;; Quantity propositions for compound circuit elements
  ;;  remove substructure of compounds.
  ;; The presence of the sort is a bit worrisome since
  ;;  there is no "orderless" enforcing it in the KB.
  (mapcar #'(lambda (x) (sort (flatten (second x)) #'string<))
	  (remove-if
	   #'(lambda (x) (not (member x *circuit-compounds*)))
	   (problem-givens problem) 
	   :key #'car)))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp number-of (number-of ?name)
  :rank scalar
  :symbol-base |N|     
  :short-name "number"	
  :units nil ;dimensionless
  :new-english (property-object "number" ?name))

;; variation for surface, where "in" is appropriate
;; see feature gauss
(def-qexp charge-in (charge ?name :surface t :time ?time)
  :rank scalar
  :symbol-base |q|     
  :short-name "charge"	
  :units |C|
  :new-english ((the) "charge in" (component ?name) (time ?time)))

(def-qexp max-charge (max-charge ?name :time ?time)
  :rank scalar
  :symbol-base |q|     
  :short-name "max-charge"	
  :units |C|
  :new-english ((the) (or "maximum" "max") "charge in" 
		(component ?name)  (time ?time)))

;; ind3a "self-inductance"
;; from resitance:  "the inductance of inductor L1"
(def-qexp self-inductance (self-inductance ?inductor)
  :rank scalar
  :symbol-base |L|     
  :short-name "self-inductance"	
  :units |H|
  :new-english ((the) (or ((preferred "self") "inductance")
			  "self-inductance")
	        "of" (allowed "inductor") (component ?inductor)))

(def-qexp mutual-inductance (mutual-inductance orderless . ?inductors)
  :rank scalar
  :symbol-base |M|     
  :short-name "mutual inductance"	
  :units |H|
  :new-english ((the) "mutual inductance" (or "between" "of") 
		(conjoin (or "and" "&") . ?inductors)))

;;; power as used in circuits problem has slightly different definition
;;; than power in mechanics: no agent, and may denote power output (from
;;; battery into charges) or power dissipated (through resistor).
(def-qexp electric-power (electric-power ?b :time ?time)
  :rank scalar
  :symbol-base |P|     
  :short-name "electric power"	
  :units W
  :new-english ("power transferred through" ?b (time ?time)))
;; We could define a generic rate-of-change function, but we don't have a way 
;; to define units as function of units of base ?quant over time, 
;; I don't think.  For now just define rate of change of a current 
;; thru component

(def-qexp current-change (rate-of-change (current-thru ?comp :time ?time))
  :rank scalar
  :symbol-base |dIdt|     
  :short-name "rate of change of current"	
  :units |A/s|
  :new-english (time-derivative (current-thru ?comp) :time ?time))

(def-qexp time-constant (time-constant orderless . ?quants)
  :rank scalar
  :symbol-base |\\tau|     
  :short-name "time constant"	
  :units |s|
  ;; translated wb body arg is (compound orderless comp1 comp2 ...)
  :new-english ((the) "time constant for" (conjoin (or "and" "&") . ?quants)))

;; Have field in "region" which should be nil.
;;    all cases have associated  (homogeneous-field region ...)
;;    but some other problem quantities sometimes refer to region.
;; Some cases of a specific region, surface, many instances of points.
(def-qexp E-field (field electric :location ?region :source ?source :time ?time)
  :rank vector
  :units |N/C|
  :short-name "electric field"
  :new-english (any-field (or "electric" "E") 
			  :location ?region :source ?source :time ?time))

(def-qexp B-field (field magnetic :location ?region :source ?source :time ?time)
  :rank vector
  :units |T|
  :short-name "magnetic field"
  :new-english (any-field (or "magnetic" "B") 
			  :location ?region :source ?source :time ?time))

(def-qexp any-field (any-field ?type :location ?region :source ?source :time ?time)
  :new-english ((the) ?type "field" 
		(and (region ?region)
		     (preferred (agent ?source))
		     (time ?time))))

(def-qexp region (region ?region)
  ;; "in a region"
  ;; BvdS:  generally, a region would not be defined by the body tool?
  ;; Need some way to distinguish between regions and points.
  ;;   One way would be a problem-specific ontology.
  ;;   Bug #1724
  :new-english (eval (if (expand-new-english ?region)
			'(preferred ((or "in" "inside") ?region))
			'(allowed ("in" (the) "region")))
		     ;; include case where region is omitted.
		     (?region . (cons nil (problem-atoms *cp*)))))

;; All instances of (net-field ...) are for fields at a point
(def-qexp net-E-field (net-field ?region electric :time ?time)
  :rank vector
  :units |N/C|
  :short-name "net electric field"
  :new-english (any-net-field ?region (or "electric" "E") :time ?time))

(def-qexp net-B-field (net-field ?region magnetic :time ?time)
  :rank vector
  :units |T|
  :short-name "net magnetic field"
  :new-english (any-net-field ?region (or "magnetic" "B") :time ?time))

(def-qexp any-net-field (any-net-field ?region ?type :time ?time)
  :new-english ((the) (or "net" "total") ?type "field" 
		(and (region ?region)
		     (time ?time))))

(def-qexp potential (potential ?loc ?source :time ?time)
  :rank scalar
  :symbol-base |V|     
  :short-name "electric potential"	
  :units |V|
  :new-english ((the)
		(or "electric" "electrostatic") "potential" 
		(and (region ?loc) 
		     (preferred (agent ?source))
		     (time ?time))))

;; Need to distinguish points and regions
;; "net potential at P1"
;; "net potential in the region"
(def-qexp net-potential (net-potential ?loc :time ?time)
  :rank scalar
  :units |V|
  :short-name "net electric potential"
  :new-english ((the) (or "net" "total")  
		(or "electric" "electrostatic") "potential" 
		(and (region ?loc) (time ?time))))

(def-qexp electric-energy (electric-energy ?body ?source :time ?time)
  :rank scalar
  :units |J|
  :short-name "electric potential energy"
  :new-english ((the) (or "electric" "electrostatic") "potential energy" 
		(and (preferred (object ?body)) (preferred (agent ?source))
		     (time ?time))))

(def-qexp stored-energy (stored-energy ?component :time ?time)
  :rank scalar
  :symbol-base |U|     
  :short-name "energy stored"	
  :units |J|
  :new-english ((the) "electric energy stored in" ?component (time ?time)))
