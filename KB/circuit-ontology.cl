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
;;
;; KB/ontology: defines the expressions used in the AndesF Knowledge Base
;;

(def-qexp field (field ?field)
  :new-english (eval (when (expand-new-english ?field)
                        '("at"
                          (or (var (field ?field)) ?field)))))

(def-qexp property-field-source-time 
    (property-field-source-time ?property ?field ?source :time ?time)
  :new-english ((the)
		?property 
		(and (preferred (field ?field))
		     (preferred (agent ?source))
		     (time ?time))
		)
)

(def-qexp property-field-time (property-field-time ?property ?field :time ?time)
  :new-english	((the) 
		 ?property  		; "electric field" 
		 (and (preferred (field ?field)) 
		      (time ?time))
		 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuits quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.
(def-qexp voltage-across (voltage-across ?comp :time ?time)
  :units |V|
  :new-english ((the) "voltage across" ?comp (time ?time) ))

(def-qexp property-field-time 
    (property-field-time ?property ?field :time ?time)
  :new-english	((the) 
		 ?property  		; "electric field" 
		 (and (preferred (field ?field)) 
		      (time ?time))
		 )
)

(def-qexp property-type-field-time 
    (property-type-field-time ?type ?property ?field :time ?time)
  :new-english	((the) (field-type ?type) ?property 
		      (and (preferred (field ?field)) 
		 	   (time ?time))
		)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuits quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.
(def-qexp voltage-across (voltage-across ?comp :time ?time)
  :units |V|
  :new-english ((the) "voltage across" ?comp (time ?time) )) 

(def-qexp resistance (resistance ?names)
  :symbol-base |R| 
  :short-name "resistance"
  :units |$W|
  :new-english ((the) "resistance of" (conjoin (or "and" "&") . ?names)))

(def-qexp current (current-thru ?component :time ?time)
  :symbol-base |I| 
  :short-name "current"
  :units |A|
  ;; No sign restriction on this quantity. A few prbs (LR1b,1c,2b) restrict 
  ;; currents to be positive on a per-problem basis with :VariableMarks
  :new-english ((the) "current through" (conjoin (or "and" "&"). ?component) 
		(time ?time)))

(def-qexp capacitance (capacitance ?name)
  :symbol-base |C|     
  :short-name "capacitance"
  :units |F|
  :new-english (property-object "capacitance" 
				(or (var (body ?name) :namespace :objects) 
				    ?name) ))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp charge-on (charge ?name :time ?time)
  :symbol-base |q|     
  :short-name "charge"	
  :units |C|
  :new-english ((the) "charge on" (or (var (body ?name) :namespace :objects) 
				      ?name) (time ?time)))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp number-of (number-of ?name)
  :symbol-base |N|     
  :short-name "number"	
  :units nil ;dimensionless
  :new-english (property-object "number" ?name))

;; variation for surface, where "in" is appropriate
;; see feature gauss
(def-qexp charge-in (charge ?name :surface t :time ?time)
  :symbol-base |q|     
  :short-name "charge"	
  :units |C|
  :new-english ((the) "charge in" (or (var (body ?name)
					   :namespace :objects) ?name)  
		(time ?time)))

(def-qexp max-charge (max-charge ?name :time ?time)
  :symbol-base |q|     
  :short-name "max-charge"	
  :units |C|
  :new-english ((the) or("maximum" "max") "charge in" 
		(or (var (body ?name) :namespace :objects) 
		    ?name)  (time ?time)))

(def-qexp self-inductance (self-inductance ?inductor)
  :symbol-base |L|     
  :short-name "self-inductance"	
  :units |H|
  :new-english (property-object "inductance" ?inductor))

(def-qexp mutual-inductance (mutual-inductance orderless . ?inductors)
  :symbol-base |M|     
  :short-name "mutual inductance"	
  :units |H|
  :new-english ((the) "mutual inductance of" (conjoin (or "and" "&") ?inductors)))

;;; power as used in circuits problem has slightly different definition
;;; than power in mechanics: no agent, and may denote power output (from
;;; battery into charges) or power dissipated (through resistor).
(def-qexp electric-power (electric-power ?b :time ?time)
  :symbol-base |P|     
  :short-name "electric power"	
  :units W
  :new-english ("power transferred through" ?b (time ?time)))
;; We could define a generic rate-of-change function, but we don't have a way 
;; to define units as function of units of base ?quant over time, 
;; I don't think.  For now just define rate of change of a current 
;; thru component

(def-qexp current-change (rate-of-change (current-thru ?comp :time ?time))
  :symbol-base |dIdt|     
  :short-name "rate of change of current"	
  :units |A/s|
  :new-english ((rate (change "current")) "through" ?comp (time ?time)))

(def-qexp time-constant (time-constant orderless . ?quants)
  :symbol-base |$t|     
  :short-name "time constant"	
  :units |s|
  ;; translated wb body arg is (compound orderless comp1 comp2 ...)
  :new-english ((the) "time constant for" (conjoin (or "and" "&") ?quants)))


(def-qexp E-field (field ?region electric ?source :time ?time)
  :units |N/C|
  :new-english (property-field-source-time "electric field" ?region ?source 
					   :time ?time ))

(def-qexp B-field (field ?region magnetic ?source :time ?time)
  :units |T|
  :new-english (property-field-source-time "magnetic field" ?region ?source 
					   :time ?time ))

(def-qexp net-E-field (net-field ?region electric :time ?time)
  :units |N/C|
  :new-english (property-field-time "net electric field" ?region :time ?time ))

(def-qexp net-B-field (net-field ?region magnetic :time ?time)
  :units |T|
  :new-english (property-field-time "net magnetic field" ?region :time ?time ))

(def-qexp potential (potential ?loc ?source :time ?time)
  :symbol-base |V|     
  :short-name "electric potential"	
  :units |V|
  :new-english (property-field-source-time "electric potential" ?loc ?source 
					   :time ?time))

(def-qexp net-potential (net-potential ?loc :time ?time)
  :units |V|
  :new-english (property-field-time "net electric potential" ?loc :time ?time))

(def-qexp electric-energy (electric-energy ?body ?source :time ?time)
  :units |J|
  :short-name "electric potential energy"
  :new-english (property-object-source-time "electric potential energy" ?body 
					    ?source :time ?time))

(def-qexp stored-energy (stored-energy ?component :time ?time)
  :symbol-base |U|     
  :short-name "energy stored"	
  :units |J|
  :new-english ((the) "electric energy stored in" ?component (time ?time)))
