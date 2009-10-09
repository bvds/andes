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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuits quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.
(def-qexp voltage-across (voltage-across ?comp :time ?time)
  :units |V|
  :nlg-english ("the voltage across ~A~@[ ~A~]" ?comp (nlg ?time 'pp)))

(def-qexp resistance (resistance ?names)
  :symbol-base |R| 
  :short-name "resistance"
  :units |$W|
  :nlg-english ("the resistance of ~A" (conjoined-names ?names)))

(def-qexp current (current-thru ?component :time ?time)
  :symbol-base |I| 
  :short-name "current"
  :units |A|
  ;; No sign restriction on this quantity. A few prbs (LR1b,1c,2b) restrict 
  ;; currents to be positive on a per-problem basis with :VariableMarks
  :nlg-english ("the current through ~A~@[ ~A~]" (conjoined-names ?component) 
	    (nlg ?time 'pp)))

(def-qexp capacitance (capacitance ?name)
  :symbol-base |C|     
  :short-name "capacitance"
  :units |F|
  :nlg-english ("the capacitance of ~A" ?name))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp charge-on (charge ?name :time ?time)
  :symbol-base |q|     
  :short-name "charge"	
  :units |C|
  :nlg-english ("the charge on ~A" (nlg ?name 'at-time ?time)))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp number-of (number-of ?name)
  :symbol-base |N|     
  :short-name "number"	
  :units nil ;dimensionless
  :nlg-english ("the number of ~As" (nlg ?name)))

;; variation for surface, where "in" is appropriate
;; see feature gauss
(def-qexp charge-in (charge ?name :surface t :time ?time)
  :symbol-base |q|     
  :short-name "charge"	
  :units |C|
  :nlg-english ("the charge in ~A" (nlg ?name 'at-time ?time)))

(def-qexp max-charge (max-charge ?name :time ?time)
  :units |C|)

(def-qexp self-inductance (self-inductance ?inductor)
  :symbol-base |L|     
  :short-name "self-inductance"	
  :units |H|
  :nlg-english ("the inductance of ~A" (nlg ?inductor)))

(def-qexp mutual-inductance (mutual-inductance orderless . ?inductors)
  :symbol-base |M|     
  :short-name "mutual inductance"	
  :units |H|
  :nlg-english ("the mutual inductance of ~A" 
	    (nlg ?inductors 'conjoined-defnp)))

;;; power as used in circuits problem has slightly different definition
;;; than power in mechanics: no agent, and may denote power output (from
;;; battery into charges) or power dissipated (through resistor).
(def-qexp electric-power (electric-power ?b :time ?time)
  :symbol-base |P|     
  :short-name "electric power"	
  :units W
  :nlg-english ("power transferred through ~a" (nlg ?b 'at-time ?time)))
;; We could define a generic rate-of-change function, but we don't have a way 
;; to define units as function of units of base ?quant over time, 
;; I don't think.  For now just define rate of change of a current 
;; thru component

(def-qexp current-change (rate-of-change (current-thru ?comp :time ?time))
  :symbol-base |dIdt|     
  :short-name "rate of change of current"	
  :units |A/s|
  :nlg-english ("the rate of change of the current through ~a" 
	    (nlg ?comp 'at-time ?time)))

(def-qexp time-constant (time-constant orderless . ?quants)
  :symbol-base |$t|     
  :short-name "time constant"	
  :units |s|
  ;; translated wb body arg is (compound orderless comp1 comp2 ...)
  :nlg-english ("the time constant for ~A" 
		       (nlg ?quants 'conjoined-defnp)))

(def-qexp E-field (field ?region electric ?source :time ?time)
  :units |N/C|
  :nlg-english ("the electric field at ~A due to ~A~@[ ~A~]" 
	    (nlg ?region 'at-time ?time) (nlg ?source 'agent) (nlg ?time 'pp)))

(def-qexp B-field (field ?region magnetic ?source :time ?time)
  :units |T|
  :nlg-english ("the magnetic field at ~A due to ~A~@[ ~A~]" 
	    (nlg ?region) (nlg ?source 'agent) (nlg ?time 'pp)))

(def-qexp net-E-field (net-field ?region electric :time ?time)
  :units |N/C|
  :nlg-english ("the net electric field at ~A~@[ ~A~]" 
	    (nlg ?region) (nlg ?time 'pp)))

(def-qexp net-B-field (net-field ?region magnetic :time ?time)
  :units |T|
  :nlg-english ("the net magnetic field at ~A~@[ ~A~]" 
	    (nlg ?region) (nlg ?time 'pp)))
  
(def-qexp potential (potential ?loc ?source :time ?time)
  :symbol-base |V|     
  :short-name "electric potential"	
  :units |V|
  :nlg-english ("the electric potential at ~a due to ~A~@[ ~A~]" 
	       (nlg ?loc) (nlg ?source 'agent) (nlg ?time 'pp)))

(def-qexp net-potential (net-potential ?loc :time ?time)
  :units |V|
  :nlg-english ("the net electric potential at ~a from all sources~@[ ~A~]" 
	    (nlg ?loc) (nlg ?time 'pp)))

(def-qexp electric-energy (electric-energy ?body ?source :time ?time)
  :units |J|
  :short-name "electric potential energy"
  :nlg-english ("the electric potential energy of ~A" 
	    (nlg ?body 'at-time ?time)))

(def-qexp stored-energy (stored-energy ?component :time ?time)
  :symbol-base |U|     
  :short-name "energy stored"	
  :units |J|
  :nlg-english ("the electric energy stored in ~a" 
	    (nlg ?component 'at-time ?time)))
