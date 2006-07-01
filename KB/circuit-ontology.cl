;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KB/ontology: defines the expressions used in the AndesF Knowledge Base
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuits quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.
(def-qexp voltage-across (voltage-across ?comp :time ?time)
  ;; custom dialog box "voltage"
  :units |V|
  :english ("the voltage across ~A~@[ ~A~]" ?comp (nlg ?time 'pp)))

(def-qexp resistance (resistance ?names)
  :symbol-base |R| 
  :short-name "resistance"
  ;; custom dialog box, need something here to make entry in scalars.tsv
  :dialog-text ""  
  :units |$W|
  :english ("the resistance of ~A" (conjoined-names ?names)))

;;  The dialog box should look something like the resistance dialog box.
;;  After the student makes an entry, however, the helpsystem should
;;  match the student entry to any system entry that contains all of the 
;;  components selected by the student as a subset.
;;
;; In order for the hints to be useful, it is necessary that 
;; ?component would be a list of names that would mean something
;; to the student.  For the current through a branch, we take components 
;; defined by (circuit-element ...) 
(def-qexp current-thru (current-thru ?component :time ?time)
  :symbol-base |I| 
  :short-name "current"
  ;; custom dialog box, need something here to make entry in scalars.tsv
  :dialog-text ""  
  :units |A|
  ;; No sign restriction on this quantity. A few prbs (LR1b,1c,2b) restrict 
  ;; currents to be positive on a per-problem basis with :VariableMarks
  :english ("the current through ~A~@[ ~A~]" (conjoined-names ?component) 
	    (nlg ?time 'pp)))

(def-qexp capacitance (capacitance ?name)
  :symbol-base |C|     
  :short-name "capacitance"
  ;; custom dialog box, need something here to make entry in scalars.tsv
  :dialog-text ""  
  :units |F|
  :english ("the capacitance of ~A" ?name))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp charge-on (charge ?name :time ?time)
  :symbol-base |q|     
  :short-name "charge"	
  :dialog-text "on [body:bodies]"
  :units |C|
  :fromWorkbench (if time `(charge ,body :time ,time) `(charge ,body))
  :english ("the charge on ~A" (nlg ?name 'at-time ?time)))

;; variation for surface, where "in" is appropriate
;; see feature gauss
(def-qexp charge-in (charge ?name :surface t :time ?time)
  :symbol-base |q|     
  :short-name "charge"	
  :dialog-text "in [body:bodies]"
  :units |C|
  :fromWorkbench (if time `(charge ,body :surface t :time ,time)
		   `(charge ,body :surface t))
  :english ("the charge in ~A" (nlg ?name 'at-time ?time)))

(def-qexp max-charge (max-charge ?name :time ?time)
  :units |C|)

(def-qexp self-inductance (self-inductance ?inductor)
  :symbol-base |L|     
  :short-name "self-inductance"	
  :dialog-text "of [body:bodies]"
  :units |H|
  :fromWorkbench `(self-inductance ,body)
  :english ("the inductance of ~A" (nlg ?inductor)))

(def-qexp mutual-inductance (mutual-inductance orderless . ?inductors)
  :symbol-base |M|     
  :short-name "mutual inductance"	
  :dialog-text "between [body:bodies] and [body2:bodies]"
  :units |H|
  :fromWorkbench `(mutual-inductance orderless ,body ,body2)
  :english ("the mutual inductance of ~A" 
	    (nlg ?inductors 'conjoined-defnp)))

;;; power as used in circuits problem has slightly different definition
;;; than power in mechanics: no agent, and may denote power output (from
;;; battery into charges) or power dissipated (through resistor).
(def-qexp electric-power (electric-power ?b :time ?time)
  :symbol-base |P|     
  :short-name "electric power"	
  :dialog-text "transferred through [body:bodies] at time [time:times]"
  :units W
  :english ("power transferred through ~a" (nlg ?b 'at-time ?time)))
;; We could define a generic rate-of-change function, but we don't have a way 
;; to define units as function of units of base ?quant over time, 
;; I don't think.  For now just define rate of change of a current 
;; thru component

(def-qexp current-change (rate-of-change (current-thru ?comp :time ?time))
  :symbol-base |dIdt|     
  :short-name "rate of change of current"	
  :dialog-text "through [body:bodies] at time [time:times]"
  :units |A/s|
  :fromWorkbench `(rate-of-change (current-thru ,body :time ,time))
  :english ("the rate of change of the current through ~a" 
	    (nlg ?comp 'at-time ?time)))

(def-qexp time-constant (time-constant orderless . ?quants)
  :symbol-base |$t|     
  :short-name "time constant"	
  :dialog-text "for circuit elements [body:bodies] and [body2:bodies]"
  :units |s|
  ;; translated wb body arg is (compound orderless comp1 comp2 ...)
  :fromWorkbench `(time-constant orderless ,@(bodyterm-complist body))
  :english ("the time constant for ~A" 
		       (nlg ?quants 'conjoined-defnp)))

(def-qexp E-field (field ?region electric ?source :time ?time)
  :units |N/C|
  :english ("electric field at ~A due to ~A~@[ ~A~]" 
	    (nlg ?region 'at-time ?time) (nlg ?source 'agent) (nlg ?time 'pp)))

(def-qexp B-field (field ?region magnetic ?source :time ?time)
  :units |T|
  :english ("magnetic field at ~A due to ~A~@[ ~A~]" 
	    (nlg ?region) (nlg ?source 'agent) (nlg ?time 'pp)))

(def-qexp net-field (net-field ?region ?type :time ?time)
  :units |N/C|
  :english ("net ~A field at ~A from all sources~@[ ~A~]" 
	    (nlg ?type) (nlg ?region) (nlg ?time 'pp)))
  
(def-qexp potential (potential ?loc ?source :time ?time)
  :symbol-base |V|     
  :short-name "potential"	
  :dialog-text "at [body:positions] due to [body2:bodies] at time [time:times]"
  :units |V|
  :fromWorkbench (if (or (null body2) (string-equal body2 '|all sources|))
                     `(net-potential ,body :time ,time)
                  `(potential ,body ,body2 :time ,time))
  :english ("the electric potential at ~a due to ~A~@[ ~A~]" 
	       (nlg ?loc) (nlg ?source 'agent) (nlg ?time 'pp)))

(def-qexp net-potential (net-potential ?loc :time ?time)
  :units |V|
  :english ("the net electric potential at ~a from all sources~@[ ~A~]" 
	    (nlg ?loc) (nlg ?time 'pp)))

(def-qexp electric-energy (electric-energy ?body ?source :time ?time)
  ;; custom dialog box "energy"
  :units |J|
  :english ("the electric potential energy of ~A" 
	    (nlg ?body 'at-time ?time)))

(def-qexp stored-energy (stored-energy ?component :time ?time)
  :symbol-base |U|     
  :short-name "energy stored"	
  :dialog-text "in [body:bodies] at time [time:times]"
  :units |J|
  :english ("the electric energy stored in ~a" 
	    (nlg ?component 'at-time ?time)))




