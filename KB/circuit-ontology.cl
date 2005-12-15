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
          :english ("the voltage across ~A~@[ ~A~]" ?comp (nlg ?time 'pp)))

(def-qexp resistance (resistance ?names)
	:units |$W|
	:english ("the resistance of ~A" (conjoined-names ?names)))

(def-qexp current-thru (current-thru ?component :time ?time)
	:units |A|
        ;; !!! HACK until sgg provides a correct way to do this  
	;; uncomment the following and reload kb just before problems 
	;; LR1b,1c,2b that require
	;; that solver know current is positive. comment out agin then 
	;; reload before others.
	;:restrictions positive 
	:english ("the current through ~A~@[ ~A~]" ?component (nlg ?time 'pp)))

(def-qexp current-in (current-in ?branch :time ?time)
	:units |A|
	:english ("the current in branch ~A~@[ ~A~]" ?branch (nlg ?time 'pp)))

(def-qexp capacitance (capacitance ?name)
  :units |F|
  :english ("the capacitance of ~A" ?name))

;;; in the workbench, the time slot is added if feature changing-voltage
;;; is included.
(def-qexp charge-on (charge-on ?name :time ?time)
  :units |C|
  :fromWorkbench (if time `(charge-on ,body :time ,time) `(charge-on ,body))
  :english ("the charge on ~A" (nlg ?name 'at-time ?time)))

(def-qexp max-charge (max-charge ?name :time ?time)
  :units |C|)

(def-qexp inductance (inductance ?inductor)
          :units |H|
	  :fromWorkbench `(inductance ,body)
	  :english ("the inductance of ~A" (nlg ?inductor)))

;;; power as used in circuits problem has slightly different definition
;;; than power in mechanics: no agent, and may denote power output (from
;;; battery into charges) or power dissipated (through resistor).
(def-qexp electric-power (electric-power ?b :time ?time)
  :units W
  :english ("power transferred through ~a" (nlg ?b 'at-time ?time)))
;; We could define a generic rate-of-change function, but we don't have a way 
;; to define units as function of units of base ?quant over time, 
;; I don't think.  For now just define rate of change of a current 
;; thru component
(def-qexp current-change (rate-of-change (current-thru ?comp :time ?time))
  :units |A/s|
  :fromWorkbench `(rate-of-change (current-thru ,body :time ,time))
  :english ("the rate of change of the current through ~a" 
	    (nlg ?comp 'at-time ?time)))

(def-qexp time-constant (time-constant ?c1 ?c2)
          :units |s|
	  :fromWorkbench `(time-constant ,@(sort (list body body2) #'expr<))
	  :english ("the time constant for circuit components ~A and ~A" 
		       (nlg ?c1) (nlg ?c2)))

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
  :units |J|
  :english ("the electric potential energy of ~A" 
	    (nlg ?body 'at-time ?time)))

(def-qexp stored-energy (stored-energy ?component :time ?time)
  :units |J|
  :english ("the electric energy stored in ~a" 
	    (nlg ?component 'at-time ?time)))




