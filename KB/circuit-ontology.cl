;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KB/ontology: defines the expressions used in the AndesF Knowledge Base
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuits quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.
(def-qexp voltage-across (voltage-across ?comp)
          :units |V|
          :english ("the voltage across ~A" ?comp))
#| ;not used at the moment
(def-qexp voltage-btwn (voltage-btwn ?pt1 ?pt2)
	:units |V|
	:english ("the potential difference between ~A and ~A" ?pt1 ?pt2))
|#
(def-qexp resistance (resistance ?names)
	:units |ohm|
	:english ("the resistance of ~A" (conjoined-names ?names)))
(def-qexp current-thru (current-thru ?component)
	:units |A|
        ; !!! HACK until sgg provides a correct way to do this  
	; uncomment the following and reload kb just before problems LR1b,1c,2b that require
	; that solver know current is positive. comment out agin then reload before others.
	;:restrictions positive 
	:english ("the current through ~A" ?component))
#| ; not used at the moment
(def-qexp current-at (current-at ?point)
	:units |A|
	:english ("the current at ~A" ?point))
|#
(def-qexp current-in (current-in ?branch)
	:units |A|
	:english ("the current in branch ~A" ?branch))
(def-qexp capacitance (capacitance ?name)
        :units |F|
	:english ("the capacitance of ~A" ?name))
(def-qexp charge-on (charge-on ?name)
        :units |C|
	:fromWorkbench `(at (charge-on ,body) ,time)
        :english ("the charge on ~A" (nlg ?name)))
(def-qexp max-charge (max-charge ?name)
          :units |C|)
#|
(def-qexp time (time ?name)
          :units |ms|
	  :english ("the time at ~A" (moment ?name)))
|#
(def-qexp inductance (inductance ?inductor)
          :units |H|
	  :fromWorkbench `(inductance ,body)
	  :english ("the inductance of ~A" (nlg ?inductor)))

;;; power as used in circuits problem has slightly different definition
;;; than power in mechanics: no agent, and may denote power output (from
;;; battery into charges) or power dissipated (through resistor).
(def-qexp electric-power (electric-power ?b)
  :units W
  :english ("power transferred through ~a" (nlg ?b)))
;; We could define a generic rate-of-change function, but we don't have a way to
;; define units as function of units of base ?quant over time, I don't think.
;;; For now just define rate of change of a current thru component
(def-qexp current-change (rate-of-change (current-thru ?comp))
  :units |A/s|
  :fromWorkbench `(at (rate-of-change (current-thru ,body)) ,time)
  :english ("the rate of change of the current through ~a" (nlg ?comp)))

(def-qexp time-constant (time-constant ?c1 ?c2)
          :units |s|
	  :fromWorkbench `(time-constant ,@(sort (list body body2) #'expr<))
	  :english ("the time constant for circuit components ~A and ~A" 
		       (nlg ?c1) (nlg ?c2)))

(def-qexp E-field (field ?region electric ?source)
  :units |N/C|
  :english ("electric field at ~A due to ~A" 
	    (nlg ?region) (nlg ?source 'agent)))

(def-qexp B-field (field ?region magnetic ?source)
  :units |T|
  :english ("magnetic field at ~A due to ~A" 
	    (nlg ?region) (nlg ?source 'agent)))

(def-qexp net-field (net-field ?region ?type)
  :units |N/C|
  :english ("net ~A field at ~A from all sources" (nlg ?type) (nlg ?region) ))
  
(def-qexp potential (potential ?loc ?source)
  :units |V|
  :fromWorkbench (if (or (null body2) (string-equal body2 '|all sources|))
                     `(at (net-potential ,body) ,time)
                  `(at (potential ,body ,body2) ,time))
  :english ("the electric potential at ~a due to ~a" 
	       (nlg ?loc) (nlg ?source 'agent)))

(def-qexp net-potential (net-potential ?loc)
  :units |V|
  :english ("the net electric potential at ~a from all sources" (nlg ?loc) ))

(def-qexp electric-energy (electric-energy ?body ?source)
  :units |J|
  :english ("the electric potential energy of ~A" (nlg ?body)))

(def-qexp stored-energy (stored-energy ?component)
  :units |J|
  :english ("the electric energy stored in ~a" (nlg ?component)))

(defun conjoined-names (names)
   (if (listp names) (format NIL "~A~{ and ~A~}" (first names) (rest names))
     (format NIL "~A" names)))




