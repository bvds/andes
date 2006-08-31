;;;;
;;; (voltage R1) when PtA is left of R1 and PtB is right of R1 
;;; means the potential(PtB)-Potential(PtA)
;;; (current R1) means from PtA to PtB

;;;EQUIVALENT RESISTANCE
(defoperator define-resistance-var (?res)   
  :preconditions (
		  (bind ?r-var (format-sym "R_~A" (body-name ?res)))
		  )
  :effects (
	    (variable ?r-var (resistance ?res))
	    (define-var (resistance ?res))
	    )
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting resistance." 
			     ((resistance ?res) def-np)))
	 ))

(def-psmclass equiv-resistance-series (equiv-resistance-series ?res-list) 
  :complexity major
  :short-name "equivalent resistance series"
  :english ("Equivalent resistance of series resistors")
  :eqnFormat ("Req = R1 + R2 + ...") )

(defoperator equiv-resistance-series-contains (?sought)
  :preconditions (
		  (any-member ?sought ((resistance ?res-list)))
		  (test (listp ?res-list))
		  )
  :effects(
	   (eqn-contains (equiv-resistance-series ?res-list) ?sought)
	   ))

(
 def-psmclass equiv-resistance-parallel (equiv-resistance-parallel ?res-list) 
 :complexity major
  :short-name "equivalent resistance parallel"
 :english ("Equivalent resistance of parallel resistors")
 :eqnFormat ("1/Req = 1/R1 + 1/R2 + ...") )

(defoperator equiv-resistance-parallel-contains (?sought)
  :preconditions (
		  (any-member ?sought ((resistance ?res-list)))
		  (test (listp ?res-list))
		  )
  :effects(
	   (eqn-contains (equiv-resistance-parallel ?res-list) ?sought)
	   ))



(defoperator equiv-resistance-series (?res-list)
  :specifications "doc"
  :preconditions (
		  ;; verify sought list equivalent to some set of series resistors
		  ;; For simplicity we just put info about series resistors into givens. 
		  ;; List can include complex equivalents, e.g (series-resistors (R1 (R2 R3 R4) (R5 R6)))
		  ;; but should only contain one level of nesting, a list of sets of atoms

		  (series-resistors ?series-list)
		  ;; make sure the set of atomic resistors in series-list
		  ;; equals to the set of resistors sought in res-list. 
		  (test (null (set-exclusive-or (flatten ?series-list) 
						?res-list)))
		  (map ?res ?series-list
		       (variable ?r-var (resistance ?res))
		       ?r-var ?r-vars)
		  (variable ?tot-res (resistance ?res-list))
		  )
  :effects(
	   (resistance ?tot-res)                
	   (eqn (= ?tot-res (+ . ?r-vars)) (equiv-resistance-series ?res-list))
	   )
  :hint(
	(point (string "Write an equation for the equivalent resistance in terms of the individual resistances that are in series."))
	(point (string "The resistors that are in series are ~a" (?series-list conjoined-names)))
	(teach (string "The equivalent resistance for resistors in series is equal to the sum of the individual resistances."))
	(bottom-out (string "You need to add the individual resistances for ~a, and set it equal to the equivalent resistance ~a" 
			    (?series-list conjoined-names) (?tot-res algebra)))  
	))


(defoperator equiv-resistance-parallel (?res-list)
  :specifications "doc"
  :preconditions(
		 ;; verify sought list equivalent to some list of parallel resistors
		 ;; List can include complex 
		 ;; equivalents, e.g (parallel-resistors (R1 (R2 R3 R4) (R5 R6)))
		 ;; but should only contain one level of nesting, a list of sets of atoms
             
		 (parallel-resistors ?parallel-list) 
		 ;; make sure the set of atomic resistors in parallel-list
		 ;; equals to the set of resistors sought in res-list. 
		 (test (null (set-exclusive-or (flatten ?parallel-list) 
					       ?res-list)))
              
		 ;; pull out terms for each resistance
		 (map ?res ?parallel-list
		      (variable ?r-var (resistance ?res))
		      ?r-var ?r-vars)
 
		 (map ?res ?r-vars
		      (bind ?x (list '/ 1 '?res))
		      ?x ?rec-r-vars)
		 (variable ?tot-res (resistance ?res-list))
		 )
  :effects (
	    (resistance ?tot-res)
	    (eqn (= (/ 1 ?tot-res) (+ .  ?rec-r-vars)) (equiv-resistance-parallel ?res-list))
	    )
  :hint(
	(point (string "Write an equation for the equivalent resistance in terms of the individual resistances that are in parallel."))
	(point (string "The resistors that are in parallel are ~a"  (?parallel-list conjoined-names)))
	(teach (string "The reciprocal of the equivalent resistance for resistors in parallel is equal to the sum of the reciprocals of the individual resistances."))
	(bottom-out (string "Write the equation ~a" ((= (/ 1 ?tot-res) (+ .  ?rec-r-vars)) algebra)))
	))


;;;
;;;                      CURRENTS
;;;

(defoperator relate-path-and-branch (?path)
  :preconditions
  (
   ;; the ?path contains junctions and other objects
   ;; the ?branch contains objects in the path that have been
   ;; declared components.
   (branch ?name ?whatever1 ?whatever2 ?path)
   ;; find all circuit components in ?path to construct the ?branch
   ;; order of branch is given by order in ?path
   (setof (circuit-component ?compo ?type) ?compo ?compos)
   ;; Find any positions available to the student
   (bind ?positions (cadr (find 'positions (problem-choices *cp*) :key #'car)))
   (bind ?branch 
	 (remove-if-not 
	  #'(lambda (x) (member x (append ?compos ?positions) :test #'equal))
		  ;; Express any composite in terms of constituent elements.
		  ;; There may be multiple ?path for a given ?branch.
		  (flatten ?path)))
   )
  :effects ((path-to-branch ?branch ?path)))   

;; (compo-or-branch ...) fails if ?compo is in ?path for some 
;; (branch ... ?path) but has not been declared (circuit-component ...)
(defoperator use-component-for-current (?compo)
  :preconditions ((not (branch ?name ?whatever1 ?whatever2 ?path) 
		       (member ?compo ?path :test #'equal)))
  :effects ((compo-or-branch ?compo ?compo)))

(defoperator use-branch-for-current (?compo ?branch)
  :preconditions
  (
   ;; ?compo may be member of more than one ?path
   (branch ?name ?whatever1 ?whatever2 ?path)
   (path-to-branch ?branch ?path)
   ;; A loop has the same junction at beginning and end.
   (bind ?reduced-path (remove-duplicates ?path))
   ;; If ?compo is not bound, iterate over distinct elements in ?path.
   (any-member ?compo ?reduced-path)
   )
  :effects ((compo-or-branch ?compo ?branch)))


;; We should eventually get rid of this in favor of using the current
;; through the branch as the only internal variable
(def-psmclass current-thru-what (current-thru-what ?what ?branch ?t) 
  :complexity minor 
  :short-name "current in branch"
  :english ("Current in branch")
  :Expformat ("Relating the current through ~A to the current in the branch containing it" ?what)
  :eqnFormat ("Icomp = Ibranch"))

(defoperator current-thru-what-contains (?sought)
  :preconditions 
  (
   ;; note that ?t may be timeless
   (any-member ?sought ((current-thru ?branch :time ?t)
			(current-thru ?what :time ?t)))
   (compo-or-branch ?what ?branch)
   ;; Anything that is not a junction is allowed.
   (not (junction ?what . ?whatever))
   ;; For a composite object, the current variable is already equal.
   (test (not (equal ?what ?branch)))
   ;; On the workbench, the student has no way to define both the current
   ;; through a component and the branch containing only that component
   (test (not (equal (list ?what) ?branch)))
   )
  :effects ((eqn-contains (current-thru-what ?what ?branch ?t) ?sought)))

(defoperator write-current-thru-what (?what ?branch ?t)
  :specifications "doc"
  :preconditions 
  (
   (variable ?i-what-var (current-thru ?what :time ?t))
   (variable ?i-br-var (current-thru ?branch :time ?t))
   )
  :effects (
	    (eqn (= ?i-what-var ?i-br-var) (current-thru-what ?what ?branch ?t))
	    )
  :hint(
	(point (string "Write an equation relating the current through a circuit component to the current through a branch of the circuit containing the component."))
	(point (string "Consider the current through the circuit component ~a" (?what adj)))
	(teach (string "The current through a circuit component is equal to the current through the branch of the circuit containing the component."))
	(bottom-out (string "The current through the component, ~a is the same as the current through the branch of the circuit, ~a." (?i-what-var algebra) (?i-br-var algebra)))
	))

(defoperator define-current-thru (?what ?t)
  :preconditions 
  (
   ;; only use time when allowed by feature changing-voltage
   (test (eq (null ?t) 
	     (null (member 'changing-voltage (problem-features *cp*)))))
   ;; Current through a capacitor may be confusing to the student
   (not (circuit-component ?what capacitor))
   ;; ?what could be a list naming a compound (equivalent) circuit element.
   (bind ?i-what-var (format-sym "I_~A~@[_~A~]" (body-name ?what) 
				 (time-abbrev ?t)))
   )
  :effects (
	    (variable ?i-what-var (current-thru ?what :time ?t))
	    (define-var (current-thru ?what :time ?t))
	    )
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting current." 
			     ((current-thru ?what :time ?t) def-np)))
	 ))

;;;
;;;     Ohm's law
;;;		       

(def-psmclass ohms-law (ohms-law ?res ?t) 
  :complexity definition
  :short-name "Ohm's Law"
  :english ("Ohm's Law")
  :ExpFormat ("applying Ohm's law to ~A" (nlg ?res))
  :eqnFormat ("V = I*R"))

;;May need to uncomment (resistance) as a sought to get currents to work
(defoperator ohms-law-contains-resistor (?sought)
  :preconditions(
		 (any-member ?sought ((current-thru ?branch :time ?t ?t)
				      (voltage-across ?res :time ?t ?t)))
		 (time ?t) ;in case ?t is not bound
		 (circuit-component ?res resistor)
		 (compo-or-branch ?res ?branch)
		 ;;Added mary/Kay 7 May
		 ;;(branch ?br-res ?dontcare1 ?dontcare2 ?path)
		 ;;(test (member ?res ?path :test #'equal))     
		 )
  :effects(
	   (eqn-contains (ohms-law ?res ?t) ?sought)
	   ))

(defoperator single-resistance-contains (?sought)
  :preconditions(
		 (any-member ?sought ((resistance ?res)))
		 ;; only apply for resistance of atomic resistor:
		 ;; (Why? -- AW)
		 (test (atom ?res))
		 (time ?t)
		 (circuit-component ?res resistor)
		 ;;(branch ?br-res ?dontcare1 ?dontcare2 ?path)
		 ;;(test (member ?res ?path))
		 )
  :effects(
	   (eqn-contains (ohms-law ?res ?t) ?sought)
	   ))

(defoperator write-ohms-law (?res ?t)
  :specifications "doc"
  :preconditions
  (
   (variable ?r-var (resistance ?res))
   (any-member ?tot (?t nil)) 
   (compo-or-branch ?res ?branch)
   (variable ?i-var (current-thru ?branch :time ?tot))
   (any-member ?tot2 (?t nil)) 
   (variable ?v-var (voltage-across ?res :time ?tot2))
   )
  :effects(
	   (eqn (= ?v-var (* ?r-var ?i-var)) (ohms-law ?res ?t))
	   )
  :hint(
	(point (string "Apply Ohm's Law to the resistor, ~a." ?res))
	(point (string "Write an equation relating the voltage across the resistor ~a to the current through the resistor ~a and the resistance ~a." (?v-var algebra) (?i-var algebra) (?r-var algebra)))
	(teach (string "The voltage across the resistor is equal to product of the current through the resistor and the resistance."))
	(bottom-out (string "The voltage across the resistor ~a is equal to the current through the resistor ~a times the resistance ~a." (?v-var algebra) (?i-var algebra) (?r-var algebra)))
	))


(defoperator define-voltage-across (?comp ?t)
  :preconditions 
  (
   ;; only use time when allowed by feature changing-voltage
   (test (eq (null ?t) 
	     (null (member 'changing-voltage (problem-features *cp*)))))
   
   (bind ?v-var (format-sym "deltaV_~A~@[_~A~]" (body-name ?comp)
			    (time-abbrev ?t))))
  :effects (
	    (variable ?v-var (voltage-across ?comp :time ?t))
	    (define-var (voltage-across ?comp :time ?t))
	    )
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting voltage." 
			     ((voltage-across ?comp :time ?t) def-np)))
	 ))

(def-psmclass loop-rule  (loop-rule ?branch-list ?t)  
  :complexity major 
  :short-name "Kirchoff's loop rule"
  :english ("Kirchoff's loop rule")
  :eqnFormat ("V1 + V2 + V3 ... = 0"))

(defoperator loop-rule-contains (?comp ?t)
  :preconditions (
		  (closed-loop ?branch-list ?p1 ?p2 ?path ?reversed)
		  (test (member ?comp ?path :test #'equal))
		  (time ?t)
		  )
  :effects ( (eqn-contains (loop-rule ?branch-list ?t) 
			   (voltage-across ?comp :time ?t ?t)) ))

(defoperator closed-branch-is-loop (?branch)
  :preconditions (
		  (branch  ?branch ?dontcare  closed ?path)
		  )
  :effects (
	    (closed-loop ?branch ?path ?path ?path 0)
	    ))

;; This is quite similar to relate-path-and-branch 
(defoperator get-compos-for-path (?path)
  :preconditions
  (
   ;; the ?path contains junctions and other objects
   ;; the ?branch contains objects in the path that have been
   ;; declared components.
   (branch ?name ?whatever1 ?whatever2 ?path)
   ;; find all circuit components in ?path to construct the ?branch
   ;; order of branch is given by order in ?path
   (setof (circuit-component ?compo ?type) ?compo ?compos)
   (bind ?compos-in-path (remove-if-not
			  #'(lambda (x) (member x ?compos :test #'equal))
			  ?path))
   )
  :effects ((compos-in-path ?compos-in-path ?path)))   

;; their intersection won't work on :test #'equal
(defun our-intersection (x y)
  (intersection (flatten x) (flatten y)))

(defoperator form-two-branch-loop (?br1 ?br2)
  :preconditions 
  (
   (branch ?br1 ?dontcare1 open ?path1)
   (branch ?br2 ?dontcare2 open ?path2)
   (compos-in-path ?path-comps1 ?path1)
   (compos-in-path ?path-comps2 ?path2)
   ;;                       
   (test (null (and (our-intersection ?path-comps1 ?path2)
		    (our-intersection ?path-comps2 ?path1))))
                             
   ;; only try if branch names are in loop-id order
   (test (expr< ?br1 ?br2))
                             
   ;; test paths are connected. Two cases, depending on whether
   ;; path2 or (reverse path2) is needed to make a closed loop
   (test (or (and (equal (car (last ?path1)) (first ?path2))
		  (equal (car (last ?path2)) (first ?path1)))
	     (and (equal (first ?path1) (first ?path2))
		  (equal (car (last ?path1)) (car (last ?path2))))))
   ;; loop-id is sorted branch list
   (bind ?branch-list (list ?br1 ?br2))
                             
   ;; build loop path. Duplicate starting point at end to ensure all
   ;; components have two terminal points in path.
   (bind ?loop-path (if (not (equal (first ?path1) (first ?path2)))
			(append ?path1 (subseq ?path2 1))
		      (append ?path1 (subseq (reverse ?path2) 1))))
   (bind ?reversed (if (not (equal (first ?path1) (first ?path2))) 0 1))
   )
  :effects (
	    (closed-loop ?branch-list ?path-comps1 ?path-comps2 ?loop-path ?reversed)
	    ))

(defoperator form-three-branch-loop (?br1 ?br2)
  :preconditions 
  (
   (branch ?br1 ?dontcare1 open ?path1)
   (branch ?br2 ?dontcare2 open ?path2)
   (branch ?br3 ?dontcare3 open ?path3)
   ;; only try if branch names are in loop-id order
   (test (and (expr< ?br1 ?br2) (expr< ?br2 ?br3)))
   
   ;;(test (and (not (equal ?br1 ?br2))
   ;;               (not (equal ?br2 ?br3))
   ;;               (not (equal ?br3 ?br1))))
   ;; test not a self loop
   (compos-in-path ?path-comps1 ?path1)
   (compos-in-path ?path-comps2 ?path2)
   (compos-in-path ?path-comps3 ?path3)
   ;;
   (test (null (our-intersection ?path-comps1 ?path2)))
   (test (null (our-intersection ?path-comps1 ?path3)))
   (test (null (our-intersection ?path-comps2 ?path3)))
      
   ;; test paths are connected. Two cases, depending on whether
   ;; path2 or (reverse path2) is needed to make a closed loop
   (test (and (equal (car (last ?path1)) (first ?path2))
	      (equal (car (last ?path2)) (first ?path3))
	      (equal (car (last ?path3)) (first ?path1))))
   
   ;; loop-id is sorted branch list
   (bind ?branch-list (list ?br1 ?br2 ?br3))
   
   ;; build loop path. Duplicate starting point at end to ensure all
   ;; components have two terminal points in path.
   (bind ?path-comps4 (append ?path-comps1 ?path-comps2))
   (bind ?loop-path (append ?path1 (subseq ?path2 1) (subseq ?path3 1)))
                             
   (bind ?reversed 0)	;did not reverse
   ;; (bind ?reversed (if (not (equal (first ?path1) (first ?path2))) 0 1))
   )
  :effects 
  (
   (closed-loop ?branch-list ?path-comps4 ?path-comps3 ?loop-path ?reversed)
   ))

(defoperator form-four-branch-loop (?br1 ?br2)
  :preconditions 
  (
   (branch ?br1 ?dontcare1 open ?path1)
   (branch ?br2 ?dontcare2 open ?path2)
   (branch ?br3 ?dontcare3 open ?path3)
   (branch ?br4 ?dontcare4 open ?path4)
   ;; only try if branch names are in loop-id order
   (test (and (expr< ?br1 ?br2) (expr< ?br2 ?br3) (expr< ?br3 ?br4)))
   
   (test (and (= (length (intersection ?path1 ?path2)) 1)
	      (= (length (intersection ?path2 ?path3)) 1)
	      (= (length (intersection ?path3 ?path4)) 1)
	      (= (length (intersection ?path1 ?path4)) 1)
	      (= (length (intersection ?path2 ?path4)) 0)
	      (= (length (intersection ?path1 ?path3)) 0)
	      ))
   
   ;; test not a self loop
   (compos-in-path ?path-comps1 ?path1)
   (compos-in-path ?path-comps2 ?path2)
   (compos-in-path ?path-comps3 ?path3)
   (compos-in-path ?path-comps4 ?path4)
   
   (test (null (and (our-intersection ?path-comps1 ?path2)
		    (our-intersection ?path-comps1 ?path3)
		    (our-intersection ?path-comps1 ?path4)
		    (our-intersection ?path-comps2 ?path3)
		    (our-intersection ?path-comps2 ?path4)
		    (our-intersection ?path-comps3 ?path4)
		    )))
  
   ;; test paths are connected. Two cases, depending on whether
   ;; path2 or (reverse path2) is needed to make a closed loop
   (test (and (equal (car (last ?path1)) (first ?path2))
	      (equal (car (last ?path2)) (first ?path3))
	      (equal (car (last ?path3)) (first ?path4))
	      (equal (car (last ?path4)) (first ?path1))))
   
   ;; loop-id is sorted branch list
   (bind ?branch-list (list ?br1 ?br2 ?br3 ?br4))
   
   ;; build loop path. Duplicate starting point at end to ensure all
   ;; components have two terminal points in path.
   (bind ?path-comps5 (append ?path-comps1 ?path-comps2))
   (bind ?path-comps6 (append ?path-comps3 ?path-comps4))
   (bind ?loop-path (append ?path1 (subseq ?path2 1) (subseq ?path3 1) (subseq ?path4 1)))
   
   (bind ?reversed 0)	;did not reverse
   )
  :effects 
  (
   (closed-loop ?branch-list ?path-comps5 ?path-comps6 ?loop-path ?reversed)
   ))


(defoperator write-loop-rule-resistors (?branch-list ?t)
  :preconditions (
		  ;;Stop this rule for RC/LRC problems
		  (not (circuit-component ?dontcare capacitor))
		  (not (circuit-component ?dontcare inductor))

		  (in-wm (closed-loop ?branch-list ?p1 ?p2 ?path ?reversed))

		  ;;Make sure ?p2 is a list
		  ;;If ?rev ends up nil then ?p2 was reversed in ?path
		  (bind ?rev (member (second ?p2) (member (first ?p2) ?path :test #'equal) :test #'equal))
		  (bind ?p3 (if (equal ?rev nil) (reverse ?p2) ?p2))
                       
		  ;;get the set of resistors
		  (setof (circuit-component ?comp1 resistor)
			 ?comp1 ?all-res)
		  ;;get the set of batteries
		  (setof (circuit-component ?comp2 battery)
			 ?comp2 ?all-batts)
                      
		  ;;get all the resistor delta variables for ?p1
		  (any-member ?tot (?t nil))
		  (map ?comp (intersection ?p1 ?all-res :test #'equal)
		       (variable ?v-var (voltage-across ?comp :time ?tot))
		       ?v-var ?v-res1-vars)

		  ;;get all the battery delta variables for ?p1
		  (map ?comp (intersection ?p1 ?all-batts :test #'equal)
		       (variable ?v-var (voltage-across ?comp :time ?tot))
		       ?v-var ?v-batt1-vars)

		  ;;get all the resistor delta variables for ?p2
		  (map ?comp (intersection ?p3 ?all-res :test #'equal)
		       (variable ?v-var (voltage-across  ?comp :time ?tot))
		       ?v-var ?v-res2-vars)

		  ;;get all the battery delta variables for ?p2
		  (map ?comp (intersection ?p3 ?all-batts :test #'equal)
		       (variable ?v-var (voltage-across  ?comp :time ?tot))
		       ?v-var ?v-batt2-vars)

		  ;;determine whether ?p1 + ?p2 or ?p1 - ?p2
		  (bind ?sign (if (equal ?reversed 0) '+ '-))
		  (test (or (not (equal ?v-res1-vars nil))
			    (not (equal ?v-res2-vars nil))))
		  (test (or (not (equal ?v-batt1-vars ?v-batt2-vars))
			    (and (equal ?v-batt1-vars nil) (equal ?v-batt2-vars nil))))
		  ;; improve formatting for sums
		  (bind ?vb1-terms (format-plus ?v-batt1-vars))
		  (bind ?vr1-terms (format-plus ?v-res1-vars))
		  (bind ?vb2-terms (format-plus ?v-batt2-vars))
		  (bind ?vr2-terms (format-plus ?v-res2-vars))
		  )
  :effects 
  (
   (eqn (= 0 (?sign (- ?vb1-terms ?vr1-terms) (- ?vb2-terms ?vr2-terms)))
	(loop-rule ?branch-list ?t))
   ;; Due to paths containing composite components, there can be
   ;; multiple versions of loop-rule for a given loop.  Only allow one
   ;; version in a solution.
   (assume using-loop-rule ?branch-list (?p1 ?p2) ?t)
   )
  :hint
  (
   (point (string "Find a closed loop in this circuit and apply Kirchhoff's Loop Rule to it."))
   (point (string "To find the closed loop, pick any point in the circuit and find a path through the circuit that puts you back at the same place."))
   (point (string "You can apply Kirchoff's Loop Rule to the loop containing the components ~A and ~A." (?p1 conjoined-names) (?p2 conjoined-names)))
   (point (string "Once you have identified the closed loop, write an equation that sets the sum of the voltage across each component around the closed circuit loop to zero."))
   (teach (string "The sum of the voltage around any closed circuit loop must be equal to zero.  If you are going in the same direction as the current the voltage across a resistor is negative, otherwise it is positive. If you go across the battery from the negative to the positive terminals, the voltage across the battery is positive, otherwise it is negative."))
   (teach (string "Pick a consistent direction to go around the closed loop. Then write an equation summing the voltage across the battery and the voltages across the resistors, paying attention to whether you are going with or against the current."))
   (bottom-out (string "Write the equation ~a."
		       ((= 0 (?sign (- ?vb1-terms ?vr1-terms)
				    (- ?vb2-terms ?vr2-terms))) algebra) ))
   ))


(defoperator write-single-loop-rule (?branch-list ?t)
  :preconditions 
  (
   (in-wm (closed-loop ?branch-list ?p1 ?p1 ?path ?reversed))
   
   ;;get the set of resistors
   (setof (circuit-component ?comp1 resistor)
	  ?comp1 ?all-res)
   ;;get the set of batteries not switched out at t
   (setof (active-battery ?comp2 ?t)
	  ?comp2 ?all-batts)
   ;;get the set of capacitors
   (setof (circuit-component ?comp3 capacitor)
	  ?comp3 ?all-caps)
   ;;get the set of all inductors
   (setof (circuit-component ?comp4 inductor)
	  ?comp4 ?all-inds)
   
   ;;get all the resistor delta variables for ?p1
   (any-member ?tot (?t nil))
   (map ?comp (intersection ?p1 ?all-res :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-res1-vars)
   
   ;;get all the battery delta variables for ?p1
   (map ?comp (intersection ?p1 ?all-batts :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-batt1-vars)
   
   ;;get all the capacitor delta variables for ?p1
   (map ?comp (intersection ?p1 ?all-caps :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-cap-vars)
   
   ;; get all the inductor delta variables for ?p1
   (map ?comp (intersection ?p1 ?all-inds :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-ind-vars)
   ;; list batteries and inductors for positive sum
   (bind ?emf-vars (append ?v-batt1-vars ?v-ind-vars))
   ;; list capacitors and resistors for negative term
   (bind ?drop-vars (append ?v-res1-vars ?v-cap-vars))
   ;;
   (bind ?emf-terms (format-plus ?emf-vars))
   (bind ?drop-terms (format-plus ?drop-vars))
   )
  :effects 
  (
   (eqn (= 0 (- ?emf-terms ?drop-terms))
	(loop-rule ?branch-list ?t))
   ;; Due to paths containing composite components, there can be
   ;; multiple versions of loop-rule for a given loop.  Only allow one
   ;; version in a solution.
   (assume using-loop-rule ?branch-list ?p1 ?t)
   )
  :hint(
	;;(point (string "Apply Kirchhoff's Loop Rule to the circuit."))
	(point (string "You can apply Kirchoff's Loop Rule to the loop containing ~A." (?p1 conjoined-names)))
	;;(point (string "Write an equation that sets the sum of the voltage across each component around the closed circuit loop to zero."))
	(teach (string "Kirchoff's Loop Rule states that the sum of the voltages around any closed circuit loop must be equal to zero."))
	(teach (string "Pick a consistent direction to go around the closed loop. Then write an equation summing the voltage across the battery and the voltages across the circuit components, paying attention to whether you are going with or against the current."))
	(bottom-out (string "Write the equation ~A"
			    ((= 0 (- ?emf-terms ?drop-terms)) algebra) ))
	))

;; filter to use when fetching batteries for loop rule, because a battery 
;; may be present in problem but switched out as for LC decay.
;; Used only by lr3b.
;; BvdS:  this is certainly the wrong way to do this.  If the 
;;        circuit topology changes with time, then the topology should
;;        be given time dependence
(defoperator get-active-battery (?bat ?t)
  :preconditions (
		  (in-wm (circuit-component ?bat battery))
		  (not (switched-out ?bat ?t))
		  ) :effects ( (active-battery ?bat ?t) ))

(defoperator loop-rule-two-contains (?sought)
  :preconditions (
		  (closed-loop ?compo-list :time ?tt)
		  (any-member ?sought ((voltage-across ?compo :time ?t ?t)))
		  (time ?t)
		  (test (member ?compo ?compo-list :test #'eql))
		  (test (tinsidep-include-endpoints ?t ?tt))
		  )
  :effects ((eqn-contains (loop-rule ?compo-list ?t) ?sought)))

(defoperator write-loop-rule-two (?c1 ?c2 ?t)
  :preconditions (
		  (any-member ?tot (?t nil)) 
		  (variable ?v1 (voltage-across ?c1 :time ?tot))
		  (variable ?v2 (voltage-across ?c2 :time ?tot))
		  )
  :effects ((eqn (= ?v1 ?v2) (loop-rule (?c1 ?c2) ?t)))
  :hint 
  (
   (point (string "The components ~A and ~A are in parallel~@[~A]." 
		  ?c1 ?c2 (?t time))) 
   (teach (string "The voltage across any components in parallel is equal."))
   (bottom-out (string "Write the equation ~A." ((= ?v1 ?v2) algebra)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;               Junction rule
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-psmclass junction-rule  (junction-rule ?junction ?t)  
  :complexity major 
  :short-name "Kirchoff's junction rule"
  :english ("Kirchoff's junction rule")
  :eqnFormat ("Iin = Iout"))

(defoperator junction-rule-contains (?Sought)
  :preconditions 
  (
   ;; note that ?t may end up being timeless
   (any-member ?sought ((current-thru ?branch :time ?t)))
   (branch ?name ?whatever1 ?whatever2 ?path)
   (path-to-branch ?branch ?path)
   (junction ?junction ?names)
   (test (member ?name ?names))
   ;; Exclude cases where a junction is also connected to outside world
   (test (not (member 'unknown ?names)))
   ;; sanity test
   (test (or (equal ?junction (car ?path)) 
	     (equal ?junction (car (last ?path))) 
	     (error "improperly formed junction")))
   )
  :effects ((eqn-contains (junction-rule ?junction ?t) ?sought)))

(defoperator write-junction-rule (?junction ?t)
  :preconditions 
  (
   (junction ?junction ?names)
   (map ?name ?names (branch ?name ?whatever1 ?whatever2 ?path) ?path ?paths)
   (bind ?in-paths (remove ?junction ?paths :key #'car :test #'equal))
   (bind ?out-paths (remove ?junction ?paths 
			    :key #'(lambda (x) (car (last x))) 
			    :test #'equal))
   ;; find the branch names
   (map ?in-path ?in-paths
	(path-to-branch ?in-branch ?in-path) ?in-branch ?in-branches)
   (map ?out-path ?out-paths
	(path-to-branch ?out-branch ?out-path) ?out-branch ?out-branches)
   ;; find the variable names
   (map ?in-branch ?in-branches
	(variable ?in-var (current-thru ?in-branch :time ?t))
		       ?in-var ?in-vars)
   (map ?out-branch ?out-branches
	(variable ?out-var (current-thru ?out-branch :time ?t))
		       ?out-var ?out-vars)
   )
  :effects (
	    (eqn (= (+ . ?in-vars) (+ . ?out-vars))
		 (junction-rule ?junction ?t))
	    )
  :hint(
	(point (string "Apply Kirchhoff's Junction Rule to this circuit."))
	(point (string "Pick a junction. A junction occurs when two or more branches meet."))
	(teach (string "The sum of the currents into a junction must equal the sum of the currents out of the junction."))
	(teach (string "Set the sum of the currents into the junction equal to the sum of the currents out of the junction."))
	(bottom-out (string "Write the equation ~A"
			    ((= (+ . ?in-vars) (+ . ?out-vars)) algebra) ))
	))

;;;EQUIVALENT CAPACITORS
(defoperator define-capacitance-var (?cap)   
  :preconditions (
		  (bind ?c-var (format-sym "C_~A" (body-name ?cap)))
		  )
  :effects (
	    (variable ?c-var (capacitance ?cap))
	    (define-var (capacitance ?cap))
	    )
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting capacitance." 
			     ((capacitance ?cap) def-np)))
	 ))

(def-psmclass equiv-capacitance-series (equiv-capacitance-series ?cap-list) 
  :complexity major
  :short-name "equivalent capacitance series"
  :english ("Equivalent capacitance of series capacitors")
  :eqnFormat ("1/Ceq = 1/C1 + 1/C2 + ...") )

(defoperator equiv-capacitance-series-contains (?sought)
  :preconditions (
		  (any-member ?sought ((capacitance ?cap-list)))
		  (test (listp ?cap-list))
		  )
  :effects(
	   (eqn-contains (equiv-capacitance-series ?cap-list) ?sought)
	   ))

(def-psmclass equiv-capacitance-parallel (equiv-capacitance-parallel ?cap-list) 
  :complexity major
  :short-name "equivalent capacitance parallel"
  :english ("Equivalent capacitance of parallel capacitors")
  :eqnFormat ("Ceq = C1 + C2 + ...") )

(defoperator equiv-capacitance-parallel-contains (?sought)
  :preconditions (
		  (any-member ?sought ((capacitance ?cap-list)))
		  (test (listp ?cap-list))
		  )
  :effects(
	   (eqn-contains (equiv-capacitance-parallel ?cap-list) ?sought)
	   ))


(defoperator equiv-capacitance-series (?cap-list)
  :specifications "doc"
  :preconditions (
		  ;; verify sought list equals some list of series capacitors
		  ;; For simplicity we just put info about series capacitors into givens. 
		  ;; List can include complex equivalents, e.g (series-capacitors (C1 (C2 C3 C4) (C5 C6)))
		  ;; but should only contain one level of nesting, a list of sets of atoms

		  (series-capacitors ?series-list)
		  ;; make sure the ones found equal the sought cap-list
		  (test (null (set-exclusive-or (flatten ?series-list) 
						?cap-list)))
		  (map ?cap ?series-list
		       (variable ?c-var (capacitance ?cap))
		       ?c-var ?c-vars)
		  (map ?cap ?c-vars
		       (bind ?x (list '/ 1 '?cap))
		       ?x ?cap-c-vars)
		  (variable ?tot-cap (capacitance ?cap-list))
		  )
  :effects (
	    ;; (capacitance ?tot-cap)
	    (eqn (= (/ 1 ?tot-cap) (+ .  ?cap-c-vars)) (equiv-capacitance-series ?cap-list))
	    )
  :hint(
	(point (string "You can write an equation for the equivalent capacitance in terms of the individual capacitances that are in series."))
	(point (string "The capacitors that are in series are ~a" (?series-list conjoined-names)))
	(teach (string "The reciprocal of the equivalent capacitance for capacitors in series is equal to the sum of the reciprocals of the individual capacitances."))
	(bottom-out (string "Write the equation ~a"  ((= (/ 1 ?tot-cap) (+ .  ?cap-c-vars)) algebra)))
	))



(defoperator equiv-capacitance-parallel (?cap-list)
  :specifications "doc"
  :preconditions(
		 ;; verify sought list equals some list of parallel capacitors
		 ;; This list may not be able to include complex 
		 ;; equivalents, e.g (parallel-capacitors (C1 (C2 C3 C4) (R5 C6)))
		 ;; but should only contain one level of nesting, a list of sets of atoms
             
		 (parallel-capacitors ?parallel-list) 
		 ;; make sure the ones found equal the sought cap-list
		 (test (null (set-exclusive-or (flatten ?parallel-list) 
					       ?cap-list)))
              
		 ;; pull out terms for each capacitance
		 (map ?cap ?parallel-list
		      (variable ?c-var (capacitance ?cap))
		      ?c-var ?c-vars)
		 (variable ?tot-cap (capacitance ?cap-list))
		 )
  :effects(
	   (capacitance ?tot-cap)                
	   (eqn (= ?tot-cap (+ . ?c-vars)) (equiv-capacitance-parallel ?cap-list))
	   )
  :hint(
	(point (string "You can write an equation for the equivalent capacitance in terms of the individual capacitances that are in parallel."))
	(point (string "The capacitors ~a are in parallel." (?parallel-list conjoined-names)))
	(teach (string "The equivalent capacitance for capacitors in parallel is equal to the sum of the individual capacitances."))
	(bottom-out (string "You need to add the individual capacitances for ~a, and set it equal to the equivalent capacitance ~a" (?parallel-list conjoined-names) (?tot-cap algebra)))  
	))

;; This could be generalized to handle any quantity
(def-psmclass charged-particles (number-of-particles charge ?p ?b) 
  :complexity definition
  :short-name "charged particles"
  :english ("Number of charged particles")
  ;; not the right part of speech for ?p, but the desired behavior
  :ExpFormat ("finding the number of charged ~As in ~A" (nlg ?p 'adj) (nlg ?b))
  :eqnFormat ("Q = N*q"))

(defoperator number-of-particles-contains (?sought)
  :preconditions
  (
   (number-of-particles ?p ?b :quantity ?quant . ?rest)
   (any-member ?sought ((?quant ?p)
			(?quant ?b)
			(number-of ?p)))
   )
  :effects ((eqn-contains (number-of-particles ?quant ?p ?b) ?sought)
	    ))

(defoperator write-number-of-particles (?quant ?b)
  :preconditions
  (
   ;; The number of particles is generally a magnitude
   ;; Put in a minus by hand when the quantities don't have the right signs
   (number-of-particles ?p ?b :quantity ?quant :minus ?flag)
   (variable ?p-var (?quant ?p))
   (variable ?b-var (?quant ?b))
   (variable ?n-var (number-of ?p))
   (bind ?b-term (if ?flag (list '- ?b-var) ?b-var))
   )
  :effects
  (
   (eqn (= (* ?n-var ?p-var) ?b-term) (number-of-particles ?quant ?p ?b))
   )
  :hint
  (
   (point (string "Relate the ~A of ~A to the ~A of ~A." 
	(?quant adj) (?p indef-np) (?quant adj) ?b))
	(teach (string "The total ~A of an object is equal to the number of ~As times the ~A of each ~A." (?quant adj) (?p adj) (?quant adj) (?p adj)))
	(bottom-out (string "Write the equation ~a." 
			    ((= (* ?n-var ?p-var) ?b-term) algebra)))
	))


(def-psmclass capacitance-definition (capacitance-definition ?cap ?t) 
  :complexity definition
  :short-name "capacitance defined"
  :english ("Definition of capacitance")
  :ExpFormat ("applying the definition of capacitance to ~A" (nlg ?cap))
  :eqnFormat ("C = q/V"))

(defoperator capacitor-definition-contains (?sought)
  :preconditions(
		 (any-member ?sought ((charge ?cap :time ?t ?t)
				      (voltage-across ?cap :time ?t ?t)))
		 (time ?t)
		 (circuit-component ?cap capacitor)
		 )
  :effects(
	   (eqn-contains (capacitance-definition ?cap ?t) ?sought)
	   ))

(defoperator capacitor-definition-single-contains (?sought)
  :preconditions(
		 (any-member ?sought ((capacitance ?cap) ))
		 (test (atom ?cap))
		 (time ?t)
		 (circuit-component ?cap capacitor)
		 )
  :effects(
	   (eqn-contains (capacitance-definition ?cap ?t) ?sought)
	   ))

(defoperator write-capacitance-definition (?cap ?t)
  :specifications "doc"
  :preconditions(
		 (variable ?c-var (capacitance ?cap))
		 (any-member ?tot (?t nil)) 
		 (variable ?q-var (charge ?cap :time ?tot))
		 (any-member ?tot2 (?t nil)) 
		 (variable ?v-var (voltage-across ?cap :time ?tot2))
		 )
  :effects(
	   ;; handles zero charge OK
	   (eqn (= (* ?c-var ?v-var) ?q-var) (capacitance-definition ?cap ?t))
	   )
  :hint(
	(point (string "Write an equation for the capacitance of ~a." (?cap adj)))
	(point (string "The capacitance of the capacitor ~a is defined in terms of its charge and the voltage across it." (?cap adj)))
	(teach (string "The capacitance is defined as the charge on the capacitor divided by the voltage across the capacitor."))
	(bottom-out (string "Write the equation defining the capacitance ~a as charge ~a divided by voltage ~a." (?c-var algebra) (?q-var algebra) (?v-var algebra)))
	))


(defoperator write-loop-rule-capacitors (?branch-list ?t)
  :preconditions 
  (
   ;;Stop this rule for LC/LRC problems
   (not (circuit-component ?dontcare inductor))
   
   (in-wm (closed-loop ?branch-list ?p1 ?p2 ?path ?reversed))
   ;;Make sure ?p2 is a list
   ;;If ?rev ends up nil then ?p2 was reversed in ?path
   (bind ?rev (member (second ?p2) (member (first ?p2) ?path :test #'equal) :test #'equal))
   (bind ?p3 (if (equal ?rev nil) (reverse ?p2) ?p2))
   
   ;;get the set of capacitors
   (setof (circuit-component ?comp1 capacitor)
	  ?comp1 ?all-cap)
   ;;get the set of batteries
   (setof (circuit-component ?comp2 battery)
	  ?comp2 ?all-batts)
   
		  ;;get all the capacitor delta variables for ?p1
   (any-member ?tot (?t nil))
   (map ?comp (intersection ?p1 ?all-cap :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-cap1-vars)
   
   ;;get all the battery delta variables for ?p1
   (map ?comp (intersection ?p1 ?all-batts :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
		       ?v-var ?v-batt1-vars)
   
   ;;get all the capacitor delta variables for ?p2
   (map ?comp (intersection ?p3 ?all-cap :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-cap2-vars)
   
   ;;get all the battery delta variables for ?p2
   (map ?comp (intersection ?p3 ?all-batts :test #'equal)
	(variable ?v-var (voltage-across ?comp :time ?tot))
	?v-var ?v-batt2-vars)
   
   ;;determine whether ?p1 + ?p2 or ?p1 - ?p2
   (bind ?sign (if (equal ?reversed 0) '+ '-))
   (test (or (not (equal ?v-cap1-vars nil))
	     (not (equal ?v-cap2-vars nil))))
   (test (or (not (equal ?v-batt1-vars ?v-batt2-vars))
	     (and (equal ?v-batt1-vars nil) (equal ?v-batt2-vars nil))))

   ;; format terms
   (bind ?vb1 (format-plus ?v-batt1-vars))
   (bind ?vc1 (format-plus ?v-cap1-vars))
   (bind ?vb2 (format-plus ?v-batt2-vars))
   (bind ?vc2 (format-plus ?v-cap2-vars))
   )
  :effects (
	    (eqn (= 0 (?sign (- ?vb1 ?vc1) (- ?vb2 ?vc2)))
		 (loop-rule ?branch-list ?t))
	    )
  :hint
  (
   (point (string "Find a closed loop in this circuit and apply Kirchhoff's Loop Rule to it.  To find the closed loop, pick any point in the circuit and find a path around the circuit that puts you back at the same place."))
   (point (string "You can apply Kirchoff's Loop Rule to the loop formed by the branches ~A." (?branch-list conjoined-names)))
   ;;(point (string "Once you have identified the closed loop, write an equation that sets the sum of the voltage across each component around the closed circuit loop to zero."))
   (teach (string "The sum of the voltages around any closed circuit loop must be zero.  Take the side of the capacitor closest to the positive terminal of the battery to be at high potential. If you reach this side of the capacitor first as you go around the loop, subtract the voltage across the capacitor from the battery voltage, otherwise add it."))
	;;(bottom-out (string "Pick a consistent direction to go around the closed loop. Then write an equation summing the voltage across the battery and the voltages across the capacitors, paying attention to whether you should add or subtract the voltage across each capacitor."))
   (bottom-out (string "The loop rule for ~A can be written as ~A" (?branch-list conjoined-names)
		       ((= 0 (?sign (- ?vb1 ?vc1) (- ?vb2 ?vc2))) algebra)  ))
   ))


(def-psmclass junction-rule-cap (junction-rule-cap ?junction ?t)  
  :complexity major 
  :short-name "capacitor junction rule"
  :english ("junction rule for capacitors")
  :eqnFormat ("Qin = Qout"))

(defoperator junction-rule-cap-contains (?Sought)
  :preconditions 
  (
   ;; ?t may end up timeless (nil)
   (any-member ?sought ((charge ?cap :time ?t)))
   (branch ?name ?whatever1 ?whatever2 ?path)
   (test (member ?cap ?path))
   (junction ?junction ?names)
   (test (member ?name ?names))

   ;; Exclude cases where a junction is also connected to outside world
   (test (not (member 'unknown ?names)))

   ;; Test that each branch in the junction contains a capacitor
   (setof (circuit-component ?cc capacitor) ?cc ?all-caps)
   (map ?jname ?names (branch ?jname ?jw1 ?jw2 ?jpath) ?jpath ?paths)
   (test (every #'(lambda (x) (intersection x ?all-caps)) ?paths))
   ;; sanity test
   (test (or (equal ?junction (car ?path)) 
	     (equal ?junction (car (last ?path))) 
	     (error "improperly formed junction")))
   )		  
  :effects ((eqn-contains (junction-rule-cap ?junction ?t) ?sought)
   ))


(defoperator write-junction-rule-cap (?junction ?t )
  :preconditions 
  (
   (junction ?junction ?names)
   (map ?name ?names (branch ?name ?whatever1 ?whatever2 ?path) ?path ?paths)
   ;; find paths coming into/out of junction
   (bind ?in-paths (remove ?junction ?paths :key #'car :test #'equal))
   (bind ?out-paths (remove ?junction ?paths 
			    :key #'(lambda (x) (car (last x))) 
			    :test #'equal))
   ;;find the first/last capacitor in each branch
   (setof (in-wm (circuit-component ?cap capacitor)) ?cap ?all-caps)
   (bind ?in-caps (mapcar #'(lambda (x) (car (intersection x ?all-caps))) 
			  (reverse ?in-paths)))
   (bind ?out-caps (mapcar #'(lambda (x) (car (intersection x ?all-caps))) 
			  ?out-paths))
   ;; define a variable for each capacitor   
   (map ?x ?in-caps (variable ?q-var (charge ?x :time ?t))
	?q-var ?q-in-vars)
   (map ?x ?out-caps (variable ?q-var (charge ?x :time ?t))
	?q-var ?q-out-vars)
   ;; Format the sum
   (bind ?in-term (format-plus ?q-in-vars))
   (bind ?out-term (format-plus ?q-out-vars))
   )
  :effects ((eqn (= ?in-term ?out-term) 
		 (junction-rule-cap ?junction ?t)))
  :hint
  (
   (point (string "What do you know about the charges on capacitors connected to a junction where two or more branches meet?"))
   ;;(point (string "Find the capacitors closest to the junction on both sides."))
   (teach (string "The sum of the charges on one side of a junction must equal the sum of the charges on the other side of the junction."))
   (bottom-out (string "Set the sum of the charges on one side of the junction equal to the sum of the charges on the other side of the junction: Write the equation ~A" 
		       ((= ?in-term ?out-term) algebra)))
	))


(def-psmclass charge-same-caps-in-branch (charge-same-caps-in-branch ?caps ?t) 
  :complexity major
  :short-name "charge on series capacitors"
  :english ("Charge on series capacitors")
  :expformat("Using the fact that series capacitors have the same charge.")
  :eqnFormat ("q1 = q2"))

(defoperator charge-same-caps-in-branch-contains (?sought)
  :preconditions
  (
   ;; ?t may end up timeless (nil)
   (any-member ?sought ((charge ?cap1 :time ?t)
			(charge ?cap2 :time ?t)))
   (branch ?br-res given ?dontcare1 ?path)
   ;; select capacitors
   (circuit-component ?cap1 capacitor)
   (circuit-component ?cap2 capacitor)
   ;; Test capacitors are distinct and ordered on branch
   ;; There is no test that the capacitors are adjacent.
   ;; Might add such a test if the number of solutions blows up
   (test (member ?cap2 (rest (member ?cap1 ?path))))
   )
  :effects
  ( (eqn-contains (charge-same-caps-in-branch (?cap1 ?cap2) ?t) ?sought) ))

(defoperator write-charge-same-caps-in-branch (?cap1 ?cap2 ?t)
  :preconditions 
  (
   (variable ?q1 (charge ?cap1 :time ?t))
   (variable ?q2 (charge ?cap2 :time ?t))
   )
  :effects ( (eqn (= ?q1 ?q2) (charge-same-caps-in-branch (?cap1 ?cap2) ?t)) )
  :hint(
	(point (string "Find two capacitors in series.  Two capacitors are in series when they occur in the same branch."))
	(teach (string "When two capacitors are in series their charges are the same."))
	(bottom-out (string "Set the charge ~a equal to the charge ~a." (?q1 algebra) (?q2 algebra)))
	))

(def-psmclass cap-energy (cap-energy ?cap ?t) 
  :complexity major 
  :short-name "energy stored in a capacitor"
  :english ("The formula for energy stored in a capacitor")
  :expformat("Applying the formula for energy stored in a capacitor to ~A" (nlg ?cap))
  :eqnFormat ("U = 0.5*Q*V"))

(defoperator cap-energy-contains (?sought)
  :preconditions (
		  (any-member ?sought ((charge ?cap :time ?t ?t)
				       (voltage-across ?cap :time ?t ?t)
				       (stored-energy ?cap :time ?t)))
		  (circuit-component ?cap capacitor)
		  (time ?t) ;not always bound
		  (test (time-pointp ?t))
		  ) :effects ( 
		  (eqn-contains (cap-energy ?cap ?t) ?sought) 
		  ))

(defoperator write-cap-energy (?cap ?t)
  :preconditions (
		  (any-member ?tot (?t nil)) 
		  (variable ?Q (charge ?cap :time ?tot))
		  (any-member ?tot2 (?t nil)) 
		  (variable ?V (voltage-across ?cap :time ?tot2))
		  (variable ?U (stored-energy ?cap :time ?t))
		  )
  :effects (
	    (eqn (= ?U (* 0.5 ?Q ?V)) (cap-energy ?cap ?t))
	    )
  :hint (
	 (teach (string "The electric energy stored in a capacitor can be calculated as one half times the charge on the capacitor times the voltage across the capacitor.  This formula can be combined with the definition of capacitance to calculate the energy from other variables."))
	 (bottom-out (string "Write the equation ~A" ((= ?U (* 0.5 ?Q ?V)) algebra)))
	 ))


(defoperator define-stored-energy-var (?b ?t)
  :preconditions 
  (
 (bind ?U-var (format-sym "U_~A~@[_~A~]" (body-name ?b) 
					   (time-abbrev ?t))) ) 
  :effects ( (variable ?U-var (stored-energy ?b :time ?t))
	     (define-var (stored-energy ?b :time ?t)) ) 
  :hint 
  ( (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Stored Energy." 
			((stored-energy ?b :time ?t) def-np)))
    ))

;;; RC CIRCUITS

;; ?quants are ordered, see (def-qexp time-constant ...)
(defoperator define-time-constant (?quants)
  :preconditions 
  ( (bind ?tau-var (format-sym "tau~{_~A~}" (mapcar #'body-name ?quants))) )
  :effects ( (variable ?tau-var (time-constant orderless . ?quants))
	    (define-var (time-constant orderless . ?quants)) )
  :hint 
  ( (bottom-out (string "Define a variable for ~A by using the Add Variable command and selecting Time Constant"  
			((time-constant orderless . ?quants) def-np) ))
    ))

(def-psmclass RC-time-constant (RC-time-constant ?res ?cap) 
  :complexity definition 
  :short-name "RC time constant"
  :english ("RC time constant")
  :eqnFormat ("$t = R*C"))

(defoperator RC-time-constant-contains (?sought)
  :preconditions (
		  (circuit-component ?cap capacitor)
		  (circuit-component ?res resistor)
		  (any-member ?sought ((time-constant orderless ?res ?cap)
				       (capacitance ?cap)
				       (resistance ?res)))
		  )
  :effects (
	    (eqn-contains (RC-time-constant ?res ?cap) ?sought)
	    ))

(defoperator write-RC-time-constant (?res ?cap)
  :preconditions 
  (
   (variable ?tau (time-constant orderless ?res ?cap))
   (variable ?c-var (capacitance ?cap))
   (variable ?r-var (resistance ?res))
   )
  :effects (
	    (eqn (= ?tau (* ?c-var ?r-var)) (RC-time-constant ?res ?cap))
	    )
  :hint (
	 (point (string "You need to define the RC time constant."))
	 (bottom-out (string "Write the equation ~A" ((= ?tau (* ?c-var ?r-var)) algebra)))
	 ))

;;;     time constant for RLC circuit

(def-psmclass RLC-time-constant (RLC-time-constant ?circuit) 
  :complexity definition 
  :short-name "RLC time constant"
  :english ("RLC time constant")
  :eqnFormat ("$t = 2*L/R"))

(defoperator RLC-time-constant-contains (?sought)
  :preconditions 
  (
   (closed-loop (?res ?ind ?cap) :name ?circuit) ;should be cyclic
   (circuit-component ?res resistor)
   (circuit-component ?ind inductor)
   (circuit-component ?cap capacitor)
   (any-member ?sought ((time-constant orderless ?res ?ind ?cap)
			(resistance ?res)
			(self-inductance ?ind)) )
   )
  :effects (
	    (eqn-contains (RLC-time-constant ?circuit) ?sought)
	    ))

(defoperator write-RLC-time-constant (?circuit)
  :preconditions 
  (
   (in-wm (closed-loop (?res ?ind ?cap) :name ?circuit))
   (variable ?tau (time-constant orderless ?res ?ind ?cap))
   (variable ?l-var (self-inductance ?ind))
   (variable ?r-var (resistance ?res))
   )
  :effects (
	    (eqn (= ?tau (/ (* 2 ?l-var) ?r-var)) 
		 (RLC-time-constant ?circuit))
	    )
  :hint 
  (
   (point (string "You need to define the time constant for ~A." ?circuit))
	 (bottom-out (string "Write the equation ~A" 
			     ((= ?tau (/ (* 2 ?l-var) ?r-var)) algebra)))
	 ))

;;;      Angular frequency for LC circuit

(def-psmclass LC-angular-frequency (LC-angular-frequency ?circuit) 
  :complexity definition 
  :short-name "LC angular frequency"
  :english ("the angular frequency of an LC circuit")
  :eqnFormat ("$w = 1/sqrt(L*C)"))

(defoperator LC-angular-frequency-contains (?sought)
  :preconditions 
  (
   (closed-loop (?ind ?cap) :name ?circuit)  ;should be made cyclic
   (circuit-component ?cap capacitor)
   (circuit-component ?ind inductor)
   (any-member ?sought ((angular-frequency ?circuit)
			(capacitance ?cap)
			(self-inductance ?ind)))
   )
  :effects (
	    (eqn-contains (LC-angular-frequency ?circuit) ?sought)
	    ))

(defoperator write-LC-angular-frequency (?circuit)
  :preconditions 
  (
   (in-wm (closed-loop (?ind ?cap) :name ?circuit))  ;should be made cyclic
   (variable ?omega (angular-frequency ?circuit))
   (variable ?c-var (capacitance ?cap))
   (variable ?l-var (self-inductance ?ind))
   )
  :effects (
	    (eqn (= 1 ( * (^ ?omega 2) ?c-var ?l-var))
		 (LC-angular-frequency ?circuit))
	    )
  :hint 
  (
   (point (string "What is the formula for the frequency of oscillations of ~A?" 
 ?circuit))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?omega (/ 1 (sqrt (* ?c-var ?l-var)))) algebra)))
   ))

;;;      Angular frequency for RLC circuit

(def-psmclass RLC-angular-frequency (RLC-angular-frequency ?circuit) 
  :complexity definition 
  :short-name "RLC angular frequency"
  :english ("the angular frequency of an RLC circuit")
  :eqnFormat ("$w = sqrt(1/(L*C)-R^2/(2*L)^2)"))

(defoperator RLC-angular-frequency-contains (?sought)
  :preconditions 
  (
   (closed-loop (?res ?ind ?cap) :name ?circuit) ;should be cyclic
   (circuit-component ?res resistor)
   (circuit-component ?cap capacitor)
   (circuit-component ?ind inductor)
   (any-member ?sought ((angular-frequency ?circuit)
			(capacitance ?cap)
			(resistance ?res)
			(self-inductance ?ind)))
   )
  :effects (
	    (eqn-contains (RLC-angular-frequency ?circuit) ?sought)
	    ))

(defoperator write-RLC-angular-frequency (?circuit)
  :preconditions 
  (
   (in-wm (closed-loop (?res ?ind ?cap) :name ?circuit))
   (variable ?omega (angular-frequency ?circuit))
   (variable ?r-var (resistance ?res))
   (variable ?l-var (self-inductance ?ind))
   (variable ?c-var (capacitance ?cap))
   )
  :effects (
	    (eqn (= (^ ?omega 2) (- (/ 1 (* ?c-var ?l-var)) 
				    (^ (/ ?r-var (* 2.0 ?l-var)) 2)))
		 (RLC-angular-frequency ?circuit))
	    )
  :hint 
  (
   (point (string "Use the formula for the frequency of an RLC circuit."))
   (bottom-out (string "Write the equation ~A" 
		       ((= ?omega (sqrt (- (/ 1 (* ?c-var ?l-var)) 
					   (^ (/ ?r-var (* 2.0 ?l-var)) 2)))) 
			algebra)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  circuit with resistor and capacitor in series.
;;;

(def-psmclass discharging-capacitor-at-time (discharging-capacitor-at-time 
					     ?components ?time) 
  :complexity major
  :short-name "charge on capacitor in RC circuit"
  :english ("the charge on capacitor in RC circuit, initially full")
  :eqnFormat ("q = qi*exp(-t/$t)"))

(defoperator discharging-capacitor-at-time-contains (?sought)
  :preconditions
  (
   ;; should generalize to cyclic permutations of loop 
   ;; and time intervals containing ?t1 ?t2
   (closed-loop (?res ?cap) :time (during ?t1 ?t2))
   (circuit-component ?res resistor)
   (circuit-component ?cap capacitor)
   (any-member ?sought ((charge ?cap :time ?t1)
			(charge ?cap :time ?t2)
			(duration (during ?t1 ?t2))
			(time-constant orderless ?res ?cap)
			))
   ;; make sure we have a time interval:
   (time (during ?t1 ?t2))
   )
  :effects(
	   (eqn-contains (discharging-capacitor-at-time (?res ?cap) (during ?t1 ?t2)) ?sought)
	   ))

(defoperator discharging-capacitor-at-time (?res ?cap ?t1 ?t2)
  :preconditions 
  (
   (variable ?q1-var (charge ?cap :time ?t1))
   (variable ?q2-var (charge ?cap :time ?t2))
   (variable ?c-var (capacitance ?cap))
   (variable ?t-var (duration (during ?t1 ?t2)))
   (variable ?tau-var (time-constant orderless ?res ?cap))
   )
  :effects 
  ((eqn (= ?q2-var (* ?q1-var (exp (/ (- ?t-var) ?tau-var))))
	(discharging-capacitor-at-time (?res ?cap) (during ?t1 ?t2))))
  :hint
  (
   (point (string "Write the equation for the charge on the capacitor ~a at time ~a." ?cap (?t2 time)))
   (bottom-out (string "Write the equation ~a"
		       ((= ?q2-var (* ?q1-var (exp (/ (- ?t-var) ?tau-var)))) algebra) ))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  circuit with battery, resistor, and capacitor in series.
;;;

(def-psmclass charging-capacitor-at-time (charging-capacitor-at-time 
					  ?components ?time) 
  :complexity major
  :short-name "charge on capacitor in RC circuit with battery"
  :english ("the charge on capacitor in RC circuit, initially empty")
  :eqnFormat ("q = C*Vb*(1 - exp(-t/$t))"))

(defoperator charging-capacitor-at-time-contains (?sought)
  :preconditions
  (
   ;; in principle, should matching under cyclic permutations
   ;; maybe do this by an extension to unify
   ;; also, could generalize the time to contain ?t1 and ?t2
   (closed-loop (?bat ?res ?cap) :time (during ?t1 ?t2))
   (circuit-component ?bat battery)
   (circuit-component ?res resistor)
   (circuit-component ?cap capacitor)
   (any-member ?sought ((charge ?cap :time ?t2)
			(duration (during ?t1 ?t2))
			(time-constant orderless ?res ?cap)
			))
   (time (during ?t1 ?t2))		;sanity test
   (given (charge ?cap :time ?t1) 0) ;boundary condition
   )
  :effects(
	   (eqn-contains (charging-capacitor-at-time (?bat ?res ?cap) (during ?t1 ?t2)) ?sought)
	   ))

(defoperator charging-capacitor-at-time (?bat ?res ?cap ?t1 ?t2)
  :preconditions 
  (
   (variable ?q-var (charge ?cap :time ?t2))
   (variable ?c-var (capacitance ?cap))
   (variable ?v-var (voltage-across ?bat :time (during ?t1 ?t2)))
   (variable ?t-var (duration (during ?t1 ?t2)))
   (variable ?tau-var (time-constant orderless ?res ?cap))
   )
  :effects 
  ((eqn (= ?q-var (* ?c-var ?v-var (- 1 (exp (/ (- ?t-var) ?tau-var)))))
	(charging-capacitor-at-time (?bat ?res ?cap) (during ?t1 ?t2))))
  :hint
  (
   (point (string "Write the equation for the charge on the capacitor ~a at time ~a." ?cap (?t2 time)))
   (bottom-out (string "Write the equation ~a"
		       ((= ?q-var (* ?c-var ?v-var (- 1 (exp (/ (- ?t-var) ?tau-var))))) algebra) ))
   ))

;;;
;;;  BvdS:  Why is this a separate law????
;;;

(def-psmclass current-in-RC-at-time (current-in-RC-at-time ?components ?time) 
  :complexity major
  :short-name "current in RC circuit"
  :english ("Current in RC circuit")
  :eqnFormat ("I = (Vb/R)*exp(-t/$t)"))

(defoperator current-in-RC-at-time-contains (?sought)
  :preconditions
  (
   ;; in principle, should test for matching under cyclic permutations
   ;; also, could generalize the time to contain ?t1 and ?t2
   (closed-loop (?bat ?res ?cap) :time (during ?t1 ?t2))
   (circuit-component ?bat battery)
   (circuit-component ?cap capacitor)
   (circuit-component ?res resistor)
   (any-member ?sought ((current-thru ?res :time ?t2)
			;; also contains V, R, C, t
			(time-constant orderless ?res ?cap)
			))
   (time (during ?t1 ?t2))		;sanity test
   (given (charge ?cap :time ?t1) 0) ;boundary condition
   )
  :effects
  ((eqn-contains (current-in-RC-at-time (?bat ?res ?cap) (during ?t1 ?t2)) ?sought)
   ))


(defoperator write-current-in-RC-at-time (?bat ?res ?cap ?t1 ?t2)
  :preconditions 
  (
   (variable ?i-var (current-thru ?res :time ?t2))
   (variable ?v-var (voltage-across ?bat :time (during ?t1 ?t2)))
   (variable ?r-var (resistance ?res))
   (variable ?t-var (duration (during ?t1 ?t2)))
   (variable ?tau-var (time-constant orderless ?res ?cap))
   )
  :effects 
  ((eqn (= ?i-var (* (/ ?v-var ?r-var) (exp (/ (- ?t-var) ?tau-var))))
	(current-in-RC-at-time (?bat ?res ?cap) (during ?t1 ?t2)))  
   )
  :hint
  (
   (point (string "Write the equation for the current in the RC circuit at time ~a." (?t2 time)))
   (bottom-out (string "Write the equation ~a"
		       ((= ?i-var (* (/ ?v-var ?r-var) (exp (/ (- ?t-var) ?tau-var)))) algebra) ))
   ))

;;;
;;;  BvdS:  This could be rewritten in terms of (fraction-of ....)
;;;

(def-psmclass charge-capacitor-percent-max (charge-capacitor-percent-max ?cap . ?times) 
  :complexity minor 
  :short-name "RC charge as fraction of max"
  :english ("the RC circuit charge as percent of maximum")
  :eqnFormat ("q = fraction*C*Vb"))

(defoperator charge-capacitor-percent-max-contains (?sought)
  :preconditions(
		 (any-member ?sought ((charge ?cap :time ?t2)
				      ;;(max-charge ?cap :time inf)
				      ;; also contains C and V
				      ))
		 (percent-max ?cap ?value ?t2)
		 (circuit-component ?cap capacitor)
		 (given (charge ?cap :time ?t1) 0) ;boundary condition
		 )
  :effects(
	   (eqn-contains (charge-capacitor-percent-max ?cap ?t1 ?t2) ?sought)
	   ))


(defoperator charge-capacitor-percent-max (?cap ?t)
  :preconditions (
		  (variable ?C-var (capacitance ?cap))
					; use constant Vb defined across charging interval.
		  (circuit-component ?bat battery)	 
		  (variable ?V-var (voltage-across ?bat :time (during ?t0 ?t)))
		  (variable ?q-var (charge ?cap :time ?t))
		  (percent-max ?cap ?fraction ?t)
		  )
  :effects (
	    (eqn (= ?q-var (* ?fraction ?C-var ?V-var))
		 (charge-capacitor-percent-max ?cap ?t0 ?t))  
	    )
  :hint(
	(point (string "Write the equation for the charge on the capacitor ~a ~a in terms of the percentage of the maximum charge." ?cap (?t pp)))
	(point (string "Use the definition of capacitance, remembering that when the charge on the capacitor is at its maximum, the voltage across the capacitor ~a equals the voltage across the battery ~a." ?cap ?bat))
	(teach (string "The maximum charge on a capacitor equals the capacitance times the battery voltage. You can express the charge at ~a as a fraction of this quantity." (?t pp)))
	(bottom-out (string "Write the equation ~A"  
			    ((= ?q-var (* ?fraction ?C-var ?V-var)) algebra) ))
	))

;;;;---------------------------------------------------------------------------
;;;;
;;;;                               Inductance
;;;;
;;;;---------------------------------------------------------------------------

;; define inductance var
(defoperator define-self-inductance-var (?ind)   
  :preconditions ((circuit-component ?ind inductor)
		  (bind ?L-var (format-sym "L_~A" (body-name ?ind))))
  :effects ((variable ?L-var (self-inductance ?ind))
	    (define-var (self-inductance ?ind)))
  :hint 
  ((bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting inductance." 
		       ((self-inductance ?ind) def-np)))))

;; define mutual inductance var
(defoperator define-mutual-inductance-var (?inds)   
  :preconditions 
  ( (bind ?L-var (format-sym "M_~{_~A~}" (mapcar #'body-name ?inds))) )
  :effects ( (variable ?L-var (mutual-inductance orderless . ?inds))
	    (define-var (mutual-inductance orderless . ?inds)) )
  :hint 
  ( (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting mutual inductance." 
			((mutual-inductance orderless . ?inds) def-np))) ))

;;;              Magnetic field inside a long solenoid

(def-psmclass solenoid-self-inductance (solenoid-self-inductance ?solenoid)
  :complexity major
  :short-name "self-inductance of long, uniform solenoid"
  :english ("the self-inductance of a long, uniform solenoid")
  :ExpFormat ("finding the self-inductance of ~A" (nlg ?solenoid))
  :EqnFormat ("L = $m0*N^2*A/l" ))

(defoperator solenoid-self-inductance-contains (?sought)
  :preconditions 
  (
   (inside-solenoid ?inside ?solenoid)  ;given that there is a solenoid
   (vacuum ?inside)  ;air-core solenoid
   (any-member ?sought (
			(length ?solenoid)
			(turns ?solenoid)
			(area ?solenoid)
			(self-inductance ?solenoid)
			))
   )
  :effects ((eqn-contains (solenoid-self-inductance ?solenoid) ?sought)))

(defoperator write-solenoid-self-inductance (?solenoid)
  :preconditions 
  ( 
   (variable ?length (length ?solenoid))
   (variable ?N (turns ?solenoid))
   (variable ?A (area ?solenoid))
   (variable ?L (self-inductance ?solenoid))
   )
  :effects ( 
	    (eqn (= (* ?L ?length) (* |mu0| ?N ?N ?A))
		 (solenoid-self-inductance ?solenoid))
	    )
  :hint 
  (
   (point (string "What is the self-inductance of ~A?" ?solenoid))
   (teach (string "Find the formula for the self-inductance of a long, uniformly wound, solenoid."))
   (bottom-out (string "Write the equation ~A"  
		       ((= (* ?L ?length) (* |mu0| (^ ?N 2) ?A)) algebra) ))
   ))

;; Generic wrapper to define the derivative of a generic quantity
;; Partially because these quantities correspond to distinct variable
;; choices in the user interface, a unique def-qexp will be needed for
;; each quantity.
;;
;; The hint is a bit weak because KB has no information about 
;; the menu item name given in scalars.tsv

(defoperator define-rate-of-change-var (?quant)
  :preconditions 
  (
   ;; getting the base variable name means that student has
   ;; to also define the base variable
   ;;(variable ?var ?quant)
   ;;
   ;; Since we define derivatives explicitly on the Workbench, we need
   ;; to test to make sure this quantity is in Ontology.
   (test (lookup-expression-struct `(rate-of-change ,?quant)))
   ;;
   ;; make unique variable name:  not too pretty, but it works
   (bind ?change-var (format-sym "d~A_~A_dt~@[_~a~]"  (first ?quant) 
				 (sxhash (rest (remove-time ?quant)))
				 (time-abbrev (time-of ?quant))))
   )
  :effects ((variable ?change-var (rate-of-change ?quant))
	    (define-var (rate-of-change ?quant)))
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu." 
			     ((rate-of-change ?quant) var-or-quant) ))
	 ))

;;; Generic definition of average rate of change of some quantity

(def-psmclass average-rate-of-change (average-rate-of-change ?quant)
  ;; This is actually an important equation, but it is often shown
  ;; combined with other equations.
  :complexity definition 
  :short-name "average rate of change"
  :english ("the average rate of change of quantity")
  :expformat ("applying definition of average rate of change to ~A" 
	      (nlg ?quant)) 
  :EqnFormat ("dy/dt_avg = (y2-y1)/(t2-t1)"))

;; This is rather incomplete:  it should also allow the time interval
;; or the end values to be the sought
(defoperator average-rate-of-change-contains (?sought)
  :preconditions 
  (
   (time ?t)
   (test (time-intervalp ?t))
   (any-member ?sought ((rate-of-change ?quant)))
   (test (equal ?t (time-of ?quant)))
   )
  :effects ((eqn-contains (average-rate-of-change ?quant) ?sought)
  ))

(defoperator average-rate-of-change-contains2 (?sought)
  :preconditions 
  (
   (time ?t)
   (test (time-intervalp ?t))
   (any-member ?sought (?q-end))
   (test (tendpointp (time-of ?q-end) ?t))
   (bind ?quant (set-time ?q-end ?t))
   )
  :effects ((eqn-contains (average-rate-of-change ?quant) ?sought)
	    ))

;;  Backwards chaining fails spectacularly when the 
;;  sought is the duration.  Here we make use of the fact that the 
;;  a source of (rate-of-change ...) is the inductor rule.
(defoperator average-rate-of-change-contains-inductor-current (?sought)
  :preconditions 
  (
   (any-member ?sought ((duration ?tt)))
   (circuit-component ?compo inductor)
   (compo-or-branch ?compo ?branch)
   (any-member ?quant ((current-thru ?branch :time ?tt)))
   )
  :effects ((eqn-contains (average-rate-of-change ?quant) ?sought)))

(defoperator write-average-rate-of-change (?quant)
 :preconditions 
 (
  (bind ?t (time-of ?quant))
  (bind ?q1 (set-time ?quant (second ?t)))
  (bind ?q2 (set-time ?quant (third ?t)))
  (variable ?qavg (rate-of-change ?quant))
  (variable ?q1-var ?q1)
  (variable ?q2-var ?q2)
  (variable ?dt (duration ?t))
 )
 :effects 
 ( (eqn (= ?qavg (/ (- ?q2-var ?q1-var) ?dt)) 
	(average-rate-of-change ?quant)) )
 :hint (
	(point (string "Find the average value of ~A." 
		        ((rate-of-change ?quant) def-np)))
	(teach (string "The average rate of change is the change in value divided by the change in time."))
 	(bottom-out (string "Write the equation ~A."  
			    ((= ?qavg (/ (- ?q2-var ?q1-var) ?dt))  algebra)))
	))


;; voltage across an inductor V = -L*dI/dt
(def-psmclass inductor-emf (inductor-emf ?inductor ?time) 
  :complexity major
  :short-name "inductor EMF"
  :english ("EMF (voltage) across inductor")
  :ExpFormat ("finding the EMF across inductor ~A ~A" 
	      (nlg ?inductor) (nlg ?time 'pp))
  :eqnFormat ("V = -L*dIdt") 
  )

(defoperator inductor-emf-contains (?sought)
  :preconditions 
  (
   (circuit-component ?ind inductor)
   (any-member ?sought ( (voltage-across ?ind :time ?time)
			 (self-inductance ?ind)
			 (rate-of-change (current-thru ?branch :time ?time)) ))
   (compo-or-branch ?ind ?branch)
   (time ?time)
   )
  :effects ( (eqn-contains (inductor-emf ?ind ?time) ?sought) ))

(defoperator inductor-emf (?ind ?time)
  :preconditions 
  (
   (variable ?V (voltage-across ?ind :time ?time))
   (variable ?L (self-inductance ?ind))
   (compo-or-branch ?ind ?branch)
   (variable ?dIdt (rate-of-change (current-thru ?branch :time ?time)))
   )
  :effects (
	    (eqn (= ?V (- (* ?L ?dIdt))) (inductor-emf ?ind ?time))
	    )
  :hint (
	 (point (string "The voltage across the ends of an inductor is related to the inductance and the rate at which the current through it is changing"))
	 (teach (string "The EMF (voltage) produced between the ends of an inductor is proportional to its inductance and the instantaneous time rate of change of the current.  The voltage is conventionally shown as negative for increasing positive current to indicate that the induced EMF opposes the change."))
	 (bottom-out (string "Write the equation ~A" ((= ?V ( - (* ?L ?dIdt))) algebra) ))
	 ))

;; Mutual inductance version of inductor-emf

(def-psmclass mutual-inductor-emf (mutual-inductor-emf ?ind1 ?ind2 ?time) 
  :complexity major
  :short-name "mutual inductor EMF"
  :english ("induced EMF (voltage) across ~A due to ~A" 
	    (nlg ?ind1) (nlg ?ind2))
  :eqnFormat ("V2 = -M12*dI1dt") 
  )

(defoperator mutual-inductor-emf-contains (?sought)
  :preconditions 
  (
   (mutual-inductor orderless . ?inds)
   ;; can use either order for the two coils
   (any-member ?inds ((?ind1 ?ind2) (?ind2 ?ind1)))
   (any-member ?sought ((mutual-inductance orderless . ?inds) 
			(voltage-across ?ind1 :time ?time)
			(rate-of-change (current-thru ?ind2 :time ?time)) ))
   (time ?time)
   )
  :effects ( (eqn-contains (mutual-inductor-emf ?ind1 ?ind2 ?time) ?sought) ))

(defoperator write-mutual-inductor-emf (?ind1 ?ind2 ?time)
  :preconditions 
  (
   (variable ?V (voltage-across ?ind1 :time ?time))
   (variable ?M (mutual-inductance orderless ?ind1 ?ind2))
   (variable ?dIdt (rate-of-change (current-thru ?ind2 :time ?time)))
		  )
  :effects (
	    (eqn (= ?V (- (* ?M ?dIdt))) 
		 (mutual-inductor-emf ?ind1 ?ind2 ?time))
	    )
  :hint 
  (
   (point (string "The voltage across ~A is related to the change in the current through ~A" ?ind1 ?ind2))
   (teach (string "The EMF (voltage) generated in a coil due to changing current in a second coil is given by the mutual inductance of the two coils times the instantaneous rate of current change in the second coil.  The voltage is conventionally shown as negative for increasing positive current to indicate that the induced EMF opposes the change."))
   (bottom-out (string "Write the equation ~A" ((= ?V (- (* ?M ?dIdt))) algebra) ))
   ))


;; energy stored in an inductor
(def-psmclass inductor-energy (inductor-energy ?ind ?t) 
  :complexity major 
  :short-name "energy stored in inductor"
  :english ("the formula for energy stored in a inductor")
  :expformat("Applying the formula for energy stored in a inductor to ~A" (nlg ?ind))
  :eqnFormat ("U = 0.5*L*I^2"))

(defoperator inductor-energy-contains (?sought)
  :preconditions 
  (
   (any-member ?sought ( (self-inductance ?inductor)
			 (current-thru ?branch :time ?t ?t)
			 (stored-energy ?inductor :time ?t)))
   (time ?t)
   (test (time-pointp ?t))
   (circuit-component ?inductor inductor)
   (compo-or-branch ?inductor ?branch)
   ) 
  :effects ( 
	    (eqn-contains (inductor-energy ?inductor ?t) ?sought) 
	    ))

(defoperator write-inductor-energy (?inductor ?t)
  :preconditions (
		  (variable ?U (stored-energy ?inductor :time ?t))
		  (variable ?L (self-inductance ?inductor))
		  (any-member ?tot (?t nil))
		  (compo-or-branch ?inductor ?branch)
		  (variable ?I (current-thru ?branch :time ?tot))
		  )
  :effects (
	    (eqn (= ?U (* 0.5 ?L (^ ?I 2))) (inductor-energy ?inductor ?t))
	    )
  :hint (
	 (teach (string "The electric energy stored in the magnetic field of an a inductor can be calculated as one half times the inductance times the square of the current. ")) 
	 (bottom-out (string "Write the equation ~A" ((= ?U (* 0.5 ?L (^ ?I 2)) algebra)) ))
	 ))

;;
;; LR circuits
;;

;;; Some formula use tau for the LR-time constant L/R. 

(def-psmclass LR-time-constant (LR-time-constant ?ind ?res) 
  :complexity definition 
  :short-name "LR time constant"
  :english ("the LR circuit time constant")
  :eqnFormat ("$t = L/R"))


(defoperator LR-time-constant-contains (?sought ?ind ?res)
  :preconditions (
		  (circuit-component ?ind inductor)
		  (circuit-component ?res resistor)
		  (any-member ?sought ((time-constant orderless ?ind ?res)
				       (self-inductance ?ind)
				       (resistance ?res)))
		  )
  :effects (
	    (eqn-contains (LR-time-constant ?ind ?res) ?sought)
	    ))


(defoperator write-LR-time-constant (?ind ?res)
  :preconditions (
		  (variable ?tau (time-constant orderless ?ind ?res))
		  (variable ?L-var (self-inductance ?ind))
		  (variable ?r-var (resistance ?res))
		  )
  :effects (
	    (eqn (= ?tau (/ ?L-var ?R-var)) (LR-time-constant ?ind ?res))
	    )
  :hint (
	 (point (string "The inductive time constant $t of an LR circuit is a function of the inductance and the resistance in the circuit."))
	 (teach (string "The inductive time constant of an LR circuit is equal to the inductance divided by the resistance."))
	 (bottom-out (string "Write the equation ~A" ((= ?tau (/ ?L-var ?R-var)) algebra)))
	 ))


(def-psmclass LR-current-growth (LR-current-growth ?ind ?res ?branch . ?times) 
  :complexity major
  :short-name "LR current growth"
  :english ("current growth in an LR circuit")
  :eqnFormat ("I = Imax*(1 - exp(-t/$t))"))

(defoperator LR-current-growth-contains (?sought)
  :preconditions
  (
   ;; Following in the givens tells us that circuit and switch
   ;; are configured so current grows during this interval.
   (LR-current-growth ?name (during ?t1 ?tf))
   (branch ?name ?whatever1 ?whatever2 ?path)
   (path-to-branch ?branch ?path)
   (circuit-component ?res resistor)
   (circuit-component ?ind inductor)
   (test (member ?res ?branch))
   (test (member ?ind ?branch))
   (any-member ?sought ((current-thru ?branch :time ?t2)
			(duration (during ?t1 ?t2))
			(time-constant orderless ?ind ?res)))
   ;; this applies to any t2 between t1 and tf
   (time ?t2)	       ; have to bind if sought is tau
   (test (time-pointp ?t2))
   (test (< ?t2 ?tf))
   )
  :effects
  (
   (eqn-contains (LR-current-growth ?ind ?res ?branch ?t1 ?t2 ?tf) ?sought)
   ))

(defoperator LR-current-growth (?ind ?res ?t1 ?t2 ?tf)
  :preconditions 
  (
   (variable ?i-var (current-thru ?branch :time ?t2))
   (variable ?Imax-var (current-thru ?branch :time ?tf))
   (variable ?t-var (duration (during ?t1 ?t2)))
   (variable ?tau-var (time-constant orderless ?ind ?res))
   )
  :effects (
	    (eqn (= ?i-var (* ?Imax-var (- 1 (exp (/ (- ?t-var) ?tau-var)))))
		 (LR-current-growth ?ind ?res ?branch ?t1 ?t2 ?tf))  
	    )
  :hint(
	(point (string "After the battery is switched in, the current in an LR circuit rises towards its maximum value as an exponential function of time"))
	(teach (string "The rising current in an LR circuit at a time equals the maximum current multiplied by a factor of 1 less a decreasing exponential term. The exponential term is given by e raised to a negative exponent (so this term goes to zero over time) of the time over the inductive time constant, tau. In ANDES you express e raised to the x power by the function exp(x)."))
	(bottom-out (string "Write the equation ~a"
			    ((= ?i-var (* ?Imax-var (- 1 (exp (/ (- ?t-var) ?tau-var))))) algebra) ))
	))

;; Formula Imax = Vb/R is true for both LR growth and decay, 
;; but we treat as two psms because we show different variables in 
;; the two cases, "If" for growth vs. "I0" for decay.
;; Whether it is best to treat as two psms or one w/two ways of writing 
;; the equation depends mainly on how we want our review page to look.

(def-psmclass LR-growth-Imax (LR-growth-Imax ?res ?bat ?branch ?time)
  :complexity major 
  :short-name "LR growth final current"
  :english ("LR circuit growth final current")
  :eqnFormat ("Imax = Vb/R"))

(defoperator LR-growth-Imax-contains (?sought)
  :preconditions 
  (
   ;; have to be told we have LR-current growth over interval
   (LR-current-growth ?name (during ?ti ?tf))
   (branch ?name ?whatever1 ?whatever2 ?path)
   (path-to-branch ?branch ?path)
   (any-member ?sought ( (current-thru ?branch :time ?tf)
			 (voltage-across ?bat :time (during ?ti ?tf))
			 (resistance ?res) ))
   (circuit-component ?res resistor)
   (circuit-component ?bat battery)
   (test (member ?res ?branch))
   (test (member ?bat ?branch))
   )
  :effects 
  (  (eqn-contains (LR-growth-Imax ?res ?bat ?branch 
				   (during ?ti ?tf)) ?sought) ))

(defoperator LR-growth-Imax (?branch ?ti ?tf)
  :preconditions (
		  (variable ?Imax-var (current-thru ?branch :time ?tf))
		  (variable ?v-var (voltage-across ?bat :time (during ?ti ?tf)))
		  (variable ?r-var (resistance ?res))
		  )
  :effects ( (eqn (= ?Imax-var (/ ?v-var ?r-var)) 
		  (LR-growth-Imax ?res ?bat ?branch (during ?ti ?tf))) )
  :hint (
	 (point (string "What must the maximum value of the current be?"))
	 (point (string "At its maximum value, the current in an LR circuit is nearly constant, so there is no EMF due to the inductor. Since the only source of EMF at this time is the battery, Ohm's Law V = I*R determines the current through the resistor to be the battery voltage divided by resistance."))
	 (bottom-out (string "Write the equation ~a" ((= ?Imax-var (/ ?v-var ?r-var)) algebra)))
	 ))

;; LR circuit decay:
(def-psmclass LR-current-decay (LR-current-decay ?res ?ind ?branch ?time) 
  :complexity major
  :short-name "LR current decay"
  :english ("current decay in an LR circuit")
  :eqnFormat ("I = I0*exp(-t/$t)"))

(defoperator LR-current-decay-contains (?sought)
  :preconditions
  (
   ;; Following in the givens tells us that circuit and switch
   ;; are configured so current decays during this interval.
   (LR-current-decay ?name (during ?t1 ?tf))
   (branch ?name ?whatever1 ?whatever2 ?path)
   (path-to-branch ?branch ?path)
   (circuit-component ?res resistor)
   (circuit-component ?ind inductor)
   (test (member ?res ?branch))
   (test (member ?ind ?branch))
   (any-member ?sought ((current-thru ?branch :time ?t2)
			(duration (during ?t1 ?t2))
			(time-constant orderless ?res ?ind)
			;; also contains I0
			))
   ;; this applies to any t2 between t1 and tf
   (test (time-pointp ?t2))
   (test (<= ?t2 ?tf))
   )
  :effects
  ((eqn-contains (LR-current-decay ?res ?ind ?branch (during ?t1 ?t2)) ?sought)
   ))

(defoperator write-LR-current-decay (?ind ?res ?t1 ?t2)
  :preconditions 
  (
   (variable ?i-var (current-thru ?branch :time ?t2))
   (variable ?I0-var (current-thru ?branch :time ?t1))
   (variable ?t-var (duration (during ?t1 ?t2))) 
   (variable ?tau-var (time-constant orderless ?ind ?res))
   )
  :effects (
	    (eqn (= ?i-var (* ?I0-var (exp (/ (- ?t-var) ?tau-var))))
		 (LR-current-decay ?ind ?res ?branch (during ?t1 ?t2)))  
	    )
  :hint(
	(point (string "When the battery is switched out, the initial current in an LR circuit decays towards zero as an exponential function of time"))
	(teach (string "The decaying current in an LR circuit at a time equals the initial current multipled by a factor of a decreasing exponential term. The exponential term is given by e raised to a negative exponent (so this term goes to zero over time) of the time over the inductive time constant, tau. In ANDES you express e raised to the x power by the function exp(x)."))
	(bottom-out (string "Write the equation ~a"
			    ((= ?i-var (* ?I0-var (exp (/ (- ?t-var) ?tau-var)))) algebra) ))
	))

(def-psmclass LR-decay-Imax (LR-decay-Imax ?res ?bat ?branch ?time)
  :complexity major 
  :short-name "LR circuit initial current"
  :english ("LR circuit initial current")
  :eqnFormat ("I0 = Vb/R"))

(defoperator LR-decay-Imax-contains (?sought)
  :preconditions 
  (
   ;; have to be told we have LR-current decay over interval
   (LR-current-decay ?name (during ?ti ?tf))
   (branch ?name ?whatever1 ?whatever2 ?path)
   (path-to-branch ?branch ?path)
   (any-member ?sought ( (current-thru ?branch :time ?ti)
			 (voltage-across ?bat :time (during ?ti ?tf))
			 (resistance ?res) ))
   (circuit-component ?res resistor)
   (circuit-component ?bat battery)
   (test (member ?res ?branch))
   (test (member ?bat ?branch))
   )
  :effects 
  ((eqn-contains (LR-decay-Imax ?res ?bat ?branch (during ?ti ?tf)) ?sought) ))

(defoperator LR-decay-Imax (?branch ?ti ?tf)
  :preconditions (
		  (variable ?Imax-var (current-thru ?branch :time ?ti))
		  (variable ?v-var (voltage-across ?bat :time (during ?ti ?tf)))
		  (variable ?r-var (resistance ?res))
		  )
  :effects 
  ( (eqn (= ?Imax-var (/ ?v-var ?r-var)) 
	 (LR-decay-Imax ?res ?bat ?branch (during ?ti ?tf))) )
  :hint (
	 (point (string "What is the initial value of the current when the switch is opened?"))
	 (point (string "At its maximum value, the current in an LR circuit is nearly constant, so there is no EMF due to the inductor. Since the only source of EMF at this time is the battery, Ohm's Law V = I*R determines the current through the resistor to be the battery voltage divided by resistance."))
	 (bottom-out (string "Write the equation ~a" ((= ?Imax-var (/ ?v-var ?r-var)) algebra)  ))
	 ))


;;; Power "through" component = V*I
(def-psmclass electric-power (electric-power ?comp ?t)
  :complexity major
  :short-name "electric power"
  :english ("the formula for electric power")
  :eqnFormat ("P = V*I"))

(defoperator electric-power-contains (?sought)
  :preconditions 
  ( 
   (any-member ?sought ( (voltage-across ?comp :time ?t ?t) 
			 (current-thru ?branch :time ?t ?t)
			 (electric-power ?comp :time ?t) ))
   (compo-or-branch ?comp ?branch)
   ;; We might apply this to things not declared to be a
   ;; circuit element
   (test (atom ?comp))
   (time ?t)
   ) 
  :effects ( (eqn-contains (electric-power ?comp ?t) ?sought) ))

(defoperator write-electric-power (?comp ?t)
  :preconditions 
  (
   (any-member ?tot (?t nil)) 
   (variable ?V (voltage-across ?comp :time ?tot) )
   (any-member ?tot2 (?t nil))
   (compo-or-branch ?comp ?branch) 
   (variable ?I (current-thru ?branch :time ?tot2))
   (variable ?P  (electric-power ?comp :time ?t)) 
   )
  :effects ( (eqn (= ?P (* ?V ?I)) (electric-power ?comp ?t)) )
  :hint (
	 (point (string "Power specifies the rate at which energy is transferred. Think about how the rate of energy transferred as charge moves across a component can be related to the current and the difference in electric potential across the endpoints."))
	 (teach (string "The potential difference (voltage) between two points is defined as the work needed to move a unit charge between those points. Power is the rate of doing work. For a battery with EMF V to produce a current I, it must move I unit charges per second through a potential difference of V, so its power output equals V*I. The same formula will give the amount of power DRAWN by a resistor as charge moves through it from higher to lower potential, when the electrical potential energy is converted to other forms of energy such as heat or light and dissipated from the circuit.)"))
	 (bottom-out (string "Write the equation ~a" ((= ?P (* ?V ?I)) algebra)))
	 ))

(defoperator define-electric-power-var (?b ?t)
  :preconditions ((bind ?power-var (format-sym "power_~A_~A" 
				(body-name ?b) (time-abbrev ?t)))) 
  :effects (
	    (define-var (electric-power ?b :time ?t))
	    (variable ?power-var (electric-power ?b :time ?t))
	    )
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting electric power." 
			     ((electric-power ?b :time ?t) def-np) ))
	 ))

;;; transformer voltage relation


(def-psmclass transformer-voltage (transformer-voltage ?coils ?t)
  :complexity major
  :short-name "transformer voltage relation"
  :english ("the voltage relation for an ideal transformer")
  :eqnFormat ("V1/N1 = V2/N2"))

(defoperator transformer-voltage-contains (?sought)
  :preconditions 
  ( 
   (mutual-inductor . ?coils)
   (any-member ?sought ( (voltage-across ?coil :time ?t) 
			 (turns ?coil))) 
   (test (member ?coil (rest ?coils)))
   (time ?t)
   ) 
  :effects 
  ( (eqn-contains (transformer-voltage ?coils ?t) ?sought) ))

(defoperator write-transformer-voltage (?coils ?t)
  :preconditions 
  (
   (test (orderless-p ?coils))
   (bind ?coil1 (second ?coils))
   (bind ?coil2 (third ?coils))
   (variable ?V1 (voltage-across ?coil1 :time ?t))
   (variable ?N1 (turns ?coil1))
   (variable ?V2 (voltage-across ?coil2 :time ?t))
   (variable ?N2 (turns ?coil2))
   )
  :effects 
  ( (eqn (= (* ?N1 ?V2) (* ?N2 ?V1)) (transformer-voltage ?coils ?t)) )
  :hint (
	 (point (string "The input and output voltages of a transformer are related."))
	 (bottom-out (string "Write the equation ~a" 
			     ((= (/ ?V1 ?N1)  (/ ?V2 ?N2)) algebra)))
	 ))

;;; transformer power relation


(def-psmclass transformer-power (transformer-power ?coils ?t)
  :complexity major
  :short-name "transformer power relation"
  :english ("the power relation for an ideal transformer")
  :eqnFormat ("P1 = P2"))

(defoperator transformer-power-contains (?sought)
  :preconditions 
  ( 
   (mutual-inductor . ?coils)
   (any-member ?sought ( (electric-power ?coil :time ?t) ))
   (test (member ?coil (rest ?coils)))
   (time ?t)
   ) 
  :effects 
  ( (eqn-contains (transformer-power ?coils ?t) ?sought) ))

(defoperator write-transformer-power (?coils ?t)
  :preconditions 
  (
   ;; should also test for only two coils
   (test (orderless-p ?coils))
   (bind ?coil1 (second ?coils))
   (bind ?coil2 (third ?coils))
   (variable ?P1 (electric-power ?coil1 :time ?t))
   (variable ?P2 (electric-power ?coil2 :time ?t))
   )
  :effects 
  ( (eqn (= ?P1 ?P2) (transformer-power ?coils ?t)) )
  :hint (
	 (point (string "The input and output power of an ideal transformer are related."))
	 (bottom-out (string "Write the equation ~a" 
			     ((= ?P1 ?P2) algebra)))
	 ))
