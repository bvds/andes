;;; (voltage R1) when PtA is left of R1 and PtB is right of R1 
;;; means the potential(PtB)-Potential(PtA)
;;; (current R1) means from PtA to PtB


;;; FUNCTIONS
; is ?x is (R_1  2  3) it returns the symbol R_123
; Called by comp-name
(defun convert-to-symbol (?x)
  (setf ?len (length ?x))
  (setf ?y (format nil "~a~a" (nth 0 ?x)(nth 1 ?x)))
  (cond ((= ?len 1) (setf ?y (format nil "~a" (nth 0 ?x)))
                    (setf ?z (intern ?y)))
        ((= ?len 2) (setf ?y (format nil "~a~a" (nth 0 ?x)(nth 1 ?x)))
                   (setf ?z (intern ?y)))
        (t (do ((count 2 (+ count 1)))
               ((= count ?len) ?y)
             (setf ?y (intern (format nil "~a~a" ?y (nth count ?x)))))
           (setf ?z (intern ?y)))))

    
    
;If ?res = (R1 R2 R3), this returns the R_123  
;Called by define-resistance-var
(defun comp-name (?res ?what)
  ; if res name is atomic, just return it unchanged
  (when (atom ?res) 
      (return-from comp-name ?res)) 
  ; else go on to construct name for compound
  (setf ?res (flatten ?res))  
  (setf ?len (length ?res))
  (setf ?new (first ?res))
  (cond ((eq ?what 'R)
         (setf ?d (string-left-trim "R" (string ?new)))
         (setf ?d (concatenate 'string "R_" ?d)))
        ((eq ?what 'C)
         (setf ?d (string-left-trim "C" (string ?new)))
         (setf ?d (concatenate 'string "C_" ?d))))

  (multiple-value-bind (ans num)
      (read-from-string ?d) 
    (setf ?new ans))
  (setf ?temp (list ?new))
  (do ((counter 1 (+ counter 1)))
      ((= counter ?len) ?temp)
    (cond ((eq ?what 'R)
           (setf ?a (string-left-trim "R" (string (nth counter ?res)))))
          ((eq ?what 'C)
           (setf ?a (string-left-trim "C" (string (nth counter ?res))))))

    (multiple-value-bind (ans num)
        (read-from-string ?a)
             (setf ?temp (append ?temp (list ans)))))
  (convert-to-symbol ?temp))


;If ?x = R_R123, this returns (R1 R2 R3)
; Called by current-in-branch-contains
(defun explode-resistor (?x)
  (setf ?res (string-left-trim "R_" (string ?x))) ;?res="123"
  (setf ?len (- (length (string ?x)) 3))
  (setf ?temp (list))
  (do ((counter 0 (+ counter 1)))
      ((= counter ?len) ?temp)
    (setf ?a (string (char ?res counter)))  ;pull off each number
    (setf ?b (concatenate 'string "R" ?a))  ; make it "R1"
    (multiple-value-bind (ans num)
        (read-from-string ?b)               ; make it R1
      (setf ?temp (append ?temp (list ans))))))

;Accepts '(a b c) and returns '((a b)(b c))
(defun form-adj-pairs (x)
  (setf temp '())
  (do ((c 0 (+ c 1)))
      ((= c (- (length x) 1)) (reverse temp))
    (setf a (nth c x))
    (setf b (nth (+ c 1) x))
    (setf temp (append (list(list a b)) temp ))))

(defun remove-paths (r p)
  (setf np '())
  (do ((c 0 (+ c 1)))
      ((= c (length p)) (reverse np))
    (cond ((not (eq nil (intersection (nth c p) r :test #'equal)))
           (setf np (append (list (nth c p)) np)))
          (t (setf np np)))))


; Called by comp-name
;This so every is at the same level
(defun flatten (x)
   (cond ((null x) x)
         ((atom (car x)) (cons (car x) (flatten (cdr x))))
         (t (append (flatten (car x)) (flatten (cdr x))))))

;Called by find-parallel-resistors
;This flattens out one level only
(defun flatten1 (x)
  (cond ((null x) x)
        ((atom (car x))(cons (car x) (flatten1 (cdr x))))
        ((= 1 (length (car x))) (append (car x) (flatten1 (cdr x))))
        (t (append (list (car x)) (flatten1 (cdr x))))))


;Called by same-elements
(defun check-list (x y)
  (cond ((null y) t)
        ((eq x (car y)) (check-list x (cdr y)))
        (t nil)))

;Returns t if all the elements of a list are the same
;Called by find-parallel-resistors
(defun same-elements (x)
  (cond ((null x) nil)
        (t (check-list (car x)(cdr x)))))



;if ?x = BrR123, this returns BrR_123
(defun convert-series-branch-name (?x)
  (setf ?y (string-left-trim "BrR" (string ?x))) ;?y="123"
  (setf ?b (concatenate 'string "BrR_" ?y))  ; make it "BrR_123"
  (multiple-value-bind (ans num)
      (read-from-string ?b)
    (setf ?temp ans)))

;Converts (during 1 2) to during_12
(defun convert-time (x)
  (concatenate 'string "during_" (format nil "~a" (second x))(format nil "~a" (third x))))

  
; if p = (JunA PtB R1 PtC R2 PtD JunE) and res-list = (R1 R2)
; it returns (JunA PtB (R1 R2) PtD JunE)
(defun convert-path (p res-list)
  (cond ((atom res-list) (setf res-list (list res-list))))
  (setf fr (car res-list))
  (setf lr (car (reverse res-list)))
  (append (reverse (cdr (member fr (reverse p)))) (list res-list) (cdr (member lr p))))

;Returns the list of branches coming out of a junction
(defun get-out-br (br-list names path jun)
  (setf temp '())
  (setf len (length br-list))
  (do ((c 0 (+ c 1)))
      ((= c len) temp)
       (setf x (nth c br-list))
       (setf y (position x names))
       (setf z (nth y path))
    (if (equal jun (first z)) 
        (setf temp (cons (nth c br-list) temp))
      temp))
  (reverse temp))

;Returns the list of branches going into of a junction
(defun get-in-br (br-list names path jun)
  (setf temp '())
  (setf len (length br-list))
  (do ((c 0 (+ c 1)))
      ((= c len) temp)
    (setf x (nth c br-list))
       (setf y (position x names))
       (setf z (nth y path))
    (if (equal jun (first (last z))) 
        (setf temp (cons (nth c br-list) temp))
      temp))
  (reverse temp))


;Returns the first capacitor in a path 
;Doesn't work if capacitor is in a nested list    
(defun find-cap (path)
  (setf done nil)
  (do ((c 0 (+ c 1)))
      ((or (eq done t)(= c (length path))) ans)
    (setf ans (nth c path))
    (if (equal #\C (char (string ans) 0)) (setf done t)(setf ans nil))))

;Returns a path with only one capacitor
(defun strip-cap (path)
  (setf x (first path))
  (setf y (last path))
  (setf z (find-cap path))
  (cons x (cons z y)))

;Returns a list of paths with no more than one capacitor per path
(defun modify (paths caps)
  (setf fans '())
  (do ((c 0 (+ c 1)))
      ((= c (length paths)) fans)
    (setf x (nth c paths))
    (if (= 1 (length (intersection x caps))) (setf fans (append (list x) fans))
      (setf fans (append (list (strip-cap x)) fans)))))

;y is a list contain x. This returns a list containing x and the element in front or behind it
(defun shrink (x y)
  (setf len (length y))
  (setf loc (position x y))
  (cond ((= len (+ loc 1)) 
         (setf a (nth (- loc 1) y))
         (list a x))
        (t (setf a (nth (+ loc 1) y))
           (list x a))))

;Returns the list of paths coming out of a junction
(defun get-out-paths (br-list names paths jun)
  (setf temp '())
  (setf len (length br-list))
  (do ((c 0 (+ c 1)))
      ((= c len) temp)
       (setf x (nth c br-list))
       (setf y (position x names))
       (setf z (nth y paths))
    (if (equal jun (first z)) 
        (setf temp (append (list z) temp))
      temp))
  (reverse temp))

;Returns the list of paths going into of a junction
(defun get-in-paths (br-list names paths jun)
  (setf temp '())
  (setf len (length br-list))
  (do ((c 0 (+ c 1)))
      ((= c len) temp)
    (setf x (nth c br-list))
       (setf y (position x names))
       (setf z (nth y paths))
    (if (equal jun (first (last z))) 
        (setf temp (append (list z) temp))
      temp))
  (reverse temp))

           
;their intersection won't work on :test #'equal
(defun our-intersection (x y)
  (intersection (flatten x)(flatten y)))

; This swaps the xth and yth elements in a list z
(defun swap (x y z)
  (append (subseq z 0 x) (list (nth y z)) 
          (subseq z (+ x 1) y) (list (nth x z)) (subseq z (+ y 1)(length z))))
        
        
        
; returns the location of the smallest element in a list starting at position ?i
(defun find-small (i x)
  (setf sm (nth i x))
  (setf loc i)
  (do ((c (+ i 1) (+ c 1)))
       ((= c (length x)) loc)
    (cond ((and (atom sm)(atom (nth c x))(string< (nth c x) sm))
           (setf sm (nth c x))
           (setf loc c))
          ((and (atom sm)(listp (nth c x))(string< (first (nth c x)) sm))
           (setf sm (nth c x))
           (setf loc c))
          ((and (listp sm)(atom (nth c x))(string< (nth c x) (first sm)))
           (setf sm (nth c x))
           (setf loc c))
          ((and (listp sm)(listp (nth c x))(string< (first (nth c x))(first sm)))
           (setf sm (nth c x))
           (setf loc c))
          (t))))
         



; Returns the list with the elements in sorted order. Allows for one level of nesting.
(defun insert-sort (x)
  (setf y x)
  (do ((c 0 (+ c 1)))
      ((= c (length x)) y)
    (setf w (find-small c y))
       (if (not (eq c w)) 
           (setf y (swap c w y)))))



#|
11Jun
;If ?p = (JunA (R1 R2) JunB), returns (JunA R_12 JunB)
(defun replace-comp-list (p)
  (cond ((null p) p)
        ((atom (car p)) (cons (car p)(replace-comp-list (cdr p))))
        (t (cons (comp-name (car p) 'R)(replace-comp-list (cdr p)))))
  )


;Returns true if the paths are not the same path or a combined version of a path
(defun self-loop (p1 p2)
  (or (member (find-foo p1) p2 :test #'equal)
      (member (find-foo p2) p1 :test #'equal))
)


;If ?x = BrR123, this returns (R1 R2 R3)
(defun explode-branch (?x)
  (setf ?res (string-left-trim "BrR" (string ?x))) ;?res="123"
  (setf ?len (- (length (string ?x)) 3))
  (setf ?temp (list))
  (do ((counter 0 (+ counter 1)))
      ((= counter ?len) ?temp)
    (setf ?a (string (char ?res counter)))  ;pull off each number
    (setf ?b (concatenate 'string "R" ?a))  ; make it "R1"
    (multiple-value-bind (ans num)
        (read-from-string ?b)               ; make it R1
      (setf ?temp (append ?temp (list ans))))))
                       
;if ?x = BrR123, this returns (R123)
(defun strip-Br (?x)
  (setf ?res (string-left-trim "BrR" (string ?x))) ;?res="R123"
  (setf ?b (concatenate 'string "R" ?res))  ; make it "R1"
  (multiple-value-bind (ans num)
      (read-from-string ?b)
    (setf ?temp (list ans))))
|#

;;;EQUIVALENT RESISTANCE
(defoperator define-resistance-var (?res)   
             :preconditions (
                             (bind ?r-var (format-sym "~A" (comp-name ?res 'R)))
                             )
             :effects (
               (variable ?r-var (resistance ?res))
	       (define-var (resistance ?res))
                       )
	      :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting resistance." ((resistance ?res) def-np)))
              ))

(def-psmclass equiv-resistance-series (equiv-resistance-series ?res-list) 
  :complexity major
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

(def-psmclass equiv-resistance-parallel (equiv-resistance-parallel ?res-list) 
  :complexity major
  :english ("Equivalent resistance of parallel resistors")
  :eqnFormat ("1/Req = 1/R1 + 1/R2 + 1/R3 + ...") )

(defoperator equiv-resistance-parallel-contains (?sought)
             :preconditions (
                             (any-member ?sought ((resistance ?res-list)))
                             (test (listp ?res-list))
                             )
             :effects(
                      (eqn-contains (equiv-resistance-parallel ?res-list) ?sought)
                      ))


#| ; don't use these, just put info into the givens

(defoperator find-series-resistors (?res-list)
             :preconditions (
		     ; find a series branch spec and pull out resistors in its path
                             (branch ?br-name ?dontcare ?dontcare2 ?path)
                            
                             (setof (in-wm (circuit-component ?res resistor))
                                    ?res ?all-res)
                             
                             (bind ?path-res (intersection ?all-res ?path :test #'equal))
                        
                             ; make sure these form the desired res-list. 
                             ; (test (null (set-exclusive-or ?path-res ?res-list :test #'equal)))
                             ;(test (equal (insert-sort ?res-list) (insert-sort ?path-res)))
                             )
             :effects(
                      (series-resistors ?res-list)
                      )
             :hint(
                   (point (string "Identify which resistors in branch ~a are in series." ?br-name))
                   (teach (string "Any resistors occuring in the same branch are in series."))
                   (bottom-out (string "The resistors ~a are in series in branch ~a." ?res-list ?br-name))
                   ))



(defoperator find-parallel-resistors (?res-list)
             :preconditions (
                             ; find the parallel paths
                             (setof (in-wm (branch ?name ?dontcare open ?path))
                                    ?path ?all-paths)
                             (bind ?new-paths (remove-paths ?res-list ?all-paths))
                             (test (eq (length ?res-list)(length ?new-paths)))
                             (bind ?a (mapcar #'(lambda (x) (car x)) ?new-paths))
                             (bind ?b (mapcar #'(lambda (x) (car (reverse x))) ?new-paths))
                             (test (and (same-elements ?a)(same-elements ?b)))
                             
                             ; collect the resistors from those paths
                             (setof (in-wm (circuit-component ?res resistor))
                                    ?res ?all-res)
                             (bind ?par-res (reverse (flatten1 (mapcar #'(lambda (x) (intersection x ?all-res :test #'equal))?all-paths))))
                             )
                                          
             :effects(
                      (parallel-resistors ?res-list)
                      )
             :hint(
                   (point (string "Identify which branches are in parallel."))
                   (point (string "If parallel branches have single resistors in them, then the resistors are in parallel."))
                   (teach (string "Branches are in parallel if they have the same junctions at both end."))
                   (bottom-out (string "The resistors ~a are in parallel." ?res-list ))
                   ))
|# ; end unused


(defoperator equiv-resistance-series (?res-list)
             :specifications "doc"
             :preconditions (
   	      ; verify sought list equivalent to some set of series resistors
	      ; For simplicity we just put info about series resistors into givens. 
	      ; List can include complex equivalents, e.g (series-resistors (R1 (R2 R3 R4) (R5 R6)))
              ; but should only contain one level of nesting, a list of sets of atoms

                             (series-resistors ?series-list)
                             ; make sure the set of atomic resistors in series-list
			     ; equals to the set of resistors sought in res-list. 
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
	      ; verify sought list equivalent to some list of parallel resistors
	      ; List can include complex 
	      ; equivalents, e.g (parallel-resistors (R1 (R2 R3 R4) (R5 R6)))
              ; but should only contain one level of nesting, a list of sets of atoms
             
                            (parallel-resistors ?parallel-list) 
                             ; make sure the set of atomic resistors in parallel-list
			     ; equals to the set of resistors sought in res-list. 
                             (test (null (set-exclusive-or (flatten ?parallel-list) 
			                                   ?res-list)))
              
                            ; pull out terms for each resistance
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



;;;CURRENTS
(def-psmclass current-thru-what (current-thru-what ?what ?branch ?t) 
  :complexity minor 
  :english ("Current in branch")
  :Expformat ("Relating the current through ~A to the current in the branch containing it" ?what)
  :eqnFormat ("Icomp = Ibranch"))

(defoperator current-pt-or-comp-contains (?sought)
      :preconditions (
                      (any-member ?sought ((at (current-thru ?what) ?t)))
                      (branch ?branch ?dontcare1 ?dontcare2 ?path)
                      (test (member ?what ?path :test #'equal) )
                      )
      :effects (
                (eqn-contains (current-thru-what ?what ?branch ?t) ?sought)
                ))

(defoperator current-in-branch-same-resistor-contains (?sought)
      :preconditions (
                      (any-member ?sought ((at (current-in ?branch) ?t)))
                      (branch ?branch ?dontcare1 ?dontcare2 ?path)
                      (circuit-component ?what ?comp-type)
		      (test (member ?comp-type '(resistor inductor)))
                      (test (member ?what ?path :test #'equal))
                      )
      :effects (
                (eqn-contains (current-thru-what ?what ?branch ?t) ?sought)
                ))

(defoperator write-current-thru-what (?what ?branch ?t)
             :specifications "doc"
             :preconditions (
                             (variable ?i-what-var (at (current-thru ?what) ?t))
                             (variable ?i-br-var (at (current-in ?branch) ?t))
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




(defoperator current-in-branch-contains (?sought)
      :preconditions (
                      (any-member ?sought ((at (current-in ?branch) ?t)))
                      (setf ?res-list (explode-resistors ?branch))
                      (branch ?branch ?dontcare1 ?dontcare2 ?path)
                      )
             :effects (
                       (resistance ?res-list)
                       (eqn-contains (equiv-resistance-series ?res-list) ?sought)
                       ))

(defoperator define-current-thru-var (?what ?t)
             :preconditions (
		 ; ?what could be a list naming a compound (equivalent) circuit element.
                             (bind ?i-what-var (format-sym "I_~A$~A" (comp-name ?what 'R) (time-abbrev ?t)))
                             )
             :effects (
                       (variable ?i-what-var (at (current-thru ?what) ?t))
                       (define-var  (at (current-thru ?what) ?t))
                       )
             :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting current." ((at (current-thru ?what) ?t) def-np)))
                  ))
		       

;If we need time the change the bind
(defoperator define-current-in-var (?branch ?t)
             :preconditions (
                             (bind ?i-br-var (format-sym "I_~A$~A" ?branch (time-abbrev ?t)))
                             )
             :effects (
                       (variable ?i-br-var (at (current-in ?branch) ?t))
		       (define-var (at (current-in ?branch) ?t))
                       )
             :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting current." ((at (current-in ?branch) ?t) def-np)))
		       ))

(def-psmclass ohms-law (ohms-law ?res ?t) 
  :complexity major 
  :english ("Ohm's Law")
  :eqnFormat ("V = I * R"))

;May need to uncomment (resistance) as a sought to get currents to work
(defoperator ohms-law-contains-resistor (?sought)
             :preconditions(
                            (any-member ?sought ((at (current-thru ?res) ?t)
                                                 (at (voltage-across ?res) ?t)))
                            (time ?t)
                            (circuit-component ?res resistor)
                            ;Added mary/Kay 7 May
                            ;(branch ?br-res ?dontcare1 ?dontcare2 ?path)
                            ;(test (member ?res ?path :test #'equal))     
                            )
             :effects(
                      (eqn-contains (ohms-law ?res ?t) ?sought)
                      ))

(defoperator single-resistance-contains (?sought)
             :preconditions(
                            (any-member ?sought ((resistance ?res)))
			    ; only apply for resistance of atomic resistor:
			    ; (Why? -- AW)
                            (test (atom ?res))
                            (time ?t)
                            (circuit-component ?res resistor)
                            ;(branch ?br-res ?dontcare1 ?dontcare2 ?path)
                            ;(test (member ?res ?path))
                            )
             :effects(
                      (eqn-contains (ohms-law ?res ?t) ?sought)
                      ))

(defoperator ohms-law (?res ?t)
             :specifications "doc"
             :preconditions(
			    ; if we want to use branch current var:
                            ;(branch ?br-res ?dontcare1 ?dontcare2 ?path)
                            ;(test (member ?res ?path))
                            ;(variable ?i-var (at (current-in ?br-res) ?t))
                            (variable ?r-var (resistance ?res))
                            (variable ?i-var (at (current-thru ?res) ?t))
                            (variable ?v-var (at (voltage-across ?res) ?t))
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


 
(def-psmclass current-equiv (currents-same-equivalent-branches ?res ?br-res ?t) 
  :complexity minor
  :english ("Current in equivalent branches")
  :eqnFormat ("Iequiv = Iorig"))
             
(defoperator currents-same-equivalent-branches-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (current-in ?br-res) ?t)
                                                 (at (current-in ?br-res2) ?t)))
                            (solve-using-both-methods)
                            (time ?t)
                            (branch ?br-res given ?dontcare1 ?path1)
                            (branch ?br-res2 combined ?dontcare1 ?path2)
                            (setof (circuit-component ?comp1 ?dontcare4) ?comp1 ?all-comps)
                            (bind ?path-comps1 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                ?path1))
                            (bind ?path-comps2 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                ?path2))
                            (test (or (equal (car ?path-comps1) ?path-comps2) 
                                      (equal (car ?path-comps2) ?path-comps1)))
                            (test (eq (car ?path1) (car ?path2)))
                            (test (eq (car (reverse ?path1)) (car (reverse ?path2))))          
                            )
             :effects(
                      (eqn-contains (currents-same-equivalent-branches ?br-res ?br-res2 ?t) ?sought)
                      ))

(defoperator currents-same-equivalent-branches (?br-res ?br-res2 ?t)
             :preconditions (
                             (variable ?i-var1 (at (current-in ?br-res) ?t))
                             (variable ?i-var2 (at (current-in ?br-res2) ?t))
                             )
             :effects (
                       (eqn (= ?i-var1 ?i-var2)(currents-same-equivalent-branches ?br-res ?br-res2 ?t))
                       )
             :hint(
                   (point (string "What do you know about the current through a set of series resistors and the current through the equivalent resistor?"))
                   (point (string "Consider the branch ~a and the branch ~a." (?br-res adj) (?br-res2 adj)))
                   (teach (string "The current through a set of series resistors is equal to the current through the equivalent resistor."))
                   (bottom-out (string "Set the current in branch ~a equal to the current in branch ~a." (?br-res adj) (?br-res2 adj)))
                   ))


(defoperator define-voltage-across-var-resistor (?comp ?t)
             :preconditions (
                             (bind ?nt (if (atom ?t) ?t (convert-time ?t)))
                             (bind ?v-var (format-sym "deltaV_~A$~A" (comp-name ?comp 'R) ?nt))
                             )
             :effects (
                       (variable ?v-var (at (voltage-across ?comp) ?t))
                       (define-var (at (voltage-across ?comp) ?t))
                       )
	     :hint (
(bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting voltage." ((at (voltage-across ?comp) ?t) def-np)))
		   ))

(def-psmclass loop-rule  (loop-rule ?branch-list ?t ?dir)  
  :complexity major 
  :english ("Kirchoff's loop rule")
  :eqnFormat ("V1 + V2 + V3 ... = 0"))

(defoperator loop-rule-contains (?comp ?t)
      :preconditions (
                      (closed-loop ?branch-list ?p1 ?p2 ?path ?reversed)
                      (test (member ?comp ?path :test #'equal)))
      :effects (
                (eqn-contains (loop-rule ?branch-list ?t forward) (at (voltage-across ?comp) ?t))
                ))


(defoperator closed-branch-is-loop (?branch)
             :preconditions (
                             (branch  ?branch ?dontcare  closed ?path)
                             )
             :effects (
                       (closed-loop ?branch ?path ?path ?path 0)
                       ))



(defoperator form-two-branch-loop (?br1 ?br2)
             :preconditions (
                             (branch ?br1 ?dontcare1 open ?path1)
                             (branch ?br2 ?dontcare2 open ?path2)
                             ; test not a self loop
                             (setof (circuit-component ?comp1 ?dontcare4) ?comp1 ?all-comps)
                             (bind ?path-comps1 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path1))
                             (bind ?path-comps2 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path2))
                       
                             (test (null (and (our-intersection ?path-comps1 ?path2)
                                              (our-intersection ?path-comps2 ?path1))))
                             
                             ; only try if branch names are in loop-id order
                             (test (expr< ?br1 ?br2))
                             
                             ; test paths are connected. Two cases, depending on whether
                             ; path2 or (reverse path2) is needed to make a closed loop
                             (test (or (and (equal (car (last ?path1)) (first ?path2))
                                            (equal (car (last ?path2)) (first ?path1)))
                                       (and (equal (first ?path1) (first ?path2))
                                            (equal (car (last ?path1)) (car (last ?path2))))))
                             ; loop-id is sorted branch list
                             (bind ?branch-list (list ?br1 ?br2))
                             
                             ; build loop path. Duplicate starting point at end to ensure all
                             ; components have two terminal points in path.
                             (bind ?loop-path (if (not (equal (first ?path1) (first ?path2)))
                                                  (append ?path1 (subseq ?path2 1))
                                                (append ?path1 (subseq (reverse ?path2) 1))))
                             (bind ?reversed (if (not (equal (first ?path1) (first ?path2))) 0 1))
                             )
             :effects (
                       (closed-loop ?branch-list ?path-comps1 ?path-comps2 ?loop-path ?reversed)
                       ))

(defoperator form-three-branch-loop (?br1 ?br2)
             :preconditions (
                             (branch ?br1 ?dontcare1 open ?path1)
                             (branch ?br2 ?dontcare2 open ?path2)
                             (branch ?br3 ?dontcare3 open ?path3)
                             ; only try if branch names are in loop-id order
                             (test (and (expr< ?br1 ?br2)(expr< ?br2 ?br3)))
 
                             ;(test (and (not (equal ?br1 ?br2))
                         ;               (not (equal ?br2 ?br3))
                         ;               (not (equal ?br3 ?br1))))
                             ; test not a self loop
                             (setof (circuit-component ?comp1 ?dontcare4) ?comp1 ?all-comps)
                             (bind ?path-comps1 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path1))
                             (bind ?path-comps2 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path2))
                             (bind ?path-comps3 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path3))

                             (test (null (and (our-intersection ?path-comps1 ?path2)
                                              (our-intersection ?path-comps1 ?path3)
                                              (our-intersection ?path-comps2 ?path3)
                                              )))
                             
                            
                             ; test paths are connected. Two cases, depending on whether
                             ; path2 or (reverse path2) is needed to make a closed loop
                             (test (and (equal (car (last ?path1)) (first ?path2))
                                        (equal (car (last ?path2)) (first ?path3))
                                        (equal (car (last ?path3)) (first ?path1))))
                                        
                             ; loop-id is sorted branch list
                             (bind ?branch-list (list ?br1 ?br2 ?br3))
                             
                             ; build loop path. Duplicate starting point at end to ensure all
                             ; components have two terminal points in path.
                             (bind ?path-comps4 (append ?path-comps1 ?path-comps2))
                             (bind ?loop-path (append ?path1 (subseq ?path2 1)(subseq ?path3 1)))
                             
                             (bind ?reversed 0) ;did not reverse
                            ; (bind ?reversed (if (not (equal (first ?path1) (first ?path2))) 0 1))
                             )
             :effects (
                       (closed-loop ?branch-list ?path-comps4 ?path-comps3 ?loop-path ?reversed)
                       ))

(defoperator form-four-branch-loop (?br1 ?br2)
             :preconditions (
                             (branch ?br1 ?dontcare1 open ?path1)
                             (branch ?br2 ?dontcare2 open ?path2)
                             (branch ?br3 ?dontcare3 open ?path3)
                             (branch ?br4 ?dontcare4 open ?path4)
                             ; only try if branch names are in loop-id order
                             (test (and (expr< ?br1 ?br2)(expr< ?br2 ?br3)(expr< ?br3 ?br4)))

                             (test (and (= (length (intersection ?path1 ?path2)) 1)
                                        (= (length (intersection ?path2 ?path3)) 1)
                                        (= (length (intersection ?path3 ?path4)) 1)
                                        (= (length (intersection ?path1 ?path4)) 1)
                                        (= (length (intersection ?path2 ?path4)) 0)
                                        (= (length (intersection ?path1 ?path3)) 0)
                                        ))



                             ; test not a self loop
                             (setof (circuit-component ?comp1 ?dontcare5) ?comp1 ?all-comps)
                             (bind ?path-comps1 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path1))
                             (bind ?path-comps2 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path2))
                             (bind ?path-comps3 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path3))
                             (bind ?path-comps4 (remove-if-not #'(lambda(elt) (member elt ?all-comps :test #'equal)) 
                                                               ?path4))



                             (test (null (and (our-intersection ?path-comps1 ?path2)
                                              (our-intersection ?path-comps1 ?path3)
                                              (our-intersection ?path-comps1 ?path4)
                                              (our-intersection ?path-comps2 ?path3)
                                              (our-intersection ?path-comps2 ?path4)
                                              (our-intersection ?path-comps3 ?path4)
                                              )))

                             
                             ; test paths are connected. Two cases, depending on whether
                             ; path2 or (reverse path2) is needed to make a closed loop
                             (test (and (equal (car (last ?path1)) (first ?path2))
                                        (equal (car (last ?path2)) (first ?path3))
                                        (equal (car (last ?path3)) (first ?path4))
                                        (equal (car (last ?path4)) (first ?path1))))
                                       
                             ; loop-id is sorted branch list
                             (bind ?branch-list (list ?br1 ?br2 ?br3 ?br4))
                             
                             ; build loop path. Duplicate starting point at end to ensure all
                             ; components have two terminal points in path.
                             (bind ?path-comps5 (append ?path-comps1 ?path-comps2))
                             (bind ?path-comps6 (append ?path-comps3 ?path-comps4))
                             (bind ?loop-path (append ?path1 (subseq ?path2 1)(subseq ?path3 1)(subseq ?path4 1)))
                             
                             (bind ?reversed 0) ;did not reverse
                             )
             :effects (
                       (closed-loop ?branch-list ?path-comps5 ?path-comps6 ?loop-path ?reversed)
                       ))




(defoperator write-loop-rule-resistors (?branch-list ?t)
      :preconditions (
                      ;Stop this rule for RC/LRC problems
                      (not (circuit-component ?dontcare capacitor))
                      (not (circuit-component ?dontcare inductor))

                      (in-wm (closed-loop ?branch-list ?p1 ?p2 ?path ?reversed))

                      ;Make sure ?p2 is a list
                      ;If ?rev ends up nil then ?p2 was reversed in ?path
                      (bind ?rev (member (second ?p2) (member (first ?p2) ?path :test #'equal) :test #'equal))
                      (bind ?p3 (if (equal ?rev nil) (reverse ?p2) ?p2))
                       
                      ;get the set of resistors
                      (setof (circuit-component ?comp1 resistor)
                             ?comp1 ?all-res)
                      ;get the set of batteries
                      (setof (circuit-component ?comp2 battery)
                             ?comp2 ?all-batts)
                      
                      ;get all the resistor delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-res :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-res1-vars)

                      ;get all the battery delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-batts :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-batt1-vars)

                      ;get all the resistor delta variables for ?p2
                      (map ?comp (intersection ?p3 ?all-res :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-res2-vars)

                      ;get all the battery delta variables for ?p2
                      (map ?comp (intersection ?p3 ?all-batts :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-batt2-vars)

                      ;determine whether ?p1 + ?p2 or ?p1 - ?p2
                      (bind ?sign (if (equal ?reversed 0) '+ '-))
                      (test (or (not (equal ?v-res1-vars nil))
                                (not (equal ?v-res2-vars nil))))
                      (test (or (not (equal ?v-batt1-vars ?v-batt2-vars))
                                (and (equal ?v-batt1-vars nil)(equal ?v-batt2-vars nil))))
                      )
             :effects (
                       (eqn (= 0 (?sign (- (+ . ?v-batt1-vars)(+ . ?v-res1-vars))
                                 (- (+ . ?v-batt2-vars)(+ . ?v-res2-vars))))
                            (loop-rule ?branch-list ?t forward))
                       )
             :hint(
                   (point (string "Find a closed loop in this circuit and apply Kirchhoff's Loop Rule to it."))
                   (point (string "To find the closed loop, pick any point in the circuit and find a path through the circuit that puts you back at the same place."))
 (point (string "You can apply Kirchoff's Loop Rule to the loop formed by the branches ~A." (?branch-list conjoined-names)))
                   (point (string "Once you have identified the closed loop, write an equation that sets the sum of the voltage across each component around the closed circuit loop to zero."))
                   (teach (string "The sum of the voltage around any closed circuit loop must be equal to zero.  If you are going in the same direction as the current the voltage across a resistor is negative, otherwise it is positive. If you go across the battery from the negative to the positive terminals, the voltage across the battery is positive, otherwise it is negative."))
                   (teach (string "Pick a consistent direction to go around the closed loop. Then write an equation summing the voltage across the battery and the voltages across the resistors, paying attention to whether you are going with or against the current."))
		   (bottom-out (string "Write the equation ~a."
                           ((= 0 (?sign (- (+ . ?v-batt1-vars)(+ . ?v-res1-vars))
                                 (- (+ . ?v-batt2-vars)(+ . ?v-res2-vars)))) algebra) ))
                   ))


(defoperator write-single-loop-rule (?branch-list ?t)
             :preconditions (
                      (in-wm (closed-loop ?branch-list ?p1 ?p1 ?path ?reversed))
                     
                      ;get the set of resistors
                      (setof (circuit-component ?comp1 resistor)
                             ?comp1 ?all-res)
                      ;get the set of batteries not switched out at t
                      (setof (active-battery ?comp2 ?t)
                             ?comp2 ?all-batts)
                      ;get the set of capacitors
                      (setof (circuit-component ?comp3 capacitor)
                             ?comp3 ?all-caps)
		      ;get the set of all inductors
		      (setof (circuit-component ?comp4 inductor)
		             ?comp4 ?all-inds)

                      ;get all the resistor delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-res :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-res1-vars)

                      ;get all the battery delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-batts :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-batt1-vars)
                             
                      ;get all the capacitor delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-caps :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-cap-vars)

		      ; get all the inductor delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-inds :test #'equal)
                         (variable ?v-var (at (voltage-across  ?comp) ?t))
                         ?v-var ?v-ind-vars)
		      ; list batteries and inductors for positive sum
		      (bind ?emf-vars (append ?v-batt1-vars ?v-ind-vars))
		      ; list capacitors and resistors for negative term
		      (bind ?drop-vars (append ?v-res1-vars ?v-cap-vars))
                      )
             :effects (
                       (eqn (= 0 (- (+ . ?emf-vars) (+ . ?drop-vars)))
                            (loop-rule ?branch-list ?t forward))
                       )
             :hint(
                   ;(point (string "Apply Kirchhoff's Loop Rule to the circuit."))
		   (point (string "You can apply Kirchoff's Loop Rule to the loop formed by the branches ~A." (?branch-list conjoined-names)))
                   ;(point (string "Write an equation that sets the sum of the voltage across each component around the closed circuit loop to zero."))
                   (teach (string "Kirchoff's Loop Rule states that the sum of the voltages around any closed circuit loop must be equal to zero."))
                   (teach (string "Pick a consistent direction to go around the closed loop. Then write an equation summing the voltage across the battery and the voltages across the circuit components, paying attention to whether you are going with or against the current."))
		   (bottom-out (string "Write the equation ~A"
                     ((= 0 (- (+ . ?emf-vars) (+ . ?drop-vars))) algebra) ))
                   ))

; filter to use when fetching batteries for loop rule, because a battery may be present 
; in problem but switched out as for LC decay.
(defoperator get-active-battery (?bat ?t)
  :preconditions (
      (in-wm (circuit-component ?bat battery))
      (not (switched-out ?bat ?t))
  ) :effects ( (active-battery ?bat ?t) ))

(def-psmclass junction-rule  (junction-rule ?br-list1 ?br-list2 ?t)  
  :complexity major 
  :english ("Kirchoff's junction rule")
  :eqnFormat ("Iin = Iout"))

(defoperator junction-rule-contains (?sought)
      :preconditions (
                      (in-branches ?dontcare ?br-list1)
                      (out-branches ?dontcare ?br-list2)
                      (time ?t)
                      )
      :effects (
                (eqn-contains (junction-rule ?br-list1 ?br-list2 ?t) ?sought)
                ))


(defoperator junction-rule (?br-list1 ?br-list2 ?t )
             :preconditions (
                             (any-member ?sought ((at (current-in ?branch) ?t)
                                                  (at (current-thru ?res) ?t)
                                                  (at (voltage-across ?res) ?t)))
                             (time ?t)
                             ;find the in branches current variables
                             (map ?br ?br-list1
                               (variable ?v-var (at (current-in ?br) ?t))
                               ?v-var ?v-in-br-vars)
                             
                             ;find the out branches current variables
                             (map ?br ?br-list2
                               (variable ?v-var (at (current-in ?br) ?t))
                               ?v-var ?v-out-br-vars)
                             )
             :effects (
                       (eqn (= (+ . ?v-in-br-vars) (+ . ?v-out-br-vars))
                            (junction-rule ?br-list1 ?br-list2 ?t))
                       )
             :hint(
                   (point (string "Apply Kirchhoff's Junction Rule to this circuit."))
                   (point (string "Pick a junction. A junction occurs when two or more branches meet."))
                   (teach (string "The sum of the currents into a junction must equal the sum of the currents out of the junction."))
                   (teach (string "Set the sum of the currents into the junction equal to the sum of the currents out of the junction."))
                   (bottom-out (string "Write the equation ~A"
		      ((= (+ . ?v-in-br-vars) (+ . ?v-out-br-vars)) algebra) ))
                   ))


(defoperator find-out-branches ()
             :preconditions (
                             (junction ?jun ?br-list)                        
                             ; find all branches 
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?name ?all-names)

                             ; find all the paths
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?path ?all-paths)
                             (bind ?out-br (get-out-br ?br-list ?all-names ?all-paths ?jun)) 
                             (test (not (equal nil ?out-br)))
                             )
             :effects (
                       (out-branches ?jun ?out-br)
                       ))


(defoperator find-in-branches ()
             :preconditions (
                             (junction ?jun ?br-list)                        
                             ; find all branches 
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?name ?all-names)

                             ; find all the paths
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?path ?all-paths)
                             (bind ?in-br (get-in-br ?br-list ?all-names ?all-paths ?jun))
                             (test (not (equal nil ?in-br)))
                             )
             :effects(
                      (in-branches ?jun ?in-br)
                      ))


;;;EQUIVALENT CAPACITORS
(defoperator define-capacitance-var (?cap)   
             :preconditions (
                             (bind ?c-var (format-sym "~A" (comp-name ?cap 'C)))
                             )
             :effects (
               (variable ?c-var (capacitance ?cap))
               (define-var (capacitance ?cap))
                       )
	     :hint (
(bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting capacitance." ((capacitance ?cap) def-np)))
		   ))

(def-psmclass equiv-capacitance-series (equiv-capacitance-series ?cap-list) 
  :complexity major
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

#| ; for now, don't even try to infer series or parallel capacitors

(defoperator find-series-capacitors (?cap-list)
             :preconditions (
		     ; find a series branch spec and pull out capacitors in its path
                             (branch ?br-name ?dontcare ?dontcare2 ?path)
                            
                             (setof (in-wm (circuit-component ?cap capacitor))
                                    ?cap ?all-cap)
                       
                             (bind ?path-cap (intersection ?all-cap ?path :test #'equal))
                      
		     ; make sure these form the desired res-list. 
                             ; (test (null (set-exclusive-or ?path-cap ?cap-list :test #'equal)))
                             ;(test (equal (insert-sort ?cap-list) (insert-sort ?path-cap)))
                             )
             :effects(
                      (series-capacitors ?cap-list)
                      )
             :hint(
                   (point (string "Identify which capacitors in branch ~a are in series." (?br-name adj)))
                   (teach (string "Any capacitors occuring in the same branch are in series."))
                   (bottom-out (string "The capacitors ~a are in series in branch ~a." (conjoined-names ?cap-list) (?br-name adj)))
                   ))


(defoperator find-parallel-capacitors (?cap-list)
             :preconditions (
                             ; find the parallel paths
                             (setof (in-wm (branch ?name ?dontcare open ?path))
                                    ?path ?all-paths)
                             (bind ?new-paths (remove-paths ?cap-list ?all-paths))
                             (test (eq (length ?cap-list)(length ?new-paths)))
                             (bind ?a (mapcar #'(lambda (x) (car x)) ?new-paths))
                             (bind ?b (mapcar #'(lambda (x) (car (reverse x))) ?new-paths))
                             (test (and (same-elements ?a)(same-elements ?b)))
                             
                             ; collect the capacitors from those paths
                             (setof (in-wm (circuit-component ?cap capacitor))
                                    ?cap ?all-cap)
                             )
                                          
             :effects(
                      (parallel-capacitors ?cap-list)
                      )
             :hint(
                   (point (string "Identify which branches are in parallel."))
                   (point (string "If parallel branches have single capacitors in them, then the capacitors are in parallel. Branches are in parallel if they have the same junctions at both end."))
                   
                   ))
|#

(defoperator equiv-capacitance-series (?cap-list)
             :specifications "doc"
             :preconditions (
   	      ; verify sought list equals some list of series capacitors
	      ; For simplicity we just put info about series capacitors into givens. 
	      ; List can include complex equivalents, e.g (series-capacitors (C1 (C2 C3 C4) (C5 C6)))
              ; but should only contain one level of nesting, a list of sets of atoms

                             (series-capacitors ?series-list)
		             ; make sure the ones found equal the sought cap-list
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
                       ; (capacitance ?tot-cap)
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
	      ; verify sought list equals some list of parallel capacitors
	      ; This list may not be able to include complex 
	      ; equivalents, e.g (parallel-capacitors (C1 (C2 C3 C4) (R5 C6)))
              ; but should only contain one level of nesting, a list of sets of atoms
             
                            (parallel-capacitors ?parallel-list) 
		            ; make sure the ones found equal the sought cap-list
                            (test (null (set-exclusive-or (flatten ?parallel-list) 
			                                   ?cap-list)))
              
                            ; pull out terms for each capacitance
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



;;;CHARGES ON CAPACITORS
(defoperator define-charge-on-cap-var (?what ?t)
             :preconditions (
                             (test (and (atom ?t)(not (eq ?t 'inf))))
                             (circuit-component ?what capacitor)
                             (bind ?q-what-var (format-sym "Q_~A$~A" (comp-name ?what 'C) ?t))
                             )
             :effects (
                       (variable ?q-what-var (at (charge-on ?what) ?t))
                       (define-var (at (charge-on ?what) ?t))
                       )
 	     :hint (
(bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting charge." ((at (charge-on ?what) ?t) def-np)))
		   ))


(def-psmclass cap-defn (cap-defn ?cap ?t) 
  :complexity major
  :english ("Definition of capacitance")
  :eqnFormat ("C = q/V"))

(defoperator capacitor-definition-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (charge-on ?cap) ?t)
                                                 (at (voltage-across ?cap) ?t)))
                            (time ?t)
                            (test (atom ?t))
                            (circuit-component ?cap capacitor)
                            )
             :effects(
                      (eqn-contains (cap-defn ?cap ?t) ?sought)
                      ))

(defoperator capacitor-definition-single-contains (?sought)
             :preconditions(
                            (any-member ?sought ((capacitance ?cap) ))
                            (test (atom ?cap))
                            (time ?t)
                            (circuit-component ?cap capacitor)
                            )
             :effects(
                      (eqn-contains (cap-defn ?cap ?t) ?sought)
                      ))

(defoperator write-cap-defn (?cap ?t)
             :specifications "doc"
             :preconditions(
                            (variable ?c-var (capacitance ?cap))
                            (variable ?q-var (at (charge-on ?cap) ?t))
                            (variable ?v-var (at (voltage-across ?cap) ?t))
                            )
             :effects(
                      (eqn (= ?c-var (/ ?q-var ?v-var)) (cap-defn ?cap ?t))
                      )
             :hint(
                   (point (string "Write an equation for the capacitance of ~a." (?cap adj)))
                   (point (string "The capacitance of the capacitor ~a is defined in terms of its charge and the voltage across it." (?cap adj)))
                   (teach (string "The capacitance is defined as the charge on the capacitor divided by the voltage across the capacitor."))
                   (bottom-out (string "Write the equation defining the capacitance ~a as charge ~a divided by voltage ~a." (?c-var algebra) (?q-var algebra) (?v-var algebra)))
                   ))


(defoperator write-loop-rule-capacitors (?branch-list ?t)
      :preconditions (
                      ;Stop this rule for LC/LRC problems
                      (not (circuit-component ?dontcare inductor))

                      (in-wm (closed-loop ?branch-list ?p1 ?p2 ?path ?reversed))
                      ;Make sure ?p2 is a list
                      ;If ?rev ends up nil then ?p2 was reversed in ?path
                      (bind ?rev (member (second ?p2) (member (first ?p2) ?path :test #'equal) :test #'equal))
                      (bind ?p3 (if (equal ?rev nil) (reverse ?p2) ?p2))

                      ;get the set of capacitors
                      (setof (circuit-component ?comp1 capacitor)
                             ?comp1 ?all-cap)
                      ;get the set of batteries
                      (setof (circuit-component ?comp2 battery)
                             ?comp2 ?all-batts)
                      
                      ;get all the capacitor delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-cap :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-cap1-vars)

                      ;get all the battery delta variables for ?p1
                      (map ?comp (intersection ?p1 ?all-batts :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-batt1-vars)

                      ;get all the capacitor delta variables for ?p2
                      (map ?comp (intersection ?p3 ?all-cap :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-cap2-vars)

                      ;get all the battery delta variables for ?p2
                      (map ?comp (intersection ?p3 ?all-batts :test #'equal)
                        (variable ?v-var (at (voltage-across  ?comp) ?t))
                        ?v-var ?v-batt2-vars)

                      ;determine whether ?p1 + ?p2 or ?p1 - ?p2
                      (bind ?sign (if (equal ?reversed 0) '+ '-))
                      (test (or (not (equal ?v-cap1-vars nil))
                                (not (equal ?v-cap2-vars nil))))
                      (test (or (not (equal ?v-batt1-vars ?v-batt2-vars))
                                (and (equal ?v-batt1-vars nil)(equal ?v-batt2-vars nil))))
                      )
             :effects (
                       (eqn (= 0 (?sign (- (+ . ?v-batt1-vars)(+ . ?v-cap1-vars))
                                        (- (+ . ?v-batt2-vars)(+ . ?v-cap2-vars))))
                            (loop-rule ?branch-list ?t forward))
                       )
             :hint(
                   (point (string "Find a closed loop in this circuit and apply Kirchhoff's Loop Rule to it.  To find the closed loop, pick any point in the circuit and find a path around the circuit that puts you back at the same place."))
                   (point (string "You can apply Kirchoff's Loop Rule to the loop formed by the branches ~A." (?branch-list conjoined-names)))
                   ;(point (string "Once you have identified the closed loop, write an equation that sets the sum of the voltage across each component around the closed circuit loop to zero."))
                   (teach (string "The sum of the voltages around any closed circuit loop must be zero.  Take the side of the capacitor closest to the positive terminal of the battery to be at high potential. If you reach this side of the capacitor first as you go around the loop, subtract the voltage across the capacitor from the battery voltage, otherwise add it."))
                   ;(bottom-out (string "Pick a consistent direction to go around the closed loop. Then write an equation summing the voltage across the battery and the voltages across the capacitors, paying attention to whether you should add or subtract the voltage across each capacitor."))
                   (bottom-out (string "The loop rule for ~A can be written as ~A" (?branch-list conjoined-names)
		                    ((= 0 (?sign (- (+ . ?v-batt1-vars)(+ . ?v-cap1-vars))
                                        (- (+ . ?v-batt2-vars)(+ . ?v-cap2-vars)))) algebra)  ))
                   ))



(defoperator junction-rule-cap-contains (?sought)
      :preconditions (
                      (in-paths ?dontcare ?path-list1)
                      (out-paths ?dontcare ?path-list2)
                      ;get the set of capacitors
                      (setof (circuit-component ?comp1 capacitor)
                             ?comp1 ?all-cap)
                      (test (intersection (first ?path-list1) ?all-cap :test #'equal))
                      ;Stop a=b+c and b+c=a from both coming out
                      (bind ?p1 (if (expr< ?path-list1 ?path-list2) ?path-list1 ?path-list2))
                      (bind ?p2 (if (expr< ?path-list1 ?path-list2) ?path-list2 ?path-list1))
                      (time ?t)
                      )
      :effects (
                (eqn-contains (junction-rule-cap ?p2 ?p1 ?t) ?sought)
                ))

(defoperator junction-rule-cap (?path-list1 ?path-list2 ?t )
             :preconditions (
                             (any-member ?sought ((at (charge-on ?cap) ?t)
                                                  (at (voltage-across ?cap) ?t)))
                             (time ?t)
                             ;find the charge variables for capacitor going into the branch                            
                             (setof (in-wm (circuit-component ?cap capacitor))
                                    ?cap ?all-caps)
                             
                             (bind ?p-list1 (modify ?path-list1 ?all-caps))
                             
                             (bind ?in-caps (intersection ?all-caps (flatten ?p-list1)))
                             (test (not (equal nil ?in-caps)))
                             (map ?x ?in-caps  
                               (variable ?q-var (at (charge-on ?x) ?t))
                               ?q-var ?q-in-path-vars)
                             
                             ;find the charge variables for capacitor going into the branch  
                             (bind ?p-list2 (modify ?path-list2 ?all-caps))
                             
                             (bind ?out-caps (intersection ?all-caps (flatten ?p-list2)))
                             (test (not (equal nil ?out-caps)))

                             (map ?x ?out-caps
                               (variable ?q-var (at (charge-on ?x) ?t))
                               ?q-var ?q-out-path-vars)
                             )
             :effects (
                       (eqn (= (+ . ?q-in-path-vars) (+ . ?q-out-path-vars))
                            (junction-rule-cap ?path-list1 ?path-list2 ?t))
                       )
             :hint(
                   (point (string "What do you know about the charges on capacitors connected to a junction where two or more branches meet?"))
                   ;(point (string "Find the capacitors closest to the junction on both sides."))
                   (teach (string "The sum of the charges on one side of a junction must equal the sum of the charges on the other side of the junction."))
                   (bottom-out (string "Set the sum of the charges on one side of the junction equal to the sum of the charges on the other side of the junction: Write the equation ~A" ((= (+ . ?q-in-path-vars) (+ . ?q-out-path-vars)) algebra)
		   ))
                   ))


(defoperator find-out-paths ()
             :preconditions (
                             (junction ?jun ?br-list)                        
                             ; find all branches 
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?name ?all-names)

                             ; find all the paths
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?path ?all-paths)

                             (bind ?out-path (get-out-paths ?br-list ?all-names ?all-paths ?jun)) 
                             (test (not (equal nil ?out-path)))
                             )
             :effects (
                       (out-paths ?jun ?out-path)
                       ))


(defoperator find-in-paths ()
             :preconditions (
                             (junction ?jun ?br-list)                        
                             ; find all branches 
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?name ?all-names)

                             ; find all the paths
                             (setof (in-wm (branch ?name given ?dontcare ?path))
                                    ?path ?all-paths)
                             (bind ?in-path (get-in-paths ?br-list ?all-names ?all-paths ?jun))
                             (test (not (equal nil ?in-path)))
                             )
             :effects(
                      (in-paths ?jun ?in-path)
                      ))


(def-psmclass charge-same-caps-in-branch (charge-same-caps-in-branch ?cap ?t) 
  :complexity major
  :english ("Charge on series capacitors")
  :expformat("Using the fact that series capacitors have the same charge.")
  :eqnFormat ("q1 = q2"))

(defoperator charge-same-caps-in-branch-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (charge-on ?cap1) ?t)))
                            (time ?t)
                            (branch ?br-res given ?dontcare1 ?path)
                            (test (member ?cap1 ?path) :test #'equal)
                            ;Are there other capacitors in path
                            (setof (in-wm (circuit-component ?cap capacitor))
                                    ?cap ?all-caps)
                            (bind ?path-caps (intersection ?all-caps ?path))
                            (test (> (length ?path-caps) 1))
                            ;Stops a=b coming out twice for when a & b are both sought
                            (bind ?temp-caps (shrink (second (second ?sought)) ?path-caps))
                            )
             :effects(
                      (eqn-contains (charge-same-caps-in-branch ?temp-caps ?t) ?sought)
                      ))

  
(defoperator charge-same-caps-in-branch (?temp-caps ?t)
             :preconditions (
                            ; (bind ?temp-caps (shrink (second (second ?sought)) ?path-caps))
                             (map ?cap ?temp-caps
                               (variable ?q-var (at (charge-on ?cap) ?t))
                               ?q-var ?q-path-cap-vars)
                             (bind ?adj-pairs (form-adj-pairs ?q-path-cap-vars))
                             (bind ?pair (first ?adj-pairs))
                             (bind ?first (first ?pair))
                             (bind ?second (second ?pair))
                             )
             :effects (
                       (eqn (= ?first ?second) (charge-same-caps-in-branch ?temp-caps ?t))
                       )
             :hint(
                   (point (string "Find two capacitors in series.  Two capacitors are in series when they occur in the same branch."))
                   (teach (string "When two capacitors are in series their charges are the same."))
                   (bottom-out (string "Set the charge ~a equal to the charge ~a." (?first algebra) (?second algebra)))
                   ))

(def-psmclass cap-energy (cap-energy ?cap ?t) 
  :complexity major 
  :english ("The formula for energy stored in a capacitor")
  :expformat("Applying the formula for energy stored in a capacitor to ~A" (nlg ?cap))
  :eqnFormat ("U = (1/2)*Q*V"))

(defoperator cap-energy-contains (?sought)
  :preconditions (
    (any-member ?sought ( (at (charge-on ?cap) ?t)
			  (at (voltage-across ?cap) ?t)
			  (at (stored-energy ?cap) ?t)))
   (circuit-component ?cap capacitor)
   (test (time-pointp ?t))
  ) :effects ( 
      (eqn-contains (cap-energy ?cap ?t) ?sought) 
  ))

(defoperator write-cap-energy (?cap ?t)
  :preconditions (
     (variable ?Q (at (charge-on ?cap) ?t))
     (variable ?V (at (voltage-across ?cap) ?t))
     (variable ?U (at (stored-energy ?cap) ?t))
  )
  :effects (
     (eqn (= ?U (* 0.5 ?Q ?V)) (cap-energy ?cap ?t))
  )
  :hint (
     (teach (string "The electric energy stored in a capacitor can be calculated as one half times the charge on the capacitor times the voltage across the capacitor.  This formula can be combined with the definition of capacitance to calculate the energy from other variables."))
     (bottom-out (string "Write the equation ~A" ((= ?U (* 0.5 ?Q ?V)) algebra)))
  ))

(defoperator define-stored-energy-cap-var (?cap ?t)
  :preconditions (
             (circuit-component ?cap capacitor)
             (bind ?U-var (format-sym "U_~A$~A" (comp-name ?cap 'C) ?t))
  ) :effects (
             (variable ?U-var (at (stored-energy ?cap) ?t))
             (define-var (at (stored-energy ?cap) ?t))
  ) :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Stored Energy." 
         ((at (stored-energy ?cap) ?t) def-np)))
  ))

;;; RC CIRCUITS

(defoperator define-time-var (?t)   
             :preconditions (
                             (bind ?t-var (if (atom ?t) (format-sym "~A" (concatenate 'string "T$" (format nil "~a" ?t)))
                                            (format-sym "~A" (concatenate 'string "T$during_" (format nil "~a" (second ?t))(format nil "~a" (third ?t))))))
                             )
             :effects (
               (variable ?t-var (time ?t))
               (define-var (time ?t))
                       )
               :hint (
(bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting time." ((time ?t) def-np)))
                     ))

;(defoperator define-tinterval-var (?t)   
;             :preconditions (
;                             (bind ?x (second ?t))
;                             (bind ?y (third ?t))
;                             (bind ?t-var (format-sym "~A" (concatenate 'string "T$during_" (format nil "~a" ?x)(format nil "~a" ?y))))
;                             )
;             :effects (
;               (variable ?t-var (tinterval ?t))
;                       ))

(def-psmclass charge-on-capacitor-at-time (charge-on-capacitor-at-time ?cap ?time) 
  :complexity major
  :english ("Charge on RC circuit capacitor")
  :eqnFormat ("q = C*Vb*(1 - exp(-t/R*C))"))

(defoperator charge-on-capacitor-at-time-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (charge-on ?cap) ?t2)
                                                 (duration (during ?t1 ?t2))
						 ; also contains C, Vb
						 ))
			    ; make sure we have a time interval:
			    (time (during ?t1 ?t2))
			    ; !!! following means rc problems must have a switch
                            (switch ?dontcare closed (during ?t1 ?t2))
                            ; Make sure capacitor is empty when switch closes
                            (circuit-component ?cap capacitor)
                            (empty-capacitor ?cap ?t1)
                            )
             :effects(
                      (eqn-contains (charge-on-capacitor-at-time ?cap (during ?t1 ?t2)) ?sought)
                      ))

(defoperator charge-on-capacitor-at-time (?cap ?t1 ?t2)
             :preconditions (
                             (circuit-component ?res resistor)
                             (circuit-component ?bat battery)
                             (variable ?q-var (at (charge-on ?cap) ?t2))
                             (variable ?c-var (capacitance ?cap))
                             (variable ?v-var (at (voltage-across ?bat) (during ?t1 ?t2)))
                             (variable ?t-var (duration (during ?t1 ?t2)))
                             (variable ?r-var (resistance ?res))
                             )
             :effects (
                       (eqn (= ?q-var (* ?c-var ?v-var (- 1 (exp (/ (- ?t-var) (* ?r-var ?c-var))))))
		            (charge-on-capacitor-at-time ?cap (during ?t1 ?t2)))  
                       )
             :hint(
                   (point (string "Write the equation for the charge on the capacitor ~a at time ~a." ?cap (?t2 time)))
                   (bottom-out (string "Write the equation ~a"
                          ((= ?q-var (* ?c-var ?v-var (- 1 (exp (/ (- ?t-var) (* ?r-var ?c-var)))))) algebra) ))
                   ))


(def-psmclass current-in-RC-at-time (current-in-RC-at-time ?res ?time) 
  :complexity major
  :english ("Current in RC circuit")
  :eqnFormat ("I = (Vb/R)*exp(-t/R*C))"))

(defoperator current-in-RC-at-time-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (current-thru ?res) ?t2)
			                         ; also contains V, R, C, t
			                         ))
			    (time (during ?t1 ?t2))
                            (circuit-component ?cap capacitor)
                            (circuit-component ?res resistor)
                            (switch ?dontcare closed (during ?t1 ?t2))
                            ;Make sure capacitor is empty when switch closes
                            (empty-capacitor ?cap ?t1)
                            )
             :effects(
                      (eqn-contains (current-in-RC-at-time ?res (during ?t1 ?t2)) ?sought)
                      ))

(defoperator current-in-RC-at-time (?res ?t1 ?t2)
             :preconditions (
                             (circuit-component ?res resistor)
                             (variable ?i-var (at (current-thru ?res) ?t2))
                             (circuit-component ?bat battery)
                             (variable ?v-var (at (voltage-across ?bat) (during ?t1 ?t2)))
                             (variable ?r-var (resistance ?res))
                             (circuit-component ?cap capacitor)
                             (variable ?c-var (capacitance ?cap))
                             (variable ?t-var (duration (during ?t1 ?t2)))
                             )
             :effects (
                       (eqn (= ?i-var (* (/ ?v-var ?r-var) (exp (/ (- ?t-var) (* ?r-var ?c-var)))))
		            (current-in-RC-at-time ?res (during ?t1 ?t2)))  
                       )
             :hint(
                   (point (string "Write the equation for the current in the RC circuit at time ~a." (?t2 time)))
                   (bottom-out (string "Write the equation ~a"
                      ((= ?i-var (* (/ ?v-var ?r-var) (exp (/ (- ?t-var) (* ?r-var ?c-var))))) algebra) ))
                   ))


(def-psmclass charge-on-capacitor-percent-max (charge-on-capacitor-percent-max ?cap ?time) 
  :complexity minor 
  :english ("RC charge as percent of maximum")
  :eqnFormat ("q = percent*C*V"))

(defoperator charge-on-capacitor-percent-max-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (charge-on ?cap) ?t2)
                                                 ;(at (max-charge ?cap) inf)
						 ; also contains C and V
						 ))
                            (percent-max ?cap ?value ?t2)
                            (circuit-component ?cap capacitor)
                            (switch ?dontcare closed (during ?t1 ?t2))
                            )
             :effects(
                      (eqn-contains (charge-on-capacitor-percent-max ?cap ?t2) ?sought)
                      ))


(defoperator charge-on-capacitor-percent-max (?cap ?t)
             :preconditions (
			     (variable ?C-var (capacitance ?cap))
			     ; use constant Vb defined across charging interval.
			     (circuit-component ?bat battery)	 
			     (time (during ?t0 ?t))
                             (switch ?dontcare closed (during ?t0 ?t))
                             (variable ?V-var (at (voltage-across ?bat) (during ?t0 ?t)))
                             (variable ?q-var (at (charge-on ?cap) ?t))
                             (percent-max ?cap ?fraction ?t)
                             )
	     :effects (
		     (eqn (= ?q-var (* ?fraction ?C-var ?V-var))
		          (charge-on-capacitor-percent-max ?cap ?t))  
                       )
             :hint(
                   (point (string "Write the equation for the charge on the capacitor ~a ~a in terms of the percentage of the maximum charge." ?cap (?t pp)))
                   (point (string "Use the definition of capacitance, remembering that when the charge on the capacitor is at its maximum, the voltage across the capacitor ~a equals the voltage across the battery ~a." ?cap ?bat))
                   (teach (string "The maximum charge on a capacitor equals the capacitance times the battery voltage. You can express the charge at ~a as a fraction of this quantity." (?t pp)))
                   (bottom-out (string "Write the equation ~A"  
		                        ((= ?q-var (* ?fraction ?C-var ?V-var)) algebra) ))
                   ))


#| ; not used yet
(defoperator charge-on-capacitor-max-contains (?sought)
             :preconditions(
                            (any-member ?sought ((time ?t2)))
                            (test (listp ?t2))
                            (percent-max ?cap ?value ?t)
                            (circuit-component ?cap capacitor)
                            (switch ?dontcare closed ?t2)
                            (test (listp ?t2))
                            (bind ?y (third ?t2))
                            (test (tinsidep-include-endpoints ?t ?t2))
                            (variable ?q-var (at (charge-on ?cap) ?y))
                            ;Make sure capacitor is empty when switch closes
                            (empty-capacitor ?cap ?t3)
                            (test (or (and (listp ?t3)(eq (third ?t3)(second ?t2)))
                                      (and (atom ?t3)(eq ?t3 (second ?t2)))))
                            (variable ?qm-var (at (max-charge ?cap) inf))
                            ;(test (eq ?t3 'inf)
                            )
             :effects(
                      (eqn-contains (charge-on-capacitor-max ?cap ?t ?t2 inf) ?sought)
                      ))

(defoperator charge-on-capacitor-max (?cap ?t ?t2 ?t3)
             :preconditions (
                             (variable ?qm-var (at (max-charge ?cap) ?t3))
			     (variable ?q-var (at (charge-on ?cap) ?t))
			     (test (and (atom ?t)(atom ?t3)(not (eq ?t 'inf))))
                             (variable ?c-var (capacitance ?cap))
                             (circuit-component ?res resistor)
                             (variable ?r-var (resistance ?res))
                             (variable ?t-var (time ?t2))
                             (test (listp ?t2))
                             )
             :effects (
                       (eqn (= ?q-var (* ?qm-var (- 1 (exp (/ (- ?t-var) (* ?r-var ?c-var))))))(charge-on-capacitor-max ?cap ?t ?t2 ?t3))  
                       )
             :hint(
                   (point (string "Have you defined a variable to represent the maximum charge on the capacitor?"))
                   (point (string "Write the equation for the charge on the capacitor ~a at time ~a in terms of the maximum charge on the capacitor." ?cap ?t))
                   (bottom-out (string "Write the equation ~a = ~a * (1 - exp(- ~a / (~a * ~a)))." ?q-var ?qm-var ?t-var ?r-var ?c-var))
                   ))

(defoperator charge-on-capacitor-inf-time-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (charge-on ?cap) ?t)
                                                 (at (max-charge ?cap) inf)))
                            (test (atom ?t))
                            (circuit-component ?cap capacitor)
                            (switch ?dontcare closed ?t2)
                            (variable ?qm-var (at (max-charge ?cap) inf))
                            (percent-max ?cap ?value ?t)
                            )
             :effects(
                      (eqn-contains (charge-on-capacitor-inf-time ?cap ?t ?t2 inf) ?sought)
                      ))


(defoperator charge-on-capacitor-inf-time (?cap ?t ?t2 ?t3)
             :preconditions (
                             (variable ?qm-var (at (max-charge ?cap) ?t3))
                             (circuit-component ?bat battery)
                             (variable ?v-var (at (voltage-across ?bat) ?t2))
                             (variable ?c-var (capacitance ?cap))
                             ;(percent-max ?cap ?value ?t)
                             )
             :effects (
                       (eqn (= ?qm-var (* ?c-var ?v-var))(charge-on-capacitor-inf-time ?cap ?t ?t2 ?t3))  

                       )
             :hint(
                   (point (string "Write an equation for the maximum charge on the capacitor ~a." ?cap))
                   (point (string "Use the definition of capacitance, remembering that the voltage across the capacitor ~a equals the voltage across the battery ~a when the charge on the capacitor is maximum." ?cap ?bat))
                   (teach (string "The charge on the capacitor ~a is maximum after the current has fallen to zero.  Therefore, the voltage across the capacitor is equal to the battery voltage."))
                   (bottom-out (string "Write the equation ~a = ~a * ~a." ?qm-var ?c-var ?v-var))
                   ))
|# ; end unused



#| ; don't use this yet
(defoperator time-interval-contains (?sought)
             :preconditions(
                            (any-member ?sought ((time ?t)))
                            (test (listp ?t))
                            (bind ?t1 (second ?t))
                            (bind ?t2 (third ?t))
                           )
             :effects(
                      (eqn-contains (time-interval ?t ?t1 ?t2) ?sought)
                      ))

(defoperator time-points-contains (?sought)
             :preconditions(
                            (any-member ?sought ((time ?t)))
                            (time ?t1)
                            (test (and (atom ?t)(atom ?t1)))
                            (test (not(= ?t ?t1)))
                            (bind ?t2 (if (< ?t ?t1)(cons 'during (cons ?t (list ?t1)))(cons 'during (cons ?t1 (list ?t)))))
                            (bind ?x (if (< ?t ?t1) ?t ?t1))
                            (bind ?y (if (< ?t ?t1) ?t1 ?t))                           )
             :effects(
                      (eqn-contains (time-interval ?t2 ?x ?y) ?sought)
                      ))


(defoperator time-interval (?t ?t1 ?t2)
             :preconditions(
                            (test (and (listp ?t)(atom ?t1)(atom ?t2)))
                            (variable ?t-var (time ?t))
                            (variable ?t-var1 (time ?t1))
                            (variable ?t-var2 (time ?t2))                                      
                            )
             :effects(
                      (eqn (= ?t-var (- ?t-var2 ?t-var1))(time-interval ?t ?t1 ?t2))  
                      )
             :hint(
                   (point (string "Write an equation the expresses the length of the time interval ~a in terms of its beginning and end points, ~a and ~a." ?t ?t1 ?t2))
                   (teach (string "The length of a time interval is defined as the difference between the end point and beginning point."))
                   (bottom-out (string "The time interval ~a equals ~a - ~a." ?t ?t2 ?t1))
                   ))
|# ; end unused

#| ; can use generic inherit-constant-value for this:

(defoperator battery-voltage-same-different-times-contains (?sought)
             :preconditions(
                            (any-member ?sought ((at (voltage-across ?name) ?t)))
                            (time ?t)
                            (time ?t1)
                            (test (listp ?t1))
                            (circuit-component ?name battery)
                            )
             :effects(
                      (eqn-contains (battery-voltage-same-different-times ?t ?t1) ?sought)
                      ))

(defoperator battery-voltage-same-different-time (?t ?t1)
             :preconditions (
                             (circuit-component ?name battery)
                             (variable ?v-var1 (at (voltage-across ?name) ?t))                          
                             (variable ?v-var2 (at (voltage-across ?name) ?t1))
                             )
             :effects (
                       (eqn (= ?v-var1 ?v-var2)(battery-voltage-same-different-times ?t ?t1))
                       )
             :hint(
                   (point (string "The voltage across the battery doesn't change with time."))
                   (point (string "You need to define the voltage across the battery at time ~a." ?t))
                   (teach (string "The voltage across the battery doesn't change so the voltage at a time point is the same as the voltage for any time interval containing that time point."))
                   (bottom-out (string "Set the voltage across the battery at time ~a equal the voltage at time ~a." ?t ?t1))
                   ))
|#


#|
(defoperator define-QMax-var (?cap)   
             :preconditions (
                             (bind ?qm-var (format-sym "QMax_~A$inf" (comp-name ?cap 'C)))
                             )
             :effects (
               (variable ?qm-var (at (max-charge ?cap) ?t))
               (define-var (at (max-charge ?cap) ?t))
                       )
              :hint (
(bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting max charge." ((at (max-charge ?cap) ?t))))
                     ))
|#

;;--------------------------------------------------
;; Inductance
;;--------------------------------------------------

;; define inductance var
(defoperator define-inductance-var (?ind)   
             :preconditions (
			     (circuit-component ?ind inductor)
                             (bind ?L-var (format-sym "~A" (comp-name ?ind 'L)))
                             )
             :effects (
               (variable ?L-var (inductance ?ind))
	       (define-var (inductance ?ind))
                       )
	      :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting inductance." 
            ((inductance ?ind) def-np)))
              ))

;; define variable for time rate-of-change of current, dI/dt
;; Rate of change may defined over either interval, for average rate of change, or instant,
;; just like acceleration.  Problems using average rate of change over interval will 
;; generally use a constant rate of change. 
;;
;; The derivative is a function of a function, so the form for this quantity
;; embeds another quantity form (sans time) as argument:
;;   (at (rate-of-change ?quantity-form) ?time)
;; This rate-of-change form is thus a perfectly generic time derivative notation --
;; any time-varying quantity forms could be used as argument.  We don't have any rules 
;; for dealing generically with derivatives yet, but possibly some could be added in the future.
;; (Of course velocities and accelerations are names for derivatives with their own notation.)
;; For now we only apply this to currents.
;;
;; Unfortunately we have two current quantities -- (current-thru ?comp) and (current-in ?branch)
;; For now we always use current-thru ?comp, since current through the inductor is most germane
;; for these problems.
(defoperator define-dIdt-var (?comp ?time)
             :preconditions (
			     ; might want to require student to define a current var first on Andes interface
                             (bind ?dIdt-var (format-sym "dI_~A_dt$~A" ?comp (time-abbrev ?time)))
                             )
             :effects (
               (variable ?dIdt-var (at (rate-of-change (current-thru ?comp)) ?time))
               (define-var         (at (rate-of-change (current-thru ?comp)) ?time))
                       )
	      :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Current Change Rate." 
                     ((at (rate-of-change (current-thru ?comp)) ?time) var-or-quant) ))
                     ))

;; voltage across an inductor V = -L*dI/dt
(def-psmclass inductor-emf (inductor-emf ?inductor ?time) 
  :complexity major
  :english ("EMF (voltage) across inductor")
  :eqnFormat ("V = -L*dIdt") 
)

(defoperator inductor-emf-contains (?ind ?time)
   :preconditions (
      (circuit-component ?ind inductor)
      (any-member ?sought ( (at (voltage-across ?ind) ?time)
                            (inductance ?ind)
                            (at (rate-of-change (current-thru ?ind)) ?time) ))
      (time ?time)
   )
   :effects ( (eqn-contains (inductor-emf ?ind ?time) ?sought) ))

(defoperator inductor-emf (?ind ?time)
   :preconditions (
       (variable ?V (at (voltage-across ?ind) ?time))
       (variable ?L (inductance ?ind))
       (variable ?dIdt (at (rate-of-change (current-thru ?ind)) ?time))
   )
   :effects (
      (eqn (= ?V (* (- ?L) ?dIdt)) (inductor-emf ?ind ?time))
   )
   :hint (
      (point (string "The voltage across the ends of an inductor is related to the inductance and the rate at which the current through it is changing"))
      (teach (string "The EMF (voltage) produced between the ends of an inductor is proportional to its inductance and the instantaneous time rate of change of the current. The voltage is conventionally shown as negative for increasing positive current to indicate that the induced EMF opposes the change."))
      (bottom-out (string "Write the equation ~A" ((= ?V (* (- ?L) ?dIdt)) algebra) ))
   ))

;; need rule that average rate of change dIdt12 = (I2-I1)/t12

(def-psmclass avg-rate-current-change (avg-rate-current-change ?ind ?time)
  :complexity major 
  :english ("definition of average rate of current change")
  :eqnFormat ("dIdt_avg = (I2 - I1)/t12") 
)

(defoperator avg-rate-current-change-contains (?sought)
    :preconditions (
       (any-member ?sought ( (at (rate-of-change (current-thru ?ind)) (during ?t1 ?t2))
                             (at (current-thru ?ind) ?t2)
                             (at (current-thru ?ind) ?t1) ))
       (time (during ?t1 ?t2))
     )
    :effects (
      (eqn-contains (avg-rate-current-change ?ind (during ?t1 ?t2)) ?sought)
    ))

(defoperator avg-rate-current-change (?ind ?t1 ?t2)
   :preconditions (
       (variable ?dIdt (at (rate-of-change (current-thru ?ind)) (during ?t1 ?t2)))
       (variable ?I2 (at (current-thru ?ind) ?t2))
       (variable ?I1 (at (current-thru ?ind) ?t1))
       (variable ?t12 (duration (during ?t1 ?t2)))
   )
   :effects (
     (eqn (= ?dIdt (/ (- ?I2 ?I1) ?t12)) (avg-rate-current-change ?ind (during ?t1 ?t2)))
   )
   :hint (
     (teach (string "The average rate of change of current over a time interval is simply the difference between the final value of the current and the initial value divided by the time. If the rate of change is constant, then the average rate of change will equal the instantaneous rate of change at any point during the time period"))
     (bottom-out (string "Write the equation ~a" ((= ?dIdt (/ (- ?I2 ?I1) ?t12)) algebra) ))
   ))

;; Perhaps also that instantaneous = average if it is given as constant,
;; for any point in the interval 
;; Also possibly need something dIbranch/dt = dIcomp/dt where Ibranch = Icomp

; energy stored in an inductor

(def-psmclass inductor-energy (inductor-energy ?ind ?t) 
  :complexity major 
  :english ("The formula for energy stored in a inductor")
  :expformat("Applying the formula for energy stored in a inductor to ~A" (nlg ?ind))
  :eqnFormat ("U = 0.5*L*I^2"))

(defoperator inductor-energy-contains (?sought)
  :preconditions (
    (any-member ?sought ( (inductance ?inductor)
			  (at (current-thru ?inductor) ?t)
			  (at (stored-energy ?inductor) ?t)))
  (time ?t)
  (test (time-pointp ?t))
  ) :effects ( 
      (eqn-contains (inductor-energy ?inductor ?t) ?sought) 
  ))

(defoperator write-inductor-energy (?inductor ?t)
  :preconditions (
     (variable ?U (at (stored-energy ?inductor) ?t))
     (variable ?L (inductance ?inductor))
     (variable ?I (at (current-thru ?inductor) ?t))
  )
  :effects (
     (eqn (= ?U (* 0.5 ?L (^ ?I 2))) (inductor-energy ?inductor ?t))
  )
  :hint (
     (teach (string "The electric energy stored in the magnetic field of an a inductor can be calculated as one half times the inductance times the square of the current. ")) 
     (bottom-out (string "Write the equation ~A" ((= ?U (* 0.5 ?L (^ ?I 2)) algebra)) ))
  ))

(defoperator define-stored-energy-inductor-var (?inductor ?t)
  :preconditions (
             (circuit-component ?inductor inductor)
             (bind ?U-var (format-sym "U_~A$~A" (comp-name ?inductor 'L) ?t))
  ) :effects (
             (variable ?U-var (at (stored-energy ?inductor) ?t))
             (define-var (at (stored-energy ?inductor) ?t))
  ) :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Stored Energy." 
         ((at (stored-energy ?inductor) ?t) def-np)))
  ))

;;
;; LR circuits
;;

; Some formula use tau for the LR-time constant L/R. This is a slight difference from RC equations,
; which just write out R*C in the formula. This is done because its more complicated to write L/R in
; the denominator of an exponential and get the grouping right, and also because we have some problems
; that just give the time constant. But might want to change RC formula to do the same.

; What parameters do we need for a time constant var? Really want a circuit-id. The time constant
; could then be either an LR or an RC time constant, depending on the type of circuit.
; In our initial problems there will only be one circuit -- though could have two if we want
; to combine an RC or LR circuit with induction in another circuit, say. 
; But we don't have any circuit IDs in givens. So for now, we always use NIL arg, meaning the default circuit.
(def-psmclass LR-time-constant (LR-time-constant ?ind ?res) 
  :complexity major ; ? 
  :english ("LR time constant")
  :eqnFormat ("$t = L/R"))

(defoperator define--time-constant (?circuit-id) 
   :preconditions (
     (bind ?tau-var (format-sym "tau"))
   )
   :effects (
      (variable ?tau-var (time-constant ?circuit-ID))
      (define-var (time-constant ?circuit-ID))
   )
   :hint (
     (bottom-out (string "Define a variable for the inductive time constant of the circuit by using the Add Variable command and selecting Time Constant" ))
   ))

(defoperator LR-time-constant-contains (?sought ?ind ?res)
   :preconditions (
     (circuit-component ?ind inductor)
     (circuit-component ?res resistor)
     (any-member ?sought ((time-constant NIL)
                          (inductance ?ind)
                          (resistance ?res)))
   )
   :effects (
     (eqn-contains (LR-time-constant ?ind ?res) ?sought)
   ))


(defoperator LR-time-constant (?ind ?res)
  :preconditions (
     (variable ?tau     (time-constant NIL))
     (variable ?L-var   (inductance ?ind))
     (variable ?r-var   (resistance ?res))
  )
  :effects (
     (eqn (= ?tau (/ ?L-var ?R-var)) (LR-time-constant ?ind ?res))
  )
  :hint (
    (point (string "The inductive time constant $t of an LR circuit is a function of the inductance and the resistance in the circuit."))
    (teach (string "The inductive time constant of an LR circuit is equal to the inductance divided by the resistance."))
    (bottom-out (string "Write the equation ~A" ((= ?tau (/ ?L-var ?R-var)) algebra)))
  ))


(def-psmclass LR-current-growth (LR-current-growth ?res ?time) 
  :complexity major
  :english ("Current growth in LR circuit")
  :eqnFormat ("I = Imax*(1 - exp(-t/$t)"))

(defoperator LR-current-growth-contains (?sought)
             :preconditions(
			    ; Following in the givens tells us that circuit and switch
			    ; are configured so current grows during this interval.
                            (LR-current-growth ?branch (during ?t1 ?tf))
                            (any-member ?sought ((at (current-in ?branch) ?t2)
			                         (duration (during ?t1 ?t2))
						 (time-constant NIL)
			                         ))
			    ; this applies to any t2 between t1 and tf
			    (time ?t2)  ; have to bind if sought is tau
			    (test (and (time-pointp ?t2) (< ?t2 ?tf)))
			    (circuit-component ?res resistor)
                            )
             :effects(
                      (eqn-contains (LR-current-growth ?res (during ?t1 ?t2)) ?sought)
                      ))

(defoperator LR-current-growth (?res ?t1 ?t2)
             :preconditions (
                             (LR-current-growth ?branch (during ?t1 ?tf))
                             (circuit-component ?bat battery)
                             (circuit-component ?ind inductor)
                             (variable ?i-var (at (current-in ?branch) ?t2))
			     (variable ?Imax-var (at (current-in ?branch) ?tf))
                             (variable ?t-var (duration (during ?t1 ?t2)))
			     (variable ?tau-var (time-constant NIL))
                             )
             :effects (
                       (eqn (= ?i-var (* ?Imax-var (- 1 (exp (/ (- ?t-var) ?tau-var)))))
		            (LR-current-growth ?res (during ?t1 ?t2)))  
                       )
             :hint(
                   (point (string "After the battery is switched in, the current in an LR circuit rises towards its maximum value as an exponential function of time"))
		   (teach (string "The rising current in an LR circuit at a time equals the maximum current multiplied by a factor of 1 less a decreasing exponential term. The exponential term is given by e raised to a negative exponent (so this term goes to zero over time) of the time over the inductive time constant, tau. In ANDES you express e raised to the x power by the function exp(x)."))
                   (bottom-out (string "Write the equation ~a"
                      ((= ?i-var (* ?Imax-var (- 1 (exp (/ (- ?t-var) ?tau-var))))) algebra) ))
                   ))

; Formula Imax = Vb/R is true for both LR growth and decay, but we treat as two psms because we
; show different variables in the two cases, "If" for growth vs. "I0" for decay.
; Whether it is best to treat as two psms or one w/two ways of writing the equation depends 
; mainly on how we want our review page to look.

(def-psmclass LR-growth-Imax (LR-growth-Imax ?time)
  :complexity major 
  :english ("LR growth final current")
  :eqnFormat ("Imax = Vb/R"))

(defoperator LR-growth-Imax-contains (?sought)
   :preconditions (
      ; have to be told we have LR-current growth over interval
      (LR-current-growth ?branch (during ?ti ?tf))
      (any-member ?sought ( (at (current-in ?branch) ?tf)
                            (voltage-across ?bat) (during ?ti ?tf))
                            (resistance ?res) )
  )
   :effects (  (eqn-contains (LR-growth-Imax (during ?ti ?tf)) ?sought) ))

(defoperator LR-growth-Imax (?ti ?tf)
  :preconditions (
          (LR-current-growth ?branch (during ?ti ?tf))
	  (circuit-component ?res resistor)
	  (circuit-component ?bat battery)
	  (variable ?Imax-var (at (current-in ?branch) ?tf))
          (variable ?v-var (at (voltage-across ?bat) (during ?ti ?tf)))
          (variable ?r-var (resistance ?res))
  )
  :effects ( (eqn (= ?Imax-var (/ ?v-var ?r-var)) (LR-growth-Imax (during ?ti ?tf))) )
  :hint (
      (point (string "What must the maximum value of the current be?"))
      (point (string "At its maximum value, the current in an LR circuit is nearly constant, so there is no EMF due to the inductor. Since the only source of EMF at this time is the battery, Ohm's Law V = I*R determines the current through the resistor to be the battery voltage divided by resistance."))
      (bottom-out (string "Write the equation ~a" ((= ?Imax-var (/ ?v-var ?r-var)) algebra)))
  ))

; LR circuit decay:

(def-psmclass LR-current-decay (LR-current-decay ?res ?time) 
  :complexity major
  :english ("Current decay in LR circuit")
  :eqnFormat ("I = I0*exp(-t/$t)"))

(defoperator LR-current-decay-contains (?sought)
             :preconditions(
			    ; Following in the givens tells us that circuit and switch
			    ; are configured so current decays during this interval.
                            (LR-current-decay ?branch (during ?t1 ?tf))
                            (any-member ?sought ((at (current-in ?branch) ?t2)
						 (duration (during ?t1 ?t2))
			                         ; also contains tau and I0
			                         ))
			     ; this applies to any t2 between t1 and tf
			    (test (and (time-pointp ?t2) (<= ?t2 ?tf)))
			    (circuit-component ?res resistor)
                            )
             :effects(
                      (eqn-contains (LR-current-decay ?res (during ?t1 ?t2)) ?sought)
                      ))

(defoperator LR-current-decay (?res ?t1 ?t2)
             :preconditions (
                             (LR-current-decay ?branch (during ?t1 ?tf))
                             (circuit-component ?bat battery)
                             (circuit-component ?ind inductor)
                             (variable ?i-var (at (current-in ?branch) ?t2))
			     (variable ?I0-var (at (current-in ?branch) ?t1))
                             (variable ?t-var (duration (during ?t1 ?t2)))
			     (variable ?tau-var (time-constant NIL))
                             )
             :effects (
                       (eqn (= ?i-var (* ?I0-var (exp (/ (- ?t-var) ?tau-var))))
		            (LR-current-decay ?res (during ?t1 ?t2)))  
                       )
             :hint(
                   (point (string "When the battery is switched out, the initial current in an LR circuit decays towards zero as an exponential function of time"))
		   (teach (string "The decaying current in an LR circuit at a time equals the initial current multipled by a factor of a decreasing exponential term. The exponential term is given by e raised to a negative exponent (so this term goes to zero over time) of the time over the inductive time constant, tau. In ANDES you express e raised to the x power by the function exp(x)."))
                   (bottom-out (string "Write the equation ~a"
                      ((= ?i-var (* ?I0-var (exp (/ (- ?t-var) ?tau-var)))) algebra) ))
                   ))

(def-psmclass LR-decay-Imax (LR-growth-Imax ?time)
  :complexity major 
  :english ("LR decay initial current")
  :eqnFormat ("I0 = Vb/R"))

(defoperator LR-decay-Imax-contains (?sought)
   :preconditions (
      ; have to be told we have LR-current decay over interval
      (LR-current-decay ?branch (during ?ti ?tf))
      (any-member ?sought ( (at (current-in ?branch) ?ti)
                            (voltage-across ?bat) (during ?ti ?tf))
                            (resistance ?res) )
  )
   :effects (  (eqn-contains (LR-decay-Imax (during ?ti ?tf)) ?sought) ))

(defoperator LR-decay-Imax (?ti ?tf)
  :preconditions (
          (LR-current-decay ?branch (during ?ti ?tf))
	  (circuit-component ?res resistor)
	  (circuit-component ?bat battery)
	  (variable ?Imax-var (at (current-in ?branch) ?ti))
          (variable ?v-var (at (voltage-across ?bat) (during ?ti ?tf)))
          (variable ?r-var (resistance ?res))
  )
  :effects ( (eqn (= ?Imax-var (/ ?v-var ?r-var)) (LR-decay-Imax (during ?ti ?tf))) )
  :hint (
      (point (string "What is the initial value of the current when the switch is opened?"))
      (point (string "At its maximum value, the current in an LR circuit is nearly constant, so there is no EMF due to the inductor. Since the only source of EMF at this time is the battery, Ohm's Law V = I*R determines the current through the resistor to be the battery voltage divided by resistance."))
      (bottom-out (string "Write the equation ~a" ((= ?Imax-var (/ ?v-var ?r-var)) algebra)  ))
  ))


;;; Power "through" component = V*I
(def-psmclass electric-power (electric-power ?comp ?t)
   :complexity major
   :english ("the formula for electric power")
   :eqnFormat ("P = I*V"))

(defoperator electric-power-contains (?sought)
    :preconditions ( 
       (any-member ?sought ( (at (voltage-across ?comp) ?t) 
                             (at (current-thru ?comp) ?t)
			     (at (electric-power ?comp) ?t) )) ; omit "agent" for electric power ?
      ; we only apply this to batteries and resistors
      (in-wm (circuit-component ?comp ?comp-type))
      (test (member ?comp-type '(battery resistor)))
    ) 
    :effects ( (eqn-contains (electric-power ?comp ?t) ?sought) ))

(defoperator electric-power (?comp ?t)
     :preconditions (
         (variable ?V (at (voltage-across ?comp) ?t) )
         (variable ?I (at (current-thru ?comp) ?t))
         (variable ?P  (at (electric-power ?comp) ?t)) 
     )
     :effects (
        (eqn (= ?P (* ?V ?I)) (electric-power ?comp ?t))
     )
     :hint (
	(point (string "Power specifies the rate at which energy is transferred. Think about how the rate of energy transferred as charge moves across a component can be related to the current and the difference in electric potential across the endpoints."))
	(teach (string "The potential difference (voltage) between two points is defined as the work needed to move a unit charge between those points. Power is the rate of doing work. For a battery with EMF V to produce a current I, it must move I unit charges per second through a potential difference of V, so its power output equals V*I. The same formula will give the amount of power DRAWN by a resistor as charge moves through it from higher to lower potential, when the electrical potential energy is converted to other forms of energy such as heat or light and dissipated from the circuit.)"))
        (bottom-out (string "Write the equation ~a" ((= ?P (* ?V ?I)) algebra)))
     ))

(defoperator define-electric-power-var (?b ?t)
  :preconditions (
    (bind ?power-var (format-sym "power_~A_~A" (body-name ?b) (time-abbrev ?t)))
 ) :effects (
   (define-var (at (electric-power ?b) ?t))
   (variable ?power-var (at (electric-power ?b) ?t))
 )
 :hint (
   (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting power." ((at (electric-power ?b) ?t) def-np) ))
 ))


