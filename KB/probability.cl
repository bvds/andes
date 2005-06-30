;; 	Probability family of equations.
;; For a vector psm with subsidiary compo-eqns, we use major
;; vector psm id as group id
;; 	probability theory
    
(def-psmgroup ProbabilityTheory	
    :doc "The probability principals of the events, such as: the addition theorem, de Morgan law."
    :english ("The probability theory."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   complement-theorem                                                                                                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   complement-theorem                                                                                                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass complement-theorem(complement-theorem  ?events )
     :group ProbabilityTheory
     :complexity minor
     :english ("the complement theorem")
     :EqnFormat ("p(A)+ p(~A) =1 "))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability a)))
     '((task ab)
             (event-objects (a b))
             (type ab complement-not) 
             (independent-events (a b))                      
             (given (probability a)  0.1)
             (given (probability b)  0.05)   ))
  ------------------------------------------------------------------------------------------- |#     
(defoperator complement-theorem-contains (?sought)
  :preconditions
  ((sample-space ?tevents)
   (mutually-exclusive-events ?tevents)
   (bind ?events (sort-list ?tevents))
   (test (member ?sought ?events :test #'equalp))
	        )
  :effects
  ((eqn-contains (complement-theorem  ?events) (probability  ?sought))))

(defoperator complement-theorem-contain_nots (?sought)
  :preconditions
  ( (task ?task)
    (type ?task probability)  
    (type ?task complement-not)
    (any-member ?sought ( 
                        (probability ?event)                      	
                        ))  
    (bind ?events (sort-list (list ?event (eno-event ?event)))) 
  )
  :effects
  ((eqn-contains (complement-theorem  ?events) ?sought)))
  
#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
 (qsolve-for '((eqn ?e (complement-theorem (a b c))))
      '((variable pa  (probability a)) (variable pb (probability b)) (variable pc  (probability c))))
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-complement-theorem2 (?events)
  :specifications "If the goal is to write the complement theorem equation,
   the subgoals are to define the probability of the events in the sample,
   space; and then write the sum of them is equalp to 1. "
  :preconditions
  ( 
    (map ?event ?events
    (variable ?quant (probability ?event))
    ?quant ?compo-quants)
   )
  :effects
  ( (eqn (= (+ . ?compo-quants) 1) (complement-theorem ?events))     
     )
  :hint
  ((point (string "Since ~a is equal to sample space, You can apply the complement theorem" (?events nlg-or-single-events)))
   (point (string "The definition of complement theorem is: if A1, A2, ... An are events and A1$ÈA2$È...$ÈAn is equal to sample space, then we have p(A1$ÈA2$È...$ÈAn)=1. Here we know that ~a is equal to the sample space,  you can apply definition of complement theorem  " (?events nlg-or-single-events)))
   (bottom-out (string "Write the equation ~a "	 ((= 1 (+ . ?compo-quants)) algebra)))
   ))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   addition-theorem                                                                                                                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   addition-theorem                                                                                                                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-psmclass addition-theorem(addition-theorem  ?event )
     :group ProbabilityTheory
     :complexity major
     :english ("the addition theorem for two events")
     :EqnFormat ("p(A$È B) = p(A)+p(B)-p(A$ÇB)"))

#|test: ------------------------------------------------------------------------------------------
 (qsolve-for '((eqn-contains ?e  (probability (eand a b)))) 
 '((task car) (type car probability) (variable ?q (probability (eor a b))) (variable ?q (probability (eand a b))) ))
 -------------------------------------------------------------------------------------------|#  
(defoperator addition-theorem-contains (?sought)
  :preconditions
    ( (task ?task)
      (type ?task ?something)
      (test (string= ?something 'probability))  
      (reasonable-event ?e)
      (test (equalp (length (eor-list ?e)) 2))
      (bind ?elist (eor-list ?e))
      (bind ?q1 (first ?elist)) 
      (bind ?q2 (second ?elist))  
      (bind ?q1oq2 (eoperator-event 'eor  ?elist))
      (bind ?q1q2 (eoperator-event 'eand  ?elist))
      (any-member ?sought ( 
                       (probability ?q1)
                        (probability ?q2)
                        (probability ?q1oq2)
                        (probability ?q1q2)                       	
                        ))   
  )
  :effects
  ((eqn-contains (addition-theorem  (eor ?q1 ?q2))  ?sought)))           



#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
 (qsolve-for '((eqn ?eqn (addition-theorem (eor a b))))
 '((variable pa  (probability a)) (variable pb (probability b)) (variable paob (probability (eor a b))) (variable pab (probability (eand a b)))) )
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#  

(defoperator write-addition-theorem ((eor ?e1 ?e2))
  :preconditions
  ((variable ?quant1 (probability ?e1))
   (variable ?quant2 (probability ?e2))
   (variable ?quant1o2 (probability (eor ?e1 ?e2)))
   (variable ?quant12 (probability (eand ?e1 ?e2)))   
   )
  :effects
  ((eqn (=  (- (+ ?quant1 ?quant2) ?quant12) ?quant1o2) (addition-theorem (eor ?e1 ?e2)))
     )
  :hint
  ((point (string "Please apply the addition theorem for two events: event ~a and event ~a" (?e1 nlg-event) (?e2 nlg-event) ))
   (point (string "The definition of addition theorem for two events:  for two events A and B,  P(A$ÈB)=P(A)+ P(B)- P(A$ÇB). Please apply the addition theorem on event ~a and event ~a"  (?e1 nlg-event) (?e2 nlg-event) ))  
   (bottom-out (string "Write the equation ~a "   ((= ?quant1o2 (- (+ ?quant1 ?quant2) ?quant12)) algebra)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   addition-theorem3                                                                                                                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   addition-theorem3                                                                                                                                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-psmclass addition-theorem3(addition-theorem3  ?event )
     :group ProbabilityTheory
     :complexity major
     :english ("the addition theorem for three events")
     :EqnFormat ("p(A$È B$È C) = p(A) + p(B) +p(C)-p(A$ÇB)-p(B$ÇC)-p(A$ÇC)+p(A$È B$È C) "))

    

#|test: ------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains (addition-theorem3 ?e)  (probability c))) 
'((task abc)
             (event-objects (a b c))
             (type abc probability) 
             (type (a b c) independent-events)                      
             (given (probability a)  1/3)
             (given (probability b)  1/4)
             (given (probability (eor a b c))  3/4)     ))
 -------------------------------------------------------------------------------------------|#  
(defoperator addition-theorem3-contains (?sought)
  :preconditions
    ((task ?task)
     (type ?task ?something)
     (test (string= ?something 'probability))  
     (reasonable-event ?e) 
     (test (equalp (length (eor-list ?e)) 3)) 
     (bind ?elist (eor-list ?e))   
      (bind ?q1 (first ?elist)) 
      (bind ?q2 (second ?elist)) 
      (bind ?q3 (third ?elist)) 
      (bind ?q1oq2oq3 (eoperator-event 'eor  (list ?q1 ?q2 ?q3)))
      (bind ?q1q2 (eoperator-event 'eand  (list ?q1 ?q2)))
      (bind ?q2q3 (eoperator-event 'eand  (list ?q2 ?q3)))
      (bind ?q1q3 (eoperator-event 'eand  (list ?q1 ?q3)))
      (bind ?q1q2q3 (eoperator-event 'eand  (list ?q1 ?q2 ?q3)))
     (any-member ?sought ( 
                        (probability ?q1)
                        (probability ?q2)
                        (probability ?q3)
                        (probability ?q1oq2oq3)
                        (probability ?q1q2)  
                        (probability ?q2q3) 
                        (probability ?q1q3) 
                        (probability ?q1q2q3)                     	
                        ))      
  )
  :effects
  ((eqn-contains (addition-theorem3  ?q1oq2oq3)  ?sought)))     



#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------
 (qsolve-for '((eqn ?e (addition-theorem3 (eor a b c))))
             '((task abc)
             (event-objects (a b c))
             (type abc probability) 
             (type (a b c) independent-events)                      
             (given (probability a)  1/3)
             (given (probability b)  1/4)
             (given (probability (eor a b c))  3/4)     ))
 ------------------------------------------------------------------------------------------------------------------------------------------------------------|#  
 (defoperator write-addition-theorem3 ((eor ?e1 ?e2 ?e3))
  :preconditions
  (  (variable ?quant1 (probability ?e1))
     (variable ?quant2 (probability ?e2))
     (variable ?quant3 (probability ?e3))
     (variable ?quant1o23 (probability (eor ?e1 ?e2 ?e3)))
     (variable ?quant12 (probability (eand ?e1 ?e2)))  
     (variable ?quant13 (probability (eand ?e1 ?e3)))   
     (variable ?quant23 (probability (eand ?e2 ?e3)))   
     (variable ?quant123 (probability (eand ?e1 ?e2 ?e3)))    
   )
  :effects
  ((eqn (=  (+ (- (- (- (+ (+ ?quant1 ?quant2) ?quant3) ?quant12) ?quant13) ?quant23) ?quant123) ?quant1o23) (addition-theorem3 (eor ?e1 ?e2 ?e3)))
     )
  :hint
  ((point (string "Please apply the addition theorem for three events on event ~a, ~a and ~a " (?e1 nlg-event) (?e2 nlg-event) (?e3 nlg-event)))
   (point (string "The definition of addition theorem for three events:  for three events A, B and C,  P(A$ÈB$ÈC)=P(A)+ P(B)+p(C)-P(A$ÇB)-P(B$ÇC)-P(B$ÇC)+P(A$ÇB$ÇC).  Please apply the addtion theorem on event ~a, ~a and ~a " (?e1 nlg-event) (?e2 nlg-event) (?e3 nlg-event)))  
   (bottom-out (string "Write the equation ~a " ((= ?quant1o23 (+ (- (- (- (+ (+ ?quant1 ?quant2) ?quant3) ?quant12) ?quant13) ?quant23) ?quant123)) algebra)))  
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   decomposition-law                                                                                                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   decomposition-law               pb=pa/\b + pb/\na                                                                                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass decomposition-law(decomposition-law  ?event ?events )
     :group ProbabilityTheory
     :complexity major
     :english ("the decomposition law")
     :EqnFormat ("p(B)=p(A$ÇB)+p(~A $Çb)"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?priciple  (probability (eand  a b))))
     '((sample-space (a na)) (mutually-exclusive-events (a na)) (variable pb (probability b))  (task car) (type car probability)))
  ------------------------------------------------------------------------------------------- |#     
(defoperator decomposition-law-contains (?sought)
  :preconditions
  (     (task ?task)
        (type ?task ?something)
        (test (string= ?something 'probability))        
        (sample-space ?events)
        (mutually-exclusive-events ?events) 
        (reasonable-event ?event)
        (test (atomeventp ?event))
        (bind ?event1 (eoperator-event 'eand (list (first ?events) ?event)))   
        (bind ?event2 (eoperator-event 'eand (list (second ?events) ?event)))  
        (given (probability  ?event)  ?mv)
        (given (probability  ?tevent)  ?v)
        (test (or (string= (event-name ?tevent) (event-name ?event1))
                  (string= (event-name ?tevent) (event-name ?event2))))  
        (any-member ?sought (
		 (probability ?event)
		 (probability ?event1)
		 (probability ?event2))))
  :effects
  ((eqn-contains (decomposition-law ?event ?events) ?sought)))

#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
 (qsolve-for '((eqn ?eqn (decomposition-law a (b nb))))
      '((variable pa  (probability a)) (variable pab (probability (eand a b))) (variable panb (probability (eand a nb))) ))
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-decomposition-law (?event ?events)
  :preconditions
  ( 
   (bind ?event1 (eoperator-event 'eand (list (first ?events) ?event)))
   (bind ?event2 (eoperator-event 'eand (list (second ?events) ?event)))  
   (bind ?e1 (first ?events))
   (bind ?e2 (second ?events))  
   (variable ?quanta (probability ?event))
   (variable ?quantab (probability ?event1))
   (variable ?quantanb (probability ?event2))      
   )
  :effects
  ( (eqn (= ?quanta  (+ ?quantab ?quantanb)) (decomposition-law ?event ?events))     
     )
  :hint
   ((point (string "Can you write an equation of the decomposition law?"))
   (point (string "The probability of ~a is equal to the probaility of ~a$Ç~a plus the probability of ~a$Ç~a." 
		  ?event ?event ?e1  ?event ?e2))  
   (bottom-out (string "Write the equation ~A" ((= ?quanta  (+ ?quantab ?quantanb)) algebra)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   de-morgan                                                                                                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   de-morgan                                                                                                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass de-morgan(de-morgan  ?event)
     :group ProbabilityTheory
     :complexity major
     :english ("the De-Morgan law")
     :EqnFormat ("p(~(A$ÇB))=p(~A$È ~B); p(~(A$ÇB))=p(~A$È ~B)"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability (eno (eand b c (eno a))))))
     '((task car) (type car probability) (variable pnboa (probability (eor a (eno b) (eno c))))  ))
     
(qsolve-for '((eqn-contains ?e  (probability (eno (eor b c (eno a))))))
     '((task car) (type car probability) (variable pnboa (probability (eand a (eno b) (eno c))))  ))
  ------------------------------------------------------------------------------------------- |#     
(defoperator de-morgan-contains (?sought)
  :preconditions
  (     (task ?task)
        (type ?task probability)
        (type ?something complement-not)
         (reasonable-event ?event)     
        (test (or (> (length (eor-list ?event)) 1)
                     (> (length (eand-list ?event)) 1)))
        (bind ?nevent (if (> (length (eor-list ?event)) 1) (cons 'eno (list (cons 'eand (eno-list (eor-list ?event))))) 
               (cons 'eno (list  (cons 'eor (eno-list (eand-list ?event)))))))
        (any-member ?sought (
		 (probability ?event)
		 (probability ?nevent)		
	        )))
  :effects
  ((eqn-contains (de-morgan ?event) ?sought)))   

#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
 (qsolve-for '((eqn ?e (de-morgan (eor a (eno b) (eno c))))) 
           '((variable pnbna (probability (eno (eand b c (eno a))))) (variable pboa (probability (eor a (eno b) (eno c))))  ))
 (qsolve-for '((eqn ?e (de-morgan (eand a (eno b) (eno c))))) 
           '((variable pnbna (probability (eno (eor b c (eno a))))) (variable pboa (probability (eand a (eno b) (eno c))))  ))
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-de-morgan (?event)
  :preconditions
  ( 
    (bind ?nevent (if (> (length (eor-list ?event)) 1) (cons 'eno (list (cons 'eand (eno-list (eor-list ?event))))) 
               (cons 'eno (list  (cons 'eor (eno-list (eand-list ?event)))))))
    (variable ?quant (probability  ?event))
    (variable ?quantn (probability ?nevent))
   )
  :effects
  ( (eqn (= ?quant  ?quantn) (de-morgan ?event))     
     )
  :hint
  ((point (string "Please apply the de morgan's law on event ~a" (?event nlg-event)))
   (point (string "De Morgan's law is: p(~~(A1$ÇA2$Ç...$ÇAn))=p(~~A1$È~~A2$È,...,$È~~An);or p(~~(A1$ÈA2$È...$ÈAn))=p(~~A1$Ç~~A2$Ç... $Ç~~An). Please apply De Morgan's law on event ~a" (?event nlg-event))) 
   (bottom-out (string "Write the equation ~a " ((= ?quant  ?quantn)  algebra)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   Mutually Exclusive Events                                                                                                                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;   Mutually Exclusive Events                                                                                                                                                           ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass mutually-exclusive-events(mutually-exclusive-events  ?event)
     :group ProbabilityTheory
     :complexity minor
     :english ("the definition of the mutually exclusive events ")
     :EqnFormat ("p(A$Ç~A)=0 "))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?principles  (probability (eand a b))))
     '((mutually-exclusive-events (a b c))  (variable pab (probability (eand a b)))  ))
  ------------------------------------------------------------------------------------------- |#     
(defoperator mutually-exclusive-events-contains (?sought)
  :preconditions
  (     (mutually-exclusive-events ?events)
        (any-member ?sought (
		 (probability ?event)		
		 ))
        (test (and (> (length (eand-list ?event)) 1)
		   (subsetp ?events (eand-list ?event)  :test #'(lambda (s c) (equalp s c)) )))
	;; If only subsetq will not test some example like ex132. 
	)
  :effects
  ((eqn-contains (mutually-exclusive-events ?event) ?sought)))
  
  
#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
 (qsolve-for  '((eqn ?e (mutually-exclusive-events (eand b c (eno a))))) 
                '((variable pbcna (probability (eand b c (eno a))))  ))
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-mutually-exclusive-events (?event)
  :preconditions
  (
   (mutually-exclusive-events ?events)
   (test (and (> (length (eand-list ?event)) 1)
	      (subsetp ?events (eand-list ?event)  :test #'(lambda (s c) (equalp s c)) )))
   (variable ?quant (probability  ?event))
   )
  :effects
  ( (eqn (= ?quant 0) (mutually-exclusive-events ?event))     
     )
  :hint
  ((point (string "Please apply the definition of mutually exclusive events on the event ~a" (?event nlg-event)))
   (point (string "Definition of mutually exclusive events: if A1$ÇA2$Ç...$ÇAn is equal to empty event, then p(A1$ÇA2$Ç...$ÇAn)=0. ~a Please apply the definition of mutual exclusive events on event ~a " ((?event ?events) nlg-mutually-exclusive-event) (?event nlg-event)))  
   (bottom-out (string "Write the equation ~a " ((= 0  ?quant)  algebra)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   Conditional Probability                                                                                                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   Conditional Probability          p(B|A)=p(A/\B)/p(A)                                                                                                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass conditional-probability(conditional-probability  ?event)
     :group ProbabilityTheory
     :complexity major
     :english ("The definition of Conditional Probability ")
     :EqnFormat ("p(A|G)=p(A$Ç\G)/p(G)"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability (eand a b))))
     '((task car) (type car conditional-probability)
       (variable pbga (probability  (given b a)))
      (method a 1) (method b 2)))
  ------------------------------------------------------------------------------------------- |#     
(defoperator conditional-probability-contains (?sought)
  :preconditions
  (    	(task ?task)
        (type ?task conditional-probability)
        (reasonable-event ?event)
        (test (and (listp ?event) (string= (first ?event) 'given)))
        (bind ?event1 (giver ?event))
        (bind ?event2 (givee ?event))
	(test (eand-list ?event1))
	(test (eand-list ?event2)) 
	(bind ?eventand (eoperator-event  'eand  (union (member-list ?event1) (member-list ?event2))))
;;	(earlier-than ?event1 ?event2)
        (not (included ?event2 ?event1))  
        (bind ?tevent2 (first (event-list  ?event2)))
        (bind ?ntevent2 (cons 'eno (event-list ?event2)))        
       ;; (not (given (probability  (given ?tevent2 ?event1))  ?v))
       ;; (not  (given (probability (given ?ntevent2 ?event1))  ?nv))
       ;; (not (eqn ?eqn1 (bayes-rule (given ?event1 ?event2))) )
       ;; (not (and (eqn ?eqn2 (total-probability-theorem ?event2 ?events))  (member ?event1 ?events)) ) 
        (any-member ?sought (
                    ;;(probability ?event1)
                    (probability ?eventand)			
		    (probability ?event)				
	        ))               
       )
  :effects
  ((eqn-contains (conditional-probability ?event) ?sought)))  

#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Should apply for this this: 
 (qsolve-for '((eqn ?t (conditional-probability (given b a))))
       '((task car) (event-objects (a b))(type car conditional-probability)
       (variable pbga (probability  (given b a)))
      (method a 1) (method b 2)))
      
    '( (variable pa (probability a))  (variable pbga (probability (given b a)))
     (variable pab (probability (eand a b))) ))   
           
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-conditional-probability (?event)
  :specifications "If the goal is to write the definition of the probability,
   the subgoals are to define variables for probabilities,
   then write p(e) =fcase(e)/pcase(e). "
  :preconditions
  ( (bind ?event1 (giver ?event))
    (bind ?event2 (givee ?event))
    (bind ?eventand (eoperator-event  'eand  (member-list ?event)))
    (variable ?p1 (probability ?event1))
    (variable ?pand (probability ?eventand))
    (variable ?pgiven (probability ?event))  
   )
  :effects
  ( (eqn (=  (/ ?pand ?p1) ?pgiven) (conditional-probability ?event))     
     )
  :hint
  ((point (string "Please apply the definition of mutually exclusive events on the event ~a " (?event nlg-event)))
   (point (string "The definition of conditional probability is: for event A and B, p(A|B)=p(A$ÇB)/p(B).  Please apply the definition of conditional event on the event ~a"  (?event nlg-event)))  
   (bottom-out (string "Write the equation ~a " ((= ?pgiven (/ ?pand ?p1))  algebra)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   Total Probability Law.                                                                                                                                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   Total Probability Law.         p(B)=p(A)*p(B|A)+p(~A)*p(B|~A)                                                                                                                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass total-probability-theorem(total-probability-theorem  ?event ?events)
     :group ProbabilityTheory
     :complexity major
     :english ("The Law Of Total Probability")
     :EqnFormat ("p(B)=(p(B|A)*p(A)+ p(B|~A)*p(~A))"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability b)))
     '((task car) (mutually-exclusive-events (a (eno a)))
     (sample-space (a (eno a))) (type car conditional-probability)
     (method a 1) (method b 2)))
  -------------------------------------------------------------------------------------------
(defoperator total-probability-theorem-contains (?sought)
  :preconditions
  (     (mutually-exclusive-events ?events)
        (sample-space ?events)
        (task ?task)
        (stat-lite-count-on ?task) 
        (type ?something conditional-probability)
        (any-member ?sought (
                    (probability ?event)       
        )) 
        (test (equalp (intersection (member-list ?event) ?events) nil)) 
        (method ?event1 ?estp1)   
        (test (member ?event1 ?events))      
        (map ?event2 (event-list ?event)
            (method ?event2 ?estp2)
                ?estp2 ?estp-list2s)                
        (test (< ?estp1 (if (not (equal ?estp-list2s nil)) (apply #'min ?estp-list2s) 9)))            
       )
  :effects
  ((eqn-contains (total-probability-theorem ?event ?events) ?sought)))
|#     



(defoperator total-probability-theorem-contains2 (?sought)
  :preconditions
  (     (mutually-exclusive-events ?events)
        (sample-space ?events)
        (task ?task)
        (not (conditional-independent-events ?someevents ?s))  
        (type ?something conditional-probability)
        (any-member ?sought (
                    (probability ?event)       
        )) 
        (test (atomeventp ?event))
        (bind ?tevent (first (event-list ?event)))
        (bind ?ntevent (cons 'eno (event-list ?event)))
        (not (given (probability  ?tevent)  ?v))
        (not  (given (probability  ?ntevent)  ?nv))
        (test (equalp (intersection (member-list ?event) ?events) nil)) 
       )
  :effects
  ((eqn-contains (total-probability-theorem ?event ?events) ?sought)))

(defoperator total-probability-theorem-contains2 (?sought)
  :preconditions
  (     (mutually-exclusive-events ?events)
        (sample-space ?events)
        (task ?task)
        (not (stat-lite-count-on ?task) )  
        (type ?something conditional-probability)
	(conditional-independent-events ?someevents ?s)
        (any-member ?sought (
                    (probability ?event)       
		    )) 
	(test (not (and (not (equalp (intersection (event-list ?events) ?someevents) nil)) (not (equalp (intersection ?events ?someevents) nil)))))
        (test (atomeventp ?event))
        (bind ?tevent (first (event-list ?event)))
        (bind ?ntevent (cons 'eno (event-list ?event)))
        (not (given (probability  ?tevent)  ?v))
        (not  (given (probability  ?ntevent)  ?nv))
        (test (equalp (intersection (member-list ?event) ?events) nil)) 
       )
  :effects
  ((eqn-contains (total-probability-theorem ?event ?events) ?sought))) 
  
#| test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Should apply for this this: 
 (qsolve-for '((eqn ?t (total-probability-theorem b)))
    '( (variable pa (probability a))  (variable pbga (probability (given b a))) (mutually-exclusive-events (a (eno a)))
     (sample-space (a (eno a))) (variable pna (probability (eno a)))  (variable pbgna (probability (given b (eno a))))
      (variable pb (probability b)) ))             
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-total-probability-theorem (?event ?events)
  :preconditions
  ( (variable ?pevent (probability ?event))
    (map ?e ?events
            (expression ?exp (total-probability ?e ?event))
                ?exp ?exps)   
  )
  :effects
  ( (eqn (=  (+ . ?exps) ?pevent) (total-probability-theorem ?event ?events))     
     )
  :hint
  ((point (string "Please apply the total probability theorem on the event ~a" (?event nlg-event)))
   (point (string "The definition of total probability theorem is: if A1, A2,...An is a collection of mutually exclusive and exhaustive events, for any event B, we have p(B)=p(B|A1)*(A1)+p(B|A2)*p(A2)+...+p(B|An)*p(An). Here we know that ~a is a collection of mutually exclusive and exhaustive events, then we can can apply the total theorem on the event ~a " (?events nlg-single-events) (?event nlg-event)))
   (bottom-out (string "Write the equation ~a " ((= ?pevent (+ . ?exps))   algebra)))
   ))

#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Should apply for this this: 
 (qsolve-for '((expression ?t (total-probability ?e b)))
    '( (variable pa (probability a))  (variable pbga (probability (given b a))) (mutually-exclusive-events (a (eno a)))
     (sample-space (a (eno a))) (variable pna (probability (eno a)))  (variable pbgna (probability (given b (eno a))))
     (variable pb (probability b)) ))   
           
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   

(defoperator write-probability-theorem (?event1 ?event2)
  :preconditions
  (
    (variable ?pand (probability (given ?event2 ?event1))) 
    (variable ?p1 (probability  ?event1))           
   )
  :effects
  ( (expression (* ?p1 ?pand) (total-probability ?event1 ?event2))     
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;  Bayes Rule                                                                                                                                           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   Bayes Rule.       p(H|B)=p(H)*p(B|H)/{p(H)*p(B|H)+p(L)*p(B|L)}                                                                                                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass bayes-rule(bayes-rule  ?event)
     :group ProbabilityTheory
     :complexity major
     :english ("Bayes' Theorem ")
     :EqnFormat ("p(H|B)=p(H)*p(B|H)/{p(H)*p(B|H)+p(L)*p(B|L)}"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability (given a b))))
     '((task car) (mutually-exclusive-events (a (eno a)))
     (sample-space (a (eno a))) (conditional-probability car)
     (method a 1) (method b 2)))
  ------------------------------------------------------------------------------------------- |#     
(defoperator bayes-rule-contains (?sought)
  :preconditions
  (     (task ?task)
        (type ?task conditional-probability)
        (any-member ?sought (
                    (probability ?event)       
        ))   
        (test (and (listp ?event) (string= (first ?event) 'given)))
        (bind ?event1 (givee ?event))
        (bind ?event2 (giver ?event))
        (mutually-exclusive-events ?events)
        (sample-space ?events)
        (bind ?tevent1 (first (event-list  ?event1)))
        (bind ?ntevent1 (cons 'eno (event-list ?event1)))        
        (not (given (probability  (given ?tevent1 ?event2))  ?v))
        (not  (given (probability (given ?ntevent1 ?event2))  ?nv))
        (not (included ?event2 ?event1)) 
        (test (member ?event1 ?events))
;;	(not (conditional-independent-events ?abcevent  ?event2)) 
       )
  :effects
  (   (eqn-contains (bayes-rule ?event) ?sought)
  ))

#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Should apply for this this: p(H|B)=p(H)*p(B|H)/{p(H)*p(B|H)+p(L)*p(B|L)}    
 (qsolve-for '((eqn ?t (bayes-rule (given a b))))
    '((variable pa (probability a)) (variable pbga (probability (given a b))) (variable pbga (probability (given b a))) (mutually-exclusive-events (a (eno a)))
     (sample-space (a (eno a))) (variable pna (probability (eno a)))  (variable pbgna (probability (given b (eno a))))
      (variable pb (probability b)) ))             
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-bayes-rule (?eventh ?eventb)
  :preconditions
  ( (variable ?phb (probability (given ?eventh ?eventb)))
    (mutually-exclusive-events ?events)
    (sample-space ?events)
    (test (member ?eventh ?events))
    (map ?e ?events
            (expression ?exp (total-probability ?e ?eventb))
                ?exp ?exps) 
    (expression ?exphb (total-probability ?eventh ?eventb)) 
  )
  :effects
  ( (eqn (=  (/ ?exphb (+ . ?exps)) ?phb) (bayes-rule (given ?eventh ?eventb)))     
     )
  :hint
  ((point (string "Please apply the Bayes's theorem on the event ~a " (?event nlg-event))) 
   (point (string "The definition of Bayes's theorem is: if A1, A2,...An is a collection of mutually exclusive and exhaustive events, for any event B, we have p(Ai|B)=p(B|Ai)*(Ai)/{p(B|A1)*(A1)+...+p(B|Ai)*p(Ai)+...+p(B|An)*p(An)}. Here we know that ~a is a collection of mutually exclusive and exhaustive events, we can can apply the bayes rule on the event ~a|~a " (?events nlg-single-events) (?eventh nlg-event) (?eventb nlg-event)))
   (bottom-out (string "Write the equation ~a" ((= ?phb (/ ?exphb (+ . ?exps)))   algebra)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;  Independent Events                                                                                                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;   Independent Events      p(A/\B)=p(A)*p(B)                                                                                                                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass independent-events(independent-events  ?event)
     :group ProbabilityTheory
     :complexity minor
     :english ("The definition of independent events ")
     :EqnFormat ("p(A$ÇB)=p(A)*p(B)"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability  (eand (eno a)  (eno b)))))
     '((task ab)
             (event-objects (a b))
             (type ab complement-not) 
             (independent-events (a b))                      
             (given (probability a)  0.1)
             (given (probability b)  0.05)      
     ))
  ------------------------------------------------------------------------------------------- |#     
(defoperator independent-events-contains (?sought)
  :preconditions
  (    (independent-events ?events) 
       (reasonable-event ?event)      
       (test (equalp (length (eand-list ?event)) 2))      
       (test (subsetp (event-list (eand-list ?event)) ?events )) 
       (bind ?e1 (first (eand-list ?event)))
       (bind ?e2 (second (eand-list ?event)))
       (any-member ?sought (
                    (probability ?event)     
                    (probability ?e1)   
                    (probability ?e2)   
        ))             
       )
  :effects
  (   (eqn-contains (independent-events ?event) ?sought)
  ))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e  (probability  a)))
     '((task car) (independent-events (a b c)) (variable pabc (probability (eand a b c)))
      (variable pab (probability (eand a b)))
     (variable pa (probability a))  (variable pc (probability c))
     (variable pb (probability b))
     ))
  -------------------------------------------------------------------------------------------     |#  
(defoperator independent-events-contains3 (?sought)
  :preconditions
  (    (independent-events ?events) 
       (reasonable-event ?event)         
       (test (equalp (length (eand-list ?event)) 3))      
       (test (subsetp (event-list (eand-list ?event)) ?events)) 
       (bind ?e1 (first (eand-list ?event)))
       (bind ?e2 (second (eand-list ?event)))
       (bind ?e3 (third (eand-list ?event)))
       (any-member ?sought (
                    (probability ?event)     
                    (probability ?e1)   
                    (probability ?e2)    
                    (probability ?e3)  
        ))             
       )
  :effects
  (   (eqn-contains (independent-events ?event) ?sought)
  ))

#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Should apply for this this:  p(A/\B)=p(A)*p(B) 
 (qsolve-for '((eqn ?t (independent-events (eand a b))))
    '((task machine)
             (event-objects (a b))
             (independent-events (a b))                      
             (given (probability a)  0.98)
             (given (probability (eand a b))   0.95)      ))             
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-independent-events (?event)
  :preconditions
  ( 
   (independent-events ?events) 
   (test (subsetp (event-list (eand-list ?event)) ?events)) 
   (variable ?pevent (probability ?event))
   (bind ?elist (eand-list ?event))
   (map ?e ?elist
             (variable ?pe (probability ?e))
                ?pe ?pbs) 
  )
  :effects
  ( (eqn (=  (* . ?pbs) ?pevent) (independent-events ?event))     
     )
  :hint
  ((point (string "~a You can apply definition of independent event." ((?event ?events) nlg-independent-event)))
   (point (string "The definition of independent events is: If two events A and B are independent events, then p(A$ÇB)=p(A)*p(B); If three events A, B and C are independent events, then p(A$ÇB$ÇC)=p(A)*p(B)*p(C),p(A$ÇB)=p(A)*p(B), p(A$ÇC)=p(A)*p(C) and p(B$ÇC)=p(B)*p(C). Here you know: ~a You can apply definition of independent event. " ((?event ?events) nlg-independent-event)))  
   (bottom-out (string "Write the equation ~a " ((= ?pevent (* . ?pbs)) algebra) ))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;  Conditional Independent Events                                                                                                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;  Conditional Independent Events      p(A/\B|C)=p(A|C)*p(B|C)                                                                                                                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-psmclass conditional-independent-events(conditional-independent-events  ?event)
     :group ProbabilityTheory
     :complexity minor
     :english ("The definition of conditional independent events  ")
     :EqnFormat ("p(A$ÇB|G)=p(A|G)*p(B|G)"))

#|------------------------------------------------------------------------------------------
(qsolve-for '((eqn-contains ?e (probability (given (eand a b) c))))
     '((task car) (conditional-independent-events (a b d) c) (variable pabgc (probability (given (eand a b) c)))
     (variable pac (probability (given a c)))
     (variable pbc (probability (given b c)))
     ))
  ------------------------------------------------------------------------------------------- |#     
(defoperator conditional-independent-events-contains (?sought)
  :preconditions
  (    (conditional-independent-events ?events ?eventb) 
       (reasonable-event ?event)      
       (test (equalp (length (eand-list ?event)) 2))      
       (test (not (member  ?eventb (eand-list ?event)) ))      
       (test (subsetp (member-list (eand-list ?event)) ?events )) 
       (bind ?e1 (eoperator-event  'given (list (first (eand-list ?event)) ?eventb)))
       (bind ?e2 (eoperator-event  'given (list (second (eand-list ?event)) ?eventb)))
       (bind ?givenevent (eoperator-event  'given (list ?event ?eventb)))
       (any-member ?sought (
                    (probability ?givenevent)     
                    (probability ?e1)   
                    (probability ?e2)   
		    ))         
       
       )
  :effects
  (   (eqn-contains (conditional-independent-events  ?givenevent) ?sought)
      ))


(defoperator conditional-independent-events3-contains (?sought)
  :preconditions
  (    (conditional-independent-events ?events ?eventb) 
       (reasonable-event ?event)     
       (test (not (member  ?eventb (eand-list ?event)) ))      
       (test (equalp (length (eand-list ?event)) 3))      
       (test (subsetp (member-list (eand-list ?event)) ?events)) 
       (bind ?e1 (eoperator-event  'given (list (first (eand-list ?event)) ?eventb)))
       (bind ?e2 (eoperator-event  'given (list (second (eand-list ?event)) ?eventb)))
       (bind ?e3 (eoperator-event  'given (list (third (eand-list ?event)) ?eventb)))
       (bind ?givenevent (eoperator-event  'given (list ?event ?eventb)))
       (any-member ?sought (
                    (probability ?givenevent)     
                    (probability ?e1)   
                    (probability ?e2)    
                    (probability ?e3)  
		    ))             
       )
  :effects
  (   (eqn-contains (conditional-independent-events  ?givenevent) ?sought)
      ))


#|test: -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Should apply for this this:  p(A/\B)=p(A)*p(B) 
 (qsolve-for '((eqn ?t (conditional-independent-events (given (eand a b) c))))
    '((variable pagc (probability (given a c))) (variable pbgc (probability (given b c)))
     (variable pabgc (probability (given (eand a b) c)))
      (variable pb (probability b)) (variable pc (probability c)) ))             
 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------|#   
(defoperator write-conditional-independent-events (?event)
  :specifications "If the goal is to write the definition of the probability,
   the subgoals are to define variables for probabilities,
   then write. "
  :preconditions
  ( (variable ?pevent (probability (given ?event ?eventb)))
    (conditional-independent-events ?events ?eventb) 
    (map ?e (eand-list ?event)
             (variable ?pe (probability (given ?e ?eventb)))
                ?pe ?pbs) 
  )
  :effects
  ( (eqn (=  (* . ?pbs) ?pevent) (conditional-independent-events (given ?event ?eventb)))     
     )
  :hint
  ((point (string "~a You can apply definition of conditional independent events." ((?event ?events ?eventb) nlg-conditional-independent-event) ))
   (point (string "The definition of conditional independent events is: If two events A and B are conditional independent events given G, then p(A$ÇB|G)=p(A|G)*p(B|G); If three events A, B and C are conditional independent events given G, then p(A$ÇB$ÇC|G)=p(A|G)*p(B|G)*p(C|G),p(A$ÇB|G)=p(A|G)*p(B|G), p(A$ÇC|G)=p(A|G)*p(C|G) and p(B$ÇC|G)=p(B|G)*p(C|G). Here we know: ~a You can apply definition of conditional independent events." ((?event ?events ?eventb) nlg-conditional-independent-event)))  
   (bottom-out (string "Write the equation ~a " ((= ?pevent (* . ?pbs)) algebra)))
   ))
