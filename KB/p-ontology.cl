;;; Modifications by Anders Weinstein 2000-2008
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
;; Probability quantities
;; note arguments are usually proper names -- R1, PtA, etc -- not common nouns
;; so we don't use nlg types that add articles.


    
(def-qexp event	(event ?event)
  :units nil
  :restrictions positive
  :fromWorkbench `(event, event)
  :english ("event" (nlg ?event)))

(defoperator define-event (?event)
  :preconditions
   ( (bind ?e-var (format-sym "e_~A" (event-name ?event))) )
  :effects ( (variable ?e-var (event ?event))))
       
(def-qexp task	(task ?task)
  :units nil
  :restrictions positive
  :fromWorkbench `(task, task)
  :english ("task" (nlg ?task)))
  
(defoperator define-task (?task)
  :preconditions
   ((task ?task)
    (bind ?t-var (format-sym "~A" (body-name ?task))) )
  :effects ((variable ?t-var (task ?task))))
  
 (def-qexp quality	(quality ?quality)
  :units nil
  :restrictions positive
  :fromWorkbench `(quality, quality)
  :english ("quality" (nlg ?quality)))

 (defoperator define-quality (?quality)
  :preconditions
   ((task ?task)
    (choices ?task ?qualities)
    (test (member ?quality ?qualities))
    (bind ?q-var (format-sym "~A" (body-name ?quality))) )
  :effects ((variable ?q-var (quality ?quality))))
  
  
(def-qexp events (events ?event1 ?event2)
  :english ("the set of ~A and ~A" (nlg ?event1) (nlg ?event2)))   
       

(def-qexp probability	(probability ?event)
  :units nil
  :restrictions nonnegative
  :fromWorkbench `(probability ,(make-event body))
  :english ("the probability of ~A" (nlg-event ?event))) 

             
(defoperator define-probability (?event)
  :preconditions
   ((event-objects ?events)
    (test (subsetp (event-list  ?event) ?events))
    (test (and 	(check-given ?event)
    		(check-reasonable (event-list  ?event))
                   (or  (atomeventp ?event)
                          (and (compound-eventp ?event)
                                  (not (< (length (member-list  ?event)) (length (member-repeated-list ?event)))) 
                                  )))) 
    (bind ?pvar (format-sym "p_~A" (event-name ?event)))
   )
  :effects ( (variable ?pvar (probability ?event))
             (define-var (probability ?event)))
  :hint (
	 (bottom-out (string "Define a variable for the probability of the event ~a by using the Add Variable command on the Variable menu and selecting probability." (?event nlg-event)))
       ))        


(def-qexp probability-and2	(probability-and2 (eand ?event1 ?event2))
  :units nil
  :restrictions nonnegative
  :fromWorkbench `(probability-and2 (eand ,body ,body2))
  :english ("the probability of ~A and ~A" (nlg ?event1) (nlg ?event2))) 

             
(defoperator define-probability-and2 (?event)
  :preconditions
   ((bind ?event1 (second ?event))
    (bind ?event2 (third ?event)) 
    (bind ?pvar (format-sym "p_~A" (event-name ?event)))
   )
  :effects ( (variable ?pvar (probability-and2 ?event))
             (define-var (probability-and2 ?event)))
  :hint (
       (bottom-out (string "Define a variable for the probability of the event ~A and ~A by using the Add Variable command on the Variable menu and selecting probability."  ?event1 ?event2))
       ))        

(def-qexp probability-or2	(probability-or2 (eor ?event1 ?event2))
  :units nil
  :restrictions nonnegative
  :fromWorkbench `(probability-or2 (eor ,body ,body2))
  :english ("the probability of ~A or ~A" (nlg ?event1) (nlg ?event2))) 

             
(defoperator define-probability-or2 (?event)
  :preconditions
  ((bind ?event1 (second ?event))
   (bind ?event2 (third ?event))
    (bind ?pvar (format-sym "p_~A" (event-name ?event)))
   )
  :effects ( (variable ?pvar (probability-or2 ?event))
             (define-var (probability-or2 ?event)))
  :hint (
       (bottom-out (string "Define a variable for the probability of the event ~A and ~A by using the Add Variable command on the Variable menu and selecting probability"  ?event1 ?event2))
       ))        
       
  
  #|test: 
  (qsolve-for '((easily-reasonable-event ?e)) 
    '( (task ab)
             (event-objects (a b))
             (type ab complement-not) 
             (type (a b) independent-events)                      
             (given (probability (event a))  0.1)
             (given (probability (event b))  0.05)  ))|#
 
 (defoperator define-easily-reasonable-event ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 1))
    (bind ?e1 (first ?events))
   )
  :effects ((easily-reasonable-event ?e1)
             ))
             
(defoperator define-easily-reasonable-event2 ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 2))
    (bind ?e1 (first ?events))
    (bind ?e2 (second ?events))
   )
  :effects ( (easily-reasonable-event ?e1)
  		(easily-reasonable-event ?e2)	
             ))

(defoperator define-easily-reasonable-event3 ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 3))
    (bind ?e1 (first ?events))
    (bind ?e2 (second ?events))
    (bind ?e3 (third ?events))
   )
  :effects ( (easily-reasonable-event ?e1)
  		(easily-reasonable-event ?e2)	
  		(easily-reasonable-event ?e3)
             ))
             
(defoperator define-easily-reasonable-event4 ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 4))
    (bind ?e1 (first ?events))
    (bind ?e2 (second ?events))
    (bind ?e3 (third ?events))
    (bind ?e4 (fourth ?events))
   )
  :effects ( (easily-reasonable-event ?e1)
  		(easily-reasonable-event ?e2)
  		(easily-reasonable-event ?e3)
  		(easily-reasonable-event ?e4)	
             ))
                          
(defoperator define-easily-reasonable-event_eno ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 1))
    (type ?something  complement-not)
    (bind ?ne (list 'eno (first ?events)))
   )
  :effects ( (easily-reasonable-event ?ne)
             ))
             
   (defoperator define-easily-reasonable-event_eno2 ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 2))
    (type ?something  complement-not)
    (bind ?e1 (list 'eno (first ?events)))
    (bind ?e2 (list 'eno (second ?events)))
   )
  :effects ( (easily-reasonable-event ?e1)
  		(easily-reasonable-event ?e2)	
             ))           
                          
 (defoperator define-easily-reasonable-event_eno3 ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 3))
    (type ?something  complement-not)
    (bind ?ne1 (list 'eno (first ?events)))
    (bind ?ne2 (list 'eno (second ?events)))
    (bind ?ne3 (list 'eno (third ?events)))
   )
  :effects ( (easily-reasonable-event ?ne1)
  		(easily-reasonable-event ?ne2)	
  		(easily-reasonable-event ?ne3)
             ))
             
(defoperator define-easily-reasonable-event_eno4 ()
  :preconditions
   ((event-objects ?events)
    (test (equalp (length ?events) 4))
    (type ?something  complement-not)
    (bind ?ne1 (list 'eno (first ?events)))
    (bind ?ne2 (list 'eno (second ?events)))
    (bind ?ne3 (list 'eno (third ?events)))
    (bind ?ne4 (list 'eno (fourth ?events)))
   )
  :effects (  (easily-reasonable-event ?ne1)
  		(easily-reasonable-event ?ne2)	
  		(easily-reasonable-event ?ne3)
  		(easily-reasonable-event ?ne4)
             ))   
 
             
 #|test: 
  (qsolve-for '((reasonable-event ?e) ) 
    '( (task ab)
             (event-objects (a b))
             (type ab complement-not) 
             (type (a b) independent-events)                      
             (given (probability (event a))  0.1)
             (given (probability (event b))  0.05)  ))|#             
 
 
 (defoperator define-reasonable-event1 ()
  :preconditions
   ((easily-reasonable-event ?event)
   )
  :effects ( (reasonable-event ?event)
             ))                      

#|test: 
  (qsolve-for '( (reasonable-event ?e) ) 
    '( (task ab)
             (event-objects (a b c))
             (type ab complement-not) 
             (type (a b) independent-events)                      
             (given (probability (event a))  0.1)
             (given (probability (event b))  0.05)  ))|#       
             
 (defoperator define-reasonable-event2 ()
  :preconditions
   ( (easily-reasonable-event ?e1)
   (easily-reasonable-event ?e2)
   (test (and  (check-reasonable (event-list  (list ?e1 ?e2))) 
                   (equalp (length (event-list  (list ?e1 ?e2))) 2)))
   (test (string< (event-name ?e1) (event-name ?e2)))
   (bind ?elist (list ?e1 ?e2))
   (bind ?eventand (eoperator-event 'eand ?elist))
   (bind ?eventor (eoperator-event 'eor ?elist))
   )
  :effects ( 	(reasonable-event ?eventand)
  		(reasonable-event ?eventor)
             ))                      
    
             
(defoperator define-reasonable-event_3 ()
  :preconditions
   (
   (easily-reasonable-event ?e1)
   (easily-reasonable-event ?e2)
   (easily-reasonable-event ?e3)
   (test (and (check-reasonable (event-list (list ?e1 ?e2 ?e3)))
                 (equalp (length (event-list  (list ?e1 ?e2 ?e3))) 3)))
   (test (and (string< (event-name ?e1) (event-name ?e2)) (string< (event-name ?e2) (event-name  ?e3))))
   (bind ?elist (list ?e1 ?e2 ?e3))
   (bind ?eventand (eoperator-event 'eand ?elist))
   (bind ?eventor (eoperator-event 'eor ?elist))
   )
  :effects ( (reasonable-event ?eventand)
  		(reasonable-event ?eventor)
             ))                      
#|test: 
(qsolve-for '((reasonable-event ?e)) 
             '( (task ab)
             (event-objects (a b))
             (type ab conditional-probability) 
             (type (a b) independent-events)                      
             (given (probability (event a))  0.1)
	       (given (probability (event b))  0.05)  ))|#   

(defoperator define-reasonable-event-given (?event)
  :preconditions
   (
   (easily-reasonable-event ?e1)
   (easily-reasonable-event ?e2)
   (type ?something conditional-probability)
   (test (check-reasonable (list ?e1 ?e2)))
   (conditional-independent-events ?events ?s2)
   (not  (and (member ?e1 ?events)  (member ?e2 ?events)))
   (bind ?event (eoperator-event 'given (list ?e2 ?e1)))
   )
  :effects ( (reasonable-event ?event)
             ))         

(defoperator define-reasonable-event-given (?event)
  :preconditions
   (
   (easily-reasonable-event ?e1)
   (easily-reasonable-event ?e2)
   (type ?something conditional-probability)
   (test (check-reasonable (list ?e1 ?e2)))
   (not (conditional-independent-events ?events ?s2))
   (bind ?event (eoperator-event 'given (list ?e2 ?e1)))
   )
  :effects ( (reasonable-event ?event)
             ))             	       
             
(defoperator define-reasonable-event-given (?event)
  :preconditions
   (
   (easily-reasonable-event ?e1)
   (easily-reasonable-event ?e2)
   (type ?something conditional-probability)
   (test (check-reasonable (list ?e1 ?e2)))
   (not (conditional-independent-events ?events ?s2))
   (bind ?event (eoperator-event 'given (list ?e2 ?e1)))
   )
  :effects ( (reasonable-event ?event)
             ))              
             
 (defun check-reasonable (elist)
   (or (and (equalp (length elist) 1) (atomeventp (first elist)))
       (and (equalp (length elist) 2) (not (equalp (first elist) (second elist))))
       (and (equalp (length elist) 3) (not (equalp (first elist) (second elist)))
	    (not (equalp (second elist) (third elist)))
	    (not (equalp (first elist) (third elist))))
       (and (equalp (length elist) 4) (not (equalp (first elist) (second elist)))
	    (not (equalp (first elist) (third elist)))
	    (not (equalp (first elist) (fourth elist)))
	    (not (equalp (second elist) (third elist)))
	    (not (equalp (second elist) (fourth elist)))
       				(not (equalp (fourth elist) (third elist)))				
       )))
 
 (defun check-given (event)
   (cond ( (and (listp event) (equal (first event) 'given) (equalp (length (rest event)) 2) 
                                  		(not (givenp (first (rest event))))
                                  		(not (givenp (second (rest event))))
                                  		(equalp (intersection (event-list (first (rest event))) 
                                  		            (event-list  (second (rest event)))) nil) ) t)
          ((atomeventp event)  t)
          ((and (compound-eventp event) (not (equal (first event) 'given))) (every 'check-given (rest event)))    
          (event nil)))
       
 (defun givenp (body)
   (and (listp body)  (equal (first body) 'given)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Sample Space                                                                                                                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;; Sample Space                                                                                                                             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

#|

(qsolve-for '((sample-space ?elist))
            '((event-objects (a b))
             (type ab complement-not) 
             (type (a b) independent-events)   ))  |#
     
 (defoperator define-sample-space ()
  :preconditions
   (
   (easily-reasonable-event ?event1)
   (type ?something complement-not)
   (test (atom ?event1))                  
   (bind ?elist  (list ?event1 (list 'eno ?event1)))
   )
  :effects ( (sample-space ?elist)
              (mutually-exclusive-events ?elist)
             ))             
 
                         
 (defoperator define-given-sample-space ()
  :preconditions
   ((type ?s conditional-probability)
    (reasonable-event ?event1)
    (test (and (listp ?event1) (equalp (first ?event1)  'given) (atom (givee ?event1))))
    (type ?something complement-not)
   (bind ?elist  (list ?event1 (eoperator-event 'given (list (list 'eno (givee ?event1)) (giver ?event1)))))
   )
  :effects ( (sample-space ?elist)
                (mutually-exclusive-events ?elist)
             ))        
             
                          
