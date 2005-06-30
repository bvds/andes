#|------------------------------------------------------------------------------------------
make-event transform a sting into the event form that the knowledge base can recognize!
And all the element of the sring should either be non-stansard or upcased!
------------------------------------------------------------------------------------------- |# 

;;Mostly, make-event transform a string into a list of the characters!
(defun make-event (event-inputstring)
   "transform the event string into the event form that the knowledge base can recognize!"
   (let ((event-string (string event-inputstring)))
     (if (stringp event-string)
	 (let ((event-list (string-into-list event-string)))
	   (united-event 
	    (make-event-list 
	     (add-necessary-pyretheses 
	      (list-pyretheses 
	       (delete-extra-pyretheses  
		(combine-event  event-list))))))))))


(defun dp (event-string)
   "transform the event string into the event form that the knowledge base can recognize!"
   (if (stringp event-string)
       (let ((event-list (string-into-list event-string)))
	 (delete-extra-pyretheses  event-list))))

(defun comb-event (event-string)
   "transform the event string into the event form that the knowledge base can recognize!"
   (if (stringp event-string)
       (let ((event-list (string-into-list event-string)))
	 (combine-event  event-list))))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;delete all of the extra pyrethese around a single event! (a) --> a; (~a) --> ~a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#


(defun string-into-list (event-string)
  (if (stringp event-string)
      (remove-both-pyretheses (remove #\Space (coerce (string-upcase event-string) 'list)))))

(defun remove-both-pyretheses  (event-list) ;; remove the pyretheses that at (a\\/b)--> a\\/b
  (if (and (listp event-list)
	   (char= (first event-list)#\() 
	   (equalp (corresponding-right-pyretheses-position (rest event-list) (length event-list) 1)
		   (- (length event-list) 1)))
      (remove-both-pyretheses (rest (butlast event-list 1)))
      (remove-double-pyretheses event-list)))

(defun remove-double-pyretheses  (event-list) ;; remove ((a\\/b))|g --> (a/\\b)|g
  (cond ((null event-list) nil)
	((and (listp event-list)
	   (char= (first event-list)#\() 
	   (char= (second event-list)#\() 
	   (equalp (corresponding-right-pyretheses-position (rest event-list) (length event-list) 1)
		   (+ (corresponding-right-pyretheses-position (rest (rest event-list)) (length event-list) 1) 1)))
	 (let* ((n   (corresponding-right-pyretheses-position (rest event-list) (length event-list) 1)))
	   (remove-double-pyretheses  (rest (remove #\) event-list :start n :count 1)))))
	
	((listp event-list)
	 (if (not (null  (first event-list)))
	     (cons (first event-list) (remove-double-pyretheses (rest event-list)))
	   nil))
	(T  nil)))



#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;united-event will combine the list as (eor (eor A B) C) ; (eor C (eor A B))--> (eor C A B)
;;Combine the binary event operator to be multinary! And sort the list into sequence that all
;; events defination obeys! (eand A C B) should be (eand A B C)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun united-event (event-list) 
  (progn
    ;(pprint event-list)
    (cond 
     ((null event-list) nil)
     ((symbolp event-list) event-list)
     ((and (listp event-list)
	   (equalp(length event-list) 1))  (united-event (first event-list)))
     ((and (listp event-list)
	   (equalp(length event-list) 2)
	   (equalp (first event-list) 'eno))  (list 'eno (united-event (second event-list))))
     
     ((and (listp event-list)           ;; (eor (eor A B) C) -->  (eor C A B)
	   (member (first event-list) *event-binary-united*))
	     (cons (first event-list) (sort-list (found-united-event (first event-list) (rest  event-list)))))
     
     ((and (listp event-list)   ;; Normal binary operators: eand eor and given!
	   (equalp (first event-list) 'given)
	   )  
      (list (first event-list) 
	    (united-event (second event-list)) (united-event (third event-list))))
      (T  nil)
      )))

(defparameter *event-binary-united* '(eand eor))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;found-united-event event-operator event-list
;;'(eor (eor A B) C) will find (eor A B C)
;(will combine the list as '(eor (eor A B) C) ; '(eor C (eor A B))--> (eor C A B)
;found-united-event '(eor ((eor A B) (eand C D) (eor (C D)))) will find (eor A B (eand C D) C D)
;found-united-event '(eor (eor (eor A B) (eand (eand C D) E)) (eor (C D)))) will find (eor A B (eand C D E) C D)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun found-united-event (eoperator event-list) 
    (cond 
     ((null event-list) nil)
     ((symbolp event-list) event-list)
     ((and (listp event-list)
	   (equalp(length event-list) 2)
	   (equalp (first event-list) 'eno))   
	(list 'eno (united-event (second event-list))))
     
     ((and (listp event-list)           ;; (eor (eor A B) C) -->  (eor C A B)
	   (member (first event-list) *event-binary-operators*)
	   (not (equalp (first event-list) eoperator)))
	 (united-event event-list))
     
     ((and (listp event-list)           ;; (eor (eor A B) C) -->  (eor C A B)
	   (equalp (first event-list) eoperator))
	 (found-united-event eoperator (rest event-list)))
     
     ((listp event-list)
		  (only-element-list  (found-united-event eoperator (first event-list)) (found-united-event eoperator (rest event-list))))
      (T  nil)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only-element-list is to make event-list1 and event-list2 to be a single list!
;; For example, event-list1=(c d) and event-list2 = (a (eno c) (eand (d f)))
;; (only-element-list event-list1 event-list2) will be (d c a (eno c) (eand (d f)))
;;Every element of the output list is an event!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun  only-element-list (event-list1 event-list2)
  (if (listp event-list2)
      (let ((output-list  event-list2))
	(cond 
	 ((null event-list1)   event-list2)
	 ((and (listp event-list1) (member (first event-list1) *event-complete-operators*)) (setf output-list (cons event-list1  output-list)))
	 ((symbolp event-list1)  (setf output-list (cons event-list1  output-list)))
	 ((listp event-list1)  (dolist (e event-list1) (setf output-list (cons e  output-list))))
	 )
	output-list)))
  
(defparameter *event-complete-operators* '(eand eor given eno))
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make-event-list transform a list of the characters into the event form elements. e.g "a\\/B"
;;Becomes (A eor B)
;;(format nil "~c" event-list) transform a character (must be the upper-case)into a string;
;;(intern ?string) transform a string into a aymbol;
;; (make-event "~((~a)/\\b)") does not work! 
;;(make-event "(~a)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun make-event-list (event-list) 
  (progn
    ;(pprint event-list)
    (cond 
     ((null event-list) nil)
     ((symbolp event-list) event-list)
     ((and (listp event-list)
	   (equalp(length event-list) 1))  (make-event-list (first event-list)))
     ((and (listp event-list)
	   (equalp(length event-list) 2)
	   (equalp (first event-list) 'eno))  (list 'eno (make-event-list (second event-list))))
     ((and (listp event-list)
	   (equalp(length event-list) 3)
	   (member (second event-list) *event-binary-operators*))  
      (list (second event-list) 
	    (make-event-list (first event-list)) (make-event-list (third event-list))))
      (T  nil)
     )))
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;first-event-operator: return the first event oprator of the event list: eor, eand or given!
;; the input list should be a list of no pyretheses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;add-necessary-pyretheses  ((ENO ((ENO A) EAND B) EOR C) GIVEN G) --> (((eno ((ENO A) EAND B))eor C) given g)
;; (A EOR B EOR G) --> ((A eor B) eor C)Need think about only the middle eno's are count for this add! 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun add-necessary-pyretheses (event-list)
  (progn
    ;(pprint "add-necessary-pyretheses")
    ;(pprint event-list)
    (cond 
     ((null event-list) nil)
     ((symbolp event-list) event-list)
     ((and (listp event-list)   
	   (equalp (first event-list) 'eno)
	   (symbolp (second event-list))
	   (equalp (length event-list) 2))
      event-list)
     ((and (listp event-list)   
	   (equalp (first event-list) 'eno)
	   (equalp (length event-list) 2))
      (cons (list 'eno (add-necessary-pyretheses (second event-list))) (add-necessary-pyretheses (rest (rest event-list)))))
     ((and (listp event-list)   
	   (equalp (second event-list) 'given)
	   (> (length event-list) 3))
	   (add-necessary-pyretheses (cons (add-necessary-pyretheses (first event-list)) 
		       (cons (second event-list) 
		       (list (add-necessary-pyretheses (rest (rest  event-list))))))))
		     
      ((and (listp event-list)   
	   (member (second event-list) *event-binary-operators*)
	   (> (length event-list) 3))
	   (add-necessary-pyretheses (cons (list (add-necessary-pyretheses (first event-list)) 
		       (second event-list) 
		       (add-necessary-pyretheses (third event-list)))
		       (rest (rest (rest event-list))))))
     
     ((listp event-list)
	    (cons (add-necessary-pyretheses (first event-list)) (add-necessary-pyretheses  (rest event-list))))
     (T  nil)
     )))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;list the pyretheses! #\( A eand B #\)--> (A eand B);
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun list-pyretheses(event-list)
  (progn
    ;(pprint "list-pyretheses")
    ;(pprint event-list)
    (cond 
     ((null event-list) nil)
     ((symbolp event-list) event-list)
     ((and (listp event-list)   
	   (characterp (first event-list))
	   (char= (first event-list) #\())
      (if (>	(corresponding-right-pyretheses-position (rest event-list) (length event-list) 1) 0)
	  (let* ((n (- (length event-list) (corresponding-right-pyretheses-position (rest event-list) (length event-list) 1))) 
		 (event-list1 (butlast(rest event-list) n)) (event-list2 (last event-list (- n 1))))  
	(cons (list-pyretheses event-list1) (list-pyretheses  event-list2)))
	nil))
     ((listp event-list)
            (if (not (null (list-pyretheses (first event-list))))
		(cons (list-pyretheses (first event-list)) (list-pyretheses  (rest event-list)))
	      nil))
     (T  nil)
     )))


(defparameter *event-binary-operators* '(eand eor given))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;combine the single event #\A --> A; #\~ #\A --> (eno A)
;; I commented the (if  (or (not (null (combine-event (rest event-list)))) because it will pass if the student input
;; something like (make-event ' |~A & ~C   & ~B V |)  Instead of return wrong it will return correctly. (EAND (ENO A) (ENO B) (ENO C))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun combine-event(event-list)
 ;;;(progn (pprint event-list)
	 (cond 
	  ((null event-list) nil)
	  ((and (listp event-list)   ;; single event A
		(characterp (first event-list))
		(char>= (first event-list) #\A)
		(char<=  (first event-list) #\Z)
		(not (char=  (first event-list) #\V))) 
	   (if (or (not (null (combine-event (rest event-list))))
		   (null (rest event-list)))
	       (cons (intern (format nil "~c" (first event-list))) (combine-event (rest event-list)))
	     nil))
	  ((and (listp event-list)   ;; single event ~A
		(characterp (first event-list))
		(characterp (second event-list))
		(char= (first event-list) #\~)
		(char>= (second event-list) #\A)
		(not (char=  (first event-list) #\V))
		(char<=  (second event-list) #\Z)) (cons (list 'eno (intern (format nil "~c" (second event-list)))) (combine-event (rest (rest event-list)))))
	  
	  ((and (listp event-list)    ;; single unary event operator ~
		(characterp (first event-list))
		(char= (first event-list) #\~))
	        
	   (if (char=  (second event-list) #\() 
	       (if (>	(corresponding-right-pyretheses-position (rest (rest event-list)) (length event-list) 1) 0)
		   (let* ((n (- (length event-list) 
				(corresponding-right-pyretheses-position (rest (rest event-list)) (length event-list) 1)))
			  (nevent-list2 (last event-list (- n 1))) 
			  (nevent-list1 (butlast (rest (rest event-list)) n)))
		     (cons (cons 'eno (list (combine-event nevent-list1))) (combine-event nevent-list2)))
		 nil)
	     nil))
	  
	  ((and (listp event-list)    ;; single binary event operator |
		(characterp (first event-list))
		(char= (first event-list) #\/))
	   ;;(if  (test-charater event-list 1)
	       (cons 'given (combine-event (rest event-list))))
	     ;;nil))
	  
	  ((and (listp event-list)    ;; single binary event operator |
		(characterp (first event-list))
		(char= (first event-list) #\&))
	  ;; (if  (test-charater event-list 1)
	       (cons 'eand (combine-event (rest event-list))))
	    ;; nil))
	  
	  ((and (listp event-list)    ;; single binary event operator |
		(characterp (first event-list))
		(char= (first event-list) #\V))
	   ;;(if  (test-charater event-list 1)
	       (cons 'eor (combine-event (rest event-list))))
	     ;;nil))
	  
	  ((and (listp event-list)   
		(characterp (first event-list))
		(char= (first event-list) #\())
		;;(if  (test-charater event-list 1)	
		    (cons  (first event-list) (combine-event (rest  event-list))))
		  ;;nil))
  	 ((and (listp event-list)  
		(characterp (first event-list))
		(char= (first event-list) #\)))
		;;(if  (or (not (null (combine-event (rest event-list))))
		  ;; (null (rest event-list)))
		    (cons  (first event-list) (combine-event (rest  event-list))))
		  ;;nil))
	 (T  nil)
	   ))
  
(defun test-charater(event-list pos)
	(or 		(and  	(characterp (nth pos event-list))
				(char>= (nth pos event-list) #\A)
				(char<=  (nth pos event-list) #\Z)
				(not (char= (nth pos event-list) #\V)))   
			(and 	(characterp (nth pos event-list))
				(char=  (nth pos event-list) #\())
                        (and 	(characterp (nth pos event-list))
				(char=  (nth pos event-list) #\~))))


(defun change-tt (event)
  (cond
   ((and 	(characterp event)
		(char>= event #\A)
		(not (char=  event #\V))
		(char<=  event #\Z))     event)
   ((and 	(characterp event)
		(char= event #\~))    'eno)
   ((and 	(characterp event)
		(char= event #\V))    'eor)
   ((and 	(characterp event)
		(char= event #\&))    'eand)
   ((characterp event)    event)
	))
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;combine the single event #\A --> A; #\~ #\A --> (eno A)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defun delete-extra-pyretheses(event-list)
    (cond 
     ((null event-list) nil)
     ((and (listp event-list)   ;; getting rid of the pyretheses around single thing (A)((~ A)) ((...)) --> A (~ A) (...)
	   (characterp (first event-list))
	   (char= (first event-list) #\()
	   (>	(corresponding-right-pyretheses-position (rest event-list) (length event-list) 1) 0)
	   (let* ((n (- (length event-list)
			(corresponding-right-pyretheses-position (rest event-list) (length event-list) 1)))
		  (nevent-list1 (butlast (rest event-list) n)))
	     (equalp (length (find-event nevent-list1)) 1)))  
      (let ((j (corresponding-right-pyretheses-position (rest event-list) (length event-list) 1)))
	(delete-extra-pyretheses (rest (remove #\) event-list :start j :count 1)))))
     
     
     ((and (listp event-list)   ;; (()) -->
	   (characterp (first event-list))
	   (char= (first event-list) #\()
	   (characterp (third event-list))
	   (char= (third event-list) #\)))  (cons (second event-list) (delete-extra-pyretheses (rest (rest (rest event-list))))))
     ((listp event-list)
      (cons (first event-list) (delete-extra-pyretheses  (rest event-list))))
     (T  nil)
     ))

 
(defun find-event (event-list)
  (let ((output-list nil))
    (dolist (e event-list) 
      (if (and (characterp e) (char>=  e #\A) (not (char= e  #\V)) (char<=  e #\Z)) (setf output-list (cons e output-list))))
    output-list))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;corresponding-right-pyretheses-position: Input is a list that begin with a #\(, and the number is 1 for the
;;beginning list!
;;;!!!!!!Important, the input event-list should be the event-list without the first #\)
;;;the corresponding position of the right pyretheses position:					
;;e.g.(#\( #\A #\\ #\/ #\B #\) #\| #\G) return 5.
;;(#\( #\( #\~ #\A #\) #\\ #\/ #\B #\) #\|  #\G) return 8.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
(defun corresponding-right-pyretheses-position (event-list lengthoflist number)
  (cond 
   ((equalp number 0) (- lengthoflist (length event-list) 1))
   ((null  event-list) -1)
   ((char= (first event-list) #\() (corresponding-right-pyretheses-position (rest event-list) lengthoflist (+ number 1)))
   ((char= (first event-list) #\)) (corresponding-right-pyretheses-position (rest event-list) lengthoflist (- number 1)))
   (T  (corresponding-right-pyretheses-position (rest event-list) lengthoflist number))))

(defun compound-eventp (body)
   "non-null if arg is a compound body term"
   (and (listp body) (or (and (equal (first body) 'eno) (not (atom (second body))))
                                (equal (first body) 'eand) (equal (first body) 'eor) (equal (first body) 'given) )))
                                ;;(and (equal (first body) 'given) (equalp (length (rest body)) 2) 
                                ;;   (not (givenp (first (rest body)) ))
                                 ;;  (not (givenp (second (rest body)) )) ))))

;;(defun givenp (body)
 ;;  (and (listp body)  (equal (first body) 'given)))
     
(defun giver (body)
   (if (and (listp body)  (equal (first body) 'given))
     (third body)
     ))
   
(defun givee (body)
   (if (and (listp body)  (equal (first body) 'given))
     (second body)
     ))


(defun atomeventp (body)
   "non-null if arg is a compound body term"
   (or (and (listp body) (equal (length body) 2) (equal (first body) 'eno) (atom (second body))) (atom body)))
         
(defun event-name (term)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null term) "")
        ((atom term) (string term))
        ((and (listp term) (equal (length term) 2) (equal (first term) 'eno) (atom (second term)))  
                (concatenate 'string (string 'n)  (string (second term))))
        ; Complex terms may be simple functional terms like (end-1 str) or 
	; compound body/system terms like (compound b1 b2 ...). 
	; for compound bodies we want body1&body2&body3
	((and (compound-eventp term) (equal (length term) 2) (equal (first term) 'eno) (compound-eventp (second term)))  
                (concatenate 'string (etype-prefix (first term))  (event-name (second term))))
	((and (compound-eventp term) (> (length term) 2))
	   (let ((body-list (rest term)))
             (concatenate 'string (event-name (first body-list)) 
	                  (if (rest body-list) 
		            (concatenate 'string (etype-prefix (first term)) 
		                ( if (equalp (length (rest body-list)) 1)                                               ; "&"
			            (event-name (rest body-list))
			            (event-name (remove (first body-list) term))))))))
       ((listp term) (concatenate 'string (event-name (first term)) 
					   (event-name (rest term))))
      (T    term)
	) ; signal error?
)

(defun etype-prefix (type-id)
  "returns type prefix for specified event type"
  (case type-id 
   (eand "")
   (eno "n_")
   (eor "o")
   (given  "g") ; "given" 
  ))
  
#|------------------------------------------------------------------------------------------
(eno-list  '(a (eno b))) should return (b (eno a))
  ------------------------------------------------------------------------------------------- |#      
(defun eno-list (e)   
  (cond ((null e) nil)
        ((atom e) (list 'eno e))      
	((and (listp e) (equal (length e) 2) (equal (first e) 'eno) (atom (second e)))
	    (second e))
	((listp e)   (sort-list (cons  (eno-list (first e)) (eno-list (cdr e))))) 	  
) ; signal error?
)

#|------------------------------------------------------------------------------------------
(eor-list  '(eor a (eno b))) should return (A (ENO B))
  ------------------------------------------------------------------------------------------- |#   

(defun eor-list (e)
    "given event form as: a\/b\/c, returns list of the (a b c) and also it is sorted!" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null e) nil)
        ((atomeventp e) (list e))      
	((compound-eventp e)
	    (if (equal (first e) 'eor)
	        (let ((body-list (rest e)))
	            (if (atom-event body-list)
	             (if (rest body-list)	                                
                            (sort-list body-list))))))))

#|------------------------------------------------------------------------------------------
(eand-list  '(eand a (eno b))) should return (A (ENO B))
  ------------------------------------------------------------------------------------------- |#  
  
(defun eand-list (e)
    "given event form as: a/\b/\c, returns list of the (a b c) and also it is sorted!" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null e) nil)
        ((atomeventp e) (list e))       
	((compound-eventp e)
	    (if (equal (first e) 'eand)
	        (let ((body-list (rest e)))
	            (if (atom-event body-list)
	             (if (rest body-list)	                                
                            (sort-list body-list))))))))	


(defun atom-event (elist)
    "whether elist is a list of event!" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((equal elist nil) t)
    ((and (equal (length elist) 1) (atomeventp (first elist))) t)
    ((listp elist) (if (atomeventp (first elist))
           (atom-event (rest elist))))
) ; signal error?
)

;; if the input is 2 (a b c) then the output is ((a b) (b c ) (a c))
(defun times-list (num elist)
             (let ((newlist nil))
             (if  (equal num 2) 
                (let ((i 0) (totallist (cartesian elist elist)))                                    
                            (do ((i 0 (+ i 1))) 
                                  ((equal i (length totallist)))                                                        
                                            (if (string-lessp (first (nth i totallist)) (second (nth i totallist)))                                                            
                                                 (setf newlist (cons  (nth i totallist) newlist)))))
               (if  (equal num 3) 
                    (let ((i 0) (totallist (cartesian3 elist elist elist)))                                    
                            (do ((i 0 (+ i 1))) 
                                  ((equal i (length totallist)))                                                         
                                            (if (and (string-lessp (first (nth i totallist)) (second (nth i totallist)))
                                                        (string-lessp (first (nth i totallist)) (third (nth i totallist)))
                                                        (string-lessp (second (nth i totallist)) (third (nth i totallist))))                                                            
                                                 (setf newlist (cons  (nth i totallist) newlist))))))
)                                                                      
               newlist))  
               
(defun list-union (compo-quants1 compo-quants2 compo-quants3)  
   (union (union compo-quants1 compo-quants2) compo-quants3))
   
(defun list-intersection (compo-quants1 compo-quants2)  
   (intersection compo-quants1 compo-quants2))
    
(defun list-minus (compo-quants1 compo-quants2)  
   (set-difference compo-quants1 compo-quants2))                            
                            
#|(cartesian '(1 2) '(r g)), should return: ((1 r) (1 g) (2 r) (2 g)) |#

(defun cartesian (events1 events2)
             (let ((val1 (length events1)) (val2 (length events2)))
                (let ((i 0) (newlist nil))                                    
                            (do ((i 0 (+ i 1))) 
                                  ((equal i val1))                                                                  
                                  (setf newlist (union (cartesian1 (nth i events1) events2) newlist)))                                                                               
                            newlist)))     
                            
                                  #|(list-of 3 'f), should return: (f f f) |#
            (defun list-of (n elt)
                  (if (zerop n)
                       nil
                       (cons elt (list-of (- n 1) elt))))           
          
            #|(cartesian1 'f '(1 t)), should return: ((f 1) (f t)) |#   
            (defun cartesian1 (events1 events2)   
                                (let ((val2 (length events2)))                
                                        (let ((tempnewlist (list-of val2 events1)))
                                            (mapcar #'list tempnewlist events2)))) 

(defun cartesian3 (events1 events2 events3)
             (let ((val1 (length events1)) (val2 (length events2)) (val3 (length events3)))
                (let ((i 0) (newlist nil))                                    
                            (do ((i 0 (+ i 1))) 
                                  ((equal i val1))  
                                        (do ((j 0 (+ j 1))) 
                                                ((equal j val2))                                                                  
                                                (setf newlist (union (cartesian31 (nth i events1) (nth j events2) events3) newlist))) )                                                                              
                            newlist))) 
                    
           #|(cartesian1 'f  '4 '(1 t)), should return: ((f 4 1) (f 4 t)) |#   
            (defun cartesian31 (event1 event2 events)   
                                (let ((val2 (length events)))                
                                        (let ((tempnewlist (list-of val2 event1)))
                                            (let ((newlist (list-of val2 event2)))
                                                (mapcar #'list tempnewlist newlist events)))))
                                                         
                            
      

(defun nlg-event (term)
   (nlg-event1 term 1)
  )   

        
(defun nlg-event1 (term num)

  (cond ((null term) nil)
        ((atom term) (string term))
        ((and (listp term) (equal (length term) 2) (equal (first term) 'eno) (atom (second term)))  
	 (format nil "~~~a" (nlg-event1  (second term) 1)))
	((and (compound-eventp term) (equal (length term) 2) (equal (first term) 'eno))  
	 (format nil "~~~a" (nlg-event1  (second term) 0)))
	((and (compound-eventp term) (equal (length term) 3)(= num 0))  
	 (format nil "(~a~a~a)" (nlg-event1  (second term) 0) (etype-operate (first term)) (nlg-event1  (third term) 0)))
	((and (compound-eventp term) (equal (length term) 3)(= num 1))  
	 (format nil "~a~a~a" (nlg-event1  (second term) 0) (etype-operate (first term)) (nlg-event1  (third term) 0)))
	((and (compound-eventp term) (equal (length term) 4) (= num 0))  
	 (format nil "(~a~a~a~a~a)" (nlg-event1  (second term) 0) (etype-operate (first term)) (nlg-event1  (third term) 0) (etype-operate (first term)) (nlg-event1  (fourth term) 0)  ))
	((and (compound-eventp term) (equal (length term) 4) (= num 1))  
	 (format nil "~a~a~a~a~a" (nlg-event1  (second term) 0) (etype-operate (first term)) (nlg-event1  (third term) 0) (etype-operate (first term)) (nlg-event1  (fourth term) 0)  ))
	))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;nlg part for the knowledge base.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for a list of events.

(defun nlg-single-events (events)

  (cond ((equal (length events) 2)  (format nil "~a and ~a" (nlg-event  (first events)) (nlg-event  (second events))))
	((equal (length events) 3)  (format nil "~a, ~a and ~a" (nlg-event  (first events)) (nlg-event  (second events)) (nlg-event  (third events)) ))
	))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for or of  a list of events.

(defun nlg-or-single-events(events)

  (cond ((equal (length events) 2)  (format nil "~a$~a~a" (nlg-event  (first events)) (code-char 200) (nlg-event  (second events))))
	((equal (length events) 3)  (format nil "~a$~a~a$~a~a" (nlg-event  (first events)) (code-char 200) (nlg-event  (second events)) (code-char 200) (nlg-event  (third events)) ))
	))   



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for complement theorem.

(defun nlg-complement-theorem(events)

  (cond ((equal (length events) 2)  (format nil "p(~a)+ p(~a)" (nlg-event  (first events)) (nlg-event  (second events))))
	((equal (length events) 3)  (format nil "p(~a)+ p(~a)+ p(~a)" (nlg-event  (first events)) (nlg-event  (second events)) (nlg-event  (third events)) ))
	))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for total theorem.

(defun nlg-total-theorem (event events)

  (cond ((equal (length events) 2)  (format nil "p(~a)= p(~a|~a)*p(~a)+p(~a|~a)*p(~a)" (nlg-event  event) (nlg-event  event) (nlg-event  (first events)) (nlg-event  (first events)) (nlg-event  event) (nlg-event  (second events)) (nlg-event  (second events))))
	((equal (length events) 3)  (format nil "p(~a)= p(~a|~a)*p(~a)+p(~a|~a)*p(~a)+p(~a|~a)*p(~a)" (nlg-event  event) (nlg-event  event) (nlg-event  (first events)) (nlg-event  (first events)) (nlg-event  event) (nlg-event  (second events)) (nlg-event  (second events)) (nlg-event  event) (nlg-event  (third events)) (nlg-event  (third events)) ))
	)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg forbayes theorems.

(defun nlg-bayes-rule (eventh eventb  events)

  (cond ((equal (length events) 2)  (format nil "p(~a|~a)= p(~a|~a)*p(~a)/{p(~a|~a)*p(~a)+p(~a|~a)*p(~a)}" (nlg-event  eventh) (nlg-event  eventb) (nlg-event  eventb) (nlg-event  eventh) (nlg-event  eventh)(nlg-event  eventb) (nlg-event  (first events)) (nlg-event  (first events)) (nlg-event  eventb) (nlg-event  (second events)) (nlg-event  (second events))))
	((equal (length events) 3)  (format nil "p(~a|~a)= p(~a|~a)*p(~a)/{ p(~a|~a)*p(~a)+p(~a|~a)*p(~a)+p(~a|~a)*p(~a)}" (nlg-event  eventh) (nlg-event  eventb) (nlg-event  eventb) (nlg-event  eventh) (nlg-event  eventh) (nlg-event  eventb) (nlg-event  (first events)) (nlg-event  (first events)) (nlg-event  eventb) (nlg-event  (second events)) (nlg-event  (second events)) (nlg-event  eventb) (nlg-event  (third events)) (nlg-event  (third events)) ))
	)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for the independent events.

(defun nlg-mutually-exclusive-event (event)
    (cond ((equal (eand-list (first event)) (second event))  (format nil "Event ~a are mutually exclusive events. " (nlg-single-events (second event))))
	  (T (format nil "Because event ~a are mutually exclusive events, we can infer that ~a are mutually exclusive events. " (nlg-single-events (second event)) (nlg-single-events (eand-list (first event)))))	  
	  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for the independent events.

(defun nlg-independent-event (event)
    (cond ((equal (eand-list (first event)) (second event))  (format nil "Event ~a are independent events. " (nlg-single-events (second event))))
	  (T (format nil "Because event ~a are independent events, we can infer that ~a are independent events. " (nlg-single-events (second event)) (nlg-single-events (eand-list (first event)))))	  
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for the independent events.
(defun nlg-single-independent-event (event)

  (cond ((equal (length (eand-list event)) 2)  (format nil "~a and ~a" (nlg-event  (first (eand-list event))) (nlg-event  (second (eand-list event)))))
	((equal (length (eand-list event)) 3)  (format nil "~a, ~a and ~a" (nlg-event  (first (eand-list event))) (nlg-event  (second (eand-list event))) (nlg-event  (third (eand-list event))) ))
       
	))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; produce the proper nlg for the conditional independent events.

(defun nlg-conditional-independent-event (event)

  (cond ((equal (eand-list (first event)) (second event))  (format nil "From the problem statement,  we can know event ~a are conditional independent events given event ~a. " (nlg-single-events (second event)) (nlg-event (third event))  ))
	  (T (format nil "Because event ~a are conditional independent events given event ~a, we can infer that ~a are conditional independent events given event ~a. " (nlg-single-events (second event)) (nlg-event (third event)) (nlg-single-events (eand-list (first event))) (nlg-event (third event))))	  
	  ))


(defun etype-operate (type-id)
  "returns type prefix for specified event type"
  (case type-id 
   (eand (format nil "$~A" (code-char 199)))
   (eor (format nil "$~A" (code-char 200)))
   (given  "|") ; "given" 
   ))


#| need to sort the list when it has (eno a) |#
(defun sort-list (inputlist)
       (sort inputlist #'(lambda (x y) (string-lessp (event-name x) (event-name y)))))


   
(setf (symbol-function '\\/) #'(lambda (x y)   
                                             (if (or (integerp x) (integerp y)) 
                                                nil
                                                (let ((putinlist (list x y)))
                                                        (\\/ (nth 0 (sort-list putinlist)) (nth 1 (sort-list putinlist))))                              
                                               )))
                                               
;Return if there is list {a b c}, already know that a is known, then will returen {b c}                                               

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;  event-list(event) is return the list of the event that appear in the either simple event or component event  ;
;;   event-list  '(eand a (eno b)) Only return (a b)  but not:  (a (eno b)) form.                                                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun event-list (term)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null term) nil)
        ((atom term) (list term))
        ((and (listp term) (equal (length term) 2) (equal (first term) 'eno) (atom (second term)))  
                (list (second term)))       
	((compound-eventp term)
	   (event-list (rest term)))
        ((listp term) (sort-list (union (event-list (first term)) (event-list (rest term)))))
	(T    term)) ; signal error?
)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;  member-list(event) is return the list of the event that appear in the either simple event or component event  ;
;;     (member-list  '(eand a (eno b))) Only return (a (eno b)) form.                                                                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun member-list (term)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null term) nil)
        ((atom term) (list term))
        ((and (listp term) (equal (length term) 2) (equal (first term) 'eno) (atom (second term)))  
                (list term))       
	((compound-eventp term)
	   (member-list (rest term)))
        ((listp term) (sort-list (union (member-list (first term)) (member-list (rest term))  :test #'equalp)))
	(T    term)) ; signal error?
)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;  member-repeated-list(event) is return the list of the event that appear in the either simple event or component event  ;
;;     (member-list  '(eand a (eno b))) Only return (a (eno b)) form.                                                                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun member-repeated-list (term)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null term) nil)
        ((atomeventp term) (list term))  
	((compound-eventp term)
	   (member-repeated-list (rest term)))
        ((listp term) (sort-list (union-repeated (member-repeated-list (first term)) (member-repeated-list (rest term)))))
	(T    term)) ; signal error?
)

(defun union-repeated (list1 list2)
  "Gien two list, return one list that permitted duplicated items!" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null list1) nil)
        ((equalp (length list1) 1) (cons (first list1) list2))
        ((> (length list1) 1) (union-repeated (rest list1) (cons (first list1) list2))) ))
 

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;  included(event1, event2) is return ture if the event-list of event1 and event-list of event2 have no element in common.
;;                                                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun included (event1 event2)
   (if  (intersection (event-list event1) (event-list event2))                                    
			         t
			       nil))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;  eoperator-event (eand (a b)) is return the operation and the list of the events ;
;;                                                                                                    return (eand a b)        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eoperator-event (opt term)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null term) nil) 
           ((and (atom term) (equalp opt 'eno)) (list opt term))  
          ((and (listp term) (>= (length term) 2) (or (equalp opt 'eand) (equalp opt 'eor ))) (cons opt (sort-list term))) 
          ((and (listp term) (= (length term) 2) (or (equalp opt 'given) )) (cons opt term))       
	
))     

(defun enoand-events (opt events)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null events) nil)   
        ((and (listp term) (>= (length term) 2) (or (equalp opt 'eand) (equalp opt 'eor ))) 
                   (union (eno-events opt 1 events) (union (eno-events opt 2 events) (eno-event opt 3 events))))      
	
))      

(defun eno-events (opt num events)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null events) nil)   
           ((and (listp term) (>= (length term) 2)) 
               (let ((term (first events)))(union (eno-events opt 1 events) (union (eno-events 2 events) (eno-events 3 events))))  )    
	
))    

(defun eno-event (event)
  "Given an event, return the complement event. A return na; (eand a b) returns (eno (eand a b))" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null event) nil)
  	((atom event)  (list 'eno event)) 
        ((and (listp event) (= (length event) 2) (equal (first event) 'eno)) (second event))
        ((and (listp event) (> (length event) 2)) (list 'eno event))
        (t nil)
  ))   
                  
