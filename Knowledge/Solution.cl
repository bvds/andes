#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution.cl
;; Collin Lynch
;; 3/14/2001
;; 

;;;; Note the terminology issues with eqnsets and solutions will need
;;;; to be resolved at a later date.

;; This file defines the Equation set collection code for Solutiongraphs
;; its function is to collect up the set of distinct equation and
;; quantity sets that can be used to solve for a sought quantity in a
;; Solutiongraph.  These sets are returned for use.
;;

The algorithm proceeds in two stages.  

Stage1 is a black solution which is defined as a solution that contains
a red solution and an unsolved-for quantity.

A Red solution consists of a set of quantities and the equations that 
solve for them.  A graph is complete when you have a red solution with
no hanging quantities.


To build a red/black traversal of a given solutiongraph do the following.

1. Slect a quantity.
2.   If the quantity is known move on.
3.   Otherwize select an euation containing that quantity and solve it.
4.   

1. Place the sought quantitie(s) into a black solution.
2. Select a quantity from the pool.
3. Select an equation attatched to the quantity.
4. If the equation is already in the solution.
5.    Then the quantity is known, repeat.
6. If the equation is not in the solution 
7.    Then add each of its quantities to the pool and repeat.
8. When the pool is empty the solution is complete.



#####################################################
Solutions are stored for help time use as a list of 
the form (<IDS> <EqnSet> <Assumptions>) this set is 
then used to setup the solutions for later use. 
These ids are a list of bgnode gindicies.
the eqnset is a list of eqn indicies.
The Assumptions are a list of assumptions.


|#




;;======================================================================
;; Structures.

(defstruct (Solution (:print-function print-solution))
  ID
  Knowns
  Soughts
  Assumptions
  Eqnset)				; The set of currently active equation ids.


(defun print-solution (Solution &optional (Stream t) (Level 0) (form t))
  "Print out the specified solution to the stream."  
  (case Form
    (Full (print-full-solution Solution Stream Level))
    (ProblemFile (print-problemfile-solution Solution Stream Level))
    
    (Mini (pprint-Indent :block Stream Level)
	  (format Stream "<B: ~A>~%" (Solution-ID Solution)))
    	   
    (Interface (pprint-Indent :block Stream Level)
	       (format Stream "<B: Path:        ~A>~%" (reverse (Solution-Knowns Solution))))
         
    (t (print-basic-solution Solution Stream Level))))


(defun print-full-solution (Solution &optional (Stream t) (level 0))
  (pprint-Indent :block Stream Level)
  (format Stream "<B: ~A~%" (Solution-ID Solution))
  (pprint-Indent :block Stream Level)
  (format Stream "    ~A~%" (Solution-Knowns Solution))
  (pprint-Indent :block Stream Level)
  (format Stream "    ~A~%" (Solution-Soughts Solution))
  (pprint-Indent :block Stream Level)
  (format Stream "    ~A~%" (Solution-Assumptions Solution))
  (pprint-Indent :block Stream Level)
  (format Stream "    ~A~%" (Solution-Eqnset Solution)))


(defun print-problemfile-solution (Solution &optional (Stream t) (Level 0))
   (pprint-Indent :block Stream Level)
   (format Stream "(~W~%" (Solution-ID Solution))
   (pprint-Indent :block Stream Level)
   (format Stream "~W~%" (Solution-Knowns Solution))
   (pprint-Indent :block Stream Level)
   (format Stream "~W~%" (Solution-Soughts Solution))
   (pprint-Indent :block Stream Level)
   (format Stream "~W~%" (Solution-Assumptions Solution)))

(defun print-basic-solution (Solution &optional (Stream t) (Level 0))
  (pprint-Indent :block Stream Level)
  (format Stream "<B: Knowns:      ~A~%" (Solution-Knowns Solution))
  (pprint-Indent :block Stream Level)
  (format Stream "    ~A~%" (Solution-Assumptions Solution))
  (pprint-Indent :block Stream Level)
  (format Stream "    Soughts:    ~A>~%" (Solution-Soughts Solution)))
         



    



;; -------------------------------------------------------
;; Solution set.
;; listing of solutions for use.

(defstruct (EqnSet (:type list))
  Eqns
  Nodes
  Assumpts
  Solutions)

(defun print-numbered-eqnset (Num Set &optional (Stream t) (Level 0))
  "Print out a numbered eqn set."
  (pprint-indent :block Level Stream)
  (format Stream "~A:-----------------~%" Num)
  (format Stream "Algebra:~%~{ ~A~%~}~%" (mapcar #'eqn-algebra (eqnset-eqns Set))) 
  (format Stream "Exps:   ~%~{ ~A~%~}~%" (mapcar #'eqn-exp (eqnset-eqns Set)))
  (format Stream "Nodes:  ~W~2%" (eqnSet-Nodes Set)))

;;; For the purporses of answer reporting this code prints
;;; out the contents of an eqnset in report fasion I.E.
;;; it prints the psms then the entries and equations.
(defun print-numbered-report-eqnset (Num Set &optional (Stream t) (level 0))
  (format Stream "~A: Problem Solution Methods.~%" num) 
  (format Stream "~{    ~A~%~}" (EqnSet-Eqns set))
  (format Stream "~A: Entries.~%" num)
  (print-eqnset-entries Set Stream Level)
  (format Stream "~A: Implicit Equations~%" num)
  (print-eqnset-implicit-eqns Set Stream Level)
  (format Stream "~A: Explicit Equations~%" num)
  (print-eqnset-explicit-eqns Set Stream Level))

(defun print-eqnset-entries (Set Stream Level)
  (declare (ignore Level))
  "Print the eqnset entries."
  (format Stream "~{    ~A~%~}" 
	  (sort-ascending-string-length
	   (remove-if-not 
	    #'(lambda (e) (and (kb-entryprop-p e)
			       (not (kb-eqn-entryprop-p e))))
	    (mapunion #'collect-psmgraph-csdo-effects
		      (mapcar #'(lambda (n) 
				  (if (Enode-p n) (enode-path n)
				    (qnode-path n)))
			      (Eqnset-Nodes Set))
		      :test #'equalp)))))
		     

(defun print-eqnset-implicit-eqns (Set Stream Level)
  "Print the implicit equations in the EqnSet."
  (declare (ignore Level))
  (format Stream "~{    ~A~%~}" 
	  (sort-ascending-string-length
	   (mapcar #'eqn-algebra
		   (remove-if-not
		    #'(lambda (e) (eql (eqn-type e) 'implicit-eqn))
		    (enodes->eqns (EqnSet-Eqns Set)))))))
   
     
(defun print-eqnset-explicit-eqns (Set Stream Level)
  "Print the implicit equations in the EqnSet."
  (declare (ignore Level))
  (format Stream "~{    ~A~%~}" 
	  (sort-ascending-string-length
	   (mapcar #'eqn-algebra
		   (remove-if
		    #'(lambda (e) (eql (eqn-type e) 'implicit-eqn))
		    (enodes->eqns (EqnSet-Eqns Set)))))))
  

(defun print-num-eqnset-wsols (Num Set &optional (Stream t) (Level 0))
  "Print out a numbered eqn set."
  (print-numbered-eqnset Num Set Stream Level)
  (dolist (S (EqnSet-Solutions Set))
    (pprint-indent :block Level Stream)
    (format Stream "~w~2%" S)))


(defun print-mreadable-eqnsets (Sets Stream)
  (format Stream "(")
  (dolist (S Sets)
    (print-mreadable-Eqnset S Stream))
  (format Stream ")~%"))

(defun print-mreadable-EqnSet (Set Stream)
  (format Stream "(~w ~w ~w)~%"
	  (collect-eqns->indicies
	   (remove-duplicates  
	    (list-base-eqns 
	     (collect-bgnodes->eqns (EqnSet-Eqns Set)))))
	  (collect-nodes->gindicies (EqnSet-Nodes Set))
	  (EqnSet-Assumpts Set)))

(defun read-mreadable-EqnSets (Stream EqnIndex)
  (loop for S in (read Stream "Error: Malformed Equation Sets.")
      collect (list (collect-indicies->eqns (car S) EqnIndex)
		    (cadr S) (caddr S))))

(defun solution-equalp (B1 B2)
  "Determine iff two solutions are set-equal."
  (and (sets-equalp (Solution-Soughts B1)
		    (Solution-Soughts B2))
       (sets-equalp (Solution-Knowns B1)
		    (Solution-Knowns B2))
       (sets-equalp (Solution-Assumptions B1)
		    (Solution-Assumptions B2))))


;;; Given a set of eqnsets collect the set of nodes within
;;; them for later use.
(defun collect-eqnsets->nodes (EqnSets)
  (loop for S in EqnSets
      append (EqnSet-Nodes S)))

;;;--------------------------------------------------------------------
;;; sort eqnsets by optimality
;;; Given the equation sets sort them in the order from shortest
;;; (most optimal) to longest (least optimal).
(defun sort-eqnsets-by-optimality (Sets)
  (sort Sets #'<= :key #'(lambda (n) (length (eqnset-nodes n)))))

;;;--------------------------------------------------------------------
;;; get-optimal-solution-path
;;; Once we have found solutions then the we will want to find the
;;; optimal solution to a specific problem.  This function will select
;;; from among the eqnsets supplied to it returning the path or nodes
;;; from the shortest I.E. most optimal path.
(defun get-optimal-solution-path (solutions)
  "Get the path from the optimal solutions."
  (let ((Best (EqnSet-Nodes (car Solutions))))
    (dolist (S (cdr Solutions))
      (when (< (length (EqnSet-Nodes S)) (length Best))
	(setq Best (EqnSet-Nodes S))))
    Best))



  