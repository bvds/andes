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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Solution.cl
;;;;; Collin Lynch
;;;;; 3/14/2001
;;;;; 
;;;
;;;;;;; Note the terminology issues with eqnsets and solutions will need
;;;;;;; to be resolved at a later date.
;;;
;;;;; This file defines the Equation set collection code for Solutiongraphs
;;;;; its function is to collect up the set of distinct equation and
;;;;; quantity sets that can be used to solve for a sought quantity in a
;;;;; Solutiongraph.  These sets are returned for use.
;;;;;
;;;
;;;The algorithm proceeds in two stages.  
;;;
;;;Stage1 is a black solution which is defined as a solution that contains
;;;a red solution and an unsolved-for quantity.
;;;
;;;A Red solution consists of a set of quantities and the equations that 
;;;solve for them.  A graph is complete when you have a red solution with
;;;no hanging quantities.
;;;
;;;
;;;To build a red/black traversal of a given solutiongraph do the following.
;;;
;;;1. Slect a quantity.
;;;2.   If the quantity is known move on.
;;;3.   Otherwise select an equation containing that quantity and solve it.
;;;4.   
;;;
;;;1. Place the sought quantitie(s) into a black solution.
;;;2. Select a quantity from the pool.
;;;3. Select an equation attatched to the quantity.
;;;4. If the equation is already in the solution.
;;;5.    Then the quantity is known, repeat.
;;;6. If the equation is not in the solution 
;;;7.    Then add each of its quantities to the pool and repeat.
;;;8. When the pool is empty the solution is complete.
;;;
;;;
;;;
;;;Solutions are stored for help time use as a list of 
;;;the form (<IDS> <EqnSet> <Assumptions>) this set is 
;;;then used to setup the solutions for later use. 
;;;These ids are a list of bgnode gindicies.
;;;the eqnset is a list of eqn indicies.
;;;The Assumptions are a list of assumptions.



;;======================================================================
;; Structures.

(defstruct (Solution (:print-function print-solution))
  ID
  Knowns
  Soughts
  Assumptions
  Eqnset)		   ; The set of currently active equation ids.


(defun print-solution (Solution &optional (Stream t) (Level 0) (Form t))
  "Print out the specified solution to the stream."  
  (case Form
    (Full (print-full-solution Solution Stream Level))
    (ProblemFile (print-problemfile-solution Solution Stream Level))
    (Mini (pprint-Indent :block Level Stream)
	  (format Stream "<B: ~A>~%" (Solution-ID Solution)))
    (Interface (pprint-Indent :block Level Stream)
	       (format Stream "<B: Path:        ~A>~%" 
		       (reverse (Solution-Knowns Solution))))
    (t (print-basic-solution Solution Stream Level))
    ))


(defun print-full-solution (Solution &optional (Stream t) (level 0))
  (pprint-Indent :block Level Stream)
  (format Stream "<B: ~A~%" (Solution-ID Solution))
  (pprint-Indent :block Level Stream)
  (format Stream "    ~A~%" (Solution-Knowns Solution))
  (pprint-Indent :block Level Stream)
  (format Stream "    ~A~%" (Solution-Soughts Solution))
  (pprint-Indent :block Level Stream)
  (format Stream "    ~A~%" (Solution-Assumptions Solution))
  (pprint-Indent :block Level Stream)
  (format Stream "    ~A~%" (Solution-Eqnset Solution)))


(defun print-problemfile-solution (Solution &optional (Stream t) (Level 0))
   (pprint-Indent :block Level Stream)
   (format Stream "(~W~%" (Solution-ID Solution))
   (pprint-Indent :block Level Stream)
   (format Stream "~W~%" (Solution-Knowns Solution))
   (pprint-Indent :block Level Stream)
   (format Stream "~W~%" (Solution-Soughts Solution))
   (pprint-Indent :block Level Stream)
   (format Stream "~W~%" (Solution-Assumptions Solution)))

(defun print-basic-solution (Solution &optional (Stream t) (Level 0))
  (pprint-Indent :block Level Stream)
  (format Stream "<B: Knowns:      ~A~%" (Solution-Knowns Solution))
  (pprint-Indent :block Level Stream)
  (format Stream "    ~A~%" (Solution-Assumptions Solution))
  (pprint-Indent :block Level Stream)
  (format Stream "    Soughts:    ~A>~%" (Solution-Soughts Solution)))
         



    



;;; -------------------------------------------------------
;;; Solution set.
;;; listing of solutions for use.

(defstruct (EqnSet (:type list))
  Eqns		; list of all Enodes for solution PSMs
  Nodes		; list of all Enodes and Qnodes in solution
  Assumpts	; assumptions used in this solution
  Solutions)	; used in SGG/SolutionSet.cl, not persisted to file

(defun EqnSet-AllEqns (Set)
"return list of all eqns within a solution EqnSet"
 (remove-duplicates  
    (list-base-eqns  ; ignores derived
       (collect-bgnodes->eqns (EqnSet-Eqns Set)))))

;; Following format used by "pe" sgg function
(defun print-numbered-eqnset (Num Set &optional (Stream t) (Level 0))
  "Print a particular eqn set with its number." 
  (pprint-indent :block Level Stream)
  (format Stream "~A:-----------------~%" Num)
  (format Stream "Equations:~%")
  (dolist (soleqn (eqnset-eqns Set))
     (format Stream "~A~%        ~A ~A~%" (enode-algebra soleqn) (Enode-id soleqn) (enode-note soleqn)))
  ; not useful -AW
  ;(format Stream "~%Nodes:  ~W~2%" (eqnSet-Nodes Set))
  )

(defun enode-note (enode)
;; return printable "note" showing if eqnode represents COMBINABLE or MAJOR 
;; eqn  NB: this makes use of eqn index in *cp* and predicates in 
;; Help/interpret-equation
  (let ((eqn (find-exp->eqn (enode-id enode) (Problem-EqnIndex *cp*)))) ; find in index
      (cond ((null eqn) "")
            ((combinable-eqn-p eqn) "[COMBINABLE]")
            ((major-eqn-p eqn) "[MAJOR]")
	    (T ""))))

;;; For the purporses of answer reporting this code prints
;;; out the contents of an eqnset in report fasion I.E.
;;; it prints the psms then the entries and equations.
(defun print-numbered-report-eqnset (Set &optional (Stream t) (level 0))
  (format Stream "Problem Solution Methods.~%") 
  (format Stream "~{    ~S~%~}" 
	  (mapcar #'Enode-id (EqnSet-Eqns set)))
  (format Stream "Entries.~%")
  (print-eqnset-entries Set Stream Level)
  (format Stream "Implicit Equations~%")
  (print-eqnset-implicit-eqns Set Stream Level)
  (format Stream "Explicit Equations~%")
  (print-eqnset-explicit-eqns Set Stream Level))


(defun print-eqnset-entries (Set Stream Level)
  (declare (ignore Level))
  "Print the eqnset entries."
  (format Stream "~{    ~S~%~}" 
	   (remove-if-not 
	    #'(lambda (e) (and (any-entryprop-p e)
			       (not (any-eqn-entryprop-p e))))
	    (mapunion #'collect-psmgraph-csdo-effects
		      (mapcar #'(lambda (n) 
				  (cond ((Enode-p n) (enode-path n))
					((Qnode-p n) (qnode-path n))
					(t nil))) ; shouldn't happen now
			      (Eqnset-Nodes Set))
		      :test #'unify)))) ;could use "equal"
		     
(defun print-eqnset-implicit-eqns (Set Stream Level)
  "Print the implicit equations in the EqnSet."
  (declare (ignore Level))
  (format Stream "~{    ~A~%~}" 
	   (mapcar #'eqn-algebra
		   (remove-if-not
		    #'(lambda (e) (eql (eqn-type e) 'implicit-eqn))
		    (mappend #'Enode-Subeqns (EqnSet-Eqns Set)))))) 
     
(defun print-eqnset-explicit-eqns (Set Stream Level)
  "Print the explicit equations in the EqnSet."
  (declare (ignore Level))
  (format Stream "~{    ~A~%~}" 
	   (mapcar #'eqn-algebra
		   (remove-if
		    #'(lambda (e) (eql (eqn-type e) 'implicit-eqn))
		    (mappend #'Enode-Subeqns (EqnSet-Eqns Set))))))  

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
          ; In initial Version NIL format, what was written was the expansion of the concise sgg 
	  ; solution representation (as list of psm enodes) into the full list of all (non-derived)
	  ; equations within the nodes, which may include implicit and subsidiary equations. 
	  ; These had to be referenced by equation indices, not enode indices.
	  ; Version 2 changes to store the same representation as sgg generates, so just 
	  ; writes enode indices to the file. 
	  ; (mapcar #'Eqn-Index  (EqnSet-AllEqns Set))  ; writes old format
	  (collect-nodes->gindicies (EqnSet-Eqns Set))  ; new format
	  (collect-nodes->gindicies (EqnSet-Nodes Set))
	  (EqnSet-Assumpts Set)
	  ))

(defun eqns-to-enodes (eqns Graph)
 "collect enodes for psm eqns in list, ignoring all other eqns"
 ; note this doesn't work for solutions using cons-energy or change-ME because
 ; their psm equation is a derived-eqn so not included in the set of all soln eqns.
     (remove-if #'null
          (mapcar #'(lambda (eqn) 
		       (match-algebra->enode (eqn-algebra eqn) Graph))
		  eqns)))

(defun read-mreadable-EqnSets (Stream EqnIndex Graph Version)
  (loop for S in (read Stream "Error: Malformed Equation Sets.")
      collect (make-EqnSet 
	       :Eqns (cond ; switch on file version
	               ((null Version) ; old format: try to convert
	                  (eqns-to-enodes (collect-indicies->eqns (first S) EqnIndex) Graph))
	               ((and (numberp Version) (>= Version 2)) ; new format
	                 (map-indicies->bgnodes (first S) Graph)))
	       :Nodes  (map-indicies->bgnodes (second S) Graph)
	       :Assumpts (third S))))

 (defun solution-equalp (B1 B2)
   "Determine iff two solutions are set-equal."
   (and (equal-sets (Solution-Soughts B1)
                   (Solution-Soughts B2))
        (equal-sets (Solution-Knowns B1)
                   (Solution-Knowns B2))
        (equal-sets (Solution-Assumptions B1)
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



  
