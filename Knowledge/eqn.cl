;; Eqn.cl
;; Collin Lynch
;; 2/7/2001
;;; Modifications by Anders Weinstein 2002-2008
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file defines the Bubblegraph data structures for the 
;; Andes2 help system.  The structures themselves are generated by the
;; code located in sgg/GraphGenerator.cl  This code has been located
;; here to facilitate its use by other parts of the help system.
;;
;; A bubblegraph itself is a list of the form 
;; (<Qnodes> <Enodes> <Vars> <Eqns>) where: 
;; 

;;==============================================================
;; Equation structs are used to catalog the equation elements
;; in a system.
  
(defstruct (eqn (:print-function print-eqn))
  index
  Type	   ;One of Eqn, Given-Eqn, Derived-Eqn  
  Algebra
  Exp
  Nodes
  Solved)  ;if t then this equation has been solved.

(defun print-eqn (Eqn &optional (Stream t) (Level 0))
  "Print out an eqn as a list."
  (pprint-Indent :block Level)
  (format Stream "~S" 
	  (list 'eqn (Eqn-Index Eqn) (Eqn-Type Eqn) (Eqn-Algebra Eqn) 
		(Eqn-Exp Eqn) (Eqn-Nodes Eqn) (Eqn-Solved Eqn))))

(defun print-mreadable-eqn (Eqn &optional (Stream t) (Level 0))
  "Print out an eqn as a list."
  (pprint-Indent :block Level)
  (format Stream "(Eqn ~W" (Eqn-Index Eqn))
  (format Stream " ~W" (Eqn-Type Eqn))
  (format Stream " ~W" (Eqn-Algebra Eqn))
  (format Stream " ~W" (Eqn-Exp Eqn)) 
  (format Stream " ~W" (collect-nodes->gindicies (Eqn-Nodes Eqn)))
  (format Stream " ~W)~%" (Eqn-Solved Eqn)))


(defun print-mreadable-eqns (E S)
  "Print the specified Eindex E in mreadable form."
  (format S "(")
  (dolist (Eq E)
    (print-mreadable-eqn Eq S))
  (format S ")~%"))


(defun read-mreadable-eqns (S G)
  "Read in an Mreadable Eindex associated with Bubblegraph G."
  (loop for E in (read S "Malformed Eqns file")
      collect (EqnList->Eqn E G)))

(defun Eqnlist->Eqn (L Graph)
  "Given a list of an eqn generate an eqn struct from it."
  (make-eqn :Index (nth 1 L)
	    :Type (nth 2 L)
	    :Algebra (nth 3 L)
	    :Exp (nth 4 L)
	    :nodes (collect-gindicies->nodes (nth 5 L) Graph)
	    :Solved (nth 6 L)))

(defun eqns-equalp (X Y)
  "Determine if the two eqns are equal or can be merged."
  (let ((exp (unify (Eqn-Exp X) (Eqn-Exp Y)))
	(alg (equalp (Eqn-Algebra X) (Eqn-Algebra Y)))) ; equalp so 0 matches 0.0
    (when (not (eql (null exp) (null alg)))
      (error "eqns-equalp:  both Algebra and Exp should match:~%     ~S~%     ~S~%" 
	     X Y))
    (and exp alg (merge-eqn-types (Eqn-Type X) (Eqn-Type Y)))))


;;;-----------------------------------------------------------------
;;;
;;;   Rules for merging equations
;;;   Matching equations must have identical Eqn-Algebra and Eqn-Exp
;;;   and Eqn-Type must be mergable.
;;;   Eqn-Index and Eqn-Solved are ignores.

(defun merge-duplicate-eqns (Eqns)
  "Iterate through the list merging duplicate eqns."
  (let ((R (list (first Eqns))))
    (dolist (A (rest Eqns))
      (let ((B (find A R :test #'eqns-equalp)))
	(if B (merge-eqns A B) (push A R))))
    R))

(defun merge-eqns (E1 E2)
  "Merge Eqn 1 into Eqn 2.  These are assumed to be equal under Eqns-Equalp."
  (setf (Eqn-Nodes E2)
	;; equality is when both point to same object
	(union (Eqn-Nodes E2) (Eqn-Nodes E1)))
  (setf (Eqn-Type E2)
	(merge-eqn-types (Eqn-Type E1) (Eqn-Type E2)))
  )

;;  This tells us which equation types can actually be merged
(defun merge-eqn-types (T1 T2)
  "If the two equation types can be merged, return merged type."
  (let ((m1 '(Implicit-Eqn Given-Eqn))
	(m2 '(Implicit-Eqn Eqn)))
    (cond 
     ((eq T1 T2) T1)  ;exact equality 
     ;; AW -- change this to Given-Eqn to treat as given
     ;; in this case.  Arises for given components, Bug 691
     ;; merge Implicit-Eqn and Given-Eqn into Implicit-Eqn
     ((and (member T1 m1) (member T2 m1)) 'Given-Eqn)
     ;; merge Implicit-Eqn and Eqn into Implicit-Eqn
     ((and (member T1 m2) (member T2 m2)) 'Implicit-Eqn)
     (t nil))))


(defun mark-unsolved-eqns (Uvars Eqns)
  "Given the list of equations mark those that contain unsolved vars."
  (loop for E in Eqns
      when (loop for V in Uvars
	       when (contains-sym (Eqn-Algebra E) (Qvar-var V))
	       return t)
      do (setf (Eqn-Solved E) nil)
      else do (setf (Eqn-Solved E) t))
  Eqns)
			     

(defun collect-solved-eqns (Eqns)
  "Get the solved equations from the list."
  (loop for E in Eqns
      when (Eqn-Solved E)
      collect E))

(defun eqns->IndyEqns (Eqns)
  (push-index
   (loop for E in Eqns
       when (Eqn-Solved E)
       collect (list (Eqn-Algebra E)))))

(defun eqns->Help-Sys-Eqns (Eqns)
  (push-index
   (loop for E in Eqns
       when (and (not (eq (eqn-type E) 'Derived-eqn))
		 (Eqn-Solved E))
       collect (list (Eqn-Algebra E)))))

(defun list-base-eqns (Eqns)
  "Collect the list of all base eqns."
  (loop for E in Eqns
      when (not (eq (Eqn-Type E) 'Derived-Eqn))   ;; IE Eqn, Given Eqn and Implicit-Eqn.
      collect E))


;;-----------------------------------------------------------------
;; Equation Index.

(defun Index-eqn-list (Eqns)
  "Set the equation indicies in the list."
  (dotimes (N (length Eqns))
    (setf (Eqn-Index (nth N Eqns)) N))
  Eqns)

(defun find-algebra->eqn (Alg Eqns)
  "Find the eqn that matches Algebra."
  (find Alg Eqns :key #'eqn-Algebra :test #'equal))

(defun find-exp->eqn (Exp Eqns)
  "Obtain the Eqn that is connected to the Exp supplied."
  (find Exp Eqns :key #'Eqn-Exp :test #'unify))


(defun collect-indicies->eqns (Indicies Eqns)
  "Given a list of indicies and a list of vars collect the specified vars."
  (loop for I in Indicies
      collect (collect-index->eqn I Eqns)))


(defun collect-index->eqn (I Eqns)
  "Given a list of indicies and a list of vars collect the specified vars."
  (let ((eqn (nth I eqns)))
    (when (null eqn) (error "Index i=~A larger than ~A eqns~%" 
			    i (length eqns)))
    (when (not (= I (Eqn-Index eqn)))
      (error "Incompatible variable index ~A ~A" I Eqns))
    eqn))
  
