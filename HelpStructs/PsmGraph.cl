;; PsmGraph.cl
;; Collin Lynch 
;; 4/10/2001
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file defines the PSM Graph and associated 
;; code within it.  At SGG time the graph is stored
;; as a flat list of lists.  At Help time this list
;; needs to be modified to contain the structs
;; encapsulating the information.  These structs can then 
;; be indexed to generate the Entry table.
;;
;; This file contains code to index the graph and to 
;; obtain other informaton from it.

;;; action types used as part of actions that appear in action lists
;;; and solution graphs

(defconstant +do-operator+ 'DO 
  "Action prefix meaning that an operator is executed")
(defconstant +next-operator+ 'SG
  "Action prefix meaning that a subgoal is being started for an operator")
(defconstant +goal-unified-with-fact+ 'WM
  "Action prefix meaning that a goal unified with a working memory element")
(defconstant +goal-unified-with-effect+ 'OP
  "Action prefix meaning that an operator was started")


;;=============================================================
;; PSM Graph.
;; Equation nodes and given Qnodes have psm graphs below them.
;; the code in this section deals with those lists and the
;; modification of them.


(defun cssplit-p (wm) 
  (equalp wm 'split))

(defun csnext-p (wm)
  (equalp wm 'Next))

(defun csjoin-p (wm)
  (equalp wm 'join))

(defun cschoose-p (wm)
  (and (listp wm)
       (eql (car wm) 'choose)))

(defun cswm-p (wm) 
  (and (listp wm)
       (eql (car wm) +goal-unified-with-fact+)))

(defun csop-p (wm)
  (and (listp wm)
       (eql (car wm) +goal-unified-with-effect+)))


(defstruct (cssg (:type list))
  (Label +next-operator+)
  op
  goal)

(defun cssg-p (wm)
  (and (listp wm)
       (eql (car wm) +next-operator+)))


(defstruct (csdo (:type list))
  (Label +do-operator+)
  op
  effects
  varvals
  entries  ;only used by help system
  )
  
(defun csdo-p (wm)
  (and (listp wm)
       (eql (car wm) +do-operator+)))

;; SystemEntry and csdo are interdependent structures

(defun csdo-enteredp (do)
  "Return t iff the entry attached to the do has been entered."
  (let ((Entries (remove-if #'systementry-implicit-eqnp 
			    (csdo-entries do))))
    (and Entries 
	 (loop for e in Entries 
	     always (SystemEntry-Entered e)))))

;;--------------------------------------------------------------
;; psm graph modification.

(defun psmg->help-psmg (Graph)
  "Given an mreadable psm graph list set it up as necessary."
  (dotimes (n (length Graph))
    (when (listp (nth N Graph))
      (cond ((csdo-p (nth n Graph))
	     (setf (csdo-effects (nth n Graph)) 
		   (kb-effects->help-effects (csdo-effects (nth n graph)))))
	    ((equalp (car (nth n Graph)) 'choose)
	     (setf (nth n Graph) 
	       (psmg-choose->help-psmg (nth n Graph)))))))
  Graph)


(defun kb-effects->help-effects (Effects)
  "Cycle through the list of effects converting them from kb form to help form."
  (loop for E in Effects
      collect (kb-prop->help-prop E)))


(defun psmg-choose->help-psmg (choose)
  (cons 'choose
	(loop for G in (cdr choose)
	    collect (psmg->help-psmg G))))

;;
;;  Find prop in graph
;;

(defun find-prop-in-path (prop path &optional stack)
  "Go through path and find any instances of effect prop, returning prerequisite steps for the associated op."
  (dolist (step path)
    (cond 
      ;; ignore wm, op, sg
      ((or (cswm-p step) (csop-p step) (cssg-p step)) nil)
      ((csdo-p step)
       (when (csdo-entries step) (push step stack))
       (dolist (effect (csdo-effects step))
	 (when (unify effect prop)
	     (return-from find-prop-in-path stack))))
	;; 'SPLIT' (beginning of unordered)  (not implemented)
	;; Choose alternate paths.	 
	((cschoose-p step)
	 (dolist (x (cdr step))
	   (let ((y (find-prop-in-path prop x stack)))
	   (when y (return-from find-prop-in-path Y)))))
	(T (warn "invalid graph form ~A" step)))))
			   

;;;----------------------------------------------------------------
;;; PSM Graph Collections.
;;; For the purposes of some code it is necessary to collect info
;;; on the contents of the PSM Graphs.  This code does that by 
;;; cycling though the graph and pulling out the relevant info.

;;; psmgraph-mapcar
;;; Cycle through each element in the psm graph splitting the 
;;; search applied to chooses and return the results of calling
;;; function on each element.  This code also ignores splits
;;; next's and joins.  Note that this returns a flat set without
;;; ordering or matching the graph form.  Other times this may be 
;;; changed to preserve the ordering but not quite yet.

(defun psmgraph-mapcar (func Graph &optional (Result nil))
  "Apply the mapcar to the graph."
  (cond ((null Graph) Result)

	((not (listp (car Graph)))
	 (psmgraph-mapcar func (cdr Graph) Result))
	
	((and (listp (car Graph))
	      (eq (caar Graph) 'Choose))
	 (psmgraph-choose-mapcar func (cdar Graph) Result))
	
	(t (psmgraph-func-mapcar func Graph Result))))

(defun psmgraph-choose-mapcar (func Choose Result)
  "Apply the mapcar to the elements in choose."
  (cond ((null Choose) Result)
	(t (psmgraph-choose-mapcar
	    func (cdr choose)
	    (append (psmgraph-mapcar func (car Choose))
		    Result)))))

(defun psmgraph-func-mapcar (func Graph Result)
  "Apply the function to the car of graph and cycle."
  (let ((tmp (funcall func (car Graph))))
    (if (null tmp) (psmgraph-mapcar func (cdr Graph) Result)
	(psmgraph-mapcar func (cdr Graph) (cons tmp Result)))))


;;; psm-opapps
;;; Given a psm graph collect all of the operator apps 
;;; within the graph via the psmgraph mapcar code.
(defun collect-psmgraph-csdos (graph)
  "Collect the csdo's from the psmgraph."
  (psmgraph-mapcar 
   #'(lambda (d) (if (csdo-p d) d))
   graph))


;;; Psm-effects
;;; Collect the effects from the psmgraph and return them.
(defun collect-psmgraph-csdo-effects (graph)
  "Collect the effects from the psmgraph csdos."
  (remove-duplicates 
   (mappend #'csdo-effects (collect-psmgraph-csdos graph))
   :test #'equalp))
