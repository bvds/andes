;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PsmGraph.cl
;; Collin Lynch 
;; 4/10/2001
;; This file defines the PSM Graph and associated 
;; code within it.  At SGG time the graph is stored
;; as a flat list of lists.  At Help time this list
;; needs to be modified to contain the structs
;; encapsulating the information.  These structs can then 
;; be indexed to generate the Entry table.
;;
;; This file contains code to index the graph and to 
;; obtain other informaton from it.



;;=============================================================
;; PSM Graph.
;; Equation nodes and given Qnodes have psm graphs below them.
;; the code in this section deals with those lists and the
;; modification of them.

(defun csstruct-p (wm)
  "Is the node a structural element."
  (or (cssplit-p wm)
      (csnext-p wm)
      (csjoin-p wm)
      (cschoose-p wm)))

(defun cssplit-p (wm) 
  (equalp wm 'split))

(defun csnext-p (wm)
  (equalp wm 'Next))

(defun csjoin-p (wm)
  (equalp wm 'join))

(defun cschoose-p (wm)
  (and (listp wm)
       (equalp (car wm) 'choose)))

(defun csstep-p (wm)
  "Is the em element a cognitive step."
  (or (cswm-p wm)
      (cssg-p wm)
      (csop-p wm)
      (csdo-p wm)))

(defun cswm-p (wm) 
  (and (listp wm)
       (equalp (car wm) 'wm)))

(defun csop-p (wm)
  (and (listp wm)
       (equalp (car wm) 'op)))


(defstruct (cssg (:type list))
  (Label 'sg)
  op
  goal)

(defun cssg-p (wm)
  (and (listp wm)
       (equalp (car wm) 'sg)))


(defstruct (csdo (:type list))
  (Label 'do)
  op
  effects
  varvals
  entries)

(defun do->csdo (do)
  "Convert a four element do into the five element csdo."
  (make-csdo :op (nth 1 do)
	     :effects (nth 2 do)
	     :varvals (nth 3 do)))
  
(defun csdo-p (wm)
  (and (listp wm)
       (equalp (car wm) 'do)))

;; SystemEntry and csdo are interdependent structures

(defun SystemEntries->csdos (entries)
  "return all distinct csdos associated with a list of SystemEntries"
  (sort (remove-duplicates (mappend #'SystemEntry-Sources entries)
			   :key #'csdo-op :test #'unify) 
	#'expr< :key #'csdo-op))

(defun csdo-enteredp (do)
  "Return t iff the entry attatched to the do has been entered."
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
      (cond ((equalp (car (nth n Graph)) 'do)
	     (setf (nth N Graph) (do->csdo (nth N Graph)))
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



;;----------------------------------------------------------------
;; pmgraph-path-enteredp 
;; In order to determine if a specific enode has been entered we 
;; conduct a depth-first traversal of the graph returning t when
;; we encounter a path that has been completely entered or nil
;; otherwize.  
;;
;; becuase of the split/next/join nodes we cannot
;; guarantee that the last element in the stack contains all of the
;; elements as prerequisites but we can use it as a heuristic.
;; Requires systementries to be loaded.

(defun psmg-path-enteredp (psmg)
  "Return t iff the psmg is entered."
  (cond ((null psmg) t)
	
	((not (listp (car psmg)))
	 (psmg-path-enteredp (cdr psmg)))
	
	((csdo-p (car psmg))
	 (psmg-csdo-enteredp psmg))
	
	((and (listp (car psmg))
	      (eq (caar psmg) 'CHOOSE))
	 (psmg-choose-path-enteredp (cdar psmg)))
	
	(t (psmg-path-enteredp (cdr psmg)))))


;;; If the search encounters a csdo it continues
;;; iff the csdo has no entry or the csdo has 
;;; been entered.
(defun psmg-csdo-enteredp (psmg)
  "If the car of psmg is a csdo."
  (cond ((not (csdo-entries (car psmg)))
	 (psmg-path-enteredp (cdr psmg)))
	((csdo-enteredp (car psmg))
	 (psmg-path-enteredp (cdr psmg)))))


(defun psmg-choose-path-enteredp (Choices)
  "Return t iff one of the chooses is entered."
  (loop for C in Choices
      when (psmg-path-enteredp C)
      return t))


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


;;; psm-optags
;;; Collect the operator tags that are used in the 
;;; psmgraph supplied and return them for later use.
(defun collect-psmgraph-optags (graph)
  "Collect the op tags from the psm graph."
  (psmgraph-mapcar
   #'(lambda (d) (if (csdo-p d) (csdo-op d)))
   graph))

;;; Psm-effects
;;; Collect the effects from the psmgraph and return them.
(defun collect-psmgraph-csdo-effects (graph)
  "Collect the effects from the psmgraph csdos."
  (remove-duplicates 
   (mappend #'csdo-effects (collect-psmgraph-csdos graph))
   :test #'equalp))

;;; "Effect-Opinst pair" -- two-element list whose first element is 
;;; a list of effect proposition and whose second entry is the instance
;;; form for the op that has those effects. Utility for reporting.
;;; NB: effects are lists and might not all be entry propositions. So 
;;; still must filter to find entry props
(defun collect-psmgraph-effect-op-pairs (graph)
  (psmgraph-mapcar 
      #'(lambda (d) (when (csdo-p d) 
                       (list (csdo-effects d) (csdo-op d))))
      graph))


