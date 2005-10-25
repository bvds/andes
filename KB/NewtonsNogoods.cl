;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NewtonsNogoods.cl
;; Collin Lynch
;; 01/11/2001
;;
;; This file defines nogoods for the Newtons2.cl physics code.
;;

(defnogood only-one-form-of-dot
  ((using-dot ?a ?b ?x1-rot)
   (using-dot ?a ?b ?x2-rot)
   (test (not (equal ?x1-rot ?x2-rot))))
  :Specs ("Prevents the use of more than form of dot product")
  :message (Only one form of dot ?a ?b))


;; mainly redundant with multiple-coords-for-body
(defnogood Max-axis-compo-free
  ((using-compo-free (?a ?v ?r1 ?b))
   (using-compo-free (?a ?v ?r2 ?b))
   (test (not (equalp ?r1 ?r2))))
  
  :Specs ("Prevents the use of more than one compo-free eqn per body and vector.")
  :message (Max compo-free eqns per axis ?v ?r1 ?r2))
  

;; Algebraic independence alone will not in general prohibit using equations
;; projecting a single body's vectors along non-orthogonal pairs of axis 
;; directions for a single body or from using multiple axes on the same body';s
;; vectors for different parts of the problem. Although this might conceivably 
;; be a useful strategy in some problems, it is not a usual problem-solving 
;; practice for introductory problems and is not expected to be useful 
;; in any Andes problems.  
;
;; It is still undecided whether we will enforce constraints on settings of
;; axes in within a single solution. Following are nogoods that could do this. 
;; Note they constrain the axis drawing itself, this is more fundamental than
;; choice of equations.
;
;; Note that since the Andes axis tool always draws a coordinate *system* of 
;; two orthogonal axes, the following two ways of formulating axes constraints 
;; should be entirely equivalent: if you have multiple inequivalent coordinate
;; systems for the same body, then there must exist a non-orthogonal x-y axis 
;; pair, and vice versa. So only one should really be needed unless we modify 
;; the axis tool to allow setting one axis direction only.

(defnogood multiple-coords-for-body
    ((axis-for ?b x ?rot1)
     (axis-for ?b x ?rot2)
     (test (not (equal ?rot1 ?rot2))))
  :specs ("prevent use of multiple coordinate systems for same body and time")
  :message ("Multiple inequivalent coordinate systems for ?b with x at ?rot1 and ?rot2"))


(defnogood non-orthogonal-axes
    ((axis-for ?b x ?x-rot)
     (axis-for ?b y ?y-rot)
     (test (not (= ?y-rot (+ ?x-rot 90)))))
  :Specs ("Prevent use of non-orthogonal xy axes pairs for same body and time")
  :message (Non-orthogonal axes for body ?b at ?t x ?x-rot y ?y-rot))

;; Following heuristic rule to limit explosion of solutions where both compound
;; and individual bodies can be used: must use same axes on all of them
;; It represents the simple rule of picking the same axes for the whole solution
(defnogood diff-axes-compound-part
    ((axis-for (compound . ?bodies) x ?rot1)
     (axis-for ?b x ?rot2)
     (test (member ?b ?bodies :test #'equal))
     (test (not (equal ?rot1 ?rot2))))
   :Specs ("Prevent use of different axes for compound and for part in same solution")
   :message (Different axes used for part ?b and compound of ?bodies))

;; don't use both Net-force and sum of forces form of Newton's Law
(defnogood redundant-NSL-forms
    ((using-compo-free (?eq1 ?xyz1 ?rot1 (NL ?b ?t)))
     (using-compo-free (?eq2 ?xyz2 ?rot2 (NL-net ?b ?t))))
  :specs ("Prevent combining different ways of writing Newton's Law -- net force and sum of forces form -- on a body and time")
  :message (Redundant NL forms ?b ?t))


  
     
     
