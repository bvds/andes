;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NewtonsNogoods.cl
;; Collin Lynch
;; 01/11/2001
;;
;; This file defines nogoods for the Newtons2.cl physics code.
;;
(clear-nogoods)		;; reset list on loading this file

#| independence checking should handle this
(defnogood lk-max
    ((using lk-eqn ?b ?t1 ?t2 ?xyz ?rot) ; match one lk application to bind variables
     
					; and make sure no more than 2 with same vars
     (count ?lk-eqns (using lk-eqn ?b ?t1 ?t2 ?xyz ?rot))
     (test (< 2 ?lk-eqns)))
  
  :specs ("Defines the lk-max nogood.")
  :message (Max Lk Eqns supplied ?lk-eqns))			
|#

#|
(defnogood rk-max
  ((using-rk-eqn ?b ?t1 ?t2)
  (count ?rk-eqns (using-rk-eqn ?b ?t1 ?t2))
  (test (< 2 ?rk-eqns)))
  :specs ("Prevent use of more than two rotkin equations")
  :message (Redundant rotational kinematic equations used ?rk-eqns)
  )
|#
  
;; ??? do we still need this ??? - AW  
(defnogood Max-axis-compo-free
    ((using-compo-free (?a ?v ?r1 ?b))
     (using-compo-free (?a ?v ?r2 ?b))
     (test (not (equalp ?r1 ?r2))))
  
  :Specs ("Prevents the use of more than one compo-free eqn per body and vector.")
  :message (Max compo-free eqns per axis ?v ?r1 ?r2))
  
#|
  (defnogood conflicting-compo-free-rotations
  ((using-compo-free (?a ?v1 ?r1 ?b))
  (using-compo-free (?a ?v2 ?r2 ?b))
  (test (neq ?v1 ?v2))
  (test (not (eql 90 (abs (- ?r1 ?r2))))))
  
  :Specs ("Ensures that the compo free eqns are compatible.")
  :message (Conflicting compo-free eqn rotations ?v1 ?r1 ?v2 ?r2))
|#

; Algebraic independence alone will not in general prohibit using equations
; projecting a single body's vectors along non-orthogonal pairs of axis 
; directions for a single body or from using multiple axes on the same body';s
; vectors for different parts of the problem. Although this might conceivably 
; be a useful strategy in some problems, it is not a usual problem-solving 
; practice for introductory problems and is not expected to be useful 
; in any Andes problems.  
;
; It is still undecided whether we will enforce constraints on settings of
; axes in within a single solution. Following are nogoods that could do this. 
; Note they constrain the axis drawing itself, this is more fundamental than
; choice of equations.
;
; Note that since the Andes axis tool always draws a coordinate *system* of 
; two orthogonal axes, the following two ways of formulating axes constraints 
; should be entirely equivalent: if you have multiple inequivalent coordinate
; systems for the same body, then there must exist a non-orthogonal x-y axis 
; pair, and vice versa. So only one should really be needed unless we modify 
; the axis tool to allow setting one axis direction only.

(defnogood multiple-coords-for-body
    ((axis-for ?b ?t x ?rot1)
     (axis-for ?b ?t x ?rot2)
     (test (not (equal ?rot1 ?rot2))))
  :specs ("prevent use of multiple coordinate systems for same body and time")
  :message ("Multiple inequivalent coordinate systems for ?b at ?t with x at ?rot1 and ?rot2"))


(defnogood non-orthogonal-axes
    ((axis-for ?b ?t x ?x-rot)
     (axis-for ?b ?t y ?y-rot)
     (test (not (= ?y-rot (+ ?x-rot 90)))))
  :Specs ("Prevent use of non-orthogonal xy axes pairs for same body and time")
  :message (Non-orthogonal axes for body ?b at ?t x ?x-rot y ?y-rot))

; Following heuristic rule to limit explosion of solutions where both compound
; and individual bodies can be used: must use same axes on all of them
; It represents the simple rule of picking the same axes for the whole solution
(defnogood diff-axes-compound-part
    ((axis-for (compound . ?bodies) ?t1 x ?rot1)
     (axis-for ?b ?t2 x ?rot2)
     (test (member ?b ?bodies :test #'equal))
     (test (not (equal ?rot1 ?rot2))))
   :Specs ("Prevent use of different axes for compound and for part in same solution")
   :message (Different axes used for part ?b and compound of ?bodies))

; don't use both Net-force and sum of forces form of Newton's Law
(defnogood redundant-NSL-forms
    ((using-compo-free (?eq1 ?xyz1 ?rot1 (NL ?b ?t)))
     (using-compo-free (?eq2 ?xyz2 ?rot2 (NL-net ?b ?t))))
  :specs ("Prevent combining different ways of writing Newton's Law -- net force and sum of forces form -- on a body and time")
  :message (Redundant NL forms ?b ?t))

;; compound body nogoods
;; don't use both weight law for compound and general force-compound law
;; don't use both NTL on internal forces and compound-body solution.
;; [? maybe break open compound for different unknown in another part?]

#| ; independence checking should handle this

;; Following only blocks simple redundant triad A=B, B=C, A=C
;; This suffices for our energy problems which don't have more than 3 times
;; We need more general test for interval composed of several sub-intervals 
;; e.g. if have (1 2), (2 4), (4 5), (5 6) then any aggregates are nogood:
;;              (1 4), (1 5), (1 6)
;;                     (2 5), (2 6)
;;                            (4 6)
;; This is difficult to code in our simple nogood language. But expect
;; that an independence checker from our algebra module will handle it.

(defnogood redundant-cons-energy
    ((cons-energy ?b ?ti ?tf)
     (test (> (- ?tf ?ti) 1))		; compound-intervalp
     (cons-energy ?b ?ti ?t2)
     (test (neq ?t2 ?tf))
     (cons-energy ?b ?t2 ?tf))
  :Specs ("Prohibits use of redundant conservation of energy eqns for compound intervals if already applied over parts, e.g. between 1 and 2, 2 and 3 and 1 and 3")
  :message (Redundant cons energy triad ?ti ?t2 ?tf))
|#
  
  
  
(defmacro neq (i j)
  `(not ,`(equalp ',i ',j)))

     
     
