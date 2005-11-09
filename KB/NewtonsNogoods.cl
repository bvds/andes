;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NewtonsNogoods.cl
;; Collin Lynch
;; 01/11/2001
;;
;; This file defines nogoods for the Newtons2.cl physics code.
;;
;; Nogood rules restrict allowable combinations of psm equations into 
;; solution sets, operating on assumptions defined in the psms. Assumptions
;; are arbitrary propositions introduced by (assume ....) in the effects
;; of an operator; their only effect is in these nogood rules.
;;
;; Note: associated messages not currently printed by the sgg.


;; Algebraic independence alone will not in general prohibit using equations
;; projecting a single body's vectors along non-orthogonal pairs of axis 
;; directions for a single body or from using multiple axes on the same body's
;; vectors for different parts of the problem. Although this might conceivably 
;; be a useful strategy in some problems, it is not a usual problem-solving 
;; practice for introductory problems and is not expected to be useful 
;; in any Andes problems.  
;
;; Following rules constrain the axis drawing itself, this is more fundamental 
;; than choice of equations to combine in a solution.
;
;; Note that since the Andes axis tool always draws a coordinate *system* of 
;; two orthogonal axes, the following two ways of formulating axes constraints 
;; should be entirely equivalent: if you have multiple inequivalent coordinate
;; systems for the same body, then there must exist a non-orthogonal x-y axis 
;; pair, and vice versa. So only one should really be needed unless we modify 
;; the axis tool to allow setting one axis direction only.

(defnogood multiple-coords-for-body
    ((axis-for ?b ?xyx1 ?rot1)
     (axis-for ?b ?xyz2 ?rot2)
     (test (not (equal ?rot1 ?rot2))))
  :specs ("prevent use of multiple coordinate systems for same body")
  :message ("Multiple inequivalent coordinate systems for ?b with x at ?rot1 and ?rot2"))

;; Prevent applying same compo-equation along x [or y] axis with
;; different rotations in the same solution. e.g. NSL at x=0 and x=30 deg
;; this constraint now probably redundant with multiple-coords-for-body
(defnogood Max-axis-compo-free
    ;; assumption args are from compo-eqn-id, eg:
    ;;      (compo-eqn avg-vel ?xy ?rot (avg-velocity ?b ?t)))
    ((using-compo-free (?id ?xyz1 ?rot1 ?family-id))
     (using-compo-free (?id ?xyz2 ?rot2 ?family-id))
     (test (not (equal ?rot1 ?rot2))))
  :Specs ("Prevents the use of same compo-free eqn at different x or y axis rotations")
  :message (Max compo-free eqns per axis ?id ?rot1 ?rot22))

;; Following rule limits explosion of solutions where both compound and 
;; individual bodies can be used: must use same axes on all of them.  It 
;; represents the simple rule of picking the same axes for the whole solution.
(defnogood diff-axes-compound-part
    ((axis-for (compound . ?bodies) ?xyz1 ?rot1)
     (axis-for ?b ?xyz2 ?rot2)
     (test (member ?b ?bodies :test #'equal))
     (test (not (equal ?rot1 ?rot2))))
   :Specs ("Prevent use of different axes for compound and for part in same solution")
   :message (Different axes used for part ?b and compound of ?bodies))

;; don't use both Net-force and sum of forces form of Newton's Law
(defnogood redundant-NSL-forms
    ((using-compo-free (NSL ?xyz1 ?rot1 ?family-id))
     (using-compo-free (NSL-net ?xyz2 ?rot2 ?family-id)))
  :specs ("Prevent combining different ways of writing Newton's Law -- net force and sum of forces form -- on a body and time")
  :message (Redundant NSL forms for ?family-id))

(defnogood only-one-form-of-dot
  ((using-dot ?a ?b ?x1-rot)
   (using-dot ?a ?b ?x2-rot)
   (test (not (equal ?x1-rot ?x2-rot))))
  :Specs ("Prevents the use of more than form of dot product")
  :message (Only one form of dot for ?a ?b))

;;;
;;;  don't use both impulse and force versions of Newton's third law
;;;

(defnogood only-one-form-of-NTL
    ((using-NTL ?bodies ?type ?t)
     (using-NTL-impulse ?bodies ?t))
  :specs ("Prevent combining different ways of writing Newton's third Law -- net force and impulse form -- on two bodies at a time")
  :message (Redundant NTL forms for ?bodies at ?t))
