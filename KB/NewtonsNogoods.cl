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
    ((axes-for ?b ?rot1)
     (axes-for ?b ?rot2)
     (test (not (equal ?rot1 ?rot2))))
  :specs ("prevent use of multiple coordinate systems for same body")
  :message (Multiple inequivalent coordinate systems for ?b with x at ?rot1 and ?rot2))

(defnogood multiple-coords-for-move-together
    ((move-together orderless . ?bodies)
     (axes-for ?b1 ?rot1)
     (axes-for ?b2 ?rot2)
     (test (not (equal ?b1 ?b2)))
     (test (not (equal ?rot1 ?rot2)))
     (test (member ?b1 ?bodies :test #'equal))
     (test (member ?b2 ?bodies :test #'equal)))
  :specs ("prevent use of multiple coordinate systems for bodies that are moving together")
  :message (Multiple inequivalent coordinate systems for ?b1 and ?b2))

;; Prevent applying same compo-equation along x [or y] axis with
;; different rotations in the same solution. e.g. NSL at x=0 and x=30 deg
;; this constraint now probably redundant with multiple-coords-for-body
(defnogood Max-axis-compo
    ;; assumption args are from compo-eqn-id, eg:
    ;;      (compo-eqn avg-vel ?xy ?rot (avg-velocity ?b ?t)))
    ((using-compo (?id ?xyz1 ?rot1 ?family-id))
     (using-compo (?id ?xyz2 ?rot2 ?family-id))
     (test (not (equal ?rot1 ?rot2))))
  :Specs ("Prevents the use of same compo-free eqn at different x or y axis rotations")
  :message (Max compo eqns per axis ?id ?rot1 ?rot2))



;; This was an issue for dt5a.
(defnogood redundant-lk-equations
  ;; The solver seems to have trouble determining independence.
  ;; This is problably an issue because lk-no-t and lk-no-vf are non-linear.
  ((using-compo (lk-no-vf ?xyz1 ?rot (lk . ?args)))
   (using-compo (lk-no-s ?xyz2 ?rot (lk . ?args)))
   (using-compo (lk-no-t ?xyz3 ?rot (lk . ?args))))
  :Specs ("Prevents the application of three lk equations to the same object.")
  :message (Max lk equations for . ?args))

;; This was an issue for dt5a.
(defnogood redundant-lk-components
  ;; Don't apply more than one component of a lk equation.
  ;; This is redundant since the direction of motion is known.
  ;; The solver seems to have trouble determining independence.
  ;; This is problably an issue because lk-no-t and lk-no-vf are non-linear.
  ((using-compo (?id ?xyz1 ?rot (lk . ?args)))
   (using-compo (?id ?xyz2 ?rot (lk . ?args)))
   (test (not (equal ?xyz1 ?xyz2))))
  :Specs ("Prevents the application of three lk equations to the same object.")
  :message (Multiple components of lk equations for . ?args))

;; Following rule limits explosion of solutions where both compound and 
;; individual bodies can be used: must use same axes on all of them.  It 
;; represents the simple rule of picking the same axes for the whole solution.
(defnogood diff-axes-compound-part
    ((axes-for (compound orderless . ?bodies) ?rot1)
     (axes-for ?b ?rot2)
     (test (member ?b ?bodies :test #'equal))
     (test (not (equal ?rot1 ?rot2))))
   :Specs ("Prevent use of different axes for compound and for part in same solution")
   :message (Different axes used for part ?b and compound of ?bodies))

(defnogood only-one-form-of-hat
  ((using-hat ?vec ?rot1 ?form1)
   (using-hat ?vec ?rot2 ?form2)
   (test (not (and (equal ?rot1 ?rot2) (equal ?form1 ?form2)))))
  :Specs ("Prevents the use of more than form of unit vector")
  :message (Only one form of hat for ?vec))

(defnogood only-one-form-of-dot
  ((using-dot ?a ?b ?x1-rot)
   (using-dot ?a ?b ?x2-rot)
   (test (not (equal ?x1-rot ?x2-rot))))
  :Specs ("Prevents the use of more than form of dot product")
  :message (Only one form of dot for ?a ?b))

(defnogood only-one-form-of-cross
  ((using-cross ?a ?b ?xyz1 ?rot1 ?flag1)
   (using-cross ?a ?b ?xyz2 ?rot2 ?flag2)
   (test (not (and (equal ?flag1 ?flag2) (equal ?rot1 ?rot2)))))
   :Specs ("Prevents the use of more than form of cross product")
  :message (Only one form of cross product for ?a ?b))

;;;
;;;  don't use both net-force and explicit versions of Newton's second law
;;;

(defnogood only-one-form-of-NSL
    ((using-NSL ?type1 ?b ?t)
     (using-NSL ?type2 ?b ?t)
     (test (not (eq ?type1 ?type2))))
  :specs ("Prevent combining different ways of writing Newton's second Law")
  :message (Redundant NSL forms for ?b at ?t))


;;;
;;;  don't use both impulse and force versions of Newton's third law
;;;

(defnogood only-one-form-of-NTL
    ((using-NTL ?bodies ?type ?t)
     (using-NTL-impulse ?bodies ?t))
  :specs ("Prevent combining different ways of writing Newton's third Law -- net force and impulse form -- on two bodies at a time")
  :message (Redundant NTL forms for ?bodies at ?t))

;;;
;;;  don't use both forms of Snell's law
;;;

;; Also used for total internal reflection, Brewster's angle
(defnogood only-one-form-of-snells-law
    ((using-snells-law ?lines ?type1 ?flag1)
     (using-snells-law ?lines ?type2 ?flag2)
     (test (not (and (eq ?type1 ?type2) (eq ?flag1 ?flag2)))))
  :specs ("Prevent using both forms of Snell's law at once")
  :message (Redundant Snells law forms for ?lines))

;;;
;;;  don't want both component and magnitude form of a vector equation
;;;  Also use to disallow both projection and pyth-theorem for a vector
;;;

(defnogood component-or-magnitude-form
    ((using-magnitude ?family-id)
     ;; asserted in apply-vector-PSM* and mag-pyth
     (using-compo (?id ?xyz ?rot ?family-id)) 
     )
  :specs ("Prevent both component and magnitude form of a vector equation")
  :message (Both component and magnitude forms for ?family-id))

;; Get inconsistant solutions in ref3a when both complimentary-angles
;; and angle-direction are used on the same triangle.
(defnogood complimentary-or-angle-direction 
    ((using-complimentary-angles ?ang1 ?ang2)
     (using-angle-direction ?ang1)
     (using-angle-direction ?ang2)
     )
  :specs ("Prevent both component and magnitude form of a vector equation")
  :message (Both component and magnitude forms for ?family-id))
