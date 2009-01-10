;; NewtonsNogoods.cl
;; Collin Lynch
;; 01/11/2001
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
  :Specs ("Prevents the use of same compo-free eqn a different x or y axis rotations")
  :message (Max compo eqns per axis ?id ?rot1 ?rot2))

;;;  Don't allow all three forms of LK since any one can be derived
;;;  from the other two.
;;
;; Disallow two components of the same LK along with a second LK:  
;; This suggests that the two components of the first LK are 
;; being used to relate the components of one of the vectors
;; (hence the need for a second LK).
;;

;; This was an issue for dt5a.
(defnogood redundant-lk-equations
  ;; the first two equations could be combined
  ((using-compo (?id1 ?xyz ?rot (lk . ?args)))
   (test (member ?id1 '(lk-no-s lk-no-t lk-no-vf))) 
   ;; ?id2 will also be in this set
   (using-compo (?id2 ?xyz ?rot (lk . ?args)))
   (test (not (eq ?id1 ?id2)))
   ;;
   ;; The third equation must be distinct from the first two.
   ;; In the case that ?xyz3 is distinct from ?xyz, this is simply
   ;; removing more complicated, but logically valid, solutions.
   ;; In the case that ?xyz3 equals ?xyz, this removes truely redundant
   ;; solutions.
   (using-compo (?id3 ?xyz3 ?rot (lk . ?args)))
   (test (member ?id3 '(lk-no-s lk-no-t lk-no-vf))) 
   (test (not (and (eq ?xyz3 ?xyz) (or (eq ?id3 ?id1) (eq ?id3 ?id2)))))
   )
  :Specs ("Prevents the application of three lk equations to the same object.")
  :message (lk equations for projection for . ?args))

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

;; Only one form of cross product for a given component
;; Can't make this stronger since cross-zero is listed as "angle-form"

(defnogood only-one-form-of-cross
  ((using-cross ?a ?b ?xyz ?rot ?flag1)
   (using-cross ?a ?b ?xyz ?rot ?flag2)
   (test (not (eq ?flag1 ?flag2))))
   :Specs ("Prevents the use of more than form of cross product")
  :message (Only one form of cross product for ?a ?b))

(defnogood only-one-form-of-loop-rule
  ((using-loop-rule ?b1 ?p1 ?t)
   (using-loop-rule ?b2 ?p2 ?t)
   (test (not (equal-sets ?b1 ?b2)))
   (test (equal-sets (flatten ?p1) (flatten ?p2)))
   )
  :Specs ("Prevents mulitiple application of loop rule to a given loop.")
  :message (Only one form of loop rule for ?b1 ?b2))

;;;
;;;  don't use both net-force and explicit versions of Newton's second law
;;;

(defnogood only-one-form-of-NL
    ((using-NL ?type1 ?b ?t)
     (using-NL ?type2 ?b ?t)
     (test (not (eq ?type1 ?type2))))
  :specs ("Prevent combining different ways of writing Newton's second law")
  :message (Redundant NL forms for ?b at ?t))


;;;
;;;  don't use both impulse and force versions of Newton's third law
;;;

(defnogood only-one-form-of-NTL
    ((using-NTL ?bodies ?type ?t)
     (using-NTL-impulse ?bodies ?t))
  :specs ("Prevent combining different ways of writing Newton's third law -- net force and impulse form -- on two bodies at a time")
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

;;; Energy conservation is expressed in two ways:
;;;    cons-energy and change-me
;;; We want to apply only one at a time.
;;; cons-energy is a special case of change-me.
(defnogood one-form-of-energy-conservation
    ((using-energy-conservation ?type1 ?b ?t1 ?t2)
     (using-energy-conservation ?type2 ?b ?t1 ?t2)
     (test (not (eq ?type1 ?type2)))
     )
  :specs ("Use only one form of energy conservation")
  :message (Both ?type1 ?type2 used for energy conservation))
