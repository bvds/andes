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

;;; Geometrical Optics 


;; Optics variables. Question how to define image-distance and object-distance,
;; which are really just distances in the context of optics problems.
;; For simplicity we use two different quantities object-distance and
;; image-distance, which take as single argument the lens wrt which distance is
;; calculated. We don't include time arguments, thus limiting these to static problems. 
;; We also don't include an argument naming the object or image, assuming that the 
;; problem context uniquely determines which is meant. If these decisions turn out 
;; problematic, it shouldn't be difficult to redo with additional arguments.
;;
;; Note we will allow for two touching lenses to be treated as a single compound lens
;; with its own focal length and magnification. These could be represented just 
;; as compound bodies are by complex terms of the form (compound orderless ?lens1 ?lens2).  
;; If that were done any property or equation mentioning a lens would
;; have to be prepared to take a compound lens term as argument.  But in fact interfacing 
;; with the workbench dialog boxes is much easier if we just predefine a single name for 
;; the compound lens in the problem statement, and declare it as such in the problem 
;; statement.  So that is what we do.
;; We also use systems of more than 1 lens for which a net magnification can be defined. 
 
;; ex) "object distance for lens1" by Bob
(def-qexp object-distance (object-distance ?lens)
  :rank scalar
  :symbol-base |do|     
  :short-name "object distance"	
  :units |m|
  :new-english ((the) 
		(or (  "distance"
		       (and (preferred ("of" (the) "object"))
		            (preferred ((or "from" "for") ?lens))))
		    (  "object distance" 
		       (preferred ((or "from" "for") ?lens))))))

(defoperator define-object-distance (?lens)
  :preconditions ( (bind ?do-var (format-sym "do_~A" (body-name ?lens))) )
  :effects ( (variable ?do-var (object-distance ?lens))
             (define-var (object-distance ?lens)))
  :hint (
	 (bottom-out (string "Define a variable for ~A by using ~A."  
			     ((object-distance ?lens) def-np)
			     (*text-tool* eval)
			     )) ))

;; ex) "image distance for lens1" by Bob
(def-qexp image-distance (image-distance ?lens)
  :rank scalar
  :symbol-base |di|     
  :short-name "image distance"	
  :units |m|
  :new-english ((the) 
		(or (  "distance"
		       (and (preferred ("of" (the) "image"))
		            (preferred ((or "from" "for") ?lens))))
		    (  "image distance" 
		       (preferred ((or "from" "for") ?lens))))))

(defoperator define-image-distance (?lens)
  :preconditions ( (bind ?di-var (format-sym "di_~A" (body-name ?lens))) )
  :effects ( (variable ?di-var (image-distance ?lens))
             (define-var (image-distance ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using ~A."  
			   ((image-distance ?lens) def-np)
			     (*text-tool* eval)
			     ))
       ))

(def-qexp focal-length (focal-length ?lens)
  :rank scalar
  :symbol-base |f|     
  :short-name "focal length"	
  :units |m|
  :new-english (property-object "focal length" ?lens)
)

(defoperator define-focal-length (?lens)
  :preconditions ( (bind ?f-var (format-sym "f_~A" (body-name ?lens))) )
  :effects ( (variable ?f-var (focal-length ?lens))
             (define-var (focal-length ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using ~A."  
			   ((focal-length ?lens) def-np)
			     (*text-tool* eval)
			     ))
       ))

(def-qexp magnification (magnification ?lens)
  :rank scalar
  :symbol-base |m|     
  :short-name "magnification"	
  :units NIL
  :new-english (property-object "magnification" ?lens)
) 

(defoperator define-magnification (?lens)
  :preconditions ( (bind ?m-var (format-sym "m_~A" (body-name ?lens))) )
  :effects ( (variable ?m-var (magnification ?lens))
            (define-var (magnification ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using ~A."  
			   ((magnification ?lens) def-np)
			     (*text-tool* eval)
			     ))
       ))

(def-qexp radius-of-curvature (radius-of-curvature ?mirror)
  :rank scalar
  :symbol-base |r|     
  :short-name "radius of curvature"	
  :units |m|
  :new-english (property-object "radius of curvature" ?mirror)
)

(defoperator define-radius-of-curvature (?mirror)
 :preconditions ( (bind ?r-var (format-sym "r_~A" (body-name ?mirror))) )
 :effects ( (variable ?r-var (radius-of-curvature ?mirror))
            (define-var (radius-of-curvature ?mirror)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using ~A."  
			   ((radius-of-curvature ?mirror) def-np)
			     (*text-tool* eval)
			     ))
       ))

(def-qexp distance-between (distance-between orderless . ?objects)
  :rank scalar
  :symbol-base |d|     
  :short-name "distance"	
  :units |m|
  :new-english ((the) "distance between" (conjoin (or "and" "&") . ?objects))
) 

(defoperator define-distance-between (?objects)
 :preconditions 
 ( (bind ?d-var (format-sym "d~{_~A~}" (mapcar #'body-name ?objects))))
 :effects ( (variable ?d-var (distance-between orderless . ?objects))
            (define-var (distance-between orderless . ?objects)) )
 :hint (
       (bottom-out (string "Define a variable for ~A by using ~A."
                           ((distance-between orderless . ?objects) def-np)
			     (*text-tool* eval)
			     ))
       ))

;; student:  "d: separation between slits"
(def-qexp slit-separation (slit-separation ?grating)
  :rank scalar
  :symbol-base |d|     
  :short-name "slit separation"	
  :units |m|
  :restrictions positive
  :new-english ((the) (or "distance" "separation") 
		"between" (the) "slits" (preferred ((or "of" "in") ?grating)))
  )

(defoperator define-slit-separation (?grating)
  :preconditions ( (bind ?do-var (format-sym "ds_~A" (body-name ?grating))) )
  :effects ( (variable ?do-var (slit-separation ?grating))
             (define-var (slit-separation ?grating)))
  :hint (
	 (bottom-out (string "Define a variable for ~A by using ~A."  
			     ((slit-separation ?grating) def-np)
			     (*text-tool* eval)
			     ))
	 ))

(def-qexp resolution-angle (resolution-angle ?grating)
  :rank scalar
  :symbol-base |\\theta|     
  :short-name "angle of resolution"
  :units |deg|
  :restrictions positive
  :new-english ((the) "minimum angle of resolution for" ?grating)
)

(defoperator define-resolution-angle (?grating)
  :preconditions ( (bind ?do-var (format-sym "thetares_~A" (body-name ?grating))) )
  :effects ( (variable ?do-var (resolution-angle ?grating))
             (define-var (resolution-angle ?grating)))
  :hint (
	 (bottom-out (string "Define a variable for ~A by using ~A."  
			     ((resolution-angle ?grating) def-np)
			     (*text-tool* eval)
			     ))
	 ))


;;
;; Optics Equations
;;

; focal length of spherical mirror = r/2
(def-psmclass focal-length-mirror (focal-length-mirror ?mirror)
  :complexity major  ; ??
  :short-name "focal length of mirror"
  :nlg-english ("the focal length of spherical mirror")
  :tutorial "SphericalMirror.html"
  :ExpFormat ("applying the formula for the focal length of a spherical mirror to ~A" (nlg ?mirror))
  :EqnFormat ("f = r/2")) 

(defoperator focal-length-mirror-contains (?sought)
   :preconditions (
     ;; must declare object to be a mirror this way in problem statement
     (mirror ?mirror)
     (any-member ?sought ( (focal-length ?mirror)
                           (radius-of-curvature ?mirror) ))
   )
   :effects (
     (eqn-contains (focal-length-mirror ?mirror) ?sought)
   ))

(defoperator focal-length-mirror (?mirror)
   :preconditions (
       (variable ?f (focal-length ?mirror))
       (variable ?r (radius-of-curvature ?mirror))
   )
   :effects (
     (eqn  (= ?f (/ ?r 2)) (focal-length-mirror ?mirror))
   )
   :hint (
      (point (string "Think about how the focal length of a spherical mirror is related to its radius of curvature."))
      ; maybe this should be considered bottom-out (or just left out):
      (teach (string "For a spherical mirror, the focal length is half the radius of curvature."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?f (/ ?r 2)) algebra) ))
   ))

;;
;; lens/mirror equation
;;
(def-psmclass lens-eqn (lens-eqn ?lens)
  :complexity major
  :short-name "thin lens/mirror equation"
  :nlg-english ("the thin lens/mirror equation")
  :tutorial "LensEquation.html"
  :ExpFormat ("applying the thin lens/mirror equation to ~A" (nlg ?lens))
  :EqnFormat ("1/do + 1/di = 1/f")) 

(defoperator lens-eqn-contains (?sought)
   :preconditions (
     (any-member ?sought ( (image-distance ?lens)
                           (object-distance ?lens)
			   (focal-length ?lens) ))
   )
   :effects (
     (eqn-contains (lens-eqn ?lens) ?sought)
   ))

(defoperator lens-eqn (?lens)
   :preconditions (
       ; don't write this way for object or image at infinity
       (not (infinite (object-distance ?lens)))
       (not (infinite (image-distance ?lens)))
       (variable ?di (image-distance ?lens))
       (variable ?do (object-distance ?lens))
       (variable ?f  (focal-length ?lens))
   )
   :effects (
     (eqn  (= (+ (/ 1 ?do) (/ 1 ?di)) (/ 1 ?f)) (lens-eqn ?lens))
   )
   :hint (
      (point (string "The thin lens/mirror equation can be used to relate object distance, image distance, and focal length."))
      ; maybe this should be bottom-out:
      (teach (string "The thin lens/mirror equation states that the reciprocal of the object distance plus the reciprocal of the image distance equals the reciprocal of the focal length. This equation is valid for all situations provided the appropriate sign conventions are observed."))
      (bottom-out (string "Write the equation ~A" 
                     ((= (+ (/ 1 ?do) (/ 1 ?di)) (/ 1 ?f)) algebra) ))
   ))

; variant of lens equation written when object at infinite distance
(defoperator lens-eqn-obj-at-infinity (?lens)
   :preconditions (
       ; give this way in problem statement:
       (in-wm (infinite (object-distance ?lens)) )
       (variable ?di (image-distance ?lens))
       (variable ?f  (focal-length ?lens))
   )
   :effects (
     (eqn  (= ?di ?f) (lens-eqn ?lens))
   )
   :hint (
      (point (string "Because the image-forming object in this case is effectively at an infinite distance from ~A, the lens/mirror equation reduces to a very simple relation between the image distance and focal length." ?lens))
      (point (string "Rays coming from an very large distance are nearly parallel. Parallel rays incident on a lens or mirror converge at a point, and the focal length specifies the distance to that point. So, for objects at infinity, the image distance equals the focal length."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?di ?f)  algebra) ))
   ))

; variant of lens equation written when image forms at infinite distance (rays parallel)
(defoperator lens-eqn-image-at-infinity (?lens)
   :preconditions (
       ; give this way in problem statement:
       (in-wm (infinite (image-distance ?lens) ))
       (variable ?do (object-distance ?lens))
       (variable ?f  (focal-length ?lens))
   )
   :effects (
     (eqn  (= ?do ?f) (lens-eqn ?lens))
   )
   :hint (
     (point (string "Because the image-forming rays from ~A are effectively parallel, there is a very simple relation between the object distance and focal length of ~a." ?lens ?lens))
      (point (string "Parallel rays mean the image distance is effectively infinite, so the term 1/di in the lens/mirror equation may be treated as zero. Toproduce this situation, the object must therefore be situated at the focal point."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?do ?f)  algebra) ))
   ))


;;
;; Magnification
;;

; could also include definition m = image_height/object_height
; but we have so far avoided using image_height and object_height variables
; (though they probably will be wanted someday)

; Note: turns out psm ids can't exactly match quantity ids, since equations 
; for givens get the quantity id as their equation id, and then uniqueness of
; equation ids would be violated. So we can't call an equation 
;      (magnification ?lens) 
; if that form is also a quantity that might be given.
(def-psmclass magnification-eqn (magnification-eqn ?lens)
  :complexity major
  :short-name "(lateral) magnification"
  :nlg-english ("the formula for (lateral) magnification")
  :tutorial "Magnification.html"
  :ExpFormat ("applying the formula for magnification to ~A" (nlg ?lens))
  :EqnFormat ("m = -di/do")) 

(defoperator magnification-eqn-contains (?sought)
   :preconditions (
     (any-member ?sought ( (magnification ?lens)
                           (image-distance ?lens)
                           (object-distance ?lens) ))
     ; only apply this to atomic lenses, not systems of lenses
     (not (lens-system ?lens . ?dontcare))
   )
   :effects (
     (eqn-contains (magnification-eqn ?lens) ?sought)
   ))

(defoperator magnification-eqn (?lens)
   :preconditions (
       ; can't apply this if object at infinity 
       (not (infinite (object-distance ?lens))) 
       (variable ?m  (magnification ?lens))
       (variable ?di (image-distance ?lens))
       (variable ?do (object-distance ?lens))
   )
   :effects (
     (eqn  (= ?m (- (/ ?di ?do))) (magnification-eqn ?lens))
   )
   :hint (
      (point (string "The lateral magnification of ~a can be computed from the object distance and the image distance." ?lens))
      (point (string "The lateral magnification of a lens or mirror is defined as the ratio of the image height to the object height. Geometry shows that this is equal to minus the ratio of the image distance to the object distance. The negative sign expresses the convention that magnification is negative for an inverted image and positive for an upright image."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?m (- (/ ?di ?do))) algebra) ))
	 ))

;;
;; Combinations of lenses:
;;
;; Idea is to treat image i1 as object for lens2 :
;;   o1........lens1....i1......lens2 .....
;;               |--di1--|--do2--|
;; So di1 + d02 = d12, where d12 = distance betw/lenses. This works
;; even if i1 is not between lenses w/sign conventions on di1, do2.
;;
;; We use a special-purpose variable for distance between lenses, mainly
;; because if we offered a generic distance-between variable students
;; might choose it for things like object-distance, and it won't work.
;; 
(def-psmclass lens-combo (lens-combo ?lens1 ?lens2)
  :complexity major
  :short-name "combination of lenses"
  :nlg-english ("the lens combination relation")
  :tutorial "CombinedLenses.html"
  :ExpFormat ("applying the lens combination relation to ~A and ~A" 
	      (nlg ?lens1) (nlg ?lens2))
  :EqnFormat ("do2 = d12 - di1")) 

(defoperator lens-combo-contains (?sought)
  :preconditions (
  ;; the lens-system statement tells us order of lenses with 
  ;; respect to light source, and gives the distance between them
  (in-wm (lens-system ?sys (?lens1 ?lens2)) )
  (any-member ?sought ( (image-distance ?lens1)
                        (object-distance ?lens2) ))
  )
  :effects (
    (eqn-contains (lens-combo ?lens1 ?lens2) ?sought)
  ))

(defoperator lens-combo (?lens1 ?lens2)
  :preconditions (
     (in-wm (lens-system ?sys (?lens1 ?lens2)))
     (variable ?do2 (object-distance ?lens2))
     (variable ?d12 (distance-between orderless ?lens1 ?lens2))
     (variable ?di1 (image-distance ?lens1))
  )
  :effects (
    (eqn (= ?do2 (- ?d12 ?di1)) (lens-combo ?lens1 ?lens2))
  )
  :hint (
     (point (string "When light passes through more than one lens, you should treat the image formed by the first lens as the object for the second lens."))
     (teach (string "In a series of lenses, set the object distance for the second lens equal to the distance between lenses minus the image distance for the first lens. This relation is correct no matter where the first image is located as long as the standard sign conventions are observed."))
     (bottom-out (string "Write the equation ~A"  
        ((= ?do2 (- ?d12 ?di1)) algebra) ))
  ))

(def-psmclass combo-magnification (combo-magnification ?lens1 ?lens2)
  :complexity major
  :short-name "combined magnification"
  :nlg-english ("combined lens magnification")
  :tutorial "Magnification.html"
  :ExpFormat ("applying the formula for magnification to ~A and ~A" (nlg ?lens1) (nlg ?lens2))
  :EqnFormat ("m12 = m1 m2")) 

(defoperator combo-magnification-contains (?sought)
   :preconditions (
     (in-wm (lens-system ?sys (?lens1 ?lens2)) )
     (any-member ?sought ( (magnification ?sys)
                           (magnification ?lens1)
                           (magnification ?lens2) ))
   )
   :effects (
     (eqn-contains (combo-magnification ?lens1 ?lens2) ?sought)
   ))

(defoperator combo-magnification (?sys)
   :preconditions (
       (in-wm (lens-system ?sys (?lens1 ?lens2)) )
       (variable ?m12 (magnification ?sys))
       (variable ?m1  (magnification ?lens1))
       (variable ?m2  (magnification ?lens2))
   )
   :effects (
     (eqn  (= ?m12 (* ?m1 ?m2)) (combo-magnification ?lens1 ?lens2))
   )
   :hint (
      (point (string "The magnification of a combination of lenses is a simple function of the magnifications of the individual lenses."))
      (point (string "The magnification of a combination of lenses is the product of the magnifications of the individual lenses in the system."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?m12 (* ?m1 ?m2)) algebra) ))
	 ))

; Following computes equivalent focal length of a compound lens, assumed to be
; two touching lenses. Not all books give this equation. One could use the general
; combined lens method with zero distance between lenses instead.

; could recode ala mass-compound (see Newtons2.cl) to allow arbitrary number of lenses.
; Fixed number of two will work for our problems.
(def-psmclass compound-focal-length (compound-focal-length ?lens-name)
  :complexity major
  :short-name "compound lens (touching lenses)"
  :nlg-english ("the focal length of a compound lens")
  :tutorial "TouchingLenses.html"
  :ExpFormat ("applying the formula for the focal length of a compound lens")
  :EqnFormat ("1/f12 = 1/f1 + 1/f2")) 

(defoperator compound-focal-length-contains (?sought)
  :preconditions(
     ; name for compound lens must be defined in the problem statement:
     (compound-lens ?compound (?lens1 ?lens2))
     (any-member ?sought ( (focal-length ?lens1)
                           (focal-length ?lens2)
			   (focal-length ?compound) ))
  )
  :effects (
    (eqn-contains (compound-focal-length ?compound) ?sought)
  ))

(defoperator compound-focal-length (?compound)
  :preconditions(
     (in-wm (compound-lens ?compound (?lens1 ?lens2)))
     (variable ?f1  (focal-length ?lens1))
     (variable ?f2  (focal-length ?lens2))
     (variable ?f12 (focal-length ?compound))
  )
  :effects (
    (eqn (= (/ 1 ?f12) (+ (/ 1 ?f1) (/ 1 ?f2))) 
	 (compound-focal-length ?compound))
  )
  :hint (
    (point (string "When two lenses are touching, you can treat them as a single compound lens with a focal length that is a function of the focal lengths of the two individual lenses."))
    (teach (string "It can be shown that the reciprocoal of the focal length of a compound lens formed by two touching lenses is equal to the sum of the reciprocals of the focal lengths of the two individual lenses."))
    (bottom-out (string "Write the equation ~A" 
         ((= (/ 1 ?f12) (+ (/ 1 ?f1) (/ 1 ?f2))) algebra) ))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                          line drawing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the wrapper (line ...) is probably superfluous, see Bug #1683
;; Need to add trivial wrapper to Ontology, to compensate.
(def-qexp line (line ?r)
  :units nil
  ;; Used in the hint for the line tool in quantity-html-link.
  ;; indefinite noun phrase.
  :short-name "a line" 
  :new-english ?r)

;; Helper routine to give expression for a line
(def-qexp line-between (line-between-points ?a ?b)
  :new-English ((preferred ((the) "line"))
		(or ("from" (conjoin "to" ?a ?b))
		    ("between" (conjoin (or "and" "&") ?a ?b)))))

(defoperator draw-line-given-dir (?r)
  :preconditions
  ( (given (dir (line ?r)) ?dir-in)
    (not (draw-line (line ?r) ?dontcare))
    (test (degrees-or-num ?dir-in))
    (bind ?dir (mod (convert-dnum-to-number ?dir-in) 180))
    (bind ?mag-var (format-sym "l_~A" (body-name ?r)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "draw line ~A at ~A~%" ?r ?dir) )
  :effects ( (draw-line (line ?r) (dnum ?dir |deg|))
	     (variable ?mag-var (mag (line ?r)))
	     (variable ?dir-var (dir (line ?r))))
  :hint 
  ( (bottom-out (string "Use ~A to draw a line for ~A in a direction of ~A degrees with respect to the horizontal." 
			(*line-tool* eval)
			?r ?dir))
    ))

(defoperator draw-line-unknown-dir (?line)
  :preconditions
  (
   ;; This is not ideal, but we use (greater-than ...) as declaration
   ;; for all lines.  
   ;; Note that (greater-than ...) is also used for vectors.
   (greater-than (line . ?line1) (line . ?line2))
   (any-member ?line ((line . ?line1) (line . ?line2)))
   (not (given (dir ?line) ?dir)) ;test direction not given
   (not (draw-line ?line ?dontcare))
   (bind ?mag-var (format-sym "l_~A" (body-name (second ?line))))
   (bind ?dir-var (format-sym "O~A" ?mag-var))
   (debug "draw line ~A in unkown direction~%" ?line)
   )
  :effects
  ( (draw-line ?line unknown)
    (variable ?mag-var (mag ?line))
    (variable ?dir-var (dir ?line)) )
  :hint
  ((bottom-out (string "Use ~A to draw ~A in an unkown direction." 
		       (*line-tool* eval)
		       ?line))
   ))

;;; make trig test variable (for implicit equations only)

;; This is a dummy variable for making restrictions for trig equations
(defoperator define-test-var (?angle)
  :preconditions ((bind ?var (format-sym "test~{_~A~}" ?angle)))
  ;; no define-var for this since no student entry required (or possible)
  :effects ((variable ?var (test-var . ?angle))) )

;;; Snell's Law

(def-psmclass snells-law (snells-law (orderless . ?lines) ?angle-flag)
  :complexity major
  :short-name "Snell's law"
  :nlg-english ("Snell's law")
  :ExpFormat ("using Snell's law for ~A" (nlg ?lines 'conjoined-defnp))
  :eqnFormat ("n1 sin(&theta;1) = n2 sin(&theta;2)"))


(defoperator snells-law-contains (?sought)
  :preconditions 
  ( (snell-system ?normal-to-surface orderless . ?lists)
    (any-member ?lists (((?line1 ?medium1) (?line2 ?medium2))
			((?line2 ?medium2) (?line1 ?medium1))))
    (any-member ?sought ((angle-between orderless (line ?line1) 
					(line ?normal-to-surface))
			 (index-of-refraction ?medium1)
			 (dir (line ?line1))))
    (any-member ?flag (t nil))
    ;; can't do directions if ?sought is angle-between, etc.
    (test (if ?flag (not (eq (first ?sought) 'angle-between))
	    (not (eq (first ?sought) 'dir))))
    (wave-medium ?medium1) (wave-medium ?medium2) ;sanity check
    )
  :effects 
  ( (eqn-contains (snells-law (orderless ?line1 ?line2) ?flag) ?sought) ))

(defoperator use-angle-between-for-angle (?theta)
  :preconditions 
  ((variable ?theta (angle-between orderless ?line ?normal-to-surface)))
  :effects ((angle-expression ?theta ?line ?normal-to-surface nil)))

;; alternative form of angle for cases where the direction
;; of the line is sought
;; All the angles are mod 180 degrees, but this fact should be
;; transparent to the student (and the solver).  Thus, we only 
;; allow directions where the incident angle and angle of refraction
;; can be calculated without taking the mod.
(defoperator use-directions-for-angle1 (?theta)
  :preconditions 
  ( (greater-than ?line ?normal-to-surface)
    (variable ?angle (dir ?line))
    (variable ?anglen (dir ?normal-to-surface))
    (bind ?theta `(- ,?angle ,?anglen)) )
  :effects 
  ( (angle-expression ?theta ?line ?normal-to-surface t)))

(defoperator use-directions-for-angle2 (?theta)
  :preconditions 
  ( (greater-than ?normal-to-surface ?line)
    (variable ?angle (dir ?line))
    (variable ?anglen (dir ?normal-to-surface))
    (bind ?theta `(- ,?anglen ,?angle)) )
  :effects 
  ( (angle-expression ?theta ?line ?normal-to-surface t)))
 
(defoperator write-snells-law (?lines ?angle-flag)
  :preconditions 
  (
   (any-member ?lines ((?line1 ?line2)))
   (snell-system ?normal-to-surface orderless . ?lists)
   (any-member ?lists (((?line1 ?medium1) (?line2 ?medium2))))
   ;;
   (variable ?n1 (index-of-refraction ?medium1))
   (variable ?n2 (index-of-refraction ?medium2))
   (angle-expression ?theta1 (line ?line1) (line ?normal-to-surface) 
		     ?angle-flag)
   (angle-expression ?theta2 (line ?line2) (line ?normal-to-surface) 
		     ?angle-flag)
   ;; ?dummy1 and ?dummy2 are non-negative
   (variable ?dummy1 (test-var ?line1 ?normal-to-surface))
   (variable ?dummy2 (test-var ?line2 ?normal-to-surface))
   (debug "do Snell's law for ~A and ~A~%" ?lines ?angle-flag)
   )
  :effects 
  ( (eqn (= (* ?n1 (sin ?theta1)) (* ?n2 (sin ?theta2))) 
	 (snells-law (orderless . ?lines) ?angle-flag))
    (assume using-snells-law ?lines snell ?angle-flag)
    ;; Implicit equations to enforce the correct root for the sines
    ;; This is needed if one wants to calculate one of the angles
    (implicit-eqn (= ?dummy1 (cos ?theta1)) 
		  (angle-constraint ?angle-flag orderless 
				    (line ?line1) (line ?normal-to-surface)))
    (implicit-eqn (= ?dummy2 (cos ?theta2)) 
		  (angle-constraint ?angle-flag orderless 
				    (line ?line2) (line ?normal-to-surface)))
    )
  :hint (
	 (point (string "How is the direction of ~A related to the direction of ~A?"
			?line1 ?line2))
	 (teach (string "Use Snell's law.")) 
	 (bottom-out (string "Write the equation ~A." 
			     ((= (* ?n1 (sin ?theta1)) 
				 (* ?n2 (sin ?theta2))) algebra)))
	 ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Formula for total internal reflection
;;;

;; This is basically a copy of Snell's law
;; It does not really handle the inequality
(def-psmclass total-internal-reflection 
  (total-internal-reflection ?line1 ?angle-flag)
  :complexity major
  :short-name "total internal reflection (minimum angle)"
  :nlg-english ("Formula for angle of total internal reflection")
  :ExpFormat ("using total internal reflection formula for ~A (minimum angle)" 
	      (def-np ?line1))
  :eqnFormat ("n1 sin(&theta;1) = n2"))

(defoperator total-internal-reflection-contains (?sought)
  :preconditions 
  ( (total-internal-reflection-system ?normal-to-surface 
				      ?line1 ?medium1 ?medium2)
    (any-member ?sought ((angle-between orderless (line ?line1) 
					(line ?normal-to-surface))
			 (index-of-refraction ?medium1)		
			 (index-of-refraction ?medium2)
			 (dir (line ?line1))))
    (any-member ?flag (t nil))
    ;; can't do directions if ?sought is angle-between, etc.
    (test (if ?flag (not (eq (first ?sought) 'angle-between))
	    (not (eq (first ?sought) 'dir))))
    (wave-medium ?medium1) (wave-medium ?medium2) ;sanity check
    )
  :effects ( (eqn-contains (total-internal-reflection ?line1 ?flag) ?sought) ))
 
(defoperator write-total-internal-reflection (?line1 ?angle-flag)
  :preconditions 
  (
   (total-internal-reflection-system ?normal-to-surface 
				     ?line1 ?medium1 ?medium2)
   ;;
   (variable ?n1 (index-of-refraction ?medium1))
   (variable ?n2 (index-of-refraction ?medium2))
   (angle-expression ?theta1 (line ?line1) (line ?normal-to-surface)
				    ?angle-flag)
   ;; ?dummy1 is non-negative
   (variable ?dummy1 (test-var ?line1 ?normal-to-surface)) 
   (debug "total internal reflection for ~A~%" ?line1)
   )
  :effects 
  ( (eqn (= (* ?n1 (sin ?theta1)) ?n2) 
	 (total-internal-reflection ?line1 ?angle-flag))
    (assume using-snells-law ?line1 total-internal-reflection ?angle-flag)
    ;; Implicit equations to enforce the correct root for the sines
    ;; This is needed if one wants to calculate one of the angles
    (implicit-eqn (= ?dummy1 (cos ?theta1)) 
		  (angle-constraint ?angle-flag orderless 
				    (line ?line1) (line ?normal-to-surface)))
    )
  :hint 
  ((point (string "What is the direction of ~A if there is to be total internal reflection?"
		  ?line1))
   (teach (string "Use the formula for total internal reflection.")) 
   (bottom-out (string "Write the equation ~A." 
		       ((= (* ?n1 (sin ?theta1)) ?n2) algebra)))
   ))

;;;
;;;                Complimentary and Supplementary angles
;;;

(def-psmclass complimentary-angles (complimentary-angles orderless . ?angles)
  :complexity minor
  :short-name "complimentary angles"
  :nlg-english ("Complimentary angles")
  :ExpFormat ("using complimentary angles for ~A" 
	      (nlg ?angles 'conjoined-defnp))
  :eqnFormat ("&theta;1 + &theta;2 = 90 deg"))

(defoperator complimentary-angles-contains (?sought)
  :preconditions 
  ( (complimentary-angles orderless . ?angles)
    (any-member ?sought ?angles)
    )
  :effects 
  ( (eqn-contains (complimentary-angles orderless . ?angles) ?sought) ))

 
(defoperator write-complimentary-angles (?angles)
  :preconditions 
  (
   (any-member ?angles ((?ang1 ?ang2)))
   (variable ?angle1 ?ang1)
   (variable ?angle2 ?ang2)
   )
  :effects ( (eqn (= (+ ?angle1 ?angle2) (dnum 90 |deg|)) 
		  (complimentary-angles orderless . ?angles)) 
	     ;; inconsistancies arise in ref3a when used in combination 
	     ;; with angle-direction
	     (assume using-complimentary-angles . ?angles))
  :hint 
  ((point (string "What is the relation between ~A and ~A?"
		  (?ang1 variable-defnp) (?ang2 variable-defnp)))
   (teach (string "If two angles are complimentary, then their measure adds up to 90 degrees.")) 
   (bottom-out (string "Write the equation ~A." 
		       ((= (+ ?angle1 ?angle2) (dnum 90 |deg|)) algebra)))
   ))


(def-psmclass supplementary-angles (supplementary-angles orderless . ?angles)
  :complexity minor
  :short-name "supplementary angles"
  :nlg-english ("Supplementary angles")
  :ExpFormat ("using supplementary angles for ~A" 
	      (nlg ?angles 'conjoined-defnp))
  :eqnFormat ("&theta;1 + &theta;2 = 180 deg"))

(defoperator supplementary-angles-contains (?sought)
  :preconditions 
  ( (supplementary-angles orderless . ?angles)
    (any-member ?sought ?angles)
    )
  :effects 
  ( (eqn-contains (supplementary-angles orderless . ?angles) ?sought) ))

 
(defoperator write-supplementary-angles (?angles)
  :preconditions 
  (
   (any-member ?angles ((?ang1 ?ang2)))
   (variable ?angle1 ?ang1)
   (variable ?angle2 ?ang2)
   )
  :effects ( (eqn (= (+ ?angle1 ?angle2) (dnum 180 |deg|)) 
		  (supplementary-angles orderless . ?angles))
    )
  :hint 
  ((point (string "What is the relation between ~A and ~A?"
		  (?ang1 def-np) (?ang2 def-np)))
   (teach (string "If two angles are supplementary, then their measure adds up to 90 degrees.")) 
   (bottom-out (string "Write the equation ~A." 
		       ((= (+ ?angle1 ?angle2) (dnum 90 |deg|)) algebra)))
   ))

(def-psmclass right-triangle-tangent 
  (right-triangle-tangent ?angle ?opposite ?adjacent)
  :complexity minor
  :short-name "tangent for right triangle"
  :nlg-english ("tangent formula for right triangles")
  :ExpFormat ("using the tangent formula for ~A"  (nlg ?angle))
  :eqnFormat ("tan(&theta;) = opposite/adjacent"))

(defoperator right-triangle-tangent-contains (?sought)
  :preconditions 
  ( (right-triangle ?angle ?opposite ?adjacent . ?whatever)
    (any-member ?sought (?angle ?opposite ?adjacent))
    )
  :effects 
  ((eqn-contains (right-triangle-tangent ?angle ?opposite ?adjacent) ?sought)))

 
(defoperator write-right-triangle-tangent (?angle)
  :preconditions 
  (
   (variable ?angle-var ?angle)
   (variable ?opp-var ?opposite)
   (variable ?adj-var ?adjacent)
   )
  :effects ( (eqn (= (tan ?angle-var) (/ ?opp-var ?adj-var)) 
		  (right-triangle-tangent ?angle ?opposite ?adjacent))
    )
  :hint 
  ((point (string "Note that ~A and ~A are two sides of a right triangle."
		  (?opposite def-np) (?adjacent def-np)))
   (teach (string "Recall the trigonometric formulas for a right triangle.")) 
   (bottom-out (string "Write the equation ~A." 
		       ((= (tan ?angle-var) (/ ?opp-var ?adj-var)) algebra)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Formula for Brewster's law
;;;

;; This is basically a copy of Snell's law
(def-psmclass brewsters-law (brewsters-law ?normal ?line1 ?angle-flag)
  :complexity major
  :short-name "Brewster's law"
  :nlg-english ("Formula for angle of Brewster's law")
  :ExpFormat ("using Brewster's law formula for ~A" 
	      (nlg (list 'line ?line1)))
  :eqnFormat ("n1 tan(&theta;1) = n2"))


;; form with angle-between
(defoperator brewsters-law-contains (?sought)
  :preconditions 
  ( (snell-system ?normal-to-surface orderless . ?lists)
    (brewsters-law-system ?normal-to-surface ?line1)
    (any-member ?lists (((?line1 ?medium1) (?line2 ?medium2))
			((?line2 ?medium2) (?line1 ?medium1))))
    (any-member ?sought ((angle-between orderless (line ?line1) 
					(line ?normal-to-surface))
			 (index-of-refraction ?medium1)		
			 (index-of-refraction ?medium2)
			 (dir (line ?line1))))
    (any-member ?flag (t nil))
    ;; can't do directions if ?sought is angle-between, etc.
    (test (if ?flag (not (eq (first ?sought) 'angle-between))
	    (not (eq (first ?sought) 'dir))))
    (wave-medium ?medium1) (wave-medium ?medium2) ;sanity check
    )
  :effects 
  ( (eqn-contains (brewsters-law ?normal-to-surface ?line1 ?flag) ?sought) ))

 
(defoperator write-brewsters-law (?line1 ?angle-flag)
  :preconditions 
  (
   (snell-system ?normal-to-surface orderless . ?lists)
   (brewsters-law-system ?normal-to-surface ?line1)
   (any-member ?lists (((?line1 ?medium1) (?line2 ?medium2))
		       ((?line2 ?medium2) (?line1 ?medium1))))
   (bind ?lines (sort (list ?line1 ?line2) #'expr<))
   ;;
   (variable ?n1 (index-of-refraction ?medium1))
   (variable ?n2 (index-of-refraction ?medium2))
   (angle-expression ?theta1 (line ?line1) (line ?normal-to-surface)
				    ?angle-flag)
   ;; ?dummy1 is non-negative
   (variable ?dummy1 (test-var ?line1 ?normal-to-surface)) 
   (debug "Brewster's law for ~A and ~A~%" ?lines ?angle-flag)
   )
  :effects 
  ( (eqn (= (* ?n1 (tan ?theta1)) ?n2) 
	 (brewsters-law ?normal-to-surface ?line1 ?angle-flag))
    ;; We don't want to apply both explicit Brewster's law and 
    ;; Snell's law to the same system.
    (assume using-snells-law ?lines brewsters ?angle-flag)
    ;; Implicit equations to enforce the correct root for the tangent
    ;; This is needed if one wants to calculate one of the angles
    ;; Right now, the routine in Algebra/src/nlsolvov.cpp assumes the
    ;; root is in [-pi/2,pi/2].  See Bug #797
    (implicit-eqn (= ?dummy1 (cos ?theta1)) 
		  (angle-constraint ?angle-flag orderless 
				    (line ?line1) (line ?normal-to-surface)))
    )
  :hint 
  ((point (string "What is the direction of ~A if Brewster's law applies?"
		  ?line1))
   (teach (string "Use the formula for Brewster's law.")) 
   (bottom-out (string "Write the equation ~A." 
		       ((= (* ?n1 (tan ?theta1)) ?n2) algebra)))
   ))

;;;;             Polarizer

(def-psmclass polarizer-intensity
  (polarizer-intensity ?beam ?incoming ?outgoing ?fraction ?t ?angle-flag)
  :complexity major
  :short-name ((polarization-intensity-eqn-name ?fraction))
  :nlg-english ("the effect of a polarizer on the intensity of a beam of light")
  :ExpFormat ("using the effect of a polarizer on the intensity of ~A light"
	      (polarization-fraction ?fraction))
  :EqnFormat ((polarization-intensity-eqn ?fraction))) 

(defun polarization-intensity-eqn-name (fraction)
  (if (unify fraction '0) "polarizer and unpolarized light" "Malus' law"))

(defun polarization-intensity-eqn (fraction)
  (if (unify fraction '0) "If = 0.5 Ii"	"If = Ii cos(&theta;)<sup>2</sup>"))

(defun polarization-fraction (fraction)
  (cond ((unify fraction '0) "unpolarized")
	((unify fraction '1) "polarized")
	(t "partially polarized")))

(defoperator polarizer-intensity-contains (?sought)
   :preconditions 
   (
    (polarization-triple ?beam ?incoming ?polarizer ?outgoing)
    (polarization ?beam ?incoming ?dir)
    (bind ?fraction (if ?dir 1 0))
    ;; can't use to find angle
    (any-member ?sought ((intensity ?beam at ?incoming :time ?t)
			 (intensity ?beam at ?outgoing :time ?t) ))
    (any-member ?angle-flag (t nil))
   )
   :effects 
   ((eqn-contains (polarizer-intensity ?beam ?incoming ?outgoing 
				       ?fraction ?t ?angle-flag) ?sought)
	     ))

;; enable line drawing for a polarizer direction
;; might want to eventually have a separate drawing rule
(defoperator line-dir-given-polarization (?polarizer)
  :preconditions ((polarization ?polarizer ?dirp))
  :effects ((given (dir (line ?polarizer)) ?dirp)))

;; find polarization of beam after going through a polarizer
(defoperator do-polarization (?beam ?outgoing)
  :preconditions 
  ((polarization-triple ?beam ?incoming ?polarizer ?outgoing)
   (polarization ?polarizer ?dirp))
  :effects ((polarization ?beam ?outgoing ?dirp) ))

(defoperator polarization-intensity-use-angle (?beam ?incoming ?outgoing 
						     ?angle-flag)
 :preconditions 
 ( ;; get name of first polarizer
   (polarization-triple ?beam ?whatever ?p1 ?incoming)
   ;; get name of second polarizer
   (polarization-triple ?beam ?incoming ?p2 ?dontcare)
   (angle-expression ?angle (line ?p1) (line ?p2) ?angle-flag)
   )
 :effects 
 ((polarization-angle-term ?angle ?beam ?incoming ?outgoing ?angle-flag)))

(defoperator polarization-intensity-unpolarized (?beam ?incoming ?outgoing)
 :preconditions ( (polarization ?beam ?incoming nil) ) ;incoming unpolarized
 :effects ((polarization-angle-term nil ?beam ?incoming ?outgoing nil)))

(defoperator write-polarizer-intensity 
  (?beam ?incoming ?outgoing ?t ?angle-flag)
  :preconditions 
  (
   (variable ?iin (intensity ?beam at ?incoming :time ?t))
   (variable ?iout (intensity ?beam at ?outgoing :time ?t))
   ;; Only force angle variable definitions when they are needed
   (polarization-angle-term ?angle ?beam ?incoming ?outgoing ?angle-flag)
   (bind ?fraction-term (if (= ?fraction 1)
			 `(^ (cos ,?angle) 2) '0.5))
   (bind ?help (if (= ?fraction 1) 
		   "polarized at an angle of &theta; relative to the transmission axis of the polarizer, the intensity of the transmitted beam is cos(&theta;)<sup>2</sup> times the incoming intensity." 
		 "unpolarized, half the intensity is absorbed by the polarizer."))
   )
  :effects 
  ( (eqn (= ?iout (* ?iin ?fraction-term)) 
	 (polarizer-intensity ?beam ?incoming ?outgoing 
			      ?fraction ?t ?angle-flag))
    ;; OK, this really doesn't have anything to do with Snell's law
    (assume using-snells-law (?beam ?incoming ?outgoing ?t) nil ?angle-flag) )
  :hint (
      (point (string "Relate the intensity of ~A at ~A to the intensity at ~A." 
		     ?beam ?outgoing ?incoming))
      (point (string "If the incoming beam is ~A" (?help identity)))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?iout (* ?iin ?fraction-term)) algebra) ))
	 ))

;;;;   Interference pattern for grating with slits

(def-psmclass slit-interference (slit-interference ?grating ?light ?angle
						   ?max-flag ?angle-flag)
  :complexity major
  :short-name "interference for parallel slits"
  :nlg-english ("the interference pattern for waves going through parallel slits")
  :ExpFormat ("finding the angles for ~:[destructive~;constructive~] interference"
	      ?max-flag)
  :EqnFormat ("d sin(&theta;) = n &lambda;")) 

(defoperator slit-interference-contains-max (?sought)
  :preconditions 
  ( (parallel-slit-system ?grating ?light ?central-max :minima ?min-list 
			  :maxima ?max-list)
    (any-member ?max ?max-list)
    (any-member ?sought ((slit-separation ?grating) 
			 (wavelength ?light ?medium)
			 (angle-between orderless (line ?central-max) 
					(line ?max))
			 (dir (line ?max))))
    (any-member ?angle-flag (t nil))
    ;; can't do directions if ?sought is angle-between, etc.
    (test (if ?angle-flag (not (eq (first ?sought) 'angle-between))
	    (not (eq (first ?sought) 'dir))))
    )
  :effects ( (eqn-contains (slit-interference ?grating ?light 
					      ?max t ?angle-flag) ?sought) ))

(defoperator slit-interference-contains-min (?sought)
  :preconditions 
  ( (parallel-slit-system ?grating ?light ?central-max :minima ?min-list 
			  :maxima ?max-list)
    (any-member ?min ?min-list)
    (any-member ?sought ( (slit-separation ?grating) 
			  (wavelength ?light ?medium)
			  (angle-between orderless (line ?central-max) 
					 (line ?min))
			  (dir (line ?min))))
    (any-member ?angle-flag (t nil))
    ;; can't do directions if ?sought is angle-between, etc.
    (test (if ?angle-flag (not (eq (first ?sought) 'angle-between))
	    (not (eq (first ?sought) 'dir))))
    )
  :effects ((eqn-contains (slit-interference ?grating ?light 
					     ?min nil ?angle-flag) ?sought) ))

(defoperator write-slit-interference (?grating ?light ?angle ?flag ?angle-flag)
   :preconditions 
   (
    (in-wm (parallel-slit-system ?grating ?light ?central-max 
				 :minima ?min-list :maxima ?max-list))
    (wave-medium ?medium)
    (variable ?lambda (wavelength ?light ?medium))
    (variable ?d (slit-separation ?grating))
    (angle-expression ?theta (line ?angle) (line ?central-max) ?angle-flag)
    (bind ?n (if ?flag (+ 1 (position ?angle ?max-list))
	       (+ 0.5 (position ?angle ?min-list))))
    (variable ?dummy (test-var ?grating ?light ?n))
    )
   :effects 
   ((eqn (= (* ?d (sin ?theta)) (* ?n ?lambda)) 
	 (slit-interference ?grating ?light ?angle ?flag ?angle-flag))
    ;; Implicit equation to enforce the correct root for the sine
    ;; This is needed if one wants to calculate the angle
    (implicit-eqn (= ?dummy (cos ?theta)) 
		  (angle-constraint ?angle-flag orderless 
				    (line ?angle) (line ?central-max)))
    )
   :hint 
   (
    (point (string "Waves passing through ~A will interfere ~:[destructively~;constructively~] at certain angles." 
		   ?grating (?flag identity)))
    (bottom-out (string "Write the equation ~A" 
			((= (* ?d (sin ?theta)) (* ?n ?lambda)) algebra) ))
    ))

;;;;   Frauenhofer diffraction formula

(def-psmclass frauenhofer-diffraction 
  (frauenhofer-diffraction ?grating ?light ?angle ?angle-flag)
  :complexity major
  :short-name "single slit diffraction (minima)"
  :nlg-english ("the interference pattern for waves going through a single slit")
  :ExpFormat ("finding the angles for destructive interference")
  :EqnFormat ("w sin(&theta;) = n &lambda;")) 

(defoperator frauenhofer-diffraction-contains (?sought)
   :preconditions 
   ( (single-slit-system ?grating ?light ?central-max :minima ?min-list)
     (any-member ?min ?min-list)
     (any-member ?sought ( (slit-separation ?grating) 
			   (wavelength ?light ?medium)
			   (angle-between orderless 
					  (line ?central-max) (line ?min))
			   (dir (line ?min))))
     (any-member ?flag (t nil))
     ;; can't do directions if ?sought is angle-between, etc.
     (test (if ?flag (not (eq (first ?sought) 'angle-between))
	     (not (eq (first ?sought) 'dir))))
     )
   :effects ( (eqn-contains (frauenhofer-diffraction ?grating ?light 
						     ?min ?flag) ?sought) ))

(defoperator write-frauenhofer-diffraction (?grating ?light ?angle ?flag)
  :preconditions 
  ( (in-wm (single-slit-system ?grating ?light ?central-max 
			       :minima ?min-list))
    (wave-medium ?medium)
    (variable ?lambda (wavelength ?light ?medium))
    (variable ?d (width ?grating))
    (angle-expression ?theta (line ?angle) (line ?central-max) ?flag)
    (bind ?n (+ 1 (position ?angle ?min-list)))
    (variable ?dummy (test-var ?grating ?light ?n))
    )
  :effects 
  ((eqn (= (* ?d (sin ?theta)) (* ?n ?lambda)) 
	(frauenhofer-diffraction ?grating ?light ?angle ?flag))
   ;; Implicit equation to enforce the correct root for the sine
   ;; This is needed if one wants to calculate the angle
   (implicit-eqn (= ?dummy (cos ?theta)) 
		 (angle-constraint ?flag orderless 
				   (line ?angle) (line ?central-max)))
   )
  :hint 
  (
   (point (string "Waves passing through ~A will interfere destructively at certain angles." 
		  ?grating))
   (teach (string "Read about Frauenhofer diffraction in your textbook."))
   (bottom-out (string "Write the equation ~A" 
		       ((= (* ?d (sin ?theta)) (* ?n ?lambda)) algebra) ))
   ))

;;; Minimum angle of resolution for system with circular aperature

(def-psmclass resolution-circular-aperture 
  (resolution-circular-aperture ?mirror ?light ?medium)
  :complexity major 
  :short-name "resolution (circular)"
  :nlg-english ("the minimum angle of resolution for a circular aperture")
  :ExpFormat ("applying the minimum angle of resolution for a system with a circular aperture")
  :EqnFormat "&theta; = 1.22 &lambda;/d")

(defoperator resolution-circular-aperture-contains (?sought)
   :preconditions (
     (any-member ?sought ( (resolution-angle ?mirror)
                           (diameter-of-circle ?mirror)
			   (wavelength ?light ?medium) ))
     (shape ?mirror circle)
     (sinusoidal ?light) ;in case it isn't bound
     (wave-medium ?medium)
     )
   :effects (
     (eqn-contains 
      (resolution-circular-aperture ?mirror ?light ?medium) ?sought)
   ))

(defoperator write-resolution-circular-aperture (?mirror ?light ?medium)
   :preconditions 
   (
    (variable ?theta (resolution-angle ?mirror))
    (variable ?d (diameter-of-circle ?mirror))
    (variable ?lambda (wavelength ?light ?medium))
    )
   :effects (
	     (eqn (= (* ?theta ?d) (* 1.22 ?lambda)) 
		   (resolution-circular-aperture ?mirror ?light ?medium))
	     )
   :hint (
      (point (string "How is ~A determined from the geometry of ~A?" 
		     ((resolution-angle ?mirror) def-np) ?mirror))
      (teach (string "In your textbook, find Rayleigh's criterion for the minimum angle of resolution for a circular aperature."))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?theta (/ (* 1.22 ?lambda) ?d)) algebra) ))
   ))
