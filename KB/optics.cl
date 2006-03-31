
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
 

(def-qexp object-distance (object-distance ?lens)
  :symbol-base |do|     
  :short-name "object distance"	
  :dialog-text "with respect to [body:bodies]"
   :units |m|
   :english ("the distance of the object from ~A" (nlg ?lens))
   :fromWorkbench `(object-distance ,body)
)

(defoperator define-object-distance (?lens)
  :preconditions ( (bind ?do-var (format-sym "do_~A" (body-name ?lens))) )
  :effects ( (variable ?do-var (object-distance ?lens))
             (define-var (object-distance ?lens)))
  :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Object Distance."  
			   ((object-distance ?lens) def-np)))
       ))

(def-qexp image-distance (image-distance ?lens)
  :symbol-base |di|     
  :short-name "image distance"	
  :dialog-text "with respect to [body:bodies]"
   :units |m|
   :english ("the distance of the image from ~A" (nlg ?lens))
   :fromWorkbench `(image-distance ,body)
)

(defoperator define-image-distance (?lens)
  :preconditions ( (bind ?di-var (format-sym "di_~A" (body-name ?lens))) )
  :effects ( (variable ?di-var (image-distance ?lens))
             (define-var (image-distance ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Image Distance."  
			   ((image-distance ?lens) def-np)))
       ))

(def-qexp focal-length (focal-length ?lens)
  :symbol-base |f|     
  :short-name "focal length"	
  :dialog-text "of [body:bodies]"
   :units |m|
   :english ("the focal length of ~A" (nlg ?lens))
   :fromWorkbench `(focal-length ,body)
)

(defoperator define-focal-length (?lens)
  :preconditions ( (bind ?f-var (format-sym "f_~A" (body-name ?lens))) )
  :effects ( (variable ?f-var (focal-length ?lens))
             (define-var (focal-length ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Focal Length."  
			   ((focal-length ?lens) def-np)))
       ))

(def-qexp magnification (magnification ?lens)
  :symbol-base |m|     
  :short-name "magnification"	
  :dialog-text "of [body:bodies]"
  :units NIL
   :english ("the magnification of ~A" (nlg ?lens))
   :fromWorkbench `(magnification ,body)
) 

(defoperator define-magnification (?lens)
  :preconditions ( (bind ?m-var (format-sym "m_~A" (body-name ?lens))) )
  :effects ( (variable ?m-var (magnification ?lens))
            (define-var (magnification ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Magnification."  
			   ((magnification ?lens) def-np)))
       ))

(def-qexp radius-of-curvature (radius-of-curvature ?mirror)
  :symbol-base |r|     
  :short-name "radius of curvature"	
  :dialog-text "of [body:bodies]"
   :units |m|
   :english ("the radius of curvature of ~A" (nlg ?mirror))
   :fromWorkbench `(radius-of-curvature ,body)
)

(defoperator define-radius-of-curvature (?mirror)
 :preconditions ( (bind ?r-var (format-sym "r_~A" (body-name ?mirror))) )
 :effects ( (variable ?r-var (radius-of-curvature ?mirror))
            (define-var (radius-of-curvature ?mirror)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Radius of Curvature."  
			   ((radius-of-curvature ?mirror) def-np)))
       ))

(def-qexp lens-distance (lens-distance ?lens1 ?lens2)
  :symbol-base |d|     
  :short-name "distance"	
  :dialog-text "between [body:bodies] and [body2:bodies]"
  :units |m|
  :english ("the distance between ~a and ~a" (nlg ?lens1) (nlg ?lens2))
  :fromWorkbench `(lens-distance ,body ,body2) ; might want to canonicalize order!
) 

(defoperator define-lens-distance (?lens1 lens2)
 :preconditions ( (bind ?d-var (format-sym "d_~A_~A" (body-name ?lens1) (body-name ?lens2))) )
 :effects ( (variable ?d-var (lens-distance ?lens1 ?lens2))
            (define-var (lens-distance ?lens1 ?lens2)) )
  :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Distance between Lenses."  
                           ((lens-distance ?lens1 ?lens2) def-np) ))
       ))

(def-qexp distance-between (distance-between orderless . ?objects)
  :symbol-base |d|     
  :short-name "distance"	
  :dialog-text "between [body:bodies] and [body2:bodies]"
  :units |m|
  :english ("the distance between ~a and ~a" (nlg ?objects 'conjoined-defnp))
  :fromWorkbench `(distance-between orderless ,body ,body2) 
  ) 

(defoperator define-distance-between (?objects)
 :preconditions 
 ( (bind ?d-var (format-sym "d~{_~A~}" (mapcar #'body-name ?objects))))
 :effects ( (variable ?d-var (distance-between orderless . ?objects))
            (define-var (distance-between orderless . ?objects)) )
 :hint (
       (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Distance between Lenses."  
                           ((distance-between orderless . ?objects) def-np) ))
       ))

(def-qexp slit-separation (slit-separation ?grating)
  :symbol-base |d|     
  :short-name "slit separation"	
  :pre-dialog-text "separation between slits"
  :dialog-text "in [body:bodies]"
  :units |m|
  :restrictions positive
  :english ("the distance between slits in ~A" (nlg ?grating))
  :fromWorkbench `(slit-separation ,body)
  )

(defoperator define-slit-separation (?grating)
  :preconditions ( (bind ?do-var (format-sym "ds_~A" (body-name ?grating))) )
  :effects ( (variable ?do-var (slit-separation ?grating))
             (define-var (slit-separation ?grating)))
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Object Distance."  
			     ((slit-separation ?grating) def-np)))
	 ))

(def-qexp resolution-angle (resolution-angle ?grating)
  :symbol-base |$q|     
  :short-name "angle of resolution"
  :pre-dialog-text "minimum angle of resolution"	
  :dialog-text "for [body:bodies]"
  :units |deg|
  :restrictions positive
  :english ("the minimum angle of resolution for ~A" (nlg ?grating))
  :fromWorkbench `(resolution-angle ,body)
  )

(defoperator define-resolution-angle (?grating)
  :preconditions ( (bind ?do-var (format-sym "thetares_~A" (body-name ?grating))) )
  :effects ( (variable ?do-var (resolution-angle ?grating))
             (define-var (resolution-angle ?grating)))
  :hint (
	 (bottom-out (string "Define a variable for ~A by using the Add Variable command on the Variable menu and selecting Object Distance."  
			     ((resolution-angle ?grating) def-np)))
	 ))

;;
;; Optics Equations
;;

; focal length of spherical mirror = r/2
(def-psmclass focal-length-mirror (focal-length-mirror ?mirror)
  :complexity major  ; ??
  :short-name "focal length of mirror"
  :english ("the focal length of spherical mirror")
  :ExpFormat ("applying the formula for the focal length of a spherical mirror")
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
  :english ("the thin lens/mirror equation")
  :ExpFormat ("applying the thin lens/mirror equation")
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
     (point (string "Because the image-forming rays from ~A are effectively parallel, there is a very simple relation between the object distance and focal length of ~a." ?lens))
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
  :english ("the formula for (lateral) magnification")
  :ExpFormat ("applying the formula for magnification")
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
(def-psmclass lens-combo (lens-combo ?lens1 lens2)
  :complexity major
  :short-name "combination of lenses"
  :english ("the lens combination relation")
  :ExpFormat ("applying the lens combination relation")
  :EqnFormat ("do2 = d12 - di1")) 

(defoperator lens-combo-contains (?sought)
  :preconditions (
  ; the lens-system statement tells us order of lenses with 
  ; respect to light source, and gives the distance between them
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
     (variable ?d12 (lens-distance ?lens1 ?lens2))
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

(def-psmclass combo-magnification (combo-magnification ?sys)
  :complexity major
  :short-name "combined magnification"
  :english ("combined lens magnification")
  :ExpFormat ("applying the formula for magnification of combined lenses")
  :EqnFormat ("m12 = m1*m2")) 

(defoperator combo-magnification-contains (?sought)
   :preconditions (
     (in-wm (lens-system ?sys (?lens1 ?lens2)) )
     (any-member ?sought ( (magnification ?sys)
                           (magnification ?lens1)
                           (magnification ?lens2) ))
   )
   :effects (
     (eqn-contains (combo-magnification ?sys) ?sought)
   ))

(defoperator combo-magnification (?sys)
   :preconditions (
       (in-wm (lens-system ?sys (?lens1 ?lens2)) )
       (variable ?m12 (magnification ?sys))
       (variable ?m1  (magnification ?lens1))
       (variable ?m2  (magnification ?lens2))
   )
   :effects (
     (eqn  (= ?m12 (* ?m1 ?m2)) (combo-magnification ?sys))
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
  :english ("the focal length of a compound lens")
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
    (eqn (= (/ 1 ?f12) (+ (/ 1 ?f1) (/ 1 ?f2))) (compound-focal-length ?compound))
  )
  :hint (
    (point (string "When two lenses are touching, you can treat them as a single compound lens with a focal length that is a function of the focal lengths of the two individual lenses."))
    (teach (string "It can be shown that the reciprocoal of the focal length of a compound lens formed by two touching lenses is equal to the sum of the reciprocals of the focal lengths of the two individual lenses."))
    (bottom-out (string "Write the equation ~A" 
         ((= (/ 1 ?f21) (+ (/ 1 ?f1) (/ 1 ?f2))) algebra) ))
  ))

;;; line drawing

;; this draws a line, with the bodies as a choice.

(def-qexp line (line ?r)
  :units nil
  :english ("~A" (nlg ?r)))

(defoperator draw-line-given-dir (?r)
  :preconditions
  ( (given (dir (line ?r)) ?dir-in)
    (not (draw-line (line ?r) ?dontcare))
    (bind ?dir (mod (convert-dnum-to-number ?dir-in) 180))
    (bind ?mag-var (format-sym "l_~A" (body-name ?r)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "draw line ~A at ~A~%" ?r ?dir) )
  :effects ( (draw-line (line ?r) (dnum ?dir |deg|))
	     (variable ?mag-var (mag (line ?r)))
	     (variable ?dir-var (dir (line ?r))))
  :hint ((bottom-out (string "Use the line tool to draw a line for ~A in a direction of ~A degrees with respect to the horizontal." 
			     ?r ?dir))
	 ))

(defoperator draw-line-unknown-dir (?r)
  :preconditions
  ( (setof (given (dir (line ?r)) ?dir) ?dir ?dirs)
    (test (null ?dirs))
    (not (draw-line (line ?r) ?dontcare))
    (bind ?mag-var (format-sym "l_~A" (body-name ?r)))
    (bind ?dir-var (format-sym "O~A" ?mag-var))
    (debug "draw line ~A in unkown direction~%" ?r)
    )
  :effects
  ( (draw-line (line ?r) unknown)
    (variable ?mag-var (mag (line ?r)))
    (variable ?dir-var (dir (line ?r))) )
  :hint
   ((bottom-out (string "Use the line tool to draw a line for ~A in an unkown direction." 
			?r))
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
  :english ("Snell's law")
  :ExpFormat ("using Snell's law for ~A" (nlg ?lines 'conjoined-defnp))
  :eqnFormat ("n1*sin($q1) = n2*sin($q2)"))


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
   (debug "do Snell's law for ~A and ~A~%" ?line1 ?line2)
   )
  :effects 
  ( (eqn (= (* ?n1 (sin ?theta1)) (* ?n2 (sin ?theta2))) 
	 (snells-law (orderless . ?lines) ?angle-flag))
    (assume using-snells-law ?lines ?angle-flag)
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

(def-psmclass angle-direction (angle-direction ?greater ?lesser)
  :complexity definition
  :short-name "angle between"
  :english ("angle between")
  :ExpFormat ("finding the angle between ~A and ~A" 
	      (nlg ?greater) (nlg ?lesser))
  :eqnFormat ("$q12 = $q2 - $q1"))

(defoperator angle-direction-contains (?sought)
  :preconditions 
  ;; This proposition implies that one can subtract directions without 
  ;; needing the modulus (mod 360 for vectors and mod 180 for lines).
  ((greater-than ?greater ?lesser)
   (any-member ?sought ((angle-between orderless ?greater ?lesser)
			(dir ?greater) (dir ?lesser))))
  :effects 
  ( (eqn-contains (angle-direction ?greater ?lesser) ?sought) ))

(defoperator relate-inequality (?greater ?lesser)
  :preconditions ((less-than ?lesser ?greater))
  :effects ((greater-than ?greater ?lesser)))

(defoperator greater-than-given-directions (?greater ?lesser)
  :preconditions 
  ( (given (dir ?greater) ?dg)
    (given (dir ?lesser) ?dl)
    (test (and (degrees-or-num ?dg) (degrees-or-num ?dl)))
    (test (> (convert-dnum-to-number ?dg) (convert-dnum-to-number ?dl))))
  :effects ((greater-than ?greater ?lesser)))

(defoperator write-angle-direction (?greater ?lesser)
  :preconditions 
  (
   (variable ?dg (dir ?greater))
   (variable ?dl (dir ?lesser))
   (variable ?angle (angle-between orderless ?greater ?lesser))
   )
  :effects 
  ( (eqn (= ?angle (- ?dg ?dl)) (angle-direction ?greater ?lesser)))
  :hint (
	 (point (string "Express the angle between ~A and ~A in terms of directions."
			?greater ?lesser))
	 (teach (string "The angle is simply the difference of the two directions.")) 
	 (bottom-out (string "Write the equation ~A." 
			     ((= ?angle (- ?dg ?dl)) algebra)))
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
  :english ("Formula for angle of total internal reflection")
  :ExpFormat ("using total internal reflection formula for ~A (minimum angle)" 
	      (nlg ?line1))
  :eqnFormat ("n1*sin($q1) = n2"))

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
    (assume using-snells-law ?line1 ?angle-flag)
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
  :english ("Complimentary angles")
  :ExpFormat ("using complimentary angles for ~A" 
	      (nlg ?angles 'conjoined-defnp))
  :eqnFormat ("$q1 + $q2 = 90 deg"))

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
		  (complimentary-angles orderless . ?angles)) )
  :hint 
  ((point (string "What is the relation between ~A and ~A?"
		  (?ang1 def-np) (?ang2 def-np)))
   (teach (string "If two angles are complimentary, then their measure adds up to 90 degrees.")) 
   (bottom-out (string "Write the equation ~A." 
		       ((= (+ ?angle1 ?angle2) (dnum 90 |deg|)) algebra)))
   ))


(def-psmclass supplementary-angles (supplementary-angles orderless . ?angles)
  :complexity minor
  :short-name "supplementary angles"
  :english ("Supplementary angles")
  :ExpFormat ("using supplementary angles for ~A" 
	      (nlg ?angles 'conjoined-defnp))
  :eqnFormat ("$q1 + $q2 = 180 deg"))

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
  :english ("tangent formula for right triangles")
  :ExpFormat ("using the tangent formula for ~A"  (nlg ?angles))
  :eqnFormat ("tan($q) = opposite/adjacent"))

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
		       ((= (tan ?angle) (/ ?opp-var ?adj-var)) algebra)))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Formula for Brewster's law
;;;

;; This is basically a copy of Snell's law
(def-psmclass brewsters-law (brewsters-law ?line1 ?angle-flag)
  :complexity major
  :short-name "Brewster's law"
  :english ("Formula for angle of Brewster's law")
  :ExpFormat ("using Brewster's law formula for ~A" 
	      (nlg ?line1))
  :eqnFormat ("n1*tan($q1) = n2"))


;; form with angle-between
(defoperator brewsters-law-contains (?sought)
  :preconditions 
  ( (brewsters-law-system ?normal-to-surface 
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
  :effects ( (eqn-contains (brewsters-law ?line1 ?flag) ?sought) ))

 
(defoperator write-brewsters-law (?line1 ?angle-flag)
  :preconditions 
  (
   (brewsters-law-system ?normal-to-surface ?line1 ?medium1 ?medium2)
   ;;
   (variable ?n1 (index-of-refraction ?medium1))
   (variable ?n2 (index-of-refraction ?medium2))
   (angle-expression ?theta1 (line ?line1) (line ?normal-to-surface)
				    ?angle-flag)
   ;; ?dummy1 is non-negative
   (variable ?dummy1 (test-var ?line1 ?normal-to-surface)) 
   (debug "Brewster's law for ~A~%" ?line1)
   )
  :effects 
  ( (eqn (= (* ?n1 (tan ?theta1)) ?n2) 
	 (brewsters-law ?line1 ?angle-flag))
    (assume using-snells-law ?line1 ?angle-flag)
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
  (polarizer-intensity ?beam ?incoming ?outgoing ?fraction ?t)
  :complexity major
  :short-name ("polarizer for ~A light" 
	       (polarization-fraction ?fraction))
  :english ("the effect of a polarizer on the intensity of a beam of light")
  :ExpFormat ("using the effect of a polarizer on the intensity of ~A light"
	      (polarization-fraction ?fraction))
  :EqnFormat ((polarization-intensity-eqn ?fraction))) 

(defun polarization-intensity-eqn (fraction)
  (cond ((and (numberp fraction) (= fraction 0)) "If = 0.5*Ii")
	(t "If = Ii cos($q)^2")))

(defun polarization-fraction (fraction)
  (cond ((and (numberp fraction) (= fraction 0)) "unpolarized")
	((and (numberp fraction) (= fraction 1)) "polarized")
	(t "partially polarized")))

(defoperator polarizer-intensity-contains (?sought)
   :preconditions 
   (
    (polarization-triple ?beam ?incoming ?polarizer ?outgoing)
    (polarization ?beam ?incoming ?dir)
    (bind ?fraction (if ?dir 1 0))
    (any-member ?sought ((intensity ?beam at ?incoming :time ?t)
			 (intensity ?beam at ?outgoing :time ?t) ))
   )
   :effects ((eqn-contains (polarizer-intensity ?beam ?incoming ?outgoing 
						?fraction ?t) ?sought)
	     ))

;; find polarization directions.
;; Currently, the student does not define any quantity representing 
;; the direction of polarization.
(defoperator do-polarization (?beam ?outgoing)
  :preconditions 
  ((polarization-triple ?beam ?incoming ?polarizer ?outgoing)
   (polarization ?polarizer ?dirp))
  :effects ((polarization ?beam ?outgoing ?dirp)))

(defoperator write-polarizer-intensity (?beam ?incoming ?outgoing ?t)
  :preconditions 
  (
   (polarization ?beam ?incoming ?dirin)
   (polarization ?beam ?outgoing ?dirout)
   (variable ?iin (intensity ?beam at ?incoming :time ?t))
   (variable ?iout (intensity ?beam at ?outgoing :time ?t))
   (bind ?angle (when ?dirin (get-angle-between ?dirin ?dirout)))
   (test (or (null ?dirin) ?angle))
   (bind ?angle-term (if ?dirin `(^ (cos (dnum ,?angle |deg|)) 2) '0.5))
   (bind ?help (if ?dirin 
		   "polarized at an angle of $q relative to the direction of polarization of the polarizer, the transmitted intensity is cos($q)^2 times the incoming intensity." 
		 "unpolarized, half the intensity is absorbed by the polarizer."))
   )
  :effects ( (eqn (= ?iout (* ?iin ?angle-term)) 
		   (polarizer-intensity ?beam ?incoming ?outgoing ?fraction ?t)) )
  :hint (
      (point (string "Relate the net intensity of ~A to the net intensity of ~A." 
		     ?outgoing ?incoming))
      (point (string "If the incoming beam is ~A" (?help identity)))
      (bottom-out (string "Write the equation ~A" 
                     ((= ?iout (* ?iin ?angle-term)) algebra) ))
	 ))

;;;;   Interference pattern for grating with slits

(def-psmclass slit-interference (slit-interference ?grating ?light ?angle
						   ?max-flag ?angle-flag)
  :complexity major
  :short-name "interference for parallel slits"
  :english ("the interference pattern for waves going through parallel slits")
  :ExpFormat ("finding the angles for ~:[destructive~;constructive~] interference"
	      ?flag)
  :EqnFormat ("d*sin($q) = n*$l")) 

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

(defoperator write-slit-interference (?grating ?light ?angle ?angle-flag)
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
		   ?grating ?flag))
    (bottom-out (string "Write the equation ~A" 
			((= (* ?d (sin ?theta)) (* ?n ?lambda)) algebra) ))
    ))

;;;;   Frauenhofer diffraction formula

(def-psmclass frauenhofer-diffraction 
  (frauenhofer-diffraction ?grating ?light ?angle ?angle-flag)
  :complexity major
  :short-name "single slit diffraction (minima)"
  :english ("the interference pattern for waves going through a single slit")
  :ExpFormat ("finding the angles for destructive interference")
  :EqnFormat ("w*sin($q) = n*$l")) 

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
  :english ("the minimum angle of resolution for a circular aperture")
  :ExpFormat ("applying the minimum angle of resoltuon for a system with a circular aperture")
  :EqnFormat "$q = 1.22*$l/d")

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
