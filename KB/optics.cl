
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
;; as compound bodies are by complex terms of the form (compound ?lens1 ?lens2).  
;; If that were done any property or equation mentioning a lens would
;; have to be prepared to take a compound lens term as argument.  But in fact interfacing 
;; with the workbench dialog boxes is much easier if we just predefine a single name for 
;; the compound lens in the problem statement, and declare it as such in the problem 
;; statement.  So that is what we do.
;; We also use systems of more than 1 lens for which a net magnification can be defined. 
 

(def-qexp object-distance (object-distance ?lens)
   :units |m|
   :english ("the distance of the object from ~A" (nlg ?lens))
   :fromWorkbench `(object-distance ,body)
)

(defoperator define-object-distance (?lens)
  :preconditions ( (bind ?do-var (format-sym "do_~A" (body-name ?lens))) )
  :effects ( (variable ?do-var (object-distance ?lens))
             (define-var (object-distance ?lens)))
  :hint (
       (bottom-out (string "Define a variable for the distance of the object from ~A by using the Add Variable command on the Variable menu and selecting Object Distance."  ?lens))
       ))

(def-qexp image-distance (image-distance ?lens)
   :units |m|
   :english ("the distance of the image from ~A" (nlg ?lens))
   :fromWorkbench `(image-distance ,body)
)

(defoperator define-image-distance (?lens)
  :preconditions ( (bind ?di-var (format-sym "di_~A" (body-name ?lens))) )
  :effects ( (variable ?di-var (image-distance ?lens))
             (define-var (image-distance ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for the distance of the image from ~A by using the Add Variable command on the Variable menu and selecting Image Distance."  ?lens))
       ))

(def-qexp focal-length (focal-length ?lens)
   :units |m|
   :english ("the focal length of ~A" (nlg ?lens))
   :fromWorkbench `(focal-length ,body)
)

(defoperator define-focal-length (?lens)
  :preconditions ( (bind ?f-var (format-sym "f_~A" (body-name ?lens))) )
  :effects ( (variable ?f-var (focal-length ?lens))
             (define-var (focal-length ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for the focal length of ~A by using the Add Variable command on the Variable menu and selecting Focal Length."  ?lens))
       ))

(def-qexp magnification (magnification ?lens)
   :units NIL
   :english ("the magnification of ~A" (nlg ?lens))
   :fromWorkbench `(magnification ,body)
) 

(defoperator define-magnification (?lens)
  :preconditions ( (bind ?m-var (format-sym "m_~A" (body-name ?lens))) )
  :effects ( (variable ?m-var (magnification ?lens))
            (define-var (magnification ?lens)) )
  :hint (
       (bottom-out (string "Define a variable for the magnfication of ~A by using the Add Variable command on the Variable menu and selecting Magnification."  ?lens))
       ))

(def-qexp radius-of-curvature (radius-of-curvature ?mirror)
   :units |m|
   :english ("the radius of curvature of ~A" (nlg ?mirror))
   :fromWorkbench `(radius-of-curvature ,body)
)

(defoperator define-radius-of-curvature (?mirror)
 :preconditions ( (bind ?r-var (format-sym "r_~A" (body-name ?mirror))) )
 :effects ( (variable ?r-var (radius-of-curvature ?mirror))
            (define-var (radius-of-curvature ?mirror)) )
  :hint (
       (bottom-out (string "Define a variable for the radius of curvature of ~A by using the Add Variable command on the Variable menu and selecting Radius of Curvature."  ?mirror))
       ))

(def-qexp lens-distance (lens-distance ?lens1 ?lens2)
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

;;
;; Optics Equations
;;

; focal length of spherical mirror = r/2
(def-psmclass focal-length-mirror (focal-length-mirror ?mirror)
  :complexity major  ; ??
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
  :effects ( (draw-line (line ?r) ?dir)
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
  :preconditions ((bind ?var (format-sym "test_~A" ?angle)))
  :effects ((variable ?var (test-var ?angle))
	  (define-var (test-var ?angle))))

;;; Snell's Law

(def-psmclass snells-law (snells-law ?line1 ?line2 ?angle-flag)
  :complexity major
  :english ("Snell's law")
  :ExpFormat ("using Snell's law for ~A and ~A" (nlg ?line1) (nlg ?line2))
  :eqnFormat ("n1*sin($q1) = n2*sin($q2)"))

(defoperator snells-law-contains (?sought)
  :preconditions 
  ( (snell-system ?line1 ?medium1 ?line2 ?medium2 ?normal-to-surface)
    (any-member ?sought (
			 (angle-between (line ?line1) ?whatever)
			 (angle-between (line ?line2) ?whatever)
			 (angle-between ?whatever (line ?line1))
			 (angle-between ?whatever (line ?line2))
			 (dir (line ?line1))
			 (dir (line ?line2))
			 (index-of-refraction ?medium1)		
			 (index-of-refraction ?medium2)))
    (bind ?angle-flag (eq (first ?sought) 'dir))
    (wave-medium ?medium1) (wave-medium ?medium2) ;sanity check
    )
  :effects ( (eqn-contains (snells-law ?line1 ?line2 ?angle-flag) ?sought) ))

;; construct variable for angle of incidence or angle of refraction.
(defoperator get-snell-angle1 (?theta)
  :preconditions 
  ( (bind ?l (sort `((line ,?line) (line ,?normal-to-surface)) #'expr<))
    (variable ?theta (angle-between . ?l)))
  :effects ((snell-angle ?theta ?line ?normal-to-surface nil)))

;; alternative form of angle for cases where the direction
;; of the line is sought
(defoperator get-snell-angle2 (?theta)
  :preconditions 
  ( (variable ?angle (dir (line ?line)))
    (variable ?norm (dir (line ?normal-to-surface)))
    (variable ?dummy-var (test-var ?angle))
    (bind ?theta `(- ,?angle ,?norm)))
  :effects 
  ( (snell-angle ?theta ?line ?normal-to-surface t)
    (implicit-eqn (= ?dummy-var (cos ?theta)) (draw-line (line ?line) unknown))
    ))
 
(defoperator write-snells-law (?line1 ?line2 ?angle-flag)
  :preconditions 
  ( 
   (snell-system ?line1 ?medium1 ?line2 ?medium2 ?normal-to-surface)
   ;;
   (variable ?n1 (index-of-refraction ?medium1))
   (variable ?n2 (index-of-refraction ?medium2))
   (snell-angle ?theta1 ?line1 ?normal-to-surface ?angle-flag)
   (snell-angle ?theta2 ?line2 ?normal-to-surface ?angle-flag)
   (debug "do Snell's law for ~A and ~A~%" ?line1 ?line2)
   )
  :effects ( (eqn (= (* ?n1 (sin ?theta1)) (* ?n2 (sin ?theta2))) 
		  (snells-law ?line1 ?line2 ?angle-flag)))
  :hint (
	 (point (string "How is the angle of ~A related to the angle of ~A?"
			?line1 ?line2))
	 (teach (string "Use Snell's law")) 
	 (bottom-out (string "Write the equation ~A" 
			     ((= (* ?n1 (sin ?theta1)) 
				 (* ?n2 (sin ?theta2))) algebra)))
	 ))
