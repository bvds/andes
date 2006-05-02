;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Physics-Funcs.cl
;;; Kurt VanLehn
;;; 10/20/2000
;;;
;;; This file defines functions that support the code in Newtons2.cl
;;; but can be safely compiled for use in the solutions.
;;;


;;; ====================== Time point and intervals =================
;;; Assumes that time intervals are represented by (During <start>
;;; <finish>) where <start> and <finish> are time points.  Assumes
;;; that time points are represented by integers that are
;;; chronologically ordered.  That is, 1 stands for a point in time
;;; that is before the point in time represented by 2.  Assumes that
;;; intervals are open (they don't include their endpoints) because a
;;; velocity can be at-rest at the begining of an interval but
;;; non-zero everywhere else, so we need open intervals to make the
;;; tinsidep of draw-velocity-straight to work.


(defun tintersect (times)
   "Given a list of time points or intervals, returns their intersection or NIL"
   (cond ((null times) NIL)
         ((not (consp times)) (error "Non-list given to tintersect: ~a" times))
         ((null (cdr times)) (car times))
         (T  (tintersect2 (car times) (tintersect (cdr times))))))

(defun tintersect2 (t1 t2)
   "Given two times, returns their intersection or NIL"
   (cond ((and (null t1) (null t2)) 
	   (error "tintersect2 can't return for both times nil."))
	 ((null t1) t2)
         ((null t2) t1)
         ((equal t1 t2) t1)
         ((and (time-pointp t1)
               (time-intervalp t2)
               (< (second t2) t1 (third t2)))
          t1)
         ((and (time-pointp t2)
               (time-intervalp t1)
               (< (second t1) t2 (third t1)))
          t2)
         ((and (time-intervalp t1)
               (time-intervalp t2))
          (tintersect3 (second t1) (third t1) (second t2) (third t2)))))

(defun tintersect3 (t1-start t1-finish t2-start t2-finish)
   "Given beginning and ending points of two time intervals, returns their intersection or NIL"
   (let ((t3-start (max t1-start t2-start))
         (t3-finish (min t1-finish t2-finish)))
      (if (< t3-start t3-finish)
          (list 'during t3-start t3-finish))))

(defun time-intervalp (x)
   "Return the argument if it is a time interval and NIL otherwise"
   (when (and (consp x) (equal (car x) 'during))
      (if (not (and (time-pointp (second x))
                    (time-pointp (third x))
                    (< (second x) (third x)) 
                    (null (cdddr x))))
         (error "~a has illegal time interval syntax." x))
      x))

(defun time-consecutivep (x)
  "Assuming times are labeled by consecutive integers, true if times in interval x are consecutive"
  (and (time-intervalp x)
       (= (- (third x) (second x)) 1)))

(defun time-pointp (x)
   "Non-null if the argument is a time-point."
   (and (integerp x) (not (minusp x))))


(defun tinsidep (t1 t2)
   "non-null if the first time is inside the second time."
   (cond ((null t2) t) ;timeless is expressed as null time
	 ((time-pointp t1)
	  (cond ((time-intervalp t2)
		 (< (second t2) t1 (third t2)))
		((time-pointp t2)
		 (equal t1 t2))))
	 ((time-intervalp t1)
	  (cond ((time-intervalp t2)
		 (<= (second t2) (second t1) (third t1) (third t2)))))))

(defun tendpointp (t1 t2)
  "true is first time is an endpoint of the second time"
  (and (time-intervalp t2) (or (equal t1 (second t2)) (equal t1 (third t2)))))

(defun tinsidep-include-endpoints (t1 t2)
  "non-null if the first time is inside the second time, the second time is a closed interval"
  (or (tinsidep t1 t2) (tendpointp t1 t2)))

(defun tinsidep-include-second-endpoint (t1 t2)
  "non-null if the first time is inside the second time, the second time is a half-open interval"
  (or (tinsidep t1 t2)
      (and (time-intervalp t2)
	   (time-pointp t1)
	   (equal t1 (third t2)))))

(defun tbeforep (t1 t2)
   "non-null if t1 and t2 are both time points and t1 is before t2"
   (and (time-pointp t1)
	(time-pointp t2)
	(< t1 t2)))

(defun tearlierp (t1 t2) ; unlike tbeforep, applies to points and intervals
 "true if t1 is strictly earlier than t2 (all points in t1 are before any in t2)" 
 (cond ((and (time-pointp t1) (time-pointp t2))
            (< t1 t2))
       ((and (time-pointp t1) (time-intervalp t2))
            (<= t1 (second t2)))
       ((and (time-intervalp t1) (time-pointp t2))
            (<= (third t1) t2))
       ((and (time-intervalp t1) (time-intervalp t2))
            (<= (third t1) (second t2)))
	(T (error "bad time arg to tearlierp: ~A ~A~%" t1 t2))))

(defun tsomewhat-earlierp (t1 t2) 
 "true if there is no point in t2 that is earlier than any point in t1" 
 (cond ((and (time-pointp t1) (time-pointp t2))
            (<= t1 t2))
       ((and (time-pointp t1) (time-intervalp t2))
            (<= t1 (second t2)))
       ((and (time-intervalp t1) (time-pointp t2))
            (<= (second t1) t2))
       ((and (time-intervalp t1) (time-intervalp t2))
            (<= (second t1) (second t2)))
	(T (error "bad time arg to tsomewhatealierp: ~A ~A~%" t1 t2))))

(defun successive-intervals (tt)
  "return list of successive sub-intervals making up t1 to t2"
  (let ((t1 (second tt)) (t2 (third tt)))
   (cond ((= (- t2 t1) 1) (list tt))
         ((> (- t2 t1) 1) 
	   (cons `(during ,t1 ,(1+ t1)) 
	          (successive-intervals (list 'during (1+ t1) t2))))
         (T NIL))))

;; this is for use in rules that sum over sub-intervals
(defun proper-subintervalp (t1 tt)
  "true if tt is not atomic and t1 is an atomic sub-interval of tt or t1 equals tt"
  (and (time-intervalp tt)
       (> (- (third tt) (second tt)) 1) ;this prevents trivial equations
       (tinsidep t1 tt)
       (or (equal t1 tt) (time-consecutivep t1) )
       ))


;;; ===========================================================================
;;; Geometry helper functions
;;; This are written to take either numbers, zero, unknown,
;;; or (dnum <number> |deg|), or (parameter <atom>).
;;; Eventually, they may get smarter and be able to accept algebraic 
;;;  formulas in terms
;;; of (dnum ...) and (parameter ...)
;;; Now also have z-axis directions as 'into or 'out-of or 'z-unknown
;;;
;;; All Andes vectors are either in the xy-plane or along the z-axis.
;;; z-unknown guarantees vector lies along z axis, and there are some things we
;;; can use this for, e.g z-unknown is perpendicular to any x-y plane angle 
;;; and has no cross-product with another zdir angle.
;;;
;;; Assume that 'unknown means the vector is in the xy-plane.
;;; Should, in principle have something to indicate that the direction
;;; of a vector is completely unknown, but this is not needed yet.  
;;; 

(defun perpendicularp (x1 x2)
   "true if the two angles are known to be perpendicular"
   (cond
         ((equal x1 'zero) T)  ; zero vector orthogonal to anything
         ((equal x2 'zero) T)
	 ((z-dir-spec x1) (not (z-dir-spec x2)))
	 ((z-dir-spec x2) (not (z-dir-spec x1)))
	 ;; both angles in x-y plane.
	 ((parameter-or-unknownp x1) NIL)
         ((parameter-or-unknownp x2) NIL)
	 ;; else both should be known xy angle specifiers
         (T (equal 90 (mod (- (convert-dnum-to-number x1) 
			      (convert-dnum-to-number x2)) 180)))))

(defun non-zero-projectionp (vector-dir xyz axis-rot)
   "non-null if the vector is not known to be orthogonal to the given axis."
   (not (perpendicularp vector-dir (axis-dir xyz axis-rot))))

;; Note that the behavior for 'unknown and 'z-unknown are rather different
(defun parallel-or-antiparallelp (x1 x2)
   "true if the two angles are known to be parallel or anti-parallel"
   (cond 
    ((equal x1 'zero) NIL)
    ((equal x2 'zero) NIL)
    ;; if first is zdir vec, it's parallel if second is zdir
    ((z-dir-spec x1) (z-dir-spec x2))
    ((z-dir-spec x2) NIL) ;since x1 is in xy-plane
    ;; must be in xy-plane
    ((parameter-or-unknownp x1) NIL)
    ((parameter-or-unknownp x2) NIL)
    ;; known angle in xy-plane
    (T (zerop (mod (- (convert-dnum-to-number x1) 
		      (convert-dnum-to-number x2)) 180)))))

;; Given an axis specified by 'x|'y|'z + rotation, return 3D direction code.
;; maps z-axes to direction code 'out-of [the plane.]
(defun axis-dir (xyz rot)
 "return direction of given axis in degress or z-axis specification"
 (cond ((eq xyz 'x) rot)
       ((eq xyz 'y) (+ rot 90))
       ((eq xyz 'z) 'out-of)		; ignore rot
       (t (error "invalid axis specification~%"))))

(defun axis-name (xyz)
 "return string name for each axis"
 (cond ((eq xyz 'x) "x")
       ((eq xyz 'y) "y")
       ((eq xyz 'z) "z")
       (t (error "invalid axis specification~%"))))



; !!! following only applies to known xy plane angles.
; calling with zero-length, dir unknown or z-axis dirs will throw error
(defun convert-dnum-to-number (x)
  "Expects either a number or a number of degrees, and returns a number."
  (cond ((numberp x) x)
	((degree-specifierp x) (second x))
        (T (error "Non-numerical angle measure: ~a" x))))

(defun parameterp (x)
  "Non-null if the argument has the form (parameter <symbol>)"
      (and (consp x)
	   (equal (first x) 'parameter)
           (symbolp (second x)) 
           (null (cddr x))))

(defun parameter-or-unknownp (x)
  "Non-null if argument is parameter or 'unknown or 'z-unknown"
   (or (eq x 'unknown) (eq x 'z-unknown) (parameterp x)))

(defun degree-specifierp (x)
  "Non-null if the argument has the form (dnum <number> |deg|) or (dnum <number> DEG)"
  (and (or (unify x '(dnum ?val |deg| :error ?err))
	   (unify x '(dnum ?val DEG :error ?err)))
       (numberp (second x))))

; Following for use in contexts in which arg is either dnum or a plain
; number we know to be an angle measure (e.g. an axis rotation).
(defun degrees-or-num (x)
 "true if x is either DNUM in degrees or a plain number"
   (or (numberp x) (degree-specifierp x)))

(defun same-angle (x1 x2)
   "true if two forms, e.g. dnum and number, represent same angle measure"
   (cond ((and (known-z-dir-spec x1) (known-z-dir-spec x2)) 
            (equal x1 x2))
         ((and (degrees-or-num x1) (degrees-or-num x2))
	  (zerop (mod (- (convert-dnum-to-number x1) 
			 (convert-dnum-to-number x2)) 360)))
       (T NIL)))

(defun opposite (x)
   "returns a direction specifier that is 180 degrees opposite the given direction"
   (cond ((eq x 'into) 'out-of) ; special cases for z-axis dir specifiers
         ((eq x 'out-of) 'into)
	 ((eq x 'z-unknown) x)
	 ((eq x 'unknown) x)
	 ((degrees-or-num x) 
	  (dir-to-dnum (mod (+ (convert-dnum-to-number x) 180) 360)))
	 (t (error "Opposite for ~A" x))))

(defun minimal-x-rotations (Bag)
   "Given a bag of directions, returns set of number between 0 and 90 for x-rotation of axes"
   (loop for dir in bag with Result finally (return Result) do
      (if (degree-specifierp dir)
	  (pushnew (mod (second dir) 90) Result))))

(defun z-dir-spec (x)
    "returns true if expression is a z-axis direction specifier"
   (or (known-z-dir-spec x) (eq x 'z-unknown)))

(defun known-z-dir-spec (x)
    "returns true if expression is a z-axis direction specifier for a known direction"
   (or (eq x 'into) (eq x 'out-of)))

(defun rotation-zdir (rotate-dir)
   "Convert given rotation ('cw or 'ccw) to z-axis direction specifier by rhr"
   (cond ((equal rotate-dir 'cw) 'into)
         ((equal rotate-dir 'ccw) 'out-of)
	 (T (error "bad rotation direction for rhr: ~A" rotate-dir))))

(defun zdir-phi (zdir)
  "Convert given z-axis direction (into or out-of) into polar angle degrees"
  (when (known-z-dir-spec zdir)
    (if (equal zdir 'into) '(dnum 180 |deg|) '(dnum 0 |deg|))))

; following assumes f-dir and r-dir are in the plane, 
; and also that not parallel nor anti-parallel, so torque is non-zero
(defun torque-rotate-dir (f-dir r-dir)
 "return rotation tendency ('cw or 'ccw) of force given dir and dir of relative position of point of application from axis (deg)"
 (if (> (- f-dir r-dir) 0) 
      (if (> (- f-dir r-dir) 180) 'cw
        'ccw)
  (if (> (- r-dir f-dir) 180) 'ccw
    'cw)))

(defun torque-zdir (f-dir r-dir)
 "return torque vector z-axis direction by rhr from force and relpos dirs"
   (rotation-zdir (torque-rotate-dir f-dir r-dir)))

(defun rad-to-deg (radians)
 (* radians (/ 180 pi)))

(defun dir-from-compos (xc yc)
 "return nearest integral direction given vector components"
  (dir-to-dnum (round (mod (rad-to-deg (atan yc xc)) 360))))

;;; In following, a a "dir" is either a plain number representing xy angle 
;;; in degrees or one of the special atoms: unknown, zero, into, out-of, 
;;; z-unknown.
;;; a "dir-term" may be a dnum term in degrees.  
;;; Following is to convert dir-terms to dirs for internal calculations 
;;; and back again to return.
;; !!! this doesn't handle parameter terms (little used)
(defun term-to-dir (dir-term) ; term may be dnum or 'into 'out-of 'zero
"extract number of degrees from dnum, otherwise just return arg uncharged"
  (if (degree-specifierp dir-term) (second dir-term) 
               dir-term))

(defun dir-to-dnum (dir)
"return dnum term for number of degrees, else arg unchanged"
   (if (numberp dir) `(dnum ,dir |deg|) dir))

(defun get-angle-between (dir1-term dir2-term)
  "return dir of angle between two vector direction terms, NIL if unknown"
  (let ((dir1 (term-to-dir dir1-term))
	(dir2 (term-to-dir dir2-term)))
    (cond ((eq dir1 'zero) nil) ;treat zero vector as unknown direction
	  ((eq dir2 'zero) nil) 
	  ;; any zdir (incl z-unknown) orthogonal to any xy-plane dir 
	  ;; (incl unknown) test this before checking for unknown.
	  ((and (z-dir-spec dir1) (not (z-dir-spec dir2))) 90)
	  ((and (z-dir-spec dir2) (not (z-dir-spec dir1))) 90)
	  ;; else both zdir or both xy-dir, but either may be unknown.
	  ((or (eq dir1 'z-unknown) (eq dir2 'z-unknown)) NIL) ;can't determine
	  ((z-dir-spec dir1) (if (eq dir1 dir2) 0 180))
	  ;; else both xy-dir, but maybe unknown
	  ((or (eq dir1 'unknown) (eq dir2 'unknown)) NIL) ;can't determine
	  ;; else two known xy-plane angles:
	  (T (min (mod (- dir1 dir2) 360)
		  (mod (- dir2 dir1) 360))))))

;; For computing cross product direction.
(defun cross-product-dir (dir-term1 dir-term2)
"return term for cross product direction of two given vector dir terms"
  (dir-to-dnum (cross-product (term-to-dir dir-term1)
                              (term-to-dir dir-term2))))

(defun cross-product (dir1 dir2)
  "return direction of cross product of two Andes vector dirs"
  (cond ((parallel-or-antiparallelp dir1 dir2) 'zero)
	((eq dir1 'zero) 'zero)
	((eq dir2 'zero) 'zero)
	;; cross non-zdir with zdir:
	((z-dir-spec dir2) (opposite (cross-product dir2 dir1)))
	((and (z-dir-spec dir1) (eq dir2 'unknown)) 'unknown)
	;; so not both zdir (else would be parallel-or-antiparallelp)
	((eq dir1 'z-unknown) 'unknown) 
	;; cross z-dir vector with non-zdir:
	((eq dir1 'out-of) (mod (+ dir2 90) 360))
	((eq dir1 'into)   (mod (- dir2 90) 360))
	;; else must be crossing two non-parallel xy-plane vectors
	((eq dir1 'unknown) 'z-unknown)
	((eq dir2 'unknown) 'z-unknown)
	;; use function for dir torque(fdir rdir), inverting order of args
	(T  (torque-zdir dir2 dir1))
   ))

(defun horizontal-or-vertical (dir-expr)
 "return true if this specifies a horizontal or vertical direction"
    (when (degree-specifierp dir-expr)
       (= (mod (term-to-dir dir-expr) 90)
          0)))

;;
;; Compound body terms: (compound orderless body1 ... bodyn)
;; Body names should be sorted by expr<
;; Must have more than one body. 
;;
(defun compound-bodyp (body)
   "non-null if arg is a compound body term"
   (and (consp body) (eq (car body) 'compound) (orderless-p (cdr body))))

#|  ;; not used anywhere
(defun part-of-body (body1 body2)
   "non-null if body1 is part or all of body2"
   (or (equal body1 body2) ; same
       (and (atom body1) (compound-bodyp body2)
            (member body1 (cdr body2)))))
|#

(defun simple-parts (body)
   "get list of atomic parts of a body, simple or compound"
   (cond ((atom body) (list body))
         ((compound-bodyp body) (cddr body))
	 (T (error "bad body term: ~A" body))))

;; 
;; For multiple body systems: (system body1 ... bodyn)
;; Body names should be sorted by expr<
;; Shouldn't have compound body terms in list, list all simple-parts instead.
;; Possible to have a system with has only one body
;;
(defun systemp (sys)
   "non-null if arg is many-body system term"
   (and (listp sys) (equal (first sys) 'system)))

(defun part-of-sys (obj sys)
   "non-null if object is part or all of sys"
   (or (equal obj sys) ; obj is same system
       (and (compound-bodyp obj) (systemp sys)
             (subsetp (simple-parts obj) (cdr sys)))
       (and (atom obj) (systemp sys)
            (member obj (cdr sys)))))

;;; ============
;;; Algebraic manipulation


;;; vars-in-eqn returns the variables in a given equation.
;;; It assumes any atom that is not a number, parameter,  dimensioned number
;;; algebraic operator, or physical constant symbol must be a variable.

(defun vars-in-eqn (eqn)
   "returns the set of variables in a given equation."
   (cond ((null eqn) NIL)
	 ((numberp eqn) NIL)
         ((parameterp eqn) NIL)
         ((dimensioned-numberp eqn) NIL)
         ((member eqn *algebraic-operators*) NIL)
	 ((physconstp eqn) NIL)    ; defined in constants.cl
         ((symbolp eqn) (list eqn))
         ((consp eqn) (union (vars-in-eqn (car eqn)) (vars-in-eqn (cdr eqn))))
         (T (error "~&Non-eqn ~a" eqn))))
;;;
;;;  Yuck!  This should match the operators independently re-defined in:
;;;  Help/parse-andes.cl
;;;  Help/pre2in.cl
;;;  ReportGenerators/SolcompRep.cl
;;;  ReportGenerators/SolutionComponents.cl
;;;
(defparameter *algebraic-operators* 
    '(= + - * / ^ sin cos tan abs ln log10 sqrt exp))

(defun dimensioned-numberp (x)
  "Non-null if the argument has the form (dnum number units :error err)"
  ;; NB: equations on rotational problems can have (DNUM $p rad/s). 
  ;; We return NIL to prevent second arg from being used as a number
  ;; Means this doesn't test just for dnum-termhood.
  (and (unify x '(dnum ?val ?units :error ?err))
       (numberp (second x))))

(defun numvalp (exp) 
"true if exp is a number with or without units"
    (or (numberp exp) (dimensioned-numberp exp)))

;; should test both arguments using dimensioned-numberp
;; before calling this routine.
(defun compare-dnums (x y &optional (bindings no-bindings))
  "Compare two dnums, properly handling :error keyword."
  (let* ((dnum-expr '(dnum ?val ?units :error ?err))
	 (x-err (or (cdr (get-binding '?err (unify x dnum-expr))) 0))
	 (y-err (or (cdr (get-binding '?err (unify y dnum-expr))) 0))
	 (x-val (second x))
	 (y-val (second y)))
    (if (and (>= (+ x-val x-err) (- y-val y-err))
	     (<= (- x-val x-err) (+ y-val y-err)))
	(unify (third x) (third y) bindings)
      fail)))

;;; This is used in Newton's laws to convert a component equation 
;;; into an equation with the component variables replaced by expressions.

(defun subst-parallel-lists (old new expr)
   "Given list of old elements as first argument, and list of new elements as
    second argument, replaces the old elements with the corresponding new ones
    in a copy of given expressin, which is in the third argument."
   (let ((nexpr (copy-tree expr)))
        (loop for o1 in old as n1 in new do 
           (nsubst n1 o1 nexpr :test #'equal))
        nexpr))


(defun scalar-quantityp (q)
  "True if the given quantity is neither a magnitude nor a direction."
   (not (and (listp q)
	     (or (eq 'compo (first q))
		 (eq 'mag (first q))
		 (eq 'dir (first q))))))
	 
;; Takes axis-expr of form (axis ?xyz ?rot)
(defun vector-compo (vec-expr axis-expr)
  "Given vector expr return expr for component along specified axis"
   (list 'compo (second axis-expr) (third axis-expr) vec-expr))

;; for vector component expressions:
(defun componentp (expr)
   "true if expr denotes vector component (at time)"
   (and (eq (first expr) 'compo)
	(= (length expr) 4)))

(defun compo-base-vector (expr)
   "extract base vector from term for vector component (at time)"
   ;; (compo ?axis ?rot ?vector)
   (fourth expr))

(defun compo-axis-rot (expr)
   "extract axis rotation from term for vector component (at time)"
   ;; (compo ?axis ?rot ?vector)
   (third expr))

(defun compo-axis-xyz (expr)
   "extract axis label from term for vector component (at time)"
   ;; (compo ?axis ?rot ?vector)
   (second expr))

;; for all possibly-time-indexed quantity expressions:
(defun time-of (quant)
  "return time of a quantity, NIL if none"
  (cond ((atom quant) (error "~A must be a list~%" quant))
	((member ':time quant) (second (member ':time quant)))
	;; this covers (compo ...), (mag ...), (dir ...):
	((listp (first (last quant))) (time-of (first (last quant))))))

;; for all possibly-time-indexed quantity expressions:
(defun remove-time (expr)
  "remove any time from quantity"
  (cond ((atom expr) expr)
	((member ':time expr) 
	 (let* ((key (member ':time expr)) 
		(post (cddr key)))
	  (get-any-default-value post)	;remove any default value from y
	  (append (ldiff expr key) post))) ;expr with keyword pair removed
	;; this covers (compo ...), (mag ...), (dir ...):
	(t (mapcar #'remove-time expr))))

(defun set-time (expr time)
  "apply time to quantity"
    (cond  ((equal (first expr) 'compo)     ;case (compo xyz axis quant)
	    (append (subseq expr 0 3) (list (set-time (fourth expr) time))))
	   ;; case (mag quant) or (dir quant)
	   ((or (equal (first expr) 'mag) (equal (first expr) 'dir))
	    (list (first expr) (set-time (second expr) time)))
	   ;; all others assumed scalar
	   ((listp expr) (append (remove-time expr) `(:time ,time)))
	   (t (error "can't add time ~A to expression ~A~%" time expr))))

;; for all possibly-axis-indexed quantity expressions:
(defun axis-of (quant)
  "return time of a quantity, NIL if none"
  (cond ((atom quant) (error "~A must be a list~%" quant))
	((member ':axis quant) (second (member ':axis quant)))
	;; this covers (compo ...), (mag ...), (dir ...):
	((listp (first (last quant))) (axis-of (first (last quant))))))

;;; ===================== hacks ========================================

(defparameter *lk-hack* nil
  "When true, causes only one lk equation to be generated.")

(defun not-too-many-lk-eqns (N)
  "If *lk-hack* is non-null, then true when N=0 else true when N=0 or N=1.
   Use *lk-hack* to prevent generating all 10 possible lk solutions"
  (if *lk-hack* (zerop N)
    (< N 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format-sym
;; Shorthand to build a symbol from a format string and arguments
;; Interns symbol in current package so Lisp doesn't print it with #:
;; which is bad for algebra code reading our equation output.
(defmacro format-sym (spec &rest args)
  `(intern (format NIL ,spec ,@args)))

;;;
;;; For forming variable names:
;;;

(defun time-abbrev (x)
  "returns string of form \"1\" for points or \"12\" for intervals to abbreviate time in variable names" 
  (cond ((null x) nil)
	((time-intervalp x) (format NIL "~A~A" (second x) (third x)))
	(t (format NIL "~A" x))))

(defun body-name (term)
  "given body term, returns string to put in body name slot in variable names" 
  ; !!! should also strip bad characters, e.g. hyphen, dollar sign
  (cond ((null term) "")
        ((atom term) (string term))
        ;; Complex terms may be simple functional terms like (end-1 str) or 
	;; compound body/system terms like (compound orderless b1 b2 ...). 
	;; for compound bodies we want body1&body2&body3
	((compound-bodyp term)
	 (let ((y (mapcar #'body-name (cddr term))))
	   (format nil "~A~{__~A~}" (car y) (cdr y))))
	;; For anything else just concatenate all symbols in the list, 
	;; recursing down tree in case of nested complex terms, e.g. 
	;; (foo (end-1 str) block (end-2 str))
        ((listp term) (concatenate 'string (body-name (first term)) 
					   (body-name (rest term))))
	(T    term)) ; signal error?
)
