;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ENTRY-API -- "adapter" functions to handle the Andes Workbench API calls
;;              for non-EQ student entries
;;
;; This module contains one entry-handler function for each Workbench entry
;; API call.  These handlers interpret the parameters sent in the API calls,
;; translating the calls into the representations needed by the help 
;; system. 
;;
;; For each API call foo, the API handler is called On-foo and takes the same
;; arguments.
;;
;; Each API handler function normally returns a Student Entry struct 
;; representing the entry.  As side-effects the handlers also update the 
;; symbol table with student label entries as appropriate and install the
;; entry on the entry list.
;;
;; An API handler may return T or NIL in case there is no good entry to be 
;; looked up; this value should be returned to the workbench.  This is 
;; currently only used for some angle labelling entries which need not 
;; correspond to solution graph actions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :user)

;;-----------------------------------------------------------------------------
;; Helper functions for workbench API arguments 
;;-----------------------------------------------------------------------------

;;
;; Times
;;

(defun arg-to-time (time-arg)
  "convert workbench time argument symbol into a KB time expression"
  (cond 
	; may be NIL on probs w/o distinguished times
        ((null time-arg) NIL) ;(get-default-time))
	; Andes1 WB allowed defined student variable for duration as interval name.
	; no longer used in Andes2
	; ((get-student-time time-arg))
	; may be time interval symbol of form |T0 to T1|
	((wb-interval-namep time-arg) (get-wb-interval time-arg))
	; or may be time point name of form T1
        ((wb-time-pt-namep time-arg) (get-wb-time-pt time-arg)) 
	(T (warn "unrecognized time argument:~A" time-arg)
	   NIL)))

(defun get-default-time () 
  "return the default KB time for the problem"
   1) ; assuming it will always be time point 1 

(defun get-student-time (label)
  "return time term for student-defined duration, NIL if not a duration"
  (let* ((label-str (if (symbolp label) (symbol-name label) label))
         (quant (symbols-referent label-str)))
    (if (and (consp quant) 
             (eq (first quant) 'duration))
        (second quant))))

;; The Andes1 helpsys relied on a problem-specific file giving a map of the 
;; predefined WB time point symbols for the problem to the KB time point 
;; numbers together with their verbal descriptions used to refer to them. 
;; With this method, any symbols at all could in theory have been used for the 
;; predefined time points.  
;;
;; However the WB time point symbols are *always* of form Ti where i = WB 
;; time point index.  Moreover, in *almost* all problems, the WB time point 
;; indices start at 0.  Andes2 KB time point numbers always start at 1 so we 
;; can easily convert by adding 1.
;;
;; However, for no particular reason, the WB time point names in problems 
;; Exlmom* and Exvec[2-5]a happen to start with T1 instead of T0. 
;; We will probably change these workbench problem files so workbench times 
;; adhere to the 0-based convention uniformly in the future. However, if 
;; backwards compatibility is required, say to test the Andes2 help system on 
;; entries in Andes1 logs, then we would need to insert a little filter here 
;; to adjust the time point mapping based on the problem name.
 
(defun wb-time-pt-namep (sym)
   "true if this has form of a predefined workbench time point symbol"
   (let ((str (symbol-name sym)))
    (and (equal (subseq str 0 1) "T")
	 (not (wb-interval-namep sym))
         (numberp (read-from-string (subseq str 1))))))

(defun get-wb-time-pt (sym)
   "convert WB time point label to KB time point number"
   ; assume it's 1+ the number read after initial T
   (1+ (read-from-string (subseq (symbol-name sym) 1))))

;; Workbench time interval name is symbol of form |T0 to T1| 
;; where the endpoint expressions are WB time point names.
(defun wb-interval-namep (sym)
  "true if this is a workbench time interval name"
  (search " to " (symbol-name sym) :test #'equalp))

(defun get-wb-interval (sym)
  "convert WB time interval name to KB time interval expression"
  (let* ((str (string-trim '(#\Space) (symbol-name sym)))
	 (pt1 (read-from-string (subseq str 0 (position #\Space str))))
	 (pt2 (read-from-string (subseq str (1+ (position #\Space str :from-end t))))))

     (list 'during (get-wb-time-pt pt1) (get-wb-time-pt pt2))))

;;
;; Vector directions
;;
(defun arg-to-dir (dir-arg &optional mag-arg (modulus 360))
 "Convert WB API direction and magnitude argument pair to KB direction term"
  (cond ; zero-mag vectors have no direction: use special atom 'zero
        ((and (numberp mag-arg)
	      (= 0 mag-arg)) 'zero)
        ; else may be NIL if unspecified -- use special atom 'unknown
        ((NULL dir-arg) 'unknown)
	; else should be a number
	((not (numberp dir-arg)) (error "non-numeric direction arg:~A" dir-arg))
        ; negative numbers code z-axis directions, use special atoms
        ((= dir-arg -1)  'out-of)
	((= dir-arg -2)  'into)
	((= dir-arg -3)  'z-unknown)
	; else should be xy plane angle in degrees. 
	(T  `(dnum ,(mod dir-arg modulus) |deg|))))

;;
;; Bodies
;;
;; Atomic body name argument is either a KB name (stored in WB file) or a 
;; student-defined compound body label. However, a few names in the existing 
;; problems have hyphens in them. These were changed to underscores in Andes2 
;; problems since algebra module can't handle hyphens in variable names and 
;; variable names are formed with body names as parts. Also, body arguments 
;; are sometimes passed in vbars so preserve mixed case while read, even 
;; though all KB names are upcased, so we upcase them as well.
;;
;; We also handle body arg consisting of a list of simple body names and
;; form a compound body term for it. Note: Originally this arg form was only 
;; sent to assert-compound-object. But it now occurs elsewhere, for example
;; when defining a variable for resistance of a set of components. So the 
;; translation of the set into compound body term may have to be undone when 
;; forming the quantity below in make-quant. 
;;
;; NB: can also get NIL body-arg in some cases, such as agent of net-work.
;; should remain NIL for none.
;;
;; Note we assume body names have no spaces, even if wrapped in vbars
;; (read-from-string used to get symbol in fix-body-name.)
(defun arg-to-body (body-arg)
  "Convert WB API body argument to KB body term" 
  (if (consp body-arg)  ; list of body names: make compound body term
      `(compound orderless ,@(mapcar #'fix-body-name body-arg)) 
  ; else atomic body name:
   ; check if this is a student-defined compound-body label
   (let ((stud-quant (symbols-referent (string body-arg))))
     (if (and stud-quant (compound-object-term stud-quant)) stud-quant
       ; else simple body name:
       (fix-body-name body-arg)))))

(defun fix-body-name (body-arg)
  "Convert atomic body name symbol read from WB arg to KB body symbol"
  ;; NB: can't use read-from-string to make symbol because now some body args 
  ;; from variable dialog are not really body names & might contain spaces 
  ;; (e.g. |all forces|).  Instead, intern in current package 
  ;; (to prevent Lisp printing with #:)
  (intern (string-upcase (substitute #\_ #\- (string body-arg)))))

; For circuits, we have to handle defined compound component terms as arguments.
; E.g. say student variable "foo" has earlier been defined as (resistance (R5 R6)), meaning
; the resistance of the compound of R5 and R6. The workbench now allows "foo" to be used as a 
; name for the compound resistor in OTHER variable definitions.  For example, student may define 
; a variable for voltage-across with body 'foo. This is a form of overloading: "foo" officially 
; stands for the resistance of a compound component, but is overloaded to stand for the compound
; component itself. The body arg "foo" in *this* context will need ultimately to be translated to
; (R1 R2) get us to (voltage-across (R1 R2)) to match the form used in the knowledge base. 
;
; In this case, (arg-to-body 'foo) will simply lookup the definition of "foo" and return 
; (resistance (R5 R6)).  The make-quant cases for circuit component attributes below have been
; coded to be prepared to receive such a quantity form as the body-term and pull out the constituents
; appropriately wherever variable args can be compound equivalents. 

(defun compound-object-term (quant)
   (or (compound-bodyp quant)
       (compound-componentp quant)))

(defun compound-componentp (quant)
   (and (consp quant)
	(= (length quant) 2)
        (or (eq (first quant) 'resistance)
	    (eq (first quant) 'capacitance))
	(consp (second quant))))

(defun bodyterm-complist (bodyterm)
 "extract component list from a compound component term as built by arg-to-body"
 ; arg-to-body builds: (compound orderless C1 C2 ...)
 (cddr bodyterm))  

;; 
;; Type/subtype identifiers
;; Some of these have to be translated using the appropriate mapping table.
;;
;; We don't want case to be significant in symbols used as IDs. 
;; KB ID symbols get converted to upper case by read; we write them here
;; in upper case just to emphasize that fact.  However, workbench wraps some
;; id symbols in vbars when sending to be safe against contained spaces.
;; This leads them to be read in as symbols with mixed-case printnames. 
;; Any id arg passed through arg-to-id will be converted to upper case if
;; not otherwise translated.
;;
(defun arg-to-id (alist id-arg)
  "map a WB API id symbol to a KB symbol"
  (or (cdr (assoc id-arg alist :test #'sym-match))
      ; else no translation: convert to upper case symbol and return it
      (sym-upcase id-arg)))

(defun sym-upcase (sym)
  "given possibly mixed-case symbol, return symbol upper case printname"
    (read-from-string (string-upcase (string sym))))

(defun sym-match (sym1 sym2)
   "case independent comparison of symbol names"
   (string-equal (string sym1) (string sym2)))

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **force-types** '(
		   (grav . WEIGHT)
		   (|Kinetic Friction| . KINETIC-FRICTION)
		   (|Static Friction| . STATIC-FRICTION)
		   ;; Contact force type not used in Andes2. 
		   ;; Replaced by APPLIED in most places,
		   ;; though changed to Normal force in a couple of problems.
		   (Contact . APPLIED)
		   ;; others just need case adjustment
		   )  #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **vector-types** '(
		    (Acceleration . ACCEL)
		    (Ang-Acceleration . ANG-ACCEL)
		    (Position . RELATIVE-POSITION)
		    ;; others just need case adjustment
		    ) #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **quantity-types** '(
		      ;; Most of these mappings now obsolete since workbench 
		      ;; changed to try to send the exact helpsys id. 
		      ;; Just keeping them for backwards compatibility.
		      (|distance travelled| . DISTANCE)
		      (|distance between| . DISTANCE-BETWEEN) ; not used in Andes2 yet
		      (|gravitational acceleration| . GRAVITATIONAL-ACCELERATION)
		      (radius . REVOLUTION-RADIUS)
		      (spring-const . SPRING-CONSTANT)
		      (comp-dist . COMPRESSION)
		      (charge . CHARGE-ON) ;for backwards compatibility
		      (rate-of-current-change . CURRENT-CHANGE)
		      ;; (energy . TOTAL-ENERGY)
		      ;; include vector type ids as well:
		      (Acceleration . ACCEL)
		      (Ang-Acceleration . ANG-ACCEL)
		      (Position . RELATIVE-POSITION)
)  #+sbcl #'equalp)

; KB potential energy type (grav-energy or spring-energy) is implicit in 
; type of body arguments sent from workbench (one should be planet or spring)
; Following helps determine KB PE type to use.  We could look up types in 
; problem givens, but for now for now we just use SLEAZY HACK exploiting fact 
; that in all Andes probs these are named 'spring or 'earth, respectively.
(defun planetp (body-term)
  (sym-match body-term 'earth))
(defun springp (body-term)
  (sym-match body-term 'spring))

;; Dispatch function takes an atom coding a vector attribute and a vector-term
;; Builds and returns a term for that attribute of the vector.
;; Uses function vector-compo defined in kb/Physics-Funcs.cl
;; prop-id = 'mag 'dir 'xc 'yc 'zc 
;;            NIL or anything else gets vector term unchanged
(defun vec-prop (prop-id vector-term)
 (case prop-id
   (mag  `(mag ,vector-term))
   (dir  `(dir ,vector-term))
   (xc (vector-compo vector-term '(axis x 0)))
   (yc (vector-compo vector-term '(axis y 0)))
   (zc (vector-compo vector-term '(axis z 0)))
   (otherwise vector-term)))

;;;; ===========================================================================
;;;; wb-quant -- convert a variable-defining API call to a quantity
;;;;
;;;; Following is used to handle quantity choice results. The two
;;;; arguments are: first, an unevaluated API call call which is either 
;;;; define-variable or define-angle-variable. This is the call that would be
;;;; sent if the student had actually defined a var in the relevant dialog.
;;;; This is convenient way of packaging the spec because the workbench already 
;;;; has code to build these api calls at the end of definition dialogs. 
;;;; The second argument is an optional vector-attribute id, which tells which
;;;; attribute of the vector is to be used, default mag.
;;;; Usage examples:
;;;;    (wb-quant '(define-variable "" average Velocity car T1 NIL NIL) 'mag)
;;;; or for angle quantities
;;;;    (wb-quant '(define-angle-variable "" 270 posx "vf" NIL))
;;;; Return value is the quantity term specified.
;;;;
;;;; In fact the workbench is clever and uses this as follows: when the student
;;;; response is a quantity selection, it sends a command of the form
;;;; (handle-student-response #.(wb-quant (define-variable ....) mag)
;;;; When read by the dispatcher, this has the effect of
;;;;       (handle-student-response <quant-term>)
;;;; so responder code doesn't have to know about the translation.

(defun wb-quant (api-call &optional (vecprop 'mag))
  (case (first api-call)
    ('define-variable (make-quant (third api-call) (fourth api-call) 
				  (fifth api-call) (seventh api-call)
				  (sixth api-call) vecprop))
    ('define-angle-variable (make-angle-quant (fourth api-call) 
                                              (fifth api-call)))
    (otherwise
    	(warn "wb-quant: Unrecognized quantity spec: ~A~%" api-call)
	NIL)))


;;; make-quant -- Build a quantity expr from define-variable arg list. 
;;; Note args come in workbench form. They should be converted to helpsys
;;; form before the case. 
;;;
;;; Prior to Andes 5.0* students could define vector variables without drawing
;;; them, so could also specify vector quantities this way. We handle these
;;; also because this argument list will also be used to send quantity choice 
;;; responses, even though define-variable won't send them. 
;; optional prop is vector attribute to use, 
(defun make-quant (type quant body body2 time &optional (prop 'mag))
  (let* ((quant-type (arg-to-id **quantity-types** quant))
	 (subtype    (if (eq quant-type 'force) (arg-to-id **force-types** type)
		       (arg-to-id **quantity-types** type)))
	 (time-term  (arg-to-time time))
	 (body-term  (arg-to-body body))
	 (body2-term (arg-to-body body2))
	 (fromWorkbench-fn (lookup-exptype-fromWorkbench quant-type)))
    ;; if quantity has registered a fromWorkbench handler in ontology, 
    ;; just use that
    (when fromWorkbench-fn
      (return-from make-quant 
	(apply fromWorkbench-fn (list subtype body-term body2-term time-term))))
    
    ;; else use forms coded here. These blocks could be moved to ontology, 
    ;; but generally use that for simple translations, and 
    ;; leave more involved code here.
    (case quant-type
      ;; scalar quantities:
      (work				; work may be net work, work-nc, or work by an agent
       (cond ((or (null body2-term) (string-equal body2-term '|all forces|))
	      `(net-work ,body-term :time ,time-term))
	     ((or (string-equal body2-term '|nonconservative|)
		  ;; NB: arg-to-body converts hyphen in '|non-conservative forces|
		  (string-equal body2-term '|non_conservative forces|)) 
	      `(work-nc ,body-term :time ,time-term))
	     (T     `(work ,body-term ,body2-term :time ,time-term))))
      (power				; may be net power or power from an agent
       (if (or (null body2-term) (string-equal body2-term '|all forces|)) 
	   `(net-power ,body-term :time ,time-term)
	 `(power ,body-term ,body2-term :time ,time-term)))
      (energy      (case subtype
		     (total   `(total-energy ,body-term :time ,time-term))
		     (kinetic `(kinetic-energy ,body-term :time ,time-term))
		     (rotational `(rotational-energy ,body-term :time ,time-term))
		     ;; new style:
		     ;; Change 9.0.2: add agent slot to PE quants
		     (electric `(electric-energy ,body-term ,body2-term :time ,time-term))
		     (gravitational `(grav-energy ,body-term ,body2-term :time ,time-term))
		     (elastic   `(spring-energy ,body-term ,body2-term :time ,time-term))
		     ;; nuisance: for dipoles we have to construct relevant field term from
		     ;; specified body and type. 
		     (electric-dipole `(dipole-energy ,body-term 
		                           ,(find-dipole-field body-term 'electric)
					   :time ,time-term))
		     (magnetic-dipole `(dipole-energy ,body-term 
		                           ,(find-dipole-field body-term 'magnetic)
					   :time ,time-term))

		     ;; old style: same subtype was sent for all types of P.E:
                  (potential 
		   ;; URGH, whether PE is elastic or gravitational can only be
	           ;; determined by type of body (spring or planet). .
		   ;; Also allow args in any order. Prefer grav if they include 
		   ;; planet. 
		   (cond ((planetp body2-term) 
			  `(grav-energy ,body-term :time ,time-term))
			 ((planetp body-term) 
			  `(grav-energy ,body2-term :time ,time-term))
			 ((springp body2-term)
			  `(spring-energy ,body-term :time ,time-term))
			 ((springp body-term)
			  `(spring-energy ,body2-term :time ,time-term))
			 (T		; no planet or spring! Use grav, it always exists.
					; include both terms so error handlers can detect
			  `(grav-energy (,body-term ,body2-term :time ,time-term))))
		   )))
      (coef-friction			; !!!student has to get body args in right order:
       (if (null subtype) `(coef-friction ,body-term ,body2-term static)
	 `(coef-friction ,body-term ,body2-term ,subtype)))
      (voltage    (case subtype
		    (across		; body-term may come here as (resistance (a b c)) for equiv.
		     (if (atom body-term) `(voltage-across ,body-term :time ,time-term)
                       `(voltage-across ,(second body-term) :time ,time-term)))
		    (otherwise		; no longer used
		     `(voltage-btwn ,body-term ,body2-term :time ,time-term))))
      (current     (case subtype
		     (through		; now body-term always comes as '(compound orderless A B C)
		      (if (atom body-term) `(current-thru ,body-term :time  ,time-term)
		         ; used to be arg might come here (resistance (a b c)) for current through student-defined 
		         ; equivalent Rabc. Can't happen any more since revised current dialog no longer includes named
			 ; equivalents as choices.
		          `(current-thru ,(find-closest-current-list (bodyterm-complist body-term)) :time ,time-term)))
		     (in        `(current-in ,body-term :time ,time-term))))
      (current-change `(rate-of-change (current-thru ,(find-closest-current-list (list body-term) 'current-change)
                                          :time ,time-term)))  ; NB: body-term always comes as atom from old dialog
      (resistance			; body-term may come here as (compound a b c) for equivalent resistance
       (if (atom body-term) `(resistance ,body-term)
	 `(resistance ,(bodyterm-complist body-term))))
      (capacitance			; body-term may come here as (compound a b c) for equivalent capacitance
       (if (atom body-term) `(capacitance,body-term)
	 `(capacitance ,(bodyterm-complist body-term))))
      
      ;;
      ;; vector quantities -- form appropriate prop (mag or dir or compo)
      ;;
      ((relative-position position)	; include workbench type id to be safe
       (vec-prop prop `(relative-position ,body-term ,body2-term
			   :time ,time-term)))
      (relative-vel (vec-prop prop `(relative-vel ,body-term ,body2-term
					:time ,time-term)))
      (E-field      (vec-prop prop 
			      (if (or (null body2-term) (string-equal body2-term '|all sources|))
				  `(net-field ,body-term electric :time ,time-term)
				`(field ,body-term electric ,body2-term :time ,time-term))))
      (B-field      (vec-prop prop 
			      (if (or (null body2-term) (string-equal body2-term '|all sources|))
				  `(net-field ,body-term magnetic :time ,time-term)
				`(field ,body-term magnetic ,body2-term :time ,time-term))))
      (displacement (vec-prop prop `(displacement ,body-term :time ,time-term)))
      (velocity     (vec-prop prop `(velocity ,body-term :time ,time-term)))
      ((accel acceleration)		; include workbench type id to be safe
       (vec-prop prop `(accel ,body-term :time ,time-term)))
      (force        (vec-prop prop 
			      (if (eq subtype 'Net) 
				  `(net-force ,body-term :time ,time-term)
				`(force ,body-term ,body2-term ,subtype :time ,time-term))))
      (momentum     (vec-prop prop `(momentum ,body-term :time ,time-term)))
      (impulse      (vec-prop prop `(impulse ,body-term ,body2-term :time ,time-term)))
      ((ang-accel ang-acceleration)	; include workbench type id to be safe
       (vec-prop prop `(ang-accel ,body-term :time ,time-term)))
      (ang-velocity (vec-prop prop `(ang-velocity ,body-term :time ,time-term)))
      (ang-displacement 
       (vec-prop prop `(ang-displacement ,body-term :time ,time-term)))
      (ang-momentum (vec-prop prop `(ang-momentum ,body-term :time ,time-term)))
      (torque       (vec-prop prop 
			      (cond ((eq subtype 'net) 
				     `(net-torque ,body-term ,body2-term 
						  :time ,time-term))
				    ((eq subtype 'couple)
				     `(torque ,body-term
					      (couple orderless ,body-term 
						      ,body2-term) 
					      :time ,time-term))
				    ((eq subtype 'dipole)
				     (set-time (find-dipole-torque-term 
						body-term body2-term) 
					       time-term))
				    (t (set-time (find-torque-term 
						  body-term body2-term) 
						 time-term)))))
      (mag-dipole  (vec-prop prop `(dipole-moment ,body-term magnetic :time ,time-term)))
      (elec-dipole (vec-prop prop `(dipole-moment ,body-term electric :time ,time-term)))
      ; unit-normal, unit-from, unit-towards: assume these will never be soughts so will
      ; never appear on quantity choice menu.
      ;; unknown:
      (otherwise   (format T "~&Warning!! unknown type to make-quant: ~A~%" 
			   quant-type)
		   ;; what the hey, give it our best shot:
		   (set-time
			    (if body2-term `(,quant-type ,body-term ,body2-term) 
			      `(,quant-type ,body-term)) time-term))
      )))

(defun find-dipole-field (body type)
"given dipole name and field type, return term for field dipole is in in this problem, if any"
  ; temporary hack: point is always 'region, source is always 'unspecified
  ; could just do a find via unify over all valid dipole-energy quants in variable index 
  ; still need to construct something if this is problem without dipole-energy, though.
  `(field region ,type unspecified)
)

(defun ndiffs (set1 set2)
  (length (set-difference set1 set2)))
 
;; For (current-thru ?arg ...) and (rate-of-change (current-thru ?arg ...)), the student
;; arg is a set of branch components which may be a subset of the full set used in the kb
;; quantity form. Normally the kb list is maximal (all components in a branch), but it
;; may be smaller in an introductory problem where we use explicit equality of current 
;; through point A and point B in the same branch. 
;; Following expands the student's arg set to the closest matching arg set among the defined
;; variables. Quant may be 'current-thru or 'current-change
(defun find-closest-current-list (studset &optional (quant 'current-thru))
"return current variable argument list that best matches comps listed in student definition"
  (let ((quantform (if (eq quant 'current-thru) '(current-thru ?comp :time ?t)
                     '(rate-of-change (current-thru ?comp :time ?t))))
        bestset 	; initially NIL for empty set
        bestarg)	
   ; do for each current variable definition
   (dolist (cdefprop (sg-fetch-entry-props `(define-var ,quantform)))
      (let* ((sysarg (if (eq quant 'current-thru) (second (second cdefprop))
                       (second (second (second cdefprop)))))
	     ; urgh, kb uses atomic args in some cases
             (sysset (if (atom sysarg) (list sysarg) sysarg)))
	; update best if this has fewer diffs than best match so far
        (when (and (subsetp studset sysset) ; a match!
	           (or (null bestset) ; first match found
	               ; or closer match than best so far 
	               (< (ndiffs sysset studset)
		          (ndiffs bestset studset)))
	    (setf bestset sysset) 
	    (setf bestarg sysarg)))))

   ; finally: return best arg or student's list if no match found
   (or bestarg studset)))

;;
;; Answer box identifiers
;;
;; In Andes1 problems, the ids on answer boxes were of form Answer-CLIPSVAR
;; where CLIPSVAR specified the relevant quantity by giving its variable in
;; the CLIPS solutions. This id was embedded in the .fbd problem file loaded
;; by the workbench. In order to handle messages from these old files without
;; change, this routine breaks out the CLIPSVAR and returns the Andes2 KB 
;; quantity expression for it. All the work is in functions in clips-vars.cl.
;;
;; In future Andes2 problems, we expect the ids to be of the form Answer-N,
;; where N is the 1-based ordinal of the quantity in the problem soughts.
;; So we prepare for this case as well.
(defun get-answer-quant (answer-id)
  (let* ((id-str (string answer-id))
         (pos    (position #\- id-str))
         (quant-id-part  (if pos (subseq id-str (1+ pos))
	                    id-str))
	 (quant-id  (read-from-string quant-id-part)))
    (if (numberp quant-id) 
        (nth (1- quant-id) (problem-soughts *cp*))
      (clips-var->quant quant-id))))





;;-----------------------------------------------------------------------------
;; Workbench Entry API Handler functions
;;-----------------------------------------------------------------------------

;;
;; Following entry handling calls are ordered in roughly the order the student 
;; should follow: First choose body, then draw vectors and other diagram 
;; entries, maybe define some variables, draw axes, maybe draw components, 
;; then write equations, solve for desired variable and enter answer. 
;;

;; Note on entry ids and symbol table manipulations:
;;
;; The workbench allows the student to modify an existing entry by editing 
;; its properties in a dialog. In this case the workbench sends the same entry 
;; id for the revised submission as it did with the first. This edit can 
;; modify any property of the entry including the label.  The effect should 
;; be just the same as if the student had deleted the existing entry and 
;; then submitted a new one with the same id.
;;
;; To handle this possibility we have to be sure to delete any existing entry 
;; and undo all its effects on our state (on the symbol table entries, e.g.) 
;; before updating our state with the new entry. This is now handled 
;; automatically by the entry-list manager in the add-entry call. This call
;; will remove any existing entry with the given id, and this will call back 
;; to our cleanup routine "undo-entry" to do the work of undoing its 
;; effects, e.g. by calling symbols-delete-dependents to remove symbol
;; table entries dependent on the earlier entry contents.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-object -- checks the correctness of a student defined single body
;; argument(s):
;;  label:  the label of the body
;;  name(s): the name the body(s) was assigned in the problem description
;;  time:  for backward compatability
;;  id: is assigned to this object by the work-bench
;; returns: StudentEntry
;; note(s):
;;  Defines a mass variable whose name has "m" concatenated to the given label.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-assert-object (label name &optional time id xpos ypos)
  (let* ((body-term (arg-to-body name))
	 (time-term (arg-to-time time))
	 (action   `(body ,body-term :time ,time-term))
	 (entry     (make-StudentEntry :id id :prop action))
	 ;; this entry automatically defines a mass variable named m+label
	 ;; we build an implicit entry for this to mark it done as well.
         (mass-label  (concatenate 'string "m" label))
	 (mass-term `(mass ,body-term))
	 (mass-var-entry (make-StudentEntry :id id 
	                                    :prop `(define-var ,mass-term))))

  ;; associate implicit entry
  ;(add-implicit-eqn entry mass-var-entry) ; take out automass

  (add-entry entry)   ;remove existing info and update
  ;(symbols-enter mass-label mass-term id) ; take out automass
  ;; for compound bodies, enter body label so it can be recognized when
  ;; referenced in subsequent quantity definitions for compound's attributes.
  (when (compound-bodyp body-term)
  	(symbols-enter label body-term id))
  entry))  ;finally return entry 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-compound-object - checks the correctness of a student defined com-
;;  pound body
;; argument(s):
;;  label: the label of the body
;;  name(s): list of names as symbols of simple bodies making up the compound. 
;; Each is KB name for that body.
;;  time:  for backward compatability
;;  id: is assigned to this object by the work-bench
;; returns: Studententry 
;; note(s):
;;  Defines a mass variable whose name has "m" concatenated to the given label.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-assert-compound-object (label names &optional time id)
      ;; can just pass along to assert-object. arg-to-body knows how to make
      ;; compound body term out of list argument.
      (on-assert-object label names time id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-vector -- check the correctness of a vector drawn by the student. May
;;  be any vector type except force
;; argument(s):
;;  label: the vector label
;;  avg-inst: is the vector an average or instantanepus quantity? value is
;;    either 'average or 'instantaneous
;;  type: the type of vector: velocity, acceleration, displacement, etc.
;;  system: the body that is moving. May be system label (student defined) or
;;    body name (given)
;;  dir: angle of the motion vector from horizontal (0->360 degrees) or a nega-
;;    tive number coding a z-axiz direction as follows (-1->out of plane; -2
;;    is into plane; -3 unknown but along z axis
;;  mag: magnitude of the vector or nil if unspecified
;;  time: the time period during which the vector is constant. 
;;    if nil and system is a student defined system, the time will be 
;;    taken from the system definition
;;  id: id assigned to vector by the workbench
;; returns: StudentEntry
;; note(s):
;;  if the vector is correct, the help system marks the corresponding system
;;  entry as "entered", defines the magnitude and direction variables, and
;;  enters the variables in the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-lookup-vector (label avg-inst type system dir mag &optional time id)
 (let* ((vtype        (arg-to-id **vector-types** type))
        (body-term    (arg-to-body system))
        (time-term    (arg-to-time time))
        ;; avg-inst choice is redundant with time -- WB ensures consistency
	;; However, for vector types with two arguments, arg carries 
	;; the second body argument 
	;; also, field quant forms are odd in that it includes field type slot
        (vquant-term (cond 
	               ((equal vtype 'E-field) 
			    (if (null avg-inst) ;null agent => net-field
			          `(net-field ,body-term electric)
		             `(field ,body-term electric ,(arg-to-body avg-inst))))
                        ((equal vtype 'B-field) 
			    (if (null avg-inst) ;null agent => net-field
			          `(net-field ,body-term magnetic)
		             `(field ,body-term magnetic ,(arg-to-body avg-inst))))
			; WB unit vector type codes pack in kb subtype argument:
			((equal vtype 'unit-Normal) 
			     `(unit-vector normal-to ,body-term :at NIL))
			((equal vtype 'unit-towards)
			     `(unit-vector towards ,(arg-to-body avg-inst) :at ,body-term))
			((equal vtype 'unit-away-from)
			     `(unit-vector away-from ,(arg-to-body avg-inst) :at ,body-term))
	                ;; a two-argument vector type:
	                ((or (equal vtype 'relative-position) (equal vtype 'relative-vel)
			     (equal vtype 'impulse) (equal vtype 'dipole-moment))
		           `(,vtype ,body-term ,(arg-to-body avg-inst)))
		        ;; else single argument vector: 
	                (T `(,vtype ,body-term))))
	(dir-term (arg-to-dir dir mag)))

    (make-vector-entry label vquant-term time-term dir-term id)))

;; worker routine to do generic tasks common to vector entries 
;; once the vector quantity has been formed
(defun make-vector-entry (label vquant-term time-term dir-term id)
   (let* ((vector-term (append vquant-term `(:time ,time-term)))
          (action      `(vector ,vector-term ,dir-term))
          (entry        (make-StudentEntry :id id :prop action))
	  ;; this defines magnitude and direction variables
	  (vector-mag-term `(mag ,vector-term))
	  (vector-dir-term `(dir ,vector-term))
	  ; xy plane vectors get theta prefix, z axis ones get phi
	  ; Greek symbols expressed by $ + char position in symbol font
	  (dir-label  (format NIL "~A~A" (if (z-dir-spec dir-term) "$j" "$q")
                                        label)))
    ; remove existing entry and update
    (add-entry entry)
    (symbols-enter label vector-mag-term id)
    (symbols-enter dir-label vector-dir-term id)

    ;; if any axes are defined must add all component variables as well
    (dolist (axis-sym (symbols-fetch '(axis ?xyz ?dir)))
      (let* ((axis-label (sym-label axis-sym))
             (axis-term  (sym-referent axis-sym))
	     (axis-entry-id (first (sym-entries axis-sym)))
             (compo-var  (format NIL "~A_~A" label axis-label))
	     (compo-term (vector-compo vector-term axis-term)) ; Physics-Funcs
	    )
        (symbols-enter compo-var compo-term (list id axis-entry-id))))

    ; if vector is zero-length, associate implicit equation magV = 0
    ; also add component eqns vc = 0 for all component variables in solution.
    (when (equal dir-term 'zero)
       (add-implicit-eqn entry (make-implicit-assignment-entry label 0))
       (dolist (syscomp (get-soln-compo-vars vector-term))
            ; skip make-implicit-assignment-entry since we have sysvar, not studvar
            (add-implicit-eqn entry (make-implicit-eqn-entry `(= ,syscomp 0)))))
 
    ; if vector is a unit vector, associate equation magV = 1 
    ; !!! We are stuffing this into the entry's associated "given" equation list, 
    ; because there is currently only one implicit equation slot for the entry 
    ; and that might be needed for direction. Should clean this up, perhaps use one
    ; list for both sorts of associated equation entries.
    (when (eq (first vector-term) 'unit-vector)
        (add-implicit-eqn entry (make-implicit-assignment-entry label 1)))
    ; if direction is known, associate implicit equation dirV = dir deg.
    (when (degree-specifierp dir-term)          ; known xy plane direction
       (add-implicit-eqn entry (make-implicit-assignment-entry dir-label dir-term)))
    (when (and (z-dir-spec dir-term) 
               (not (equal dir-term 'z-unknown))) ; known z axis direction
       (add-implicit-eqn entry (make-implicit-assignment-entry dir-label (zdir-phi dir-term))))
   
    ; Associated eqns will be entered later if entry is found correct.

    ; finally return entry
    entry))

; fetch list of system vars denoting components of vector term
(defun get-soln-compo-vars (vector-term)
  (let ((compo-pattern (vector-compo vector-term '(axis ?xyz ?rot))))
   (mapcar #'qvar-var
     (remove-if-not #'(lambda (qvar) 
                           (unify (qvar-exp qvar) compo-pattern))
                    (problem-varIndex *cp*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-line -- check the correctness of a line drawn by the student.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-lookup-line (label body-arg dir mag &optional time id)
  (let* ((body-term (arg-to-body body-arg))
	 (time-term (arg-to-time time))
	 ;; note dir may be dnum or 'unknown (and maybe into/out-of)
	 (dir-term (arg-to-dir dir mag 180)) ;lines defined mod 180 deg
	 (line-term `(line ,body-term :time ,time-term))
	 (action `(draw-line ,line-term ,dir-term)) 
	 (entry (make-StudentEntry :id id :prop action))
	 ;; this defines magnitude and direction variables
	 (line-mag-term `(mag ,line-term))
	 (line-dir-term `(dir ,line-term))
	 ;; xy plane lines get theta prefix, z axis ones get phi
	 ;; Greek symbols expressed by $ + char position in symbol font
	 (dir-label  (format NIL "~A~A" (if (z-dir-spec dir-term) "$j" "$q")
			     label)))
    ;; remove existing entry and update
    (add-entry entry)
    (symbols-enter label line-mag-term id)
    (symbols-enter dir-label line-dir-term id)
    
    ;; if direction is known, associate implicit equation dirV = dir deg.
    (when (degree-specifierp dir-term)          ; known xy plane direction
	 (add-implicit-eqn entry 
			   (make-implicit-assignment-entry dir-label dir-term)))
    (when (and (z-dir-spec dir-term) 
	       (not (equal dir-term 'z-unknown))) ; known z axis direction
      (add-implicit-eqn entry (make-implicit-assignment-entry dir-label (zdir-phi dir-term))))
    ;; Associated eqns will be entered later if entry is found correct.

    ;; Include implicit equation cos angle = dummy, where dummy is 
    ;; nonnegative.  This is associated with Snell's law, but it
    ;; is convenient to add this eqn when drawing the line.
    ;; Treat this equation as entered by the student so the solve tool can 
    ;; solve student's system the same way as at sgg time.
    (dolist (eqn (Problem-EqnIndex *cp*))
      (when (and (unify '(angle-constraint t orderless . ?quants) 
			(eqn-exp eqn)) (member line-term (eqn-exp eqn) 
					       :test #'unify))
	(add-implicit-eqn entry (make-implicit-eqn-entry (eqn-algebra eqn)))))
    
    ;; finally return entry
    entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-force - check correctness of a force vector drawn by the student
;; argument(s):
;;  label: the force label
;;  type: tension or grav or spring or friction ... a net force is indicated
;;   by putting NET here
;;  system: the label given to the body the force is acting on. may be either
;;   system label or body name
;;  agent:
;;   the label given to the body the force is exerted by. NIL if this is a
;;   net force
;;  dir:
;;   angle of the force vector from horizontal 0->360 degrees or a negative
;;   number coding a z-axis direction as follows -1 => out of page, -2 into
;;   page, -3 => unknown but along the z-axis
;;  mag:
;;   magnitude of the force (zero vs. non-zero) or nil if unspecified
;;  time:
;;   the time period during which the force is constant; if nil and system is
;;   a student-defined system, the time will be taken from the system definition
;;  id:
;;   id assigned to the force vector by the workbench
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;   if the force is correct then the help system marks the corresponding sys-
;;   tem entry as "entered" it also defines magnitude and direction variables
;;   for the force, and enters them into the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-lookup-force (label type system agent dir mag &optional time id)
   (let* ((body-term (arg-to-body system))
	  (time-term (arg-to-time time))
	  (agent-term (arg-to-body agent))
          (type-term (arg-to-id **force-types** type))
	  ; net force is special:
	  (vquant-term (if (eq type-term 'Net) `(net-force ,body-term)
	                 `(force ,body-term ,agent-term ,type-term)))
	  (dir-term (arg-to-dir dir mag)))

    (make-vector-entry label vquant-term time-term dir-term id)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-torque - check correctness of a torque vector drawn by student
;; Arguments:
;; label: the torque label
;; type: net, couple, or other.
;; body: Net torque: the label of the whole torqued body. 
;;       Individual torque: the point of application of the force
;; axis: dipole torque:  the agent of the field
;;       couple:  the other body
;;       the pt about which the rotation axis is located
;; dir: angle of the torque vector from horizontal: 0<=dir<360
;;       If the vector is out of the screen, value is -1, 
;;       if it is into the screen, value is -2
;; mag: magnitude of the torque or NIL if unspecified
;; time: the time period during which the torque is constant
;;       if the whole problem this can be NIL
;; id: numerical id assigned to the force vector by the workbench
;; 
;; Returns:  entry status return value
;; 
;; Side Effects: Updates state as for other vector entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-lookup-torque (label type body axis dir mag time id)
 (let* ((body-term   (arg-to-body body))
	(time-term   (arg-to-time time))
	(axis-term   (arg-to-body axis))
	;; net torque is easy. For individual torques, must find body 
	;; containing the point of application as part and also full term 
	;; for force applied at that point, assumed to be unique.  Note if 
	;; no such force we can't determine a full quantity spec at all.
	(vquant-term (cond
		      ((eq type '|Net|) `(net-torque ,body-term ,axis-term))
		      ((eq type '|Couple|) 
		       `(torque ,body-term 
				(couple orderless ,body-term ,axis-term)))
		      ((eq type '|Dipole|)
		       (find-dipole-torque-term body-term axis-term))
	              (t (find-torque-term body-term axis-term))))
	(dir-term (arg-to-dir dir mag)))

    (make-vector-entry label vquant-term time-term dir-term id))
)

(defun find-torque-term (pt axis)
  "fill in fully explicit torque term implicitly defined by point of force application and axis"
  (let* (
	 ;; get force by searching entries for force at pt of application
	 (force-matches (sg-fetch-entry-props 
			 `(vector (force ,pt ?agent ?type :time ?t) ?dir)))
	 (force-match   (if (= (length force-matches) 1)
			    (remove-time (second (first force-matches)))
	                  `(force ,pt unknown unknown)))
	 ;; get main body by searching problem givens for point-on-body
	 (body-matches (filter-expressions `(point-on-body ,pt ?b) 
					   (problem-givens *cp*)))
	 (body-match   (if (= (length body-matches) 1) (car body-matches)))
	 (body-term    (if body-match (third body-match) 'unknown)))
    ;; return the torque quantity
    `(torque ,body-term ,force-match :axis ,axis) 
    ))

(defun find-dipole-torque-term (dipole field-name)
  ;; second arg is student's label for field. Find in symbol table
  ;; referent should be (mag (field ... ... ... :time ...))
  (let* ((field-match (symbols-referent (string field-name)))
         (field-term  (if field-match (remove-time (second field-match))
	               ; else referent not found! shouldn't happen
	                  `(field nil nil nil))))
    ;; return the torque quantity
    `(torque ,dipole ,field-term) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; label-angle -- assigns the given label to the angle between two objects with
;;  given degrees size
;; argument(s):
;;  label: the label given the angle by the student
;;  degrees: the size of the angle
;;  id-vector1: id of one vector forming the angle (may refer to an axis)
;;  id vector2: id of other vector forming angle (may refer to axis if previos
;;    does not
;;  id-angle: the id of the new angle object
;;  axis: the part of coordinate system that is used (posx, negx, posy, negy)
;; returns: StudentEntry
;; note(s):
;;  adds angle label to the list of variables
;; The id arguments are workbench-generated ids of the *entries* that drew
;; the relevant objects -- vectors or coordinate systems. The complete 
;; specification of an axis includes the coordinate system id plus the 
;; code posx, posy etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-label-angle (label degrees id-vector1 id-vector2 id-angle &optional axis)
  ; if angle between vectors then have to make entry and check for match.
  ; if angle between vector and axis, then just install it in symbol table
  ; as label for appropriate angle expression.  Dispatch based on angle type:
  (if axis (on-angle-with-axis label degrees id-vector1 id-vector2 
                               id-angle axis)
    (on-angle-between-vectors label degrees id-vector1 id-vector2 id-angle :drawn T))
)

;; Worker routine to handle entry of term for angle between vectors.  
;; "Drawn" argument specifies if drawn as diagram entry or defined as variable
;;  Either way this should match a solution graph entry.
(defun on-angle-between-vectors (label degrees id-vector1 id-vector2 id-angle &key drawn) 
   (declare (ignore drawn)) ; AW: no longer used
   ;; need to map entry id to referent vectors. Note this may now be used for 
   ;; drawn line entries as well.  Code should work without change, because 
   ;; on-lookup-line enters line label as symbol for (mag (line ...)) 
   ;; exactly as for vectors.
   ;; Take second to extract time-indexed vector quant terms from (mag ?vector)
   (let* ((v1-term (second (symbols-entry-referent '(mag ?vector) id-vector1)))
          (v2-term (second (symbols-entry-referent '(mag ?vector) id-vector2)))
	  (angle-term `(angle-between orderless ,v1-term ,v2-term))
	  (action `(define-var ,angle-term))
          (entry (make-StudentEntry :id id-angle :prop action))
	 )

   ; delete existing entry info and update
   (add-entry entry)
   (symbols-enter label angle-term id-angle)
   ; add implicit equation for variable if value known. This means they
   ; don't have to enter equation giving the value. Interface does
   ; show them the value on the dialog box.
    (when (numberp degrees)          ; known xy plane direction
       (add-implicit-eqn entry (make-implicit-assignment-entry label `(dnum ,degrees |deg|))))
 
    ;; The following is for angle between lines. 
    ;; Safe for vectors because we won't find angle-constraint in solution.
    ;; Include implicit equation cos angle = dummy, where dummy is 
    ;; nonnegative.  This is associated with Snell's law, etc., but it
    ;; is convenient to add this eqn when defining the associated quantity.
    ;; Treat this equation as entered by the student so the solve tool can 
    ;; solve student's system the same way as at sgg time.
    (dolist (eqn (Problem-EqnIndex *cp*))
      (when (unify `(angle-constraint nil orderless ,v1-term ,v2-term) 
		   (eqn-exp eqn))
	(add-implicit-eqn entry (make-implicit-eqn-entry (eqn-algebra eqn)))))

   ;; finally return entry
   entry))

; Worker routine to handle labelling angle made with an axis. No matching entry ; in solution graph so doesn't 
; matter whether angle was drawn or defined as a variable
(defun on-angle-with-axis (label degrees id-vector1 id-vector2 id-angle axis) 
  (declare (ignore id-vector1 id-vector2 axis))
  ; if degree value known, just treat label as term for degree value
  ; could also add implicit equation for variable if value known. But should be
  ; unnecessary if it is always mapped to dnum in equations sent to algebra.
  ; Note label is entered in symbol table with appropriate workbench entry id 
  ; so later deletions remove it, but no studententry is created for this -- OK?
  (when degrees
    (symbols-delete-dependents id-angle)
    (symbols-enter label `(dnum ,degrees |deg|) id-angle `(dnum ,degrees |deg|))
    ; return T to indicate always correct
    (return-from on-angle-with-axis T))

  ; else degree value unknown -- what to do? Could enter as shorthand for 
  ; complex term in terms of angle of vector and known orientation of axis.
  ;  theta = Ov - (x-axis-dir + axis adjustment)
  ; Need to handle order of arguments as well, angle always counterclockwise
  ; from first arg to second.  This facility is mainly here to handle angle 
  ; made with positive x axis, though; may restrict it to that in the future.
  ; For now we just don't handle this case. 
  ; ! Without saving an entry struct there's no good way to hang a whatswrong 
  ; on this. Returning string sends a message to the workbench while leaving 
  ; the entry colored red as if an error occurred.
  (strcat "Variables for unknown angles are not supported. Use the "
	  "automatically defined $q variables for orientation angles "
	  "instead. To ask Andes to solve-for a $q variable, right-click "
	  "on its name in the variable window and select Solve-For.")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-angle-variable: undocumented special Andes1 API used for defining 
;;                        angle variable without drawing it
;;
;; In this API, the label arguments are student *labels* for the vectors or 
;; special symbols posx, posy, etc if one of the arguments is part of the
;; one and only one coordinate axis.  No provision for multiple axes. 
;; Don't ask why these arguments are different than those of label-angle
;; Note: In Andes1, the label arguments were sent by workbench as naked 
;; symbols so got upcased when command string passed through read. This 
;; is bad for case-sensitive Andes2 symbols, so changed workbench in 
;; Andes 6.0.3 workbench to wrap args in vbars. So now use case-insensitive
;; sym-match test to check for axis id symbols with lower case!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun axis-codep (arg) ; arg should be symbol
  (member arg '(posx posy negx negy) :test #'sym-match))

(defun on-define-angle-variable (label degrees label1 label2 id-angle)
 ; lookup syminfo (si) to convert label args to entry id args used by 
 ; label-angle worker routines
 (let*  ((si1 (if (axis-codep label1) 
                    (first (symbols-fetch '(axis x ?dir))) ; should be only one
                (symbols-lookup (string label1))))
	 (id1 (if si1 (first (sym-entries si1))))
	 (si2 (if (axis-codep label2) 
                    (first (symbols-fetch '(axis x ?dir))) ; should be only one
                (symbols-lookup (string label2))))
	 (id2 (if si2 (first (sym-entries si2))))
	 (axis (first (or (axis-codep label1) (axis-codep label2)))))
    ; then delegate to appropriate hander
    (if (not axis) ; angle between vectors: must specify not drawn for match
        (on-angle-between-vectors label degrees id1 id2 id-angle :drawn NIL) 
     (on-angle-with-axis label degrees id1 id2 id-angle axis))))

; helper for wb-quant -- get angle between quantity from two student labels
(defun make-angle-quant (label1 label2)
   ; take second because referent is (mag (?vector-type . ?args))
   ; works for vectors AND lines because lines register as (mag (line...))
   (let* ((v1-term (second (symbols-referent (string label1))))
	  (v2-term (second (symbols-referent (string label2)))))
     `(angle-between orderless ,v1-term ,v2-term)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; label-radius -- label radius of revolution of an object moving in a circle
;; argument(s):
;;  label: the label given to the radius by the student
;;  id: the id of the radius object
;;  name: the body that is moving in a circle with this radius
;; returns: StudentEntry
;; note(s):
;;  add a variable for the radius to the list of variables
;;
;; This API is called when a revolution-radius is drawn on the diagram 
;; rather than defined in the variable window. Since we don't care about
;; the difference, we just translate it into a define-variable call. 
;; [Workbench could be changed to do this just as well, but this is simpler.]
;; !!! Note Workbench API for this quant doesn't include a time -- so really
;; only usable if default time on steady-state problem is OK. That should 
;; generally happen since uniform circular motion is a steady state. However,
;; we could be interested in uniform circular motion before and after coupling,
;; in a conservation of angular momentum problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-label-radius (label id name)
  (on-define-variable label NIL 'revolution-radius name 'T0 NIL id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-variable - define a variable to stand for a certain quantity. this is
;;  called when the student uses the "variable" menu, but not when variables are
;;  defined by other tools such as the force drawing tool or the body tool.
;; argument(s):
;;  var: the variable assigned to the object
;;  type: depends on the value of quant (below) --
;;    if quant is "force" the type of force
;;    if quant is "energy" the type of energy
;;        (one of 'total, 'spring, 'potential, 'kinetic)
;;    if quant is velocity, acceleration, speed either average or instantaneous
;;    otherwise nil
;;  quant: the type of quantity. one of the following
;;    velocity, acceleration, force, displacement, distance between, distance
;;    travelled, gravitational acceleration, duration, speed, radius,
;;    spring-constant, comp-dist, energy, mass
;;  body: the body the quantity is a property of
;;  time: the time during which the quantity exists
;;  agent: the agent of the force, if it is a force; otherwise nil
;;  id: the id assigned to the variable by workbench
;;  directionp: WAS: t if the quantity is a direction; nil otherwise
;; returns: StudentEntry
;; note(s):
;;  if the variable definition is correct, marks the corresponding system entry
;;  as "entered" and enters the student's variable name into the symbol table
;;  paired with the corresponding system variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-define-variable (var type quant body time body2 id &optional value)
 (let* ((quant-term (make-quant type quant body body2 time))
	(action    `(define-var ,quant-term))
	(entry      (make-StudentEntry :id id :prop action)))

  ; install new variable in symbol table
  (add-entry entry)
  (symbols-enter var quant-term id)

  ; record associated given value equation entry
  (when value  ; NIL => unspecified. (use empty string => unknown)
    (let ((given-eqn (make-given-eqn-entry var value)))
    ; NB! make-given-eqn-entry returns NIL if no system var found for studvar
    ; Should mean bad var def in this case. But maybe better to change to always
    ; have a dangling given eqn entry in this case?
    (when given-eqn
       (setf (studentEntry-GivenEqns entry) (list given-eqn)))))

  ; finally return entry 
  entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-x-axis - check correctness of coordinate axis drawn by student
;; argument(s):
;;  body: the label given to the body the axis applies to
;;  dir: the angle of the x-axis form horizontal (0 -> 360)
;;  id: is assigned to the object by the workbench
;;  x-label: label given to x axis by the student
;;  y-label: label given to y axis by the student
;; returns: StudentEntry
;; note(s):
;;  adds x and y axes to (student entries) -- asserts observed to assessor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-assert-x-axis (body dir
		      &optional id (x-label "x") (y-label "y") (z-label "z"))
  (declare (ignore body))
  ;; workbench doesn't associate axes with bodies yet, so we leave this
  ;; out of the entry proposition we match (see kb/ontology.cl).
  ;; !! need to make sure axis between zero and 90 and canonicalize if
  ;; not
  (let* ((action  `(draw-axes ,dir)) ; dir is naked degree value
	 (entry    (make-StudentEntry :id id :prop action))
	 (x-term  `(axis x ,dir))
	 (y-term  `(axis y ,dir))
	 (z-term  `(axis z ,dir))
	 (xdir-dnum `(dnum ,dir |deg|))
	)

   ;; install symbols for x, y, and z axes
   ;; these can't be used by themselves in equations but are needed by us
   ;; later when autodefining vector component variables for existing axes. 
   ;; They would also be needed for referring to the axes by label in help 
   ;; messages if there is more than one set of axes.
   (add-entry entry)
   (symbols-enter x-label x-term id)
   (symbols-enter y-label y-term id)
   (symbols-enter z-label z-term id) 

   ;; automatically define thetaX as label for direction of positive x-axis
   (symbols-enter (strcat "$q" x-label) xdir-dnum id xdir-dnum)

  ;; if any vectors are defined add all component variables along 
  ;; these new axes as well
  (dolist (vector-sym (symbols-fetch '(mag ?vector)))
    (dolist (axis-label (list x-label y-label z-label))
      (let* ((vector-label (sym-label vector-sym))
             (mag-term     (sym-referent vector-sym))
	     (vector-entry-id (first (sym-entries vector-sym)))
	     ;; need time-indexed vector quant term from (mag ?vector)
	     (vector-term (second mag-term))
             (compo-var    (format NIL "~A_~A" vector-label axis-label))
	     (axis-term    (symbols-referent axis-label))
	     (compo-term   (vector-compo vector-term axis-term)) ; in Physics-Funcs
	    )
        (symbols-enter compo-var compo-term (list id vector-entry-id)))))

  ; finally return entry
  entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-component - check a student drawn vector component
;; argument(s):
;;  label: the label of this component (should be the vector label w/subscript
;;    of axis label
;;  compo-of: the label given to the vector this is a component of (this means
;;    that a component can not be drawn before its vector
;;  axis: the label of the axis of the projection ('x or 'y)
;;  id: id assigned by the workbench
;;  dir: direction (degrees from horizontal right) in which the vector is
;;   pointing
;; returns: StudentEntry
;; note(s):
;;  if the component is correct, marks the corresponding system entry as "en-
;;  tered", figures out which algebraic function of the system's component var-
;;  iable corresponds to this variable given the student's axis rotation, and
;;  enters into the symbol table a pairing consisting of that expression and the
;;  student's variable name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-lookup-component (label compo-of axis mag &optional dir id)
  (declare (ignore label compo-of axis mag dir id))
  ; We don't draw vector components in our solutions. Instructors have been
  ; discouraging the drawing of components as well, and there are deficiencies
  ; in the current component dialog box: 1. it has no direction slot, but 
  ; direction may not be exactly drawn.
  ; 2. it allows student to introduce any non-standard name other than
  ; F_x for the component variable, which we is also discouraged and should
  ; be disallowed in the future.
  ; In fact all checking of components could in theory be done by workbench, 
  ; since drawing a component requires vector and axis to be drawn, so only 
  ; real mistake possible is in direction, which workbench could catch. 
  ; It is also possible to draw a component not needed for the solution, though,
  ; so we might check that.
   (make-dialog-turn "Your instructor recommends you not draw vector components on your diagram to avoid confusion. You can refer to vector components in equations using the subscript notation." NIL)
  )


;-----------------------------------------------------------------------------
;; for processing deletions of entries
;-----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-object -- delete the student defined object with the given label and
;;  or id from the student entries list
;; argument(s):
;;  label: the label given to the object by the student
;;  id: the unique is assigned to the object by the workbench
;; returns: garbage
;; note(s):
;;  marks the corresponding system entries as "unentered" If the object involves
;;  a variable, the removes that variable from the symbol table.
;; Note that elsewhere label arguments are passsed as quoted strings, so
;; case is preserved, but in this API argument is sent as naked symbol -- 
;; so gets upcased when command string is read. 
;; Happily we should be able to ignore it and rely on entry id.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-delete-object (label &optional id)
  (declare (ignore label))
  ;; Remove from entry list. This function knows to call back to undo-entry 
  (remove-entry id) 
)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-equation - deletes a student equation from the student-entries list
;; argument(s):
;;  id: the id of the equation being deleted. Integer id starts at 0 for first
;;    equation box.
;; returns: garbage
;; note(s):
;;  removes this student entry from the entered-by fields of each of the cor-
;;  responding system equations
;;
;; Believe this API is obsolete and will never be called. Effect is now 
;; achieved by submitting an empty equation string. Here we translate 
;; to that just in case it ever arises.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun on-delete-equation (id)
   (lookup-eqn-string "" id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo-entry -- undo all state effects of a particular Student entry
;; Argument: enty -- the StudentEntry to be undone.
;;
;; Note: This worker routine is automatically invoked by the remove-entry 
;; function to cleanup state on removal of an entry from the entry list -- 
;; a bit like a C++ destructor for student entries.  That ensures that
;; entry effects are always undone when a student entry is removed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun undo-entry (entry)
  ; undo entry effects specific to correct entries:
  (when (equal (StudentEntry-state entry) 'Correct)
	; unmark entry interpretations as done in solution graph
  	(sg-delete-StudentEntry entry)
        ; undo any implicit eqn entry associated with this
  	(dolist (ie (StudentEntry-ImplicitEqns entry))
           (undo-entry ie))
        ; undo any given eqn entry associated with this
	(dolist (ge (StudentEntry-GivenEqns entry))
	  (when (not (blank-given-value-entry ge))
	    (undo-entry ge))))

  ; remove all labels dependent on it from symbol table
  (symbols-delete-dependents (StudentEntry-ID entry))

  ; special to equation entries: remove from algebra system
  (when (or (equal (first (StudentEntry-prop entry)) 'eqn)
            (equal (first (StudentEntry-prop entry)) 'implicit-eqn))
	(undo-eqn-entry entry))
)



;;----------------------- Checking Entries ----------------------------------
;; 
;; Following two functions implement to the non-eqn portion of the
;; EntryInterpreter. EntryInterpreter for equations is in another file

;;
;; Check-NonEq-Entry -- Generic checker for non-equation student entry 
;; Returns: tutor turn
;;
;; Note, could be done differently with a cond or two.
(defun Check-NonEq-Entry (entry)
  
  ;; Special Case: Entry-API handler has failed to build an entry for some 
  ;; reason and wants to send back a message.  If the entry is a string then
  ;; we will produce and return a color-red show-hint turn and return the
  ;; message with no menu.
  (when (stringp Entry)
    (return-from Check-NonEq-Entry (make-red-turn entry)))
      
  ;; special case: Entry-API handler can return T or NIL rather than an entry
  ;; when the status is determined without looking for a system entry. This
  ;; happens for some but not all angle entries.  Return appropriate turn.
  (when (eq entry NIL) 
    (return-from Check-NonEq-Entry (make-red-turn)))
  (when (eq entry T) 
    (return-from Check-NonEq-Entry (make-green-turn)))
    
  ;; else have a real student entry to check
  (format T "~&Checking entry: ~S~%" (StudentEntry-prop entry))
  (let (cand		; candidate (state . interpretation) pair
        match 		; correct system entry matched
        result) 	; final result to return

    ;; Get set of candidate interpretations into PossibleCInterps.  
    ;; For generality needed for equation entries, an "interpretation" 
    ;; is a list of primitive system entries -- steps -- effected by this 
    ;; student entry.  Each *candidate* interpretation is a pair whose car 
    ;; is the status [CORRECT, DEAD-PATH, PREMATURE, FORBIDDEN etc.] and 
    ;; whose cdr is an interpretation = sysent list, thus candidate list is 
    ;; of form ((PREMATURE sysent1 sysent2) (DEAD-PATH sysent2 sysent3) ...)
    ;; For non-equations, we should always get singleton set of candidates
    ;; or NIL, so we always just use the first one or mark incorrect if none.
    ;; Interp itself should contain exactly one sysent.
    (sg-match-StudentEntry Entry)
    (unless  (setf cand (first (StudentEntry-PossibleCInterps Entry))) 
        (format T "No matching system entry found~%")
	(setf (StudentEntry-state entry) 'incorrect)
	; run whatswrong help to set error interp now, so diagnosis
	; can be included in log even if student never asks whatswrong
        (diagnose Entry)
        ;; For now, log error tag and target info here.  Really should be 
	;; recorded in result turn for inserting with other assocs though. 
	(log-entry-info Entry)
	(return-from Check-NonEq-Entry (make-red-turn))) ; go no further

    ;; else got a match: set state and correctness from candidate
    (setf (StudentEntry-State entry) (car cand))
    (setf (StudentEntry-CInterp entry) (cdr cand))
    (setf match (first (StudentEntry-CInterp entry)))
    (format T "Matched ~A system entry: ~A~%" (car cand) match)
    ; For now, write this log info here. Note for variables with given equations, we are
    ; logging correctness of the variable definition substep, but the given value substep, 
    ; hence whole entry, might still be wrong.
    (log-entry-info Entry)

    ; decide what to return based on major state of entry
    (case (StudentEntry-State entry)
     ('correct 
      ; check any given value equations associated. At first one that is wrong, its
      ; result turn becomes the result for the whole entry. If wrong, checking routine 
      ; updates main entry record with error interp of equation.
      (dolist (e (StudentEntry-GivenEqns entry))
         (setf result (Check-Given-Value-Entry entry e))
	 (when (not (eq (turn-coloring result) **color-green**))
	     (return-from Check-NonEq-Entry result)))
          
      ; enter step as done in solution graph
       (sg-enter-StudentEntry Entry)
      ; if entry has associated implicit equations, enter them as done well
       (dolist (e (StudentEntry-ImplicitEqns entry))
	 (enter-implicit-entry e))
      ; if entry has associated given value equations, enter them as well
      ; Note we need parsed equation in systemese
      (dolist (e (StudentEntry-GivenEqns entry))
         (enter-given-eqn e))
      ; Everything is OK!
      (setf result (make-green-turn)))

     ; give special messages for some varieties of incorrectness:
     ('forbidden (setf result (chain-explain-more **Forbidden-Help**)))
     ('premature-entry 
     		 (setf result (chain-explain-more **Premature-Entry-Help**)))
     ('dead-path (setf result (chain-explain-more **Dead-Path-Help**)))
     ('nogood    (setf result (chain-explain-more **Nogood-Help**)))
     (otherwise  (error "Unrecognized interp state! ~A~%" 
                        (StudentEntry-state entry))
                 (setf result (make-red-turn))))

    ;; finally return result
    result))

; 
; log-entry-info -- insert extra info for entry into Andes log
;
; This function can be used for any student entry including eqns, 
; after status and possibly error info has been assigned.
; Logs: parsedEqn (eqns only), error label (if assigned), 
;       target step and op lists (if one is found)
; Runs wwh to get an error handler if one is unset.
; Sends async commands to workbench to do the logging, so
; the entries go before the final result in the log.
;
(defun log-entry-info (entry)
  (when (null entry) 
       (return-from log-entry-info NIL)) ; just in case
  ; don't waste time adding info when checking init entries 
  ; ?? might we want it anyway ??
  (when (not **checking-entries**)
     ; trap errors so any bug in lightly tested logging code 
     ; won't crash the whole entry handling:
     (safe-apply #'do-log-entry-info (list entry))))
  
(defun do-log-entry-info (entry)
  (let ((target-entries)
        (parse (StudentEntry-ParsedEqn entry)))
    ; fetch target entry list for correct or incorrect entries 
    (cond ((eq (StudentEntry-state entry) 'incorrect)
	    ; if needed, run whatswrong help to set error interp now, so diagnosis
	    ; can be included in log even if student never asks whatswrong
            (unless (StudentEntry-ErrInterp entry) (diagnose Entry))
	    (setf target-entries (Error-Interp-Intended (StudentEntry-ErrInterp Entry))))

	  ((eq (StudentEntry-state entry) 'correct)
	     (setf target-entries (studententry-Cinterp entry))))
   
    ; OK, do the logging
    (let ((*print-pretty* NIL))   ; no line breaks when formatting cmd strings!

       ; log the parse if we have it.  Note for syntax errors it may be list 
       ; containing partial parse tree printed as (#S(PARSE :TREE (LM m) :REM =x8*/7))
       ; print parse of ? for this
       (when (consp parse)  ; non-NIL => either prefix eqn or list of parse trees
           (send-fbd-command (format nil "assoc parse ~A" 
	                               (if (eq (type-of (first parse)) 'parse) "?" parse))))

       ; For non-eq entries, show entry prop in our notation, so we can identify common errors.
       ; For correct non-eq entries, it will be the step, but for errors we add it.
       (when (and (not (eq (first (studentEntry-prop entry)) 'eqn))
                  (eq (StudentEntry-state entry) 'incorrect))
            (send-fbd-command (format nil "assoc entry ~A" (studentEntry-prop entry))))

       ; log the error tag if one was found
       (when (StudentEntry-ErrInterp entry)
           (send-fbd-command (format nil "assoc error ~A" 
				     (error-interp-name (StudentEntry-ErrInterp Entry)))))

        ; log the target entry info if we have any. This shows comma-separated lists of entry props
	; ("steps") and parallel list of opnames
       (when target-entries
           (send-fbd-command (format nil "assoc step ~{~A~^,~}" 
	                                  (mapcar #'systementry-prop target-entries)))
           (send-fbd-command (format nil "assoc op ~{~A~^,~}" 
                                         (mapcar #'sg-map-systementry->opname target-entries))))
     )))

; following does the work of entering an implicit entry associated with
; a principal entry, either an implicit equation or an implicit variable
; entry, assuming the principal entry is correct. Arg is implicit entry.
(defun enter-implicit-entry (entry)
   (cond ((eq (first (studentEntry-prop entry)) 'implicit-eqn) 
          (enter-implicit-eqn entry))
	 (T ; other type, i.e. auto mass variable
	    (when (sg-match-studententry entry)  ; correct
	      (setf (studententry-cinterp entry) 
	          (cdr (first (studententry-PossibleCinterps entry))))
	      (setf (studentEntry-state entry) 'correct)
	      (sg-enter-StudentEntry entry)))))

;;
;; enter-implicit-eqn -- do the work of entering an implicit equation
;; Intended to be called for correct non-eqn entries only
;;
(defun enter-implicit-eqn (eqn-entry)
"enter implicit equation defined by correct non-eqn entry"
  (enter-subentry-eqn eqn-entry (second (StudentEntry-prop eqn-entry))))

(defun enter-given-eqn (eqn-entry)
"enter given value equation defined by correct non-eqn entry"
 ; don't enter if entry says value is unknown. 
 (when (not (blank-given-value-entry eqn-entry))
   (enter-subentry-eqn eqn-entry (studentEntry-ParsedEqn eqn-entry))))

; enter some dependent equation
(defun enter-subentry-eqn (eqn-entry eqn)
  (let* (; Set entry id to free high equation slot, aborting entry
	 ; if ran out of slots (should ensure enough so never happens)
         (slot (or (setf (StudentEntry-ID eqn-entry) (get-unused-implicit-slot))
	           (return-from enter-subentry-eqn)))
	 ; verify it with algebra and enter it in slot
         (result (solver-StudentAddOkay slot eqn)))

     ; if equation is not judged algebraically correct something is seriously 
     ; wrong, since original entry has been judged correct.
     (unless (and (numberp result) 
                  (or (= result 0) (= result 7)))
         (warn "Implicit eqn ~A judged bad by algebra!! (result=~A)~%" 
	        eqn result)) 
     (setf (StudentEntry-State eqn-entry) 'correct)
 
     ; To choose interpretation for solution graph marking, delegate to 
     ; interpret equation. This routine knows how to process the tagged
     ; sets of interpretations returned by sg and choose the best one, 
     ; leaving it into entry's Cinterps and ignoring its tutor turn result.
     (interpret-equation eqn-entry)
     ; The above routine might select an interpretation in which the entry 
     ; is premature or has some other flaw, but we don't care for implicit 
     ; equations: just go on to mark it done in the solution graph in any case
     (sg-enter-StudentEntry eqn-entry)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-answer -- lookup a students answer in the answer box
;; argument(s):
;;  answer: the contents of the answer field the student entered
;;  answer-id: the author-defined id for the answer field. must start with the
;;    string "ANSWER"
;; returns: StudentEntry
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Do-Check-Answer -- worker routine handle answer submission via check-answer
;; Returns: tutor-turn.
;;
#|
(defun Do-Check-Answer (answer-str sought-quant)
  (format T"~&Checking answer ~A = ~A~%" sought-quant answer-str)
  ; ignore blank answer box submissions
  (when (= 0 (length (remove #\Space answer-str)) )
       (return-from Do-Check-Answer NIL))

  ; cheap method -- pretend it's just another equation. 
  ; Later will add code to make sure it has correct form for answer
  ; Note student may or may not type "var =" in answer box
  ; !!! if they did, must verify var is the right one
  (let* ((stud-var (symbols-label sought-quant))
	 (answer-var stud-var)	; default, unless NIL, see below
	 (answer-eqn answer-str) ; default unless no =, see below
	 result-turn)
      ; verify there is a system variable for sought
      (unless (quant-to-sysvar sought-quant)
         (error "No system variable for ~A. Possible mismatch with answer box."
                 sought-quant))
      ; We need to build a student equation. If no student variable for answer
      ; quant has been defined, we define a temporary one. 
      ; !!! should ensure no conflict with any real one.
      (when (not stud-var)
          (symbols-enter "Answer" sought-quant sought-quant)
	  (setf answer-var "Answer"))
      ; construct a student equation unless entered in eqn form
      (unless (find #\= answer-str) 
          (setf answer-eqn (format NIL "~A=~A" answer-var answer-str)))

      ; do the work of checking an answer eqn, different from other eqns
      ; !! might want unwind-protect here to ensure cleanup
      (setf result-turn (check-answer-eqn answer-eqn Solver-*temp-eqn-slot*))

      ; undo temporary state changes for checking answer
      ; !!! We don't save entry so can't ask whats wrong!
      (solver-StudentEmptySlot Solver-*temp-eqn-slot*)
      (symbols-delete "Answer")

      ;finally return result 
      result-turn))

; this function no longer used by revised Do-Check-Answer
(defun check-answer-eqn (student-answer-eqn-str slot)
   ; temp: delegate to student equation checker.
   ; NB: not do-lookup-eqn-string, which does a return-turn
   (do-lookup-equation-answer-string student-answer-eqn-str slot))

|#
;; do-check-answer moved to parse-andes.cl


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MC Answers
;; Lookup-mc-answer is sent for multiple choice answers.  This is used for both 
;; the n-way multiple choice questions of the type in faa1 where the student is
;; selecting from among a set of choices.  It is also used in the fbd-only 
;; problems and other problems as an "I am done" button.  
;;
;; The two check functions below are designed to handle the separate cases.  
;; The lookup-mc-answer function in Commands.cl will determine which one of these
;; should be called and will do so handling the result as necessary.  In the 
;; comments below I will specify what occurs on each action.  



;; When the students are working on non-quant problems and the student has completed
;; a goal then they will will check an "I am done" box.  That commend will be sent 
;; to the help system as a call to (lookup-mc-answer (Answer-N 1)).  The value is 0
;; if the student is unchecking the box or 1 if they are checking it.  Unchecking 
;; the box is treated as a retraction of the assertion that the student is done 
;; *not* an assertion that they are not done.
;; 
;; If they check the box in a no-quant problem and this is sent then we will handle 
;; it using the code below.  If they have checked the box then we will look up the 
;; sought that is associated with the answer (in this case the n'th one) and will
;; then determine if the psm associated with it has been completed.  If so then it
;; will return a green turn else it will return a red turn.  No entry will be 
;; produced.
;;
;; If possible this will associate an automatic error interp that says "you have
;; not completed all of the steps necessary." Please call NSH.

(defun do-check-mc-no-quant-done-answer (ID Value)
  (cond ((not (numberp Value))
	    (error "Value not numberp in do-check-mc-answer: ~A" Value))
        ;; if cleared done button, delete any prior entry for this button
	;; and leave control black. 
	((= 0 Value) (remove-entry ID)
	             (make-black-turn))
	;; Treat any non-zero value as T, just in case other non-zero 
	;; comes from C 
	(T (check-mc-no-quant-done-answer-sought ID))))

;; Given an mc-no-quant done answer lookup the corresponding sought.  
;; Having done that look up the corresponding PSM and determine if the PSM 
;; has been completed if so then the value is green if not then don't. 
(defun check-mc-no-quant-done-answer-sought (ID)
  (let ((PSM (match-exp->enode (get-answer-quant ID) (problem-graph *cp*)))
        (Entry (make-StudentEntry :ID ID :prop `(lookup-mc-answer ,ID))))
    (cond 
     ;; If the PSM is not found then we need to throw an error saying that.
     ((null PSM) 
      (make-bad-problem-turn 
       :error (format nil "No problem step found for button labelled ~a" ID)))
     
     ;; If this is not a non-quant psm then we also need to thro an error 
     ;; asserting that fact. 
     ((not (enode-has-mark? PSM 'non-quant))
      (make-bad-problem-turn 
       :Error (format nil "Unmarked enode matching non-quant IDNum ~a ~a" PSM ID)))
     
     ;; Otherwize test to see if it present and behave appropriately.
     (t    (add-entry Entry)  ; save the entry
	   (cond ((psmg-path-enteredp (enode-path PSM))
		          (setf (StudentEntry-state entry) **CORRECT**)
		          (make-green-turn))
	         (T (setf (StudentEntry-state entry) **INCORRECT**)
		    (make-red-turn)))))))



;; If this is a true multiple-choice answer problem then we will be passed an
;; answer-id of the form "MC-#".  The code below will handle this appropriately.
;; Given an id of the form "MC-#" and a numerical value this code will produce
;; a non-eqn entry of the appropriate form, and then enter it before passing it
;; back to the handle-non-eq code for testing.  
;;
;; The Entry-proposition that will be produced is of the form: 
;;  (CHOOSE-ANSWER MC-# #)
;;
;; The ID in question will be the MC-# value supplied by the workbench.  Because
;; the entries are zero-counted in the help system but one-counted on the 
;; workbench we will subtract one from the value.  

(defun do-check-mc-multiple-choice-answer (ID Value)
  "Generate a new studententry for the selection and add it."
  (let* ((Prop `(CHOOSE-ANSWER ,ID ,Value))
	 (Entry (make-StudentEntry :ID ID :prop Prop)))
    (add-entry Entry)
    Entry))
