#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HelpSystem_0.cl
;;; Linwood Taylor and Collin Lynch.
;;;
;;; This file contains helpsysytem functions that mirror those
;;; located in Help/Commands.cl.  When the Log2English system
;;; parses logfiles these functions will be called in order to 
;;; translate the api calls.  
;;;
;;; This file was originally written by Linwood Taylor whith 
;;; subsequent modification by collin lynch.
;;;
;;; ------------------------------------------------------------
;;; ChangeLog:
;;;
;;;  3/4/2003:  Modified read-problem-info to print out stetment.
;;;  3/5/2003:  Modified handle-student response for new forms.
|#

;; Global declaration for the no-bindings var.
;;(declare (special no-bindings))


;;;; -------------------------------------------------------
;;;; Variables.
;;;; The variables list tracks the students entries and actions
;;;; by system and student variable.  This tracking allows us to 
;;;; lookup the expressions by either type of variable and 
;;;; to delete them as necessary.
;;;;
;;;; Each entry in the list is of the form:
;;;;  (<ID> <StudentVar> <Exp> <Str>)
;;;;
;;;; Where: ID is the system-supplied entry id.
;;;;        Label is the user-supplied label.
;;;;
;;;; The system can be searched by each one.

(defparameter **variables** nil)

(defstruct HSVar
  ID     ;; The System ID for the entry.
  Label  ;; The student's label for the given entry.
  Exp    ;; The entryprop expression for the entry.
  Str)   ;; An optional string form for the entry used in formatting.


;;; Add a variable to the list by generating a new struct
;;; for it and filling it as necessary.  This will replace
;;; any preexisting vars that match with the same id or
;;; StudentVar.
(defun add-hsvar (&key (ID Nil) (Label Nil) (Exp Nil) (Str Nil))
  "Add the specified student variable to the list."
  (if (find-hsvar-id ID) (delete-hsvar-id id))
  (if (find-hsvar-label Label) (delete-hsvar-label Label))
  (push (make-hsvar :ID ID :Label Label :Exp Exp :Str Str) **Variables**))


(defun delete-hsvar-id (ID)
  "Delete the var matching id from list if it exists."
  (when ID
    (setq **Variables** 
      (remove ID **Variables** :key #'hsvar-id :test #'equalp))))

(defun delete-hsvar-label (Label)
  "Delete the var matching label from list if it exists."
  (when Label
    (setq **Variables** 
      (remove Label **Variables** :key #'hsvar-label :test #'equalp))))


(defun find-hsvar-id (ID)
  "Lookup a variable that matches the specified id."
  (if ID (find ID **Variables** :Key #'hsvar-ID :test #'equalp)))

(defun find-hsvar-label (Label)
  "Look up an HSvar by label."
  (if Label (find Label **Variables** :Key #'hsvar-Label :test #'equalp)))

(defun find-hsvar-unify-exp (Exp &key (Bindings no-bindings))
  "Find the variable that unifies with Exp."
  (declare (special no-bindings))
  (loop for V in **Variables**
      when (unify Exp (HSVar-Exp V) Bindings)
      return V))


(defun collect-hsvars-unify-exp (Exp &key (Bindings no-bindings))
  "Collect all of the variables that unify with the specified exp."
  (declare (special no-bindings))
  (loop for V in **Variables**
      when (unify Exp (HSVar-Exp V) Bindings)
      collect it))



#|;;;; The variables list is used to store the entries that the
;;;; students have made along with their ids.  The entries are
;;;; stored as psm expressions apart from the axes which are 
;;;; stored as a list of the form (<exp> <str>) where str is
;;;; a comma separated list of the vector names.
;;;;
;;;; Variables are added using add-variable for each student entry
;;;; they are deleted using delete-variable for each deletion call.
;;;; Variables can be looked up by variable-name using the find-variable 
;;;; command or unified using the unify-variable-exp command.

(defun add-variable (var info)
  ;;(format t "Adding variable ~A with data ~A~%" var info)
  (if (member var **variables** :test #'(lambda (x y) (equal x (car y))))
      (setf **variables** (remove var **variables** :test #'(lambda (x y) (equal x (car y))))))
  (setf **variables** (append **variables** (list (list var info)))))

(defun delete-variable (var)
  ;;(format t "Deleting variable ~A~%" var)
  (if (member var **variables** :test #'(lambda (x y) (equal x (car y))))
      (setf **variables** (remove var **variables** :test #'(lambda (x y) (equal x (car y)))))))

(defun find-variable (var)
  (let ((tmp (assoc var **variables**)))
    ;;(if tmp (format t "Found ~W~%" tmp) (format t "NOT found ~W~%" var))
    (if tmp
	(second tmp)
      nil)))

;;; Note:: this returns the var-exp pair.
(defun unify-variable-exp (Exp &optional (Bindings no-bindings))
  "Search for the variable whose exp matches the supplied exp."
  (loop for V in **Variables**
      when (unify Exp (second V) Bindings)
      return V))		 
|#


;;;; ---------------------------------------------------------------------------
;;;; Entry-API Glue code.
;;;; The functions in this section have been adapted from Entry-API.cl to permit
;;;; the code that I have copied from there into the functions below to work.
;;;;
;;;; In some cases the argument have been adapted in some cases not.  


(defparameter *CP* "The currently loaded problem.")

;;
;; Times
;;

(defun arg-to-time (time-arg)
  "convert workbench time argument symbol into a KB time expression"
  (cond 
	;; may be NIL on probs w/o distinguished times
        ((null time-arg) (get-default-time))
	;; WB allows defined student variable for duration as interval name.
	((get-student-time time-arg))
	;; may be time interval symbol of form |T0 to T1|
	((wb-interval-namep time-arg) (get-wb-interval time-arg))
	;; or may be time point name of form T1
        ((wb-time-pt-namep time-arg) (get-wb-time-pt time-arg)) 
	(T (warn "unrecognized time argument:~A" time-arg)
	   NIL)))

(defun get-default-time () 
  "return the default KB time for the problem"
   1) ; assuming it will always be time point 1 


;; In some cases the students can specify their own var names
;; for the times.  This code looks up the time by symbol for
;; later use.
(defun get-student-time (label)
  "return time term for student-defined duration, NIL if not a duration"
  (let* ((label-str (if (symbolp label) (symbol-name label) label))
         (var (find-hsvar-label label-str))
	 (quant (if var (hsvar-exp var))))
    (when Quant
      (if (and (consp quant) 
	       (eq (first quant) 'duration))
	  (second quant)))))

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


;;; Translate the (possibly) numeric answer into a valid form
;;; for the system.
(defun arg-to-dir (dir-arg &optional mag-arg)
  "Label the WB API direction appropriately."
  (cond	;; zero-mag vectors have no direction: use special atom 'zero
        ((and (numberp mag-arg)
	      (= 0 mag-arg)) 'zero)
        ;; else may be NIL if unspecified -- use special atom 'unknown
        ((NULL dir-arg) 'unknown)
	;; else should be a number
	((not (numberp dir-arg)) (error "non-numeric direction arg:~A" dir-arg))
        ;; negative numbers code z-axis directions, use special atoms
        ((= dir-arg -1)  'out-of)
	((= dir-arg -2)  'into)
	((= dir-arg -3)  'z-unknown)
	;; else should be xy plane angle in degrees. 
	(T  `(dnum ,(mod dir-arg 360) |deg|))))    



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
;; form a compound body term for it. Currently this arg form is only sent to
;; assert-compound-object but might be used elsewhere in the future.
;; NB: can also get NIL body-arg in some cases, such as agent of net-work.
;; should remain NIL for none.
;;
;; Note we assume body names have no spaces, even if wrapped in vbars
;; (read-from-string used to get symbol in fix-body-name.)
(defun arg-to-body (body-arg)
  "Convert WB API body argument to KB body term" 
  (if (consp body-arg) ;; list of body names: make compound body
      `(compound ,@(sort (mapcar #'fix-body-name body-arg) #'expr<)) 
    ;; else atomic body name:
    ;; check if this is a student-defined compound-body label
    (let* ((Body-Var (find-hsvar-Label (string body-arg)))
	  (stud-quant (if Body-Var (hsvar-exp Body-Var))))
      (if (and stud-quant (compound-bodyp stud-quant)) 
	  stud-quant
	(fix-body-name body-arg)))))

(defun fix-body-name (body-arg)
  "Convert atomic body name symbol read from WB arg to KB body symbol"
  (read-from-string (string-upcase (substitute #\_ #\- (string body-arg)))))


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

(defconstant **force-types** '(
  (grav . WEIGHT)
  (|Kinetic Friction| . KINETIC-FRICTION)
  (|Static Friction| . STATIC-FRICTION)
  ; Contact force type not used in Andes2. Replaced by APPLIED in most places,
  ; though changed to Normal force in a couple of problems.
  (Contact . APPLIED)
  ; others just need case adjustment
))
(defconstant **vector-types** '(
  (Acceleration . ACCEL)
  (Ang-Acceleration . ANG-ACCEL)
  (Position . RELATIVE-POSITION)
  ; others just need case adjustment
))
(defconstant **quantity-types** '(
  (|distance travelled| . DISTANCE)
  (|distance between| . DISTANCE-BETWEEN) ; not used in Andes2 yet
  (|gravitational acceleration| . GRAVITATIONAL-ACCELERATION)
  (radius . REVOLUTION-RADIUS)
  (spring-const . SPRING-CONSTANT)
  (comp-dist . COMPRESSION)
  ;(energy . TOTAL-ENERGY)
  ; include vector type ids as well:
  (Acceleration . ACCEL)
  (Ang-Acceleration . ANG-ACCEL)
  (Position . RELATIVE-POSITION)
))

; KB potential energy type (grav-energy or spring-energy) is implicit in 
; type of body arguments sent from workbench (one should be planet or spring)
; Following helps determine KB PE type to use.  We could look up types in 
; problem givens, but for now for now we just use SLEAZY HACK exploiting fact 
; that in all Andes probs these are named 'spring or 'earth, respectively.
(defun planetp (body-term)
  (sym-match body-term 'earth))
(defun springp (body-term)
  (sym-match body-term 'spring))

; Dispatch function takes an atom coding a vector attribute and a vector-term
; Builds and returns a term for that attribute of the vector.
; Uses functions defined in kb/Physics-Funcs.cl
; prop-id = 'mag 'dir 'xc 'yc 'zc 
;            NIL or anything else gets vector term unchanged
(defun vec-prop (prop-id vector-term)
 (case prop-id
   (mag  (vector-mag vector-term))
   (dir  (vector-dir vector-term))
   (xc   (vector-xc vector-term))
   (yc	 (vector-yc vector-term))
   (zc	 (vector-zc vector-term))
   (otherwise vector-term)))



; make-quant -- Build a quantity expr from define-variable arg list. 
; Note args come in workbench form. They should be converted to helpsys
; form before the case. 
;
; Prior to Andes 5.0* students could define vector variables without drawing
; them, so could also specify vector quantities this way. We handle these
; also because this argument list will also be used to send quantity choice 
; responses, even though define-variable won't send them. 
; optional prop is vector attribute to use, 
; !!! This functionality could be moved into the ontology tables.
(defun make-quant (type quant body body2 time &optional (prop 'mag))
 (let* ((quant-type (arg-to-id **quantity-types** quant))
	(subtype    (if (eq quant-type 'force) (arg-to-id **force-types** type)
	              (arg-to-id **quantity-types** type)))
        (time-term  (arg-to-time time))
	(body-term  (arg-to-body body))
	(body2-term (arg-to-body body2)))
 (case quant-type
  ; scalar quantities:
  (duration   `(duration ,time-term))
  (distance   `(at (distance ,body-term) ,time-term))
  (distance-between 
              `(at (distance-between ,body-term ,body2-term) ,time-term))
  (mass       `(mass ,body-term))
  (speed      `(at (speed ,body-term) ,time-term))
  (revolution-radius 
              `(at (revolution-radius ,body-term) ,time-term))
  (period     `(period ,body-term))
  (gravitational-acceleration 
               `(gravitational-acceleration ,body-term))
  (work        ; work may be net work, work-nc, or work by an agent
               (cond ((null body2-term) 
	                    `(at (net-work ,body-term) ,time-term))
		     ((string-equal body2-term '|nonconservative|)
	                    `(at (work-nc ,body-term) ,time-term))
	             (T     `(at (work ,body-term ,body2-term) ,time-term))))
  (power       ; may be net power or power from an agent
               (if (not body2-term) `(at (net-power ,body-term) ,time-term)
	           `(at (power ,body-term ,body2-term) ,time-term)))
  (energy      (case subtype
                  (total   `(at (total-energy ,body-term) ,time-term))
                  (kinetic `(at (kinetic-energy ,body-term) ,time-term))
                  (potential 
		   ; URGH, whether PE is elastic or gravitational can only be
	           ; determined by type of body (spring or planet). .
		   ; Also allow args in any order. Prefer grav if they include 
		   ; planet. 
		    (cond ((planetp body2-term) 
                              `(at (grav-energy ,body-term) ,time-term))
			  ((planetp body-term) 
                              `(at (grav-energy ,body2-term) ,time-term))
		          ((springp body2-term)
	                      `(at (spring-energy ,body-term) ,time-term))
		          ((springp body-term)
			      `(at (spring-energy ,body2-term) ,time-term))
		          (T ; no planet or spring! Use grav, it always exists.
			     ; include both terms so error handlers can detect
			      `(at (grav-energy (,body-term ,body2-term)) ,time-term)))
	          )))
  (compression 
              `(at (compression ,body-term) ,time-term))
  (spring-constant 
              `(spring-constant ,body-term))
  (coef-friction  ; !!!student has to get body args in right order:
              `(coef-friction ,body-term ,body2-term ,subtype))
  (height     `(at (height ,body-term) ,time-term))
  (moment-of-inertia 
              `(at (moment-of-inertia ,body-term) ,time-term))
  (length     `(at (length ,body-term) ,time-term))
  (width      `(at (width ,body-term) ,time-term))
  (radius     `(at (radius ,body-term) ,time-term))
  ; for circuits kb:
  (voltage    (case subtype
                  (across `(at (voltage-across ,body-term) ,time-term))
		  (otherwise	  
		       	   `(at (voltage-btwn ,body-term ,body2-term) ,time-term))))
  (current     (case subtype
		   (through   `(at (current-thru ,body-term)  ,time-term))
                   ;(at       `(at (current-at ,body-term) ,time-term))
		   (in        `(at (current-in ,body-term)  ,time-term))))
  (resistance  ; body-term may come as (compound a b c) for equivalents
               (if (atom body-term) `(resistance ,body-term)
                `(resistance ,(cdr body-term))))
  (capacitance ; body-term may come as (compound a b c) for equivalents
               (if (atom body-term) `(capacitance,body-term)
                `(capacitance ,(cdr body-term))))
  (charge      `(at (charge-on ,body-term) ,time-term))
  ;
  ; vector quantities -- form appropriate prop (mag or dir or compo)
  ;
  ((relative-position position) ; include workbench type id to be safe
                (vec-prop prop `(at (relative-position ,body-term ,body2-term) 
                                    ,time-term)))
  (displacement (vec-prop prop `(at (displacement ,body-term) ,time-term)))
  (velocity     (vec-prop prop `(at (velocity ,body-term) ,time-term)))
  ((accel acceleration) ; include workbench type id to be safe
                (vec-prop prop `(at (accel ,body-term) ,time-term)))
  (force        (vec-prop prop 
                  (if (eq subtype 'Net) 
                      `(at (net-force ,body-term) ,time-term)
                   `(at (force ,body-term ,body2-term ,subtype) ,time-term))))
  (momentum     (vec-prop prop `(at (momentum ,body-term) ,time-term)))
  ((ang-accel ang-acceleration) ; include workbench type id to be safe
                (vec-prop prop `(at (ang-accel ,body-term) ,time-term)))
  (ang-velocity (vec-prop prop `(at (ang-velocity ,body-term) ,time-term)))
  (ang-displacement 
                (vec-prop prop `(at (ang-displacement ,body-term) ,time-term)))
  (ang-momentum (vec-prop prop `(at (ang-momentum ,body-term) ,time-term)))
  (torque       (vec-prop prop 
                  (if (eq subtype 'net) 
		       `(at (net-torque ,body-term ,body2-term) ,time-term)
		    (find-torque-term body-term body2-term))))
  ;; unknown:
  (otherwise   (format T "~&Warning!! unknown type to make-quant: ~A~%" 
                          quant-type)
               ; what the hey, give it a try:
	       (if body2-term 
	           `(at (,quant-type ,body-term ,body2-term) ,time-term)
               `(at (,quant-type ,body-term) ,time-term)))
 )))


; Following is used to handle quantity choice results. The two
; arguments are: first, an unevaluated API call call which is either 
; define-variable or define-angle-variable. This is the call that would be
; sent if the student had actually defined a var in the relevant dialog.
; This is convenient way of packaging the spec because the workbench already 
; has code to build these api calls at the end of definition dialogs. 
; The second argument is an optional vector-attribute id, which tells which
; attribute of the vector is to be used, default mag.
; Usage examples:
;    (wb-quant '(define-variable "" average Velocity car T1 NIL NIL) 'mag)
; or for angle quantities
;    (wb-quant '(define-angle-variable "" 270 posx "vf" NIL))
; Return value is the quantity term specified.
;
; In fact the workbench is clever and uses this as follows: when the student
; response is a quantity selection, it sends a command of the form
; (handle-student-response #.(wb-quant (define-variable ....) mag)
; When read by the dispatcher, this has the effect of
;       (handle-student-response <quant-term>)
; so responder code doesn't have to know about the translation.
;
(defun wb-quant (api-call &optional (vecprop 'mag))
  (case (first api-call)
    ('define-variable (make-quant (third api-call) (fourth api-call) 
				  (fifth api-call) (seventh api-call)
				  (sixth api-call) vecprop))
    ; !!! need routine to handle following case:
    ; maybe shouldn't be allowed now that have dir vecprop
    ;('define-angle-variable )
    (otherwise
    	(warn "wb-quant: Unrecognized quantity spec: ~A~%" api-call)
	NIL)))

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


;;; If the system supplied a point of force and an axis,
;;; locate the corresponding torque term in the variables list.
;;; this is done by unifying against the variable expression.
;;;
;;; NOTE:: Modified hack.
(defun find-torque-term (pt axis)
"fill in fully explicit torque term implicitly defined by point of force application and axis"
  (let* (; get force by searching entries for force at pt of application
	 (force-matches (find-hsvar-unify-exp `(vector (at (force ,pt ?agent ?type) ?t) ?dir)))
	 (force-match   (if (= (length force-matches) 1) (car force-matches)))
	 (force-quant   (if force-match (second (second force-match))
	                  `(force ,pt unknown unknown)))
	 ;; get main body by searching problem givens for part-of
	 (body-matches (filter-expressions `(part-of ,pt ?b) 
					   (problem-givens *cp*)))
	 (body-match   (if (= (length body-matches) 1) (car body-matches)))
	 (body-term    (if body-match (third body-match) 'unknown)))
    ;; return the torque quantity
   `(torque ,body-term ,axis ,force-quant) 
  ))



;;; This function is here to do the final formatting of vector entries that
;;; is common to all of the vector entry apis.  This will produce the final
;;; string that is returned to the user.  
;;;
;;; It will also add the specified entry to the list of **variables**
(defun format-vector-entry (label vquant-term time-term dir-term id)
  (let* ((vector-term `(at ,vquant-term ,time-term))
	 (action      `(vector ,vector-term ,dir-term)))
    (add-hsvar :ID ID :Label Label :Exp action)
    (format nil "Student draws a ~a called ~a~%"
	    (nlg action 'nlg-entryprop) label)))











;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; On a read student info we clear the state and set the student information
;;; for later use.

;; ok

(defun read-student-info (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (setf **lapsed-time** 0)
  (setf **tracking-time** nil)
  (respond 2)
  (setf **student-name** (car in))
  (format nil "Student: I am ~A.~%" (car in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; When a problem is opened the sysytem clears the state including the problems and 
;;; vars before loading and setting up the current problem.  It then prints out the 
;;; problem name and the problem statement to the file. 
;;;
;;; Becuase of the need to print the problem statement without any oddities
;;; I have chosen to print directlry to the stream in this function rather
;;; than to print in another way in order to acoid the problems that I was
;;; having when I just passed the string back.
;;;
;;; ok

(defun read-problem-info (stream lineno time in &optional (str ""))
  (declare (ignore Lineno Time Str))
  (respond 4)
  (setf **equations** nil)
  (setf **variables** nil)
  (setf **current-problem** (car in))
  (setf **the-output-stream-name**
    (concatenate 'string **student-name** "." **current-problem** ".prb"))
  ;;(format t "Changed Student: Open problem ~A. ***********************************~%" (car in))
  (setq *cp* (get-problem (read-from-string (car in))))
  (format Stream "~%.~% Student: Open Problem ~a.~%~,,30,'*a~%" (car in) #\*)
  (format 
   Stream (l-format (format Nil " ~a~%" (if *cp* (apply #'concatenate 'string (problem-statement *cp*))))
		    ""))
  (format Stream "~,,30,'*a~%" #\*)
  "")
  
;;  (format Nil "~%.~% Student: Open Problem ~a.~%~,,30,'*a~%~a~%~,,30,'*a~%"
;;	  (car in) #\* (if *cp* (apply #'concatenate 'string (problem-statement *cp*)))
;;	  #\*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; When a problem is opened the system will load any saved entries if they exist.
;;; This process will be bracketed by calls to check-entries 1 and check-entries 0.
;;; This code will print out that fact to the stream and move on.

(defun check-entries (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time In Str))
  (setf **get-proc-help-count** (+ 1 **get-proc-help-count**))
  (respond 0)
  (setq State (car in))
  (if (string= State "1")
      (format Nil "Reading saved entries.~%")
    (format Nil "Done reading saved entries.~%")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; On a call to get prob help update the count of the help, and report the 
;;; decision to the file.
;;;
;;; ok

(defun get-proc-help (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time In Str))
  (setf **get-proc-help-count** (+ 1 **get-proc-help-count**))
  (respond 0)
  (format nil "~%.~%          Student asks what should be done next?~%"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The handle student-response function deals with the student's response to a query
;; this is typically invoked by the help system when it is carrying on a dialog with
;; the student.  
;; Student responses can be:
;;  1. A PsmGroup name.
;;  2. A list of the form (<PsmGroup> <Bindings>)
;;  3. A PsmClass name.
;;  4. A list of the form (<PsmClass> <Bindings>)
;;  5. An equation name.
;;  6. A list of the form (<EquationName> <Bindings>)
;;  7. A specialized atom such as 'yes.
;;  8. A variable definition produced by wb-quant.
;;  9. A free text string.
;
;; The student responses can come in several forms

;; Start Here:: Test on sample of logs grep for errors and "Unknown Response"
;;
;; ok

(defun handle-student-response (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 1)
  (cond 
   ((equal (car in) "OK")               ;; If the student picked "ok"
    (format nil "Student says OK.~%"))   ;; Then say so.
   ((cdr in)                            ;; If there is a follow up
    (format nil "Student:2 ~A.~%"  in))  ;; then just print that.
   ((atom (car in))                     ;; If it is an atom then
    (hsr-handle-atom (car in)))         ;; handle it.
   (t (hsr-handle-list (car in)))))

;;; If the student's response is an atom then it can be a specialized
;;; atom such as 'No' 'Yes' or "Continue".  If not then it is a psmclass
;;; or psmgroup name and will be printed as such.  If it is an atom but
;;; does not fit into one of these classes then it will be echoed to the
;;; student.  
(defun hsr-handle-atom (A)
  (let (TemporaryVar)
    (cond
     ((numberp A) (format nil "Student answers with '~A'.~%"  A))
     ((equal 'No A) (format nil "Student says 'No'.~%"))
     ((equal 'Yes A) (format nil "Student says 'Yes'.~%"))
     ((equal 'Cancel A) (format nil "Student ignores the question.~%"))
     ((equal "Continue" A) (format nil "Student chooses to continue with current plan.~%"))
     ((equal "Replan" A) (format nil "Student wishes to replan.~%"))
     ((equal 'close-lesson A) (format nil "Student closes this lesson.~%"))
     ((setq TemporaryVar (lookup-psmgroup-name A))  ;; if it is a psmgroup
      (format Nil "Student Chooses: ~a.~%" (car (psmgroup-english TemporaryVar))))    ;; return the english form.
     ((setq TemporaryVar (lookup-psmclass-name A))  ;; if it is a psmclass
      (format Nil "Student Chooses: ~a.~%" (car (psmclass-english TemporaryVar))))    ;; then print that.
     ((setq TemporaryVar (lookup-name->Equation A))
      (format Nil "Student chooses the: ~a equation.~%" 
	      (car (equation-english TemporaryVar))))
     (t (format nil "Student: ~A.~%"  A)))))


;;; If the student response is a list then it should be either a list of 
;;; the form (<PSmGroup>/<PsmClass>/<Equation> <Bindings>) or a variable 
;;; expression.  If it is anything else then it will be echoed to the 
;;; log as-is.
(defun hsr-handle-list (A)
  (let (TemporaryVar)
    (cond 
     ((setq TemporaryVar (lookup-psmgroup-name (car A))) 
      (format Nil "Student Chooses: ~a.~%" 
	      (car (psmgroup-english TemporaryVar))))
     ((setq TemporaryVar (lookup-psmclass-name (car A))) 
      (format Nil "Student Chooses: ~a.~%"
	      (car (psmclass-english TemporaryVar))))
     ((setq TemporaryVar (lookup-name->Equation (car A)))
      (format Nil "Student chooses the: ~a equation.~%" 
	      (car (equation-english TemporaryVar))))
     ((valid-expression-p a) 
      (format Nil "Student chooses: ~a.~%" (nlg a)))
     (t (format Nil "Unknown response ~a.~%" a)))))
    
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; On an explain-more increment the count and return the string.
;;
;; ok

(defun explain-more (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time In Str))
  (setf **explain-more-Count** (+ 1 **explain-more-Count**))
  (respond 0)
  (format nil "Student requests more help.~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On an object assertion we want to add the object to the variables list so that 
;; it can be used later and then return a string describing it to the user.
;;
;; Start here testing.

(defun assert-object (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let* ((label (first in)) (name (second in)) 
	 (time (third in)) (id (fourth in))
	 (time-term (format nil "~(~a~)" time))
         (body-term (format nil "~(~a~)" name))
	 (action   `(body ,body-term ,time-term)))
    (add-hsvar :ID ID :Label Label :Exp Action)
    (format nil "Student draws a body called '~(~A~)' for: ~A.~%" 
	    label (nlg action 'nlg-entryprop))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When compound objects are declared we should simply pass the command back 
;; to the assert-object code since the output will be handled the same way.
(defun assert-compound-object (stream lineno time in &optional (str ""))
  ;;(declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let ((label (first in))
	(names (second in))
	(time-arg (third in))
	(id (fourth in)))
    (assert-object Stream Lineno time (list Label Names Time-Arg ID) Str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOTE:: This doesn't really handle the vquant term properly for non-net torques becuase that
;;   necessitates a memory lookup of the problem definition.  Once other things are done I will
;;   return to this as necessary to repair it.  
;;
;; This code defines a torque expression and torque variable and then calls the format-vector
;; entry command to format it and to store the HSVar.
(defun lookup-torque (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let* ((label (first in)) (net (second in))
	 (body-term (format Nil "~(~a~)" (third in)))
	 (axis-term (format Nil "~(~a~)" (fourth in)))
	 (dir-term (arg-to-dir (fifth in) (sixth in)))
	 (time-term (seventh in))
	 (ID (eighth in))
	 ;; net torque is easy. For individual torques, must find body containing
	 ;; point of application as part and also full term for force applied
	 ;; at that point, assumed to be unique. Note if no such force we can't 
	 ;; determine a full quantity spec at all.
	 ;;
	 ;; For now this just generates a dummy quantity spec although that 
	 ;; may change.
	 (vquant-term (if net `(net-torque ,body-term ,axis-term)
			`(torque ,body-term ,axis-term Unknown-force))))
    (format-vector-entry label vquant-term time-term dir-term id)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given the definition for a force entry form the arguments necessary for a format
;; vector entry call.
(defun lookup-force (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let* ((label (car in)) 
	 (type-term   (arg-to-id **force-types** (second in)))
	 (body-term   (arg-to-body (third in)))
	 (agent-term  (arg-to-body (fourth in)))
	 (dir-term    (arg-to-dir (fifth in) (sixth in)))
	 (time-term   (arg-to-time (seventh in)))
	 (id (eighth in))
	 ;; net force is special:
	 (vquant-term (if (eq type-term 'Net) `(net-force ,body-term)
			`(force ,body-term ,agent-term ,type-term))))
	 
    (format-vector-entry label vquant-term time-term dir-term id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given a generic vector definition format the information necessary for
;; a format-vector entry call.
(defun lookup-vector (stream lineno time in &optional (str ""))
  (declare (ignore Str Time LineNo Stream))
  (respond 0)
  (let* ((label (first in)) 
	 (avg-inst (second in))
	 (type (third in)) 
	 (system (fourth in)) 
	 (dir (fifth in))
	 (mag (sixth in))
	 (time (seventh in))
	 (id (eighth in))
	 (vtype       (arg-to-id **vector-types** type))
	 (body-term    (arg-to-body system))
	 (time-term    (arg-to-time time))
	 ;; avg-inst choice is redundant with time -- WB ensures consistency
	 ;; However, for relative-position vectors only, avg-inst arg carries 
	 ;; the second body argument = origin from which position is defined.
	 (vquant-term (if (equal vtype 'relative-position) 
			  `(,vtype ,body-term ,(arg-to-body avg-inst))
			`(,vtype ,body-term)))
	 (dir-term     (arg-to-dir dir mag)))
    (format-vector-entry label Vquant-Term Time-Term Dir-Term id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When the student asserts an axis we want to form that information for them as 
;; well as store the axis information for later use.  Because the draw-axes exp 
;; does not contain slots for the axis names we will store that information in
;; the hsvar's string field for later.
(defun assert-x-axis (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  ;; Let statement to dup the variables.
  (let* (;; Not necessary for now. (body (first in)) 
	 (dir (second in)) 
	 (id (third in))
	 (x-label (fourth in))
	 (y-label (fifth in)) 
	 (z-label (sixth in))
	 (action `(draw-axes ,dir))
	 (form (format nil "~a,~a,~a" X-Label Y-Label Z-Label))
	 ;; The string that we will use is dependent upon whether
	 ;; or not the var is new or being reused.
	 (mod (if (find-hsvar-id id) "modifies" "draws")))

    ;; Since nothing is lost by rebuilding the var do it replacing
    ;; the old one if it is necessary.  No Label is given so none is used.
    (add-hsvar :ID ID :Exp Action :Str Form)
    (format Nil "Student ~a an axis ~a at ~a degrees.~%" Mod Form Dir)))
    
#|    
    ;; When we have not yet stored the axis var store it. (in str form)
    (let ((str (if (find-hsvar-id id) 
		   (format Nil "Student modifies axis called ~a ~a at ~a degrees.~%"
			   Id Form Dir)
		 (format Nil "Student draws axis called ~a ~a at ~a degrees. ~%"
			 ID Form Dir))))
    
			 (cond ((find-hsvar-id id)
				
	   
	   
	   (format Nil "Student modifies axis called ~a ~a at ~a degrees.~%"
		   Id Form Dir))
	  (t (add-variable id Form)
	     (format Nil "Student draws axis called ~a ~a at ~a degrees. ~%"
		     ID Form Dir)))))
		     
		     |#

#|
    
  (if (find-variable (nth 2 in))
      (format nil "Student modifies axis ~A,~A,~A at ~A degrees.~%" 
	      (nth 3 in) (nth 4 in) (nth 5 in) (second in))
    (let ((tmp (nth 2 in)))
      (add-variable tmp (format nil "~A,~A,~A" (nth 3 in) (nth 4 in) (nth 5 in)))
      ;;(format t "~W~%" **variables**)
      (format nil "Student draws axis ~A,~A,~A at ~A degrees.~%" 
	      (nth 3 in) (nth 4 in) (nth 5 in) (second in)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On a solve for var increment the count and inform the user.  Since the result
;; of the solve-for is not given here don't bother to make a var for it.
(defun solve-for-var (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (setf **solve-for-var-count** (+ 1 **solve-for-var-count**))
  (respond 0)
  (let ((Var (car in)) 
	(New-ID (cadr in)))
    (declare (ignore New-ID))
    (format nil "Student asks Andes to solve for '~A'~%" Var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On a check-answer print the student's request to the user and make no other changes.
(defun check-answer (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let ((inputo (first in)) 
	(sought-quant (second in)) 
	(id (third in)))
    (declare (ignore id))
    (format nil "Student states that the answer for ~A is '~A'.~%" 
	    Sought-Quant Inputo)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MC-answers are multiple-choice answers that we use to handle some of the specialized
;; multiple-choice-question problems.  
(defun lookup-mc-answer (stream lineno time in &optional (Str ""))
  "Handle a multiple choice question."
  (declare (ignore Stream Lineno time Str))
  (respond 0)
  (let ((ID (first in))
	(Value (second in)))
    (format Nil "Student states that the multiple-choice answer for ~a is '~a'.~%"
	    ID Value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On a close-problem handle the time changes and inform the user.
(defun close-problem (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Str))
  (respond 0)
  (handle-time-quit-tracking time)
  (handle-time-report)
  (format nil "Student is finished with problem ~A for now.~%" (car in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On a close lesson print no string.
(defun close-lesson (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time In Str))
  (respond 0)
  (format nil ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When the student exits andes echo that to the system.
(defun exit-andes (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time In Str))
  (respond 0)
  (format nil "Student: I'm finished with Andes for now.~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For a dialog response just echo it to the file.  
(defun get-dialog-response (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 3)
  (format nil "Student answers with '~A'~%"  (car in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On a calculate equation string increment the count and echo to the file.
(defun calculate-equation-string (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (setf **calculate-equation-string-count** (+ 1 **calculate-equation-string-count**))
  (respond 0)
  (format nil "Student asks Andes to solve the equation '~A'~%"  (car in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When the student looks up an equation string we need have one of three cases to handle.
;;
;; If the equation specified is "" then the student either deleted equation or hit the
;; return key in an empty equation box.  If they hit the key just echo a string to that
;; effect and return.  
;;
;; If they deleted an equation then we need to remove the equation from the var list and
;; then go ahead and get the specified equation from the **Equations** list (see the 
;; listing in SLog2English.cl).  Then echo a string to the file.
;;
;; Else they have entered a new equation and we want to store it for later and echo 
;; to the file.
;;
;; NOTE:: For Occulted reasons the test is here to check if str is "".  If this is
;;  the case then the system will just skip any equation with a "" string.  Normally
;;  the string will contail the same set of information as in and should not be empty
;;  as lookup-eqn-string should include (at least) the eqn-id.

(defun lookup-eqn-string (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time))
  (respond 0)
  (let ((Eqn-String (car in)) (Id (cadr in)))
    (cond ((equal "" str) "")
	  ((equal Eqn-String "")
	   (let ((tmp (get-equation ID)))
	     (if (null tmp) (format Nil "Student hit enter empty equation box #~a.~%" ID)
	       (progn (delete-equation ID)
		      (format Nil "Student deletes the equation '~a'.~%" tmp)))))
	  (t (add-equation Eqn-String ID)
	     (format Nil "Student enters the equation: ~a~%" Eqn-String)))))
	      
#|  (if (not (equal "" str))
      (cond
       ((equal "" (car in))
	(let ((tmp (get-equation (second in))))
	  (cond
	   (tmp
	    (delete-equation (second in))
	    (format nil "Student deletes the equation '~A'.~%"  tmp))
	   (t
	    (format nil "Student hit enter in an empty equation box.~%")))))
       (t
	(add-equation (car in) (second in))
	(format nil "Student enters the equation: ~A~%"  (car in))))
    ""))
    |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When the student asks what is wrong with an eqn exho it to the system.
(defun why-wrong-equation (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (setf **why-wrong-equation-count** (+ 1 **why-wrong-equation-count**))
  (format nil "~%.~%          Student wonders what is wrong with the equation '~A'.~%"
	  (get-equation (car in))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When a delete-object is called then it signals that the student removed a preexisting 
;; object or that they started to define an object and then changed their mind.  
;; When this occurs this code will test to see if an object matching the id or
;; label exists.  If it does then the hsvar will be removed and a string describing
;; the transaction will be echoed to the file.  If not then a string informing the 
;; user that the student began to define an object then stopped will be sent.
;;
;; NOTE:: For Occulted reasons the test is here to check if str is "".  If this is
;;  the case then the system will just respond with a stock line.  I have no expectiations
;;  of this code being exercised but I do not yet have the time to test it out now.
;;
;; Note:: I have chosen to comment out the str="" code for now.  If it gives me problems
;;  then I will reintroduce it at a later date.
(defun delete-object (stream lineno time in &optional (str ""))
  (declare (ignore Str Stream Lineno Time))
  (respond 0)
  ;; Start by labeling the delete-object arguments and then lookup
  ;; the variable itself to see if it does or does not exist.
  (let* ((Label (car in)) (id (cadr in))
	 (Var (or (and Label (find-hsvar-label Label))
		  (and ID (find-hsvar-id ID)))))
    (cond
     ;; If no Var exists then the student is not deleting a 
     ;; preexsiting object but was in the process of defining 
     ;; an object and then changed their mind.  In that case 
     ;; echo the comment to the file.
     ((null Var) 
      (format Nil "Student began to define an object and then changed mind.~%"))
     
     ;; If the Var exists then the student is deleting a 
     ;; preexisting variable and we need to remove it and
     ;; echo that to the file.  The way that we inform them
     ;; depends upon what information we can get from the var.
     ((hsvar-label Var) ;; Delete with label.
      (delete-hsvar-label (hsvar-label Var))
      (format Nil "Student deleted the object: '~a'.~%" (hsvar-label Var)))
     
     ((hsvar-ID Var) ;; Delete with ID (Only if no label exists).
      (delete-hsvar-ID (hsvar-ID Var))
      (format Nil "Student deleted the object: '~a'.~%" (hsvar-ID Var)))
     
     ;; It Should be impossible for the students to generate or
     ;; delete a variable with no label or id but just in case.
     (t (format Nil "Student deleted an unlabelled object.~%")))))


#|			
    (cond 
     (Label ;; Student labels.
      (setq Var (find-hsvar-label Label))
      (if 
      (delete-hsvar-label Label)
      (format Nil "Student deleted the object: '~a'.~%" Label))
     
     (ID ;; System ids.
      (setq Var (find-hsvar-id ID))
      (delete-hsvar-id ID)
      (format Nil "Student deleted the object: '~a'.~%" 
	      (if (HSVar-Label Var) (HSVar-Label Var) ID)))
     
     (t ;; The student is not deleting a predefined object. 
      (format Nil "Student began to define an object and then changed mind.~%")))))
|#
#|      ((equal Str "")
      
      
      (if Label (find-hsvar-label Label))
	
	  
	  (format nil "Student deletes object '~A'.~%"  Label)
	(format nil "Student deletes an object~%")))

     ;; If there is a Str then delete the appropriate var and 
     ;; Return a string informing the user. 
     ;; May need to use string for formatting.
     |#

#|	 
	(cond
     ((car in)
      (delete-variable (second in))
      (format nil "Student deleted object '~A'.~%" 
	      (subseq str (+ 1 (position #\Space str))
		      (position #\Space str :from-end 0))))
     ((find-variable (second in))
      (let ((tmp (find-variable (second in))))
	(delete-variable (second in))
	(format nil "Student deleted the object '~A'~%" tmp)))
     (t
      (format nil "Student began to define an object and then changed mind~%")))))
      |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If the student asks what is wrong with an object then print a string stating
;; that depending upon the value.
;;
;; As with other args, the Str "" trap is in place.

(defun why-wrong-object (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time))
  (respond 0)
  (setf **why-wrong-object-count** (+ 1 **why-wrong-object-count**))
  (let ((Label (car in)) (id (cadr in)) Tmp)
    (cond 
     ((equal Str "") ;; If the string is nil just print this.
      (if (car in)
	  (format nil "~%.~%          Student asks why '~A' is wrong.~%"  (car in))
	(format nil "~%.~%          Student asks why it's wrong.~%")))

     ((and Label ID) ;; If both a label and ID are present."
      (format nil "~%.~%          Student asks why '~A' is wrong.~%"  (car in)))
     
     (ID ;; If no label is present.
      (format nil "~%.~%          Student asks why it's wrong.~%"))
     
     ((setq Tmp (find-hsvar-label Label)) ;; shouild be axes only test it.
      (format nil "~%.~%          Student asks what's wrong with the axis ~A.~%"
	      (hsvar-str Tmp)))
     
     (t ;; The default.
      (format nil "~%.~%          Student asks what's wrong with that.~%")))))
     
      
#|
      
    
  (if (equal str "")
      (if (car in)
	  (format nil "~%.~%          Student asks why '~A' is wrong.~%"  (car in))
	(format nil "~%.~%          Student asks why it's wrong.~%"))
    (cond
     ((= (length in) 2)
      (if (car in)
	  (format
	   nil "~%.~%          Student asks what's wrong with '~A'.~%" 
	   (remove #\| (subseq str (+ 1 (position #\Space str))
			       (position #\Space str :from-end 0))))
	(format nil "~%.~%          Student asks what's wrong with that.~%")))
     (t
      (if (find-variable (car in))
	  (format nil "~%.~%          Student asks what's wrong with the axis ~A.~%"
		  (find-variable (car in)))
	(format nil "~%.~%          Student asks what's wrong with that.~%"))))))
	|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code recursively calls the define-variable code below that does the core work.  

(defun label-radius (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let ((Label (car in)) (id (cadr in)) (name (caddr in)))
    (define-variable-worker-routine Label Nil 'revolution-radius Name Nil Nil id)))
    
#| (if (null Label) ""
   (format
    nil "Student designates the radius of ~a the astronaut a radius called ~A attached to the ~(~A~).~%" 
     (first in) (third in))
      
  (if (car in)
      (format
       nil "Student designates the radius of ~a the astronaut a radius called ~A attached to the ~(~A~).~%" 
       (first in) (third in))
    ""))
    |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Since we have long-since eliminated motion diagrams therefore this code just displays
;; the arguments and returns.

(defun lookup-md (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (if (car in)
      (format nil "Student <LOOKUP-MD> ~(~A~).~%"  (car in))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We no longer support components in Andes but we do warn the student when they
;; try to make one.  This code just echoes the label to the screen.
(defun lookup-component  (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (if (car in)
      (format nil "Student <LOOKUP-COMPONENT> ~A~%"  (car in))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label angle is a little used command that allows the student to define a variable for the 
;; angle between two vectors.  It is accessed by selecting two vectors and then "drawing" an
;; arc between them.  The tool can only be accessed if the two vectors are selected and if 
;; origins are located "close enough" to each other.   The definition of 'close-enough' seems
;; to be debatable.  
;;
;; Label angle produces an angle between the vectors as such I will use the angle entryprop
;; to nlg this unless one of the choices is an axis.
;;
;; This code hwere will generate a var and perform the nlg as necessary.  

(defun label-angle (Stream LineNo Time In &optional (Str ""))
  "Handle a label-angle argument."
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let ((label (first in)) 
	(degrees (second in))
	(id-vector1 (third in)) 
	(id-vector2 (fourth in))
	(id-angle (fifth in)) 
	(axis (sixth in)))
    (if Axis 
	(hs0-label-angle-with-axis Label Degrees id-vector1 id-vector2 id-angle axis)
      (hs0-label-angle-between-vectors Label Degrees id-vector1 id-vector2 id-angle :drawn t))))


;;; This is the common case of label angle.  When the student labels an
;;; angle between vectors then the system will generate an entry for it
;;; using the two vectors and check it in the same way that all other
;;; entries are checked.  This code treats that as such by forming an
;;; entryprop for it and running that prop.
;;;
;;; In order to form the appropriate proposition we need to fetch the 
;;; specified vector terms from the symbol table.  Andrs does this from
;;; the symbol table in the helpsyste via the mag term.  I will just 
;;; look up the var terms driectly.  
;;;
;;; For reasons that are not exactly clear the code includes the 
;;; possibilyt of the "undrawn case" where the student is defining
;;; a variable.  This info is displayed as such.
(defun hs0-label-angle-between-vectors (Label Degrees vector1-var vector2-var id-angle &key (Drawn Nil))
  (declare (ignore Degrees))
  (let* ((v1-term (or (hsvar-label vector1-var) (hsvar-id vector1-var)))
	 (v2-term (or (hsvar-label vector2-var) (hsvar-id vector2-var)))
	 ;; Sort the vectors into canonical order for display.
	 ;;(vector-terms (sort (list exp-vector1 exp-vector2) #'expr<))
	 (vector-terms (list v1-term v2-term))
	 (angle-term `(angle-between ,@vector-terms))
	 (action      (if drawn `(angle ,@vector-terms)
			`(define-var ,angle-term))))
	     
    ;; Provided both of the vector terms are found return the string
    ;; appropriately nlg'd if not then return just one of them.  
    (cond 
     ((null v1-term) 
      (format nil "**Error** Unknown vector supplied to label angle ~a~%" vector1-var))
     ((null v2-term) 
      (format nil "**Error** Unknown vector supplied to label angle ~a~%" vector2-var))
     (t 
      (add-hsvar :ID ID-angle :label Label :Exp Action)
      (format Nil "Student defines ~a to be ~a~%" Label (nlg action 'nlg-entryprop))))))


;;; When the student defines an angle with an axis we want to form an appropriate
;;; listing for the variable.  If the degrees are known then it is treated as a
;;; variable for the angle.  This will always return t.  If not then a hint will 
;;; always be sent to the student informing them that the command is wrong so just
;;; echo a hack for now.  
;;;
;;; NOTE:: if it is necessary add the vector and axis ids to the printout.
;;;  For now I think that it can be safewly ignored.  
(defun hs0-label-angle-with-axis (Label Degrees id-vector1 id-vector2 id-angle axis)
  (declare (ignore id-vector2))
  (let ((Exp `(dnum ,degrees |deg|)))
    (cond 
     (Degrees ;; If the degrees are known form an entry.
      (add-hsvar :ID ID-Angle :Label Label :Exp Exp)
      (format Nil "Student defines ~a to be ~a.~%" Label (nlg Exp)))
     (t (format Nil (concatenate 'string 
		      "student erroniously defines ~a to be the "
		      "unknown angle between ~a and ~a~%")
		ID-Vector1 Axis)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The define angle variable command is an undocumented API that we use to define 
;; angle vars without drawing them.  This code forms the appropriate arguments and
;; then passes them to the hs0-label-angle-between-vectors.  
;;
;; This code has been copied (badly) from Help/Entry-Api.cl

(defun axis-codep (arg) ; arg should be symbol
  (member arg '(posx posy negx negy) :test #'sym-match))

(defun define-angle-variable (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time Str))
  (respond 0)
  (let* ((Label (car in)) (degrees (second in))
	 (label1 (third in)) (label2 (fourth in))
	 (id-angle (fifth in))
	 ;; Below this the code was copied from EntryAPI.cl
	 ;; It has been hacked quite badly to acheive the 
	 ;; desired behavior.
	 (si1 (if (axis-codep label1)  
		  (find-hsvar-unify-exp '(draw-axes ?dir))
		(find-hsvar-label (string label1))))
	 ;;(id1 (if si1 (hsvar-label si1)))
	 (si2 (if (axis-codep label2) 
		  (find-hsvar-unify-exp '(draw-axes ?dir))
		(find-hsvar-label (string label2))))	
	 ;;(id2 (if si2 (hsvar-exp Si2)))
	 (axis (first (or (axis-codep label1) (axis-codep label2)))))
    ;; then delegate to appropriate hander
    (if (not axis)			; angle between vectors: must specify not drawn for match
	(hs0-label-angle-between-vectors label degrees si1 si2 id-angle :drawn NIL) 
     (hs0-label-angle-with-axis label degrees si1 si2 id-angle axis))))

	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This appears to be a completely unused command since the motion diagrams 
;; are no longer implemented so I am going to leave it as-is.
(defun delete-md-object (stream lineno time in &optional (str ""))
  (declare (ignore Stream Lineno Time))
  (respond 0)
  (if (car in)
      (format
       nil "Student <DELETE-MD> ~A~%" 
       (remove #\| (subseq str (+ 1 (position #\Space str)) (position #\Space str :from-end 0))))
    ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is an access routine that is used to handle the variable definitions.  It calls the 
;; def-var-worker routine to handle the variable storage and definition as well as the 

(defun define-variable (stream lineno time in &optional (str ""))
  (declare (ignore Str Time LineNo Stream))
  (respond 0)
  (let* ((var (first in))
	 (type (second in)) 
	 (quant (third in)) 
	 (body (fourth in)) 
	 (time (fifth in))
	 (body2 (sixth in))
	 (id (seventh in))
	 (directionp (eighth in)))
    ;;; Call the define variable worker routine.
    (define-variable-worker-routine Var type quant body time body2 id directionp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is a generic worker routine that is used to generate and store variables as well as 
;; formatting them for output.  It will also add the specified hsvar to the *variables* list.
;;
;; This routine will be called by others apart from the define-variable routine when a 
;; generic variable entry needs to be produced.
(defun define-variable-worker-routine (Var type quant body time body2 id &optional directionp)
  (declare (ignore directionp))
  (let* ((quant-term (make-quant type quant body body2 time))
	 (action    `(define-var ,quant-term)))
    (add-hsvar :ID id :label Var :Exp Action)
    (format Nil "Student defines: ~a called ~(~a~)~%" (nlg Action 'nlg-entryprop) Var)))
#|	   
  (cond
   ((find-variable (nth 6 in))
    (add-variable (nth 6 in) (first in))
    (if (fifth in)
	(if (fourth in)
	    (format nil
		    "Student redefines the variable called '~A' to be the ~A for the ~A at time ~A~%"
		    (first in) (third in) (fourth in) (fifth in))
	  (format nil
		  "Student redefines the variable called '~A' to be the ~A at time ~A~%"
		  (first in) (third in) (fifth in)))
      (cond
       ((equal '|coef-friction| (third in))
	(format
	 nil
	 "Student redefines the variable called '~A' to be the ~A coefficient of friction between the ~A and the ~A.~%"
	 (first in) (if (second in) (second in) "") (fourth in) (nth 5 in)))
       ((equal '|mass| (third in))
	(format nil "Student redefines the variable called '~A' to be the mass of the '~A'.~%"
		(first in) (fourth in)))
       (t
	(if (fourth in)
	    (format nil "Student redefines the variable called '~A' to be the ~A on the ~A~%"
		    (first in) (third in) (fourth in))
	  (format nil "Student redefines the variable called '~A' to be the ~A~%"
		  (first in) (third in)))))))
   (t
    (add-variable (nth 6 in) (first in))
    (if (fifth in)
	(if (fourth in)
	    (format nil
		    "Student defines the variable called '~A' to be the ~A for the ~A at time ~A~%"
		    (first in) (third in) (fourth in) (fifth in))
	  (format nil
		  "Student defines the variable called '~A' to be the ~A at time ~A~%"
		  (first in) (third in) (fifth in)))
      (cond
       ((equal '|coef-friction| (third in))
	(format
	 nil
	 "Student defines the variable called '~A' to be the ~A coefficient of friction between the ~A and the ~A.~%"
	 (first in) (if (second in) (second in) "") (fourth in) (nth 5 in)))
       ((equal '|mass| (third in))
	(format nil "Student defines the variable called '~A' to be the mass of the '~A'.~%"
		(first in) (fourth in)))
       (t
	(if (fourth in)
	    (format nil "Student defines the variable called '~A' to be the ~A on the ~A.~%"
		    (first in) (third in) (fourth in))
	  (format nil "Student defines the variable called '~A' to be the ~A.~%"
		    (first in) (third in)))))))))
|#  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

