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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(in-package :cl-user)

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
	   `(unrecognized ,time-arg)) ;need error handler
	))

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
	((not (numberp dir-arg)) 
	 (warn "non-numeric direction arg:~A" dir-arg) 
	 `(unrecognized ,dir-arg)) ;an error handler can look for this
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
     (if (and stud-quant (compound-bodyp stud-quant)) stud-quant
       ; else treat as simple body name:
       (fix-body-name body-arg)))))

(defun fix-body-name (body-arg)
  "Convert atomic body name symbol read from WB arg to KB body symbol"
  ;; NB: can't use read-from-string to make symbol because now some body args 
  ;; from variable dialog are not really body names & might contain spaces 
  ;; (e.g. |all forces|).  Instead, intern in current package 
  ;; (to prevent Lisp printing with #:)
  (intern (string-upcase (substitute #\_ #\- (string body-arg)))))

;; Extract constituents from list-valued circuit quantity argument
(defun bodyterm-complist (bodyterm)
 "extract component list from a compound component term as built by arg-to-body"
 ; arg-to-body builds: (compound orderless C1 C2 ...)
 (cddr bodyterm))  

; For circuits, we sometimes have to handle defined compound component labels as arguments.
; E.g. say student variable "foo" has earlier been defined as (resistance (R5 R6)), meaning
; the resistance of the compound of R5 and R6. The workbench now allows "foo" to be used as a 
; name for the compound resistor in OTHER variable definitions.  For example, student may define 
; a variable for voltage-across with body 'foo. This is a form of overloading: "foo" officially 
; stands for the resistance of a compound component, but is overloaded to stand for the compound
; component itself. The body arg "foo" in *this* context will need ultimately to be translated to
; (R1 R2) get us to (voltage-across (R1 R2)) to match the form used in the knowledge base. 
;
; There are only certain contexts in which a compound equivalent label might occur as an
; argument and need to be expanded into its constituents -- right now it is only voltage quantities. 
; So arg-to-body itself does NOT automatically expand them.  (See bug 1478). Rather, the relevant 
; make-quant clause will use component-elements to look up the definition of "foo", 
; find (resistance (R5 R6)), and extract (R5 R6) when building the voltage term.  

(defun compound-componentp (quant)
"return true if quantity is a compound equivalent resistor or capacitor"
   (and (consp quant)
	(= (length quant) 2)
        (or (eq (first quant) 'resistance)
	    (eq (first quant) 'capacitance))
	(consp (second quant))))

(defun component-elements (comp-name)
"return elements of named component: constituents for defined compounds, else itself"
   (let ((stud-quant (symbols-referent (string comp-name))))
     (if (and stud-quant (compound-componentp stud-quant)) stud-quant
      ; else
         comp-name)))

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
;;  marks the corresponding system entry as "entered". defines a mass variable
;;  whose name has "m" concatenated to the given label. Enters into the symbol
;;  table this name paired with the system's name for the same quantity.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-object (entry)
  (let ((id (StudentEntry-id entry))
	 (text (StudentEntry-text entry))
	 (symbol (StudentEntry-symbol entry))
	 best
	 sysent
	body-term)

    ;; see comments in define-variable
    (setf best (best-matches 
		(pull-out-quantity symbol text)
		(mapcar #'(lambda (x) (cons (nlg (cadr (SystemEntry-prop x))) x))
			(remove '(body . ?rest) *sg-entries* 
				:key #'SystemEntry-prop :test-not #'unify))))

    ;; see comments in define-variable
    ;; Should have real help error handling for bad matching...
    (cond ((= (length best) 1)
	   (setf sysent (car best)))
	  ((= (length best) 0)
	   (error "Can't find any matches for ~A from ~A" text *sg-entries*))
	  (t (error "too many matches: ~A" best)))

    ;; Should used unify for this...
    (setf body-term (second (SystemEntry-prop sysent)))

    (setf (StudentEntry-prop entry) (SystemEntry-prop sysent))
    (add-entry entry)   ;remove existing info and update
    ;; for compound bodies, enter body label so it can be recognized when
    ;; referenced in subsequent quantity definitions for compound's attributes.
    (when (compound-bodyp body-term)
    ;; probaby should switch to whole SystemEntry or at least prop
      (check-symbols-enter symbol body-term id))

    (check-noneq-entry entry)))  ;finally return entry 

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
;;  marks the corresponding system entry as "entered". defines a mass variable
;;  whose name has "m" concatenateded to the given label. Enters into the sym-
;;  bol table this name paired with the system's name for the same quantity.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-compound-object (label names &optional time id)
      ;; can just pass along to assert-object. arg-to-body knows how to make
      ;; compound body term out of list argument.
      (check-noneq-entry (assert-object label names time id)))

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
(defun lookup-vector (entry)
  (let* ((id (StudentEntry-id entry))
	 (text (StudentEntry-text entry))
	 (symbol (StudentEntry-symbol entry))
	 (drawn-mag (StudentEntry-radius entry))
	 best sysent
	 ;; For now, these are all null, since they are done 
	 ;; as separate steps
	 given-mag given-xc given-yc given-zc
	 ;;
	 ;; defined after match
	 action vector-term vector-mag-term vector-dir-term
	 ;;
	 ;; In Andes3, there is no user-interface command to specify
	 ;; the direction as "unknown."  If the student happens to get
	 ;; the correct direction (if the direction can eventually be 
	 ;; calculated), then fine.  
	 ;; Otherwise, we check the drawn direction of the vector against 
	 ;; other vector and axis directions, and their opposites.
	 ;; If there is a match, provide unsolicited hint that another 
	 ;; direction should be chosen.  Something like,
	 ;; "The direction of F1 is unknown.  Drawing it in the direction
	 ;;  180 degrees suggests that it is parallel to the velocity of 
	 ;;  the ball.  Please choose another direction."
	 (dir (StudentEntry-angle entry))
	 (dir-term (arg-to-dir dir drawn-mag))
	 ;; xy plane vectors get theta prefix, z axis ones get phi
	 ;; Greek symbols expressed by $ + char position in symbol font
	 (dir-label (format NIL "~A~A" (if (z-dir-spec dir-term) "$j" "$q")
			    symbol)))

    ;; see comments in define-variable
    (setf best (best-matches
		(pull-out-quantity symbol text)
		(mapcar #'(lambda (x) 
			    (cons (nlg (cadr (SystemEntry-prop x))) x))
			(remove '(vector . ?rest) *sg-entries* 

    ;; See comments in define-variable
    (when (= (length best) 1)
      (setf sysent (car best)))
				:key #'SystemEntry-prop :test-not #'unify))))

    (setf action (SystemEntry-prop sysent))
    ;; dir-term could just as well be gotten from action...
    ;; how to we check the two?
    (setf vector-term (second action))
    (setf vector-mag-term `(mag ,vector-term))
    (setf vector-dir-term `(dir ,vector-term))

    (setf (StudentEntry-prop entry) action)
    ;; remove existing entry and update
    (add-entry entry)
    (check-symbols-enter symbol vector-mag-term id)
    (check-symbols-enter dir-label vector-dir-term id)

    ;; if any axes are defined must add all component variables as well
    (dolist (axis-sym (symbols-fetch '(axis ?xyz ?dir)))
      (let* ((axis-label (sym-label axis-sym))
             (axis-term  (sym-referent axis-sym))
	     (axis-entry-id (first (sym-entries axis-sym)))
             (compo-var  (format NIL "~A_~A" symbol axis-label))
	     (compo-term (vector-compo vector-term axis-term)) ; Physics-Funcs
	    )
        (check-symbols-enter compo-var compo-term (list id axis-entry-id))))

    ;; Different given values are handled in different ways:
    ;; 1. Direction value or unknown or zero-mag values get checked 
    ;; automatically as part of the vector entry proposition.  These continue 
    ;; to use the implicit equation machinery so as to record their equations 
    ;; as side effects, but not to check them. 
    ;; 2. For non-zero-mag vectors, given mag or compos handled via the given
    ;; equation mechanism, which checks their values.
    ;; We currently rely on drawn-mag argument to detect zero-mag vector, 
    ;; not :given-mag. 
    ;; This is OK because workbench updates drawn-mag if non-zero mag is 
    ;; specified.

    ;; !!! Now if student specifies values by components, workbench 
    ;; automatically sends dir as unknown.  This could be a problem if dir 
    ;; represented as known.

    ;; if vector is zero-length, associate implicit equation magV = 0
    ;; also add component eqns vc = 0 for all component variables in solution.
    (when (equal dir-term 'zero)
       (add-implicit-eqn entry (make-implicit-assignment-entry symbol 0))
       (dolist (syscomp (get-soln-compo-vars vector-term))
	 ;; skip make-implicit-assignment-entry since we have sysvar, 
	 ;; not studvar
	 (add-implicit-eqn entry (make-implicit-eqn-entry `(= ,syscomp 0)))))

    ;; if vector not zero-length, record given equations for any specified 
    ;; magnitude or component values.  Note given eqn maker takes student 
    ;; variable args.
    (when (not (equal dir-term 'zero))
       (when given-mag
	 (add-given-eqn entry (make-given-eqn-entry symbol given-mag 'given-mag)))  
       ;; Components in reverse order because add-given-eqn pushes at front. 
       ;; Want xc to wind up first so error there gets reported first.
       (when given-zc
           (add-given-eqn entry 
	       (make-given-eqn-entry (strcat symbol "_z") given-zc 'given-zc)))
       (when given-yc
           (add-given-eqn entry 
	       (make-given-eqn-entry (strcat symbol "_y") given-yc 'given-yc)))  
       (when given-xc
           (add-given-eqn entry 
	       (make-given-eqn-entry (strcat symbol "_x") given-xc 'given-xc))))
 
    ; if vector is a unit vector, associate implicit equation magV = 1 
    (when (eq (first vector-term) 'unit-vector)
        (add-implicit-eqn entry (make-implicit-assignment-entry symbol 1)))
    ; if direction is known, associate implicit equation dirV = dir deg.
    (when (degree-specifierp dir-term)          ; known xy plane direction
       (add-implicit-eqn entry (make-implicit-assignment-entry dir-label dir-term)))
    (when (and (z-dir-spec dir-term) 
               (not (equal dir-term 'z-unknown))) ; known z axis direction
       (add-implicit-eqn entry (make-implicit-assignment-entry dir-label (zdir-phi dir-term))))
   
    ;; Associated eqns will be entered later if entry is found correct.

    ;; finally return entry
     (check-noneq-entry entry)))

; fetch list of system vars denoting components of vector term
(defun get-soln-compo-vars (vector-term)
  (let ((compo-pattern (vector-compo vector-term '(axis ?xyz ?rot))))
   (mapcar #'qvar-var
     (remove-if-not #'(lambda (qvar) 
                           (unify (qvar-exp qvar) compo-pattern))
                    (problem-varIndex *cp*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-line -- check the correctness of a line drawn by the student.
;; argument(s):
;;  label: the line label
;;  body:  the ``body'' associated with the line.
;;  dir: angle of the line from horizontal (0->360 degrees) or a nega-
;;    tive number coding a z-axiz direction as follows (-1->out of plane; -2
;;    is into plane; -3 unknown but along z axis
;;  mag: length of line or nil if unspecified
;;  time: the time period during which the vector is constant. if nil and
;;    system is a student defined system, the time will be taken from
;;     the system definition
;;  id: id assigned to vector by the workbench
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  if the line is correct, the help system marks the corresponding system
;;  entry as "entered", defines the magnitude and direction variables, and
;;  enters the variables in the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lookup-line (entry)
  (let* ((id (StudentEntry-id entry))
	 ;; Needs to be determined from natural language 
	 time body-arg
	 ;;
	 (mag (StudentEntry-radius entry))
	 (dir (StudentEntry-angle entry))
	 (label (StudentEntry-symbol entry))
	 (body-term (arg-to-body body-arg))
	 (time-term (arg-to-time time))
	 ;; note dir may be dnum or 'unknown (and maybe into/out-of)
	 (dir-term (arg-to-dir dir mag 180)) ;lines defined mod 180 deg
	 (line-term `(line ,body-term :time ,time-term))
	 (action `(draw-line ,line-term ,dir-term)) 
	 ;; this defines magnitude and direction variables
	 (line-mag-term `(mag ,line-term))
	 (line-dir-term `(dir ,line-term))
	 ;; xy plane lines get theta prefix, z axis ones get phi
	 ;; Greek symbols expressed by $ + char position in symbol font
	 (dir-label  (format NIL "~A~A" (if (z-dir-spec dir-term) "$j" "$q")
			     label)))

    (setf (StudentEntry-prop entry) action)
    ;; remove existing entry and update
    (add-entry entry)
    (check-symbols-enter label line-mag-term id)
    (check-symbols-enter dir-label line-dir-term id)
    
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
    (check-noneq-entry entry)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun label-angle (label degrees id-vector1 id-vector2 id-angle &optional axis)
  ; if angle between vectors then have to make entry and check for match.
  ; if angle between vector and axis, then just install it in symbol table
  ; as label for appropriate angle expression.  Dispatch based on angle type:
 (check-noneq-entry
  (if axis 
      (on-angle-with-axis label degrees id-vector1 id-vector2 
                               id-angle axis)
      (on-angle-between-vectors label degrees id-vector1 id-vector2 id-angle :drawn T))))

;; Worker routine to handle entry of term for angle between vectors.  
;; "Drawn" argument specifies if drawn as diagram entry or defined as variable
;;  Either way this should match a solution graph entry.
(defun on-angle-between-vectors (label degrees id-vector1 id-vector2 id-angle &key drawn) 
   (declare (ignore drawn)) ; AW: no longer used
   ;; need to map entry id to referent vectors. Note this may now be used for 
   ;; drawn line entries as well.  Code should work without change, because 
   ;; lookup-line enters line label as symbol for (mag (line ...)) 
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
   (check-symbols-enter label angle-term id-angle)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun axis-codep (arg) ; arg should be symbol
  (member arg '(posx posy negx negy) :test #'sym-match))

(defun define-angle-variable (label degrees label1 label2 id-angle)
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
    (check-noneq-entry
     (if (not axis) ; angle between vectors: must specify not drawn for match
	 (on-angle-between-vectors label degrees id1 id2 id-angle :drawn NIL) 
	 (on-angle-with-axis label degrees id1 id2 id-angle axis)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-variable - define a variable to stand for a certain quantity. this is
;;  called when the student uses the "variable" menu, but not when variables are
;;  defined by other tools such as the force drawing tool or the body tool.
;; returns: StudentEntry
;; note(s):
;;  if the variable definition is correct, marks the corresponding system entry
;;  as "entered" and enters the student's variable name into the symbol table
;;  paired with the corresponding system variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun define-variable (entry)
  (let ((id (StudentEntry-id entry))
	(text (StudentEntry-text entry))
	(symbol (StudentEntry-symbol entry))
	body-term
	value ;not in Andes3
	best sysent)

    (unless text (warn "Definition must always have text")
	       (setf text ""))
 
    ;; match up text with SystemEntry
    ;; Right now, this is a simple first effort:
    ;; Here is what is missing:
    ;;  Should tokenize the text and the model texts and do
    ;;     combination of tree matching and minimum edit distance
    ;;     for tree leaves.
    ;;  Should include "bad" quantity definitions in matching.
    ;;  Should have minimum acceptable match cutoff.
    ;;  Should have criteria for almost equivalent matches.
    ;;  Should have alternative texts for matching.
    ;;  Should correctly handle "Let xxx be ... " wrapper, matching to 
    ;;     symbol from client.
    ;;  Should have something to handle extra stuff like setting
    ;;     given values in definition.  (either handle it or warning/error).
    (setf best (best-matches
		(pull-out-quantity symbol text)
		(mapcar #'(lambda (x) 
			    (cons (nlg (cadr (SystemEntry-prop x))) x))
			(remove '(define-var . ?rest) *sg-entries* 
				:key #'SystemEntry-prop :test-not #'unify))))

    ;; Need error handlers for the following cases:
    ;; no match:  "Cannot understand entry" and hint sequence
    ;; two matches:  "Did you mean this or this?" and hint sequence.
    ;; more than two:  "Your definition is ambiguous"" and hint sequence.
    (when (= (length best) 1)
      (setf sysent (car best)))

    ;; this is almost certainly wrong for some variables ....
    (setf body-term (second (SystemEntry-prop sysent)))

    (setf (StudentEntry-prop entry) (SystemEntry-prop sysent))
    ;; install new variable in symbol table
    (add-entry entry)
    ;; probably should switch to whole SystemEntry
    (check-symbols-enter symbol body-term id)

    ;; record associated given value equation entry
    (when value  ; NIL => unspecified. (Empty string => unknown)
      (add-given-eqn entry (make-given-eqn-entry symbol value 'value)))
    ;; NB! make-given-eqn-entry can return NIL if no system var found for 
    ;; studvar.
    ;; Normally means var def will be incorrect. No given-eqn added in this case.
    ;; But maybe better have a dangling given eqn entry anyway?

    ;; finally return entry 
    (check-noneq-entry entry)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-x-axis (entry)
  ;; workbench doesn't associate axes with bodies yet, so we leave this
  ;; out of the entry proposition we match (see KB/Ontology.cl).
  ;; !! need to make sure axis between zero and 90 and canonicalize if
  ;; not
  (let* ((dir (StudentEntry-angle entry))
	 (id (StudentEntry-id entry))
	 (x-label (StudentEntry-x-label entry))
	 (y-label (StudentEntry-y-label entry))
	 (z-label (StudentEntry-z-label entry))
	 (action  `(draw-axes ,dir)) ; dir is naked degree value
	 (x-term  `(axis x ,dir))
	 (y-term  `(axis y ,dir))
	 (z-term  `(axis z ,dir))
	 (xdir-dnum `(dnum ,dir |deg|))
	)

    (setf (StudentEntry-prop entry) action)
   ;; install symbols for x, y, and z axes
   ;; these can't be used by themselves in equations but are needed by us
   ;; later when autodefining vector component variables for existing axes. 
   ;; They would also be needed for referring to the axes by label in help 
   ;; messages if there is more than one set of axes.
   (add-entry entry)
   (check-symbols-enter x-label x-term id)
   (check-symbols-enter y-label y-term id)
   (check-symbols-enter z-label z-term id) 

   ;; automatically define thetaX as label for direction of positive x-axis
   (check-symbols-enter (strcat "$q" x-label) xdir-dnum id xdir-dnum)

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
        (check-symbols-enter compo-var compo-term (list id vector-entry-id)))))

  ; finally return entry
   (check-noneq-entry entry)))

;-----------------------------------------------------------------------------
;; for processing deletions of entries
;-----------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo-entry -- undo all state effects of a particular Student entry
;; Argument: enty -- the StudentEntry to be undone.
;;
;; Note: This worker routine is automatically invoked by the delete-object 
;; function to cleanup state on removal of an entry from the entry list -- 
;; a bit like a C++ destructor for student entries.  That ensures that
;; entry effects are always undone when a student entry is removed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun undo-entry (entry)
  ;; undo entry effects specific to correct entries:
  (when (equal (StudentEntry-state entry) **correct**)
	;; unmark entry interpretations as done in solution graph
  	(sg-delete-StudentEntry entry)
        ;; undo any implicit eqn entry associated with this
  	(dolist (ie (StudentEntry-ImplicitEqns entry))
           (undo-entry ie))
        ;; undo any given eqn entry associated with this
	(dolist (ge (StudentEntry-GivenEqns entry))
	  (when (not (blank-given-value-entry ge))
	    (undo-entry ge))))

  ;; remove all labels dependent on it from symbol table
  (symbols-delete-dependents (StudentEntry-ID entry))

  ;; special to equation entries: remove from algebra system
  (when (or (equal (first (StudentEntry-prop entry)) 'eqn)
            (equal (first (StudentEntry-prop entry)) 'implicit-eqn))
	(undo-eqn-entry entry))
)



;;----------------------- Checking Entries ----------------------------------
;; 
;; Following functions implement the non-eqn portion of the
;; EntryInterpreter. EntryInterpreter for equations is in another file

;;
;; check-symbols-enter -- make an entry in the symbol table of student
;;                        variables, checking for redefinition.
;;
;; This takes the same arguments as symbols-enter in symbols.cl, but
;; wraps error checking around it. In case of symbol definition error, 
;; the entry is tagged incorrect with the appropriate error interpretation.
;; NB: if entries is not a singleton, the error is hung on the first
;; one, which should be the principal entry being evaluated
(defun check-symbols-enter (label referent entry-ids &optional sysvar)
   (cond ((symbols-lookup label) ;; variable already defined!
      ;; find entry. entry-ids arg may be atom or list, but should not be nil
      (let ((entry (find-entry (if (atom entry-ids) entry-ids (first entry-ids))))
	     rem)
	  ;; build the error remediation turn
	  (setf rem (make-hint-seq (list
		 (format nil "The variable ~A is in use to define ~A. Please choose a different label." 
		         label (nlg (symbols-referent label))))))
	  (setf (turn-coloring rem) **color-red**)
	  ;; set state of entry and attach error. But only do if not done already, so 
	  ;; only report on the first error found.
	  (unless (studentEntry-ErrInterp entry)
	     (setf (studentEntry-state entry) 'incorrect)
             (setf (studentEntry-ErrInterp entry)
       	          (make-ErrorInterp :diagnosis '(variable-already-in-use)
                                    :remediation rem)))))

	  ;; else no conflict: just make the definition
          (T (symbols-enter label referent entry-ids sysvar))))

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

  ;; special case: Entry-API handler can attach an error interp for certain
  ;; errors such as symbol redefinitions that prevent the entry from
  ;; being processed further. Return appropriate turn with unsolicited 
  ;; error message in this case.
  (when (studentEntry-ErrInterp entry) 
    (return-from Check-NonEq-Entry 
                     (ErrorInterp-remediation (studentEntry-ErrInterp entry))))

  ;; else have a real student entry to check
  (format *debug-help* "Checking entry: ~S~%" (StudentEntry-prop entry))
  (let (cand		; candidate (state . interpretation) pair
        match 		; correct system entry matched
        result) 	; final result to return
    
    ;; Special for vector entries: If any given eqns are set, ensure the 
    ;; form of the givens is correct before we do any other checking. 
    ;; This avoids bad error handlers when source is now really wrong form.
    ;; Testing for the presence of GivenEqns is just to make this backwards 
    ;; compatible with old logs which never set any.
    (when (and (eq (first (StudentEntry-prop Entry)) 'vector)
               (StudentEntry-GivenEqns Entry))
        (setf result (Check-Vector-Given-Form Entry))
	(when (not (eq (turn-coloring result) **color-green**))
	    (return-from Check-NonEq-Entry result))) ; early exit

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
        (format *debug-help* "No matching system entry found~%")
	(setf (StudentEntry-state entry) **Incorrect**)
	; run whatswrong help to set error interp now, so diagnosis
	; can be included in log even if student never asks whatswrong
        (diagnose Entry)
	(setf result (make-red-turn :id (StudentEntry-id Entry)))
	;; log and push onto result list.
	(setf (turn-result result)
	      (append (log-entry-info Entry) (turn-result result)))
	(return-from Check-NonEq-Entry result)) ; go no further

    ;; else got a match: set state and correctness from candidate
    (setf (StudentEntry-State entry) (car cand))
    (setf (StudentEntry-CInterp entry) (cdr cand))
    (setf match (first (StudentEntry-CInterp entry)))
    (format *debug-help* "Matched ~A system entry: ~S~%" (car cand) match)

    ;; decide what to return based on major state of entry
    (case (StudentEntry-State entry)
     ('correct 
      ; check any given value equations associated. At first one that is wrong, its
      ; result turn becomes the result for the main entry; checking routine 
      ; updates main entry record with error interp of the bad equation.
       (dolist (e (StudentEntry-GivenEqns entry))
        (let ((result (Check-Given-Value-Entry entry e)))
          (when (not (eq (turn-coloring result) **color-green**))
	    (return-from Check-NonEq-Entry result))))

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
      (setf result (make-green-turn :id (StudentEntry-id entry))))

     ;; give special messages for some varieties of incorrectness:
     ('Forbidden (setf result (chain-explain-more **Forbidden-Help**)))
     ('Premature-Entry 
     		 (setf result (chain-explain-more **Premature-Entry-Help**)))
     ('Dead-Path (setf result (chain-explain-more **Dead-Path-Help**)))
     ('Nogood' (setf result (chain-explain-more **Nogood-Help**)))
     (otherwise (warn "Unrecognized interp state! ~A~%" 
                        (StudentEntry-state entry))
                 (setf result (make-red-turn :id (StudentEntry-id entry)))))

    ;; Note for variables with given equations, we are
    ;; logging correctness of the variable definition substep, 
    ;; but the given value substep, hence whole entry, 
    ;; might still be wrong.

    (setf (turn-result result)
	  (append (log-entry-info Entry) (turn-result result)))

    ;; finally return result
    result))



(defun sol-gives-magdir (vector-quant)
"non-nil if magnitude or direction or vector are given in current problem"
  ; fetch vector entry prop to see if drawn with known direction
  (let ((vector-entry (sg-find-vector-entry vector-quant)))
    ; true if either drawn non-unknown... 
    (or (and vector-entry    ; prop form is (vector ?quant ?dir)
             (not (eq (third (SystemEntry-prop vector-entry)) 
	               'unknown)))
	; ... or mag has a given value
        (given-quant-p `(mag ,vector-quant)))))

(defun sol-gives-component (vector-quant)
"non-nil if some component of vector is given in current problem"
  ; !!! 0 rotation assumes given compos always along standard axes. 
  ; True for now but could change in future
  (some #'given-quant-p
	 `((compo x 0 ,vector-quant)
	   (compo y 0 ,vector-quant)
	   (compo z 0 ,vector-quant))))

 ; Code below uses 'compo and 'magdir as form codes
(defun sol-has-givens (vector-quant form)
 "true if solution has given values of specified form"
 (if (eq form 'compo) (sol-gives-component vector-quant)
   (sol-gives-magdir vector-quant)))

(defun other-form (form)
   (if (eq form 'compo) 'magdir 'compo))

(defun compo-assignment-p (entry)
  "true if given student eqn entry states value of a component variable" 
    (and (eq (first (StudentEntry-prop entry)) 'eqn)
         (componentp  (symbols-referent (second (StudentEntry-prop entry))))))
	
;; Give special message if student chooses wrong form (mag/dir vs.
;; components) for given values of vector attributes.
(defun Check-Vector-Given-Form (entry)
"check given values sent with a vector entry"
  (let ((stud-form   (if (some #'compo-assignment-p
                                (StudentEntry-GivenEqns entry)) 'compo
	               'magdir))
	(vector-quant (second (StudentEntry-prop entry))))
    ; Check for mismatch in form of givens.
    ; NB: if system gives nothing, either form should be OK for unknown.
    ; A student's form is incorrect iff no attribute of the student form 
    ; is given AND some attribute of the other form is given. 
    (when (and (not (sol-has-givens vector-quant stud-form))
               (sol-has-givens vector-quant (other-form stud-form)))
          (format T "mismatch: student form: ~A system form: ~A~%" 
                     stud-form (other-form stud-form))
          ; flag main entry as wrong and fill in error interp
          (setf (StudentEntry-state entry) **Incorrect**)
	  (return-from Check-Vector-Given-Form
	   (if (eq stud-form 'compo) (should-be-magdir-form entry vector-quant)
	     (should-be-compo-form entry vector-quant)))))

   ; get here=> no errors. Signal with green turn
   (make-green-turn :id (StudentEntry-id entry)))

(defun should-be-compo-form (se quant)
  (declare (ignore quant))
  (let ((rem (make-hint-seq
	      (list (format nil "Use component form for this vector.")))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(should-be-compo-form)
       ; unclear what intended should be
       ; :intended (get-given-interp quant)
       :remediation rem))
    (setf (turn-coloring rem) **color-red**)))

(defun should-be-magdir-form (se quant)
  (declare (ignore quant))
  (let ((rem (make-hint-seq
	      (list (format nil "Use mag/dir form for this vector.")))))
    (setf (StudentEntry-ErrInterp se)
      (make-ErrorInterp
       :diagnosis '(should-be-magdir-form)
       ; unclear what intended should be, maybe eqn for one given compo
       ; :intended (get-given-interp `(compo x 0 ,quant))
       :remediation rem))
    (setf (turn-coloring rem) **color-red**)))

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
  ; don't waste time adding info when checking init entries 
  ; ?? might we want it anyway ??
  (when (and entry (not **checking-entries**))
     (do-log-entry-info entry)))
  
(defun do-log-entry-info (entry)
  (let (result (target-entries)
	       (parse (StudentEntry-ParsedEqn entry)))
    ;; fetch target entry list for correct or incorrect entries 
    (cond ((eq (StudentEntry-state entry) **Incorrect**)
	   ;; if needed, run whatswrong help to set error interp now, so diagnosis
	   ;; can be included in log even if student never asks whatswrong
	   (unless (StudentEntry-ErrInterp entry) (diagnose Entry))
	   (setf target-entries (ErrorInterp-Intended (StudentEntry-ErrInterp Entry))))
	  
	  ((eq (StudentEntry-state entry) **correct**)
	   (setf target-entries (studententry-Cinterp entry))))
    
    ;; OK, do the logging
    (let ((*print-pretty* NIL)) ;no line breaks when formatting cmd strings!
      
      ;; log the parse if we have it.  Note for syntax errors it may be list 
      ;; containing partial parse tree printed as (#S(PARSE :TREE (LM m) :REM =x8*/7))
      ;; print parse of ? for this
      (when (consp parse)  ; non-NIL => either prefix eqn or list of parse trees
	(push `((:action . "log") 
		(:parse . ,(format nil "~S" 
				   (if (eq (type-of (first parse)) 
					   'parse) '? parse))))
	      result))
      
      ;; For non-eq entries, show entry prop in our notation, so we can 
      ;; identify common errors.   For correct non-eq entries, it will be 
      ;; the step, but for errors we add it.
      (when (and (not (eq (first (studentEntry-prop entry)) 'eqn))
		 (eq (StudentEntry-state entry) **Incorrect**))
	(push `((:action . "log") 
		(:entry . ,(format nil "~S" (studentEntry-prop entry))))
	      result))
      
      ;; log the error tag if one was found
      (when (StudentEntry-ErrInterp entry)
	(push `((:action . "log") 
		(:error-type . ,(format nil "~S" 
					 (ErrorInterp-name 
					  (StudentEntry-ErrInterp Entry)))))
	      result))
      
      ;; log the target entry info if we have any. This shows 
      ;; pairs of opnames and entry props "steps."
      (when target-entries
	(push `((:action . "log") 
		(:assoc . ,(mapcar #'opname-prop-pair target-entries)))
	      result)))

    result))

(defun opname-prop-pair (x)
  (cons (sg-map-SystemEntry->opname x) 
	(format nil "~s" (SystemEntry-prop x))))


; following does the work of entering an implicit entry associated with
; a principal entry, either an implicit equation or an implicit variable
; entry, assuming the principal entry is correct. Arg is implicit entry.
(defun enter-implicit-entry (entry)
   (cond ((eq (first (studentEntry-prop entry)) 'implicit-eqn) 
	  ;; enter an implicit equation defined by correct non-eqn entry
          (enter-subentry-eqn entry (second (StudentEntry-prop entry))))
	 (T ; other type, i.e. auto mass variable
	    (when (sg-match-studententry entry)  ; correct
	      (setf (studententry-cinterp entry) 
	          (cdr (first (studententry-PossibleCinterps entry))))
	      (setf (studentEntry-state entry) **correct**)
	      (sg-enter-StudentEntry entry)))))

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
     (setf (StudentEntry-State eqn-entry) **correct**)
 
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
	((= 0 Value) (delete-object ID)
	             (make-black-turn :id id))
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
	(format nil "No problem step found for button labelled ~a" ID)))
     
      ;; If this is not a non-quant psm then we also need to thro an error 
      ;; asserting that fact. 
      ((not (enode-has-mark? PSM 'non-quant))
       (make-bad-problem-turn 
	(format nil "Unmarked enode matching non-quant IDNum ~a ~a" PSM ID)))
     
      ;; Otherwize test to see if it present and behave appropriately.
      (t    (add-entry Entry)  ; save the entry
	    (cond ((psmg-path-enteredp (enode-path PSM))
		   (setf (StudentEntry-state entry) **CORRECT**)
		   (make-green-turn :id (StudentEntry-id entry)))
		  (T (setf (StudentEntry-state entry) **INCORRECT**)
		     (make-red-turn :id (StudentEntry-id entry))))))))



;; If this is a true multiple-choice answer problem then we will be passed an
;; answer-id of the form "MC-#".  The code below will handle this appropriately.
;; Given an id of the form "MC-#" and a numerical value this code will produce
;; a non-eqn entry of the appropriate form, and then enter it before passing it
;; back to the check-noneq-entry code for testing.  
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
