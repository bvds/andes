;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operators.cl
;; Collin Lynch
;; 10/26/2000
;;
;; This file defines a struct called operators for use with the Andes2 system.
;; These are standard strips-style operators with the addition of three-tier
;; hinting for use with the Andes system.  
;;
;; These operators can be loaded for use in any strips-style planning system
;; and are inteded to support other similar projects to Andes2.
;;
;; The operators are indexed using a pair of hashtables and are meant to 
;; be accessed using only those functions that have been labelled "public."
;; In the future packaging will make this more explicit.
;;
;; The Operator's Fields are:
;;  name: A unique name that will be used to index the operator for searching.
;;
;;  arguments:  A list of argument variables that will be used in defining 
;;    the Operator's S-expression for later use.  This is stored in the 
;;    stack for loop-prevention at runtime.
;;
;;  preconditions:  A list of preconditions that must be true before this 
;;    operator can fire.  These will be tested by the system at runtime
;;    as goals before the operator itself is fired.  
;;
;;  effects:  The effects that this operator has when fired.  At problem
;;    solution time the system will test these goals to determine which
;;    operator should be fired.  If the operator succeeds in being fired 
;;    then these effects (with variables bound) will be added to the working
;;    memory.  
;;  
;;  hint: This is a list of hints that will be used at runtime to guide the 
;;    student along.  The list is of the form ((<type> <Hints>)...)  where
;;    <Type> is one of [Point, Teach, Apply, Bottom-Out] and describes the
;;      specificity of the hints within it.
;;    <Hints> are list of operator hints of the form (<OpHintType> <Args>)
;;      And will be evaluated as necessary to provide feedback.
;;
;;  HintVars:  A listing of all the variables used in the hints.  These will
;;    Be used later for forming the hints at runtime.  
;;  
;;  specifications:  A Comment-ish stringdescribing the operator itself and
;;    later useful for documentation.
;;
;;  Variables:  A list of all the variables that appear in this operator 
;;    taken from the hints, args, preconditions, effects, etc.  This list
;;    will be stored later with the bindings for runtime use.
;;  
;;  Features:  A list of operator features e.g. PSM, and 'Ordered' which are
;;    used for specialized filtering. 
;;              
;;  CogLoad:  At present this is unused.  It defines the "Cognitive Load"
;;    (Difficulty) of the operator and, in-turn the likelyhood that the 
;;    students would be to use it.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SAMPLE
;;
;; (defoperator write-projections (?compo-vars)
;;  :features (unordered)
;;  :specifications "
;;   If the goal is to write extra equations for component variables,
;;   then create subgoals, one for each, to write the equations,
;;   and remember what the expressions corresponding to each variable is."
;;  :preconditions  ((map ?var ?compo-vars
;;	              (eqn (= ?var ?expr) (projection ?var))
;;                    ?expr ?compo-exprs))
;;  :effects ((projections ?compo-vars ?compo-exprs))
;;  :hint
;;   ((point (string 
;;            "Your next step should be to write equations for each of the 
;;             component variables that express the variable in simpler form, 
;;             as an expression of magnitude and direction variables." ))
;;    (bottom-out (string
;;	           "You need to write projection equations for the components 
;;                 you will use."))))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At problem solution time, the system will backward-chain using the operators.  
;; Given the current sought it will select the subset of operators containing 
;; effects that match it.  For each operator it will split the search and attempt
;; to solve the preconditions.  If an operator succeeds it will move on etc.
;; The Operator structs will become the basis of the solution graphs and will be 
;; stored for later use along with their variable values.  These values will then
;; be used to provide hints at runtime.


;;=======================================================================
;; Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *Operators-By-Effect*
;; This parameter contains a global hashtable that we will use to store 
;; and access the operators at runtime.
;; When operators are defined they are registered with the *Operators*
;; hastable for future use.
;;
;; Since we will not be adding operators at runtime we can trade addition
;; cost for access speed.

(defparameter *Operators-by-Effect* (make-hash-table :test #'equalp
						     :size 20
						     :rehash-size 5
						     :rehash-threshold .8)
  "The hashtable where we will index operators by their effects for searching.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *Operators-by-name*
;; A hashtable in which we will store operators by name for efficient selection
;; when necessary.

(defparameter *Operators-By-Name* (make-hash-table :test #'equalp
						   :size 20
						   :rehash-size 5
						   :rehash-threshold .8)
  "A hashtable in which we will store the operators by their names for efficient access.")


;;========================================================================
;; Operator Struct
;;
;; The operator struct defines a strips based operator which is defined
;; by a unique name, a set of arguments, a list of preconditions and a
;; list of effects.
;;
;; In the Andes solution system these operators are used in a goal-first
;; manner.  However they can be employed in any reasonable strips system.
;; 
;; Future versions of this structure will contain help strings for the 
;; Andes/Atlas help systems.
;;
;; Note:  The definition of this structure includes the creation of 
;; operator-p to test for structures.  hence the need for a different
;; name for the s-expression form.

(defstruct (operator (:print-function print-operator))
  name             ;;Unique Operator name.
  arguments        ;;List of operator arguments for use in defining the operator s-expression.
  preconditions    ;;The preconditions for this operator to occur.

  effects          ;;The operator's effects.
  hint             ;;The hint field.
  HintVars         ;; Argument elements used to compare two opapps at help time for use by 
                   ;; the comparison system.  
  
  specifications   ;;The operaor specifications.

  Variables        ;;A list consisting of all the variables in this operator.
  
  Features         ;;A list of operator features e.g. PSM, and Ordered which
                   ;;are used to determine aspects of the operator.
  
  (CogLoad 1 :type real)      ;;The 'cognitive load' of this operator used for min searching.
                              ;; Needs to be edded into the loop.
  
  )


;;----------------------------------------------------------------------
;; print-operator
;; Pretty-print the specified operator to the specified stream.
;;
;; At the moment this is incomplete.
;;

(defun print-operator (Operator Stream Level)
  "Print out the specified operator."
  (pprint-Indent :block Level)
  (format Stream
	  "(~A ~A)"
	  (operator-name Operator)
	  (operator-arguments Operator)))


;;-----------------------------------------------------------------------------
;; Operator Definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defoperator (public) 
;; define an operator with the given values and add
;; it to the *Known-Operators* list.
;;
;; Arguments:
;;  Name:  The operator name.
;; 
;; &key
;;  Arguments:  The operator Arguments.
;;  Precondintions:  The operator preconditions.
;;  Effects:  The operator's effects.
;;
;; Returns: A new operator with the given values.
;;

(defmacro defoperator (Name Arguments 
		       &key Preconditions Effects 
			    Hint Specifications Features
			    Load)
  
  "Define a new operator with the specified values and add it to *operators*."
  
  (let ((Op (eval `(make-operator	;Produce the operator struct.
		    :Name ',Name 
		    :Arguments ',Arguments 
		    :Preconditions ',Preconditions 
		    :Effects ',Effects
		    :Hint ',(subst-nlgs-hints Hint) ;Substitute the NLG functions into the system.
		    :Specifications ',Specifications
		    :CogLoad ',(if Load 
				   Load
				 1)))))
    
    (if (not (list-of-atoms-p Features)) ;Ensure that the features list is valid.
	(error "The specified Features list for ~A is invalid ~A." ;if not signal an error. 
	       Name Features) 
      (setf (Operator-Features Op) Features)) ;otherwize set the features.
	
                                                                 
    (setf (Operator-Variables Op)	;Set the variables list.
      (get-operator-variables Op))

    (setf (Operator-hintVars Op)	;Set the hintvars list.
      (get-operator-hintvars Op))

    (Register-operator Op)		;Store the operator struct.
    
    Op))				;and return it.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst-nlg-funcs
;; The assumption made by the hint system is the operator hint variables
;; need to be nlg'd at helptime.  The nlg function itself is located 
;; elsewhere.  This function takes a hint spec of the form:
;;            (<type> <specs>)
;;            where specs are of the form.
;;            (<Type> <Format Str> <Vars>)
;; where each var is either an atom such as ?body or a list such as 
;; (?body 'defnp) and substitutes a 'nlg onto the fromt of all lists
;; or replaces atoms with lists (nlg ?body).
(defun subst-nlgs-hints (hints)
  "Subst the nlgs into the hints."
  (loop for h in hints
      collect (cons (OpHint-type h) 
		    (subst-nlgs-specs (Ophint-Hintspecs H)))))
  
(defun subst-nlgs-specs (Specs)
  (loop for S in Specs
      collect (append (var-free-hintspec S)
		      (subst-nlgs-specvars (HintSpec-Vars S)))))

(defun subst-nlgs-specvars (Vars)
  (mapcar #'(lambda (v) 
	      (if (listp v)
		  (cons 'nlg v)
		(list 'nlg v)))
	  Vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-operator-variables
;; Obtain a list of all the unique variables in this operator.
;; When the operator is defined this list is used to initialize the 
;; Variables field.
;;
;; Arguments: Op: The operator to be polled.
;; Returns:  A unique list of all unique variables in the operator.

(defun get-operator-variables (Op)
  "Obtain a list of all variables in this operator."
  (remove-duplicates                                             ;;Cull the copies from the results and return.
   (append (Operator-arguments Op)
	   (variables-in (Operator-preconditions op))
	   (variables-in (Operator-Effects Op)))))


(defun get-operator-hintvars (Op)
  "Obtain a list of all variables in this operator."
  (remove-duplicates                                             ;;Cull the copies from the results and return.
   (append (Operator-arguments Op)
	   (variables-in (Operator-hint OP)))))


(defun bind-operator-vars (op vals)
  "Match the operator with the values."
  (or (if (null vals) no-bindings) 
      (unify (operator-variables op) vals)
      (unless  (and (null vals) (null (operator-variables op)))
	(format t "Operator variables don't match operator values: ~a~%    May need to regenerate problem file.~%    operator-variables:  ~A~%    values:  ~A~%" 
		(operator-name op) (operator-variables op) vals)
	nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resgister-operator
;; Index the specified operator in the *Operators-by-Effect* and *Operators-by-name*
;; 
;; Arguments: Op: The operator being indexed.
;;
;; Returns: The operator after indexing (unchanged)

(defun register-operator (Op)
  "Index the specified operator in *Operators-by-Effect* and *Operators-by-name*"
  (dolist (Eff (Operator-Effects Op))                                             ;;for each effect Eff of the operator.   
    (if (not (member Op (gethash (car Eff) *Operators-by-Effect*))) ;;If Op is not already associated with
       	                                                            ;;the car of Eff in *Operators-By-Effect*
	(push Op (gethash (car Eff) *Operators-By-Effect*))))                     ;;then associate it.
  (setf (gethash (Operator-name Op) *Operators-By-Name*) Op)                      ;;Index the operator by name.
  Op)                                                                              ;;return the operator.





;;---------------------------------------------------------------------------------
;; Operator selection
;; These functions are called at runtime by the problem solver and the 
;; help system to retreive the operators for solution or help queries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-operator-by-name (public)
;; Get the specified operator by its name.
;;
;; Arguments: Opname: The operator name.
;; Returns: The Operator iff it is indexed by name in *Operators-by-name* 
;;          or nil if it is not.

(defun get-operator-by-name (opname)
  "Get the specified operator by name or nil if it does not exist."
  (gethash Opname *Operators-By-Name*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-operators-by-effect (public)
;; get the specified operators that have effects of the specified predicate type.
;;
;; Arguments: Predicate: The predicate e.g. 'Variable' that we are seaching for.
;; Returns: A list of operators who have at least one effect of the specified type
;;          ornil if noe exist.

(defun get-operators-by-effect (Predicate)
  "Obtain a list of operators that have an effect of the specified predicate type or nil if none exist."
  (gethash Predicate *Operators-By-Effect*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-operator-by-tag
;; given an operator tag (<Name> <args>) get the
;; operator that matches it.
(defun get-operator-by-tag (tag)
  (get-operator-by-name (car tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clear-Ops (public)
;; Clear the operator indexes.

(defun clear-ops ()
  "Clear the *Operators-By-Effect* and *Operator-By-Name* indexes."
  (clrhash *Operators-By-Name*)
  (clrhash *Operators-By-Effect*))





;;-----------------------------------------------------------------------------
;; Operator-var-copy (public)
;; In order to avoid unconcious overwriting and to permit recursion this 
;; function systematically replaces all variables in an operator with a 
;; matching renamed set.

(defun operator-var-copy (Operator)
  "Obtain a copy of Op with the vars changed."

  (let* ((Op (copy-Operator Operator))
	 (oplist (list (Operator-arguments Op)         ;;Generate a single expression of the operator.
		       (Operator-Preconditions Op)
		       (Operator-Effects Op)
		       (Operator-Variables Op)
		       (Operator-hintvars Op))))
    
    (setq oplist (rename-variables oplist))            ;;replace all the variables with new vars.
    (setf (operator-arguments Op) (nth 0 Oplist))      ;;Set the new Operator-arguments.
    (setf (operator-preconditions Op) (nth 1 Oplist))  ;;Set the new preconditions.
    (setf (operator-effects Op) (nth 2 Oplist))        ;;set the new effects.
    (setf (Operator-Variables Op) (nth 3 Oplist))
    (setf (Operator-hintvars Op) (nth 4 Oplist))
    Op))                                               ;;return the new form of Op
  


;; --------------------------------------------------------------------
;; Operator hints
;; The Operator hints consists of a list of point, teach and 
;; aplly hints of the form (<Type> . <Specs>) where type is one
;; of POINT, TEACH, or APPLY.  <Specs> is a list of hint 
;; specifications of the form (<class> <string> . <vars>)
;; where:
;;   <Class> is one of STRING KCD MINILESSON
;;   <String> is a format string complete with ~A's.
;;   <Vars> is alist of operator vars that will be
;;          subsituted into the string via a format.
;;
;; The operator hints are designed to be passed to the students
;; when 

;; The operator hints can be turned into tutor turns by passing
;; them to the appropriate code in the HelpStructs/Tutorturn.cl
;; file.  The code in this sections allows you to retreive the
;; tutorturns with the variable values substituted into them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-op-hints
;; Given an operator struct and an optional list of operator values 
;; the hints themselves from the operator with the values substituted
;; in if supplied.
(defun get-op-hints (Op &optional (Values ()))
  "Get the operator hints with vals substituted."
  (when Op
    (subst-bindings
     (bind-operator-vars Op Values)
     (Operator-Hint Op))))


;;--------------------------------------------------------------------
;; Individual Operator Hints.
;; Are of the form (<Type> <Hints>) Where
;;  <Type> is one of [Point, Teach, Apply, Bottom-Out] and describes
;;   The specificity of the hint.
;;
;;  <Hints> are a list of HintSpecs which will be selected from and 
;;   presented to the student as necessary.
;;
;; This code provides access to the individual operator hints. 

(defparameter **OpHint-Types** '(Point Teach Apply Bottom-Out))

(defun OpHint-type (hint)
  "One of (Point, Teach apply, etc.)."
  (car hint))

(defun OpHint-Hintspecs (hint)
  "Get the individual hint-specs from the hint."
  (cdr Hint))

(defun OpHint-P (Hint)
  (and (listp Hint)
       (member (car Hint) **OpHint-Types**)
       (null (remove-if #'HintSpec-P (cdr Hint)))))

(defun filter-hints-by-type (hints types)
  (remove-if-not #'(lambda (x) (member x types :test #'eql))
		 hints :key #'OpHint-type))

;;; Hinting the step itself involves collecting the hint specs
;;; from the operator and returning the result.
(defun collect-step-hints (step &key (types nil))
  "Collect the hints from an op step."
  (let ((hints (get-op-hints 
		(get-operator-by-tag (csdo-op step))
		(csdo-varvals step))))
    (if Types (filter-hints-by-type hints types)
      hints)))


;;--------------------------------------------------------------------
;; HintSpecs
;; The Hint Specifications are of the form: (<Type> <String> <Vars>)
;;  <Type> is one of [String, kcd, Minilesson, Eval, Function]
;;    These identify the appropriate response to the hint at runtime
;;  <Form> is an atom (See below)
;;  <vars> is an optional list of variables that, if present might
;;    be substituted in for later use. 
;;
;; Hintspecs will be interpreted differently at runtime depending 
;; upon their type.  The following describes the cases.
;;
;;  String: the Form is a format string which will be called along
;;    with the values of the (optional) variables.
;;
;;  kcd: the Form is a string that will be (after substitution of
;;    the vars) used to call the appropriate kcd by name.
;;
;;  Minilesson:  the form is a string that (after subst) will be 
;;    treated as a minilesson filename and loaded by the help sys.
;;
;;  Eval:  The form is a lisp-expression that will be evaluated to
;;    Produce a list of hints that will be used (along with the
;;    remaining hints to generate a Hint Sequence.
;;
;;  Function:  The form is a function or function name that will be
;;    fuincalled to produce a hint-sequecne.  Any hints after this
;;    in the list will be ignored.  

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
	**HintSpec-Types** '(String KCD MiniLesson Eval Function)
	#+sbcl #'equalp)

(defun HintSpec-Type (Spec)
  "Get the hintspec's class" 
  (nth 0 Spec))

(defun HintSpec-form (Spec)
  "Get the hintspec's form."
  (nth 1 Spec))

(defun hintspec-vars (Spec)
  "Get the hintspec's variables."
  (subseq Spec 2))

(defun var-free-hintspec (spec)
  "Get the hintspec sans vars."
  (subseq spec 0 2))

(defun Hintspec-P (Spec)
  "Is the supplied elt a HintSpec."
  (and (listp Spec) (member (car Spec) **HintSpec-Types**)))

(defun format-hintspec (Spec)
  "Get a string form of the hintspec."
  (if (stringp Spec) Spec
    (apply #'format nil 
	   (hintspec-form Spec)
	   (mapcar #'eval-spec-arg (hintspec-vars Spec)))))


(defun eval-spec-arg (arg)
  (if (and (consp arg) 
           (eq (first arg) 'nlg))
    (apply #'nlg (rest arg))	; if nlg'ing, don't evaluate rest
    (andes-eval arg)))


;;;----------------------------------------------------------------------------
;;; Collecting hint specs
;;; Given an operator it may be necessary to extract specific elements such
;;; as the kcd or minilesson names that might be hinted within it.  This code
;;; does that returning the kcd name or minilesson names (all we care about 
;;; for hints of those types) to the user.

(defun collect-operator-kcds (op)
  "Collect the kcd's hinted in op."
  (remove-if 
   #'null
   (mapcan #'(lambda (h) 
	       (mapcar #'hintspec-form
		       (collect-ophint-types 'KCD h)))
	   (operator-hint op))))
   
(defun collect-operator-Minilessons (op)
  "Collect the kcd's hinted in op."
  (remove-if 
   #'null
   (mapcan #'(lambda (h) 
	       (mapcar #'hintspec-form 
		(collect-ophint-types 'Minilesson h)))
	   (operator-hint op))))


(defun collect-ophint-types (type hint)
  "Collect the kcds from an ophint."
  (remove-if
   #'null (mapcar #'(lambda (s)
		      (if (eq (hintspec-type s) type) s))
		  (Ophint-hintspecs hint))))



   


;;;===========================================================
;;; Mapping from operator tags to non-op-elements.

;;; Given a list of operator tags collect the matching
;;; operators.
(defun map-optags->operators (tags)
  "Collect the operators for the tags."
  (remove-duplicates
   (remove-if 
    #'null (mapcar #'get-operator-by-tag tags))))

;;; Given a list of operators map them to the list of 
;;; kcds that appear within their hint specifications.
(defun map-operators->kcds (operators)
  (remove-duplicates
   (mapcan #'collect-operator-kcds
	   operators)))

;;; Given a list of operators map them to the list of 
;;; minilessons that appear within their hint specifications.
(defun map-operators->minilessons (operators)
  (remove-duplicates
   (mapcan #'collect-operator-kcds
	   operators)))


(defun map-optags->kcds (tags)
  "Map the operator tags to the kcds."
  (map-operators->kcds 
   (map-optags->operators tags)))


(defun map-optags->minilessons (tags)
  "Map the operator tags to the kcds."
  (map-operators->minilessons
   (map-optags->operators tags)))

  



;;===========================================================
;; trace-operators 
;; trace the relevant operator functions.

(defun trace-operators ()
  "Trace the operator functions."
  
  (trace defoperator)
  (trace get-operator-by-name)
  (trace get-operators-by-effect)
  (trace operator-var-copy)
  )		 


(defun trace-depops ()
  (trace operator-struct)
  (trace is-operator)
  (trace operator-s-expression))

