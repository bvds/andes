;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ontology.cl
;; Collin Lynch
;; 2/21/2001
;;
;; This file defines the ontology facility for Andes2.
;; The purpose of this facility is to allow for the definition
;; of appropriate Andes2 syntax.  This defines a modified form 
;; of a chmosky context-free grammar that can be used for testing
;; expressions with variable depth, and with variable values.
;;
;; Expressions are defined by a struct labelled by type and
;; containing a list of IDs, a documentation string and a translation
;; string for describing the element in english after substitution
;; (to be defined by Lynwood Taylor).
;;
;; Note that all items are stored in a fcfs manner and when a search
;; is made to match equations the process will occur in the same 
;; manner.  The lookup functions will (unless otherwize stated) return
;; the first but not the only item found testing matches in the order
;; in which they were added to the system.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Params

(defparameter *Ontology-features* () "List of feature sets")
(defparameter *Ontology-features-ignore* () "List of feature sets")
(defparameter *Ontology-ExpTypes* () "List of valid expression types")
(defparameter *Ontology-EntryProp-Types* () "List of valid entry proposition prefixes.")
(defparameter *Ontology-GoalProp-Types* () "List of valid Goal proposition prefixes.")
(defparameter *Ontology-Equation-Types* () "List of valid equation types.")
(defparameter *Ontology-Equations* () "List of equation-definitions.")
(defparameter *Ontology-EqnMerge-Lists* () "List of valid type pairs for eqn merging.")
(defparameter *Ontology-PSMGroups* () "List of valid psm groups.")
(defparameter *Ontology-PSMClasses* () "List of valid psm classes.")


;;;;;====================================================================
;;;;; Ontology structures.
;;;;; The ontology consists of several distinct structures related in a
;;;;; containment heiarchy.  At the base level is an expression.
;;;;; An expression such as "(during 1 2)" is defined as a unique type 
;;;;; defined by its particular form such as "(during ?t1 ?t2)" and can
;;;;; contain within it other expression types.  At the root of the
;;;;; expression heirachy is the entity expression which is equiveland to
;;;;; lisp atoms each of which evaluates to itself.  
;;;;;
;;;;; Containing expressions are entryprops such as "(define-var ?v ?e)"
;;;;; which represent atomic entries on the workbench and can contain in
;;;;; their fields expressions but not other entry props.
;;;;;
;;;;; Similarly psmtypes can contain within their filed other expressions
;;;;; but not psmtypes or entryprops.  
;;;;; 
;;;;; In all of the classes the type field is an atom such as 'duration
;;;;; 
;;;;; Form fields consist of pattern matching forms including variable
;;;;; definitions such as "(define-var ?var ?exp)"
;;;;;
;;;;; fields define the specific field types amnd are of the form
;;;;; (<var> . type(s)) where var is a '?var' that appears in at
;;;;; least one of the field listings and types are atomic expression
;;;;; types 
;;;;;


;;--------------------------------------------------------------------
;; Overall ontology functions.

(defun clear-ontology ()
  "Clear out the stores expressions."
  (setq *Ontology-features* nil)
  (setq *Ontology-features-ignore* nil)
  (setq *Ontology-ExpTypes* nil)
  (setq *Ontology-EntryProp-Types* nil)
  (setq *Ontology-Equation-Types* nil)
  (setq *Ontology-Equations* Nil)
  (setq *Ontology-PSMGroups* nil)
  (setq *Ontology-PSMClasses* nil)
  (setq *Ontology-GoalProp-Types* nil))


;;;;====================================================================
;;;; Expression structure.
;;;;
;;;; The expression structure fedines a viable expression type
;;;; the expressions are assumed to be defined as a list of slots
;;;; prefixed by the expression type.  this type is stored in the list
;;;; the slots list is a list of slots each of which can be either an
;;;; expression type or a list of expression types.  
;;;;
;;;; The slots are ands if a list is supplied for a slot then the 
;;;; slot can be one of the set of values.  
;;;;
;;;; alternately a function can be supplied for a slot type which must
;;;; return t when compared to a supplied value.
;;;;
;;;; For each ExpType a function will be generated of the form 
;;;; make-<Typename>-Exp which takes a set number of arguments
;;;; (one for each field) and substitutes them into the expression
;;;; form after performing type checking.  The resulting expression 
;;;; is a form with as many fully bound elements as possible.
;;;; This function is stored in the Func field.
;;;;
;;;; Subsections:
;;;;  ExpType Struct.
;;;;  Definition Functions.
;;;;  Lookup Functions.
;;;;  Backwards Compatability.
(defstruct ExpType

  type          ;; The expression type e.g. at
  form          ;; The expression definition form.  
  
  fields        ;; The fields types in the same order as vars in the form
                ;; these will be used for type checking later on.
  
  symbol-base   ;; Base to present for symbol name in dialog box
  short-name    ;; String with short name for quantity (for use in menu)
  pre-dialog-text  ;; hack to get starting text in variable definition dialog
  dialog-text   ;; remaining text in variable definition dialog.
  Units         ;; A function or atom returning the units.
  restrictions  ;; a list of atoms such as nonnegative placing restictions on the value.
  documentation ;; A documentation string for the item

  VarFunc       ;; Function that translates the Expression to a var.
  FromWorkbench ;; constructs expression from workbench api args (see make-quant)
  
  english       ;; a format style string determining the english expression of this
                ;; expresson.  not used at present.
  )


(defmacro def-qexp (type Form 
		   &key Fields
                        symbol-base
                        short-name
                        pre-dialog-text
                        dialog-text
 			Units
			restrictions
			documentation
			VarFunc
			FromWorkbench
			english)
  "Define a quantity expression."
  (define-exptype :type type 
    :form Form
    :fields Fields
    :symbol-base symbol-base
    :short-name short-name
    :pre-dialog-text pre-dialog-text
    :dialog-text dialog-text
    :Units Units
    :restrictions Restrictions
    :documentation documentation
    :varfunc Varfunc
    :FromWorkbench FromWorkbench
    :English English))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-exptype (Hidden)
;; This function defines an expression type and stores it 
;; in the *ontology-exptypes* The values of the fields are
;; primarily set directly from the input.  However this 
;; function will perform error checking, will fill in
;; unit types on the fields that are not supplied and 
;; if the user provides a variable for the restrictions
;; then that will be tested for here.
;;
;; This function is not intended to be called directly
;; by the users.
(defun define-exptype (&key type form fields symbol-base short-name 
			    pre-dialog-text dialog-text 
			    units 
			    restrictions 
			    documentation
			    varfunc english fromWorkbench)
  "Define and store the specified expression if possible."
  (when (expression-type-p type) 
    (error "exptype ~A already exists." type))

  (when (lookup-expression-struct Form)
    (error "exptype ~A matching form already exists." 
	   (exptype-type (lookup-expression-struct Form))))
  
  (let ((E (make-exptype 
	    :type type
	    :form form
	    :fields (fill-field-defs fields form)	
	    :documentation documentation
	    :symbol-base symbol-base
	    :short-name short-name
	    :pre-dialog-text pre-dialog-text
	    :dialog-text dialog-text
	    :units Units
	    :restrictions restrictions
	    :Varfunc varfunc
	    ;; if supplied, arg should be body of fn to be called with these args
	    :fromWorkbench (when fromWorkbench
	                     (coerce `(lambda (subtype body body2 time) 
	                                   ,fromWorkbench) 'function))
	    :english english)))
    (postpend *Ontology-ExpTypes* E)
    E))


;;;-------------------------------------------------------
;;; Expression Lookup Functions. (public)
;;; These functions are used for searches and function
;;; analasys.  Almost all of these are predicates.
(defun expression-type-p (type)
  "Is type a valid exp type?"
  (lookup-exptype-struct type))


(defun lookup-exptype-struct (type)
  "Lookup the exp struct of ExpType 'TYPE'"
  (find type *Ontology-ExpTypes*
	:key #'ExpType-Type))


(defun lookup-expression-struct (exp &optional (bindings no-bindings))
  "Lookup the first exp with the specified-form."
;; This could work as a replacement, not tested yet
;;  (let ((type (find exp *Ontology-ExpTypes* :key #'exptype-form 
;;		   :test #'(lambda (x y) (unify x y bindings)))))
;; (values type (unify exp (exptype-form exp) bindings))) 
  (lookup-expression-struct-chain exp bindings *Ontology-ExpTypes*))

(defun lookup-expression-struct-chain (exp binds types)
  "An internal function for looking up expression structs."
  (when types
    (let ((b (unify exp (exptype-form (car types)) binds)))
      ;; (format t "***unify ~s and ~S to get ~S~%" ;debug print
      ;;     exp (exptype-form (car types)) b)
      (if b (values (car types) b)
	(lookup-expression-struct-chain 
	 exp binds (cdr Types))))))
		      

(defun lookup-expression-fieldtype (Field Struct)
  "Lookup the struct corresponding to the field."
  (cdr (find Field (ExpType-fields Struct) :key #'car)))
    
(defun lookup-expression-type (exp)
  "Lookup the type of an expression."
  (func-lookup-expression-struct 
   exp #'ExpType-type))

(defun lookup-expression-units (exp)
  "Lookup the expression units."
  (let ((s) (B))
    (multiple-value-setq (s b)
      (lookup-expression-struct exp))
    (if (variable-p (exptype-units s))
	(lookup-expression-units
	 (subst-bindings b (exptype-units s)))
      (exptype-units s))))
  
(defun lookup-expression-restrictions (exp)
  "Lookup the expression restrictions."
  (let ((s) (b))
    (multiple-value-setq (s b)
	(lookup-expression-struct exp))
    (if s (bind-lookup-expression-restrictions s b))))


(defun bind-lookup-expression-restrictions (S B)
  "Collect the restrictions with the bindings."
  (loop for r in (force-to-list (ExpType-Restrictions S))
      append (if (variable-p r)
		 (lookup-expression-restrictions 
		  (subst-bindings b r))
	       (list r))))

(defun lookup-exptype-fromWorkbench (exptype) 
"Fetch the from workbench form for given exptype"
  (let ((s (lookup-exptype-struct exptype)))
    (when s 
        (exptype-fromWorkbench s))))

;;;;============================================================
;;;; Entry Propositions.
;;;; The entry propositions exists to provide information for the
;;;; system users.  and to translate information from expressions
;;;; to the appropriate propositional form.
(defstruct EntryProp
  Type     ;; Proposition type label.
  KBForm   ;; Proposition KB form. 
  HelpForm ;; Propositin help form for mapping.
  fields   ;; field types specific to the kb fields as help
           ;; fields are assumed to be a subset of them.  
  Doc      ;; Documentation.
  english  ;; Englishification code.
  )

;;(defun print-entryprop (Prop &optional (Stream t) (Level 0))
;;  (pprint-indent :block Level Stream)
;;  (format Stream "EntryProp ~A~%" (EntryProp-Type Prop))
;;  (pprint-indent :block Level Stream)
;;  (format Stream "  KB:   ~A~%" (EntryProp-KB Prop))
;;  (pprint-indent :block Level Stream)
;;  (format Stream "  Help: ~A~%" (EntryProp-Help Prop)))
  
(defmacro def-entryprop (Type form
			 &key (fields nil)
			      (helpform form)
			      (doc nil) (English Nil))
  "Define an Entry proposition type and store the value."
  (let ((E (make-entryProp 
	    :type Type
	    :kbform form
	    :helpform helpform
	    :fields (fill-field-defs 
		     fields form helpform)
	    :Doc Doc
	    :English English)))
    (postpend *Ontology-EntryProp-Types* E)
    E))


(defun kb-entryprop-p (Prop)
  "Return t iff Prop is a valid kb form entry prop type."
  (loop for P in *Ontology-EntryProp-types*
      when (unify (EntryProp-kbform P) Prop)
      return P))

(defun help-entryprop-p (Prop)
  "Is the specified prop a valid helpsys form entry proposition."
  (loop for P in *Ontology-EntryProp-types*
      when (unify (EntryProp-helpform P) Prop)
      return P))

;;; to match either help or kb forms, as for reporting:
(defun any-entryprop-p (Prop)
  "True if either help or kb form entry prop"
   (or (kb-entryprop-p prop) (help-entryprop-p prop)))

(defun help-entryprop-type (Prop)
  "Return the type of the help entryprop or nil if it isn't one."
  (loop for P in *Ontology-EntryProp-types*
      when (unify (EntryProp-helpform P) Prop)
      return (EntryProp-type P)))

(defun entryprop-type-p (type)
  "Return t iff the specified type is an entry type."
  (member type *Ontology-EntryProp-Types*
	  :key #'EntryProp-Type
	  :test #'unify ))		;could use "equal"


(defun kb-prop->help-Prop (Prop)
  "Given a proposition if it is an EntryProp return the help form."
  (let ((P (kb-entryprop-p Prop)))
    (cond ((null P) Prop)
	  (t (subst-bindings 
	      (unify (EntryProp-kbform P) Prop)
	      (Entryprop-helpform P))))))

(defun lookup-entryprop-type (Type)
  (find type *Ontology-EntryProp-Types*
	:key #'EntryProp-Type
	:test #'unify ))		;could use "equal"




;;;;=====================================================
;;;; Equation Types.
;;;; equation types are used to determine if an
;;;; equation needs to be handled as a prop or
;;;; not.  This is a simple interface at present.

;;; Some of the equation entries are also props.
;;; This macro facilitates the definition of both.

(defmacro def-eqn-entryprop (type form
			   &key (helpform form)
				(doc nil) (English Nil))
  "define an eqn entry proposition."
  `(progn (def-eqntype ',type)
	  (def-entryprop ,type ,form
	    :helpform ,helpform
	    :doc ,doc
	    :English ,English)))

(defun def-eqntype (type)
  "Define an equation type."
  (push type *Ontology-Equation-types*))

(defun eqn-prop-p (prop)
  "Return t iff the prop is an eqn prop."
  (find (car prop) *Ontology-Equation-types*))

(defun kb-eqn-entryprop-p (prop)
  "Is the prop an eqn and a kb form enttyprop?"
  (and (eqn-prop-p prop)
       (kb-entryprop-p prop)))

;;; to match either help or kb forms, as for reporting:
(defun any-eqn-entryprop-p (prop)
  "True if either kb or helpsys eqn entry prop"
  (or (help-eqn-entryprop-p prop) (kb-eqn-entryprop-p prop)))

(defun help-eqn-entryprop-p (prop)
  "Is the prop an eqn and a helpsys form enttyprop?"
  (and (eqn-prop-p prop)
       (help-entryprop-p prop)))

(defun eqn-entry-type-p (type)
  "Is the specified type an eqn and an entry type."
  (and (find type *Ontology-Equation-Types*)
       (entryprop-type-p type)))


;;;;=============================================================
;;;; Equations
;;;; Equation entries are defined using the entryprop format.  
;;;; However it is necessary to keep track of individual equations
;;;; by type and form for later use.  The code in this section is
;;;; used to define equations by proposition for later use.
;;;;
;;;; The equations themselves are generated as entries by the 
;;;; problem solver at runtime.  The propositions are encoded 
;;;; within the Newtons2 and other kb files.  What this list does
;;;; is index those values.  There is no programmatic location
;;;; that enforces consistency between what is supplied for equation
;;;; prfopositions in that file and what is listed here but it is
;;;; our hope that the consistency will be maintained by the KB
;;;; writer.
;;;;
;;;; In the end more programmatic linking for all of these would 
;;;; be ideal.

(defstruct equation
  Name   ;; The equation's atomic name
  Form   ;; The equation's expression form with variables.
  Fields ;; The fields within the form.
  Complexity ;; major, minor, etc as for psmclass
  English ;; Englishification code.
  EqnFormat ;; A format string and vars for the platonic equation form.
  Doc    ;; Documentation.
  )


;;; This function is called by the psmclass definition code to
;;; generate the equation definition for that PSMClass
(defun add-equation-definition (Name Form Fields English EqnFormat Doc Complexity)
 (let (tmp (E (make-equation
	       :Name Name
	       :Form Form
	       :Fields (fill-field-defs fields form)
	       :Complexity Complexity
	       :English English
	       :EqnFormat EqnFormat
	       :Doc Doc)))
    (cond ((setq tmp (lookup-name->equation Name))
	   (error "The equation ~a has already been specified here: ~a" Name tmp))
	 ;; ((setq tmp (lookup-expression->equation Form))
	 ;;  (error "An equation matching ~a has already been defined here: ~a" Form tmp))
	  (t (push E *Ontology-Equations*)
	     E))))

;;; -------------------------------------------------------
;;; Searching for equations by values.

(defun lookup-name->equation (Name)
  (find Name *Ontology-Equations* :key #'Equation-Name))


(defun lookup-expression->Equation (Exp &optional (Bindings no-bindings))
  (find-if #'(lambda (E) (unify Exp (equation-form E) Bindings))
	   *Ontology-Equations*))


(defun equation-major-p (Equation)
  "Return t iff the equation's complexity is Major."
  (eq 'major (equation-complexity Equation)))







;;;;============================================================
;;;; Goal propositions.
;;;; Goal props are located within the sg subsections of the 
;;;; qsolver trees.  Goald such as draw-forces are used in the
;;;; knowledge base to express the goals that we want to acheive
;;;; by specific actions.  Goal props are used in the nlg code
;;;; for the purposes of nlg.
(defstruct GoalProp
  Type     ;; Proposition type label.
  Form     ;; Unification form for the goal.
  Doc      ;; Documentation.
  
  english  ;; Englishification code.
  )


(defmacro def-goalprop (Type form
			&key (English nil)
			     (doc nil))
  "Define an Entry proposition type and store the value."
  (let ((E (make-goalProp 
	    :type Type
	    :form form
	    :Doc Doc
	    :english English)))
    (postpend *Ontology-GoalProp-Types* E)
    E))



(defun goalprop-type-p (type)
  "Return t iff the specified type is an entry type."
  (member type *Ontology-goalProp-Types*
	  :key #'GoalProp-Type
	  :test #'unify ))		;could use "equal"


(defun goalprop-exp-p (exp)
  "Is the expression a goalprop expression?"
  (find-if #'(lambda (g) (unify exp g))
	   *Ontology-GoalProp-Types*
	   :key #'GoalProp-Form))


;;;;=================================================================
;;;; PSMS
;;;; Problem Solution Methods are descriptions of general principles
;;;; that the student can use to structure their search process.
;;;; The purpose of this code is to facilitate the discussion of 
;;;; psms with the student during next-step-help and other things.
;;;;
;;;; PSMs themselves describe specific solution methods.  Related
;;;; PSMs can be grouped into PSMGroups for general disucussion with
;;;; the student.  The code in this section facilitates that and 
;;;; allows for the psms to be catalogued for later use.

(defun lookup-psm-name (name)
  "Lookup the psm class or group with name NAME."
  (or (lookup-psmgroup-name name)
      (lookup-psmclass-name name)))


;;; Occasionally we will want to traverse the heiarchy upwards 
;;; selecting the most general psm group for a class or the 
;;; most general psmgroup for a class that fits some spec
;;; this function does that traversing the group heiarchy
;;; upwards returning the topmost psm group or the group
;;; immediately prior to the one that fails the test.
(defun most-general-psmform (psm &optional (test #'identity))
  "Retrun the most general psmgroup for the class or group."
  (cond ((psmclass-P PSM)
	 (if (not (and (psmclass-group psm)
		       (funcall test (psmclass-group psm))))
	     psm
	   (most-general-psmform (psmclass-group psm) test)))
	((psmgroup-p psm)
	 (if (not (and (psmgroup-supergroup psm)
		       (funcall test (psmgroup-supergroup psm))))
	     psm
	   (most-general-psmform (psmgroup-supergroup psm) test)))))
	 
	 

;;; Given a psm and a form determine if the form is an example
;;; of (unifies with) the specified psmclass or psmgroup.
;;; The psmclass or psmgroup can be supplied as either a 
;;; name or as a struct.
(defun exp-of-psmtype? (exp psm &optional (Bindings no-bindings))
  "Is the specified exp an example of the psmclass or psmgroup supplied?"
  (cond ((psmgroup-p psm) (unify exp (psmgroup-form psm) bindings))
	((psmclass-p psm) (unify exp (psmclass-form psm) bindings))
	((not (listp psm)) (exp-of-psmtype? exp (lookup-psm-name psm)))
	(t (error "Unrecognized psm type supplied to psm-exp-of-type? ~A" psm))))


;;; exp-of-psmtype-set?
;;; Hiven an exp and a set of psmclasses or groups determine if
;;; the psm is a member of any one of the set and return the 
;;; type if it is and the bindings as a secondary value.
(defun exp-of-psmtype-set? (exp psms)
  "Is the exp a form of one of the supplied psms."
  (when psms
    (let ((r (exp-of-psmtype? exp (car psms))))
      (if r (values (car psms) r)
	(exp-of-psmtype-set? exp (cdr psms))))))


;;;--------------------------------------------------------------
;;; PSM group.
;;; psms exist as part of specified groups that can be selected
;;; for from memory and searched through.  Each psm is a member
;;; of one and only one group and shares some general concepts 
;;; with that group.  
;;;
;;; Each group consists of a group name, a set of members that
;;; may be psms or psm classes that are part of it, a doc string 
;;; and possibly some hint information that is associated with 
;;; the gorup.
(defstruct (psmgroup (:print-function print-psmgroup))
  name       ;; The group's name
  form       ;; An optional pattern for unification.
  members    ;; The psms or psm classes that are part of this.
  supergroup ;; The superclass of this class if any.
  help       ;; hints or other help info.
  doc        ;; Documentation string info.
  english    ;; English
  expformat  ;; expform description.
  )

(defun print-psmgroup (group &optional (stream t) (level 0))
  (declare (ignore level))
  (format Stream "{PSMGroup:    ~A~%" (psmgroup-name group))
  (format Stream "  Form:       ~A~%" (psmgroup-form group))
  (format Stream "  Members:    ~A~%" 
	  (loop for M in (psmgroup-members group)
	      collect (psmclass-name M)))
  (format Stream "  SuperGroup: ~A~%" (psmgroup-supergroup group))
  (format Stream "  Help:       ~A~%" (psmgroup-help group))
  (format Stream "  Doc:        ~A~%" (psmgroup-doc group))
  (format Stream "  English:    ~A~%" (psmgroup-english group))
  (format Stream "  expformat:    ~A}~%" (psmgroup-expformat group)))


(defmacro def-psmgroup (name 
			&key form members supergroup
			     help doc english ExpFormat)
  "Define a new psmgroup."
  (test-defpsmgroup-errors name supergroup members)
  (let* ((sup (lookup-psmgroup-name supergroup))
	 (new (make-psmgroup
	       :name name
	       :form form
	       :supergroup sup
	       :help help
	       :doc doc
	       :english english
	       :expformat expformat)))
    
    (setf (psmgroup-members new)
      (loop for c in members
	  collect (lookup-psm-name c)))
    (if sup (push new (psmgroup-members sup)))
    (postpend *Ontology-PSMGroups* New)
    New))



(defun test-defpsmgroup-errors (name supergroup members)
  "Test for errors in the defpsmclass call."
  (cond ((lookup-psmgroup-name name)
	 (error "PsmClass ~A already exists." name))
	((lookup-psmclass-name name)
	 (error "A PSM Type named ~A already exists." name))
	((and supergroup (not (lookup-psm-name supergroup)))
	 (error "Designated superclass ~A does not exist."
		supergroup))
	(t (loop for n in members
	       when (not (lookup-psm-name n))
	       do (error "Some designated member class in ~A does not exist."
			 members)))))


(defun lookup-psmgroup-name (name)
  "Lookup psm groups by name."
  (find name *Ontology-PSMGroups*
	:key #'PsmGroup-Name))

(defun lookup-psmgroups-form (form &optional (bindings no-bindings))
  "Collect the psmgroups that unify with pattern."
  (loop for G in *Ontology-PSMGroups*
      when (unify (psmgroup-form G) form bindings)
      collect G))


(defun lookup-expressions->psmgroups (exp &optional (bindings no-bindings))
  "Find all psmgroups matching expression exp."
  (loop for G in *Ontology-PSMGroups*
      when (unify (psmgroup-form G) exp bindings)
      collect G))


;;;-------------------------------------------------------------
;;; Psm Classes
;;; Psm expressions will need to be unified with at help
;;; time for the purposes of analizing student responses.
;;; This code performs that comparison allowin the response
;;; to be tested agains one of the specified types and to 
;;; be flagged if necessary.  
;;;
;;; The goal of psms is to setup the ability to access psms
;;; individually and to obtain comments on them from the 
;;; students hence the selection of psms by class and 
;;; psm struct lists.

(defstruct (psmclass (:print-function print-psmclass))
  name       ;; type name e-g compo-free-nsl
  form       ;; Form to unify with.
  fields     ;; field types in order of vars.
  
  group      ;; The class that this psm is a part of e.g Kinematics.
  Complexity ;; Planning Commplexity clas (simple, link, major)
  help       ;; Help information.
  short-name ;; String with short name (for use in menu), see eval-print-spec
  english    ;; Storage for english phrasing.
  ExpFormat  ;; Format string for expressions of this type (with vars).
  EqnFormat  ;; Format for the equation form of this psm, see eval-print-spec  
  doc        ;; description of the psm.
  )


(defun print-psmclass (class &optional (stream t) (level 0))
  (declare (ignore level))
  (format Stream "[PSMClass:   ~A~%" (psmclass-name class))
  (format Stream "  Form:      ~A~%" (psmclass-form class))
  (format Stream "  Fields:    ~A~%" (psmclass-fields class))
  (format Stream "  Group:     ~A~%" (psmclass-group class))
  (format Stream "  Complexity ~A~%" (psmclass-complexity class))
  (format Stream "  Help:      ~A~%" (psmclass-help class))
  (format Stream "  Short name ~A~%" (psmclass-short-name class))
  (format Stream "  English:   ~A~%" (psmclass-english class))
  (format Stream "  Doc:       ~A]~%" (psmclass-doc class)))


;;; In order to display the psmclass and its english form to the 
;;; USNA this print function was written.  It prints out the 
;;; psmclass in a useful "enlgish" form by name and descriptors.
(defun print-display-psmclass (class &optional (Stream t) (Level 0))
  (declare (ignore level))
  (let ((form (strip-replace-exp-vars (psmclass-form Class))))
    (format Stream "~a~% \"~a\"~%  \"~a\"~2%"
	    (psmclass-name Class) (nlg form 'psm-english) (nlg form 'psm-exp))))



(defmacro def-psmclass (name form
			&key fields group complexity
			     help short-name english
			     doc  ExpFormat EqnFormat)
  "Define and store a psm type."
  (test-defpsmclass-errors name group)
  (let* ((g (lookup-psmgroup-name group))
	 (FieldDefs (fill-field-defs Fields Form))
	 (New (make-psmclass
	       :name name
	       :form form
	       :fields FieldDefs
	       :group g
	       :complexity complexity
	       :help help
	       :short-name short-name
	       :english english
	       :ExpFormat ExpFormat
	       :EqnFormat EqnFormat
	       :doc doc)))
    (if g (push New (psmgroup-members g)))
    (postpend *Ontology-PSMClasses* New)
    
    ;;; Add the psmclass to the equations as well.
    (add-equation-definition Name Form FieldDefs English EqnFormat Doc Complexity) 
    
    New))


(defun test-defpsmclass-errors (name group)
  "Test for errors in a psmclass definition."
  (cond ((lookup-psm-name name)
	 (error "PsmClass name ~A is already in use." name))
	((and group (not (lookup-psmgroup-name Group)))
	 (error "Designated group ~A does not exist." group))))



(defun lookup-psmclass-name (name)
  "Lookup psm classes by name."
  (find name *Ontology-PSMClasses*
	:key #'PsmClass-Name))


(defun lookup-psmclass-exp (exp)
  (car (lookup-psmclasses-exp exp)))


(defun lookup-psmclasses-exp (exp)
  "Lookup the psm classes that unify with exp."
  (let ((Classes) (Binds) (tmp))
  (dolist (c *Ontology-PSMClasses*)
    (when (setq tmp (unify (psmclass-form c) exp))
      (push c Classes)
      (push tmp Binds)))
  (values Classes Binds)))


(defun lookup-expression->psmclasses (exp &optional (bindings no-bindings))
  "Find all psmgroups matching expression exp."
  (loop for C in *Ontology-PSMClasses*
      when (unify (PSMClass-form C) exp bindings)
      collect C))


(defun lookup-expression->psmclass (exp &optional (bindings no-bindings))
  (car (lookup-expression->psmclasses exp bindings)))


;;; Lookup the psmclasses that match the expression.
(defun lookup-expressions->psmclasses (exps &optional (bindings no-bindings))
  "Lookup the matching psmclasses for the set of expressions provided."
  (mapcar #'(lambda (e) (lookup-expression->psmclass e bindings))
	  exps))

(defun eval-print-spec (x &optional (bindings no-bindings))
  "evaluate, using andes-eval, a printing specification in def-psmclass"
  (cond ((listp x)
	 (format nil "~{~@?~}" (mapcar #'andes-eval 
				       (subst-bindings-quoted bindings x))))
	((typep x 'string) x)
	(t (error "invalid print spec ~A" x))))

;;;---------------------------------------------------------
;;; Complexity tests.

(defun psmclass-major-p (Class)
  "Is this a major PSM?"
  (equal 'major (psmclass-complexity Class)))



#|
  ;;;;=========================================================================
  ;;;; Field defininitions.
  ;;;; For the purposes of storing field information in the various sturctures
  ;;;; I have here defined the internal field struct.  Each field has a name
  ;;;; a variable, and various specifications that can be added over time.
  ;;;; These will be used to gather data about the proposition and enter it
  ;;;; as necessary.  
  
  (defstruct (pfield (:print-fuinction print-pfield))
  
  Name ;; The field name.
  Var  ;; The unification var for the field.
  type ;; The field's type.
  )
  
  
  (defun print-pfield (field &optional (Stream t) (Level 0))
  (declare (ignore Level))
  (format Stream "~a" (list (pfield-name field) (pfield-var field)
  ':type (pfield-type field)))) 
  |#



;;; Field definitions are added automatically by the system although 
;;; they can be updated by the user as necessary.  This code loads the 
;;; values and produces the definitions.  In the future this will be 

(defun fill-field-defs (fields &rest forms)
  "Fill in the field definitions."
  (let ((F fields) 
	(v (remove-duplicates 
	    (variables-in forms))))
    (cond ((< (length v) (length f))
	   (error "Too many field defs supplied."))
	  ((> (length v) (length f))
	   (dolist (var v)
	     (when (not (member var f :key #'car))
	       (push (cons var 'Entity) F)))))
    F))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Currently unnecessary
;; field definitions are assumed to be of a 
;; general type that is a list of the form
;; (var . type) from this list and a matching 
;; list of values we can construct a binding list.
;;(defun get-field-bindings (fields vals &optional (result no-bindings))
;;  "Generate a set of bindings from the fields and vals."
;;  (let (R)
;;    (dotimes (n (min (length fields) (length vals)))
;;      (push (cons (Car (nth n fields)) (nth n vals)) R))))


;;; --------------------------------------------------------------
;;; Complexity classes.
;;; Given a list of psmclasses calculate the planning complexity
;;; of the set by determining the number of major, connect, and
;;; minor psms in the set.
;;; 
;;; This is stored as an alist of values for later use.

;;; Iterate through the list of psms incrementally building
;;; an alist of classes and their counts.  This defines the
;;; complexity of the associated set.
(defun psmclasses->complexity (psms &optional (comp ()))
  "Given the set of psms calculate their complexity."
  (let ((c) (classes (if comp comp nil)))
    (dolist (p psms)
      (setq c (assoc (psmclass-complexity p) classes))
      (if c (incf (cdr c))
	(push (cons (psmclass-complexity p) 1) classes)))
    classes))
	  

;;; Given a list of complexity classes calculate 
;;; the average values of each set.
(defun calc-complexity-averages (sets)
  "calculate the averages of the complexity sets."
  (let (Result Class)
    ;;; Calculate the initial values.
    (dolist (Set sets)
      (dolist (Val Set)
	(setq Class (assoc (car Val) Result))
	(when (null Class)
	  (setq Class (cons (car Val) 0))
	  (push Class Result))
	(setf (cdr Class) (+ (cdr Class) (cdr Val)))))

    ;;; Average the final Values.
    (dolist (Val Result)
      (setf (cdr Val) (/ (cdr Val) (length sets))))

    Result))


;;; Given a list of psms identify them by class
;;; associating them by the appropriate complexity.
(defun assoc-psms-by-complexity (psms)
  "Assoc the psms by complexity class."
  (let (Result Class) 
    (dolist (P Psms)
      (cond ((setq Class (assoc (psmclass-complexity P) Result))
	     (setf (cdr Class) (cons P (cdr Class))))
	    (t (setq Class (cons (psmclass-complexity P) (list P)))
	       (push Class Result))))
    Result))


;;; --------------------------------------------------------------
;;; Lookup-maching-variable-values
;;; Given a variable, a list of expression forms all of which 
;;; contain that variable and a list of bound expressions this 
;;; function will extract all of the assertions inside the list
;;; that can be bound to at least one of the supplied expression
;;; types.  It will then extract the variable value from each of
;;; those bindings.
;;;
;;; This code begins by testing errors then iterates over the items
;;; in the list conducting the match tests.
;;;
;;; NOTE:: This code makes use of all the matches that can be made
;;;  for a given expression within the forms.  That is, for each
;;;  expression E it attempts to match it to each and every form 
;;;  and takes the binding values for any and all that return them.
;;;
;;; NOTE:: This is a short-term hack that will most likely be 
;;; replaced at a later date by a more full-fledged knowledge-base 
;;; lookup and type-checking system.  
;;;
;;; NOTE:: This code will not work properly when we are attempting 
;;;  to bind one variable with another.  The process has a tendency 
;;;  to reverse the order of the variables in the binding lists.  
;;;  Therefore in order for this code to work properly the list of
;;;  exp's must be fully bound.  

(defun lookup-matching-var-vals (Var Forms Exps)
  (when (not (variable-p Var))
    (error "Specified var ~a is not a valid variable." Var))
  
  (dolist (Form Forms)
    (when (not (member Var (variables-in Form) :test #'unify)) ;could use "equal"
      (error "Specified form ~a does not contain var: ~a" Form Var)))

  (when (variables-in Exps)
    (error "Variables found in Expressions."))
  
  (let (R B)
    (dolist (Exp Exps)
      (when (setq B (unify-with-list Exp Forms))
	(dolist (Binding B)
	  (push (lookup Var Binding) R))))
    (remove-duplicates R :test #'unify))) ;could use "equal"

;;;
;;;                 Feature sets
;;;


(defun define-feature-set (name quants)
  "Construct feature sets, with inheritance."
  (when (member name *Ontology-features* :key #'first)
    (error "feature set ~A already defined" name))
  (dolist (set *Ontology-features*) 
    ;; when a featureset is a member of itself, don't inherit
    (when (and (member (first set) quants) 
	       (not (member (first set) (second set)))) 
      (setf quants (union (second set) quants)))
    (when (and (member name (second set)) 
	       (not (member name quants))) 
      (setf (second set) (union (second set) quants))))
  (postpend *Ontology-features* (list name quants)))

(defmacro def-feature-set (feature quants)
  "define feature sets"
  (define-feature-set feature quants) 
  nil)  ;macro return value

(defun quant-allowed-by-features (quant x)
  (when x (or (member quant *Ontology-features-ignore*) 
	      (member quant (second (find (car x) *ontology-features* 
					  :key #'first)))
	      (quant-allowed-by-features quant (cdr x)))))

