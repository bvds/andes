;; Ontology.cl
;; Collin Lynch
;; 2/21/2001
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

(defvar *Ontology-ExpTypes* () "List of valid expression types")
;; Most (or all) ontology forms start with a bound symbol; we can use
;; this fact to speed up matching of forms.
(defvar *Ontology-form-table* (make-hash-table :test #'equal :size 100)
  "Hash table to give leading unbound members of form.")
(defvar *Ontology-EntryProp-Types* () "List of valid entry proposition prefixes.")
(defvar *Ontology-GoalProp-Types* () "List of valid Goal proposition prefixes.")
(defvar *Ontology-Equation-Types* () "List of valid equation types.")
(defvar *Ontology-PSMGroups* () "List of valid psm groups.")
(defvar *Ontology-PSMClasses* () "List of valid psm classes.")

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
;;;;;


;;--------------------------------------------------------------------
;; Overall ontology functions.

(defun clear-ontology ()
  "Clear out the stores expressions."
  (setq *Ontology-ExpTypes* nil)
  (clrhash *Ontology-form-table*)
  (setq *Ontology-EntryProp-Types* nil)
  (setq *Ontology-Equation-Types* nil)
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
    
  rank          ;; vector, scalar, or ...
  symbol-base   ;; Base to present for symbol name in dialog box
  short-name    ;; String with short name for quantity (for use in menu)
  Units         ;; A function or atom returning the units.
  restrictions  ;; a list of atoms such as nonnegative placing restictions 
                ;; on the value.
  documentation ;; A documentation string for the item

  new-english   ;; Model structure for matching, with multi-word
                ;; strings allowed.  Any s-expressions are matched to ontology.
  )


(defmacro def-qexp (type Form 
		   &key rank
                        symbol-base
                        short-name
 			Units
			restrictions
			documentation
	        	new-english)
  "Define a quantity expression."

  ;; Compile-time tests

  (when (match:matches-model-syntax form)
    (error "Ontology ~A form ~A matches model syntax" type form))

  (when (and rank (not short-name))
    (error "short-name must be supplied for ~A" type))


  `(let ((E (make-exptype 
             :type ',type
             :form ',form
             :rank ',rank
             :documentation ',documentation
             :symbol-base ',symbol-base
             :short-name ',short-name
             :units ',Units
             :restrictions ',restrictions
             :new-english (compile-evals ',new-english))))

     ;; Remove any existing entry of this name (thus, allowing updates)
     (when (expression-type-p ',type)
       (let ((old-bound-sub (get-bound-sub 
                             (ExpType-form (lookup-exptype-struct ',type)))))
         (setf (gethash old-bound-sub *Ontology-form-table*)
               (delete ',type 
                       (gethash old-bound-sub *Ontology-form-table*) 
                       :key #'ExpType-Type)))
       (setf *Ontology-ExpTypes*
             (delete ',type *Ontology-ExpTypes* :key #'ExpType-Type :count 1)))

     (let ((matching-form (lookup-expression-struct ',Form)))
       (when matching-form
         (error "Adding ~A with form ~A: ~%conflict for exptype ~A with form ~A."
                ',type ',form 
                (exptype-type matching-form)
                (exptype-form matching-form))))

    (push E *Ontology-ExpTypes*)
    (push E (gethash (get-bound-sub ',form) *Ontology-form-table* nil))))


(defun get-bound-sub (x)
  "Create unique identifier for expression based on first member."
  (cond ((variable-p x) (error "Unbound expr ~A" x))
	((atom x) x)
	((variable-p (car x)) (error "Unbound expr ~A" x))
	(t (list (car x)))))


(defun compile-evals (model)
  "Recurse through new-english and compile any evals."
  (cond ((atom model) model)
	((and (consp model) (eq (car model) 'eval))
         ;; sbcl version 1.2.2 introduces a struct
         ;; for backquote commas.  So, we need to expand
         ;; backquotes before collecting variables.
	 (let ((params (variables-in
                        (sbcl-expand-backquote (second model)))))
	   (append (list 'eval-compiled
			 (compile nil `(lambda ,params ,(second model)))
			 params)
		 (cddr model))))
	;; Recursion through model
	((consp model) (reuse-cons (compile-evals (car model))
				   (compile-evals (cdr model))
				   model))))


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

(defun lookup-expression-struct (exp)
  "Lookup the first exp with the specified-form."
  (dolist (qexp (gethash (get-bound-sub exp) 
			 *Ontology-form-table* nil))
    (let ((bindings (unify exp (ExpType-form qexp))))
      (when bindings
	(return-from lookup-expression-struct
	  (values qexp bindings))))))

(defmacro with-ontology-exptypes (arg &rest body)
  "Iterate over members of quantity Ontology."
  `(dolist (,arg *Ontology-ExpTypes*) ,@body))
		      
; For readability when used as a predicate:
(defun quantity-expression-p (exp)
 "Non-null if given form is a declared quantity expression"
  (lookup-expression-struct exp))
    
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


;;;;============================================================
;;;; Entry Propositions.
;;;; The entry propositions exists to provide information for the
;;;; system users.  and to translate information from expressions
;;;; to the appropriate propositional form.
(defstruct EntryProp
  Type     ;; Proposition type label.
  KBForm   ;; Proposition KB form. 
  HelpForm ;; Propositin help form for mapping.
  Doc      ;; Documentation.
  )

;;(defun print-entryprop (Prop &optional (Stream t) (Level 0))
;;  (pprint-indent :block Level Stream)
;;  (format Stream "EntryProp ~A~%" (EntryProp-Type Prop))
;;  (pprint-indent :block Level Stream)
;;  (format Stream "  KB:   ~A~%" (EntryProp-KB Prop))
;;  (pprint-indent :block Level Stream)
;;  (format Stream "  Help: ~A~%" (EntryProp-Help Prop)))

(defmacro def-entryprop (Type form
			 &key (helpform form)
			 doc)
  "Define an Entry proposition type and store the value."
  `(let ((E (make-entryProp 
	     :type ',Type
	     :kbform ',form
	     :helpform ',helpform
	     :Doc ',Doc)))
    (push-to-end E *Ontology-EntryProp-Types* :key #'entryProp-type)))


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
	  :test #'unify))		;could use "equal"


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
	:test #'unify))		;could use "equal"


;;;;=====================================================
;;;; Equation Types.
;;;; equation types are used to determine if an
;;;; equation needs to be handled as a prop or
;;;; not.  This is a simple interface at present.

;;; Some of the equation entries are also props.
;;; This macro facilitates the definition of both.

(defmacro def-eqn-entryprop (type form
			   &key (helpform form)
				doc)
  "define an eqn entry proposition."
  `(progn (pushnew ',type *Ontology-Equation-types*)
	  (def-entryprop ,type ,form
	    :helpform ,helpform
	    :doc ,doc)))

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


;;;;============================================================
;;;; Goal propositions.
;;;; Goal props are located within the sg subsections of the 
;;;; qsolver trees.  Goals such as draw-forces are used in the
;;;; knowledge base to express the goals that we want to acheive
;;;; by specific actions. 
;;;
;;; These are used solely for hinting in Help/NextStepHelp.cl


(defstruct GoalProp
  Type     ;; Proposition type label.
  Form     ;; Unification form for the goal.
  Doc      ;; Documentation.
  
  nlg-english  ;; Englishification code.
  )


(defmacro def-goalprop (Type form
			&key nlg-English doc)
  "Define an Entry proposition type and store the value."
  `(let ((E (make-goalProp 
	    :type ',Type
	    :form ',form
	    :Doc ',Doc
	    :nlg-english ',nlg-English)))
    (push-to-end E *Ontology-GoalProp-Types* :key #'goalProp-type)))

(defun goalprop-exp-p (exp)
  "Is the expression a goalprop expression?"
  (find exp *Ontology-GoalProp-Types*
	:test #'unify
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
  nlg-english    ;; English
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
  (format Stream "  English:    ~A~%" (psmgroup-nlg-english group))
  (format Stream "  expformat:    ~A}~%" (psmgroup-expformat group)))


(defmacro def-psmgroup (name 
			&key form members supergroup
			help doc nlg-english ExpFormat)
  "Define a new psmgroup."
  `(let* ((sup (lookup-psmgroup-name ',supergroup))
	  (new (make-psmgroup
		:name ',name
		:form ',form
		:supergroup sup
		:help ',help
		:doc ',doc
		:nlg-english ',nlg-english
		:expformat ',expformat)))
    
    ;; Load-time tests
    
    (when (lookup-psmclass-name ',name)
      (error "A PSM Type named ~A already exists." ',name))
    
    (when (and ',supergroup (not (lookup-psm-name ',supergroup)))
      (error "Designated superclass ~A does not exist."
	     ',supergroup))
    
    (dolist (n ',members)
      (when (not (lookup-psm-name n))
	(error "Designated member class ~N in ~A does not exist."
	       n ',members)))
    
    (setf (psmgroup-members new)
      (loop for c in ',members
	  collect (lookup-psm-name c)))
    (if sup (push new (psmgroup-members sup)))
    (push-to-end New *Ontology-PSMGroups* :key #'psmgroup-name)))


(defun lookup-psmgroup-name (name)
  "Lookup psm groups by name."
  (find name *Ontology-PSMGroups*
	:key #'PsmGroup-Name))

(defun lookup-expression->psmgroup (exp)
"return first psmclass whose form unifies with expression"
  (find exp *Ontology-PSMGroups* 
        :key #'psmgroup-form :test 'unify))

;;;-------------------------------------------------------------
;;; Psm Classes
;;; Psm expressions will need to be unified with at help
;;; time for the purposes of analyzing student responses.
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
  
  group      ;; The class that this psm is a part of e.g Kinematics.
  Complexity ;; Planning Commplexity clas (simple, link, major)
  help       ;; Help information.
  short-name ;; String with short name (for use in menu), see eval-print-spec
  nlg-english    ;; Storage for english phrasing.
  tutorial   ;; File containing tutorial (string)
  ExpFormat  ;; Format string for expressions of this type (with vars).
  EqnFormat  ;; Format for the equation form of this psm, see eval-print-spec  
  doc        ;; description of the psm.
  )


(defun print-psmclass (class &optional (stream t) (level 0))
  (declare (ignore level))
  (format Stream "[PSMClass:   ~A~%" (psmclass-name class))
  (format Stream "  Form:      ~A~%" (psmclass-form class))
  (format Stream "  Group:     ~A~%" (psmclass-group class))
  (format Stream "  Complexity ~A~%" (psmclass-complexity class))
  (format Stream "  Help:      ~A~%" (psmclass-help class))
  (format Stream "  Short name ~A~%" (psmclass-short-name class))
  (format Stream "  English:   ~A~%" (psmclass-nlg-english class))
  (format Stream "  Doc:       ~A]~%" (psmclass-doc class)))


(defmacro def-psmclass (name form
			&key group complexity
			help short-name nlg-english tutorial
			doc  ExpFormat EqnFormat)
  "Define and store a psm type."
  `(let* ((g (lookup-psmgroup-name ',group))
	  (class (make-psmclass
		  :name ',name
		  :form ',form
		  :group g
		  :complexity ',complexity
		  :help ',help
		  :short-name ',short-name
		  :nlg-english ',nlg-english
		  :tutorial ',tutorial ;should verify file exists.
		  :ExpFormat (sbcl-expand-backquote ',ExpFormat)
		  :EqnFormat ',EqnFormat
		  :doc ',doc)))
    
    ;; Load-time tests
    
    (when (and ',group (not g))
      (error "Designated group ~A does not exist." ',group))
    
    (when g (push class (psmgroup-members g)))
    
    (push-to-end class *Ontology-PSMClasses* :key #'PSMClass-Name)))

;; Keyword defaults get evaled before they are used.
(defmacro push-to-end (mem class &key (key (quote #'identity)))
"If object matches one already in list, replace old object, otherwise add object onto end."
  `(setf ,class (replace-or-postpend ,mem ,class :key ,key)))
	  
(defun replace-or-postpend (mem class &key (key #'identity))
  "Replace element of list, or add to end."
  (cond ((null class) (list mem)) ;add to end
	((eql (funcall key mem) (funcall key (car class)))
	 (warn "Redefining operator ~A." (funcall key mem))
	 (cons mem (cdr class))) ;found match, replace
	;; recursion
	(t (cons (car class)
		 (replace-or-postpend mem (cdr class) :key key)))))

(defun lookup-psmclass-name (name)
  "Lookup psm classes by name."
  (find name *Ontology-PSMClasses*
	:key #'PsmClass-Name))

(defun lookup-expression->psmclass (exp)
"return first psmclass whose form unifies with expression"
  (find exp *Ontology-PSMClasses* 
        :key #'psmclass-form :test 'unify))


(defun eval-print-spec (x &optional (bindings no-bindings))
  "evaluate, a printing specification in def-psmclass"
  (cond ((listp x)
	 (format nil "~{~@?~}" (mapcar #'eval 
				       (subst-bindings-quoted bindings x))))
	((typep x 'string) x)
	(t (error "invalid print spec ~A" x))))
