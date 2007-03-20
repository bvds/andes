;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Executable2.cl
;; Kurt Vanlehn and Collin Lynch
;; 10/24/2000
;;
;; This file contains the executable code for the solution graph
;; generator.  This code allows for the includion of explicit tests
;; and bindings within the code.
;;
;; The format for all executable functions is that they are passed the executable
;; expression in full and the current state and may return a new state, a list
;; of states or nil.  For saafety purposes the functions should modify copies of the 
;; current state not the state itself.
;;
;; Currently this file defines the following classes of executables:
;;  (Bind <var> <Value>)                       --> Bind the specified variable to the specified value in
;;                                                 the current state.
;;
;;  (test <Function>)                          --> Test to determine if the given function is true in the 
;;                                                 current state.  If it is return the state otherwize return nil.
;;
;;  (in-wm <Expression>)                       --> Test to determine if the given expression is in the 
;;                                                 state's wm and return true if it is.
;;
;;  (not <Expression>)                         --> Test to determine if the given expression is not in 
;;                                                 mem and return the state if it is nil otherwize.
;;  
;;  (any-member <var> <Function>)              --> Generate a set of states with var bound to the 
;;                                                 successive elements of the list returned by Function.
;;
;;  (debug <Form> <vars>)                      --> Pass the specified format string and vars to format.
;;
;;  (rdebug <Form> <vars>)                      --> Pass the specified format string and vars to format.
;;
;;  (count <exp> <form>)                       --> Count the number of wme's in the current state 
;;                                                 that unify with form and store the result in exp.
;;
;; Modifications:  
;;   10/16/2000 <Collin Lynch> Modified execute bind to fail if 
;;                             func returns nil. 
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-executable
;; Returns true iff the specified expression is an executable goal.

(defun executable-p (Ex)
  (and (listp ex)
       (member (car ex)
	       '(bind not in-wm wm-or-derive add-to-wm test any-member debug rdebug count setof))))

;; execute-executable
;; Given the specified State and 
;;
;; Arguments: Ex: The executable to be executed.
;;            St: State that has the stack popped & the predecessor link in place
;;
;; Returns: Set of states that are copies of the given state

(defun executable-successors (Ex St)
  "Execute the given executable with the given state."
  (case (car ex)
    (bind (execute-bind ex st))
    (test (execute-test ex st))
    (not (execute-not ex st))
    (in-wm (execute-in-wm ex st))
    (wm-or-derive (execute-wm-or-derive ex st))
    (add-to-wm (execute-add-to-wm ex st))
    (any-member (execute-any-member ex st))
    (debug (execute-debug ex st))
    (rdebug (execute-rdebug ex st))
    (count (execute-count ex st))
    (setof (execute-setof ex st))
    ))

;;; This executable precondition should have the form (bind <variable>
;;; <form>).  It evaluates the Lisp form, after first substituting in 
;;; bindings for any varibles in <form>.  The result of evaluating the
;;; form in Lisp is unified with the variable expression.
;;; The variable expression may be unbound, partially bound.
;;; Since the state was copied by the caller and this is the only
;;; function to get it, it is safe to modify the bindings of the
;;; state.  On success, the given state, with any new bindings, is returned.

(defun execute-bind (E State)
  "Implements the (bind <variable> <function>) condition of operators.
   Returns zero or one state."
  (let* ((Variable (second E))
         (New-Value (eval (subst-bindings-quoted (St-Bindings State) 
						 (third E))))
	 (new-bindings (unify Variable New-Value (St-Bindings State))))
    (when new-bindings (setf (st-bindings state) new-bindings) (list State))))
	   
;;; The executable precondition (test <form>) succeeds if
;;; evaluating the given form in Lisp returns non-nil.  ?variables in
;;; the form are "bound" by replacing them with (quote <value>) where
;;; <value> is the variables value in the given state.  The state was
;;; already copied, so there's nothing left to do here.

(defun execute-test (E State)
   "Implements the (test <function>) condition of operators."
   (if (eval (subst-bindings-quoted (st-bindings State) (second E)))
       (list State)))

;;; Should have the form (add-to-wm <expression>) Can't fail, so it
;;; just adds a ground version of the expression to working memory in
;;; the given state.

(defun execute-add-to-wm (Ex State)
  "Implements the add-to-wm executable precondition"
  (push-wm (subst-bindings (st-Bindings State) (second Ex))
	   State)
  (list State))

    
;;; precondition (in-wm <propositions>) returns a singleton set
;;; consisting of the given state if the proposition unifies with some
;;; element of working memory.  If there is no unifier, then it
;;; returns an empty set.  If a unifier is found, the bindings are
;;; added to the state.  This is just like goal-successors except that
;;; it doesn't call goal-successors-effects.  By passing NIL as the
;;; value for action-flag, it suppresses adding an action to the
;;; states.

(defun execute-in-wm (Ex State)
  "Returns a set of states, one for each unification of the given 
   goal with a wme." 
  (goal-successors-wmes (second Ex) State NIL))

;;; (wm-or-derive <goal>) checks if a goal can be satisfied by wm. If yes,
;;; successor states are the wm satisfiers alone; otherwise it attempts 
;;; to derive the goal by operators. This can be used for efficiency to 
;;; suppress needless search through operators in contexts where all we care 
;;; about is the result in wm.  Maybe needs a better name. Ensure-in-wm? 
(defun execute-wm-or-derive (Ex State)
  (or (goal-successors-wmes (second Ex) State t)
      (goal-successors-effects (second Ex) State)))

;;; The executable (Any-member <term> <set>) expects <set> to be a
;;; list of terms when bindings are substituted into it.  It returns a
;;; set of states, one for each member of the list that the given
;;; <term> unifies with, with the variable in the term bound to the
;;; member.

(defun execute-any-member (Ex State)
  "Returns a set of states, one for each member of the third element of 
   (any-member <term> <set>) that unifies with the given second element."
  (let ((Term (second Ex))
	(TermSet (subst-bindings (st-bindings State) (third Ex)))
        (bindings)  (NewState)  (successors))
    (unless (listp TermSet) (error "Non-list ~A given to any member" TermSet))
    (dolist (Value TermSet)
      (when (setq bindings (unify Term Value (st-bindings State)))
	(setq NewState (copy-st State))      
	(setf (st-bindings NewState) bindings)
	(push NewState Successors))) 
    (nreverse Successors)))

;;; The executable (not <proposition>) returns a set containing the
;;; given state if the given atomic proposition does not unify with
;;; any working memory element.  If the proposition does unify with
;;; some wme, it returns NIL.
;;;
;;; Not also includes an optional second argument that is a test
;;; proposition. If present this proposition will be evaluated with 
;;; the bindings substituted and iff it returns t the not will
;;; return nil.

(defun execute-not (Ex St)
  "Returns NIL if the <prop> of executable (not <prop> &optional <test>) 
   matches a wme and <test> if present returns t, a set consisting of 
   the given state otherwise."
  (if (= (length (cdr Ex)) 2)
      (execute-test-not Ex St)
    (execute-unary-not Ex St)))


(defun execute-test-not (Ex St)
  "Execute the (not <prop> <test>) form."

  (let ((B))
    (if (loop for wme in (st-wm St) never 
	      (and (setq B (unify (second ex) wme (st-bindings st)))
		   (eval (subst-bindings-quoted B (third ex)))))
	(list St))))


(defun execute-unary-not (Ex St)
  "Execute the (not <prop>) form."
  (if (loop for wme in (st-wm St) never 
	    (unify (second ex) wme (st-bindings st)))
      (list St)))


;;; The executable (debug <string> . <args>) is like a format
;;; statement, except that it only prints when *debug* is non-null and
;;; it acts like the values of ?variables are bound by replacing them
;;; with (quote <value>).

(defparameter *debug* NIL 
  "When true, the executable precondition (debug <string> . <args>) prints")

(defun Execute-Debug (Exp St)
  "If *debug* is non-null, prints the given format stuff."  
  (let ((call (cdr Exp)))
    (cond ((not *debug*))
	  ((Stringp (car call))		;If this is a typical debug call
	   (eval (concatenate 'list 
		   '(format t) 
		   (subst-bindings-quoted (st-Bindings St) call))))
	  ((eql (car call) 'State)	;If the user wants the state printed.
	   (eval (concatenate 'list 
		   '(format t) 
		   (subst-bindings-quoted (st-Bindings St) (cdr call))))
	   (format t "Current Solver State ~A~2%" St))
	  (t (error "Malformed Debug Executable:~%  ~A" Exp)))
    (list st)))

;;; The executable (rdebug <string> . <args>) is like a format
;;; statement, except that it only prints when *rdebug* is non-null and
;;; it acts like the values of ?variables are bound by replacing them
;;; with (quote <value>).

(defparameter *rdebug* NIL 
  "When true, the executable precondition (debug <string> . <args>) prints")

(defun Execute-rdebug (Exp St)
  "If *rdebug* is non-null, prints the given format stuff."  
  (let ((call (cdr Exp)))
    (cond ((not *rdebug*))
	  ((Stringp (car call))                                           ;;If this is a typical debug call
	   (eval (concatenate 'list 
		   '(format t) 
		   (subst-bindings-quoted (st-Bindings St) call))))
	  ((eql (car call) 'State)                                      ;;If the user wants the state printed.
	   (eval (concatenate 'list 
		   '(format t) 
		   (subst-bindings-quoted (st-Bindings St) (cdr call))))
	   (format t "Current Solver State ~A~2%" St))
	  (t (error "Malformed rdebug Executable:~%  ~A" Exp)))
    (list st)))


;; The Count form implements (Count <Var> <Form>) Count binds Var to
;; the number of WME's in the current working memory that unify with
;; Form and returns a list containing the state.

(defun execute-count (Exp State)
  "For the executable (count <var> <prop>), binds <var> to the number of wme
   in the given state unify with <prop>."
    (push-binding (second Exp)
		  (loop for wme in (st-wm State) 
		      count (unify (third Exp) wme (st-bindings State)))
		  State)
    (list State))

;;; This executable is (setof <Goal> <Term> <SetVar>) where <Setvar>
;;; is a ?-prefixed variable, <Term> is a term and <Goal> is an atomic
;;; proposition.  Goal must include Term.  Setof finds all possible
;;; proofs of Goal, then puts ground versions of <Term>, one for each
;;; state, into SetVar's value.  Setof returns a set consisting of
;;; single state.  The state is a copy of the given state, except that
;;; SetVar is bound to the set, and the working memories and histories
;;; of the derivations are merged into its wm and history.  The path
;;; is strange (see setof-paths). For instance,

;; (setof (vector ?b (force ?b . ?rest) ?dir)
;;        (force ?b . ?rest) 
;;        ?forces)

;;; will generate all force vectors for ?b, and return a state which
;;; contains those derivations in its working memory and history, plus
;;; a binding of ?forces to the set of force terms.

(defun execute-setof (Ex State)
  "Executes the setof precond executable and returns a set containing a single state"
  (let* ((Goal (second Ex))
	 (Term (third Ex))
	 (SetVar (fourth Ex))
	 (Initial-state (copy-st State))
	 (Result))
    (setf (st-stack Initial-state) (list Goal))
    (dolist (SubState (solution-sts initial-state))
      (setf (st-wm State)
	(union (st-wm SubState) (st-wm State) :test #'unify))
      (setf (st-history State)
	(union (st-history SubState) (st-history State) :test #'unify))
      (pushnew (subst-bindings (st-bindings SubState) Term)
	       Result :test #'unify))
    (push-binding SetVar Result State)
;; The history is long and not in tree form
;; However, entries are used by the Helpsystem
    (setof-actions state initial-state)
;; Show the end result; see removable-actionp
    (push (list 'setof-result Result) (st-actions State))
    (list State)))

;;; The action list for the resulting state consists of <subacts> 
;;; for each substate.  Here <subacts> is a list
;;; of all the actions taken to achive the substate, with the oldest
;;; action first.  However, if none of the substates have any actions,
;;; then the action list is empty.

;; version without split and join and all that
(defun setof-actions (state initial-state)
  "Collects actions from the substates and puts them on the state"
  (setf (st-predecessor initial-state) NIL) ;halts actions-along-path
  (dolist (SubState (solution-sts initial-state))
    (setf (st-actions state)
	  (append (actions-along-path SubState)
		  (st-actions state)))))

;;; The action list for the resulting state starts SPLIT, then NEXT
;;; <subacts> for each substate, then JOIN.  Here <subacts> is a list
;;; of all the actions taken to achive the substate, with the oldest
;;; action first.  However, if none of the substates have any actions,
;;; then the action list is empty.  This is detected by comparing the
;;; number of actions with the number of states.  If there are N+1
;;; actions for N states, then N of the "actions" are *next*
;;; and the last one is *join*, so there were no real actions.

#|  ;not working for the case without joins and splits

(defun setof-actions (state initial-state)
  "Collects actions from the substates and puts them on the state"
  (setf (st-actions state) (list *join*)) ; so JOIN will be last
  (setf (st-predecessor initial-state) NIL) ;halts actions-along-path
  (dolist (SubState (solution-sts initial-state))
    (setf (st-actions state)
      (cons *next*
	    (append (actions-along-path SubState)
		    (st-actions state)))))
  (if (= 1 (- (length (st-actions state))
		  (length (solution-sts initial-state))))
      (setf (st-actions state) NIL)
    (push *split* (st-actions state))))
|#

;;; This converts a list of states into a list of the actions that
;;; they contain.

(defun actions-along-path (st)
  "returns a list of actions from the given state back to the initial one,
   with the oldest action first."
  (if st
      (append (actions-along-path (st-predecessor st))
	      (st-actions st))))
