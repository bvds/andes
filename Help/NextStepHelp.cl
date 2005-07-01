;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NextStepHelp.cl
;; Collin Lynch (Cl) <CollinL@pitt.edu>
;; Kurt VanLehn (Kvl) <VnaLehn@cs.pitt.edu>
;; 7/18/2002
;;;
;; ChangeLog:
;;   6/12/2003 - (Cl) - fixing compiler warnings.
;;    1. Declared *nsh-solution-sets* to be special in nsh-multi-axis-problemp.
;;    2. Declared **nsh-standard-axis-prompt**, **nsh-rotated-axis-bottom-prompt**,
;;       and **nsh-rotated-axis-prompt** to be special in nsh-make-axis-prompt.
;;    3. removed bad ignore declaration from nsh-pick-fp.
;;
;;   1/6/2004 - (CL) - Added note reflecting exposed state of 
;;      nsh-collect-principle-bodyents and added note to the effect on 
;;      todo list.
;;

;;; This file provides next-step-help for the andes2 system.  It 
;;; was based initially on psudocode wroitten by Kurt VanLehn and 
;;; has since been adapted by Collin Lynch to provide the necessary
;;; interactivity and operability.  The essential algorithm has 
;;; remained unchanged however the system itself has changed somewhat
;;; in the essential supporting elements.

;;; The comment below states in more verbose language than here what the
;;; NSH algorithm is.  In breif we want to guide the students through the
;;; following solution Algorithm:
;;;
;;; 1. Define Bodies.
;;;    Identify all of the useful bodies in the problem (appear in any
;;;    solution) and use the body tool to enumerate them on the screen.
;;; 
;;; 2. Define Axes.
;;;    Draw a set of Axes that look appropriate.  For problems that do not
;;;    contain axes at all we don't want the students to do this.  In future
;;;    the physicists have specified that they want it to be correct to 
;;;    define axes on ALL problems.  
;;;
;;; 3. Define Givens
;;;    Define all reasonable Givens here defined as any givens that are
;;;    used in any problems, ever.  
;;;
;;; 4. Select the sought
;;;    This is a matter of picking (one of) the problem's sought(s) from
;;;    the quantities menu.   
;;;
;;; 5. First Principle(s).
;;;    Pick the first principle that you will use to find the sought.
;;;    Note that this is not, in fact, necessarily connected to the 
;;;    sought directly.
;;;
;;;   NOTE:: At this point we may find that the ideal solution has different
;;;      Axes rotation than whatever axes the student has drawn.  Before we 
;;;      begin prompting the student to work on the principle we will want to
;;;      offer them the chance to alter their axis rotation or to go on to
;;;      a different solution.  This may have to be part of the selection
;;;      process.  
;;;
;;; 6. Do the rest
;;;    This is a simple iterative prompt cycle that we use to tell the
;;;    students to complete the two-pass model that we defined below.
;;;
;;;
;;; NOTE: Because we want the students to begin with bodies and axes, then
;;;  we do not consider completing these to be "entering" a principle.
;;;  this is a fairly academic issue but it does affect the hints that we
;;;  will give I.E. "you should Continue" v. "you should start"


;;; =================== New USNA Comment =================================
;;; The goal of NSH is still to guide the students through the problem
;;; in a (generally) DFS manner.  However several changes have been made.
;;; Firstly we have moved to a new "two-pass" backward chaining model.
;;; Rather than encourage the students to simply select the next principle
;;; in the graph the USNA physicists want thew students to complete all 
;;; of the Major PSMS first and then go back and complete all other minor
;;; PSMS.
;;;
;;; Secondly (and more importantly) we no longer make use of the preamble
;;; when the students call up NSH initially they will be given a breif 
;;; preamble to get them into sought.  However that is the last that they
;;; will see of the questions.  At that point it will be entirely run by
;;; the system which will prompt the students with the principles that
;;; they should work on.  
;;;
;;; The following is copied from my e-mail to the USNA:
;;; When the student calls NSH they will be in one of 3 states:
;;;
;;; 1. they have done no work and the problem is a blank, or
;;; 2. They have done some work on their own and may or may 
;;;    not have completed any principles.
;;; 3. The student has been following NSH and has a stored plan.
;;;
;;; In the event of 1 it is my understanding that you want us to give them
;;; a preamble similar to the one currently provided:  
;;;
;;; [Student Opens EXDT2A]
;;;
;;; T: What quantity is the problem seeking?
;;; S: The vertical component of the Net Force on block2 at time T0 to T1
;;; 
;;; T: That quantity is not mentioned in the Andes solution.   
;;;    Please try again.  What quantity is the problem seeking?
;;; S: The distance traveled by block2 at time T0 to T1
;;; 
;;; T: That quantity is not mentioned in the Andes solution.   
;;;    Please try again.  What quantity is the problem seeking?
;;; S: The magnitude of the Displacement of block2 at time T0 to T1
;;; 
;;; T: That's right.  Which major principle should you use to find it?
;;; S: Constant acceleration
;;; 
;;; T: That's right...
;;; 
;;; 
;;; 
;;; If the student is in state 2 then the system will test to determine if 
;;; the student has completed the last principle in their plan.  If so then 
;;; they will be moved to 3.  If not, then the next entry in that principle 
;;; will be hinted to them using the same goal/hints that we use now:
;;; 
;;; T: That's right..  Try drawing a diagram showing all of the needed 
;;; kinematic vectors of the block2 from T0 to T1 and coordinate axes
;;;         Explain further     OK 
;;; 
;;; T: That's right.  It is a good idea to begin by choosing the 
;;;    body or system of bodies you are going to focus on.
;;;         Explain further     OK 
;;; 
;;; T: First figure out which object you want to apply the principle to, 
;;;    and if necessary, what time or time interval to analyze.  Then 
;;;    use the body tool (looks like a dot) to indicate your selections.
;;;         Explain further     OK 
;;; 
;;; T: You should use the body tool to draw a body choosing the block2 
;;;    as the body and from T0 to T1 as the time.
;;;         OK 
;;; 
;;; 
;;; 
;;; Lastly, if the student is in state 3 then NSH will determine what principles 
;;; the student has completed.  If they have completed all of the principles in a 
;;; problem solution then they will be prompted to write the answer and quit.  If 
;;; not then Andes will prompt them to complete the next major principle in the 
;;; solution as follows:
;;; 
;;; T: At this point you should be working on the Newton's 
;;;    Second Law major principle.
;;; S: OK...
;;; 
;;; 
;;; If the student has completed all of the major principles then Andes will prompt 
;;; them to work on the next minor principle in the solution as follows:
;;; 
;;; T: At this point you should be working on the tensions 
;;;    equal on strings minor principle.
;;; S: OK...
;;; 
;;; At no time (apart from the beginning or in KCDs) will the students see the preamble 
;;; as we know it.  This version of the solution takes away their ability to decide 
;;; exactly what plan they should be using in order to eliminate the confusing 
;;; question/answer stack.  
;;;
;;;
;;; This has since been updated to the following: 
;;; 
;;; When the student calls NSH:
;;;
;;; If they have done no work 
;;;    Then prompt them to define a given psm.
;;;
;;; If they have been using NSH and have a **last-psm** stored
;;;    Then, if it is complete:
;;;       Clear it and prompt them to move on.
;;;    Else, prompt them to complete it.
;;;
;;; If they have not completed all of the givens
;;;    Prompt them to complete them and move on.
;;;
;;; If they have completed all of the givens, 
;;;    Prompt them to pick a sought and a Major PSM
;;;      to complete it.  Unless they have already
;;;      been working on that sought in which case they should continue.
;;;
;;; If they have completed all of the major PSMS 
;;;   Then promp them to complete the minor ones.
;;;
;;; And they are done.
;;;
;;; ================== Original NSH Comment ==============================
;;; The basic idea behind the next-step help is that the student will
;;; be taken step by step throughthe graph first at the bubblegraph
;;; level and then later at the psm path level.  In order for the help
;;; to be provided the bubblegraph is linked to for future use.
;;;
;;; Initially the system will ask the student what quantity the problem
;;; is seeking.  The help system does this by generating a tutor turn for
;;; use by the workbench.  In the workbench this will pop up in a dialog
;;; box with a menu or some other slection system that allows them to 
;;; identify the specific quantity.  There may be more than one in which 
;;; case the student is asked to select the one that they are working on 
;;; presently.  
;;; done with the problem.  If the quantity has been assigned a value in
;;; the student equations (by means of an assignment statement) then they
;;; will be prompted to enter the value in the answer box.  If the 
;;; quantity has been completed (IE. all of the necessary work has been 
;;; done to write the assignment statement but the statement has not yet
;;; been written) then the student will be prompted to write the statement.
;;; If the quantity is invalid (I.E. is not one of the soughts) then they
;;; will be informed of this error and asked to try again.  
;;;
;;; If the selected quantity is a valid sought but has not yet been completed
;;; then the system will prompt the student to select the psm that they
;;; are using in order to find it.  As before this will bring up a selection
;;; method (probably a menu) for them to use.  If they select a psm that is 
;;; unconnected to the sought quantity then they will be prompted to select 
;;; again.  Similarly if the psm is on a dead path or is forbidden then they 
;;; will be informed of it and asked to select again.  If the student fails 
;;; to select a correct quantity within a set number of tries they will be
;;; given a quantity to use.
;;;
;;; If the psm is a valid one and a complete set of the systementries within 
;;; it have been entered by the student then they will be prompted to select 
;;; which of the remaining quantities in the psm (apart from the sought) 
;;; they are attempting to find.  They will again be prompted with the same 
;;; menu and can make the same choices.  Once they have selected a valid 
;;; quantity (apart from the sought) then the process recurses.  They will 
;;; be asked to select a psm to find this new quantity and will again determine 
;;; if it is correct.  This psm will be subject to the same error checking 
;;; save that it will error if the psm appears above in the stack.  As with
;;; the quantities the students are given a maximum number of tries to select
;;; a psm corrdctly before they will be told one to use.
;;;
;;; If the psm has not been entered then the system will traverse the psm's 
;;; path to determine what is the next entry that needs to be made.  In the 
;;; process of doing so the system will accumulate a stack of goals from the
;;; path itself.  The system will then send a series of tutor turns to the 
;;; student.  Each turn will prompt the student in increasing detail about 
;;; the desired step beginning with something of the form:
;;;     'Newton's law depends upon mass'
;;; The student will then be given the option of ending the prompt or 
;;; continuing further.  Eventually the system will 'bottom out' with
;;; a hint such as: 'Define the mass variable.'  Hints can also be 
;;; minilessons or more complex tutorials as defined in the operators
;;; that they are taken from.


;;;; The following comments are from Kurt VanLehn's Psudocode.

;;; This file (NextStepHelp2.cl) is pseudo code to test a proposed
;;; mechanism for controling the tutorial dialog in Andes2.  It is
;;; only concerned with the next step help, because it generates the
;;; most complex dialog.  The basic idea is that when the student
;;; clicks on the next-step help button, the workbench sends a message
;;; to the help system (lisp).  The message is deciphered by Linn's
;;; dialog manager (not included in this file).  Since it turns out to
;;; be a request for next step help, Linn's code calls the function
;;; next-step-help, which appears below.  It returns a tutor turn
;;; struct.  This struct has a type, which defaults to dialog (the
;;; other types are play-minilesson, color-red and color-green).  When
;;; the struct has type dialog, then the text field is a string to be
;;; printed in the dialog window.  The menu field is the name of a
;;; menu that is predefined in the workbench.  The responder field is
;;; a one-argument function for handling the student's response.

;;(defstruct tutor-turn
;;  (type 'dialog)				;an atom: defaults to DIALOG
;;  text					; a string (or atom for some types)
;;  menu					; the name of a workbench menu
;;  responder				        ; a one argument function
;;  )

;;; When Linn's code gets a tutor-turn struct back from calling
;;; next-step help, if the type is dialog, then it puts the workbench
;;; into dialog mode if it is not in that mode already.  It then has
;;; the workbench display the text in the dialog window and put up the
;;; menu.  Then it waits for the student.  When the student's response
;;; arrives from the workbench, it will usually be a response off the
;;; menu.  However, it might be some kind of escape (e.g., QUIT).  We
;;; haven't figured out yet what all of them will be, but we will at
;;; least need some way for the student to stop Andes from running
;;; even if the student is in the middle of a dialog.  If the student
;;; action is an escape, then Linn's code handles it itself.  On the
;;; other hand, if the student action is not an escape of some kind
;;; but instead is a response, then Linnn's code calls the
;;; one-argument function of the tutor turn struct on that response.
;;; That function returns either another tutor tutor struct or the
;;; atom HIDE, which indicates that the dialog is over.  Linn's code
;;; handles this tutor turn struct just as described before.  The atom
;;; HIDE causes Linn's code to tell the workbench to end Dialog mode
;;; and resume problem solving mode.

;;; If Linn's code ever gets a play-minilesson tutor turn struct,
;;; here's what it does.  It tells the workbench to play the
;;; minilesson, and passes it the file name, which is in the text
;;; field of the struct.  When the workbench reports that the
;;; minilesson has finished playing, Linn's code puts the workbench in
;;; dialog mode if it is not in dialog mode already.  It displays the
;;; message "You can either get another hint or resume problem
;;; solving" then puts up the menu in the menu field of the tutor turn
;;; struct.  The response to this menu is handled by the function in
;;; the responder field of the tutor turn struct.

;;; I won't bother to describe how Linn's code handles other types of
;;; tutor turn structs, since they are not used by this code.

;;; The one-argument functions that occupy the responder fields of the
;;; tutor turn struct are closures. That is, they refer to data (e.g.,
;;; the state of the dialog) that was present at the time the tutor
;;; turn struct was created.  The alternative is to keep the
;;; appropriate dialog state in global variables.  However, the
;;; closure-based design seems more flexible.  To add a new dialog,
;;; the coder can create arbitrary dialog state in the arguments of
;;; his functions, referring to it with closures, and not have to
;;; change any of the other dialog code.

;;; However, when the dialog is over, all this dialog state vanishes.
;;; That's a good thing, since we don't need it anymore.  However,
;;; some aspects of the dialog need to be remembered across dialogs
;;; (but not across different sessions).  For these, the global
;;; variable *dialog-history* is used.  Currently, it contains only a
;;; list of hints that have been given.  They are needed so that if
;;; there are two or more hints with the same function, then Andes
;;; will choose one that has not yet been given. In the future, we may
;;; want to use the dialog history in order to avoid repeating hints
;;; or preamble questions that have been used before.  This would make
;;; the conversation shorter and that might make students more willing
;;; to ask for help, but it risks generating misunderstandings.  It
;;; assumes that the student actually read the hint or preamble
;;; questions when it was given earlier, and that the student
;;; remembers it.  We need data from pilot subjects before we can know
;;; how much repetition to avoid, if any.
;;; =================== End Old NSH Comment ==============================


;;; A word on ASSOCS
;;; For the purposes of hinting NSH uses assocs to facilitate machine catologuing
;;; of the later activities.  NSH calls that use assocs will be of the form 
;;; (nsh <FuncName> <Values>) where Funcname is the assoc being called and 
;;; the values are the values relevant to reconstructing the hint independent
;;; of the text itself.  These values will typically be axes or other calls
;;; and will be dependent upon the <FuncName> in question.


;;;=====================================================================
;;; NSH changes.  12/13/2001
;;; In order to upodate NSH several changes are being made to the system:
;;;
;;; 1. Use of NLG.  Rather than returning explicit text NSH will now 
;;;    return intentional algorithms that interface with the NLG system.
;;;    Each call will be an expression of the form (<type> <Args>) 
;;;    Where type is an Expression type and <Args> is a list of arguments.
;;;    
;;;    The NLG system will take this expression and turn it into a
;;;    random english phrase.  That is then passed to the student.
;;;    This will ensure that the NSH/Student Dialogue is not repetative
;;;    to its current extent.
;;;
;;; 2. Nonlinear planning.  Allthough NSH is still constrained to 
;;;    describing plans in linear fasion it will now store the complete
;;;    nonlinear plan that the student has assembled.  This will allow
;;;    nsh to better backtrack when the students are done with one branch
;;;    and to keep track of circular plans.
;;;
;;; 3. Improved summarization.  Rather than explicitly walking students 
;;;    down the full path from the top to the bottom the system will 
;;;    attempt to summarize their plan.  This summary will be time 
;;;    dependent s.t. The longer the student has been out of the 
;;;    help the more they will be told.  
;;;
;;;    This time dependent hinting reminds them of the most recent goal
;;;    backwards or "up" to the sought.  Describing the tree is the issue.
;;;
;;;    This is coupled with more responsive hinting.  Which is based upon
;;;    three variables, time, state change, and Bottoming out.
;;;   

;;; There are four variable metrics that we can use to deal with the nsh 
;;; sorting.  These are polled everytime that NSH is called.

;;; CorrectEntry:  Did the students make the desired entry the between the 
;;; last time that they called NSH and now?  State Change:  Did the students 
;;; change the state in any way (made entry/deleted entry) 



;;; 4. Color coding.  The system will now use color coding rather than 
;;;    italics for emphasis and dialogue separation.  This will help the
;;;    students to read and keep up with the dialogue and to reread it.
;;;

;;;-----------------------------------------------------------------------
;;; On NSH:
;;;    If Plan = Nil
;;;       Walk(ask("What quantity is the problem seeking."))
;;;    Else 
;;;      If Plan = Complete.
;;;         Hint("You are done.")
;;;      Else
;;;        time-remind-plan.
;;;
;;; On Time-remind-plan:
;;;    If last-wal

;;;
;;; 

;;;        Time-remind-plan
;;;        
;;;        If Hint("Do you wish to continue?") = No
;;;           Walk(ask("What quantity is the problem seeking?"))
;;;        Else
;;;          If Current-Plan.next = decision.
;;;             walk(ask(current-plan.next))
;;;          Else 
;;;             walk(current-plan.next)
;;;
;;; On Hint(<Expression>):
;;;    return tutor-turn(nlg(expression))
;;;
;;; On Walk(<Location>):
;;;    Hint location
;;;       


;;;; ==========================================================================
;;;; TODO List.
;;;; 1. Move or replicate nsh-collect-principle-bodyents code in another
;;;;    location to avoid its current exposed state.
;;;; 2. Determine if nsh-collect-old-first-principles is still used on 
;;;     any problem and if not remove it.  


;;;; ======================== Public utility code =============================
;;;; This section contains some public utility code designed to aid in Andes2
;;;; Development that makes sense here.

;;; Return t if one of the stored solutions contains more than one axis.
(defun nsh-multi-axis-problemp ()
  "Return t if one of the stored solutions contains more than one axis."
  (declare (special *nsh-solution-sets*))
  (member-if #'nsh-multi-axis-solutionp *nsh-solution-sets*))


;;;; ========================= Stored params =================================
;;;; The following parameters store the current state of the help system and 
;;;; keep track of the student's work for later use.  

;; Retry values.  These parameters control the number of times that a student
;; may retry the sought and first-principle selections.
(defparameter **max-sought-tries** 3 
  "The maximum number of errors permitted to the students in NSH.")
(defparameter **max-first-principle-tries** 2 
  "The maximum number of errors permitted to the students in NSH.")


;; Debugging output.  These parameters control the appearence 
;; (or not) of debugging messages. 
(defparameter **Print-NSH-Stack** nil 
  "Print out the NSH stack for hint purposes.")

;; These parameters hold flags for the current problem These values
;; will be reset when a new problem is loaded.
(defparameter *nsh-problem-type* () "The type of the problem (quant/no-quant).")
(defparameter *nsh-valid-entries* () "The valid entries for NSH to prompt.")

(defparameter *nsh-bodysets* () "List of per-solution body drawing entry sets.")
(defparameter *nsh-axis-entries* () "A zero axis if it is correct.")
(defparameter *nsh-givens* () "The givens in NSH.")
(defparameter *nsh-solution-sets* () "The individual solution sets for the student.")

(defparameter *nsh-last-node* () "The last principle being solved.")
(defparameter *nsh-current-solutions* () "A temporary var containing the active solutions.")

;;; This parameter holds an alist that associates soughts to first principles.  When 
;;; a search for the first principles given the sought is made then the system will 
;;; first search here to see if the value has been cached.  If not then it will search
;;; for the values directly and then store them in here.
;;;
;;; This parameter should only be directly addressed by the nsh-collect-first-principles
;;; function.  
(defparameter *nsh-first-principles* () 
  "The relavent first-principles for this problem.")


;; Removable 
(defparameter *nsh-nodes* () "The nodes that can be hinted.")



;;;; ======================== Setup code ======================================
;;;; The clear-nsh and main nsh functions are used to clear NSH and then to
;;;; set it up for runtime use.  The main NSh function is the one that will 
;;;; be called whenever NSH is needed, and it will behave differently depending 
;;;; upon the problem state.  The state is dealt with by the student's current 

;;; Setting up Next-step help is a process of clearing out the stored entires.
;;; In the case of no-quant problems we just want to obtain the nodes in the
;;; graph (on the grounds that all are necessary, and to store them for later
;;; use along with a flag specifying the type of NSH.
;;;
;;; Quantity problems are more complex as we want to obtain the given and non-given
;;; nodes and split off the solutions for later use.  This is done and then the 
;;; results are stored for later use along with the flag specifying the type of
;;; solution that it is.

(defun reset-next-step-help ()
  "Reset the Nexst-step help settings."
  (nsh-reset-general)

  (if (no-quant-problem-p *cp*)
      (if (nsh-multiple-choice-only-problem-p *cp*)
	  (nsh-reset-multiple-choice)
	(nsh-reset-no-quant))
    (nsh-reset-quant))
  
  (nsh-collect-valid-entries)
  (nsh-collect-body-entries)
  (nsh-collect-axis-entries))



(defun nsh-reset-general ()
  (setq *nsh-current-solutions* nil)
  (setq *nsh-last-node* nil)
  (setq *nsh-bodysets* nil)
  (setq *nsh-givens* nil)
  (setq *nsh-axis-entries* nil)
  (setq *nsh-first-principles* nil)

  (nsh-clear-next-call) 

  (setq *nsh-nodes* 
    (remove-if-not 
     #'bgnode-entries
     (append (remove-if #'qnode-parameterp (bubblegraph-qnodes (problem-Graph *cp*)))
	     (bubblegraph-enodes (problem-graph *cp*))))))


;;; --------------------------- No Quant ------------------------------------
;;; For no-quant problems we need to set the problem to ID that and then
;;; collect all of the entries in the graph (no separate solutions will 
;;; exist).  These then become the one and only problem solution.  
;;; Any givens will be removed from this set and added to the *nsh-givens*
(defun nsh-reset-no-quant ()
  "Setup NSH for no-quant use."
  (setq *nsh-problem-type* 'no-quant)
  (setq *nsh-solution-sets* (list (remove-if #'nsh-given-principle-p *nsh-Nodes*)))
  (setq *nsh-givens* (remove-if-not #'nsh-given-principle-p *nsh-Nodes*)))



;;; ---------------------- Multiple Choice ----------------------------------
;;; Multiple choice-only problems are those in which the student only needs
;;; to answer a set of multiple choice questions.  No other steps need occur
;;; in the future we may combine multiple-choice questions with other 
;;; types of soughts such as quantity soughts but we have none right now.

;;; A problem is multiple-choice only if all of its soughts are of the form: 
;;; (CHOOSE-ANSWER MC-# #).  This code will return t if that is the case.
;;; Note: This code assumes that the problem has at least one sought.  
(defun nsh-multiple-choice-only-problem-p (Problem)
  "Return t if this is a multiple-choice only problem."
  (nsh-mcop-p (problem-soughts Problem)))

(defun nsh-mcop-p (Soughts)
  "Return t if all the soughts are 'choose-answer'"
  (when (equalp (caar Soughts) 'CHOOSE-ANSWER)
    (if (= 1 (length Soughts)) t
      (nsh-mcop-p (cdr Soughts)))))


;;; For these problems we will need to set the problem type and then to 
;;; collect the set of multiple choice soughts.  When the student asks for
;;; help all that we want to tell them is to work on the next sought.  There
;;; is no need to collect givens or other values.  
(defun nsh-reset-multiple-choice ()
  "Reset NSH for a multiple-choice only problem."
  (setq *nsh-problem-type* 'mc-only))


;;; ------------------------- Quant -----------------------------------
;;; Quantity problems will have solutions and givens so we make use
;;; of those parameters to make our decisions.
;;; 
;;; Set the appropriate problem type for later use, and then
;;; collect up the nodes that should be in NSH and the 
;;; appropriate solutions.  This includes collecting all
;;; nodes with entries, principles and quantities such as
;;; parameters.  These latter will fall in with the minor
;;; principles at the end of the solution hinting.

(defun nsh-reset-quant ()
  "Setup the appropriate settings for the quant problem."
  (setq *nsh-problem-type* 'quant)
  (let ((nodes (nsh-collect-quant-snodes *cp*)))
    (setq *nsh-givens* (sort (remove-duplicates (mapcan #'car nodes))
                             #'earlier-given :key #'nsh-given-node-quant))
    (setq *nsh-solution-sets* (mapcar #'cadr nodes))))



(defun nsh-given-node-quant (enode)
  "return quantity for a GIVEN enode"
    (second (nsh-principle-expression enode))) ; form is (GIVEN ?quant)

;;; For now, very simple sort only ensures earlier time quants come before 
;;; later in list.  More sophisticated sort to be added later.
(defun earlier-given (quant1 quant2)
  "true if given quant1 should be entered before given quant2"
  (let* ((t1 (time-of quant1))		; time-of, tearlierp in KB/PhysicsFuncs.cl
         (t2 (time-of quant2)))
     (and t1 t2 (tearlierp t1 t2))))



;;; Given a standard quantity problem collect all of the 
;;; nodes in each solution splitting them into 
;;; (givens . Non-givens) and saving the result(s) for
;;; later.
(defun nsh-collect-quant-snodes (Problem)
  "Collect the solution nodes from the nsh-problem."
  (mapcar #'(lambda (S) (nsh-cqsnodes-lambda S (Problem-graph Problem))) 
   	  (problem-solutions Problem)))

(defun nsh-cqsnodes-lambda (Solution Graph)
  "This lambda func should only be called from above."
  (nsh-sort-node-lists
   (reverse 
    (remove-if-not 
     #'bgnode-entries 
     (map-indicies->bgnodes (Eqnset-Nodes Solution) Graph)))))



;;; sort-node-lists
;;; When presented with a list of bgnodes sort them into three
;;; sets, Givens, Major psm nodes (the first of which will be 
;;; the first major node, and all the rest.  This is done so that
;;; later on we can test them by class. 
;;;
;;; Note the last major principle node in the list will be the first one
;;; that we want the students to select, hence its reversal.
;;;
;;; Note remove and remove-if preserve order.
(defun nsh-sort-node-lists (nodes)
  "Sort the suppliued nodes into (<Givens> <Major> . <Rest>)"
  (list (remove-if-not #'nsh-given-principle-p nodes)
	(append (remove-if-not #'nsh-major-principle-P Nodes)
		(remove-if #'(lambda (N) (or (nsh-given-principle-p N)
					     (nsh-major-principle-p N)))
			   nodes))))



;;;------------------------- Entries ---------------------------------
;;; Searching for entries.
;;; Once we have identified the entries for both the bodies and axes
;;; because we only want to hint relevant entries, we need to select
;;; strictly from among those in the solution sets and givens.  The
;;; code here collects entries from these locations by unifying them 
;;; with the specified propositions.

;;; Collect all of the entries that are part of any NSH valid node
(defun nsh-collect-valid-entries ()
  "Collect all entries that are part of any NSH valid node."
  (setq *nsh-valid-entries*
    (remove-duplicates
     (loop for P in (cons *nsh-givens* *nsh-solution-sets*)
	 append (loop for N in P 
		    append (bgnode-entries N))))))
		  

(defun nsh-filter-valid-entries (filter)
  (remove-if-not 
   #'(lambda (E) 
       (unify filter (systementry-prop E)))
   *nsh-valid-entries*))



;;; -------------------- axis entries ---------------------------------
;;; Filter the problem to collect the axes drawing entries 
;;; and store them.
(defun nsh-collect-axis-entries ()
  "Collect all of the axis entries in the problem."
  (setq *nsh-axis-entries*
    (nsh-filter-valid-entries '(draw-axes ?rot))))



;;; --------------------- Collecting Bodies --------------------
;;; We have the pedagogical goal of getting the students to 
;;; define the bodies necessary for a solution first.  In theory 
;;; we would like to tell the students to do everything necessary
;;; for a solution but that is not the same for many problems.  
;;; therefore, in lieu of anything else I have taken to prompting
;;; them with body solutions in this way.  That way, in the case
;;; of the book on a package on a table problem the student can 
;;; go ahead and just define the compound body and not waste any
;;; time rather than defining the full suite of potential bodies
;;; (book package and table) and then being prompted to define
;;; the compound body later.
;;;
;;; There are actually two rounds of the hints here.  We want the
;;; students (so far as I know) to think of this process as one of
;;; drawing all of the necessary bodies to solve the problem but 
;;; there are really two rounds of bodies.  The first group come 
;;; from the givens and operate on the principle that, if we will
;;; have them define mass then we will have them do it
;;;
;;; The bodies that are appropriate are collected from the set of
;;; principle nodes.  Nodes that are psms can have bodies as one 
;;; or more of their arguments.  If a body appears at this level in
;;; the graph then it will be considered a necessary or useful body
;;; and I will add (the entry producing it) to the list.  If no
;;; corresponding entry is found then an error will occur.
;;;
;;; As an argument the collection function takes a list of the complete
;;; solution nodes including givens (for each solution) and extracts 
;;; the set from there.  The trick is to determine when the students
;;; have completed all of the necessary bodies for at least one 
;;; solution.  
(defun nsh-collect-body-entries ()
  "Collect list of per-solution body entries."
  (setq *nsh-bodysets* (remove-if #'null (nsh-get-solution-bodies))))

; Collect lists of unique body entries in each solution
; Returns: a list containing one set of body entries per solution,
; paralleling the list of solutions in the solution graph.
; NB: this subroutine also used by grading initialization code, which
; wants NULLs left in result list for solutions that have no bodies. 
; NSH prompts to draw bodies if *any* solution draws bodies, so removes 
; NULL sets above (desirable?)
; !!! No attempt to distinguish body entries that are optional in a solution,
; in the sense that they don't occur on all paths through the psm graph.
(defun nsh-get-solution-bodies ()
     (mapcar #'(lambda (S) 
		 (remove-if 
		  #'null
		  (remove-duplicates 
		   (mapcan #'nsh-collect-principle-bodyents S))))
	   (get-solution-enode-sets)))
		
; AW: for some reason *nsh-solution-sets* sometimes includes
; qnodes. Not sure if this indicates a bug setting it up.
; In any case, only want enodes passed to body-entry collecting tests.
(defun get-solution-enode-sets ()
  (mapcar #'(lambda (nodelist) (remove-if-not #'enode-p nodelist))
         *nsh-solution-sets*))

;;; ------------------- principle bodyents ----------------------------
;;; Collecting entries from the principle is simply a matter of 
;;; locating the body variables in the principles and then matching
;;; them with the body entries on each principle.  

;;; Given a principle collect the entries beneath it that correspond 
;;; to drawing bodies at the top-level.  These entries will consitute 
;;; the necessary body(ies) for the principle.  This takes the individual
;;; bodies and locates the corresponding entries. 
;;;
;;; NOTE::  This function is called by the code in Tests.cl so it is 
;;;   Exposed and needs to be considered as such for now.  When time 
;;;   permits this code will be duplicated or moved elsewhere but 
;;;   not at present.  

(defun nsh-collect-principle-bodyents (principle)
  (let ((Bodies (nsh-collect-principle-bodies principle)))
    (when Bodies
      (mapcar #'(lambda (B) 
		  (nsh-lookup-body-entry 
		   B (nsh-principle-entries Principle)))
	      Bodies))))



;;; --------------- principle bodies -----------------------------------
;;; Given a principle we can lookup its class and obtain a list of all
;;; "body" fields.  Because we can safely assume that the standard of
;;; ?body, ?body0 ... ?bodyN and ?bodies has been adhered to we know
;;; how to locate these as necessary.  Once this is done the system will
;;; lookup their bindings and return a list of the results (accounting for
;;; multiple bodies listed in ?bodies.
(defun nsh-collect-principle-bodies (principle)
  (let* (class Bindings)
    (setq Class (nsh-lookup-principle-class principle))
    (when Class
      (setq bindings (unify (nsh-principle-expression principle)
			    (psmclass-form Class)))
      (append (nsh-filter-single-body-vars Bindings)
	      (nsh-filter-compound-body-vars Bindings)
	      (nsh-filter-multiple-body-vars Bindings)))))
    


(defun nsh-filter-single-body-vars (Bindings)
  "Filter out the compound body variables."
  (mapcar #'cdr (remove-if-not #'nsh-single-bodyvar-p 
			       Bindings :key #'car)))


(defun nsh-filter-compound-body-vars (Bindings)
  "Filter compound body variables."
  (mapcar #'(lambda (V) 
	      (if (listp (car V)) 
		  (cons 'compound (car V))
		(cons 'compound V)))
	  (mapcar #'cdr (remove-if-not #'nsh-compound-bodyvar-p 
				       Bindings :key #'car))))


(defun nsh-filter-multiple-body-vars (Bindings)
  "Collect the multi-var arguments."
  (apply #'append 
	 (mapcar #'cdr (remove-if-not #'nsh-multi-bodyvar-p
				      Bindings :key #'car))))
	 



;;; given a variable return t if it is prefixed by "?body"
(defun nsh-single-bodyvar-p (Var)
  "Given a variable return t if it is prefixed by \"?body\""
  (let ((str (format Nil "~a" Var)))
  ;;  (pprint Str)
    (and (<= 5 (length Str)) (string-equal (subseq Str 0 5) "?body"))))


;;; Return t if the variable is prefixed with ?bodies
(defun nsh-multi-bodyvar-p (Var) 
  (let ((Str (format nil "~a" Var)))
    (and (<= 7 (length Str)) (string-equal (subseq Str 0 7) "?bodies"))))


;;; Return t if the variable is prefixed with ?compound
(defun nsh-compound-bodyvar-p (Var) 
  (let ((Str (format nil "~a" Var)))
    (and (<= 9 (length Str)) (string-equal (subseq Str 0 9) "?compound"))))



;;; ---------------- Lookup Body Entry ---------------------------
;;; Given a body name and an (optional) list of body entries lookup
;;; the body drawing entry that matches the supplied body name.
(defun nsh-lookup-body-entry (Body &optional (Entries *nsh-valid-entries*))
  (let ((form (subst-bindings
	       (list (make-binding '?body Body))
	       (entryprop-helpform 
		(lookup-entryprop-type 'Body)))))
;;    (pprint form)
    (find-if #'(lambda (E) (unify (systementry-prop E) Form))
	     Entries)))





    






















;;;; ============================ Main Code ====================================
;;;; The basic algorithm that we wish to instill in the students for solving 
;;;; problems is the following:
;;;; 1. Write down all the bodies necessary for a solution.  This is, in essence
;;;;    A single body for each block, or a compound body.  I.E. 

;;;;    One single body for each individual 
;;;; 1. Write down all of the relevant givens including defining whatever variables
;;;;    and/or vectors are necessary, and assignment statements for their values.
;;;; 2. Read the problem statement and identify the sought quantity as well as
;;;;    the first major principle that you will use to find it.  In many ways 
;;;;    this is the classifciation step of the problem.  Note this major principle
;;;;    need not contain the sought quantity directly, but you must be able to use
;;;;    it to find said quantity.  Knowing which principle will be appropriate is
;;;;    a big deal to the physicists and this is a primary skill that they want us
;;;;    to teach, as is knowing the difference between major and minor principles.
;;;; 3. Complete the rest of the major principles necessary to solve the problem.
;;;; 4. Complete all of the minor "linking" principles such as the weight law that
;;;;    are necessary to solve the problem.  
;;;; 5. Enter the answer in the box.
;;;;
;;;; Fundamentally we view step 2 as the deciding step.  Apart from this, the student
;;;; is given no obvious choice as to what entry they should make, nor are they able
;;;; to really change their road except by deleting entries.  Selecting the major
;;;; principle that they will begin with consititues choosing (to us) the solution 
;;;; that they will use.  Once they have selected this principle (and completed it)
;;;; we will want them to stay on that road until they are on and continue to prompt
;;;; from it until they delete that first entry.  
;;;;
;;;; When the student calls NSH they can be in one of the following states:
;;;; 1. They have done no work whatsoever
;;;; 2. They have been working on some (nsh-prompted) node but not completed it.
;;;; 3. They have completed some principle but not all of the givens.
;;;; 4. They have been working but have not completed all of the givens but not
;;;;    selected a major principle.
;;;; 5. They have selected (via NSH) a major principle and been working on the solutions.
;;;; 6. They have entered all of the nodes necessary for a solution.
;;;;
;;;; For each state we will do the following:
;;;; 1. We want to prompt them to start working on the givens and then 
;;;;    hint the first given.
;;;; 2. We want to prompt them to continue on with the node.
;;;; 3. We want to prompt them to work on the next given to complete it.
;;;; 4. We want to prompt them to pick a sought and major principle 
;;;;    (thus selecting a solution) and to start on it, or continue if
;;;;    they have already begun.
;;;; 5. We want them to continue working on the solutions that they have selected
;;;;    prompting the major principles if they have not yet done them or the 
;;;;    minor principles.  
;;;; 6. We want to prompt them to enter the value and be done with it.
;;;;
;;;; Obviously this has some potential problems.  Once the studsent has "selected" a
;;;; first principle (and with it a set of solutions) then they are constrained to 
;;;; continue with that set until they delete all of their work on the first principle
;;;; this may anger some of the students but, forcing them to go this route might also
;;;; help to dissuade them from changing their minds constantly and filling the screen
;;;; with useless cruft.   
;;;;
;;;; The other downside is, that their selection of sought, although it is propogated 
;;;; in the code does not actually influence their choice of first principle.  In single
;;;; sought problems this is fine because the only acceptable first-principles are those
;;;; that are closest to that sought anyway and therefore are appropriate.  In multi-sought
;;;; problems, however the system doesn't bother storing all permutations of soughts and 
;;;; the principles that go with them so it has no way of determinging which first principle
;;;; is the appropriate one for any sought but the first one in the problem spec.  For
;;;; now we just assume that the first one is okay but it might be better to do something
;;;; more specific later on, either storing the solutions, or running a solver process at
;;;; runtime to keep track of the activities.  
;;;;
;;;; All of the code below (up to ask sought) assumes that it may be called directly when
;;;; the student has done no work.  At that point it is best to tell the student to start
;;;; the problem with the chosen entry type.  If they have not done any of the work.  
;;;;
;;;; The **nsh-next-call** facility is a utility that allows next-step-help to behave 
;;;; differently based upon previous commands.  In essence this facility allows the
;;;; help system to specify the response that NSH will give the next time that the student
;;;; calls it.  If a function exists it will be called first before any node is continued
;;;; or any other work is done.  If not then NSH will behave as normally.
;;;;
;;;; This main algorithm is, of course complicated by the fact that we have essentially
;;;; three distinct problem types: 
;;;;   Quant:  The standard problems that contain quantity soughts.
;;;;   No-quant:  Problems where no quantity is sought and the student must complete
;;;;     a series of steps before selecting an "I am Done" button or some other val.
;;;;   MC-Answer: Multiple choice problems where the student must answer a set of 
;;;;     multiple choice questions alone.
;;;;
;;;; In reality these are, of curse arbitrary divisions and we would like to have NSH 
;;;; smoothly deal with a set of them allowing us to combine the different types of 
;;;; soughts.  For now each type gets treated differently by NSH and so each one 
;;;; has a distinct subsection below.  

(defun next-step-help ()
  "Call next-step help."
  (cond ((nsh-next-call-set?) (nsh-execute-next-call))
	((nsh-continue-last-node?) (nsh-prompt-last-node))
	((not (nsh-student-has-done-work?)) (nsh-prompt-start))
	(t (nsh-prompt-next))))



;;;; -------------------- Continuing Work ---------------------------------
;;;; If the student has started a node but not completed it then our first
;;;; priority is always to get them to continue doing so.  

(defun nsh-continue-last-node? ()
  "Return t iff *nsh-last-principle* exists and hasn't been completed."
  (and *nsh-last-Node* 
       (not (nsh-node-completed-p *nsh-last-Node*))))


(defun nsh-prompt-last-Node ()
  "Prompt for the student to continue the last principle."
  (nsh-prompt-Node  
   "Why don't you continue working on "
   *nsh-last-Node*
   :Assoc `(nsh prompt-last-node ,(nsh-node-expression *nsh-last-node*))))




;;;; -------------------- Prompt Start ----------------------------------
;;;; If the student has not done any work yet, then we want them to go 
;;;; ahead and begin working on the problem starting with an appropriate
;;;; initial location.  Depending upon the nature of this problem that
;;;; may be with a body, axis, given, or just the start of the stored
;;;; solution.  

;;; Student has done work
;;; A student is defined as having done work (presently) if they have 
;;; made no entries at all.  In future this may change (to ignore axes say)
;;; but for now null *studententries* is all.
(defun nsh-student-has-done-work? ()
  "Return t iff the student has done no work."
  *studententries*)


(defun nsh-prompt-start ()
  "Prompt the student to start with the appropriate problem feature."
  (cond ((nsh-prompt-bodies?) (nsh-start-bodies)) 
	((nsh-prompt-axis?) (nsh-new-start-axis))
	((nsh-prompt-givens?) (nsh-new-start-givens))
	((equal *nsh-problem-type* 'no-quant) (nsh-start-no-quant))
	((equal *nsh-problem-type* 'mc-only) (nsh-mc-only-start))
	((nsh-start-principle-free?) (nsh-start-principle-free))
	;; At this point we know that an error has occured so we
	;; need to signal that to the system for later use.
	(t (error "Invalid problem setup supplied."))))


;;;; -----------------------------------------------------------------------
;;;; Prompting the rest.
;;;; Once the student has been working and has completed the last principle
;;;; then we need to hint them appropriately for the next entry.  The code 
;;;; here will select the next thing for the student to work on and prompt
;;;; them to continue with that.

(defun nsh-prompt-next ()
  (cond	
   ((equal *nsh-problem-type* 'mc-only) 
            (if (nsh-mc-only-done?) (nsh-mc-only-prompt-done)
               (nsh-mc-only-prompt)))
   ((nsh-prompt-bodies?) (nsh-prompt-bodies))
   ((nsh-prompt-axis?) (nsh-prompt-axis))
   ((nsh-prompt-givens?) (nsh-prompt-givens))
   ((nsh-start-no-quant?) (nsh-start-no-quant))
   ((nsh-no-quant-done?) (nsh-prompt-no-quant-done))
   ((nsh-done?)  (nsh-prompt-done))
   ((nsh-ask-sought?) (nsh-ask-sought-and-fp))
   ((nsh-start-principle-free?) (nsh-start-principle-free))
   (t (nsh-continue-solutions))))



;;;; ====================== Prompt Bodies ========================================
;;;; We want the students to begin all problems by defining all of the "necessary"
;;;; bodies in for a solution.  If they ask NSH will prompt them to make all of 
;;;; the necessary body entries for the ideal solution.  Bodies are considered
;;;; necessary if they appear in the top level of a PSMclass.  
;;;;
;;;; Note that there are two issues at work here.  The first is that we are asking
;;;; the students to draw bodies before we really have them get started on a 
;;;; solution.  The second is that Body drawing is still a synonim for defining
;;;; a mass variable so it is possible that, later on in the hinting process, the
;;;; students will be prompted to draw a body that we have implicitly labelled as
;;;; "unecessary" by not including it here and that might bother them.  
;;;;
;;;; As an exampke consider exs3a (book on package on table).  For the ideal 
;;;; solution the student needs only 1 body, the compound Book/Package body.  
;;;; However, they will need the mass variables in that body later on.  
;;;; Therefore they might be prompted to draw a body that they got away with ignoring 
;;;; before this is the downside to our legacy of treating bodies and mass variables
;;;; the same.  As such we may find it necessary to eliminate those prompts later on.
;;;;
;;;; A final issue to consider here is that bodies are tied to solutions.  Implicitly,
;;;; by declaring a list of bodies the student is also declaring tat they will work on
;;;; a solution containing those bodies.  Therefore, in exs3a by drawing only the 
;;;; compound body I am also implicitly declaring that I will work on the ideal solution
;;;; which makes no use of the other bodies.  
;;;;
;;;; I could, however choose to work on a solution that contains other bodies (and will
;;;; be subsequentry prompted to draw them).  As a result they may "declare" one 
;;;; solution implicitly via the bodies and then do another.  This may (pedagogically)
;;;; be a bit of a problem.  
;;;;
;;;; (put these in issues to consider).

;;; We want to prompt body drawing if *nsh-bodysets* is t
(defun nsh-prompt-bodies? ()
  (and *nsh-bodysets*
       ; NOT Exists a per-solution bodyset S s.t. all body entries in S entered
       (not (member-if #'(lambda (S) 
			   (null (remove-if #'systementry-entered S)))
		       *nsh-bodysets*))))



;;; When we want the student to start the problem by drawing a body
;;; (this will be the case on any problem for which there are bodies)
;;; we will give them a general hint about it being a good idea to 
;;; do so and then immediately tell them to draw the body.  
(defun nsh-start-bodies ()
  (let ((Body (nsh-pick-body-to-hint)))
    (nsh-bottom-hint-target-entry
     (Strcat "It is a good idea to begin any problem by enumerating "
	     "the bodies at work within it.  Doing so helps to frame "
	     "later principle applications.  Why don't you do so now.")
     Body
     :Assoc `(nsh start-bodies ,(nsh-entry-prop Body)))))



;;; If the student has already begun work and asks for help then we 
;;; will want to prompt them to continue (or start) entering the 
;;; necessary bodies.  
(defun nsh-prompt-bodies ()
  (if (nsh-bodyentry-made?)
      (nsh-continue-bodies)
    (nsh-start-bodies)))


;;; Return t if any one of the bodyentries in any of the sets
;;; has been made.
(defun nsh-bodyentry-made? ()
  (member-if #'(lambda (E) (member-if #'systementry-entered E))
	     *nsh-bodysets*))

;;; If the student is continuing bodies then we want to remind them 
;;; of what they should be doing and then to prompt them to contine
;;; doing so.  
(defun nsh-continue-bodies ()
  (let ((Body (nsh-pick-body-to-hint)))
    (nsh-bottom-hint-target-entry
     (strcat "You should continue enumerating all of "
	     "the necessary bodies for this problem.  ")
     Body
     :Assoc `(nsh continue-bodies ,(nsh-entry-prop Body)))))


;;; As with solutions, we want to favor the ideal activity possible
;;; for the student.  In this case, if they have not started any 
;;; bodies then we will prompt them to draw bodies contained in
;;; the ideal solution.  If not then we prompt the first solution
;;; in the list that they have started.
(defun nsh-pick-body-to-hint ()
  "Pick the body to hint."
  (or (find-if-not 
       #'systementry-entered
       (find-if #'(lambda (S) 
		    (member-if #'systementry-entered S))
		*nsh-bodysets*))
      (caar *nsh-bodysets*)))


;;;; =================== Prompt Axis ===========================================
;;;; On most problems we want the students to begin by drawing an axis to ground 
;;;; themselves.  The code in this section tests to see if they have defined an 
;;;; axis already.  If not then it prompts them to draw one and move on.  If 
;;;; they have not done any work then it will tell them to start the problem 
;;;; with the axis.  If they have done work then we will mention that it's best 
;;;; to have an axis and prompt them to produce one.  
;;;;
;;;; One issue with this is that we may confuse the students by telling them to 
;;;; draw standard axes and then tell them that they will be better off with other
;;;; axes.  Won't this bother them?

;;; If the code contains an axis entry then we want the student to 
;;; make it and so we will prompt them to do so.
(defun nsh-prompt-axis? ()
  (and *nsh-axis-entries*
       (not (member-if #'systementry-entered *nsh-axis-entries*))))


;;; Once we have established that there is an axis, then we want 
;;; to prompt the student to enter one of them with the following
;;; hint.
(defun nsh-prompt-axis ()
  (let ((Rot (nsh-get-axis-Rot)))
    (make-dialog-turn
     (strcat "It is now a good idea for you to draw an axis.  This "
	     "will help to ground your work and be useful later on "
	     "in the process.")
     **Explain-More**
     :Responder #'(lambda (Response)
		    (when (equal Response **Explain-More**)
		      (nsh-make-axis-prompt Rot)))
     :Assoc `(nsh prompt-axis ,Rot))))

		      
    
			     
;;; On problems where the axis is the first entry that the student 
;;; should be working on, we want to tell them that an appropriate
;;; way to start this (and most) problems is to draw such a thing.
(defun nsh-new-start-axis ()
  (let ((Rot (nsh-get-axis-Rot)))
    (make-dialog-turn
     (strcat "It is a good idea to begin most problems by drawing "
	     "an axis.  This helps to ground your work and will be "
	     "useful later on in the process.")
     **Explain-More**
     :Responder #'(lambda (Response)
		    (when (equal Response **Explain-More**)
		      (nsh-make-axis-prompt Rot)))
     :Assoc `(nsh new-start-axis ,Rot))))


;;; Once we begin prompting the student to work on axes we 
;;; need to give them a standard set of prompts.  That is done
;;; here.
(defun nsh-make-axis-prompt (Rot)
  "Make the axes prompts."
  (declare (special **nsh-standard-axis-prompt** **nsh-rotated-axis-prompt**
		    **nsh-rotated-axis-bottom-prompt**))
  (make-hint-seq 
   (if (= Rot 0) (list **nsh-standard-axis-prompt**)
     (list (format Nil **nsh-rotated-axis-prompt** (nsh-get-axis-rot))
	   (format Nil **nsh-rotated-axis-bottom-prompt** (nsh-get-axis-rot))))
   :Assoc `(nsh make-axis-prompt ,Rot)))



(defun nsh-get-axis-rot ()
  (if (member 0 *nsh-axis-entries* :key #'(lambda (E) (cadr (systementry-prop E))))
      0
    (cadr (systementry-prop (car *nsh-axis-entries*)))))
    


(defparameter **nsh-standard-axis-prompt**
    "Why don't you draw a pair of standard axes setting the positive X axis at 0 degrees.")

(defparameter **nsh-rotated-axis-prompt**
    (strcat "On this problem you should draw a pair of axes with the "
	    "positive X axis set to ~a degrees.  This will line them up "
	    "with the vectors that you will be drawing and make your "
	    "solution simpler."))

(defparameter **nsh-rotated-axis-bottom-prompt**
    "Draw a pair of axes setting the posivie X axis at ~a  degrees.")



;;;; ==================== Prompting Givens ===================================
;;;; On problems with givens we want the students to enumerate all of them 
;;;; before moving on to the major principles and the other non-major PSMS
;;;; nsh-new-start-givens prompts the student to begin the problem by 
;;;; working on the givens.  nsh-prompt-givens will determine if the students
;;;; have been working on the givens or if this is just the beginning.  If 
;;;; they have been then they will be prompted to continue.  If not, then 
;;;; they will be prompted to start.  

;;; We want to prompt the givens on a problem if *nsh-givens* exist,
;;; and the student has not completed them.  
(defun nsh-prompt-givens? ()
  (and *nsh-givens* (remove-if #'nsh-principle-completed-p *nsh-givens*)))

;;; Test to determine if the students have started the givens.
;;; If not then prompt them to start them else, prompt them to 
;;; continue with them.
(defun nsh-prompt-givens ()
  (if (member-if #'nsh-principle-started-p *nsh-givens*)
      (nsh-prompt-continue-givens)
    (nsh-prompt-start-givens)))


(defun nsh-new-start-givens ()
  "Prompt the student to begin the nsh with the first given."
  (nsh-prompt-principle
   (strcat "It is a good idea to begin any problem by enumerating "
	   "the given quantities.  Why don't you start by doing "
	   "that now.  Why don't you work on ")
   (car *nsh-givens*)
   :Assoc `(nsh new-start-givens ,(nsh-principle-expression (car *nsh-givens*)))))


(defun nsh-prompt-start-givens ()
  "Prompt for the student to start working on the givens."
  (nsh-dialog-prompt-Node
   (strcat "At this point it would be a good idea to begin enumerating "
	   "all of the useful given quantities in the problem"  )
   "why don't you start with "
   (car *nsh-givens*)
   :Assoc `(nsh prompt-start-givens ,(nsh-node-expression (car *nsh-givens*)))))


(defun nsh-prompt-continue-givens ()
  "Prompt the students to continue working on the given principles."
  (let ((given (find-if-not #'nsh-principle-completed-p *nsh-givens*)))
    (nsh-prompt-principle
     (strcat "You should finish entering all of the useful given "
	     "quantities in the problem.  Why don't you work on ")
     Given
     :Assoc `(nsh prompt-continue-givens ,(nsh-principle-expression Given)))))




;;;; ==================== Non-quant problems ================================
;;;; No quant problems come in two forms.  In the first case the students are 
;;;; dealing with a problem that has a single "I am done" button and one 
;;;; non-quantity PSM that they must complete.  In the second case the 
;;;; students are being given a multiple-choice problem in which they 
;;;; must go ahead an answer all of the multiple choice questions in 
;;;; the problem.  In the latter case the only guidance that we can give
;;;; them is to consider the questions themselves.  
;;;;
;;;; For the "I am done" problems we need to prompt the students to work 
;;;; for non-quant problems we need to prompt the student's to work their 
;;;; way through the problem without picking a sought quantity.  This code
;;;; does that by informing them that they need to simply complete all of
;;;; the desired principles and then go ahead and check the done boxes.

;;; -------------------------------------------------------------------------
;;; Return t to prompt on a no-quant problem if this problem is of the 
;;; no-quant type and they have not been working on the stored solution
;;; yet.  In essence this tells us whether or not we need to inform them
;;; that this is a no-quant problem and what to do or not.
(defun nsh-start-no-quant? ()
  "Do we prompt this as a no-quant problem?"
  (and (equal *nsh-problem-type* 'no-quant)
       (not (member-if #'nsh-principle-started-p (car *nsh-solution-sets*)))))




;;; when this is a no-quant problem and we want the students to start
;;; working on (the core of) it, then we tell them so and indicate 
;;; what it is that they have to do.  
(defun nsh-start-no-quant ()
  "Propt the student to start working on the no-quant problem."
  (let ((principle (caar *nsh-solution-sets*)))
    (make-dialog-turn
     (strcat "On problems such as this you need to complete "
	     "all of the individual goals listed in the "
	     "problem description.  ")
     **Explain-More**
     :Responder #'(lambda (Response)
		    (when (equal Response **Explain-more**)
		      (nsh-prompt-Node 
		       "Why don't you start with "
		       Principle
		       :Assoc `(nsh start-no-quant-explain-more
				    ,(nsh-node-expression Principle))))) 
     :Assoc `(nsh start-no-quant))))



;;; -------------------------------------------------------------------------
;;; If this is a no-quant problem and the student is done with it then
;;; we need to give them a specialized done hint.  
(defun nsh-no-quant-done? ()
  (and (equal *nsh-problem-type* 'no-quant)
       (null (remove-if #'nsh-principle-completed-p 
			(car *nsh-solution-sets*)))))

;;; Inform the students that they are done, and prompt them to go 
;;; ahead and check off the boxes (if they have not already done so
;; and tell them to quit.
(defun nsh-prompt-no-quant-done ()
  "Prompt that the student is done with the problem."
  (make-end-dialog-turn
   (strcat "You have completed all of the principles necessary "
	   "in this problem.  " (nsh-get-no-quant-done-str))
   :Assoc '(nsh prompt-no-quant-done)))

(defun nsh-get-no-quant-done-str ()
  (if (< 1 (length (problem-soughts *cp*)))
      "Please check the \"I am done\" checkboxes on the screen."
    "Please check the \"I am done\" checkbox and quit."))

  


;;;; ====================== Multiple-choice-Problems =============================
;;;; For the Multiple-choice only problems the only tasks that they students need
;;;; to complete are the multiple-choice problems themselves.  Therefore the only
;;;; help that NSH will give is to alert the students to this fact and to prompt 
;;;; them to coninue their work on the problem.  

;;; -------------------------------- Done ----------------------------------------
;;; On a multiple-choice only problem the student is done working on the problem
;;; when they have answered all of the questions.  This is defined by them having
;;; defined one entry for each sought.  I am not going to require that the entries
;;; be correct only that they be made.  
(defun nsh-mc-only-done? ()
  (when *Studententries*
    (nsh-mc-only-done-rec (problem-soughts *cp*))))

(defun nsh-mc-only-done-rec (Soughts)
  (when (nsh-mc-only-match-SE (car Soughts))
    (if (= 1 (length Soughts)) t
      (nsh-mc-only-done-rec (cdr Soughts)))))


(defun nsh-mc-only-match-SE (Sought)
  "Attempt to locate a matching entry for the sought."
  (find (nth 1 Sought) *Studententries*
	:key #'(lambda (E) (nth 1 (Studententry-prop E)))
	:Test #'equalp))

;;; Return t if all of the multiple-choice entries have been made and 
;;; are correct.  
(defun nsh-mc-only-done-correct-p ()
  (let ((Soughts (remove-if-not #'(lambda (V) (equalp 'CHOOSE-ANSWER V)) 
				(problem-soughts *cp*) :key #'car)))
    (nsh-mc-only-done-correct-p-rec Soughts)))

(defun nsh-mc-only-done-correct-p-rec (Soughts)
  "Recursively deal with the correctness."
  (let ((Ent (nsh-mc-only-match-SE (car Soughts))))
    (when (and Ent (equalp (studententry-state Ent) 'correct))
      (if (= 1 (length Soughts)) t
	(nsh-mc-only-done-correct-p-rec (cdr Soughts))))))


;;; When the student is done then we need to inform them that they have 
;;; answered all the questions.  If they have not answered all of the 
;;; equations successfully then we can offer them the chance to change 
;;; their answers if they wish.
(defun nsh-mc-only-prompt-done ()
  "Inform the students that they are done."
  (if (nsh-mc-only-done-correct-p)
      (nsh-mc-only-prompt-done-correct)
    (nsh-mc-only-prompt-done-incorrect)))


(defun nsh-mc-only-prompt-done-correct ()
  (make-end-dialog-turn
   (strcat "You have correctly completed all of the multiple-choice "
	   "questions on this problem.  There is no more that you "
	   "need to do.  You can now move on to the next problem.")
   :Assoc '(nsh mc-only prompt-done-correct)))


(defun nsh-mc-only-prompt-done-incorrect ()
  (make-dialog-turn 
   (strcat "You have answered all of the questions on this problem "
	   "but some of your answers are incorrect.  If you want to "
	   "you can stop here and move on to another problem.  "
	   "However you can also change your answers if you wish to "
	   "try again.")
   **Explain-More**
   :Responder #'(lambda (Response)
		  (when (equalp Response **Explain-More**)
		    (nsh-mc-only-prompt-done-reconsider)))
   :Assoc '(nsh mc-only prompt-done-incorrect)))

;;; If the students are done but incorrect we can offer them the chance
;;; to reconsider some of their incorrect answers by listing them here.
(defun nsh-mc-only-prompt-done-reconsider ()
  (make-end-dialog-turn
   (format Nil "Why don't you reconsider your ~a."
	   (nlg (studententry-prop 
		 (nsh-mc-only-select-first-incorrect)) 
		'nlg-entryprop))
   :Assoc '(nsh mc-only prompt-done-reconsider)))



;;; ---------------------------- Start -----------------------------------------
;;; When starting an mc-only problem we will give the initial preamble and them 
;;; inform the students that they should start work on this task. 
(defun nsh-mc-only-start ()
  (make-dialog-turn 
   (strcat "On problems of this type you need to answer "
	   (if (> 1 (length (problem-soughts *cp*)))
	       "all of the multiple choice questions on your screen."
	     "the multiple choice question on your screen.")
	   "You do not need to make any other entries.")
   **Explain-More**
   :Responder #'(lambda (Response)
		  (when (equal Response **Explain-More**)
		    (nsh-mc-only-prompt-next)))
   :Assoc '(nsh mc-only start)))


;;; -------------------------------- Next ---------------------------------------
;;; When the student has already been working on an mc-only problem we want to
;;; give them the same message that they were given originally but, if they have
;;; made any incorrect entries we want to offer them the chance to change those 
;;; entries.
(defun nsh-mc-only-prompt ()
  (make-dialog-turn 
   (strcat "On problems of this type you need to answer "
	   (if (> 1 (length (problem-soughts *cp*)))
	       "all of the multiple choice questions on your screen."
	     "the multiple choice question on your screen.")
	   "  You do not need to make any other entries.")
   **Explain-More**
   :Responder #'(lambda (Response)
		  (when (equal Response **Explain-More**)
		    (if (nsh-incorrect-mc-entries-made-p)
			(nsh-mc-only-prompt-reconsider)
		      (nsh-mc-only-prompt-next))))
   :Assoc '(nsh mc-only prompt)))


;;; If the student has made any incorrect entries then we want to locate them
;;; and then prompt them to reconsider the earliest one.  This will be done 
;;; by selecting the subset of them and then sorting them by the mc value.

;;; Collect the incorrect mc-only answers that the student has made (if any).
(defun nsh-collect-mc-only-incorrect-attempts ()
  (remove-if-not 
   #'(lambda (E) 
       (and (unify (studententry-prop E) '(CHOOSE-ANSWER ?A ?B))
	    (not (equalp (studententry-state E) 'correct))))
   *Studententries*))


(defun nsh-mc-only-select-first-incorrect ()
  "Collect the first incorrect answer given."
  (car (sort (nsh-collect-mc-only-incorrect-attempts) 
	     #'String<
	     :key #'(lambda (E) 
		      (format 
		       Nil "~a" 
		       (nth 1 (studententry-prop E)))))))
  
(defun nsh-incorrect-mc-entries-made-p ()
  (when (and *Studententries* 
	     (nsh-collect-mc-only-incorrect-attempts))
    t))


(defun nsh-mc-only-prompt-reconsider ()
  (let ((incorrect (nsh-collect-mc-only-incorrect-attempts)))
    (make-dialog-turn 
     (strcat (if (> 1 (length incorrect))
		 "You have made more than one incorrect answer attempts.  "
	       "You have made an incorrect answer attempt.  ")
	     "Do you want to reconsider it or work on the next question")
     '("Reconsider" "Next")
     :Responder #'(lambda (Response)
		    (if (string-equal Response "Reconsider")
			(nsh-mc-only-prompt-do-reconsider Incorrect)
		      (nsh-mc-only-prompt-next)))
     :Assoc '(nsh mc-only prompt-reconsider))))


;;; If the student elects to reconsider one or more of their incorrect
;;; entries.  Then we will select the first incorrect answer attempt 
;;; and will suggest that they to reconsider it.
(defun nsh-mc-only-prompt-do-reconsider (Incorrect)
  "Prompt the student to reconsider the specific value."
  (let ((Entry (car (sort Incorrect #'String<
			  :key #'(lambda (E) 
				   (format 
				    Nil "~a" 
				    (nth 1 (studententry-prop E))))))))
    (make-end-dialog-turn
     (format Nil "Why don't you reconsider your ~a."
	     (nlg (studententry-prop Entry) 'nlg-entryprop))
     :Assoc `(nsh mc-only prompt-do-reconsider ,(studententry-prop Entry)))))


;;; If the student is starting the problem, has made no incorrect entries
;;; or has not elected to work on them then we want to prompt them to make
;;; the next necessary entry.  
(defun nsh-mc-only-prompt-next ()
  (let ((next (nsh-mc-only-pick-next-undone-sought))) 
    (make-end-dialog-turn
     (format Nil "Why don't you work on ~a" 
	     (nlg Next 'nlg-entryprop))
     :Assoc `(nsh mc-only prompt-next ,Next))))


(defun nsh-mc-only-pick-next-undone-sought ()
  "Select the next mc-answer that the student should work on."
  (car (sort (remove-if #'nsh-mc-only-match-SE (problem-soughts *cp*))
	     #'string<
	     :key #'(lambda (S) (format Nil "~a" (nth 1 S))))))




(defun nsh-trace-mc-only ()
  (trace nsh-mc-only-done?
	 nsh-mc-only-done-rec
	 nsh-mc-only-match-SE
	 nsh-mc-only-prompt-done
	 nsh-mc-only-prompt-done-correct
	 nsh-mc-only-prompt-done-incorrect
	 nsh-mc-only-prompt-done-reconsider
	 nsh-mc-only-start
	 nsh-mc-only-prompt
	 nsh-collect-mc-only-incorrect-attempts
	 nsh-incorrect-mc-entries-made-p
	 nsh-mc-only-prompt-reconsider
	 nsh-mc-only-prompt-do-reconsider 
	 nsh-mc-only-prompt-next
	 nsh-mc-only-pick-next-undone-sought))
	 



;;;; ================== prompting the sought/first principle =====================
;;;; Once the student has completed the givens we want to prompt them to 
;;;; identify whant quantities the problem is seeking and then what major 
;;;; principle the student will use to find (it|them).  This takes the student 
;;;; through a series of dialogues designed to make them select the soughts
;;;; and the first (if any) major principle that they will use.  


;;; ----------------- ask sought ---------------------------------------------
;;; We want to prompt the sought if:
;;;  This problem has major principles (not in the case of potential dummy problems.)
;;;  And, The student has not specified any current solutions (done by selecting a 
;;;       major principle via NSH, 
;;;       OR, They have not completed the first principle of any of these solutions
;;;  If none of this is the case then we want them to move on.
;;;
;;; We already know (when the test comes around) that all of the givens are 
;;; done so the issue is to determine if they have begun any of the first
;;; principles.  If they have started (but not finished any) then we will
;;; move on from there.  
;;;
;;; Either there are no *nsh-current-solutions* or the student has not
;;; started the first principle of (it|them) in this case, they will have
;;; picked them at some time and then deleted the values.  In either case
;;; we want them to pick a sought/major priunciple.
(defun nsh-ask-sought? ()
  "Should NSH prompt the sought(s) to the student?"
  (and (member-if #'nsh-major-principle-p *nsh-nodes*)
       (or (null *nsh-current-solutions*)
	   (null (find-if #'nsh-principle-started-p
			  *nsh-current-solutions*
			  :key #'car)))))



;;; Prompting for the sought is a matter of displaying the old what quant?
;;; dialog from the previous form of NSH.  Once the student gets through that,
;;; then we move on to selecting the first major principle that they will use
;;; for now the system gives fairly minimal feedback on their choices.
;;; Later on it should be updated to:
;;; 1. Notice when they are choosing something that is part of a solution
;;;    but not the front.
;;; 2. Notice when they are choosing something forbidden
;;; 3. slap them silly when they keep making the same mistake.

(defparameter **NSH-Sought-Str**
    (strcat "Most problems ask you to find the value of one or more "
	    "quantities.  To solve such problems it is helpful to "
	    "first identify precisely a sought quantity because this "
	    "may remind you of a principle that can be used to find it."))


(defun nsh-ask-sought-and-fp (&optional (prefix ""))
  "Start the NSH sought and FP loop."
  (nsh-ask-sought 
   (strcat Prefix 
	   (nsh-asfp-given-str) 
	   (nsh-asfp-sought-str))))



;;; If the problem has givens in it then we want to mention it as a 
;;; refrent, else we want the students to start off with no givens
;;; being discussed.
(defun nsh-asfp-given-str ()
  (if *nsh-givens*
      (strcat "Now that you have stated all of the given information, you "
	      "should start on the major principles.  ")
    "You should now start on the major principles.  "))


(defun nsh-asfp-sought-str ()
  "Get the prompt string for nsh-ask-sought."
  (if (multi-sought-problem-p *cp*)
      "What is one quantity that the problem is seeking?  "
    "What quantity is the problem seeking?  "))


;;; This is the loopback point that will be used for the dialogues at runtime 
;;; The case refers to case information that will be used to define errors 
;;; in NSH selection.  If case is 'init then the system is just starting the
;;; call.  If not it will be some atom that defines the error that brought 
;;; the call back to this point.
(defun nsh-ask-sought (&optional (prefix "") (past nil) (Case 'init))
  "prompt the student to select the primary principle."
  (make-dialog-turn
   prefix
   **Quant-Menu**
   :responder #'(lambda (resp)
		  (nsh-check-sought-resp resp past))
   :Assoc `(nsh ask-sought ,Case)))


;;; Once the student has selected their sought we jump to checking it
;;; this code determines if this is in-fact a sought in the problem 
;;; and if not prompts the student to select again.  Note that they
;;; are prevented from selecting the same sought more than once.
;;;
;;; If they select a quantity that is sought then they will be 
;;; told that they are correct and sent on to select the major principle
;;; If they fail to select a correct sought, then if it is present in
;;; the graph then they will get some specialized prompting or else they
;;; will get a generaic incorrect response and move on.

(defun nsh-check-sought-resp (response past)
  "Check the sought response."
  (let ((Q (nsh-convert-response->quantity response)))                           ;; uses *current graph*
    (cond ((member response past :test #'equal) (nsh-sought-resp-rep past))      ;; have they tried this before.
	  ((null Q) (nsh-sought-resp-nil (cons response past)))                  ;; is the quantity valid?
	  ((not (nsh-quantity-soughtp Q)) (nsh-sought-resp-ns (cons response past))) ;; Is the quantity sought?
	  (t (nsh-ask-first-principle (random-positive-feedback) Q)))))          ;; Else use the value and move on.


(defun nsh-convert-response->quantity (Q)
  "Convert the kb-style quantity expression response to a quantity (qnode.)"
  (cond ((not (quantity-expression-p Q))
	 (error "Non-quantity-expression returned as response."))
	(t (match-exp->qnode Q (problem-graph *cp*)))))



;;; If the student has tried this value before and been told 
;;; that it was incorrect then we want to merely remind them
;;; of that fact and move on but not let them bottom out.
;;;
;;; possibly we could be heiarchical here and tell them that
;;; a mass is always wrong so  no mass is good, etc.
(defun nsh-sought-resp-rep (Past)
  "Return a message signifying that you tried this already."
  (nsh-wrong-sought-resp 
   (strcat "In order to move ahead, you have to try selecting "
	   "something that you haven't tried before.")
   Past :Case 'Repeat))


;;; If they select a value that is not in the graph, then we want 
;;; to tell them so and move on emphasizing that it will never 
;;; arise in a solution to this problem.
(defun nsh-sought-resp-nil (Past)
  "Return a message signifying wrong sought supplied."
  (nsh-wrong-sought-resp 
   "That quantity is not mentioned in the Andes solution."
   Past :Case 'Null))


;;; If they select a value that is in the graph but not sought 
;;; then, again, we giuve them that information and move them 
;;; on with appropriate information.  
(defun nsh-sought-resp-ns (Past)
  "Return a message signifying that quantity is not sought."
  (nsh-wrong-sought-resp 
   "That quantity is not sought by the problem statement."
   Past :Case 'not-sought))



;;; ------------------ wrong-sought ---------------------------------
;;; When the student picks the wrong sought then we need to give them
;;; a message alerting them to that fact and prompting them to try 
;;; again.  If they fail **max-sought-tries** unique times then we
;;; will simply tell them which quantity to use. 
(defun nsh-wrong-sought-resp (msg past &key (Case 'default-wrong-sought))
  (if (< (length past) **Max-sought-Tries**)
      (nsh-ask-sought msg past Case)
    (nsh-tell-sought msg Case)))


;;; Telling the student the sought is a matter of picking the first 
;;; sought quantity in the list and telling them to use that.  It 
;;; doesn't really matter (for the time being) which one that we 
;;; choose but one of then should be used. (in future this may
;;; tell all of the soughts.  Else they'll never see the others.
(defun nsh-tell-sought (message Case)
  "Tell the student what the problem is seeking and move on."
  (let ((Sought (car (problem-soughts *cp*))))
    (make-dialog-turn
     (strcat message "Let's just assume that you are seeking "
	     (nlg Sought 'def-np) ".  ")
     **OK-Menu**
     :responder #'(lambda (response)
		    (declare (ignore response))
		    (nsh-ask-first-principle 
		     "" (match-exp->qnode Sought (problem-graph *cp*))))
     :Assoc `(nsh tell-sought ,Case ,Sought))))


;;; Sought tracing.
(defun trace-nsh-ask-sought ()
  (trace nsh-ask-sought? nsh-ask-sought-and-fp
	 nsh-asfp-given-str nsh-asfp-sought-str
	 nsh-ask-sought 

	 nsh-check-sought-resp
	 nsh-convert-response->quantity
	 nsh-sought-resp-rep
	 nsh-sought-resp-nil
	 nsh-sought-resp-ns
	 
	 nsh-wrong-sought-resp
	 nsh-tell-sought))
	 
	 
;;; --------------- ask first principle -------------------------------------
;;; prompting the first principle is a matter of selecting the appropriate
;;; first principle from the problem solutions.  If we reach this point, we 
;;; know that the student has already done all of the givens but not entered
;;; any of the first principles so all that we have to do is test their choices
;;; against the first principles in the lists.  Whichever one that they choose
;;; is valid iff it is one of those principles.  If not then if it is in thr 
;;; graph we can discuss that else if it is not major, again disucss, else 
;;; accept.  As above we keep track of the responses to prevent overlap.

(defparameter **NSH-AFP-String**
    "What is the first principle application that you would like to work on?")

(defparameter **NSH-AFP-Single-String**
    (strcat "Hint: this principle application will usually be one that mentions "
	    "the sought quantity explicity.  Therefore its equation may contain "
	    "the sought quantity that the problem seeks."))

(defparameter **NSH-AFP-Multi-String**
    (strcat "Hint: usually this principle application will usually mention one "
	    "of the sought quantities explicity.  That is, its equation may "
	    "contain one of the sought quantities that the problem seeks."))

(defparameter  **NSH-AFPC-DIRECT-String**
    (strcat "What is the first principle application that you wish to work on?"
	    "  You should look for a principle that contains the sought "
	    "quantity itself."))

(defparameter  **NSH-AFPC-INDIRECT-String**
    (strcat "What is the first principle application that you wish to work on?"
	    "  You should look for a principle that is related to the sought "
	    "quantity indirectly."))
      
;;; When we begin asking the student for their first principle we find it
;;; useful to discuss the principle behind the entry I.E. explaining to 
;;; them the principle behind the first principle.  
(defun nsh-ask-first-principle (prefix Sought &optional (past nil))
  "Prompt the student for the first principle in the list."
  (make-dialog-turn
   (strcat prefix **NSH-AFP-String**
	   "  " (if (multi-sought-problem-p *cp*)
		    **NSH-AFP-Multi-String**
		  **NSH-AFP-Single-String**))
   **PSM-MENU**
   :responder #'(lambda (response)
		  (nsh-check-first-principle-response
		   Response sought past))
   :Assoc `(nsh ask-first-principle ,(nsh-node-expression Sought))))

   

;;; After the student has been prompted for the first principle once it is probably
;;; not necessary to bore them with the same long dialogue every time, a single 
;;; sentance will do.  
;;;
;;; When we are informing thew student that their choice was not successful we also
;;; want to provide some guidance on how best to make their next choice For that 
;;; reason this code will determine what the first principles are and then provide
;;; a context-sensitive use message either informing the students that they should 
;;; look for a principle that is directly connected to the sought (if they all are)
;;; or for one that is "related" to the sought quantity if they are not.  
;;;
;;; In order to provide this hint the code must know what the first principles are.
;;; To this end it will call the nsh-collect-first-principles function which should
;;; have already cached the principles associated with this sought.  It will then 
;;; test to see if all the principles are directly connected to the sought and, that
;;; being done deliver the appropriate message.  

(defun nsh-ask-first-principle-cont (prefix Sought &optional (past nil) (Case Nil)) 
  "Prompt the student for the first principle in the list."
  (make-dialog-turn
   (nsh-afp-cont-format-msg Sought Prefix)
   **PSM-MENU**
   :responder #'(lambda (response)
		  (nsh-check-first-principle-response
		   response sought past))
   :Assoc `(nsh ask-first-principle-cont ,Case ,(nsh-node-expression Sought))))


;;; When the student responds incorrectly to the first-principle selection
;;; command then we want to inform them of their mistake and prompt them 
;;; to continute.  The Prefix will encode a selection message.  In addition
;;; to that message and the hint we want to aid them in making their selection
;;; by informing them that they should look for a quant that does or does not
;;; mention the sought explicitly by including the fact that it is or is not
;;; present in the list.
;;;
;;; At present this gives them the direct message if at least one of the first
;;; principles is directly connected to this sought.  Else it gives the indirect
;;; message.  In the future we may prefer erring on the other side.  
(defun nsh-afp-cont-format-msg (Sought Prefix)
  (strcat prefix "  " 
	  (if (nsh-atleast1-first-principle-directly-connected-p Sought)
	      **NSH-AFPC-DIRECT-String**
	    **NSH-AFPc-INDIRECT-STRing**)))



;;; --------------- Check First Principle -------------------------------------
;;; Once the student has made their selection then we need to test it.  This
;;; code assumes that the student's response will be of the form: 
;;; (<Name> . <Bindings>)  
;;; Where <Name> is the principle name being selected, and <Bindings> is a 
;;; list of variable bingings og the form ((?Body . Car) ...)  These bindings
;;; will be subsititued in to filter the principles as we test them for
;;; correctness.  
;;;
;;; If the student has repeated a selection that we have already informed them
;;; is incorrect then we will alert them to that and prompt them to select
;;; again.  This error does not bottom them out.
;;;
;;; If the student has chosen a forbidden principle then we will tell them that
;;; they have done so and prompt them to pick again or bottom out if they have \
;;; reached the limit.  Note we also doublecheck for forbiddenness below as well
;;; after we have obtaind the psmclass.  This test is left here to save the work
;;; that would otherwize be done if we waited until the PSMClass was looked up.
;;;
;;; If they have picked a principle type that we have a record of (this is to 
;;; deal with points where the menu does not correspond to the kb) then we will
;;; go ahead an test to see if the principle is a valid one.  
;;;
;;; If they have not picked a valid principle then they will be told that and
;;; prompted to select again or bottomed out if they have made too many errors.


(defparameter **nsh-repeat-fp-resp**
    (strcat "You have already tried that principle application.  As I "
	    "told you then it is wrong.  In order to move ahead you "
	    "need to try something that you have not tried before.  Why "
	    "don't you do that now."))


(defun nsh-check-first-principle-response (response sought past)
  (let (Type (Value (if (listp response) Response (list Response))))
    (cond ((member Value past :test #'equal) 
	   (nsh-wrong-fp-resp **nsh-repeat-fp-resp** Sought Past
			      :Case 'Repeat))
	  
	  ;; If the student selects a forbidden principle then we want to
	  ;; tell them that.
	  ((member (car Value) (problem-forbiddenPSMs *cp*)) 
	   (nsh-cfp-forbidden Sought Past))
	  
	  ;; If we have a non-forbidden and non-repeated response we need to 
	  ;; determine if it is an appropriate type in its own right.  Once that 
	  ;; is done then we can go ahead and test the type against the solutions.
	  ((setq Type (nsh-lookup-principle-type (car Value)))
	   (nsh-cfp-check-filter Type (if (cadr Value) (cadr Value) no-bindings) 
				 Sought (cons Value past)))
	  
	  ;; If the principle is not of a recognized type then deal with it.
	  (t (nsh-cfp-principle-null Sought past)))))



;;; If the students have elected to work on a forbidden principle then we will
;;; tell them that and prompt them to move on.  This will also be called if the
;;; student has chosen a principle that is forbidden by virtue of being part of 
;;; a forbidden PSMGroup.
(defun nsh-cfp-forbidden (Sought Past)
  (nsh-wrong-fp-resp 
   (strcat "That principle application is forbidden by the problem "
	   "statement.  Why don't you re-read it and try again.")
   Sought past
   :Case 'Forbidden))




;;; An element is a valid filter iff it is a PSMClass struct or a PSMGroup
;;; struct and posesses a unification form.
;;;
;;; Looking up a psm filter by name is the same as looking up a
;;; psmclass or psmgroup by name and testing if it is a valid
;;; filter type.  As above this exists for cleanliness purposes
;;; only.
(defun nsh-lookup-principle-type (type)
  "Lookup the specified principletype by name."
  (let ((Class (lookup-psm-name Type)))
    (when (and Class (or (PSMCLass-p Class)
		     (and (PSMGroup-P Class)
			  (PSMGroup-form Class))))
      Class)))


;;; If the student selects a nonexistent PSMClass or PSMGroup name, or
;;; an invalid one, then the system will warn them that that is an
;;; error and prompt them to both select again and to inform the
;;; system administrator of their problem.  This will only occur when
;;; there is a difference between the knowledge base as stored in the
;;; help system and the list of psms as displayed to the user.  For
;;; now the workbench list is encoded separately from the knowledge
;;; base.
(defun nsh-cfp-principle-null (Sought past)
  "Account for an unknown psm name."
  (nsh-wrong-fp-resp
   (strcat "Although that principle application is on the menu, Andes has "
	   "no record of it internally.  Please inform the Andes Development "
	   "group or your professor and select again.")
   Sought past
   :Case 'Null))






;;; ------------------- check filter -----------------------------------------
;;; Once the student has selected a valid filter type and we have determined
;;; that it is not a repeat, then we want to check to determine if one of the
;;; principle nodes that it matches to is a valid first principle.  This is a
;;; matter of collecting all of the principes that match it, and determining
;;; if Any one of them is a valid first-principle for this problem.
;;;
;;; When we get around to adding in heiarchical hints this will be the place in
;;; which that is done.  For now it will simply use the form + bindings as a
;;; filter.  
;;;
;;; When the student makes a selection we have one of the following cases:
;;;  1. They have selected a valid principle.  In which case we inform them of
;;;     this and prompt them to begin working on it.
;;;  2. They have selected an invalid principle that they attempted once 
;;;     before.  In which case we remind them of that fact and move on.
;;;  3. They have selected a principle that is not present in the graph
;;;     because it is forbidden and is therefore an unacceptable choice.
;;;     In this case we will inform them of this fact and prompt them to 
;;;     try again.
;;;  4. They have selected a first-principle that is not a valid choice 
;;;     either because it is not in the graph or because it is not 
;;;     connected appropriately to the sought.  And the first principles
;;;     are connected directly to the soughts.
;;;  5. They have selected a first-principle that is not a valid choice 
;;;     either because it is not in the graph or because it is not 
;;;     connected appropriately to the sought.  And the first principles
;;;     are not connected directly to the soughts.
;;;
;;; In the event of multiple principles unifying we select the most charitable
;;; response favoring ones in the ideal solution over all others else we pick
;;; randomly.


;;; Given a Principle class, a set of bindings for the variables within
;;; the class, the sought, and a record of past selections:
;;;  1. Test to see if the student has chosen a forbidden class.  If so 
;;;     then inform them of that fact and move on.  Note that this 
;;;     is a backup from what occured above.
;;;  2. Collect all of the potential first-principles in the solution.
;;;  3. Filter those with the supplied class and bindings to locate 
;;;     any that match the student's selection.  


;;;  2. 
;;;  1. Collect all of the potential first-principles in the solution.
;;;  2. Filter those with the supplied class and bindings to locate 
;;;     any that match the student's selection.  
;;;  3. Test the set:
;;;      If it is null
(defun nsh-cfp-check-filter (Class bindings Sought Past)
  (if (nsh-forbidden-psmgroupp (psmclass-group Class)) (nsh-cfp-forbidden Sought Past)
    (let* ((FirstPrinciples  (nsh-collect-first-principles Sought))
	   (Choices (nsh-filter-principles-by-form (psmclass-form Class) Bindings))
	   (Valids (intersection Choices FirstPrinciples)))
      (cond ((null Choices) (nsh-cfp-filter-null Class Bindings Sought Past))
	    (Valids (nsh-cfp-choose-best-fp Valids Sought Past))
	    (t (nsh-cfp-invalid Choices Sought Past))))))
      

;;(Valids (principles (nsh-filter-principles-by-form (psmclass-form Class) Bindings)))
;;      (cond ((null principles) (nsh-cfp-filter-null Class Bindings Sought Past))
;;	    ((nsh-forbidden-psmgroupp (psmclass-group Class)) (nsh-cfp-forbidden Sought Past))
;;	    ((setq Valids (nsh-cfp-collect-valid-fps Principles Sought))
;;	     (nsh-cfp-choose-best-fp Valids Sought Past))
;;	    (t (nsh-cfp-invalid Principles Sought Past)))))


;;; Given a psmgroup determine if it is forbidden in the problem description
;;; this is done by checking to see if its name or the name of any of its 
;;; supergroups is listed in problem's forbiddenPSMs list.  If so then it
;;; is forbidden if not then it is not.
(defun nsh-forbidden-psmgroupp (Group)
  (when Group
    (or (member (psmgroup-name group) (problem-ForbiddenPSMS *cp*))
	(nsh-forbidden-psmgroupp (psmgroup-supergroup Group)))))
			       



;;; Given a form cycle through the list of principles in the solution
;;; filtering out all those that don't unify with the form.
(defun nsh-filter-principles-by-form (form Bindings)
  "Collect the principles that fit the form."
  (remove-if-not 
   #'(lambda (P) 
       (unify (nsh-principle-expression P) form Bindings))
   (bubblegraph-enodes (problem-Graph *cp*))))
  


;;; ------------------------ Selecting valid ------------------------------
;;; Given a list of principles and the sought select collect up the
;;; set of valid first principles from the list and return them.  This 
;;; list will be used by the system to guide the student into working on
;;; an appropriate solution.  The list of *first-principles* itself
;;; was set in the NSH intialization. 
(defun nsh-cfp-collect-valid-fps (Principles Sought)
  "Collect the valid first-principles from the set."
  (intersection Principles (nsh-collect-first-principles Sought))) 
  

;;; This function collects up all the valid first-principles for a given 
;;; sought.  This code defined what it means to be an acceptable 
;;; first-principle initially we defined the first-principle as being the 
;;; first major principle encountered in the solution. 
;;;
;;; Now we define it as all major or definition principles located in
;;; the "first-tier" from the sought.  Therefore any major principles 
;;; or definitions that contain the sought are valid.  If no major 
;;; principles or definitions contain the sought then the all major
;;; principles or definitions 1 hop away from the sought are valid.
;;; The definition proceeds for n until some major principle or definition
;;; is reached.  If none is found then the system will throw an error.
;;;
;;; This function makes use of caching.  Gien a sought it attempts to 
;;; determine if the appropriate first-principles for that sought have
;;; been found and cached in the *nsh-first-principles* parameter.  If 
;;; it has then the value will be returned.  If not then the code will
;;; collect the values and cache them in the parameter.
;;;
;;; When collecting the values, it begins by attempting to collect all
;;; of the acceptable principles right next to the sought.  Failing 
;;; that it recurses on the quantities one-tier away from the sought in 
;;; a Breath-first-search manner.
;;;
;;; If this fails to work the system will use the old standard for first
;;; principles namely the first principle in the set.  This should only 
;;; be called on truly old problems.  
(defun nsh-collect-first-principles (Sought)
  "Collect the acceptable first-principles for the sought."
  (let ((Principles (assoc Sought *nsh-first-principles* :test #'equalp)))
    (cond 
     (Principles (cdr Principles))
     
     ((setq Principles  
	(or (nsh-recursively-collect-fps (list Sought))
	    (nsh-collect-old-first-principles)))
      (push (cons Sought Principles) *nsh-first-principles*)
      Principles)
     
     (t (error "No Compatibe first-principles found by NSH.")))))

;;; Collecting first-principles recursively is a matter of selecting all of the 
;;; principles that are n steps away from the sought.  Once those are collected 
;;; we collect all of their quantities (ignoring the current sought) and test 
;;; all of their principles to see if they are valid.  If none return t then we 
;;; up the ante and recurse again.  It is possibly for there to be cycles in this
;;; process but, because we search the graph in a breath-first-search manner this
;;; will not be a problem.
(defun nsh-recursively-collect-fps (Soughts &optional (ignore Nil))
  "Print out the first principles."
  (let (tmp (Successors (nsh-collect-soughts-successors Soughts Ignore)))
    (when Successors
      (if (setq tmp (remove-if-not #'nsh-acceptable-fp-typep Successors)) tmp
	(nsh-recursively-collect-fps (nsh-collect-principles-successors Successors Soughts) 
				     (append Successors ignore))))))


;;; Given a list of quantities collect their principle successors connected to
;;; them ignoring any that are located in the ignore list.
(defun nsh-collect-soughts-successors (Soughts Ignore)
  "Collect the nonignore successors of soughts."
  (remove-if
   #'(lambda (N) (member N Ignore))
   (remove-duplicates
    (apply #'append (mapcar #'nsh-quantity-principles Soughts)))))


;;; given a list of principles collect the quantities that are connected
;;; to them ignoring those that are in the Ignore list.
(defun nsh-collect-principles-successors (Principles Ignore)
  "Collect the quantity successors to the principles."
  (remove-if
   #'(lambda (N) (member N Ignore))
   (remove-duplicates 
    (apply #'append (mapcar #'nsh-principle-quantities Principles)))))
  

;;; Is this an aceptable fp type?
(defun nsh-acceptable-fp-typep (Principle)
  "IS the specified principle an accpetable type for an fp?"
  (or (nsh-major-principle-p Principle) 
      (nsh-definition-principle-p Principle)))


;;; Collect the first principles in the old format for backup.  
(defun nsh-collect-old-first-principles ()
  (mapcar #'car *nsh-solution-sets*))



;;; In some instances it is necessary to determine whether or not the sought is
;;; directly connected to the first-principle without actually searching for it 
;;; This predicate will return t if there is at least one first principle that 
;;; is directly connected to the sought.  Otherwize it will return nil.
(defun nsh-atleast1-first-principle-directly-connected-p (Sought)
  "Return t if at leasrt one first principle is directly connected."
  (let ((Principles (nsh-collect-first-principles Sought)))
    (loop for P in Principles
	when (member Sought (nsh-principle-quantities P) :test #'equalp)
	return t)))


(defun trace-nsh-first-principles ()
  (trace nsh-cfp-collect-valid-fps
	 
	 nsh-collect-first-principles 
	 nsh-recursively-collect-fps
	 nsh-collect-soughts-successors
	 nsh-collect-principles-successors
	 	 
	 nsh-collect-old-first-principles
	 
	 nsh-acceptable-fp-typep))



;;; ------------------- null principles ------------------------------
;;; If no principles in the graph unify with the supplied form then
;;; one of two things has occured.  Either the student has selected
;;; something that is simply not present in the graph or they have
;;; made a bad axis choice.  The bad axis choice can be due to a 
;;; misconception or to a simply slip of the mouse.  Either way it 
;;; seems likely to occur on a regular basis and so we want to trap
;;; for it and offer the student a special hint, and a chance to 
;;; change their axis if necessary.  The cfp-filter-null func
;;; tests to determine if the student has the right psmclass but 
;;; wrong axis.  If so then it gives them a specialized prompt to 
;;; change their axis.  If not then it will send them an error 
;;; message and move them on.  
(defun nsh-cfp-filter-null (Class Bindings Sought Past)
  (if (get-binding '?Axis Bindings) 
      (nsh-cfp-change-axis Class Bindings Sought Past)
    (nsh-cfp-filter-null-resp Sought Past)))


;;; If the student has supplied a binding for the axis then we will test 
;;; to see if a changed axis will help them any.  If so then we will tell
;;; them that and go on with it.  If not then we will try something else.
;;;
;;; Here we are only interested in whether or not there are valid first-
;;; principles on the other axis, we do not care about picking them.  
;;; therefore the system merely collects them up to see if the set is null.
(defun nsh-cfp-change-axis (class Bindings Sought Past)
  (let* ((OldAxis (lookup '?Axis Bindings))
	 (NewAxis (if (equal OldAxis 'X) 'Y 'X))
	 (NewBindings (change-bindings '?Axis NewAxis Bindings))
	 (Choices (nsh-filter-principles-by-form (psmclass-form Class) NewBindings))
	 (Valid (nsh-cfp-collect-valid-fps Choices Sought)))
    (cond ((null Choices) (nsh-cfp-filter-null-resp Sought Past))
	  ((null Valid) (nsh-cfp-invalid-axis-resp OldAxis NewAxis Sought Past))
	  (t (nsh-cfp-prompt-change-axis 
	      NewAxis Choices Class NewBindings Sought Past)))))



;;; If there are no acceptable principles of their chosen type then we take
;;; the student here and give them the following message.
(defun nsh-cfp-filter-null-resp (Sought Past)
  (nsh-wrong-fp-resp
     "No principle applications of the type that you selected appear in the solution.  "
     Sought Past :case 'FilterNull))




;;; If the student has picked a principle/axis pair where instances of the
;;; principle appear in the solution but not on the axis that they specified
;;; yet it is still invalid as a first-principle choice, then we want to 
;;; tell them that and move on.  
(defun nsh-cfp-invalid-axis-resp (OldAxis NewAxis Sought Past)
  (nsh-wrong-fp-resp
   (format Nil (strcat "No application of that principle on the ~a axis appear in "
		       "a solution to this problem.  However, that principle can "
		       "be applied on the ~a axis.  Unfortunately, they cannot be "
		       "used to start the solution.") OldAxis NewAxis)
   Sought Past :Case 'Invalid-Axis))


;;; If changing the axis will be profitable then we will offer that as a 
;;; possibility to the students and let them decide whether they wish to 
;;; use that or not.  Depending upon their choice we will move on from 
;;; there.  
(defparameter **nsh-cfp-change-axis-string**
    (strcat "The principle application that you chose is not correct.  However, it "
	    "is correct to apply that same principle along the ~a axis.  Do you "
	    "with to use that axis instead?"))

(defun nsh-cfp-prompt-change-axis (Axis Choices Class Bindings Sought Past)
  "Prompt the student to change their axis to the other or try again."
  (declare (ignore Bindings) (ignore Class))
  (make-dialog-turn 
   (format Nil **nsh-cfp-change-axis-string** Axis)
   **YES-No-Menu**
   :Responder #'(lambda (Response)
		  (if (String-Equal Response "NO")
		      (nsh-wrong-fp-resp "" Sought Past :Case 'ChangeNo)
		    (nsh-cfp-choose-best-fp
		     (nsh-cfp-collect-valid-fps Choices Sought) 
		     Sought Past)))
   :Assoc `(nsh cfp-prompt-change-axis ,Axis)))



;;; -------------------------- Choose-best-fp ----------------------------------------
;;; Once the student has selected (or been given) a valid first-principle filter then 
;;; we want to choose the specific first-principle/solution that we will prompt them
;;; to make.  This code takes a list of valid first principles, the sought, and Past
;;; and, after mapping principles to solutions, selects the best.  
;;;
;;; The student will favor principles that are part of the ideal solutions and have
;;; been completed.  
;;;
;;; If the student's current axes are not part of the ideal solution then we will
;;; prompt them to change it if they wish.  If the student chooses to change their axes
;;; then we will go with the ideal solution.  If not then we will prompt them with a 
;;; solution that matches their current axes or, if there is none, prompt them to 
;;; pick a different first principle.  
;;;
;;; NOTE:: This code assumes that the list of principles will contain AT LEAST ONE
;;;        valid fp.

;;; those principles
;;; that are valid and then pick the best of them.  First eliminate
;;; all that are invalid then find the solutions with them, then 
;;; pick the best. 
;;;
;;; At this point we also want to test whether or not the student is on
;;; the best axis-dirven path.  When selecting the best solution, the 
;;; system will favor those solutions that have the least number of 
;;; uncompleted principles.  One side-effect of this, however is that
;;; the student may have drawn different axes than the ones specified
;;; by the ideal solution.  This can be particularly confusing in the
;;; case where they have been following NSH and we told them to draw one
;;; set of axes only now to tell them to draw others.  
;;;
;;; In order to deal with this the system will prompt the student with
;;; a dialog to deal with the situation.  They will be offered the chance
;;; to ditch their current axes and move on to a much easier path, or
;;; to stand by them and suffer the consequences.  If the spineless curs
;;; choose to abandon their dear freinds like that we will prompt them
;;; to re-draw their axes and then return to NSH, else we will just tell
;;; them to move on with life and not ask too many questions.
;;;
;;; NOTE: that we don't take into account whether or not the principle 
;;; is linked to the sought because we have no guarantee that it will be
;;; just being first is usually sufficient.

(defun nsh-cfp-choose-best-fp (Principles Sought Past)
  "Choose the best first principle choice."
  (let ((Solutions (nsh-filter-solutions-by-fps Principles)))
;;    (if (= 1 (length Solutions)) 
;;	(nsh-cbf-success Solutions Principles Sought Past)
      (nsh-cbf-filter-solutions Solutions Principles Sought Past)))


;;; Given a set of solutions pick the best and then test to see if its axes
;;; match the students current axes.  If so then accept it and choose among
;;; the valid fps within it.  If not then prompt the student to change their
;;; axes or choose another.
;;;
;;; Note:: For solutions that contain no axes we treat them as if the axes 
;;;  have always been entered this fits in with our general principle that
;;;  the students should always draw axes even if they turn out to be 
;;;  unnecessary.  
(defun nsh-cbf-filter-solutions (Solutions Principles Sought Past)
  "Given a set of solutions choose the best for the student."
  (let ((best (nsh-pick-best-solution Solutions)))
    (cond ((null (nsh-pick-axes-done-solutions Solutions))
	   (nsh-cbf-invalid Best Solutions Principles Sought Past))
	  ((nsh-1solution-axis-enteredp best)
	   (nsh-cbf-success Best Principles Best Principles))
	  (t (nsh-cbf-prompt-change Best Solutions Principles Sought Past)))))


;;; If no solutions contain both the student's selected sought, and the axis(ies)
;;; that the student has drawn then we need to alert the student to that fact and
;;; prompt them to either change their axis or to choose another first principle.
(defun nsh-cbf-invalid (Best Solutions Principles Sought Past)
  "Alert the student to their state and start them over."
  (make-dialog-turn
   (strcat "While the principle application that you have chosen a valid way to "
	   "start a problem solution, there are no solutions that apply it with "
	   "the coordinate system that you have currently drawn.  Do you want to "
	   "change your axes rotationor pick a different principle application?")
   '("Axes" "Principle")
   :Responder #'(lambda (Response)
		  (if (string-equal Response "Axes")
		      (nsh-cbf-invalid-Axis
		       Best Solutions Principles Sought Past)
		    (nsh-wrong-fp-resp "" Sought Past :case 'cbf-invalid-no)))
   :Assoc '(nsh cbf-invalid)))


;;; If the student elects to change their axes then we need to determine what an acceptable
;;; axis choice is (taking the axes from the best solution.  And then prompt them to make 
;;; it.  Once they have done that then we tell them to ask for NSH again and bring them
;;; back to the current location.  This necessitates making NSH capable of taking a 
;;; temporary continuation function that will be called the next time NSH is called.
(defun nsh-cbf-invalid-axis (Best Solutions Principles Sought Past)
  "Prompt the student to change their axes to those supplied and move on."
  (let ((Axes (nsh-collect-correct-student-axes)) 
	(Prompt (car (nsh-collect-solution-axes Best)))) 
    
    ;; Set the function that will be used to test the next NSH call.
    (nsh-set-next-call
     (nsh-cbf-invalid-func
      Axes Prompt Best Solutions Principles Sought Past))
    
    ;; Give the student the initial prompt that we want them to carry out.
    (nsh-cbf-axis-change-prompt Axes Prompt))) 



;;; This function builds and returns the closure that will be called by NSH the 
;;; next time that it is called (after the student makes their changes).  If [1] 
;;; the student has deleted the axes (s.t. there are no correct axes entries) 
;;; then they will be told to draw axes at the target rotation.  If [2] they have 
;;; made no change to their axes then they will be given the same prompt that they
;;; were before.  If [3] they have made the correct axis then they will be sent 
;;; on their way and the function will be cleared.  Lastly, [4] if they have 
;;; changed to a different set of correct axes then they will be told that they
;;; may stick with these or move on.  
(defun nsh-cbf-invalid-func (Axes Prompt Best Solutions Principles Sought Past)
  #'(lambda ()
      (let ((Student (nsh-collect-correct-student-axes)))
	(cond 
	 ;; IF the student has deleted all their axes.
	 ((null Student) (nsh-cbf-axis-draw-prompt Prompt))
	 ;; If the student has not changed their axes at all then prompt them again.
	 ((equalp Axes Student) (nsh-cbf-axis-change-prompt Student Prompt))
	 ;; Success the student did what we asked (good student) and we move on.
	 ((member Prompt Student)
	  (nsh-clear-next-call)
	  (nsh-cbf-filter-solutions Solutions Principles Sought Past))
	 ;; The student has chjanged to a correct set but not the set that
	 ;; we wanted so move them along.
	 (t (nsh-cbf-invalid-different Student Prompt Best Solutions Principles Sought Past))))))


;;; If the student has changed from the axes that we specified to new correct axes 
;;; (NOTE:: we really require that only one of the axes be correct incorrect are
;;;  ignored this might be slightly but not that completely confusing.) then we 
;;; want to tell them what they have done and offer them the chance to stick with 
;;; these new axes or be prompted on to the ones that we meant.  Note that we Don't 
;;; offer them the chance to go back because we are assuming that they want to move 
;;; forward (as they said).  
(defun nsh-cbf-invalid-different (New Prompt Best Solutions Principles Sought Past)
  "Prompt the student to their wrongs."
  (let ((form (format Nil "~a" Prompt)))
    (make-dialog-turn
     (strcat "While those axes are also correct, they are not the "
	     form " degree axes that you were prompted to draw.  "
	     "Do you want to stay with these new axes or switch to "
	     "the " form " ones?")
     '("Stay" "Switch")
     :Responder #'(lambda (Response)
		    (cond ((string-equal Response "Stay")
			   (nsh-clear-next-call)
			   (nsh-cbf-filter-solutions Solutions Principles Sought Past))
			  (t  ;; Note the recursion here is intended to reset the lambda 
			   ;; closure that gets stored in invalid-axis.  This allows the 
			   ;; system to be dealing with the appropriate axes.
			   (nsh-cbf-invalid-axis Best Solutions Principles Sought Past))))
     :Assoc `(nsh cbf-invalid-different ,New ,Prompt))))


;;; If the student's choices are compatible but the student's chosen axes are not the same
;;; as in the ideal solution then we will inform them that they can reduce the complexity
;;; of their solution by changing their axes and offer them a chance to do so.  
;;;
;;; If the student elects to change their axes then we will prompt them to do so.  Picking
;;; an axis from the best solution.  If they elect not to then we will prompt them to 
;;; work on the best solution of the set containing the axes that they have drawn.
(defun nsh-cbf-prompt-change (Best Solutions Principles Sought Past)
  "Prompt the student to change their axes rotations for the best."
  (make-dialog-turn
   (strcat "You can form a solution using the principle application that you "
	   "have chosen and the axes that you have drawn.  However if you "
	   "change the rotation you can craft a less complex solution.  "
	   "Do you want to change your axes?")
   **Yes-No-Menu**
   :Responder #'(lambda (Response)
		  (if (string-equal Response "Yes")
		      (nsh-cbf-change-yes Best Solutions Principles Sought Past)
		    (nsh-cbf-change-no Solutions Principles Sought Past)))
   :Assoc `(nsh cbf-prompt-change)))


;;; If the student elects to change their solution then we will go ahead and prompt them
;;; to draw the axes of the best solution and then call NSH when they are done.  If, after
;;; seeing the prompt they do not change their axes (or delete them) then we will give 
;;; them the same hints.  If, however they change to a different set of correct axes, 
;;; neither what they had nor what they were prompted to do then we will give them a 
;;; different message.  This new message will explain what they have done and then offer 
;;; them the choice to stick with these new axes, return to the old ones or move on to new 
;;; ones. 
(defun nsh-cbf-change-yes (Best Solutions Principles Sought Past)
  "Change the axes Yes."
  (let ((Axes (nsh-collect-student-axes)) 
	(Prompt (car (nsh-collect-solution-axes Best)))) 
    
    ;; Set the function that will be used to test their next NSH call.
    (nsh-set-next-call 
     (nsh-cbf-change-yes-func 
      Axes Prompt Best Solutions Principles Sought Past))
    
    ;; Prompt them to actually make the change.
    (nsh-cbf-axis-change-prompt Axes Prompt))) 


;;; This function builds and returns the closure that will be called by NSH the 
;;; next time that it is called (after the student makes their changes).  If [1] 
;;; the student has deleted the axes (s.t. there are no correct axes entries) 
;;; then they will be told to draw axes at the target rotation.  If [2] they have 
;;; made no change to their axes then they will be given the same prompt that they
;;; were before.  If [3] they have made the correct axis then they will be sent 
;;; on their way and the function will be cleared.  Lastly, [4] if they have 
;;; changed to a different set of correct axes then they will be told that they
;;; may stick with these or move on.  
(defun nsh-cbf-change-yes-func (Axes Prompt Best Solutions Principles Sought Past)
  #'(lambda ()
      (let* ((student (nsh-collect-correct-student-axes)))
	(cond 
	 ;; If the student has deleted their axes rather than changed them.
	 ((null Student) (nsh-cbf-axis-draw-prompt Prompt))
	 ;; If the student has not made any changes them prompt them again.
	 ((equalp Axes Student) (nsh-cbf-axis-change-prompt Student Prompt))
	 ;; If they have done what we asked they both get a spooful of sugar 
	 ;; and get to move on (actually I was kidding ther is no spoon).
	 ((member Prompt (nsh-collect-student-axes)) 
	  (nsh-clear-next-call)
	  (nsh-cbf-filter-solutions Solutions Principles Sought Past))
	 ;; The student has changed to a different (correct) set of axes so
	 ;; we prompt the, to work on that instead.
	 (t (nsh-cbf-change-different Student Prompt Best Solutions Principles Sought Past))))))



;;; If the student has changed from the axes that we specified to new incorrect axes
;;; then we want to tell them what they have done and offer them the chance to stick 
;;; with these new axes or be prompted on to the ones that we meant.  Note that we 
;;; Don't offer them the chance to go back because we are assuming that they want
;;; to move forward (as they said).  
(defun nsh-cbf-change-different (New Prompt Best Solutions Principles Sought Past)
  "Prompt the student to their wrongs."
  (let ((form (format Nil "~a" Prompt)))
    (make-dialog-turn
     (strcat "While those axes are also correct, they are not the "
	     form " degree axes that you were prompted to draw.  "
	     "Do you want to stay with these new axes or switch to "
	     "the " form " ones?")
     '("Stay" "Switch")
     :Responder #'(lambda (Response)
		    (cond ((string-equal Response "Stay")
			   (nsh-clear-next-call)
			   (nsh-cbf-filter-solutions Solutions Principles Sought Past))
			  (t ;; Note the recursion here is intended to reset the lambda 
			   ;; closure that gets stored in change-yes.  This allows the 
			   ;; system to be dealing with the appropriate axes.
			   (nsh-cbf-change-yes Best Solutions Principles Sought Past))))
     :Assoc `(nsh cbf-change-different ,New ,Prompt))))

  

;;; If the student has elected to stick with their current axes then we want to 
;;; filter the solutions that are compatible with their selection, and to prompt
;;; them to work on the best.
(defun nsh-cbf-change-no (Solutions Principles Sought Past)
  "Prompt the best of the compatible solutions."
  (nsh-cbf-success (nsh-pick-best-solution (nsh-pick-axes-done-solutions Solutions))
		   Principles Sought Past 
		   :Prefix "As you wish.  "))



;;; When we are prompting the students to change their axes these are the hints that
;;; we will give them.
(defun nsh-cbf-axis-change-prompt (Axes Prompt)
  (make-hint-seq 
   (list 
    (strcat "You should change your X axes to " (format Nil "~a" Prompt) " degrees.")
    (strcat "You should doubleclick on the axes that you have already drawn and "
	    "change the angle in the dialog to " (format Nil "~a" Prompt) " degrees."))
   :Assoc `(nsh cbf-axis-change-prompt ,Axes ,Prompt)))


;;; When we are prompting the students to change their axes and they delete
;;; the entries (giving them no correct axes entries) then we want to use
;;; these hints to tell them to draw axes at the specified rotations.
(defun nsh-cbf-axis-draw-prompt (Prompt)
  (make-hint-seq 
   (list 
    (strcat "You should draw axes at " (format Nil "~a" Prompt) " degrees.")
    (strcat "You should use the axes tool (looks like a +) to draw a pair of"
	    "axes at " (format Nil "~a" Prompt) " degrees.  By selecting the "
	    "tool, drawing the axes, and then entering " (format Nil "~a" Prompt)
	    " into the dialog box when it appears."))
   :Assoc `(nsh cbf-axes-draw-prompt ,Prompt)))


;;; ---------------------------- Success ----------------------------------------------
;;; Once the cbf has selected a solution that we wish to hint we need to identify the
;;; specific first principle (from among the set of principles) that will be prompted.
;;; Once that is done the specific first-principle will be prompted according to its 
;;; state, and the *nsh-current-solutions* will be defined as the set of all solutions
;;; that contain the chosen-fp.  This reverse-lookup is done for the purposes of later
;;; solving.
;;;
;;; If One is completed take it.
;;; Elif One is started then take it.
;;; Else take the first one.  
;;;
;;; NOTE:: for the purposes of this code I am assuming that all acceptable fp's are
;;;        the same and am ignoring the Principle/Definition divide.  
(defun nsh-cbf-success (Solution Principles Sought Past &Key (Prefix ()))
  (declare (ignore Sought) (ignore Past))
  (let (Best (Choices (intersection Solution Principles))
	(Comment (or Prefix (random-positive-feedback))))
    (cond ((setq Best (find-if #'nsh-node-completed-p Choices))
	   (nsh-cfp-success-completed Comment Best Solution))
	  (t (setq Best (or (find-if #'nsh-node-started-p Choices) (car Choices)))
	     (nsh-cfp-success-uncompleted Comment Best)))))


;;; Given a successful first-principle collect the set of 
;;; solutions that contain it.
(defun nsh-cfp-match-fp-sol (Principle)
  (remove-if-not #'(lambda (S) (member Principle S))
		 *nsh-solution-sets*))



;;; If the student has already completed the best principle, then
;;; we want to prompt them to move on to the next principle in the
;;; solution.  
(defun nsh-cfp-success-completed (Prefix Best Solution)
  "Promp the student to continue on when the first principle is completed."
  (setq *nsh-current-solutions* (nsh-cfp-match-fp-sol Best))
  (make-dialog-turn
   (strcat Prefix "  You have already completed the "
	   (nlg (nsh-principle-expression Best) 'psm-exp) 
	   " so why don't you move on to the next step in the process.")
   **explain-more**
   :Responder #'(lambda (Resp)
		  (when (eq Resp **Explain-More**)
		    (nsh-prompt-solution "Why don't you work on " Solution)))
   :Assoc `(nsh cfp-success-completed ,(nsh-Node-expression Best))))



;;; If the student has not completed the first principle
;;; then we want to go ahead and prompt them to make the
;;; entry as necessary.
(defun nsh-cfp-success-uncompleted (Prefix Best)
  "Prompt the student to work on an uncompleted first principle."
  (setq *nsh-current-solutions* (nsh-cfp-match-fp-sol Best))
  (nsh-walk-node-graph Prefix Best))


	 
  
  
;;; ----------------- invalid choices --------------------------------
;;; If the student selects principles that are present but invalid
;;; then we want to tell them that.  In future this will be modified
;;; to accunt for heiarchical hints but, for the time being, this 
;;; will simply stick with the stock message.  Later on we want to 
;;; start discussing whether they have the right type of principle
;;; but the wrong axis for example.  
(defun nsh-cfp-invalid (Principles Sought Past)
  (declare (ignore Principles))
  (nsh-wrong-fp-resp 
   (strcat "That principle is part of a valid solution "
	   "to this problem but you cannot use it as the "
	   "first principle.")
   Sought Past :Case 'Invalid))



;;;--------------- Wrong principle -------------------------------
;;; If the student fails to select the appropriate principle 
;;; then we want to give them an appropriate response message
;;; and, if they have failed **max-first-principle-tries** then 
;;; we want to tell them which principle to use.  If it comes to 
;;; that then the system will search for first any solution where 
;;; the first principle has been completed and then the shortest
;;; solution (accounting for entered principles.)
;;;
;;; This function can also take two optopnal arguments  Case which
;;; lists the reason for the repeat, and FPs which may or may not 
;;; contain the acceptable first principles for this problem.  If
;;; These are passed in then they will be used to direct the 
;;; continue message.  If not then they will be generated as needed.
(defun nsh-wrong-fp-resp (message Sought past &key (Case 'default-wrong)) 
  (if (< (length past) **max-first-principle-tries**)
      (nsh-ask-first-principle-cont message Sought past Case)
    (nsh-pick-fp message Sought :Case Case)))


;;; If the student has failed too many times then select the
;;; appropriate principle first looking for finished, then
;;; for an appropriate completed solution.
;;; prompting a principle will vary if it is done or not.
;;;
;;; The problem is how to deal with solu
(defun nsh-pick-fp (message Sought &key (Case 'Default-nsh-pick-best))
  "pick the appropriate first principle."
  (let* ((P (nsh-pick-best-fp Sought))
	 (s (remove-if-not #'(lambda (S) (equal (car S) P)) *nsh-solution-sets*))) 
    (setq *nsh-current-solutions* S)
    (if (nsh-principle-completed-p P)
	(nsh-prompt-done-fp message (nsh-pick-best-solution S) :Case Case)
      (nsh-prompt-fp-solution message P :Case Case))))


;;; If we have to tell the student what first-principle to begin with then we
;;; will pick the best solution that matches the axes that they have currently
;;; drawn and then go ahead and pick the best availible first principle from
;;; that solution based upon the sought.  This is a matter of intersecting the
;;; set of acceptable first-principles with the best axes-done solution and then
;;; returning the result(s).  
(defun nsh-pick-best-fp (Sought)
  (let* ((Principles (nsh-collect-first-principles Sought))
	 (Best (nsh-pick-best-solution
		(nsh-filter-solutions-by-fps
		 Principles (nsh-pick-axes-done-solutions)))))
    (if Best (car (intersection Principles Best))
      (error "Invalid first-principle selection."))))
	

;;; If the first principle in our chosen solution is done, then we 
;;; will go ahead and prompt the students to do it with a comment
;;; Again this is stubbed for now.
(defun nsh-prompt-done-fp (message Solution &key Case)
  "Prompt the solution."
  (make-dialog-turn
   (strcat Message "  You have already completed "
	   (nlg (nsh-principle-expression (car Solution)) 'psm-exp)
	   ".  Which is acceptable as an initial principle application.  "
	   "Why don't you start on the next principle.")
   **Explain-More**
   :Responder #'(lambda (Response)
		  (when (equal Response **Explain-More**)
		    (nsh-prompt-solution "Why don't you work on " Solution)))
   :Assoc `(nsh prompt-done-fp ,Case ,(nsh-principle-expression (car Solution)))))



;;; Once we have a principle to prompt then we want to tell the students
;;; to work on it.  The solutions that contain it will have already been 
;;; saved so all that is necesssary it to tell them to continue on with it.  
(defun nsh-prompt-fp-solution (Message Principle &key Case)
  "prompt the student to work on the selected first principle."
  (nsh-prompt-principle 
   (strcat Message "Lets just assume that you will work on ")
   Principle
   :Assoc `(nsh prompt-fp-solution ,Case ,(nsh-principle-expression Principle))))




;;;; Trace afp.
(defun trace-nsh-ask-first-principle ()
  (trace nsh-ask-first-principle 
	 nsh-ask-first-principle-cont
	 
	 nsh-check-first-principle-response 
	 nsh-lookup-principle-type
	 nsh-cfp-principle-null
	 
	 nsh-cfp-check-filter
	 nsh-filter-principles-by-form
	 nsh-cfp-collect-valid-fps
	 
	 nsh-cfp-filter-null
	 nsh-cfp-change-axis
	 nsh-cfp-filter-null-resp
	 nsh-cfp-invalid-axis-resp
	 nsh-cfp-prompt-change-axis
	 
	 nsh-cfp-choose-best-fp
	 nsh-cbf-filter-solutions
	 nsh-cbf-invalid
	 nsh-cbf-invalid-axis
	 
	 nsh-cbf-prompt-change
	 nsh-cbf-change-yes
	 nsh-cbf-change-no
	 nsh-cbf-axis-change-prompt
	 nsh-cbf-axis-draw-prompt
	 
	 nsh-cbf-success
	 nsh-cfp-match-fp-sol
	 nsh-cfp-success-completed
	 nsh-cfp-success-uncompleted
	 
	 nsh-cfp-invalid
	 
	 nsh-wrong-fp-resp
	 nsh-pick-fp
	 nsh-pick-best-fp
	 nsh-prompt-done-fp
	 nsh-prompt-fp-solution))



;;; Tracing ask.
(defun trace-nsh-ask ()
  (trace-nsh-ask-sought)
  (trace-nsh-ask-first-principle))

	 
;;;; ===================== Prompting Done =========================================
;;;; If the student has completed all of the work in a given solution, then we 
;;;; can know that they are done.  In this case, we need to prompt them to 
;;;; complete the problem by either entering the answer in the answer box (quant 
;;;; probs), or prompting them to select the combo box signifying that they are
;;;; done (no-quant problems).
;;;;
;;;; Note, if we have time, it might be nice to expand this to have the students
;;;; guided through the completion process with specific hints (e.g. write x)
;;;; but this is not necessary.

(defun nsh-done? ()
  (and (nsh-solution-completed-P *nsh-givens*)
       (find-if #'nsh-solution-completed-P
		*nsh-solution-sets*)))


(defun nsh-prompt-done ()
  "Prompt that the student is done with the problem."
  (make-end-dialog-turn
   (strcat "You have completed all of the principle applications "
	   "necessary to solve this problem.  " (nsh-prompt-done-string))
   :assoc '(nsh prompt-done)))


(defparameter **nsh-single-sought-done-str**
    (strcat "Right click in the first empty line in the equation pane "
            "and use 'Solve For...' to find a value for "
	    "the sought quantity (if you have not already) and then "
	    "enter that value in the answer box."))


(defparameter **nsh-multi-sought-done-str**
    (strcat "Right click in the first empty line in the equation pane "
            "and use 'Solve For ...' to find a value for each of the "
	    "sought quantities (if you have not already) and then "
	    "enter those values in the answer boxes."))

(defun nsh-prompt-done-string ()
  (if (cdr (problem-soughts *cp*)) ; more than 1 sought
       **nsh-multi-sought-done-str**
    **nsh-single-sought-done-str**))



(defun trace-nsh-done ()
  (trace nsh-done? nsh-prompt-done
	 nsh-prompt-done-string))


;;;; =================== nsh-start-principle-free ==============================
;;;; On problems that contain no principles we want the students to simply work
;;;; on the ideal solution without asking them what the sought is.  For now this
;;;; will tell them that they need to begin working on the core of the problem
;;;; but nothing else. 

(defun nsh-start-principle-free? ()
  (and (or (null *nsh-givens*) (nsh-solution-completed-P *nsh-givens*))
       (null *nsh-current-solutions*)))


;;; When the student is starting a principle-free problem then we want to 
;;; inform them that they should asess the problem and decide what definition
;;; to apply first.  We will then prompt the first element in the ideal 
;;; solution for them.  
(defun nsh-start-principle-free ()
  (setq *nsh-current-solutions* (list (nsh-pick-best-solution *nsh-solution-sets*)))
  (make-dialog-turn
   (strcat "Excellent, you should now continue the problem by making "
	   "the entries that are necessary to complete the problem "
	   (if (> 1 (length (problem-soughts *cp*))) "goals." "goal."))
   **Explain-More**
   :Responder #'(lambda (Response)
		  (when (string-equal Response "Explain-More")
		    (nsh-prompt-solution 
		     "Why don't you begin by " 
		     (car *nsh-current-solutions*))))
   :Assoc '(nsh start-principle-free)))



(defun trace-nsh-start-principle-free ()
  (trace nsh-start-principle-free?
	 nsh-start-principle-free))
	 
;;;; =================== nsh-continue-solutions ================================
;;;; NSH-Prompt-continue-solution
;;;; If the student has reached this point then we know that they have selected
;;;; a solution and we want to continue on with it.  In order to do that we 
;;;; simply take their stored solutions, select the best one in the list, and 
;;;; prompt them to complete the next PSM within it.

(defun nsh-continue-solutions ()
  (nsh-prompt-solution 
   "Why don't you continue with the solution by working on "
   (nsh-pick-best-solution *nsh-current-solutions*)
   :Assoc `(nsh continue-solutions)))










;;; ============================ Prompting ====================================
;;;
;;; ---------------------------- Prompt Solution ----------------------------
;;; prompting the next solution
;;; Once NSH has a solution that the student should be prompted on, it will 
;;; pick the first uncompleted principle in the list and then prompt the 
;;; students to continue on with it, selecting the first undone step in the
;;; solution to work on.  
    
(defun nsh-prompt-solution (prefix solution &key Assoc)
  "Prompt the next principle in the solution."
  (let ((next (find-if #'(lambda (P) (not (nsh-Node-completed-p P))) Solution)))
    (nsh-prompt-Node
     prefix Next
     :Assoc (or Assoc `(nsh prompt-solution ,(nsh-node-expression Next))))))




;;; ---------------------- Dialog prompting nodes ----------------------------
;;; Given a dialog stack that we want to give the student prior to prompting
;;; the node itself this code does that.  It will generate a dialog stack 
;;; the final element of which will seque into the node's hint stack.

(defun nsh-dialog-prompt-node (Dialog Prefix Node &key Assoc)
  "Prompt the specified node after the specified dialog stack."
  (make-dialog-turn
   Dialog **explain-more**
   :Responder #'(lambda (Resp)
		  (when (eq Resp **Explain-More**)
		    (nsh-prompt-node 
		     Prefix Node
		     :Assoc `(nsh dialog-prompting-node ,(nsh-node-expression Node)))))
   :Assoc Assoc))






;;; ----------------------------- Prompting a node ------------------------------
;;; Prompting a principle occurs when we want to student to continue with, or
;;; finish an already begun principle.  This code will construct a dialog stack
;;; that prompts them to start (or complete) the specifed principle.  The student
;;; may not know that they have begun a principle (shared entries and all that) 
;;; but the system will go ahead and assume that they do in any case.
;;;
;;; Note that the message comes from the function that called prompt principle
;;; this function, knowing the context, should construct an appropriate first 
;;; message for the principle and go from there.  What is added is a simple 
;;; entry directive.
;;;
;;; Note this may be confusing terminology for the students so we might want
;;; to go ahead and just tell them to work on set principles without specifying
;;; whether or not they have already begun.
;;; Note any of these can be called depending upon how specific the knowledge 
;;; of the system is.

(defun nsh-prompt-node (Prefix Node &key Assoc)
  "Prompt the node as appropriate."
  (if (nsh-quantity-p Node)
      (nsh-prompt-quantity Prefix Node :Assoc Assoc)
    (nsh-prompt-Principle Prefix Node :Assoc Assoc)))


;;; -------------------------------------------------------------------------------
;;; Prompting a Quantity.
;;; Prompting a quantity is a matter of telling the student to work on the 
;;; appropriate quantity by type.  At present only parameter nodes should
;;; have any subentries so the system will error if they do not but that may
;;; change if we find it necessary.
(defun nsh-prompt-quantity (Prefix Quantity &key Assoc)
  (cond ((nsh-quantity-parameter-P quantity) 
	 (nsh-prompt-parameter Prefix Quantity :Assoc Assoc))
			       
	(t (error "Inappropriate quantity passed to hint ~a" Quantity))))


;;; Prompting a parameter is a matter of telling the students to work on it
;;; by name and then giving them the sandard psm graph hints.  
(defun nsh-prompt-parameter (Prefix Parameter &key Assoc)
  "Prompt the student to work on the parameter."
  (setq *nsh-last-node* Parameter)
  (make-dialog-turn 
   (strcat prefix (nlg (nsh-Node-expression Parameter) 'psm-exp) ".  ")
   **Explain-More**
   :responder #'(lambda (Response)
		  (when (equal Response **Explain-More**)
		    (nsh-walk-node-graph "" Parameter)))
   :Assoc Assoc)) 


;;; ----------------------------------------------------------------------------
;;; Prompting a principle
;;; Prompting principles differentiate by the type of the principle the message
;;; that will be used.  Apart from that they are all the same.

(defun nsh-prompt-principle (Prefix principle &key Assoc)
  "Prompt the specified principle appropriately based upoin how \"done\" it is"
  (cond ((nsh-goal-principle-P Principle) (nsh-prompt-goal-principle Prefix Principle Assoc))
	((nsh-major-principle-P Principle) (nsh-prompt-major-principle Prefix Principle Assoc))
	((nsh-given-principle-P Principle) (nsh-prompt-given-principle Prefix Principle Assoc))
	((nsh-Definition-principle-P Principle) (nsh-prompt-definition-principle Prefix Principle Assoc))
	(t (nsh-prompt-minor-principle Prefix Principle Assoc))))
  

;;; Goal principles, unlike the major/minor/given principles found in quantity
;;; problems are not psm-classes or psmtypes they are simply goalprops.  Therefore
;;; we need to prompt them using a separate form.
(defun nsh-prompt-goal-principle (prefix Principle Assoc)
  "Prompt the specified major principle."
  (setq *nsh-last-node* Principle)
  (make-dialog-turn 
   (strcat prefix (nlg (nsh-principle-expression principle) 'goal) ".  ")
   **Explain-More**
   :responder #'(lambda (response)
		  (when (equal response **Explain-More**)
		    (nsh-walk-Node-graph "" principle)))
   :Assoc Assoc))




;;; The following three pompt types all make use of the final 
;;; principle prompting to provide the student with appropriate
;;; hinting as they all follow the same format. They have been 
;;; left in here in the event of my needing to provide specialized
;;; hits for each type.  
(defun nsh-prompt-major-principle (prefix Principle Assoc)
  "Prompt the specified major principle."
  (nsh-prompt-principle-final Prefix Principle Assoc))


(defun nsh-prompt-given-principle (prefix Principle Assoc)
  "Prompt the specified major principle."
  (nsh-prompt-principle-final Prefix Principle Assoc))


(defun nsh-prompt-minor-principle (prefix Principle Assoc)
  "Prompt the specified major principle."
  (nsh-prompt-principle-final Prefix Principle Assoc))


(defun nsh-prompt-definition-principle (prefix Principle Assoc)
  "Prompt the specified definition principle."
  (nsh-prompt-principle-final Prefix Principle Assoc))


(defun nsh-prompt-principle-final (prefix principle Assoc)
  (setq *nsh-last-node* Principle)
  (make-dialog-turn 
   (strcat prefix (nlg (nsh-principle-expression principle) 'psm-exp) ".  ")
   **Explain-More**
   :responder #'(lambda (response)
		  (when (equal response **Explain-More**)
		    (nsh-walk-Node-graph "" principle)))
   :Assoc Assoc))




;;; Tracing prompting
(defun trace-nsh-prompting ()
  (trace nsh-prompt-solution
	 nsh-dialog-prompt-node
	 nsh-prompt-node
	 nsh-prompt-quantity
	 nsh-prompt-parameter
	 nsh-prompt-principle
	 nsh-prompt-goal-principle
	 nsh-prompt-major-principle
	 nsh-prompt-given-principle
	 nsh-prompt-minor-principle
	 nsh-prompt-principle-final))





;;;; ======================== Support functions ===================================
;;;; The functions following this line are general-use code that will be called in
;;;; multiple locations within NSH.

;;; ---------------------------------------------------------------------------------
;;; Return t if at least one of the axes in the solution has been entered or if the
;;; solution has no axes in it at all.
(defun nsh-1solution-axis-enteredp (Solution)
  (let ((Axes (nsh-collect-solution-axes-entries Solution)))
    (or (null Axes) (member-if #'systementry-entered Axes))))
	     


;;; ---------------------------------------------------------------------------------
;;; Collect the axes that the student has drawn.
(defun nsh-collect-student-axes ()
  "Collect the axes that the student has drawn (correct and incorrect)."
  (mapcar #'(lambda (E) (cadr (studententry-prop E)))
	  (remove-if-not #'(lambda (E) (equalp (car (studententry-prop E)) 'draw-axes))
			 *studententries*)))


(defun nsh-collect-correct-student-axes ()
  "Collect the correct axes that the student has drawn."
  (mapcar #'(lambda (E) (cadr (studententry-prop E)))
	  (remove-if-not #'(lambda (E) (and (equalp (car (studententry-prop E)) 'draw-axes)
					    (equalp (studententry-state E) **Correct**)))
			 *studententries*)))

		 
;;;; ============================== Solutions ==================================

;;; ----------------------- Picking the best Solution ---------------------------
;;; Given a set of solutions, this code favors those solutions that have the 
;;; least number of undone principles remaining.  In the event of a tie it 
;;; favors those solutions that have standard axes or where the student has
;;; entered at least one of the axes in the solution.
;;;
;;; Note::  that for solutions that contain no axes whatsoever (nil axes 
;;;  solutions) the axes are ALWAYS done whether or not the student has 
;;;  entered any axes.
(defun nsh-pick-best-solution (&optional (Solutions *nsh-solution-sets*))
  "Pick the best solution."
  (let ((Sols (nsh-pick-least-work-solutions Solutions)))
    (if (= 1 (length Sols)) (car Sols)
      (nsh-pick-best-solution-ad Sols))))

(defun nsh-pick-best-solution-ad (Solutions)
  "Filter best solution candidates by axes done."
  (let ((Sols (nsh-pick-axes-done-solutions Solutions)))
    (if (= 1 (length Sols)) (car Sols)
      (nsh-pick-best-solution-sa (or Sols Solutions)))))

(defun nsh-pick-best-solution-sa (Solutions)
  (let ((Sols (nsh-filter-solutions-by-axis 0 Solutions)))
    (if (= 1 (length Sols)) (car Sols)
      (car (nsh-pick-most-worked-solutions (or Sols Solutions))))))
	

;;; ------------------------------------------------------------------------------
;;; Given a set of solutions select the ones with the least amount of uncompleted
;;; principles remaining.

(defun nsh-pick-least-work-solutions (&optional (Solutions *nsh-solution-sets*))
  "Pick the solution with the least remaining work."
  (let ((best (list (car Solutions))) (count (nsh-Solution-numUndone (car Solutions))) tmp)
    (dolist (S (cdr Solutions))
      (setq tmp (nsh-Solution-numUndone S))
      (cond ((= Tmp Count) (push S Best))
	    ((< Tmp Count) 
	     (setq Best (list S))
	     (setq Count Tmp))))
    Best))



;;; ----------------------------------------------------------------------------
;;; Given a set of solutions pick the ones on which the most principles
;;; have been completed.
(defun nsh-pick-most-worked-solutions (&optional (Solutions *nsh-solution-sets*))
  "Pick the solutions on which the most work has been done."
  (let ((best (list (car Solutions))) (count (nsh-solution-numdone (car Solutions))) tmp)
    (dolist (S (cdr Solutions))
      (setq tmp (nsh-solution-numdone S))
      (cond ((= Tmp Count) (push S Best))
	    ((> Tmp Count) 
	     (setq Best (list S))
	     (setq Count Tmp))))
    Best))


;;; --------------------------------------------------------------------------------
;;; Given a set of solutions pick those for which the student has completed 
;;; at least one of the axes within them.
;;;
;;; Note:: Solutions that contain no axis are always counted as having their 
;;;  axes entered irrespective of what entries the student has made.
(defun nsh-pick-axes-done-solutions (&optional (Solutions *nsh-solution-sets*))
  "Pick solutions on which one of the axes have been drawn."
  (remove-if-not #'nsh-1solution-axis-enteredp Solutions))


;;; --------------------------------------------------------------------------------
;;; Given a set of solutions pick those that contain the specified axis.
(defun nsh-filter-solutions-by-axis (&optional (Axis 0) (Solutions *nsh-solution-sets*))
  "Pick solutions on which one of the axes have been drawn."
  (remove-if-not
   #'(lambda (S) 
       (member Axis (nsh-collect-solution-axes-entries S) 
	       :key #'(lambda (E) (cadr (systementry-prop E)))))
   Solutions))


;;; ---------------------------------------------------------------------------------
;;; Given a list of solutions 
(defun nsh-filter-solutions-by-axes (Axes &optional (Solutions *nsh-solution-sets*))
  "Pick solutions on which one of the axes have been drawn."
  (remove-if-not
   #'(lambda (S) (intersection Axes (nsh-collect-solution-axes S)))
   Solutions))


;;; ---------------------------------------------------------------------------------
;;; Given a set of solutions collect all those that contain at least one of the
;;; proposed first principles.
(defun nsh-filter-solutions-by-fps (Principles &optional (Solutions *nsh-solution-sets*))
  "Remove all solutions that do not contain at least one of the principles."
  (remove-if #'(lambda (S) (not (intersection Principles S))) Solutions))


;;; ------------------------------------------------------------------------------------
;;; Given a set of nodes collect all of the axes drawing entries that appear
;;; within them.
(defun nsh-collect-solution-axes-entries (Solution)
  (remove-duplicates
   (mapcan
    #'(lambda (P)
	(remove-if-not 
	 #'(lambda (E) (equalp (car (systementry-prop E)) 'draw-axes))
	 (nsh-Node-entries P)))
    Solution)))


;;; -------------------------------------------------------------------------------------
;;; Collect all of the individual axes located in each solution.
(defun nsh-collect-solution-axes (Solution)
  (mapcar #'(lambda (E) (cadr (systementry-prop E)))
	  (nsh-collect-solution-axes-entries Solution)))


;;; --------------------------------------------------------------------------------------
;;; Given a solution return t if it contains more than one axis.
(defun nsh-multi-axis-solutionp (Solution)
  "Given a solution return t if it contains more than one axis."
  (< 1 (length (nsh-collect-solution-axes-entries Solution))))




;;; ----------------------------------------------------------------------------
;;; Test to see if the specified solution has been completed or not.
(defun nsh-solution-completed-p (Solution)
  "Return t iff the solution is completed."
  (not (find-if-not #'nsh-Node-completed-p Solution)))



;;; --------------------------- Solution NumUndone -----------------------------
;;; Calculate the number of principles in the solution
;;; that are uncompleted (no paths in psmgraph entered).
(defun nsh-Solution-numUndone (Solution)
  "Calculate the number of uncompleted nodes."
  (let ((R 0))
    (dolist (N Solution)
      (when (not (nsh-Node-completed-P N))
	(incf R)))
    R))


;;; -------------------------- Solution NunDone ---------------------------------
;;; Calculate the number of principles in the solution
;;; that were completed (one path in psmgraph entered).
(defun nsh-Solution-numDone (Solution)
  "Calculate the number of completed nodes."
  (let ((R 0))
    (dolist (N Solution)
      (when (nsh-Node-completed-P N)
	(incf R)))
    R))




(defun trace-nsh-solutions ()
  (trace nsh-pick-best-solution
	 nsh-pick-best-solution-ad
	 nsh-pick-best-solution-sa
	 
	 nsh-pick-least-work-solutions
	 nsh-pick-most-worked-solutions
	 nsh-pick-axes-done-solutions

	 nsh-filter-solutions-by-axis
	 nsh-filter-solutions-by-axes
	 nsh-filter-solutions-by-fps
	 
	 nsh-collect-solution-axes-entries
	 nsh-collect-solution-axes
	 nsh-multi-axis-solutionp
	 nsh-solution-completed-p
	 nsh-solution-numUndone
	 nsh-solution-numDone
	 ))


;;; ========================== Entries ======================================
;;; NSh rarely deals with entries directly but when information is needed from
;;; them that is done here.
(defun nsh-entry-prop (Entry)
  (Systementry-prop Entry))



;;; =========================== Nodes =======================================
;;; It is possible for the quantities as well as principles to contain paths
;;; and entries.  This code generalizes accessing the two to make it possible
;;; to deal with both, as necessary.

(defun nsh-node-graph (Node) 
  "Get the specified node's graph."
  (bgnode-path Node))

;;; Has the specified node been completed?  
;;; Uses the psmg entered values.
(defun nsh-node-completed-p (Node)
  "Return t iff the specified principle has been completed."
  ;;;(psmg-path-enteredp (enode-path principle)))  ;; Doesn't work not sure why yet.
  (path-completedp (bgnode-path Node)))


;;; Has the specified node been completed?  
;;; Uses the psmg entered values.
(defun nsh-node-uncompleted-p (Node)
  "Return t iff the specified principle has been completed."
  ;;;(psmg-path-enteredp (enode-path principle)))  ;; Doesn't work not sure why yet.
  (not (path-completedp (bgnode-path Node))))


;;; Has the student begun working on the node at 
;;; all or is it completely empty.  
(defun nsh-node-started-p (Node)
  "Has the student begiun working on the principle or not?"
  (and (bgnode-entries Node)
       (member-if #'systementry-entered (bgnode-entries Node))))


;;; Has the student begun working on the node at 
;;; all but not completed it?
(defun nsh-node-only-started-p (Node)
  "Has the student begiun working on the principle but not completed it?"
  (and (bgnode-entries Node)
       (member-if #'systementry-entered (bgnode-entries Node))
       (member-if-not #'systementry-entered (bgnode-entries Node))))


;;; Collect the node's entries
(defun nsh-node-entries (Node)
  "Collect the node entries."
  (bgnode-entries Node))


(defun nsh-node-expression (Node)
  (bgnode-exp Node))


(defun trace-nsh-nodes ()
  (trace nsh-node-graph
	 nsh-node-completed-p
	 nsh-node-uncompleted-p
	 nsh-node-started-p
	 nsh-node-only-started-p
	 nsh-node-entries
	 nsh-node-expression))



;;; ====================== quantities =========================

(defun nsh-quantity-p (Q)
  (Qnode-P Q))


(defun nsh-quantity-parameter-p (Q)
  "Get the expression form of a quantity."
  (qnode-parameterp Q))

(defun nsh-quantity-soughtp (Q)
  "Is the specified quantity sought?"
  (qnode-soughtp Q))

(defun nsh-quantity-answer-enteredp (Q)
  "Has the specified quantitry been entered in an answer box?"
  (declare (ignore q))
  nil)

(defun nsh-quantity-expression (Q)
  "Get the expression form of a quantity."
  (qnode-exp Q))

(defun nsh-quantity-principles (Q)
  "Get the principles connected to quantity."
  (qnode-eqns Q))


(defun trace-nsh-quantities ()
  (trace nsh-quantity-p
	 nsh-quantity-parameter-p
	 nsh-quantity-soughtp
	 ;;nsh-answer-enteredp
	 nsh-quantity-expression
	 nsh-quantity-principles))
	 
	


;;; =================== Principles ================================
;;; Because of the way parameters are handled a principle 

;;; nsh-principle-completed?
;;; Has the specified principle been completed?  
;;; Uses the psmg entered values.
(defun nsh-principle-completed-p (principle)
  "Return t iff the specified principle has been completed."
  ;;;(psmg-path-enteredp (enode-path principle)))  ;; Doesn't work not sure why yet.
  (path-completedp (bgnode-path principle)))


;;; nsh-principle-completed?
;;; Has the specified principle been completed?  
;;; Uses the psmg entered values.
(defun nsh-principle-uncompleted-p (principle)
  "Return t iff the specified principle has been completed."
  ;;;(psmg-path-enteredp (enode-path principle)))  ;; Doesn't work not sure why yet.
  (not (path-completedp (enode-path principle))))


;;; nsh-principle-started-p
;;; Has the student begun working on the principle at 
;;; all or is it completely empty.  
(defun nsh-principle-started-p (principle)
  "Has the student begiun working on the principle or not?"
  (and (enode-entries principle)
       (member-if #'systementry-entered (enode-entries principle))))


;;; nsh-principle-only-started-p
;;; Has the student begun working on the principle at 
;;; all but not completed it?
(defun nsh-principle-only-started-p (principle)
  "Has the student begiun working on the principle but not completed it?"
  (and (enode-entries principle)
       (member-if #'systementry-entered (enode-entries principle))
       (member-if-not #'systementry-entered (enode-entries principle))))



;;; Is this a given principle node?
(defun nsh-given-principle-p (node)
  "Return t iff the node is a given principle node."
  (and (enode-p node)
       (let ((class (lookup-expression->psmclass (enode-id Node))))
	 (and Class (equal 'given (psmclass-name Class))))))


;;; Is this a major principle node.
(defun nsh-major-principle-p (node)
  "Return t iff the node is a major principle node."
  (and (enode-p node)
       (let ((class (lookup-expression->psmclass (enode-id Node))))
	 (and Class (equal 'major (psmclass-complexity Class))))))


;;; Is this a Definition principle node.
(defun nsh-definition-principle-p (node)
  "Return t iff the node is a definition principle node."
  (and (enode-p node)
       (let ((class (lookup-expression->psmclass (enode-id Node))))
	 (and Class (equal 'definition (psmclass-complexity Class))))))



;;; Return t if this principle is a goal principle 
;;; (found in no quant problems)
(defun nsh-goal-principle-p (node)
  (and (enode-P Node) (goalprop-exp-p (enode-id Node))))



(defun nsh-principle-p (principle)
  (enode-p principle))


;;; principle graph
(defun nsh-principle-graph (principle)
  (when (nsh-principle-p Principle)
    (enode-path principle)))

(defun nsh-principle-expression (principle)
  (enode-id principle))

(defun nsh-principle-entries (principle)
  (enode-Entries principle))


;;; Lookup the psmclass that matches the principle supplied.
(defun nsh-lookup-principle-class (principle)
  (lookup-psmclass-exp (enode-ID Principle)))


;;; Given a principle collect the quantities within it.
(defun nsh-principle-quantities (Principle)
  "Given a principle collect the quantities within it."
  (enode-qnodes Principle))




(defun trace-nsh-principles ()
  (trace nsh-principle-completed-p
	 nsh-principle-uncompleted-p
	 nsh-principle-started-p
	 nsh-principle-only-started-p
	 nsh-given-principle-p
	 nsh-major-principle-p
	 nsh-goal-principle-p
	 nsh-principle-p
	 nsh-principle-graph
	 nsh-principle-expression
	 nsh-principle-entries
	 nsh-lookup-principle-class
	 nsh-principle-quantities))



;;;; ======================== random text generation code ========================

(defun random-positive-feedback ()
  (random-elt
   '("Good!  "
     "Right.  "
     "Correct.  "
     "Yes.  "
     "Yep.  "
     "That's right.  "
     "Very good.  "
     "Right indeed.  ")))

(defun random-goal-prefix ()
  "Generates phrases that can precede a goal phrase expressed as a gerund"
  (random-elt
   '("Try "
     "Try "
     "You should be "
     "A good step would be "
     "Your goal should be ")))








;;;;============================ psm graph ==================================
;;;; we encounter the psm graphs in three instances.  Firstly when asking if
;;;; a psm is entered.  Secondly when we are traversing the psm path to find
;;;; a set of system entries to hint the student on.  Thirdly when a qnode 
;;;; with a path is encountered.  Because of the nature of this code it is 
;;;; rather closely tied to the strucutre of the paths and will be unlikely 
;;;; to be that distant from the code.
;;;;

;;;; For now the walking is working as-is although we probably want to update it slightly.

;;; ----------------------- walk-psm-graph -------------------------
;;; Once a student has selected a psm or quantity with a path that
;;; we wish to hint them on we will walk the graph to find the 
;;; necessary steps that we want to hint them on.  This is done by 
;;; recursively walking the graph until an undone step is found.  
;;; It has a prefrence for selecting steps on partially completed 
;;; branches if possible.  The system begins with the cdr of the path
;;; as the car of each path is always an sg dummy.
;;;
;;; psm path walking is a recursive process.  At each stage the decision
;;; to traverse the path is made based upon the structure of the graph.
;;; this depends upon the graph structures defined in the psmg file
;;; of helpstructs.
(defun nsh-walk-node-graph (prefix node)
  (walk-psm-path prefix (cdr (nsh-node-graph Node)) nil))

(defun walk-psm-path (prefix path stack)
  (cond ((null path) 
	 (error "Reached end of psm path before finding target entry"))
	((cswm-p (car Path)) (walk-wm-step prefix path stack))          ;; If the step is one of the
	((csop-p (car Path)) (walk-op-step prefix path stack))	        ;; cognitive steps (op, do,  
	((cssg-p (car Path)) (walk-sg-step prefix path stack))          ;; wm, sg) then take the 
	((csdo-p (car Path)) (walk-do-step prefix path stack))          ;; appropriate step action.
	((cssplit-p (car Path)) (walk-split-step prefix (cdr path) stack))   ;; 'SPLIT' (beginning of unordered)   
	((cschoose-p (car Path)) (walk-choose-step prefix (car path) stack)) ;; Choose (alternate paths).	 
	(T (Error "Car of path has illegal syntax"))))                       ;; Or signal a syntax error.


;;; wm steps represent the unification of a goal with something already in
;;; working memory.  The goal that it solves is popped off of the stack and 
;;; the search continues.
(defun walk-wm-step (prefix path stack)
  "Remove the matching goal from the wm and continue searching."
  (walk-psm-path prefix (cdr path) (cdr stack)))


;;; op steps represent the selection of an operator to acheive 
;;; the current goal.  This has no effect upon the search.
(defun walk-op-step (prefix path stack)
  "Work on a op step."
  (walk-psm-path prefix (cdr path) stack))


;;; sg nodes represent the decision to solve a specific goal.  As 
;;; such they will be added to the stack unless they are of a type
;;; that is flagged to be ignored.  
(defun walk-sg-step (prefix path stack)
  "Add a goal to the stack."
  (walk-psm-path 
   prefix (cdr path)
   (cond ((not (sg-ignore-class (car path))) 
	  (cons (car Path) Stack))
	 ((and (eq (car (cssg-goal (car path))) 'setof)
	       (eq (cadr path) 'next))
	  (cons (cssg-goal (car path)) stack))
	 ((and (eq (car (cssg-goal (car path))) 'Any-Member)
	       (listp (cadr path))
	       (eq (caadr path) 'choose))
	  (cons (cssg-goal (car path)) stack))
	 (t Stack))))


;;; kvl changed case statement to member
(defun sg-ignore-class (sg)
  "True if the given sg step should be ignored when pushing goals onto the stack"
  (member (car (cssg-goal sg)) 
	  '(test bind not in-wm debug rdebug setof any-member)))


;;; do steps represent the specific entries to be made.  These are
;;; linked to the systementries that will reflect the students actions.
;;; when the corresponding entry has been entered the system will
;;; continue to search.  When it has not the system will return the
;;; entry to the student.
(defun walk-do-step (prefix path stack)
  "Process a do step that has been made."
  (if (or (null (csdo-entries (car path)))           ;; If there is no entry in this op,
	  (csdo-enteredp (car path)))                ;; or if the step has been entered.
      (walk-psm-path prefix (cdr path) (cdr stack))  ;; pop the sg and continue searching.
    (hint-target-entry prefix (car path) stack)))    ;; Otherwize hint this step.







;;;------------------------ walk-structure-steps ---------------------------
;;; Structural steps such as splits, next, joins and chooses represent 
;;; divergent choices in the system or alternate orderings of steps 
;;; (representing unordered operators.)  The code below deals with splits
;;; nexts, joins and chooses.


;;; Chooses represent exclusive divisions within the search procedure.
;;; When a choose is encountered then the system must choose one of
;;; the branches on which to hint the student.  The prefrence is for
;;; branches that have been partially entered.  (kvl says:) If there
;;; are none then take the first path.  When author's write unordered
;;; preconditions, they should write them in the order they would
;;; prefer to see NSH suggest them in.
;;; AW: this always chooses the first partially entered path it finds.
;;; So if there are two choices for achieving some goal which we think of
;;; as exclusive, and student has done one of them, this can wind up 
;;; hinting the other as well way if that other path happens to be 
;;; partially entered and is found first -- bad! It appears the logic 
;;; to find the most entered path is only applied to splits, not chooses.
(defun walk-choose-step (prefix path stack)
  (walk-psm-path
   prefix
   (or (find-if #'path-partially-enteredp (cdr path))
       (car (cdr path)))
   stack))


;;; Splits, Nexts, and Joins delineate paralell unordered branches
;;; in the path.  They are generated by an operator that has been
;;; marked as unordered.  In this event all of its preconditions 
;;; must still be satisfied but they can be done in any order.  A
;;; Split marks the beginning of the branches.  Nexts mark the 
;;; beginning of each individual branch and the join marks the end
;;; of the unordered portion.
;;;
;;; When a set of paralell branches is encountered the system will
;;; collect each of the individual paralell branches.  If one or
;;; more of the branches contains unfinished dos then the hint
;;; process will continue with one of them.  As with choose nodes
;;; the prefrence is for partially completed branches first.  If
;;; all of the branches have been completed (or contain no entries)
;;; then the search process will continue with the remaining path
;;; after the join node treating the contents of the branches as
;;; done.  The contents of the branches will not be pushed onto the
;;; stack.
(defun walk-split-step (prefix path stack)
  "Walk the split step selecting the path to hint on if any."
  (let ((branches) (remainder))
    (multiple-value-setq (branches remainder)
      (gather-branches path))
    
    (walk-psm-path 
     prefix
     (or (find-if #'path-partially-enteredp branches)
	 (find-if #'path-unenteredp branches)
	 remainder)
     stack)))


;;; Given a path that starts with the first branch of a set of
;;; parallel branches, return a list of parallel branches.  As a side
;;; effect, this resets the special variable remaining-paths to the
;;; portion of the path that follows the join that terminates this set
;;; of parallel branches.  This works by calling gather-branch, which
;;; collects one branch and returns a list of the remaining branches
;;; which will then be recursively gathered.  If that starts with JOIN, 
;;; then we're done and can return the set of branches.  If that 
;;; starts with NEXT, then we need to collect another branch and repeat.
(defun gather-branches (path &optional (branches nil))
  (let ((result))
    (cond ((eq 'NEXT (car path))                        ;; If there are further
	   (setq result (gather-branch (cdr path)))     ;; branches to collect 
	   (gather-branches                             ;; do so and recursively
	    (cadr result)                               ;; call the code with 
	    (cons (car result) branches)))              ;; the result stored.
	  ((eq 'JOIN (car path))                        ;; If a join is encountered
	   (values (reverse branches) (cdr path)))         ;; then we are done.
	  (T (Error "Expected NEXT or JOIN as car of path")))))  ;; give an error if found.
  
  
;;; gathering a branch is a process of cycling through the list collecting
;;; elements from the current branch.  If a split is encountered the search
;;; simply collects every element within it and moves on.  If a choose is 
;;; encountered then the system incorporates it into the branch returning a
;;; branch with the choose node embedded in it.  However the system can exploit
;;; the later properties of the split to ignore the choose for future branches.
(defun gather-branch (path &optional (branch nil) (depth 0))
  (cond ((eq 'JOIN (car path)) 
	 (gather-join-branch path branch depth))

	((eq 'NEXT (car path)) 
	 (gather-next-branch path branch depth))

	((eq 'SPLIT (car path)) 
	 (gather-split-branch path branch depth))
	
	((and (listp path) (eq 'CHOOSE (caar path)))
	 (gather-choose-branch path branch depth))
	
	(t (gather-branch                      ;; Lastly add to the branch and
	    (cdr path)                         ;; recurse.
	    (append branch (list (car path))) 
	    depth))))

;;; Joins signal the end of a split-next-join group.  If we are at the top level
;;; then the system returns the current branch and the remainder of the path.
;;; Otherwize the depth is decremented and the search continues.
(defun gather-join-branch (path branch depth)
  "If depth is 0 return else recurse."
  (if (= 0 depth)
      (list branch path)
    (gather-branch (cdr path) 
		   (append  branch '(join))
		   (- depth 1))))


;;; Nexts signal the end of a signal the beginning of a new paralell branch
;;; This can be the signal for the end of the branch the system is currently
;;; collecting or simply an internal branch.  The system will either return
;;; or recursively continue the search depending upon the depth count.
(defun gather-next-branch (path branch depth)
  "Gather the branch following a next node."
  (if (= 0 depth)
      (list branch path)
    (gather-branch (cdr path) 
		   (append branch '(Next)) 
		   depth)))


;;; When a split is encountered in the search the system will increment the
;;; search depth and continue the search taking in the split node in the process.
(defun gather-split-branch (path branch depth)
  "Gather the branch following a split incrementing the depth."	
  (gather-branch (cdr path)             ;; then increment the depth and continue 
		 (append branch (list (car path)))  ;; the search.
		 (+ 1 depth)))

;;; when a choose branch is encountered we will need to select 
;;; the best of the alternative branches to continue along.  
;;; this is done by selecting the completed path first.  If not 
;;; the 'most-completed' of the entered paths will be selected.
;;; lastly the first of the availible paths will be selected.
(defun gather-choose-branch (path branch depth)
  (gather-branch 
   (choose-best-path (cdar path))
   branch depth))


(defun choose-best-path (paths)
  (let ((best (find-if #'path-completedp paths)))
    (when (null best)
      (setq best (most-entered-path paths)))
    (when (null best)
      (setq best (car paths)))
    best))





;;;---------------------- path-comparison ------------------------
;;; In the process of conducing the path search the system will 
;;; often need to obtain information about the paths themselves.
;;; The code in this section provides that information.
;;;
;;; When tests are made for path contents/entries one of the things
;;; that must be determined is if any entries are present to be 
;;; made in the first place.  Thus the path-unentered? and path-completed?
;;; code both return three values:
;;;    -1 => The path has no entries.
;;;     0 => The path is not (completed v entered)
;;;     1 => The path is (completed v entered.)
;;;
;;; The predicate functions *p such as path-enteredp collapse 
;;; these values into nil and t respectively depending upon the
;;; desired meaning -1 always goes to nil 0 and 1 may flip.
;;; Be aware of this when using them.

;;; A path is unentered if none of the steps within it have
;;; been entered hence this code.  A path containing a choose
;;; is unentered iff no entries have been made in any of its
;;; subpaths.   Apaet from that the system can be tested in
;;; a simple linear manner.
;;; 
;;; The result is -1 if no entries. 0 if none made 1 if entered.
;;; 
;;; count is an incremented number of entries found if 0 it 
;;; signals for a -1 result.
(defun path-entered? (path &optional (count 0))
  (cond ((null path)
	 (cond ((= 0 count) -1)   ;; If no entries were found.
	       ((< 0 count) 0)))  ;; If they have been then 0.
	  
	  ((not (listp (car path)))
	   (path-entered? (cdr path) count))
	  
	  ((eq 'CHOOSE (caar path))
	   (path-entered-choose? (cdar path) count))
	  
	  ((csdo-p (car path))
	   (path-entered-csdo? path count))
	  
	  (t (path-entered? (cdr path) count))))


;;; When the path-entered? search encounters a csdo it tests to see
;;; if the csdo has entries and if so if it has not been entered  
;;; increment the count and recurse.  Otherwize return 1 (path entered.)
(defun path-entered-csdo? (path count)
  "Has the csdo been entered or not?"
  (cond ((null (remove-if #'systementry-implicit-eqnp
			  (csdo-entries (car path))))  ;; If there is no entry
	 (path-entered? (cdr path) count))             ;; then recurse.
	((not (csdo-enteredp (car path)))              ;; If the entry has not been
	 (path-entered? (cdr path) (+ 1 count)))       ;; made then increment and recurse.
	(t 1)))                                        ;; else succeed.


;;; When the psmpath-enteredp encounters a choose then the search splits
;;; each separate path is tested individually.  If none of the paths have
;;; been entered (or if none are supplied then the count is tested.  If
;;; the count is 0 (no entries found above) then -1 (path entry-free) is 
;;; returned.  Else 0 is returned to signal that the path is not entered.
;;; 
;;; If any one of the paths has been entered then a value of 1 is returned
;;; signifying success.  The test searches the paths in order until the first
;;; success is found.  
(defun path-entered-choose? (paths count)
  "Test to see if any of the branches in the choose node has been entered."
  (cond ((null paths) (if (= 0 count) -1 0))            
	((= 1 (path-entered? (car paths) count)) 1)
	(t (path-entered-choose? (cdr paths) count))))




;;; A path is completed when it has entries and none of
;;; those entries are unentered.
(defun path-completedp (path)
  (= 1 (path-completed? path)))


;;; A path is completed iff there exists some route through it
;;; from root to leaf on which all of the entries have been 
;;; entered.  This code uses a simple recursive search to test
;;; employing an or logic at the choose nodes, and if it contains
;;; any nodes to be entered at all hence this code follows the
;;; same conventions as those displayed in the path-entered?
;;; function.
(defun path-completed? (path &optional (count 0))
  (cond ((null path)                          ;; If the path has reached the 
	 (cond ((= 0 count) -1)               ;; then if count is 0 return -1
	       ((< 0 count) 1)))              ;; If count is < 0 return 1.
	
	((not (listp (car path)))             ;; for 'SPLIT' 'NEXT' and 'JOIN'
	 (path-completed? (cdr path) count))  ;; nodes simply move on.
   
	((csdo-p (car path))                  ;; test a do and return 0 if
	 (path-completed-csdo? path count))   ;; it is uncompleted.
	
	((eq 'CHOOSE (caar path))                    ;; For choose nodes split the 
	 (path-completed-choose? (cdar path) count)) ;; search for each branch.
		
	(t (path-completed? (cdr path) count)))) ;; continue the search.

;;; csdo completion is the same as above for path-entered?
;;; At some time I should collapse these two branches into one thing.
(defun path-completed-csdo? (path count)
  "Has the csdo been completed?"
  (cond ((null (remove-if #'systementry-implicit-eqnp
			  (csdo-entries (car path))))  ;; If no entry then move on
	 (path-completed? (cdr path) count))           ;; using the cdr of the path.
	((csdo-enteredp (car path))                    ;; If entered then increment
	 (path-completed? (cdr path) (+ 1 count)))     ;; count and use it.
	(t 0)))                                        ;; else return 0.


;;; Path completion is a matter of splitting the search over the set of
;;; alternate paths.  The code below does just that and distributes the
;;; search for each branch returning when the first completed branch
;;; returns.
(defun path-completed-choose? (paths count)
  (cond ((null paths) (if (= 0 count) -1 0))            
	((= 1 (path-completed? (car paths) count)) 1)
	(t (path-completed-choose? (cdr paths) count))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; path comparison prediactes.
;;; When we want to test for the state of a path then we 
;;; use the predicates all of them collapse a return of -1 
;;; to nil.

;;; A path is partially entered when it has both completed
;;; and uncompleted do nodes.
(defun path-partially-enteredp (path)
  (and (= 1 (path-entered? path))
       (= 0 (path-completed? path))))

;;; A path is defined as entered if it has at least one 
;;; completed step within it.  Therefore this search simply
;;; recurses through the path returning t iff there exists 
;;; some entered step within the path.
(defun path-enteredp (path)
  (= 1 (path-entered? path)))

(defun path-unenteredp (path)
  (= 0 (path-entered? path)))

;;; A path is defined as entry free when it has no entries 
;;; within it anywhere.
(defun path-entry-freep (path)
  (cond ((null path) t)
	
	((and (csdo-p (car path))
	      (null (csdo-entries (car path))))
	 (path-entry-freep (cdr path)))
	
	((not (listp (car path)))
	 (path-entry-freep (cdr path)))
	
	((eq (caar path) 'CHOOSE)
	 (loop for p in (cdar path)
	     always (path-entry-freep p)))
	
	(t (path-entry-freep (cdr path)))))


;;; some uncompleted paths are 'more entered' than others meaning
;;; that the student has done more work on them than they have on
;;; any other path.  This code determines that by counting the 
;;; number of completed steps on the path minus the number of 
;;; unentered values (accounting for choose splits and returning 
;;; the one with the highest value and the value (as return pairs.)
(defun most-entered-path (paths)
  (let ((tmp) (path (car paths))
	(v (entered-path-ratio (car paths))))
    (dolist (P (cdr paths))
      (setq tmp (entered-path-ratio p))
      (when (< v tmp)
	(setq v tmp)
	(setq path p)))
    (values Path v)))


;;; collection of a path's entered ratio is a recursive process
;;; of collecting the number of entered entries and unentered
;;; entries from the path (connected to the csdo's) and obtaining
;;; their ratio.
(defun entered-path-ratio (path &optional (count 0))
  "Get the entered path ratio."
  (cond ((null path) count)
	
	((not (listp (car path)))
	 (entered-path-ratio (cdr path) count))
	
	((eq (caar path) 'CHOOSE)
	 (choose-entered-path-ratio (cdar path) count))
	
	;; At some point this code should take into account null entries for 
	;; testing purposes.  However this causes a bug that I do not yet have
	;; time to fix.  Since everything is weighted evenly -1 by it is does
	;; not affect the logic of the result only its appearence.  The only 
	;; null entry csdo's are the dummies at the end of the path.
	((csdo-p (car path))
	 (cond ((csdo-enteredp (car path))
		(entered-path-ratio (cdr path) (+ 1 count)))
	       (t (entered-path-ratio (cdr path) count ))))
	
	(t (entered-path-ratio (cdr path) count))))
	

(defun choose-entered-path-ratio (paths count)
  "Get the maximal entered path ratio for the chooses."
  (+ count (eval (cons 'max (mapcar #'Entered-path-ratio paths)))))







;;; ====================== hint-target-entry =========================
;;; This generates a sequence of hints leading up to the target entry
;;; itself.  The first few hints come from the stack of goals leading
;;; to the entry.  The remaining hints try to get the student to do
;;; the entry itself.  The prefix is a string generated earlier in
;;; response to the student's action but not yet presented to the
;;; student.  The step is a DO path-node for the target step.  The
;;; stack is a list of SG path-node for the goals that have not yet
;;; been achieve, with top goal last.  Returns a tutor turn.
;;; kvl: Put the prefix only on the first hint in the sequence.
;;;
;;; When the **Print-NSH-Stack** is set to t then the system will print
;;; out a reversed form of the stack so that you can see the hint stack
;;; down to the step from top to bottom.  
(defun hint-target-entry (prefix step stack)
  (when **Print-NSH-Stack** (pprint (reverse Stack)))
  (make-hint-seq  
   (append (collect-stack-hintstrs stack)
	   `((function make-hint-seq 
		       ,(collect-step-hints step) 
		       :prefix ,prefix
		       :OpTail ,(list (csdo-op Step)))))
   :prefix Prefix))




;;; the goal hintstrs returns a list of hint strings for each goal
;;; located in the stack in reverse order.  This sequence of strings 
;;; which have been passed to nlg can be used to generate a hint 
;;; sequence.
(defun collect-stack-hintstrs (stack)
  "Collect the stack hint strings."
  (let ((hints))
    (dolist (g stack)
      (if (goalprop-exp-p (cssg-goal g))
	  (push	`(goal ,(strcat (random-goal-prefix)
				(nlg (cssg-goal g) 'goal))
		       (goal ,(cssg-op g) ,(cssg-goal g)))
		
		hints)
	(format t "NSH: Skipping unhintable goal ~A~2%" (cssg-goal g))))
    hints))




;;; Hinting the step itself involves collecting the hint specs
;;; from the operator and returning the result.
(defun collect-step-hints (step &key (type nil))
  "Collect the hints from an op step."
  (let ((hints (get-op-hints 
		(get-operator-by-tag (csdo-op step))
		(csdo-varvals step))))
    (if (null Type) Hints
      (filter-ophints-by-type Hints Type))))


;;; ------------------------------------------------------------
;;; As a special case on some hints, we want to tell the student
;;; to make a specified entry but only want to give them the 
;;; bottom-out hint, nothing more.  This is the case with the
;;; axis drawing hints and a few others where our goal is to 
;;; make them do something but not, per-se, to worry about the
;;; context of it, or that we have already provided the context.
(defun nsh-bottom-hint-target-entry (Initial step &key Assoc)
  (make-dialog-turn
   Initial **Explain-More**
   :Responder #'(lambda (Response)
		  (when (equal Response **Explain-More**)
		    (let ((source (car (systementry-Sources step))))
		    (make-hint-seq 
		     (collect-step-hints Source :type 'bottom-out)
		     :OpTail (list (csdo-op Source))))))
   
   :Assoc Assoc))



  



;;;; ============================================================================
;;;; Next-call-facility
;;;; It is possible for the help system to specify alternate NSH functions to be
;;;; executed at runtime.  This NSH-next-call facility allows us to prompt the 
;;;; student to make some entry and then alter our subsequent behavior when they
;;;; call NSH again.  This facility was put in initially to aid in the rotation 
;;;; change hints.

(defparameter **nsh-next-call** Nil "The next call to be made for NSH.")


(defun nsh-set-next-call (Func)
  "Set the next Call to be made."
  (setq **nsh-next-call** Func))


(defun nsh-clear-next-call ()
  (setq **nsh-next-call** nil))


(defun nsh-next-call-set? ()
  **nsh-next-call**)


(defun nsh-execute-next-call ()
  (funcall **nsh-next-call**))


(defun trace-nsh-next-call ()
  (trace nsh-set-next-call nsh-clear-next-call
	 nsh-next-call-set? nsh-execute-next-call))



















;;; =================== Trace functions ==========================================

(defun trace-nsh ()
  ;;(trace-nsh-setup)
  (trace-nsh-ask)
  (trace-nsh-done)
  (trace nsh-continue-solutions)
  (trace-nsh-start-principle-free)
  (trace nsh-1solution-axis-enteredp
	 nsh-collect-student-axes)
  (trace-nsh-solutions)
  (trace-nsh-nodes)
  (trace-nsh-quantities)
  (trace-nsh-principles)
  (trace-nsh-next-call))

