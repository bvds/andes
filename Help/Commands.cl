;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands.lsp/cl -- Lisp functions handling API commands sent by the Andes 
;;  Workbench to the Help System.
;;
;; This corresponds to the "Manager" in the spec. It has one handler function
;; for each API command. These handler functions implement the commands mainly
;; by delegating to worker functions in the relevant modules. This module 
;; also maintains the record of the current dialog state if any for use in 
;; responding to subsequent student input.
;; Copyright (C) 2001 by ?????????????????????????????? -- All Rights Reserved.
;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (CL)  <Collinl@pitt.edu>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;  20 May 2002 - (CL) -- Added in entry-auto-logging and other macros to
;;   permit the creation of the *Studentactions* Stack in future these may
;;   be made a stock part of the system but, for now, it is a compiler setting.
;;   The additions have been made to:
;;     Check-noneq-entries:  Added log-studententry-act
;;     Handle-student-response: Added log-turn-response-act
;;  3/12/2003 -- (Cl) -- Added declaration to lookup-andes-command-type to
;;     suppress special warning.
;;  7/10/2003 -- (CL) -- 
;;     1. Moved the execute-andes-command function to Dispatcher.cl
;;     2. Modified the State apis to return empty green-turns as opposed to the
;;        T value that they were returning previously.  
;;     3. Moved return-turn to Dispatcher.cl
;;  
;;     Lastly I completed reorganizing the commands to clean up the code and
;;     to facilitate the execution process. 
;;
;;  7/30/2003 -- (CL) -- Removing calls to return-turn from funcs.
;;
;; This file contains the API calls that are called by the Andes workbench.  
;; as such it represents the language that the workbench uses to communicate
;; with the help system.  Additional meta-information about the API is located
;; in the file API.cl which is present in the Help module and in the 
;; LogProcessing/CmdReader/ module.  Between these two files we have the 
;; complete communication language for the Andes2 system from help system
;; to workbench and back.  
;;
;; When the student initiates a command on the workbench it will produce an 
;; appropriate api call and then send it through the TCP/IP stream.  Initially
;; these calls were read directly off from the stream by the server code in 
;; Andes2-Main.cl and called using a safe-apply.  Now the calls are passed on 
;; to the execute-andes-command which generates a cmd struct for them and then
;; calls them using the safe-apply function.  
;;
;; It might be feasible to move some of the processing that is done in interface.cl
;; into the functions here but I have chosen to locate it there for semantic
;; purposes.

;; Initially those calls were read directlyu off of the TCP/IP stream by the 
;; server code in Andes2-Main and called using a safe-apply.  Now the calls
;; are passed to the execute-andes-command function located in interface.cl
;; before being called using a safe-apply.  
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :user)





		     
;;;; ============================================================================
;;;; Utility functions.
;;;; These functions are called by many of the api calls located below.


;; general procedure to apply to non-equation entries
;; The entry and call will be logged in *studentactions*
;; along with the result (generated here) which will also 
;; be passed back via return-turn.
(defun handle-non-eq (Entry)
  (let ((Result (check-noneq-entry entry)))
    (log-studentaction **last-api-call** Result Entry)
    Result))





;;; =============================================================================
;;; State API calls.
;;; The do definitions are located in state.cl

;;-------------------------------------------------------------------------------
;; Session ID info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-session-id
;; argument(s): The session ID.
;; returns: Nil for failure color-green for success.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-session-id (SessionID)
  (do-set-session-id :SessionID SessionID))

;;-------------------------------------------------------------------------------
;; Student control info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-student-info
;; argument(s): student name
;; returns: NIL for failure, non-NIL for success
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-student-info (name &optional ConcHelp)
  (do-read-student-info name ConcHelp))

;;-------------------------------------------------------------------------------
;; Problem Control info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-problem-info open a new problem
;; argument(s): problem id
;; returns: NIL for failure, non-NIL for success
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-problem-info (name &optional kb-type (bn-alg 0))
  (do-read-problem-info name kb-type bn-alg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-problem -- close the specified problem 
;; argument(s): 
;;  name: the problem name.
;;  Done:  If t then the student is declaring the problem 'done' and we should 
;;         give them the autograde and voluntary-mastery dialog box.  If not then
;;         we will close the problem as we always have.
;; returns: unused
;; note(s): should be current problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun close-problem (name &optional (Done Nil))
  (do-close-problem name Done))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exit-andes -- end the Andes session
;; argument(s):
;; returns: unused
;; note(s): This sets flag to terminate event processing, which leads to
;; server shutdown when event loop runs.
;;
;; There is no need for a return-turn here because no processing occurs
;; following the exit-andes call.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exit-andes ()
  (do-exit-andes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check-entries -- start or stop loading saved entries.
;; argument:
;;  State: T or nil indicating that the workbench is beginning to or will now
;;    cease sending saved entries to the help system.
;;
;; note(s): This was added to port grading from the the cmdreader to the help
;;  system and will now be used for that purpose.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-entries (State)
  (do-check-entries State))


;;; ============================================================================
;;; Statistics component.
;;; The code in this section is used to query the automatic statistics code 
;;; for student grading.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-stats -- Get the current saved statistics.
;;   This function when called returns a stat-turn containing a space-separated 
;;   string of tuples representing the student's scores.  
;; Argument:
;;  Type:  Either 'scores' or 'all' or 'persist
;;    If 'stats' then the values will be the total list of stats that Andes is
;;       collecting irrespective of their weigths.  
;;    If 'scores' then the result will be the statistics that have had non-zero
;;       weights assigned to them for computation of the total.
;;    If 'persist then only 
;; Result:  A stat-turn containing the result values.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-stats (Type)
  (on-stats-get-stats Type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-stats -- restore specified score values
;;
;; This is used by the workbench to restore persistent scores saved in the
;; the solution file on problem open. The implementation function that does the 
;; work is in HelpStructs/RuntimeTest.cl
;;
;; Argument list for this API call should be a Lisp-readable sequence of pairs 
;; of the form (score value-expr) (score value-expr) (score value-expr) ...
;; Currently the only value types we persist will be either a number for a simple 
;; count or a list of two numbers for a fractional score.  
;; Example command string:
;;
;;   (set-stats (NSH_BO_Call_Count 3) (WWH_BO_Call_Count 2) 
;;          (Correct_Entries_V_Entries (3 5))
;;          (Correct_Answer_Entries_V_Answer_Entries (0 6)))
;;
;; [Quote is not needed in command strings sent from the workbench because they
;; are Lisp read then dispatched by (funcall (first cmd) (rest cmd)) so args are
;; not evaluated, though they must be readable.]
;;
;; Result: normally NIL. No indication of success or failure.
;;
(defun set-stats (&rest score-value-list)
  (set-runtime-test-stats score-value-list))


;;; ============================================================================
;;; NonEq-Entry commands
;;; The APIs in this section handle NonEq entries.
;;;

;;
;; Following entry handling calls are ordered in roughly the order the student 
;; should follow: First choose body, then draw vectors and other diagram 
;; entries, maybe define some variables, draw axes, maybe draw components, 
;; write equations, solve for desired variable and enter answer. 
;; Help requests and generic entry management stuff is separated.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-object -- checks the correctness of a student defined single body
;; argument(s):
;;  label:  the label of the body
;;  name(s): the name the body(s) was assigned in the problem description
;;  time:  retained for backward compatability
;;  id: is assigned to this object by the work-bench
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  marks the corresponding system entry as "entered". defines a mass variable
;;  whose name has "m" concatenated to the given label. Enters into the symbol
;;  table this name paired with the system's name for the same quantity.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-object (label name &optional time id)
  (handle-non-eq (on-assert-object label name time id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-compound-object - checks the correctness of a student defined com-
;;  pound body
;; argument(s):
;;  label: the label of the body
;;  name(s): the name the body(s) was assigned in the problem description
;;  time:  retained for backward compatability
;;  id: is assigned to this object by the work-bench
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  marks the corresponding system entry as "entered". defines a mass variable
;;  whose name has "m" concatenateded to the given label. Enters into the sym-
;;  bol table this name paired with the system's name for the same quantity.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-compound-object (label names &optional time id)
  (handle-non-eq (on-assert-compound-object label names time id)))

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
;;  time: the time period during which the vector is constant. if nil and 
;;    system is a student defined system, the time will be taken from 
;;     the system definition
;;  id: id assigned to vector by the workbench
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  if the vector is correct, the help system marks the corresponding system
;;  entry as "entered", defines the magnitude and direction variables, and
;;  enters the variables in the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup-vector (label avg-inst type system dir mag &optional time id)
  (handle-non-eq 
     (on-lookup-vector label avg-inst type system dir mag time id)))


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup-line (label body dir mag &optional time id)
  (handle-non-eq 
     (on-lookup-line label body dir mag time id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;   if the force is correct the the help system marks the corresponding sys-
;;   tem entry as "entered" it also defines magnitude and direction variables
;;   for the force, and enters them into the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup-force (label type system agent dir mag &optional time id)
  (handle-non-eq 
    (on-lookup-force label type system agent dir mag time id)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-torque - check correctness of a torque vector drawn by student
;; Arguments:
;; label: the torque label
;; net: T if this is net torque, else NIL
;; body: Net torque: the label of the whole torqued body. 
;;       Individual torque: the point of application of the force
;; axis: the pt about which the rotation axis is located
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
(defun lookup-torque (label net body axis dir mag time id)
 (handle-non-eq  
   (on-lookup-torque label net body axis dir mag time id)))

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
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  adds angle label to the list of variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun label-angle (label degrees id-vector1 id-vector2 id-angle &optional axis)
 (handle-non-eq
  (on-label-angle label degrees id-vector1 id-vector2 id-angle axis)))

;;
;; define-angle-variable: undocumented Andes1 API used on defining angle 
;;                        variable without drawing it
;; Full description to be added.
;;
(defun define-angle-variable (label degrees label1 label2 id-angle)
 (handle-non-eq
  (on-define-angle-variable label degrees label1 label2 id-angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; label-radius -- label radius of revolution of an object moving in a circle
;; argument(s):
;;  label: the label given to the radius by the student
;;  id: the id of the radius object
;;  name: the body that is moving in a circle with this radius
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  add a variable for the radius to the list of variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun label-radius (label id name)
 (handle-non-eq (on-label-radius label id name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-variable - define a variable to stand for a certain quantity. this is
;;  called when the student uses the "variable" menu, but not when variables 
;;  are defined by other tools such as the force drawing tool or the body tool.
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
;;  directionp: t if the quantity is a direction; nil otherwise
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  if the variable definition is correct, marks the corresponding system entry
;;  as "entered" and enters the student's variable name into the symbol table
;;  paired with the corresponding system variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun define-variable (var type quant body time agent id &optional directionp)
  (handle-non-eq
   (on-define-variable var type quant body time agent id directionp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assert-x-axis - check correctness of coordinate axis drawn by student
;; argument(s):
;;  body: the label given to the body the axis applies to
;;  dir: the angle of the x-axis form horizontal (0 -> 360)
;;  id: is assigned to the object by the workbench
;;  x-label: label given to x axis by the student
;;  y-label: label given to y axis by the student
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  adds x and y axes to (student entries) -- asserts observed to assessor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun assert-x-axis (body dir
		      &optional id (x-label "x") (y-label "y") (z-label "z"))
 (handle-non-eq
   (on-assert-x-axis body dir id x-label y-label z-label)))

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
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  if the component is correct, marks the corresponding system entry as "en-
;;  tered", figures out which algebraic function of the system's component var-
;;  iable corresponds to this variable given the student's axis rotation, and
;;  enters into the symbol table a pairing consisting of that expression and the
;;  student's variable name.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup-component (label compo-of axis mag &optional dir id)
 (handle-non-eq
  (on-lookup-component label compo-of axis mag dir id)))




;;; ===========================================================================
;;; Eqn-entry
;;; Equation entry commands.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-eqn-string -- check correctness of a student equation entry
;; argument(s):
;;  eqn-string: the equation as the student entered is
;;  id: the slot the workbench holds the students input in (0 based indexing)
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  This is a hack-ish way to get the assoc value but (for now), it works.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lookup-eqn-string (eqn-string &optional id)
  (let ((result (do-lookup-eqn-string eqn-string id)))
    (log-studentaction **last-api-call** result (car *studententries*))
    Result))





;;; ===========================================================================
;;; Algebra API calls.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate-equation-string -- takes an equation, substitutes all known values
;;  that have been entered by the student into it and the simplifies the result.
;;  returns the simplified string. lookup-eqn-string will be called on the ori-
;;  ginal equation before calling this function to update the correct/incorrect
;;  status. Substitutions will anly be done for *correct* values that the stu-
;;  dent has entered. therfore the correct/incorrect status of the resulting
;;  equation will be the same as the original equation (ie. if the equation be-
;;  ing substituted into is correct, the result will be correct as well.
;; argument(s):
;;  string: the equation string to be simplified into
;;  new-id: the id of the new equation to be entered
;; returns:
;;  the string representing the simplified equation if it could be simplified,
;;  otherwise nil
;; note(s):
;;  creates a new equation entry representing the newly simplified equation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eqn-match (s1 s2) 
    (equal (trim-eqn s1) (trim-eqn s2)))

(defun find-eqn-entry (eqn-str) 
  (find eqn-str *StudentEntries* :key #'StudentEntry-Verbatim
		                 :test #'eqn-match))

#|;;; Note, I have modified this from its original form (here in the comment)
  ;;; To the form below for the purposes of dealing with *studentactions* logging.
  (defun calculate-equation-string (eqn-str new-id)
  ;; need to map equation string to slot number
  (let ((eqn-entry (find-eqn-entry eqn-str)))
  
  (unless eqn-entry 
  (return-from calculate-equation-string "!Internal error: entry for equation not found!"))
  (unless (equal (StudentEntry-state eqn-entry) 'Correct)
  (return-from calculate-equation-string "!Only correct equations may be simplified."))
  (let ((result (solver-eqn-simplify (StudentEntry-id eqn-entry) new-id)))
  (cond ; result may be equation s-expr, NIL or error message string 
  ((and result (listp result)) ; an equation
  ;; just return eqn text until appropriate turns are implemented
  (let* ((studEqn  (subst-student-vars (pre2in result)))
  ;; suppress *print-pretty* since it could insert newlines 
  ;; into long result, and WB requires single-line eqn string
  (infixStr (write-to-string studEqn :pretty NIL :escape NIL))
  ;; strip outer parens from equation string
  (studText (subseq infixStr 1 (- (length infixStr) 1)))) 
  ;; save final result as if it were a new student entry. We need to 
  ;; remember slot is occupied for add-entry to trigger automatic
  ;; cleanup of equation in algebra on new entry.
  (add-entry (make-StudentEntry :id new-id
  :verbatim studText
  :prop `(eqn ,studText)
  :parsedEqn result
  :state **Correct**))
  ;; finally return student equation 
  studText
  ))
  ((stringp result) ; error message
  (format NIL "!Unable to simplify ~A: ~A" eqn-str result))
  (T (format NIL "!Unable to simplify ~A." eqn-str))))))
|#

(defun calculate-equation-string (eqn-str new-id)
 ;; No longer needed. (return-turn 
  ;; need to map equation string to slot number
  (let ((eqn-entry (find-eqn-entry eqn-str))
	;; Log the studentaction (this will be updated later)
	(action (log-studentaction **last-api-call**)))
    
    (cond 
     ;; If no mathching entry can be found then set the action and return an error.
     ((null eqn-entry)
      (setf (studentaction-result action) 'error)
      (setf (studentaction-assoc action) "Internal error: entry for equation not found!")
      (make-eqn-failure-turn "Internal error: entry for equation not found!"))
     
     ;; If the selected equation is not correct then send an error. 
     ((not (equal (StudentEntry-state eqn-entry) 'correct))
      (setf (studentaction-result action) 'error)
      (setf (studentaction-assoc action) "Only correct equations may be simplified.")
      (make-eqn-failure-turn "Only correct equations may be simplified."))

     (t (calculate-equation-string-internal eqn-str new-id eqn-entry action)))))


;;; If we have gotten to this point then the student's equation exists and
;;; is correct.  Therefore, the equation computation proceeds as normal and
;;; either an entry is produced and entered, or the system produces a runtime
;;; error.
(defun calculate-equation-string-internal (eqn-str new-id eqn-entry action)
  (let ((result (solver-eqn-simplify (StudentEntry-id eqn-entry) new-id)))
    (cond  ;; result may be equation s-expr, NIL or error message string
     
     ((and result (listp result)) ;; If the result is valid, then we want to generate an entry and store it.
      (calculate-equation-string-success result new-id action))
     
     ((stringp result) ;; Else if the result is a string then we need to deal with it.
      (calculate-equation-string-int-err eqn-str result action))

     ;; Else we have a generic error and need to deal with it.
     (T (calculate-equation-string-int eqn-str action)))))


;; Given a string error signal it to the student and return.
(defun calculate-equation-string-int-err (eqn-str result action)
  (let ((error (format NIL "Unable to simplify ~A: ~A" eqn-str result)))
    (setf (studentaction-result action) 'error)
    (setf (studentaction-assoc action) error)
    (make-eqn-failure-turn error)))


;; Given a generic error log it and signal it to the student.
(defun calculate-equation-string-int (eqn-str action)
  (let ((error (format NIL "Unable to simplify ~A" eqn-str)))
    (setf (studentaction-result action) 'error)
    (setf (studentaction-assoc action) error)
    (make-eqn-failure-turn error)))
     

;; In the event of this being a successful simplification we need to 
;; generate and store the equation entry and then go through the process
;; of storing the entry and then reporting the equation string back to 
;; the workbench.
(defun calculate-equation-string-success (result new-id action)
  ;; just return eqn text until appropriate turns are implemented
  (let* ((studEqn  (subst-student-vars (pre2in result)))
	 ;; suppress *print-pretty* since it could insert newlines 
	 ;; into long result, and WB requires single-line eqn string
	 (infixStr (write-to-string studEqn :pretty NIL :escape NIL))
	 ;; strip outer parens from equation string
	 (studText (subseq infixStr 1 (- (length infixStr) 1)))
	 ;; Generate the new studententry that we will add to the list.
	 (entry (make-StudentEntry :id new-id
				   :verbatim studText
				   :prop `(eqn ,studText)
				   :parsedEqn result
				   :state **Correct**)))
    
    ;; Store the resulting entry/result equation in studentactions
    ;; for later, and then update entry list and the results.
    (setf (studentaction-result action) studtext)
    (setf (studentaction-assoc action) entry)
    
    ;; save final result as if it were a new student entry. We need to 
    ;; remember slot is occupied for add-entry to trigger automatic
    ;; cleanup of equation in algebra on new entry.
    (add-entry entry)
    ;; finally return student equation turn
    (make-eqn-turn studText)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; solve-for-var -- solve for the given var using the equations the student has
;;  entered so far. Only uses correct equations, so the result must also be
;;  correct
;; argument(s):
;;  var: the variable to solve for a string
;;  new-id: the id number of the equation window in which to put the new sim-
;;    plified equation.
;; returns:
;;  a string representing the equation to be put into the new id-slot and op-
;;  tionally a message to be displayed in the hint window. the equation gives
;;  the value of the var if possible, otherwise it gives an equation with all
;;  other known values substituded in.
;; note(s):
;;  adds the new equation to the list of entered equations, and the value of the
;;  variable, if found, to the value of the corresponding scalar/magnitude.
;;
;; It is also designed to log the result of the call for future use.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun solve-for-var (var new-id)
  (let* ((tmp (student-to-canonical var))
	 (result (if tmp (solver-power-solve 31 (student-to-canonical var) new-id) nil))
	 (action (log-studentaction **last-api-call**)))
    
    (cond ((and result (listp result)) (solve-for-var-success new-id result action))
	  ((stringp result) 
	   (solve-for-var-error 
	    (format NIL "Unable to solve for ~A: ~A" var result) action))
	  (t (solve-for-var-error
	      (get-failure-to-solve-hint var)  ; implemented in next-step-help.cl
	      action)))))

(defun solve-for-var-success (new-id result action)
  (let* ((studEqn  (subst-student-vars (pre2in result)))
	 ;; suppress *print-pretty* since it could insert newlines 
	 ;; into long result, and WB requires single-line eqn string
	 (infixStr (write-to-string studEqn :pretty NIL :escape NIL))
	 ;; strip outer parens from equation string
	 (studText (subseq infixStr 1 (- (length infixStr) 1)))
	 ;; Generate the studententry
	 (entry (make-StudentEntry :id new-id
				   :verbatim studText
				   :prop `(eqn ,studText)
				   :parsedEqn result
				   :state **Correct**)))
    
    ;; Store the result in the action
    (setf (studentaction-result action) studText)
    (setf (studentaction-assoc action) entry)
    
    ;; save final result as if it were a new student entry. We need to 
    ;; remember slot is occupied for add-entry to trigger automatic
    ;; cleanup of equation in algebra on new entry.
    (add-entry entry)
    ;; finally return student equation turn
    (make-eqn-turn studText)))


(defun solve-for-var-error (error action)
  ;; Store the result in the action
  (setf (studentaction-result action) 'error)
  (setf (studentaction-assoc action) error)
  ;; and return it
  (make-eqn-failure-turn error))






;;; =============================================================================
;;; Answer API Calls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-answer -- lookup a students answer in the answer box
;; argument(s):
;;  answer: the contents of the answer field the student entered
;;  answer-id: the author-defined id for the answer field. must start with the
;;    string "ANSWER"
;; returns:
;;  entry status return value -- see end of code for description of this
;; note(s):
;;  This uses the same hack on lookup-eqn-string to obtain the entry.  It works
;;  but it ain't exactly clean.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-answer (answer answer-id)
  ; for Skatz experiment, and maybe generally:
  ; remember done state before checking answer to detect event of
  ; "finishing" problem.
  (let ((was-done (all-answers-done-p))
        (result (Do-Check-Answer answer (get-answer-quant answer-id) answer-id)))
    (when (and (not was-done)
               (all-answers-done-p)    ; is now done
	       (not-curr-checking-problemp)) ; ignore if in initial entry check
       (add-followup-if-needed result))

    (log-studentaction **last-api-call** result (car *Studententries*))
    Result))

; display followup dialog in browser on done for certain problems in 
; Skatz experiment
(defvar *followup-problems* '(e1b e2a e2c e4a e5a e7a e8b e9b e10a))

(defun add-followup-if-needed (result-turn)
  (when (and (member (problem-name *cp*) *followup-problems*)
	     (equal (type-of result-turn) 'Turn))
     ; Add show lesson command to result turn. Gross hackery.
     ; We have to change existing result turn (presumably type dialog turn
     ; with color green and no message) into a minilesson turn with the 
     ; appropriate URL (and color) in order to get the command piggybacked
     ; on the returned color.  If we used an async command to do it, it would
     ; enter mode before color result is returned, I think.
     ; !!! Should verify we aren't clobbering an existing command
     (setf (turn-type result-turn) **Minil-Turn**)
     (setf (turn-text result-turn)
       (format NIL "http://136.142.94.84/cgi-bin/navalkcds?user=~a;prob=~a"
                   (student-name) (problem-name *cp*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lookup-mc-answer
;; Argument(s):
;;   ID:  The MC answer ID e.g. "Done-1" being selected.
;;   Value: The value 1 if checked 0 if not.
;;
;; returns:  Entry status return value.
;;
;; Lookup-mc-answer is sent for multiple choice answers.  This is used for both 
;; the n-way multiple choice questions of the type in faa1 where the student is
;; selecting from among a set of choices.  It is also used in the fbd-only 
;; problems and other problems as an "I am done" button.  
;;
;; At present the code below will handle the two cases by passing them to two 
;; distinct functions.  This will be done by splitting on the form of the id.
;; if the ID is "Answer-##" then we will assume that this is a no-quant problem
;; with an "I am done" button and will handle it accordingly.  If the answer is
;; "MC-##" then we will assume that this is a multiple-choice answer box and 
;; will handle it accordingly.  
;;
;; The lookup-mc-answer code is used in two cases.  In the former case we are 
;; dealing with non-quantity seeking problems.  In that case we are using the
;; mc-answer to an "I am done" button on the workbench.  In this case no entry
;; will be generated and a tutor turn will be returned directly.
;;
;; In the latter case we are dealing with a true multiple choice problem where 
;; the student is selecting one of the choices in a set.  In this case we will
;; produce a special non-equation multiple-choice entry.  
;;
;; The code below identifies the case that we are in and then calls the 
;; appropriate handler code in entry-api.  
(defun lookup-mc-answer (ID Value)
  (let* ((Result)
	 (IDStr (format Nil "~a" ID))
	 (pos (position #\- IDStr))
	 (IDPref (subseq IDStr 0 Pos))
	 (IDEnd (Subseq IDStr (+ 1 Pos))))
	 
    (cond
     ;; Handle the Answer case by simply dealing with it directly.
     ((string-equal IDPref "Answer") 
      (setq Result (do-check-mc-no-quant-done-answer ID Value))
      (log-studentaction **last-api-call** Result)
      Result)
     
     ;; Handle the multiple choice case by generating an entry and then
     ;; handling it like any other.
     ((string-equal IDPref "MC") 
      (handle-non-eq (do-check-mc-multiple-choice-answer ID Value)))

     ;; In the event that an unrecognized type is supplied handle it like so.
     (t (error "Unrecognized mc-answer entry supplied: ~a ~a" ID Value)))))



;;; =============================================================================
;;; Delete types
;;; Deletions are kept separate because of the

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-object -- delete the student defined object with the given label and
;;  or id from the student entries list
;; argument(s):
;;  label: the label given to the object by the student
;;  id: the unique is assigned to the object by the workbench
;; returns:
;;  the newe student-entries list. return value is ignored by the workbench
;; note(s):
;;  marks the corresponding system entries as "unentered" If the object involves
;;  a variable, the removes that variable from the symbol table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-object (label &optional id)
  (let ((entry (find-entry id))
	(Result (on-delete-object label id)))
    (log-studentaction **last-api-call** result Entry)
    Result))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-equation - deletes a student equation from the student-entries list
;; argument(s):
;;  id: the id of the equation being deleted. Integer id starts at 0 for first
;;    equation box.
;; returns:
;;   the new student-equations list. return value is ignored by the workbench
;; note(s):
;;  removes this student ewntry from the entered-by fields of each of the cor-
;;  responding system equations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-equation (id)
  (let ((entry (find-entry id))
	(result (on-delete-equation id))) 
    (log-studentaction **last-api-call** result Entry)
    result))






;;; ===========================================================================
;;; Help API Types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-student-response -- dispatch student dialog response to responder 
;;                            set in most recent tutor turn.
;; Returns: result string to be sent to workbench
;; Side-effect: updates last turn via return-turn.
;;
;; A responder function should take a single response code argument and return 
;; the next tutor turn. Reponse codes are not examined here but just passed 
;; through as a magic cookie.  The response codes must be defined by the 
;; protocol with the workbench so that it identifies the response chosen by 
;; the student for the purposes of the responder functions.
(defun handle-student-response (response-code)
  (cond ((and (atom response-code) (sym-match response-code 'cancel))
	 ;; student cancelled out of dialog sequence (must be next-step-help).
	 ;; give null response, but *don't* don't clobber saved turn with
	 ;; return-turn. subsequent next-step help resumes dialog (not done yet).
	 (log-turn-response-studentaction Response-Code (make-noop-turn)))

	;; else If we have a previous tutor turn, and a responder function
	((and *last-tutor-turn* (turn-responder *last-tutor-turn*))
	 ;; dispatch the response to the responder function, logging it 
	 ;; if necessary in the acts, and then return the turn setting the
	 ;; *last-tutor-turn value in the process.
	 (log-turn-response-studentaction 
	   Response-Code 
	   (apply (turn-responder *last-tutor-turn*) 
		  (list response-code))))

	;; else no responder!
	(T (log-turn-response-studentaction
	      Response-code
	      (make-dialog-turn 
	       (format NIL "An internal error occurred: no response found for ~A" 
		       response-code)
	       NIL)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-dialog-response:  API invoked on submission of free-text response
;;                       to a dialog, presumably inside a kcd
;; response-text is string containing student's response.
(defun get-dialog-response (response-text)
  ; just delegate to responder function, should be set by kcd turn
  (handle-student-response response-text))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; why-wrong-object -- get a message explaining why an entry was judged to be
;;  incorrect. the message is displayed in the workbench window. the messages
;;  for why-wrong-object are created when the object is first entered, so all
;;  this function has to do is find the string and return it.
;; argument(s):
;;  label: the label (may not be unique) given to the object by the student
;;  id: the unique id assigned to the object by the workbench
;; returns:
;;  HintSpec -- see end of this file for further description
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun why-wrong-object (&optional label id)
  (declare (ignore label))
  (let ((result (do-whats-wrong id)))
    (log-studentaction 
     **last-api-call** Result
     (if (and Result (turn-p result)) (turn-assoc result)))
    Result))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; why-wrong-equation - get a message explaining why the equation entry is not
;;  correct. unlike with diagram objects, computing the what's wrong is done
;;  when why-wrong-equation is called, rather than when the equation is entered,
;;  since it can take a few seconds to complete
;; argument(s):
;;  id: the number of the equation in the 0 based equation array
;; returns:
;;  HintSpec -- see end of this file for further description
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun why-wrong-equation (id)
  (let ((result (do-whats-wrong id)))
    (log-studentaction 
     **last-api-call** Result
     (if (and Result (turn-p result)) (turn-assoc result)))
    Result))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cancel-help-system -- tells the system has cancelled the latest pending help
;;  request
;; argument(s):
;;   NONE
;; returns:
;;   T (ignored by the WB)
;; note(s):
;;  sets global variale *command-cancelled* to true, which causes some loops in
;;  the help system to exit immediately
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cancel-help-request ()
  (error-message "not implemented"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-proc-help -- Begins the next step help process for the student/
;; argument(s):
;;  NONE
;; returns:
;;  Turn -- A tutor turn that 
;; note(s):
;;  updates the assessor bayesioan to reflect the fact that the student has re-
;;  cieved this hint  Also updates the *studentactions* log if that is set.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-proc-help ()
  (let ((result (next-step-help)))
    (log-studentaction 
     **last-api-call** result
     (if (and Result (turn-p result)) (turn-assoc result)))
    Result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explain-more triggered when the studfent clicks on "explain further". gives
;;  more specific information about the last hint that was given. 
;; argument(s):
;;  NONE
;; returns:
;;  HintSpec -- see end of this file for further description
;; Notes: now delegates to the generic "handle-student-response" routine.
;; In the future, the workbench may call that directly; for backwards compatibility
;; with Andes1 protocol, we handle this old API here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun explain-more ()
  (handle-student-response 'explain-more))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hint-next-substep - triggered when the student clicks on "how do I do that?"
;;  finds the next substep in the goal tree after the last hint and produces a
;;  hint about that substep.
;; argument(s):
;;  NONE
;; returns:
;;  HintSpec -- see end of this file for further description
;; note(s):
;;  updates the assessor bayesian network to reflect the fact that the student
;;  has received this hint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hint-next-substep ()
  (error-message "not implemented"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; why - gets an explanation of the last hint it gave, based on the rule that is
;;  identifed with the hint in the solution graph. The explanations are canned
;;  text that are stroed in the rules.lsp kb file
;; argument(s):
;;  NONE
;; returns:
;;  HintSpec -- see end of this file for further description
;; note(s):
;;  updates the assessor bayesian network to reflect the fact that the student
;;  has received this hint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun why ()
  (error-message "not implemented"))

;;
;; history file queries -- API calls to give workbench access to the student.dat
;; history file. These apply to current student, so must have initialized first
;; Note "get" on unset values return NIL.
;;
(defun history-get (key)
   (studentfile-ask key))

(defun history-set (key value)
   (studentfile-tell key value))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Status Return Values are 3+ field strings of the form:
;;   StatusCode;ErrorList!Command  where
;;  StatusCode -> T | NIL | <nada>
;;  ErrorList -> SlotName | SlotName;MoreErrors | <nada>
;;  MoreErrors -> SlotName | SlotName;MoreErrors
;;  Command -> WBCommand | <nada>
;;  SlotName -> name -- student's label
;;              body -- principal body
;;              time -- time on which defined
;;              type -- qualified when required:
;;                  force - force type
;;                  vector - average or instantaneous
;;                  energy - total, kinetic, or grav-potential
;;              spring
;;              agent -- second body where required
;;                  force -- force agent
;;                  variable - second body in some defs, e.g. distance between
;;              dir -- vector - direction
;;              zdir -- vector - z-axis direction (only in rotational problems)
;;              ang -- vector - angular or linear (only in rotational problems)
;;              side1 - first argument of angle dialog (Angle Dialog)
;;              side2 - second side argument (Angle Dialog)
;;              bodies - list of bodies include in system (System Dialog)
;;  WBCommand -> show-hint <HintSpec> -- show a hint in the tutor mesg pane
;;               show-lesson lesson-file-name -- show an ANDES min-lesson
;;               open-browser URL -- open a lesson viewer on any URL
;;               close-browser -- close lesson viewer if open
;;               msg <message text> -- show text in a dialog box
;;               training-card card-id -- pop up a training card
;;               show-demo demo-file-name -- play a log file as a demo script
;;  HintSpec -> MessageBody[Flags]
;;  Flags -> Flag | Flag Flags
;;  Flag -> w -- why -- continuation (why)
;;          e -- explain further -- continuation (explain)
;;          h -- how do I do that -- continuation (hint-next-substep)
;;          ? -- <free text response> -- continuation (get-dialog-response str)
;;  MessageBody ->
;;          free form text with crude hypertext facilities
;;          (\d text) -- RTF-like tags text as definition
;;          (\v text) -- hidden text
;;          (\h text) -- callback to help-system
;;                     (\h "What's wrong?") (\v what-is-wrong) calls help
;;                     function what-is-wrong if the link "What's wrong?" is
;;                     selected.
;;          Support for Greek alphabet names (ie $a short for greek alpha char)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file commands.lsp/cl
;; Copyright (C) 2001 by ??????????????????????????????? -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
