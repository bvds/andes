;;; API.cl
;;; Collin Lynch
;;; 4/16/2003
;;; Modifications by Anders Weinstein 2004-2008
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
#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The code in this file provides meta-information about the Andes2 API
;;; the parameters and functions are used to classify the API calls into
;;; categoried by name and to associate features to calls including the 
;;; type of return value and the location (if any) of the id.
;;;
;;; Copies of this file are located in two modules, Help and 
;;; LogProcessing/CmdReader/.  At runtime it will be used by the code in
;;; the CmdReader to generate the appropriate cmd structs from the logfiles
;;; and the code in interface.cl to generate the appropriate cmd structs.
;;; 
;;; -------------------- old comment -----------------------------------
;;; The code in this file is used to process the api calls that come in 
;;; on the dde's and dde-posts.  My goal in loading the code into this
;;; file is to locate most if not all of the api-specific code into a 
;;; single file.  In reality, not all of the knowledge will be here but
;;; a sufficient portion hopefully.  
;;;
;;; This code is loaded by the cmd.cl file and the cmdreader file.  It is
;;; intended for use by those files but not necessarily for general use.  
;;; The predicates within this file are here for parsing and classifying
;;; the individual apis as are the predicates for extracting portions of
;;; them.  They can be accessed by anyone but it might not be a ideal to
;;; do so.  The cmd.cl file provides wrapper functions for accessing the
;;; relavent info as necessary.  
;;;
;;; NOTE:: The functions in this file should not be called directly, 
;;;        instead users should make use of the wrapper functions that
;;;        have been added to cmd.cl and cmdreader.cl  
;;;
;;; NOTE:: This code has been developed by reverse-engineering the APIs
;;;        If older apis varied a great deal it may be necessary to 
;;;        alter this into some version-dependent system.  In that case
;;;        the best bet would be to take the attitude carried out by the
;;;        states and compile the information at read time into the cmd
;;;        for now this is not an issue but it could become so later.
;;;    
|#


;;;; ==============================================================================
;;;; cmd-classes
;;;; in order to control some of the cmd behavior the commands are classified
;;;; into specific types for processing.  These classes define the result type
;;;; to be expected from the dde's and other values.  the functions and params
;;;; in this section define those classes and their relationship to the expected
;;;; result types.
;;;;
;;;; The parameters and functions here are used by the cmdreader code to set the
;;;; type and class values of the resulting cmds.  At runtime, the user should 
;;;; extract the values from the cmd struct directly.  

;;; --------------------------------------------------------------------------
;;; Command types.
;;; this parameter associates the command types to their respective classes 
;;; these classes are then used to deal with result types.  I have placed the 
;;; ontology here because it does not need to be in the kb section.
;;;
;;; The Unknown class will be given special treatment.  These are dde-types that 
;;; I do not have the time to deal with now or cannot determine what they are 
;;; For more info see below.
(defparameter **andes-command-classes**
    '(;; -------------------------------------------------------------
      ;; State
      ;; State calls are used to update or alter the system state and 
      ;; signify values being refrshed/set.  As dde's these calls will
      ;; get a status return-val type
      (State 
       (set-session-id
	read-student-info
	read-problem-info 
	close-problem 
	check-entries
	))
      
      ;; --------------------------------------------------------------
      ;; Autocalc
      ;; There is only one autocalc api.  It is what we use to return
      ;; the score values and as such constiutes a separate class of 
      ;; return value.  This is used for displaying the students scores
      ;; to them at runtime.  
      (Statistics
       (set-stats
	get-stats))
      
      ;; -------------------------------------------------------------
      ;; Noneq-Entry
      ;; non-eq entries such as assert object are dde's that post a 
      ;; value to the system.  As such they get a status return val
      (noneq-entry
       (assert-object
	assert-compound-object 
	lookup-vector 
	lookup-force 
	lookup-torque
	label-angle 
	define-angle-variable
	label-radius 
	define-variable
	assert-x-axis 
	))
      
      
      ;; --------------------------------------------------------------------
      ;; Eqn-Entries 
      ;; lookup-eqn string is our only eqn entry function.  If Async-mode
      ;; is 0 then this will be posted as a dde and get a status-return-val
      ;; if it is 1 then this will be posted as a dde and we will get a 
      ;; EQ-Result as an asynchronous call sometime later.  This distinction
      ;; will be handled by the reader below.  
      ;;
      ;; If async is set to 1 and a lookup-eqn-string appears as a dde then
      ;; the system will throw an error.
      (eq-entry 
       (lookup-eqn-string))
      
      ;; -------------------------------------------------------------------
      ;; Algebra
      ;; The algebra calls are expected to return equations so they will get
      ;; an eqn-result. 
      (Algebra 
       (calculate-equation-string
	solve-for-var
	))
      
      ;; ------------------------------------------------------------------
      ;; Answers 
      ;; Answers deal with final answer submissions either in the answer
      ;; field or a bring up a status-return-va
      (Answer 
       (check-answer
	lookup-mc-answer
	))
      
      ;; -------------------------------------------------------------------
      ;; Deletions (post only no return)
      ;; Deletions remove an entry either equations or nonequations from the 
      ;; screen.  Deletions are distributed as dde-posts and get no return
      ;; value.  If no is found as a dde then an error will be thrown.
      (Delete 
       (delete-object
	))
      
      ;; ------------------------------------------------------------------
      ;; Help 
      ;; help calls are expected to bring back hint responses to they get a
      ;; Hint-return-val.
      (Help
       (handle-student-response
	do-whats-wrong
	get-proc-help
	explain-more
	))      
      ))
     
;;; Note, this is used by the cmdreader and does not need to be directly 
;;;  accessed by the user.  
(defun lookup-command->class (Type &optional (Types **Andes-Command-classes**))
  "Given a command lookup its class."
  (when types (if (member Type (cadar Types)) 
		  (caar Types)
		  (lookup-command->Class Type (cdr Types)))))


;;; ----------------------------------------------------------------------------
;;; Associate the command class with the return type.
;;;
;;; The returntypes are used to drive the dde-resultparser.  The alist here 
;;; associates classes to the returntypes.  The assumption is that if the
;;; returntype is nil then it should be only a post and an error will be
;;; thrown.
;;;
;;; Ignore types will be stored with no parsing. 
(defparameter **Commandclasses->ResultClasses**
    '((State . status-return-val)
      (Statistics . stat-result)
      (noneq-entry . status-return-val)
      (eq-entry . status-return-val)
      (algebra . eqn-result)
      (answer . status-return-val)
      (help . hint-return-val)
      (Unknown . Ignore)))


;;; Note this is accessed by the cmdreader and need not be directly
;;; called by the users.
(defun lookup-commandclass->resultClass (Class)
  "Lookup the resulttype for the supplied class."
  (cdr (assoc Class **commandclasses->resultclasses**)))

