;;; RuntimeTest.cl
;;; Collin Lynch
;;; 8/11/2003
;;; Modifications by Anders Weinstein 2003-2008
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
;;;
;;; This file defines the runtime-test struct and the code that 
;;; is used to load and store them.  The runtime-test struct is 
;;; used to represent the statistical tests that are used for 
;;; autograding at runtime.  These tests are used to calculate 
;;; the students behavior.  The definitions can be found in 
;;; KB/runtimetests.cl.  The Process code is located in 
;;; help/Statistics.cl.
;;;
;;; The tests themselves will be loaded and executed at runtime.
;;; When stored they will be loaded by means of a custom reader 
;;; defined below.  


;;;; ========================================================================
;;;; Parameters.  
;;;; The following parameters are used to store the runtime tests and the 
;;;; scores for later use.

;;; The list of runtime tests constitutes a single list of the test functions
;;; that are to be updated during this run of Andes.  These will have to be 
;;; loaded at runtime and used by the execution and update code.  
(defparameter *Runtime-Testset* () "The runtime autocalc tests.")
(defparameter *global-Runtime-Testset* () "The runtime autocalc tests.")

;;; The list of grade-testsets consists of only those testsets that have 
;;; nonzero weight and are used to compute the student's runtime score. 
;;; This list is maintained to prevent excess searching of the full testset
;;; when we are obtaining grades.
(defparameter *Runtime-Score-Testset* () "The Scored autocalc tests.")

;;; In some cases it is necessary to execute some functions before the tests
;;; themselves are setup.  Such preparatory work is often used to cache 
;;; data or carry out other steps.  When the runtime tests are reset then
;;; any functions listed in this parameter (see the registration func below)
;;; will be called first.  
(defparameter *runtime-testset-prep-funcs* NIL "The preparatory funcs.")


;;; For many problems we will have several tests that have solution-specific
;;; values.  That is, depending upon what solution we are considering the
;;; total score is different.  In order to Facilitate this this parameter
;;; will store an index to the currently chosen solution (by number).  This
;;; index should be updated every time that the score changes.  The rubric
;;; that we use is one where the highest-score is used.  Thus this will be
;;; changed after each score update to point to the one with the highest 
;;; score value.
(defparameter *Runtime-testset-current-Solindex* 0 "The current Runtime score val.")


;;; Because it will be necessary to compute the total score each time that 
;;; the tests are updated or reset I will cache the value here in order to
;;; avoid unnecessary repetition.  When the score is requested for return 
;;; it will be taken from here.
(defparameter *Runtime-Testset-current-total-score* 0 "The current total score.")


;;;; =======================================================================
;;;; Test Struct
;;;; This struct encodes the information about the runtime tests.  It is used
;;;; to organize the tests for processing at runtime and to make it possible
;;;; for the tests to be traded around.  
;;;;
;;;; In the future this struct and any processing code associated directly to
;;;; it will be moved to HelpStructs

(defstruct runtime-test ;;(:Print-function print-runtime-test))
  name     ;; A symbolic id for the test.
  printstr ;; A nicely formatted string for the printout. (no spaces at present).
  func     ;; The single-arg function that will be evaluated at runtime.
           ;; this func takes the current value as arg and produces a new one.
  CurrVal  ;; The current test value.
  InitFunc ;; An optional zero-argument function that will generate an initial
           ;; value for each problem instance. If this exists then it will be
           ;; funcalled to set the CurrVal at the start of each new problem.
  InitVal  ;; The initial (nil) value for the score.  If InitFunc is nil then 
           ;; this will be the starting value of the test on each problem.
  Weight   ;; A floating point number (possibly 0) that will be used to calculate
           ;; the runtime score.  If it is 0 then this test will play no part in
           ;; the score.
  (CreditType 'Credit) ;; The Credit type specifies how this grade is used to 
                       ;; compute the Score at runtime.  The type should be 
                       ;; one of Credit (the default), Extra, or Debit.  If
                       ;; The type is Credit then it will be used to compute
                       ;; the scalar.  If it is Extra then the weight is assumed
                       ;; to be a positive amount that will be added to the 
                       ;; score.  If it is Debit then the weight is assumed to
                       ;; be a negative number that will be subtracted from the 
                       ;; score.  If is extra then it is a positive number that
                       ;; will be added to the score.  
                       ;; See the Rescale code below for more info.
  
  ScaledWeight  ;; A scaled form of the weight value used to compute the scaled
                ;; Weight form.  This is done to ensure that the score is always
                ;; between 0 and 100 and the relative contributions are always 
                ;; the same.
  (ActiveCond t) ;; A testable predicate (or constant) indicating whether or not the
                 ;; Test is active.  When the test is to be reset or updated this 
                 ;; will be tested.  If it is t (or the value is t) then it will
                 ;; be used.  If not then it will be ignored.
  
  (Loadable t) ;; This handles the tests that should be reloaded when previously
               ;; opened problem is brought up.  This defaults to t and should 
               ;; be explicitly set for those tests that represent the current
               ;; solution state.

  (Mergefunc Nil)  ;; This function is used to merge multiple test scores together
  ;; When a pair of runtime tests are loaded by name the resulting
  ;; values will be merged using this function which should be 
  ;; type-appropriate and take a list of values as its argument.  
  )




;;;; =======================================================================
;;;; Test Loading.

;;; -----------------------------------------------------------------------
;;; Test Predicates.

(defun credit-testp (Test)
  (equalp (Runtime-Test-CreditType Test) 'Credit))

(defun extra-credit-testp (Test)
  (equalp (Runtime-Test-CreditType Test) 'Extra))

(defun debit-testp (Test)
  (equalp (Runtime-Test-CreditType Test) 'Debit))


;;; -----------------------------------------------------------------------
;;; Test Registration.
;;; The code in this section is used to register the tests for the runtime 
;;; scoring.  Given a list of test functions it adds them to the testset
;;; and then removes them as necessary.  

;;; This function is used to register new tests at runtime.  It
;;; should be called in the appropriate knowledge file and used
;;; to define the usable tests.
(defmacro add-runtime-test (name &key PrintStr (Func #'(lambda (X) X)) 
				      InitFunc InitVal 
				      (Weight 0) (CreditType 'Credit)
				      (ActiveCond t) (Loadable t) (MergeFunc Nil))
  "Generate a runtime test and add it to the set."
   (let ((Test (eval `(make-runtime-test 
		       :Name ',name
		       :PrintStr ,PrintStr
		       :Func ,Func
		       :InitFunc ,InitFunc
		       :InitVal ,InitVal
		       :Weight ,Weight
		       :CreditType ',CreditType
		       :Activecond ,ActiveCond
		       :Loadable ,Loadable
		       :MergeFunc ,MergeFunc))))
    (push Test *global-Runtime-Testset*)
     Test))
 
;;; Clear the list of stored tests by setting the 
;;; list to nil and making it possible for a new 
;;; set of tests to be added.
(defun clear-runtime-testsets ()
  "Clear the current tests and scores."
  (setq *global-Runtime-Testset* Nil))

;;; Make session copy of runtime testsets
(defun session-local-runtime-testset ()
  "Make session-local copy of *runtime-testset*"
  (setq *Runtime-Testset* nil)
  (setq *Runtime-Score-Testset* nil)
  (dolist (global-test *global-Runtime-Testset*)
    (let ((Test (copy-runtime-test global-test)))
      (push Test *Runtime-Testset*)
      (when (not (= 0 (runtime-test-Weight Test)))
	(push Test *Runtime-Score-Testset*))))
  ;; In Andes2, these were applied at compile-time
  ;; in Testcode/Tests.cl
  (sort-runtime-tests-by-weight)
  (reset-runtime-testset-scores))


;;; Given a runtime test name obtain the matching runtime test
;;; from the stored tests list. Takes either the internal
;;; id symbol or possibly different friendly PrintStr
(defun lookup-name->runtime-test (Name)
  ; find by either name (a symbol) or printstr (a string)
  (or (find Name *Runtime-Testset*
	:key #'runtime-test-name
	:test #'sym-match) ; case-independent symbol 
      (find (string Name) *Runtime-Testset*
	:key #'runtime-test-PrintStr
	:test #'string-equal)))

;;; The runtime testsets are entered into the parameters in a FIFO order. 
;;; This function if called will sort them in descending order by weight
;;; s.t. the nonzero weights (even negative numbers always preceed the 
;;; zero weights.
(defun sort-runtime-tests-by-weight ()
  ;; First sort the total tests.
  (setq *Runtime-testset*
    (sort *Runtime-TestSet* #'>
	  :key #'runtime-test-weight))

  ;; Then sort the score set as it is different.
  (setq *Runtime-Score-testset*
    (sort *Runtime-Score-TestSet* #'>
	  :key #'runtime-test-weight)))


;;; Iterate over the tests in the runtime tests and sort those with 
;;; weights into the runtime-score-testset.  When this is done the 
;;; ScaledWrights also need to be set.  This is done to ensure that
;;; the 
(defun reset-runtime-score-testset ()
  (setq *Runtime-Score-Testset* Nil)
  (dolist (Test *Runtime-Testset*)
    (when (and (not (= 0 (runtime-test-weight Test)))
	       (runtime-test-activep Test))
      (push Test *Runtime-Score-Testset*)))
  (rescale-score-weights))


;;; This code resets the scaled values from each test in the set.
;;; This will be computed by iterating over all of the credit tests
;;; in the *Runtime-Score-Testset*.  From the iteration it will 
;;; compute multiplier that will be used to produce the scaledweight
;;; value that will be used at runtime to compute the scores.
;;;
;;; For Extra and Debit credit tests the system will set the scaled
;;; weight to be the same as the standard weight.  
;;;
;;; The purpose in computing the tests in this fasion is to ensure 
;;; that the code will compute the scores appropriately and that 
;;; some values can be specified to count "above and beyond" the 
;;; normal format.  
(defun rescale-score-weights ()
  (let ((Mult (compute-rescale-multiplier)))
    (dolist (Test *Runtime-Score-Testset*)
      (if (credit-testp Test)
	  ;;; Set the scaled weight for credit tests.
	  (setf (runtime-test-ScaledWeight Test)
	    (* (runtime-test-Weight Test) Mult))
	;; Set the scaled weight to weight for credit tests.
	(setf (runtime-test-ScaledWeight Test)
	  (runtime-test-Weight Test))))))


(defun compute-rescale-multiplier ()
  (let ((Multiplier 0))
    (loop for Test in *Runtime-Score-Testset*
	when (credit-testp Test)
	do (setq Multiplier (+ Multiplier (runtime-test-weight Test))))
    
    (if (= 0 Multiplier) 0
      (/ 1 Multiplier))))

    

;;;; --------------------------------------------------------------------
;;;; Weight access code
;;;; For the physicists, we want to make it possible to change the weights
;;;; of specified tests at runtime.  This will make it possible for 
;;;; them to modify the code as needed to experiment with appropriate
;;;; weights.  This function will lookup the tests by name and, if a 
;;;; match is found set the weight appropriately.
;;;;
;;;; Once this has been run we want to re-sort the weights and to reset
;;;; the *runtime-score-testset* to reflect the changes.
;;;;
;;;; Each Weight is assumed to be a list of 3 elements containing the 
;;;; The Test name, a weight, and a credit type.  The values will then 
;;; be set into the loaded code at runtime.  

(defun set-runtime-test-weights (NewWeights)
  (let (Test)
    (dolist (New NewWeights)
      (when (or (not (listp New))
		(not (numberp (second New)))
		(not (member (third New) '(Credit Debit Extra Nil) 
			     :test #'equalp)))
      	(error "Improper test weight supplied."))
      
      (setq Test (lookup-name->runtime-test (car New)))
      (when Test 
	(setf (runtime-test-weight Test) (second New))
	(setf (runtime-test-CreditType Test) (third New))))
    
    ;;; Now resort the weights and the runtime-score-testsets.
    (reset-runtime-score-testset)
    (sort-runtime-tests-by-weight)))



;;;; ================================================================
;;;; Preperatory Funcs
;;;; The preperatory funcs are used to setup any caching or other 
;;;; code for the runtime tests.  They will be called before the
;;;; tests themselves are reset any time that reset-runtime-test-scores
;;;; is called.  They are assumed to be zero-argument procedures.
;;;;
;;;; NOTE:: These funcs will be called in FIFO order.

(defun clear-runtime-test-prep-funcs ()
   (setf *runtime-testset-prep-funcs* NIL))

(defun add-runtime-test-prep-func (func)
  "Add a preperatory func to be executed."
  (setq *runtime-testset-prep-funcs*
    (append *runtime-testset-prep-funcs* (list Func))))

(defun execute-runtime-test-prep-funcs ()
  (dolist (Func *runtime-testset-prep-funcs*)
    (funcall Func)))


;;;; ================================================================
;;;; Scores
;;;; The code in this section is used to manipulate the scores
;;;; including resettiong them when problems are opened and updating
;;;; them to new values.  

;;; Runtime tests are active if the Active condition in the test is t
;;; for many tests this will simply be the constant t but if a function
;;; is provided then it will be evaluated to determine if the test 
;;; should be used.  
(defun runtime-test-activep (Test)
  (let ((Cond (runtime-test-activecond Test)))
    (if (functionp Cond)
	(funcall Cond)
      Cond)))

; following initializes a runtime test value, either to static value
; or by dynamic function call.
(defun runtime-test-init-value (Test)
"set runtime test to its designated initial value"
  (setf (runtime-test-CurrVal Test)
     (if (Runtime-Test-InitFunc Test) (funcall (Runtime-Test-InitFunc Test))
        (Runtime-Test-InitVal Test))))
  

;;; Iterate over the set of runtime-tests and reset the scores to their
;;; initial values.  This is called when a new problem is loaded at 
;;; runtime.  Set each value to the InitFunc result if initfunc 
;;; exists or the InitVal if it is nil.
;;;
;;; Before setting up the tests run the prep funcs. 
;;; When this code is called it will also reset the runtime score
;;; testset.  Doing so will ensure that the tests which are or are
;;; not to be used for scoring will be used properly.  
(defun reset-runtime-testset-scores ()
  (execute-runtime-test-prep-funcs)
  (dolist (Test *Runtime-Testset*)
      (when (Runtime-test-activep Test)
	(format webserver:*stdout* "reset-runtime-testset-scores init ~A~%"
		(runtime-test-name test))
        (runtime-test-init-value Test)))
  (reset-runtime-score-testset)
  (select-current-runtime-testset-solution))


;;; Iterate over the tests funcalling their funcs to set the new scores.
;;; Each func is assumed to take a single argument which is the current 
;;; score value.
;;;
;;; The RuntimeTestScore functions should update the objects in-place so
;;; it is not necessary to set the current value merely to call the update
;;; test.
;;;
;;; In order to avoid overcounting this code will not run when the system
;;; is in the process of checking entries.  I.E. when **checking-entries**
;;; is t.
(defun update-runtime-testset-scores ()
  (format webserver:*stdout* " update-runtime-testset-scores checking ~A tests ~a~%" (not **checking-entries**) (length *Runtime-Testset*))
  (when (not **checking-entries**)
    (dolist (Test *Runtime-Testset*)
      (when (runtime-test-activep Test)
	;; Andes2 comments suggest that this can only occur
	;; as an error.  But it seems that some tests
	;; can only be activated this way. 
	(when (null (runtime-test-CurrVal Test)) 
	  (runtime-test-init-value Test))
	(format webserver:*stdout* "   runningchecking test ~a~%" 
		(runtime-test-name test))
	(funcall (Runtime-test-func Test) 
		 (runtime-test-CurrVal Test))))
    (select-current-runtime-testset-solution)))


;;; Each time that the score is updated or reset we need to determine
;;; which of the solution set scores we consider to be the ideal.  
;;; That is we need to determine which solution set has the highest 
;;; score.  This is important for problems where we have multiple
;;; solutions and tests that have solution-specific scores.  
;;; 
;;; This will be computed by iteratively testing each of the possible
;;; indicies and determining which one returns the highest score.  
;;; The one with the highest score will be set in the runtime score
;;; cache.  
;;;
;;; NOTE:: I am using the local let variables to compute the index 
;;;  and score in since they are going to be closer in memory and 
;;;  therefore more cachable at runtime.
;;;
;;; NOTE:: This code will still be used on single solution problems
;;;  but will not involve multiple tests or the loop construct.
(defun select-current-runtime-testset-Solution ()
  (let (CScore (BestIndex 0) 
	(BestScore (calculate-runtime-total-score 0)))
    
    ;; If there is more than one solution iterate over them
    ;; to select the remaining values. 
    (dotimes (N (length (cdr *Sg-Solutions*)))
      (setq CScore (calculate-runtime-total-score (+ 1 N)))
      (when (> CScore BestScore)
	(setq BestScore CScore)
	(setq BestIndex (+ 1 N))))
    
    ;; Store the final Result.
    (setq *Runtime-testset-current-Solindex* BestIndex)
    (setq *Runtime-Testset-current-total-score* BestScore)))


;;; At runtime, it is necessary to calculate the total score that we will then
;;; later return to the students.  This will be done to gain a single value 
;;; that is then returned to the students at runtime.  In multi-solution 
;;; problems this will be used to select the most appropriate solution and
;;; is dependent upon the current solution.  Therefore this calculation code
;;; takes in an index argument that is used to compute the solution val.
;;;
;;; The Solindex argument is used to select the solution that we are computing
;;; the solution against.  
;;;
;;; The ScaledWeight value is done to ensure that the scores will be valued 
;;; appropriately.  
(defun calculate-runtime-total-score (SolIndex)
  "Return the current runtime score."
  (let ((Score 0))
    (dolist (Test *Runtime-Score-TestSet*)
      (when (runtime-test-activep test)
	;;(pprint (runtime-test-name Test))
	(format webserver:*stdout* "     test ~A val ~A weight ~A~%" 
		(runtime-test-name test)  (map-rt-val->float 
			   (runtime-test-currval Test) 
			   SolIndex) (runtime-test-ScaledWeight Test))
	(setq Score
	      (+ Score (* (map-rt-val->float 
			   (runtime-test-currval Test) 
			   SolIndex)
			  (runtime-test-ScaledWeight Test))))))
    (format webserver:*stdout* "  calculate-runtime-total-score ~A got ~A~%"
	    SolIndex score)
    Score))




;;;; ======================================================================
;;;; Output code
;;;; The code in this section is used to translate the scores into a form
;;;; that can be returned to the workbench via the stat-turn.  Thee functions
;;;; are used to collect up the individual tests and to map them as necessary.

;;; Get the cached runtime testset-total-score and return it 
;;; to the user for later use. For now this code will multiply
;;; the total score by 100 and send the integer value.  This may
;;; change later on when the renormalization is put in place.
(defun get-current-runtime-total-score ()
  (round (* 100 *Runtime-Testset-current-total-score*)))


;;; Given a set of runtime tests translate it into a list of 3-tuples
;;; containing the name of each test, its weight and its score.
;;; Uses internal id by default. Specify #'runtime-test-PrintStr to 
;;; use friendly name
(defun map-tests->list-results (Tests &key (name-fn #'runtime-test-Name))
  (mapcar 
   #'(lambda (Test) 
       (list (funcall name-fn Test)
	     (runtime-test-ScaledWeight Test)
	     (map-rt-val->display-form
	      (runtime-test-currval Test)
	      *Runtime-testset-current-Solindex*)))
   Tests))



;;;; ============================================================================
;;;; Storage
;;;; For record and backup purposes we want to keep the students scores on file.
;;;; These score values will be used in two ways.  Firstly, some but not all of
;;;; the saved scores will be loaded at runtime to keep some of the student's 
;;;; records up to date.  The scores will also be uploaded to the fileserver 
;;;; where they will be sent to the faculty on a regular basis and analysed
;;;; for teaching and research purposes.
;;;;
;;;; The code in this section will be used to format the score values for storage
;;;; and write them into the student.dat file.  It will also be called at runtime
;;;; to update the scores, reading them from the student.dat file at startup time
;;;; and using them to reset the student's scores when necessary.
;;;;
;;;; The student.dat file is formatted as an alist.  Within that file the scores
;;;; will be associated with the symbol 'runtime-test-stats'.  When Andes is 
;;;; loaded the student.dat file will be read and the 'runtime-test-stats' value
;;;; will be stored in the *runtime-testset-saved-stats* struct.  When a new 
;;;; problem is opened the reloadable scores will be loaded from there before
;;;; any other action is taken.  
;;;;
;;;; When the student has completed a problem the scores will be echoed to the 
;;;; *runtime-test-stats* struct.  Once that is done the student.dat file will
;;;; be updated with the new data.
;;;;
;;;; The scores themselves are stored in an alist associating problem names to 
;;;; lists of saved testscores.  The saved testscores themselves will be stored
;;;; in the structs defined below.  When new results are added to the lists they
;;;; will be pushed onto the saved score stack.  This will give us a saved score
;;;; session for each of the times that the student has worked on the given 
;;;; problem.  When the scores are loaded the most recent saved score will be
;;;; used.  

;;; -----------------------------------------------------------------------
;;; Saved-TestScores
;;; This struct is used to store the scores themselves.  They are sorted by
;;; the Date and time (taken from the session ID) and the time (recorded by
;;; the help system) that the problem was opened (the problem-session-id).  
;;; 
;;; These scores will be stored in the student.dat file in the following form:
;;;  (Scores . (<Problem0> . <Scoreslist0>) ... (<ProblemN> . <ScoresListN>))
;;;
;;; Where:
;;;  <Problem> is the problem name as a string.
;;;  <ScoresList> is a list of saved-score structs that can be used to recreate
;;;    the initial scores.
;;;
;;; The Saved-Score structs are used to record a single score set.  This set
;;; tracks the raw value of each score as an alist along with the following
;;; fields:
;;;   problem:      The problem name nil for non-problem scores.
;;;   SessionID:    The session ID that the problem occured on.
;;;   SessionUtime: The Session Utime that the problem occurred on.
;;;   PITime:       The start time of the problem session itself.  This
;;;                   time point will be used to sort the most recent problem
;;;                   instances and to later encode them into the database.
;;;   Merge:        If t then this is a merged score thjat was generated from
;;;                 A set of preexisting scores.
;;;   Scores:   An alist associating test names with scores.  This list will be
;;;             Loaded to bring up the problem scores.  

(defstruct Saved-Stats
  (ProblemName Nil)
  (SessionID Nil)
  (SessionUtime Nil)
  (PITime Nil) ;; change to piutime at some point.
  (Merge Nil)
  (Scores Nil))


;; set-stats -- restore specified score values
;;
;; This is used by the workbench to restore persistent scores saved in the
;; the solution file on problem open. The implementation function that does 
;; the work is in HelpStructs/RuntimeTest.cl
;;
;; Argument list for this API call should be a Lisp-readable sequence of pairs 
;; of the form (score value-expr) (score value-expr) (score value-expr) ...
;; Currently the only value types we persist will be either a number for a 
;; simple count or a list of two numbers for a fractional score.  
;; Example command string:
;;
;;   (set-stats (NSH_BO_Call_Count . 3) (WWH_BO_Call_Count . 2) 
;;          (Correct_Entries_V_Entries . (3 5))
;;          (Correct_Answer_Entries_V_Answer_Entries . (0 6)))
;;
;;
;; Result: normally NIL. No indication of success or failure.

;;; Following supports new persistence via storage of persistent stats in 
;;; workbench solution file, to be restored by an API call from the workbench 
;;; on problem load.
;;; Argument for this function should be list of pairs of the form
;;;        ((score . value-expr) (score . value-expr) (score . value-expr))
;;; where value-expr is a Lisp expression suitable as an argument 
;;; set-rt-score-value. 
;;; Currently the only value types we persist will be either a number for 
;;; a simple count or a list of two numbers for a fractional score.  
;;; Example list:
;;;         ((NSH_BO_Call_Count . 3) (WWH_BO_Call_Count . 2) 
;;;          (Correct_Entries_V_Entries . (3 5))
;;;          (Correct_Answer_Entries_V_Answer_Entries . (0 6))))
;;;
;;; Note: This routine may not work on other possible types of score values (e.g. the 
;;; per-solution value lists), but it works on what we need now.
;;;
;;; Note also: a ratio score like Correct_Entries_V_Entries is updated entirely
;;; independently of the counts it depends on. So in future sessions, the 
;;; Entry_Count score will start at 0 while the Correct_Entries_V_Entries might
;;; be reset to 3 of 5. This inconsistency won't matter because the Entry_Count 
;;; statistic is never used in the score computation; rather, for ratio scores 
;;; like the correct entry rate, the numerator is bumped on every entry. (See
;;; test update functions in Testcode/Tests.cl)
;;;  
(defun set-stats (score-value-pair-list)
  (dolist (pair score-value-pair-list)
     (let ((test (lookup-name->runtime-test (car pair))))
        (when (and Test (runtime-test-activep test))
           (set-rt-score-value (runtime-test-currval test) 
	                       (cdr pair)))))) 

