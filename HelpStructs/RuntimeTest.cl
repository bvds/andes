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


;;; The Saved-Scores parameter is used to store the saved score results that 
;;; are stored to and read from the Student.dat.  These scores will be stored
;;; in a single struct that is read from the student.dat file when it is 
;;; loaded and then stored to it when Andes is closed.  Each time that the 
;;; scores are set to be reset the struct itself will be updated and the 
;;; results reloaded.
(defparameter *Runtime-Testset-saved-stats* Nil "Saved-Stats.")


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
     (if (not (= 0 (runtime-test-Weight Test)))
	 (push Test *Runtime-Score-Testset*))
     (push Test *Runtime-Testset*)
     Test))
 


;;; Clear the list of stored tests by setting the 
;;; list to nil and making it possible for a new 
;;; set of tests to be added.
(defun clear-runtime-testsets ()
  "Clear the current tests and scores."
  (setq *Runtime-Testset* Nil)
  (setq *Runtime-Score-Testset* Nil))
;;(setq *Stats-Current-Scores* Nil))


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
  (when (not **checking-entries**)
    (dolist (Test *Runtime-Testset*)
      (when (runtime-test-activep Test)
	; AW: following error sometimes occurs after reloads in development environment:
	(when (null (runtime-test-CurrVal Test)) 
	     (format T "Warning: score update found ~A active but not initialized! Initializing now.~%" 
	               (runtime-test-name Test))
	     (runtime-test-init-value Test))
	 ; (format T "updating ~A (Currval=~A)~%" (runtime-test-name Test) (runtime-test-CurrVal Test))
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
	(setq Score
	  (+ Score (* (map-rt-val->float 
		       (runtime-test-currval Test) 
		       SolIndex)
		      (runtime-test-ScaledWeight Test))))))
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


;;; This is a simple wrapper function for the collection to abstract how
;;; the score tests are stored.
(defun collect-runtime-score-tests ()
  *Runtime-Score-Testset*)


;;; This is a simple wrapper function to abstract how all of the
;;; tests are stored.  
(defun collect-all-runtime-tests ()
  *Runtime-testset*)

;;; This returns those tests whose value must persist across sessions
;;; NB: only returns tests used in score, not all tracked stats.
(defun collect-persistent-score-tests ()
  (remove-if-not #'runtime-test-loadable *Runtime-Score-Testset*))


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


;;; ------------------------------------------------------------------------------
;;; Score storage.
;;; When the student closes a problem then we want to store the last recorded set
;;; of scores that they obtained to the *runtime-test-saved-stats* struct.  The 
;;; Scores will be stored by problem name.  If no instance of the problem exists
;;; then a new one will be loaded.

;;; This function will start by looking up the problem supplied in the 
;;; argument.  If it is present then the system will add a new score 
;;; record to the existing set.  If it is not present then the system
;;; will generate a new assoc and store the initial problem in the set.
;;;
;;; Here Key is a symbolic problem name.
(defun store-runtime-test-stats (Key)
  "Store the current stats by key."
  (let ((Stats (find-stored-runtime-test-stats Key)))
    (if (null Stats) (srt-generate-stats Key)
      (srt-append-stats Key Stats))
    (studentfile-tell 
     'Saved-Stats *Runtime-Testset-Saved-Stats*)))


;;; Generate a new stats list associated with the supplied key and store
;;; it in the list.  The new stats list will contain a key and a single
;;; score instance taken from the current scores.
(defun srt-generate-stats (Key)
  (push (list Key (srt-generate-saved-stats Key))
	*runtime-testset-saved-stats*))


;;; Append a new stats set to the current set of stats and keep it
;;; stored at runtime.
(defun srt-append-stats (Key OldStats)
  (setf (cdr OldStats) 
    (cons (srt-generate-saved-stats Key) 
	  (cdr OldStats))))


;;; Generating a new set of instance stats from the current stats is a 
;;; matter of wrapping up the stat values in a list and storing them
;;; in an instance struct.  Only the raw score values are stored.  The 
;;; other values will be stored in the runtime structs.  
(defun srt-generate-saved-stats (Key)
  (make-saved-stats
   :ProblemName Key
   :SessionID *Current-Andes-Session-ID*
   :SessionUtime *Current-Andes-Session-Start-Utime*
   :PITime *Current-Problem-Instance-Start-UTime*
   :Scores (srt-collect-saved-stat-scores)))

;;; Initially I had hoped to only store the active tests but, for simplicity's
;;; sake I am going to forego that and to store all of the tests.  This will
;;; make it possible for the tests to be stored as the scores are reset.
(defun srt-collect-saved-stat-scores ()
  "Map function for collecting the current scores."
  (loop for Test in *Runtime-TestSet*
      when (runtime-test-CurrVal Test) 
      collect (list (runtime-test-name Test)
		    (pack-rt-val (runtime-test-CurrVal Test)))))


;;; --------------------------------------------------------------------------
;;; Runtime-Stat-init
;;; Between Andes sessions the student's saved stats are stored in the 
;;; student.dat file.  When the student data is read at the start of 
;;; the Andes session the student file will be loaded and this code
;;; will be called to initialize the student's saved stats.  

;;; At runtime the saved scores are kept in the *Runtime-Testset-saved-stats*
;;; variable.  When the styudent loggs in that variable will be refreshed from
;;; the studentfile using the code below.  

(defun load-saved-scores-cache ()
  "Load the saved stats from the studentfile."
  (setq *runtime-testset-saved-stats*
    (studentfile-ask 'saved-stats)))



;;; ------------------------------------------------------------------------
;;; Runtime Score Loading.
;;; When a new problem instance or non-problem-instance is begun the system
;;; will lookup the matching set of saved runtime test stats.  Those stats
;;; will then be loaded so long as the runtime test is marked loadable.  
;;; If the LoadAll keyword is supplied then they will be loaded irrespective
;;; of whether or not they are marked as such.  
;;;
;;; I opted to store all the data and to have the tests specify whether or
;;; not the value(s) are reset in order to keep the data in the student.dat
;;; for later analysis.  
;;;
;;; The purpose of the loadall keyword is to facilitate loading for log 
;;; combination.

;;; This code will be called by the interface code.

(defun load-stored-runtime-test-stats (Key &key (LoadAll Nil))
  "Load the current runtime tests stats by matching with key."
  (let (Test (Set (find-stored-runtime-test-stats Key)))
    (when Set
      (dolist (Val (Saved-Stats-Scores (cadr Set)))	
	(setq Test (lookup-name->runtime-test (car Val)))
	;;(pprint Test)
	;;(pprint Val)
	;;(if (runtime-test-activep Test) (print 'active))
	;;(if (runtime-test-loadable Test) (print 'loadable))
	(when (and Test (or Loadall (and (runtime-test-activep Test) 
					 (runtime-test-loadable Test))))
	  ;;(print Val)
	  (setf (runtime-test-currval Test)
	    (unpack-rt-val (cadr Val))))))))


;;; Following supports new persistence via storage of persistent stats in workbench 
;;; solution file, to be restored by an API call from the workbench on problem load.
;;; Argument for this function should be list of pairs of the form
;;;        ((score value-expr) (score value-expr) (score value-expr))
;;; where value-expr is a Lisp expression suitable as an argument set-rt-score-value. 
;;; Currently the only value types we persist will be either a number for a simple count 
;;; or a list of two numbers for a fractional score.  Example list:
;;;         ((NSH_BO_Call_Count 3) (WWH_BO_Call_Count 2) 
;;;          (Correct_Entries_V_Entries (3 5))
;;;          (Correct_Answer_Entries_V_Answer_Entries (0 6))))
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
(defun set-runtime-test-stats (score-value-pair-list)
  (dolist (pair score-value-pair-list)
     (let ((test (lookup-name->runtime-test (first pair))))
        (when (and Test (runtime-test-activep test))
           (set-rt-score-value (runtime-test-currval test) 
	                       (second pair)))))) 

;;;; ------------------------------------------------------------------------
;;;; ------------------------------------------------------------------------
;;;; Saved stats manipulation/
;;;; The code in this subsection is for use in loading and manipulating the 
;;;; saved stats in batch mode from the student.dat files.  This code will
;;;; be used for storing the scores into the update database and to make 
;;;; other changes such as merging two or more sets of saved scores.
;;;;
;;;; In this code I have deliberately avoided making use of the global variables
;;;; that exist above.  My goal in doing so is to avoiud stomping on that code
;;;; and to avoid any necessary hacks that would result from using them.
;;;;
;;;; Tasks supported:
;;;;   Score database generation/update.
;;;;   Score merging for multiple branches.
;;;;   
;;;;
;;;; NOTE: For now some of the columns in the code will generate Nil values while 
;;;; others do not.  This may be changed later on.  

;;; ---------------------------------------------------------------------------
;;; Load saved stats set.
;;; Given a specific set of saved stats load it into the current runtime 
;;; testset.  This can then be used for output or other selection.  
;;; This is similar to the load-stored-runtime-test-stats function above 
;;; but will be used for score storage in the database not runtime score 
;;; loading.  

(defun load-saved-stats->runtime-testset (Saved-Stats &key (LoadAll t))
  "Iterate over the stats for later use."
  (let (Test)
    (dolist (Val (saved-stats-scores Saved-Stats))
      (setq Test (lookup-name->runtime-test (car val)))
      (if (null Test) 
	  (error "Supplied Runtime test not recognized.")
	(when (or Loadall (and (runtime-test-activep Test) (runtime-test-loadable Test)))
	  (setf (runtime-test-currval Test)
	    (unpack-rt-val (cadr Val))))))))





;;; --------------------------------------------------------------------
;;; Score Iterating.
;;; For the purposes of iterating over the runtime scores it is sometimes
;;; necessary to explicitly load the old scores not just the most recent
;;; set.  This code will iterate over all of the saved stats calling
;;; the supplid function on them.
;;;
;;; The function is assumed to take a single argument which is the
;;; saved stats.

(defun iterate-score-lambda (Func Saved-Scores)
  "Iterate over the problems and scores in the Saved-Scores list."
  (dolist (Scores Saved-Scores)
    ;;(pprint (list "SCORES" Scores))
    (dolist (Stats (cdr Scores))
      ;;(pprint (list "STATS" stats))
      (funcall Func Stats))))


;;; -------------------------------------------------------------------
;;; -------------------------------------------------------------------
;;; Database output.  
;;; The code in this subsection is used to facilitate database output.
;;; It will be used to form database table specs for the 

;;; Given the current set of runtime testsets iterate over them to 
;;; obtain a list of the form: ((<Testname> <ExternalType>) ...)
;;; Where TestName is the name of the test and ExternalType is the 
;;; lisp type that will be passed to the database code.  This type
;;; will then be translated appropriately to obtain the requisite 
;;; database format.  

;;; AW: this routine currently unused. It had the only substantive 
;;; use of the old ValType slot, now replaced by type-of. 

(defun map-testset->names-externtype-lst ()
  "Map the teset to names and external types."
  (mapcar 
   #'(lambda (Test)
       ;;(pprint test)
       (unless (runtime-test-CurrVal test)
          (runtime-test-init-value Test))
       (list (runtime-test-printstr Test)
	     (map-rt-val-classname->externtype 
	      (type-of (runtime-test-CurrVal Test)))))
   *Runtime-testset*))
       


;;; ----------------------------------------------------------------------
;;; Score utility funcs.

;;; Locating preexisting stored runtime stats consists of searching
;;; through the existing list for any set whose car is equalp to 
;;; KEY.  
(defun find-stored-runtime-test-stats (Key)
  "Find the runtime test records."
  (find Key *Runtime-Testset-saved-stats*
	:key #'car :test #'equalp))







;;;; ----------------------------------------------------------------------
;;;; Score Merging.
;;;; The saved scores as stored in the student.dat files are intended to 
;;;; reflect the student's state.  Unfortunately, few students ever 
;;;; geneerate a single student.dat file.  Present versions of Andes load
;;;; the student.dat file from the host machine when the user loggs in.  
;;;; future versions may attempt to extract the state from a central server.
;;;;
;;;; Unfortunately, when the users work on other machines (with friends, 
;;;; faculty, or in labs) the system may not properly load their initial 
;;;; file.  Mistyped logins (p001 v P001 v 1) will also result in multiple
;;;; files being generated.  Lastly, for notebook or other "untethered" 
;;;; users, fetching the state from the server may not be an option.  
;;;;
;;;; In these cases it will be necessary (some :time point) to merge the 
;;;; disconnected files.  The code in this subsection will be used to merge
;;;; mutliple sets of saved scores.  Other code will handle the merging
;;;; of other studentfile portions.  
;;;;
;;;; Given several sets of saved scores the system will merge them in the 
;;;; following fasion.
;;;;    If a set of problem results occurs in only one of the sets then 
;;;;     it will be copied directly into the result set.  
;;;;    If a set of problem results occurs in more than one of the sets 
;;;;     then the sets will be sorted in-order according to the problem
;;;;     instance ID or, if that is nil (as it is in some early logs) 
;;;;     then the SessionUtime and their order in the list.  
;;;;    Once they are sorted a dummy sum value representing the sum of 
;;;;     the values (combined using the test definitions).  The 
;;;;     SessionUtime will be set to be set to the current time.  The
;;;;     Merge flag will also be set to t.  The PITime will be set to 
;;;;     Nil.  
;;;;
;;;;  The Individual runtime-tests are flagged with functions that specify
;;;;  how the saved scores from them are meant to be combined.  These funcions
;;;;  are assumed to take a set of saved score values of the appropriate 
;;;;  type 

(defun merge-saved-score-sets (&rest ScoresToMerge)
  (merge-saved-score-sets-r ScoresToMerge))
  
				     
(defun merge-saved-score-sets-r (ScoresToMerge &optional (Result Nil))
  "Merge the sets of saved scores."
  (if (null ScoresToMerge) Result 
    (let ((Curr (car ScoresToMerge))
	  (Others (cdr ScoresToMerge)) Matches)
      (dolist (Prob Curr)
	(setq Matches (msss-find-matching-probs Prob Others))
	(if (null Matches) (setq Result (msss-add-prob Prob Result))
	  (setq Result
	    (msss-add-prob
	     (msss-merge-probs (car Prob) (mapcar #'cdr (cons Prob Matches)))
	     Result))))
      (merge-saved-score-sets-r Others Result))))
		

;;; Given a problem saved stats set locate the other saved problem sets from 
;;; the supplied list of scores.  Based upon sfm-find-matching-tells.
(defun msss-find-matching-probs (Prob Others)
  (let (R New)
    (dotimes (N (length Others))
      (setq New (find (car Prob) (nth N Others) :key #'car :test #'equalp))
      (when New
	(push New R)
	(setf (nth N Others) (remove New (nth N Others)))))
    R))


;;; Given a problem and a result set add the problem to the list overwriting
;;; any value if it is present or just pushing it on if it is not.
(defun msss-add-prob (Prob Lst)
  (let ((other (find (car Prob) Lst :key #'car :test #'equalp)))
    (if (null Other) (cons Prob Lst)
      (cons Prob (remove Other Lst)))))


;;; Merging the problems has two tasks.  The first is to generate a new special
;;; "merged" final score for the problem from the set of supplied sets of 
;;; of problem instances.  This merged score will reflect all of the students
;;; work up to this point.  
;;;
;;; The second is to sort those instances into a single problem set.  Because I
;;; want the final order to continue to reflect the total order I have written 
;;; my own sorting algorithm.  Initial saved-score sets do not include the PIUtime
;;; and so it was necessary to write my own sorting algorithm. 
;;;
;;; In Absence of the PITime it is known that the items are in an ordered (LIFO)
;;; stack.  They will be sorted as such into groups and then have a final merged
;;; form generated for them once that is done.  The final form will have a time
;;; set to the time that it is generated with no PITime.
;;;
;;; 
;;;
;;; NOTE:: Although this code has problem instance utimes the initial
;;;    code did not so I have written this to keep all instances in a
;;;    given session together (in the intial sets no PITimes existed.

(defun msss-merge-probs (Name Probs);; &optional (Result Nil))
  "Merge the contents of the problems."
  (let ((R (list (msss-make-merged-scores Name (mapcar #'car Probs)))))
    (do* ((NewProbs Probs (remove-if #'null NewProbs))
	 (Next (msss-rmp-next NewProbs)
	       (msss-rmp-next NewProbs)))
	((Null (nth 1 Next)) (cons Name (reverse R)))
      (push (nth 1 Next) R)
      (setf (nth (car Next) NewProbs) 
	(cdr (nth (car Next) NewProbs))))))



;;; Get the next item from the list or return nil none can be found.
(defun msss-rmp-next (Probs)
  (let ((P1 (caar Probs)) (I 0) P2)
    (dotimes (N (length Probs))
      (setq P2 (car (nth N Probs)))
      (when (< (Saved-Stats-SessionUtime P1) 
	       (Saved-Stats-SessionUtime P2))
	(setq P1 P2)
	(setq I N)))
    (list I P1)))


;;; Given a list of the first (latest) scores from each of the saved
;;; scores score sets.  Load them into the system and then merge the 
;;; scores to generate a new value.
;;;
;;; Sort the scores initialy by date so that the oldest scores are 
;;; handled first.  Note that this sorts soley on the session UTime 
;;; since that is not shared between files and is assumed to be 
;;; identical.
(defun msss-make-merged-scores (Name Scores)
  "Generate the merged scores."
  (let* ((STime (Get-universal-time))
	 (SortScores (sort Scores #'<  :key #'saved-stats-SessionUTime)))
    (make-saved-stats
     :ProblemName Name
     :SessionID (make-merge-sessionid Stime)
     :SessionUTime Stime
     :PITime Nil
     :Merge t
     :Scores (msss-mknew (mapcar #'saved-stats-scores SortScores)))))


;;; Temporary func to make a session-id for the merge-time.
;; start here.
(defun make-merge-sessionid (Stime)
  (format nil "merge-~a" Stime))


;;; Given pointers to the lists of (unread) saved-stat-scores
;;; iterate over the sets reading each one in and then merging
;;; the values for later use.  
;;;
;;; This merge process will copy the incoming lists and read in
;;; the saved scores before moving on to merge the values and 
;;; to store the results for later.  

(defun msss-mknew (ScoreLists)
  (do* ((Result (msss-mms-i
		 (read-scoreset (car ScoreLists)) (cadr ScoreLists))
		(msss-mms-i Result (car RestScores)))
	(RestScores (cddr ScoreLists) (cdr RestScores)))
      ((null RestScores) 
       (mapcar #'(lambda (V) (list (nth 0 V) (pack-rt-val (nth 1 V))))
	       Result))))



;;; Given an unread list of scores iterate over the list generating a 
;;; new set of score values and returning those to the user.
(defun read-scoreset (ScoreList)
  (loop for V in ScoreList
      with V2
      collect (list (nth 0 V) (unpack-rt-val (nth 1 V)))))

;;; Given a pair of scoresets the first of which has been read in 
;;; iterate over the latter locating matching values and merging 
;;; in the results.  
;;;
;;; Given a list of scores Iterate over the sets and merge the values.
;;; This is done using the score merging functions that are registered
;;; with the Rumtime-Tests themselves.  If an unrecognized test name is
;;; found then an error will be thrown.  Any values that are present in
;;; one list but not in the other will simply be added to the set.  
(defun msss-mms-i (Old New)
  "Merge old and New."
  (let (Result (NewSet New))
    (do* ((OldSet Old (cdr OldSet))
	  (O (car OldSet) (car OldSet))
	  (NIndex (position (car O) NewSet 
			    :Key #'car :Test #'equalp)
		  (position (car O) NewSet 
			    :Key #'car :Test #'equalp)))
	((null OldSet) (append (read-scoreset NewSet) Result))
      
      ;;; When the NIndex exists then pop the value off the list and merge them
      ;;; Else just add O to the result set.  
      (cond 
       ((null Nindex) (push O Result))
       (t (push (msss-mms-i-merge O (nth NIndex NewSet)) Result)
	  (setq NewSet (append (subseq NewSet 0 NIndex)
			       (subseq NewSet (+ 1 NIndex)))))))))
	  



;;; Given a pair of score values locate the corresponding test and funcall
;;; the score combination function on the values returning the result 
;;; along with the TestName added.
(defun msss-mms-i-merge (Old New)
  "Merge old and new and return the result."
  (let* ((TestName (car Old))
	 (Test (lookup-name->runtime-test TestName)))
    (cond 
     ((null Test) (error "Unrecognized test: ~a Supplied" TestName))
     ((null (runtime-test-mergefunc Test))
      (error "Test ~a has no mergefunc." TestName))
     (t (list 
	 TestName
	 (funcall (runtime-test-mergefunc Test)
		  (cadr Old) (unpack-rt-val (cadr New))))))))











      

(defun trace-runtime-test-merge ()
  (trace merge-saved-score-sets
	 msss-find-matching-probs
	 msss-add-prob
	 msss-merge-probs
	 ;; msss-recurse-merge-probs
	 msss-rmp-next
	 msss-make-merged-scores
	 make-merge-sessionid
	 msss-mknew
	 read-scoreset
	 msss-mms-i
	 msss-mms-i-merge))
	 
;;	 msss-mms
;;	 msss-mss-read-saved-scores
;;	 msss-mms-i
;;	 msss-mms-i-merge))
  



(defun trace-runtime-tests ()
  (trace clear-runtime-testsets
	 add-runtime-test
	 runtime-test-activep
	 reset-runtime-testset-scores
	 update-runtime-testset-scores
	 get-current-runtime-total-score
	 collect-runtime-score-tests
	 collect-all-runtime-tests
	 map-tests->list-results
	 ;;map-runtime-test-val->out
	 ))
