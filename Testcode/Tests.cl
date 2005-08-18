#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests.cl
;;; Collin Lynch
;;; 8/8/2003
;;; Copyright Kurt VanLehn.
;;;
;;; At runtime we want the system to load and maintain a list of scores
;;; that reflect the student's actions in some set ways.  This is the 
;;; feature formerly known as autograding.  This feature will be used
;;; to keep running records of the student's problem-solving behavior
;;; and their state on the problem.  
;;;
;;; Those scores will be communicated to the student via the workbench and
;;; the faculty my some means that has not yet been settled.  
;;;
;;; Ideally this output will be used to help the students guide themselves
;;; especially in the use of voluntary mastery.  It will also, I hope be 
;;; used by the faculty to thelp them better guide the students by working
;;; with a fuller picture of the student's state.  Most importantly, I hope
;;; that it will NOT be used to bias the professors against students or to 
;;; be the sole source of their grades.  
;;;
;;; The code in this file will handle the creation and maintenance of the 
;;; students scores incuding the initial registration of test functions, 
;;; the updating of the scores at runtime, reporting those scores to the
;;; student, and reporting those scores to the professors.  
;;;
;;; Score reporting may involve multiple mechanisms.  Updating the students
;;; scores at runtime to the workbench will be done by using a dde-command
;;; attached to the tutor turn.  Sending the stoces to te faculty members 
;;; will either be done by the workbench in which case the raw scores will 
;;; have to be requested by the workbench using a new API command and then 
;;; returned using a new tutor-turn type.  Failing that they will be send to
;;; the professors directly by the help system.  
;;;
;;; The code below is grouped into functional sections.  The initial section
;;; lists the runtime parameters.  The following section will contain the code 
;;; for initial setup and registration of tests.  Following that will be the
;;; runtime testing and update code.  Finally the Student reporting code and
;;; faculty reporting code.
;;;
;;; Tests are assumed to be functions that take in some current score value
;;; or nil and return a score value that will be stored as the result.  Unlike
;;; the cmdreader-based system this one does not use the testset struct.  
;;; that form is relatively unweildy I think for this.  

;; Start here considering this.  


;;; When Andes is first loaded, some code in the system will load the 
;;; executable tests.  This will most likely be located in the startup
;;; file but it can be compiled in elsewhere perhaps in a separate module.
;;;
;;; At that point the system will then iterate and update the scores
;;; on each entry clearning them on each problem instance.  
;;;
;;; There needs to be an individual test struct comprising the information
;;; about each test including its behavior, stored scores, etc.  
;;;
;;; Public funcs.
;;;  1. Loading new tests and clearing registered tests. (called at startup).
;;;  2. Loading the test weights and clearing the test weights.  (part of 
;;;     the registration.  each test func will contain the necessary weight
;;;     information within it.)
;;;  3. Clearing the scores and updating the scores (called on each open-problem
;;;     and each command.)
;;;  4. Obtaining the raw scores as opposed to the weighted values.  (called
;;;     intermittently by the workbench.  
;;;
;;;  The wieights along with a total score will be submitted back to the workbenc
;;;  at runtime, further changes will vary as necessary.  



;;;; --------------------  Solution-Based Scores ---------------------------------
;;;; Each problem solution is a subset of all of the possible equation and non-eqn
;;;; on a given problem.  The equation and non-eqn-entry subscores compute the 
;;;; number of these entries that the student has completed at any given time.
;;;; Since we do not know a-priori which of these solutions that the student 
;;;; is pursuing it will be necessary for us to compute the score on all of them
;;;; and to return the best of all possible scores after each entry and upon each
;;;; score requests.  This policy will extend to the final scores as well.  
;;;;
;;;; This raises towo potential problems.  The foirst is that maintaining these 
;;;; parallel scores will excess computation time.  This is probably surmountable
;;;; as it is possible to eliminate those requirements fairly easily. 
;;;;
;;;; The second problem is more apt to affect the students directly.  So long as
;;;; the system continues to assume that the student is working on a single solution
;;;; their total score, and the breakdown will behave in much the way that they 
;;;; suspect. If the system has guessed correctl;y what score the student is 
;;;; pursuing then a score such as "major equations: 3/5" indicates correctly that
;;;; they have 3 of five equations to work on.  If, however the system has not 
;;;; guessed correctly then the information will not necessarily be accurate
;;;; in the student's mind.  
;;;;
;;;; Additionally, the system will change the solution that it assumes from time to
;;;; time.  When this occurs the denominator of some fractional entries such as the 
;;;; major equations example above will change.  I am not saying that these problems 
;;;; will necessarily damage the system but they are things that we should consider.
;;;;
;;;; This will be handled by breaking the elements of the Solutions into parameters
;;;; and addressing those at test-time.

;;;; ----------------------- :Fract and :Fract1 scores ----------------------------
;;;; The :Fract values indicate a ratio.  These ratios will be used to compute the 
;;;; ratios.  When those ratios are translated into a total number then the number
;;;; can be defined one of two ways.  for :Fract values 0/0 will be taken to mean 0
;;;; for :Fract1 values 0/0 will be taken to mean 1.  
;;;;
;;;; This distinction exists because on some tests (axes-entry-ratio) for example we
;;;; want to give the students credit for not needing to make any entries (:Fract1)
;;;; wheras on others (help entry) we need to give them no credit (since credit is 
;;;; bad).  This redefinition of an undefined value was the solution that I chose.

;;;; =============================================================================
;;;; ChangeLog
;;;;   1/6/2004 - (CL) - Altered test-cache-solution-objects so that it no longer 
;;;;     makes ude of *nsh-bodies*.  System now uses test-cache-collect-bodies-list
;;;;     that function makes use of nsh-collect-principle-bodyents.

;;;; ==============================================================================
;;;; TODO.
;;;;  1. End dpeendence of tsch-cache-collect-bodies-list on nsh internal code.
|#

(clear-runtime-testsets)


;;;; ===============================================================
;;;; Parameter Data.

;;; This is the pause htime total.  It will be 
;;; used at runtime as a threshold for detecting
;;; a pause.
(defparameter **Testing-Pause-Time-Threshold** (make-htime :Sec 20))


;;;; ==================================================================
;;;; Cached Data
;;;; In order to speed up some tests It is necessary to pre-cache data
;;;; when the problem is first loaded or Andes first opened.  The code
;;;; in this section does that.  The requisite functions are registered
;;;; with the setqup eqn-solutions code for registration at runtime.



;;;; ---------------------------------------------------------------------
;;;; Entry Caching.
;;;; This code data is cached in order to make it possible for the entry
;;;; and equation tests to be run.  It is registered with the set-equation
;;;; scores code and will be executed there.
;;;;
;;;; One of the major issues here is how to classify the "necessary" components 
;;;; of a solution.  The set of all entries necessary to complete a spacific 
;;;; solution is smaller than the set of all entries within a solution.  In 
;;;; particular not all of the vectors are necessary (especially if they
;;;; can cancel out in an equation).  Some of the Implicit-equations can be
;;;; classified as optional equations, and it is possible to derive some of
;;;; the optional equations to avoid others.  
;;;;
;;;; Since we have determined that trhe student should write all of the eqns 
;;;; in symbolic form and that those equations appear at the top of nodes we
;;;; can classify them as necessary.  Given equations are in the same position
;;;; they cannot *really* be eliminated from the problems.  We have an 
;;;; existing rubric for axes (all in a solution are necessary).
;;;;
;;;; In addition to that we have a rough rubric for bodies (if it appears in a
;;;; principle expression then it should be written).  But this can be debated
;;;; Mass variables can also be used for these.  Moreover we still use times 
;;;; in the body expressions.  For that reason we will be forcing the students
;;;; to define the same body at several different times.  This seems odd to 
;;;; some people.  
;;;;
;;;; Because of this I am going to only write tests for the eqns, given-eqns,
;;;; bodies and axes.  Vectors can be added later if it becomes necessary to
;;;; do so.  

(defparameter *test-cache-eqn-entries* ())
(defparameter *test-cache-given-eqn-entries* ())
;;(defparameter *test-cache-derived-eqn-entries* ())
;;(defparameter *test-cache-implicit-eqn-entries* ())
;;(defparameter *test-cache-vector-entries* ())
(defparameter *test-cache-axis-entries* ())

(defparameter *test-cache-objects* ())
;;(defparameter *test-cache-body-entries* ())



;;; This parameter is used in the test-cbe-get-object function below 
;;; and in the runtime test-cache-entered-objects-count function below
;;; that.  This is a handy way of caching the lookup so that it doesn't
;;; need to be re-done constantly while still staying kb-driven.
;;;
;;; Becuase of the need to reload the kb at runtime I have set this code
;;; so that this lookup will only be performed if a problem has been 
;;; loaded.  In fact this parameter will be reset each time a new 
;;; problem is loaded.  This is somewhat inefficient but it is a 
;;; nice brute-force method of handling the reload issue.  
(defparameter **Current-Body-Expression-Form** Nil
  "Cache the help-expression form of the body entryprop for later use.")




;;; On the multiple choice problems the student can only make 
;;; check-answer entries.  We want to flag these problems so
;;; that we will not penalize the studetns for not writing 
;;; equations on them.  Because the testing task is so time-consuming
;;; I will cache the value here.  In the future it would (again) be
;;; nice to hang some of these values on the problem struct itself
;;; so that they can be tested more directly there.
(defparameter **Current-Prob-Has-nonanswer-entries** t)



;;;; -------------------------------------------------------------------------
;;;; Cache Setup.
;;;; This predicate will be added to the list of test-prep-functions and will
;;;; execute the setup code at runtime in order to prep the entry and object 
;;;; caches for use at runtime.

;;;; Add the cache code to the preperatory funcs list for
;;;; use at runtime.  Note that it must only run when a 
;;;; problem has been loaded.
;;;;
;;;; This code will reset the global parameter containing the 
;;;; body entry form each time a new problem is loaded as a 
;;;; way of supporting kb reloads.  

(clear-runtime-test-prep-funcs) ; do on loading before first add

(add-runtime-test-prep-func 
 #'(lambda () 
     (when (problem-p *cp*) 
       (setq **Current-Body-Expression-Form**
	 (entryprop-helpform (lookup-entryprop-type 'body)))
       ; optimization: optionality test does graph search. Memoize it on
       ; each new problem so search is only done once for each systementry.
       ; (each call to memoize clears memory of saved results.)
       (memoize 'sg-systementry-optional-p :test #'eq)
       (test-cache-solution-entries)
       (test-cache-solution-objects)
       ;(format T "required axes: ~A~%" *test-cache-axis-entries*)
       ;(format T "required bodies: ~A~%" *test-cache-objects*)
       (test-problem-nonanswer-entries))))





;;; -----------------------------------------------------------------------
;;; Entry Caching
;;; The code in this section is used to cache the individual object entries
;;; and to calculate the initial score values for the fractional entry tests.
;;; This setup code will prepare the *test-cache-eqn-entries* 
;;; *test-cache-given-eqn-entries* and *test-cache-axis-entries* parameters
;;; for use at runtime.  
;;;
;;; The test-cache-solution-entries function will be registeres as a prep
;;; function to be executed whenever a new problem is opened.  

;;; Setup the three entry cache parameters for use at runtime.
(defun test-cache-solution-entries ()
  "Cache the test information."
  (test-cache-set-lists)

  ;; For each of the solutions.
  (dotimes (N (length *SG-solutions*))
    ;; Iterate over the entries.
    (dolist (Entry (sgsol-entries (nth N *sg-solutions*)))
      (case (help-entryprop-type (systementry-prop Entry))
	(draw-axes (test-cache-axes-entry N Entry))
	(eqn (test-cache-eqn-entry N Entry)))))) ;; Not takes in given eqns too.


;;; Set each of the cache parameters to be a list of 
;;; nils of equal length to the number of solutions.
(defun test-cache-set-lists ()
  (let ((len (length *sg-solutions*)))
    ;; Just a little hack to ensure that at least some scores are recorded.
    ;; This will probably be removed later.
    (if (= 0 Len) (setq Len 1))
    (setq *test-cache-eqn-entries* (make-list Len)) 
    (setq *test-cache-given-eqn-entries* (make-list Len))
    (setq *test-cache-axis-entries* (make-list Len))))


;;; If the entry is an axis then test the relavent list in the 
;;; list and store it unless there is already an axis there and
;;; this is not a multi-axis-problem.  If that occurs throw an
;;; error.
;;;
;;; NOTE:: At one time it was agreed upon that multiple-axis problems
;;;  would be labeled with the feature 'multi-axis-problem.  That is
;;;  apparently no longer the case so when I developed this code I 
;;;  put in the error to catch badly formed problems.  Since noone has
;;;  been using the feature I have commented it out here.
(defun test-cache-axes-entry (Solution Entry)
  "Cache the axes entry."
  ;;(if (and (nth Solution *Test-cache-axis-entries*)
  ;;   (single-axis-problemp *cp*))
  ;;     (error "Multiple axes in single-axis problem solution.")
 (when (not (sg-systementry-optional-p Entry)) ; ignore if optional
  (push Entry (nth Solution *test-cache-axis-entries*))))


;;; If the entry is an equation then we need to store or ignore
;;; it depending upon its type.  This is done using the 
;;; match-systementry->eqn-type predicate in Systementry.cl
;;;
;;; If the entry is an eqn (major eqn) then it will be cached,
;;; ditto for any given equation.  Other equation types will 
;;; be ignored.  
(defun test-cache-eqn-entry (Solution Entry)
  "Cache the equation entries."
  (let (Class (Eqn (match-systementry->eqn Entry (problem-Eqnindex *cp*))))
    (when Eqn
      (cond 
       ;; If it is a given equation then we will add it to the given-eqns cache.
       ((equalp (eqn-type Eqn) 'given-eqn)
	     (push Entry (nth Solution *test-cache-given-eqn-entries*)))
	    
       ;; If it is an eqn and it is not a trivial system equation, and it is 
       ;; and it is a major psm equation or a major equation then
       ;; we want to add it to the major equations cache.  
       ;; The trivial-syseqn-p predicate is defined in interpret-equation.cl
       ((and (equalp (eqn-type Eqn) 'Eqn)
	     (not (trivial-syseqn-p Entry))
	     (or (and (setq Class (lookup-Expression->psmclass (eqn-exp Eqn)))
		      (psmclass-major-p Class))
		 (and (setq Class (lookup-expression->equation (eqn-exp Eqn)))
		      (equation-major-p Class))))
	
	(push Entry (nth Solution *test-cache-eqn-entries*)))))))

	 

;;; -------------------------------------------------------------------
;;; SolSet Entry Updates.
;;; The code in this section us used to update the SolSets that are 
;;; based upon the entry caches above.  At runtime these functions 
;;; are used by the tests below to update the numerator portion of 
;;; each Fract or Fract1 within the solsets to reflect the number of
;;; required entries that the student has made.  

;;; This utility function is used to update the SolSet scores for a 
;;; given cache.  This will take in an existing solset and then 
;;; update the first element in each fractional value to be the 
;;; current score value.  This one is hard-coded to test whether
;;; or not the entries have been made at all.  others will be 
;;; used to test other values.  
(defun test-cache-update-fract-SolSet (Score Cache)
  "Update the fractional SolSet Score."
  (dotimes (N (length Cache))
    (update-rt-solset-val
     Score 
     :Index N
     :Func #'(lambda (Curr)
	       (declare (ignore Curr))
	       (loop for E in (nth N Cache)
		   count (systementry-entered E))))))

;;; This utility function is used to update the SolSet scores for a 
;;; given cache.  This will take in an existing solset and then 
;;; update the first element in each fractional value to be the 
;;; current score value.  
;;;
;;; This code makes use of a test predicate that will be called
;;; on each element in the cache.  If the test returns t then
;;; the code will be used if not then it won't.

(defun test-cache-test-update-fract-SolSet (Score Test Cache)
  "Update the fractional SolSet Score."
  (dotimes (N (length Cache))
    (update-rt-solset-val
     Score 
     :Index N
     :Func #'(lambda (Curr)
	       (declare (ignore Curr))
	       (loop for E in (nth N Cache)
		   count (and (systementry-entered E)
			      (funcall Test E)))))))



;;; ---------------------------------------------------------------------------
;;; Body Object Cache
;;; The code below is used to setup the body entry cache.  It will be called 
;;; at runtime when a problem has been opened to setup the list of entries 
;;; for use at a later time. 

;;; The body entries are an interesting challenge.  For historical reasons the
;;; body entries have both an object and a time component.  Therefore people 
;;; often need to define a body for the motorboat at time 1, time 2 and from
;;; time 1 to time 2.  This is somewhat nonsensical since the purpose of the 
;;; body tool is to reify the objects within the system irrespective (really)
;;; of time.  For now we will not change this in the workbench or help system 
;;; but, for mastery purposes we will grade the people on objects not entries.  
;;;
;;; When the student requests next-step-help one of the first things that they
;;; are prompted to do is write the "necessary" body entries.  These are 
;;; defined as the bodies that appear at the "top-level" of the principles.  
;;; On many problems these will include many overlapping entries such as
;;; the motorboat at different times.  
;;;
;;; For the purposes of mastery-grading we will extract the unique objects from
;;; the entries in this parameter and require that the students write one body
;;; entry for each object.  The test will also be written here for runtime use.
;;;
;;; This code will form a list of lists of the form: (<Solution0> ... <SolutionN>)
;;; where each Solution is a list of lists of the form (<Obj0> ... <Objn>).  
;;; each object is a list of the form (<Obj> . <Entries>) where Obj is an object
;;; name or a list of names (representing a compound object) and <Entries> is a 
;;; list of all the systementries in the solution that reify that object at some 
;;; time.  
;;;
;;; The purpose of this convoluted cache form is to speed up the checking process
;;; at runtime as it will occur on each and every body entry or object deletion. 
;;; Rather than slow the system down by constant unification I opted to chache 
;;; the comparisons in advance in this manner.  
;;;
;;; The code in this function will first begin by generating the initial alist of 
;;; body entries to object.  It will then iterate over the problem solutions 
;;; collecting the sets of "top-level" body entries that occur in the different
;;; solutions.  These will be used to create a parallel list to the one above.
;;; that list will be used at runtime.  
;;;
;;; The code to build the list is similar to code located in NextStepHelp but NSH 
;;; needs to eliminate the nil sets.  Therefore it is somewhat replicated here.  
;;; This code does make use of the nsh-collect-principle-bodyent code as 
;;; needed.

(defun test-cache-solution-objects ()
  "Cache the body objects."
  ;; Before anything else is done test to ensure that the initial
  ;; bodyexp setup was handled properly.  if it was not then none
  ;; of the remainder will work.
  (when (null **Current-Body-Expression-Form**) 
    (error "no matching body entry fround in Ontology."))
  
  (let ((Bodies (test-cache-collect-bodies-list)))
    ;; If there are no bodies in the bodies list. 
    ;; then just set the list to nil.
    (if (null Bodies) (setq *test-cache-objects* '(()))

    ;; then associate the systementries with their objects.
    (let ((Alist (test-cbo-assoc-entries->objects))
	  (Len (length Bodies)))
      
      ;; Clear the objects list.
      (setq *test-cache-objects* (make-list Len))
      ;; Iterate over the entries extracting the object and storing the 
      ;; values in the appropriate list iff the list is unique.  
      (dotimes (N (length Bodies))
	(dolist (Entry (nth N Bodies))
	  (test-cbe-handle-Entry Entry N Alist)))))))


;;; In order to associate the object entries with the solutions we need
;;; to collect the set of all "top-level" objects that app[ear at the 
;;; "top-level" of the solution.  For speed's sake I will make use of 
;;; some code that is "internal" to the NextStepHelp system.  Note that
;;; in doing so I have connected the two libraries together in an 
;;; unelegant way.  In the future (when time permits) I will move that
;;; code to a distinct location to make the split more "clean".  For
;;; Now I will use it as-is.
(defun test-cache-collect-bodies-list ()
 ; remove optional entries from each solution's set of body entries
 (mapcar #'(lambda (set) (remove-if #'sg-systementry-optional-p set))
  (nsh-get-solution-bodies)))

;;; In order to associate the objects with their entries we need
;;; to iterate over the list of all systementries.  If a body 
;;; entry is found then we want to associate it with the object
;;; in the output list.  If an alist for the object already exists
;;; then the entry will be added to the list.  If it does not then 
;;; a new list will be added as necessary.
(defun test-cbo-assoc-entries->objects ()
  (let (Alist Binds Obj Curr)
    (dolist (Entry *Sg-Entries*)
      (setq Binds (unify (systementry-prop Entry) **Current-Body-Expression-Form**))
      (when Binds 
	(setq Obj (binding-val (get-binding '?Body Binds)))
	(setq Curr (find Obj Alist :key #'car :test #'equalp))
	(if Curr (push Entry (cdr Curr)) ;; No it really does work.
	  (push (list Obj entry) Alist))))
    Alist))


;;; For each entry in the bodies list we need to get the object that it reifies.
;;; We then need to add that object's entry list from the Alist to the current
;;; entry cache for later use.  If An Alist for the object already exists in 
;;; the cache list then we can go ahead and ignore it as we know the entry will
;;; already be there.  This must be done on a per-solution basis.
(defun test-cbe-handle-entry (Entry Index Alist)
  (let (A (Obj (test-cbe-get-object Entry)))
    (if (null Obj) (error "Body binding failed.")
      (when (not (member Obj (nth Index *test-cache-objects*)
			 :Key #'car :test #'equalp))
	(setq A (find Obj Alist :key #'car :test #'equalp))
	(push A (nth Index *test-cache-objects*))))))


;;; Obtain the object part of an entry for identification purposes.
(defun test-cbe-get-object (Entry)			    
  (binding-val 
   (get-binding '?body (unify (Systementry-prop Entry) 
			      **Current-Body-Expression-Form**))))


;;; -------------------------------------------------------------------
;;; Test solution-objects Solset updates.
;;; The code in this section is used to update a fractional sol-set that
;;; is defined on the *test-cache-objects* parameter.  Given a AolSet it
;;; it will iterate over the set in question and update the numerator of
;;; each fract based upon the number of objects in each of the tests that
;;; has been reified at least once.  This is written here to facilitate
;;; comparison.  

(defun test-cache-update-body-fract-solset (Score)
  "Update a SolSet based upon the body score."
  (dotimes (N (length *test-cache-objects*))
    (update-rt-solset-val
     Score
     :Index N
     :Func #'(lambda (Curr)
	       (declare (ignore Curr))
	       (loop for O in (nth N *test-cache-objects*)
		   count (member-if #'systementry-entered (cdr O)))))))
			
  


;;; -------------------------------------------------------------------
;;; Fract Solset Setup.
;;; The code in this section will be used to setup the fract solset
;;; solutions for use at runtime.  The code will generate a SolSet
;;; of the form: (:SolSet <S0> ... <SN>)  where each S is a Fract 
;;; or Fract1 solution list of the form: (<Type> 0 <D>)  Where 
;;; <Type> is one of :Fract or :Fract1 and D is an integer.
;;;
;;; These solsets will be updated at runtime to reflect the number of
;;; entries that the students have made and objects that they have 
;;; completed.  


;;; This utility function is used to access the caches and setup the 
;;; initial scores by solution.  All of the caches defined above are
;;; lists of entries that *must* (so to speak) be entered.  This code
;;; will take a single cache parameter as an argument and will return
;;; a SolSet of Fract tuples that correspond to the initial value.
;;(defun test-cache-setup-fract-SolSet (Cache)
;;  (cons :SolSet
;;	(loop for S in Cache
;;	    collect (list :Fract 0 (length S)))))


(defun test-cache-setup-fract-SolSet (Cache)
  (make-rt-solset
   (loop for C in Cache
       collect (make-rt-fract-num-score 0 (length C)))))



;;; This utility function is used to access the caches and setup the 
;;; initial scores by solution.  All of the caches defined above are
;;; lists of entries that *must* (so to speak) be entered.  This code
;;; will take a single cache parameter as an argument and will return
;;; a SolSet of Fract1 tuples that correspond to the initial value.
;;;
;;; NOTE:: Fract1 tuples are set s.t. at total calculation time 0/0=1
;;(defun test-cache-setup-fract1-SolSet (Cache)
;;  (cons :SolSet
;;	(loop for S in Cache
;;	    collect (list :Fract1 0 (length S)))))


;;; --------------------------------------------------------------------
;;; Test for nonanswer entries.
;;; Test the problem to determine if there are any non check-answer 
;;; entries within it and set the cache flag appropriately.  

(defun test-problem-nonanswer-entries ()
  "Test the problem for nonanswer entries."
  (if (remove-if 
       #'(lambda (S) (unify (Systementry-prop S) '(choose-answer ?a ?b)))
       *sg-entries*)
      (setq **Current-Prob-Has-nonanswer-entries** t)
    (setq **Current-Prob-Has-nonanswer-entries** Nil)))


;;;; =========================================================================
;;;; General Mergefunc code.

(defun mergefunc-pick-newest (Old New)
  "Return the Newest value."
  New)

;;; Sum the two values together.
(defun mergefunc-sum-values (Old New)
  "Return the summation of the values."
  (inc-rt-sum-score-value Old (rt-val-value New))
  Old)

;;;; =========================================================================
;;;; Answer Subscore
;;;; The answer subscore is used to compute how many of the required scores
;;;; that the student has entered.  This code will be used at runtime to 
;;;; set the number of required Answers that the student has made.  
;;;; This is a fractional score result and will be used (most likely) as
;;;; part of the students scores.  

;;; -----------------------------------------------------------------------
;;; Answer_Entries
;;; This code is used to calculate the number of Normal answers that the 
;;; student must enter at runtime.  This is a fractional value and will
;;; be used on all non-quant problems.

(defun art-count-correct-answer-ents (V)
  "Count the number of correct answer entries the student has made."
  (declare (ignore v))
  (length 
   (remove-if-not 
    #'(lambda (E) (and (equalp (studententry-state E) **correct**)
		       (equalp (car (studententry-prop E)) 'Answer)))
    *Studententries*)))




(add-runtime-test
 Answer_Entry_Subscore
 :PrintStr "Answer_Entry_Subscore"
 :ValType rt-fract-num-score
 :Func #'(lambda (X) (update-rt-score-value X #'art-count-correct-answer-ents))
 :InitFunc #'(lambda () 
	       (make-rt-fract-num-score 
		0 (length (remove-if-not #'quantity-expression-p (problem-soughts *cp*)))))
 :Weight 0.20
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (member-if #'Quantity-expression-p (problem-soughts *cp*))))
;;		      (not (no-quant-problem-p *cp*))))
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )




;;; ----------------------------------------------------------------------
;;; MC-answers
;;; This test is used to keep track of the number of mc-answers that 
;;; the student has entered.  MC-answers are multiple-choice dialogs
;;; that (at present) are used on no-quant problems.  The student is
;;; required to select a check-box to indicate whether or not they
;;; are done with the problem.  
;;;
;;; Because the mc-answers are submitted (at present) as only on-off
;;; checkboxes then it is impossible to activate them unless they
;;; are first deactivated.  If the student selects an mc-answer to 
;;; indicate that they are done it will be a color-red or color-green
;;; turn.  Because the answers are on-off they must uncheck the 
;;; turn before checking it again.  
;;;
;;; This code counts the number of soughts in the problem (there should
;;; be one per mc-answer) to define the denominator.  It also keeps track
;;; of the number of correct mc-answer submissions that the student has 
;;; made, decrementing the count each time an incorrect or color-black
;;; response is made.  

(defun count-correct-mc-answer-subscore (Count)
  (declare (ignore Count))
  (length 
   (remove-if-not 
    #'(lambda (E) (and (equalp (studententry-state E) **correct**)
		       (equalp (car (studententry-prop E)) 'lookup-mc-Answer)))
    *Studententries*)))

   
;;(cond ((not (equalp (car (cmd-call **current-cmd**)) 'lookup-mc-answer)) Count)
;;((correct-cmdp **current-cmd**) (+ 1 Count))
;;(t (if (= 0 Count) 0 (- Count 1)))))
	
(add-runtime-test
 MC_Answer_Entry_Subscore
 :PrintStr "MC_Answer_Entry_Subscore"
 :ValType rt-solset
 :Func #'(lambda (X) (update-rt-score-value X #'count-correct-mc-answer-subscore))
 :InitFunc #'(lambda () 
	       (make-rt-fract-num-score 
		0 (length 
		   (remove-if-not 
		    #'(lambda (S) 
			(and (not (quantity-expression-p S))
			     (not (equalp (car S) 'choose-answer))))
		    (problem-soughts *cp*)))))
 :Weight 0.20
 :CreditType Credit
 :ActiveCond #'(lambda ()
		 (and (problem-loadedp)
		      (member-if #'(lambda (S) 
				     (and (not (quantity-expression-p S))
					  (not (equalp (car S) 'choose-answer))))
				 (problem-soughts *cp*))))
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )


;;; ----------------------------------------------------------------------
;;; MC-problems
;;; Multiple choice problems are problems in which the students are given 
;;; a set of radio-byutton answers to define.  Our goal in these problems
;;; is to have them answer general or conceptual questions about the 
;;; topic at hand.  
;;;
;;; This test will keep track of the number of multiple choice problems 
;;; that they have solved correctly within the problem.  These are the 
;;; distinct entries for which a correct entry has been made.  
;;;
;;; This code counts the number of soughts in the problem (there should
;;; be one per mc-question) to define the denominator.  It also keeps 
;;; track of the number of correct mc-answer submissions that the student 
;;; has made, decrementing the count each time an incorrect or color-black
;;; response is made.  

(defun count-correct-multiple-choice-answer-subscore (Count)
  (declare (ignore Count))
  (length 
   (remove-if-not 
    #'(lambda (E) (and (equalp (studententry-state E) **correct**)
		       (equalp (car (studententry-prop E)) 'Choose-Answer)))
    *Studententries*)))


(add-runtime-test
 Multiple_Choice_Answer_Entry_Subscore
 :PrintStr "Multiple_Choice_Answer_Entry_Subscore"
 :ValType rt-solset
 :Func #'(lambda (X) 
	   (update-rt-score-value 
	    X #'count-correct-multiple-choice-answer-subscore))
 
 :InitFunc #'(lambda () 
	       (make-rt-fract-num-score 
		0 (length 
		   (remove-if-not 
		    #'(lambda (S) (equalp (car S) 'choose-answer))
		    (problem-soughts *cp*)))))
 :Weight 0.25
 :CreditType Credit
 :ActiveCond #'(lambda ()
		 (and (problem-loadedp)
		      (member-if #'(lambda (S) (equalp (car S) 'choose-answer))
				 (problem-soughts *cp*))))
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )





;;;; =========================================================================
;;;; Equation Subscore.
;;;; The student's equation subscore is based upon the number of required
;;;; equations that they write and the number of that set that are written
;;;; "explicitly".  In order to asess this we need to determine how many
;;;; equations of each type are necessary for each solution set, and then 
;;;; to determine which among those we want the students to use.  


;;; --------------------------------------------------------------------------
;;; Count the number of Equations that the student has written 
;;; versus the number that they need to for each solution set.  This gets
;;; stored as a single set list the result of which is taken later.
;;;
;;; This test is only active if there are eqn-entries to be made.
;;; If not then it will be inactive and the score will be scaled 
;;; appropriately.

(add-runtime-test
 Equation_entry_Subscore
 :PrintStr "Equation_entry_Subscore"
 :ValType rt-solset

 :Func #'(lambda (S) 
	   (test-cache-update-fract-SolSet
	    S *test-cache-eqn-entries*))

 :InitFunc #'(lambda ()
	       (test-cache-setup-fract-solset
		*test-cache-eqn-entries*))
 :Weight 0.20
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (car *test-cache-eqn-entries*)))
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )



;;; --------------------------------------------------------------------------
;;; Count the number of Equations that the student has written explicitly
;;; versus the number that they need to for each solution set.  This gets
;;; stored as a single set list the result of which is taken later.

(add-runtime-test
 Explicit_Equation_Entry_Subscore
 :PrintStr "Explicit_Equation_Entry_Subscore"
 :ValType rt-solset
 
 :Func #'(lambda (S) 
	   (test-cache-test-update-fract-SolSet
	    S #'entered-explicitly 
	    *test-cache-eqn-entries*))

 :InitFunc #'(lambda ()
	       (test-cache-setup-fract-solset
		*test-cache-eqn-entries*))

 :Weight 0.15
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (car *test-cache-eqn-entries*)))
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )



;;; ---------------------------------------------------------------------------
;;; Count the number of given-equations that the student has written in some
;;; form or another.

(add-runtime-test
 Given_Equation_Entry_Subscore
 :PrintStr "Given_Equation_Entry_Subscore"
 :ValType rt-solset

 :Func #'(lambda (S) 
	   (test-cache-update-fract-SolSet
	    S *test-cache-given-eqn-entries*))
 
  :InitFunc #'(lambda ()
	       (test-cache-setup-fract-solset
		*test-cache-given-eqn-entries*))

 :Weight 0.10
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (car *test-cache-given-eqn-entries*))) 
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )


;;; --------------------------------------------------------------------------
;;; Count the number of Equations that the student has written explicitly
;;; versus the number that they need to for each solution set.  This gets
;;; stored as a single set list the result of which is taken later.

(add-runtime-test
 Explicit_Given_Equation_entry_Subscore
 :PrintStr "Explicit_Given_Equation_entry_Subscore"
 :ValType rt-solset
 
 :Func #'(lambda (S) 
	   (test-cache-test-update-fract-SolSet
	    S #'entered-explicitly 
	    *test-cache-given-eqn-entries*))
 
 :InitFunc #'(lambda ()
	       (test-cache-setup-fract-solset
		*test-cache-given-eqn-entries*))
 
 :Weight 0.05
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (car *test-cache-given-eqn-entries*))) 
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )



;;; -----------------------------------------------------------------
;;; Body Entry Subscore.
;;; Every Andes problem solution contains a list of required bodies 
;;; that are used in the top-level principle equations.  These are 
;;; considered to be the "necessary objects" within the solution.  
;;; When working on the problem the physicists want the students to 
;;; identify these objects explicitly using the body tool in order to
;;; reify their importance in the solution.  
;;;
;;; The test code in this section is used to maintain a fractional 
;;; score reflecting the number of objects that the student has 
;;; reified at runtime.  
;;;
;;; For historical reasons the body entries contain a time component.
;;; For the purposes of this grade we do not want to force the students
;;; to define a separate body at each time component.  Therefore this 
;;; code will test whether or not they have defined a body for each 
;;; of the necessary oabject at *any* time.  If they have defined an
;;; entry for a body object at at least one time point then they will
;;; be able to make the entry.
;;;
;;; The test in this section makes use of the body entry test code
;;; that was defined above.

(add-runtime-test
 Body_Entry_Subscore10
 :PrintStr "Body_Entry_Subscore"
 :ValType rt-solset
 
 :Func #'test-cache-update-body-fract-solset

 :InitFunc #'(lambda ()
	       (test-cache-setup-fract-solset
		*test-cache-objects*))

 :Weight 0.10
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (car *test-cache-objects*))) 
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )




;;; ----------------------------------------------------------------------------
;;; Count the percentage of Axers entries that the student has made. 

(add-runtime-test
 Axis_Entry_Subscore
 :PrintStr "Axis_Entry_Subscore"
 :ValType rt-solset
 
 :Func #'(lambda (S) 
	   (test-cache-update-fract-SolSet
	    S *test-cache-axis-entries*))

 :InitFunc #'(lambda ()
	       (test-cache-setup-fract-solset
		*test-cache-axis-entries*))

 :Weight 0.05
 :CreditType Credit
 :ActiveCond #'(lambda () 
		 (and (problem-loadedp) 
		      (car *test-cache-axis-entries*))) 
 :Loadable Nil
 :MergeFunc #'mergefunc-pick-newest
 )
 

;;;; =========================================================================
;;;; Help Calls
;;;; The code in this section calculates subscores for help use, and help abuse.

;;; --------------------------------------------------------------------
;;; NSH calls.  
;;; This is simply a literal count of the number of times NSH was called.

(add-runtime-test
 NSH_Call_Count
 :PrintStr "NSH_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S) 
	   (if (get-proc-help-cmdp **current-cmd**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
 )


;;; --------------------------------------------------------------------
;;; NSH Bottom-Out-Hint calls.  
;;; This is simply a literal count of the number of times NSH was called and
;;; the student went to the bottom-out-hint.

(add-runtime-test
 NSH_BO_Call_Count
 :PrintStr "NSH_BO_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (proc-bottom-out-hintp **Current-cmd-Stack**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :Weight -0.05
 :CreditType Debit
 :MergeFunc #'mergefunc-sum-values
 )


;;; --------------------------------------------------------------------
;;; NSH BO over NSH  
;;; Count the number times the student has bottomed-out NSH over
;;; the number of times that they have called NSH.

;;; If the current cmd is a call to get-proc-help then increment the 
;;; denominator.  If not then increment the numerator.
(defun art-nshbovnsh-test (Score)
  (cond ((get-proc-help-cmdp **Current-Cmd**)
	 (inc-rt-sum-score-value Score '(0 1)))
	((proc-bottom-out-hintp **Current-CMD-Stack**)
	 (inc-rt-sum-score-value Score '(1 0)))))

(add-runtime-test
 NSH_BO_V_NSH
 :PrintStr "NSH_BO_V_NSH"
 :ValType rt-fract-sum-score
 :Func #'art-nshbovnsh-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :MergeFunc #'mergefunc-sum-values
 )



;;; ------------------------------------------------------------------
;;; WWH Use.
;;; This is a count of how often the students made use of WWH

(add-runtime-test
 WWH_Call_Count
 :PrintStr "WWH_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (why-wrong-cmdp **Current-cmd**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------
;;; WWH BO_Use.
;;; This is a count of how often the students made use of WWH and
;;; went to the bottom-out-hints.

(add-runtime-test
 WWH_BO_Call_Count
 :PrintStr "WWH_BO_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (wwh-bottom-out-hintp **Current-cmd-Stack**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :Weight -0.05
 :CreditType Debit
 :MergeFunc #'mergefunc-sum-values
 )




;;; ------------------------------------------------------------------
;;; WWH BO/WWH Use.
;;; Count the number of times that they bottomed-out on WWH over
;;; the number of times that they called WWH.

;;; If the current call is a call to WWH then increment 
;;; the denominator else increment the numerator.
(defun art-wwhbo-wwh-test (Score)
  (cond ((why-wrong-cmdp **Current-Cmd**)
	 (inc-rt-sum-score-value Score '(0 1)))
	((wwh-bottom-out-hintp **Current-CMD-Stack**)
	 (inc-rt-sum-score-value Score '(1 0)))))

(add-runtime-test
 WWH_BO_V_WWH
 :PrintStr "WWH_BO_V_WWH"
 :ValType rt-fract-sum-score
 :Func #'art-wwhbo-wwh-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :MergeFunc #'mergefunc-sum-values
 )


;;; -------------------------------------------------------------------
;;; WWO Use.  
;;; How often did the student ask for help on an Object.

(add-runtime-test
 WWO_Call_Count
 :PrintStr "WWO_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (why-wrong-obj-cmdp **Current-cmd**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------
;;; WWO BO_Use.
;;; This is a count of how often the students made use of WWO and
;;; went to the bottom-out-hints.

(add-runtime-test
 WWO_BO_Call_Count
 :PrintStr "WWO_BO_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (wwo-bottom-out-hintp **Current-cmd-Stack**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------
;;; WWO BO/WWO Use.
;;; Count the number of times that the student has bottomed-out on 
;;; WWO help over the number of times that they have called WWO help.

;;; If the current call is a call to WWO then increment 
;;; the denominator if a wwo bottom out call then increment
;;; the numerator.
(defun art-wwobo-wwo-test (Score)
  (cond ((why-wrong-obj-cmdp **Current-Cmd**)
	 (inc-rt-sum-score-value Score '(0 1)))
	((wwo-bottom-out-hintp **Current-CMD-Stack**)
	 (inc-rt-sum-score-value Score '(1 0)))))

(add-runtime-test
 WWO_BO_V_WWO
 :PrintStr "WWO_BO_V_WWO"
 :ValType rt-fract-sum-score
 :Func #'art-wwobo-wwo-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 ;;:Weight 0
 :MergeFunc #'mergefunc-sum-values
 )


;;; -------------------------------------------------------------------
;;; WWE Use.  
;;; How often did the student ask for help on an Equation.

(add-runtime-test
 WWE_Call_Count 
 :PrintStr "WWE_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (why-wrong-eqn-cmdp **Current-cmd**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------
;;; WWE BO_Use.
;;; This is a count of how often the students made use of WWE and
;;; went to the bottom-out-hints.

(add-runtime-test
 WWE_BO_Call_Count
 :PrintStr "WWE_BO_Call_Count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (wwe-bottom-out-hintp **Current-cmd-Stack**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
 )



;;; ------------------------------------------------------------------
;;; WWE BO/WWE Use.
;;; Count the number of times that the student has bottomed-out on 
;;; WWE help over the number of times that they have called WWE help.

;;; If the current call is a call to WWE then increment 
;;; the denominator if a wwe bottom out call then increment
;;; the numerator.
(defun art-wwebo-wwe-test (Score)
  (cond ((why-wrong-eqn-cmdp **Current-Cmd**)
	 (inc-rt-sum-score-value Score '(0 1)))
	((wwe-bottom-out-hintp **Current-CMD-Stack**)
	  (inc-rt-sum-score-value Score '(1 0)))))

(add-runtime-test
 WWE_BO_V_WWE
 :PrintStr "WWE_BO_V_WWE"
 :ValType rt-fract-sum-score
 :Func #'art-wwebo-wwe-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :MergeFunc #'mergefunc-sum-values
 )



;;; ------------------------------------------------------------------------
;;; Unsolicited help.
;;; Count the number of times that the students were given unsolicited help.

(add-runtime-test
 Unsol_Help_count
 :PrintStr "Unsol_Help_count"
 :ValType rt-int-sum-score
 :Func #'(lambda (S)
	   (if (unsolicited-hint-cmdp **Current-cmd**)
	       (inc-rt-sum-score-value S)))
 :InitFunc #'make-rt-int-sum-score
 :MergeFunc #'mergefunc-sum-values
  )


;;;; ========================================================================
;;;; Floundering
;;;; The tests in this section are meant to cover general problem-solving
;;;; tasks such as the number of entries that the student made overall, 
;;;; the number incorrect, etc.  The goal here is to form a picture of 
;;;; how the student did their work.  In future these values (like help use)
;;;; will persist from session to session but they do not yet.  

;;; A little utility function for incrementing 
;;; predicate based count results. 
(defun art-cmd-predicate-inc (Predicate Score)
  (when (funcall Predicate **Current-Cmd**)
    (inc-rt-sum-score-value Score)))


;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student made an entry.

(add-runtime-test
 Num_Entries 
 :PrintStr "Num_Entries"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'entry-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond  #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student made a correct entry.

(add-runtime-test
 Num_Correct_Entries
 :PrintStr "Num_Correct_Entries"
 :ValType rt-int-sum-score
 :Func #'(lambda (S) (art-cmd-predicate-inc #'correct-entry-cmdp S))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student made an incorrect entry.

(add-runtime-test
 Num_Incorrect_Entries
 :PrintStr "Num_Incorrect_Entries"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'incorrect-entry-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp  
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the ratio of correct entries to total 
;;; entries that the student made.  In order for this test to be viable 
;;; we need to ensure that there is at least one non-answer entry that
;;; the student can make.  The only problems on which this is an issue 
;;; are those where the student is asked to select from several multiple
;;; choice questions.  

(defun art-correctent-ent-test (Score)
  "The Correct entries v entries overall."
  (when (entry-cmdp **Current-Cmd**)
    (if (correct-cmdp **Current-Cmd**)
	(inc-rt-sum-score-value Score '(1 1))
      (inc-rt-sum-score-value Score '(0 1)))))

(add-runtime-test
 Correct_Entries_V_Entries
 :PrintStr "Correct_Entries_V_Entries"
 :ValType rt-fract-sum-score
 :Func #'art-correctent-ent-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :Weight 0.05
 :ActiveCond #'(lambda () 
		 (and *cp* (not-curr-checking-problemp)
		      **Current-Prob-Has-nonanswer-entries**))
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student made an eqn-entry.

(add-runtime-test
 Num_Eq_Entries
 :PrintStr "Num_Eq_Entries"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'eq-entry-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the ratio of correct eq-entries to total 
;;; eq-entries that the student has made.

(defun art-correcteqent-eqent-test (Score)
  "The Correct entries v entries overall."
  (when (eq-entry-cmdp **Current-Cmd**)
    (if (correct-cmdp **Current-Cmd**)
	(inc-rt-sum-score-value Score '(1 1))
      (inc-rt-sum-score-value Score '(0 1)))))

(add-runtime-test
 Correct_EQ_Entries_V_EQ_Entries
 :PrintStr "Correct_EQ_Entries_V_EQ_Entries"
 :ValType rt-fract-sum-score
 :Func #'art-correcteqent-eqent-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student made a noneq-entry.

(add-runtime-test
 Num_noneq_Entries
 :PrintStr "Num_noneq_Entries"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'noneq-entry-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the ratio of correct noneq-entries to total 
;;; noneq-entries that the student has made.

(defun art-correctnoneqent-noneqent-test (Score)
  "The Correct entries v entries overall."
  (when (noneq-entry-cmdp **Current-Cmd**)
    (if (correct-cmdp **Current-Cmd**)
	(inc-rt-sum-score-value Score '(1 1))
      (inc-rt-sum-score-value Score '(0 1)))))

(add-runtime-test
 Correct_NonEQ_Entries_V_NonEQ_Entries
 :PrintStr "Correct_NonEQ_Entries_V_NonEQ_Entries"
 :ValType rt-fract-sum-score
 :Func #'art-correctnoneqent-noneqent-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :ActiveCond #'not-curr-checking-problemp
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student made an answer-entry.

(add-runtime-test
 Num_Answer_Entries
 :PrintStr "Num_Answer_Entries"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'answer-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )

;;; ------------------------------------------------------------------------
;;; Calculate the ratio of correct answer-entries to total 
;;; answer-entries that the student has made.

(defun art-canswerents-answerents-test (Score)
  "The Correct entries v entries overall."
  (when (answer-cmdp **Current-Cmd**)
    (if (correct-cmdp **Current-Cmd**)
	(inc-rt-sum-score-value Score '(1 1))
      (inc-rt-sum-score-value Score '(0 1)))))

(add-runtime-test
 Correct_Answer_Entries_V_Answer_Entries
 :PrintStr "Correct_Answer_Entries_V_Answer_Entries"
 :ValType rt-fract-sum-score
 :Func #'art-canswerents-answerents-test
 :initFunc #'(lambda () (make-rt-fract-sum-score 0 0))
 :Weight 0.05
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student used an algebra
;;; command

(add-runtime-test
 Num_Algebra_calls
 :PrintStr "Num_Algebra_calls"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'algebra-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )


;;; ------------------------------------------------------------------------
;;; Calculate the number of times that the student deleted an entry
;;; command

(add-runtime-test
 Num_Deletions
 :PrintStr "Num_Deletions"
 :ValType rt-int-sum-score
 :Func #'(lambda (X) (art-cmd-predicate-inc #'delete-cmdp X))
 :InitFunc #'make-rt-int-sum-score
 :ActiveCond #'not-curr-checking-problemp ;; Will be uncom,mented once score storage is up.
 :MergeFunc #'mergefunc-sum-values
 )






;;;; ========================================================================
;;;; Time
;;;; For the purposes of testing we also want to keep track of the amount
;;;; of time that the student has taken irrespective of "pause time" and 
;;;; counting "pause" time.  This should persist from state to state.

;;; -------------------------------------------------------------------------
;;; Keep track of the amount of time that the student has spent working
;;; on this problem.  This is complicated by the fact that the student 
;;; will be pausing and so on so it is necessary to take something other
;;; than the current time.  Therefore this code will store the total time
;;; and, each time it is called add the time between the current cmd and
;;; the previous one.  This may seem odd but once we start loading saved 
;;; times it will make more sense.

;;; If there is only one command in the stack then the student has just
;;; opened the problem and we therefore need to just return the total.
;;; If not then we subtract the times appropriately.
(defun art-add-last-time (Score)
  "add the time between curr-cmd and previous one to Tot and return."
  (when (nth 1 **current-cmd-stack**) 
    (inc-rt-sum-score-value 
     Score 
     (sub-htimes
      (cmd-time **Current-Cmd**)
      (cmd-time (nth 1 **Current-Cmd-Stack**))))))
   

(add-runtime-test
 Total_Open_Time
 :PrintStr "Total_Open_Time"
 :ValType rt-htime-sum-score
 :Func #'art-add-last-time
 :InitFunc #'make-rt-htime-sum-score 
 :ActiveCond #'not-curr-checking-problemp
 :MergeFunc #'mergefunc-sum-values
 )


;;; -------------------------------------------------------------------------
;;; Keep track of the amount of time that the student has spent working
;;; on this problem skipping what we consider to be "pause" time.  That is
;;; if the time between the current cmd and the previous one is greater 
;;; than our pause threshold then we want to ignore it.  If not then we
;;; will count the time.  



;;; If there is only one command in the stack then the student has just
;;; opened the problem and we therefore need to just return the total.
;;; If not then we subtract the times appropriately.  In this case 
;;; adding any time above pause time.

(defun art-add-last-non-pause-time (Score)
  "Add the time between curr-cmd and previous one to Tot and return."
  (when (nth 1 **current-cmd-stack**)
    (let ((Time (sub-htimes
		 (cmd-time **Current-Cmd**)
		 (cmd-time (nth 1 **Current-Cmd-Stack**)))))
      (if (htimes< Time **Testing-Pause-time-Threshold**)
	  (inc-rt-sum-score-value Score Time)))))



(add-runtime-test
 Total_Non_Pause_Open_Time
 :PrintStr "Total_Non_Pause_Open_Time"
 :ValType rt-htime-sum-score
 :Func #'art-add-last-non-pause-time
 :InitFunc #'make-rt-htime-sum-score 
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )




;;; -------------------------------------------------------------------------
;;; Keep track of the amount of pause time that the student has spent.  That
;;; is if the time between the current cmd and the previous one is greater
;;; than the pause threshold then add it.  If not then don't.

;;; If there is only one command in the stack then the student has just
;;; opened the problem and we therefore need to just return the total.
;;; If not then we subtract the times appropriately.  In this case 
;;; adding any time above pause time.
(defun art-add-last-pause-time (Score)
  "add the time between curr-cmd and previous one to Tot and return."
  (when (nth 1 **current-cmd-stack**)
    (let ((Time (sub-htimes
		 (cmd-time **Current-Cmd**)
		 (cmd-time (nth 1 **Current-Cmd-Stack**)))))
      (if (htimes< **Testing-Pause-time-Threshold** Time)
	  (inc-rt-sum-score-value Score Time)))))

(add-runtime-test
 Total_Pause_Open_Time
 :PrintStr "Total_Pause_Open_Time"
 :ValType rt-htime-sum-score
 :Func #'art-add-last-pause-time
 :InitFunc #'make-rt-htime-sum-score 
 :ActiveCond #'not-curr-checking-problemp 
 :MergeFunc #'mergefunc-sum-values
 )





;;; =========================================================================
;;; Initialization code.
;;; Initialize the scores of the newly loaded tests.
(sort-runtime-tests-by-weight)
(reset-runtime-testset-scores)


