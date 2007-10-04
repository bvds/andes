#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics.cl
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

|#


;;; Most of the code to operate this file is actually located in other parts
;;; this file only defines one function, that is the on-get-stats function
;;; that is used to handle the statistical info.  I may just move this to 
;;; State later on.

;;; Return the runtime stats.  In a Stat turn with the 
;;; Runtime tests requested in the values.  This uses
;;; the map-tests->listresults to translate the list 
;;; of testsets into a list of 3-tuples for testing
;;; this may be changed later.
(defun on-stats-get-stats (Type)
  (make-stat-turn
   (map-tests->list-results
    ; set to map:
    (cond ((eq Type 'score) (collect-runtime-score-tests))
          ((eq Type 'persist) (collect-persistent-score-tests))
	  (T (collect-all-runtime-tests)))
    ; use friendly name for score, else internal id
    :name-fn (if (eq Type 'score) #'runtime-test-PrintStr 
               #'runtime-test-name))))
