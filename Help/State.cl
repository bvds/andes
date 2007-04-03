;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State.cl
;; Collin Lynch (CL) <CollinL@pitt.edu>
;; Anders Weinstein (AW) <AndersW@pitt.edu>
;; Lynwood Taylor (LHT) <lht@lzri.com>
;; 4/20/2001
;;
;; This file defines basic state storage functions and
;; state information for the help system.  
;;
;; Changelog:
;; 3/12/2003 - (CL) -- Commented out Collect-useful-givens and 
;;  collect-useful-nodes as they were unused.
;; 8/11/2003 - (CL) -- Added in Done flag to close-problem.
;;

;;========================================================
;; Storage elements.

(defvar *cp* Nil)                  ; the current problem
(defvar *StudentEntries* NIL)  ; list of current student entries

;;; The Andes2 Configuration file is a lisp-source file that is
;;; loaded (and evaluated) at runtime.  This file is intended to
;;; set parameters and make any modifications that are necessary
;;; but cannot be hardcoded into the distribution.  
(defparameter **Config-File-Name** "Config.cl")

;;; Checking entries is a global flag that is used to indicate
;;; whether or not the workbench is currently sending saved entries
;;; to the help system.  If it is t then any entry that comes in is
;;; assumed to be a saved entry that is being sent automatically.  If
;;; it is nil then it is assumed to be a user entry.
(defparameter **Checking-Entries** Nil)

;;; The Session ID is a value that is sent by the Workbench when it 
;;; loggs in.  This ID consists of a date-time pair in the following
;;; format: <Month><DAY>-<HR>-<Min>-<Sec>  
;;; Where: 
;;;  <Month> is a 3-character string listing the month name (Aug, Sep, etc.)
;;;  <Day> is a 2-character day format (01 - 31)
;;;  <Hr> is a 2-character hour representation (0-23)
;;;  <Min> is a 2-digit integer minute representation (0-59)
;;;  <Sec> is a 2-digit Second representation (0-59).
(defvar *Current-Andes-Session-ID* Nil "The current Session ID.")

;;; The current Andes Session Start UTime is an encoded universal time
;;; that is used to timestamp the Scores for storage and for later
;;; sorting of the scores.  
;;;
;;; For now this value is parsed from The Session ID although that 
;;; may change at a later date.
(defparameter *Current-Andes-Session-Start-Utime* Nil "The start time.")

;;; The current andes session start date is a listing of the date 
;;; that the session was started not the current date at any point 
;;; in time.  This is maintained becuase we want to tie data to a 
;;; single session, and the students have shown their willingness 
;;; to leave a single andes session running for more than 24 hours.  
;;; This allows us to link sessions by date.  
;;;
;;; This value will be set form the session ID for now although it may 
;;; change later.
;;(defvar *Current-Andes-Session-Start-Date* Nil "The current Session Date.")

;;; The Current Andes session start time is an htime taken from the 
;;; Andes session iD representing the time of day that the current 
;;; Andes session was begun.  This value is not used at present but 
;;; is calculated as necessary.
;;(defvar *Current-Andes-Session-Start-Time* Nil "The current Session Start time.")

;;; Problem Instance Time.
;;; Whenever the student starts andes, opens a new problem, or closes a problem
;;; then they are beginning a new "problem instance".  This instance represents
;;; a single session of work on a specified problem.  When no problem is open 
;;; (following a close problem or before any problem is opened) then the 
;;; problem in question is Nil.  
;;;
;;; The purpose of maintaining the problem instance times is to make it 
;;; possible for the statistics to be sorted efficiently.  This variable is 
;;; used to store the current problem instance time when it is created for 
;;; later access.  The value could be pulled from the cmd stack but this is 
;;; more efficient.
;;;
;;; This value will be set here and read by the problem storage code in 
;;; runtimetest.cl
(defvar *Current-Problem-Instance-Start-UTime* Nil "The Current PITime.")




;------------------------------------------------------------------------------
; Overall Session structure bracketed as follows:
;
;  Read-student-info -- begin andes session with given student
;  +-> Read-problem-info -- open new problem
;  |
;  |       <entry, help, calculate API calls>
;  |
;  |   Close-problem     -- close currently open problem
;  +------+
;  Exit-Andes        -- End Andes session
;------------------------------------------------------------------------------


;;; ===========================================================================
;;; State API calls.

;;=============================================================================
;; Session Control Info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-session-id
;; Argument(s): session id
;; returns: nil for failure, non-nil for success.
;; note(s):
;;   The Session id is, at present a flag that we will use to identify 
;;   individual sessions within Andes.  It is used to store the score values 
;;   and to set other test information.  In time it may take on some sort of 
;;   logging role for use in identification and secure testing but not yet.
;;
;;   Initially the Current-Andes-Session-Start-Utime was set using the contents of
;;   the session id.  This is no longer the case as it was deemed necessary to make
;;   the values link up to the problem instance times which are set by the help 
;;   system.  
;;
;;   In the future the API may be altered so that the workbench time is supplied 
;;   to the help system api along with several commands.  This will be used to 
;;   maintain a link between the internal scoring and the workbench info.
;; 
;;   Note:: We may need more security later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-set-session-id (&key (SessionID Nil))
  "Set the session ID."
  (setq *Current-Andes-Session-ID* SessionID)
  (setq *Current-Andes-Session-Start-UTime*
    (get-universal-time))
  ;;(state-parse-sessionid->date SessionID))
  (setq *Current-Problem-Instance-Start-UTime*
    *Current-Andes-Session-Start-UTime*))


;;; Given the remainder of the Session ID including the date and time
;;; Encode a universal time based upon it's contents and return the 
;;; value.  The Year will be taken from the current time Since that is
;;; not sent as part of the solution.  
(defun state-parse-sessionid->date (SessionID)
  "Parse the sessionID into a date."
  (let (Pos Pos1 Month Day Year Hour Min Sec)
        
    ;; Start by parsing the Month and Date string by
    ;; locating the hyphens that bracket the MMMDD subseq
    ;; and reading from it.
    (setq Pos (position #\- SessionID))
    (pprint Pos)
    (setq Pos1 (position #\- SessionID :start (+ 1 Pos)))
    (pprint Pos1)
    (setq Month (state-parse-si->date-mo (subseq SessionID (+ 1 Pos) (+ 4 Pos))))
    (setq Day (read-from-string (subseq SessionID (+ 4 Pos) Pos1)))
    
    ;; Then generate the year from a current decoded time since 
    ;; it is not sent in with the session ID>
    (setq Year (nth 5 (multiple-value-list (get-decoded-time))))
    
    ;; Then parse the individual HH-MM-SS components from the
    ;; tail of the string.  
    (incf Pos1)
    (setq Pos (+ 2 Pos1))
    (setq Hour (read-from-string (subseq SessionID Pos1 Pos)))
    (incf Pos)
    (setq Pos1 (+ 2 Pos))
    (setq Min (read-from-string (subseq SessionID Pos Pos1)))
    (setq Sec (read-from-string (subseq SessionID (+ 1 Pos1))))
    
    ;; Encode the result value.
    (encode-universal-time Sec Min Hour Day Month Year)))


;;; Parse the incoming Month value translating the 3-char
;;; string into an appropriate integer.
(defun state-parse-si->date-mo (MonthStr)    
  (case (read-from-string MonthStr)
    (JAN 1)
    (FEB 2)
    (MAR 3)
    (APR 4)
    (MAY 5)
    (JUN 6)
    (JUL 7)
    (AUG 8)
    (SEP 9)
    (OCT 10)
    (NOV 11)
    (DEC 12)
    (t (error "Unrecognized month: ~a" MonthStr))))
    
;;==============================================================================
;; Student control info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-student-info
;; argument(s): student name, optional condition
;; returns: NIL for failure, non-NIL for success
;; note(s): Old ConcHelp arg now used to set experimental condition id
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-read-student-info (name &optional (Conchelp Nil))
  (declare (ignore Conchelp))
  (set-student-name name)
  ;; Lisp errors within API calls like this one are caught by the API call
  ;; dispatcher,  causing a special "call failed" signal to be returned to
  ;; the workbench. However, the workbench treats failure on this call to mean
  ;; the help system has not initialized and is unavailable. (Maybe this 
  ;; should be changed?) So don't want to return call-failed if we can in fact 
  ;; continue, perhaps without student information, so don't let Lisp errors
  ;; propagate out of this routine. !!! Might want to communicate warning 
  ;; message in case of failure to load student file, though.

  ;; Don't fail call if fail to load student file. 
  (safe-apply 'StudentFile-load (list name))
   
  ;; we want to load the config file after student name is known, so it can
  ;; include customizations based on student name (used in some experiments).
  (safe-apply 'Load-Config-File)			

  ;; color-green result signals success
  (make-green-turn))			


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem Control info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-problem-info open a new problem
;; argument(s): problem id
;; returns: NIL for failure, non-NIL for success
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-read-problem-info (name &optional kb-type (bn-alg 0))
  (declare (ignore kb-type bn-alg))
  ; reset run-time data structures for new problem:
  (parse-initialize) 	;; re-initialize parser
  (symbols-reset)   	;; clear out symbol table
  (clear-entries)	;; clear out student entry list
  ;; use problem name as seed for random elt
  (initialize-random-elt (string-downcase name)) 

  ;; Set the Problem Instance time for this work on the problem.
  (setq *Current-Problem-Instant-Start-UTime* (get-universal-time))
  
  ;; Clear out the student's actions. This may change later. 
  (setq *studentactions* nil) 
  
  ;; clear flag for detecting when problem done
  (reset-done-flag)
  
  ;; Load the current problem and set into global *cp* 
  ;; NB: for case-sensitive filesystems, ensure we convert the problem name, 
  ;; passed as a string, to canonical upper case used for problem ids.
  (setf *cp* (read-problem-file (string-upcase name) :path (andes-path "Problems/")))
  ;; If the problem failed to load then we will submit a color-red turn
  ;; to the workbench in order to make the case known.  If not then the 
  ;; code will set up the problem for use and then return a color-green
  ;; turn.  Previously we returned NIL in the case of failue and relied
  ;; upon the server code to recognize it as such.  Using the color-red
  ;; turn is more pedagogically sound.
  (if (null *cp*) (make-red-turn)
    (do-read-problem-info-setup)))


;; Once the problem has been loaded successfully into the *cp* parameter
;; then we need to setup the struct for runtime use.  This code will do 
;; that and conclude by returning a color-green-turn.
(defun do-read-problem-info-setup ()
  "Setup the loaded problem."
  (format *debug-help* "Current Problem now ~A~%" (problem-name *cp*))
  
  ;; Initialize sg structures
  (sg-setup *cp*)
  ;;(format T "~&Solution Entries:~%~{~A~}" *sg-entries*)
  
  ;; enter appropriate predefined student labels into symbol table: 
  (enter-predefs)
  
  ;; re-initialize the dialog state
  (reset-next-step-help)
  
  ;; return T for success. 
  (make-green-turn))  ;; Return a color green result.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-problem -- close the specified problem 
;; argument(s): 
;;  Name:  The problem id
;;  Done:  If t singifies that the student is done with the problem and that
;;    we should give them a message dealing with their scores.
;; returns: unused
;; note(s): should be current problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-close-problem (name Done)
   (declare (ignore name)) 
   ;; for now, leave this state around for debugging
   ;; empty symbol table and entry list
   ;; (symbols-reset) 
   ;; (clear-entries)

   ;; Clear the record of the students actions.
   ;; Note that this may change to a record of this act.
   (setq *studentactions* nil)
   
   ;; unload current problem with its sgraph structures
   (setf *cp* NIL)

   ;; Set the current problem instance time from the universal time.
   (setq *Current-Problem-Instance-Start-UTime* (get-universal-time))
   
   ;; Test whether or not the student has signalled that they are "done."
   ;; IF not then just generate a green-turn.  If they do say done then in
   ;; the future we will generate a green-turn here but we will leave that
   ;; stubbed for now.
   (if (null Done) 
       (make-green-turn)
     (make-green-turn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exit-andes -- end the Andes session
;; argument(s):
;; returns: unused
;; note(s): This sets flag to terminate event processing, which leads to
;; server shutdown when event loop runs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-exit-andes ()
  ;; Clear the record of the student's actions.
  ;; This may change later.
  (setq *studentactions* nil)
  (andes-stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; dynamic KB loading
;;
;; For ease of extensibility, the helpsystem loads the kb files dynamically.
;; This way new knowledge can be tested in Andes without rebuilding.
;; We first load kb/AMfile-helpsys.cl from the Andes directory. This defines
;; the AndesModule structure specifying the kb files and adds it to the list
;; of system modules. We then load the kb files through AndesModule functions.  
;; Note: we are not allowed to use the Lisp compiler in a runtime distribution, 
;; so the kb files must either be loaded as source or precompiled.
;; !!! Must handle errors loading kb, which is fatal to helpsys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun load-kb ()
  (format T "Loading Andes knowledge base")
  #+asdf (asdf:operate 'asdf:load-op 'andes)
  #-asdf (load (*Andes-path* "KB\\AMFile-helpsys.cl"))
  #-asdf (load-named-Andes-module 'Physics-kb)
)




;;=============================================================================
;; Student Entry List management
;;
;; Maintains a list of all current student entries. Note this includes both
;; correct and incorrect entries. A student entry structure can be entered on 
;; the list as soon as the entry is received and before its interpretations
;; or correctness state has been filled in by the entry interperter. Thus 
;; remembering an entry on the student entry list does not do anything 
;; concerning its correctness, in particular does not cause any marking of 
;; solution graph steps.
;;
;; However, the list management functions are designed to do certain updates 
;; automatically as side effects to ensure consistency:
;;
;; add-entry automatically removes any existing entry via remove-entry
;;   remove-entry automatically undoes entry's effects via undo-entry
;;     undo-entry (in entry interpreter) resets all state, including the
;;                undoing of solution graph updates for correct entries.
;;       undo-eqn-entry does further undoing specific to equation entries
;;
;; We use "entry" without qualifier in function name to mean a
;; StudentEntry struct.  "SystemEntry" is written out when that is meant.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-entry -- Record a new student entry as having been made.
;; Arguments: Student Entry to be added.
;; Returns: garbage
;;
;; Important: To ensure consistency, if an entry with same id exists, it is 
;; automatically deleted via remove-entry. This will call undo-entry on the
;; existing entry to undo its effects as well, see below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-entry (Entry)
  "Add the specified student entry struct, deleting any existing entry."
  ; remove any existing entry with same id 
  (remove-entry (StudentEntry-ID Entry)) 
  ; add new entry
  (format *debug-help* "Adding entry: ~A ~S~%" 
	  (studententry-id entry) (studententry-prop entry))
  (push Entry *StudentEntries*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-entry -- remove existing student entry, undoing its effects on state
;; Arguments: id  	the workbench-assigned entry id
;; Returns: T or NIL according as an entry was actually deleted
;; Note: safe to call if there is no existing entry.
;;
;; Calls back to undo-entry in EntryInterpreter module to do the work of
;; undoing an entry, because that is where the knowledge of what to do is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-entry (Id)
 "Remove any existing student entry with specified ID, undoing its effects"
 (let ((old-entry (find-entry Id)))
   (when old-entry
      (format *debug-help* "Removing entry: ~A ~S~%" 
	      (studententry-id old-entry) (studententry-prop old-entry))
      (undo-entry old-entry))
      ; and remove it from Entry listS
      (setf *StudentEntries*
        (delete Id *StudentEntries* :key #'StudentEntry-ID :test #'equal))
      ; in case useful: return T if entry was in fact deleted
      T))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-entry -- lookup student entry by ID
;; Arguments: id   workbench-assigned entry id
;; Returns: student entry structure or NIL if not found
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-entry (Id)
  "find student entry by workbench assigned entry id"
  (find id *StudentEntries* :key #'StudentEntry-ID :test #'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clear-entries  -- remove all student entries (no arguments).
;; Note this does *not* undo the entry effects -- it just empties the list.
;; This is suitable when just discarding all existing state on a new problem.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clear-entries ()
  (setq *StudentEntries* nil))

;;=============================================================================
;; Helpers for implicit equation entries associated with diagram entries
;;
;; Certain diagram entries determine "implicit equations". For example, drawing
;; a zero-length vector determines that that magV = 0, and drawing a vector at
;; a known angle determines thetaV = angle deg. Since these do not come 
;; from student equation box entries, they have to be reported to the algebra
;; module separately, so they can be exploited by the solve tool. In addition
;; these are fundamental equations in the solution graph so must be marked
;; when entered.
;; 
;; The algebra module provides slots above the number reserved for student
;; equation entries for this purpose. We need to be able to find an unused
;; slot number when we need them and also associate the implicit equations
;; with source entry ids, so that we can undo them if the source entry is
;; removed. We build an associated equation student entry which dangles
;; off the main diagram entry for these. These dangling entry structs are
;; not placed on the main student entry list but only accessed through their
;; containing diagram entry.

;; How we manage the slot range provided by the solver dll:
;; First 40 slots (index 0 ->39) are for student equation entries 
;; since this is the number in the workbench.
;; Our convention is to use most of the extra slot range to store 
;; implicit equation entries (see state.cl), but reserve the very last slot 
;; for use as a general purpose temp "register" for querying candidate eqns.
(defconstant *num-workbench-slots*  40)
(defconstant *max-student-slot*     (1- *num-workbench-slots*))
(defconstant *first-implicit-slot*  (1+ *max-student-slot*)) 
(defconstant *max-implicit-slot*    (1- *solver-max-eqn-slot*))

(defun StudentEntry-ImplicitEqnSlots (entry)
"Return list of implicit equation slot number used by a student entry"
  (mapcar #'StudentEntry-ID (StudentEntry-ImplicitEqns entry)))

(defun StudentEntry-GivenEqnSlots (entry)
"return list of given equation slots used by a student entry"
  (mapcar #'StudentEntry-ID (StudentEntry-GivenEqns entry)))

(defun entry-uses-slot (entry slot)
"true if student entry uses eqn slot for dependent entry"
  (or (member slot (StudentEntry-ImplicitEqnSlots entry))
      (member slot (StudentEntry-GivenEqnSlots entry)
              :test #'eql)))
	     
(defun eqn-slot-in-use (slot)
"return true if slot number is in use for a dependent equation entry"
  (some #'(lambda (e) (entry-uses-slot e slot))
        *StudentEntries*))
       

(defun get-unused-implicit-slot ()
  "return next high equation slot not in use by any entry, NIL if no more"
   (let (slot)
     (dotimes (offset (- *max-implicit-slot* *first-implicit-slot*))
       (setf slot (+ *first-implicit-slot* offset))
       (when (not (eqn-slot-in-use slot))
          (return-from get-unused-implicit-slot slot))))
   ; get here => failed to find one
   (warn "Ran out of implicit equation slots!")
   NIL)

;; Make implicit assignment entry -- initialize and return a candidate
;; StudentEntry struct for equation setting student variable to given value.
;; Value should be either a dnum term or dimensionless 0
;;
;; The result will look just like any other student equation entry, however,
;; id = slot number and status are not filled in yet.  Slot and other fields
;; will be filled in later after the main entry is checked and found 
;; correct.  If the main entry is not correct then the implicit equation entry
;; will not be processed further. Thus it does not need to be cleaned up if
;; the containing entry is not correct.
;;
(defun make-implicit-assignment-entry (studvar value)
"make implicit equation entry setting student var to specified value expression"
   ; make sure corresponding system var exists
   ; if not, source entry is probably an error
   (let ((sysvar (student-to-canonical studvar)))
    (when sysvar
      (make-implicit-eqn-entry `(= ,sysvar ,value)))))

(defun make-implicit-eqn-entry (eqn)
"construct implicit eqn entry struct for given systemese equation"
     (make-StudentEntry :prop `(implicit-eqn ,eqn)))

;; Note attempt to build an implicit equation entry above may fail w/NIL
;; Use following when adding to protect against adding a NULL implicit eqn
(defun add-implicit-eqn (mainEntry implicitEntry)
"add implicit eqn entry to mainEntrys list if non-NULL"
   (when implicitEntry
      (push ImplicitEntry (studentEntry-ImplicitEqns mainEntry))))

;; unlike implicit equations, which can be represented in systemese,
;; a given eqn entry is a studentese variable and a studentese expression
;; string (which may be empty string if no value set). So this winds up
;; almost like an equation entry. However, for these entries, we split studvar 
;; and value string into separate arguments in the proposition, for easy access.
;; We still begin prop with eqn so sg-entering code can recognize it as an 
;; equation entry.  That's OK, because that code only uses the car of 
;; the prop, the difference in the rest of it should not matter since it's
;; never used (check).

(defun make-given-eqn-entry (studvar value)
"make equation entry setting student var to given value"
   (when (student-to-canonical studvar)
     (make-StudentEntry :prop `(eqn ,studvar ,value))))

(defun blank-given-value-entry (eqn-entry)
"true if given value entry is blank for unknown"
  (= (length (trim-eqn (third (StudentEntry-Prop eqn-entry)))) 0))

;;===========================================================================
;; Student Name
;; The Student-name is stored at startup time in the 
;; **Current-Student-Name** parameter and can be accessed 
;; using the set-student-name and student-name functions.
(defparameter **Current-Student-Name** ())

(defun set-student-name (name)
  (setq **Current-Student-Name** name))

(defun student-name ()
  **Current-Student-Name**)

;;==============================================================
;; Experimental condition
;;
;; Could be set in config file.
;; In OLI version, workbench may set this from OLI-sent task
;; descriptor before opening problem. 
;;
(defvar **Condition** NIL)
(defun set-condition (value) 
  (format *debug-help* "Setting **condition** to ~A~%" value)
  (setq **Condition** value))
(defun get-condition () **Condition**)


;;==============================================================
;; Configuration files
;; The config file is essentially a lisp-source file that
;; is loaded (and evaluated in the process) at runtime.
;; this file may set parameters as necessary for experiements
;; it may also modify the state of the system depending upon
;; other info such as the student-name.
(defun load-config-file ()
  "Load the configuration file."
  (load (andes-path **Config-File-Name**)))


;;;; ===============================================================
;;;; StudentActions
;;;; For the purposes of filtering and tracking student actions 
;;;; we need to keep up to date on what the student has done including
;;;; deleted actions.  The following assists in developing and maintaining
;;;; a stack of student actions All of the actions in the list will be
;;;; Act Structs that link to the relevant incoming command and any
;;;; resultant structures within them.  
;;;;
;;;; The Search functions below allow for this list to be tested and 
;;;; maintained.  And used (eventually) for behavior tracking.  I have
;;;; elected to add this behavior tracking in the backend so that I 
;;;; can access the entry structs.  

;;; This is a stack containing the actions that the student has 
;;; performed.  These include student entries and help calls 
;;; along with their respective values.  This includes the help
;;; calls and their results.  The actions are stored in a special
;;; act struct.  See HelpStructs/StudentAction.cl for the definition.


;;; ---------------------------------------------------------------
;;; Parameters.

(defparameter *StudentActions* Nil "A Stack of the actions the student.")

(defparameter **log-student-actions** t 
  "Turns studentactions logging on and off at compile time.") 


;;; -----------------------------------------------------------------
;;; Logging commands
;;; The following commands allow actions to be logged into the 
;;; *StudentActions* list for later use. The main log func is 
;;; a generic alternative but typically the specialized calls 
;;; should be used.

;;; Once an act has been generated, add it to 
;;; the *studentactions* stack for later use 
;;(defun log-StudentAction (Act)
;;  "Log a new act that has occured for the student."
;;  (push Act *StudentActions*))


;;; Add the specified StudentEntry (Non-Eqn, Eqn and answer
;;; to the log for later use.
(defun log-entry-StudentAction (Entry Result &optional (Time nil))
  "Log the specified studententry in the list."
  (when **log-student-actions**
    (let ((action (make-StudentAction 
		   :Type (Help-entryprop-type (Studententry-prop Entry))
		   :Call (StudentEntry-Prop Entry)
		   :Result Result
		   :Assoc Entry)))
      (if Time (setf (Studentaction-Time Action) Time)
	(setf (studentaction-time Action) (studententry-time Entry)))
      (push Action *StudentActions*)))
  Result)


;;; Log the specified tutor turn.  Using the current time as its
;;; logtime.  This can be spoofed later by the Help Driver.
(defun log-turn-response-StudentAction (Response Result &optional (Time nil))
  "Log the supplied tutor turn."
  (when **log-student-actions**
    (let ((action (make-StudentAction
		   :type 'Tutor-turn
		   :Call (list 'handle-student-response response)
		   :Result Result
		   :Assoc (if (and Result (turn-p Result)) 
			      (turn-assoc Result)))))
      (if Time (setf (Studentaction-Time Action) Time))
      (push action *StudentActions*)))
  Result)


;;; Log the student's action call, and its result
;;; all the necessary values are supplied.
(defun log-studentaction (Call &optional (Result nil) (Assoc nil) (Time nil))
  "Generate a studentaction log and store the value."
  (when **log-student-actions**
    (let ((action (make-studentaction
		   :type (car Call) 
		   :Call Call
		   :Result Result
		   :Assoc Assoc)))
      (if Time (setf (Studentaction-Time Action) Time))
      (push action *StudentActions*)
      action)))





;;;; =====================================================================
;;;; Shared utility problems
;;;; The functions here have no other appropriate homes and so they will
;;;; be placed here to weather the forlorn storm of hope surrounded by the
;;;; salty sardines of despiration and the wet kelp of kelpiness.

;;; Get-useful givens
;;; Collect all of the useful given nodes from the current problem
;;; in order to determine if the student has, in fact, done any of 
;;; them.  
;;(defun collect-useful-givens ()
;;  (remove-if-not #'nsh-given-principle-p (collect-useful-nodes)))
;;
;;(defun collect-useful-nodes ()
;;  "Collect all of the nodes in the problem graph that contain entries."
;;  (let ((indicies (mapcan #'Eqnset-nodes (problem-solution *cp*))))
;;    (remove-if-not 
;;     #'(lambda (N) (and (bgnode-entries N) (member (bgnode-gindex N) indicies)))
;;     (append (bubblegraph-qnodes Bubblegraph)
;;	     (bubblegraph-enodes Bubblegraph)))))
;;

(defun axes-drawnp ()
"true if axes have been drawn in current problem"
  (find '(draw-axes ?dontcare) *studententries* :key #'studentEntry-Prop            :test #'unify))


;;; Return t if *cp* contains a problem.  This is used to facilitate
;;; some easy runtime testing.
(defun problem-loadedp ()
  (problem-p *cp*))


;;; Return t iff a problem is open and the 
;;; system is not checking entries.
(defun not-curr-checking-problemp ()
  (and (problem-loadedp) 
       (not **Checking-Entries**)))

;;
;; For detecting completion status
;;
(defun answer-entry-p (E)
"true if given student entry is for an answer submission"
;; Props in studententry recording answer submissions take the following forms:
;;    (ANSWER (MASS BLOCK))         Quantitative answer, arg is quantity
;;    (LOOKUP-MC-ANSWER ANSWER-1)   Done button for qualitative goal
;;    (CHOOSE-ANSWER MC-4 3)        Multiple-choice question answer
   (member (first (studententry-prop E)) 
           '(ANSWER LOOKUP-MC-ANSWER CHOOSE-ANSWER)))

;; test if student has correctly answered all parts on a problem
;; This is basically "done-p". 
(defun all-answers-done-p ()
 (let ((ncorrect-answer-entries 
          (length (remove-if-not 
                     #'(lambda (E) (and (answer-entry-p E) 
		                        (equalp (studententry-state E) **correct**)))
                     *Studententries*))))
  (= ncorrect-answer-entries (length (problem-soughts *cp*)))))

;; We don't have an "I'm done" button on the interface, but we may need to do
;; certain things when problem is completed, as in Sandy Katz followup experiment.
;; So we use a flag to detect first time in a problem session we change into the done state.
;; Following code can be used if only interested in the first time they become done, in which
;; case flag should NOT reset if they make a change to exit the done state.  Note: if a saved 
;; solution in the done state is opened, the flag will change values during the initial entry check.  
;; Code to process the transition is in the handler for answer submissions.

(defvar *problem-finished* NIL)     ; set if cp was finished in this session

(defun reset-done-flag ()	    ; call to reset flag on new problem open
    (setf *problem-finished* NIL))

(defun just-became-done ()	    ; call after answer submission to update flag
   (when (and (not *problem-finished*)  ; hasn't happened already in this session
              (setf *problem-finished* (all-answers-done-p)))))

          
    
      
;;;; ======================================================================
;;;; Filtering
;;;; Filtration of entries is done in Commands.cl the code here simply maintains
;;;; the current filters and allows for tests to be done on them.  The filter is
;;;; a 3-tuple of values.  Corresponding to three different classes types,
;;;; commands and entries.  If a type, command, or entry-prop is presnt in its 
;;;; relative section then it is blocked.  If not then it is permitted.

(defstruct (api-filter (:print-function print-api-filter))
  Types 
  Commands
  Entries)

(defun print-api-filter (filter &optional (stream t) (level 0))
  (declare (ignore Level))
  (format Stream "FILTER::~%  Types: ~a~%  Commands: ~a~%  Entries: ~a~%"
	  (api-filter-Types Filter) (api-filter-Commands Filter) 
	  (api-filter-Entries Filter)))




;;; -----------------------------------------------------------------------------
;;; The current filter is set here and maintained by the functions below for
;;; use by Commands.cl

(defparameter **current-api-filter** (make-api-filter))



(defun clear-api-filter (&optional (filter **current-api-filter**))
  "Clear the supplied filter."
  (setf (api-filter-types Filter) Nil)
  (setf (api-filter-Commands Filter) Nil)
  (setf (api-filter-Entries Filter) Nil))



;;; Return t if the specified type is acceptable to the filter (defualt
;;; to **current-api-filter**).
(defun filter-blocked-typep (type &optional (Filter **Current-API-Filter**))
  "Is the specific type acceptable to the filter?"
  (let ((F (api-filter-Types Filter)))
    (or (null F) (member Type F))))

(defun filter-blocked-commandp (Command &optional (Filter **Current-API-Filter**))
  "Is the specific command acceptable to the filter?"
  (let ((F (api-filter-Commands Filter)))
    (or (null F) (member Command F))))

(defun filter-blocked-entryp (Entry &optional (Filter **Current-API-Filter**))
  "Is the specific command acceptable to the filter?"
  (let ((F (api-filter-Entries Filter)))
    (or (null F) (member Entry F))))



;;; Adding elements to the filter is simply a matter of 
;;; pushing them onto the relevant locations.
(defun add-type-to-filter (Type &optional (Filter **current-API-Filter**))
  (when (not (member Type (api-filter-types Filter)))
    (push Type (api-filter-types Filter))))

(defun add-Command-to-filter (Command &optional (Filter **current-API-Filter**))
  (when (not (member Command (api-filter-Commands Filter)))
    (push Command (api-filter-Commands Filter))))

(defun add-Entry-to-filter (Entry &optional (Filter **current-API-Filter**))
  (when (not (member Entry (api-filter-Commands Filter)))
    (push Entry (api-filter-Commands Filter))))



