;; State.cl
;; Collin Lynch (CL) <CollinL@pitt.edu>
;; Anders Weinstein (AW) <AndersW@pitt.edu>
;; Lynwood Taylor (LHT) <lht@lzri.com>
;; 4/20/2001
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file defines basic state storage functions and
;; state information for the help system.  
;;
;; Changelog:
;; 3/12/2003 - (CL) -- Commented out Collect-useful-givens and 
;;  collect-useful-nodes as they were unused.
;; 8/11/2003 - (CL) -- Added in Done flag to close-problem.
;;

(in-package :cl-user)
(eval-when (:load-toplevel :compile-toplevel)
  (use-package :symbols))

;;========================================================
;; Storage elements.

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
;------------------------------------------------------------------------------


;;; ===========================================================================
;;; State API calls.


;; Problem Control info.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read-problem-info open a new problem
;; argument(s): problem id
;; returns: T for success, NIL or 'WRONG-VERSION-PRB for error
;; note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-problem-info (name)

  (parse-initialize) 	;set up session-local memos.
  ;; reset run-time data structures for new problem:
  (setf **grammar** nil)
  (grammar-add-grammar '**grammar** **common-grammar**)
  (initialize-symbol-table)
  (setf *StudentEntries* nil)
  ;; use problem name as seed for random elt
  (initialize-random-elt (string-downcase name)) 

  ;; Load the current problem and set into global *cp* 
  ;; NB: for case-sensitive filesystems, ensure we convert the problem name, 
  ;; passed as a string, to canonical upper case used for problem ids.
  ;;  
  ;; In this case, the problems are each time the files
  ;; are loaded.  They are kept separate from the problem
  ;; registry created by defproblem.
  (setf *cp* (read-problem-file (string-upcase name) 
				:path (andes-path "solutions/")))

  (if *cp* 
      ;; Once the problem has been loaded successfully into the *cp* parameter
      ;; then we need to setup the struct for runtime use.
      (progn
	(match:initialize-word-count-memo) ;session-local memo for word-count.
	(sg-setup *cp*) ;Initialize sg structures
	
	;; enter appropriate predefined student labels into symbol table: 
	(enter-predefs)
	
	;; re-initialize the dialog state
	(reset-next-step-help)

	(initialize-grading))
      ;; Raising an error, rather than a warning, keeps subsequent
      ;; code in open-problem from being executed and sends a 
      ;; message to the student.
      (error 'log-condition:log-error :tag 'problem-load-failed
	     :text (strcat "Unable to load problem " (string name)
			   ".  Please try another problem." ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close-problem -- close the specified problem 
;; returns: unused
;; note(s): should be current problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun do-close-problem ()
  (when *cp* ;*cp* not defined if load failed.
    (setf *sg-eqns* nil)
    
    (setf *cp* nil))
  
  ;; empty symbol table and entry list
  (clear-symbol-table)
  (setf **grammar** nil)
  (setq *StudentEntries* nil)

  (parse-clear)  

  ;; Set the current problem instance time from the universal time.
  (setq *Current-Problem-Instance-Start-UTime* (get-universal-time)))


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
;; add-entry automatically removes any existing entry via delete-object
;;    delete-object automatically undoes entry's effects via undo-entry
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
;; automatically deleted via delete-object. This will call undo-entry on the
;; existing entry to undo its effects as well, see below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-entry (Entry)
  "Add the specified student entry struct, deleting any existing entry."
  ;; remove any existing entry with same id 
  (delete-object (StudentEntry-id Entry))
  ;; add new entry
  (when *debug-help* 
    (format t "Adding entry: ~A ~S~%" 
	    (studententry-id entry) (studententry-prop entry)))
  (push Entry *StudentEntries*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-object -- remove existing student entry, undoing its effects on state
;; Arguments: id  	the workbench-assigned entry id
;; Returns:   empty tutor turn.
;;
;; Calls back to undo-entry in EntryInterpreter module to do the work of
;; undoing an entry, because that is where the knowledge of what to do is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-object (Id)
  "Remove any existing student entry with specified ID, undoing its effects"
  (let ((old-entry (find-entry Id)))
    (when old-entry 
      (when *debug-help* 
	(format t "Removing entry: ~A ~S~%" 
		(studententry-id old-entry) (studententry-prop old-entry)))
      (undo-entry old-entry)
      ;; and remove it from Entry lists
      (setf *StudentEntries*
	    (delete Id *StudentEntries* 
		    :key #'StudentEntry-ID :test #'equal))
      (make-noop-turn))))

;;=============================================================================
;; Helpers for implicit equation entries associated with diagram entries
;;
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

(defun make-given-eqn-entry (studvar value id)
"make equation entry setting student var to given value"
   ; construct this even if it is an unused variable.
   ; will fail when checked.
   (make-StudentEntry :prop `(eqn ,studvar ,value) :id id))

(defun blank-given-value-entry (eqn-entry)
"true if given value entry is blank for unknown"
  (= (length (trim-eqn (third (StudentEntry-Prop eqn-entry)))) 0))

;; Note attempt to build a given equation entry above may fail w/NIL
;; Use following when adding to protect against adding a NULL given eqn
(defun add-given-eqn (mainEntry givenEntry)
"add implicit eqn entry to mainEntrys list if non-NULL"
   (when givenEntry
      (push givenEntry (studentEntry-GivenEqns mainEntry))))



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
		                        (equalp (studententry-state E) +correct+)))
                     *Studententries*))))
  (= ncorrect-answer-entries (length (problem-soughts *cp*)))))
          
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
