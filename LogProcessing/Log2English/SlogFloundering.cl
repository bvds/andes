#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slog-Iterate
;;; Collin Lynch
;;; 4/25/2002
;;;
;;; This file defines code that iterates over a single WB logfile and 
;;; collects information on their behavior.  Specifically it iterates
;;; over the list to determine iff they are floundering and, if so at
;;; what points and by how much.  The goal og this code is to determine
;;; how often the students participate in specific behaviors and to
;;; return those values.  Those behaviors are defined by stack predicates
;;; that test at any given time to see if the student is floundering, 
;;; and records the line number and time at which it was detected and 
;;; how long it lasts (still need to work on this one Probably, as long
;;; as the predicate remains t.
;;;
;;; The first part of the file contains the main functions to be used in
;;; loading the file and returning the numerical results.  This controls
;;; the opening of files and the iteration over them by the analizer.
;;; It's primary behavior is to generate a stack of actions from the log
;;; which will then be passed to the analizer.
;;; 
;;; Part 2 contains the analizer itself.  This is a fairly simple system 
;;; that executes each predicate in the list for for each entry in the 
;;; stack.  
;;;
;;; Part 3 contains the predicate definitions that will be stored for 
;;; for later, and the definitions themselves.  
;;;
;;; The support files contain the modified code from Linn's parser to 
;;; iterate over the list and return the result values as necessary.  
;;;

What I am interestedin doing here is getting a stack of student entries as 
they would be submitted to the workbench.  Therefore what this does is 
iterate over the list collecting DDE commands and matching them with their
corresponding results those can then be treated as an entry stack by the 
system itself for later processing.

|#



;;;; =============================================================================
;;;; primary loop.
;;;; The primary loop of the analizer operates as follows:
;;;; 1. Open the specified file (if possible)
;;;; 2. For each line in the file.
;;;;    3. Increment the Lineno
;;;;    4. read in the Line as a string.
;;;;    5. Process the line into an action which will then be processed later.
;;;;    6. Add the action to the stack.
;;;;    7. Test the state via the predicates and update the state list
;;;;       Accordingly.  
;;;; 4. And return the results when you are done.

(defun pattern-search-file (Filename)
  "Pattern seach through the file for pattern-matched data."
  (with-open-file (FILE Filename :direction :input)
      (do* ((lineno 0 (+ 1 lineno))
	    (LineStr (read-line FILE nil 'eof) (read-line FILE nil 'eof))
	    (Action (process-LineStr LineStr))
	    (Stack (list Action) (update-action-stack Stack Action))
	    (States Nil (test-States LineNo Stack States)))	  
	  ((eql LineStr 'EOF) States))))




;;;; ============================================================================================
;;;; Load files.  
;;;; The code below was modified from Linwood Taylor's Process-Files.cl located in his LogParser
;;;; directory.  The functions load the specified files and iterate over them calling the 
;;;; parse-line function that determines the results.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-files will process a group of files
;; directory - (string) of name directory containing files to be processed
;; file-chooser - a predicate function that takes a pathname (struct) and returns t iff file
;;  should be processed otherwise should return nil. file-chooser must be written to take at
;;  least one argument (a pathname (struct) of file being considered for choosing).
;; file-chooser-arguments - list of arguments for file-chooser (nil is legal)
;; process - function to be performed on each file chosen (should be written to handle one line
;;  per invocation)
;; process-arguments - list of arguents for process (nil is legal)
(defun process-files (directory file-chooser file-chooser-arguments process process-arguments)
  (dolist (file (directory directory))
    (when (safe-apply t file-chooser (cons file file-chooser-arguments))
      (format t "Processing '~A'~%" file)
      (process-file file process process-arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; same as process-files but will recurse
(defun process-dirs (directory file-chooser file-chooser-arguments process process-arguments)
  (dolist (dir (directory directory))
    (let* ((pdir (concatenate 'string (namestring dir) "/"))
	   (tmp (directory pdir)))
      ;;(format t "Directory ~A (~A)~%" pdir tmp)
      (if tmp
	  (process-dirs pdir file-chooser file-chooser-arguments process process-arguments)
	(when (safe-apply t file-chooser (cons dir file-chooser-arguments))
	  (format t "Processing '~A'~%" dir)
	  (process-file dir process process-arguments))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter **the-output-stream-name** "Temporary.prb")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-file processes a single file
;; infile - pathname (NOTE: struct not string) of file to read from
;; process - written to handle a single line of a file (must be written to accept at least three
;;  arguments
;;    1) infile - the pathname (struct) of the file being processed
;;    2) linenumber - the current line number being read (starts at zero ('0'))
;;    3) info - a string containing the line read
;; process-arguments - list of arguments for process (nil is legal)
(defun process-file (infile)
  (setf **the-output-stream-name** "Temporary.prb")
  (with-open-file (str infile :direction :input)
    (parse-line Str 0)))

;;    (do* ((linenumber 0 (+ 1 linenumber))
;;	  (info (read-line str nil 'eof) (read-line str nil 'eof))
;;	  (Entries (list (parse-line linenumber info nil))
;;		   (cons (parse-line linenumber info Entries) Entries)))
;;	((eql info 'eof) Entries))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example file-choosers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-file-type-p -- check if a file is a file with specified extension 
;;  file - pathname (struct) of file to be validated
;;  extension - suffix (string) of file type
;; returns: 'nil if file is not a file ending with '.log' otherwise returns 't
(defun is-file-type-p (file extension)
  (equal extension (pathname-type file)))

;; is-exactly-p -- returns t if and only if file is named name with extension extension
;;  file - pathname (struct) of file to be validated
;;  name - name (string) that needs to be matched
;;  extension - suffix (string) of file type
;; returns: 'nil if file is not file specified by name and extension
(defun is-exactly-p (file name extension)
  (and (is-file-type-p file extension)
       (equal name (pathname-name file))))





;;;; =============================================================================
;;;; Parsing.
;;;; The code in this section is responsible for the stack parsing  At present
;;;; what it does is a simple recursive parsing process.  The system operates by
;;;; Calling parse-line on a specific func.  If the line is a comment then it 
;;;; will be ignored.  Otherwize, it will assume that the line is a command of
;;;; the form:  <LogTime>     <Command> <Values>     
;;;; Where:
;;;;   Logtime is a time of the form [[[H]H:]M]M:SS
;;;;   Command is a command string such as "DDE-POST"
;;;;   Values  is a specific DDE value or command context.
;;;;
;;;; For the time being the parser will ignore everything except DDE commands.


;;; ===========================================================================================
;;; Parse-line
;;; parsing a line is a recursive call that takes an open file stream and continues reading off
;;; lines until some termination test is reached.  Its behavior with regard the the lines is 
;;; determined by the line type and its current state.  The arguments are:
;;;   Stream   -- The incomiung file stream.
;;;   LineNo   -- The current line number
;;;   Lines    -- A list of previously read lines
;;;
;;; EOF causes the run to terminate and return the results.
;;; Comment lines are skipped.  Else the line will be processed.
(defun parse-line (Stream LineNo &optional (PastLines nil))
  "Parse the specified line and recurse or return the result as necessary."
  (let* ((Line (read-line Stream nil 'EOF)))
    (cond ((equal Line 'EOF) (list LineNo PastLines))
	  ((or (<= (length line) 0) (System-comment-line-p Line) (User-comment-line-P Line)) 
	   (format t "Comment: ~a~%" Line)
	   (parse-line Stream (+ LineNo 1) PastLines))
	  (t (process-line Line Stream LineNo PastLines)))))


(defun system-comment-line-p (LineStr)
  "Is the line prefixed with #?"
  (eq (char LineStr 0) #\#))

(defun user-comment-line-p (Linestr)
  "Is the line prefixed with ';'?"
  (eq (char LineStr 0) #\;))


;;;-----------------------------------------------------------------------------------
;;; Process-line
;;; Once we have decided that a line should be parsed we will determine iff it is a 
;;; command.  If we can parse it into proper command form then it will be and we will
;;; process it based upon the command type.  Else the system will skip it (with an 
;;; appropriate message).
(defun process-line (LineStr Stream LineNo PastLines)
  (let ((tab (position #\Tab LineStr)))
    (if tab
	(let* ((time (time2secs (subseq LineStr 0 tab)))
	       (cmd-line (subseq LineStr (+ tab 1)))
	       (space (position #\Space cmd-line))
	       (cmd (if space (subseq cmd-line 0 space) cmd-line))
	       (args (if space (if (>= space (length cmd-line)) nil
				 (subseq cmd-line (+ space 1))) nil)))
	  (parse-cmd LineNo time cmd args Stream  PastLines))
    
      (progn (format t "Unhandled Line: ~a ~a~%" LineNo LineStr)
	     (parse-line Stream (+ LineNo 1) PastLines)))))


;;;; ===========================================================================================
;;;; Time Parsing.
;;;; The contents of this section are based upon Linn's time parsing code defined in his 
;;;; Slog2English code.  It converts a set Logfile time into a universal time for use by the 
;;;; stack system.  

;;; c2i performs a basic Char-to-int service.
(defun c2i (char)
  "Convert a Numerical char to its corresponding (base 10) int."
  (cond
   ((equal char #\0) 0)
   ((equal char #\1) 1)
   ((equal char #\2) 2)
   ((equal char #\3) 3)
   ((equal char #\4) 4)
   ((equal char #\5) 5)
   ((equal char #\6) 6)
   ((equal char #\7) 7)
   ((equal char #\8) 8)
   ((equal char #\9) 9)
   (t -1)))


(defun time2secs (time)
  "Convert a workbench time [Hours:]Min:Sec to an integer number of seconds."
  (let ((len (length time))
	(sec -1))
    (if (> len 3)
	(setf sec (+ (+ (c2i (char time (- len 1))) (* 10 (c2i (char time (- len 2)))))
		     (* 60 (c2i (char time (- len 4)))))))
    (if (> len 4)
	(setf sec (+ sec (* 600 (c2i (char time (- len 5)))))))
    (if (> len 6)
	(setf sec (+ sec (* 3600 (c2i (char time (- len 7)))))))
    (if (> len 7)
	(setf sec (+ sec (* 36000 (c2i (char time (- len 8)))))))
    sec))



(defun secs2time (time)
  "Given an integer number of seconds write it as [Hours:]Min:Sec."
  (let* ((seconds (rem time 60))
	 (minutes (truncate time 60)))
    (if **tracking-time**
	(if (<= time 0)
	    (format nil "     ")
	  (if (< seconds 10)
	      (if (< minutes 10)
		  (format nil "0~A:0~A" minutes seconds)
		(format nil "~A:0~A" minutes seconds))
	    (if (< minutes 10)
		(format nil "0~A:~A" minutes seconds)
	      (format nil "~A:~A" minutes seconds))))
      (format nil ""))))




;;;; ==========================================================================
;;;; Parse-cmd
;;;; Parsing a command is a process of identifying the command (by type)
;;;; and then calling the appropriate function on it once that type has been
;;;; defined.  The command types are defined in the **workbench-commands** list 
;;;; that is located below.  
;;;;
;;;; Parsing commands is a process of ientifying the command type and then calling
;;;; the appropriate function.  At present only the DDE commands have any behaviors 
;;;; attatched to them but that may change.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a-list - intent is 'if (assoc word **workbench-commands**) then execute associated function
;; Note: each associated function takes four arguments;
;;  1) a stream to output to
;;  2) the line number of the originally read line (integral)
;;  3) the time stamp of the line (as given by workbench (string HH:MM:SS or MM:SS))
;;  4) the arguments to the command (string)
(defparameter **workbench-commands** ;; routines follow
    '(("DDE" handle-dde)
      ("DDE-RESULT" handle-dde-result)
      ("DDE-POST" handle-dde-post)
      ("DDE-FAILED" handle-dde-failed)


      ;;("Comment" handle-comment)

      ;;("Check-Entries" handle-check-entries)
      
      ;;("END-LOG" handle-end-log)
      ;;("Andes-Version" handle-andes-version)
      ;;("FBD-Version" handle-fbd-version)
      ;;("Open-Problem" handle-open-problem)
      ;;("Warning-MsgBox" handle-warning-msgbox)
      ;;("BTN-CLICK" handle-btn-click)
      #| these are workbench commands that are not currently addressed (NOTE: incomplete list)
      Async-mode
      Login-dlg
      F
      C
      Next-Id
      Select-tool
      L
      Begin-draw
      Select
      M
      Up
      System
      System-dlg
      SEL-LIST
      DRP
      CLOSE
      SEL
      c
      Sysprops
      Select-variable
      Delete-Variable
      Menu
      Hint-Hise
      FBD-Whatswrong
      Edit-props
      New-Variable
      Declare-Variable-dlg
      Vector-dlg
      Variable-Menu
      Modify-Variable
      E
      K
      EQ-F
      S
      EQ-SUBMIT
      EQ-SolveFor
      Ans-Enter
      Ans
      Ans-submit
      Ans-status
      Ans-exit
      Close
      App-deactivate
      App-activate
      Open-Lesson
      Close-Lesson
      Go
      PSM-select
      PSM-expand
      Begin-vector
      Vector
      Vtype
      Vecprops
      Axes-dlg
      Axesprops
      CLOSE-APP
      |#
      ))



;;; -----------------------------------------------------------------------
;;; Parse-CMD 
;;; And incoming command will be associated with the **Workbench-commands**
;;; If a relevant function exists that function will be called.  Else the 
;;; System will skip the command and parse the next line.

(defun parse-CMD (LineNo time Command args Stream PastLines)
  (let ((Func (second (assoc Command **Workbench-Commands** :test #'equal))))
    (if Func (safe-apply Func (list LineNo time args Stream PastLines))
      (progn (format t "Skipping: ~a ~a ~a ~a~%" LineNo Time COmmand Args)
	     (parse-line Stream (+ LineNo 1) PastLines)))))



;;; ----------------------------------------------------------------------------
;;; DDE
;;; DDE's are posted when the user executes a command for which a response is 
;;; expected.  In theory at some lateer point in the file a DDE-result will
;;; appear.  In fact since DDE's are asynchronous, the next DDE-result will 
;;; correspond to the current one.
;;;
;;; In future more complex additions may be made to this.
(defun handle-dde (LineNo time Action Stream PastLines)
  "Post a new DDE."
  (parse-line Stream (+ LineNo 1) (cons (list 'DDE LineNo Time Action 'no-result) PastLines)))


;;; DDE-Result
;;; DDE-results signal the Help system's views on the last DDE that the 
;;; student sent in.  The DDE-Result may be "||", "|T|" "|NIL|"
;;; or some hint string.
;;;
;;; When a DDE-result is found it will be matched to its corresponding
;;; DDE (added to the end) so that the system can check it directly and 
;;; then it will be added into the list with its own DDE added to the end.
(defun handle-dde-result (LineNo time Result Stream PastLines)
  (let ((DDE (find 'DDE PastLines :key #'car))) 
    (if (null DDE) (error "No matching DDE found ~a ~a ~a" lineno time Result))
    (setf (nth 4 DDE) Result)
    (parse-line
     Stream (+ LineNo 1) 
     (cons (list 'DDE-Result LineNo Time Result DDE) PastLines))))


;;; DDE-Failed
;;; DDE-failed is sent back to the student when their command has crashed or otherwize
;;; caused the Help system to boot.  Like the DDE-result these will be paired with the
;;; command to which they belong and returned.  
(defun handle-dde-failed (LineNo time Result Stream PastLines)
  (declare (ignore result))
  (let ((DDE (find 'DDE PastLines :key #'car)))
    (if (null DDE) (error "No matching DDE found ~a ~a ~a" lineno time 'DDE-Failed))
    (setf (nth 4 DDE) 'DDE-Failed)
    (parse-line
     Stream (+ LineNo 1) 
     (cons (list 'DDE-Failed LineNo Time DDE) PastLines))))



;;; DDE-Post
;;; DDE-Posts are DDE-Procedures with no expected return value.
;;; Unless the post is:
;;;   (Exit-andes)
;;; The post will be added to the stack and the system will move
;;; on.
(defun handle-dde-post (LineNo time Post Stream PastLines)
  (cond ((String-equal Post "(exit-andes)") (list LineNo PastLines))
	(t (parse-line Stream (+ LineNo 1)
		       (cons (list 'DDE-Post LineNo Time Post) PastLines)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools.cl -- routines to support general lisping parsing of student logs written by workbench
;; Copyright (C) 2001 by <Linwood H. Taylor's (lht@lzri.com) Employer> -- All Rights Reserved
;; Author(s): Linwood H. Taylor (lht@lzri.com)
;; Modified:
;;      13 December 2001 - (lht) -- created/split from SLog2English.cl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; capture list errors and write error to stream
(defconstant **trap-lisp-errors** t) ;; t turns safe-apply on

(defun safe-apply (fn args)
  (if **trap-lisp-errors**
      (let (result)
	(handler-case (setf result (apply fn args))
	  (error (c) (format t "**** Error: executing command ~A.~%" c) :error)
	  (:no-error (c) (declare (ignore c)) result)))
    (apply fn args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formats paragraphs to block width characters wide
(defconstant ***width*** 72)

;; returns the location of the character after the end of word
(defun z-next-word (str)
  (cond
   ((equal str "") 0)
   ((position #\Space str) (position #\Space str))
   (t (length str))))

;; the optional indent is to support hanging indentation (where body is indented but opening
;; line is not
(defun l-format (the-str &optional (indent "        "))
  (let ((frm "") (cnt 0) (str (string-trim " " the-str)))
    (do ((done nil (equal (length (string-trim " " str)) 0)))
	(done (string-trim " " frm))
      (let ((tmp (z-next-word str)))
	(when (> tmp 0)
	  (if (>= (+ tmp cnt) ***width***)
	      (setf frm (concatenate 'string frm (format nil "~A" #\LineFeed) indent
				     (string-trim " " (subseq str 0 tmp))))
	    (setf frm (concatenate 'string frm " " (string-trim " " (subseq str 0 tmp)))))
	  (setf str (string-trim " " (subseq str tmp)))
	  (if (>= (+ tmp cnt) ***width***)
	      (setf cnt tmp)
	    (setf cnt (+ cnt tmp 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of Tools.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's (lht@lzri.com) Employer> -- All Rights Reserved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
