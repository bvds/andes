;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLog2English -- routines to support processing of andes student logs; Converts Student logs
;;   from WorkBench to a pseudo conversation between the student and the andes tutor.
;; Copyright (C) 2001 by <Linwood H. Taylors (lht@lzri.com) Employer> -- All Rights Reserved
;; Author(s): Linwood H. Taylor (lht@lzri.com)
;; Modified:
;;      2 December 2001 - (lht) -- created
;;     12 December 2001 - (lht) -- 'finished'
;;     13 December 2001 - (lht) -- split into sub files to facilitate processing files for other
;;                                 data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-new ()
  (load "C:/Code/LogUtils/ProcessFiles.cl") ;; NOTE: needed
  (load "C:/Code/LogUtils/SLogParse.cl") ;; NOTE: needed
  (load "C:/Code/LogUtils/Tools.cl") ;; NOTE: needed
  (load "C:/Code/LogUtils/HelpSystem_0.cl")) ;; NOTE: needed

(defun load-fas ()
  (load "C:/Code/LogUtils/ProcessFiles") ;; NOTE: needed
  (load "C:/Code/LogUtils/SLogParse") ;; NOTE: needed
  (load "C:/Code/LogUtils/Tools") ;; NOTE: needed
  (load "C:/Code/LogUtils/HelpSystem_0")) ;; NOTE: needed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a-list - intent is 'if (assoc word **workbench-commands**) then execute associated function
;; Note: each associated function takes four arguments;
;;  1) a stream to output to
;;  2) the line number of the originally read line (integral)
;;  3) the time stamp of the line (as given by workbench (string HH:MM:SS or MM:SS))
;;  4) the arguments to the command (string)
(defparameter **workbench-commands** ;; routines follow
    '(("DDE-RESULT" handle-dde-result)
      ("DDE" handle-dde)
      ("DDE-POST" handle-dde-post)
      ("DDE-FAILED" handle-dde-failed)
      ("DDE-COMMAND" handle-dde-command)
      ("Comment" handle-comment)
      ("Check-Entries" handle-check-entries)
      ("END-LOG" handle-end-log)
      ("Andes-Version" handle-andes-version)
      ("FBD-Version" handle-fbd-version)
      ("Open-Problem" handle-open-problem)
      ("Warning-MsgBox" handle-warning-msgbox)
      ("BTN-CLICK" handle-btn-click)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-parse-line-output-file-stream-name (file lineno line)
  (declare (ignore Lineno Line))
  (setf **the-output-stream-name** (concatenate 'string (namestring file) ".ss"))
  **the-output-stream-name**)

(defun make-parse-line-output-file-stream-name-prb (file lineno line)
  (declare (ignore Lineno Line))
  (when (equal **the-output-stream-name** "Temporary.prb")
    ;;(format t "Changing '~A' to '~A'~%" **the-output-stream-name** (namestring file))
    (setf **the-output-stream-name** (concatenate 'string (namestring file) ".ss")))
  **the-output-stream-name**)



;;;;;===========================================================================================
;;;;; This is the main parsing function for the 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle parse - takes an appropriate action based on command passed
;;        a) an output stream
;;        b) the current linenumber
;;        d) string containing the time line was entered
;;        d) string containing the command
;;        e) string containing the arguments for the command
(defun handle-parse (stream lineno time command arguments)
  (let ((tmp (assoc command **workbench-commands** :test #'equal)))
    (if tmp
	(safe-apply stream (second tmp) (list stream lineno time arguments))
      nil))) ;; ignore command -- not one we're intersested in













(defun handle-comment (stream lineno time arguments)
  (declare (ignore LineNo))
  (format stream "~A (~A) Student comments '~A'~%" time (secs2time **lapsed-time**) arguments))

(defun handle-check-entries (stream lineno time arguments)
  (declare (ignore LineNo))
  (when (equal arguments "1")
    (format stream "~A (~A) Andes begins loading previous work~%"
	    time (secs2time **lapsed-time**)))
  (when (equal arguments "0")
    (format stream "~A (~A) Andes finished loading previous work~%"
	    time (secs2time **lapsed-time**))
    ;;(format t "Tracking: ~W~%" arguments)
    (handle-time-start-tracking time)))

(defun handle-end-log (stream lineno time arguments)
  (declare (ignore LineNo Arguments))
  (format stream "~A (~A) Student has left the building.~%" time (secs2time **lapsed-time**))
  (setf **the-output-stream-name** "Temporary.prb")
  (setf **TotalTime** (time2secs time)))

(defun handle-andes-version (stream lineno time arguments)
  (declare (ignore LineNo))
  (setf **lapsed-time** 0)
  (format stream "~A (~A) Andes Version ~A~%" time (secs2time **lapsed-time**) arguments)
  (setf **TotalTime** (time2secs time)))

(defun handle-fbd-version (stream lineno time arguments)
  (declare (ignore LineNo))
  (format stream "~A (~A) FBD (WB) Version ~A~%" time (secs2time **lapsed-time**) arguments)
  (setf **TotalTime** (time2secs time)))

(defun handle-open-problem (stream lineno time arguments)
  (declare (ignore LineNo Arguments Stream))
  (setf **StartProblem** (time2secs time)))

(defvar **catch-button-click** nil)
(defun handle-warning-msgbox (stream lineno time arguments)
  (declare (ignore LineNo))
  ;;(format t "~W~%" arguments)
  ;;;(if (search "Your previous definition was incorrect." (format nil "~A" arguments))
  (format stream "  ~A (~A) Andes warns '~A'~%" time (secs2time **lapsed-time**) arguments)
  (setf **catch-button-click** t)
  )

(defun handle-btn-click (stream lineno time arguments)
  (declare (ignore LineNo))
  (when **catch-button-click**
    (setf **catch-button-click** nil)
    (format stream "~A (~A) Student says ~A~%" time (secs2time **lapsed-time**)
	    (cond
	     ((search "OK" arguments) "Okay")
	     ((search "Cancel" arguments) "Cancel.")
	     ((search "Close" arguments) "Close.")
	     (t (format nil "~A" (subseq arguments 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DDE-Commands get used in many ways.  For now the only ones that we want to cover
;; are the set-score commands so those will be printed here.

(defun handle-dde-command (stream lineno time arguments)
  (declare (ignore Lineno))
  (setf **get-proc-help-count** (+ 1 **get-proc-help-count**))
  ;;(pprint arguments)
  (when (string= "set-score" (subseq Arguments 0 9))
    (format stream "~A (~A) Andes says Student's score is: ~A~%" time (secs2time **lapsed-time**)
	    (format nil "~A" (subseq arguments 9)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flag to help define what is done with a response to user input
;; 0 means ignore
;; 1 is output Correct on 't
;; 2 is output Okay on 't
;;   more to come
(defparameter **response** 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun respond (&optional (val 0))
  (setf **response** val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; holds student 'name' as soon as it is found -- default is "Student"
(defparameter **student-name** "Student")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde-result (stream lineno time arguments)
  (declare (ignore LineNo))
  (handle-time-stats-reply time)
  (cond
   ((equal "||" arguments) nil)
   ((equal "|NIL|" arguments)
    (cond
     ((eq **response** 4)
      (format stream "~A (~A)  Andes: Could not open problem~%" time
	      (secs2time **lapsed-time**)))
     (t
      (format stream "~A (~A)  Andes: That is not correct.~%" time
	      (secs2time **lapsed-time**)))))
   ((equal "|T|" arguments)
    (cond
     ((eq **response** 1)
      (format stream "~A (~A)  Andes: Correct!~%" time (secs2time **lapsed-time**)))
     ((or (eq **response** 2) (eq **response** 4))
      (format stream "~A (~A)  Andes: Okay.~%" time (secs2time **lapsed-time**)))
     ((eq **response** 3)
      (format stream "~%~A (~A)  Andes: You have stated a correct answer!!!~%" 
	      time (secs2time **lapsed-time**))) 
     (t
      nil)))
   (t
    (let ((index (search "show-hint" arguments))
	  (index2 (search "show-lesson" arguments))
	  (index1 (position #\~ arguments)))
      (if index
	  (if index1
	      (format stream "~A (~A)  Andes: ~A~%" time (secs2time **lapsed-time**)
		      (l-format
		       (remove #\|
			       (subseq arguments (+ index (length "show-hint") 1) index1))))
	    (format stream "~A (~A)  Andes~A: ~A~%" time (secs2time **lapsed-time**)
		    (if index1 "" "(bottom-hint)")
		    (l-format (remove #\|
				      (subseq arguments (+ index (length "show-hint") 1))))))
	(if index2
	    (format stream "~A (~A)  Andes shows lesson: ~A.~%" time (secs2time **lapsed-time**)
		    (remove #\| (subseq arguments (+ index2 (length "show-lesson") 1))))
	  (if (equal (char (remove #\| arguments) 0) #\!)
	      (format stream "~A (~A)  Andes: ~A~%" time (secs2time **lapsed-time**)
		      (l-format (subseq (remove #\| arguments) 1)))
	    (format stream "~A (~A)  Andes answers '~A'~%" time (secs2time **lapsed-time**)
		    (l-format (remove #\| arguments))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde-failed (stream lineno time arguments)
  (declare (ignore LineNo))
  (respond 0)
  (handle-time-stats-failed time)
  (if (< **lapsed-time** 60)
      (format stream "~%  ~A (~A) Andes Help System did not respond to request: ~W.~%~%"
	      time  (secs2time **lapsed-time**) arguments)
    (format stream "~%  ~A (~A) Andes WorkBench timed out waiting for response: ~W.~%~%"
	    time  (secs2time **lapsed-time**) arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde-post (stream lineno time arguments)
  (let ((command-list (read-from-string arguments)))
    (handle-time-stats-post time)
    (format stream "~A (~A) ~A" time (secs2time **lapsed-time**)
	    (l-format (safe-apply
		       stream (car command-list) (list stream lineno time (rest command-list)
						       (string-trim " " arguments)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde (stream lineno time arguments)
  (let ((command-list (read-from-string (remove #\\ arguments))))
    (handle-time-stats time)
    (format stream "~A (~A) ~A" time (secs2time **lapsed-time**)
	    (l-format (safe-apply
		       stream (car command-list) (list stream lineno time (rest command-list)
						       (string-trim " " arguments)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to track equations by slot #
(defparameter **equations**
    '())

;; add equation to slot removing old equation (if it existed)
(defun add-equation (equation slot)
  (if (member slot **equations** :test #'(lambda (x y) (equal x (car y))))
      (setf **equations** (remove slot **equations** :test #'(lambda (x y) (equal x (car y))))))
  (setf **equations** (append **equations** (list (list slot equation)))))

;; remove equation at slot
(defun delete-equation (slot)
  (if (member slot **equations** :test #'(lambda (x y) (equal x (car y))))
      (setf **equations** (remove slot **equations** :test #'(lambda (x y) (equal x (car y)))))))

;; get the equation at slot
(defun get-equation (slot)
  (let ((tmp (assoc slot **equations**)))
    (second tmp)))

;; No longer necessary.
;;(defun wb-quant (&rest args) (append (car args) (list (second args))))
;; necessary kludge for workbench line that looks like:
;; (handle-student-response #,(wb-quant '(blah blah blah) 'blah))
;; see (handle-student-response)

;; Because of the need to load help system code I am now mocking up the tell macro
;; to be a noop.
(defun tell (&rest args) (declare (ignore Args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file SLog2English.cl
;; Copyright (C) 2001 by <Linwood H. Taylors (lht@lzri.com) Employer> -- All Rights Reserved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




