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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLog2English -- routines to support processing of andes student logs
;;  includes code for converting log data to a pseudo conversation between the student and the
;;  andes tutor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-files will process a group of files
;; directory - containing files to be processed
;; file-chooser - takes a file name and returns t iff file should be processed else nil
;;  file-chooser must be written to take at least one argument (a pathname of file being
;;  considered for choosing.
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
;; process-file processes a single file
;; infile - pathname of file to read from
;; process - written to handle a single line of a file (must be written to accept at least three
;;  arguments
;;    1) the path
;;    2) the current line number being read (starts at zero ('0'))
;;    3) a string containing the line read
;; process-arguments - list of arguments for process (nil is legal)
(defun process-file (infile process process-arguments)
  (with-open-file (str infile :direction :input)
    (do ((linenumber 0 (+ 1 linenumber))
	 (info (read-line str nil 'eof) (read-line str nil 'eof)))
	((eql info 'eof))
      (safe-apply t process (append (list infile linenumber info) process-arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example file-choosers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-file-type-p -- check if a file is a file with specified extension 
;; file - name of file to be validated
;; returns: 'nil if file is not a file ending with '.log' otherwise returns 't
(defun is-file-type-p (file extension)
  (equal extension (pathname-type file)))

(defun is-exactly-p (file name extension)
  (and (is-file-type-p file extension)
       (equal name (pathname-name file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example process routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-line breaks a line from a student log into 3 parts; time, command, and args; and
;;   hands them to a routine to handle the parts
;; file - file read
;; linenumber - the number of the line read
;; line - the data contained on that line
;; handler - function that gets passed parse data (takes at least 5 args)
;;        a) an output stream
;;        b) the current linenumber
;;        d) string containing the time line was entered
;;        d) string containing the command
;;        e) string containing the arguments for the command
;; handler-arguments - optional list of arguments to handler
(defun parse-line (file lineno line handler &optional (handler-args nil))
  (with-open-file (str (concatenate 'string (namestring file) ".ss")
		   :direction :output :if-exists :append :if-does-not-exist :create)
    (cond
     ((<= (length line) 0) nil)
     ((eq (char line 0) #\#) nil) ;; ignore system comments
     ((eq (char line 0) #\;) ;; comment added by researcher studying log
      (format str "Researcher Comments at line #~A.~%  '~A~%'" lineno line))
     (t ;; split line into parts and pass on to handler
      (let ((tab (position #\Tab line)))
	(if tab
	    (let*
		((time (subseq line 0 tab))
		 (cmd-line (subseq line (+ tab 1)))
		 (space (position #\Space cmd-line))
		 (cmd (if space (subseq cmd-line 0 space) cmd-line))
		 (args (if space (if (>= space (length cmd-line)) nil
				   (subseq cmd-line (+ space 1))) nil)))
	      (safe-apply str handler (append (list str lineno time cmd args) handler-args)))
	  (format str "*** Unhandled line #~A.~%  ~A" lineno line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directly deals with the commands as passed to parse-handler by parse-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specific to workbench
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a-list - intent is 'if (assoc word **workbench-commands**) then execute associated function
(defparameter **workbench-commands** ;; routines follow
    '(("DDE-RESULT" handle-dde-result)
      ("DDE" handle-dde)
      ("DDE-POST" handle-dde-post)
      ("DDE-FAILED" handle-dde-failed)
      #| these are workbench commands that are not currently addressed (NOTE: incomplete list)
      Andes-Version
      FBD-Version
      Async-mode
      Login-dlg
      F
      C
      BTN-CLICK
      Open-Problem
      Check-Entries
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
      Warning-MsgBox
      Axes-dlg
      Axesprops
      CLOSE-APP
      END-LOG
      |#
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle parse - takes an appropriate action based on command passed
;;        a) an output stream
;;        b) the current linenumber
;;        d) string containing the time line was entered
;;        d) string containing the command
;;        e) string containing the arguments for the command
(defun handle-parse (stream linenumber time command arguments)
  (let ((tmp (assoc command **workbench-commands** :test #'equal)))
    (if tmp
	(safe-apply stream (second tmp) (list stream time arguments))
      nil))) ;; ignore command -- not one we're intersested in

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde-result (stream time arguments)
  (cond
   ((equal "|T|" arguments)
    (cond
     ((eq **response** 1)
      (format stream "  Andes: Correct!~%"))
     ((eq **response** 2)
      (format stream "  Andes: Okay.~%"))
     (t
      nil)))
   ((equal "||" arguments) "")
   ((equal "|NIL|" arguments)
    (format stream "  Andes: That is not correct.~%"))
   (t
    (let ((index (search "show-hint" arguments))
	  (index2 (search "show-lesson" arguments))
	  (index1 (position #\~ arguments)))
      (if index
	  (if index1
	      (format stream "  Andes: ~A~%"
		      (l-format
		       (remove #\|
			       (subseq arguments (+ index (length "show-hint") 1) index1))))
	    (format stream "  Andes:~A~%"
		    (l-format (remove #\|
				      (subseq arguments (+ index (length "show-hint") 1))))))
	(if index2
	    (format stream "  Andes shows lesson: ~A.~%"
		    (remove #\| (subseq arguments (+ index2 (length "show-lesson") 1))))
	  (if (equal (char (remove #\| arguments) 0) #\!)
	      (format stream "  Andes: ~A~%" (l-format (subseq (remove #\| arguments) 1)))
	    (format stream "  Andes answers '~A'~%" (l-format (remove #\| arguments))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde (stream time arguments)
  (let ((command-list (read-from-string arguments)))
    (let ((tmp (assoc (car command-list) **help-system-commands**)))
      (if tmp 
	  (format stream "~A"
		  (string-trim " " (l-format (safe-apply
					      stream (second tmp) (list (rest command-list))))))
	(format stream "~%***Unhandled DDE command '~A'~%~%" arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde-failed (stream time arguments)
  (format stream "~% *****************************~% * Andes Help System Failed. * ~A~% *****************************~%~%" arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun handle-dde-post (stream time arguments)
  (let ((command-list (read-from-string arguments)))
    (let ((tmp (assoc (car command-list) **help-system-commands**)))
      (if tmp 
	  (format stream "~A"
		  (string-trim " " (l-format (safe-apply
					      stream (second tmp) (list (rest command-list))))))
	(format stream "~%***Unhandled DDE-POST command '~A'~%~%" arguments)))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specific to helpsystem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to track equations by slot #
(defparameter **equations**
    '())

(defun add-equation (equation slot)
  (if (member slot **equations** :test #'(lambda (x y) (equal x (car y))))
      (remove slot **equations** :test #'(lambda (x y) (equal x (car y)))))
  (setf **equations** (append **equations** (list (list slot equation)))))

(defun delete-equation (slot)
  (if (member slot **equations** :test #'(lambda (x y) (equal x (car y))))
      (remove slot **equations** :test #'(lambda (x y) (equal x (car y))))))

(defun get-equation (slot)
  (let ((tmp (assoc slot **equations**)))
    (second tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; same intent as **workbench-commands** only for help system commands
;;  in - is arguments as called from work bench
;; NOTE: routines expected to return a string
(defparameter **help-system-commands**
    '(;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (read-student-info
       (lambda (in)
	 (respond 2)
	 (setf **student-name** (car in))
	 (format nil "~A: I am ~A.~%" **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (read-problem-info
       (lambda (in)
	 (respond 2)
	 (format nil "~%~A: Open problem ~A. *************************************~%"
	  **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (get-proc-help
       (lambda (in)
	 (respond 0)
	 (format nil "~%~A asks what should be done next?~%" **student-name**)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (handle-student-response
       (lambda (in)
	 (respond 1)
	 (if (equal (car in) "OK")
	     ""
	   (if (null (cdr in))
	       (if (atom (car in))
		   (cond
		    ((equal 'Cancel (car in))
		     (format nil "~A ignores the question.~%" **student-name**))
		    ((equal 'avg-velocity (car in))
		     (format nil "~A chooses average velocity.~%" **student-name**))
		    ((equal 'net-disp (car in))
		     (format nil "~A chooses new displacement.~%" **student-name**))
		    ((equal "Continue" (car in))
		     (format nil "~A chooses to continue with current plan.~%" **student-name**))
		    ((equal "Replan" (car in))
		     (format nil "~A wishes to replan.~%" **student-name**))
		    ((equal 'lk (car in))
		     (format nil "~A chooses linear kinematics.~%" **student-name**))
		    ((equal 'close-lesson (car in))
		     (format nil "~A closes this lesson.~%" **student-name**))
		    ((equal 'wt-law (car in))
		     (format nil "~A chooses the weight law.~%" **student-name**))
		    ((equal 'num-forces (car in))
		     (format nil "~A chooses the number of forces.~%" **student-name**))
		    ((equal 'static-friction (car in))
		     (format nil "~A chooses the static friction.~%" **student-name**))
		    ((equal 'kinetic-friction (car in))
		     (format nil "~A chooses the kinetic friction.~%" **student-name**))
		    ((equal 'ang-sdd (car in))
		     (format nil "~A chooses angular distance/rate/time.~%" **student-name**))
		    ((equal 'free-fall-accel (car in))
		     (format nil "~A chooses free fall acceleration.~%" **student-name**))
		    ((equal 'lk-no-s-avg-accel (car in))
		     (format nil "~A chooses linear kinematics with no displacement or average acceleration.~%" **student-name**))
		    (t
		     (format nil "~A:0 ~W~%" **student-name** in)))
		 (if (equal (caar in) 'define-variable)
		     (if (nth 6 (car in))
			 (if (nth 4 (car in))
			     (if (nth 2 (car in))
				 (format nil "~A answers: ~A ~A between the ~A and the ~A.~%"
					 **student-name** (nth 2 (car in)) (nth 3 (car in))
					 (nth 4 (car in)) (nth 6 (car in)))
			       (format nil "~A answers: ~A between the ~A and the ~A.~%"
				       **student-name**  (nth 3 (car in))
				       (nth 4 (car in)) (nth 6 (car in))))
			   (if (nth 2 (car in))
			       (format nil "~A answers: ~A ~A of the ~A.~%"
				       **student-name** (nth 2 (car in)) (nth 3 (car in))
				       (nth 6 (car in)))
			     (format nil "~A answers: ~A of the ~A.~%"
				     **student-name** (nth 3 (car in)) (nth 6 (car in)))))
		       (if (nth 2 (car in))
			   (format nil "~A answers: ~A ~A of the ~A.~%"
				   **student-name** (nth 2 (car in)) (nth 3 (car in))
				   (nth 4 (car in)))
			 (format nil "~A answers: ~A of the ~A.~%"
				 **student-name** (nth 3 (car in)) (nth 4 (car in)))))
		   (format nil "~A:1 ~A~%" **student-name** in)))
	     (format nil "~A:2 ~A~%" **student-name** in)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (explain-more
       (lambda (in)
	 (respond 0)
	 (format nil "~A requests more help.~%" **student-name**)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (assert-object
       (lambda (in)
	 (respond 0)
	 (format nil "~A drew a body called '~A' for the ~A at time ~A.~%" **student-name**
	  (first in) (second in) (third in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (lookup-force
       (lambda (in)
	 (respond 0)
	 (if (equal (second in) "Net")
	     (format nil "~A draws a Net force called ~A that acted on the ~A during ~A at a direction of ~A degrees.~%" **student-name** (first in) (third in) (nth 6 in) (nth 4 in))
	   (format nil "~A draws a ~a force called ~A that acted on the ~A during ~A at a direction of ~A degrees.~%" **student-name** (second in) (first in) (third in) (nth 6 in) (nth 4 in)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (lookup-vector
       (lambda (in)
	 (respond 0)
	 (if (nth 4 in)
	     (format nil "~A draws a vector called '~A' for the ~A ~A of the ~A at time ~A at a direction of ~A degrees.~%" **student-name** (first in) (second in) (third in) (fourth in) (nth 6 in) (nth 4 in))
	   (format nil "~A draws a zero-length vector called '~A' for the ~A ~A of the ~A at time ~A.~%" **student-name** (first in) (second in) (third in) (fourth in) (nth 5 in)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (assert-x-axis
       (lambda (in)
	 (respond 0)
	 (format nil "~A draws axis ~A,~A,~A at ~A degrees.~%" **student-name**
	  (nth 3 in) (nth 4 in) (nth 5 in) (second in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (solve-for-var
       (lambda (in)
	 (respond 0)
	 (format nil "~A asks Andes to solve for '~A'~%" **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (check-answer
       (lambda (in)
	 (respond 0)
	 (format nil "~A states that the answer is '~A'.~%" **student-name** (subseq (car in) 0 2))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (close-problem
       (lambda (in)
	 (respond 0)
	 (format nil "~A is finished with problem ~A for now.~%"
	  **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (close-lesson
       (lambda (in)
	 (respond 0)
	 (format nil "")))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (exit-andes
       (lambda (in)
	 (respond 0)
	 (format nil "~A: I'm finished with Andes for now.~%" **student-name**)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (get-dialog-response
       (lambda (in)
	 (respond 0)
	 (format nil "~A answers with '~A'~%" **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (calculate-equation-string
       (lambda (in)
	 (respond 0)
	 (format nil "~A asks Andes to solve the equation '~A'~%" **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (lookup-eqn-string
       (lambda (in)
	 (respond 0)
	 (cond
	  ((equal "" (car in))
	   (let ((tmp (get-equation (second in))))
	     (when tmp
	       (delete-equation (second in))
	       (format nil "~A deletes the equation '~A'.~%" **student-name** tmp))))
	  (t
	   (add-equation (car in) (second in))
	   (format nil "~A enters the equation: ~A~%" **student-name** (car in))))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (why-wrong-equation
       (lambda (in)
	 (respond 0)
	 (format nil "~A wonders what is wrong with equation #~A.~%" **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (delete-object
       (lambda (in)
	 (respond 0)
	 (if (car in)
	     (format nil "~A deletes object '~A'.~%" **student-name** (car in))
	   "")))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (why-wrong-object
       (lambda (in)
	 (respond 0)
	 (format nil "~%~A asks why '~A' is wrong.~%" **student-name** (car in))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (assert-compound-object
       (lambda (in)
	 (respond 0)
	 (format nil
	  "~A defines a compound body called '~A' over the time ~A made of the bodies ~A.~%"
	  **student-name** (first (car in)) (second (car in)) (third (car in)))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (define-variable
	  (lambda (in)
	    (respond 0)
	    (if (fifth in)
		(format nil "~A defines the variable called '~A' to be the ~A at time ~A~%"
			**student-name** (first in) (third in) (fifth in))
	      (cond
	       ((equal 'coef-friction (third in))
		(format
		 nil
		 "~A defines the variable called '~A' to be the coefficient of friction.~%"
		 **student-name** (first in)))
	       (t
		(format nil "~A defines the variable called '~A' to be the ~A~%"
			**student-name** (first in) (third in)))))))
      ))

(defun wb-quant (&rest args) (car args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; capture list errors and write error to stream
(defconstant **trap-lisp-errors** t) ;; t turns safe-apply on

(defun safe-apply (stream fn args)
  (if **trap-lisp-errors**
      (let (result)
	(handler-case (setf result (apply fn args))
	  (error (c) (format stream "**** Error: executing command ~A.~%" c) :error)
	  (:no-error (c) (declare (ignore c)) result)))
    (apply fn args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formats paragraphs to block width characters wide
(defconstant ***width*** 72)

(defun z-next-word (str)
  (cond
   ((equal str "") 0)
   ((position #\Space str) (position #\Space str))
   (t (length str))))

(defun l-format (the-str)
  (let ((frm "") (cnt 0) (str (string-trim " " the-str)))
    (do ((done nil (equal (length (string-trim " " str)) 0)))
	(done (string-trim " " frm))
      (let ((tmp (z-next-word str)))
	(when (> tmp 0)
	  (if (>= (+ tmp cnt) ***width***)
	      (setf frm (concatenate 'string frm (format nil "~A" #\LineFeed) "        "
				     (string-trim " " (subseq str 0 tmp))))
	    (setf frm (concatenate 'string frm " " (string-trim " " (subseq str 0 tmp)))))
	  (setf str (string-trim " " (subseq str tmp)))
	  (if (>= (+ tmp cnt) ***width***)
	      (setf cnt tmp)
	    (setf cnt (+ cnt tmp 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test routines and development support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rld () ;; reload file after possible edits
  (load "C:/Andes2/Help/SLog2English.cl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run () ;; test run batch
  (process-files "C:/Andes2/Help/Evals/" #'is-file-type-p '("log") #'parse-line '(handle-parse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chk (&optional (args '("A19-Nov26-18-12-35" "log")))
  (process-files "C:/Andes2/Help/Evals/" #'is-exactly-p args #'parse-line '(handle-parse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
