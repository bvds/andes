;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time2English -- The routines to transform a colon separated time as stored by the workbench
;;     Logs into a useable time for the runtime log system.  
;; Copyright (C) 2001 by <Linwood H. Taylors (lht@lzri.com) Employer> -- All Rights Reserved
;; Author(s): Linwood H. Taylor (lht@lzri.com) Collin Lynch (Collinl@pitt.edu)
;; Modified:
;;      2 December 2001 - (lht) -- created
;;     12 December 2001 - (lht) -- 'finished'
;;     13 December 2001 - (lht) -- split into sub files to facilitate processing files for other
;;                                 data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The code in this file performs two roles.  The first is to translate "Log format" time into 
;; an integer of seconds and back again.  The second is to keep a running log of time as a parser
;; is in operation and to report on it as necessary.  The code is grouped into these two sections
;; below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; ---------------------------------------------------------------------------
;;; Time Tracking
;;; As files are processed iteratively, it is sometimes necessary to keep track

(defparameter **tracking-time** nil)
(defparameter **turns** 0)
(defparameter **TotalTime** 0)
(defparameter **StartTime** 0)
(defparameter **lapsed-time** 0)
(defparameter **StudentTotalTime** 0)
(defparameter **HelpSystemTotalTime** 0)
(defparameter **help-system-turns** 0)
(defparameter **student-turns** 0)
(defparameter **current-problem** "")
(defparameter **StartProblem** "")

(defparameter **get-proc-help-count** 0)
(defparameter **explain-more-count** 0)
(defparameter **solve-for-var-count** 0)
(defparameter **calculate-equation-string-count** 0)
(defparameter **why-wrong-equation-count** 0)
(defparameter **why-wrong-object-count** 0)


(defun handle-time-quit-tracking (time)
  ;;(format t "Stop Tracking~%")
  (setf **TotalTime** (- (time2secs time) **StartProblem**))
  (setf **tracking-time** nil)
  (setf **lapsed-time** 0))

(defun handle-time-start-tracking (time)
  ;;(format t "Start Tracking~%")
  (setf **tracking-time** t)
  (setf **turns** 0)
  (setf **TotalTime** (time2secs time))
  (setf **StudentTotalTime** 0)
  (setf **HelpSystemTotalTime** 0)
  (setf **help-system-turns** 0)
  (setf **student-turns** 0)
  (setf **StartTime** (time2secs time))
  (setf **get-proc-help-count** 0)
  (setf **explain-more-count** 0)
  (setf **solve-for-var-count** 0)
  (setf **calculate-equation-string-count** 0)
  (setf **why-wrong-equation-count** 0)
  (setf **why-wrong-object-count** 0)
  (setf **lapsed-time** 0))

(defun handle-time-stats-failed (time)
  ;;(format t "Failed ~A (~A)~%" time (time2secs time))
  (handle-time-stats-reply time))

(defun handle-time-stats-reply (time)
  (when (equal **tracking-time** t)
    (setf **help-system-turns** (+ **help-system-turns** 1))
    (let ((current-time (time2secs time)))
      (setf **lapsed-time** (- current-time **StartTime**))
      (setf **HelpSystemTotalTime** (+ **HelpSystemTotalTime** 1 **lapsed-time**))
      ;;(format t "Replied CT: ~A ST: ~A S: ~A H: ~A~%"
      ;;current-time **StartTime** **StudentTotalTime** **HelpSystemTotalTime**)
      (setf **StartTime** current-time))))

(defun handle-time-stats (time)
  (when (equal **tracking-time** t)
    (setf **student-turns** (+ **student-turns** 1))
    (let ((current-time (time2secs time)))
      (setf **lapsed-time** (- current-time **StartTime**))
      (setf **StudentTotalTime** (+ **StudentTotalTime** **lapsed-time**))
      ;;(format t "   Said CT: ~A ST: ~A S: ~A H: ~A~%"
      ;;current-time **StartTime** **StudentTotalTime** **HelpSystemTotalTime**)
      (setf **StartTime** (time2secs time)))))

(defun handle-time-stats-post (time)
  (when (equal **tracking-time** t)
    (setf **student-turns** (+ **student-turns** 1))
    (let ((current-time (time2secs time)))
      (setf **lapsed-time** (- current-time **StartTime**))
      (setf **StudentTotalTime** (+ **StudentTotalTime** **lapsed-time**))
      ;;(format t "   Post CT: ~A ST: ~A S: ~A H: ~A~%"
      ;;current-time **StartTime** **StudentTotalTime** **HelpSystemTotalTime**)
      (setf **StartTime** (time2secs time)))))

(defun handle-time-report (&optional (file "Time-Stats.txt"))
  (with-open-file (str (namestring file)
		   :direction :output :if-exists :append :if-does-not-exist :create)
    ;;(format str (concatenate 'string "Problem,Student,,Total,Student,HelpSys,,Student,"
    ;;			     "HelpSys,,Student,HelpSys,,Help,Explain,Solve,Calculate,"
    ;;	  	             "Wrong,Wrong~%"))
    ;;(format str (concatenate 'string ",,,Time,Time,Time,,Action,Action,,Ave.,Ave.,,"
    ;;			     "Count,Count,Count,Count,Equation,Object~%"))
    (format str "~A,~A" **current-problem** **student-name**)
    (format str ",,~A,~A,~A" **TotalTime** **StudentTotalTime** **HelpSystemTotalTime**)
    (format str ",,~A,~A" **student-turns** **help-system-turns**)
    (format str ",,~A,~A"
	    (if (equal 0 **student-turns**)
		0
	      (float (/ **StudentTotalTime** **student-turns**)))
	    (if (equal 0 **help-system-turns**)
		**help-system-turns**
	      (float (/ **HelpSystemTotalTime** **help-system-turns**))))
    (format str ",,~A,~A,~A,~A,~A,~A"
	    **get-proc-help-count** **explain-more-count** 
	    **solve-for-var-count** **calculate-equation-string-count**
	    **why-wrong-equation-count** **why-wrong-object-count**)
    (format str "~%")))





;;; ---------------------------------------------------------------------------------------------
;;; Translation
;;; Log format time is a string of the form "[<Hours>:]<Minutes>:<Seconds>" where <Minutes> and 
;;; <Seconds> are two digit base ten represntations of the time and <Hours> is a flexible length
;;; number.  C2i is a simple char-to-int function.

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


