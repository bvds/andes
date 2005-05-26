;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history.lsp/cl - provides historical tracking of data
;; Copyright (C) 2001 by ?????????????????????????????? -- All Rights Reserved.
;; Author(s)
;;   Linwood H. Taylor -- (lht) -- <lht@lzri.com>
;; Modified:
;;   12 March 2001 - (lht) -- created
;;   ?? July 2001 - (lht) -- added utilities for processing log files
;; Note(s):
;;   Use is intended for creating logs of all data transcations specified
;;   Items are written (one per line) in the form
;;       ((date) (data stored))
;;   Log utilities are at end of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar **History** nil
  "The log list itself.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history-new - define name of history log file
;; argument(s):
;;  fileName - string containing base name of file
;; returns
;;  full pathname of file to be used to hold history logged information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun history-new (fileName)
  "Define a new history log"
  (setf **History** (andes-path (format nil "~A~A.log" fileName 
			    (get-universal-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history-log - writes specified data to current histroy log file
;; argument(s):
;;  data that will be 'listed and then written to file
;; returns
;;  nil ... always
;; note(s):
;;  probably should alter to return something a bit more useful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun history-log (data &optional (isComment nil))
  "Add a history entry"
  (if **History**
      (with-open-file
	  (aStream **History** :direction :output
	   :if-exists :append :if-does-not-exist :create)
	(if isComment
	    (format aStream "~W~%" (list (get-universal-time) (format nil "#~W" data)))
	  (format aStream "~W~%"
		  (list (get-universal-time) (if (stringp data) data (format nil "~W" data)))))))
  data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; below here are utilities for testing results from log files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun process-history (file fn)
  (setf **ignore-errors** nil)
  (format t "Begin of ~W~%" file)
  (with-open-file (str file :direction :input)
    (do ((question (read str nil 'eof)
		   (read str nil 'eof)))
	((eql question 'eof))
      (let* ((query (second question))
	     (q (if (and (equal (char query 0) #\?)) (position #\: query)))
	     (c (if (and (equal (char query 0) #\!)) t)))
	(if (or q c)
	    (let* ((response (read str nil 'eof))
		   (pert (eql response 'eof))
		   (sr (if pert nil (format nil "~A" (second response)))))
	      (if (not pert)
		  (if q
		      (let ((id (read-from-string (subseq query 1 q)))
			    (cmd (read-from-string (subseq query (+ q 1)))))
			(funcall fn id cmd sr (car question) (car response)))
		    (if c
			(let ((cmd (read-from-string (subseq query 1))))
			  (funcall fn -1 cmd sr (car question) (car response)))
		      (format t "Undefined Command Request in ~W~%" file)))
		(format t "End of ~W~%" file)))
	  (format t "Expected Command Request in ~W~%" file))))))
;;(history-for-all-andes-log "/Andes2/Log/" #'history-grab '(lookup-eqn-string))
;;(history-for-all-andes-log "/Andes2/Log/" #'history-grab-2-file '(lookup-eqn-string "Equations.linn"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support functions to use in calls to process-history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun print-log (id cmd ans st en)
  (format t "Command ~W) ~W returned ~W within ~W seconds.~%" id cmd ans (+ (- en st) 1)))

(defparameter **HistoryDebugLog** "Check")
(defparameter **CheckVerbose** nil)

(defun run-as-wb (id cmd ans start end)
  (declare (ignore Start End))
  (let ((nans (format nil "~A" (safe-apply (car cmd) (cdr cmd)))))
    (if (not (equalp nans ans))
	(with-open-file
	    (str **HistoryDebugLog** :direction :output
	     :if-exists :append :if-does-not-exist :create)
	  (format str "Command ~W) ~W returned ~W;~%           instead of ~W~%" id cmd nans ans))
	(if **CheckVerbose**
	    (with-open-file
		(str **HistoryDebugLog** :direction :output
		 :if-exists :append :if-does-not-exist :create)
	      (format str "Command ~W) ~W returned ~W as expected~%" id cmd nans))))))

(defmacro grab-all (type &optional (file nil))
  (if file
      `#'(lambda (id cmd ans start end)
	   (if (equal (car cmd) ,type)
	       (with-open-file
		   (aStream ,file :direction :output
		    :if-exists :append :if-does-not-exist :create)
		 (format aStream "~A ;; (~A) ~A~%" cmd (+ (- end start) 1) ans))))
    `#'(lambda (id cmd ans start end)
	 (if (equal (car cmd) ,type)
	     (format t "~A ;; (~A) ~A~%" cmd (+ (- end start) 1) ans)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support functions to use in calls to process-history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun history-test (infile &optional (outfile "Check"))
  (solver-load)
  (enable-debug)
  (doSafety :in2pre)
  (history-new "Andes")
  (enable-errors)
  (physics-algebra-rules-initialize)
  (parse-initialize)
  (symbols-reset)
  (setf **HistoryDebugLog** (format nil "/Andes2/Log/~A~A.log" outfile (get-universal-time)))
  (process-history infile #'run-as-wb)
  (solver-unload))

(defun history-dump (file)
  (process-history file #'print-log))

(defun history-grab (file type &optional (out nil))
  (process-history file (grab-all type out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to process all files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun history-for-all-andes-log (root fn &optional (args nil)) ;like "/Andes2/Log/"
  (setf **ignore-errors** nil)
  (dolist (file (directory root))
    (if (and (equal (pathname-type file) "log")
	     (equal (subseq (pathname-name file) 0 5) "Andes"))
	(if args
	    (safe-apply fn (cons file args))
	  (safe-apply fn (list file))))))
;;(history-for-all-andes-log "/Andes2/Log/" #'history-grab '(lookup-eqn-string))
;;(history-for-all-andes-log "/Andes2/Log/" #'history-grab '(lookup-eqn-string "Equations.linn"))

;;; this will run the help system for each AndesXXXXXXXXXX.log file in "/Andes2/Log/" and
;;; write any errors to files called CheckNNNNNNNNNNN.log
(defun history-batch-confirm ()
  (history-for-all-andes-log "/Andes2/Log/" #'history-test))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of history.lsp/cl
;; Copyright (C) 2001 by ????????????????????????????? -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
