;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readlogs.lsp/cl - provides historical tracking of data from student logs
;; Copyright (C) 2001 by <Linwood H. Taylor's employer> -- All Rights Reserved.
;; Author(s)
;;   Linwood H. Taylor -- (lht) -- <lht@lzri.com>
;; Modified:
;;   13 August 2001 - (lht) -- created
;; Note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; just to nake writing/testing/debugging of this file quicker
(defun rldrl ()
  (load "ReadLogs"))
(defun tsrl ()
  (readlogs-check-all))
(defun tsge ()
  (readlogs-grab-equations))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these are for specifying other paths instead of defaults
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *LogRoot* "T:/VanLehn/Andes2/Log/")
(defun readlogs-set-root (&optional (root "T:/VanLehn/Andes2/Log/"))
  (if (stringp root)
      (setf *LogRoot* root)
    (format t "Argument to readlogs-set-root needs to be a string")))

(defparameter *SubjectsDirectory* "Subjects/")
(defun readlogs-subjects-directory (dir)
  (if (stringp dir)
      (setf *SubjectsDirectory* dir)
    (format t "Argument to readlogs-subjects-directory needs to be a string")))

(defparameter *GreenDirectory* "Green/")
(defun readlogs-green-directory (dir)
  (if (stringp dir)
      (setf *GreenDirectory* dir)
    (format t "Argument to readlogs-green-directory needs to be a string")))

(defparameter *RedDirectory* "Red/")
(defun readlogs-red-directory (dir)
  (if (stringp dir)
      (setf *RedDirectory* dir)
    (format t "Argument to readlogs-red-directory needs to be a string")))

(defparameter *BuggyDirectory* "Buggy/")
(defun readlogs-buggy-directory (dir)
  (if (stringp dir)
      (setf *BuggyDirectory* dir)
    (format t "Argument to readlogs-buggy-directory needs to be a string")))

(defparameter *OutputDirectory* "/Andes2/Temp/")
(defun readlogs-output-directory (dir)
  (if (stringp dir)
      (setf *BuggyDirectory* dir)
    (format t "Argument to readlogs-buggy-directory needs to be a string")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-process-workbench-file (file outdir)
  (with-open-file (str file :direction :input)
    (do ((c 0 (+ 1 c))
	 (info (read-line str nil 'eof) (read-line str nil 'eof)))
	((eql info 'eof))
      (if (and (> (length info) 0) (not (eq (char info 0) #\#)))
	  (if (search "DDE-RESULT" info)
	      (with-open-file (str (format nil "~A~A~A" outdir (pathname-name file) ".log")
			       :direction :output
			       :if-exists :append
			       :if-does-not-exist :create)
		(format str "(~W ~W)~%" 
			c (subseq info (+ 11 (search "DDE-RESULT" info)))))
	    (if (search "DDE-POST" info)
		(with-open-file (str (format nil "~A~A~A" outdir (pathname-name file) ".log")
				 :direction :output
				 :if-exists :append
				 :if-does-not-exist :create)
		  (format str "(~W ~W)~%"
			  c (concatenate 'string "!" 
					(subseq info (+ 9 (search "DDE-POST" info))))))
	      (if (search "DDE" info)
		  (with-open-file (str (format nil "~A~A~A" outdir (pathname-name file) ".log")
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)
		    (format str "(~W ~W)~%"
			    c (concatenate 'string "?" (format nil "~A" c) ":"
					   (subseq info (+ 4 (search "DDE" info)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-type-workbench (file)
  (with-open-file (str file :direction :input)
    (let ((line (read-line str nil 'eof)))
      (and (not (eql line 'eof))
	   (> (length line) 0)
	   (eq (char line 0) #\#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-check-specific (theDirectory &optional (outdir *OutputDirectory*)
						       (fn #'run-help))
  (if (stringp outdir)
      (dolist (infile (directory (format nil "~A~A" *LogRoot* theDirectory)))
	(format t "Reading file [~W]~%" infile)
	(if (equal (pathname-type infile) "log")
	    (if (readlogs-type-workbench infile)
		(readlogs-process-workbench-file infile outdir)
	      (readlogs-process-help-file infile outdir fn))))
    (format t "Argument to readlogs-check-subjects needs to be a string")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-check-subjects (&optional (outdir *OutputDirectory*) (fn #'run-help))
  (readlogs-check-specific *SubjectsDirectory* outdir fn) ;; convert to help format
  (readlogs-set-root "")
  (readlogs-check-specific outdir outdir fn)
  (readlogs-set-root)
  (dolist (file (directory outdir))
    (if (equal (pathname-type file) "log")
	(delete-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-clean-good (outdir)
  (dolist (file (directory outdir))
    (if (equal (pathname-type file) "checked")
	(let ((okay nil))
	  (with-open-file (str file
			   :direction :output
			   :if-exists :append
			   :if-does-not-exist :create)
	    (if (eql (file-length str) 0)
		(setf okay t)))
	  (if okay
	      (format t "No changes recorded in ~A~%" (pathname-name file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-safe-apply (ostr fn args) 
  (let (result)
    (handler-case 
	(setf result (apply fn args))
      (error (c) 
    	(error-message (format ostr "Executing command: ~A" c))
        :error)
      (:no-error (c) 
	(declare (ignore c))
    	result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-process-help-file (infile outdir fn)
  (format t "Checking ~A" infile)
  (solver-load)
  (physics-algebra-rules-initialize)
  (parse-initialize)
  (symbols-reset)
  (with-open-file (str infile :direction :input)
    (do ((quest (read str nil 'eof) (read str nil 'eof)))
	((eql quest 'eof))
      (let ((question (format nil "~A" quest)))
	(if (and (> (length question) 0) (not (eq (char question 0) #\;)))
	    (let* ((query (second quest))
		   (q (if (equal (char query 0) #\?) (position #\: query)))
		   (c (if (and (equal (char query 0) #\!)) t)))
	      (if (or q c)
		  (let* ((response (read str nil 'eof))
			 (pert (eql response 'eof))
			 (sr (if pert nil (format nil "~A" (second response)))))
		    (if (not pert)
			(if q
			    (let ((id (read-from-string (subseq query 1 q)))
				  (cmd (read-from-string (subseq query (+ q 1)))))
			      (readlogs-safe-apply
			       t fn (list
				     (format nil "~A~A.checked" outdir (pathname-name infile))
				     id cmd sr)))
			  (if c
			      (let ((cmd (read-from-string (subseq query 1))))
				(readlogs-safe-apply
				 t fn (list
				       (format nil "~A~A.checked" outdir (pathname-name infile))
				       -1 cmd sr)))
			    (format t "Undefined Command Request in ~W~%"
				    (pathname-name infile))))))
		(format t "Expected Command Request in ~W~%"
			(pathname-name infile))))))))
  (solver-unload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-help (outname id cmd ans)
  (with-open-file (ostr outname
		   :direction :output :if-exists :append :if-does-not-exist :create)
    (let ((nans (format nil "~A" (readlogs-safe-apply ostr (car cmd) (cdr cmd)))))
      (if (not (equalp nans (remove #\| ans)))
	  (format ostr "Command ~W) ~W returned ~W;~%           instead of ~W~%" id cmd nans ans)
	(if **CheckVerbose**
	    (format ostr "Command ~W) ~W returned ~W as expected~%" id cmd nans))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun grab-equations (outname id cmd ans)
  (with-open-file (ostr "/Andes2/Temp/Equations.eqn"
		   :direction :output :if-exists :append :if-does-not-exist :create)
    (if (equal (car cmd) 'lookup-eqn-string)
	(format ostr "(setf **eqtst** (append **eqtst** (list ~W)))~%" (second cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-for-all-logs (&optional (outdir *OutputDirectory*) (fn #'run-help))
  (readlogs-check-specific *GreenDirectory* outdir fn)
  (readlogs-check-specific *RedDirectory* outdir fn)
  (readlogs-check-specific *BuggyDirectory* outdir fn)
  (readlogs-check-subjects outdir fn)
  (readlogs-clean-good outdir))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-grab-equations ()
  (readlogs-for-all-logs *OutputDirectory* #'grab-equations))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun readlogs-check-all ()
  (readlogs-for-all-logs *OutputDirectory* #'run-help))

(defun readlogs-test-equations()
  (let ((**eqtst** nil))
    (load "/Andes2/Temp/Equations.eqn")
    (physics-algebra-rules-initialize)
    (parse-initialize)
    (symbols-reset)
    (let ((good-cnt 0)
	  (empty-cnt 0)
	  (multiple-cnt 0)
	  (invalid-cnt 0))
      (dolist (eq **eqtst**)
	(with-open-file (ostr "/Andes2/Temp/Equations.txt"
			 :direction :output :if-exists :append :if-does-not-exist :create)
	  (let ((equation (string-trim " " eq)))
	    (format nil "Handling Equation <~W>" equation)
	    (if (= 0 (length (remove #\Space equation)))
		(progn
		  (setf empty-cnt (+ empty-cnt 1))
		  (format ostr "Equation::Empty Equation <~W>~%" eq))
	      (let* ((assignmentp (position #\= equation))
		     (parses (if assignmentp (parse-equation **grammar** equation) nil))
		     (complete (if assignmentp (parse-get-complete parses) nil))
		     (valid (if assignmentp (parse-get-valid 'final complete) nil)))
		(cond
		 ((= (length valid) 0)
		  (setf invalid-cnt (+ invalid-cnt 1))
		  (format t "~W: ... Syntax Error~%" equation)
		  (format ostr "Equation::~A Invalid parses in <~W>~%" (length parses) eq)
		  (dolist (p parses) (format ostr "~W~%" p)))
		 ((> (length valid) 1)
		  (setf multiple-cnt (+ multiple-cnt 1))
		  (if (> (length valid) 10)
		      (format t "~W: ... Many parses~%" equation))
		  (format ostr "Equation::~A Multiple parses <~W>~%" (length valid) eq)
		  (dolist (p valid) (format ostr "~W~%" p)))
		 (t
		  (setf good-cnt (+ good-cnt 1))
		  (format ostr "Equation::Good parse <~W>~%~W~%" equation valid))))))))
      (format t "Stats: good ~A~%       invalid ~A~%       multiple ~A~%       empty ~A~%"
	      good-cnt invalid-cnt multiple-cnt empty-cnt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of read-logs.lsp/cl
;; Copyright (C) 2001 by <Linwood H. Taylor's employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
