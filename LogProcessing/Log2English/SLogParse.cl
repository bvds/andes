;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLogParse -- routines to supporting parsing of student logs written by workbench
;; Copyright (C) 2001 by <Linwood H. Taylor's (lht@lzri.com) Employer> -- All Rights Reserved
;; Author(s): Linwood H. Taylor (lht@lzri.com)
;; Modified:
;;      13 December 2001 - (lht) -- created/split from SLog2English.cl
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
;; NOTE: must define the function make-parse-line-output-file-stream-name (file lineno line)
;;  that will return a string representing the filename of the output stream
(defun parse-line (file lineno line handler &optional (handler-args nil))
  (let ((name (make-parse-line-output-file-stream-name file lineno line)))
    (with-open-file (str name
		     :direction :output :if-exists :append :if-does-not-exist :create)
      ;;(format t "Output '~A'~%" name)
      (cond
       ((<= (length line) 0) nil)
       ((eq (char line 0) #\#)
	(let ((fnd (search " by " line)))
	  (if fnd
	      (format str "~A~%" (subseq line 0 fnd))))) ;; ignore system comments
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
	    (format str "*** Unhandled line #~A.~%  ~A" lineno line))))))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of SLogParse.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's (lht@lzri.com) Employer> -- All Rights Reserved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;