#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LogReader.cl
;;; Collin Lynch 
;;; 5/13/2001
;;;
;;; The logreader is designed to take generic Andes logs and to open them
;;; as a stream.  The incoming log will be opened as a file stream and the
;;; relavant header data such as the version number and so on will be read
;;; off.  The file will then be made availible for reading via the get-next
;;; function.  The get-next function simply returns the next line in the 
;;; file stored as a line struct.  The line struct can then be parsed
;;; as needed by other tools.  
;;;
;;; TODO:: Username and time parsing.
|#

;;;; ========================================================================
;;;; The logreader struct is intended to encapsulate an open logfile that 
;;;; we are parsing.  When created it should be loaded with the open 
;;;; stream as well as information such as the helpsystem version, workbench
;;;; version, async mode and date.  This will all automatically be read off
;;;; from the file before any other reading is to be done.  

(defstruct LogReader
  path       ;; The path to the file being opened.
  Version    ;; The Andes/HelpSystem version of the system.
  WBVersion  ;; The Workbench version of the log.
  AsyncMode ;; The asynchronous mode of the file.
  Time       ;; The Eventually a universaltime.
  UserName   ;; The student's username.
  (LineNum 0);; The current line number in the file.
  LastLine   ;; Stores a copy of the last line read in the file.
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Internal fields.
  Stream     ;; The file stream itself.
  ;;Buffer     ;; Overflow Buffer to deal with the fact that
             ;; Some DDEs can be sent before a DDE response
             ;; Arrives. 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Settings that control the parsing behavior
  ;; These fileds will affect what is done by the 
  ;; reader when specific situations are encountered.
  (EofError Nil)          ;; Will an error be generated on "normal" eof.
  )



;;;; ========================================================================
;;;; Line
;;;; Lines are intended to encapsulate the form of each unparsed logline.
;;;; loosely in order to read a line from the log we need to parse it into 
;;;; four values, a line number, a time, a type, and a value.  The Line is
;;;; what will be returned by the parser for later use.

(defstruct (logLine (:print-function print-logline))
  Type
  Num
  Time
  Value)


(defun print-logline (Line &optional (Stream t) (Level 0))
  "Print out the logline."
  (declare (ignore Level))
  (format Stream "~a: ~a ~a ~a~%"
	  (Logline-Num Line) (Logline-Time  Line)
	  (Logline-Type Line) (Logline-Value Line)))


;;;; ========================================================================
;;;; Open
;;;; Given a logfilename open the file and set its basic parameters before 
;;;; parsing the first command.

(defun open-LogReader (Filename &key (EOFError Nil))
  "Open the specified logfile and set the info."
  (let ((Reader (make-LogReader :Path FileName :Stream (open FileName)
				:EOFError EofError)))
    (when (LogReader-Stream Reader)
      (logReader-set-nameandTime Reader)
      (logreader-set-Versions Reader)
      Reader)))


(defun LogReader-Open-Logfile (FileName)
  "Open the specified logfile for the user."
  (open FileName :direction :input
	:if-does-not-exist :error))


;;; The Logfile time is encoded into the filename.  For now we will
;;; just take the filename whole.  Once I'm sure that this works I 
;;; will make the system deal better with parsing.
(defun logreader-set-nameandtime (Reader)
  "Set thelogreader time."
  (setf (LogReader-Time Reader) (LogReader-Path Reader))
  (setf (LogReader-UserName Reader) (LogReader-Path Reader)))


;;; The version numbers, (wb and HelpSys) and the Ansync mode of the file 
;;; are all located within the first few lines of the file itself.  They
;;; appear as normal lines.  In order to set them and to prepare the file
;;; for reading this code will read from the file in order to obtain the
;;; relavent lines and then let the reader start reading from that point
;;; when the file is done.  If a DDE or end-log is encountered before the
;;; values are set then an improper-fileheader error will be thrown.  
(defun logreader-set-versions (Reader)
  "Set the logreader versions."
  (setf (LogReader-Version Reader) Nil)
  (setf (LogReader-WBVersion Reader) Nil)
  (setf (logReader-AsyncMode Reader) Nil)
  (let ((line (logreader-readline Reader)))
    (if (null line) (error "Empty file supplied.")
      (logreader-sv-loop Reader Line))))


(defun logreader-sv-loop (Reader Line)
  "Set the type based upon the line and recurse if necessary."
  (if (or (null Line) (member (LogLine-Type Line) '(dde dde-post check-entries End-Log)))
      (error "Improper Fileheader."))
  
  (case (LogLine-Type Line)
      (Andes-Version (setf (Logreader-Version Reader) (LogLine-Value Line)))
      (FBD-Version (setf (Logreader-WBVersion Reader) (LogLine-Value Line)))
      (Async-Mode (setf (LogReader-AsyncMode Reader) 
		    (read-from-string (LogLine-Value Line)))))
  
  (if (not (and (logreader-version Reader) (Logreader-wbversion Reader) 
		(LogReader-AsyncMode Reader)))
      (logreader-sv-loop Reader (logreader-readline Reader))))
  

  


  

;;; =========================================================================
;;; Close the specified logreader freeing up the space.
(defun close-logreader (Reader)
  "Close the logreader's stream."
  (close (LogReader-Stream Reader)))


;;; =========================================================================
;;; Reading a line from the log is a matter of popping the next line off of 
;;; the stream and Splitting it into the relavent components before returning
;;; it to the user.  No more needs to be done.  Comment strings will be ignored.
;;;
;;; If an empty line is encountered then it will be ignored

;; This was written for debugging purposes.
;;(defun foo (Fname)
;;  (with-open-file (F Fname :direction :input)
;;    (do ((Count 0 (+ Count 1))
;;	 (Line (read-line F) (Read-line F)))
;;	((null Line) (format t "Line nil ~a" Count))
;;      (format t "~w~%" Line))))

;; Read the next line from the log stream and perform some rudimentary parsing
;; on it before returning it as a logline to the user.  This function maintains
;; the current line number in the logreader as well as detecting and skipping
;; comments.  It also explaits a side-effect of setf to set the logreader's 
;; lastline field to the new Logline and to return that value.  
(defun LogReader-ReadLine (Reader)
  "Read the next line from the Log Stream."
  (let ((Line (read-line (LogReader-Stream Reader) 
			 (LogReader-EOFError Reader) nil)))
    (when Line 
      (incf (LogReader-LineNum Reader))
      (if (or (string-equal Line "") (LogReader-Comment-StringP Line))
	  (LogReader-ReadLine Reader)
	(setf (Logreader-LastLine Reader)
	  (LogReader-MakeLine (LogReader-LineNum Reader) Line))))))
	

;;; Comments are prefixed with a ; or # and will 
;;; be ignored in the processing.
(defun LogReader-Comment-StringP (String)
  "Is the specified line a comment (prefixed with # or ;)?"
  (or (char= (char String 0) #\#) 
      (char= (char String 0) #\;)))


;;; Given a Raw string split it into the individual components 
;;; and reorder them into a tuple <Type> <LineNum> <Time> <Rest>
(defun LogReader-MakeLine (LineNum Line)
  "Split the supplied line in an appropriate way."
  ;;(format t "SL: ~a ~a ~a~%" (length Line) (char Line 0) Line)
  (let* ((tab (position #\Tab Line))
	 (spc (position #\Space Line :start tab))
	 (Time (subseq Line 0 Tab))	
	 (Type (if Spc (subseq Line Tab Spc) (subseq Line Tab)))
	 (Rest (if Spc (subseq Line (+ 1 Spc)))))
    (make-logline :type (read-from-string Type)
		  :Num LineNum :time Time :Value Rest)))





;;; ================================================================
;;; Testing
;;; Open the specified log(s) and run them.

(defun testLogReader (&optional (Path "./"))
  "Test all the logs in path iterate and print lines."
  (let ((files (directory (concatenate 'string Path "*.log"))))
    (dolist (F files)
      (format t "~%##### Starting File: ~a #########################~%" F)
      (let (R Out) 
	(setq Out (open (concatenate 'string (namestring F) ".ln")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create))
	(handler-case 
	    (progn (setq R (open-logreader F))
		   (format Out "~a~2%" R)
		   (do ((CMD (LogReader-ReadLine R) (LogReader-ReadLine R)))
		       ((null CMD) (format t "### EOF Found ######################~%"))
		     (format Out "~a~%" CMD))
		   (close-Logreader R))
	  (error (E) (format Out "**ERROR**:: ~a~%" E)))
	(close Out))
      (excl:gc)
      )))


(defun tstlr ()
  (mapcar #'testLogReader 
	  '("c:/Andes2/Log/" "C:/Andes2/Log/Pitt/Fall1999/"
	    "C:/Andes2/Log/Pitt/Fall2000/"
	    "C:/Andes2/Log/Pitt/Fall2001/"
	    "C:/Andes2/Log/USNA/Fall1999/"
	    "C:/Andes2/Log/USNA/Fall2000/"
	    "C:/Andes2/Log/USNA/Fall2001/"
	    "C:/Andes2/Log/USNA/Fall2002/")))

;;(testLogReader "c:/Andes2/Log/")
;;(testLogReader "C:/Andes2/Log/Pitt/Fall2001/")
;;(testLogReader "C:/Andes2/Log/USNA/Fall2001/")
;;(testLogReader "C:/Andes2/Log/USNA/Fall2002/")



;;; =================================================================
;;; Tracing

(defun trace-logreader ()
  (trace open-logreader
	 logreader-open-logfile
	 logreader-set-nameandtime
	 logreader-set-versions
	 logreader-sv-loop
	 
	 close-logreader
	 
	 logreader-readline
	 logreader-comment-stringp
	 logreader-makeline))
	 
	 
	 
	 
	 
	 
