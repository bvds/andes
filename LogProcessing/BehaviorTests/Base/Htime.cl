#|===============================================================================
;;; Htime.cl
;;; Collin Lynch
;;; 7/2/2002
;;;
;;; the htime is a simple struct specifying time in terms of hours minutes
;;; and seconds.  Rtime is used to keep track of the time since the student
;;; began this run of Andes, and will be used for comparison purposes.  
;;; This is a fairly simple hack but it allows us to run with the system
;;; quite well.  
;;;
;;; The code in this file allows the system to specify the time as well as 
;;; get the current time, and compare times.  The **base-Htime** is a stored
;;; universal time that will be used as a relative offset for the stored
;;; time value to determine the current time relative to the time that the
;;; help system was started.  
|#


;;;;================================================================
;;;; Stored parameters

(defparameter **base-Htime** ()
  "The base from which htimes will be asessed.")




;;;;=================================================================
;;;; Structure definition.
;;;;
;;;; The Htime tracks times in terms of hours, minutes, and 
;;;; seconds.  By default it is parsed in from strings as and 
;;;; printed out in the form [H:]M:S with the days being added
;;;; to the hours times 24.  This was done to make it conform
;;;; to the standards of the runtime logs.

(defstruct (Htime (:print-function print-htime))
  "Print out the htime as specified."
  (Sec 0)
  (Min 0)
  (Hour 0))
    


(defun print-htime (time &optional (Stream t) (Level 0))
  (declare (ignore Level))
  (format Stream "~a:~a:~a" 
	  (htime-hour time) (htime-min time) (htime-sec time)))



;;; Parse the time as stored in string form into an htime by reading 
;;; in the numbers and then returning the values as a new htime.
;;; For the purposes of parsing the Htime form is assumed to be:
;;;       [[D:]H:]M:S
;;; This code will search for the separating colons and read the 
;;; individual components from the string into their component forms.
;;;
;;; The logs will always return data in the [H:]M:S case but that 
;;; may not be the case with others.  If the data includes a days 
;;; filed then that will be added to the hours.
(defun parse-htime (String)
  "Parse the string-form of the htime."
  (let* ((SMDiv (position #\: String :from-end t))
	 (MHDiv (position #\: String :From-end t :end SMDiv))
	 (HDDiv (if (null MHDiv) Nil (position #\: String :from-end t :end MHDiv)))
	 (time (make-htime :sec (read-from-string (subseq String (+ 1 SMDiv))))))
    
    (if (null MHDiv) 
	(setf (Htime-Min Time) (read-from-string (subseq String 0 SMDiv)))
      (progn 
	(setf (Htime-Min Time) (read-from-string (subseq String (+ 1 MHDiv) SMDiv)))
	(if (null HDDiv) 
	    (setf (htime-Hour Time) (read-from-string (subseq String 0 MHDiv)))
	  (progn
	    (setf (htime-hour Time) 
	      (+ (read-from-string (subseq String (+ 1 HDDiv) MHDiv))
		 (* 24 (read-from-string (subseq String 0 HDDiv)))))))))
    
    Time))
    
    

(defun new-htime (Sec Min Hour)
  "Generate a new Htime corresponding to the specified time."
  (make-htime :Sec Sec :Min Min :Hour Hour))


;;;;=================================================================
;;;; Functional definitions

;;; Get the current htime (time since **base-htime**)  This involves
;;; getting the current hour, min, sec, and date (day) from the current
;;; universal time, and then subtracting the base htime value from it.
(defun get-current-htime (&optional (base-htime (or **Base-Htime** (make-htime))))
  "Get the current htime (relative to the supplied base."
  (sub-htimes (universal-time->htime (get-universal-time)) base-htime))

;;; Convert the specified universal time to an htime.
;;; taking into account the 
(defun universal-time->htime (UTime)
  "Convert the xurrent utime to an htime."
  (let ((decoded 
	 (multiple-value-list 
	  (if UTime (decode-universal-time UTime)
	    (get-decoded-time)))))
    (make-htime :sec (nth 0 decoded) :Min (nth 1 Decoded)
		:Hour (+ (nth 2 Decoded) (* 24 (nth 3 decoded))))))


;;; Convert the Htime to a raw number of seconds
(defun convert-Htime->Secs (Htime)
  "Convert the Htime to a raw number of seconds."
  (+ (* 3600 (Htime-Hour Htime))
     (* 60 (Htime-Min Htime))
     (Htime-Sec Htime)))


;;; Convert the Htime to a raw number of minutes
(defun convert-Htime->Min (Htime)
  "Convert the Htime to a raw number of Minutes."
  (+ (* 60 (Htime-Hour Htime))
     (Htime-Min Htime)
     (if (Htime-Sec Htime)
	 (/ (Htime-Sec Htime) 60))))


;;; Subtract one time from the next.
(defun sub-htimes (&rest htimes)
  (cond ((null htimes) (error "No times supplied for sub."))
	((= 1 (length htimes)) (car htimes))
	(t (apply #'sub-htimes 
		  (sub-htimes-int (car Htimes) (cadr Htimes))
		  (cddr Htimes)))))

;;; Note, the logic below is intended to propogate values back, and
;;; is generall built under the assumption that we won't be dealing
;;; in negative time values.  
(defun sub-htimes-int (H1 H2)
  (let (tmp (Sec (- (htime-sec H1) (htime-sec H2)))
	(Min (- (htime-Min H1) (htime-Min H2)))
	(Hour (- (htime-Hour H1) (htime-Hour H2))))
	
    (when (> 0 Min) 
      (setq tmp (ceiling (/ (abs Min) 60))) 
      (setq Hour (- Hour Tmp))
      (setq Min (+ Min (* tmp 60))))

    (when (> 0 Sec) 
      (setq tmp (ceiling (/ (abs Sec) 60))) 
      (setq Min (- Min Tmp))
      (setq Sec (+ Sec (* tmp 60))))
    
    (new-htime Sec Min Hour)))


;;; Subtract one time from the next.
(defun add-htimes (&rest htimes)
  (cond ((null htimes) (error "No times supplied for sub."))
	((= 1 (length htimes)) (car htimes))
	(t (apply #'add-htimes 
		  (add-htimes-int (car Htimes) (cadr Htimes))
		  (cddr Htimes)))))

(defun add-htimes-int (H1 H2)
  (let (tmp (Sec (+ (htime-sec H1) (htime-sec H2)))
	(Min (+ (htime-Min H1) (htime-Min H2)))
	(Hour (+ (htime-Hour H1) (htime-Hour H2))))
	
    (when (< 60 Sec)
      (setq tmp (floor (/ Sec 60)))
      (setq Min (+ Min tmp))
      (setq Sec (mod Sec 60)))
    
    (when (< 60 Min)
      (setq tmp (floor (/ Min 60)))
      (setq Hour (+ Hour tmp))
      (setq Min (mod Min 60)))
    
    (new-htime Sec Min Hour)))
    



;;; Apply the specified comparison function to the
;;; times returning the result so long as the comparison
;;; returns t for all i OP j in the list.
(defun htimes-comp (func &rest Htimes)
  (if (= 2 (length Htimes)) 
      (funcall func (car Htimes) (cadr Htimes))
    (and (funcall func (car Htimes) (cadr Htimes))
	 (apply #'Htimes-comp func (cdr Htimes)))))


(defun htimes< (&rest Htimes)
  "Run a < test on the htimes."
  (apply #'htimes-comp 
	 #'(lambda (H1 H2) 
	     (< (convert-Htime->Secs H1) 
		(convert-Htime->Secs H2)))  
	 Htimes))


(defun htimes<= (&rest Htimes)
  "Run a < test on the htimes."
  (apply #'htimes-comp 
	 #'(lambda (H1 H2) 
	     (<= (convert-Htime->Secs H1) 
		 (convert-Htime->Secs H2)))  
	 Htimes))

(defun htimes>= (&rest Htimes)
  "Run a < test on the htimes."
  (apply #'htimes-comp 
	 #'(lambda (H1 H2) 
	     (>= (convert-Htime->Secs H1) 
		 (convert-Htime->Secs H2)))  
	 Htimes))


(defun htime->Secs (Htime)
  "Calculate the Htime in Seconds."
  (+ (htime-Sec Htime)
     (* 60 (+ (htime-Min Htime)
	      (* 60 (+ (Htime-Hour Htime)))))))






  #|  Alternate parse-htime code.
  (defun parse-htime (String)
  (let ((lst (parse-htime-int String)))
    (make-htime :Sec (nth 0 Lst)
		:Min (nth 1 Lst)
		:Hour (nth 2 Lst)
		:Day (nth 3 Lst))))


;;; Recursively parse the colon-separated integers from the string
;;; throwing an error if they are not ints and returning the list
;;; of values when they are done.
(defun parse-htime-int (String &optional Index Lst)
  "Parse the internal Htime string by colons."
  (let* ((Pos (if (null Index) (position #\: String)
		(position #\: String :start Index)))
	 (Val (if (null Pos) 
		  (if (read-from-string (subseq String Index))
		(read-from-string (subseq String Index (+ 1 Pos)
    
    (values Val Pos))
  
  
    (multiple-value-setq (Val End)
      (parse-htime-readval String))
    (setf (htime-sec Time) Val)
    (if (null End) Time 
      (parse-htime String End Time #'htime-min))))


;;; Parse a portion of the htime between the end and a 
;;; separator colon (if present) or the beginning of the
;;; string.  If the read does not return a number then 
;;; return nil.
;;;
;;; NOTE: Supplying Nil to subseq is a valid argument.
;;;  it acts as if no value was supplied.
(defun parse-htime-readval (String &optional End)
  (let* ((Pos (if (null End) (position #\: String :from-end t) 
		(position #\: String :from-end t :end End)))
	 (Val (if (null Pos) (read-from-string (subseq String 0 End))
		(read-from-string (subseq String (+ 1 Pos) End)))))
    
    (values Val Pos)))
|#
