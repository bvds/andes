#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cmdreader.cl
;;; Collin Lynch
;;; 3/30/2004
;;;
;;; Copyright 2003 Kurt VanLehn.
;;;
;;; The purpose of the commandstream is to pull the dde-commands and their 
;;; results from a logreader in stream form.  The Cmdreader is meant to be 
;;; compatible with Andes2 and Andes1 logfiles from 2000 on.  
;;;
;;; Opening a cmdreader will generate a new cmdreader which can then be 
;;; accessed by read-next-cmd.  This will return a CMD along with its 
;;; result set to the user.  
;;;
;;; The parsing of the file should appear constant no matter what version of
;;; the logs are being parsed.  the system will return CMDs in sequetial order 
;;; save for async 1 logs where the dde's may overlap.  More on that in notes.
;;;
;;; The first section of this file defines the cmdreader struct and the cmd and 
;;; cmdresult funtions.  The next section defins the open-cmdreader function 
;;; followed by the along with the close.  Lastly the reader functions are defined
;;; followed by tracing information and debugging tests.
;;;
;;; The buffer here is used to store temporary lines such as dde-posts that we may
;;; need for later on.  The buffer can only be in a few legal states at any given 
;;; time.  It may contain DDE-Posts DDE-Commands, DDE-results and DDE-Faileds but only
;;; where the following caveats hold:
;;;
;;;  1. only 1 DDE-Post may be present unless mult-interim-posts is t.  
;;;  2. DDE-Commands may be present only if we are are in Async-Mode 1
;;;     and we are searching for or have just completed searching for 
;;;     an asynchronous (dde-post) lookup-eqn-string call.
;;;  3. If a DDE-Result or DDE-Failed is present then either it must 
;;;     be on its own (located below dde-posts and commands only) or
;;;     Async-mode must be 1 and we are searching for the result of an
;;;     asynchronous lookup-eqn-string.
;;;
;;; Error checking for this takes place when we are looking up dde-results
;;; or looking up the asynchronous result of a lookup-eqn-string.
;;;
;;; Todo:: get logfilename and time reading working.
;;;
;;; NOTE:: One of the issues with running this code lies in the distinction
;;;  between windows and unix sides.  The Windows side outputs \r\n on each
;;;  line of the logfile.  If this code is run over the logfiles on windows
;;;  then it will process these lines appropriately.  If, however the code 
;;;  is run on unix then it will throw errors if the excess \r has not yet
;;;  been stripped from each line of the files.  

|#

;;(load "./Base/Htime")
;;(load "./Base/LogReader")
;;(load "./Base/CMD")

;;;; =======================================================================
;;;; The cmdreader is a wrapper for the logreader struct
;;;; it contains a logreader as well as specific fields controlling the 
;;;; command reading behavior.

(defstruct cmdreader 
  Logreader  ;; The logreader itself.
  Buffer  ;; A line buffer for dealing with interim results and dde-posts.
  
  (Mult-Interim-Posts Nil)  ;; Do we allow more than 1 interim DDe-Post
                          ;; Between a DDE-call and its result.
  (ErrorOnDDEEOF t)       ;; Do we return an error when EOF is encountered
                          ;; While searching for a DDE Reault/Failed.

  ;; This was done because in log-parsing the ignoring
  ;; check-entries will need to be done and it is more 
  ;; efficient to do it here.
  (ignore-check-entries nil) ;; If t then the system will ignore all ddes located
                             ;; between "check-entries 1" and "check-entries 0"

  ;; The use of check-entries is complicated by the existednce of new-style check
  ;; entries commands and old style.  On some more recent logs the check-entries 
  ;; commands appear as ddes.  Therefore when post-processing the logs it is 
  ;; necessary to keep those in mind.  In the future I may add a programming
  ;; flag here to handle code changes around this but it is not necessary at 
  ;; present.  
  
  ;; When searching in async-mode then we will encounter async lookup-eqn-strings
  ;; if convert-les-dde is t and async-mode is 1 then they will be "converted" 
  ;; into ddes (by chaging the type) and the eq0results will be "converted" into
  ;; dde-results in order to simplify later parsing.
  ;;(convert-ales-dde nil)


  ;; When Searching through this file do we expect to see "set-score" commands or
  ;; not.  If so then the system will deal with them appropriately.  If not then
  ;; it will throw an error.  
  (handle-set-scores Nil)
  

  ;; ----------------------------------------------------------------
  ;; State variables
  
  ;; in-check-entries is used to keep track of the current state of
  ;; processing.  If in-check-entries is t then the system is not
  ;; ignoring check-entries and has encountered a check-entries 1
  ;; line but not a check-entreies 0 line.
  (in-check-entries Nil)

  ;; This variable like lastline in the logreader is used to keep track of the 
  ;; last cmd read from the command reader.  It permits the user to regain 
  ;; the previously used cmd for their own purposes.  
  lastcmd 
  )


;;; The logreader has an asyncmode value.  Since it may be necessary for the
;;; cmdreader it is set here.
(defun cmdreader-asyncmode (Reader)
  "The cmdreader-async-mode."
  (logreader-AsyncMode (cmdreader-logreader Reader))) 


;;; -----------------------------------------------------
;;; Buffer control functions
;;; The buffer can contain any line of any type 

(defun cmdreader-clear-buffer (Reader)
  "Clear the commandreader-buffer."
  (setf (cmdreader-buffer Reader) Nil))

(defun cmdreader-pop-buffer (Reader)
  "Clear the commandreader-buffer."
  (pop (cmdreader-buffer Reader)))

(defun cmdreader-prepend-to-buffer (Lines Reader)
  "Prepend the list of items to the buffer."
  (setf (cmdreader-buffer Reader)
    (append Lines (cmdreader-buffer Reader))))

(defun cmdreader-append-to-buffer (Lines Reader)
  "Append the list of items to the buffer."
  (setf (cmdreader-buffer Reader)
    (append (cmdreader-buffer Reader) Lines)))

(defun cmdreader-add-to-buffer (Line Reader)
  "Add the specified item to the buffer."
  (setf (cmdreader-buffer Reader)
    (append (cmdreader-buffer Reader) (list Line))))




;;;; =========================================================================
;;;; Open a cmdreader is a matter of opening the underlying logreader 
;;;; without error and then returning a new cmdreader struct containing it.
;;;; todo add flags as they are supported.
(defun open-cmdreader (Filename &key (ignore-check-entries Nil) (handle-set-scores Nil))
  "Open a commandstream to the specified file."
  (make-cmdreader 
   :LogReader (open-logreader Filename)
   :ignore-check-entries Ignore-check-entries
   :Handle-Set-Scores Handle-Set-Scores))


;;;; ==========================================================================
;;;; Closing a cmdreader is a matter of closing the underlying logreader

(defun close-cmdreader (Cmdreader)
  "Close the specified cmdreader."
  (when (and Cmdreader (cmdreader-p Cmdreader))
    (close-logreader (cmdreader-logreader cmdreader))))



;;;; =========================================================================
;;;; Reading from a cmdreader is a different and more complex issue.  because 
;;;; of the different versions of the logs it is necessary to change the 
;;;; behavior of the reader based upon specific settings. 
;;;;

;;; --------------------------------------------------------------------------
;;; Parsing CMDs
;;; Parsing off the CMDs is a matter of reading the next element off of the 
;;; buffer or off of the Stream if the buffer is empty.  The buffering is 
;;; done because it is possible under some circumstances for dde-commands to 
;;; be overlapped.  That is, under some circumstances the system will submit 
;;; a DDE command and then follow it with another DDE-post before the Original 
;;; DDE returned.  If things become more complex then so will the buffer.  
;;;
;;; The parsing proceeds as follows.  Given a request for the next CMD, the 
;;; search will first parse through the Buffer in search of a DDE or DDE-Post.
;;; If one is found then it will produce a new command with it and begin searcing
;;; for a dde result and possible assocs (assocs always come immediately before 
;;; dde results.  If not then it will begin pulling commands off of the stream
;;; until one is found ignoring anything else that is found.  
;;;
;;; Once a DDE or DDE-Post is found, if the command is a DDE then the system will
;;; begin searching for a DDE-Result/Assoc pair.  If a DDE is encountered before 
;;; a pair is reached then an error will be thrown.  If a DDE-Post is encountered
;;; before a pair is reached then it will be added to the buffer.  Once a pair is
;;; found then that pair will be parsed and added to the DDE command's slots.
;;;
;;; Any lines other than DDE's DDE-Posts, DDE-Results and DDE-Failed's will be 
;;; ignored.  
;;;
;;; Parsing the stream is a matter of reading off from it until we get to a dde-command
;;; or dde-post.  Any errant dde-results that we encounter in the process will cause 
;;; an error to be reaised.  IF the end of the file is encountered then Nil will be 
;;; returned.  
;;;
;;; DDEs
;;; See below
;;;
;;; Parsing DDE-Posts
;;; When a DDE-Post has been obtained then we set the CMD appropriately
;;; and print out the relavent information.  Since DDE-Posts get no
;;; responses we only need to fill in the initial information and return
;;; the CMD.  The rest can be left as-is.
;;;
;;; DDE-Commands 
;;; DDE commands are asynchronous calls that come from the help system to 
;;; the workbench (as opposed to DDE-Posts which are async from the WB to 
;;; the HS.  If they are encountered when searching for DDE-Results then 
;;; they will be added to the DDE.  If not then they will be returned
;;; on their own as one of the possible CMD types.
;;;
;;; NOTE:: This code sets the lastcmd call when executed.  Because it is recursive
;;;  this is not, perhaps, the most efficient way to go about it but it does get
;;;  the job done.  
(defun read-next-cmd (Reader &key (Verbose Nil))
  (declare (ignore Verbose))
  (let (CMD (Line (cmdreader-readline Reader)))
    (when Line
      (setq CMD
      	(case (logline-type Line)
	  (DDE (cmdreader-process-DDE Line Reader))
	  (DDE-Post (cmdreader-process-DDE-Post Line Reader))
	  (DDE-Command (cmdreader-generate-CMD Line Reader))
	  (Check-Entries (cmdreader-process-CheckEntries Line Reader))
	  (t (Read-Next-CMD Reader))))
      (if CMD (setf (cmdreader-lastcmd Reader) CMD)))))




;;; -----------------------------------------------------------------------------
;;; There are two ways to read a line the first is to read it directly from the 
;;; logreader contained within the cmdreader.  The second is to the read it off
;;; of the cmdreader's internal buffer.  If the buffer has a line in it then that
;;; is what will be read by default if not then it will be read from the stream
;;; under other circumstances (below) the system will read directly from the 
;;; underlying logreader.
(defun cmdReader-ReadLine (Reader)
  "Read the next line from buffer or reader."
  (if (cmdReader-Buffer Reader) 
      (cmdreader-pop-buffer Reader)
    (cmdreader-readline-direct Reader)))

;;; Read the specified line directly from the underlying logreader thus bypassing
;;; the buffer.  This will be directly called when we are searching for results.
(defun cmdreader-readline-direct (Reader)
  "Read the next line from the logreader in the cmdreader."
  (logreader-readline (cmdreader-logreader Reader)))




;;; -------------------------------------------------------------------------------
;;; DDE-POST
;;; DDE-Posts can be simple to process or complex to process.  The difficulty lies
;;; in the lookup-equation-string calls, and in the existence of the set-score 
;;; commands.
;;;
;;; For the 1999 (and possibly a few of the 2000 logs) the system ran in async-mode
;;; where equation submissions were sent as asynchronous dde-posts.  The result was
;;; returned in a line labeled EQ-Result.  Because this is sent asynchronously the
;;; line can be intermixed with other ddes and dde-posts.  There is also no 
;;; guarantee about the order in which the lines are returned.  These calls will be
;;; parsed by this code into cmds and the asynchronous calls will be stored in the
;;; cmd-result field.
;;;
;;; Aditionally.  Whenever the student hits enter in an empty equation box, deletes 
;;; an equation, or, has the cursor in an equation box when the box loses focus then
;;; a dde-post of the form (lookup-eqn-string "" ?id) will be sent as a dde-post.
;;; This call will get no result.  This is true for all logs.  These calls will be
;;; stored as under the class of 'deletes but the call itself will remain the same
;;; the utils section at the base of this file has a delete-equation-cmdp function
;;; that can be used to test whether a call is a delete-equation or not.  
;;;
;;; When a dde-post is found it will be tested.  If the call is a lookup-eqn-string
;;; then the system will parse it appropriately.  If it is a delete call then the
;;; system will define it as a cmd and return without searching for a result.  If
;;; the call has an equation string then if we are in async mode the result will be
;;; sought and the interim lines buffered for later use.  If not then the system 
;;; will throw an error.
;;;
;;; Typically the dde-posts have no "result" per-se.  However the dde-posts can alter
;;; the scores.  Therefore, if the cmdreader is set to look for set-score commands 
;;; then this code will search for the next set-score command and generate a result
;;; for this dde-post containing that set score.  All interim lines will be appended 
;;; to the buffer for later processing.  If no such score is found then the code will
;;; throw an error.  The score flag is 'handle-set-scores'
;;;
;;; The lone dde-post that is exempted from this is the exit-andes call which will 
;;; terminate the session and will therefore have no scores. 
(defun cmdreader-process-dde-post (Line Reader)
  "Process the dde-post for data."
  (let ((call (cmdreader-hash-parse-lineval Line)))
    (if (equalp (car Call) 'Lookup-eqn-string)
	(cmdreader-process-eqn-post Call Line Reader)
      (cmdreader-generate-cmd 
       Line Reader :call Call
       :Result (when (and (cmdreader-handle-set-scores Reader)
			  (not (or (equalp (car Call) 'exit-andes))))
				 ;;  (equalp (car Call) 'check-entries))))
		 (cmdreader-process-dde-post-scores Reader))))))


;;; Given a lookup-eqn-string dde-post determine if it is a deletion and if we are 
;;; or are not in async mode.  Parse deletes appropriately.  If it is not a delete
;;; then, if we are in async mode parse it appropriately else throw an error.
(defun cmdreader-process-eqn-post (Call Line Reader)
  (if (cmdreader-empty-stringp (nth 1 Call))
      (cmdreader-generate-delete-eqn Call Line Reader)
    (if (= 1 (cmdreader-asyncmode Reader))
	(cmdreader-process-async-les Call Line Reader)
      (error "Lookup-eqn-string found on non-async dde-post."))))


;;; A string is "empty" if it is "" or contains only whitespace characters.
(defun cmdreader-empty-stringp (Str)
  (or (equal (length Str) 0)
      (and (member (char Str 0) '(#\Space #\Tab))
	   (cmdreader-empty-stringp (subseq Str 1)))))


;;; Given a lookup-eqn-string that is also a delete then generate a call
;;; containing the data but explicitly setting the type to be a delete.
;;; Note that, If a delete is found there may still be an effect on the
;;; the scores therefore this code will search for a set-scores connected
;;; with this call.  There will be no eqn-result so this code will only
;;; search for the scores.
(defun cmdreader-generate-delete-eqn (Call Line Reader)
  "Generate a Deletion equation with the specified call line etc."
  (cmdreader-generate-cmd 
   Line Reader :Class 'DELETE :Call Call
   :Result (when (cmdreader-handle-set-scores Reader)
	     (cmdreader-process-dde-post-scores Reader))))


;;; Processing an async-mode lookup-eqn-string is a matter of searching for the
;;; asynchronous eq-result function and then parsing it.  The Eq-result is of
;;; the form:  
;;;    EQ-RESULT <id> <Result>  
;;;  where <id> is the numerical equation id the 2nd argument in the call and
;;;  <Result> is an entry-result-type response from the call.
(defun cmdreader-process-async-les (Call Line Reader)
  (let ((id (or (nth 2 Call) (error "Null-async-les id: ~a" Line))))
    ;;(pprint (nth 1 Call))
    (cmdreader-generate-cmd 
     Line Reader :Call Call 
     :Result (cmdreader-lookup-eq-result Call id Reader))))

    
;;; Searching for an eq-result is simply a matter of popping successive lines 
;;; off of the buffer and then the stream until a logline of type EQ-Result is 
;;; found and then parsing the result.  The difficulty in doing this lies in
;;; the fact that the calls are asynchronous.  Therefore it is possible for 
;;; any number of dde's dde-posts and other asynchronous calls (including 
;;; eq-results) to appear in the list.  The search will add everything to the
;;; buffer until an eq-result with a matching id is found.
;;;
;;; We do not need to deal with set-score values here as the asynchronous 
;;; lookup-eqn-string calls were only used in the 1999 logs.  The set-score
;;; features were not added until 2003.  
(defun cmdreader-lookup-eq-result (Call id Reader)
  "Lookup the eq result."
 (let ((Cache (cmdreader-buffer Reader)))
   (pprint Cache)
   (cmdreader-clear-buffer Reader)
    (do* ((Buffer Cache (cdr Buffer))
	  (Line (or (car Buffer) (cmdreader-readline-direct Reader))
		(or (car Buffer) (cmdreader-readline-direct Reader))))
	((cmdreader-ler-term-test Line Call id Reader)
	 (cmdreader-append-to-buffer Buffer Reader)
	 (cmdreader-ler-term Line id reader))
      (pprint Buffer)

      (cmdreader-add-to-buffer Line Reader))))


;;; We want to stop searching for the cmdresult when nil is encountered
;;; or we locate an eq-result whose id matches the id that we are searching 
;;; for.  Alternately if we encounter a dde-failed whose value is identical
;;; to the original dde-post's then we want to terminate as such.
;;;
;;; Note the parse of line for dde-failed is due to the fact that fix-quotes
;;; introduces spaces around the equation.  This is a simple hack that ensures
;;; that any additions due to the parsing process are replicated when testing
;;; for equality.
(defun cmdreader-ler-term-test (Line Call id Reader)
  (declare (ignore Reader))
  ;; (format t "id: ~a ~a~%" id (read-from-string (logline-value Line)))
  (or (null Line)
      (and (equal (logline-type Line) 'EQ-RESULT)
	   (equal id (read-from-string (logline-value Line))))
      (and (equal (logline-type Line) 'DDE-FAILED)
	   (equalp Call (cmdreader-hash-parse-lineval Line)))))


;;; Once we have found a termination test then, if the line is null then 
;;; we want to throw an error of error-ondde-eof is t else the system will
;;; parse the line and return the relavent cmdresult.
(defun cmdreader-ler-term (Line id Reader)
  "Parse the eq result and return the term."
  (if Line (cmdreader-ler-parse-eq-result Line Reader)
    (when (cmdreader-erroronddeeof Reader)
      (error "Eof encountered on Eq-Result search at ~a for ~a."
	     (Logline-num Line) id))))


;;; The result of a dde-post lookup-eqn-string can be an eq-result or a
;;; dde-failed.  If it is an eq-result then the value will be a 
;;; status-return val.  If it is a dde-failed then the value will be a
;;; repeat of the dde-result and will be stored as a string.
;;; 
;;; An eq-reuslt is of the form:
;;;   EQ-Result <id> <Value>
;;; where <id> is the id that we have been searching for and <Value> is 
;;; a result-state that will be parsed.  
;;;
;;; Note: The class will be eq-result although this might be changed
;;;       into a dde-result if it becomes necessary to facilitate
;;;       processing.
(defun cmdreader-ler-parse-eq-result (line Reader)
  (declare (ignore reader))
  (let ((sp (position #\Space (logline-value Line))))
    (make-cmdresult
     :Class (Logline-Type Line)
     :LineNum (Logline-Num Line)
     :Time (logline-time Line)
     :Value (case (Logline-Type Line)
	      (DDE-FAILED (Logline-Value Line))
	      (EQ-RESULT (cmdreader-make-status-return-val
			  (subseq (logline-value Line) (+ 1 Sp))))
	      (otherwise (error "unrecognized eq-result type: ~a" Line))))))



;;; When the system is reading in a dde-post we will have it search for a 
;;; set-score by iterating over the log until one is located.  The first
;;; set-score found will be associated with a result and returned.  If no
;;; set-score is found then an error should be thrown.  
(defun cmdreader-process-dde-post-scores (Reader)
  (let* ((Buffer (cmdreader-buffer Reader))
	 (nextfunc #'(lambda () 
		       (if Buffer (pop Buffer)
			 (cmdreader-readline-direct Reader)))))
    ;;(pprint Buffer)
    (cmdreader-clear-buffer Reader)
    (do ((line (funcall nextfunc) (funcall nextfunc)))
	((and (equalp (logline-type Line) 'DDE-COMMAND)
	      (string-equal "set-score" (subseq (logline-value Line) 0 9)))
	 (cmdreader-append-to-buffer Buffer Reader)
	 (make-cmdresult :score Line))
      ;; (pprint Line)
      ;; (pprint (subseq (logline-value line) 0 8))
      (if (null Line) 
	  (error "Premature EOF found in dde-post-scores.")
	(cmdreader-add-to-buffer Line Reader)))))
  

	     


	 
;;; ---------------------------------------------------------------------------
;;; Parsing a DDE is a matter of locating the corresponding DDE result and any
;;; associated DDE commands (such as assocs or open-browser).  The Assumption 
;;; is that any dde-commands that occur after a DDE call but prior to the
;;; corresponding DDE-result or DDE-failed are assumed to be a result of the 
;;; DDE call and will be included (as CMDs under Commands.  Separate Commands
;;; will be produced on their own.
;;;
;;; Note that if no result class is found for the dde then an error will be thrown.
;;;
;;; Collection of dde-results depends upon the result-type.  The assumption is that 
;;; the next dde-result found either in the buffer or on the stream will be 
;;; the dde-result associated with the call in question.
;;;
;;; The search begins by cycling thought the buffer.  If the corresponding 
;;; dde-result or dee-failed is found then it will be associated with the cmd.  
;;; The assumption is that the first dde-result found in the buffer will be 
;;; the one that we are searching for.  If an intervening dde-command is found then 
;;; it may be associated with the dde.  If the command is an assoc then it will be 
;;; appended to the result if it is the first one found.  If more than one is found
;;; then an error will be thrown.  If the command is a set-score then it will be 
;;; associated with the result if it is the first one.  All other set-scores will
;;; be appended to the buffer for later.  All other commands will be associated 
;;; with the result.  
;;;
;;; If, however the flag handle-set-scores is set to nil and a set score is encountered
;;; then this code will throw an error.  
;;;
;;; If a dde is encountered before the result is found then an error will be thrown.
;;;
;;; If a dde-post is encountered then the search will test to see if one has already
;;; been found.  If not then it will add it to the buffer.  If so then the system will
;;; test to see if Mult-Interim-Posts is t if so it will be ignored.  If not then an
;;; error will be thrown.  
;;;
;;; If the result is found in the buffer and more remains then it will be left in the
;;; buffer for future reads.
;;;
;;; The code here begins generates the cmd and sets its result through the search process
(defun cmdReader-process-DDE (Line Reader)
  (let* ((cmd (cmdreader-generate-cmd Line Reader))
	 (Class (lookup-commandclass->resultclass (cmd-class cmd))))
    
    ;; Test for an unrecognized class error.
    (when (null class)
      (error "Unrecognized dde-type found ~a ~a." 
	     (logline-value Line) (logreader-linenum (cmdreader-Logreader Reader))))
    
    ;; Generate and return the cmd.
    (setf (cmd-result cmd) (make-cmdresult))
    (cmdreader-lookup-dde-result Reader Class (cmd-result cmd))
    
    CMD))
  

;;; The search process is conducted by the loop below.  It begins by clearing the 
;;; buffer and establishing a queue for the lines.  Incoming lines will be parsed
;;; off of the queue until it is empty at which point they will be taken from the
;;; stream itself.  As each line comes in, if it is a result line then it will be 
;;; passed on to be parsed into a cmdresult.  If not then it will be processed
;;; according to its type throwing an error if necessary.
(defun cmdreader-lookup-dde-result (Reader Class Result)
  "Lookup the necessary DDe-result."
  (let (Posts)    
    (do* ((Queue (cmdreader-ldr-init-queue Reader) (cdr Queue))
	  (Line (cmdreader-ldr-getline Queue Reader) 
		(cmdreader-ldr-getline Queue Reader)))
	
	((cmdreader-ldr-term-linep Line)
	 (cmdreader-append-to-buffer Queue Reader)	 
	 (cmdreader-ldr-term Result Line Class Reader))
      
      (case (logline-type Line)
		
	(DDE-POST ;; If this is a post then we add it to 
	 (cmdreader-add-to-buffer Line Reader)
	 (push Line Posts)
	 (cmdreader-ldr-post Reader Posts))
	
	(DDE-COMMAND (cmdreader-ldr-command Line Result Reader))
	(DDE (cmdreader-ldr-dde Line Reader))
	(CHECK-ENTRIES (cmdreader-ldr-ce Line Reader))))))


;;; At the beginning of the search we want to initialize the queue
;;; by setting it to the contents of the buffer and then clearing 
;;; the buffer itself.
(defun cmdreader-ldr-init-queue (Reader)
  "Initialize the queue itself."
  (let ((queue (cmdreader-buffer Reader)))
    (cmdreader-clear-buffer Reader)
    Queue))


;;; At each successive update we want to get the first line in the
;;; buffer or pull a line off of the stream if the buffer is empty.
(defun cmdreader-ldr-getline (Queue Reader)
  "Get the first line from the queue or read from the stream."
  (or (car Queue) (cmdreader-readline-direct Reader)))


;;; A line is a result-line if it is null or of the type dde-result
;;; or dde-failed.
(defun cmdreader-ldr-term-linep (Line)
  (or (null Line)
      (equal (logline-type Line) 'DDE-RESULT)
      (equal (logline-type Line) 'DDE-FAILED)))


;;; When you are given a resultline or a null line it will be passed on
;;; to the parser or an error will be thrown if cmdreader-erroronddeeof 
;;; is t.
;;;
;;; If there is a line then update the buffer and 
;;; parse the line.
;;;
;;; If there is no line and the erroronddeeof is t then 
;;; throw an error to the user.
(defun cmdreader-ldr-term (Result Line Class Reader)
  "Respond to an eof when searching for results."
  (if Line (cmdreader-parse-result Result Line Class Reader)
    (when (cmdreader-erroronddeeof Reader)
      (error "EOF encountered while searching for Result: ~a"
	     (LogReader-LineNum (cmdreader-logreader Reader))))))



;;; When a dde-post is encountered then the system will determine if it 
;;; already has a dde-post in the buffer.  If it does and mult-interim-posts 
;;; is nil then an error will be thrown otherwize it will be added to the 
;;; buffer and the system will move on.
(defun cmdreader-ldr-post (Reader Posts)
  "Handle the dde-post."
  (if (and (< 1 (length Posts)) (not (cmdreader-mult-interim-posts Reader)))
      (error "Multiple Interim Posts Found: ~a." (logline-num (car Posts)))))
	

;;; If we hit a DDE-command then it could be an assoc (which we will have 
;;; to check), a set-score command, or some other async command such as 
;;; wb-open browser.  This code will append the command strings (without
;;; parsing them) to the command.  If more than one assoc is encountered
;;; then the system will throw an error.  If more than one set-score is 
;;; found then it will be appended to the buffer for later processing.
(defun cmdreader-ldr-Command (Line Result Reader) 
  "Commands are async calls that are added to the listing for later."
  (let* ((Pos (position #\Space (logline-Value Line)))
	 (subs (if Pos (subseq (Logline-Value Line) 0 Pos) "")))
    (cond
     ;; If this is an assoc then test to see whether or not an 
     ;; assoc has already been found for this result.  If one 
     ;; has then throw an error.  If not then append the 
     ;; assoc to the result.
     ((string-equal subs "assoc")     ;; Deal with assocs.
      (if (cmdresult-Assoc Result)
	  (error "Multiple Assocs Found: ~a" (Logline-num Line))
	(setf (cmdresult-Assoc Result) (logline-value Line))))
	
     ;; If This is a set score determine whether or not one has
     ;; been seen already.  If not then go ahead and add the score
     ;; to the result.  If so then add the line to the buffer for 
     ;; later processing.
     ((string-equal subs "set-score")
      (cond 
       ((null (cmdreader-handle-set-scores Reader))
	(error "Set-score command found in non-set-score mode: ~a." Line))
       ((CmdResult-Score Result)
	(cmdreader-add-to-buffer Line Reader))
       (t (setf (cmdresult-Score Result) Line))))
     
     ;; If neither of those conditions are met then we need to 
     ;; add the results to the result-commands for later 
     ;; processing.
     (t (push Line (cmdresult-Commands Result))))))


;;; If a dde is encountered then we also need to respond appropriately
;;; either by signaling an error.
(defun cmdreader-ldr-DDE (Line Reader)
  "Deal with the interim DDE encountered."
  (declare (ignore Line))
  (error "Interim DDE term encountered: ~a" (LogReader-LineNum Reader)))


;;; If a checkentries is encountered then an error will be thrown because
;;; the system should not enter into a checkentries until a result is
;;; returned.
(defun cmdreader-ldr-ce (Line Reader)
  (declare (ignore Reader))
  "Deal with locating a checkentries during a resultsearch."
  (error "Checkentries encountered during result search ~a."
	 (logline-num Line)))



;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;; parsing reply
;;; The systax of dde-results are described in responses.txt in this directory
;;; the purpose of this reader is to split the incoming dde-reply value to 
;;; produce the relavent information for the user.  The issue of course is that
;;; in order to properly parse the result one must know what type of result it
;;; is.  This is a pain in the ass and necessitates my putting knowledge in here
;;; that I didn't want to add.  
;;;
;;; The code below will take the logline and generate an appropriate cmdresult
;;; struct for it.  If the result is a dde-failed then the relavent fields 
;;; will be set and the system will return.  
;;;
;;; if it is a dde-result then the system will produce the appropriate struct
;;; and then parse the logline-s value according to its type.  This will return 
;;; an appropriate dde-result superstruct encapsulated in the cmdresult struct.

(defun cmdreader-parse-result (Result Line Class Reader)
  "Parse the result according to its type."
  (declare (ignore Reader))
  (setf (cmdresult-class Result) (Logline-type Line))
  (setf (cmdresult-LineNum Result) (Logline-Num Line))
  (setf (cmdresult-Time Result) (Logline-Time Line))
  (setf (cmdresult-Value Result)
    (cmdreader-parse-result-val Line Class)))
  
;;  (make-cmdresult
;;   :Class (Logline-type Line)
;;   :LineNum (Logline-Num Line)
;;   :Time (Logline-time Line)
;;   :Value (cmdreader-parse-result-val Line Class)
;;   :Assoc Assoc
;;   :Commands Commands))


;;; Given a dde-failed logline, the resulting value will be the rest
;;; of the line (logline-value) and no more.  If not then the value 
;;; of the line will be a struct of the appropriate type.
;;;
;;; If an ignore is found then
(defun cmdreader-parse-result-val (Line Class)
  "Parse the line for value."
  (if (equal (Logline-type Line) 'DDE-Failed) (Logline-value Line)
    (let ((Str (cmdreader-strip-bars (logline-value Line))))
      (case Class
	(Status-return-val (cmdreader-make-status-return-val Str))
	(eqn-result (cmdreader-make-eqn-result Str))
	(hint-return-val (cmdreader-make-hint-return-val Str))
	(Ignore (Logline-Value Line))))))

   

;;; ----------------------------------------------------------------------------
;;; Hint-return-val
;;; Help requests such as get-proc-help and WWH return help results of the form:
;;;    e | <Hint-Spec> | !<Command>

;;; Given a logline that we know to be a hint-return-val the code will parse the
;;; line and return the appropriate fields in a tuple of the form (cmd Val)
;;; that will then be rolled into a hint-return-val struct.
;;;
;;; If the second value in the parse is a list then we have to parse it for a 
;;; value and menu.  If not then the value of the result will be the element
;;; and the menu will be nil.
(defun cmdreader-make-hint-return-val (Str)
  (let ((parse (cmdreader-parse-hint-return-val Str)))
    (cond 
     ;; Handle menu-free values.
     ((stringp (nth 1 Parse))
      (make-hint-return-val
       :Command (nth 0 Parse)
       :Value (nth 1 Parse)))
     
     ;; Handle show-hint values.
     ((listp (nth 1 Parse))
      (make-hint-return-val 
       :Command (nth 0 Parse)
       :Value (nth 0 (nth 1 Parse))
       :Menu (nth 1 (nth 1 Parse))))
     
     (t (error "Badly formed result value supplied.")))))

;;; This code parses it and returns the form:  Nil | (cmd-type cmd-value)
;;; <Hint-Specs> will be assigned the dummy cmd-type show-hint.
;;; This dummy assignment is made so that they will look the same as other
;;; results and make searching easier.  
;;; 
;;; Empty hint-return-val usually signify errors having occured but no error will 
;;; be thrown when one is found.
(defun cmdreader-parse-hint-return-val (Str)
  "Parse the hint-return-val."
  (when (not (string-equal Str ""))
    (if (equal (char Str 0) #\!)
	(cmdreader-parse-command (subseq Str 1))
      (list 'show-hint (cmdreader-parse-hintspec Str)))))



;;; -------------------------------------------------------------------------
;;; Eqn-result
;;; Equation results are returned from calls to the algebra system such as 
;;; solve-for and calculate.  Equation results are of the form:
;;;   (e | <EquationString>)(!<Hint-Spec>)

;;; pass the logline's value in for parsing.  The result will be a tuple of
;;; the form (<EqnString> <HintSpec>) Where Eqnstring is an equation and 
;;; hintspec is a hint specification to be displayed.  This code will set
;;; the appropriate values in a make-eqn-result struct (the command will 
;;; always be set to 'show-hint) and it will be returned.
(defun cmdreader-make-eqn-result (Str)
  "Make and eqnresult line."
  (let ((Parse (cmdreader-parse-eqn-result Str)))
    (make-eqn-result 
     :Eqn (nth 0 Parse) 
     :Command (if (cadr Parse) 'Show-hint)
     :Value (nth 0 (nth 1 Parse))
     :Menu (nth 1 (nth 1 Parse)))))


;;; This code looks for a ! in the result string.  If one exists then the 
;;; equation string will be everything before it (or nil if the position is
;;; 0.  Anything following it will be parsed as a hintstring.
(defun cmdreader-parse-eqn-result (ResultStr)
  "Parse the equation result str."
  (when (not (string-equal ResultStr ""))
    (let ((pos (position #\! ResultStr)))
      (if (null Pos) (list ResultStr)
	(list (if (not (= Pos 0)) (subseq ResultStr 0 Pos))
	      (cmdreader-parse-hintspec (subseq ResultStr (+ 1 Pos))))))))







;;; -----------------------------------------------------------------
;;; Status-Return-Val
;;; Status return vals supplied as a result of entry instructions such
;;; as lookup-vector and lookup-eqn-string.  The status return values 
;;; are of the following form:
;;;   <Status>(<errors>)(!<Command>)
;;; Where the errors are ; separated.

;;; Given a tuple of the form (<Status> <Errors> <Command>) by the parser
;;; this code will produce a status-return-val struct with the values of 
;;; the appropriate fields set.
(defun cmdreader-make-status-return-val (Str)
  (let ((Parse (cmdreader-parse-status-return-val Str)))
    (make-status-return-val 
     :Coloring (car Parse)
     :Errors (nth 1 Parse)
     :Command (car (nth 2 Parse))
     :Value (nth 0 (nth 1 (nth 2 Parse)))
     :Menu (nth 1 (nth 1 (nth 2 Parse))))))

;;; Given a string corresponding to a status return-val


;;; This code will parse the status return value into a tuple of the
;;; form:  (<Status> <Errors> <Command>)  where errors and command can
;;; be nil.
;;;
;;; Note that if a hint exists then a ';' may be a part of it so the code
;;; below will only parse for errors if a semicolon preceeds the '!' command
;;; separator.  
;;;
;;; Note that the semicolons are part of the error structure so they will
;;; be passed into the error parser.  But, the exclamation point is not so 
;;; it will be used as a separator.
;;;
;;; Note that many of the 1999 logs contain status return vals wthat were strictly 
;;;  hints of the form "if you want ..." they were not preceeded by any status
;;;  val of any type.  Therefore if the supplied hint is longer than 3 characters
;;;  and contains no ! or ; then it will be parsed as a hintspec.
(defun cmdreader-parse-status-return-val (ValStr)
  "Parse the supplied status return val."
  (when (not (string-equal ValStr ""))
    (let* ((len (length ValStr))
	   (PosS (or (position #\; ValStr) Len))
	   (PosE (or (position #\! ValStr) Len)))
            
      ;; If the 1st semicolon is after the ! then it is embedded in a hint and
      ;; we don't want to use it.  Therefore set PosS to be PosE.
      (if (< PosE PosS) (setq PosS PosE))
      
      (if (cmdreader-parse-srv-as-hintspecp ValStr Len PosS PosE) 
	  (cmdreader-parse-srv-as-hintspec ValStr)
	(cmdreader-parse-srv-standard ValStr Len PosS PosE)))))


;;; Given a status return val string Coupled with the length and
;;; Position measurements we need to determine whether or not it
;;; should be parsed as a hintspec.
(defun cmdreader-parse-srv-as-hintspecp (Str Len PosS PosE)
  (declare (ignore Str))
  (and (= Len PosS) (= Len PosE) (< 3 Len)))

;;; When parsing a status return val as a hintspec we need to 
;;; fill in a dummy list of values with no  
(defun cmdreader-parse-srv-as-hintspec (ValStr)
  "Pare the status return val as a hintspec."
  `(Nil Nil ('show-hint ,(cmdreader-parse-hintspec ValStr))))
  

;;; parse the Value as the first element up to the Semicolon.
;;; parse the Errors as the value up to the Exp and the command
;;; as everything after that.  Keeping in mind that the subseqs
;;; for errors and command may be "".
(defun cmdreader-parse-srv-standard (ValStr Len PosS PosE)
  "Parse the status return value in the standard way."
  (list (cmdreader-parse-status-val (subseq ValStr 0 PosS))
	(cmdreader-parse-errors (subseq ValStr PosS PosE))
	(cmdreader-parse-command
	 (if (= PosE Len) "" (subseq ValStr (+ 1 PosE))))))
	      

;;; -----------------------------------------------------------------
;;; Status
;;; The status is either T or NIL and is not optional if it is present.
;;; T indicates 'green' NIL indicates 'RED' and no value indicates
;;; no change.  This code will parse the incoming values and return
;;; the appropriate atom.
;;;
;;; NOTE:: Some of the fall2000 logs have a return value of |1| following
;;;  A close-problem.  Therefore for backwards compatability I have
;;;  taken "1" as a status-val to be green.
(defun cmdreader-parse-status-val (StatusStr)
  "The status return value."
  (cond ((String-equal StatusStr "") Nil)
	((String-equal StatusStr "T") 'Green)
	((String-equal StatusStr "1") 'Green)
	((String-equal StatusStr "NIL") 'Red)
	(t (error "Unrecognized status string \'~a\' supplied." StatusStr))))


;;; -----------------------------------------------------------------
;;; Errors
;;; Error fields are flexible fields used to specify a list of
;;; error atoms.  These were used in Andes1 to signify the fields
;;; in a dialgo box that were wrong.  They can be used for any return
;;; value.  The fields are of the form:  
;;;     ;<Error0>;...;<Errorn>
;;;
;;; The parser here will return a list of the error stings supplied
;;; after stripping off the semicolons.
(defun cmdreader-parse-errors (ErrorStr &optional (Errlst Nil))
  "Parse the error string."
  (if (string-equal ErrorStr "") (reverse ErrLst)
    (let ((pos (position #\; ErrorStr :start 1)))
      (if Pos (cmdreader-parse-errors 
	       (subseq ErrorStr Pos)
	       (cons (subseq ErrorStr 1 Pos) Errlst))
	(if (<= (length ErrorStr) 1) (reverse Errlst)
	  (reverse (cons (subseq ErrorStr 1) Errlst)))))))
	   


;;; ----------------------------------------------------------------------
;;; Command
;;; Commands are result calls to the system of the form:
;;;   <CmdName> <Value> 
;;; Commands signal some action to be performed by the workbench
;;; such as show-hint show-lesson training-card etc.  The commands
;;; appear in dde-commands, asynchronous calls sent from the help system
;;; to the workbench, and optionally in help request-results or status
;;; return values.  This code parses the command and its optional values 
;;; into the appropriate resulting form. 
(defun cmdreader-parse-command (CommandStr)
  (when (not (string-equal CommandStr ""))
    (let ((pos (position #\Space CommandStr)))
      (if (null Pos) (list (read-from-String CommandStr))
	(let ((cmd (read-from-string (subseq CommandStr 0 Pos))))
	  (list cmd (cmdreader-parse-command-val 
		     cmd (subseq CommandStr (+ 1 Pos)))))))))


;;; Parse the value depending upon the supplied command type at present this 
;;; only handles show-hint.  The other commands will simply be returned as-is
(defun cmdreader-parse-command-val (cmd Rest)
  (if (equal cmd 'show-hint) (cmdreader-parse-hintspec Rest)
    Rest))
  



;;; -----------------------------------------------------------------
;;; Hintspec
;;; Hintspecs are strings of the form: <HintString>(~<Menu>)
;;; the last ~ in the sting signals the start of the menu.  
;;; this parser will return a tuple of the form:
;;;  (<HintString> <menu>)
(defun cmdreader-parse-hintspec (Hint)
  "Parse the supplied hintspec."
  (if (string-equal Hint "") Nil
    (let ((pos (position #\~ Hint :from-end t)))
      (if (null Pos) (list Hint)
	(list (subseq Hint 0 Pos)
	      (cmdreader-parse-menu (subseq Hint (+ 1 Pos))))))))


;;; ------------------------------------------------------------------------
;;; Menu
;;; A menu is a single character or sequence of the form:
;;;
;;; MENU ::= <empty> | w | e | h | P | Q | E | '|<String0>|...|<Stringn>|'
;;;
;;; this code will parse the menustring that has been supplied and return
;;; the value where:
;;;
;;;  <empty> = NIL
;;;  w = 'why'
;;;  e = 'explain-more'
;;;  h = 'how'
;;;  P = 'Principle'
;;;  Q = 'Quantity'
;;;  E = 'Equation'
;;;  |<String0>| ... | <StringN>| is a tuple of the form (<String0> ... <Stringn>)
;;;
;;; note that the ~ is assumed to have been stripped off already.

;;; atomic menu constants.
(defconstant **explain-more** 'explain-more)
(defconstant **why** 'why)
(defconstant **how** 'how)
(defconstant **Principle** 'Principle)
(defconstant **Quantity** 'Quantity)
(defconstant **Equation** 'Equation)
(defconstant **Free-Text** 'free-text) ;; the kcd menus.


(defun cmdreader-parse-menu (Menu)
  "Parse the incoming menu string."
  (cond ((string-equal Menu "") Nil) 
	((equal (char Menu 0) #\|) (cmdreader-parse-menulst Menu))
	(t (cmdreader-parse-atomicmenu Menu))))
           

;;; Parsing the menutuple is done in recursive fasion.  The first items is 
;;; popped off of the list and added to the tuple until the remaining string
;;; is "|"  at which point the (now reversed) tuple of strings is returned
(defun cmdreader-parse-menulst (Menu &optional (Tuple Nil))
  (if (<= (length Menu) 2) (reverse Tuple)
    (let ((pos (position #\| Menu :Start 1)))
      (cmdreader-parse-menulst 
       (subseq Menu Pos)
       (cons (copy-seq (subseq Menu 1 Pos)) Tuple)))))
  

;;; Given an atomic (single-item) list thenidentify what menu it corresponds 
;;; to and return the name value.
(defun cmdreader-parse-atomicmenu (Menu)
  "Parse the atomic menu supplied."
  (case (char Menu 0)
    (#\| (error "Badly formed menu supplied:: \'~a\'" Menu))
    (#\e **Explain-more**)
    (#\w **why**)
    (#\h **how**)
    (#\P **Principle**)
    (#\Q **Quantity**)
    (#\E **Equation**)
    (#\? **Free-Text**)
    (t (error "Unrecognized menu upplied:: ~a" Menu))))



;;; Given a string enclosed in bars '|' strip off the surrounding bars
;;; to return the string inside or throw an error if no bars are found.
(defun cmdreader-strip-bars (Str)
  (if (and (equal (char Str 0) #\|)
	   (equal (char Str (- (length Str) 1)) #\|))
      (subseq Str 1 (- (length Str) 1))
    (error "Invalid string '~a' passed to strip-bars." str)))   




;;; ----------------------------------------------------------------------------
;;; Given a LogLine struct we will need to generate the CMD from it and return.
;;; The line itself should be a logline struct.
;;;
;;; note that this will have to lookup class.
(defun Cmdreader-generate-CMD (Line Reader &key (Class Nil) (Call Nil) (Result Nil))
  (declare (ignore Reader))
  (let ((LocalCall (or Call (Cmdreader-hash-parse-lineval Line))))
    (make-cmd :Class (or Class (lookup-command->class (car LocalCall)))
	      :Type (logline-type Line)
	      :LineNum (logLine-Num Line)
	      :Time (parse-htime (logline-time Line))
	      :Call LocalCall
	      :Result Result)))
			




;;; ---------------------------------------------------------------------------
;;; Parsing the command on a line is a matter of of read-from-string with some
;;; error prevention code to eliminate instances suchg as \\\" which Allegro
;;; fails to encode correctly as \" and instead causes an eof error.  This
;;; code is based upon the code within Andes2-Main.cl
;;;
;;; This will be called on all CMDs, Commands and other calls so that the system
;;; has processable sequences to make use of.
(defun Cmdreader-parse-func-string (Command-String)
  "Parse the specified command prior to calling it."
  (let* ((Cmd-obj nil) (Command nil) (Arguments nil)
	 (check (read-from-string (subseq command-string 1 (position #\Space command-string)))))
    (cond
     ((equal check 'lookup-eqn-string)
      (setf command 'lookup-eqn-string)
      (setf arguments
	(list
	 (fix-quotes (trim-eqn (subseq command-string
		 (position #\Space command-string)
		 (position #\Space command-string
			   :from-end (- (length command-string) 1)))))
	 (read-from-string (subseq command-string
				   (position #\Space command-string
					     :from-end (- (length command-string) 1)))))))
      (t
       (setf cmd-obj (read-from-string (quote-special-characters command-string)))
       (setf command (first cmd-obj))
       (setf arguments (rest cmd-obj))))
    ;;(format t "Command: ~w ~w :Done~%" Command Arguments)
    (cons Command Arguments)))


;;; In order to avoid the crashes that I have been encountering this 
;;; function strips the #. out of an incoming string before parsing it
;;; since wb-quants are only used in NSH responses and only in the
;;; form (workbench command type) so we can parse these differently
;;; and this frees up the need to depend upon the help system.
(defun strip-hashperiod (String)
  (let ((pos (position #\# String)))
    (cond ((null Pos) String)
	  ((= Pos (- (length String) 1)) String)
	  ((not (equal #\. (char String (+ 1 Pos))))
	   (concatenate 'string (subseq String 0 (+ 1 Pos)) 
			(strip-hashperiod (subseq String (+ 1 Pos)))))
	  (t (concatenate 'string (subseq String 0 Pos) 
			  (strip-hashperiod (subseq String (+ 2 Pos)))))))) 

;; (defun testhashperiod (Filename)
;;   (with-open-file (File Filename :direction :input)
;;   (do ((line (read-line File Nil Nil) (read-line File Nil Nil)))
;;   ((null Line))
;;   (format t "~a~%   ~a~%" Line (strip-hashperiod Line)))))




(defun quote-special-characters (string)
  (loop for i from 0 to (1- (length string))
      with special = '(#\, #\\)
      appending
	(let ((c (char string i)))
	  (if (member c special) (list #\\ c)
	    (list c)))
      into newstring
      finally (return (concatenate 'string newstring))))
  
  
(defparameter ***bad-character-codes***
    '(33 34 35 37 38 39 44 58 59 60 62 63 64 91 92 93 96))
(defun fix-quotes (string)
  (let ((len (length string)))
    (if (> len 0)
	(do ((i 0 (+ i 1)))
	    ((>= i len) string)
	  (cond
	   ((< (char-code (char string i)) 32) (setf (char string i) #\Space))
	   ((> (char-code (char string i)) 122) (setf (char string i) #\Space))
	   ((= (char-code (char string i)) 91) (setf (char string i) #\())
	   ((= (char-code (char string i)) 93) (setf (char string i) #\)))
	   (t (if (member (char-code (char string i)) ***bad-character-codes***)
		  (setf (char string i) #\Space)))))
      string)))


(defun trim-eqn (eq)
  (if (> (length eq) 0)
      (let ((p (position #\; eq)))
	(if p
	    (string-trim " " (subseq eq 0 p))
	  (string-trim " " eq)))
    eq))


;;; Given a function string that might contain lisp reader macros
;;; such as #. this code stips off the macros before filtering.
(defun cmdreader-hash-parse-lineval (Line) 
  (cmdreader-parse-func-string (strip-hashperiod (LogLine-Value Line))))

	    
;;; -----------------------------------------------------------------------------
;;; Checkentries commands can be dealt with in a number of ways.  If 
;;; ignore-check-entries is t then when a "check-entries 1" line is found then 
;;; the code will skip over all lines until it reaches a "check-entries 0".  When
;;; logparsing is being done this will allow the system to skip these lines in the
;;; most efficient manner.  
;;;
;;; If ignore-check-entries is nil then the system will generate a cmd for the
;;; check-entries line and return it.  
;;;
;;; the check-entries handling sets a global state variable for the command reader 
;;; if in-check-entries is t then the cmds being read are appearing between
;;; checkentries 1 and checkentries 0.  If not then they are being made by the
;;; student.
;;;
;;; One of the complications of check-entries cases is that in newer logs we have
;;; now added check-entries ddes to the workbench.  These will show up immediately
;;; after the "check-entries 1" lines and before the "check-entries 0" line.  For 
;;; now no action will be taken on these.  
(defun cmdreader-process-CheckEntries (Line Reader)
  (if (cmdReader-Ignore-Check-Entries Reader) 
      (cmdreader-ignore-CE Line Reader)
    (cmdreader-handle-checkentries Line Reader)))

;;; process the checkentries line setting or unsetting the global in-check-entries
;;; variable in the process.  If an error occurs (checkentries 1 when state is 1
;;; then throw an error.  
(defun cmdreader-handle-checkentries (Line Reader)
  (let ((Value (read-from-string (logline-value Line))))
    
    ;; Check for errors in the checkentries by ensuring that if we encounter
    ;; a 1 or a zero that the global stats is set properly.
    (if (or (and (cmdreader-in-check-entries Reader) (equal Value 1))
	    (and (null (cmdreader-in-check-entries Reader)) (equal Value 0)))
	(error "Bad-checkentries settings encountered."))

    ;; Having eliminated the possiblities of an error now set the state var.
    (setf (cmdreader-in-check-entries Reader) (if (equal Value 1) t Nil))
    
    ;; Now produce and return the cmd.
    (make-cmd :Type (logline-type Line)
	      :LineNum (logline-Num Line)
	      :Time (parse-htime (Logline-time Line))
	      :Call (read-from-string (logline-Value Line)))))


;;; If the cmdreader is ignoring checkentries then this func will test the
;;; current line.  If it is "check-entries 1" then it will iterate until the
;;; "check-entries 0" is found.  If an eof is encountered during the search
;;; then an error will be raised.  If a Check-Entries 1 is encountered while
;;; the search is proceeding then an error will also be raised.  This should
;;; not occur and indicates a file error if it does.
;;;
;;; Note that the buffer should be empty at this when a checkentries is 
;;; encountered.  If it is not then an error will be raised.
;;;
;;; Once the system has read through to the next checkentries then it will
;;; recursively fetch the next cmd from the list. 
(defun Cmdreader-ignore-ce (Line Reader)
  "cycle through the check-entries calls."
  (cond ((not (equal (char (LogLine-Value Line) 0) #\1))
	 (error "Bad Logfile:  bad Check-Entries Line passed to Ignore-CE"))
	((cmdreader-Buffer Reader)
	 (error "Non-Null Buffer of check-entries 1.  Corrupted Logfile."))
	(t (cmdreader-ignore-ce-loop Reader)
	   (read-next-cmd Reader))))


;;; Iteratively search through the file for a check-entries 0.
;;; throw an error if an eof (null logline) is found or if a 
;;; check-entries line is found that does not match 
;;; "check-entries 0"
(defun cmdreader-ignore-ce-loop (Reader)
  "Loop over the file until check-entries 0 is found."
  (do ((Line (cmdreader-ReadLine-direct Reader) 
	     (cmdReader-ReadLine-direct Reader)))
      ((Equal (LogLine-Type Line) 'check-entries)
       (if (not (equal (char (logline-Value Line) 0) #\0))
	   (error "Badly formed Check-entries pair found.")))

    (if (null line) (error "EOF encountered when between check-entries."))))




;;;; ===================================================================
;;;; Util functions

;;;; Iterate over an open log until the end is reached.  Return the 
;;;; resulting set of cmds.
(defun iterateCMDreader (Reader)
  (let (Result)
    (handler-case 
	(progn (do ((CMD (Read-next-CMD Reader) 
			 (Read-next-CMD Reader)))
		   ((null CMD))
		 (push CMD Result)))
      (error (E) (format t "**ERROR**:: ~a~%" E)))
    (reverse Result)))

;; no error catching
(defun iterateCMDreader2 (Reader)
  (let (Result)
    (progn (do ((CMD (Read-next-CMD Reader) 
			 (Read-next-CMD Reader)))
		   ((null CMD))
		 (push CMD Result)))))
    



;;;; Given a cmdreader and a function iterate over the reader calling the 
;;;; func on each cmd as it comes off the reader.
(defun func-iterateCMDreader (Reader Func)
  (let (Result)
    (handler-case 
	(progn (do ((CMD (Read-next-CMD Reader) 
			 (Read-next-CMD Reader)))
		   ((null CMD))
		 (funcall Func Cmd)))
      (error (E) (format t "**ERROR**:: ~a~%" E)))
    (reverse Result)))









;;;; ==================================================================
;;;; Testing functions 
;;;; Test CMDlog Iterate over a specified logfile printing all of the 
;;;; entries but do not close it.  
(defun testCMDLog (&key (Path "./") (ignore-check-entries Nil)
			(handle-set-scores Nil))
  "Iterate over a single file printout out all of the entries within it."
  (let (R (Out (open (concatenate 'string (namestring Path) ".cmd")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)))
    (handler-case 
	(progn (setq R (open-cmdreader 
			Path :ignore-check-entries ignore-check-entries
			:handle-set-scores Handle-set-scores))
	       (format Out "~a~2%" R)
	       (do ((CMD (Read-next-CMD R) (Read-next-CMD R)))
		   ((null CMD) (format t "### EOF Found ######################~%"))
		 (format Out "~a~%" CMD))
	       (close-cmdreader R))
      (error (E) 
	(format Out "**ERROR**:: ~a~%" E)
	(close-cmdreader R)))
    (close Out)))



;;;; Iterate over the logs supplied pulling out the relavent commands
;;;; for testing purposes.
(defun testCMDLogs (&key (Path "./") (ignore-check-entries Nil) 
			      (handle-set-scores Nil))
  "Test all the logs in path iterate and print lines."
  (let ((files (directory (concatenate 'string Path "*.log"))))
    (dolist (F files)
      (format t "~%##### Starting File: ~a #########################~%" F)
      (let ((R ()) Out) 
	(setq Out (open (concatenate 'string (namestring F) ".cmd")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create))
	(handler-case 
	    (progn (setq R (open-cmdreader F :ignore-check-entries ignore-check-entries
					   :handle-set-scores Handle-set-scores))
		   (format Out "~a~2%" R)
		   (do ((CMD (Read-next-CMD R) (Read-next-CMD R)))
		       ((null CMD) (format t "### EOF Found ######################~%"))
		     (format Out "~a~%" CMD))
		   (close-cmdreader R))
	  (error (E) 
	    (format Out "**ERROR**:: ~a~%" E)
	    (if R (close-cmdreader R))))
	(close Out))
      ;;(excl:gc)
      )))



(defun tstcmd ()
  (mapcar #'testCMDLogs 
	  '("c:/Andes2/Log/local/*/" 
	    "c:/Andes2/Log/Pitt/Fall1999/*/"
	    "C:/Andes2/Log/Pitt/Fall2000/*/"
	    "C:/Andes2/Log/Pitt/Fall2001/*/"
	    "C:/Andes2/Log/USNA/Fall1999/*/"
	    "C:/Andes2/Log/USNA/Fall2000/*/"
	    "C:/Andes2/Log/USNA/Fall2001/*/"
	    "C:/Andes2/Log/USNA/Fall2002/*/"
	    )))


  
;;;; Stack test cmd log
;;;; Given a cmd log iterate over it adding commands to the stack and
;;;; calling the specified test predicate.  print the results.
(defun func-stack-iterateCMDreader (Reader Func)
  (let (Result)
    (handler-case 
	(progn (do* ((CMD (Read-next-CMD Reader) 
			 (Read-next-CMD Reader))
		    (Stack (list CMD) (cons CMD Stack)))
		   ((null CMD))
		 (funcall Func Stack)))
      (error (E) (format t "**ERROR**:: ~a~%" E)))
    (reverse Result)))




;;;; ===================================================================
;;;; Tracing the cmdreader.

(defun trace-cmdReader ()
  (trace-cmdreader-parse-reply)
  (trace-cmdreader-dde-post)
  
  (trace Make-cmdReader
	 
	 Open-cmdReader
	 close-cmdreader

	 Read-Next-CMD
	 Cmdreader-ReadLine
	 cmdreader-readline-direct
	 
	 Cmdreader-process-DDE
	 Cmdreader-lookup-dde-result
	 cmdreader-ldr-init-queue
	 cmdreader-ldr-getline
	 cmdreader-ldr-term-linep
	 cmdreader-ldr-term
	 cmdreader-ldr-post
	 cmdreader-ldr-command
	 cmdreader-ldr-dde
	 cmdreader-ldr-ce
	 
	 #|
	 cmdreader-result-search-queue
	 cmdreader-rsq-result
	 cmdreader-rsq-post
	 cmdreader-rsq-command
	 
	 cmdreader-result-search-stream
	 cmdreader-result-null
	 cmdreader-rss-post
	 cmdreader-rss-command
	 
	 cmdreader-rs-dde
	 cmdreader-rs-ce
	 |#
	 	 
	 cmdreader-generate-cmd
	 cmdreader-parse-func-string

	 cmdreader-process-checkentries
	 
	 ;;cmdreader-parse-reply
	 
	 parse-htime
	 ))
	 
	 
(defun trace-cmdreader-dde-post ()
  (trace cmdreader-process-dde-post
	 cmdreader-process-eqn-post
	 cmdreader-empty-stringp
	 cmdreader-generate-delete-eqn
	 cmdreader-process-async-les
	 cmdreader-lookup-eq-result
	 cmdreader-ler-term-test
	 cmdreader-ler-term
	 cmdreader-ler-parse-eq-result))



(defun trace-cmdreader-parse-reply ()
  (trace cmdreader-parse-result
	 cmdreader-parse-result-val
   
	 cmdreader-make-hint-return-val
	 cmdreader-parse-hint-return-val
	 
	 cmdreader-make-eqn-result
	 cmdreader-parse-eqn-result
	 
	 cmdreader-make-status-return-val
	 cmdreader-parse-status-return-val
	 
	 cmdreader-parse-status-val
	 cmdreader-parse-errors
	 cmdreader-parse-command
	 cmdreader-parse-command-val
	 cmdreader-parse-hintspec
	 cmdreader-parse-menu
	 cmdreader-parse-menulst
	 cmdreader-parse-atomicmenu
	 
	 cmdreader-strip-bars
	 ))



