;; Author(s):
;;   Mike Ringenberg (mr) <
;;   Linwood H. Taylor (lht) <lht@lzri.com>
;;   Anders Weinstein (a?w) <andersw+@pitt.edu>
;; Modified:
;;   unknown - created
;;   7 February 2001 - (lht) - editing for documentation and new help dialog
;;   ?????????? 2001 - (a?w) - added fucntionality fro debugging and added
;;                             support for symbol table processing
;;   23 April 2001 - (lht) - renamed from tcp-wb.lsp to Andes-Main.cl
;;                           adding some polishing to support final code
;;    5 June 2001 - (lht) - editied to load/initialize new parseing/etc.
;;; Modifications by Brett van de Sande, 2005-2009
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

(in-package :cl-user)

(defun |get-problem| (&key session time problem user) 
  "initial problem statement" 
  '(((:action . "new-object") (:id . 0) (:type . "text") (:mode . "locked")
     (:x . 3) (:y . 5) (:text-width . 80) (:text . "A spherical ball with a mass of 2.00 kg rests in the notch ..."))
    ((:action . "new-object") (:id . 1) (:type . "graphics") (:mode . "locked")
     (:x . 53) (:y . 15) (:dx . 150) (:dy . 180)
     (:href . "/images/s2e.gif"))))

(defun |solution-step| (&key session time id action type mode x y text-width
			text dx dy radius symbol x-label y-label angle) 
  "problem-solving step" 
  `(((:action . "set-score") (:score . 57))
    ((:action . "modify-object") (:id . ,id) (:mode . "right"))))

(defun |seek-help| (&key session time action href value text) 
  "ask for help, or do a step in a help dialog" 
  '(((:action . "show-hint") 
     (:text . "Now that you have stated all of the given information, you should start on the major principles. What quantity is the problem seeking?"))
    ((:action . "focus-hint-text-box"))))

(defun |close-problem| (&key session time) 
  "shut problem down" 
  (format webserver:*stdout* 
	  "in closeproblem  session ~S time ~S~%" session time)
  '(((:action . "show-hint") 
     (:text . "Finished working on problem s2e."))
    ((:action . "log") 
     (:score . (("NSH_BO_Call_Count" . (-0.05 0)) 
      ("WWH_BO_Call_Count" . (-0.05 0))
      ("Correct_Entries_V_Entries" . (0.05 17 19))
      ("Correct_Answer_Entries_V_Answer_Entries" . (0.05 1 2)))))))

;; Define *student-entries* and *cp* using defvar in thread, local to thread.
;; Define hash table *sessions* (or tables) with the session id as the key.
;; each session contains *student-entries* and *cp* for that session.

;; which equality is most appropriate for sxhash keys?
(defparameter *sessions* (make-hash-table) 
  "A list of active sessions")

(defparameter *least-idle* nil "Session having most recent interaction.")

(defparameter *most-idle* nil "Session that has been idle for the longest time.")

(defstruct session StudentEntries next previous student problem)

;; does it matter if id is always a number when doing hash?
(defun new-session (student problem)
  "initializes session and pushes onto recent activity queue, if inactive, and returns session id"
  (let ((id (sxhash (cons student problem))))
    (unless (gethash id *sessions*)
      (setf (gethash id *sessions*) 
	    (make-session :student student :problem problem 
			  ;; :next is nil
			  :previous *least-idle*))
      (if *least-idle* 
	  ;; need thread lock for these:
	  (setf (session-next (gethash *least-idle* *sessions*)) id)
	  (setf *most-idle* id))  ;only for first session
      (setf *least-idle* id))
    id))


(defun get-session (id)
  "returns a session for a given session id; update queue of recent activity"
  ;; must be fast, since this is called a lot
  (let ((session (gethash id *sessions*)))
   ;; might instead try to retrieve session from database.
   (unless session  (error "trying to access inactive session ~A" id))
   ;; need thread lock for these:
   (remove-session-from-recent-activity-queue session)
   (setf (session-next (gethash *least-idle* *sessions*)) id)
   (setf (session-next session) nil)
   ;; use same equality test as the hash table.
   (when (equalp *most-idle* id)
     (setf *most-idle* (session-next session)))
   (setf (session-previous session) *least-idle*)
   (setf (session-next (gethash *least-idle* *sessions*)) id)
   (setf *least-idle* id)
   session))

(defun remove-session-from-recent-activity-queue (session)
  (let ((previous-session (when (session-previous session)
			    (gethash (session-previous session) *sessions*)))
	(next-session (when (session-next session)
			(gethash (session-next session) *sessions*))))
    ;; need thread lock for these:
    (when previous-session
      (setf (session-next previous-session)
	    (when next-session (session-previous next-session))))
    (when next-session
      (setf (session-previous next-session)
	    (when previous-session (session-next previous-session))))))


(defun close-session (id)
  "close a session if it is open; update queue of recent activity"
  ;; should also save session to database?
  (let (previous next (session (gethash id *sessions*)))
    ;; might instead try to retrieve session from database.
    (unless session (error "trying to access inactive session ~A" id))
    (remove-session-from-recent-activity-queue session)
    ;; use same equality test as the hash table.
    (when (equalp *most-idle* id)
      (setf *most-idle* (session-next session)))
    (when (equalp *least-idle* id)
      (setf *least-idle* (session-previous session)))))

(defun list-active-sessions (&optional (id *least-idle*))
  "Return a list of sessions in order of recent activity."
  (when *most-idle* 
    (cons id (unless (equal id *most-idle*) 
	       (list-active-sessions 
		(session-previous (gethash id *sessions*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Global Variables 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; *andes-path* -- pathname of directory in which Andes files are installed
;;                 as logical pathname object
;; In the runtime image this will be set on startup to the working directory 
;; of the Lisp process -- see top of andes-start.  The workbench sets process 
;; working directory when it launches the help system.

(defun andes-path (relative-path)
"merge relative path with *andes-dir* returning new pathname"
    (merge-pathnames relative-path *andes-path*))


(defvar *andes-stream* nil
  "The stream that represents the character socket that serves help requests.")

(defvar *debug-help* t
  "The stream showing help system runtime activities.")



(defun andes-run ()
  "Executes delayed tasks and listens for new events on the stream to process."
  (unless *andes-stop*
     (format *debug-help* "~&Running server event processing loop~%"))
  (unwind-protect
      ;; outer loop just repeats forever until server termination flag gets 
      ;; set or connection no longer exists.
      (loop until (or *andes-stop* 
                      (not (connection-running)))
	  do
	    ;; 1: loop to drain all pending work from our queue by interleaving
	    ;; execution of delayed tasks from task queue with polling and 
	    ;; dispatching of ready input events from the command stream.
	    (loop until (null *task-list*)
		do (eval (pop *task-list*)) ;tasks can have side-effects
		   ;; handle input events while draining the task queue.
		   (process-stream-event :blocking nil))
	    ;; 2: no work to do right now
	    ;; Do blocking wait on next command so lisp process does not domi- 
	    ;; nate the system's resources busy-waiting when no work to do.
	    (process-stream-event :blocking t))

    ;; protected post-loop cleanup: Note could have unwound out of loop to here
    ;; after error.  In runtime image, always just terminate
    #+allegro-cl-runtime (andes-terminate)
    ;; Otherwise make sure session was normally ended before we terminate 
    ;; server instance.  In interactive Lisp can restart loop after throwing 
    ;; out of error to continue.
    #-allegro-cl-runtime 
    (if (not *andes-stop*)
       (format *debug-help* 
              "~&Exited server event loop! Call \"andes-run\" to resume event processing~%")
     (andes-terminate)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-stream-event - Listens for message on *andes-stream* and then exe-
;;    cutes it as a command.
;; argument(s):
;;    &key :blocking : if true, function will wait for an event. default nil.
;; returns: Garbage
;; note(s): If there is an error on the stream, it tries to handle it. 
;;          It starts the process of executing the command on the stream.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun process-stream-event (&key blocking)
  "Listens for a message on *andes-stream* and then executes it as a command."
  (let ((fbd-message nil))
    ;; determines if it should wait for a message or just return unless
    ;; there is a message.
    (when (or blocking (listen *andes-stream*))
      (setq fbd-message 
	(handler-case (read-line *andes-stream*) ;; get command string
	  ; we normally terminate on the "exit-andes" API call before we 
	  ; read EOF on the socket, so have never seen this happen:
	  (end-of-file (condition) 
	    (error-message
	     (format
	      nil
	      "stream termination on ~S.~%Exiting Andes help gracefully.~%"
	      (stream-error-stream condition)))
	    (andes-stop)	; sets stop flag to andes-run loop
	    NIL)		; no message to process in this case
          ; Can happen for connection reset; remote crash, net failure:
	  (error (condition) 
	    (error-message 
	      (format NIL "Unexpected error: ~A~%Andes help quitting."
	              condition))
	     (andes-stop)	; sets stop flag to andes-run loop
	     NIL)))		; no message to process in this case

      ; if got message OK, then execute it 
      (when fbd-message
        (execute-stream-event fbd-message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: execute-stream-event
;; Parameters: str: a string containing the message from the workbench
;; Purpose: This function determines what to do with a command message string 
;;          based on the tag at the head of the string.  It is also
;;          responsible for removing the tag.
;; returns: Garbage
;; Side Effects: If the string is a notify command, it attempts to execute it.
;;               If it is a execute command, it will execute it and return the
;;               results to *andes-stream*.  Gives an error message on unknown
;;               command types.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun execute-stream-event (str)
  "Determines from the 1st char of string how to handle the message"
  (format *debug-help* "Stream-event ~A~%" str)
  (cond ((string= str &notify& :end1 1)	;Workbench does not expect a reply
	 (dispatch-stream-event (remove &notify& str :count 1)))
	((string= str &exec& :end1 1)	;Workbench expects reply
	 (dispatch-and-return-stream-event (remove &exec& str :count 1)))
	(t (error-message
	    (format nil "unrecognized command: ~A" str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: dispatch-stream-event
;; Parameters: command-string: a string containing a lisp function call
;;             dde:  t/nil indicating whether this is a dde (needs reply
;;                   or a dde-post (no reply needed).
;; Purpose: To safely execute a command from the workbench, return the result
;;          of the function call, and to do some book-keeping.
;; returns: Results of the function call or :error if failed.
;; Side Effects: Signals errors when execution fails, executes command in the
;;               command-string which might have side effects, records the
;;               time the command was executed, and updates the help system's
;;               records.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dispatch-stream-event (command-string &key (dde Nil))
  "Reads the string and tries to execute it, returns the result of the execution while also performing some bookkeeping."
  ;; AW: we used to preprocess command string to ensure it could be passed safely through
  ;; Lisp reader, which we use to parse it into a list of Lisp objects. This was particularly
  ;; an issue for equation box contents, which could contain characters like quotes that confuse read.
  ;; Now we just it to the workbench to ensure all arguments in command strings are properly
  ;; escaped for Lisp.  Still, wrap the work in safe-apply to recover in case of error in Lisp read.
  (safe-apply 'do-dispatch-stream-event (list command-string dde)))

(defun do-dispatch-stream-event (command-string dde)
  (let ((cmd-obj (read-from-string command-string))) 
    (format *debug-help* "~&~%Executing ~A~%(Apply ~W ~W)~%" command-string (first cmd-obj) (rest cmd-obj))
    ;; Pass parsed call to to the main dispatch wrapper in interface.cl
    (execute-andes-command (first cmd-obj) (rest cmd-obj) dde)))


     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For runtime dist, trap all Lisp errors and return special :error value
;; instead. When debugging, just use apply to debug on errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+allegro-cl-runtime 
(defvar *ignore-errors* T)	; T => trap LISP errors in command execution
#-allegro-cl-runtime 
(defvar *ignore-errors* NIL)    

;; Apply function to args, trapping Lisp errors and returning :error in this
;; case as controlled by *ignore-errors* flag
(defun safe-apply (fn &optional (args nil)) 
  (if (not *ignore-errors*) 
      (apply fn args)
  ; else trap errors
  (let (result)
    (handler-case 
	(setf result (apply fn args))
      (error (c) 
    	(error-message (format nil "Executing command: ~A" c))
        :error)
      (:no-error (c) 
	(declare (ignore c))
    	result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: dispatch-and-return-stream-event
;; Parameters: command-string: a string containing a lisp function call
;; Purpose: To execute the command and return the results on *andes-stream*
;; returns: garbage
;; Side Effects: Signals errors when there was a caught error on the execution
;;               of the command, makes sure that the stream is not being
;;               buffered, and writes the results of the execution onto the
;;               stream with the message id and type identifier.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dispatch-and-return-stream-event (command-string)
  "Dissects a command string to pull out the message id, executes the command, and prints the results with id to *andes-stream*"
  (let* ((id (subseq command-string 0 (position #\: command-string)))
	 (cmd (subseq command-string (+ (position #\: command-string) 1)))
	 ;; Execute the DDE passing the fact that it is a dde on to the
	 ;; Dispatch-stream-event call.
	 (results (dispatch-stream-event cmd :DDE t)))
    (cond ((eq results :error) ;; If there was a problem executing the string
	   (format *debug-help* "~&Returned: ~A~A: for ~A~%"
		   &nack& id command-string)
	   (format *andes-stream* "~A~A:~%" &nack& id)) ;; return negative ack
	  (t ;; otherwise, simply print the results to the stream.
	   (format *debug-help* "~&Returned: ~A~A:~A~%" &reply& id results)
	   (format *andes-stream* "~A~A:~A~%" &reply& id results)))
    ;; push the text onto the stream to prevent buffering
    (force-output *andes-stream*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: send-fbd-command
;; Parameters: command: a string containing a command that the workbench
;;                      understands.
;; Purpose: To send an unsolicited command to the workbench.
;; returns: Garbage (nil)
;; Side Effects: Prints the command to *andes-stream* and then lushes the
;;               output buffer.
;; IMPORTANT: Do not use this method to send a command 
;; that will put the interface in a modal loop (such as opening a
;; dialog box) if it is waiting for a return result from the 
;; help system. This will result in losing the help system's return
;; value. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun send-fbd-command (command)
  "Sends a command to the workbench using *andes-stream*."
  (format *debug-help* "Sending: ~A~A~%" &cmd& command)
  (format *andes-stream* "~A~A~%" &cmd& command)
  (finish-output *andes-stream*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function: connection-running
;; Parameters: nil
;; Purpose: To check *andes-stream* to see if the connection is still open.
;; returns: t: if stream is still open.
;;          nil: if the stream is nil or closed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun connection-running ()
  "Checks to see if *andes-stream* is still open."
  (and *andes-stream* (open-stream-p *andes-stream*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This was applied to incoming raw command strings as defense before passing 
;; it through read-from-string: escape comma and backslash, which will interfere
;; with parsing into objects by Lisp read. This simple method applies this blindly 
;; throughout the string, without taking into account whether it occurs within
;; vbar-delimited symbol, or quote-delimited string, for example. 
;;
;; This should not be necessary now that the workbench tries to ensure that 
;; arguments in all command strings are sent in a Lisp-readable form.
;; That was not done perfectly in past versions of Andes; in particular,
;; bad chars in student-typed-equation box contents used to crash the helpsys.
;; Should be safer now, and problems should be fixed on workbench side.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun escape-special-characters (string)
  (loop for i from 0 to (1- (length string))
      with special = '(#\, #\\)
      appending
	(let ((c (char string i)))
	  (if (member c special) (list #\\ c)
	    (list c)))
      into newstring
      finally (return (concatenate 'string newstring))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dummy 'main' or begin function
;;
;; if wb-port is specified, make an active connection to that port on
;; local host so as to attach to a running workbench listening on that port. 
;; Otherwise we listen for connections as a server on the default port
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun andes-start (&key wb-port)
  "initialize the andes help system server state"
  (andes-init)
  ; in runtime version: wb can pass a port number in to us on command line
  ; in which case we will actively connect to that port. 
  #+allegro-cl-runtime (when (>= (sys:command-line-argument-count) 2)
                         (setf wb-port
                           (read-from-string (sys:command-line-argument 1))))
  (if wb-port (make-active-connection wb-port)
     (await-passive-connection))
  (andes-run)
  ;; andes-run should always call andes-terminate when done so following 
  ;; shouldn't be necessary, but shouldn't hurt to be safe just in case
  #+allegro-cl-runtime (exit 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Andes-init (CL)
;; Initialize Andes for execution but do not start the tcp server.  This is 
;; called directly only when using the HelpDriver to execute Andes within the
;; same lisp process.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun andes-init ()
  "initialize the andes help system server state"
  ;; Set the base help system time
  (setq **base-Htime** (universal-time->htime (get-universal-time)))
  
  ;; Mainly for safety in runtime image: ensure Lisp reads floating point 
  ;; numbers into doubles, no matter what setting had been in effect before.
  (setq *read-default-float-format* 'double-float)
  
  ;; in runtime version only: set *andes-path* to process working directory
  #+allegro-cl-runtime (setf *andes-path* 
			     (make-pathname :host (pathname-host *default-pathname-defaults*)
					    :device (pathname-device *default-pathname-defaults*)
					    :directory (pathname-directory *default-pathname-defaults*)
					    :name nil :type nil))
  ;; We also fix up the AndesModule system's compiled-in base-name var 
  ;; (set when helpsys was built) so runtime use loads from the runtime 
  ;; Andes directory.
  #-asdf (setf *Base-Andes-Module-Path* (namestring *andes-path*))
  (format T "Starting Andes, *andes-path* = ~A~%" *andes-path*)
  (doSafety :in2pre)
  (solver-load)
  (solver-logging *solver-logging*)
  (physics-algebra-rules-initialize) ;initialize grammar
  (enable-errors)
  )

(defun andes-stop ()
"set the exit flag to cause the server to exit event loop"
  (setq *andes-stop* t))

(defun andes-terminate ()
"terminate this instance of the help server on session end"
  (terminate-server)
  (solver-unload)
  (format *debug-help* "~&Andes session finished!~%")
  ; in runtime version only: exit Lisp when session is done
  #+allegro-cl-runtime (exit 0))
