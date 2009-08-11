;; Author(s):
;;   Brett van de Sande, Feb 2009
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
;; ctl-c ctl-k compiles entire file 
(in-package :cl-user)

(defpackage :webserver
  (:use :cl :hunchentoot :json)
  (:export :defun-method :start-json-rpc-service :stop-json-rpc-service 
	   :*stdout* :print-sessions :*env* :close-idle-sessions *debug*
	   :get-session-env))

(in-package :webserver)

(defvar *server* nil)
(defvar *stdout* *standard-output*)
(defvar *service-methods* (make-hash-table :test #'equal))

(defvar *debug* t "Special error conditions for debugging")

(defun start-json-rpc-service (uri &key (port 8080))
  "Start a web server that runs a single service for handling json-rpc"
  ;; One could easily extend this to multiple web servers or multiple
  ;; services, but we don't need that now.
  (when *server* (error "server already started"))
  (setq  *dispatch-table*
	 (list #'dispatch-easy-handlers
	       (create-prefix-dispatcher uri 'handle-json-rpc)
	       #'default-dispatcher))

  ;; Error handlers
  (setq *show-lisp-errors-p* t)
  (setf *http-error-handler* 'json-rpc-error-message)

  ;; Set up database
  (create-data-objects)

  (setf *server* (start (make-instance 'acceptor :port port))))

(defun json-rpc-error-message (err)
  (format nil "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": ~A, \"message\": \"Hunchentoot error:  ~A\"}, \"id\": null}" 
	  err (reason-phrase err)))

(defun stop-json-rpc-service ()
  (destroy-data-objects)
  (stop *server*) (setf *server* nil))

(defmacro defun-method (uri name lambda-list &body body)
  "Defines a function and registers it as a method associated with a service."
  `(progn (defun ,name ,lambda-list ,@body)
    (register-method-for-service ,uri #',name 
     (string-downcase (symbol-name ',name)))))

(defun register-method-for-service (uri method method-name)
  (setf (gethash (list uri method-name) *service-methods*) method))

(defun handle-json-rpc ()
  "Handles json-rpc 1.0 and 2.0"
  (setf (content-type*) "application/json; charset=UTF-8")
  ;; Hunchentoot is supposed to take care of charset encoding
  (unless (search "application/json" (header-in* :content-type))
    ;; with the wrong content-type, just send string back
    (return-from handle-json-rpc "\"content-type must be application/json\""))
  ;; By default, cl-json turns dashes into camelcase:  we don't want that.
  (let* ((*lisp-identifier-name-to-json* #'string-downcase)
	 (in-json 
	  ;; It would be much better if we gave the source of the error.
	  (ignore-errors (decode-json-from-string 
			  (raw-post-data :force-text t))))
	 (service-uri (request-uri*))
	 (method (cdr (assoc :method in-json)))
	 (params (cdr (assoc :params in-json)))
	 (turn (cdr (assoc :id in-json)))
	 (client-id (header-in* :client-id))
	 ;; If I can't parse the json, assume it is 2.0.
	 (version (if in-json (assoc :jsonrpc in-json) '(:jsonrpc . "2.0")))
	 (method-func (gethash (list service-uri method) 
			       *service-methods*))
	 reply)
    (write-problem-transaction-to-database "client" client-id
					   (raw-post-data :force-text t))
    (when *debug* (format *stdout* "session ~A calling ~A with~%     ~S~%" 
			  client-id method (raw-post-data :force-text t)))
    
    (multiple-value-bind (result error1)
	(cond
	  ((null in-json)
	   (values nil (if version
			   `((:code . -32700) (:message . "JSON parse error")
			     (:data . ,(raw-post-data :force-text t)))
			   "JSON parse error")))
	  ;; Here, we assume that the client generates the user-problem
	  ;; session id.  Alternatively, we could have the server generate it and
	  ;; return it to the client at the beginning of a new session
	  ((null client-id)
	   (values nil (if version
			   `((:code . -32000) 
			     (:message . "missing http header client-id"))
			   (format nil "missing http header client-id"))))
	  ((null method)
	   ;; Need more general checks for bad json-rpc.  This is a bit
	   ;; hard because the clients already do this.
	   (values nil (if version
			   `((:code . -32600) (:message . "Method missing."))
			   "Method missing")))
	  ((null method-func)
	   (values nil (if version
			   `((:code . -32601) (:message . "Method not found")
			     (:data . ,method))
			   (format nil "Can't find method ~S for service ~A" 
				   method service-uri))))
	  ((and *debug* (equal (cdr (assoc :text params)) 
			       "json-rpc-test-error"))
	   (values nil (if version 
			   `((:code . -32099) 
			     (:message . "json-rpc-test-error response."))
			   "json-rpc-test-error response.")))
	  ;; need error handler for the case where the arguments don't 
	  ;; match the function...
	  (t (execute-session client-id turn method-func params)))
      
      (when *debug* 
	(format *stdout* "result ~S~%~@[error ~S~%~]" result error1))

      ;; only give a response when there is an error or id is given
      (when (or error1 turn)
	(when (or error1 (not version)) (push (cons :error error1) reply))
	(when (or result (not version)) (push (cons :result result) reply))
	(push (cons :id turn) reply)
	(when version (push version reply))
	(let ((return-json (encode-json-alist-to-string reply)))
	  ;; send to mysql
	  (write-problem-transaction-to-database "server" client-id 
						 return-json)
	  return-json)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;         Send to database
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destroy-data-objects ()
  (clsql:disconnect))

(defun create-data-objects ()

    (clsql:connect '(nil "andes" "root" "sin(0)=0") :database-type :mysql)

    ;; in mysql:  describe class_information;
    (clsql:def-view-class class_information ()
      ((classID
	:db-kind :key
	:db-constraints :not-null
	:type integer
	:initarg :classid)
       (name
	:accessor name
	:type (varchar 45)
	:initarg :name)
       (school
	:accessor school
	:type (varchar 45)
	:initarg :school)
       (period
	:accessor period
	:type (varchar 45)
	:initarg :period)
       (description
	:accessor description
	:type (varchar 250)
	:initarg :description)
       (instructorName
	:accessor instructorName
	:type (varchar 50)
	:initarg :instructorName)
       (schoolyearInfo
	:accessor schoolyearInfo
	:type (varchar 50)
	:initarg :schoolyearInfo)
       (datasetID
	:type integer
	:initarg :datasetID))
      (:base-table student_dataset))
  
  ;; describe student_dataset;
  (clsql:def-view-class student_dataset ()
    ((datasetID
      :db-kind :key
      :db-constraints :not-null
      :type integer
      :initarg :datasetID)
     (datasetname
      :accessor datasetname
      :type (varchar 250)
      :initarg :datasetname)
     (modulename
      :accessor modulename
      :type (varchar 45)
      :initarg :modulename)
     (groupname
      :accessor groupname
      :type (varchar 45)
      :initarg :groupname)
     (problemname
      :accessor problemname
      :type (varchar 45)
      :initarg :problemname)))
  
  ;; describe problem_attempt
    (clsql:def-view-class problem_attempt ()
      ((attemptID
	:db-kind :key
	:db-constraints :not-null
	:type integer
	:initarg :attemptID)
       (userName
	:accessor userName
	:type (varchar 20)
	:initarg :userName)
       (sessionID
	:accessor sessionID
	:type (varchar 45)
	:initarg :sessionID)
       (startTime
	:accessor startTime
	:type date
	:initarg :startTime)
       (classinformationID
	:type integer
	:initarg :classinformationID))
      (:base-table class_information))

  ;; describe problem_attempt_transaction;
  (clsql:def-view-class problem_attempt_transaction ()
    ((tID
      :db-kind :key
      :db-constraints :not-null
      :type integer
      :initarg :tID)
     (attemptID
      :db-kind :key
      :db-constraints :not-null
      :type integer
      :initarg :attemptID)
     (problem_attempt
      :accessor transaction_problem
      :db-kind join
      :db-info (:join-class problem_attempt
                            :home-key attemptID
                            :foreign-key attemptID
                            :set nil)) 
     (command
      :accessor command
      :type (varchar 2000)
      :initarg :command)
     (initiatingParty
      :accessor initiatingParty
      :type (varchar 30)  ;is enum in database
      :initarg :initiatingParty)))

    (setf clsql:*db-auto-sync* t)

 )

(defun write-problem-transaction-to-database (direction client-id j-string)
(let (( newProblemAttemptTransaction clsql:make-instance 'problem_attempt_transaction :attemptID client-id :command j-string :initiatingParty direction)))  
  j-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;     Session and turn management
;;;
;; json-rpc messages are asynchronous:  if several messages are sent
;; to the server, they are not guaranteed to arrive in order.  There
;; are two possible strategies:  
;;   1.  Several json-rpc messages may be sent at once.  In this case the
;;       server must ensure the order of messages and the client
;;       must ensure the order of replies.  In the case of a dropped
;;       message, error handling is difficult.
;;   2.  The client sends one message at a time, awaiting a reply from 
;;       the server before sending another.  If a message times out, the 
;;       client may re-send a message, using the same id.
;; We have chosen the latter approach.  We need to handle three kinds of 
;; errors:
;;   1.  A json-rpc message is lost on the way to the server.  In this
;;       case, the server does nothing special.  However, the server
;;       must handle the arrival of several messages with the same id.  
;;       We specify that the server will return the same reply to 
;;       to any subsequent messages having the same id.
;;   2.  A session turn hangs (help system bug).  In this case the server
;;       receives a message for a session that is locked.  
;;       If the turn (id) is the same, it waits and rechecks the lock.  
;;       If the session is still locked, the server interrupts the earlier 
;;       turn, and returns a log message.  The earlier turn will reply 
;;       with a lisp error condition.
;;   3.  A json-rpc reply is lost on the way back to the client.
;;       The server receives a repeat request (same id) as the last
;;       request.  The server returns the earlier reply.

(defparameter *sessions* (make-hash-table :test #'equal) 
  "A list of active sessions")
(defstruct ssn turn lock mutex reply time environment)
;; Each thread has the variable *env* available to store
;; sesssion-specific information between turns.
;; If a method sets *env* to nil, this indicates the session is finished.
(defvar *env*) ;Declare *env* to be special, but don't bind it globally.

(defun print-sessions (&optional str)
  "Print sessions to see what is going on."
  (let ((*print-length* 50))
    (maphash #'(lambda (id session) 
		 (format str "session ~S with turn ~A, time ~A,~%   env ~A~%" 
			 id (ssn-turn session) (ssn-time session)
			 (ssn-environment session)))
	     *sessions*)))

(defun get-session-env (session)
  (ssn-environment (gethash session *sessions*)))

(defun count-active-sessions ()
  "Count number of currently active (locked) sessions."
  (let ((i 0))
    (maphash #'(lambda (k v) (declare (ignore k)) 
		       (when (ssn-lock v) (incf i)))
	     *sessions*)
    i))
  
(defun get-session (session)
  "Return a session for a given hash or create a new one"
  (or (gethash session *sessions*)
      (setf (gethash session *sessions*) 
	    (make-ssn :time (get-internal-real-time)
		      :mutex (or #+sbcl (sb-thread:make-mutex)
				 #+bordeaux-threads 
				 (bordeaux-threads:make-lock))))))

(defun close-idle-sessions (&optional (idle 7200))
  "Close all (idle) sessions.  idle is time in seconds."
  (let ((cutoff (- (get-internal-real-time) 
		   (* idle internal-time-units-per-second))))
    (maphash #'(lambda (id session) 
		 (when (< (ssn-time session) cutoff)
		   ;; when a session is locked, we should 
		   ;; force it to return an error.
		   (when (ssn-lock session)
		     #+sbcl (sb-thread:interrupt-thread 
			     (ssn-lock session) #'hung-session-error)
		     ;; Don't know how to access other threads
		     ;; in non-sbcl case.
		     #-sbcl (error "killing dead threads only in sbcl"))
		   (remhash id *sessions*))) *sessions*)))

(defconstant *time-per-turn* 1.0 "Estimate of maximum time in seconds needed to execute a single turn.")

(defun hung-session-error ()
  (error "Help system running for too long, killing turn."))

(defmacro with-a-lock (args &body body)
  "Choose method for setting mutex"
  (or #+sbcl `(sb-thread:with-mutex ,args ,@body)
      #+bordeaux-threads `(bordeaux-threads:with-lock-held ,@body)
      '(error "no thread locking, possible race condition")))

(defun lock-session (session turn)
  "Attempt to lock a session; return a result or error if unsuccessful."
  ;; If previous attempt at this turn is still locked, give it
  ;; a chance to finish up.  This handles the case where the
  ;; connection is lost and then restored.
  (when (and (ssn-lock session) (equalp (ssn-turn session) turn))
    (sleep (* (count-active-sessions) *time-per-turn*)))
  ;; Set mutex to avoid possible race condition
  ;; with several threads trying to set (ssn-lock session).
  (with-a-lock ((ssn-mutex session) :wait-p t)
    (if (ssn-lock session)
	(progn 
	  ;; If session is still locked, try to interrupt other thread,
	  ;; so that it will return a lisp error.
	  #+sbcl (sb-thread:interrupt-thread (ssn-lock session)
					     #'hung-session-error)
	  #+(and (not sbcl) bordeaux-threads)
	  (bordeaux-threads:interrupt-thread (ssn-lock session)
					    #'hung-session-error)
	  #-(or sbcl bordeaux-threads) (error "can't kill hung thread")
	  ;; return message, since client doesn't have to 
	  ;; act on this, just log it.
	  '(((:action . "log") (:error-type . "interrupt-turn")
		 (:error . "Sent interrupt to thread running turn."))))
	(cond ((equalp (ssn-turn session) turn) 
	       ;; Return same reply we sent last time.
	       ;; This handles the case where the rpc-json reply was lost
	       (ssn-reply session))
	      ;; Message from earlier turn arrives late.
	      ;; Right now, this only works if turns are labeled
	      ;; by increasing numbers.  
	      ;; Otherwise, we would need to create a list of old ids.
	      ((and (numberp turn) (numberp (ssn-turn session))
		   (< turn (ssn-turn session)))
	       ;; return message, since client doesn't have to 
	       ;; act on this, just log it.
	       '(((:action . "log") (:error-type . "old-turn")
		 (:error . "Reply already sent for this turn."))))
	      ;; Normal case where turn is new and session is unlocked.
	      (t     
	       (setf (ssn-lock session) 
		     #+sbcl sb-thread:*current-thread*
		     #+(and (not sbcl) bordeaux-threads) 
		     (bordeaux-threads:current-thread)  
		     #-(or sbcl bordeaux-threads) t)
	       (setf (ssn-turn session) turn)
	       nil)))))

(defun unlock-session (session reply)
  "unlocks session, saving reply"
  ;; time used to determine idle sessions.
  (setf (ssn-time session) (get-internal-real-time))
  (setf (ssn-reply session) reply)
  (setf (ssn-lock session) nil))

(defun error-hint (condition)
  "Message shown to the user after a lisp error has occurred."
  `(((:action . "show-hint")
     (:text . ,(format nil "An error occurred:~%     ~A,~%Please try again." 		       condition)))))

(defun log-error (condition)
  "Log after an error or warning has occurred."
  `((:action . "log")
     (:error-type . ,(string (type-of condition)))
     (:error . ,(format nil "~A" condition))
     (:backtrace . ,(with-output-to-string 
		     (stream)
		     ;; sbcl-specific function 
		     #+sbcl(sb-debug:backtrace 20 stream)))))

(defun execute-session (session-hash turn func params)
  "Execute a function in the context of a given session when its turn comes.  If the session doesn't exist, create it.  If there is nothing to save in *env*, delete session."
  (let (func-return log-warn
	(session (get-session session-hash))
	;; Make thread-local binding of special variable *env*
	*env*)
    ;; try to lock a session, if not return the reply or error
    (let ((ret (lock-session session turn)))
      ;; if lock was unsuccessful, return with message.
      (when ret (return-from execute-session ret)))
    ;; set up session environment for this session
    ;; env is local to the thread.
    (setf *env* (ssn-environment session))
    (setf func-return 
	  ;; Lisp errors are not treated as Json rpc errors, since there
	  ;; is little the client can do to handle them.
	  ;; We log both errors and warnings.  The strategy is to use
	  ;; a warning whenever possible in the helpsystem code.
	  (block unwind
	    (handler-bind 
		;; For errors, we log and break, sending a message to 
		;; the user.
		((error #'(lambda (c) (push (log-error c) log-warn) 
				  (return-from unwind (error-hint c))))
		 ;; For warnings, we log the situation and continue
		 (warning #'(lambda (c) (push (log-error c) log-warn) 
				    (continue))))
	      ;; execute the method
	      (apply func (if (alistp params) 
			      (flatten-alist params) params)))))

    ;; Make sure method actually returned a list
    (unless (listp func-return)
      (setf func-return `(((:action . "log")
			   (:error-type . "return-format")
			   (:error . ,(format nil "Return must be a list, not ~S" 
					      func-return))))))

    ;; Add any log messages for warnings or errors to the return
    (setf func-return (append func-return log-warn))

    (if *env*
	;; save session environment for next turn
	(setf (ssn-environment session) *env*)
	;; if environment has been removed, remove session from table.
	(remhash session-hash *sessions*))
    ;; unlock session, if locked
    (unlock-session session func-return)
    func-return))

(defun alistp (x) 
  "determine if x is an alist" 
  (and (listp x) (every #'consp x)))

(defun flatten-alist (x) 
  "turn an alist into plain list"
  (mapcan #'(lambda (x) (list (car x) (cdr x))) x))
