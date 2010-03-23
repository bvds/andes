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
	   :*stdout* :print-sessions :*env* :close-idle-sessions :*debug*
	   :*debug-alloc* :get-session-env :*log-id*))

(in-package :webserver)

(defvar *server* nil)
(defvar *log-function* nil "Logging function, function of 3 variables.")
(defvar *log-id* nil "Id sent to the logging function.")
(defvar *stdout* *standard-output*)
(defvar *service-methods* (make-hash-table :test #'equal))

(defparameter *debug* t "Special error conditions for debugging")
(defparameter *debug-alloc* nil "Turn on memory profiling.")
#+sbcl (eval-when (:load-toplevel :compile-toplevel)
	 (require :sb-sprof))
(defvar *print-lock*  #+sbcl (sb-thread:make-mutex)
	#+(and (not sbcl) bordeaux-threads) (bordeaux-threads:make-lock))

(defmacro with-a-lock (args &body body)
  "Choose method for setting mutex"
  #+sbcl `(sb-thread:with-mutex ,args ,@body)
  #+(and (not sbcl) bordeaux-threads) 
  `(bordeaux-threads:with-lock-held 
       ;; ignore any sbcl-specific flags
       ,(list (car args)) ,@body)
  #-(or sbcl bordeaux-threads) 
  '(error "no thread locking, possible race condition"))

(defun start-json-rpc-service (uri &key (port 8080) log-function
			       server-log-path)
  "Start a web server that runs a single service for handling json-rpc"
  ;; One could easily extend this to multiple web servers or multiple
  ;; services, but we don't need that now.
  (when *server* (error "server already started"))
  (setq  *dispatch-table*
	 (list #'dispatch-easy-handlers
	       (create-prefix-dispatcher uri 'handle-json-rpc)
	       #'default-dispatcher))

  ;; Error handlers
  (setf *http-error-handler* 'json-rpc-error-message)
  (setf *log-function* log-function)
  ;; Log any errors in Hunchentoot or not otherwise handled.
  ;; In particular, log any errors or warning in *log-function*
  (when server-log-path (setf *MESSAGE-LOG-PATHNAME* server-log-path))

  ;; Test for multi-threading
  (unless hunchentoot::*supports-threads-p*
    (warn "Hunchentoot running without thread support, performance may be seriously degraded."))

  (setf *server* (start (make-instance 'acceptor :port port))))

(defun json-rpc-error-message (err)
  (format nil "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": ~A, \"message\": \"Hunchentoot error:  ~A\"}, \"id\": null}" 
	  err (reason-phrase err)))

(defun stop-json-rpc-service ()
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
  (let* ((in-json 
	  ;; By default, cl-json turns camelcase into dashes:  
	  ;; Instead, we are case insensitive, preserving dashes.
	  (let ((*json-identifier-name-to-lisp* #'string-upcase))
	    ;; It would be much better if we gave the source of the error.
	    (ignore-errors (decode-json-from-string 
			    (raw-post-data :force-text t)))))
	 (service-uri (request-uri*))
	 (method (cdr (assoc :method in-json)))
	 (params (cdr (assoc :params in-json)))
	 (turn (cdr (assoc :id in-json)))
	 (client-id (header-in* :client-id))
	 ;; Make thread local variable, this is the id used by logging
	 (*log-id* client-id)
	 (*trace-output* *stdout*) ;So we can see traces.
	 ;; If I can't parse the json, assume it is 2.0.
	 (version (if in-json (assoc :jsonrpc in-json) '(:jsonrpc . "2.0")))
	 (method-func (gethash (list service-uri method) 
			       *service-methods*))
	 reply)
    
    (when *debug* (with-a-lock (*print-lock* :wait-p t) 
		    (format *stdout* "session ~A calling ~A with~%     ~S~%" 
			    client-id method params)))
    #+sbcl (when *debug-alloc* 
	     (sb-sprof:start-profiling 
	      :mode :alloc :threads (list sb-thread:*current-thread*)))
    
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
	   (values nil (if version
			   `((:code . -32600) (:message . "Method missing."))
			   "Method missing")))
	  ((null method-func)
	   (values nil (if version
			   `((:code . -32601) (:message . "Method not found")
			     (:data . ,method))
			   (format nil "Can't find method ~S for service ~A" 
				   method service-uri))))
	  ;; For testing client error handler
	  ((and *debug* (equal (cdr (assoc :text params)) 
			       "json-rpc-test-error"))
	   (values nil (if version 
			   `((:code . -32099) 
			     (:message . "json-rpc-test-error response:  Yes, error messages should be concise, but the client should still be able to handle longer ones."))
			   "json-rpc-test-error response.")))
	  ;; need error handler for the case where the arguments don't 
	  ;; match the function...
	  (t (execute-session client-id turn method-func params)))
      
      #+sbcl (when *debug-alloc* (sb-sprof:stop-profiling))
      (when *debug* (with-a-lock (*print-lock* :wait-p t) 
		      (format *stdout* 
			      "session ~a result~%     ~S~%~@[     error ~S~%~]" 
			      client-id result error1)))
      
      ;; only give a response when there is an error or id is given
      (when (or error1 turn)
	(when (or error1 (not version)) (push (cons :error error1) reply))
	(when (or result (not version)) (push (cons :result result) reply))
	(push (cons :id turn) reply)
	(when version (push version reply))

	(turn-log-wrapper *log-id* 
			  (raw-post-data :force-text t)
			  reply)))))

(defun turn-log-wrapper (id input-json reply)
  "log turn pair to database"
  (let ((reply-json
	 ;; By default, cl-json turns dashes into camel-case:  
	 ;; Instead, we convert to lower case, preserving dashes.
	 (let ((*lisp-identifier-name-to-json* #'string-downcase))
	   (encode-json-alist-to-string reply))))
    
        (when *log-function*
	  ;; Log incoming and outgoing raw json-rpc and user/problem session id
	  (funcall *log-function* id input-json reply-json))
	reply-json))

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

(defvar *sessions* 
  (make-hash-table :test #'equal
		   ;; make concurrent access safe in sbcl
		   #-sbcl (warn "Access to *sessions* not thread-safe.")
		   #+sbcl :synchronized t) 
  "A list of active sessions")

(defstruct ssn turn mutex reply time environment)

;; Each thread has the variable *env* available to store
;; sesssion-specific information between turns.
;; If a method sets *env* to nil, this indicates the session is finished.
(defvar *env*) ;Declare *env* to be special, but don't bind it globally.

(defun print-sessions (&optional (str *stdout*))
  "Print sessions to see what is going on."
  (let ((*print-length* 50))
    (maphash #'(lambda (id session) 
		 (format str "session ~S with turn ~A, idle ~As~%" 
			 id (ssn-turn session) 
			 (/ (float (- (get-internal-real-time) 
				      (ssn-time session)))
			    (float internal-time-units-per-second))))
	     *sessions*)))

(defun get-session-env (session)
  (ssn-environment (gethash session *sessions*)))

(defun count-active-sessions ()
  "Count number of currently active (locked) sessions."
  (let ((i 0))
    (maphash #'(lambda (k v) (declare (ignore k)) 
		       (when (sb-thread:mutex-owner (ssn-mutex v)) (incf i)))
	     *sessions*)
    i))
  
(defun get-session (session)
  "Return a session for a given hash or create a new one"
  (sb-ext:with-locked-hash-table (*sessions*)
    (or (gethash session *sessions*)
	(setf (gethash session *sessions*) 
	      (make-ssn :time (get-internal-real-time)
			:mutex #+sbcl (sb-thread:make-mutex)
			#+(and (not sbcl) bordeaux-threads)
			(bordeaux-threads:make-lock)
			#-(or sbcl bordeaux-threads) (warn "can't lock")
			)))))

(defun close-idle-sessions (&key (idle 0) (method #'identity) params)
  "Apply method to all (idle) sessions.  idle is time in seconds."
  (let ((cutoff (- (get-internal-real-time) 
		   (* idle internal-time-units-per-second))))
    (maphash 
     #'(lambda (id session)
	 (when (< (ssn-time session) cutoff)
	   (let ;; execute-session only returns one value (no error).
	       ((result (execute-session id nil method params)))
	     (when *debug* 
	       (format *stdout* "Shutting down session ~A using ~A:~%    ~A~%" 
		       id method result))
	     ;; We can't push reply back to the client;
	     ;; but we do want to log it.
	     (turn-log-wrapper 
	      id
	      ;; Make fake log entry for command sent.
	      ;; By default, cl-json turns dashes into camel-case:  
	      ;; Instead, we convert to lower case, preserving dashes.
	      (let ((*lisp-identifier-name-to-json* #'string-downcase))
		(encode-json-alist-to-string 
		 `((:method . ,method) (:params . ,params))))
	      `((:result . ,result))))))
     *sessions*)))


(defun error-hint (condition)
  "Message shown to the user after a lisp error has occurred."
  `(((:action . "show-hint")
     (:text . ,(format nil "An error occurred:<br>~%~A~%" condition)))))

(defun log-error (condition)
  "Log after an error or warning has occurred."
  `((:action . "log")
    (:error-type . ,(string (type-of condition)))
    (:error . ,(format nil "~A" condition))
    (:backtrace . ,(with-output-to-string 
		    (stream)
		    ;; sbcl-specific function 
		    #+sbcl (sb-debug:backtrace 20 stream)))))

(defun execute-session (session-hash turn func params)
  "Execute a function in the context of a given session when its turn comes.  If the session doesn't exist, create it.  If there is nothing to save in *env*, delete session."
  (let ((session (get-session session-hash)))
    (with-a-lock ((ssn-mutex session) :wait-p t)
      (cond 
	((equalp (ssn-turn session) turn) 
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
	;; Normal case where turn is new
	(t     
	 ;; Make thread-local binding of special variable *env*
	 ;; set up session environment for this session
	 (let ((*env* (ssn-environment session))
	       s-reply log-warn)
	   (setf (ssn-turn session) turn)
	   (setf s-reply 
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
					   (muffle-warning)))
			(sb-ext:timeout 
			 #'(lambda (c) (push (log-error c) log-warn)
				   (return-from unwind (error-hint c)))))
		     
		     ;; execute the method
		     ;; Timeout not working, due to mysql, Bug #1708
		     (sb-ext:with-timeout 5
		       (apply func (if (alistp params) 
				       (flatten-alist params) params))))))
	   
	   ;; Make sure method returned a list of alists
	   (unless (and (listp s-reply) (every #'alistp s-reply))
	     (setf s-reply 
		   `(((:action . "log")
		      (:error-type . "return-format")
		      (:error . ,(format nil "Return must be a list of alists, not ~S" 
					 s-reply))))))
	   
	   ;; Add any log messages for warnings or errors to the return
	   (setf s-reply (append s-reply log-warn))
	   (setf (ssn-reply session) s-reply)
	   (setf (ssn-time session) (get-internal-real-time))	   
	   (if *env*
	       ;; save session environment for next turn
	       (setf (ssn-environment session) *env*)
	       ;; if environment has been removed, remove session from table.
	       (sb-ext:with-locked-hash-table (*sessions*)
		 (remhash session-hash *sessions*)))
	   
	   s-reply))))))

(defun alistp (x) 
  "determine if x is an alist" 
  (and (listp x) (every #'consp x)))

(defun flatten-alist (x) 
  "turn an alist into plain list"
  (mapcan #'(lambda (x) (list (car x) (cdr x))) x))
