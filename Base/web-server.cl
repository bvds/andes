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
  (:use :cl :hunchentoot :json :log-condition)
  (:export :defun-method :start-json-rpc-services :stop-json-rpc-services 
	   :*stdout* :print-sessions :*env* :close-idle-sessions :*debug*
	   :*turn-timeout* :*time*
	   :log-error :log-warn :get-any-log-tag
	   :*profile* :get-session-env :*log-id* :*log-variable*))

(in-package :webserver)

;; note that hunchentoot:*default-connection-timeout* is 20 seconds 
;; See Bug #1710
(defvar *turn-timeout* 20 "Timeout for a turn, in seconds.")
(defvar *server* nil)
(defvar *log-id* nil "Id sent to the logging function.")
(defvar *log-variable* nil "Parameter local to turn & available to logging function.")
(defvar *stdout* *standard-output*)
(defvar *service-methods* (make-hash-table :test #'equal))

(defparameter *debug* nil "Special error conditions for debugging")
(defparameter *profile* nil "Turn profiling, with report sent to *stdout*.")
(defparameter *time* nil "Turn timing, with report sent to *stdout*.")
#+sbcl (eval-when (:load-toplevel :compile-toplevel)
	 (require :sb-sprof))
(defparameter *print-lock*  #+sbcl (sb-thread:make-mutex)
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

(defun start-json-rpc-services (services &key (port 8080) server-log-path)
  "Start a web server that runs a single service for handling json-rpc"
  ;; One could easily extend this to multiple web servers or multiple
  ;; services, but we don't need that now.
  (when *server* (error "server already started"))
  (setf *dispatch-table* (list))
  (dolist (service services)
    (let ((logger (cadr (member :log-function (cdr service)))))
      (push (create-prefix-dispatcher 
	     (car service)	   
	     (cond ((cadr (member :json-rpc (cdr service)))
		    #'(lambda () (handle-json-rpc logger)))
		   ((cadr (member :post (cdr service)))
		    #'handle-post)
		   (t (error "Need valid service type."))))
	    *dispatch-table*)))
  (push #'dispatch-easy-handlers *dispatch-table*)
  
  ;; Send *debug* to file
  (unless *stdout*
    (setf *stdout* (open (merge-pathnames "help-debug.log" server-log-path)
		       :direction :output :if-exists :rename)))

  ;; Error handlers
  (setf *http-error-handler* 'json-rpc-error-message)
  (when server-log-path (setf *MESSAGE-LOG-PATHNAME* server-log-path))

  ;; Test for multi-threading
  (unless hunchentoot::*supports-threads-p*
    (warn "Hunchentoot running without thread support, performance may be seriously degraded."))

  (setf *server* (start 
		  (make-instance 
		   'easy-acceptor 
		   :port port
		   ;; access already logged by Apache
		   :ACCESS-LOG-DESTINATION nil
		   ;; Log any errors in Hunchentoot or 
		   ;; not otherwise handled.
		   ;; In particular, log any errors or 
		   ;; warning using log-function
		   :message-log-destination server-log-path
		   ))))

(defun json-rpc-error-message (err)
  (format nil "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": ~A, \"message\": \"Hunchentoot error:  ~A\"}, \"id\": null}" 
	  err (reason-phrase err)))

(defun stop-json-rpc-services ()
  (when (streamp *debug*) (close *debug*))
  (stop *server*) (setf *server* nil))

(defmacro defun-method (uri method lambda-list &body body)
  "Defines a function and registers it as a method associated with a service.  Null method indicates that this is the only handler for this service."
  (let ((function-name (or method (gensym))))
    `(progn (defun ,function-name ,lambda-list ,@body)
      (register-method-for-service ,uri #',function-name
       ,(when method (string-downcase (symbol-name method)))))))

(defun register-method-for-service (uri function-name method)
  (setf (gethash (list uri method) *service-methods*) function-name))

(defun handle-json-rpc (log-function)
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
	    ;; Failure to decode is handled as a json-rpc error below.
	    (ignore-errors (decode-json-from-string 
			    (raw-post-data :force-text t)))))
	 (service-uri (request-uri*))
	 (method (cdr (assoc :method in-json)))
	 (params (cdr (assoc :params in-json)))
	 (turn (cdr (assoc :id in-json)))
	 ;; If client-id is supplied, use it for session hash,
	 ;; else use ip address(es).
	 ;;
	 ;; A session will cease to be accessible if the client's remote 
	 ;; IP changes.  This might for example be an issue if the client 
	 ;; uses a proxy server which doesn't send correct 'X_FORWARDED_FOR' 
	 ;; headers.
	 ;;
	 ;; It might be a good idea to switch to cookies or 
	 ;; use the Hunchentoot session manager.
	 (client-id (or (header-in* :client-id) (real-remote-addr)))
	 ;; Make thread-local variable, this is the id used by logging
	 (*log-id* client-id)
	 ;; thread-local variable, available to method & log function.
	 *log-variable* 
	 (*trace-output* *stdout*) ;So we can see traces.
	 ;; If I can't parse the json, assume it is 2.0.
	 (version (if in-json (assoc :jsonrpc in-json) '(:jsonrpc . "2.0")))
	 (method-func (gethash (list service-uri method) 
			       *service-methods*))
	 #+sbcl t0 #+sbcl tt0
	 reply)
    
    (when *debug* (with-a-lock (*print-lock* :wait-p t) 
		    (format *stdout* 
			    "session ~A calling ~A with~%     ~S~%" 
			    client-id method params)))
    #+sbcl (when (or *profile* *time*)
	     (setf t0 (get-internal-run-time))
	     (setf tt0 (get-internal-real-time)))
    #+sbcl (when *profile*
	     (sb-sprof:start-profiling
	      ;; :time fails in linux :cpu fails in OS X
	      :mode #+linux :cpu #+darwin :time
	      :threads (list sb-thread:*current-thread*)))
    
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
			   "missing http header client-id")))
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
      
      ;; This prints a report based on the most recent turn.
      #+sbcl (when *profile* (sb-sprof:stop-profiling))
      #+sbcl (when (or *profile* *time*)	      
	       (let ((t1 (get-internal-run-time))
		     (tt1 (get-internal-real-time)))
		 ;; Threshold for generating a report.
		 (when (> (- tt1 tt0) (* 1.0 internal-time-units-per-second))
		   (with-a-lock (*print-lock* :wait-p t) 
		     (format *stdout* 
			     "Report for session ~A, calling ~A, ~A & ~A s, with~%    ~A~%"
			     client-id method
			     (/ (float (- t1 t0)) 
				(float internal-time-units-per-second))
			     (/ (float (- tt1 tt0)) 
				(float internal-time-units-per-second))
			     params)
		     (when *profile*
		       (sb-sprof:report :stream *stdout*))))))

      (when *debug* (with-a-lock (*print-lock* :wait-p t) 
		      (format *stdout* 
			      "session ~a result~%     ~S~%~@[     error ~S~%~]" 
			      client-id result error1)))
      
      ;; only give a response when there is an error or id is given
      (when (or error1 turn)
	(when (or error1 (not version)) (push (cons :error error1) reply))
	;; If result is empty, drop keyword entirely.
	;; This may break json-rpc or the andes3 smd?
	(when (or result (not version)) (push (cons :result result) reply))
	(push (cons :id turn) reply)
	(when version (push version reply))

	(turn-log-wrapper *log-id* log-function
			  (raw-post-data :force-text t)
			  reply)))))

(defparameter +symbol-tokens+
  (let ((x (copy-list json::+json-lisp-symbol-tokens+)))
    (setf (cdr (assoc "false" x :test #'equalp)) :false)
    x))

(defun turn-log-wrapper (id log-function input-json reply)
  "log turn pair to database"
  (let ((reply-json
	 ;; By default, cl-json turns dashes into camel-case:  
	 ;; Instead, we convert to lower case, preserving dashes.
	 (let ((*lisp-identifier-name-to-json* #'string-downcase)
               ;; turn lisp keyword :false into json boolean false.
	       (json::+json-lisp-symbol-tokens+ +symbol-tokens+))
	   (encode-json-alist-to-string reply))))
    
        (when log-function
	  ;; Log incoming and outgoing raw json-rpc and user/problem session id
	  (funcall log-function id input-json reply-json))
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

(defun close-idle-sessions (&key log-function (idle 0) (method 'identity) 
			    params)
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
	      id log-function
	      ;; Make fake log entry for command sent.
	      ;; By default, cl-json turns dashes into camel-case:  
	      ;; Instead, we convert to lower case, preserving dashes.
	      (let ((*lisp-identifier-name-to-json* #'string-downcase))
		(encode-json-alist-to-string 
		 `((:method . ,method) (:params . ,params))))
	      ;; Hack for creating an empty array in json
	      `((:result . ,(or result (make-array '(0)))))))))
     *sessions*)))


(defun error-hint (condition)
  "Message shown to the user after a lisp error has occurred."
  `(((:action . "show-hint")
     (:text . ,(format nil "An error occurred:<br>~%~A~%" condition)))))

(defun log-err (condition)
  "Log after an error or warning has occurred."
  (let ((result '((:log . "server") (:action . "log"))))
    (push `(:error-type . ,(string (type-of condition))) result)
    (push `(:text . ,(format nil "~A" condition)) result)

    ;; Name for tagged and backtrace for untagged.
    (if (member (type-of condition) '(log-error log-warn))
	(push `(:entry . ,(prin1-to-string (log-tag condition))) result)
	(let ((x (with-output-to-string (stream)
		   ;; sbcl-specific function 
		   #+sbcl (sb-debug:backtrace 25 stream))))
	  ;; Truncate length.  It would be better to truncate each 
	  ;; level, but that would involve dissecting the backtrace function.
	  ;; The backtrace function disables global formatting variables
	  ;; like *print-pretty*.
	  (push `(:backtrace . ,(subseq x 0 (min (length x) 4000))) 
		result)))
    (reverse result)))

(defun get-any-log-tag (condition)
  "Get any tag associated with condition."
  (when (member (type-of condition) '(log-error log-warn))
    (log-tag condition)))

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
	 `(((:action . "log") (:log . "server")
	    (:error-type . ,(string :log-warning))
	    (:entry . ,(string :old-turn))
	    (:text . "Reply already sent for this turn."))))
	;; Normal case where turn is new
	(t     
	 ;; Make thread-local binding of special variable *env*
	 ;; set up session environment for this session
	 (let ((*env* (ssn-environment session))
	       s-reply log-warning)
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
		       ((error #'(lambda (c) (push (log-err c) log-warning) 
					 (return-from unwind (error-hint c))))
			;; For warnings, we log the situation and continue
			(warning #'(lambda (c) (push (log-err c) log-warning) 
					   (muffle-warning)))
			(log-error #'(lambda (c) (push (log-err c) log-warning) 
					 (return-from unwind (error-hint c))))
			;; For warnings, we log the situation and continue
			(log-warn #'(lambda (c) (push (log-err c) log-warning) 
					   (muffle-warning)))
			(sb-ext:timeout 
			 #'(lambda (c) (push (log-err c) log-warning)
				   (return-from unwind (error-hint c)))))
		     
		     ;; execute the method
		     ;; See Bug #1710
		     (sb-ext:with-timeout *turn-timeout*
		       (apply func (if (alistp params) 
				       (flatten-alist params) params))))))
	   
	   ;; Make sure method returned a list of alists
	   (unless (and (listp s-reply) (every #'alistp s-reply))
	     (setf s-reply 
		   `(((:action . "log") (:log . "server")
		      (:error-type . ,(string :log-warning))
		      (:entry . ,(prin1-to-string 
				  (list :return-alist s-reply)))
		      (:text . "Return must be a list of alists.")))))
	   
	   ;; Add any log messages for warnings or errors to the return
	   (setf s-reply (append s-reply log-warning))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          Bare post and reply
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-post ()
  "Get and return (stateless) http post, no logging."
  (let* ((service-uri (request-uri*))
	 (method-func (gethash (list service-uri nil) 
			       *service-methods*))
	 (result (funcall method-func (post-parameters*))))
    (when *debug* (with-a-lock (*print-lock* :wait-p t) 
		    (format *stdout* 
			    "post calling ~A~%  with:  ~S~%  result:  ~S~%" 
			    method-func
			    (post-parameters*)
			    result)))
    result))
