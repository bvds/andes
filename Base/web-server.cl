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
	   :*stdout* :print-sessions :*env* :close-idle-sessions))

(in-package :webserver)

(defvar *server* nil)
(defvar *stdout* *standard-output*)
(defvar *service-methods* (make-hash-table :test #'equal))

(defun start-json-rpc-service (uri &key (port 8080))
  "Start a web server that runs a single service for handling json-rpc"
  ;; One could easily extend this to multiple web servers or multiple
  ;; services, but we don't need that now.
  (when *server* (error "server already started"))
  (setq  *dispatch-table*
	 (list #'dispatch-easy-handlers
	       (create-prefix-dispatcher uri 'handle-json-rpc)
	       #'default-dispatcher))

  ;; This is just for the debugging stage
  (setq *show-lisp-errors-p* t
        *show-lisp-backtraces-p* t)
  
  (setf *server* (start (make-instance 'acceptor :port port))))

(defun stop-json-rpc-service ()
  (stop *server*) (setf *server* nil))

(defmacro defun-method (uri name lambda-list &body body)
  "Defines a function and registers it as a method associated with a service."
  `(progn (defun ,name ,lambda-list ,@body)
    (register-method-for-service ,uri #',name 
     (string-downcase (symbol-name ',name)))))

(defun register-method-for-service (uri method method-name)
  (setf (gethash (list uri method-name) *service-methods*) method))

;; Still needs to be done:
;;  Need to handle errors with bad json (hard, since clients already check json)
;;  Generate smd from methods (the client is supposed to test posts against
;;       the smd, just need to make sure smd matches the methods).
;;  Need webserver handler for helpsystem timeout (returning json rpc?).
;;  There is no provision for timing out (hung help system).  Need webserver
;;       timeout set to larger than helpsystem timeout.  Maybe only kill turn
;;       after timeout if there is a subsequent turn waiting?
;;  How to handle turns that never make it to the webserver?  This will
;;       cause subsequent turns to hang.  Maybe timeout if a subsequent
;;       turn is waiting and session is unlocked.

(defun handle-json-rpc ()
  "Handles json-rpc 1.0 and 2.0"
  (setf (content-type) "application/json; charset=UTF-8")
  ;; Hunchentoot is supposed to take care of charset encoding
  (unless (search "application/json" (header-in* :content-type))
    ;; with the wrong content-type, just send string back
    (return-from handle-json-rpc "\"content-type must be application/json\""))
  (let* (result error1 reply
		(in-json (decode-json-from-string 
			  (raw-post-data :force-text t)))
		(service-uri (request-uri*))
		(method (cdr (assoc :method in-json)))
		(params (cdr (assoc :params in-json)))
		(turn (cdr (assoc :id in-json)))
		(client-id (header-in* :client-id))
		(version (assoc :jsonrpc in-json))
		(method-func (gethash (list service-uri method) 
				      *service-methods*)))
    (format *stdout* "session ~A calling ~A with ~S~%" 
	    client-id method params)
    (cond
      ;; Here, we assume that the client generates the user-problem
      ;; session id.  Alternatively, we could have the server generate it and
      ;; return it to the client at the beginning of a new session
      ((null client-id)
       (setq error1 (if version
			`((:code . -32000) 
			  (:message . "missing http header client-id"))
			(format nil "missing http header client-id"))))
      ((null method)
       (setq error1 (if version
			`((:code . -32600) (:message . "Method missing."))
			(format nil "Method missing"))))
      ((null method-func)
       (setq error1 (if version
			`((:code . -32601) (:message . "Method not found")
			  (:data . ,method))
			(format nil "Can't find method ~S for service ~A" 
				method service-uri))))
      ;; need error handler for the case where the arguments don't 
      ;; match the function...
      (t 
       (setq result (execute-session client-id turn 
				     method-func params))))
    (format *stdout* "  result ~S error ~S~%" result error1)
    ;; only give a response when there is an error or id is given
    (when (or error1 turn)
      (when (or error1 (not version)) (push (cons :error error1) reply))
      (when (or result (not version)) (push (cons :result result) reply))
      (push (cons :id turn) reply)
      (when version (push version reply))
      (encode-json-alist-to-string reply))))

;;; 
;;;     Session and turn management
;;;

(defparameter *sessions* (make-hash-table :test #'equal) 
  "A list of active sessions")
(defstruct ssn turn lock queue time environment)
;; Each thread has the variable *env* available to store
;; sesssion-specific information between turns.
;; If a method sets *env* to nil, this indicates the session is finished.
(defvar *env*) ;Declare *env* to be special, but don't bind it globally.

(defun print-sessions (&optional str)
  "Print sessions to see what is going on."
    (maphash #'(lambda (id session) 
		 (format str "session ~A with turn ~A, time ~A,~%   env ~A~%" 
			 id (ssn-turn session) (ssn-time session)
			 (ssn-environment session)))
	     *sessions*))

(defun get-session (session turn)
  "Return a session for a given hash or create a new one"
  (or (gethash session *sessions*)
      (setf (gethash session *sessions*) 
	    (make-ssn 
	     :queue #+sbcl(sb-thread:make-waitqueue :name "turn queue")
	     :lock #+sbcl(sb-thread:make-mutex :name "turn lock")
	     :time (get-internal-real-time) :turn turn))))

(defun close-idle-sessions (&optional (idle 7200))
  "Close all (idle) sessions."
  (let ((cutoff (- (get-internal-real-time) 
		   (* idle internal-time-units-per-second))))
    (maphash #'(lambda (id session) 
		 (when (< (ssn-time session) cutoff)
		   ;; when a session is locked, we should 
		   ;; force it to return an error
		   #+sbcl(when (sb-thread:mutex-value (ssn-lock session))
			    (sb-thread:interrupt-thread 
			     (sb-thread:mutex-value (ssn-lock session))
			     #'hung-session-error))
		   ;; Don't know how to access other threads
                   ;; in non-sbcl case.
		   #-sbcl(when (ssn-lock session))
		   (remhash id *sessions*))) *sessions*)))

(defun hung-session-error ()
  (error "Help system running for too long, killing turn."))

(defun lock-session (session turn)
  "locks session based on turn number"
  ;; There are dire warnings in sbcl documentation about
  ;; using get-mutex and interrupts.
  #+sbcl(sb-thread:get-mutex (ssn-lock session) nil t)
  ;; If turns can be numbered, wait until this turn is next
  (when (numberp turn)
     ;; wait until earlier turns are all done
    (loop while (> turn (+ (ssn-turn session) 1))
	  ;; Estimate minimum time needed to process  
	  ;; number of turns waiting times number of sessions
	  do #-sbcl(sleep (* (/ (hash-table-count *sessions*) 
				internal-time-units-per-second)
			     (- turn (+ (ssn-turn session) 1))))
	  #+sbcl(sb-thread:condition-wait (ssn-queue session) 
					  (ssn-lock session))))
  ;; lock session.
  #-sbcl(progn (loop until (not (ssn-lock session)) 
		     do (sleep (/ (hash-table-count *sessions*) 
				  internal-time-units-per-second)))
	       (setf (ssn-lock session) turn))
  (setf (ssn-turn session) turn))

(defun unlock-session (session)
  "unlocks session"
  ;; time used to determine idle sessions.
  (setf (ssn-time session) (get-internal-real-time))
  ;; unlock session and wake other turns in session
  #-sbcl(setf (ssn-lock session) nil)
  #+sbcl(progn (sb-thread:condition-broadcast (ssn-queue session))
		(sb-thread:release-mutex (ssn-lock session))))

(defun execute-session (session-hash turn func params)
  "execute a function in the context of a given session when its turn comes"
  (let (func-return 
	(session (get-session session-hash turn))
	;; Make thread-local binding of special variable *env*
	*env*)
    (lock-session session turn)
    ;; set up session environment for this session
    ;; env is local to the thread.
    (setf *env* (ssn-environment session))
    (setf func-return 
	  ;; Lisp errors not treated as Json rpc errors, since there
	  ;; is little the client can do to handle them.
	  (handler-case 
	      ;; execute the method
	      (apply func (if (alistp params) 
			      (flatten-alist params) params))
	    (error (condition) 
	      `(((:action . "show-hint")
		 (:text . ,(format nil "An error occurred:~%     ~A" 
				   condition)))
		((:action . "log")
		 (:error-type . ,(string (type-of condition)))
		 (:error . ,(format nil "~A" condition))
		 (:backtrace . ,(with-output-to-string 
				 (stream)
				 ;; sbcl-specific function 
				 #+sbcl(sb-debug:backtrace 10 stream))))))))
    (if *env*
	;; save session environment for next turn
	(setf (ssn-environment session) *env*)
	;; if environment has been removed, remove session from table.
	(remhash session-hash *sessions*))
    ;; unlock session, if locked
    (unlock-session session)
    func-return))

(defun alistp (x) 
  "determine if x is an alist" 
  (and (listp x) (every #'consp x)))

(defun flatten-alist (x) 
  "turn an alist into plain list"
  (mapcan #'(lambda (x) (list (car x) (cdr x))) x))
