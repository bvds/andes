;; Author(s):
;;   Brett van de Sande, Nicholas Vaidyanathan
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

(defpackage :andes-database
  (:use :cl :clsql :json)
  (:export :write-transaction :destroy :create :set-session 
	   :get-matching-sessions :first-session-p))
(in-package :andes-database)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;         Send to database
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *connection-spec* nil "connection specification")

(defmacro with-db (&body body)
  "Excecute body in context of opened, pooled, default database."
  `(if *connection-spec*
       (let ((*default-database* 
	      (connect *connection-spec* :database-type :mysql
		       :pool t :if-exists nil)))
	 (prog1 ,@body
	   (disconnect)))
       (error "No common database defined, can't continue.")))

(defun destroy ()
  (setf *connection-spec* nil)
  (disconnect-pooled))

(defun create ()
  ;; Should remove at least password from lisp and make
  ;; user enter when starting server.
  (setf *connection-spec* '(nil "andes" "root" "sin(0)=0")))

;; MySql drops connections that have been idle for over 8 hours.
;; The following wrapper intercepts the resulting error, reconnects
;; the database, and retries the function.
;; This code can be tested by logging into MySql, and using
;; SHOW PROCESSLIST; and KILL <Id>; to drop a connection.

(defmacro reconnect-when-needed (&body body)
  "Intercep disconnected database error, reconnect and start over."
  `(handler-bind ((clsql-sys:sql-database-data-error 
		   #'(lambda (err)
		       (when (eql 2006 (clsql:SQL-ERROR-ERROR-ID err)) 
			 (clsql:reconnect)  ;use default database
			 (invoke-restart (find-restart 'start-over))))))
    (loop (restart-case (return (progn ,@body))
	    (start-over ())))))


;; If a write-transaction is called before set-session, a
;; database error is given.
(defun write-transaction (client-id input reply)
  "Record raw transaction in database."

  ;; Connect to db via pool
  (with-db
    
    ;; If a post contains no json, j-string is lisp nil and 
    ;; sql null is inserted into database.
    (reconnect-when-needed
      (execute-command 
       (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A',~:[null~;~:*'~A'~],'client')" 
	       client-id (make-safe-string input))))
    (execute-command 
     (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A',~:[null~;~:*'~A'~],'server')" 
	   client-id (make-safe-string reply)))))

;; Escaping ' via '' follows ANSI SQL standard.
;; If the Database escapes backslashes, must also do those.
;; (In mysql, NO_BACKSLASH_ESCAPES is not set)
;; See http://lists.b9.com/pipermail/clsql-help/2005-July/000456.html
(defun make-safe-string (s)
  "Escape strings for database export."
  (and s (clsql-sys::substitute-chars-strings 
	  s '((#\' . "''") (#\\ . "\\\\")))))


(defun set-session (client-id &key student problem section extra)
  "Updates transaction with session information."

  (unless client-id (error "set-session called with no client-id"))
  
  
  (unless (> (length extra) 0) ;treat empty string as null
    (setf extra nil))   ;drop from query if missing.
  
  (with-db
    
    ;; session is labeled by client-id 
    ;; This sets up entry in PROBLEM attempt for a given session.
    (reconnect-when-needed
      (execute-command 
       (format nil "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID,userName,userproblem,userSection~@[,extra~]) values ('~a',~A,'~a','~A','~A'~@[,'~A'~])" 
	       extra client-id 2 student problem section extra)))))

;; (andes-database:get-matching-sessions '("solution-step" "seek-help") :student "bvds" :problem "s2e" :section "1234")
;;
(defun get-matching-sessions (methods &key student problem section extra)
  "Get posts associated with the given methods from all matching previous sessions."
  
  (unless (> (length extra) 0) ;treat empty string at null.
    (setf extra nil)) ;drop from query if missing.
  
  (with-db
    
    (let ((result (query 
		   (format nil "SELECT command FROM PROBLEM_ATTEMPT,PROBLEM_ATTEMPT_TRANSACTION WHERE userName='~A' AND userProblem='~A' AND userSection='~A'~@[ AND extra=~A~] AND PROBLEM_ATTEMPT.clientID=PROBLEM_ATTEMPT_TRANSACTION.clientID AND PROBLEM_ATTEMPT_TRANSACTION.initiatingParty='client'" 
			   student problem section extra) 
		   :flatp t))
	  ;; By default, cl-json turns camelcase into dashes:  
	  ;; Instead, we are case insensitive, preserving dashes.
	  (*json-identifier-name-to-lisp* #'string-upcase))
      
      ;; pick out the solution-set and get-help methods
      (remove-if #'(lambda (x) (not (member (cdr (assoc :method x))
					    methods
					    :test #'equal)))
		 ;; parse json in each member of result
		 (mapcar 
		  ;; A post with no json sent gets translated into nil;
		  ;; see write-transaction.
		  #'(lambda (x) (and x (decode-json-from-string x)))
		  result)))))

(defun first-session-p (&key student section extra)
  "Determine student has solved a problem previously."
  (unless (> (length extra) 0) ;can be empty string
    (with-db
	(> 2 (car (query 
		   (format nil "SELECT count(*) FROM PROBLEM_ATTEMPT WHERE userName = '~A' AND userSection='~A'" 
			   student section) 
		   :flatp t))))))
