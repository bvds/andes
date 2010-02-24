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

(defconstant *debug* nil "pool debug")

;; MySql drops connections that have been idle for over 8 hours.
;; However, clsql:connect using :pool tests for this error (2006).
;; This can be tested by logging into MySql, and using
;; SHOW PROCESSLIST; and KILL <Id>; to drop a connection.

;; CLSQL uses the connection-spec and :database-type arguments to determine
;; which connections from the pool can be used.
;; There is some code to use an explicit pool name, but there
;; is no documentation for this behavior.

(defmacro with-db (&body body)
  "Excecute body in context of opened, pooled, default database."
  `(if *connection-spec*
    ;; Set the default database only within the dynamic scope of the macro.
    (let ((*default-database* 
	   (connect *connection-spec* :database-type :mysql
		    :pool t :if-exists nil 
		    :make-default nil)))
      (unwind-protect (progn ,@body)
	(disconnect))) ;disconnect *default-database*
    (error "No common database defined, can't continue.")))

(defun create (&key host db user password)
  ;; Should remove at least password from lisp and make
  ;; user enter when starting server.
  (setf *connection-spec* (list host (or db "andes") (or user "root") 
				(or password "sin(0)=0")))
  (when *debug*
    (format webserver:*stdout* "create connected (~A):~%~{  ~A~%~}"
	    (hash-table-count clsql-sys::*db-pool*)
	    (let (z) (maphash #'(lambda (x y) (push y z)) 
			      clsql-sys::*db-pool*) z))))

(defun destroy ()
  (setf *connection-spec* nil)
  (when *debug*
    (format webserver:*stdout* "destroy connected (~A):~%~{  ~A~%~}"
	    (hash-table-count clsql-sys::*db-pool*)
	    (let (z) (maphash #'(lambda (x y) (push y z)) 
			      clsql-sys::*db-pool*) z)))
  (disconnect-pooled))

(defun write-transaction (client-id input reply)
  "Record raw transaction in database."
  
  ;; Connect to db via pool
  (with-db
      (when *debug*
	(format webserver:*stdout* "write-transaction with db=~A (~A)~%"
		*default-database* (hash-table-count clsql-sys::*db-pool*)))
      
      ;; Test that PROBLEM_ATTEMPT entry already exits or create an empty one
      ;; Generally, this will only happen if open-problem has not been called
      ;; or has failed.
      (unless 
	  (query
	   (format nil
		   "SELECT clientID FROM PROBLEM_ATTEMPT WHERE clientID = '~A'" 
		   client-id)
	   :field-names nil :flatp t :result-types :auto)
	(execute-command
	 (format nil
		 "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID) values ('~A',2)"
		 client-id)))
    
    ;; If a post contains no json, j-string is lisp nil and 
    ;; sql null is inserted into database.
      (execute-command 
       (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A',~:[null~;~:*'~A'~],'client')" 
	       client-id (make-safe-string input)))
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
      (when *debug* 
	(format webserver:*stdout* "set-session with db=~A (~A)~%"
		*default-database* (hash-table-count clsql-sys::*db-pool*)))
    
    ;; session is labeled by client-id 
    ;; This sets up entry in PROBLEM attempt for a given session.
      (execute-command 
       (format nil "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID,userName,userproblem,userSection~:[~;,extra~]) values ('~a',~A,'~a','~A','~A'~@[,'~A'~])" 
	       extra client-id 2 student problem section extra))))

;; (andes-database:get-matching-sessions '("solution-step" "seek-help") :student "bvds" :problem "s2e" :section "1234")
;;
(defun get-matching-sessions (methods &key student problem section extra)
  "Get posts associated with the given methods from all matching previous sessions."
  
  (unless (> (length extra) 0) ;treat empty string at null.
    (setf extra nil)) ;drop from query if missing.
  
  (with-db
      (when *debug*
	(format webserver:*stdout* "get-matching-sessions with db=~A (~A)~%" 
		*default-database* (hash-table-count clsql-sys::*db-pool*)))
    
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
	(when *debug*
	  (format webserver:*stdout* "first-session-p with db=~A (~A)~%" 
		  *default-database* (hash-table-count clsql-sys::*db-pool*)))
      (> 2 (car (query 
		 (format nil "SELECT count(*) FROM PROBLEM_ATTEMPT WHERE userName = '~A' AND userSection='~A'" 
			 student section) 
		 :flatp t))))))
