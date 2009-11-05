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
	   :get-matching-sessions))
(in-package :andes-database)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;         Send to database
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun destroy ()
  (disconnect))

(defun create ()
  
  ;; Should remove at least password from lisp and make
  ;; user enter when starting server.
  (connect '(nil "andes" "root" "sin(0)=0") :database-type :mysql))

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
			 (clsql:reconnect)
			 (invoke-restart (find-restart 'start-over))))))
    (loop (restart-case (return (progn ,@body))
	    (start-over ())))))


(defun write-transaction (direction client-id j-string)
  "Record raw transaction in database."
  (let ((checkInDatabase 
	 (reconnect-when-needed 
	   (query 
	    (format nil 
		    "SELECT clientID FROM PROBLEM_ATTEMPT WHERE clientID = '~A'" client-id)
	    :field-names nil :flatp t :result-types :auto))))
    (unless checkInDatabase 
      (execute-command 
       (format nil 
	       "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID) values ('~A',2)" 
	       client-id)))
    
    ;; If a post contains no json, j-string is lisp nil and 
    ;; sql null is inserted into database.
     (execute-command 
     (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A',~:[null~;~:*'~A'~],'~A')" 
	     client-id (make-safe-string j-string) direction))))

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

  (if (> (length extra) 0) ;can be empty string
    (format webserver:*stdout* "Getting extra parameter ~A~%" extra)
    (setf extra 0))

  ;;  session is labeled by client-id 
  ;; add this info to database
  ;; update the problem attempt in the db with the requested parameters
  (execute-command 
   (format nil "UPDATE PROBLEM_ATTEMPT SET userName='~A', userproblem='~A', userSection='~A' WHERE clientID='~A'" 
	   student problem section client-id)))

;; (andes-database:get-old-sessions '("solution-step" "seek-help") :student "bvds" :problem "s2e" :section "1234")
;;
(defun get-matching-sessions (methods &key student problem section extra)
  "Get posts associated with the given methods from all matching previous sessions."

  (if (> (length extra) 0) ;can be empty string
    (format webserver:*stdout* "Getting extra parameter ~A~%" extra)
    (setf extra 0))

  (let ((result (query 
		 (format nil "SELECT command FROM PROBLEM_ATTEMPT,PROBLEM_ATTEMPT_TRANSACTION WHERE userName = '~A' AND userProblem='~A' AND userSection='~A' AND PROBLEM_ATTEMPT.clientID=PROBLEM_ATTEMPT_TRANSACTION.clientID AND PROBLEM_ATTEMPT_TRANSACTION.initiatingParty='client'" 
			 student problem section) :flatp t))
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
		result))))
