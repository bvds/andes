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
  (:use :cl :json :mysql-connect)
  (:export :write-transaction :destroy :create :set-session 
	   :read-login-file
	   :get-matching-sessions :first-session-p))
(in-package :andes-database)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;         Send to database
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *connection* nil "connection to db")

(defparameter *debug* nil "database debug")
(defvar *db-lock* #+sbcl (sb-thread:make-mutex)
	      #+(and (not sbcl) bordeaux-threads) (bordeaux-threads:make-lock))

;; Should be "defconstant"
(defparameter *skip-db* nil "don't actually use db")

;; We previously used mysql for database connections.  However,
;; libmysql installs its own signal handler for SIGALRM, erasing the 
;; sbcl signal handler.  This prevents us from timing out hung session turns.

;; MySql drops connections that have been idle for over 8 hours.
;; We use idle-cleanup-function to periodically send a trivial query.
;; Instead, it may be better to catch the associated error, 
;; and attempt to reconnect to the database.
;; A dropped connection can be simulated by logging into MySql, and using
;; SHOW PROCESSLIST; and KILL <Id>; to drop a connection.

(defmacro with-db (&body body)
  "Excecute body with db mutex."
  `(unless *skip-db*
     (if *connection*
	 (sb-thread:with-mutex (*db-lock*) ,@body)
	 (error "No common database defined, can't continue."))))

(defun read-login-file (&optional path)
  "Read the database login file; file contains user name, password & (optional) database name."
  (with-open-file (f (or path (merge-pathnames "db_user_password" 
					       cl-user::*andes-path*))
		     :if-does-not-exist nil)
    (when f (values (read-line f) (read-line f) (read-line f nil)))))

(defun create (&key user password db host)

  (multiple-value-bind (u p d) (read-login-file)
    (setf user (or user u "root"))
    (setf password (or password p (error "No database password given.")))
    (setf db (or db d "andes3")))

  (unless *skip-db*
    (setf *connection* 
	  (connect :host host :user user :password password :database db))))

(defun destroy ()
  (unless *skip-db*
    (disconnect *connection*)))

(defun write-transaction (client-id input reply)
  "Record raw transaction in database."
  
  (with-db
      ;; Test that PROBLEM_ATTEMPT entry already exits or create an empty one
      ;; Generally, this will only happen if open-problem has not been called
      ;; or has failed.
      (unless 
	  (query *connection*
	   (format nil
		   "SELECT clientID FROM PROBLEM_ATTEMPT WHERE clientID = '~A'" 
		   client-id)
	   ;:field-names nil :flatp t :result-types :auto
	   )
	(query *connection*
	 (format nil
		 "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID) values ('~A',2)"
		 client-id)))
    
    ;; If a post contains no json, j-string is lisp nil and 
    ;; sql null is inserted into database.
      (query *connection* 
       (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A',~:[null~;~:*'~A'~],'client')" 
	       client-id (make-safe-string input)))
      (query *connection* 
     (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A',~:[null~;~:*'~A'~],'server')" 
	   client-id (make-safe-string reply)))))

;; Escaping ' via '' follows ANSI SQL standard.
;; If the Database escapes backslashes, must also do those.
;; (In mysql, NO_BACKSLASH_ESCAPES is not set)
;; See http://lists.b9.com/pipermail/clsql-help/2005-July/000456.html
(defun make-safe-string (s)
  "Escape strings for database export."
  (and s (substitute-chars-strings 
	  s '((#\' . "''") (#\\ . "\\\\")))))

;; Taken from clsql file sql/utils.lisp (under LLGPL).
(defun substitute-chars-strings (str repl-alist)
  "Replace all instances of a chars with a string. repl-alist is an assoc
list of characters and replacement strings."
  (declare (simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
  (do* ((orig-len (length str))
        (new-string (make-string (replaced-string-length str repl-alist)))
        (spos 0 (1+ spos))
        (dpos 0))
      ((>= spos orig-len)
       new-string)
    (declare (fixnum spos dpos) (simple-string new-string))
    (let* ((c (char str spos))
           (match (assoc c repl-alist :test #'char=)))
      (declare (character c))
      (if match
          (let* ((subst (cdr match))
                 (len (length subst)))
            (declare (fixnum len)
                     (simple-string subst))
            (dotimes (j len)
              (declare (fixnum j))
              (setf (char new-string dpos) (char subst j))
              (incf dpos)))
        (progn
          (setf (char new-string dpos) c)
          (incf dpos))))))

;; Taken from clsql file sql/utils.lisp (under LLGPL).
(defun replaced-string-length (str repl-alist)
  (declare (simple-string str)
           (optimize (speed 3) (safety 0) (space 0)))
    (do* ((i 0 (1+ i))
          (orig-len (length str))
          (new-len orig-len))
         ((= i orig-len) new-len)
      (declare (fixnum i orig-len new-len))
      (let* ((c (char str i))
             (match (assoc c repl-alist :test #'char=)))
        (declare (character c))
        (when match
          (incf new-len (1- (length
                             (the simple-string (cdr match)))))))))

(defun set-session (client-id &key student problem section extra)
  "Updates transaction with session information."

  (unless client-id (error "set-session called with no client-id"))
    
  (unless (> (length extra) 0) ;treat empty string as null
    (setf extra nil))   ;drop from query if missing.
  
  (with-db    
    ;; session is labeled by client-id 
    ;; This sets up entry in PROBLEM attempt for a given session.
      (query *connection*
       (format nil "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID,userName,userproblem,userSection~:[~;,extra~]) values ('~a',~A,'~a','~A','~A'~@[,'~A'~])" 
	       extra client-id 2 student problem section extra))))

(defun truncate-string (x)
  "Truncate arg for warning messages."
  (subseq x 0 (min (length x) 400)))

(defmacro errors-to-warnings (object &rest forms)
  "Intercept any errors, turning them into warnings, then return."
  ;; If there are json errors, we want to log them and then soldier on.
 `(handler-case (progn ,@forms)
    (error (c) (warn (format nil "~A for ~A" (type-of c) 
			     ;; The objects are generally strings and the 
			     ;; most common errors occur for very long strings.
			     (truncate-string ,object))))))

;; (andes-database:get-matching-sessions '("solution-step" "seek-help") :student "bvds" :problem "s2e" :section "1234")
;;
(defun get-matching-sessions (methods &key student problem section extra)
  "Get posts associated with the given methods from all matching previous sessions."
  
  (unless (> (length extra) 0) ;treat empty string as null.
    (setf extra nil)) ;drop from query if missing.
  
  (with-db
    (let ((result (query *connection*
		   (format nil "SELECT initiatingParty,command FROM PROBLEM_ATTEMPT,PROBLEM_ATTEMPT_TRANSACTION WHERE userName='~A' AND userProblem='~A' AND userSection='~A'~@[ AND extra=~A~] AND PROBLEM_ATTEMPT.clientID=PROBLEM_ATTEMPT_TRANSACTION.clientID" 
			   student problem section extra) 
		   ;:flatp t
		   ))
	  filtered
	  ;; By default, cl-json turns camelcase into dashes:  
	  ;; Instead, we are case insensitive, preserving dashes.
	  (*json-identifier-name-to-lisp* #'string-upcase))

      ;; Filter out turns where the reply contains a timeout error.
      ;; Unless the bug causing the timeout has been fixed, these errors
      ;; prevent a student from reopening a problem.
      (let ((last (car result)))
	(dolist (x (cdr result))
	  ;; find client turn such that any following server
	  ;; turn does not have a timeout error.
	  (when (equal (car last) "client")
	    (unless (and (equal (car x) "server")
			 (second x)
			 (server-reply-has-timeout
			  ;; Actually, we only need to decode the 
			  ;; top-level list.
			  ;; Sometimes result gets truncated on very long
			  ;; backtraces.  It might be better to just search 
			  ;; the string for the timeout message?
			  (errors-to-warnings (second x)
			    (decode-json-from-string (second x)))))
	      (push (second last) filtered)))
	  (setf last x)))
      (setf filtered (reverse filtered))

      ;; pick out the solution-set and get-help methods
      (remove-if #'(lambda (x) (not (member (cdr (assoc :method x))
					    methods
					    :test #'equal)))
		 ;; parse json in each member of result
		 (mapcar 
		  ;; A post with no json sent gets translated into nil;
		  ;; see write-transaction.
		  #'(lambda (x) (and x (errors-to-warnings x
					 (decode-json-from-string x))))
		  filtered)))))

(defun server-reply-has-timeout (reply)
  "Test whether a server reply includes a timeout error."
  (some #'(lambda (x) (and (string-equal (cdr (assoc :action x)) "log")
			   (string-equal (cdr (assoc :error-type x))
					 "timeout")))
	(cdr (assoc :result reply))))

(defun first-session-p (&key student section extra)
  "Determine student has solved any problem previously."
  (unless (or *skip-db* (> (length extra) 0)) ;can be empty string
    (with-db
      (> 2 (parse-integer 
	    (car (car (query *connection*
			     (format nil "SELECT count(*) FROM PROBLEM_ATTEMPT WHERE userName = '~A' AND userSection='~A'" 
				     student section) 
			     ))))))))
