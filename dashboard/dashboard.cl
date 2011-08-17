;;; Copyright 2011 by ...
;;; This file is part of the Andes Intelligent Tutor Stystem.
;;;
;;; The Andes Intelligent Tutor System is free software: you can redistribute
;;; it and/or modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation, either version 3
;;; of the License, or (at your option) any later version.
;;;
;;; The Andes Solver is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with the Andes Intelligent Tutor System. If not, see
;;; <http:;;;www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage :dashboard
  (:use :cl :cl-user :json :mysql-connect)
  (:export :destroy-connection :create-connection :create-local-connection
	   :dummy-process-api-request :process-api-request
	   :start :stop
	   ))

(in-package :dashboard)

(defun start (&key (port 8080) host db user password)
  (webserver:start-json-rpc-services '(("/dashboard" :json-rpc t))
				     :port 8080
				     :server-log-path
				     (merge-pathnames "dashboard-server.log" 
						      cl-user::*andes-path*))
  
  (create-connection :user user :password password :db db :host host)
  nil)

(defun stop ()
  (webserver:stop-json-rpc-services)
  ;; Stop database.
  (destroy-connection))

(webserver:defun-method "/dashboard" dashboard 
  (&key version (model () model-p)
	section (student () student-p)
	(assignment () assignment-p))
  (apply #'process-api-request 
	 (append (list :version version :section section)
		 (if model-p (list :model model))
		 (if assignment-p (list :assignment assignment))
		 (if student-p (list :student student)))))

(defvar *connection* nil "connection to db")

(defmacro with-db (&body body)
  "Excecute body with db mutex."
  `(if *connection*
       (sb-thread:with-mutex (*db-lock*) ,@body)
       (error "No common database defined, can't continue.")))

(defun read-login-file (&optional path)
  "Read the database login file; file contains user name, password & (optional) database name."
  (with-open-file (f (or path (merge-pathnames "db_user_password"
                                               cl-user::*andes-path*))
                     :if-does-not-exist nil)
    (when f (values (read-line f) (read-line f) (read-line f nil)))))

;;
(defun create-connection (&key user password db host)
  
  (multiple-value-bind (u p d) (read-login-file)
    (setf user (or user u "root"))
    (setf password (or password p (error "No database password given.")))
    (setf db (or db d "andes3")))
  
  (setf *connection*
(mysql-connect:connect :host host :user user :password password :database db)))

(defun create-local-connection (&key user password db host)
  
  (multiple-value-bind (u p d) (read-login-file "/home/benefluence/Desktop/database_stuff")
    (setf user (or user u "root"))
    (setf password (or password p (error "No database password given.")))
    (setf db (or db d "andes3")))
  
  (setf *connection*
(mysql-connect:connect :host host :user user :password password :database db)))


(defun destroy-connection ()
  (disconnect *connection*))




(defun formatted-time ()
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~A-~A-~AT~A:~A" year month date hour minute)))

(defun dummy-process-api-request (&key
				  (version 1)
				  (model "capstone")
				  (section "andesTutor")
				  (student () supply-student-p)
				  (assignment () supply-assignment-p))
  '(((:this . is) (:a . test))))

(defun process-api-request (&key
			    (version 1)
			    (model "capstone")
			    (section "andesTutor")
			    (student () supply-student-p)
			    (assignment () supply-assignment-p))
					;the student id matches and the kcs of the assignment match
					;when one is missing we just pull all of the records
					;because the assignment data is not anywhere in the table,
					;separate assignments are best run as separate queries
					;in the case of a different model we might need to return
					;different kcs, different data about those kcs.
  (let (
	(assignment-list-data
	 (get-kcs-for-assignments section
				  (if supply-assignment-p
				      (list assignment)
				      (get-assignments-for-section section))
				  model))
	(student-ids
	 (if supply-student-p
	     (list (list student))
	     (run-query (concatenate 'string
				     "SELECT DISTINCT userName FROM student_state WHERE userSection = \""
				     section
				     "\"")))))
    (list (cons :api-version version)
	  (cons :timestamp (formatted-time))
	  (cons :sectionid section)
	  (build-student-list section student-ids assignment-list-data model))))

(defun get-assignments-for-section (section)
  (if (equal section "andesTutor") (mapcar #'car (cadr cl-user::*sets*))))

(defun build-student-list (section-id student-ids assignment-list-data model)
  (cons :*student-list
	(loop for student-record in student-ids collect
	     (build-student-data section-id (car student-record) assignment-list-data model))))

(defun build-student-data (section-id student-id assignment-list-data model)
  (list
   (cons :*student-id student-id)
   (build-assignment-list section-id student-id assignment-list-data model)))

(defun build-assignment-list (section-id student-id assignment-list-data model)
  (cons :*assignment-list
	(loop for assignment-data in assignment-list-data collect
	     (build-assignment section-id student-id (first assignment-data) (second assignment-data) model))))

(defun build-assignment (section-id student-id assignment-id assignment-kcs model)
  (list
   (cons :*assignment-id assignment-id)
   (build-kc-list (run-query (kc-query-string section-id student-id assignment-kcs model)))))

(defun build-kc-list (kc-list-data)
  (cons :+kcl+ist
	(loop for kc-data in kc-list-data collect
	     (apply #'build-kc kc-data))))

(defun build-kc (name state)
  (let (kc-info (decode state))
    (append
     (list (cons :*name name)
	   (cons :*short-desc (cl-user::get-operator-short-name (intern (string-upcase name)))); if the kc is not in the andes model
	   (cons :*long-desc (cl-user::get-operator-description (intern (string-upcase name))))); these lines could cause problems
     (decode state))))

(defun kc-query-string (section-id student-id assignment-kcs model)
  (format nil "SELECT property,value FROM student_state WHERE userSection = \"~A\" ~A ~A ~A" ;need to incorporate model checking
	  section-id
	  (concatenate 'string "AND userName = \"" student-id "\"")
					;think about sanitizing inputs
	  (concatenate 'string "AND model = \"" model "\"")
	  (concatenate 'string
		       "AND (property = \"junk-kc\""
		       (format nil "~{ OR property = \"~a\"~}"
			       assignment-kcs)
		       ")")))

(defun get-dummy-kcs-for-assignments (section assignments model)
  (list '("assignment 1" ("kc1" "kc2" "kc3"))))

(defun get-kcs-for-assignments (section assignments model)
					;this function will need to pull the kcs for an assignment,
					;which will probably only be derivable from the problems in
					;the assignment. we should have a section value (nil?) that
					;pulls the data from the andes defined problem sets. what
					;happens when the assignments don't exist? do we return anything?
  (if (equal section "andesTutor"); we need to pull the kcs correctly depending on the section name
      (loop for kc-set in
	 cl-user::*set-kcs* append
	   (if (member (car kc-set) assignments :test #'equal)
	       (list
		(list (car kc-set) (apply #'append (cdr kc-set))))));combines common and uncommon kcs
      ())); right now only one section works

(defun get-kc-names (kc)
  (list kc (cl-user::get-operator-short-name kc) 
	(cl-user::get-operator-description kc)))

(defun encode (x &optional (stream nil supply-stream-p))
  (if supply-stream-p
      (json:encode-json x stream)
      (json:encode-json x)))

(defun encodea (x) (json:encode-json-alist x))

(defun encodep (x) (json:encode-json-plist x))

(defun encode-string (x) (json:encode-json-to-string x))

(defun decode (x) (with-input-from-string (stream x) (json:decode-json stream)))

(defun decode-file () (with-open-file (stream "experiment.js") (json:decode-json stream)))

(defun dummy-student-record ()
  (run-query "SELECT * FROM student_state WHERE uid=1"))

(defun run-query (query-string)
  (mysql-connect:query *connection* query-string))