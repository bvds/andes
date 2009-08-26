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
  (:use :cl :clsql)
  (:export :write-transaction :destroy :create :set-session))

(in-package :andes-database)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;         Send to database
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun destroy ()
  (disconnect))

(defun create ()
  
  ;; should test if it exists and create (using the below clos) if
  ;; it does not.
  
  (unless (probe-database '(nil "andes" "root" "sin(0)=0") :database-type :mysql)
    ;; create database
    create-database '("localhost" "andes" "root" "sin(0)=0") :database-type :mysql)

  ;;     (connect '(nil "andes" "root" "sin(0)=0") :database-type :mysql)
  ;; in mysql:  describe class_information;
  (def-view-class class_information ()
    ((classID
      :db-kind :key
      :db-constraints (:not-null :auto-increment)
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
  (def-view-class student_dataset ()
    ((datasetID
      :db-kind :key
      :db-constraints (:not-null :auto-increment)
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
    (def-view-class problem_attempt ()
      ((clientID
	:db-kind :key
	:db-constraints (:not-null)
	:type (varchar 50)
	:initarg :clientID)
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
  (def-view-class problem_attempt_transaction ()
    ((tID
      :db-kind :key
      :db-constraints (:not-null :auto-increment)
      :type integer
      :initarg :tID)
     (clientID
      :db-kind :key
      :db-constraints (:not-null)
      :type (varchar 50)
      :initarg :clientID)
     (problem_attempt
      :accessor transaction_problem
      :db-kind join
      :db-info (:join-class problem_attempt
                            :home-key clientID
                            :foreign-key clientID
                            :set nil)) 
     (command
      :accessor command
      :type (varchar 2000)
      :initarg :command)
     (initiatingParty
      :accessor initiatingParty
      :type (varchar 30)  ;is enum in database
      :initarg :initiatingParty)))

    (setf *db-auto-sync* t)

 )

(defun write-transaction (direction client-id j-string)
  ;; select from problem_attempt where client-id is input client-id
  ;; find or create
  ;; check with Brett to see if LISP makes sense
   (let (queryString (concatenate 'string "SELECT clientID FROM PROBLEM_ATTEMPT WHERE clientID =" client-id))) 
   (let (checkInDatabase (query queryString :field-names nil :flatp t :result-types :auto  )))
   (cond 
        ((nil query) make-instance 'problem_attempt_transaction :command j-string :initiatingParty direction)
  j-string)
	 (make-instance 'problem_attempt_transaction :client-id client-id :command j-string :initiatingParty direction))
)

(defun set-session (client-id &key student problem section)
  ;;  session is labeled by client-id

  ;; add above info to database
  
  ;; section is a string that is expected to be used to get the 
  ;; classinformation values from web assign, including
  ;; the class name, school, period, description, instructorName, and 
  ;; school year info. This information is used by the DataShop XML format, 
  ;; so it should be retrieved from somewhere. For now, we will use a dummy
  ;; class information created specifically for testing -- 08-19-2009 NV
  (let ((testClassInformation
	; (select 'classInfo :where [= [slot-value 'class_information 'classID] 1 ])

;	 (query "SELECT classID FROM CLASS_INFORMATION WHERE classID=1" 
;		:field-names nil :flatp t :result-types :auto)
)
	(currentTime
;	 (query "SELECT NOW();" :field-names nil :flatp t :result-types :date)
))
;    (make-instance 'problem_attempt :userName student :sessionID client-id 
;		   :startTime (car currentTime)
;		   :classInformationID (car testClassInformation))
))
