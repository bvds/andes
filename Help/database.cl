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
    (format t "Creating database~%")
    (create-database '("localhost" "andes" "root" "sin(0)=0") :database-type :mysql))


  (connect '(nil "andes" "root" "sin(0)=0") :database-type :mysql)
  
  (format t "Connected databases after ~A~%" (connected-databases))
 )

(defun write-transaction (direction client-id j-string)
  "Record raw transaction in database."
  (let* 
      ((queryString (format nil "SELECT clientID FROM PROBLEM_ATTEMPT WHERE clientID = '~A'" client-id))
       (checkInDatabase (query queryString :field-names nil :flatp t :result-types :auto)))

    (unless checkInDatabase 
      (execute-command (format nil "INSERT into PROBLEM_ATTEMPT (clientID,classinformationID) values ('~A',2)" client-id)))
    
    (setf queryString (format nil "INSERT into PROBLEM_ATTEMPT_TRANSACTION (clientID, Command, initiatingParty) values ('~A','~A','~A')" 
			      client-id j-string direction))
    (execute-command queryString)
	    
    )
)

(defun set-session (client-id &key student problem section)
  "Updates transaction with session information."
  ;;  session is labeled by client-id
  
  ;; add above info to database
  
  ;; update the problem attempt in the db with the requested parameters
  (setf queryString (format nil "UPDATE PROBLEM_ATTEMPT SET userName='~A', userproblem='~A', userSection='~A' WHERE clientID='~A'" 
			      student problem section client-id))
    (execute-command queryString)
)
