;; StudentFile.cl -- routines to suport load save and modification of student information
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com> Collin Lynch (cl) <collinl@pitt.edu>
;;  5 August 2001 - (lht) created
;;; Modifications by Anders Weinstein 2001-2008
;;; Modifications by Brett van de Sande, 2005-2008
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  26 November 2003 - (cl) modified for loading so that the studentfile can be loaded
;;    from an explicitly named file as opposed to the standard location and so that the
;;    scores can be stored to and loaded from a single location so that multiple files
;;    can be loaded at the same time.  
;;
;;    Specifying the :Data keyword specifies an alternate location for the incoming data
;;     other than the default **Studentfile** location.
;; NOTES: example usage at end of file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter **StudentFile** nil)
(defparameter **StudentFileName** nil)

(defun StudentFile-name (name &key (Explicit Nil))
  (setf **StudentFileName** 
    (if Explicit Name
      (andes-path (format nil "Log/~A.dat" name)))))

(defun StudentFile-load (name &key (Explicit Nil))
  (StudentFile-name name :Explicit Explicit)
  (setf **StudentFile** nil)
  (if (probe-file **StudentFileName**)
      (with-open-file (aStream **StudentFileName** :direction :input)
	(do ((item (read aStream nil 'eof) (read aStream nil 'eof)))
	    ((eql item 'eof))
	  (setf **StudentFile** (append **StudentFile** (list item))))))
  **StudentFile**)

(defun StudentFile-save (&key (Data **StudentFile**))
  (with-open-file
      (aStream **StudentFileName** 
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create)
    (dolist (association Data)
      (format aStream "~W~%" association))))

(defun StudentFile-ask (key &key (Data **StudentFile**))
  (let ((association (assoc key Data :test #'equal)))
    (if association
	(cdr association))))

(defun StudentFile-addto (key new-value)
  (let ((association (assoc key **StudentFile** :test #'equal)))
    (if association
	(let ((current-value (cdr association)))
	  (if (atom current-value)
	      (setf (cdr association) (list current-value new-value))
	    (setf (cdr association) (append current-value (list new-value)))))
      (setf **StudentFile** (append **StudentFile** (list (cons key new-value))))))
  (StudentFile-Save))

(defun StudentFile-increment (key)
  (let ((association (assoc key **StudentFile** :test #'equal)) (current-value))
    (if (and association (setq current-value (cdr association)))
	(if (numberp current-value)
	    (setf (cdr association) (+ 1 current-value))
	  (error "Attempt to increment non-numeric value."))
      (setf **StudentFile** (append **StudentFile** (list (cons key 1))))))
  (StudentFile-Save))

(defun StudentFile-decrement (key)
  (let ((association (assoc key **StudentFile** :test #'equal)) (current-value))
    (if (and association (setq current-value (cdr association)))
	(if (numberp current-value)
	    (setf (cdr association) (- current-value 1))
	  (error "Attempt to increment non-numeric value."))
      (setf **StudentFile** (append **StudentFile** (list (cons key 1))))))
  (StudentFile-Save))

(defun StudentFile-tell (key value)
  (let ((association (assoc key **StudentFile** :test #'equal)))
    (if association
	(setf (cdr association) value)
      (setf **StudentFile** (append **StudentFile** (list (cons key value))))))
  (StudentFile-Save))

(defun StudentFile-query (key value)
  (let ((association (assoc key **StudentFile** :test #'equal)))
    (if association
	(let ((current-value (cdr association)))
	  (if (atom current-value)
	      (equal current-value value)
	    (member value current-value))))))

(defun StudentFile-remove (key value)
  (let ((association (assoc key **StudentFile** :test #'equal)))
    (if association
	(let ((current-value (cdr association)))
	  (if (atom current-value)
	      (if (equal current-value value)
		  (setf (cdr association) nil))
	    (setf (cdr association) (remove-if #'(lambda (x)
						   (equal x value)) current-value))))))
  (StudentFile-save))

(defun StudentFile-delete (key)
  (setf **StudentFile** (remove-if #'(lambda (x) (equal (car x) key)) **StudentFile**))
  (StudentFile-save))



;;; ----------------------------------------------------------------------------------
;;; Studentfile combination.
;;; Given a set of studentfile ids open each file and then produce a new file with 
;;; the specified name and save it either in the standard location or the specified
;;; location if "Explicit" is supplied.
;;;
;;; By default the system will combine the files by taking the sum of all the elements
;;; within them and handling intersections by taking the value from the last file 
;;; supplied.  
;;;
;;; If a specialized handler is supplied for the type however that will be used for
;;; combination in lieu of the standard process.  This is assumed to be a function
;;; that will take the set of overlapping values and combine them.  
;;; 
;;; This code takes a OutName argument indicating the location to store the results, 
;;; and a list of student.dat files that will be loaded and merged into the new result
;;; file.  If the ExplicitOut flag is t then the OutName is assumed to be an explicit 
;;; file path rather than a username.  If the ExplicitFiles flag is set then the Files
;;; Are assumed to be explicit names rather than standard name.
;;;
;;; If the OutName is explicit and is present in the Files list then the system will not
;;; open it twice.  This will make it possible to merge code in without pre-copying files.

(defun StudentFile-Merge (Out Files &key (ExplicitOut Nil) (ExplicitFiles Nil))
  (let* ((StudentFiles 
	  (mapcar #'(lambda (F) 
		      (list 
		       (studentfile-name F :Explicit ExplicitFiles)
		       (StudentFile-Load F :Explicit ExplicitFiles)))
		  Files))
	 (OutFileName (studentfile-name Out :Explicit ExplicitOut))
	 (OutFile (or (find OutFileName StudentFiles :key #'car :test #'string=)
		      (studentfile-load OutFileName :Explicit t))))
    
    ;;; Recursively iterate over the StudentFiles Merging the
    ;;; Values as necessary.
    (setq OutFile (Studentfile-merge-recurse (mapcar #'cadr StudentFiles) OutFile))
    
    ;;; Save the current studentfile and be done with it.
    (setq **StudentFile** OutFile)
    (studentfile-save :Data OutFile)))


;;; Recursively iterate over the set of studentfiles handling the 
;;; Merge process for each one.  The process is as follows:
;;;  If the set is nil then return.
;;;  If There is only one Dat in the set then add its contents to
;;;   the output and return.
;;;  If the first item is nil then recurse with the cdr.
;;;  Else Pop the first item off the first set.
;;;   If it is not present in any of the others (keyword search) then
;;;    go ahead and add it to the current output file.
;;;   Else Merge the values using the registered merge code and store
;;;    the resulting values.
;;;
;;;  Note:: This code will deal with the result value as a list, it will 
;;;     not make use of the studentfile-tell code in order to avoid the 
;;;     overhead of the constant disk writes.  Instead it will just 
;;;     rely on the studentfile-merge code to perform that at the end.
;;;   
;;;     Any outgoing values will overwrite those that are present in the
;;;     Output if they are already there.  
;;;
;;; Tells are defined as assocs where the car of the assoc is a lisp
;;; expression of some kind and the cdr is a lisp expression of some kind.
(defun studentfile-merge-recurse (StudentFiles Output)
  (if (null StudentFiles) Output
    (let ((Data (car StudentFiles))
	  (Others (cdr StudentFiles)) Matches)
      (dolist (Tell Data)
	(setq Matches (sfm-find-matching-tells Tell Others))
	(if (null Matches) (setq Output (sfm-add-tell Tell Output)) 
	  (setq Output
	    (sfm-add-tell 
	     (sfm-merge-tells (cons Tell Matches))
	     Output))))
      (studentfile-merge-recurse Others Output))))


;;; Given a "Tell" taken from a student.dat file and a set of other alists
;;; taken from different files collect the set of all entries with the same
;;; value as the tell.
;;;
;;; Note that this code is destructive as well in that it also "removes" the
;;; matching tells from the remaining Others.
(defun sfm-find-matching-tells (Tell Others)
  (let (R New)
    (dotimes (N (length Others))
      (setq New (find (car Tell) (nth N Others) :key #'car :test #'equalp))
      (when New
	(push New R)
	(setf (nth N Others) (remove New (nth N Others)))))
    R))
  
;;; Given A Tell and an alist representing an output .dat add the tell to the
;;; list overriding any preexisting tell if it was there.
(defun sfm-add-tell (Tell Lst)
  (let ((other (find (car Tell) Lst :key #'car :test #'equalp)))
    (if (null Other) (cons Tell Lst)
      (cons Tell (remove Other Lst)))))


;;; Merging the individual tells depends upon the content of each tell.
;;; This code will look up the car of the tell in the
;;; **studentfile-tell-mergefuncs** parameter.  This parameter associates
;;; the car of each tell to a merge dfunction that can be used to merge 
;;; the specified files.  
;;;
;;; If no merge function can be found the values will be appended together
;;; for insertion into the new file.  
;;;
;;; As a special case Tell's who are lists (in this case ("minilesson" <name>))
;;; will be matched by car if they don't match directly.

(defparameter **studentfile-tell-mergefuncs**
    `((MINILESSON . ,#'+)
      (SAVED-STATS . ,#'merge-saved-score-sets)))

(defun sfm-merge-tells (Tells)
  (let* ((Type (caar Tells))
	 (Func (or (cdr (assoc Type **Studentfile-Tell-mergefuncs**))
		   (and (listp Type)
			(cdr (assoc (car Type) **Studentfile-Tell-mergefuncs**)))
		   #'append)))
    (cons Type (apply Func (mapcar #'cdr Tells)))))



(defun studentfile-trace-merge ()
  (trace studentfile-merge
	 studentfile-merge-recurse
	 sfm-find-matching-tells
	 sfm-add-tell
	 sfm-add-tells
	 sfm-merge-tells))
	 
	 







#| example usage
(StudentFile-load "Irene") ;; set filename and load if exists
(StudentFile-tell 'age 24) ;; set age variable to 24
  file wil have (age 24)
(StudentFile-tell 'age 34) ;; set age variable to 34 to correct typo
  file wil have (age 34)
(StudentFile-tell 'status 'married) ;; set status as married
  file wil have (age 34) (status married)
(StudentFile-tell 'pet 'cat) ;; set pet to 'cat
  file wil have (age 34) (status married) (pet cat)
(StudentFile-addto 'pet 'dog) ;; add dog to 'pet
  file wil have (age 34) (status married) (pet cat dog)
(StudentFile-addto 'pet 'canary) ;; add canary to 'pet
  file wil have (age 34) (status married) (pet cat dog canary)
(StudentFile-addto 'pet 'hamster) ;; add hamster to 'pet
  file wil have (age 34) (status married) (pet cat dog canary hamster)
(StudentFile-ask 'age) ;; ask for age
  will return 34
(StudentFile-ask 'pet) ;; ask for pet
  will return (cat dog canary hamster)
(StudentFile-remove 'pet 'canary) ;; remove canary from 'pet
  file wil have (age 34) (status married) (pet cat dog hamster)
(StudentFile-delete 'status) ;; remove status completely
  file wil have (age 34) (pet cat dog hamster)
(StudentFile-query 'pet 'cat)
  will return non-nil
(StudentFile-query 'pet 'canary)
  will return nil
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of StudentFile.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
