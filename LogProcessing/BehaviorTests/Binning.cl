#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database Binning code.
;; Collin Lynch
;; 5/27/2003
;;
;; When the files are processed the resulting values will be stored 
;; in the database by problem-instance.  The code in this file is 
;; used to group those problem-instance results into file results or 
;; other values.  
;;
;; Each of the sections below defines a different binning scheme and 
;; executes it as needed.  
|#

;;;; ======================================================================
;;;; Load Support files.
;;(load "./Base/Loader.cl")



;;;; ====================================================================
;;;; Database Bin Results
;;;; Database binning involves selecting from the database all files where
;;;; a specified condition is met into a set of temporary tables.  These
;;;; tables can then be used to perform other later processing tasks.
;;;;
;;;; When called this code will generate a binned files table based upon
;;;; the current files table and then select files into it from the initial
;;;; files table based upon their entry counts.  
;;;;
;;;; The Threshold is an integer for entries that must be entered by the 
;;;; user.  The New-TableName-Format is a format string that will be used
;;;; to form the new tablename.  Any supplied format string will be fed 
;;;; a single argument listing the Threshold used.  Verbose is sent as a 
;;;; signal to the table creation code.
(defun bin-files-by-entries (Connection 
			     &key (Threshold Nil) (new-tablename-format "binned_files_~d") 
				  (Verbose nil)) 
  (let* ((Schema (db-get-table-schema Connection "files"))
	 (Columns (mapcar #'(lambda (CS) (db-column-name CS)) (db-table-columns Schema)))
	 (NewName (format Nil New-TableName-Format Threshold))
	 (NewSchema (db-make-table-schema Connection NewName 
					  :Columns (db-table-columns Schema)
					  :Indicies (db-table-indicies Schema)))) 
    
    ;; Then copy over the data.
    (db-format-insert-select 
     Connection
     NewName
     :Columns Columns
     :Select (db-format-select 
	      Connection
	      :Cols Columns
	      :Tables '("files" "command_tests")
	      :Where (db-format-gt-exp Connection "ENTRY_COMMANDP" Threshold)))))


;;; Once the 






;;;; ====================================================================
;;;; File Bin Results
;;;; The raw results are stored in problem-instances.  Each record in the 
;;;; database records the student's behavior for a specific problem instance
;;;; within a specific file.  The code in this section combines those results
;;;; into unified file results storing them in file tables by testset.
;;;; The new testset tables will be of the form:
;;;;
;;;;    (<FileID> <NumInsts> . <Results>)
;;;;
;;;; The new table names will have the same names as their sources with a 
;;;; prefix (the defualt is "file_") attatched.
;;;;
;;;; The code operates by iterating over the list of testsets in the database
;;;; for each teset it then generates a new output file for the results and 
;;;; iterates over th list of files in the database.  For each file it 
;;;; collects and merges all the results for that file from the speciofied 
;;;; table and stores the results in the ouput table.
;;;;
;;;; Before iterating over the code it will collect the list of problems
;;;; And then begin iterating over the testsets.  For each testset it will
;;;; select the problem instances from the set ordered by the instance num
;;;; and combine them using the ResultCombiner function defined in the 
;;;; testset.
;;;;
;;;; Arguments:
;;;;  Tests:  The testsets to be processed.
;;;;  DBName: The database to be processed.
;;;;  ODBCName:  The odbc connection name to be used.
;;;;  &key
;;;;   Prefix:  The prefix to be appended to the output table names
;;;;            The default value is "File_"
;;;;   Verbose: If t the odbc sql will be echoed to the user.

(defun footest ()
  (time (file-bin-results **test-sets** "study_usna2000_2" "MySQL-odbc" :Prefix "rar_" :MaxN 5)))

(defun file-bin-results (Tests DBName ODBCName &key (Prefix "file_") (Verbose Nil) (MaxN Nil))
  "File bin the result values."
  (declare (inline fbr-handle-test))
  (let (NumFiles Connection) 
    (format t "~,,30,'=A~%  Opening Connection ~a ..." #\; ODBCName)
    (setq connection (odbc-connect ODBCName :Verbose Verbose))
    (format t "Done~%~,,30,'=a~%  Collecting NumFiles ..." #\;)
    (setq NumFiles (+ 1 (fbr-fetch-max-fileid DBName Connection :Verbose Verbose)))
    (format t "~a~%" NumFiles)
    (dolist (Test Tests)
      (fbr-handle-test NumFiles Test DBName Connection Prefix :Verbose Verbose :MaxN MaxN))))


;;; Given a testset, a DBName, a connection, a prefix and the verbosity flag
;;; bin the results for the testset by the file.  Generate a new table to hold
;;; the results and then iterate over the testset source table selecting the
;;; file results by fileid, combining those results using the testset-resultcombiner
;;; function in the testset.
(defun fbr-handle-test (NumFiles Test DBName Connection Prefix &key (Verbose Nil) (MaxN Nil))
  "File bin the results for the specified test."
  (declare (inline fbr-select-results testset-combine-resultsets))
  (format t "~,,30,'=a~%  Starting test ~a ..." #\- (testset-name Test))
  (let ((TableName (concatenate 'string Prefix (map-testset->tablename Test)))
	(ColNames (append '("FileID" "NumInstances") (testset-tests Test))) R)
    (fbr-generate-output-table TableName Test DBName Connection :Verbose Verbose)
    (dotimes (N NumFiles)
      (pprint N)
      (setq R (fbr-select-results N Test DBName Connection :Verbose Verbose))
      (when R
	(odbc-insert 
	 TableName DBName Connection
	 :Columns ColNames
	 :Record (append (list N (length R)) (testset-combine-resultsets Test R))))
      (if (and MaxN (= N MaxN)) (return))))
  (format t "Done ~%"))


;;; fetch the highest fileifd number in the files table by selecting for it
;;; with the rows reversed.  
(defun fbr-fetch-max-fileid (DBName Connection &key (Verbose Nil))
  (let ((ColID (odbc-format-id :DB DBName :Tbl "files" :Col "id")))
    (caar (odbc-select 
	   ColID (odbc-format-id :DB DBName :Tbl "files")
	   Connection
	   :Order-By (list ColID :DESC)
	   :Limit 1
	   :Types '(:INT)
	   :Verbose Verbose))))


;;; Generate an output table for the testset with the inital name now prefixed by the 
;;; supplied prefix parameter.  The table will be preceeded by two columns, a FileID
;;; and the NumInst count.
(defparameter **FBR-Static-Cols**
    (list (odbc-format-columnspec "FileID" "char(64)" :not-null t :Primary-Key t)
	  (odbc-format-columnspec "NumInstances" "INTEGER")))

(defun fbr-generate-output-table (TableName TestSet DBName Connection &key (Verbose Nil))
  (odbc-create-table
   TableName DBName Connection
   (append **FBR-Static-Cols** (map-testset->Colspecs TestSet))
   :Verbose Verbose))


;;; Given an ID integer, a testset, A DBName  and a connection as well as 
;;; the verbosity flag select all of the records for the specified file 
;;; from the testset table in the database and return them as a qresult.
;;; The results will be returned as a Qresult.
(defun fbr-select-results (Num Test DBName Connection &key (Verbose Nil))
  (let ((Tablename (map-testset->tablename Test)))
    (odbc-select
     (odbc-format-colids DBName TableName (testset-tests Test))
     (odbc-format-id :DB DBName :Tbl TableName)
     Connection
     :Where (odbc-format-rel 
	     "=" (odbc-format-id :DB DBName :Tbl TableName :Col "FileID") 
	     (odbc-format-val Num))
     :order-by (odbc-format-id :DB DBName :Tbl TableName :Col "InstNum")
     :Types (map-testset->exttypes-list Test)
     :Verbose Verbose)))


;;(defun fbr-time-single-call (FileID Test DBName TableName Connection)
;;  (let ((R (fbr-select-results FileID Test DBName Connection)))
;;    (when R
;;      (odbc-insert
;;       TableName DBName Connection
;;       :Columns 


#|;;;; =================================================================================
;;;; File Combination.
;;;; This code is designed to combine the problem-instance results that are produced 
;;;; by the hdmain code into a series of per-file results.  These per-file tables 
;;;; are intended to mirror the results that were obtained under the original 
;;;; Version of the HDmain where values were stored on a per-file basis.  
;;;;
;;;; Loosely, this code will take a set of testsets and then iterate over the files
;;;; table.  For each fileid it will load all of the problem instances for that file
;;;; and combine them in order by instance id.  The combination functions will be 
;;;; taken from the testsets.  
;;;; 
;;;; The resulting data will be stored in tables of same names as the testsets 
;;;; this time prefixed with "file_".  The results will be of the form:
;;;;   (<FileID> . <Results>)

(defun File-Bin-results (Tests DBName ODBCName &key (Prefix "File_") (Verbose Nil))
  "Bin the results by file in the sysytem."
  (format t "=================================================~%")
  (format t "Beginning File Bin.~%")
  (let* ((Connection (odbc-connect ODBCName :Verbose Verbose))
	 (Tables (fbr-generate-tables Tests Prefix DBName Connection :Verbose Verbose))
	 (FileIDs (fbr-get-fileIDs DBName Connection)))
    (format t "~a~%" FileIDs)
    (fbr-lock-tables Tables Tests DBName Connection :Verbose Verbose)
    (file-bin-results-i Connection Tables FileIDs Tests DBName Prefix :Verbose Verbose)
    (odbc-unlock-tables Connection :Verbose Verbose)
    (odbc-disconnect Connection :Verbose Verbose)))



;;; Table generation.
;;; Given a list of tests and a predicate as well as database 
;;; info generate the set of tables necessary for the file binning.  
;;; This code will generate a new table per-test-set of the form:
;;;  (<FileID> . <Tests>)
;;;
;;; When done the function will return a list of the table names.
(defparameter **FBR-Table-static-Col**
    (odbc-format-columnspec "FileID" "INTEGER" :not-null t :primary-key t))

(defun fbr-generate-tables (Tests Prefix DBName Connection &key (Verbose Nil))
  (let (Name Names)
    (dolist (Test Tests)
      (setq Name (string-downcase (format Nil "~a~a" Prefix (testset-name Test))))
      (push Name Names)
      (odbc-create-table
       Name DBName Connection
       (cons **FBR-Table-Static-Col**
	     (odbc-format-list-columnspecs
	      (Testset-tests Test) (Testset-resulttype Test)))
       :Verbose Verbose))
    (reverse Names)))


;;; Given a DBName and a connection extract the complete list of fileids
;;; from the database as a list that can be iterated over.
(defun fbr-get-fileids (DBName Connection)
  (odbc-select 
   '("ID") (list (odbc-format-id :Db DBName :Tbl "files"))
   Connection
   :Types '(:Int)
   :Qresult Nil
   :Verbose t))



;;; Given a list of table names and testsets lock the tables for reading and
;;; writing and lock the tables specified by the testsets, and the files table
;;; for reading.
(defun fbr-lock-tables (Tables Tests DBName Connection &key (Verbose Nil))
  (odbc-lock-tables 
   (odbc-format
   
   (append (list (odbc-format-lock-spec "files" DBName :Read))
	   (mapcar #'(lambda (N) (odbc-format-lock-spec N DBName :Write)) Tables)
	   (mapcar #'(lambda (Ts) 
		       (odbc-format-lock-spec 
			(string-downcase (format Nil "~a" (testset-name Ts))) 
			DBName :Read)) Tests))
   Connection 
   :Verbose Verbose))





;;; Carry out the main inner loop for the results file binning.
(defun file-bin-results-i (Connection Tables FileIDs Tests DBName Prefix &key (Verbose Nil))
  "The inner loop of the file-bin-results process."
  (let (Table Test)
    (dotimes (N (length Tables))
      (setq Table (nth N Tables))
      (setq Test (nth N Tests))
      (format t "### Starting ~a ~a ###~%" Table (testset-name Test))
      (dolist (FileID FileIDs)
	(fbr-store-results
	 Table DBName Connection Test (car FileID)
	 (testset-combine-resultsets
	  Test 
	  (fbr-get-results 
	   (car FileID) Test DBName Connection :Verbose Verbose))
	 :Verbose Verbose)))))




;;; Given a FileID, a Test set, a DBName and a connection struct extract 
;;; all of the results that match the fileid from the table corresponding
;;; to the testset.  This code will pull only those result line that 
;;; correspond to the tests from the problem-instance tables.  The results
;;; will be sorted in the order that they arose in the original problem file.
(defun fbr-get-results (FileID TestSet DBName Connection &key (Verbose Nil))
  (odbc-select 
   (map-testset->colids TestSet DBName)
   (list (map-testset->tableid TestSet DBName))
   Connection
   :Where (odbc-format-infix-wc 
	   "=" (map-testset->colid "FileID" TestSet DBName)
	   (odbc-format-val FileID))
   :Order-By (odbc-format-id :DB DBName :Tbl (testset-name TestSet) :Col "InstNum")
   :Types (map-testset->typeslist TestSet)
   :Qresult Nil
   :Verbose Verbose))
				

;;; Given a set of results store those result values in 
;;; the supplied fileresult table with the specified fileid.  
;;;
;;; Some of the files will not generate results.  In that case this 
;;; code will generate an empty result using the functions in the
;;; TestSet.

(defun fbr-store-results (TableName DBName Connection TestSet FileID Results &key (Verbose Nil))
  (odbc-insert 
   TableName DBName Connection
   (cons "FileID" (mapcar #'(lambda (Ts) (format Nil "~a" Ts)) (testset-tests TestSet)))
   (cons FileID (if Results Results (testset-make-empty-results TestSet))) 
   :Verbose Verbose))
|#





#|;;;; ===================================================================
;;;; General binning functions
;;;; The code in this section is used by the functions below to support
;;;; the binning process.  These functions should be used to generate the
;;;; tables, collect the values or carry out other specific operations.

;;; --------------------------------------------------------------------
;;; bin-generate-table
;;; Given a tablename, a testset, a dbname, a connection, and a set of
;;; additional columns generate a database containing the excplicit
;;; columns and columns for each of the tests in the testset with the 
;;; specified types.

(defun bin-generate-table (Name Columns TestSet DBName Connection 
			   &key (If-Not-Exists Nil) (Verbose Verbose))
  "Generate the specified tables."
  (odbc-create-table 
   Name DBName Connection
   (append Columns (when TestSet (map-testset->colspecs TestSet)))
   :If-Not-Exists If-Not-Exists
   :Verbose verbose))
|#




#|;;;; ===================================================================
;;;; Bin By file
;;;; Given a prefix, a set of testsets, a database name, and a connection
;;;; iterate over the tables in the database grouping the problem-instance
;;;; results by file and storing the results into new tables using the 
;;;; testset name and the supplied prefixes.  

(defparameter **UBT-Static-Cols**
    (list (odbc-format-columnspec "Username" "CHAR(64)" :not-Null t :primary-key t)
	  (odbc-format-columnspec "numfiles" "INTEGER" :not-null t)))


(defun file-bin-results (Tests DBName Connection
			 &key (Prefix "file_") (Verbose Nil))
  "Bin the results by file."
  (let (TableName NewTable Columns)
    (dolist (Test Tests)
      (setq TableName (map-testset->tablename Test))
      (setq NewTable (concatenate 'string prefix TableName))
      ;;(setq ColNames (testset-tests Test))
      ;;(if NumFiles (push "numfiles" ColNames))
      (bin-generate-table 
       NewTable **UBT-Static-Cols** Test DBName Connection :Verbose Verbose)
      
      
      (odbc-insert 
       NewTable DBName Connection
       :Select (odbc-format-select

		)))))


(defun file-bin-test (DBName testset)
  (let ((tablename (map-testset->tablename Testset))
	(FileColID (odbc-format-id :Db DBName :Table TableName :Col "FileID"))) 
  (odbc-format-select 
   (append (list FileColID (odbc-format-func "count" "*"))
	   (odbc-format-colids DBName (testset-tests Testset)))
   (list (odbc-format-id :DB DBName :Table TableName))
   
   :Group-By FileColID
   :Order-By 
   
   
   
   
   ;; Start Here.
|#


;;;;   
   

   
   
#|;;;; ===================================================================
;;;; Bin By user.  
;;;; Given a prefix, a set of testsets, a database name, and a connection
;;;; iterate over the tables grouping the specified values by user and 
;;;; storing the results into new tables using the testset name with the
;;;; specified prefixes.  
;;;;
;;;; Begin by generating the specified table 
;;;;
;;;; NOTE:: This code necessitates that the database have a files table
;;;;  within it for grouping. 

(defparameter **UBT-Static-Cols**
    (list (odbc-format-columnspec "Username" "CHAR(64)" :not-Null t :primary-key t)
	  (odbc-format-columnspec "numfiles" "INTEGER" :not-null t)))


(defun user-bin-tables (Tests DBName Connection
			   &key (prefix "user_") (Verbose Nil))
  "Bin the specified tables by user."
  (let (TableName NewTable Columns)
    (dolist (Test Tests)
      (setq TableName (map-testset->tablename Test))
      (setq NewTable (concatenate 'string prefix TableName))
      ;;(setq ColNames (testset-tests Test))
      ;;(if NumFiles (push "numfiles" ColNames))
      (bin-generate-table 
       NewTable **UBT-Static-Cols** Test DBName Connection :Verbose Verbose)
      (odbc-insert 
       NewTable DBName Connection
       

      
;;      (uibt-generate-table NewTable Test NumFiles DBName Connection :Verbose Verbose))))


       |#
       
;;; Given a tablename a testset a numfiles flag, the dbname and 
;;; a connection generate a set of columnspecs for a table and
;;; generate the table in the database.  If NumFlags is t then
;;; the table will 



      
      







;;;; ===================================================================
;;;; Static file bins.
;;;; Given a list of dates, a database, and a testset, sort the values in
;;;; the specified table into a series of static bins and store those results
;;;; in a new bin table.  
;;;;
;;;; This code generates a new summary table and stores the listed results
;;;; into that table.  
;;;; 



;;;; -----------------------------------------------------------------
;;;; bin











;;;; -----------------------------------------------------------------
;;;; Select-bin
;;;; Given a database name, and a testset, a set of tables to group by,
;;;; as well as a valid where clause that can be used for constraints
;;;; generate a sql select statement from the specified table with the 
;;;; specfied constrains and group-by settings.
;;;;
;;;; Note, this 




;;(defun Select-bin (Tables Columns GroupBy &key (Where Nil) (NumFiles Nil))
;;  (odbc-format-select
;;   Columns Tables 
   

    
    
;;;; -----------------------------------------------------------------
;;;; bin table
;;;; Given a table name A set of fields to select, a set of constraints
;;;; to group by and an optional set of where constraints select from 
;;;; the specified table according to the constraints.  
;;;;
;;;; NOTE:: The field names are assumed to be the same in the initial
;;;;  table as in the new table.  

;;(defun bin-table (TableName FieldNames




;;;; -----------------------------------------------------------------
;;;; Sum Student Work.
;;;; Given a database, and a set of testsets, generate tables that
;;;; encapsulate all of a student's work.  This is done by generating a
;;;; select statement that uses a group-by to extract the work by student
;;;; from the table and then storing the results back into a new table 
;;;; containing the same set of columns.  
;;;;
;;;; This necessitates modifications to the odbc code that will be done now.



;;;; -----------------------------------------------------------------
;;;; Static bins
;;;; Given a list of bin dates sort the result values into a series
;;;; of summary results and store those values into separate tables.
;;;;
;;;; This section takes in a set of key dates, a database, and a list of
;;;; testsets.  The code will be used to generate a set of tables 
;;;; containing the binned results and a summary set that sums all of the
;;;; retults into 


#|;;;; ----------------------------------------------------------------
;;;; Static bins.
;;;; Given a list of bin dates sort the result values into a series
;;;; of summary results and store those values into separate tables.
;;;; The code in this section will take a list of key dates, a database
;;;; and a list of test-sets and will sort the results into a series 
;;;; of groups combining those results into summary sets.
;;;;
;;;; This code will store the summary results into the database into 
;;;; separate sum tables along with a list of the bins as defined 
;;;; by the ids.  
;;;;
;;;; The bins table will be of the form:
;;;;  (<ID> <StartDate> <EndDate>)
;;;;
;;;; The new bin result tables will be of the form:
;;;;  (<ID> <UserID> <BinID> <NumFiles> . <Results>)
;;;;
;;;; The files are sorted into bons by the dates s.t. for each key
;;;; date N there is a group defined as all files where the date is
;;;; < keydate N and > bin N +1 with the caveat that bin 0 contains 
;;;; all files F s.t. 0000-00-00 < date(F) < keyDate(0).  And that 
;;;; the final group is defined as all files F s.t. 
;;;;   keyDate(Final) < date(F)
;;;;
;;;; Arguments:
;;;;  BinName: A string name that will be used as part of the bin 
;;;;      tables' names.  
;;;;  DBName:  The database to be binned.
;;;;  ODBCName:  The name of the ODBC connection to use.
;;;;  BinDates:  A list of dates of the form (<MM> <DD> <YYYY>) for
;;;;             use as bins.
;;;;  Tests:  The TestSets to be used.
;;;;  &key
;;;;   IF-Exists: One of :Error or :Overwrite which will 
;;;;              occur if the bin tables already exist
;;;;   Verbose:  If then the sysytem will echo progress to the user.  

;;;; 1. Begin by generating the list of table names.
;;;; 2. Test the database for errors such as preexisting tables.
;;;; 3. Generate the bins table.
;;;; 4. Generate the Dynamic tables.
;;;; 5. Collect the Sets of users and tests.
;;;; 6. For Each User:
;;;; 7.  For Each TestSet
;;;; 8.   Collect the resultlists for the testset
;;;; 9.   Sum them and store the results.

(defun static-bin-db (BinName BinDates Tests DBName ODBCName
 		      &key (IF-Exists :Error) (Verbose Nil))
  "Bin the files according to static dates."
  (let ((TableNames (sbdb-make-table-names BinName Tests))
	(Bins (sbdb-compile-bin-dates BinDates))
	(Connection (odbc-connect ODBCName :Verbose Verbose)))
    (sbdb-test TableNames Tests DbName Connection IF-Exists :Verbose Verbose)
    (sbdb-generate-bins-table (car TableNames) Bins DBName Connection :Verbose Verbose)
    (sbdb-generate-dynamic-tables 
     (cdr TableNames) Tests Bins DBName Connection :Verbose Verbose)
    (odbc-disconnect Connection :Verbose Verbose)))


;;; Given a Bin-name and a list of tests generate the table names
;;; that will be used for binning.  These names will be used multiple
;;; times and it is therefore useful to produce them once.  
(defun sbdb-make-table-names (BinName Tests)
  "Produce a set of tablenames from the defs."
  (cons (format Nil "~a" BinName)
	(mapcar #'(lambda (Test) (format Nil "~a_~a" BinName (TestSet-Name Test)))
		Tests)))


;;; The bins are supplied as a list of keys that would be used to segment
;;; all the individual files into separate groups.  Rather than recalculate
;;; the start and end dates for each bin, the system will compile the list 
;;; of dates into a list of the form:
;;;  ((<Start0> <End0>) ... (<StartN> <EndN>))  
;;;
;;; Where the first group starts with 0000-00-00 and the end group with 9999-12-31
;;; Each <StartM> is equal to <End(M-1)> + 1 day.
(defun sbdb-compile-bin-dates (Bins)
  (let (D1 D2 
	(R (list (list (odbc-format-date 0 0 0)
		       (apply #'odbc-format-date (car Bins))))))
    (dotimes (N (length Bins))
      (setq D1 (nth N Bins))
      (incf (nth 1 D1))
      (setq D1 (apply #'odbc-format-date D1))
      (setq D2 (apply #'odbc-format-date (or (nth (+ 1 N) Bins) '(12 31 9999))))
      (setq R (cons (list D1 D2) R)))
    (reverse R)))


	    
;;; -------------------------------------------------------------------
;;; Prep
;;; Given the list of tablenames, tests, Database name, Connection
;;; and the If-Exists policy test to ensure that the database exists
;;; that it contains preexisting file, result, and user tables and
;;; that a bintable of the specified name does not already exist or
;;; if it does overwrite it.  
    
(defun sbdb-test (TableNames Tests DBName Connection IF-Exists &key (Verbose nil)) 
  (sbdb-error-test DBName Connection Tests :Verbose Verbose)
  (case IF-Exists
    (:Error (sbdb-IFE-error TableNames DBName Connection :Verbose Verbose))
    (:Overwrite (sbdb-IFE-overwrite TableNames DBName Connection :Verbose Verbose))))
  

;;; Given a database name and a connection test:
;;; 1. Does the DB Exist?
;;; 2. Does the DB contain a files table?
;;; 3. Does it contain tables for the tests?
(defun sbdb-error-test (DBName Connection Tests &key (Verbose Nil))
  (format t "SBDB:: Testing for standard errors.~%")
  (when (not (odbc-db-exists? DBName Connection :Verbose Verbose))
    (error "SBDB:: Specified database ~a does not exist!" DBName))
  (when (not (odbc-table-exists? "files" DBName Connection :Verbose Verbose))
    (error "SBDB:: Specified database does not have a files table!" DBName))
  (dolist (TestSet Tests)
    (when (not (odbc-table-exists? 
		(string-downcase (format Nil "~a" (TestSet-Name TestSet)))
		DBName Connection :Verbose Verbose))
      (error "SBDB:: Specified TestSet ~a does not have a table." 
	     (testset-name TestSet)))))


;;; If the user selected :Error then test to see if any of the binning
;;; tables already exist.  If they do then throw an error.
(defun sbdb-ife-error (TableNames DBName Connection &key (Verbose Nil))
  "Test to see if there is an if-exists error."
  (format Nil "Testing IF-Exists :Error~%")
  (dolist (Name TableNames)
    (when (odbc-table-exists? Name DBName Connection :Verbose Verbose)
      (error "Specified Table ~a already exists!" Name))))


;;; If the student has selected overwrite then go through and drop all
;;; of the tables using the IF-Exists keyword in order to ensure that 
;;; they are not present.
(defun sbdb-ife-overwrite (TableNames DBName Connection &key (Verbose Nil))
  (format Nil "Testing IF-Exists :Overwrite~%")
  (dolist (Name TableNames)
    (odbc-drop-table Name DBName Connection :IF-Exists t :Verbose Verbose)))



;;; ------------------------------------------------------------------------
;;; Bins Table
;;; The static bins table is used to keep a recoprd of the dates for each bin
;;; for later use.  It is constructed and loaded here and then this code will
;;; ignore it until it becomes necessary at a later date.
;;;
;;; Given an ID number and a pair of dates as well as the connection information
;;; store the Static bin information into the table for future use.  If a Nil is
;;; supplied as the start date then the sysytem will store the date 0000-00-00.
;;; If Nil is supplied as the future date then the system will store 9999-12-31.
;;;
;;; For speed purposes this code puts a write lock on the table before adding in
;;; the data and then unlocks it when it is done. 
(defun sbdb-generate-bins-table (Name Bins DBName Connection &key (Verbose Nil))
  "Begin by genrerating the bins table then iterate over it storing the values."
  (sbdb-create-static-table Name DBName Connection :Verbose Verbose)
  (sbdb-writelock-static-table Name DBName Connection :Verbose Verbose)
  (dotimes (N (length Bins))
    (odbc-insert 
     Name DBName Connection 
     '("ID" "StartDate" "EndDate")
     (list N (car (nth N Bins)) (cadr (nth N Bins)))
     :Verbose Verbose))
  (odbc-unlock-tables Connection :Verbose Verbose))


(defun sbdb-create-static-table (Name DBName Connection &key (Verbose Nil))
  "Create the static Bins table for later use in the process."
  (odbc-create-table
   Name DBName Connection
   (list (odbc-format-columnspec "ID" "INTEGER" :Not-Null t :Primary-Key t)
	 (odbc-format-columnspec "StartDate" "DATE" :Not-Null t)
	 (odbc-format-columnspec "EndDate" "DATE" :Not-Null t))
   :Verbose Verbose))


(defun sbdb-writelock-static-table (TableName DBName Connection &key (Verbose Nil))
  "Lock the static table for writing."
  (odbc-lock-tables 
   Connection 
   :Write (odbc-format-id :DB DBName :Tbl TableName)
   :Verbose Verbose))

   (odbc-format-lock-spec TableName DBName :Write)
   Connection
   :Verbose Verbose))
						 


;;; ---------------------------------------------------------------------------
;;; Dynamic tables 
;;; For each testset type the system will generate a new dynamic table.  Each 
;;; one will then be loaded with one instance per user per bin.  The results
;;; will be of the form:
;;;  (<ID> <UserID> <BinID> <NumFiles> . <Results>)
;;;
;;; The sysytem will collect all of the usernames from the database and then 
;;; iterate over the testtypes generating the appropriate values for each
;;; user and storing the results.

(defconstant **SBDB-Static-Columns**
    (list (odbc-format-columnspec 
	   "ID" "INTEGER" :Not-Null t :Auto-Increment t :Primary-Key t) 
	  (odbc-format-columnspec "UserID" "CHAR(64)" :Not-Null t)
	  (odbc-format-columnspec "BinID" "INTEGER" :Not-Null t)
	  (odbc-format-columnspec "NumFiles" "INTEGER" :Not-Null t)))

(defun sbdb-generate-dynamic-tables (TableNames Tests Bins DBName Connection &key (Verbose Nil))
  "Generate the dynamc tables in the database."
  (sbdb-create-dynamic-tables TableNames Tests DBName Connection :Verbose Verbose)
  (sbdb-lock-dynamic-tables TableNames Tests DBName Connection :Verbose Verbose)
  (sbdb-load-dynamic-tables TableNames Tests Bins DBName Connection :Verbose Verbose)
  (odbc-unlock-tables Connection :Verbose Verbose))


;;; For each testset generate a dynamic table that will contain 
;;; the requisite binning info once we have finished generating it.  
(defun sbdb-create-dynamic-tables (Names Tests DBName Connection &key (Verbose Nil))
  "Create the specified dynamic table."
  (let (Name Test)
    (dotimes (n (length Names))
      (setq Name (nth N Names))
      (setq Test (nth N Tests))
      (format t "#### Generating Table ~a ####~%" Name)
      (odbc-create-table 
       Name DBName Connection
       (append **SBDB-Static-Columns**
	       (odbc-format-list-columnspecs
		(testset-tests Test) (testset-ResultType Test)))
       :Verbose Verbose))))


;;; Generate a writelock spec for each of the dynaic tables and then request
;;; a write lock on them for use by the system.  
(defun sbdb-lock-dynamic-tables (Names Tests DBName Connection &key (Verbose Nil))
  (odbc-lock-tables
   Connection
   :Read (append (odbc-format-tableids DBName '("Users" "Files"))
		 (odbc-format-tableids DBName (map-testsets->tablenames Tests)))
   :Write (odbc-format-tableids DBName Names)
   :Verbose Verbose))
			       
;;; -----------------------------------------------------------------------
;;; Load the tables.
;;; Collect the list of users and then, iterating over it, Collect the 
;;; contents of each bin for each user and echo the results out to the
;;; relavent dynaic tables.  This assumes that the tables were already 
;;; locked and will now be unlocked.
(defun sbdb-load-dynamic-tables (TableNames Tests Bins DBName Connection &key (Verbose))
  (let (Name Test (Users (sbdb-collect-users DBName Connection :Verbose Verbose)))
    (do ((User (Qresult-NextLine Users Nil 'eof) (Qresult-NextLine Users Nil 'eof)))
	((equalp User 'EOF) Nil)
      (format t "#### Starting User ~a ####~%" User)
      (sbdb-generate-user-tables (car User) TableNames Tests Bins DBName Connection :Verbose Verbose))))
  

;;; Collect the userids from the database as a list.
(defun sbdb-collect-users (DBName Connection &key (Verbose Nil))
  "Select the userIDs from the file."
  (odbc-select
   (list (odbc-format-id :Col "Name"))
   (list (odbc-format-id :DB DBName :Tbl "Users"))
   Connection
   :Verbose Verbose))

;;; Given a user iterate over the Testsets and the bins collecting the users data 
;;; combining it and storing the results back into the database in the appropriate
;;; tables.
(defun sbdb-generate-user-tables (User TableNames Tests Bins DBName Connection &key (Verbose Nil))
  "Iterate over the bins."
  (let (Bin Name Test)
    (dotimes (M (length Tests))
      (setq Name (nth M TableNames))
      (setq Test (nth M Tests))
      (dotimes (N (length Bins))
	(setq Bin (nth N Bins))
	(sbdb-handle-bin N Bin User Test Name DBName Connection :Verbose Verbose)))))



;;; Given Bin ID information, inform the user that we are beginning the bin, then
;;; collect all of the results from the bin for the current test, combine them and
;;; then store the results in the appropriate table.
;;;
;;; Note the the ID number is not necessary now so the system will offload the cost
;;; of maintaining it onto the database itself.  As the sysytem runs no insertion 
;;; will be made into the ID field on each update.  The MySQL will treat this as a
;;; a NIL And an incremented value will be added to the column.
(defun sbdb-handle-Bin (BinID Bin User Test TableName DBName Connection &key (Verbose Nil))
  "Handle the bin data."
  (format t "## Starting Bin ~a ##~%" Bin)
  (odbc-insert
   TableName DBName Connection
   (append '("UserID" "BinID" "NumFiles") (Testset-tests Test))
   (append 
    (list User BinID)
    (sbdb-combine-results 
     Test (sbdb-collect-user-bindata User Bin Test DBName Connection :Verbose Verbose)))
   :Verbose Verbose))



;;; Given a Qresult struct iterate over it combining all of the stored results into
;;; a single combined result list and return it.  Note that this code is built
;;; around the assumption that each line the system returns is of the form:
;;;   (<Results>)
;;; This code will pull each line out of the Qresult and pass it to the 
;;; result-combiner along with the previous results.  Once the Qresult 
;;; contains no more lines then the system will return.  
;;;
;;; In order to deal with nil values the code begins by generating an empty
;;; resultset using the InitResults function within the testset.  All results
;;; that are pulled from the Qresult will then be combined with the Empty
;;; resultset.  
;;;
;;; The result value will be of the form:
;;;  (<NumFiles> . <Results>)  
;;; 
;;; Where: 
;;;  NumFiles is the number of files that were combined up in this run.
;;;  Results:  Are the result values themselves.  
(defun sbdb-combine-results (TestSet Qresult)
  "Combine the results."
  (do* ((Result (testset-make-empty-results Testset))
	(count 0 (+ 1 Count))
	(Next (Qresult-nextline Qresult Nil 'EOF)
	      (Qresult-nextline Qresult Nil 'EOF)))
      ((Equalp Next 'EOF) (cons Count Result))
    (pprint Result)
    (pprint Next)
    (setq Result (testset-combine-results TestSet Result Next))))

(defun foo ()
  (do ((Count 0 (1+ Count))
       (test Nil Nil))
      ((null test) Count)))


;;; Given a user, a testset, and a bin collect the user data for 
;;; that testset from the bin in a query and return it to the user.  
(defun sbdb-collect-user-bindata (User Bin Test DBName Connection &key (Verbose Nil))
  (let ((Dateref (odbc-format-id :DB DBName :Tbl "Files" :Col "CreationDate")))
    (odbc-select 
     (mapcar #'(lambda (Col) (odbc-format-id :Tbl (Testset-name Test) :Col Col))
	     (Testset-Tests Test))
     (list (odbc-format-id :DB DBName :Tbl "Files")
	   (odbc-format-id :DB DBName :Tbl (testset-name Test)))
     Connection
     :Where (sbdb-cub-WC User DateRef Bin Test DBName)
     :Order-By (list (odbc-format-id :Tbl (testset-name Test) :Col "FileID")
		     (odbc-format-id :Tbl (Testset-name Test) :Col "InstNum"))
     :Types (map-testset->typeslist Test)
     :Verbose Verbose)))


;;; Formulate the where clause that will be used to select the incoming
;;; bin data for the static binning process.
(defun sbdb-cub-wc (User DateRef Bin Test DBName)
  (odbc-format-infix-wc 
   "&&" 
   (odbc-format-infix-wc
    "=" (odbc-format-id :DB DBName :Tbl "Files" :Col "ID")
    (odbc-format-id :DB DBName :Tbl (testset-name Test) :Col "FileID"))
   (odbc-format-infix-wc 
    "=" (odbc-format-id :DB DbName :Tbl "Files" :Col "Username") 
    (odbc-format-val User)) 
   (odbc-format-infix-wc ">=" DateRef (odbc-format-val (car Bin)))
   (odbc-format-infix-wc "<=" DateRef (odbc-format-val (cadr Bin)))))

  



;;(setq **Bins** ((8 10 2002) (9 10 2002) (10 10 2002) (11 10 2002) (12 10 2002)))



|#

#|;;; Generate the dynamic  tables by first generating the static tables then
  ;;; iterating over each of the testsets and collecting the individual elements
  ;;; for later use.  

  (defparameter **SBDB-Static-Columns**
  (list (odbc-format-columnspec "ID" "INTEGER" :Not-Null t :Primary-Key t)
  (odbc-format-columnspec "UserID" "INTEGER" :Not-Null t)
  (odbc-format-columnspec "BinID" "INTEGER" :Not-Null t)))
  
  (defun sbdb-create-dynamic-tables (TblNames Tests DBName Connection &key (Verbose Nil))
  (dotimes (N (length Tests))
  (odbc-create-table 
  (nth N TblNames) DBName Connection 
  (append **SBDB-Static-Columns**
  (odbc-format-list-columnspecs
  (testset-tests (nth N Tests))
  (testset-ResultType (nth N Tests))))
  :Verbose Verbose)))
  |#








#|;;;; =================================================================================
;;;; File Combination.
;;;; This code is designed to combine the problem-instance results that are produced 
;;;; by the hdmain code into a series of per-file results.  These per-file tables 
;;;; are intended to mirror the results that were obtained under the original 
;;;; Version of the HDmain where values were stored on a per-file basis.  
;;;;
;;;; Loosely, this code will take a set of testsets and then iterate over the files
;;;; table.  For each fileid it will load all of the problem instances for that file
;;;; and combine them in order by instance id.  The combination functions will be 
;;;; taken from the testsets.  
;;;; 
;;;; The resulting data will be stored in tables of same names as the testsets 
;;;; this time prefixed with "file_".  The results will be of the form:
;;;;   (<FileID> . <Results>)

(defun File-Bin-results (Tests DBName ODBCName &key (Prefix "File_") (Verbose Nil))
  "Bin the results by file in the sysytem."
  (format t "=================================================~%")
  (format t "Beginning File Bin.~%")
  (let* ((Connection (odbc-connect ODBCName :Verbose Verbose))
	 (Tables (fbr-generate-tables Tests Prefix DBName Connection :Verbose Verbose))
	 (FileIDs (fbr-get-fileIDs DBName Connection)))
    (format t "~a~%" FileIDs)
    (fbr-lock-tables Tables Tests DBName Connection :Verbose Verbose)
    (file-bin-results-i Connection Tables FileIDs Tests DBName Prefix :Verbose Verbose)
    (odbc-unlock-tables Connection :Verbose Verbose)
    (odbc-disconnect Connection :Verbose Verbose)))



;;; Table generation.
;;; Given a list of tests and a predicate as well as database 
;;; info generate the set of tables necessary for the file binning.  
;;; This code will generate a new table per-test-set of the form:
;;;  (<FileID> . <Tests>)
;;;
;;; When done the function will return a list of the table names.
(defparameter **FBR-Table-static-Col**
    (odbc-format-columnspec "FileID" "INTEGER" :not-null t :primary-key t))

(defun fbr-generate-tables (Tests Prefix DBName Connection &key (Verbose Nil))
  (let (Name Names)
    (dolist (Test Tests)
      (setq Name (string-downcase (format Nil "~a~a" Prefix (testset-name Test))))
      (push Name Names)
      (odbc-create-table
       Name DBName Connection
       (cons **FBR-Table-Static-Col**
	     (odbc-format-list-columnspecs
	      (Testset-tests Test) (Testset-resulttype Test)))
       :Verbose Verbose))
    (reverse Names)))


;;; Given a DBName and a connection extract the complete list of fileids
;;; from the database as a list that can be iterated over.
(defun fbr-get-fileids (DBName Connection)
  (odbc-select 
   '("ID") (list (odbc-format-id :Db DBName :Tbl "files"))
   Connection
   :Types '(:Int)
   :Qresult Nil
   :Verbose t))



;;; Given a list of table names and testsets lock the tables for reading and
;;; writing and lock the tables specified by the testsets, and the files table
;;; for reading.
(defun fbr-lock-tables (Tables Tests DBName Connection &key (Verbose Nil))
  (odbc-lock-tables 
   (odbc-format
   
   (append (list (odbc-format-lock-spec "files" DBName :Read))
	   (mapcar #'(lambda (N) (odbc-format-lock-spec N DBName :Write)) Tables)
	   (mapcar #'(lambda (Ts) 
		       (odbc-format-lock-spec 
			(string-downcase (format Nil "~a" (testset-name Ts))) 
			DBName :Read)) Tests))
   Connection 
   :Verbose Verbose))





;;; Carry out the main inner loop for the results file binning.
(defun file-bin-results-i (Connection Tables FileIDs Tests DBName Prefix &key (Verbose Nil))
  "The inner loop of the file-bin-results process."
  (let (Table Test)
    (dotimes (N (length Tables))
      (setq Table (nth N Tables))
      (setq Test (nth N Tests))
      (format t "### Starting ~a ~a ###~%" Table (testset-name Test))
      (dolist (FileID FileIDs)
	(fbr-store-results
	 Table DBName Connection Test (car FileID)
	 (testset-combine-resultsets
	  Test 
	  (fbr-get-results 
	   (car FileID) Test DBName Connection :Verbose Verbose))
	 :Verbose Verbose)))))




;;; Given a FileID, a Test set, a DBName and a connection struct extract 
;;; all of the results that match the fileid from the table corresponding
;;; to the testset.  This code will pull only those result line that 
;;; correspond to the tests from the problem-instance tables.  The results
;;; will be sorted in the order that they arose in the original problem file.
(defun fbr-get-results (FileID TestSet DBName Connection &key (Verbose Nil))
  (odbc-select 
   (map-testset->colids TestSet DBName)
   (list (map-testset->tableid TestSet DBName))
   Connection
   :Where (odbc-format-infix-wc 
	   "=" (map-testset->colid "FileID" TestSet DBName)
	   (odbc-format-val FileID))
   :Order-By (odbc-format-id :DB DBName :Tbl (testset-name TestSet) :Col "InstNum")
   :Types (map-testset->typeslist TestSet)
   :Qresult Nil
   :Verbose Verbose))
				

;;; Given a set of results store those result values in 
;;; the supplied fileresult table with the specified fileid.  
;;;
;;; Some of the files will not generate results.  In that case this 
;;; code will generate an empty result using the functions in the
;;; TestSet.

(defun fbr-store-results (TableName DBName Connection TestSet FileID Results &key (Verbose Nil))
  (odbc-insert 
   TableName DBName Connection
   (cons "FileID" (mapcar #'(lambda (Ts) (format Nil "~a" Ts)) (testset-tests TestSet)))
   (cons FileID (if Results Results (testset-make-empty-results TestSet))) 
   :Verbose Verbose))
|#


#|;;;; =================================================================================
;;;; Problem Binning (NOT WORKING)
;;;; This code is designed to take all of the problems instance results for a given
;;;; student and to couple them into a result set according to the problem and the 
;;;; student.  The results will be taken to encapsulate all of the student's work on
;;;; a given problem.  
;;;;
;;;; The results will be stored in tables of the form:
;;;;  (<StudentID> <ProblemName> . Results)
;;;;
;;;; Where the table names are taken from the original testsets, prefixed with a 
;;;; supplied value.  The default prefix value is "Problem_".
;;;;
;;;; NOTE:: This code relies on the fact that the list of problems in the database
;;;;        was precomputed and stored in the problems table.  This table simply
;;;;        contains the names and no more.

(defun problem-bin-results (Tests DBName ODBCName &key (Prefix "Problem_") (Verbose Nil))
  "Bin the results by problem and user in the system."
  (format t "=================================================~%")
  (format t "Beginning Problem Bin.~%")
  (let* ((Connection (odbc-connect ODBCName :Verbose Verbose))
	 (Tables (pbr-generate-tables Tests Prefix DBName Connection :Verbose Verbose))
	 (Usernames (pbr-get-usernames DBName Connection :Verbose Verbose))
	 (Problems (pbr-get-Problems DBName Connection :Verbose Verbose)))
    (pbr-lock-tables Tables Tests DBName Connection)
    (problem-bin-results-i 
     Connection Tables Usernames Problems Tests DBName :Verbose Verbose)
    (odbc-unlock-tables Connection :Verbose Verbose)
    (odbc-disconnect Connection :Verbose Verbose)))



;;; Table generation.
;;; Given a list of tests and a predicate as well as database 
;;; info generate the set of tables necessary for the problem binning.  
;;; This code will generate a new table per-test-set of the form:
;;;  (<UserID> <Problem> . <Tests>)
;;;
;;; When done the function will return a list of the table names.
(defparameter **PBR-Table-static-Cols**
    (list (odbc-format-columnspec "UserID" "CHAR(64)" :not-null t)
	  (odbc-format-columnspec "Problem" "CHAR(64)" :not-null t)))

(defun pbr-generate-tables (Tests Prefix DBName Connection &key (Verbose Nil))
  (let (Name Names)
    (dolist (Test Tests)
      (setq Name (string-downcase (format Nil "~a~a" Prefix (testset-name Test))))
      (push Name Names)
      (odbc-create-table
       Name DBName Connection
       (append **PBR-Table-Static-Cols**
	       (odbc-format-list-columnspecs
		(Testset-tests Test) (Testset-resulttype Test)))
       :Verbose Verbose))
    (reverse Names)))



;;; given a DBName and a connection extract the user ids from it as a list
;;; and return them in the form of a list.  The mapcar extracts the ids 
;;; from the list form.
(defun pbr-get-usernames (DBName Connection &key (Verbose Nil))
  "Collect the userid's from the database."
  (mapcar 
   #'car 
   (odbc-select 
    (list "Name") 
    (list (odbc-format-id :DB DBName :Tbl "users"))
    Connection 
    :Qresult Nil
    :Verbose Verbose)))



;;; Given a DBName and a connection extract the list of problems from
;;; the database.  This assumes that the problems table has been 
;;; constructed already.
(defun pbr-get-problems (DBName Connection &key (Verbose Nil))
  (mapcar #'car
	  (odbc-select
	   (list "name")
	   (list (odbc-format-id :DB DBName :Tbl "problems"))
	   Connection 
	   :Qresult Nil
	   :Verbose Verbose)))



;;; Given a list of table names and testsets lock the tables for reading and
;;; writing and lock the tables specified by the testsets, and the files table
;;; for reading.
(defun pbr-lock-tables (Tables Tests DBName Connection &key (Verbose Nil))
  (odbc-lock-tables 
   (append (list (odbc-format-lock-spec "files" DBName :Read)
		 (odbc-format-lock-spec "problems" DBName :Read))
	   (mapcar #'(lambda (N) (odbc-format-lock-spec N DBName :Write)) Tables)
	   (mapcar #'(lambda (Ts) 
		       (odbc-format-lock-spec 
			(string-downcase (format Nil "~a" (testset-name Ts))) 
			DBName :Read)) Tests))
   Connection 
   :Verbose Verbose))



(defun problem-bin-results-i (Connection Tables Usernames Problems Tests 
			      DBName &key (Verbose Nil))
  "The inner loop of the problem-bin-results process."
  (let (Table Test)
    (dotimes (N (length Tables))
      (setq Table (nth N Tables))
      (setq Test (nth N Tests))
      (format t "##################################~%")
      (format t "Starting ~a ~a ~%" Table (testset-name Test))
      (dolist (User Usernames)
	(dolist (Problem Problems)
	  (format t "### Starting ~a ~a ### ~%" User Problem)
	  (pbr-store-results
	   User Problem Table DBName Connection Test 
	   (testset-combine-resultsets
	    Test (pbr-get-results
		  User Problem Test DBName Connection :Verbose Verbose))
	   :Verbose Verbose))))))






;;; Given a UserID, a Problem name a testset a dbname and a connection
;;; extract all of the problem-instance results for the specified student's
;;; work on the specified problem.  The results should be ordered by the
;;; file-id and the problem-intsance-id of the instance so that the earlier
;;; work preceeds the later.  
(defun pbr-get-results (UserID ProblemName TestSet DBName Connection &key (Verbose Nil))
  (odbc-select 
   (map-testset->colids TestSet DBName)
   (list (odbc-format-id :DB DBName :Tbl "files")
	 (map-testset->tableid TestSet DBName))
   Connection
   :Where (pbr-gr-wc UserID ProblemName TestSet DBName)
   :Order-By (list (odbc-format-id :DB DBName :Tbl (testset-name TestSet) :Col "FileID")
		   (odbc-format-id :DB DBName :Tbl (testset-name TestSet) :Col "InstNum"))
   :Types (map-testset->typeslist TestSet)
   :Qresult Nil
   :Verbose Verbose))

;;; Format a where clause for the pbr-get-results function that links the files and
;;; Testset tables and that ensures that the specified username and problem name 
;;; that we are after is the one returned.
(defun pbr-gr-wc (UserID ProblemName TestSet DBName)
  (odbc-format-infix-wc
   "&&" 
   
   (odbc-format-infix-wc 
    "=" (odbc-format-id :DB DBName :Tbl "files" :Col "ID")
    (odbc-format-id :DB DBName :Tbl (testset-name TestSet) :Col "FileID"))
   
   (odbc-format-infix-wc 
    "=" (odbc-format-id :DB DBName :tbl "files" :Col "username")
    (odbc-format-val UserID))
    
   
   (odbc-format-infix-wc 
    "=" (odbc-format-id :DB DBName :tbl (testset-name TestSet) :Col "Problem")
   (odbc-format-val ProblemName))))
  
;;; Given a set of results store those result values in the supplied 
;;; ProblemResult table with the specified userid and problemname
(defun pbr-store-results (UserID ProblemName TableName DBName 
			  Connection TestSet Results &key (Verbose Nil))
  (odbc-insert 
   TableName DBName Connection
   (append '("UserID" "Problem") 
	   (mapcar #'(lambda (Ts) (format Nil "~a" Ts)) 
		   (testset-tests TestSet)))
   (append (list UserID ProblemName) 
	   (or Results (testset-make-empty-results TestSet)))
   :Verbose Verbose))



|#


#|;;;; =================================================================================
  ;;;; User Combination.
;;;; This code is designed to bin the problem results into a set of user results.
;;;; these will then be used for analysis.  The full set of results that the system
;;;; will store will be one line per user.
;;;;   (<Username> . <Results>)
;;;;
;;;; Note:: This code uses the file results as a cache of the computations since all
;;;; that we will be doing is compiling those results down futher to a 1:1 ratio with
;;;; the number of students rather than the current 1:many.
;;;;
;;;; I may change this in the future.

(defun user-Bin-results (Tests DBName ODBCName &key (Prefix "user_") (Verbose Nil))
  "Bin the results by user in the sysytem."
  (format t "=================================================~%")
  (format t "Beginning User Bin.~%")
  (let* ((Connection (odbc-connect ODBCName :Verbose Verbose))
	 (Tables (ubr-generate-tables Tests Prefix DBName Connection :Verbose Verbose))
	 (userIDs (ubr-get-userids DBName Connection)))
    (ubr-lock-tables Tables Tests DBName Connection :Verbose Verbose)
    (user-bin-results-i Connection Tables userIDs Tests DBName :Verbose Verbose)
    (odbc-unlock-tables Connection :Verbose Verbose)
    (odbc-disconnect Connection :Verbose Verbose)))



;;; Table generation.
;;; Given a list of tests and a predicate as well as database 
;;; info generate the set of tables necessary for the file binning.  
;;; This code will generate a new table per-test-set of the form:
;;;  (<username> . <Tests>)
;;;
;;; When done the function will return a list of the table names.
(defparameter **UBR-Table-static-Col**
    (odbc-format-columnspec "UserID" "INTEGER" :not-null t :primary-key t))

(defun ubr-generate-tables (Tests Prefix DBName Connection &key (Verbose Nil))
  (let (Name Names)
    (dolist (Test Tests)
      (setq Name (map-testset->tablename Test :Prefix Prefix))
      (push Name Names)
      (odbc-create-table
       Name DBName Connection
       (cons **UBR-Table-Static-Col**
	     (odbc-format-list-columnspecs
	      (Testset-tests Test) (Testset-resulttype Test)))
       :Verbose Verbose))
    (reverse Names)))


;;; given a DBName and a connection extract the user ids from it as a list
;;; and return them in the form of a list.  The mapcar extracts the ids 
;;; from the list form.
(defun ubr-get-userids (DBName Connection &key (Verbose Nil))
  "Collect the userid's from the database."
  (mapcar 
   #'car 
   (odbc-select 
    (list "ID") 
    (list (odbc-format-id :DB DBName :Tbl "users"))
    Connection 
    :Qresult Nil
    :Verbose Verbose)))


;;; Given a list of table names and testsets lock the tables for reading and
;;; writing and lock the tables specified by the testsets, and the files table
;;; for reading.
(defun ubr-lock-tables (Tables Tests DBName Connection &key (Verbose Nil))
  (odbc-lock-tables 
   (append (list (odbc-format-lock-spec "users" DBName :Read) 
		 (odbc-format-lock-spec "files" DBName :Read))
	   (mapcar #'(lambda (N) (odbc-format-lock-spec N DBName :Write)) Tables)
	   (mapcar #'(lambda (Ts) 
		       (odbc-format-lock-spec 
			(string-downcase (format Nil "file_~a" (testset-name Ts))) 
			DBName :Read)) Tests))
   Connection 
   :Verbose Verbose))



;;; Carry out the main inner loop for the results file binning.  Iterate over the
;;; set of output tables and testsets.  For each one begin binning the results 
;;; from the specified table according to user.  
;;;
;;; For each userid load the files and combine them in the order that the user
;;; created them.  
(defun user-bin-results-i (Connection Tables userIDs Tests DBName &key (Verbose Nil))
  "The inner loop of the file-bin-results process."
  (let (Table Test)
    (dotimes (N (length Tables))
      (setq Table (nth N Tables))
      (setq Test (nth N Tests))
      (format t "### Starting ~a ~a ###~%" Table (testset-name Test))
      (dolist (UserID userIDs)
	(format t "~a~%" UserID)
	(ubr-store-results
	 Table DBName Connection Test UserID
	 (testset-combine-resultsets
	  Test (ubr-get-results UserID Test DBName Connection :Verbose Verbose))
	 :Verbose Verbose)))))




;;; Given a username, a Test set, a DBName and a connection struct extract 
;;; all of the results that match the fileid from the table corresponding
;;; to the testset.  This code will pull only those result line that 
;;; correspond to the tests from the problem-instance tables.  The results
;;; will be sorted in the order that they arose in the original problem file.
(defun ubr-get-results (UserID TestSet DBName Connection &key (Verbose Nil))
  (odbc-select 
   (map-testset->colids 
    TestSet DBName :tablename (map-testset->tablename TestSet :Prefix "file_"))
   (list 
    (odbc-format-id :db DBName :Tbl "users")
    (odbc-format-id :Db DBName :Tbl "files")
    (odbc-format-id :db DBName :Tbl (map-testset->tablename TestSet :Prefix "file_")))
   Connection
   :Where (ubr-wc UserID TestSet DBName)
   :Order-By (odbc-format-id 
	      :DB DBName :Tbl (map-testset->tablename TestSet :Prefix "file_") 
	      :Col "FileID")
   :Types (map-testset->typeslist TestSet)
   :Qresult Nil
   :Verbose Verbose))
				

;;; Ensure that the userID that was passed in constrains the choice of file
;;; results appropriately by restricting us to files opened by the user.
(defun ubr-wc (UserID TestSet DBName)
  (odbc-format-infix-wc 
   "&&"

   (odbc-format-infix-wc
    "=" (odbc-format-id :DB Dbname :Tbl "files" :Col "ID")
    (map-testset->colid 
     "FileID" TestSet DBName :TableName (map-testset->tablename TestSet :Prefix "file_")))
    
   (odbc-format-infix-wc 
    "=" (odbc-format-id :DB DBName :Tbl "users" :Col "Name")
    (odbc-format-id :DB DBName :Tbl "files" :Col "Username"))
    
   (odbc-format-infix-wc 
    "=" (odbc-format-id :DB DBName :Tbl "users" :Col "ID")
    (odbc-format-val UserID))))
   
   

    

;;; Given a set of results store those result values in 
;;; the supplied fileresult table with the specified fileid.  
;;;
;;; Some of the files will not generate results.  In that case this 
;;; code will generate an empty result using the functions in the
;;; TestSet.

(defun ubr-store-results (TableName DBName Connection TestSet UserID Results &key (Verbose Nil))
  (odbc-insert 
   TableName DBName Connection
   (cons "UserID" (mapcar #'(lambda (Ts) (format Nil "~a" Ts)) (testset-tests TestSet)))
   (cons UserID (if Results Results (testset-make-empty-results TestSet)))
   :Verbose Verbose))





;;; Utility code.
(defun map-testset->TableID (TestSet DBName &key (TableName Nil))
  "Given a testset map it to a table id for the database"
  (odbc-format-id
   :DB DBName
   :Tbl (or TableName (string-downcase (format Nil "~a" (testset-name TestSet))))))


(defun map-testset->tablename (Testset &key (Prefix ""))
 (string-downcase (format Nil "~a~a" Prefix (testset-name TestSet))))

(defun map-testset->colids (TestSet DBName &key (TableName Nil))
  (mapcar 
   #'(lambda (Name) 
       (odbc-format-id 
	:DB DBName 
	:Tbl (or TableName (string-downcase (format Nil "~a" (testset-name TestSet))))
	:Col (string-downcase (format Nil "~a"  Name))))
   (testset-Tests TestSet)))


(defun map-testset->ColID (Column TestSet DBName &key (TableName Nil))
  "Given a testset map it to a table id for the database"
  (odbc-format-id
   :DB DBName
   :Tbl (or TableName (string-downcase (format nil "~a" (testset-name TestSet))))
   :Col (string-downcase (format Nil "~a" Column))))




(defun map-testset->typeslist (Testset)
  (make-list (length (testset-tests TestSet)) 
	     :initial-element (testset-ExternalType TestSet)))
|#