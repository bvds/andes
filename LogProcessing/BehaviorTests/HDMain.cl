;; ======================================================================
;; HDMain.cl
;; Collin Lynch.
;; 3/13/2003
;;
;; The code in this file is used to munge the stored student logs in order to
;; obtain useful behavioral data that we can then analize.  The code in this
;; file is designed to iterate over a set of directory-sorted logfiles and to
;; collect data using a number of tests sets.  That data will be stored in 
;; comma-separated text files or into an ODBC compliant database.  
;;
;; The root function in this file takes a pathname to a set of directory
;; sorted logfiles, a set of testsets, and an output type as well as a
;; number of other keyword arguments.  These arguments are used to control
;; the output and will be used later to determine the system's behavior.
;;
;; The testsets are structured sets of predicates that will be evaluated 
;; as each command is loaded into the file.  Each testset is a struct of
;; the form defined in Base/testset.cl  Each one has a field for 

;; The code in this file is used to generate behavior data for a set of 
;; logfiles and to store that data as text files or to an ODBC compliant
;; database.  
;;
;; The functions in this file take a pathname to a specific set of data
;; sources as well as an output specification and then execute each 
;; file with the specified tests before storing the results to the 
;; designated output file.  The code for returning results from a single
;; file is located in HelpDriver.cl
;;
;; As with previous versions of thi software the system takes a root path
;; containing a set of directory-sorted logfiles.  It begins by collecting
;; the usernames from the directories and assigning ids to them.  It then
;; Iterates over the directories collecting information on each file in the
;; system and executing the tests on each file to store the results.  
;;
;; The information from each file is collected by executing tests on the files
;; as the system iterates over them.  Tests are functions that return some 
;; value to the system.  Tests are cal
;; The tests are supplied using the test structs and need to be loaded in 
;; order 
;;
;; ------------------------------------------------------------------------
;; TODO:
;;  Modify the system to support storing multiple years in a single database
;;  by allowing the baseline userid and the baseline fileid to be passed in
;;  to the listing that will permit the results to be stored.


;;;; ======================================================================
;;;; Global Parameters.
;;;; These params handle global values that it didn't make sense to embed
;;;; in the runtime code.

;;; The file extension that will be used on all output files is unlikely to 
;;; change often from run-to-run.  Therefore I have placed it here rather
;;; than as an input argument.
(defparameter **Output-File-Ext** ".data")


;;;; ======================================================================
;;;; Main.
;;;; The code in this section represents the main entry point for use and
;;;; will call the specialized code below as necessary.  

;;;; test-logfiles
;;;; Given a root path to a directory containing a set of directory-sorted
;;;; logfiles, iterate over the files collecting user information and file
;;;; information on each one and storing the results.  
;;;;
;;;; The result values will be stored in a database of four or more tables
;;;; Or a set of files corresponding to:
;;;;  1. Users:  A list of usernames and numerical user ids.
;;;;  2. Files:  A list of file paths and file names along with beginning 
;;;;      and ending info as well as other information.
;;;;  3. Results:  This will be one or more files one per test-set where
;;;;        each one contains a sequence of problem-instance results 
;;;;        corresponding to the problems that were opened by the students
;;;;        at runtime.  
;;;; 
;;;; If the results are stored as files they will be three files stored in
;;;; the specified output location.  If they are stored in a database then
;;;; the system will log into the specified ODBC database server and create
;;;; a database of the specified name and store the results into the names
;;;; that were specified above.  
;;;;
;;;; Arguments:  
;;;;  Root: Pathname to the logfiles in question.
;;;;  Tests: The tests to be executed on each logfile (in type lists).
;;;;  Output-Type:  The output type one of :Database or :File
;;;;   If :database is supplied then the user needs to supply the 
;;;;      :ODBC name and :Dbname attributes.
;;;;   Else the user must supply the :FilePath attrobute.  
;;;;   
;;;;  :ODBCName The odbc name entry that will be used to access the database.
;;;;  :DBName The data base name that will be used.  
;;;;  :FilePath The location that any output files will be stored.
;;;;  :IfExists  If the Database or files exist behave accordingly
;;;;    this is one of :overwrite :append or :error (default)

(defun test-logfiles (Root Tests OutputType
		      &key (Host Nil) (Port Nil) (User Nil) (Pass Nil)
			   (LogStream Nil) (LogIndent Nil) 
			   (DBName Nil) (OdbcName nil) 
			   (FilePath Nil) (IfExists :Error)
			   (Condition Nil) (Year 0000) 
			   (handle-set-scores Nil) 
			   (Base-UserID 0) (Base-FileID 0) (Verbose Nil))
  (declare (ignore LogIndent LogStream Pass User Port Host))
  (case OutputType
    (:Database (database-test-logfiles Root Tests Host User Pass DBName IFExists 
				 :Condition Condition 
				 :Year Year 
				 :Handle-Set-Scores Handle-Set-Scores
				 :Base-UserID Base-USerID
				 :Base-FileID Base-FileID
				 :Verbose Verbose)) 
    (:File (file-test-logfiles Root Tests FilePath IFExists 
			       :Condition Condition
			       :Year Year
			       :Handle-Set-Scores Handle-Set-Scores
			       :Verbose Verbose))))





;;;; =========================================================================
;;;; Database Specific Code.
;;;; If the user elects to store the results in a database then the system will 
;;;; begin by opening a connection to the database and generating or testing
;;;; the necessary files before passing control to the main processing loop.
;;;; the necessary result storage calls will be passed in as closures.  When the
;;;; main loop completes then the connection to the database will be closed.
;;;;
;;;; This code is designed to make use of the db library that was defined for 
;;;; use with the mysql and other database servers.  This code makes it possible
;;;; to use the database for storage.
;;;;
;;;; This code assumes that the user has access to db compliant database 
;;;; server and the right to generate new databases and tables within those 
;;;; databases or that the tables already exist and the user has the right
;;;; to append to them.  
;;;;
;;;; The results of processing will be stored in several tables within the
;;;; database in the following forms.
;;;; Two static tables:
;;;;   USER:  A record of all the usernames in the system.
;;;;    this table is of the form  Name=CHAR(255), ID=INTEGER, GROUP=CHAR(255)
;;;;    where Integer is a primary key and Name is taken from the folder.
;;;;    group is an optional value that I will arrainge to supply as a keyword
;;;;    argument from the toploop or to extract from the pathname, I have not
;;;;    yet decided on how best to do this.  
;;;;
;;;;   FILE:  A record of all the files that are examined including pathnames
;;;;    usernames, the userids correspoinding to the files, the date and time
;;;;    that the file was begun and the length of the file that was processed.
;;;;    the spec is...
;;;;
;;;; The remaining tables will be determined by the testsets.  A new table will
;;;; be generated for each testset where each test is assigned a column within
;;;; the table and each record in the table corresponds to a separate file result.
;;;; in the future I would like to modify this so that each problem instance within
;;;; the file gets a separate result but that may not be necessary.  
;;;;
;;;; The name of each table will be taken from the result set and be of the form:
;;;;   <TestSet-Name>_Results
;;;;
;;;; The form of each table is:
;;;;   FILEID=INTEGER . RESULT(N)=TestSet-ResultType.
;;;;
;;;; NOTE:: that in future I am planning to extend this so that the results 
;;;; will be stored according to problem instance at a later date.



;;; db-test-logfiles
;;; Given a root pathname to a set of user-driectory sorted logfiles, a list
;;; of tests an DB server, user, passwrod and a Database name as well as IFExists 
;;; settings open a connection to the specified ODBC datasource throwing an error 
;;; if necessary and perform any setup and testing operations that are necessary
;;; before passing control to the main loop along with closures for the output.
;;; Once the main loop is completed then close the db connection.
;;;
;;; In the future this code will lock tables and then unlock them at the end 
;;; of the run or the system will lock them in the prep-output code.

(defun database-test-logfiles (Root Tests Host User Pass DBName IFExists 
			 &key (Port 3306) (LogStream t)  
			      (Condition Nil) (Year Nil) 
			      (Handle-Set-Scores Nil) 
			      (Base-UserID 0) (Base-FileID 0) (Verbose Nil))
  "Prepare the database connection and iterate."
  ;;(declare (ignore IfExists))
  (let (Result (Connection (mysql-db-connect :Host Host :Port Port :User User :Pass Pass
					     :DBName DBName :LogStream LogStream
					     :Verbose Verbose)))
    (database-prep-output Connection IFExists Tests :Verbose Verbose)
    (database-lock-tables Connection Tests :Verbose Verbose)
    
    (setq Result
      (main-test-logfiles
       Root Tests
       #'(lambda (Userid UserName UserCond) 
	   (if (database-test-user Connection Username USerCond :Verbose Verbose) Nil
	     (progn (database-output-user Connection UserID Username USerCond :Verbose Verbose)
		    t)))
       #'(lambda (FileID File FileYear) 
	   (database-output-file Connection FileID File :Year FileYear :Verbose Verbose))
       #'(lambda (FileID InstNum ProblemName Start End TestSet Results) 
	   (database-output-results Connection FileID InstNum ProblemName Start End TestSet Results :Verbose Verbose))
       :Condition Condition
       :Year Year
       :Verbose Verbose
       :Handle-Set-Scores Handle-Set-Scores
       :Base-UserID Base-UserID
       :Base-FileID Base-FileID))
      
    (database-unlock-tables Connection :Verbose Verbose)
    (db-disconnect Connection :Verbose Verbose)
    Result))


;;; --------------------------------------------------------------------------------
;;; db-prep-output
;;; Preparing the database output is a matter of opning the database connection
;;; testing to see if the specified database exists and, if it does whether or not
;;; the tables within it exist.  The behavior of the system depends upon the 
;;; overwrite format that the students chose.  
;;;
;;; If the user specified :Error then the system will throw an error if a database
;;; of the same name already exists.  If not then the system will generate the 
;;; specified database and populate it with the three tables.  
;;;
;;; If the user specified :Overwrite then the system will overwrite any preexisting
;;; database with a new one and then generate the tables as necessary.
;;;
;;; If the user specified :Append then the system will throw an error if the database
;;; and tables are not present or proceed as normal if they do.  
;;;
;;; NOTE:: For now the system accpets only the default value of :Error when it is operating.
;;;
;;; This code assumes that an ODBC datasource with the appropriate name exists and
;;; that it can be accessed using the supplied name.  Any ODBC errors will be thrown
;;; to the user.  The resulting database connection will be returned.

(defun database-prep-output (Connection IFExists Tests &key (Verbose Nil))
  "Prepare the database for output."
  (case IfExists
    (:Error (database-prep-output-error Connection Tests :Verbose Verbose))
    ;;(:Overwrite (db-prep-output-overwrite Tests DBName Connection :verbose Verbose))
    (:Append (database-prep-output-append Connection Tests :Verbose Verbose))
    (t (error "Unrecognized IFExists value: ~a supplied to database code." IFExists))))
  


;;; database-prep-output-error
;;; If the user selected error then test to see if the specified table(s) exist.
;;; If any of the necessary tables exist then this code will throw an error.
;;; if not then they will be created.
(defun database-prep-output-error (Connection Tests &key (Verbose Nil))
  "prep the output with errors."
  (when (database-tables-existp Connection Tests)
    (error "Database tables already exist."))
  (database-create-users-table Connection :Verbose Verbose)
  (database-create-files-table Connection :Verbose Verbose)
  (database-create-dynamic-tables Connection Tests :Verbose Verbose))

;;; database-prep-output-append
;;; If the user selected error then test to see if the specified table(s) exist.
;;; If any of the necessary tables exist then this code will throw an error.
;;; if not then they will be created.
(defun database-prep-output-append (Connection Tests &key (Verbose Nil))
  "prep the output with errors."
  (when (not (database-tables-existp Connection Tests))
    (database-create-users-table Connection :Verbose Verbose)
    (database-create-files-table Connection :Verbose Verbose)
    (database-create-dynamic-tables Connection Tests :Verbose Verbose)))


;;;; ----------------------------------------------------------------------
;;;; Tables
;;;; As I said above the system generates two static tables USER and 
;;;; FILE and one dynamic table for each individiaul testset.  The 
;;;; format for the static tables are specified as parameters below.
;;;;
;;;; The output functions for each table are also listed below.

;;; -----------------------------------------------------------------------
;;; Testing
;;; Check to see if the static tables and the required tables aready exist
;;; or not.  This is a simple name check.  No attempt is made to match the
;;; columns.  
(defun database-tables-existp (Connection Tests &key (Verbose Nil))
  "Return t if the required tables exist."
  (let ((Tables (db-list-tables Connection :Verbose (if (equalp Verbose 2) t)))
	(Tablenames (list "users" "files" (mapcar #'map-testset->tablename Tests))))
    (loop for Name in Tablenames
	when (member Name Tables :test #'string-equal)
	return t)))


;;; -----------------------------------------------------------------------
;;; Locking and unlocking.
;;; Each time that the system makes an entry to or deletes a value from a 
;;; table it must initiate a lock on the table and then unlock it when it
;;; is done.  In order to speed things up the system will go ahead and lock
;;; all the relavent tables before it starts generating data and then 
;;; unlocks them when it is done.  This should reduce the runtime of the
;;; system overall by reducing the time that each lock takes.  
;;;
;;; NOTE:: When a lock is sent to MySQL all previous locks are removed
;;;        so this has to be done with a single ODBC call.

(defun database-lock-tables (Connection Tests &key (Verbose Nil))
  "Lock all of the tables in this database."
  (if Verbose (format t "Database:: Locking Tables.~%"))
  (db-lock-tables 
   Connection 
   :Write (append (list "users" "files") 
		  (map-testsets->tablenames Tests)) 
   :Verbose (if (equalp Verbose 2) t Nil)))
   

(defun database-unlock-tables (Connection &key (Verbose Nil))
  "Unlock all of the tables currently locked by this thread."
  (if Verbose (format t "DB:: Unlocking tables.~%"))
  (db-unlock-tables Connection :Verbose (if (equalp Verbose 2) t Nil)))
 

;;; ------------------------------------------------------------------------
;;; USer Table
;;; The users table contains three fields: 
;;;   ID: an integer that is also a primary key,
;;;   Name: a char(255) field that is the usert's login,
;;;   Condition: an additional char that is used to collect users by year.
;;;
;;; ID is defined as a non-null primary key.  Later on I will add an index
;;; to make searching on the student's name and group faster if it becomes
;;; an issue.  
;;; 
;;; For now the data is simply loaded as-is.

(defun database-create-users-table (Connection &key (Verbose Nil))
  "Create the user table in the database."
  (db-create-table 
   Connection 
   (database-make-users-schema Connection)
   :If-Not-Exists t 
   :Verbose (if (equalp Verbose 2) t Nil)))


(defun database-make-users-schema (Connection)
  (db-make-table-schema 
   Connection "users"
   :Columns (list (db-make-column-schema Connection "id" 'integer :Null Nil :Primary-Key t)
		  (db-make-column-schema Connection "name" 'string :Null Nil)
		  (db-make-column-schema Connection "condition" 'string))
   :Indicies (list (db-make-index-schema Connection "PRIMARY" :table "users" :Columns '("id") :Primary-Key t)
		   (db-make-index-schema Connection "usersindex" :Table "users" :columns '("id" "name")))))


;;; ---------------------------------------------------------------------
;;; File Table.
;;; The file table contains the following fields:
;;;   ID:        A numeric file ID.
;;;   Path:      The path containing the file including the drive.
;;;   Filename:  A string containing the file name.
;;;   Username:  The username encoded in the file name.
;;;   Day:       The day that the file was created.
;;;   Time:      The time that the file was created.
;;;   
;;;  ID is the primary key to the table.
;;;  The table also contains an index on the username in order to 
;;;  speed up searches.
;;;
;;; It will be necessary to munge the date information and the 
;;; times into a format that can be read by the ODBC form and 
;;; this will take additional work.  However in the long run 
;;; it will make it possible for us to make use of the SQL comparison
;;; operatiors that the databases make availible.

;;Left over from previous versions of the system.
;;(defparameter *Months-Enum-Str*  
;;    "enum(\"AUG\", \"SEP\", \"OCT\", \"NOV\", \"DEC\", \"JAN\")")

(defun database-create-files-table (Connection &key (Verbose Nil))
  "Create the files table in the database."
  (db-create-table 
   Connection 
   (database-create-files-schema Connection)
   :If-not-exists t
   :Verbose (if (equalp Verbose 2) t)))

(defun database-create-files-schema (Connection)
  (db-make-table-schema
   Connection "files"
   :Columns (list (db-make-column-schema Connection "id" 'integer :Null Nil :Primary-Key t)
		  (db-make-column-schema Connection "path" 'string :Null Nil)
		  (db-make-column-schema Connection "filename" 'string :Null Nil)
		  (db-make-column-schema Connection "username" 'string :Null Nil)
		  (db-make-column-schema Connection "creationdate" 'db-date) 
		  (db-make-column-schema Connection "starttime" 'integer))  
   :Indicies (list 
	      (db-make-index-schema Connection "PRIMARY" :Table "files" :Columns '("id") :Primary-Key t)
	      (db-make-index-schema Connection "fileuserindex" :Table "files" :Columns '("username")))))
					 
   

;;; --------------------------------------------------------------------------
;;; Dynamic tables.

;;; Creation.
;;; Dynamic tables are constructed from the testset structs that are described
;;; in ./Base/TestSet.cl  Each testset specifies a list of tests by name and 
;;; a ResultType.  This code will generate a table of the form:
;;;  <FileID> <ProblemName> <StartTime> <EndTime> . <Results> where:
;;;  <FileID> is the ID integer of the corresponding file and is a primary key.
;;;  <InstNum> Is a number indicating where in the PI order this instance
;;;                occurred in the problem file.  
;;;  <ProblemName> is the name of the problem that the student is working on
;;;                during this problem instance.
;;;  <Start>  Is the time (within the file) that this ProblemInstance started.
;;;  <End> Is the time (within the file) that this problem instance ended.
;;;  <Results> is a list of test columns each of which will bear the name
;;;     of its corresponding test.  The type of each column will be of the type
;;;     specified in ResultType.  
;;;
;;; The name of the table will be the TestSet name.
;;; The resulting table witll have no index.  


(defun database-create-Dynamic-tables (Connection TestSets &key (Verbose Nil))
  "Create the dynamic tables for each TestSet."
  (dolist (Set TestSets)
    (db-create-table 
     Connection 
     (database-create-dynamic-table-schema Connection Set)
     :Verbose (if (equalp Verbose 2) t))))


(defun database-create-dynamic-table-schema (Connection Set)
  "Create a dynamic table schema."
  (let ((TableName (map-testset->tablename Set)))
    (db-make-table-schema
     Connection TableName
     :Columns (append
	       (list (db-make-column-schema Connection "fileid" 'integer :Null Nil)
		     (db-make-column-schema Connection "instnum" 'integer :Null Nil)
		     (db-make-column-schema Connection "problem" 'string :Null Nil)
		     (db-make-column-schema Connection "start" 'integer :Null Nil) 
		     (db-make-column-schema Connection "end" 'integer :Null Nil)) 
	       (database-map-testset->column-schemas Connection Set))
     :Indicies (list (db-make-index-schema 
		      Connection "PRIMARY" 
		      :Table TableName 
		      :Columns '("fileid" "instnum" "problem")
		      :Primary-Key t)))))
     				 

(defun database-map-testset->column-schemas (Connection Set)
  (mapcar 
   #'(lambda (Test) 
       (db-make-column-schema 
	Connection (format Nil "~a" Test) 
	(if (string-equal (testset-Resulttype Set) "INTEGER")
	    'integer
	  (error "Unrecognized type ~a supplied."))))
   (testset-tests Set)))
  
;;;; --------------------------------------------------------------------------------
;;;; Output
;;;; The output functions are table dependent.  At present each function converts the
;;;; specified input into a sql string and stores it to the table in question.  If 
;;;; it becomes necessary to speed up this process then I may move to using 
;;;; precompiled sql calls or some other technique.  
;;;;
;;;; If the tables are not already explicitly locked by the user then the system will
;;;; lock them automatically with each call.  If they are locked for writing my this
;;;; cliend then no error will be thrown.  If they are llocked for writing by someone
;;;; else then an error will be thrown.  

;;; ----------------------------------------------------------------------------
;;; User table.
;;; Insertions into the users table involves setting up a username and an ID 
;;; and then adding those values into the database itself.  Thi code makes 
;;; use of the sql syntax insertion code defined in Base/ODBC.cl
(defun database-output-user (Connection UserID UserName Condition &key (Verbose nil))
  "Output the user record to the User Table."
  (db-insert-vals 
   Connection "users" 
   :Columns '("ID" "Name" "Condition")
   :values (list (list UserID UserName Condition))
   :Verbose (if (equalp Verbose 2) t)))


;;; In order to prevent unnecessary dups this code will test (if the specified 
;;; flag is set to see whether or not the UserName, and Condition supplied are 
;;; novel.
(defun database-test-user (Connection UserName Condition &key (Verbose nil))
  (if (not 
       (numberp (db-select 
		  Connection 
		  :tables '("users")
		  :Where (db-format-and-exp
			  Connection 
			  (db-format-eq-exp Connection "Name" (db-map-lispobj->dbstr Connection UserName))
			  (db-format-eq-exp Connection "Condition" (db-map-lispobj->dbstr Connection Condition)))
		  :Stream nil
		  :Verbose Verbose)))
      t))




;;; ------------------------------------------------------------------------
;;; File Table.
;;; Insertion into the file table is somewhat more difficult.  Filenames
;;; encode a set amount of user data including the username that logged 
;;; into them, the Month that they were created, the Day that they 
;;; were created, and the start time in HH-MM-SS format.  The database
;;; stored the date and time data in two columns.  Therefore it is
;;; necessary to extract data from the filename itself and to then 
;;; munge it into the necessary form.  Because we cannot necessarily
;;; rely on the file creation dates or other metainformation I am 
;;; strictly making use of the filename info.  
;;;
;;; Because the filename doesnt contain the year the system can take it in
;;; as an optional argument here or nil will be used.
(defun database-output-file (Connection FileID File &key (Year Nil) (Verbose Nil))
  "Store the file values."
  (let* ((Path (concatenate 'string "c:" (directory-namestring File)))
	 (Filename (file-namestring File))
	 (split (split-logfilename FileName)))
    (db-insert-vals
     Connection "files"
     :Columns '("id" "path" "filename" "username" "creationdate" "starttime")
     :Values (list (list FileID Path Filename (lognamestr-username Split)
			 (database-make-output-file-date 
			  (lognamestr-Month Split) (lognamestr-Day Split) Year)
			 (database-make-output-file-time
			  (lognamestr-sec Split) (lognamestr-min Split) (Lognamestr-hour Split))))
     :Verbose (if (equalp Verbose 2) t))))

(defun database-make-output-file-date (Month Day Year)
  "Produce the output date."
  (make-db-date :Month (if (numberp Month) Month 
			 (map-monthStr->MonthNum Month))
		:Day Day
		:Year Year))

(defun database-make-output-file-time (Sec Min Hour)
  "Make the output file date."
  (convert-htime->secs
   (make-htime :Sec (read-from-string Sec)
	       :Min (read-from-string Min)
	       :Hour (read-from-string Hour))))

;;(defun odbc-format-date (Month Day Year)
;;  "Compile a date string accounting for the month possibly being a string."
;;  (format Nil "~4,'0,D-~2,'0,D-~2,'0,D" Year
;;	  (if (numberp Month) Month 
;;	    (map-monthStr->MonthNum Month))
;;	  Day))
;;
;;(defun odbc-format-time (Hour Min Sec)
;;  "Produce an encoded time string."
;;  (format Nil "~2,'0,D:~2,'0,D:~2,'0,D" Hour Min Sec))


;;; ------------------------------------------------------------------------------
;;; Dynamic tables.
;;; Adding in data to the tables is accomplished by constructing an insert string
;;; using the fileid values and the tests.  If the table is not locked before this
;;; is done then the system will lock the table, make the insert and then unlock 
;;; it unless a lock is made.
;;;
;;; This may not be the most efficient way to go about the process.  It might be 
;;; more efficient the pre-load the construct and then add in the new values each
;;; time.  For now I will test things this way and see how they go in the long run.
(defun database-output-results (Connection FileID InstNum ProblemName Start End TestSet Results &key (Verbose Nil))
  "Open the dynamic table for output and store the reults."
  (db-insert-vals 
   Connection (map-testset->tablename TestSet)
   :Columns (append (list "fileid" "instnum" "problem" "start" "end") (testset-tests TestSet))
   :Values (list (append (list FileID InstNum ProblemName 
			       (convert-htime->secs Start) 
			       (convert-htime->secs End)) 
			 Results))
   :Verbose (if (equalp Verbose 2) t)))



;;  (odbc-insert 
;;   (map-testset->tablename TestSet) DBName Connection
;;   :Columns (append (list "FileID" "InstNum" "Problem" "Start" "End") (testset-tests TestSet))
;;   :Record (append (list FileID InstNum ProblemName Start End) Results)
;;   :Verbose Verbose))



;;;; ---------------------------------------------------------------------------
;;;; Access
;;;; The functions here are used to access portions of the final database for
;;;; other uses.  I have placed them here so that they will be tied in with the
;;;; design of the database results.  This is merely for convenience purposes.
;;;;
;;;; These will be called below.

;;(defun 









#|;;;; =========================================================================
;;;; Database Specific Code.
;;;; If the user elects to store the results in a database then the system will 
;;;; begin by opening a connection to the database and generating or testing
;;;; the necessary files before passing control to the main processing loop.
;;;; the necessary result storage calls will be passed in as closures.  When the
;;;; main loop completes then the connection to the database will be closed.
;;;; The code is accessed using Allegro's ODBC connection which make use of 
;;;; Microsoft's odbc driver manager.
;;;;
;;;; This code assumes that the user has access to an ODBC compliant database 
;;;; server and the right to generate new databases and tables within those 
;;;; databases or that the tables already exist and the user has the right
;;;; to append to them.  
;;;;
;;;; If any errors occur due to the ODBC setup then the system will throw the 
;;;; error to the user.  
;;;;
;;;; The results of processing will be stored in several tables within the
;;;; database in the following forms.
;;;; Two static tables:
;;;;   USER:  A record of all the usernames in the system.
;;;;    this table is of the form  Name=CHAR(255), ID=INTEGER, GROUP=CHAR(255)
;;;;    where Integer is a primary key and Name is taken from the folder.
;;;;    group is an optional value that I will arrainge to supply as a keyword
;;;;    argument from the toploop or to extract from the pathname, I have not
;;;;    yet decided on how best to do this.  
;;;;
;;;;   FILE:  A record of all the files that are examined including pathnames
;;;;    usernames, the userids correspoinding to the files, the date and time
;;;;    that the file was begun and the length of the file that was processed.
;;;;    the spec is...
;;;;
;;;; The remaining tables will be determined by the testsets.  A new table will
;;;; be generated for each testset where each test is assigned a column within
;;;; the table and each record in the table corresponds to a separate file result.
;;;; in the future I would like to modify this so that each problem instance within
;;;; the file gets a separate result but that may not be necessary.  
;;;;
;;;; The name of each table will be taken from the result set and be of the form:
;;;;   <TestSet-Name>_Results
;;;;
;;;; The form of each table is:
;;;;   FILEID=INTEGER . RESULT(N)=TestSet-ResultType.
;;;;
;;;; NOTE:: that in future I am planning to extend this so that the results 
;;;; will be stored according to problem instance at a later date.



;;; Database-test-logfiles
;;; Given a root pathname to a set of user-driectory sorted logfiles, a list
;;; of tests an ODBCName and a Database name as well as IFExists settings
;;; open a connection to the specified ODBC datasource throwing an error if 
;;; necessary and perform any setup and testing operations that are necessary
;;; before passing control to the main loop along with closures for the output.
;;; Once the main loop is completed then close the ODBC connection.
;;;
;;; In the future this code will lock tables and then unlock them at the end 
;;; of the run or the system will lock them in the prep-output code.

(defun odbc-test-logfiles (Root Tests DBName ODBCName IFExists 
			 &key (Condition Nil) (Year Nil) (Verbose Nil))
  "Prepare the database connection and iterate."
  (let ((Connection (odbc-prep-output Tests DBName ODBCName IFExists :Verbose Verbose)))
    (odbc-lock-tables Tests DBName Connection :Verbose Verbose)
    (main-test-logfiles
     Root Tests
     #'(lambda (Userid UserName UserCond) (odbc-output-user UserID Username USerCond DBName Connection :Verbose Verbose))
     #'(lambda (FileID File FileYear) (odbc-output-file FileID File DBName Connection :Year FileYear :Verbose Verbose))
     #'(lambda (FileID InstNum ProblemName Start End TestSet Results) 
	 (odbc-output-results FileID InstNum ProblemName Start End TestSet Results DBName Connection :Verbose Verbose))
     :Condition Condition
     :Year Year
     :Verbose Verbose)
    (odbc-unlock-tables Connection :Verbose Verbose)
    (odbc-disconnect Connection :Verbose Verbose)))


;;; --------------------------------------------------------------------------------
;;; odbc-prep-output
;;; Preparing the database output is a matter of opning the database connection
;;; testing to see if the specified database exists and, if it does whether or not
;;; the tables within it exist.  The behavior of the system depends upon the 
;;; overwrite format that the students chose.  
;;;
;;; If the user specified :Error then the system will throw an error if a database
;;; of the same name already exists.  If not then the system will generate the 
;;; specified database and populate it with the three tables.  
;;;
;;; If the user specified :Overwrite then the system will overwrite any preexisting
;;; database with a new one and then generate the tables as necessary.
;;;
;;; If the user specified :Append then the system will throw an error if the database
;;; and tables are not present or proceed as normal if they do.  
;;;
;;; NOTE:: For now the system accpets only the default value of :Error when it is operating.
;;;
;;; This code assumes that an ODBC datasource with the appropriate name exists and
;;; that it can be accessed using the supplied name.  Any ODBC errors will be thrown
;;; to the user.  The resulting database connection will be returned.

(defun odbc-prep-output (Tests DBName ODBCName IFExists &key (Verbose Nil))
  "Prepare the database for output."
  (let ((Connection (odbc-connect ODBCName :Verbose Verbose)))
    (case IfExists
      (:Error (odbc-prep-output-error Tests DBName Connection :Verbose Verbose))
      (:Overwrite (odbc-prep-output-overwrite Tests DBName Connection :verbose Verbose))
      ;;(:Append (odbc-prep-output-append Connection DBName Tests)))
      (t (error "Unrecognized IFExists value: ~a supplied to database code." IFExists)))
    Connection))


;;; odbc-prep-output-error
;;; If the user has selected error then test to see if the database exists.  
;;; If it does then throw an error.  If not then generate the database and 
;;; each individual table before returning.
(defun odbc-prep-output-error (Tests DBName Connection &key (Verbose Nil))
  "prep the output with errors."
  (when (odbc-odbc-exists? DBName Connection :Verbose Verbose)
    (error "Error Database ~a already exists." DBName))
  (odbc-create-db DBName Connection :Verbose Verbose)
  (odbc-create-users-table DBName Connection :Verbose Verbose)
  (odbc-create-files-table DBName Connection :Verbose Verbose)
  (odbc-create-dynamic-tables Tests DBName Connection :Verbose Verbose))


;;; odbc-prep-output-overwrite
;;; If the user has elected to overwrite their data then the system will
;;; test to see if a database already exists with the same name.  If it
;;; does then the system will send a drop-database command to the server
;;; to remove it and then proceed to re-create the tables.  This will drop
;;; all data within the database even if it is in differing tables.
;;;
;;; The reason that I opted for this restrictive policy is that the excess 
;;; tables will still be linked into the data that will be replaced so it
;;; makes more sense to remove them.  For adding to a database use append.
(defun odbc-prep-output-overwrite (Tests DBName Connection &key (Verbose Nil))
  "Prepare the database with overwriting."
  (when (odbc-odbc-exists? DBName Connection :Verbose Verbose)
    (if Verbose (format t "DB:: Error Database ~a already exists." DBName))
    (odbc-drop-db DBName Connection))
  (odbc-create-db DBName Connection :Verbose Verbose)
  (odbc-create-users-table DBName Connection :Verbose Verbose)
  (odbc-create-files-table DBName Connection :Verbose Verbose)
  (odbc-create-dynamic-tables Tests DBName Connection :Verbose Verbose))






;;;; ----------------------------------------------------------------------
;;;; Tables
;;;; As I said above the system generates two static tables USER and 
;;;; FILE and one dynamic table for each individiaul testset.  The 
;;;; format for the static tables are specified as parameters below.
;;;;
;;;; The output functions for each table are also listed below.

;;; -----------------------------------------------------------------------
;;; Locking and unlocking.
;;; Each time that the system makes an entry to or deletes a value from a 
;;; table it must initiate a lock on the table and then unlock it when it
;;; is done.  In order to speed things up the system will go ahead and lock
;;; all the relavent tables before it starts generating data and then 
;;; unlocks them when it is done.  This should reduce the runtime of the
;;; system overall by reducing the time that each lock takes.  
;;;
;;; NOTE:: When a lock is sent to MySQL all previous locks are removed
;;;        so this has to be done with a single ODBC call.

;;; Start here testing.

(defun odbc-lock-tables (Tests DBName Connection &key (Verbose Nil))
  "Lock all of the tables in this database."
  (if Verbose (format t "DB:: Locking tables.~%"))
  (odbc-lock-tables
   Connection 
   :Write (odbc-format-tableids 
	   DBName (append (list "Users" "Files") 
			  (map-testsets->tablenames Tests)))
   :Verbose Verbose))
		   
  

(defun odbc-unlock-tables (Connection &key (Verbose Nil))
  "Unlock all of the tables currently locked by this thread."
  (if Verbose (format t "DB:: Unlocking tables.~%"))
  (odbc-unlock-tables Connection :Verbose Verbose))
 

;;; ------------------------------------------------------------------------
;;; USer Table
;;; The users table contains three fields: 
;;;   ID: an integer that is also a primary key,
;;;   Name: a char(255) field that is the usert's login,
;;;   Condition: an additional char that is used to collect users by year.
;;;
;;; ID is defined as a non-null primary key.  Later on I will add an index
;;; to make searching on the student's name and group faster if it becomes
;;; an issue.  
;;; 
;;; For now the data is simply loaded as-is.

(defun odbc-create-users-table (DBName Connection &key (Verbose Nil))
  "Create the user table in the database."
  (odbc-create-table
   "Users" DBName Connection 
   (list (odbc-format-columnspec "ID" "INTEGER" :Not-Null T :Primary-Key T) 
	 (odbc-format-columnspec "Name" "CHAR(64)" :not-Null t)
	 (odbc-format-columnspec "Condition" "CHAR(64)"))
   :Indicies (list (odbc-format-indexspec 
		    "UsersIndex" '("Name(10)" "Condition(10)")))
   :Verbose Verbose))


;;; ---------------------------------------------------------------------
;;; File Table.
;;; The file table contains the following fields:
;;;   ID:        A numeric file ID.
;;;   Path:      The path containing the file including the drive.
;;;   Filename:  A string containing the file name.
;;;   Username:  The username encoded in the file name.
;;;   Day:       The day that the file was created.
;;;   Time:      The time that the file was created.
;;;   
;;;  ID is the primary key to the table.
;;;  The table also contains an index on the username in order to 
;;;  speed up searches.
;;;
;;; It will be necessary to munge the date information and the 
;;; times into a format that can be read by the ODBC form and 
;;; this will take additional work.  However in the long run 
;;; it will make it possible for us to make use of the SQL comparison
;;; operatiors that the databases make availible.

;;Left over from previous versions of the system.
;;(defparameter *Months-Enum-Str*  
;;    "enum(\"AUG\", \"SEP\", \"OCT\", \"NOV\", \"DEC\", \"JAN\")")

(defun odbc-create-files-table (DBName Connection &key (Verbose Nil))
  "Create the files table in the database."
  (odbc-create-table
   "Files" DBName Connection 
   (list (odbc-format-columnspec "ID" "INTEGER" :Not-Null t :Primary-Key t)
	 (odbc-format-columnspec "Path" "CHAR(128)" :Not-Null t)
	 (odbc-format-columnspec "Filename" "char(64)" :Not-Null t)
	 (odbc-format-columnspec "Username" "char(64)" :Not-Null t)
	 (odbc-format-columnspec "CreationDate" "DATE")
	 (odbc-format-columnspec "StartTime" "TIME")) 
   :Indicies (list (odbc-format-indexspec "FileUserIndex" '("Username(10)")))
   :Verbose Verbose))


;;; --------------------------------------------------------------------------
;;; Dynamic tables.

;;; Creation.
;;; Dynamic tables are constructed from the testset structs that are described
;;; in ./Base/TestSet.cl  Each testset specifies a list of tests by name and 
;;; a ResultType.  This code will generate a table of the form:
;;;  <FileID> <ProblemName> <StartTime> <EndTime> . <Results> where:
;;;  <FileID> is the ID integer of the corresponding file and is a primary key.
;;;  <InstNum> Is a number indicating where in the PI order this instance
;;;                occurred in the problem file.  
;;;  <ProblemName> is the name of the problem that the student is working on
;;;                during this problem instance.
;;;  <Start>  Is the time (within the file) that this ProblemInstance started.
;;;  <End> Is the time (within the file) that this problem instance ended.
;;;  <Results> is a list of test columns each of which will bear the name
;;;     of its corresponding test.  The type of each column will be of the type
;;;     specified in ResultType.  
;;;
;;; The name of the table will be the TestSet name.
;;; The resulting table witll have no index.  

;;; Start Here.

(defparameter **Dynamic-table-StaticCols**
    (list (odbc-format-columnspec "FileID" "INTEGER" :Not-Null t)
	  (odbc-format-columnspec "InstNum" "INTEGER" :Not-Null t)
	  (odbc-format-columnspec "Problem" "CHAR(64)" :Not-Null t)
	  (odbc-format-columnspec "Start" "TIME" :Not-Null t)
	  (odbc-format-columnspec "End" "TIME" :Not-Null t)))
	  
	  
(defun odbc-create-Dynamic-tables (TestSets DBName Connection &key (Verbose Nil))
  "Create the dynamic tables for each TestSet."
  (dolist (Set TestSets)
    (odbc-create-table
     (map-testset->tablename Set)
     DBName Connection 
     (append **Dynamic-Table-StaticCols** 
	     (map-testset->columnspecs Set))
     :Verbose Verbose)))


;;;; --------------------------------------------------------------------------------
;;;; Output
;;;; The output functions are table dependent.  At present each function converts the
;;;; specified input into a sql string and stores it to the table in question.  If 
;;;; it becomes necessary to speed up this process then I may move to using 
;;;; precompiled sql calls or some other technique.  
;;;;
;;;; If the tables are not already explicitly locked by the user then the system will
;;;; lock them automatically with each call.  If they are locked for writing my this
;;;; cliend then no error will be thrown.  If they are llocked for writing by someone
;;;; else then an error will be thrown.  


;;; ----------------------------------------------------------------------------
;;; User table.
;;; Insertions into the users table involves setting up a username and an ID 
;;; and then adding those values into the database itself.  Thi code makes 
;;; use of the sql syntax insertion code defined in Base/ODBC.cl

(defun odbc-output-user (UserID UserName Condition DBName Connection &key (Verbose nil))
  "Output the user record to the User Table."
  (odbc-insert 
   "Users" DBName Connection 
   :Columns '("ID" "Name" "Condition")
   :Record (list UserID UserName Condition)
   :Verbose Verbose))



;;; ------------------------------------------------------------------------
;;; File Table.
;;; Insertion into the file table is somewhat more difficult.  Filenames
;;; encode a set amount of user data including the username that logged 
;;; into them, the Month that they were created, the Day that they 
;;; were created, and the start time in HH-MM-SS format.  The database
;;; stored the date and time data in two columns.  Therefore it is
;;; necessary to extract data from the filename itself and to then 
;;; munge it into the necessary form.  Because we cannot necessarily
;;; rely on the file creation dates or other metainformation I am 
;;; strictly making use of the filename info.  
;;;
;;; Because the filename doesnt contain the year the system can take it in
;;; as an optional argument here or nil will be used.
(defun odbc-output-file (FileID File DBName Connection &key (Year Nil) (Verbose Nil))
  "Store the file values."
  (let* ((Path (concatenate 'string "c:" (directory-namestring File)))
	 (Filename (file-namestring File))
	 (split (split-logfilename FileName)))
    (odbc-insert
     "Files" DBName Connection
     :Columns '("ID" "Path" "Filename" "Username" "CreationDate" "StartTime")
     :Record (list FileID Path Filename (lognamestr-username Split)
		   (odbc-format-date (lognamestr-Month Split) (lognamestr-Day Split) Year)
		   (odbc-format-time (Lognamestr-hour Split) (lognamestr-min Split) 
				     (lognamestr-sec Split)))
     :Verbose Verbose)))
	   



;;; ------------------------------------------------------------------------------
;;; Dynamic tables.
;;; Adding in data to the tables is accomplished by constructing an insert string
;;; using the fileid values and the tests.  If the table is not locked before this
;;; is done then the system will lock the table, make the insert and then unlock 
;;; it unless a lock is made.
;;;
;;; This may not be the most efficient way to go about the process.  It might be 
;;; more efficient the pre-load the construct and then add in the new values each
;;; time.  For now I will test things this way and see how they go in the long run.
(defun odbc-output-results (FileID InstNum ProblemName Start End TestSet Results DBName Connection &key (Verbose Nil))
  "Open the dynamic table for output and store the reults."
  (odbc-insert 
   (map-testset->tablename TestSet) DBName Connection
   :Columns (append (list "FileID" "InstNum" "Problem" "Start" "End") (testset-tests TestSet))
   :Record (append (list FileID InstNum ProblemName Start End) Results)
   :Verbose Verbose))



;;;; ---------------------------------------------------------------------------
;;;; Access
;;;; The functions here are used to access portions of the final database for
;;;; other uses.  I have placed them here so that they will be tied in with the
;;;; design of the database results.  


|#







;;;; ==========================================================================
;;;; File Specific Code.
;;;; IF the user elects to store the results in a database then the system will
;;;; generate a single file for each table that mirrors the format of the code 
;;;; above.  The Resulting files are intended to match the database tables that
;;;; would be generated using the database output code.  The fields within the 
;;;; files will be comma separated.  
;;;;
;;;; The IFExists argument that is passed in by the user will be passed on to 
;;;; the file opening code which will respond accordingly.
;;;; 
;;;; If the user elects to use file output then the system will generate the 
;;;; files and add tests if they do not already exist.  It will then pass the
;;;; same type of output functions to the main loop that are already passed in
;;;; by the database code.

(defun file-test-logfiles (Root Tests OutputPath IFExists 
			   &key (Condition Nil) (Year Nil) 
				(Handle-Set-Scores Nil) (Verbose Nil))
  "Store the output information into files."
  (file-prep-output OutputPath Tests IFExists :Verbose Verbose)
  (main-test-logfiles
   Root Tests
   #'(lambda (UserID Username USerCond) (file-output-user UserID Username UserCond OutputPath :Verbose Verbose))
   #'(lambda (FileID File FileYear) (file-output-file FileID File OutputPath :Year FileYear :Verbose Verbose))
   #'(lambda (FileID InstNum PName Start End TestSet Result) 
       (file-output-results FileID InstNum PName Start End TestSet Result OutputPath :Verbose Verbose))
   :Condition Condition
   :Year Year
   :Verbose Verbose
   :Handle-Set-Scores Handle-Set-Scores))
   


;;;; --------------------------------------------------------------------
;;;; Output Prep
;;;; Preparing the file output is a matter of generating the individual files or 
;;;; at least ensuring that they can be appended to at runtime.  The IFExists
;;;; value will be passed on to the file creation calls as necessary.
;;;;
;;;; The IFEixists setting causes the following behavior:
;;;;  Error:  The system will throw an error if any of the output files already exist.
;;;;  Overwrite:  The system will overwrite all files irrespective of their existence.
;;;;  Append:  The system will generate what new files are necessary and append to 
;;;;           those that already exist.  Note that it will not generate a new header
;;;;           row in a preexisting file.

(defun file-prep-output (OutputPath Tests IFExists &key (Verbose Nil))
  "Prepare the file path for output."
  (ensure-directories-exist OutputPath :Verbose Verbose)
  (if (equalp IFExists :Error) (file-prep-error-probe OutputPath Tests))
  (file-prep-outputF "Users" "ID,Name,Condition" OutputPath IFExists :Verbose Verbose)
  (file-prep-outputF "Files" "ID,Path,FileName,Username,CreationDate,StartTime"
		     OutputPath IFExists :Verbose Verbose)
  (file-prep-Dynamic-files Tests OutputPath IFExists :verbose Verbose))


;;; If the user has selected :Error as their IFExists value then we will need to 
;;; probe the specified files to see if any of them exist.  If any of the static
;;; or dynamic files exist at the specified directory then an error will be thrown
;;; if not then the system will return nil.
(defun file-prep-error-probe (OutputPath Tests)
  (dolist (File (append (list "Users" "Files") (mapcar #'testset-name Tests)))
    (if (probe-file (format Nil "~a~a.~a" (namestring OutputPath) 
			    File **Output-File-Ext**))
	(error "FILE:: Designated Output file ~a.~a Already Exists!" 
	       File **Output-File-Ext**))))


;;; Prepare the dynamic files by generating a new file for each testset as
;;; necessary and storing the results.
(defun file-prep-dynamic-files (Tests OutputPath IFExists &key (Verbose Nil))
  (dolist (Set Tests)
    (file-prep-outputF 
     (format Nil "~a" (Testset-name Set))
     (format Nil "FileID,InstNum,ProblemName,Start,End~{,~a~}" (testset-tests Set)) 
     OutputPath IFExists :Verbose Verbose)))


;;; Given a filename, a header row string, an IFExists setting, and oyutput path 
;;; and a verbose setting test to see if the file exists.  If Verbose is t then 
;;; inform the user and take appropriate action either throwing an error (:Error)
;;; skipping the file (:Append) or overwriting it (:Overwrite).
(defun file-prep-OutputF (Name Header OutputPath IFExists &key (Verbose Nil))
  "Print out the Outputfile."
  (let ((path (concatenate 'string (namestring OutputPath) Name **Output-File-Ext**)))
    (case IFExists
      (:APPEND (file-prep-outputf-append Path Header :Verbose Verbose))
      (:ERROR (file-prep-outputf-error Path Header :Verbose Verbose))
      (:OVERWRITE (file-prep-outputf-overwrite Path Header :Verbose Verbose))
      (t (error "FILE:: Unknown IFExists value ~a supplied." IFExists)))))


;;; If the user selected :Append then the system will avoid generating or adding a 
;;; new header row to any files that already exist.  
(defun file-prep-outputf-append (Path Header &key (Verbose Nil))
  (if (probe-file Path) 
      (if Verbose (format t "FILE:: ~a already exists, will be skipped.~%" Path))
    (progn (if Verbose (format t "FILE:: ~a does not exist, will be created.~%" Path))
	   (file-output-line Path Header :If-Exists :Append))))


;;; If the user selected :Error then the sysem will throw an error if a file already
;;; exists or will inform the user if it does not.
(defun file-prep-outputf-error (Path Header &key (Verbose Nil))
  (if (probe-file Path) (error "FILE:: ~a already exists.~%" Path)
    (progn (if Verbose (format t "FILE:: ~a does not exist, will be created.~%" Path))
	   (file-output-line Path Header :If-Exists :ERROR))))


;;; IF the user selected overwrite then inform them of the issue and, if the 
;;; files exist overwrite them before use.
(defun file-prep-outputf-overwrite (Path header &key (Verbose Nil))
  (if Verbose
      (if (probe-file Path) 
	  (format t "FILE:: ~a exists will be overwritten.~%" Path)
	(format t "FILE:: ~a does not exist, will be created.~%" Path)))
  (file-output-line Path Header :If-Exists :Overwrite))





;;;; ---------------------------------------------------------------------------------
;;;; Output
;;; Storing the user data to the file is simply a matter of printing it in a string
;;; form for later use.  If verbose is t then the value will also be echoed to the 
;;; screen.  This func will always return t to indicate that the user is new on
;;; the grounds that anything else is illogical.
(defun file-output-user (UserID Username Condition OutputPath &key (Verbose Nil))
  "Print out the user information."
  (let ((path (concatenate 'string OutputPath "Users" **Output-File-Ext**))
	(Str (format Nil "~a,~a,~a" UserID USerName Condition)))
    (if Verbose (format t "FILE:: User.~a <= ~a~%" Path Str))
    (file-output-line Path Str)
    t))


;;; Storing the file data is a matter of echoing the file info as a line to the
;;; files.**output-file-ext** file.
(defun file-output-file (FileID File OutputPath &key (Year Nil) (Verbose Nil))
  "Print out the file info."
  (let* ((path (concatenate 'string OutputPath "Files" **Output-File-Ext**))
	 (FileName (file-namestring File))
	 (split (split-logfilename FileName))
	 (Str (format Nil "~a,\'~a\',\'~a\',\'~a\',~a,~a" FileID Path FileName
		      (lognamestr-username Split)
		      (compile-date-str (lognamestr-Month Split) (lognamestr-Day Split) Year)
		      (compile-time-str (Lognamestr-hour Split) (lognamestr-min Split) 
					(lognamestr-sec Split)))))

    (if Verbose (format t "FILE:: User.~a <= ~a~%" Path Str))
    (file-output-line Path Str)))


;;; Dynamic file results are simply done by printing out the tests in a comma-
;;; separated list.  
(defun file-output-results (FileID InstNum ProblemName StartTime EndTime TestSet Result OutputPath &key (Verbose Nil))
  "Print out the result data to the file."
  (let ((path (concatenate 'string OutputPath (format Nil "~a" (testset-name TestSet))
			   **Output-File-Ext**))
	(Str (format Nil "~a,~a,~a,~a,~a,~{,~a~}" FileID InstNum ProblemName StartTime EndTime Result)))
    (if Verbose (format t "FILE:: ~a <= ~a~%" Path Str))
    (file-output-line Path Str)))
	      

;;; Printing a line to the files is accomplished by use of this generic code.  
;;; This avoids me having to re-create things on a regular basis.
(defun file-output-line (Path Line &key (if-does-not-exist :CREATE)
					(if-exists :APPEND))
  (with-open-file (File Path
		   :Direction :Output
		   :If-Exists IF-exists
		   :If-Does-Not-Exist if-does-not-exist)
    (format File "~a~%" Line)))





;;;; ==========================================================================
;;;; Main testing loop.
;;;; This is the main testing loop.  It takes as its arguments a root pathname 
;;;; listing a set of logfile directories, a set of testsets, and a set of 
;;;; output functions as well as several keyword arguments.  The ouptut 
;;;; functions will be used to store the userids, fileids and other information
;;;; as they are extracted from the files.  That extraction is done by the 
;;;; inner loops that hve been specified below.  
;;;;
;;;; The system operates by iterating over each directory taking the username
;;;; from the directory and storing it using the UserOutF function.  Once the 
;;;; user is stored then the system will recursively descend into the 
;;;; directory in order to test and store the files.
;;;;
;;;; the directory testing is handled by the mtl-subdir function which takes 
;;;; the base file ID, a directory, tests and the output functions as input
;;;; before it iterates over the files within the dir.  The main loop 
;;;; maintains the fileID in order to ensure that they are globally unique.
;;;;
;;;; I did not use closures for ease of debugging purposes.  I also chose 
;;;; not to rely on the database or storage functions to supply the ID 
;;;; because it was necessary to have it at runtime.  In the future the
;;;; structure of this process may be modified to take in baseline userIDs 
;;;; and fileids so that multiple runs may store safely in the same files
;;;; and databases.
;;;;
;;;; The actual file testing is handled by the test-logfile function.  It takes
;;;; the testsets and returns a matching list of results that will then be 
;;;; stored into the output files.  
;;;;
;;;; Arguments:
;;;;  Root:   A root path containing directory-sorted logfiles.
;;;;  Tests:  A set of testsets to be used.
;;;;  UserOutF:  A user output function that will be used to store user data.
;;;;             Args:  UserID (a numerical ID) 
;;;;                    UserName (The Username)
;;;;                    UserCond (The User's Condition.)
;;;;  FileOutF:  A function that will be used to store the file results.
;;;;             Args: FileID:  An integer ID for the files.
;;;;                   File:   The file iself with path info.
;;;;                   FileYear:  The year that the file was created.
;;;;  ResultOutF: A function that will be used to store the results of each testset.
;;;;              Args:  FileID:  A numerical ID indicating the file this is from.
;;;;                     TestSet:  The testset being dealt with.
;;;;                     Results:  The list of result alues for the testset.
;;;;  &key
;;;;   Condition:  A condition value for the users.
;;;;   Year:  AS yaer value for the files.
;;;;   Verbose:  If set to t then the system will print out the sql queries as they
;;;;             are sent.
;;;;   Base-Student-ID:  An integer value that will be used as the base value for the
;;;;                     student IDs.  They will be incremented from that point.
;;;;   Base-File-ID: An integer that will be used as a base value for the file IDs.
;;;;                 As with the base student ID this value defaults to 0.
;;;;
;;;; When the system starts each directory list it will print out header info to
;;;; the user so that they can track the progress.  IF the user selected :Verbose
;;;; then the system will print out the sql queries or file commands as they are 
;;;; sent out.  
;;;;
;;;; When the system returns it will return a list of two values the first is the 
;;;; number of students processed, the second is the number of files.  
;;;;
;;;; Each of the sublevels in this code will be wrapped in handler-cases so that
;;;; errors at any given level will be trapped and then returned.  The purpose 
;;;; of this is to avoid any errors for any directories or files.  


;;; Because the fileID must be unique, the system will start an ID count at 0 
;;; and then pass the current ID value to the file processor.  That function
;;; wil incrment the id number as necessary and return the resulting value to 
;;; the main loop for re-use.  
;;;
;;; The keyword arguments base-StudentID and base-FileID will be used to 
;;; implement appending when necessary wherin the system will 
;;;
;;; This code is designed to update the userid based upon the UserOut Function
;;; if the result is t then this was a new user and the userid count will be 
;;; incremented.  If not then this will not increment the user id.
(defun main-test-logfiles (Root Tests UserOutF FileOutF ResultOutF 
			   &key (Condition Nil) (Year Nil) (Verbose Nil)
				(Handle-Set-Scores Nil)
				(Base-UserID 0) (Base-FileID 0))
  "Iterate over the files and directories carrying out the main output code."
  (format t "Starting Files: ~a~%" Root)
  ;;(format t "Using Tests:    ~a ~%" Tests)
  
  (let ((UserID Base-UserID) Dir (Dirs (directory Root)) (FileID Base-FileID))
    (dotimes (N (length Dirs))

      ;; Wrap the directory code in a handler-case so that any
      ;; directory errors will not crash the system.
      (handler-case 
	  (progn
	    (setq Dir (nth N Dirs))
	    ;;(setq UserID (+ N Base-UserID))
	    
	    ;; Print out the information.
	    (format t "~2%################################################~%")
	    (format t "Entering Directory: ~a ~a~%" (namestring Dir) UserID)
      
	    ;; Store the user value and recurse
	    (format t "Storing User ~a~%" (file-namestring Dir))
	    (if (funcall UserOutF UserID (file-namestring Dir) Condition)
		(incf UserID))
	    (setq FileID 
	      (mtl-subdir (+ 1 FileID) Dir Tests FileOutF ResultOutF 
			  :Handle-set-scores Handle-Set-Scores 
			  :Year Year :Verbose Verbose)))
	
	;; Return the error if one occurs.
	(error (E) (format t "**Error**:: ~a~%" E))))
    
    ;;; Return the user count and File ID.
    (list UserID FileID)))


;;; Once we have selected a directory recurse into it and begin iterating over the 
;;; files within.  For each file add an entry to the file table and then execute 
;;; all of the tests on it and store the results.  
;;;
;;; The function takes as arguments the Base FileID, The directory to be tested, the
;;; tests to be executed, the file and result output functions and an optional year
;;; argument and a verbose flag.
(defun mtl-subdir (Base-FileID Dir Tests FileOutF ResultOutF 
		   &key (Handle-Set-Scores Nil) (Year Nil) (Verbose Nil))      
  "Test each file in the system."
  (let (File (FileID Base-FileID) 
	(Files (directory (concatenate 'string (namestring Dir) "/*.log"))))
    (dotimes (N (length Files))
      (setq FileID (+ N Base-FileID))
      (setq File (namestring (nth N Files)))
      (format t "### Starting ~a ~a ###~%" FileID File)
      (funcall FileOutF FileID File Year)  ;; store the file itself.
      (mtl-file ResultOutF FileID File Tests :Handle-set-scores Handle-Set-Scores :Verbose Verbose)
      (format t "### Done ~a ###~%" File))
    FileID))



;;; Given a storage function, a file ID and a filename as well as a list
;;; of tests execute the tests on the file obtaining the results and then
;;; store those results using the outputfunc.  The testing function will
;;; return the results as a list of the form:
;;;
;;;  (<ProblemInstance0> ... <ProblemInstanceM>)
;;;
;;; Where each problem Instance is a list of the form:
;;; 
;;;  ((<ProblemName> <StartTime> <EndTime> 
;;;   ((<TestSet0> . <Results0>) ... (<TestSetN> . <ResultsN>)))
;;;
;;; This information will be stored via the ResultOutF that has been
;;; supplied.  This function will be iterated over the results in each
;;; problem instance, supplied with the relavent components and stored.
(defun mtl-file (ResultOutF FileID Filename Tests &key (Handle-Set-Scores Nil) (Verbose Nil)) 
  "Iterate over the files and store the results."
  (let (Name Start End R (Results (test-logfile FileName Tests 
						:Handle-set-scores Handle-Set-Scores
						:Verbose Verbose)))
    (dotimes (InstNum (length Results))
      (setq R (nth InstNum Results))
      (setq ProblemName (nth 0 R))
      (setq Start (nth 1 R))
      (setq End (nth 2 R))
      (dolist (Test (nth 3 R))
	;;(format t "Sending: ~a ~a ~a ~a ~a ~a ~a ~2%"
	;;FileID N Name Start End (car Test) (cdr Test))))))
	(funcall ResultOutF FileID InstNum ProblemName Start End (car Test) (cdr Test))))))


;;; --------------------------------------------------------------------------
;;; test logfile.
;;; This is the core code for the testing system.  This function takes in a
;;; logfile name and then iterates over it to calculate and store the results
;;; 
;;; This function takes the following args:
;;;   Filename:  A full pathname to the file that we want to open.
;;;   Tests: a list of testsets that will be executed on the file.
;;;   &key
;;;    Verbose:  An output control.
;;; 
;;; This code operates by opening a new cmdreader for the file, and 
;;; performing any setup necessary to run the help system or maintain other 
;;; stte information as it runs.  It then iterates through the file pulling 
;;; each cmd out of the file and executing all of the testsets on it.  
;;;
;;; The results will be grouped by problem instances.  Each time that the 
;;; student opens or closes a problem, a new result set will be started and
;;; the tests will be run.  When each problem instance is started it will 
;;; be appended with the problem that is being worked on (with the initial
;;; segment before the problem opens and all non-problem segments labeled 
;;; nil).  It will also be appended with the time that the segment began and
;;; the time that it ended.  This information will be used to sort the 
;;; segments as necessary later on.  
;;;
;;; The results will be of the form:
;;;  (<SegResult0> ... <SegResultN>)
;;; Where each Segresult is of the form:
;;;  (<ProblemName> <StartTime> <EndTime> . <Results>)
;;;
;;; Once the test function has opened the cmdreader it will call the recursive
;;; tl-read-pi function.  This function will read off the problem instances 
;;; from the file and retuen once the file has been closed.  As it runs it will
;;; execute the tests and return the list of results once the run terminates.
;;;
;;; NOTE:: The Time value that is added to the results represents not the total
;;;  time between the open and close-problemfile cmds plut the leads time for 
;;;  the initial cmd.
;;;
;;; NOTE:: The system does not perform any form of error logging to keep track
;;; of what problems have occured although that might be a useful thing to try 
;;; in future soi that a more automated test of errors could be made.
;;;
;;; NOTE:: Because of its recursive nature, if any error occurs when processing
;;;  a given log all the results from the log will be lost.  Thus far the amount
;;;  of missing data has not been statistically significant.
;;;
;;; NOTE:: Because the test-logfile function traps errors I found it necessary
;;;  to produce a latter form that does not trap the error for debugging purposes.
;;;  In order top debug the runtime (and make the errors realized) you need to 
;;;  comment out the function below and then uncomment the one below that. 
(defun test-logfile (Filename Tests &key (Handle-Set-Scores Nil) (Verbose Nil))
  "Test the specified logfile."
  (declare (ignore Verbose))
  (let (Reader Results)
    (handler-case
	(progn 
	  (setq Reader (open-cmdreader Filename :ignore-check-entries t 
				       :handle-set-scores Handle-Set-Scores))
	  (setq Results (tl-read-nil-pi (make-htime) (read-next-cmd Reader) Reader Tests))
	  (close-cmdreader Reader))
  
      ;; If an error is encountered then we need to close the cmdreader
      ;; if possible and to inform the user of the error.
      (error (E) (progn (close-cmdreader Reader) (format t "**Error**:: ~a~%" E))))
    (excl:gc) 
    Results))


#|(defun test-logfile (Filename Tests &key (Verbose Nil))
  "Test the specified logfile."
  (declare (ignore Verbose))
  (let (Reader Results)
    (progn 
      (setq Reader (open-cmdreader Filename :ignore-check-entries t))
      (setq Results (tl-read-nil-pi (make-htime) (read-next-cmd Reader) Reader Tests))
      (close-cmdreader Reader))
    
    (excl:gc) 
    Results))
|#

;;; The read-pis function reads the specified file (via the open cmdreader) as 
;;; a list of problem-instances, where each problem instance is defined as a 
;;; segment of time within the problem file representing the student's work on
;;; a specific problem, or the tasks that they engaged in before opening the 
;;; problem, or at the end as the problem is closed.  
;;;
;;; This code has been written on the assumption that the only information that
;;; will be logged apart from the initial dde's that occur before the student
;;; opens a problem and the exit-andes call afterwords will be logged while a 
;;; problem is open.  As a result all of the relavent data can be logged on a 
;;; per-problem basis.
;;;
;;; This functiuon begins by reading the "preamble" from the file using the 
;;; tl-read-preamble func.  It then calls the tl-read-pi function which will
;;; recursivelyu read and return all of the problem instances.  Once that is
;;; done it will read the remaining problem information if it is possible and
;;; will then return the set of file values.  
;;;
;;; The results will be a list of the form:
;;; (<ProblemName> <StartTime> <EndTime> . ((<Test0> <R0>) ... (<TN> <RN>)))
;;;
;;; NOTE:: Because the first cmd in any new testset will be an open-problem cmd
;;; the sysytem will run the tests once initially before engaging the loop and
;;; checking for termination.

;;(defun tl-read-pis (Reader Tests)
;;  (let ((R (tl-read-preamble Reader Tests)))
;;    (cons 
;;     R (when (cmdreader-lastcmd Reader)
;;	 (append 
;;	  (tl-read-next-pi (nth 2 R) Reader Tests)
;;	  (when (cmdreader-lastcmd Reader)
;;	    (tl-read-tail Reader Tests)))))))


;;; Reading a nil problem-instance from the file is a matter of iterating 
;;; over the file until a successful read-problem-info cmd is reached or 
;;; the end of the file is reached (nil cmd).  This function takes in a 
;;; start time, a starting cmd, a reader, a set of tests and an optional
;;; set of results.  It will then iterate over the file executing the
;;; tests.
;;;
;;; When a nil cmd is reached this code will return the tests.  When a 
;;; successful open-problem cmd is reached this code will then recursively
;;; call tl-read-pi with the current results being passed in.
(defun tl-read-nil-pi (Start Command Reader Tests &optional (Results Nil))
  "Read the preamble from the problem up to the first open-problem cmd."
  (let ((R (tl-rnp-make-results Tests)))
    (do* ((cmd Command (Read-next-cmd Reader))
	  (Stack (list Cmd) (cons Cmd Stack)))
	((tl-nil-pi-term-test Cmd)
	 (tl-nil-pi-recurse Start Cmd R Reader Tests Results))
      (tl-execute-tests Cmd Stack R))))


;;; The preamble reader terminates iff the incoming cmd is nil 
;;; indicating that the file is at an end, or it is a successful
;;; read-problem-info cmd.
(defun tl-nil-pi-term-test (Cmd)
  (or (null Cmd) 
      (and (read-problem-info-cmdp Cmd)
	   (correct-cmdp Cmd))))


;;; Once the preamble has ended form a piresult with no filename a start time
;;; of 0, an end time of the last cmd read, and the stored results.  Then test
;;; the cmd.  If it is nil then return the current result.  If it is an 
;;; open-problem cmd then recursively call tl-read-next-pi.
(defun tl-nil-pi-recurse (Start Cmd R Reader Tests Results)
  (let ((NewRes (tl-pi-form-results Nil Start (cmdreader-lastCmd Reader) R Results)))
    (if (null Cmd) (reverse NewRes)
      (tl-read-pi (nth 2 (car NewRes)) Cmd Reader Tests NewRes))))
  

;;; Once we have read the preamble from the problem file we begin reading off and 
;;; testing the problem instances individually.  This code takes in a starting time
;;; an initial cmd, a Cmdreader a set of tests and a set of results.  It then
;;; reads cmds off of the file and tests them until a successful close-problem or 
;;; null cmd is found.  At that point it will recurse to read the next cmd an iterim
;;; nil-pi, or the file's tail.
(defun tl-read-pi (Start Command Reader Tests Results)
  (let ((R (tl-rnp-make-results Tests)))
    (do* ((cmd Command (read-next-cmd Reader))
	  (Stack (list Cmd) (cons Cmd Stack)))
	((tl-pi-term-test Cmd)
	 (tl-pi-recurse Cmd Stack R Start Command Reader Tests Results))
      (tl-execute-tests Cmd Stack R))))

;;; Return t iff the cmd is nil (indicating that the file has ended) or is a 
;;; successful close-problem dde.  
;;;
;;; NOTE:: Due to some problems with previous years I am not making it a 
;;;        requirement that the close-problem-cmd be correct I will test
;;;        to see if this adverseley impacts the system's performance.
(defun tl-pi-term-test (Cmd)
  (or (null Cmd) (close-problem-cmdp Cmd)))
;;      (and (close-problem-cmdp Cmd)
;;	   (correct-cmdp Cmd))))
      

;;; Once the tl-execute-pi has found a null cmd or a close-problem cmd it will 
;;; call this code.  If the cmd is nil then the system will return with the 
;;; current results. If the cmd is a close problem cmd then the sysytem will 
;;; execute the tests one more time on the current cmd before reading the next 
;;; Cmd from the stack.  If the Cmd is an open-problem cmd then the sysytem 
;;; will recursively call tl-read-pi.  If it is not then the sysytem will call
;;; tl-read-null-pi.  This code will recursively read from the sysytem until 
;;; the end of the file is reached or a successful open-problem cmd is reached.
;;;
;;; If the NextCmd is nil then the current results will be returned.
(defun tl-pi-recurse (Cmd Stack R Start Command Reader Tests Results)
  (if (null Cmd) (tl-pi-nil-result R Start Command Reader Results)
    (progn
      (tl-execute-tests Cmd Stack R)      
      (tl-pi-result-errt Command Cmd)
      (let ((NewRes (tl-pi-form-results Command Start Cmd R Results))
	    (NextCmd (read-next-cmd Reader)))
	(cond ((null NextCmd) (reverse NewRes))
	      ((and (read-problem-info-cmdp NextCmd) (correct-cmdp NextCmd))
	       (tl-read-pi (cmd-time Cmd) NextCmd Reader Tests NewRes))
	      (t (tl-read-nil-pi (cmd-time Cmd) NextCmd Reader Tests NewRes)))))))



;;; If we encounter a nil cmd whiole reading a pi (end of file signal) 
;;; then we need to return the results that we have reversing them as 
;;; necessary.
(defun tl-pi-nil-result (Result Start Command Reader Results)
  (reverse
   (cons 
    (list (cmd-call-id Command) Start
	  (cmd-time (cmdreader-lastcmd Reader))
	  Result)
    Results)))


;;; Testing for errors in the result right now is a fairly minimal thing
;;; right now this code just tests to see if the read-problem-info
;;; and close-problem commands have the same id.  if they do not then
;;; an error will be thrown.  
;;;
;;; If it becomes necessary I may extend this with testing for an open-problem
;;; following a close problem but this is unlikely to be important.
(defun tl-pi-result-errt (Open Close)
  (when (not (and (read-problem-info-cmdp Open)
		  (close-problem-cmdp Close)
		  (cmds-id-equalp Open Close)))
    (error "Invalid name match ~a ~a" Open Close)))


;;; Given an oepn0problem command a start time, a close-problem Cmd
;;; a set of testresults and a set of preexisting results form the 
;;; results list for recursion.
(defun tl-pi-form-results (OpenCmd StartTime CloseCmd TResults Results)
  (cons (list (if OpenCmd (cmd-call-id OpenCmd) "NIL") 
	      StartTime 
	      (if CloseCmd (cmd-time CloseCmd) StartTime) 
	      TResults)
	Results))



;;; Generate an empty result list.
(defun tl-rnp-make-results (Tests)
  (mapcar #'(lambda (Ts) (cons TS (testset-make-empty-results TS))) Tests))




;;; execute-tests
;;; Given a Command, A Stack, and a list of lists of the form:
;;;  ((<TestSet0> . <Results0>) ... (<TestSetN> . <ResultsN>))
;;; Iterate over the sets of tests calling the execution function 
;;; from each test set on the Command, the Stack and the Results.  
;;;
;;; For efficiencie's sake the system assumes that the execution 
;;; functions will update the results in-place thus preventing the 
;;; system from needing to waste conses storing and re-storing them.
(defun tl-execute-tests (Command Stack Tests)
  "Execute the tests."
  ;;(pprint Command)
  (dolist (Test Tests)
    (testset-execute (car Test) Command Stack (cdr Test))))





(defun trace-test-logfiles ()
  (trace test-logfile
	 tl-read-nil-pi
	 tl-nil-pi-term-test
	 tl-nil-pi-recurse
	 tl-read-pi
	 tl-pi-term-test
	 tl-pi-recurse
	 tl-pi-nil-result
	 tl-pi-result-errt
	 tl-pi-form-results
	 tl-pi-read-tail
	 tl-execute-tests))
	 
	 









