#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; problems.cl
;;; Collin Lynch
;;; 3/2/2003
;;;
;;; For some applications it is useful to have a list of the 
;;; problems precomputed in the database.  The code in this 
;;; file takes in database information with preexisting 
;;; problem-instance results and generates a table containing
;;; all of the problems (by name) that were accessed in the file.
;;;
;;; These can the be used later for binning, selection, or 
;;; calculation purposes.  
|#


;;; Given a DBName, a tablename, an odbc name, and optional output name
;;; and keyword arguments, generate a new table in the database containing
;;; all of the problem names and assign a unique problem id to each one.  
;;;
;;; Arguments:
;;;  DBName:  The database name to use.
;;;  TableName:  The source table to use.
;;;  ODBCName:  The odbc datasource to use.
;;;  &key
;;;   OutputName:  The name of the resulting table (default is "problems")
;;;   Verbose:  If t then the odbc output will be echoed to the user.
;;;
;;; NOTE:: This code is written on the assumption that the specified table
;;;  contains a column called "problem" and that that is a string form.
;;;
;;; Note:: This code does not guarantee that the same idnumber will be 
;;;  assigned to a given problem each time.  

(defun generate-problems-table (DBName TableName ODBCName 
				&key (OutputName "problems") (Verbose Nil))
  "Generate the runtime problems table."
  (format t "=============================================~%")
  (format t "Generating Problems Table.~%")
  (let ((Connection (odbc-connect ODBCName :Verbose Verbose)))
    (gpt-create-table DBName OutputName Connection :Verbose Verbose)
    (odbc-insert 
     OutputName DBName Connection
     :Columns "name"
     :Select (odbc-format-select 
	      (odbc-format-id :DB DBName :Tbl TableName :Col "problem")
	      (odbc-format-id :DB DBName :Tbl TableName)
	      :Remove-duplicates t
	      :order-by (odbc-format-id :DB DBName :Tbl TableName :Col "problem")))
    (odbc-disconnect Connection :Verbose Verbose)))
    
    
;;; Given a Database name generate the static Problems table within it
;;; using the specified connection and other values.  
(defun gpt-create-table (DBName TableName Connection &key (Verbose Nil))
  (odbc-create-table
   TableName DBName Connection
   (list (odbc-format-columnspec "id" "INTEGER" :not-null t :auto-increment t :Primary-Key t)
	 (odbc-format-columnspec "name" "CHAR(64)" :not-null t))
   :Verbose Verbose))



(defun tst-foo (dbname tablename outputname)
  (odbc-format-insert 
     OutputName DBName
     :Columns '("name")
     :Select (odbc-format-select 
	      (list (odbc-format-id :DB DBName :Tbl TableName :Col "problem"))
	      (list (odbc-format-id :DB DBName :Tbl TableName))
	      :Remove-duplicates t
	      :order-by (odbc-format-id :DB DBName :Tbl TableName :Col "problem"))))





