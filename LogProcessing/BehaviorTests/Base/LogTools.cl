#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LogTools.cl
;;; Collin Lynch
;;;
;;; The functions in these files have been setup to handle various 
;;; system processing tasks that I wanted to take such as renaming 
;;; files to remove the leading m and moving the files into a 
;;; consistent directory structure.  Some of these tools are also
;;; used in other processing tasks such as extracting the relavent
;;; information from the files.  
;;;
;;; Note that, due to changes in XP many of these must be executed by a 
;;; separate lisp instance.  Under XP when Allegro enters into a directory 
;;; it will seize control of the contents and not release them until it is
;;; shut down.  As a result any file deletions must be done by a separate
;;; allegro process or by one that you don't mind killing.
;;;
;;; The 
;;; modifying the file names as necessary.  The second section contains code 
;;; that I use to "clean up" the file names (removing excess spaces etc.) and
;;; code that is used to reorganize the files as necessary.  The root function
;;; clean-and-sort-logfiles perfroms all of these functions automatically on
;;; a given directory leaving the remaining info for later.  
;;;
;;; The last portion of the file contains code that is used to 
|#





;;;; ===========================================================================
;;;; Filename Manipulation.
;;;; The code in this section is used to read logfilenames split them into their
;;;; component elements and manipulate those elements or to take those elements
;;;; and produce a new logfilename from them.  

;;; ------------------------------------------------------------------
;;; The logfilename split is a struct defining all of the components
;;; included in a logfilename.  The purpose of this struct is to make 
;;; manipulation of the elements cleaner for later purposes.
(defstruct (LognameStr (:print-function print-LognameStr))
  Username
  Month
  Day
  Hour
  Min
  Sec)


(defun print-lognamestr (Str &optional (Stream t) (Level 0))
  (declare (ignore Level))
  (format Stream "~a-~a~a-~a-~a-~a.log" (lognamestr-Username Str)
	  (lognamestr-Month Str) (lognamestr-Day Str)
	  (lognamestr-Hour Str) (lognamestr-Min Str)
	  (lognamestr-Sec Str)))

;;; -----------------------------------------------------------------------
;;; Split Logfilename
;;; The filenames are of the form <Username>-<Month><Day>-<hr>-<Min>-<Sec>.log
;;; the functions in this section will segment a filename string and return a 
;;; LogNameStr containing the relavent components.  Thir psocess of parsing 
;;; starts from the end of the file.  

(defun split-logfilename (Filename)
  "Separate the file into meaningful tokens."
  (let ((Result (make-LogNameStr)) 
	(Pos1 (position #\. Filename :from-end t))
	(pos2 (position #\- Filename :From-end t)))
    ;; Sec
    ;;(pprint (subseq Filename (+ 1 Pos2) Pos1))
    (setf (LogNameStr-Sec Result) (subseq Filename (+ 1 Pos2) Pos1))
    
    ;; Min
    (setq Pos1 (position #\- Filename :from-end t :End Pos2))
    ;;(pprint (subseq Filename (+ 1 Pos1) Pos2))
    (setf (LogNameStr-Min Result) (subseq Filename (+ 1 Pos1) Pos2))

    ;; Hr
    (setq pos2 (position #\- Filename :From-end t :end Pos1))
    ;;(pprint (subseq Filename (+ 1 Pos2) Pos1))
    (setf (LogNameStr-Hour Result) (subseq Filename (+ 1 Pos2) Pos1))

    ;; Day
    (setq pos1 (- Pos2 2))
    ;;(pprint (subseq Filename Pos1 Pos2))
    (setf (LogNameStr-Day Result) (subseq Filename Pos1 Pos2))

    ;;Month
    ;;(pprint (subseq Filename (- Pos1 3) Pos1))
    (setf (LogNameStr-Month Result) (subseq Filename (- Pos1 3) Pos1))

    ;; Username
    (setq Pos1 (position #\- Filename :from-end t :end Pos2))
    ;;(pprint (subseq Filename 0 Pos1))
    (setf (LogNameStr-Username Result) (subseq Filename 0 Pos1))
    Result))



;;; -------------------------------------------------------------------------------
;;; Convert MonthStr to Num
;;; Months are encoded in the files as a string "Aug" "Sep" etc.
;;; For some of the files it is necessary to encode it as an integer.
;;; This function will associate the string forms of the months with 
;;; an integer form and return the result.  

(defparameter **Month-Num-list**
    '(("Jan" 1) 
      ("Feb" 2)
      ("Mar" 3)
      ("Apr" 4)
      ("May" 5)
      ("Jun" 6)
      ("Jul" 7)
      ("Aug" 8)
      ("Sep" 9)
      ("Oct" 10)
      ("Nov" 11)
      ("Dec" 12)))

(defun map-MonthStr->MonthNum (MonthStr)
  "Map the Month string to an integer form."
  (cadr (assoc MonthStr **Month-Num-List**
	       :test #'String-equal)))


(defun map-monthnum->monthstr (MonthNum)
  "Map the integer form of the month to a three character str."
  (car (find MonthNum **Month-Num-List**
	     :key #'cadr :test #'=)))
   
   



;;; ------------------------------------------------------------------------------------
;;; Collect Usernames
;;; Given a set of files collect the usernames from them removing duplicates.
;;; This is used for generating test sets and for aetting up the logdriver.
(defun collect-files->usernames (path)
  "Collect all of the usernaes in path."
  (let (Result Name)
    (dolist (F (directory (concatenate 'string Path "*.log")))
      (setq Name (LogNameStr-Username (split-logfilename (file-namestring F))))
      (when (not (member Name Result :test #'string-equal))
	(push Name Result)))
    Result))



;;; ----------------------------------------------------------------------------------
;;; Collect Subdirectory Names.
;;; Given a path to some directory containing subdirectories collect all of
;;; the subdirectory names and return them in a list.
(defun collect-subdirectory-names (path)
  "Collect all of the subdirectory names in the path."
  (mapcar #'file-namestring (remove-if-not #'excl:file-directory-p (directory path))))


;;; ----------------------------------------------------------------------------------
;;; Collect-subdir-paths
;;; Given a root directory containing student-sorted-sbudirs collect
;;; paths to the subdirectories within it.  
(defun collect-subdir-paths (path)
  "Collect the subdirectory paths."
  (remove-if-not #'excl:file-directory-p (directory path)))


;;; -----------------------------------------------------------------------
;;; collect subdir logs
;;; Given a root directory containing subdir sorted logfiles generate a list of all
;;; the individual logs in the directory.
(defun collect-subdir-logs (path)
  (directory (concatenate 'string (namestring Path) "*/*.log")))


;;; -----------------------------------------------------------------------
;;; randomly-pick-subdir-log
;;; Given a directory containing a set of logs select a file from it 
;;; randomly.  If the log does not pass the remove-if test repeat.
(defun randomly-pick-subdir-log (path &key (remove-if Nil))
  (let (Len (logs (directory (concatenate 'string (namestring path) "*.log"))))
    (if Remove-if (setq Logs (remove-if Remove-If logs)))
    (setq Len (length Logs))
    (when (< 0 Len) (nth (random Len) Logs))))


;;;; =============================================================================
;;;; Binning Logfiles
;;;; Given a list of usernames bin the names according to the supplied dates.  
;;;; this code takes a list of Moth-day pairs and a list of files and, sorts each
;;;; file into bin n where the month and day of N are less than the month and day
;;;; of the nth pair.  This is a simple sorting algorithm that we can use for 
;;;; generating filesets.
;;;;
;;;; This is used by the HelpDriver as well as other code.

;;; ----------------------------------------------------------------------------
;;; Bin Logfiles
;;; Given a set of logfiles bin them according to the locate-bin procedure.  
;;; This is done by generating an empty list of the same size as the bins
;;; and then adding filenames to each bin as necessary.
;;;
;;; Note that there will always be 1 more bin than listed in the original 
;;; procedure to handle all those files that are > the last partition supplied.
(defun bin-logfiles (Files Bins)
  (let ((Result (make-list (+ 1 (length Bins)))))
    (dolist (F Files)
      (push F (nth (logfile-locate-bin F Bins) Result)))
    Result))


;;;; Given a logfile split, locate the position where it fits I.E. the highest bin
;;;; where logfile-md<= is true and return the index of that bin.
(defun logfile-locate-bin (File Bins)
  (let* ((Split (split-logfilename (file-namestring File)))
	 (Pos (position-if #'(lambda (B) (logfile-md<= Split B)) Bins)))
    (if Pos Pos (length Bins))))



;;; -------------------------------------------------------------------
;;; Given a list of minth/day pairs compare them.
;;;
;;; Given a single month/day pair and a split logfilename return t
;;; if the split's month is less than the current month or the months
;;; are the same and the days are equal.
(defun logfile-md<= (Split pair)
  (or (logfile-m< (read-from-string (lognamestr-Month Split)) (car Pair))
      (and (logfile-m= (read-from-string (lognamestr-Month Split)) (car Pair))
	   (logfile-d<= (read-from-string (lognamestr-day Split)) (nth 1 Pair)))))



(defun logfile-m< (Month1 Month2)
  "Given a pair of month names return t if Month1 occurs before Month2."
  (let* ((months '(Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec Jan))
	(p1 (position Month1 Months :test #'equalp))
	(p2 (position Month2 Months :test #'equalp)))
    (< P1 P2)))


(defun logfile-m= (Month1 Month2)
  "Given a pair of months return t if they are =."
  (equalp Month1 Month2))


(defun logfile-d<= (Day1 Day2)
  (<= Day1 Day2))
		    

(defun trace-binning ()
  (trace logfile-d<=
	 logfile-m<=
	 logfile-md<=
	 logfile-locate-bin
	 bin-logfiles))












;;;; ==========================================================================
;;;; Sorting Files
;;;; The files in the Zips are sorted by the directory in which they appear 
;;;; on the USNA server.  Our Pitt logs are often sorted in similar fashion.
;;;; The code in this section is used to repair bad filenames (when the students
;;;; login with improper names or special characters that crash the system) and
;;;; to sort the files into subdirectories by the name.  


;;; --------------------------------------------------------------------------
;;; Copying files
;;; Given a filepath, and a destination directory copy the specified file
;;; at the source dir to the destination dir with the same name.  If :overwrite
;;; is t then do not throw an error when a file of the same name already exists.

(defun safe-copy-file (Src Dest &key (Overwrite Nil))
  (when (probe-file Dest)
    (if Overwrite (delete-file Dest)
      (error "safe-copy-file: Specified destination file ~a already exists." Dest)))
  (ensure-directories-exist Dest)
  (system:copy-file Src Dest))


;;; --------------------------------------------------------------------------
;;; safe-copy-files
;;; Given a set of files and a destination directory copy the specified files
;;; from their src dies to the dest dir generating the dest dir as necessary.
(defun safe-copy-files (Files Dest &key (Overwrite Nil))
  (let ((path (namestring Dest)))
    (dolist (File Files)
      (safe-copy-file 
       File (concatenate 'string path (file-namestring File)) 
       :Overwrite Overwrite))))


;;; --------------------------------------------------------------------------
;;; Moving files.
;;; Given a pair of filepaths translate move the file located at STARTPATH
;;; to ENDPATH.  If Verbose is true then display the relavent information
;;; as it runs.  If the NEWPAth file already exists then throw an error.
;;; If a path to a directory is supplied as Duplicate-Store then duplicate
;;; files will be placed in that location in their relavent subdirectories.
(defun move-file (OldPath NewPath &key (Verbose Nil) (Store-Duplicates Nil))
  "Move the file from Oldpath to newPath."
  (if Verbose (format t "Testing: ~a" OldPath))
  (cond ((equalp (pathname (string-downcase (namestring NewPath))) 
		 (pathname (string-downcase (namestring OldPath)))) 
	 (if Verbose (format t "Pathnames equal no change.~%")))
	((probe-file NewPath) 
	 (if Verbose (format t "~%**Error** ~a already exists.~%" NewPath))
	 (if Store-Duplicates (duplicate-move-file OldPath Store-Duplicates :verbose Verbose)))
	(t (ensure-directories-exist NewPath :Verbose Verbose)
	   (rename-file OldPath NewPath)
	   (if Verbose (format t "--> ~a~%" NewPath)))))



;;; Move the file into a storage subdirectory as it has proved to be a duplicate
;;; item.  If it turns out to be a duplicate in this instance then throw an error.
(defun duplicate-move-file (File Storage &key (Verbose Nil) (Copy Nil))
  "Move the file to the Storage path."
  (declare (ignore copy))
  (let ((Path (format Nil "~a~a" Storage (file-namestring File))))
    (when (probe-file Path) (setq Path (copy-unique-pathname Path)))
    (ensure-directories-exist Path :Verbose Verbose)
    (rename-file File Path)
    (if Verbose (format t "Dumping ~a --> ~a~%" File Path))))


;;; In order to deal with duplicates but not lose files this code makes each 
;;; duplicate pathname unique by postpending copy to the filename until it 
;;; no longer matches any other file.  
(defun copy-unique-pathname (Path)
  (let ((File (concatenate 'string Path "-Copy")))
    (if (not (probe-file File)) File 
      (copy-unique-pathname File))))



;;;; -----------------------------------------------------------------------
;;;; Renaming
;;;; Stduents have a penchant for choosing many different naming styles and
;;;; conventions.  The code in this section is used to transform the files
;;;; in various ways by changing some specific attribute of a given filename
;;;; The topmost function iterates over all the logfiles in a specified
;;;; directory and applies the various transforms to them.  The transforms 
;;;; are assumed to take a single filename, test whether or not it should be
;;;; changed and then do it as necessary.  


;;; -------------------------------------------------------------------------
;;; Iterate files
;;; Given a root pathname describing a set of files, and a list of predicates
;;; that test and then transfrom filenames as necessary, iterate over all of 
;;; the logfiles in the directory passing the names to the predicates one by
;;; one.  When a change occurs the system will test for directories as well.
;;;
;;; The transform predicates should take and return a string representing the
;;; file's name + path that will later be used to rename the file unless no
;;; change has occured.  
(defun transform-logfilenames (Path Tests &key (DriveName "c:") (Verbose t) 
					       (Store-Duplicates Nil))
  "Iterate over the logfiles in path and pass the names to the transforms."
  (let (NewName OldName (Files (directory (concatenate 'string Path "*.log"))))
    (dolist (File Files)
      (setq OldName (namestring File))
      (setq NewName OldName)
      (if Verbose (format t "Testing: ~a   " File))
      (dolist (Test Tests)
	(setq NewName (funcall Test NewName)))
      (move-file OldName (concatenate 'string DriveName NewName) 
		 :Verbose Verbose :Store-Duplicates store-Duplicates))))





;;; -----------------------------------------------------------------------------
;;; Translators.

;;; Translate Mnum Files
;;; Given a file split it into the individual components.  If the student name
;;; is of the form m###### then remove the m from the name.  This is done in 
;;; order to deal with the USNA logins.  USNA students are supposed to login 
;;; using the midshipman numbers.  Many of them simply enter the number #####
;;; many however follow the convention of using m##### to indicate their rank
;;; of midshipman.  Even worse members of the community choose to shift from
;;; standard to standard.  Since we have not yet added the electroshock feature 
;;; to the system we will have to deal with changing the names after the fact. 
(defun strip-m-from-mname (Filename)
  "Given a file determine if the student name is an m##### mnum  IF so strip off the 'm'."
  (let* ((Split (split-logfilename (file-namestring Filename)))
	 (Name (lognamestr-username Split)))
    (if (and (or (equal (char Name 0) #\m)
		 (equal (char Name 0) #\M))
	     (numberstr (subseq Name 1)))
	(progn (setf (lognamestr-username Split) (subseq Name 1))
	       (format Nil "~a~a" (directory-namestring Filename) Split))
      Filename)))

(defun numberstr (Str)
  "Is str of the form: ###### a pure seq of digits?"
  (not (loop for L below (length Str)
	   when (not (digit-char-p (char Str L)))
	   return t)))


;;; Replace-bad-chars
;;; This code iterates over the filename and replaces the specified bad
;;; characters with '_'  This is done to avoid some of the errors that 
;;; bad usernames have a tendency to cause.  
;;;
;;; At present the list of bad characters are:
;;;  #\Space, #\Tab and #\.
(defun remove-bad-chars-from-filename (Filename)
  "Replace all spaces in the filename with a '_'."
  (let* (Char (Split (split-logfilename (file-namestring Filename)))
	 (Name (lognamestr-Username Split)) (Len (length Name)) 
	 (Result (make-string Len)))
    (dotimes (N Len)
      (setq Char (char Name N))
      (setf (char Result N)
	(if (bad-filename-charp Char) #\_ Char)))
    (setf (lognamestr-Username Split) Result)
    (format Nil "~a~a" (directory-namestring Filename) Split)))


(defun bad-filename-charp (Char)
  "Return t if char is a whitespace character."
  (member Char '(#\Space #\Tab #\. #\, #\') :test #'char=))



;;; Strip Bad leading chars
;;; Some of the students chose to login with a ' or - preceeding their 
;;; names this causes errors with Excel and needs to be eliminated.  
;;; Therefore this code strips off the first character from the filename 
;;; if that character is a '.
;;;
;;; The function is called recursively to deal with a name of ''###
(defun strip-bad-lead-char-from-filename (Filename)
  (let ((Name (file-namestring filename)))
    (if (or (char= (char Name 0) #\') (char= (char Name 0) #\-)) 
	(strip-bad-lead-char-from-filename
	 (format Nil "~a~a" (directory-namestring Filename) 
		 (subseq Name 1)))
      Filename)))



;;; Strip trailing -
;;; Some of the student usernames end or begin with a "-"  
;;; this code eliminates them.  This is done because the 
;;; students often have a tendency to include them unecessarily.
;;; TODO.
(defun strip-trailing-dash-from-filename (Filename)
  (let* ((split (split-logfilename (file-namestring Filename)))
	 (Name (lognamestr-Username Split))
	 (Loc (- (length Name) 1)))
    (pprint Name)
    (pprint Loc)
    (cond ((char= (char Name Loc) #\-)
	   (setf (lognamestr-Username Split) (subseq Name 0 Loc))
	   (format Nil "~a~a" (directory-namestring Filename) Split))
	  (t Filename))))


;;; Change usernames.
;;; Given a set of filename and a username replace the previous username
;;; with the new one.
(defun change-logfile-username (Filename Newname)
  (let ((split (split-logfilename (file-namestring Filename))))
    (setf (lognamestr-Username Split) NewName)
    (format Nil "~a~a" (directory-namestring Filename) Split)))


;;; -----------------------------------------------------------------------
;;; Change usernames
;;; Given a pathname describing a set of logfiles iterate over the files 
;;; chagning the username on all of them to the supplied newname.
;;; This is used primarily for manual editing of the files.
(defun change-logfiles-usernames (Filepath Newname &key (Verbose Nil) (Store-duplicates Nil))
  (dolist (File (Directory Filepath))
    (move-file (namestring File) (change-logfile-username File NewName) 
	       :Verbose Verbose :Store-Duplicates Store-Duplicates)))



;;;; ----------------------------------------------------------------------
;;;; Shift logfiles.
;;;; When the students login to Andes they use a (hopefully) recognizeable login.
;;;; Past experience has shown that we cannot always trust this login but it is
;;;; the most reliable entry that we have from the student.  For our pitt 
;;;; experiments this login is the assigned research id.  For the USNA students
;;;; the login is (hopefully) their midshipman number.  
;;;;
;;;; However when the students go to upload the files they do not always upload 
;;;; with the appropriate id.  The uploader program is not part of the workbench
;;;; and uses a separate id to store the values on the server.  Therefore the 
;;;; students do not always use the same upload login.  Additionally the usernames
;;;; are often changed to remove error characters.
;;;;
;;;; For that reason it is often necessary to shift the files from the subdirectory 
;;;; that they are in into a different subdirectory based upon their username.
;;;;
;;;; This code iterates over all the files located in a subdirectory of the supplied
;;;; root directory based upon the username.  If necessary it will generate new 
;;;; directories as necessary.
;;;;
;;;; Note, In the event that the file has no username then the file will be stored in
;;;; the root directory.

(defun directory-sort-logfiles (Root &key (Verbose Nil) (Store-Duplicates Nil))
  "Sort the specified files into new subdirectories."
  (let (FileName Dir NewFile (FilesPath (concatenate 'string Root "*/*.log")))
    (dolist (File (directory FilesPath))
      (setq Filename (file-namestring File))
      (setq Dir (lognamestr-username (split-logfilename Filename)))
      (setq NewFile
	(if Dir (concatenate 'String Root Dir "/" Filename)
	  (concatenate 'string Root Filename)))
      (move-file (namestring File) NewFile 
		 :Verbose Verbose 
		 :Store-Duplicates Store-Duplicates))))


      


;;;; -----------------------------------------------------------------------
;;;; Delete Empty Directories
;;;; Given a pathname identify the directories within it that are empty
;;;; and delete them.  This is done for cleanup purposes more than 
;;;; anything else.
(defun remove-empty-dirs (Path &key (Verbose Nil))
  "Remove all empty directories from the current Dir."
  (dolist (F (directory Path))
    (when (and (excl:file-directory-p F)
	       (file-directory-emptyp F))
      (if Verbose (format t "Deleting: ~a~%" F)) 
      (excl:delete-directory F))))

;;; Determine if the specified directory is empty by calling directory
;;; on it and then returning t if Directory is nil.
(defun file-directory-emptyp (File)
  (let ((F (if (pathnamep File) (namestring File) File)))
    (null (directory (concatenate 'string F "\\")))))



;;;; -----------------------------------------------------------------------
;;;; Raise files.
;;;; Given a root pathname raise all of the logfiles located one directory
;;;; below it up to the current directory.

;;;; locate all of the 1st level subdirectories within this directory.
;;;; and rename them to be in the current directory.
(defun subdir-raise-logfiles (Root &key (Drive "c:") (verbose Nil)) 
  "Sort the logfiles into subdirs."
  (declare (ignore Drive))
  (let (Path)
    (dolist (F (directory (concatenate 'string Root "*/*.log")))
      (setq Path (concatenate 'string Root (file-namestring F)))
      (when Verbose 
	(format t "Moving:: ~a" F)
	(format t " To:: ~a~%" Path))
      (rename-file F Path))))



;;; This code reverses the process of subdir-sort-logfiles and locates
;;; all logfiles in a subdirectory of the current directory and brings 
;;; them to the root of the specified directory.  

;;; Given a directory locate all logs that are one level below the current
;;; level and raise them up to the current level and then delete all the
;;; empty directories.
(defun raise-and-clear-logfiles (Path &key (verbose Nil))
  (subdir-raise-logfiles Path :verbose Verbose)
  (remove-empty-dirs Path :verbose Verbose))






;;;; =====================================================================
;;;; Delete files
;;;; The tools in this section generate an array of files as did previous 
;;;; versions of Andes.  The functions in this section are used to delete
;;;; the specified files by name or by path.  

;;; ---------------------------------------------------------------
;;; Delete-directory-files
;;; Given a pathname (including wildcards) collect up the set of
;;; files that it refers to and delete them.  This code exists because
;;; the cygwin tools that I was using no longer work.  
;;;
;;; NOTE:: exceedingly slow when dealing with large nested dirs.
(defun delete-path-files (path &key (Verbose Nil))
  "Delete all of the files specified by path."
  (let ((Files (directory Path)))
    (dolist (F Files)
      (if Verbose (format t "Removing: ~a~%" F))
      (delete-file (namestring F)))))





;;; ----------------------------------------------------------------
;;; Named file removal
;;; 
;;; The functions below delete specific files by name or some other
;;; criteria.  Most of them take a root dire and append their own 
;;; information to it.

(defun remove-cmd-files (path &key (Verbose Nil))
  "Remove the .cmd files in path."
  (delete-path-files (concatenate 'string path "*.cmd") :Verbose Verbose))

(defun remove-fl-files (path &key (Verbose Nil))
  "Remove the .fl files in path."
  (delete-path-files (concatenate 'string path "*.fl") :Verbose Verbose))

(defun remove-ws-ftp-files (path &key (Verbose Nil))
  "Remove all files named \"WS_FTP.LOG\"."
  (delete-path-files (concatenate 'string path "WS_FTP.LOG") :Verbose Verbose))

(defun remove-ctx-files (path &key (Verbose Nil))
  "Remove all the files names *.ctx there were used in old versions of Andes."
  (delete-path-files (concatenate 'string path ".ctx") :Verbose Verbose)
  (delete-path-files (concatenate 'string path "*.ctx") :Verbose Verbose))

(defun remove-hst-files (path &key (Verbose Nil))
  "Remove all the files names *.hst there were used in old versions of Andes."
  (delete-path-files (concatenate 'string path ".hst") :Verbose Verbose)
  (delete-path-files (concatenate 'string path "*.hst") :Verbose Verbose))

(defun remove-old-files (path &key (Verbose Nil))
  "Remove all the files names *.old there were used in old versions of Andes."
  (delete-path-files (concatenate 'string path ".old") :Verbose Verbose)
  (delete-path-files (concatenate 'string path "*.old") :Verbose Verbose))

(defun remove-txt-files (path &key (Verbose Nil))
  "Remove all the files names *.txt that appear in many dirs."
  (delete-path-files (concatenate 'string path ".txt") :Verbose Verbose)
  (delete-path-files (concatenate 'string path "*.txt") :Verbose Verbose))



;;;; ==========================================================================
;;;; Set-up-Zips.
;;;; Given a set of files from the USNA or pitt zips where all files are located
;;;; in a single directory or in some subdirectory (nonrecursive) of the current
;;;; directory this function will do the Following:
;;;;  1. Delete all files of the following types: .hst .txt .ctx .old 
;;;;     and all of the WS_FTP.LOG files in the root directory and in
;;;;     the subdirectories.
;;;;  2. Iterate over each file reparing its name by:
;;;;     Removing 'm' from the midshipman numbers.
;;;;     Replacing all speces in the file with '_'
;;;;     Eliminating all error characters such a ^'. etc.
;;;;  3. Shifting files from their current location to directories  
;;;;     sorted by usernames.
;;;;  4. Deletion of empty directories.

(defun sort-zipped-logfiles (Root &key (verbose Nil) (store-duplicates Nil))
  (let ((Path (concatenate 'string Root "*/")))
    (format t "1. Deleting unnecessary files.~%")
    (szl-delete-unnecessary-files Path :Verbose Verbose)
    (format t "2. Repairing names.~%")
    (szl-repair-filenames Path :Verbose Verbose)
    (format t "3. Shifting files.~%")
    (directory-sort-logfiles 
     Root :Verbose Verbose :store-Duplicates Store-Duplicates)
    (format t "4. Deleting empty folders.~%")
    (remove-empty-dirs Root :verbose Verbose)))


;;; File deletion iterates over the named file types and eliminates
;;; those that we do not want to keep from the zips.  These include:
;;;   WS_FTP.LOG  The ftp logfiles left over from the requests.
;;;   .hst the old andes hst files.
;;;   .txt These were info files that were used to track the equations.
;;;   .ctx andes 1 files used for some inhuman purpose.
;;;   .old God only knows, see the line above for more info.
(defun szl-delete-unnecessary-files (Path &key (Verbose Nil))
  (remove-ws-ftp-files Path :Verbose Verbose)
  (remove-cmd-files Path :Verbose Verbose)
  (remove-fl-files Path :Verbose Verbose)
  (remove-ctx-files Path :Verbose Verbose)
  (remove-hst-files Path :Verbose Verbose)
  (remove-old-files Path :Verbose Verbose)
  (remove-txt-files Path :Verbose Verbose))


;;; Filename repair involves iterating over the files and eliminating characters 
;;; that cause runtime errors as well as altering the name to remove excess elements
;;; such as the 'm' that preceeds many (but not all) of the midshipman numbers.
;;; This can be added to as necessary.
(defun szl-repair-filenames (Path &key (Verbose Nil))
  "Repair the logfilenames."
  (transform-logfilenames
   Path
   (list #'strip-m-from-mname 
	 #'strip-bad-lead-char-from-filename
	 #'remove-bad-chars-from-filename
	 #'Strip-trailing-dash-from-filename)
   :Verbose Verbose))



;;; Iterate over the supplied dirs.
;;(defun foo ()
;;(sort-zipped-logfiles "c:/Andes2/Log/New/Pitt1999/" :verbose t :store-duplicates "c:/Andes2/Log/New/Pitt1999/DUPLICATES/")
;;(sort-zipped-logfiles "c:/Andes2/Log/New/Pitt2000/" :verbose t :store-duplicates "c:/Andes2/Log/New/Pitt2000/DUPLICATES/")
;;(sort-zipped-logfiles "c:/Andes2/Log/New/Pitt2001/" :verbose t :store-duplicates "c:/Andes2/Log/New/Pitt2001/DUPLICATES/")
  ;;(sort-zipped-logfiles "c:/Andes2/Log/New/USNA1999/" :verbose t :store-duplicates "c:/Andes2/Log/New/USNA1999/DUPLICATES")
  ;;(sort-zipped-logfiles "c:/Andes2/Log/New/USNA2000/" :verbose t :store-duplicates "c:/Andes2/Log/New/USNA2000/DUPLICATES")
  ;;(sort-zipped-logfiles "c:/Andes2/Log/New/USNA2001/" :verbose t :store-duplicates "c:/Andes2/Log/New/USNA2001/DUPLICATES")
  ;;(sort-zipped-logfiles "c:/Andes2/Log/New/USNA2002/" :verbose t :store-duplicates "c:/Andes2/Log/New/USNA2002/DUPLICATES")
  ;;)








;;; ------------------------------------------------------------
;;; Change filetypes
;;; This utility is used to change files of one type to another.
(defun change-file-types (Path NewType)
  "Change all the files specified by path to NewType."
  (let (Root Name NewName)
    (dolist (File (directory Path))
      (setq Root (directory-namestring File))
      (setq Name (pathname-name File))
      (setq NewName (format Nil "~a~a.~a" Root Name NewType))
      (if (probe-file NewName) (error "File ~a already exists." NewName))
      (format t "Renaming: ~a -> ~a~%" File NewName)
      (rename-file File NewName))))








;;;; ===========================================================================
;;;; Random Sampling
;;;;
;;;; For many of the studies it is necessary for us to randomly sample logs from
;;;; or log base.  The code in this section is used to randomly sample the work
;;;; according to different requirements.  In some instances we want a random
;;;; sample of all logs.  However directly sampling the logs will be biased
;;;; by the fact that some students generated more logs than others.  Use the
;;;; functions below as appropriate to sample as needed.
;;;;
;;;; All of the functions take the same set of arguments:
;;;;  N: The number of logs that we want to select.
;;;;  Dirs:  A set of root paths that correspond to a list of directory
;;;;         sorted logfiles.  Each path should point to a directory
;;;;         containing a set of dirs each of which corresponds to a 
;;;;         student and within which are their logs.
;;;;  &key
;;;;   Remove-if:  A predicate that takes a pathname to a log as its argument.
;;;;               If t will eliminate a log from consideration.
;;;;
;;;; All of the functions return a list of pathnames to the selected logs

;;; -----------------------------------------------------------------------
;;; Sample-Logs-by-Log
;;; Select a random sample of the logs by selecting evenly from the individual
;;; logs irrespective of student.

(defun sample-logs-by-log (N Dirs &key (Remove-if Nil))
  (let* ((Logs (apply #'append (mapcar #'collect-subdir-logs Dirs)))
	 (Len (length Logs)) (Choices Nil) (Results Nil) Log)
    (do* ((Choice (random Len) (random Len)))
	((= (length Results) N) Results)
      ;;(pprint Choice)
      (when (not (member Choice Choices))
	(setq Log (nth Choice Logs))
	(push Choice Choices)
	(when (or (null Remove-if) (not (funcall Remove-If Log)))
	  (push Log Results))))))



;;; -----------------------------------------------------------------------
;;; Sample-Logs-by-student
;;; Select a random sample of the logs by selecting evenly from the students
;;; and then, for each student, selecting randomly from the student's logs.
;;;
;;; Note:: that this should get only one file per student and thus give us
;;; a reasonable sample from the students unbiased by students who generated
;;; more logs than others.

(defun sample-logs-by-student (N Dirs &key (Remove-if Nil))
  (let* ((Dirs (apply #'append (mapcar #'collect-subdir-paths Dirs)))
	 (Len (length Dirs)) (Choices Nil) (Results Nil) Log)
    (do* ((Choice (random Len) (random Len)))
	((= (length Results) N) Results)
      (pprint Choice)
      (when (not (member Choice Choices))
	(push Choice Choices)
	(setq Log 
	  (randomly-pick-subdir-log 
	   (concatenate 'string (namestring (nth Choice Dirs)) "/")
	   :Remove-if Remove-IF))
	(when Log (push Log Results) (pprint Results))))))




;;;; ---------------------------------------------------------------------
;;;; Copy Random Sample
;;;; Given a random sample selector function, a count number, a list of
;;;; source directories, and a destination directory copy a random selection
;;;; of files from their source locations to the destination dir.
(defun copy-random-sample (Selector N Dirs Dest &key (Remove-if Nil) (Overwrite Nil))
  (safe-copy-files
   (funcall Selector N Dirs :Remove-if Remove-IF)
   Dest
   :Overwrite Overwrite))


;;;; =====================================================================
;;;; Process randomly selected files.
;;;; This function takes in a random selection function name a number and 
;;;; list of source directories, and a process function and then executes 
;;;; the process function on a set of randomly selected files.



;;(defun foo (Selector N Dirs &key (remove-if Nil))
;;  (let ((Files (funcall Selector N Dirs :Remove-If Remove-If)))

    





