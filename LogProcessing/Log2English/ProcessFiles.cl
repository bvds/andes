;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ProcessFiles -- general routines for processing files in 'batch mode'
;; Copyright (C) 2001 by <Linwood H. Taylors (lht@lzri.com) Employer> -- All Rights Reserved
;; Author(s): Linwood H. Taylor (lht@lzri.com) Collin Lynch (collinl@pitt.edu)
;; Modified:
;;      13 December 2001 - (lht) -- created/split from SLog2English.cl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains general overhead routines for running the Slog2English code on
;; a directory, of supplied, files, a set of files or an individual file.  When called 
;; these functions will perform the necessary filesystem searches to locate the files and
;; load them.  Once the supplied file(s) are located, the functions will apply PROCESS
;; to them and PROCESS-ARGUMENTS and then return the result.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-files will process a group of files
;; directory - (string) of name directory containing files to be processed
;; file-chooser - a predicate function that takes a pathname (struct) and returns t iff file
;;  should be processed otherwise should return nil. file-chooser must be written to take at
;;  least one argument (a pathname (struct) of file being considered for choosing).
;; file-chooser-arguments - list of arguments for file-chooser (nil is legal)
;; process - function to be performed on each file chosen (should be written to handle one line
;;  per invocation)
;; process-arguments - list of arguents for process (nil is legal)
(defun process-files (directory file-chooser file-chooser-arguments process process-arguments)
  (dolist (file (directory directory))
    (when (safe-apply t file-chooser (cons file file-chooser-arguments))
      (format t "Processing '~A'~%" file)
      (process-file file process process-arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; same as process-files but will recurse
(defun process-dirs (directory file-chooser file-chooser-arguments process process-arguments)
  (dolist (dir (directory directory))
    (let* ((pdir (concatenate 'string (namestring dir) "/"))
	   (tmp (directory pdir)))
      ;;(format t "Directory ~A (~A)~%" pdir tmp)
      (if tmp
	  (process-dirs pdir file-chooser file-chooser-arguments process process-arguments)
	(when (safe-apply t file-chooser (cons dir file-chooser-arguments))
	  (format t "Processing '~A'~%" dir)
	  (process-file dir process process-arguments))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter **the-output-stream-name** "Temporary.prb")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; process-file processes a single file
;; infile - pathname (NOTE: struct not string) of file to read from
;; process - written to handle a single line of a file (must be written to accept at least three
;;  arguments
;;    1) infile - the pathname (struct) of the file being processed
;;    2) linenumber - the current line number being read (starts at zero ('0'))
;;    3) info - a string containing the line read
;; process-arguments - list of arguments for process (nil is legal)
(defun process-file (infile process process-arguments)
  (setf **the-output-stream-name** "Temporary.prb")
  (with-open-file (str infile :direction :input)
    (do ((linenumber 0 (+ 1 linenumber))
	 (info (read-line str nil 'eof) (read-line str nil 'eof)))
	((eql info 'eof))
      (safe-apply t process (append (list infile linenumber info) process-arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example file-choosers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; is-file-type-p -- check if a file is a file with specified extension 
;;  file - pathname (struct) of file to be validated
;;  extension - suffix (string) of file type
;; returns: 'nil if file is not a file ending with '.log' otherwise returns 't
(defun is-file-type-p (file extension)
  (equal extension (pathname-type file)))

;; is-exactly-p -- returns t if and only if file is named name with extension extension
;;  file - pathname (struct) of file to be validated
;;  name - name (string) that needs to be matched
;;  extension - suffix (string) of file type
;; returns: 'nil if file is not file specified by name and extension
(defun is-exactly-p (file name extension)
  (and (is-file-type-p file extension)
       (equal name (pathname-name file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of ProcessFiles.cl
;; Copyright (C) 2001 by <Linwood H. Taylors (lht@lzri.com) Employer> -- All Rights Reserved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




