;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; This file provides a general main file for the file processor
;; as defined by Linwood taylor.  

;; 

;; 
;; 1. ProcessFiles.cl  -- Processing loops for iterating over individual or groups of files.
;;  The main functions are:
;;     (process-files <directory> <file-chooser> <file-chooser-arguments> <process> <process-arguments>)
;;     (process-dirs <directory> <file-chooser> <file-chooser-arguments> <process> <process-arguments>)
;;
;;        Where: <Directory> is a string-form pathname where the files are to be found.
;;               <File-Chooser> is a predicate function that takes a pathname (struct)
;; 	                        and returns t iffFile should be processed else nil.
;;               <File-Chooser-arguments> A (possibly empty) list of arguments to file-chooser
;;               <Process> A function (typically process-file) that will be called on each line
;;                         of the file in order to determine what shall be done with it.
;;               <Process-Arguments> A list of arguments for PROCESS that will be passed to it after
;;                                   the line when it is called.
;;
;;     (defun process-file <infile> <process> <process-arguments>) -- Process a single file
;;        Where: <infile> is a pathname struct for the file to be processed.
;;               <Process> is a function of (at least) three args that will be called on
;;                         Each line of the file.
;;                          1) infile - the pathname (struct) of the file being processed
;;                          2) linenumber - the current line number being read (starts at zero ('0'))
;;                          3) info - a string containing the line read
;;               <process-arguments> - list of arguments for process (nil is legal)
;;
;; --------------------------------------------------------------------------------------------
;; 2. SLogParse.cl     -- Top level parsing code for the log files.  
;;   This file defines a single function parse-line that can be used as the parser in the 
;;   functions above.   This will break each line into 3 parts; time, command, and args
;;   hands them off to the supplied subroutines to process those parts.  
;;   (parse-line <file> <lineno> <line> <handler> &optional <handler-args=nil>)
;;     Where the arguments are:
;;        file - file read
;;        linenumber - the number of the line read
;;        line - the data contained on that line
;;        handler - function that gets passed parse data (takes at least 5 args)
;;                  a) an output stream
;;                  b) the current linenumber
;;                  d) string containing the time line was entered
;;                  d) string containing the command
;;                  e) string containing the arguments for the command
;;        handler-arguments - optional list of arguments to handler
;;        NOTE: must define the function make-parse-line-output-file-stream-name (file 
;;              lineno line) that will return a string representing the filename of the 
;;              output stream
;;
;; ----------------------------------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test routines and development support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rld () ;; reload file after possible edits
  (load "C:/Code/LogUtils/SLog2English.cl")
  (load-new))
(defun lng () ;; reload file after possible edits
  (load "C:/Code/LogUtils/SLog2English")
  (load-fas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run () ;; test run batch
  (process-dirs "C:/Andes2/Help/Evals/" #'is-file-type-p '("log") #'parse-line '(handle-parse)))
(defun run2 () ;; test run batch
  (process-files "C:/Andes2/Help/Evals/" #'is-file-type-p '("log") #'parse-line '(handle-parse)))
(defun run3 () ;; test run batch
  (process-files "C:/Andes2/Help/Evals/" #'is-file-type-p '("LOG") #'parse-line '(handle-parse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun chk (&optional (args '("034068-Aug30-20-11-48" "log")) 
		      (dir "C:/Andes2/Help/Evals/")) ;;B1-Nov15-21-11-45
  ;;(prof:start-profiler)
  (process-files dir #'is-exactly-p args #'parse-line '(handle-parse))
  ;;(prof:stop-profiler)
  (handle-time-report))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slog-file-main (File)
  (process-file File #'parse-line '(handle-parse)))

(defun slog-files-main (Files)
  (process-files Files #'is-file-type-p '("log") #'parse-line '(handle-parse)))
