;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tell.cl -- debugging and messaging for code development
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  10 May 2001 - (lht) -- created
;;  13 May 2001 - (lht) -- commented and an addtional level of reporting added
;;  4 June 2001 - (lht) -- added macro support for quicker code when tell turned off
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; four levels of reporting
;;  errors, notes, informative, and general stuff
(defvar *log-ids-info* nil "Identifiers used to log informative text")
(defvar *log-ids-note* nil "Identifiers used to log warning information")
(defvar *log-ids-error* nil "Identifiers used to log errors")
(defvar *log-ids* nil "Identifiers used to log generic debugging text")
(defvar *log-indents* -1 "log indent level")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn tell on
(defun doTell () (setf *features* (append *features* '(:andes-tell))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn tell off
(defun donTell () (setf *features* (remove ':andes-tell *features*)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tell - print message at level defined -- will execute only when id has been previously defined
;; argument(s):
;;  id -- identifier for determining if message is to be printed
;;  format-string -- a standard format string used to determine format of output
;;  args -- data to be output
;; note(s):
;;  entering id in more than one level will not give multiple lines highest level has priority
;;    handling and all others are ignored
;;  If tell turned off macro expands to white space
(defmacro Tell (id format-string &rest args)
  #+:andes-tell `(Tell-Support ,id ,format-string ,@args))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun Tell-Support (id format-string &rest args) ;; Log info if id in *log-ids* (indent)
  (cond
   ((member id *log-ids-error*)
    (apply #'format *debug-io*
	   (concatenate 'string "*** Error(~w): - " format-string " ***~%") id args))
   ((member id *log-ids-note*)
    (apply #'format *debug-io* (concatenate 'string "Note(~w): " format-string "~%") id args))
   ((member id *log-ids-info*)
    (apply #'format *debug-io* format-string args))
   ((member id *log-ids*)
    (apply #'format *debug-io* (concatenate 'string "Debug(~w): " format-string "~%") id args))
   (t nil)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doLogErroradds id(s) to errors so that any tell refering to id will execute
(defun doLogError (&rest ids)
  (setf *log-ids-error* (union ids *log-ids-error*)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doLogNote -- adds id(s) to notes so that any tell refering to id will execute
(defun doLogNote (&rest ids)
  (setf *log-ids-note* (union ids *log-ids-note*)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doLogInfo -- adds id(s) to information so that any tell refering to id will execute
(defun doLogInfo (&rest ids)
  (setf *log-ids-info* (union ids *log-ids-info*)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doLog -- adds id(s) to information so that any tell refering to id will execute
(defun doLog (&rest ids)
  (setf *log-ids* (union ids *log-ids*)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; noLogError -- removes id(s) from errors -- nil empties errors
(defun noLogError (&rest ids)
  (setf *log-ids-error* (if (null ids) nil (set-difference *log-ids-error* ids))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; noLogNote -- removes id(s) from notes -- nil empties notes
(defun noLogNote (&rest ids)
  (setf *log-ids-note* (if (null ids) nil (set-difference *log-ids-note* ids))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; noLogInfo -- removes id(s) from information -- nill empties information
(defun noLogInfo (&rest ids)
  (setf *log-ids-info* (if (null ids) nil (set-difference *log-ids-info* ids))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; noLog -- removes id(s) from being logged -- nil empties all
(defun noLog (&rest ids)
  (setf *log-ids* (if (null ids) nil (set-difference *log-ids* ids))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clearLog -- removes id(s) from being logged -- nil empties all
(defun clearLog (&rest ids)
  (setf *log-ids* (if (null ids) nil (set-difference *dbg-ids* ids)))
  (setf *log-ids-error* (if (null ids) nil (set-difference *dbg-ids-info* ids)))
  (setf *log-ids-note* (if (null ids) nil (set-difference *dbg-ids-info* ids)))
  (setf *log-ids-info* (if (null ids) nil (set-difference *dbg-ids-info* ids))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dumpLog -- shows what's being logged
(defun dumpLog (&rest ids)
  (format t "~%Debugging:~%")
  (prl *log-ids*)
  (format t "~%Errors:~%")
  (prl *log-ids-error*)
  (format t "~%Warning:~%")
  (prl *log-ids-note*)
  (format t "~%Informing:~%")
  (prl *log-ids-info*)
  (format t "~%Tell is ~W%"
	  #+:andes-tell "On"
	  #-:andes-tell "Off"
	  ))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for my debugging
(defun enable-debug ()
  (donTell)
  (clearLog)
  (doTell)
  (doLog :test ;; these can be added or removed as needed
	 :do-why-wrong-equation
	 ;;:grammar-add-variable
	 ;;:grammar-remove-variable
	 ;;:grammar-clear-variables
	 ;;:do-lookup-equation-string
	 ;;:handle-bad-equation
	 ;;:handle-parsed-equation
	 ;;:handle-ambiguous-equation
	 ;;:parse
	 ;;:parse-support
	 ;;:andes-in2pre
	 ;;:parse-handler
	 ;;:symbols-enter
	 ;;:denum-mangle
	 ;;:denum
	 ;;:in2pre
	 ;;:parse-pack-to-string-lhs
	 :pre2in
	 ;;:interpret-equation
	 ;;:interpret-error
	 ;;:turn->WB-Reply
	 :do-whats-wrong
	 ;;:precheck-context
	 ;;:unrecognized-entry
	 ;;:nlg
	 ;;:interpret-equation
	 ;;:old-error
	 :why-premature
  (dumpLog)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file tell.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
