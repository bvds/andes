;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lrdc-errors.cl -- error handlers for Andes 2 Dialog Manager
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;  26 March 2001 - (lht) -- added customizing functions: set-error-stream,
;;                           enable-erros, and disable-errors and adjusted
;;                           comments to reflect these changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *error-stream* nil
  "Holds the stream that errors are written to.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *errors-on* nil
  "If t then errors are sent to *error-stream* else are ignored.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error-message -- prints a user defined error message as a lisp expression
;;   presumably for later processing to the stream *error-stream*
;; argument(s):
;;   message is a string or list representing the error that occured
;; returns:
;;   the string resultant (for now)
;; note(s):
;;   outputs string to desired stream if *errors-on* is true
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun error-message (message)
  (if *errors-on*
      (if (stringp message)
	  (format t "(Error: ~A)~%" message)
	(format t "(Error: ~A)~%" message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-error-stream - sets error messages to be output to the specified stream
;; argument(s):
;;  new-stream -- the stream to be used for output (nil performs as in format)
;; returns:
;;  ?????
;; note(s):
;;  entire purpose is to say where to output error messages -- so this will
;;  change the error reporting behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-error-stream (new-stream)
  (setf *error-stream* new-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable-errors - turns on error reporting
;; argument(s):
;;  NONE
;; returns:
;;  t
;; note(s):
;;  entire purpose is to affect the behavior of error reporting by turning on
;;  the reporting of error messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enable-errors ()
  (setf *errors-on* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable-errors - turns error reporting off
;; argument(s):
;;  NONE
;; returns:
;;  nil
;; note(s):
;;  entire purpose is to affect the behavior of error reporting by turning off
;;  (or ignoring) the reporting of error messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun disable-errors ()
  (setf *errors-on* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file lrdc-errors.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
