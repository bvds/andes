;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tools.cl -- routines to support general lisping parsing of student logs written by workbench
;; Copyright (C) 2001 by <Linwood H. Taylor's (lht@lzri.com) Employer> -- All Rights Reserved
;; Author(s): Linwood H. Taylor (lht@lzri.com)
;; Modified:
;;      13 December 2001 - (lht) -- created/split from SLog2English.cl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; capture list errors and write error to stream
(defconstant **trap-lisp-errors** t) ;; t turns safe-apply on

(defun safe-apply (stream fn args)
  (if **trap-lisp-errors**
      (let (result)
	(handler-case (setf result (apply fn args))
	  (error (c) (format stream "**** Error: executing command ~A.~%" c) :error)
	  (:no-error (c) (declare (ignore c)) result)))
    (apply fn args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formats paragraphs to block width characters wide
(defconstant ***width*** 72)

;; returns the location of the character after the end of word
(defun z-next-word (str)
  (cond
   ((equal str "") 0)
   ((position #\Space str) (position #\Space str))
   (t (length str))))

;; the optional indent is to support hanging indentation (where body is indented but opening
;; line is not
(defun l-format (the-str &optional (indent "        "))
  (let ((frm "") (cnt 0) (str (string-trim " " the-str)))
    (do ((done nil (equal (length (string-trim " " str)) 0)))
	(done (string-trim " " frm))
      (let ((tmp (z-next-word str)))
	(when (> tmp 0)
	  (if (>= (+ tmp cnt) ***width***)
	      (setf frm (concatenate 'string frm (format nil "~A" #\LineFeed) indent
				     (string-trim " " (subseq str 0 tmp))))
	    (setf frm (concatenate 'string frm " " (string-trim " " (subseq str 0 tmp)))))
	  (setf str (string-trim " " (subseq str tmp)))
	  (if (>= (+ tmp cnt) ***width***)
	      (setf cnt tmp)
	    (setf cnt (+ cnt tmp 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of Tools.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's (lht@lzri.com) Employer> -- All Rights Reserved
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
