;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities.cl -- the lisp calls that echo the commands sent by the Andes 2
;;  Workbench to the Dialog Manager. They are grouped here for that common rea-
;;  son alone. Supportive functions can be found else-where.
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;  19 April 2001 - (lht) -- added packaging code
;;  10 May 2001 - (lht) -- removed packaging code and adjusted for new grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the next four routines add functionality for programatically altering code 
;; behaviour by setting or clearing 'flags ... an example of its use is the 
;; SafeApply function that follows these.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defvar *safeties* nil "Identifiers used to switch behaviour of code")
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safety - if id is in safeties then do function call fn with arguments args
(defun doSafe (id fn &rest args)
  (if (member id *safeties*) (SafeApply fn args)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; doSafety -- adds id(s) to safety checks
(defun doSafety (&rest ids)
  (setf *safeties* (union ids *safeties*)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isSafe -- returns t if id is registered as safety -- nil returned if not
(defun isSafe (id)
  (member id *safeties*))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; noSafety -- removes id(s) from safety-checks -- nill empties safeties
(defun noSafety (&rest ids)
  (setf *safeties* (if (null ids) nil (set-difference *safeties* ids))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For runtime dist, trap all Lisp errors and return special :error value
;; When debugging, manually redefine safe-apply as apply to debug on errors.
(defun SafeApply (fn &rest args)
  (if (isSafe :safe-apply)
      (let (result)
	(handler-case (setf result (apply fn args))
	  (error (c)  nil)
	  (:no-error (c) (declare (ignore c)) result)))
    (apply fn args)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun find-all (item sequence
		 &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
	   :test (complement test) keyword-args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;
(defun char-to-list (x)
  (map 'list #'(lambda (x) x) x))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun list-to-char (x)
  (map 'string #'(lambda (x) (if (characterp x) x #\.)) x))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list printer
(defun prl (x)
  (cond
   ((null x)
    (format t "NIL"))
   ((consp x)
    (dolist (obj x) (format t "~W~%" obj)))
   (t
    (format t "~W~%" x))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun add-if-not-member (x lst)
  (if (listp lst)
      (cond
       ((null x) lst)
       ((atom x) (if (member x lst) lst (append lst (list x))))
       (t (dolist (obj x) (if (member obj lst) lst (append lst (list obj))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns the first item of a list or nil
(defun if-list-first-nil (x)
  (if (consp x) (first x) nil))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun append-atom (items item)
  (append items (list item)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun showchar ()
  (do ((i 0 (+ i 1)))
      ((> i 255) nil)
    (format t "~A = ~A[~A]~%" i (code-char i) (char-name (code-char i)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun zero-or-one-symbol-p (x)
  "Is x a variable (a symbol begining with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun symbol-from-zero-or-one-symbol (x)
  (if (zero-or-one-symbol-p x)
      (read-from-string (subseq (symbol-name x) 1))
    x))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun expand-wild-symbols (xlist)
  (let ((rl0 nil) (rl1 nil))
    (dolist (obj xlist)
      (cond
       ((zero-or-one-symbol-p obj)
	(if (null rl0)
	    (setf rl1 (append rl1 (list nil) (list (list (symbol-from-zero-or-one-symbol obj)))))
	  (dolist (next rl0)
	    (setf rl1 (append rl1 (list next)))
	    (setf rl1
	      (append rl1 (list (append next (list (symbol-from-zero-or-one-symbol obj)))))))))
       (t (if (null rl0)
	      (setf rl1 (append rl1 (list (list (symbol-from-zero-or-one-symbol obj)))))
	    (dolist (next rl0)
	      (setf rl1 (append rl1 (list (append next (list obj)))))))))
      (setf rl0 rl1)
      (setf rl1 nil))
    (remove nil rl0)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adding memoize routines from norvig's text 274-275 "Paradigms or Artificial
;;  Intelligence Programming (Case studies in LISP)"
;; NOTE: many apparent bugs can be avoided by paying careful 
;; attention to the :key of the memoize
;;  function and to the :test of the memoize function

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table-2 (make-hash-table :test test)))
    (setf (get name :memo) table-2)
    #'(lambda (&rest args)
        (let ((k (funcall key args)) (table (get name :memo)))
#|
	  #+sbcl (when (> (hash-table-size table) 100)
		   (format t "hash table size for ~A is ~A ~A~%" 
	       name (hash-table-size table) (hash-table-count table)))
|#
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
 ; (clear-memoize fn-name)
  (unless (get fn-name :memo)
    ;; try to find problem with sbcl
      #+sbcl (format t "new hash table for ~A~%" fn-name)
      (setf (symbol-function fn-name)
	    (memo (symbol-function fn-name)
		  :name fn-name :key key :test test))))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name :memo)))
    (when table 
#|
      ;; try to find problem with sbcl
      #+sbcl (format t "hash table size for ~A is ~A ~A, clearing~%" 
		     fn-name (hash-table-size table) 
		     (hash-table-count table))
|#
      (clrhash table))))

;;(defun memo (fn name key test)
;;  "Return a memo-function of fn."
;;  (let ((table (make-hash-table :test test)))
;;    (setf (get name 'memo) table)
;;    #'(lambda (&rest args)
;;	(let ((k (funcall key args)))
;;	  (multiple-value-bind (val found-p)
;;	      (gethash k table)
;;	    (if found-p val (setf (gethash k table) (apply fn args))))))))

;;(defun memoize (fn-name &key (key #'first) (test #'eql))
;;  "Replace fn-name's global definition with a memoized version."
;;  (setf (symbol-function fn-name)
;;    (memo (symbol-function fn-name) fn-name key test)))

;;(defun clear-memoize (fn-name)
;;  "Clear the hash table from a memo function."
;;  (let ((table (get fn-name 'memo)))
;;    (when table (clrhash table))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list-begins-with-p - predicate to test if a list starts with a certain item (uses equal)
;; argument(s):
;;  lhs the element that we want the list to start with
;;  lst - the list to examine
;; returns:
;;  nil if the first of list is not equal to lhs otherwise returns t
(defun list-begins-with-p (lhs lst)
  (if (and (consp lst) (equal (first lst) lhs)) t nil))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; removes extraneous nils
(defun clean (x)
	      (let ((tmp nil))
		(dolist (obj x)
		  (if (consp obj)
		      (setf tmp (append tmp (list (clean obj))))
		    (if (not (equal 'nil obj))
			(setf tmp (append tmp (list obj)))))) tmp))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file utilitiess.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
