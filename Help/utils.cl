;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils.cl -- the lisp calls that echo the commands sent by the Andes 2
;;  Workbench to the Dialog Manager. They are grouped here for that common rea-
;;  son alone. Supportive functions can be found else-where.
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;  19 April 2001 - (lht) -- added packaging code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun find-all (item sequence
		 &rest keyword-args
		 &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence
	     :test-not (complement test-not) keyword-args)
    (apply #'remove item sequence
	   :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun char-to-list (x)
  (map 'list #'(lambda (x) x) x))

(defun list-to-char (x)
  (map 'string #'(lambda (x) (if (characterp x) x #\.)) x))


(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (cl:defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table-2 (make-hash-table :test test)))
    (setf (get name :memo) table-2)
    #'(lambda (&rest args)
        (let ((k (funcall key args)) (table (get name :memo)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name :memo)))
    (when table (clrhash table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prl (x)
  (cond
   ((null x) nil)
   (t (format t "~A~%" (car x)) (prl (rest x)))))

(defun add-if-not-member (x lst)
  (if (listp lst)
      (cond
       ((null x) lst)
       ((atom x) (if (member x lst) lst (append lst (list x))))
       (t (dolist (obj x) (if (member obj lst) lst (append lst (list obj))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; returns the first item of a list or nil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun if-list-first-nil (x)
  (if (consp x) (first x) nil))

(defun append-atom (items item)
  (append items (list item)))

(defun showchar ()
  (do ((i 0 (+ i 1)))
      ((> i 255) nil)
    (format t "~A = ~A[~A]~%" i (code-char i) (char-name (code-char i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file utils.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
