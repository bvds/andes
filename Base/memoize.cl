;;  memoize routines from norvig's text 274-275 "Paradigms or Artificial
;;  Intelligence Programming (Case studies in LISP)"
;;; Copyright (c) 1991 Peter Norvig
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2008
;;
;;  There is also a version by Tim Bradshaw, but that is overkill for us.
;;

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun get-memo-table (m)
  "If m is a hash table or a symbol pointing to one, return it, else nil."
  (cond ((hash-table-p m) m)
	((and (symbolp m) (hash-table-p (symbol-value m)))
	 (symbol-value m))))

(defun memo (fn &key (key #'first) (test #'eql) name var)
  "Return a memo-function of fn."
  (let ((thehash (make-hash-table :test test)))
    (if (and var (symbolp var))
	(setf (symbol-value var) thehash)
	(setf var thehash))
    ;; At this point var is either a hash table or a symbol pointing to one.
    (setf (get name 'memo) var)
    #'(lambda (&rest args)
        (let ((k (funcall key args))
	      (table (get-memo-table var)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

;; BvdS:  Repeatedly applying memoize to a function causes a stack overflow
;; in sbcl.  Instead, clear memo if already memoized.
(defun memoize (fn-name &key (key #'first) (test #'eql) var)
  "Replace fn-name's global definition with a memoized version, or clear memo if already memoized.  Setting var to a symbol causes hash table to be saved in var."
  (if (get-memo-table (get fn-name 'memo))
      (clear-memoize fn-name)
      (setf (symbol-function fn-name)
	    (memo (symbol-function fn-name)
		  :name fn-name :key key :test test :var var))))

;; BvdS:  Add test that function has already been memoized.
(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get-memo-table (get fn-name 'memo))))
    (if table (clrhash table)
	(error "Function ~A not memoized." fn-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
