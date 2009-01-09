;;  memoize routines from norvig's text 274-275 "Paradigms or Artificial
;;  Intelligence Programming (Case studies in LISP)"
;;; Copyright (c) 1991 Peter Norvig
;;; Modifications by Anders Weinstein 2000-2008
;;; Modifications by Brett van de Sande, 2005-2008
;;
;;  There is also a version by Tim Bradshaw, but that is overkill for us.
;;

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

;; BvdS:  Repeatedly applying memoize to a function causes a stack overflow
;; in sbcl.  Instead, clear memo if already memoized.
(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version, or clear memo if already memoized."
  (if (get fn-name 'memo)
      (clear-memoize fn-name)
      (setf (symbol-function fn-name)
	    (memo (symbol-function fn-name)
		  :name fn-name :key key :test test))))

;; BvdS:  Add test that function has already been memoized.
(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (if table (clrhash table)
	(error "Function ~A not memoized." fn-name))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
