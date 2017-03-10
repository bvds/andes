;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig
;;; Taken from file auxfns.lisp which may be found at 
;;; http://www.norvig.com/paip/auxfns.lisp
;;; 
;;; Some of these routines may have been modified.


(defun mappend (fn &rest lists)
  "Append the results of calling fn on each element of list.
  Like mapcan, but uses append instead of nconc."
  (apply #'append (apply #'mapcar fn lists)))

(defun mapunion (fn list &key (test #'eql))
  "Union the results of calling fn on each list element."
  (remove-duplicates (mappend fn list) 
		     :test test))

(defun mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))

(defun flatten (x)
  "Remove embedded lists."
  (if (consp x)
      (append (flatten (car x)) (flatten (rest x)))
      x))


(defun flatten1 (exp)
  "Get rid of embedded lists (to one level only)."
  (mappend #'mklist exp))


(defun remove-duplicates-order-preserve (lst &optional (elts ()))
  (cond ((null lst) Elts)
	((member (car lst) elts) (remove-duplicates-order-preserve (cdr lst) elts))
	(t (remove-duplicates-order-preserve (cdr lst) (append elts (list (car lst)))))))


(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))


(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed.  Handle commas in sbcl, version > 1.2.2."
  (cond ((consp tree)
         (unique-find-anywhere-if
          predicate
          (first tree)
          (unique-find-anywhere-if predicate (rest tree)
                                   found-so-far)))
        #+sbcl((sb-impl::comma-p tree)
               (unique-find-anywhere-if predicate (sb-impl::comma-expr tree)
                                        found-so-far))
        ((funcall predicate tree)
         (adjoin tree found-so-far))
        (t found-so-far)))


(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?   
   Handle commas in sbcl, version > 1.2.2."
  (cond ((consp tree)
         (or (find-anywhere-if predicate (first tree))
             (find-anywhere-if predicate (rest tree))))
        #+sbcl((sb-impl::comma-p tree)
               (find-anywhere-if predicate (sb-impl::comma-expr tree)))
        (t (funcall predicate tree))))
