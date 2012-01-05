;; Utility.lisp
;; Author: Collin Lynch 1/25/2000
;;; Modifications by Anders Weinstein 2002-2008
;;; Modifications by Brett van de Sande, 2005-2008
;;; Copyright 2009 by Kurt Vanlehn and Brett van de Sande
;;;  This file is part of the Andes Intelligent Tutor Stystem.
;;;
;;;  The Andes Intelligent Tutor System is free software: you can redistribute
;;;  it and/or modify it under the terms of the GNU Lesser General Public 
;;;  License as published by the Free Software Foundation, either version 3 
;;;  of the License, or (at your option) any later version.
;;;
;;;  The Andes Solver is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with the Andes Intelligent Tutor System.  If not, see 
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The functions in this file are utility functions used to support
;; the various lisp functions.

;;; Sometimes it is inconvenient to specify an ordering for a set of objects 
;;; using a single number.  Thus we define an order specification consisting 
;;; of an alist ((class . rank) ...) where rank is a number showing the 
;;; position of the object in that class.  The classes earlier in the alist 
;;; are have higher weight.


(defun alist< (x y)
  "True if x is less than y, where x and y are alists of class-rank pairs.  Result is based on first member of x which has a matching class in y of different rank.  The orders of x and y must be consistent."
  (when x ;; empty list is equal to any list of pairs, return nil
    (let ((yy (member (caar x) y :key #'car)))
      ;; if there is a match, test that list orders are consistent 
      ;; by looking at previous pairs in y and subsequet pairs in x
      (when (and yy (intersection (ldiff y yy) (cdr x) :key #'car))
	(error "conflicting list orders for ~A and ~A" x y))
      (cond ((null yy) (alist< (cdr x) y)) ;no match, try next term	    
	    ((eps< (cdar x) (cdar yy)) t)
	    ((eps< (cdar yy) (cdar x)) nil)
	    (t (alist< (cdr x) (cdr yy))))))) ;tie, try next term 


(defun eps< (x y &key (roundoff 1.0e-8))
  "Version of < that allows for possible roundoff errors."
  (unless (< (abs (- x y)) (* roundoff (+ (abs x) (abs y)))) (< x y)))

;;;;================================ =====================
;;;;
;;;;                             expr< 
;;;;
;;;;===============================  ======================

;;; expr< is needed in order to generates sets where the elements are ordered.
;;; Otherwise the code will generate all possible orderings of the elements
;;; in the sets.  Does a tree walk, and is true if first non-equal leaf is 
;;; string<.

(defun expr< (expr1 expr2)
  "True if first arg comes before second in lexicographic ordering."
  (cond ((consp expr1)
	 (cond ((consp expr2)
		(or (expr< (car expr1) (car expr2))
		    (and (equal (car expr1) (car expr2))
			 (expr< (cdr expr1) (cdr expr2)))))
	       ((variable-p expr2) 
		(error "Can't determine order for unbound variable ~A." expr2))
	       (T nil))) ;atoms precede cons in our ordering
	;; expr1 is now an atom
	((variable-p expr1) 
	 (error "Can't determine order for unbound variable ~A." expr1))
	((consp expr2) t) ;atoms preceed cons
	((numberp expr1)
	 (cond ((Numberp expr2) (< expr1 expr2))
	       (t nil)))   ;numbers follow other atoms
	;; expr1 is now a non-numerical atom
	((numberp expr2) t) ;numbers follow other atoms
	(T (string< expr1 expr2)) ;both are non-numeric atoms
	))


;;;------------------------------------------------------------------
;;; Expression Processing.
;;; In the some instances it is necessary to operate on an expression
;;; or on a set of arbitary expressions to perform some computation
;;; this code does that.

;;; recursive-find-if
;;; Recursive form of find-if.  This will collect the set of symbols 
;;; for which predicate returns t.  If a list passes the predicate 
;;; then that list will be returned else it will be recursed into.
(defun recursive-find-if (Predicate Set &optional (Result nil))
  "Perform a recursive find-if with Predicate P on set S returning R."
  (cond ((Null Set) Result)
	
	((funcall Predicate (car Set)) 
	 (recursive-find-if 
	  Predicate (cdr Set) 
	  (cons (car Set) Result)))
	
	((listp (car Set)) 
	 (recursive-find-if 
	  Predicate (cdr Set) 
	  (append Result (recursive-find-if Predicate (Car Set)))))
	
	(t (recursive-find-if Predicate (cdr Set) Result))))

;;; Unique-recursive-find-if
;;; Get a unique list of elements after doing a recursive find if
(defun unique-recursive-find-if (Predicate Set &key (test #'equalp))
  (remove-duplicates (recursive-find-if Predicate Set) :test test))


;;; recursive-exp-dimensions
;;; Given an arbitrary expression return its dimensions according to
;;; the supplied predicate.  An expression'd dimensions in terms of
;;; some predicate is defined as the number of distinct elements 
;;; within it returned by a unique-recursive-find-if accrording
;;; to the supplied predicate.  
;;;
;;; The most common use of this is determining the dimension of an
;;; equation by identifying the number of unique variables within
;;; it.  (note this is only algebraically correct for linear 
;;; equations.  
(defun calc-exp-dimensions (P E &key (test #'Equalp))
  (length (unique-recursive-find-if P E :test test)))

;;; recursive-exp-set-dimensions
;;; Similar to the listing above save that the dimension of a 
;;; set of expressions is computed as the number of unique
;;; elements in the whole set minus the set size plus 1.  Again 
;;; this will most commonly be used on sets of equations for 
;;; variable dimensions but can apply to a greater number of 
;;; elements.
;;;
;;; For code sake this can be treated as a single exp search.
(defun calc-exp-set-dimensions (P E &key (test #'equalp))
  (+ 1 (- (calc-exp-dimensions P E :test test)
	  (length E))))



;;; Recursively test to determine iff Elt is a member of 
;;; Seq including descending into subtrees and return the
;;; first true instance in a DFS.
;;; Note Depth is not maintianed in the result.
(defun recursive-member (Elt Seq &key (test #'equal))
  "Recursively test for membership."
  (cond ((null Seq) nil)
	((funcall test Elt (car Seq)) Seq)
	((or (null (car Seq)) (atom (car Seq))) 
	 (recursive-member Elt (cdr Seq) :test Test))
	(t (or (recursive-member Elt (car Seq) :test Test) 
	       (recursive-member Elt (cdr Seq) :test Test)))))




(defun format-string-list (Strings &optional (Stream t) (Level 0))
  "Format a list of strings to the specified stream."
  (dolist (S Strings)
    (pprint-indent :current Level Stream)
    (format Stream "~W~%" S)))
  
(defun list-of-lists-p (L)
  "Return t iff L is a list of lists."
  (not (find-if #'(lambda (I) (not (listp I))) L)))


(defun force-to-list (L)
  "Force L into a list if it is not."
  (if (listp L)
      L
    (list L)))

(defun wrap-if (x) 
  "Wrap in a list if x is not nil."
  (if x (list x) x))


(defun list-of-atoms-p (L)
  "Return t iff L is a list of atoms."
  (and (listp L)
       (not (find-if #'listp L))))


(defun push-index (L)
  "Destructively push an int of index long onto each element in L."
  (dotimes (N (length L))
    (push N (nth N L)))
  L)


(defun tagp (tag)
  "Return t iff tag is of the form '<Value>'."
  (when (atom tag)
    (let ((S (string tag)))
      (and (equal #\< (char S 0))
	   (equal #\> (char S (- (length S) 1)))))))	    


(defun strcat (&rest strings)
  "Concatenate the supplied strings."
  (apply #'concatenate (cons 'string strings)))
	 

(defmacro postpend (Dest val)
  "Push but for the end."
  `(setq ,Dest ,`(append ,Dest (list ,Val)))) 

(defun contains-sym (Exp Sym)
  "Return t iff Exp contains the symbol Sym."
  (and (not (null Exp)) 
       (or (equalp (car Exp) Sym)
	   (and (listp (car Exp))
		(contains-sym (car Exp) Sym))
	   (contains-sym (cdr Exp) Sym))))


;;--------------------------------------------------------------------------
;; Calculate the number of differences between Trees a and b.
(defun tree-diff (Tree1 Tree2 &optional (count 0))
  (cond ((and (null Tree1) (null Tree2)) Count)
	((or (null Tree1) (null Tree2)) (+ 1 Count))
	((listp (car Tree1)) (tree-diff-l Tree1 Tree2 Count))
	(t (tree-diff-a Tree1 Tree2 Count))))


(defun tree-diff-l (Tree1 Tree2 Count)
  "The tree diff if car Tree1 is a list."
  (if (listp (car Tree2))
      (tree-diff (cdr Tree1) (cdr Tree2) 
		 (+ Count (tree-diff (car Tree1) (car Tree2) 0)))
    (tree-diff (cdr Tree1) (cdr Tree2) (+ Count 1))))


(defun tree-diff-a (Tree1 Tree2 Count)	
  "The tree diff if car Tree1 is an atom."
  (if (or (listp (car Tree2)) (not (equalp (car Tree1) (car Tree2))))
      (tree-diff (cdr Tree1) (cdr Tree2) (+ Count 1))
    (tree-diff (cdr Tree1) (cdr Tree2) Count)))




;;--------------------------------------------------------------------------
;; Sets code.
       

(defun sets-intersect (Set1 Set2 &key (Test #'equal))
  "Obtain the intersection of the sets."
  (loop for S in Set1
      when (find S Set2 :test Test)
      collect S))


(defun equal-sets (x y &key (test #'unify))
  "Return true iff the sets are equal, unify is default test."
  (if (and (listp x) (listp y))
      (and (subsetp x y :test test)
	   (subsetp y x :test test))
    (funcall test x y)))

(defun subset (Object Sequence &key (test #'equal) key)
  "Like remove if not with an object test."
  (if key
      (remove-if-not #'(lambda (x) (funcall test Object x)) Sequence :key key)
    (remove-if-not #'(lambda (x) (funcall test Object x)) Sequence)))
  
(defun set-split (filter set)
  "Split the set by filter."
  (let ((y) (n))
    (dolist (s set)
      (if (funcall filter s)
	  (push s y)
	(push s n)))
    (values y n)))


;;; ------------------------------------------------------
;;; Sometimes we want to locate the specific differences
;;; in a pair of lists.  The functions here collect the 
;;; lists and the differences between them.

;;; Locate the first location (index #) in the lists that 
;;; differs or nil if there is none.
(defun first-list-diff-loc (L1 L2 &optional (count 0))
  "Locate the first diff in L1 and L2."
  (format t "~a ~a ~a~%" L1 L2 Count)
  (cond ((or (and (null L1) L2)
	     (and L1 (null L2)))
	 (+ 1 Count))
	
       ((and (null L1) (null L2))
	Count)
       
       (t (first-list-diff-loc (cdr L1) (cdr L2) count))))


;;; Locate all the points where L1 and L2 differ accounting
;;; for differing lengths.
(defun count-list-diff-loc (L1 L2 &optional (count 0))
  (cond ((and (null L1) (null L2)) Count)
	((and (null L1) L2) (+ Count (length L2)))
	((and L1 (null L2)) (+ Count (length L1)))
	((equalp (car L1) (car L2)) 
	 (count-list-diff-loc (cdr L1) (cdr L2) Count))
	(t (count-list-diff-loc (cdr L1) (cdr L2) (+ 1 Count)))))


;;; --------------------------------------------------------
;;; list-diff
;;; This code was added from the BehaviorTests for the cmd.cl
;;; code.
;;; Given a pair of lists determine the number of locations 
;;; within them that differ adding up excess lengths.  
(defun list-diff (A B &optional (count 0))
  (if (or (null A) (null B))
      (+ Count (length A) (length B))
    (if (equalp (car A) (car B))
	(list-diff (cdr A) (cdr B) Count)
      (list-diff (cdr A) (cdr B) (+ 1 Count)))))

(defun list-difference (A B &key (test #'unify))
  "like set-difference, except that list order is significant"
    (cond
     ((null A) nil)
     ((null B) A)
     ((funcall test (car A) (car B)) 
      (list-difference (cdr a) (cdr b) :test test))
     ;; handle unambiguous insertions and deletions
     ((not (member (car a) b :test test))
      (cons (car A) (list-difference (cdr a) b :test test)))
     ((not (member (car b) a :test test))
      (list-difference a (cdr b) :test test))
      ;; treat everything else as a discrepency
     (t (cons (car A) (list-difference (cdr a) (cdr b) :test test)))))
  
;;; -------------------------------------------------


;; Given a sequence and a set of subsequences search for 
;; them in priority order and return the location of the 
;; first one found and it in a list else return nil.
(defun set-search (Set Sequence &key (from-end nil) (Start 0) (End))
  "Search through the sequence for elements from the set."
  (when Set
    (let ((R (search (car Set) Sequence :from-end from-end 
		     :start2 Start :end2 (if End End (length Sequence)))))
      (if R (list R (car Set)) 
	(set-search (cdr Set) Sequence :from-end from-end :start Start :end End)))))

;;------------------------------------------------------
;; printing info.

(defun bp (Bar L &rest form)
  (barprint Bar L form))

(defun barprint (Bar Lv form)
  (dotimes (L Lv)
    (format t "~%"))
  (format t "~A  " Bar)
  (apply #'format t form)
  (format t "  ~A~%" Bar))



;;---------------------------------------------------------------------
;; file macros

(defmacro wopen-scwrite (File Name &rest Body)
  "call with open file supersede with body."
  `(with-open-file (,File 
		    ,Name
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
     ,@Body))
    
(defun fprint (obj &optional (Stream t) (depth 0))
  (when (listp obj)
    (let ((D depth))
      (format Stream "(~%")
      (dolist (o obj)
	(cond ((eq o 'split) (incf d))
	      ((eq o 'join) (decf d)))
	
	(dotimes (n d)
	  (format Stream " "))
	(format Stream "~A~%" o))
      (format Stream ")~%"))))

    
(defun listpair-list-o-lists (init lists)
  (cond ((null lists) init)
	(t (let* ((count (max (length init) (length (car lists)))) 
		  (R (make-list count)))
	     (dotimes (n count)
	       (setf (nth n R)
		 (append (nth n init) (list (nth n (car lists))))))
	     (listpair-list-o-lists R (cdr lists))))))
	
		
(defun qlist (lst)
  (if (= (length lst) 1)
      (eval `(list '',(car lst)))
    (eval `(cons '',(car lst) ,`(qlist ',(cdr lst))))))    


;;-----------------------------------------------------------------------------
;; code by collin Lynch.
(defun func-eval (expressions)
  "Evaluate the expression by wrapping it as a funcall and applying."
  (funcall (append '(lambda ()) expressions)))

