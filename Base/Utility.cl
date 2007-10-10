;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility.lisp
;; Author: Collin Lynch
;; 1/25/2000
;;
;; The functions in this file are utility functions used to support
;; the various lisp functions.  It contains code from various sources
;; grouped by suthor.
;;
;; This file provides a module named "Utility1" and must be loaded
;; as such at runtime.  It provides no package thus all functions
;; will be automatically loaded by the caller.
;;

;;; Sometimes it is inconvenient to specify an ordering for a set of objects 
;;; using a single number.  Thus we define an order specification consisting 
;;; of an alist ((class . rank) ...) where rank is a number showing the 
;;; position of the object in that class.  The classes earlier in the alist 
;;; are have higher weight.

(defun alist< (x y)
  "True if x is less than y, where x and y are alists of class-rank pairs.  Result is based on first member of x which has a matching class in y of different rank."
  ;; Since ranks are sometimes calculated via floating point, we use eps<.
  (dolist (xi x nil)
    (let ((yi (assoc (car xi) y))) ;find any class matching xi
      (when yi 
	
	(when (eps< (cdr xi) (cdr yi)) (return-from alist< t))
	(when (eps< (cdr yi) (cdr xi)) (return-from alist< nil))))))

#|
(defun alist< (x y)
  "True if x is less than y, where x and y are alists of class-rank pairs.  Result is based on first member of x which has a matching class in y of different rank."
  (when x
    (let ((yi (assoc (caar x) y))) ;find any matching class in y
      (cond ((null yi) (alist< (cdr x) y)) ;no match, rescurse
|#


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

;;;;========================================================================
;;;;
;;;; Code from Peter Norvig's Paradigms of Artificial Intelligence 
;;;; Programming auxfns.lisp file.
;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flatten - gets rid of embedded lists -- completely
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flatten (x)
  (cond
   ((null x) x)
   ((atom x) x)
   ((atom (car x)) (cons (car x) (flatten (rest x))))
   (t (append (flatten (car x)) (flatten (rest x))))))

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
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))


(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))




;;===============================================
;; Code by Collin Lynch 

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


(defun print-separator (str count)
  "Print out a separator bar."
  (let ((s str))
    (dotimes (n count)
      (setq s (strcat s str)))
    (format t (strcat S "~%"))))

  
(defun list-of-lists-p (L)
  "Return t iff L is a list of lists."
  (not (find-if #'(lambda (I) (not (listp I))) L)))


(defun force-to-list (L)
  "Force L into a list if it is not."
  (if (listp L)
      L
    (list L)))


(defun force-to-ll (l)
  (if (list-of-lists-p L)
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


(defun collect-with-index (lst)
  "Collect the elements of the list with the index 
consed on if lists or as list otherwize."
  (loop for n upto (length lst)
      collect (if (listp (nth n lst))
		  (cons n (nth n lst))
		(list n (nth n lst)))))


(defun make-count-list (count &key (type nil))
  "make a sequence of integers to count."
  (cond ((eq type 'string) 
	 (loop for n upto count
	     collect (format nil "~A" n)))
	(t (loop for n upto count
	       collect n))))


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
(defun sets-difference (S &rest Sets)
  "Obtain the set of elements in S that are not present in any of the remaining sets."
  (set-difference S (loop for Sp in Sets
			append Sp)))
       

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


;;; list-difference
;;; Given two lists compare the elements in each one.  Return nil iff the two
;;; lists are identical or a list where the first element is a list of all the
;;; elements present in the first list that are not in the second and the second
;;; element is a list of all the elements in the second list that are not present 
;;; in the first.
(defun list-differences (L1 L2 &key (Test #'equalp))
  "Compare the two lists using equalp or test iff it is provided and providea a list of the results."
  (cons (set-difference L1 L2 :test Test)
	(list (set-difference L2 L1 :test Test))))


(defun filter-list (filter lst &key (key #'identity))
  "Filter out the elements from the list using key."
  (loop for l in lst 
      when (member (funcall key l) filter
		   :test #'equalp)
      collect l))

(defun list-to-strings (lst &optional (formstr "~A"))
  (mapcar #'(lambda (i) (format nil formstr i)) lst))

(defun sort-ascending-size (lst)
  (if (not (listp lst)) lst
    (sort lst #'length-LE-comp)))

(defun length-LE-comp (x y)
  (or (and (not (listp x)) (listp y))
      (and (or (listp x) (stringp x))
	   (or (listp y) (stringp y))
	   (<= (length x) (length y)))))
      

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

;;; Sort things by the length that they 
;;; appear as strings.
(defun sort-ascending-string-length (lst)
  (if (not (listp lst)) lst
    (mapcar #'read-from-string 
	    (sort (mapcar #'princ-to-string lst)
		  #'length-LE-comp))))


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





;;; Format Justify String 
;;; Given a string containing separators such as spaces and possible
;;; newline character sequences format it within the justification
;;; cap provided.
(defun format-justify-string (string &key (cap 10) (Stream t) (newlines '("\\n")) 
					  (separators '(" ")))
  "Justify the supplied string using a soft cap"
  (do ((val) (inc 1 1) (end) (start 0)) 
      ((and End (>= end (length String))))
    (cond ((setq val (set-search newlines String :start Start 
				 :end (min (+ Start Cap) (length String))))
	   (setq end (car val))
	   (setq inc (length (cadr val))))

	  ((> cap (- (length String) start))
	   (setq end (length String))
	   (setq inc 1))
	  
	  ((setq val (set-search separators String :from-end t :start Start 
				 :end (min (+ Start Cap) (length String))))
	   (setq end (car val))
	   (setq inc (length (cadr val))))
	  
	  (t (setq end (+ start cap)) (setq inc 1)))
    
    (format Stream "~A~%" (subseq String start end))
    (setq start (+ inc end))))
      

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


;;; This function is not called by anyone so I am 
;;; commenting it out for the time being.
;;(defun print-flat-by-size (lst &optional (Stream t) (Level 0))
;;  (dolist (n (sort #'shortest lst))
;;    (format-with-padding Level "~A~%" n)))


(defun segment-string (String &optional (Segment #\space))
  (let ((L) (S String) (n))
    (do () ((String= S ""))
      (when (setq n (or (position Segment S)))      
	(push (subseq S 0 n) L)
	(setq S (subseq S (+ n 1)))))
    (reverse (cons S L))))




;;; ------------------------------------------------------------------
;;; Given a list of values filter them by the supplied function
;;; all elements that return t on the function are grouped 
;;; into sets. 
(defun group-by-func (Func Set &key (test #'equal))
  "Group the elements in set into subsets by func."
  (let (Result Tmp Tmp2)
    (dolist (S Set)
      (setq tmp (funcall Func S))
      (if (setq tmp2 (find tmp Result :key #'car :test test))
	  (push S (cdr tmp2))
	(push (list tmp S) Result)))
    Result))
	    









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

(defmacro wopen-eread (File Name &rest Body)
  (eval `(with-open-file (,File 
			  ,Name
			  :direction :input
			  :if-does-not-exist :error)
	   ,@Body)))
    
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


;;; Given a set of lists generate a list of those list items paired.
(defun listpair-lists (&rest lists)
  "Pair up the lists of lists."
  (when (not (null (car lists)))
    (listpair-list-o-lists 
     (mapcar #'list (car lists))
     (cdr lists))))
    
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


;;;----------------------------------------------------------------------------
;;; Code by Lynwood Taylor.
;;;
;;; This is a replacement for eval.  According to the Allegro Lisp licensing,
;;; one is not supposed to use eval (which compiles lisp code) in the
;;; runtime code that is distributed.
;;;
(defun andes-eval (expr &optional (environment nil))
  "For use in place of eval ... works in nlg.cl but may need further cases checked."
  (cond
   ((symbolp expr) 
    (or (cdr (assoc expr environment)) (symbol-value expr)))
   ((atom expr) expr)
   ((eq (car expr) 'quote) (cadr expr))
   ((atom (car expr)) (apply (car expr) (mapcar #'andes-eval (cdr expr))))
   (t (apply (andes-eval (car expr)) (mapcar #'andes-eval (cdr expr))))))  


;;;----------------------------------------------------------------------
;;; Time decoding.
;;; Universal time can be decoded but there are no easy named functions
;;; to do so.  The following functions are those named values.
;;; The values are:
;;;     Second: Integer 0-59
;;;     Minute: Integer 0-59
;;;     Hour:   Integer 0-23
;;;     Date:   Integer between 1-31
;;;     Month:  integer 1-12
;;;     Year:   Should be full four digit year e.g 2002
;;;     Day-of-week:  Integer 0-6 Mon-Sun
;;;     Daylight-savings-time: t or nil if active.
;;;     Time-Zone:  Real # representing number of hours west of GMT
;;;                 Non int because some countries use <1hr changes.
(defun get-universal-time-component (Component Time)
  "Get the specified component from the Universal time."
  (let ((Decoded (multiple-value-list (decode-universal-time Time))))
    (case Component
      (Second (nth 0 Decoded))
      (Minute (nth 1 Decoded))
      (Hour (nth 2 Decoded))
      (Date (nth 3 Decoded))
      (Month (nth 4 Decoded))
      (Year (nth 5 Decoded))
      (Day-of-week (nth 6 Decoded))
      (Daylight-Savings-time (nth 7 Decoded))
      (Time-Zone (nth 8 Decoded)))))


;;; Get the specified component from the 
;;; current time.
(defun get-current-time-component (Component)
  "Get the specified component from the current time."
  (get-universal-time-component Component (get-universal-time)))

;;; Get the specified component (by name)
;;; from the supplied universal time.

(defun get-universal-time-second (Time)
  "Get the second from the supplied time."
  (get-universal-time-component 'Second Time))

(defun get-universal-time-Minute (Time)
  "Get the Minute from the supplied time."
  (get-universal-time-component 'Minute Time))

(defun get-universal-time-Hour (Time)
  "Get the Hour from the supplied time."
  (get-universal-time-component 'Hour Time))

(defun get-universal-time-Date (Time)
  "Get the Date from the supplied time."
  (get-universal-time-component 'Date Time))

(defun get-universal-time-Month (Time)
  "Get the Month from the supplied time."
  (get-universal-time-component 'Month Time))

(defun get-universal-time-Year (Time)
  "Get the Year from the supplied time."
  (get-universal-time-component 'Year Time))

(defun get-universal-time-Day-of-week (Time)
  "Get the Day-of-week from the supplied time."
  (get-universal-time-component 'Day-of-week Time))

(defun get-universal-time-Daylight-saving-time (Time)
  "Get the Daylight-Savings-Time from the supplied time."
  (get-universal-time-component 'Daylight-savings-time Time))

(defun get-universal-time-Time-Zone (Time)
  "Get the Time-Zone from the supplied time."
  (get-universal-time-component 'Time-Zone Time))


;;; Get the specified component (by name)
;;; from the supplied universal time.

(defun get-current-time-second ()
  "Get the second from the supplied time."
  (get-current-time-component 'Second))

(defun get-current-time-Minute ()
  "Get the Minute from the supplied time."
  (get-current-time-component 'Minute))

(defun get-current-time-Hour ()
  "Get the Hour from the supplied time."
  (get-current-time-component 'Hour))

(defun get-current-time-Date ()
  "Get the Date from the supplied time."
  (get-current-time-component 'Date))

(defun get-current-time-Month ()
  "Get the Month from the supplied time."
  (get-current-time-component 'Month))

(defun get-current-time-Year ()
  "Get the Year from the supplied time."
  (get-current-time-component 'Year))

(defun get-current-time-Day-of-week ()
  "Get the Day-of-week from the supplied time."
  (get-current-time-component 'Day-of-week))

(defun get-current-time-Daylight-saving-time ()
  "Get the Daylight-Savings-Time from the supplied time."
  (get-current-time-component 'Daylight-savings-time))

(defun get-current-time-Time-Zone ()
  "Get the Time-Zone from the supplied time."
  (get-current-time-component 'Time-Zone))




(defun collect-smallest (Set &key (Sizefunc #'length))
  "Collect the shortest lists in the set."
  (let ((size (funcall Sizefunc (car Set)))
	(shortest (list (car Set))))
    (dolist (Elt (cdr Set))
      (cond ((= (funcall Sizefunc Elt) Size)
	     (push Elt Shortest))
	    ((< (funcall Sizefunc Elt) Size)
	     (setq Size (funcall Sizefunc Elt))
	     (setq Shortest (list Elt)))))
    (reverse Shortest)))

   

;;; -----------------------------------------------
;;; Sum-lists are used by the Behavior Studies code.
;;; This will be necessary for the Cmd test code.

(defun sum-lists2 (&rest Rlists)
  (apply #'mapcar #'+ Rlists))

(defun sum-lists (&rest Rlists)
  (declare (type (cons (cons integer *) *) Rlists)
	   (optimize speed (safety 0)))
  (let ((Len (length (car Rlists)))
	(A (make-list (length (car Rlists)) :initial-element 0)))
    (dolist (L Rlists)
      (dotimes (N Len)
	(setf (nth N A) (+ (nth N A) (nth N L)))))
    A))

(defun sum-int-list-pair (L1 L2)
  (declare (type (cons integer *) L1)
	   (type (cons integer *) L2)
	   (optimize speed (safety 0)))
  (let ((Len (length L1)) (R (make-list (length L1))))
    (dotimes (N Len)
      (setf (nth N R) (+ (nth N L1) (nth N L2))))
    R))
