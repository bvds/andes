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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Not sure how to handle punctuation here.
;;
;; Syntax <model>: (<model> ...)  ordered sequence
;;                 (and <model> ...) orderless sequence 
;;                 (or <model> ...)  this is exclusive or
;;                 (preferred <model>) optional, but hinted for
;;                 (allowed <model>)  optional, but not hinted for
;;                 (conjoin <conjunction> <model> ...)  conjoin orderless
;;                               sequence (<model> ...) using <conjunction>, 
;;                               where <conjunction> is of type <model>.
;;                 (key <model>) give higher weight to model
;;                 (case-sensitive <model>) use case-sensitive matching
;;                 (case-insensitive <model>) use case-insensitive matching
;;                               (default)
;;                 (var <quant> :namespace <space>)  match student variable 
;;                               for <quant> in namespace <space>.
;;                 (eval <lisp> ...)  execute <lisp> as lisp code 
;;                 (<atom> ...)  if <atom> matches none of above, 
;;                               match with ontology
;;                 <string>      leaf nodes, after resolution, are strings
;;
;; (var ...) and (eval ...) are not handled by functions in this file.
;; Parse of student sentence is (<string> ...)
;; Could also have more complicated structure:
;;     <student>: (<student> ...)
;;                <string>
;; which would allow chunking based on punctuation.  But matching
;; to the model is much more difficult in that case.
;;

;; operations:  Resolve <quant> references, break up strings.
;;              Expand to expression (or <string> ...) for testing
;;                  with flag for preferred/allowed. 
;;              Compare <student> with <model> giving a distance metric.
;;                  Metric is minimum number of words added and deleted to
;;                  match model sentence.
;;              Make <model> subtrees (preferred ...) based on Gricean 
;;                  conversational maxims (minimum needed to distinguish
;;                  quantity).
;;

;; ctl-c ctl-k compiles entire file 
(in-package :cl-user)

(defpackage :match
	  (:use :cl)
	  (:export :*whitespace* :best-model-matches :word-parse 
		   :unknown-object-handler :match-model
		   :test-for-list
		   :word-count :word-count-handler
		   ;; Accessors for struct returned by
		   ;; match-model and best-model-matches.
		   :best-value :best-word :best-extra :best-prop
		   :make-best :copy-best :best-p
		   ;; Hook for iterating over list
		   :iteration-function :*iteration-bindings*
		   :*key-multiplier
		   :*word-cutoff*
		   :initialize-word-count-memo
		   :matches-model-syntax :word-string :*grammar-names*))

(eval-when (:load-toplevel :compile-toplevel)
  (defparameter match:*grammar-names* 
    (mapcar #'string-upcase ;lisp symbol default is upper case.
	    '("preferred" "allowed" "and" "or" "conjoin" "var" "eval" "key"
	      "case-sensitive" "case-insensitive")))

  ;; For each symbol in the grammar, if the symbol is not defined somewhere 
  ;; define symbol in :cl-user.  Then import each symbol into :match.  
  ;; This allows user to use grammar independent of :match.
  ;;
  ;; If this strategy doesn't work, an alternative would be to use 
  ;; keywords for the grammar symbols.
  (dolist (symbol-string match:*grammar-names*)
    (multiple-value-bind (symbol internal) (intern symbol-string)
      (when (eql internal :internal) (export symbol))
      (import symbol :match))))

(in-package :match)

(deftype unsigned-integer () (type-of 10000))

(defstruct best 
  (value 10000) ;score
  words         ;list containing best match
  extra         ;list containing other stuff, like bindings
  prop          ;props supplied to best-model-matches
)

(defun combine-best (b1 b2)
  "Destructively combine two best structs.  The second arg is overwritten."
  ;; Normally the props should be null.
  (unless (eql (best-prop b1) (best-prop b2)) 
    (warn "combine-best can only combine equal props, got ~A ~A"
	  (best-prop b1) (best-prop b2)))
    (setf (best-value b2) (+ (best-value b1) (best-value b2)))
    (setf (best-words b2) (append (string-list (best-words b1))
                                (string-list (best-words b2))))
    (setf (best-extra b2) (append (best-extra b1) (best-extra b2)))
   b2)

(defun string-list (x) (if (stringp x) (list x) x))

;; Now, create an internal list of these symbols from *grammar-names*
(defvar *grammar-symbols* (mapcar #'find-symbol *grammar-names*))

(defparameter *whitespace* '(#\Space #\Tab #\Newline))
(defparameter *word-delimiters* (append *whitespace* (list #\,)))

(defvar *key-multiplier* 2 "Multiplication factor for matching key submodels.")

(defvar *case-sensitive* nil "Whether to do case-sensitive matching.")

(defun test-for-list (x)
  (and (consp x) (or (stringp (car x)) (listp (car x)))))

(defun matches-model-syntax (form)
  "Top level of form matches model syntax." 
  ;; If this function is true, then form will not be
  ;; interpreted as being in the ontology
  (or (null form)
      (stringp form)
      (test-for-list form)
      (and (consp form) (member (car form) *grammar-symbols*))))

(defun word-parse-count (string)
  "Count number of words in a string."
  ;; This is a simplification of the standard utility split-sequence
  (let ((len (length string)))
    (loop for left = 0 then (1+ right)
	  for right = (or (position-if 
			   #'(lambda (x) (member x *word-delimiters*))  
			   string :start left)
			  len)
	  count (not (= left right))
	  until (>= right len))))

(defun word-parse (string)
  "Break up a string into a list of words, removing whitespace, commas."
  ;; This is a simplification of the standard utility split-sequence
  (let ((len (length string)))
    (loop for left = 0 then (1+ right)
          for right = (or (position-if 
			   #'(lambda (x) (member x *word-delimiters*))  
			   string :start left)
			  len)
	  unless (= left right)
          collect (subseq string left right)
	  until (>= right len))))
  
(defun join-words (x)
  "Join together a list of strings."
  (if (cdr x) (concatenate 'string (car x) " " (join-words (cdr x)))
      (car x)))

(defun word-string (model)
  "Make string phrase out of model."
  ;; Generally, we assume the given order of (and ...) and 
  ;; the first member of (or ....) is the preferred choice.
  (cond 
    ((stringp model) model)
    ((test-for-list model)
     ;; mapcar copies list; subsequent operations can be destructive
     (join-words (delete nil (mapcar #'word-string model))))
    ((member (car model) '(key case-insensitive case-sensitive))
     (when (second model) (word-string (second model))))
    ((eql (car model) 'and)
     (when (cdr model) (word-string (cdr model))))
    ((eql (car model) 'or)
     (pop model) ;remove 'or
     ;; Find first non-null element
     (loop while model thereis (word-string (pop model))))
    ((eql (car model) 'conjoin)
     (pop model)
     (let ((conjunction (word-string (car model)))
	   ;; mapcar copies list; subsequent operations can be destructive
	   (items (delete nil (mapcar #'word-string (cdr model)))))
       (cond 
	 ((null conjunction)
	  (warn "conjoin must have conjunction ~A" model)
	  (join-words items))
	 ((cdr items) 
	  ;; doesn't add commas, as it should, when (cdr (butlast items))
	  (join-words (nconc (butlast items) 
			      (list conjunction) (last items))))
	 (t (car items)))))
    ((eql (car model) 'allowed) nil)
    ((eql (car model) 'preferred) (word-string (cdr model)))
    (t (warn "word-string can't do ~A" model))))

(defun default-word-count-handler (model max)
  (warn "word-count:  unknown object ~A" model)
  (if max 10000 0))

(defvar word-count-handler 'default-word-count-handler 
  "By setting this function, one can extent the model grammar.")

;; This is an idea to memoize word-count function since
;; it operates repeatedly on a given model tree.
;; On an intial test, it reduced time from 5.6 seconds to 3.64 seconds.

(defvar *word-count-memo* nil)

(defun initialize-word-count-memo ()
  "In cases where repeated matches are made to a given model or several models are constructed from a submodel, then it is useful to cache the word counts."
  (if *word-count-memo*
      (progn
	(clrhash (car *word-count-memo*))
	(clrhash (cdr *word-count-memo*)))
      (setf *word-count-memo*
	    ;; Important that we reuse conses when constructing
	    ;; model trees.
	    (cons (make-hash-table :test #'eql :size 1000)
		  (make-hash-table :test #'eql :size 1000)))))

(defun word-count (model &key max)
  "find minimum (or maximum) word count in model"
  ;; Allow memoization of this function.
  (if *word-count-memo*
      ;; Strings have same upper and lower bound.
      ;; However, benchmarking shows that testing for
      ;; strings slows things down.
      (multiple-value-bind (val found-p)
	  (gethash model 
		   (if max (car *word-count-memo*) 
		       (cdr *word-count-memo*)))
	(if found-p val
	    (setf (gethash model 
			   (if max (car *word-count-memo*) 
			       (cdr *word-count-memo*)))
		  (word-count-in model max))))
      (word-count-in model max)))

(defun word-count-in (model max)
  "find minimum (or maximum) word count in model"
  ;; In general, arguments of the model can be nil.
  (cond 
    ((stringp model) (word-parse-count model))  ;count words in string
    ((null model) 0)
    ((or (atom model) (cdr (last model)))  ;test for a proper list
     (funcall word-count-handler model max))
    ;; from here on, model is a proper list
    ((member (car model) '(key case-insensitive case-sensitive))
     (word-count (second model) :max max))
    ((test-for-list model)
     (word-count-list model max))
    ((eql (car model) 'and)
     (word-count-list (cdr model) max))      ;remove the 'and 
    ((eql (car model) 'or)
     ;; don't use loop here because we have to switch between
     ;; maximize and minimize
     (apply (if max #'max #'min)
	    (mapcar #'(lambda (x) (word-count x :max max)) 
		    (cdr model))))
    ((eql (car model) 'conjoin)
     (let ((args (word-count (cddr model) :max max)))
       (if (> args 1) 
	   (+ args (word-count (second model) :max max)) ;add conjuction 
	   args)))  ;; 0 or 1 args, drop conjunction
    ((member (car model) '(allowed preferred)) 
     ;; Ignore subesequent arguments
     (if max (word-count (cadr model) :max max) 0))
    (t (funcall word-count-handler model max))))

(defun word-count-list (model max)
  (cond
    ((consp model)
      (+ (word-count (car model) :max max)
	 (word-count-list (cdr model) max)))
    ((null model) 0)
    (t (error "word-count-list:  invalid list ~A" model))))

(defun sort-by-complexity (models)
  "Sort an alist of models by increasing complexity"
  (let ((mm (mapcar #'(lambda (x) (cons (model-complexity (car x)) x))
		   models)))
    (mapcar #'cdr (sort mm #'< :key #'car))))

(defun model-complexity (model)
  "Estimate number of alternatives, when expanded."
  ;; Here, we just count number of expansions.
  (cond 
    ((stringp model) (word-parse-count model)) ;count words in string
    ((null model) nil)
    ((member (car model) '(key case-insensitive case-sensitive))
     (model-complexity (second model)))
    ((test-for-list model)
     ;; mapcar copies list; subsequent operations can be destructive
     (apply #'* (delete nil (mapcar #'model-complexity model))))
    ((eql (car model) 'and)
     (let ((rest (delete nil (mapcar #'model-complexity (cdr model)))))
       (* (factorial (length rest))
	  (apply #'* rest))))
    ((eql (car model) 'conjoin)
     (let ((rest (delete nil (mapcar #'model-complexity (cddr model)))))
       (* (factorial (length rest))
	  (apply #'* rest)
	  (if (> (length rest) 1) 
	      (model-complexity (second model)) ;add conjunction 
	      1))))  ;; 0 or 1 args, drop conjunction
    ((eql (car model) 'or) 
     (apply #'+ (delete nil (mapcar #'model-complexity (cdr model)))))
    ((member (car model) '(allowed preferred)) 
     (+ 1 (model-complexity (cdr model))))
    (t (warn "model-complexity found unexpected form ~A" model) 0)))
  
(defun factorial (x)
  (if (> x 1) (* x (factorial (- x 1))) 1))

(defmacro update-bound (best x)
  (let ((this-best (gensym)))
    `(let ((,this-best ,x))
       (when (< (best-value ,this-best) (best-value ,best)) 
	 (setf ,best ,this-best))))) 

(declaim (ftype (function (unsigned-integer unsigned-integer unsigned-integer)
			  unsigned-integer) match-bound))
(defun match-bound (lstudent l-model u-model)
  "Gives lower bound for a match based on word count"
  (max 0 (- lstudent u-model) (- l-model lstudent)))


(defun default-object-handler (student model best)
  (declare (ignore student best))
    (error "match-model:  Bad tree ~A" model))

(defvar unknown-object-handler 'default-object-handler 
  "By setting this function, one can extent the model grammar.")


(defvar iteration-function nil
  "This function takes a model and returns a list of values that *iteration-bindings* should be iterated over.  This iteration is performed for model structures: list, and, conjoin.")

;; Variable whose dynamic bindings are iterated over.
(defvar *iteration-bindings*)

(defmacro iterate-over (model matching-function)
  (let ((best (gensym)) (m-b (gensym)))
    `(if iteration-function
	 (let ((,best (make-best)))
	   (dolist (,m-b (funcall iteration-function ,model))
	     (let ((*iteration-bindings* ,m-b))
	       (update-bound ,best ,matching-function)))
	   ,best)
	 ;; Bypass all this if iteration function has not been specified.
	 ,matching-function)))

(defun match-model (student model &key (best 10000) l-model u-model)
  "Recursive match to tree, returns minimum word insertion/addition for match.  Should check for valid model structure before calling match-model."
  ;; for profiling
  (declare (notinline match-model match-model-and match-model-list match-model-conjoin)) 

  ;; See if there is any hope, based on word count, of doing better 
  ;; than bound
  (let ((this (match-bound 
	       (length student) 
	       (or l-model (word-count model)) 
	       (or u-model (word-count model :max t)))))
    (unless (< this best) (return-from match-model (make-best :value this))))

  (cond 
    ((stringp model)
     (let ((words (word-parse model)))
       (if (cdr words)
	   ;; don't need iteration wrapper here, since everything is bound.
	   (match-model-list student words best :words t)
	   ;; profiling shows that just calculating is slightly
	   ;; faster than also testing against the global best.
	   (let ((best 1))  ;score for any match to one student word.
	     (dolist (item student)
	       (setf best 
		     (min best 
			  (normalized-levenshtein-distance 
			   item (car words)))))
	     ;; best fit plus any extra student words.
	     ;; If student is nil, this should return 1.
	     (make-best :value (+ best (max 0 (- (length student) 
						 (length words))))
		        :words (car words))))))
    ;; expand-new-english and expand-vars remove any nil's from
    ;; the model tree.  This is equivalent to forbidding any matches
    ;; to it.
    ((null model) (make-best :value (if student 10000 0)))
    ((or (atom model) (cdr (last model)))  ;test for a proper list
     (funcall unknown-object-handler student model best))
    ;; from here on, model must be a proper list
    ((eql (car model) 'key)
     (let ((result (match-model student (second model) 
				:best (/ best *key-multiplier*))))
       (setf (best-value result) (* *key-multiplier* (best-value result)))
       result))
    ((member (car model) '(case-insensitive case-sensitive))
     (let ((*case-sensitive* (eql (car model) 'case-sensitive)))
       (match-model student (second model) :best best)))
    ;; model optional
    ((member (car model) '(preferred allowed))
     ;; don't match model
     (let ((result (make-best :value (min best (length student))))) 
       ;; Any (cddr model) is ignored.
       (update-bound result (match-model student (second model) 
				       :best (best-value result)))
       result))
    ;; Case (<model> ...)
    ((test-for-list model)
     (if (cdr model)
	 (iterate-over model
		       (match-model-list student model best))
	 (match-model student (car model) :best best)))
    ((eql (car model) 'and)
     (pop model)
     (cond 
       ((cdr model) ;two or more arguments of "and"
	(iterate-over model
		      (match-model-and student model best)))
       ;; and of one argument
       (model (match-model student (car model) :best best))
       (t (make-best :value (length student))))) ;empty "and"
    ((eql (car model) 'or)
     (pop model)
     (cond 
       ((cdr model) ;two or more arguments of "or"
	(let ((result (make-best :value best)))
	  (dolist (item model)
	    (update-bound result (match-model student item 
					      :best (best-value result))))
	result))
       (model (match-model student (car model) :best best))
       (t (make-best :value (length student))))) ;empty "or"
    ((eql (car model) 'conjoin)
     (pop model)
     (cond 
       ((cddr model) ;two or more items to conjoin
	(iterate-over model
		      (match-model-conjoin student model best)))
       ;; conjunction of one argument
       ((cdr model) (match-model student (second model) :best best))
       (model (make-best :value (length student))) ;empty conjunction
       (t (error "conjoin must always have a conjunction"))))
    (t
     (funcall unknown-object-handler student model best))))

(defun match-model-list (student model best &key words)
  (declare (notinline match-model)) ;for profiling
  ;; for n student words and m elements of the model list,
  ;; m n (n+1)/2 matches must be evaluated.  The following
  ;; is based on the Levenstein minimum edit distance algorithm.

  (let* ((width (1+ (length student)))
	 (col (make-array width))
	 (prev-col (make-array width))
	 (u-model (unless words 
		    (mapcar #'(lambda (x) (word-count x :max t)) model)))
	 (l-model (unless words 
		    (mapcar #'word-count model)))
	 (up 0)
	 (ur (if words (length model) (apply #'+ u-model)))
	 (lr (if words ur (apply #'+ l-model))))
    (declare (dynamic-extent col prev-col)
	     (unsigned-integer up ur lr))

    (dotimes (y width)
      (setf (svref prev-col y) 
	    (make-best :value y))) ;student is one word per slot

    ;; When the we can't to better than best-minus-rest for an
    ;; iteration of the z loop, then (aref d (1+ x) y) becomes 
    ;; equal to best-minus-rest, then any
    ;; matchings using that particular student word grouping will
    ;; be above the bound "best."
    
    (dotimes (x (length model))
      
      ;; Fill in to handle cases that are cut out by bounds.
      (dotimes (y width) (setf (svref col y) nil))

      (let ((ux (if words 1 (nth x u-model)))
	    (lx (if words 1 (nth x l-model))))
	(declare (unsigned-integer ux lx))

	(decf ur ux)
	(decf lr lx)

	(let ((uy (+ best up ux))
	      (ly (+ (- best) (- ur) (length student)))
	      (uz (+ best up))
	      (lz (+ (- best) (- ux) (- ur) (length student)))
	      (uyz (+ best ux))
	      (lyz (+ (- best) (- up) (- ur) (length student))))
	  
	  (do ((y (max 0 (+ (floor ly) 1)) (+ y 1)))
	      ((= y width) (>= y uy))
	    	    
	    ;; initial value so update-bound will work below
	    (setf (svref col y) (make-best))

	    (let ((best-minus-rest 
		   (- best (match-bound (- (length student) y) lr ur))))
	      
	      (do ((z (max 0 (+ (floor lz) 1) (+ (floor (- y uyz)) 1)) (+ z 1)))
		  ;; include z=y case
		  ((> z y) (>= z uz) (<= (- y z) lyz))
		
		(when (svref prev-col z)
		  (update-bound 
		   (svref col y)
		   (combine-best 
		    (svref prev-col z) 
		    (match-model (subseq student z y) (nth x model)
		 		 :best (- (min best-minus-rest 
					       ;; Also, exceed
					       ;; our best, so far
					       (best-value (svref col y)))
					  (best-value (svref prev-col z)))
				 :u-model ux :l-model lx))))))))

	(incf up ux))
      (rotatef col prev-col))
    
    ;; in case none comes out, go with best.
    (or (svref prev-col (length student)) (make-best :value best))))


;; The problem here is a generalization of the "Assignment 
;; problem"  The generalization being that several consecutive
;; student words may be assigned to one element of the model list.
;; It is unclear whether this generalization has a polynomial-time
;; solution.

;; It is not clear that it is worth further optimizing 
;; this, since this search does not include the search over all
;; systementries.  It may make better sense to find a strategy
;; that is optimized for the search over systementries.

(defun match-model-and (student model best)
  (declare (notinline match-model)) ;for profiling
  (let* ((width (1+ (length student)))
	 (best-result (make-best :value best))
	 ;; nil means skip
	 (matches (make-array (list (length model) width width)
			      :initial-element nil))
	 (model-free (loop for i below (length model) collect i))
	 (u-model (mapcar #'(lambda (x) (word-count x :max t)) model))
	 (l-model (mapcar #'word-count model))
	 (u-net (apply #'+ u-model))
	 (l-net (apply #'+ l-model)))
    (declare (dynamic-extent matches))
    
    ;; Iterate over possible matchings between model and student,
    ;; removing cases which cannot contribute due to the bounds.
    (dotimes (m (length model))

      (let* ((um (nth m u-model))
	     (lm (nth m l-model))
	     (uq (+ best um))
	     (lq (+ (- best) (- u-net) um (length student))))
	
	;; do triangle of matrix, including diagonals, where q=y-z
	;; q is the number of student words.
	;; Use bounds based on lengths to constrain q.
	(do ((q (max 0 (+ (floor lq) 1)) (+ q 1)))
	    ((>= q width) (>= q uq))
	  
	  (let ((best-minus-rest 
		 (- best (match-bound (- (length student) q) 
					      (- l-net lm) 
					      (- u-net um)))))
	    
	    (do ((y q (+ y 1)))
		((= y width))
	      (let* ((z (- y q))			  
		     (this (match-model (subseq student z y) (nth m model) 
					:best best-minus-rest
					:l-model lm :u-model um)))
		;; This gives a 50% improvement.
		(when (< (best-value this) best-minus-rest) 
		  (setf (aref matches m y z) this))))))))

    ;; Quick, but does not get global minimum.
    (update-bound best-result (match-model-greedy 
			matches model-free 
			(list (cons 0 (length student)))))
    
    ;; Simply iterate through all possibilities.
    ;; For n student words and m elements of the model list,
    ;; there are m! (m+n-1)!/(n! (m-1)!) different possible matches.
    (update-bound best-result (match-model-slow 
			matches model-free 
			(list (cons 0 (length student)))))
  
  ;; Simply iterate through all possibilities using by creating
  ;; a set of sequential lists.  This is even slower!
  #+never (dolist (item model)
	      (update-bound best-result
			    (match-model
			     student
			     ;; remove is non-destrutive
			     (list item (cons 'and (remove item model)))
			     :best best)))
  best-result))


;; This is designed to be fast, but is not a global best fit.
;; However, it may possibly be a starting point for a 
;; polynomial time global best fit algorithm.
(defun match-model-greedy (matches model-free student-intervals)
  "Greedy best fit tree search. Not necessarily global best."
  (let ((best-score -10000) best-m best-y best-z best-interval)
    ;; Find best match that includes the most words.
    (dolist (m model-free)
      (dolist (interval student-intervals)
	(let ((lower (car interval)) (upper (cdr interval)))
	  (do ((y lower (1+ y)))
	      ((= y (1+ upper))) 
	    (do ((z lower (1+ z)))
		;; Loop over lower triangle and diagonals.
		((> z y)) 
	      ;; Ignore any elements that are nil.  Thus we don't have
	      ;; to calculate elements that will never be a best fit.
	      (when (aref matches m y z) 
		(let ((score (- (- y z) ;number of student words
				;; Weight must be larger than 1 to favor
				;; fewer words when more doesn't improve match.
				;; Weight must be less than infinity to favor
				;; longer matches over shorter matches.
				(* 2 (best-value (aref matches m y z))))))
		  ;; (format t "  looping m y z=~A score=~A~%" (list m y z) score)
		  (when (> score best-score)
		    (setf best-score score)
		    (setf best-m m)
		    (setf best-interval interval)
		    (setf best-y y)
		    (setf best-z z)))))))))
    ;; (format t "choose m y z=~A score=~a~%" (list best-m best-y best-z) best-score)

    (if best-m ;check that matches array is non-empty
	;; remove best fit interval and add new intervals
	(let ((new-student (remove best-interval student-intervals)))
	  (push (cons (car best-interval) best-z) new-student)
	  (push (cons best-y (cdr best-interval)) new-student)
	  
	  (combine-best
	   (aref matches best-m best-y best-z)
	   (if (remove best-m model-free)
	       ;; Find best fit with this match removed.
	       (match-model-greedy matches 
				   (remove best-m model-free) 
				   new-student)
	       ;; count remaining student words
	       (make-best :value (add-interval-lengths new-student)))))
	(make-best))))

(defun add-interval-lengths (intervals)
  (loop for interval in intervals
	sum (- (cdr interval) (car interval))))

;; Simply iterate through all possibilities.
;; For n student words and m elements of the model list,
;; there are m! (m+n-1)!/(n! (m-1)!) different possible matches.

;; If this proves to be too slow, may have to find a polynomial-time
;; algorithm.  See:
;; "QuickMatch: A Very Fast Algorithm for the Assignment Problem", 
;;                  James B. Orlin, Yusin Lee
;; Lecture notes "Bipartite Matching & the Hungarian Algorithm
;;     http://www.cse.ust.hk/~golin/COMP572/Notes/Matching.pdf
;; 
(defun match-model-slow (matches model-free student-intervals)
  "Exhaustive (slow) search through all possibilities for matching student to orderless set of model phrases."
  (if model-free
      (let ((best (make-best)))
	(dolist (interval student-intervals)
	  (let ((lower (car interval)) (upper (cdr interval))
		;; Faster to remove and then copy list each time.
		(remaining-intervals (remove interval student-intervals)))
	    (do ((y lower (1+ y)))
		((= y (1+ upper))) 
	      (do ((z lower (1+ z)))
		  ;; Loop over lower triangle and diagonals.
		  ((> z y)) 
		;; Ignore any elements that are nil.  Thus we don't have
		;; to calculate elements that will never be a best fit.
		(when (aref matches (car model-free) y z) 
		  ;; remove best fit interval and add new intervals
		  (let ((new-student (copy-list remaining-intervals)))
		    (push (cons lower z) new-student)
		    (push (cons y upper) new-student)
		    (update-bound 
		     best 
		     (combine-best
		      (aref matches (car model-free) y z) 
		      ;; remove best fit interval and 
		      ;; add new intervals
		      (match-model-slow matches 
					(cdr model-free) 
					new-student)))))))))
	  best)
	;; count remaining student words
      (make-best :value (add-interval-lengths student-intervals))))

(defun match-model-conjoin (student model best)
  (declare (notinline match-model)) ;for profiling
  (let ((conjunction (pop model))
	(best-result (make-best :value best)))
    ;; Right now, this does not handle commas at all.
    (cond 
      ((cddr model) ; more than two
       ;; For now, just use dumb recursion because we don't have
       ;; any long lists, but this is very expensive, computationally.
       (dolist (item model)
	 (update-bound 
	  best-result
	  (match-model student `(,item (conjoin ,conjunction 
					,@(remove item model)))
		       :best (best-value best-result)))))
      ((cdr model) ;two arguments
       ;; Try the two possible orders
       (update-bound 
	best-result 
	(match-model student (list (first model) conjunction (second model))
		     :best best))
       (update-bound 
	best-result 
	(match-model student (list (second model) conjunction (first model))
		     :best best)))
      (t (error "match-model-conjoin should never reach here")))
  best-result))

(defparameter *debug-print* nil)  ;; debug print in best-model-matches

(defun best-model-matches (student models &key (cutoff 4) (epsilon 0.01))
  "Returns a list of best matches to text using match-model. Models is an alist of models and props."
  (declare (notinline match-model))
  (when *debug-print*
    (format t "best-model-matches for ~A models, cutoff ~A~%" 
	    (length models) cutoff))
  ;; cutoff is the maximum allowed score.
  ;; equiv maximum fraction of the best score such that a fit
  ;;    is considered equivalent to the best fit.

  ;; match-model only finds matches that are better than 
  ;; than the given bound, else it may return the bound itself.  
  ;; Thus, in the case where a nearly perfect match has been found, 
  ;; we need to adjust the bound so any other perfect matches 
  ;; may also be found.

  ;; Single-match:  return just first instance of best match,
  ;; assuming integer-valued scores.
  ;; This allows for a more efficient search.

  (unless (>= cutoff 0)
    (warn "best-model-matches:  cutoff=~A  must be nonnegative." cutoff))
  (let (this (best cutoff) quants 
	     (bound (+ cutoff epsilon))
	     (t0 (if *debug-print* (get-internal-run-time) 0)))
    ;; Do easier ones first, to establish better bound.
    ;; We have  have to do each time, since results of any eval or var 
    ;; is needed for sort.
    (dolist (x (sort-by-complexity models))
      (let ((t0 (if *debug-print* (get-internal-run-time) 0)))
	(setf this (match-model student (car x) :best bound))
	(when (and nil *debug-print*) ;too noisy for some cases
	  (format t "     Got ~A for match in ~Fs to~%       ~A~%" 
		  (best-value this)  
		  (/ (float (- (get-internal-run-time) t0))
		     (float internal-time-units-per-second))
		  (cdr x))))
      (when (< (best-value this) bound) 
	(setf (best-prop this) (cdr x))
	(push this quants))
      (when (< (best-value this) best) 
	(setf best (best-value this))
	(setf bound (+ best epsilon))))
    ;; Remove any quantities that are not equivalent with best fit
    ;; and return result in original order.
    (setf quants
	  (nreverse
	   (delete-if #'(lambda (x) (> (best-value x) bound)) quants)))
    (when *debug-print*
      (format t "  Got ~A matches, score ~a, total time ~A~@[ for:~%  ~A~]~%"
	      (length quants) best
	      (/ (float (- (get-internal-run-time) t0))
		 (float internal-time-units-per-second))
	      (mapcar #'best-prop quants)))
    quants))

;; Imposing a cutoff on word matching improves speed by 50%
;; and removes some accidental matches (words
;; that are clearly different, but have some letter overlap).
(defvar *word-cutoff* 0.4 "Assume words don't match for normalized distances larger than this cutoff.")

(defun normalized-levenshtein-distance (s1 s2)
  "Normalize levenshtein-distance so complete rewrite is 1.0 and imposing match cutoff."
  (let ((maxl (max (length s1) (length s2)))
	(minl (min (length s1) (length s2))))
    ;; Test if there is any hope of getting below cutoff
    ;; This gives a 20% improvement in speed.
    (if (> (* maxl (- 1 *word-cutoff*)) minl)
	(float 1)
	(let ((x (levenshtein-distance s1 s2)))
	  (if (> x (* maxl *word-cutoff*))
	      (float 1)
	      (/ (float x) (float maxl)))))))

;; Not used:  in Benchmarking, this is
;; significantly slower than no test at all.
(defun distance-lower-bound (str1 str2)
  "Get lower bound for Levenshtein distance."
  ;; This is order n*log(n) in string length n, while 
  ;; the Levenshtein algorithm is order n^2.
  (let ((s1 (sort (copy-seq str1) #'char<))
	(s2 (sort (copy-seq str2) #'char<))
	(matches 0))
    (do ((i 0) (j 0))
	((or (= i (length s1)) (= j (length s2))))
      (cond ((char> (schar s1 i) (schar s2 j))
	     (incf j))
	    ((char< (schar s1 i) (schar s2 j))
	     (incf i))
	    ;; match
	    (t (incf i) (incf j) (incf matches))))
    (- (max (length s1) (length s2)) matches)))

;; Levenshtein Distance function.  
;; From http://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp
;; This is considerably faster than the one at 
;;        http://www.cliki.net/Levenshtein 

(defun levenshtein-distance (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (let ((n (length str1))
	(m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
	  ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'unsigned-integer))
	  (prev-col (make-array (1+ m) :element-type 'unsigned-integer)))
      (declare (dynamic-extent col prev-col))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (aref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
	(setf (aref col 0) (1+ i))
	(dotimes (j m)
	  (setf (aref col (1+ j))
		(min (1+ (aref col j))
		     (1+ (aref prev-col (1+ j)))
		     (+ (aref prev-col j)
			(if (if *case-sensitive*
				(char= (schar str1 i) (schar str2 j))
				(char-equal (schar str1 i) (schar str2 j))) 
			    0 1)))))
	(rotatef col prev-col))
      (aref prev-col m))))
