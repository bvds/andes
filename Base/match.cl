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

(defparameter *whitespace* '(#\Space #\Tab #\Newline))

;; Not sure how to handle punctuation here.
;;
;; Syntax <model>: (<model> ...)  ordered sequence
;;                 (and <model> ...) orderless sequence 
;;                 (or <model> ...)  this is exclusive or
;;                 (var <quant>)   match student variable for <quant>
;;                 (ont <quant>)   match with ontology
;;                 (preferred <model>) optional, but hinted for
;;                 (allowed <model>)  optional, but not hinted for
;;                 <string>      leaf nodes, after resolution, are strings
;; Not sure if we need the following
;; and unsure of proper name, maybe merge with (and ...) above
;;    (nl-and <model> ...) orderless list, expressed with "and"
;;    (nl-or <model> ...) orderless list, expressed with "or"
;;
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

(defun word-parse (str &key parse)
  "Break up a string into a list of words, removing spaces, commas."
  (let ((p (position '(#\space #\tab #\,) str
		     :test #'(lambda (x y) (member y x)))))
    (if p
	(word-parse (subseq str (+ p 1)) :parse 
		    (if (> p 0) (push (subseq str 0 p) parse) parse))
	(reverse (if (> (length str) 0) (push str parse) parse)))))

(defun word-count (model &key max)
  "find minimum (or maximum) word count in model"
  (cond 
    ((null model) 0)
    ((stringp model) 1) ;; or count words in string
    ((or (listp (car model)) (stringp (car model)))
     (loop for x in model sum (word-count x :max max)))
    ((eql (car model) 'and)
     (if (cdr model)
	 (word-count (cdr model) :max max) 	 ;remove the 'and 
	 0))
    ((eql (car model) 'or)
     (if (cdr model)
	 ;; don't use loop here because we have to switch between
	 ;; maximize and minimize
	 (apply (if max #'max #'min)
		  (mapcar #'(lambda (x) (word-count x :max max)) 
			  (cdr model)))
	 0))
    ((member (car model) '(allowed preferred)) 
     (if max (word-count (cdr model) :max max) 0))
    (t (warn "word-count found unexpected form ~A" model) (if max 10000 0))))

(defmacro update-bound (best x)
  `(let ((this ,x))
     (when (< this ,best) (setf ,best this))))

(defun match-bound (student model)
  "Gives lower bound for a match based on word count"
  ;; assume student is list of words.
  (max (- (length student) (word-count model :max t))
       (- (word-count model) (length student))
       0))

(defun match-model (student model &key (best 20000))
  "Recursive match to tree, returns minimum word insertion/addition for match."

  ;; When bound is given, see if there is any hope, based on word count,
  ;; of doing better.
  (when (< best 10000)
      (let ((this (match-bound student model)))
	(unless (< this best) (return-from match-model this))))

  (cond 
    ((null student) (word-count model))
    ((null model) (word-count student))
    ((stringp model)
     (let ((best 10000)) ;ignore any global value of best
       (dolist (item student)
	 (update-bound best (normalized-levenshtein-distance item model)))
       ;; best fit plus any extra student words.
       (+ best (- (word-count student) 1))))
    ;; model optional
    ((member (car model) '(preferred allowed))
     (update-bound best (word-count student)) ;don't match model
     (update-bound best (match-model student (second model) :best best))
     best)
    ;; Case (<model> ...)
    ((or (stringp (car model)) (listp (car model)))
     (if 
      (cdr model)
      ;; for n student words and m elements of the model list,
      ;; m n (n+1)/2 matches must be evaluated.  The following
      ;; is based on the Levenstein minimum edit distance algorithm.
      (let* ((width (1+ (length student)))
	     (height (1+ (length model)))
	     (d (make-array (list height width))))
	(dotimes (y width)
	  (setf (aref d 0 y) y)) ;student is one word per slot
	(dotimes (x (length model))
	  (dotimes (y width)
	    (let ((mini (+ (word-count (nth x model)) (aref d x y))))
	      (dotimes (z y)
		(update-bound 
		 mini
		 (+ (aref d x z)
		    (match-model (subseq student z y) (nth x model)
				 ;; Need to determine empirically
				 ;; if including bound improves speed.
				 :best (- (min best mini) (aref d x z))))))
	      (setf (aref d (1+ x) y) mini))))
	(aref d (length model) (length student)))
      (match-model student (car model) :best best)))
    ((eql (car model) 'and)
     (pop model)
     (cond 
       ((cdr model) ;two or more arguments of "and"
	
	;; The problem here is a generalization of the "Assignment 
	;; problem"  The generalization being that several consecutive
	;; student words may be assigned to one element of the model list.
	;; It is unclear whether this generalization has a polynomial-time
	;; solution.

	;; Here, we try to find best fit using greedy search alogrithm.
	;; Hopefully, this will speed up the exhaustive (slow) search after.

	;; It is not clear that it is worth further optimizing 
	;; this, since it does not help with the search over all
	;; systementries.  It may make better sense to find a strategy
	;; that is optimized for the search over systementries.

	(let* ((width (1+ (length student)))
	      (matches (make-array (list (length model) width width)))
	      (model-free (loop for i below (length model) collect i)))
	  ;; Blindly collecting all possible matches is itself inefficient
	  (dotimes (m (length model))
	    (dotimes (y width)
	      ;; diagonal elements are all the same.
	      (setf (aref matches m y y) (word-count (nth m model)))
	      ;; do one triangle of off-diagonal elements
	      (dotimes (z y)
		(setf (aref matches m y z) 
		      (match-model (subseq student z y) (nth m model) 
				   :best best)))))
	  (update-bound best (match-model-and matches model-free 
					      (list (list 0 (length student)))))
	  )
	
	;; Simply iterate through all possibilities.
	;; For n student words and m elements of the model list,
	;; there are m! (m+n-1)!/(n! (m-1)!) different possible matches.

	;; This is pretty slow!  The previous alorithm may, in practice,
	;; be sufficient to get a "good enough" match.
	(dolist (item model)
         (update-bound best
                       (match-model
                        student
			;; remove is non-destrutive
                        (list item (cons 'and (remove item model)))
                        :best best)))
	best)
       (model (match-model student (car model) :best best))
       (t (word-count student)))) ;empty "and"
    ((eql (car model) 'or)
     (pop model)
     (cond 
       ((cdr model) ;two or more arguments of "or"
	(dolist (item model)
	  (update-bound best (match-model student item :best best)))
	best)
       (model (match-model student (car model) :best best))
       (t (word-count student)))) ;empty "or"
    (t (error "Bad trees ~A ~A" student model))))

;; This is designed to be fast, and may lead to a 
;; polynomial time global best fit algorithm.
(defun match-model-and (matches model-free student-intervals)
  "Greedy best fit tree search. Not necessarily global best."
  (let* ((best-score -10000) best-m best-y best-z best-interval)
    ;; Find match that uses the most words.
    (dolist (m model-free)
      (dolist (interval student-intervals)
	(let ((lower (first interval)) (upper (second interval)))
	  ;; (format t " interval ~A~%" interval)
	  (do ((y lower (1+ y)))
	      ((= y (1+ upper))) 
	    (do ((z lower (1+ z)))
		;; Loop over lower triangle and first diagonal element only.
		((= z (max y (1+ lower)))) 
	      (let ((score (- (- y z) ;number of student words
			      ;; Weight must be larger than 1 to favor
			      ;; fewer words when additions don't improve match.
			      ;; Weight must be less than infinity to favor
			      ;; longer matches over shorter matches.
			      (* 2 (aref matches m y z)))))
		;; (format t "  looping m y z=~A score=~A~%" (list m y z) score)
		(when (> score best-score)
		  (setf best-score score)
		  (setf best-m m)
		  (setf best-interval interval)
		  (setf best-y y)
		  (setf best-z z))))))))
    ;; (format t "choose m y z=~A score=~a~%" (list best-m best-y best-z) best-score)
    ;; add new intervals
    (let ((new-student (remove best-interval student-intervals)))
      (push (list (first best-interval) best-z) new-student)
      (push (list best-y (second best-interval)) new-student)
      
      ;; Find matches before and after this match.
      (+ (if (remove best-m model-free)
	     (match-model-and matches (remove best-m model-free) new-student)
	     ;; count remaining student words
	     (apply #'+ (mapcar #'(lambda (x) (- (second x) (first x))) 
				new-student)))
	 (aref matches best-m best-y best-z)))))

(defun pull-out-quantity (symbol text)
  "Pull the quantity phrase out of a definition:  should match variablname.js"
  (when symbol
    (if (not (search symbol text))
	(warn "Bad symbol definition, ~S should be found in ~S."
	      symbol text)
	;; this should be done as a parser.
	(let* ((si (+ (search symbol text) (length symbol)))
	       (nosym (string-left-trim *whitespace* (subseq text si))))
	  ;; The empty string is a catch-all in case there is no match
	  (dolist (equality '("is " ":" "=" "be " "as " "to be " ""))
	    (when (and (>= (length nosym) (length equality))
		       (string= equality (string-downcase nosym) 
				:end2 (length equality)))
	      (return-from pull-out-quantity
		(string-trim *whitespace* 
			     (subseq nosym (length equality)))))))))
  text)

(defun best-matches (text good)
  "Returns array of best matches to text.  Use minimum edit distance."
  (let (this (best 1000000) quants)
    (dolist (x good)
      ;; Normalize by maximum possible distance.
      (setf this (normalized-levenshtein-distance text (car x)))
      (cond ((< this best) 
	     (setf best this)
	     (setf quants (list (cdr x))))
	    ((= this best)
	     (push (cdr x) quants))))
    quants))

(defun normalized-levenshtein-distance (s1 s2)
  "Normalize levenshtein-distance so complete rewrite is 1."
  (/ (levenshtein-distance s1 s2) 
		    (max (length s1) (length s2))))

;;;; Levenshtein Distance function.  This implementation was converted from 
;;;; the Scheme implementation given at 
;;;; http://en.wikipedia.org/wiki/Levenshtein_distance
;;;; See http://www.cliki.net/Levenshtein
;;;; 


(defun levenshtein-distance (s1 s2)
  (let* ((width (1+ (length s1)))
	  (height (1+ (length s2)))
	  (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
	(setf (aref d (1+ y) (1+ x))
	      (min (1+ (aref d y (1+ x)))
		   (1+ (aref d (1+ y) x))
		   (+ (aref d y x)
		      (if (char= (aref s1 x) (aref s2 y))
			  0
			  1))))))
    (aref d (1- height) (1- width))))


