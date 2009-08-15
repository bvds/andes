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
;; Which would allow chunking based on punctuation.  But reasonable matching
;; is much more difficult in that case.
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

(defun word-count (model &key max)
  "find minimum (or maximum) word count in model"
  (cond 
    ((null model) 0)
    ((stringp model) 1) ;; or count words in string
    ((or (listp (car model)) (stringp (car model)))
     (+ (word-count (car model) :max max) 
	(word-count (cdr model) :max max)))
    ((eql (car model) 'and)
     (if (cdr model)
	 (+ (word-count (cadr model) :max max)
	    ;; putting the 'and back in not really necessary 
	    (word-count (cddr model) :max max))
	 0))
    ((eql (car model) 'or)
     (if (cdr model)
	 (funcall (if max #'max #'min) 
		  (word-count (cadr model) :max max) 
		  (word-count (cons 'or (cddr model)) :max max))
	 0))
    ((member (car model) '(allowed preferred)) 
     (if max (word-count (cdr model) :max max) 0))
    (t (warn "word-count found unexpected form ~A" model) (if max 10000 0))))

(defmacro update-bound (best x)
  `(let ((this ,x))
     (when (< this ,best) (setf ,best this))))

;; The method used here does a depth-first match from student sentence
;; to the model tree.  For lists, the search space goes as a factorial 
;; in the number of words.

(defun match-model (student model &key (best 20000))
  "Recursive match to tree, returns minimum word insertion/addition for match."

  ;; When bound is given, see if there is any hope, based on word count,
  ;; of doing better.
  (when (< best 10000)
      (let ((this (max (- (length student) (word-count model :max t))
		       (- (word-count model) (length student))
		       0)))
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
      ;; there are (m+n)!/(n! m!) different possible matches.
      (let (chunk (rest (copy-list student)))
	;; This loop takes the student list and divides it into
	;; "chunk" and "rest."  Then "chunk" is matched with the 
	;; first term of the model and rest is matched with the 
	;; rest of the model list.
	;;
	;; One possible improvent would be to estimate what order
	;; the loop should be executed basd on word count.
	;; Alternatively, one could calculate all the first terms,
	;; then calculate the second term an order based on
	;; the best matches to the first term.
	(loop
	   ;; Use the result of the first term to set a more precise
	   ;; bound for the second term.
	   ;; The first term should have a better bound estimate.
	   (let ((first-term (match-model chunk (car model) :best best)))
	     (update-bound best 
			   (+ first-term
			      (match-model rest (cdr model)
					   :best (- best first-term)))))
	   (when (null rest) (return))
	   ;; take first member of rest and put it at end of chunk
	   (setf chunk (append chunk (list (pop rest)))))
	best)
      (match-model student (car model) :best best)))
    ((eql (car model) 'and)
     ;; for m arguments of the model "and", there are m! possible
     ;; list matches (see above).
     (cond 
       ((cddr model) ;two or more arguments of "and"
	(dolist (item (cdr model))
	  (update-bound best 
			(match-model 
			 student 
			 (list item (remove item model))  ;non-destrutive
			 :best best)))
	best)
       ((cdr model) (match-model student (second model) :best best))
       (t (word-count student)))) ;empty "and"
    ((eql (car model) 'or)
     (cond 
       ((cddr model) ;two or more arguments of "or"
	(dolist (item (cdr model))
	  (update-bound best (match-model student item :best best)))
	best)
       ((cdr model) (match-model student (second model) :best best))
       (t (word-count student)))) ;empty "or"
    (t (error "Bad trees ~A ~A" student model))))


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


