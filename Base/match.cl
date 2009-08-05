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
;;               (and <model> ...) orderless sequence 
;;               (or <model> ...)  this is exclusive or
;;               (var <quant>)   match student variable for <quant>
;;               (ont <quant>)   match with ontology
;;               (preferred <model>) optional, but hinted for
;;               (allowed <model>)  optional, but not hinted for
;;               <string>        leaf nodes, after resolution, are strings
;;        Not sure if we need the following
;;        and unsure of proper name, maybe merge with (and ...) above
;;               (nl-and <model> ...) orderless list, expressed with "and"
;;               (nl-or <model> ...) orderless list, expressed with "or"
;;
;;  Parse of student sentence is <student>: (<student> ...)
;;                                           <string>
;;               This allows chunking based on punctuation.
;;

;; operations:  Resolve <quant> references, break up strings.
;;              expand to expression (or <string> ...) for testing
;;                  with flag for preferred/allowed. 
;;              Compare <student> with <model> giving a distance metric.
;;              Make <model> subtrees (preferred ...) based on Gricean 
;;                  conversational maxims (minimum needed to distinguish
;;                  quantity).
;;

(defun word-count (model & key max)
  "find minimum word count in model"
  (cond 
    ((stringp model) 1) ;; or count words in string
    ((or (listp (car model)) (stringp (car model)))
     (+ (word-count (car model) :max max) 
	(word-count (cdr model) :max max)))
    ((eql (car model) 'and)
     (pop model)
     (if model
	 (+ (word-count (car model) :max max)
	    ;; putting the 'and back in not really necessary 
	    (word-count (cdr model) :max max))
	 0))
    ((eql (car model) 'or)
     (pop model)
     (if model
	 (funcall (if max #'max #'min) 
		  (word-count (car model) :max max) 
		  (word-count (cons 'or (cdr model)) :max max))
	 0))
    ((member (car model) '(allowed preferred)) 
     (if max (word-count (cdr model) :max max) 0))
    ((member (car model) '(var ont)) 1)
    (t (warn "word-count found unexpected form ~A" model) 10000)))


(defun match-model (student model &key (deletes 0) (best 1000))
  "Recursive depth-first match to tree, with cut-off for score, returns minimum word insertion/addition for match."
  ;; If the situation is hopeless, don't do anything.
  (let ((lower-bound 
	 (+ (max (- (word-count model) (word-count student :max t))
		 (- (word-count student) (word-count model :max t))
		 0) 
	    deletes)))
    (when (> lower-bound best)
      (return-from match-model lower-bound)))
  
  (cond 
    ((null student) (+ (word-count model) deletes))
    ((null model) (+ (word-count student) deletes))
    ((and (stringp student) (stringp model))
     (+ (normalized-levenshtein-distance student model)
	deletes))
    ((and (listp student) (stringp model))
     (min
      (+ deletes (match-model (cdr student) model)
	 (word-count (car student)))
      (+ deletes (match-model (car student) model)
	 (word-count (cdr student)))))
    ((or (listp (car model)) (stringp (car model)))
     (let ((best (match-model (cdr student) model 
			      :deletes (+ 1 deletes)))
	   this chunk
	   (rest (copy-list student)))
       (loop	     
	  (setf this (+ (match-model chunk (car model)
				     :deletes deletes)
			(match-model rest (cdr model)
				     :deletes deletes)))
	  (when (< this best) (setf best this))
	  (when (null rest) (error "need to exit loop"))
	  (setf chunk (append chunk (list (pop rest)))))
       best))
    ((eql (car model) 'and) 
     (if (cdr model) 
	 (let ((best 1000000) this)
	   (dolist (item (cdr model))
	     (setf this (match-model 
			 student 
			 (list item (drop item model)) ;non-destrutive
			 :deletes deletes))
	     (when (< this best) (setf best this)))
	   best)
	 (match-model student nil :deletes deletes)))
    ((eql (car model) 'or) 
     (if (cdr model) 
	 (let ((best 10000) this)
	   (dolist (item (cdr model))
	     (setf this (match-model 
			 student item :deletes deletes))
	     (when (< this best) (setf best this)))
	   best)
	 (match-model student nil :deletes deletes)))
    
     
     
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


