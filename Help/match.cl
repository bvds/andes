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

(defun pull-out-quantity (symbol text)
  "Pull the quantity phrase out of a definition:  should match variablname.js"
  (if symbol
      ;; this should be done as a parser.
      (let* ((si (+ (search symbol text) (length symbol)))
	     (nosym (string-left-trim *whitespace* (subseq text si))))
	;; The empty string is a catch-all in case there is no match
	(dolist (equality '("is " ":" "=" "be " "as " "to be " ""))
	  (when (string= equality (string-downcase nosym) 
			 :end2 (length equality))
	    (format t "found ~s ~s ~s~%" nosym equality
		    (subseq nosym (length equality)))
	    (return-from pull-out-quantity
	      (string-trim *whitespace* (subseq nosym (length equality)))))))
      text))

(defun best-matches (text good)
  "Returns array of best matches to text.  Use minimum edit distance."
  (let (this (best 1000000) quants)
    (dolist (x good)
      ;; Normalize by maximum possible distance.
      (setf this (/ (levenshtein-distance text (car x)) 
		    (max (length text) (length (car x)))))
      (cond ((< this best) 
	     (setf best this)
	     (setf quants (list (cdr x))))
	    ((= this best)
	     (push (cdr x) quants))))
    quants))

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
