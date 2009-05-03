;; parse.cl --
;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
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
;; parse.cl --
;; Author(s):
;;  unknown -- originators of code from Andes team
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  12 March 2001 - (lht) -- this file created for Andes 2
;;  17 April 2001 - (lht) -- added packaging for integrating with Andes2
;;  15 May 2001 - (lht) -- mdofied to support new parsing grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree is an alist with a Left-Hand Side (lhs) and a Right-Hand Side (rhs)
(defun new-tree (cat rhs) (cons cat rhs))
(defun tree-lhs (tree) (first tree))
(defun tree-rhs (tree) (rest tree))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse is a statement of a parse condition:
;;  a tree with the current state of the parse and
;;  a remainder; the string of characters that reamin to be parsed
;; NOTE:  if parse quits and remainder is not empty, the beginning of rem 
;; indicates the point of failure.
(defstruct (parse) "A parse tree and a remainder." tree rem)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; support routine returns the lhs of the tree portion of the parse struct 
;;; passed in parse
(defun parse-lhs (parse) (tree-lhs (parse-tree parse)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a valid parse is a parse that is complete and has as it's root the lhs specified
(defun parse-get-valid (root parses)
  (remove-if-not #'(lambda (x) (equal root (car x))) parses :key #'parse-tree))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; complete-parses finds all parses that contain no remainder
;; argument(s):
;;  parses -- is a list of parse structures
;; NOTE: this returns a list of parse structures or nil if not complete parses exist
(defun parse-get-complete (parses)
  (remove-if-not #'(lambda (x) (= 0 (length x))) parses :key #'parse-rem))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defvar *parse-memo* "memo for holding session-local parse")
(defvar *grammar-get-rhs-memo* "memo for holding session-local grammar")
(defvar *grammar-get-rhs-with-first* "memo for holding session-local grammar")

(defun parse-initialize ()
  ;; makes parser act like chart parser (memoize does this)
  (memoize 'parse :key #'second :test #'equal :var *parse-memo*)
  (memoize 'grammar-get-rhs :key #'second :test #'equal 
	   :var *grammar-get-rhs-memo*)
  (memoize 'grammar-get-rhs-with-first :key #'second :test #'equal
	   :var *grammar-get-rhs-with-first*))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-equation (grammar words)
  (clear-memoize 'parse)
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first)
  (parse grammar words))

;; memoize does not work well with self-recursive functions if 
;; inlining is allowed.  See http://www.tfeb.org/programs/memoize.lisp
(declaim (notinline parse grammar-get-rhs grammar-get-rhs-with-first))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the essential parse routine is to (beginning with first letter of string) 
;; find all terminal rules that match the character and then get the next 
;; character and find all of the previously found rules that have a match with 
;; this character
(defun parse (grammar input)
  (when (> (length input) 0)
    (mapcan
     #'(lambda (rule)
	 (parse-support grammar (rule-lhs rule) (list (char input 0))
			(subseq input 1) nil))
     (grammar-get-rhs grammar (char input 0)))))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pass the left hand side of a rule, the current character (rhs), the rest of 
;; the string to be parsed and what is needed to complete the current rule
(defun parse-support (grammar lhs rhs rem needed)
  (if (null needed)
      (let ((parse (make-parse :tree (new-tree lhs rhs) :rem rem)))
	(cons parse
	      (mapcan
	       #'(lambda (rule)
		   (parse-support grammar (rule-lhs rule)
				  (list (parse-tree parse))
				  rem (rest (rule-rhs rule))))
	       (grammar-get-rhs-with-first grammar lhs))))
      (mapcan
       #'(lambda (p)
	 (when (eq (parse-lhs p) (first needed))
	   (parse-support grammar lhs (append-atom rhs (parse-tree p)) 
			  (parse-rem p) (rest needed))))
       (parse grammar rem))))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-remove-lhs (lhs parse)
  (cond
   ((null parse) parse)
   ((null (consp parse)) parse)
   ((list-begins-with-p lhs (first parse)) (parse-remove-lhs lhs (rest parse)))
   (t (cons (parse-remove-lhs lhs (first parse)) (parse-remove-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack (parse)
  (list (first parse)
	(remove #\Space (map 'string #'(lambda (x)
					 (if (characterp x)
					     x
					   #\Space))
			     (flatten (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack-cs (parse)
  (list (first parse)
	(concatenate 'string "|"
		     (remove #\Space (map 'string #'(lambda (x)
						      (if (characterp x)
							  x
							#\Space))
					  (flatten (rest parse)))) "|")))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack-lhs (lhs parse)
  (cond
   ((null parse) parse)
   ((null (consp parse)) parse)
   ((list-begins-with-p lhs (first parse))
    (cons (parse-pack (first parse)) (parse-pack-lhs lhs (rest parse))))
   (t (cons (parse-pack-lhs lhs (first parse)) (parse-pack-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack-translate (parse)
  (list (first parse)
	(map-student-atom 
	 (read-from-string
	  (remove #\Space (map 'string #'(lambda (x)
					   (if (characterp x)
					       x
					     #\Space))
			       (flatten (rest parse))))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack-translate-variables-lhs (lhs parse)
  (cond
   ((null parse) parse)
   ((null (consp parse)) parse)
   ((list-begins-with-p lhs (first parse))
    (cons (parse-pack-translate (first parse)) (parse-pack-lhs lhs (rest parse))))
   (t (cons (parse-pack-lhs lhs (first parse)) (parse-pack-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack-to-string-lhs (lhs parse)
  (cond
   ((null parse) parse)
   ((null (consp parse)) parse)
   ((list-begins-with-p lhs (first parse))
    (cons (list (first (first parse)) (format nil "~W" (second (first parse))))
	  ;;(symbol-name (second (first parse))))
	  (parse-pack-to-string-lhs lhs (rest parse))))
   (t (cons (parse-pack-to-string-lhs lhs (first parse))
	    (parse-pack-to-string-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-pack-cs-lhs (lhs parse)
  (cond
   ((null parse) parse)
   ((null (consp parse)) parse)
   ((list-begins-with-p lhs (first parse))
    (cons (parse-pack-cs (first parse)) (parse-pack-cs-lhs lhs (rest parse))))
   (t (cons (parse-pack-cs-lhs lhs (first parse)) (parse-pack-cs-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-gather-rhs-of-lhs (lhs parse)
  (cond
   ((null parse) nil)
   ((null (consp parse)) nil)
   ((list-begins-with-p lhs (first parse))
    (append (rest (first parse)) (parse-gather-rhs-of-lhs lhs (rest parse))))
   (t (append (parse-gather-rhs-of-lhs lhs (first parse))
	      (parse-gather-rhs-of-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-gather-not-rhs-of-lhs (lhs parse)
  (cond
   ((null parse) nil)
   ((null (consp parse)) nil)
   ((list-begins-with-p lhs (first parse))
    (append (parse-gather-rhs-of-lhs lhs (first parse))
	      (parse-gather-rhs-of-lhs lhs (rest parse))))
   (t
    (append (rest (first parse)) (parse-gather-rhs-of-lhs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-surround-lhs (ls rs lhs parse)
  (cond
   ((null parse) nil)
   ((null (consp parse)) parse)
   ((list-begins-with-p lhs parse)
    (append (list (first parse) ls)
	    (list (parse-surround-lhs ls rs lhs (rest parse))) (list rs)))
   (t
    (append (list (parse-surround-lhs ls rs lhs (first parse)))
	      (parse-surround-lhs ls rs lhs (rest parse))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-collapse (parse)
  (let ((tmp ""))
    (dolist (x (flatten parse))
      (if (stringp x)
	  (setf tmp (concatenate 'string tmp " " x))))
    (string-trim " " tmp)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of file parse.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

