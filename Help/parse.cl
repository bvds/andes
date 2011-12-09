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
(defvar *parse-memo*) ;memo for holding session-local parse
(defvar *lexical-rules-memo*) ;memo for holding session-local grammar
(defvar *rules-starting-with-memo*) ;memo for holding session-local grammar

(defun parse-initialize ()
  ;; makes parser act like chart parser (memoize does this)
  (memoize 'parse :key #'second :test #'eq :var '*parse-memo*)
  (memoize 'lexical-rules :key #'second :test #'equal 
	   :var '*lexical-rules-memo*)
  (memoize 'rules-starting-with :key #'second :test #'equal
	   :var '*rules-starting-with-memo*))

(defun parse-clear ()
  (clear-memoize 'parse)
  (clear-memoize 'lexical-rules)
  (clear-memoize 'rules-starting-with))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun parse-equation (grammar words)
  (parse-clear)
  (parse grammar words))

;; memoize does not work well with self-recursive functions if 
;; inlining is allowed.  See http://www.tfeb.org/programs/memoize.lisp
(declaim (notinline parse lexical-rules rules-starting-with))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; The following code is adapted from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; File syntax1.lisp: The PSG-based natural language parser.
;;;; This is the more efficient version of the non-semantic parser,
;;;; which uses the memoized functions in Section 19.3 and handles
;;;; unknown words as described in Section 19.4.
;;;; http://norvig.com/paip/syntax1.lisp
;;;;
;;;; Norvig has a license that allows distribution of his code,
;;;; and modifications thereof, under other open source licenses.
;;;; See  http://norvig.com/license.html

(defun lexical-rules (grammar word)
  "Return a list of rules with word on the right hand side."
  (find-all word grammar :key #'rule-rhs :test #'equal))

(defun rules-starting-with (grammar cat)
  "Return a list of rules where cat starts the rhs."
  (find-all cat grammar
            :key #'(lambda (rule) 
		     ;; Norvig used first-or-nil here
		     (when (consp (rule-rhs rule)) (car (rule-rhs rule))))))

;; the essential parse routine is to (beginning with first letter of string) 
;; find all terminal rules that match the character and then get the next 
;; character and find all of the previously found rules that have a match with 
;; this character
(defun parse (grammar characters)
 "Bottom-up parse, returning all parses of any prefix of words."
  (when (> (length characters) 0)
    (mapcan
     #'(lambda (rule)
	 (extend-parse grammar (rule-lhs rule) (list (char characters 0))
			(subseq characters 1) nil))
     (lexical-rules grammar (char characters 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pass the left hand side of a rule, the current character (rhs), the rest of 
;; the string to be parsed and what is needed to complete the current rule
(defun extend-parse (grammar lhs rhs rem needed)
  "Look for the categories needed to complete the parse."
  (if (null needed)
      ;; If nothing needed, return parse and upward extensions
      (let ((parse (make-parse :tree (new-tree lhs rhs) :rem rem)))
	(cons parse
	      (mapcan
	       #'(lambda (rule)
		   (extend-parse grammar (rule-lhs rule)
				 (list (parse-tree parse))
				 rem (rest (rule-rhs rule))))
	       (rules-starting-with grammar lhs))))
	;; otherwise try to extend rightward
	(mapcan
	 #'(lambda (p)
	     (when  (eq (parse-lhs p) (first needed))
	       (extend-parse grammar lhs (append1 rhs (parse-tree p)) 
			     (parse-rem p) (rest needed))))
	 (parse grammar rem))))

(defun append1 (items item)
  "Add item to end of list of items."
  (append items (list item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	(coerce (remove-if-not #'characterp (flatten (cdr parse))) 
		'string)))
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
;; end of file parse.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

