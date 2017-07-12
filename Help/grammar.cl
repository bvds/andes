;; grammar.cl -- routines for building grammars a little quicker than one rule at a time
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (CL) <CollinL@pitt.edu>
;; Modified:
;;   3 April 2001 - (lht) -- created from previous work on ANDES2 parsing
;;; Modifications by Anders Weinstein 2001-2008
;;; Modifications by Brett van de Sande, 2005-2010
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
;;   8 April 2001 - (lht) -- version finalized and source cleaned
;;  10 April 2001 - (lht) -- added cull routines
;;  10 May 2001 - (lht) -- reworked for better parse handling and to return more useable info.
;;  12 June 2003 - (cl) -- Taking care of compiler warnings with declarations.
;; Note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; declare these as global special variables
(defvar **identifier-grammar**) ;global 
(defvar **common-grammar**)     ;global
(defvar **grammar**)            ;session-local

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a rule has a:
;;  lhs - left hand side -- the 'key or name of this rule
;;  rhs - right hand side -- if this is an atom then rule is terminal otherwise is nonterminal
;;  rtn - function called when rule 'fires expects the rule that fired followed by anything else
(defstruct (rule (:type list)) lhs rhs rtn-fn)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-set MACRO -- gives a grammar a value
;; argument(s):
;;  grammar -- symbol used to reference grammar
;;  value -- value you want assigned to grammar
;; returns:
;;  grammar has value
;; note(s):
;;  grammar may not be previously created but will be after call to this macro
;; example(s):
;;  (grammar-set '**grammar** nil) ==> **grammar** is created (if not already) and set to nil
(defmacro grammar-set (grammar value)
  `(setf (symbol-value ,grammar) ,value))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following are the functions for adding rules to grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-add-grammar - adds rules from one grammar to another
;; argument(s):
;;  grammar - symbol that refers to the grammar that will have new rules appended
;;  other-grammar - the grammar whose rules are to be copied
;; returns:
;;  with grammar containing any (and all) new rules that other-grammar contained
;; 
(defun grammar-add-grammar (grammar other-grammar)
  (dolist (rule other-grammar)
    (grammar-set grammar (append (symbol-value grammar) (list rule)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-add-terminal -- adds terminal rules to grammar -- a terminal rule is a rule whose rhs
;;  is a single atom ... specificaly anything that returns nil to the predicate consp
;; argument(s):
;;  grammar - symbol that refers to the grammar that will get the new rule
;;  lhs - the 'key or name of the rule(s) to be added
;;  rhs - a single atom or a list of atoms that are language terminals (not (consp))
;;  rtn-fn - <optional:rule-rhs> function defining the return value of the rule(s)
;; returns:
;;  grammar with new rules added
;; example(s):
;;  (grammar-add-terminal 'plus #\+) adds rule (plus + rule-rhs)
;;  (grammar-add-terminal 'times '(#\* #\.)) adds the rules:
;;    (times * rule-rhs) and (times . rule-rhs)
;; note(s):
(defun grammar-add-terminal (grammar lhs rhs &optional (rtn-fn #'rule-rhs))
  (cond
   ((not (consp rhs))
    (let ((rule (make-rule :lhs lhs :rhs rhs :rtn-fn rtn-fn)))
      (if (not (member rule (symbol-value grammar) :test #'equal))
	  (grammar-set grammar (append (symbol-value grammar) (list rule))))))
   (t (dolist (obj rhs)
	(let ((rule (make-rule :lhs lhs :rhs obj :rtn-fn rtn-fn)))
	  (if (not (member rule (symbol-value grammar) :test #'equal))
	      (grammar-set grammar (append (symbol-value grammar) (list rule)))
	    ))))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-add-nonterminal -- adds nonterminal rules to **grammar**
;; argument(s):
;;  grammar - symbol that refers to the grammar that will get the new rule
;;  lhs - the 'key or name of the rule(s) to be added
;;  rhs - a list of lists of symbols that are lhs of previously added rules
;;  rtn-fn - <optional:nil> function defining the return value of the rule(s)
;; returns:
;;  grammar with new rules added
;; example(s):
;;  (grammar-add-nonterminal 'name '((sam) (sam thomas)) adds the rules:
;;    (name (sam) nil) and (name (sam thomas) nil)
(defun grammar-add-nonterminal (grammar lhs rhs &optional (rtn-fn nil))
  (cond
    ((not (consp rhs)))
    (t (dolist (obj (mapcan #'expand-optionals rhs))
	 (if (consp obj) ;non-terminal
	     (if (grammar-rhs-valid (symbol-value grammar) lhs obj)
		 (let ((rule (make-rule :lhs lhs :rhs obj :rtn-fn rtn-fn)))
		   (if (not (member rule (symbol-value grammar) :test #'equal))
		       (grammar-set grammar (append (symbol-value grammar) (list rule)))
		       (warn "rule ~A already in grammar" rule)))
		 (warn "invalid rule ~A for grammar" obj))
	     (warn "rule must be a list, not adding ~A to grammar" obj))))))

(defun expand-optionals (rule)
  "Optionals in a rule are expressed as sublists.  This takes a single rule 
   and expands any optional elements, returning a list of one or more rules."
  (if (null rule) 
      (list nil)
      (let ((y (expand-optionals (cdr rule))))
	(if (consp (first rule))
	    (append y
		    (mapcar #'(lambda (x) (append (car rule) x)) y))
	    (mapcar #'(lambda (x) (cons (car rule) x)) y)))))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-add-special -- adds new rules to grammar
;; argument(s):
;;  grammar - symbol that refers to the grammar that will get the new rule
;;  lhs - name or key of this rule
;;  rhs - string representing the identifier or a list of such strings
;;  rtn-fn - <optional:rule-rhs> function defining the return value of the rule(s)
;; returns:
;;  grammar with new special rule added
;; example(s):
;;  (grammar-add-special 'unit "Meters") adds the rule: (unit (um le lt le lr ls) nil)
;; note(s):
;;  if element is not a string then no rule is added
(defun grammar-add-special (grammar lhs rhs rtn-fn identifier-grammar)
  (clear-memoize 'lexical-rules)
  (clear-memoize 'rules-starting-with)
  (if (consp rhs)
      (dolist (obj rhs)
	(if (stringp obj)
	    (grammar-add-nonterminal grammar lhs
				     (list (grammar-string-to-rhs identifier-grammar obj))
				     rtn-fn)))
    (if (stringp rhs)
	(grammar-add-nonterminal grammar lhs
				 (list (grammar-string-to-rhs identifier-grammar rhs))
				 rtn-fn)))
  (clear-memoize 'lexical-rules)
  (clear-memoize 'rules-starting-with))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-remove-special -- removes special rules
;; argument(s):
;;  grammar - symbol that refers to the grammar that will be altered
;;  var - string representing the identifier or a list of such strings
;; returns:
;;  grammar with special rule removed
(defun grammar-remove-special (grammar lhs rhs rtn-fn support-grammar)
  (declare (ignore rtn-fn))
  (clear-memoize 'lexical-rules)
  (clear-memoize 'rules-starting-with)
  (let ((temp-grammar '())
	(rmv (grammar-string-to-rhs support-grammar rhs)))
    (dolist (obj (symbol-value grammar))
      (if (not (and (equal lhs (rule-lhs obj)) (equal rmv (rule-rhs obj))))
	  (setf temp-grammar (append temp-grammar (list obj)))))
    (grammar-set grammar temp-grammar))
  (clear-memoize 'lexical-rules)
  (clear-memoize 'rules-starting-with))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-remove-identifier -- adds new identifier rules to grammar
;; argument(s):
;;  grammar - symbol that refers to the grammar that will be altered
;;  var - string representing the identifier or a list of such strings
;; returns:
;;  grammar with identifier rule removed
(defun grammar-remove-identifier (grammar var lhs)
  (grammar-remove-special grammar lhs var nil **identifier-grammar**))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the functions above all need (as their first argument) the symbol referring to the grammar
;;  being used (i.e. (quote grammar))
;;
;; the functions below all need (as their first argument) the grammar being used:
;;  (i.e. NOT (quote grammar) but grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-string-to-rhs -- converts a string to a form sutable for rhs of rule
;; argument(s):
;;  grammar -- grammar to be used for conversion
;;  var - <string:""> the string to be converted
;; returns:
;;  a list that could be used as the rhs of a rule -- uses optional grammar to define rules
;;   to convert to
;; example(s):
;;  (string-to-rhs *identifier-characters* "sam") ==> (ls la lm)
;; note(s):
;;  grammar must have a rule for each possible character that var may contain
(defun grammar-string-to-rhs (grammar var)
  (map 'list
    #'(lambda (x) 
	(rule-lhs (first (lexical-rules grammar x))))
    var))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-rhs-valid - are the contents of a rhs valid in grammar
;; argument(s):
;;  grammar - the grammar that will be used for validation
;;  rhs - the rhs to validate
;; returns:
;;  t if rhs is valid; nil otherwise
(defun grammar-rhs-valid (grammar this x)
  "Is proposed rule grounded in given grammar?"
  (cond
   ((null x) t)
   ;; optional terms expressed as sublist
   ((consp (first x))
    (and (grammar-rhs-valid grammar this (first x))
	 (grammar-rhs-valid grammar this (rest x))))
   ((or (equal this (first x)) ;token matches name of proposed rule
	(member (first x) grammar :key #'rule-lhs :test #'equal))
    (grammar-rhs-valid grammar this (rest x)))
   (t nil)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-dump - prints grammar using prl
;; argument(s):
;;  grammar - the grammar to dump
;; returns:
;;  nil
;; note(s):
;;  prints the list with prl
(defun grammar-dump (grammar)
  (prl grammar))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of grammar.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
