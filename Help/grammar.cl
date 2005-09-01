;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar.cl -- routines for building grammars a little quicker than one rule at a time
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;;  Collin Lynch (CL) <CollinL@pitt.edu>
;; Modified:
;;   3 April 2001 - (lht) -- created from previous work on ANDES2 parsing
;;   8 April 2001 - (lht) -- version finalized and source cleaned
;;  10 April 2001 - (lht) -- added cull routines
;;  10 May 2001 - (lht) -- reworked for better parse handling and to return more useable info.
;;  12 June 2003 - (cl) -- Taking care of compiler warnings with declarations.
;; Note(s):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the following are the functions for adding rules to grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-initialize -- initialize a grammar to a specific set of rules
;; argument(s):
;;  grammar - symbol that refers to the grammar that will be initialized
;;  initial-rules: <optional:nil> a list of rules to initialize grammar with
;; returns:
;;  with grammar containing the rules specified or nil
;; note(s):
;;  will initialize to nil if initial-rules is an atom
(defun grammar-initialize (grammar &optional other-grammar)
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first)
  (cond
   ((or (null other-grammar) (not (consp other-grammar)))
    (grammar-set grammar nil))
   (t (grammar-add-grammar grammar other-grammar))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   (t (dolist (obj rhs)
	(if (consp obj)
	    (if (grammar-rhs-valid (symbol-value grammar) obj)
		(let ((rule (make-rule :lhs lhs :rhs obj :rtn-fn rtn-fn)))
		  (if (not (member rule (symbol-value grammar) :test #'equal))
		      (grammar-set grammar (append (symbol-value grammar) (list rule)))
		    ))
	      )
	  )))))
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
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first)
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
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-remove-special -- removes special rules
;; argument(s):
;;  grammar - symbol that refers to the grammar that will be altered
;;  var - string representing the identifier or a list of such strings
;; returns:
;;  grammar with special rule removed
(defun grammar-remove-special (grammar lhs rhs rtn-fn support-grammar)
  (declare (ignore rtn-fn))
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first)
  (let ((temp-grammar '())
	(rmv (grammar-string-to-rhs support-grammar rhs)))
    (dolist (obj (symbol-value grammar))
      (if (not (and (equal lhs (rule-lhs obj)) (equal rmv (rule-rhs obj))))
	  (setf temp-grammar (append temp-grammar (list obj)))))
    (grammar-set grammar temp-grammar))
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-add-identifier -- adds new identifier rules to grammar
;; argument(s):
;;  grammar - symbol that refers to the grammar that will get the new rule
;;  var - string representing the identifier or a list of such strings
;; returns:
;;  grammar with new identifier rule added
;; example(s):
;;  (grammar-add-identifier "sam") adds the rule: (identifier (ls la lm) nil)
(defun grammar-add-identifier (grammar var lhs)
  (declare (special **Identifier-Grammar**))
  (grammar-add-special grammar lhs var nil **identifier-grammar**))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-remove-identifier -- adds new identifier rules to grammar
;; argument(s):
;;  grammar - symbol that refers to the grammar that will be altered
;;  var - string representing the identifier or a list of such strings
;; returns:
;;  grammar with identifier rule removed
(defun grammar-remove-identifier (grammar var lhs)
  (declare (special **Identifier-Grammar**))
  (grammar-remove-special grammar lhs var nil **identifier-grammar**))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-remove-identifiers -- removes all identifiers from grammar
;; argument(s):
;;  grammar - symbol that refers to the grammar that will get the new rule
;; returns:
;;  **grammar** with identifier rules removed
;; note(s):
(defun grammar-remove-identifiers (grammar lhs)
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first)
  (let ((temp-grammar '()))
    (dolist (obj (symbol-value grammar))
      (if (not (equal (rule-lhs obj) lhs))
	  (setf temp-grammar (append temp-grammar (list obj)))))
    (grammar-set grammar temp-grammar))
  (clear-memoize 'grammar-get-rhs)
  (clear-memoize 'grammar-get-rhs-with-first)
  (clear-memoize 'parse))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the functions above all need (as their first argument) the symbol referring to the grammar
;;  being used (i.e. (quote grammar))
;;
;; the functions below all need (as their first argument) the grammar being used:
;;  (i.e. NOT (quote grammar) but grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	(rule-lhs (first (grammar-get-rhs grammar x))))
    var))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-get-rhs - return a list of all rules that match this rhs
;; argument(s):
;;  grammar - the grammar that will be searched
;;  rhs - an rhs to be matched against
;; returns:
;;  a possible nil list of rules that have rhs as their rhs
(defun grammar-get-rhs (grammar rhs)
  (find-all rhs grammar :key #'rule-rhs :test #'equal))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-get-rhs-with-first - return a list of all rules that have (as the first item of their
;;  rhs a match to the specified rhs
;; argument(s):
;;  grammar - the grammar to search for matches in
;;  rhs - an rhs to be matched against
;; returns:
;;  a possible nil list of rules that have rhs as the first element of their rhs
(defun grammar-get-rhs-with-first (grammar rhs)
  (find-all rhs grammar :key #'(lambda (rule) (if-list-first-nil (rule-rhs rule)))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-match-lhs - returns a list off all rules whose lhs matches argument
;; argument(s):
;;  grammar - the grammar to search for matches in
;;  lhs - the lhs to search for
;; returns:
;;   a possibly nil list of rules whose lhs is the same as lhs
(defun grammar-match-lhs (grammar lhs)
  (find-all lhs grammar :key #'rule-lhs :test #'equal))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grammar-rhs-valid - are the contents of a rhs valid in grammar
;; argument(s):
;;  grammar - the grammar that will be used for vlidation
;;  rhs - the rhs to validate
;; returns:
;;  t if rhs is valid; nil otherwise
(defun grammar-rhs-valid (grammar x)
  (cond
   ((null x) t)
   ((grammar-match-lhs grammar (first x)) (grammar-rhs-valid grammar (rest x)))
   (t nil)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of grammar.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
