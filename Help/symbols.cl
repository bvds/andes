;; symbols.lsp/cl
;; author(s):
;;   Collin Lynch (cl) -- <CollinL@pitt.edu>
;;   Linwood H. Taylor (lht) -- <lht@lzri.com>
;;   19 February 2001 - (cl) -- created
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modified:
;;   18 March 2001 - (lht) -- took over for addition to Andes2 code
;;   7 July 2003 - (cl) -- Added optional problem argument to quant-to-sysvar
;;                         sysvar-to-quant sysvar-p and quant-to-valid-sysvar
;;
;; note(s):
;; This code maintains the symbol table for the Andes2 help system.  This
;; table is tasked with enabling the mapping betweeen student variables and
;; canonical variables required for checking equations.  It also keeps a
;; record associating student labels to the quantity or other expressions 
;; they denote. By matching the quantities to the system's variables for the 
;; same quantity, we can map between student variables and system variables.
;;
;; This module maintains the master table of student symbol info. As a side
;; effect, all changes to this table are also propagated to the variable list
;; used by the equation parser.

;; Some student labels will not have corresponding system variables. There
;; are two reasons this can happen:
;; 1. Not all student-defined labels denote quantities. Variables denote
;; quantative values and can be used in equations. Student labels for bodies 
;; and axes refer to entities that are not "quantities" for which the 
;; system has variables.  We save them here anyway for various purposes.
;; 2. Not every quantity the student defines will be mentioned in the Andes
;; solution. These will be marked erroneous entries.
;;
;; Some student variables may be mapped into expressions the system can 
;; understand in equations but not to system variables. For example, student
;; labels for known angles may be mapped into an expression for a constant
;; number of degrees.

;; Student labels are generally passed to us as quoted strings so we can
;; preserve their case. We store them as strings for this reason. However, 
;; variables in equations are generally represented as symbols, which may have
;; been converted to upper case if passed through Lisp "read" without vbars.
;; We need case-sensitive variables to distinguish G from g, for example, so
;; all our lookups are case-sensitive.

;; Labels containing Greek letters come to us coded in LaTeX form.
;;
;; Allow separate namespaces for scalar variables and geometric objects,
;; such as vectors.  This allows for the common practice of using the
;; same symbol for an objects and one of its properties.  Thus, "q" might
;; refer to a particle and its charge.
;;


(in-package :cl-user)

(defpackage :symbols
  ;; import from common-lisp-user:   
  ;;     unify quant-to-valid-sysvar sysvar-to-quant
  (:use :cl :cl-user) 
  (:export :initialize-symbol-table ;State.cl
	   :clear-symbol-table ;State.cl
	   :canonical-to-student ;algebra.cl
	   :near-miss-vars :capitalization-match-var ;parse-andes.cl 
	   :symbols-sysvar ;parse-andes.cl
	   :sym-referent :sym-entries :symbols-fetch :sym-label ;Entry-API.cl 
	   :symbols-delete-dependents ;Entry-API.cl 
	   ;; More complicated
	   :student-to-canonical :symbols-delete 
	   :symbols-referent :symbols-lookup
	   ))

(eval-when (:load-toplevel :compile-toplevel)
  ;; Several of these functions are introduced as symbols, but not used,
  ;; in KB.  In that case, they already exist in :cl-user.
  ;; This allows one to use KB without loading :symbols package.  
  (dolist (symbol-string (mapcar #'string-upcase 
				 '("symbols-label" "symbols-enter")))
    (multiple-value-bind (symbol internal) (intern symbol-string)
      (when (eql internal :internal) (export symbol))
      (import symbol :symbols))))


(in-package :symbols)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syminfo struct -- Symbol table entry record
;; *variables* -- List of syminfo records concerning student labels 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (syminfo (:print-function print-sym)
                    (:conc-name sym-))
   label       ;student's label as string to preserve case
   referent    ;KB expression for referent, must exist in Ontology.
   sysvar      ;expr for referent usable in solution eqns, NIL if none
   entries     ;list of ids of entries this depends on, for deletions
)

(defun print-sym (sym &optional (Stream t) (Level 0))
  (declare (ignore Level))
  (format stream "<SYM ~A ~A>" (sym-label sym) (sym-referent sym)))


(defvar *variables*)	;lists of student variable names
(defvar *watch-symbols* NIL)  ; set to trace symbol table changes
(defparameter *variable-namespaces* '(:objects :scalars))

(defun get-variables-table (x)
  ;;  If no table is given, use :scalars
  (or (assoc (or x :scalars) *variables*)
      (warn "No such thing as variables of type ~A" x)))

(defun get-variables (&optional x) (cdr (get-variables-table x)))

(defun set-variables (new-variables &optional x)
  (setf (cdr (get-variables-table x)) new-variables))

(defun initialize-symbol-table ()
    (setf *variables* (mapcar #'list *variable-namespaces*)))

(defun clear-symbol-table ()
  (setf *variables* nil))

;;-----------------------------------------------------------------------------
;;       Symbol table manipulation functions:
;;-----------------------------------------------------------------------------

(defun symbols-enter (label referent &key entries sysvar namespace)
  "enter info about a student label into the symbol table"
  ;; The usual case for entries is a single entry.  For component vars,
  ;; it will list both vector and axes entry ids.  For predefs, it is NIL.
  (unless (listp entries) 
    (warn "symbols-enter:  entries ~A should be a list" entries))

  ;; previous code should have already handled existing entries
  (when (symbols-lookup label :namespace namespace)
     (warn "symbols-enter: new entry shadowing existing def for ~A~%" label))

  ;; if no special sysvar specified, look up matching quantity as default
  ;; OK if NIL, not all referents will have matching system vars
  (unless sysvar
     (setf sysvar (quant-to-valid-sysvar referent)))

  ;; build entry and add it to list
  (push (make-syminfo :label label :referent referent 
		      :entries entries :sysvar sysvar)
        (cdr (get-variables-table namespace)))

  ;; for debugging: dump symbol table contents after change
  (when *watch-symbols* (symbols-dump)))

(defun symbols-delete (label)
  "remove entry for a student label from the symbol table"
  ;; remove entry from our table
  (set-variables 
    (delete label (get-variables) 
            :key #'sym-label :test #'equal)))

(defun symbols-delete-dependents (entry-id)
   "Remove symbols dependent on entry with given id, returning labels of removed symbols."
   (let (drop)
     (dolist (table *variables*) ;iterate through all tables
       (let (keep)
	 (dolist (sym (cdr table))
	   (if (member entry-id (sym-entries sym))
	       (push sym drop)
	       (push sym keep)))
	 (setf (cdr table) keep))) ;removing entries for that table.
     (mapcar #'sym-label drop))) ;return dropped from all tables.


;;-----------------------------------------------------------------------------
;;       Symbol table accessor functions:
;;-----------------------------------------------------------------------------

(defun symbols-lookup (label &key namespace)
  "fetch info struct for label, NIL if not found"
   (find label (get-variables namespace) :key #'sym-label :test #'equal))

(defun symbols-referent (label &key namespace)
  "Return quantity denoted by student label or NIL if none"
  (let ((sym (symbols-lookup label :namespace namespace)))
    (when sym (sym-referent sym))))

(defun symbols-lookup-quant (quant &key namespace)
  "fetch sym info struct for quantity, NIL if not found"
   (find quant (get-variables namespace) :key #'sym-referent :test #'unify))

(defun symbols-sysvar (label)
   "return expression in system terms corresponding to student label"
   (let ((sym (symbols-lookup label)))
     (when sym (sym-sysvar sym))))

(defun symbols-label (quant &key namespace)
  "Return student's label for given quantity, NIL if none"
  (let ((sym (symbols-lookup-quant quant :namespace namespace)))
    (when sym (sym-label sym))))

(defun capitalization-match-var (var)
  "Find first match that just differs by capitalization"
  ;; for now, just look for case errors, and just return first one found
  (find var (get-variables) :key #'sym-label :test #'string-equal))

(defun near-miss-vars (var)
  "given a variable, return defined symbols with single-letter spelling errors"
  ;; For now, just look for single letter error.
  ;; Add a little padding to avoid floating point compare problems.
  (when (> (length var) 2)
    (let ((cutoff (/ 1.2 (length var)))
	  (c2 (/ 1.1 (length var)))
	  (match:*word-cutoff* 0.4)) ;only do three letter words or longer
      (mapcar #'match:best-prop
	      (remove-if #'(lambda (x) (> (match:best-value x) c2))
			 (match:best-model-matches
			  (list var)  ;student is list of words
			  (mapcar #'(lambda (x) (cons (sym-label x) x)) 
				  (get-variables))
			:cutoff cutoff))))))

(defun symbols-fetch (pat &key namespace)
  "return list of all symbol records whose referent exprs unify with pat"
  (remove-if-not #'(lambda (sym)
                     (unify pat (sym-referent sym)))
		  (get-variables namespace)))

;; following for inverting mapping from student var to sysvar:
(defun symbols-lookup-sysvar (sysvar)
 "fetch info struct for label mapped to sysvar, NIL if not found"
   ;; make sure sysvar non-NIL; many labels have NIL in sysvar slot
   (if sysvar 
      (find sysvar (get-variables) :key #'sym-sysvar :test #'equal)))

(defun symbols-sysvar-to-studvar (sysvar)
"return student's label corresponding to sysvar, NIL if none"
  (let ((sym (symbols-lookup-sysvar sysvar)))
    (when sym (sym-label sym))))
  
(defun symbols-dump (&optional (s t))
  "Print contents of symbol table"
  (dolist (vars *variables*)
    (format s "Variable Table ~A~%" (car vars))
    (dolist (sym (cdr vars))
      (format s "    ~A ~A ~A ~A~%" (sym-label sym) (sym-referent sym)
	      (sym-sysvar sym) (sym-entries sym)))))


;; Solution graph may contain variables defined along dead-paths which have
;; no value determined for them. These cannot be used in equations since
;; the solver has no value for them. Following only returns translations
;; for variables that are valid in equations, else NIL. This is what
;; we store in the translation field of the symbol info.

(defun student-to-canonical (StudAtom)
  "Map given student equation atom to its canonical var or quantity, returning NIL if no counterpart found"
   ;; NB: student variables are *strings* not symbols or other atoms
  (when (stringp StudAtom)
    (symbols-sysvar StudAtom))) ; use expression stored in symbol table

;; to map single var the other way:
(defun canonical-to-student (Canonical)
  "Match the canonical var to its student var."
   ;; The first call maps via the sysvar's quantity as defined in varindex. 
   ;; But in case of pi, no quant or entry in var index -- so try to map
   ;; via any special entries in symbol table as well (could try for all).
   (or (symbols-label (sysvar-to-quant Canonical))
       (symbols-sysvar-to-studvar Canonical)))

	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of symbols.lsp/cl
;; Copyright (C) 2001 by ??????????????????????????????? -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
