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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modified:
;;   18 March 2001 - (lht) -- took over for addition to Andes2 code
;;   5 June 2003 - (cl) -- removed depreciated calls to replace-greek.
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

;; Labels containing Greek letters come to us coded by dollar-sign escape 
;; sequences of the form "$q". These two-character sequences stand for the 
;; character corresponding to "q" in the Symbol font, in this case theta. 
;; As a convenience Andes1 allowed the student to spell out the Greek letters 
;; by name at the beginning of variables, e.g. by typing "theta" or "thetaFw"
;; instead of the sequence to produce "$q" or "$qFw". To allow this we 
;; treat the dollar-sign version as the canonical label and translate labels 
;; that may contain escapes into canonical form before entering or looking 
;; them up, using the replace-greek function from the equation grammar module. 
;; Note this can cause problems if pi is used for initial momentum.
;; !!! Now dropped -- replace-greek is currently a no-op.
;; !!! Replace greek has now been removed from this file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; syminfo struct -- Symbol table entry record
;; *variables* -- List of syminfo records concerning student labels 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct (syminfo (:print-function print-sym)
                    (:conc-name sym-))
   label	; student's label as string to preserve case
   referent	; kb expression for referent
   sysvar	; expr for referent usable in solution eqns, NIL if none
   entries	; list of ids of entries this depends on, for deletions
)
(defun print-sym (sym &optional (Stream t) (Level 0))
  (declare (ignore Level))
  (format stream "<SYM ~A ~A>" (sym-label sym) (sym-referent sym)))

(defvar *watch-symbols* NIL)  ; set to trace symbol table changes

;-----------------------------------------------------------------------------
; Symbol table manipulation functions:
;-----------------------------------------------------------------------------

(defun symbols-enter (label referent entries &optional sysvar)
  "enter info about a student label into the symbol table"
  ; Allow a single entry id argument here for the usual case where 
  ; symbol info derives from single entry. For component vars it will list
  ; both vector and axes entry ids. Form singleton list for subsequent code.
  ; if entries is NIL, as for predefs, leave it as empty list
  (when (and entries (atom entries))
      (setf entries (list entries))) 

  ; Client code should delete symbols from an existing entry before handling
  ; modified entry.  Workbench should prevent a brand new entry from redefining 
  ; any existing def of same label. 
  (when (symbols-lookup label)
     (warn "symbols-enter: new entry shadowing existing def for ~A~%" label))

  ; if no special sysvar specified, look up matching quantity as default
  ; OK if NIL, not all referents will have matching system vars
  (when (null sysvar)
     (setf sysvar (quant-to-valid-sysvar referent)))

  ; build entry and add it to list
  (push (make-syminfo :label label :referent referent 
		      :entries entries :sysvar sysvar)
        *variables*)

  ; for debugging: dump symbol table contents after change
  (when *watch-symbols* 
        (symbols-dump)))

(defun symbols-delete (label)
  "remove entry for a student label from the symbol table"
  ; remove from equation parser's grammar (safe if never added)
  (grammar-remove-variable label) 
  ; remove entry from our table
  (setf *variables*
    (delete label *variables* 
            :key #'sym-label :test #'equal)))

(defun symbols-delete-dependents (entry-id)
   "remove all symbols dependent on entry with given id"
   ; remove all affected labels from equation parsing grammar
   (dolist (sym *variables*)
      (when (member entry-id (sym-entries sym))
         (grammar-remove-variable (sym-label sym))))

   ; delete all affected symbols from symbol table
   (setf *variables* 
      (remove-if #'(lambda(sym) 
                    (member entry-id (sym-entries sym)))
	         *variables*))
   ; for debugging: dump symbol table contents after change
   (when *watch-symbols*
          (symbols-dump)))

;-----------------------------------------------------------------------------
; Symbol table accessor functions:
;-----------------------------------------------------------------------------

(defun symbols-lookup (label)
  "fetch info struct for label, NIL if not found"
   (find label *variables* :key #'sym-label :test #'equal))

(defun symbols-referent (label)
  "Return quantity denoted by student label or NIL if none"
  (let ((sym (symbols-lookup label)))
    (if sym (sym-referent sym))))

(defun symbols-lookup-quant (quant)
  "fetch sym info struct for quantity, NIL if not found"
   (find quant *variables* :key #'sym-referent :test #'unify))

(defun symbols-sysvar (label)
   "return expression in system terms corresponding to student label"
   (let ((sym (symbols-lookup label)))
     (if sym (sym-sysvar sym))))

(defun symbols-label (quant)
  "Return student's label for given quantity, NIL if none"
  (let ((sym (symbols-lookup-quant quant)))
    (if sym (sym-label sym))))

(defun symbols-fetch (pat)
  "return list of all symbol records whose referent exprs unify with pat"
  (remove-if-not #'(lambda (sym)
                     (unify pat (sym-referent sym)))
		  *variables*))

; following for inverting mapping from student var to sysvar:
(defun symbols-lookup-sysvar (sysvar)
 "fetch info struct for label mapped to sysvar, NIL if not found"
   ; make sure sysvar non-NIL; many labels have NIL in sysvar slot
   (if sysvar 
      (find sysvar *variables* :key #'sym-sysvar :test #'equal)))

(defun symbols-sysvar-to-studvar (sysvar)
"return student's label corresponding to sysvar, NIL if none"
  (let ((sym (symbols-lookup-sysvar sysvar)))
    (if sym (sym-label sym))))

; Several symbols may be entered from a single student entry.
; E.g. vector entry introduces mag, dir, and maybe x and y comps
; Axis entry introduces x and y labels.
; Following gets a symbol defined by a particular student entry id.
; Set pat appropriately to specify entry quantity type, 
; e.g. to retrieve vector or axis entry with given entry id
(defun symbols-lookup-entry (pat entry-id)
  "Return first info struct for quant matching pattern and entry id"
  (find entry-id (symbols-fetch pat) :key #'sym-entries :test #'entry-match))

(defun entry-match (id entry-list)
 "true if entry list is singleton consiting of this id"
  (equalp (list id) entry-list))

(defun symbols-entry-referent (pat entry-id)
  "return referent of entry for quant matching pat of given id"
  (let ((sym (symbols-lookup-entry pat entry-id)))
    (if sym (sym-referent sym))))
  
(defun symbols-dump ()
  "Print contents of symbol table"
  (format t "~&Student Labels Now:~%")
  (dolist (sym *variables*)
     (format t "~A ~A ~A ~A~%" (sym-label sym) (sym-referent sym)
	                       (sym-sysvar sym) (sym-entries sym))))

;-----------------------------------------------------------------------------
; Utility functions for mapping variables:
;-----------------------------------------------------------------------------
(defun quant-to-sysvar (quant &optional (Problem *cp*)) ; could be helper in solution graph module
  "lookup system variable name for quant in current problem, NIL if none"
  (let ((qvar (find quant (Problem-VarIndex Problem) 
		    :key #'qvar-exp 
		    :test #'unify)))
     (when qvar (qvar-var qvar))))

(defun sysvar-to-quant (sysvar &optional (Problem *cp*)) ; could be helper in solution graph module
  "lookup system variable name for quant in current problem, NIL if none"
  ; NB: lookup is case-sensitive
  (let ((qvar (find sysvar (Problem-VarIndex Problem) 
		    :key #'qvar-var 
		    :test #'equal)))
     (when qvar (qvar-exp qvar))))

(defun sysvar-p (sysvar &optional (Problem *cp*))
  "True if the argument is a system variable"
  (and (symbolp sysvar) (sysvar-to-quant sysvar Problem)))

; Solution graph may contain variables defined along dead-paths which have
; no value determined for them. These cannot be used in equations since
; the solver has no value for them. Following only returns translations
; for variables that are valid in equations, else NIL. This is what
; we store in the translation field of the symbol info.
(defun quant-to-valid-sysvar (quant &optional (Problem *cp*)) 
  "lookup algebraically usable system variable name for quant in current problem, NIL if none"
  (let ((qvar (find quant (Problem-VarIndex Problem) 
		    :key #'qvar-exp 
		    :test #'unify)))
     (when (and qvar (qvar-value qvar)) ; make sure it has a value
         (qvar-var qvar))))

(defun student-to-canonical (StudAtom)
  "Map given student equation atom to its canonical var or quantity, returning NIL if no counterpart found"
   ; NB: student variables are *strings* not symbols or other atoms
  (if (stringp StudAtom)
    (symbols-sysvar StudAtom))) ; use expression stored in symbol table

(defun map-student-atom (StudAtom)
 "translate atomic student expr to canonical if there is one, else return it unchanged"
   (or (student-to-canonical StudAtom)
       StudAtom))

(defun subst-canonical-vars (Exp)
  "Translate expression subsituting canonical vars for student vars throughout."
  ;; NB: student variables must be *strings* in Exp, not symbols
  ;; Strings with no matching sys expression pass through translation unchanged.
    (cond ((atom Exp) (map-student-atom Exp)) 
	  (t (cons (subst-canonical-vars (car Exp))
		   (subst-canonical-vars (cdr Exp))))))

; to map single var the other way:
(defun canonical-to-student (Canonical)
  "Match the canonical var to its student var."
   ; The first call maps via the sysvar's quantity as defined in varindex. 
   ; But in case of pi, no quant or entry in var index -- so try to map
   ; via any special entries in symbol table as well (could try for all).
   (or (symbols-label (sysvar-to-quant Canonical))
       (symbols-sysvar-to-studvar Canonical)))
 
(defun subst-student-vars (Exp)
  "Subsitute student vars into expression for their canonical counterparts."
    ; atoms may be system vars, if no counterpart studvar, leave unchanged.
    (cond ((atom Exp) (or (canonical-to-student Exp)
                          Exp))
	  (t (cons (subst-student-vars (car Exp))
		   (subst-student-vars (cdr Exp))))))



;;;----------------------------------------------------------------------------
;;; Testing the state of canonical ars such as whether or not they are 
;;; params etc is done here.  These functions look up the appropriate
;;; vars in the var-index of the problem struct and return info based upon
;;; that as necessary.  
(defun canonical-var-answer-var-p (C)
  "Is the canonical var an answer-var?"
  (answer-varp C (Problem-VarIndex *cp*)))

(defun  canonical-var-cancelling-var-p (C)
  "Is the canonical var a cancelling-var?"
  (cancelling-varp C (Problem-VarIndex *cp*)))
	   



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of symbols.lsp/cl
;; Copyright (C) 2001 by ??????????????????????????????? -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
