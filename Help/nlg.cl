;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nlg.cl -- Natural Language Generator code and data
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Collin Lynch (c?l) <collinl@pitt.edu>
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  ??? - (c?l) -- created
;;  29 June 2001 - (lht) -- begin to implement working code
;;  3/10/2003 - (c?l) -- Added support for entryprops.
;;  3/12/2003 - (c?l) -- Fixed compiler warnings.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nlg (x &optional (type 'def-np) &rest args)
  (if (null type)
     (if (consp x)
      (format nil "~(~A~)" x)
    	(format nil "~(~A~)" x))
    (if (variable-p x)
	(format nil "~@[~*at ~]some ~(~A~)" (eq type 'pp) (var-rootname x))
      (if args
	  (apply (if (equal type 'time) 'moment type) x args)
	(funcall (if (equal type 'time) 'moment type) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-print-list (x joiner art)
  (if (= (length x) 1)
      (format nil "~A" (nlg (first x) art))
    (if (= (length x) 2)
	(format nil "~A ~A ~A" (nlg (first x) art) joiner (nlg (second x) art))
      (format nil "~A, ~A ~A" (nlg (first x) art) joiner (nlg-print-list (rest x) joiner art)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-find (x lst ff ef)
  (let (bindings)
    (dolist (rule lst)
	(when (setf bindings (unify (funcall ff rule) x no-bindings))
	    (return-from nlg-find 
	        (nlg-bind rule ef bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-bind (rule ef bindings) ; ef is accessor to get nlg spec from struct
  (Tell :nlg "X <~W>" (list rule bindings))
  (let* ((spec (funcall ef rule)) ; may be NIL if no english specified
         (format (first spec))
	 (args (subst-bindings-quoted bindings (rest spec))))
    (Tell :nlg "Format <~W> ARGS <~W>" format args)
    (if spec
      (andes-eval (cons 'format (cons nil (cons format args)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-list-default (x &rest args)
  (or (nlg-find x *Ontology-ExpTypes* #'ExpType-Form #'ExpType-English)
      (format nil " ~A " x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun nlg-atom-default (x &rest args)
  (format nil "~(~A~)" x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun var-rootname (var)
  "return variable name sans question marks and gensym digits"
  ;; eg (var-rootname '?body1234) => "body" 
  (remove-if #'(lambda (c) 
		 (or (digit-char-p c) (char= c #\?)))
	     (string var)))

;;; Most of our body names are common nouns like "car", "block", "man", 
;;; which require an article, so that is default in def-np, But some problems 
;;; use "block1", "charge2", etc. which are proper names that shouldn't have 
;;; an article. In absence of a lexicon, we here use a simple heuristic to 
;;; distinguish proper names by testing whether name ends in a number. 
;;; We also treat single-character nouns as proper names, e.g. A, B, C, D for 
;;; points.
;;; This will give wrong answer for the very few common nouns we might use 
;;; such as "F-14" that end in a number, but that's preferable to being wrong 
;;; on all the block1's etc.

(defun proper-namep (x)
"true if given symbol is probably a proper name"
  (and (symbolp x) 
       (or (equal (length (string x)) 1) ; single-character noun
	   ; ends with digit:
           (numberp (read-from-string (subseq (string x) 
	                                      (1- (length (string x)))))))))

;;; Our circuit elements are named R1, C1, etc. For these we want to suppress 
;;; the default lower-casing of names done by def-np.  Similarly for 
;;; single-character names A, B, C.  For now, we just apply this to any 
;;; "proper names" of 1 or 2 characters.
(defun upper-case-namep (x)
"true if name symbol is probably best left all upper case"
   (and (proper-namep x) (<= (length (string x)) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun def-np (x &rest args)
  (if (atom x)
     (cond ((numberp x)      (format nil "~A" x))
	   ((upper-case-namep x) (format nil "~A" x))
           ((proper-namep x) (format nil "~(~A~)" x))
           ; else assuming x is a common noun
           (T                (format nil "the ~(~A~)" x)))
   ; else non-atom:
   (nlg-list-default x args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun indef-np	(x &rest args)
  (if (atom x)
      (if (stringp x)
	  (if (member (char x 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U ))
	      (format nil "an ~(~A~)" x)
	    (format nil "a ~(~A~)" x))
	(if (member (char (symbol-name x) 0) '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U ))
	    (format nil "an ~(~A~)" x)
	  (format nil "a ~(~A~)" x)))
    (nlg-list-default x args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun pp (x &rest args)
  (if (atom x)
      (if (numberp x)
	  (format nil "at T~A" (- x 1))
	(format nil "at ~(~A~)" x))
    (nlg-list-default x args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defparameter adjectives
    ;; includes some non-adj ids needing special translations
    '(; accel specs in motion descriptors:
      (speed-up . "speeding up")
      (slow-down . "slowing down")
      ;; special vector angle specifiers:
      (into . "into the plane")
      (out-of . "out of the plane")
      (z-unknown . "unknown, but either into or out of the plane")
      (zero . "zero magnitude")
      ;; rotation specs in rotational motion descriptors
      (cw . "clockwise")
      (ccw . "counterclockwise")
      ;; vector type ids needing special translation:
      (accel . "acceleration")
      (ang-displacement . "angular displacement")
      (ang-velocity . "angular velocity")
      (ang-accel . "angular acceleration")
      (ang-momentum . "angular momentum")
      (relative-position . "relative position")
      ;; energy type ides needing special translation
      (grav-energy . "gravitational potential energy")
      (spring-energy . "elastic potential energy")
      (electric-energy . "electric potential energy")
      ;; sign abbreviations
      (pos . "positive")
      (neg . "negative")
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adjective (x &rest args)
  (if (atom x)
      (let ((answer (assoc x adjectives)))
	(if answer
	    (format nil "~A" (cdr answer))
	  (format nil "~(~A~)" x)))
    (nlg-list-default x args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adj (x &rest args)
  (adjective x args))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun strip-outer-parens (s)
"if arg is string w/outer parens, remove them, else return arg unchanged"
 (if (and (stringp s)
          (equal (subseq s 0 1) "(") 
	  (equal (subseq s (1- (length s)) (length s)) ")"))
     (format nil "~A" (subseq s 1 (1- (length s))))
   s))

(defun algebra (x &rest args)
    ; x could be prefix expr (maybe DNUM), var or dimensionless number
    (strip-outer-parens (format nil "~A" (subst-student-vars (pre2in x)))))

; for concise reference to quantities in algebraic contexts:
(defun var-or-quant (x &rest args)
"return student's var for quant if one exists, else full quantity def."
    (or (symbols-label x)
        (def-np x args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun conjoined-defnp (x &rest args)
  (if (atom x)
      (nlg-atom-default x args)
    (nlg-print-list x '|and| 'def-np)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun disjoined-defnp (x &rest args)
  (if (atom x)
      (nlg-atom-default x args)
    (nlg-print-list x '|or| 'def-np)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun conjoined-indefnp (x &rest args)
  (if (atom x)
      (nlg-atom-default x args)
    (nlg-print-list x '|and| 'indef-np)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun disjoined-indefnp (x &rest args)
  (if (atom x)
      (nlg-atom-default x args)
    (nlg-print-list x '|or| 'indef-np)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun moment (x &rest args)
  (if (atom x)
      (format nil "T~(~A~)" (if (numberp x) (1- x) x))
    (nlg-list-default x args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Because times can appear either as durations or individual numbers therefore
;; this nlg-time function is necessary.
(defun nlg-time (time &rest args)
  (if (atom time) (format nil "at time ~a" time)
    (apply #'nlg-exp time args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun lower-case (x &rest args)
  (if (atom x)
      (format nil "~(~A~)" x)
    (nlg-list-default x args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun goal (x &rest args)
  (if (atom x)
      (lower-case x args)
    (or (nlg-find x *Ontology-GoalProp-Types* #'GoalProp-Form #'GoalProp-English)
	(format nil "[GOAL: ~A]" x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun psm-exp (x &rest args)
  (if (atom x)
      (nlg-atom-default x args)
    (or (nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-ExpFormat)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-ExpFormat)
	(nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-English)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-English)
	(format nil "[PSM: ~A]" x))))

(defun psm-group (x &rest args)
"kvl: returns the english for the group, of which this psm is a part"
  (cond ((atom x)
	 (nlg-atom-default x args))
	((nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-English))
	((loop for c in *Ontology-PSMClasses* with bindings 
	     when (setf bindings (unify (PSMClass-form c) x)) 
	     do (return (nlg-find (PSMClass-group c) *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-English))))
	((nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-English))
	(T (format nil "[PSM: ~A]" x))))

(defun psm-english (x &rest args)
  (if (atom x)
      (nlg-atom-default x args)
    (or (nlg-find x *Ontology-PSMClasses* #'PSMClass-Form #'PSMClass-English)
	(nlg-find x *Ontology-PSMGroups* #'PSMGroup-Form #'PSMGroup-English)
	(format nil "[PSM: ~A]" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling Entry-Props
(defun nlg-entryprop (x &rest args)
  (if (atom x) 
      (nlg-atom-default x args)
    (or (nlg-find x *Ontology-EntryProp-Types* #'EntryProp-KBForm #'EntryProp-English)
	(nlg-find x *Ontology-EntryProp-Types* #'EntryProp-HelpForm #'EntryProp-English)
	(format Nil "[EntryProp: ~a]" x))))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun psm-type (x &rest args)
    (cond ((psmgroup-p x) (car (PSMGroup-English x)))
	  ((psmclass-p x) (car (PSMclass-english x)))
	  (T (format nil "[PSM type: ~A]" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given an ExpType nlg it resulting in the appropriate form.
(defun nlg-exp (x &rest args)
  (cond ((atom x) (format nil "~A" x))
	((nlg-find x *Ontology-ExpTypes* #'Exptype-form #'ExpType-English))
	(t (format nil "exp:[~A]" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given an Equation nlg it resulting in the appropriate form.
(defun nlg-equation (x &rest args)
  (cond ((atom x) (format nil "~A" x))
	((nlg-find x *Ontology-Equations* #'Equation-form #'Equation-English))
	(t (format nil "equation:[~A]" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given an equation nlg its equation form.
(defun nlg-equation-format (x &rest args)
  (cond ((atom x) (format nil "~A" x))
	((nlg-find x *Ontology-Equations* #'Equation-form #'Equation-EqnFormat))
	(t (format nil "equation-format:[~A]" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Given a partially bound vector definition convert it to a type for the
;; nlg of the equation form.  This type will be one of 
;; Displacement d
;; Velocity v
;; accelleration a
;; force F
;; relative position r
;; momentum p
(defun nlg-vector-var-pref (x &rest args)
  (if (atom x) (format nil "~A" x)
    (case (car x)
      (displacement "d")
      (velocity "v")
      (acceleration "a")
      (force "F")
      (relative-position "r")
      (momentum "p")
      (t (format nil "vec-var:[~a]" x)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of nlg.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






