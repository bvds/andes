#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Student Entry.cl
;; Collin Lynch
;; 4/25/2002
;;
;; This file defines the Student entry facility 
;; complete with printing functions and other
;; facility elements.  It is called and used
;; primarily by 
;; 
;; Student entry -- represents a workbench action where the student made a 
;;                  problem solving entry, such as writing an equation or 
;;                  drawing a vector.  Fields are
;;     Identifier     -- atom assigned by the workbench.  When the workbench says 
;;                       something is deleted, this is the ID that it uses. 
;;     Proposition    -- an entry proposition built by the Entry interpreter. 
;;     Correctness    -- whether the entry is correct, inefficient, dead-path, 
;;                       forbidden or incorrect 
;;     Interpretation -- If the entry is correct, this is a set of system entries, 
;;                       this is the set of the system entries that correspond to it.  
;;                       Its a set because a compound student equation corresponds to 
;;                       a set of primitive equations.  If the entry is incorrect, this 
;;                       is NIL or an error interpretation produced by an error handler. 
;;     Correct-interpretations -- If the entry is correct, all logically possible 
;;                                interpretations are stored here.  One of these 
;;                                is selected by the cognitive load procedure and 
;;                                put into the Interpretation slot by the Entry Interpreter. 
;;     Error-interpretations -- If the entry is incorrect, all interpretations produced by 
;;                              the error handlers are stored here.  One of these is 
;;                              selected by What's wrong help and put in the the 
;;                              Interpretation slot. 
;;     Verbatim -- If the entry is an equation, this records the student's string exactly 
;;                 as they typed it before it was parsed.  This is useful for some error
;;          	   handlers, such as Equation-used-as-method-name. 
;;
;;     Time -- An Htime recording (typically) when the entry was created 
;;             although it may be set manually if the necessary.
|#


(defstruct (StudentEntry (:print-function print-StudentEntry))
  Id	               ;; Workbench assigned Identifier.
  Prop                 ;; Entry proposition (Equalp to System entry.)
  State                ;; One of correct, inefficient, dead0path, forbidden, incorrect.
  CInterp              ;; The Selected set of entires that constitute the final
                       ;; Interpretation of this entry (if any).
  PossibleCInterps     ;; A list of all the sets of possible correct interpretations (if any).
  Verbatim             ;; The student's entry as they typed it.
  ParsedEqn            ;; will contain the lisp (prefixed) form of the parsed equation
  ErrInterp            ;; nil or an error interpretation
  
  ImplicitEqn          ;; (lht) may not be correct thing to do
  (Time (get-current-htime))  ;;  The entry's time.  Typically the time it was created
                               ;;  But not necessarily.
  )

(defun print-StudentEntry (Entry &optional (Stream t) (Level 0))
  "Print the system entry for human consumption."
  (pprint-indent :block Level Stream)
  (format Stream "[Entry: ~A ~A ~A ~A~%" (StudentEntry-ID Entry) 
	  (StudentEntry-Prop Entry) (StudentEntry-State Entry)
	  (Studententry-time Entry))
  (pprint-indent :block Level Stream)
  (format Stream "        ~A~%" 
	  (StudentEntry-Verbatim Entry))
  (pprint-indent :block Level Stream)
  (format Stream "CorrectInterp:    ~A~%" (StudentEntry-CInterp Entry))
    (pprint-indent :block Level Stream)
  (format Stream "PossibleCorrectInterps:   ~A~%" (StudentEntry-PossibleCInterps Entry))
  (pprint-indent :block Level Stream)
  (format Stream "ErrorInterp: ~A~%" (StudentEntry-ErrInterp Entry))
  (pprint-indent :block Level Stream)
  (format Stream "ParsedEqn: ~A~%" (StudentEntry-ParsedEqn Entry)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define a student entry with no interpretations.

(defun defentry (ID Prop Verbatim)
  (make-Studententry 
   :ID ID
   :Prop Prop
   :Verbatim Verbatim))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Student entry list functions.

(defun get-id (ID Entries)
  "Return the student entry for the specified ID if it has been done."
  (find ID Entries :test #'equalp :key #'StudentEntry-ID))

(defun get-entry-prop (Prop Entries)
  "Return the student entry for the specified prop if it has been done."
  (find Prop Entries :test #'equalp :key #'StudentEntry-Prop))

(defun get-verbatim-done (Verb Entries)
  "Return the verbatim element iff it has been done in the list."
  (find Verb Entries :test #'equalp :#'StudentEntry-Verbatim))

(defun get-state-entries (State Entries)
  "Return the subset of all entries that are in the specified state."
  (subset State Entries :test #'equalp :key #'StudentEntry-State))


