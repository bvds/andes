;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(defstruct (StudentEntry (:print-function print-StudentEntry))
  Id	 ;Workbench assigned Identifier.
  ;; When the workbench says something is deleted, this is the ID that it uses.
  Prop   ;Entry proposition (Equalp to SystemEntry-prop.)
  State           ;One of correct, inefficient, dead-path, forbidden, incorrect.
  CInterp         ;The Selected set of any SystemEntries that constitute 
  ;; the final Interpretation of the student entry.
  PossibleCInterps ;A list of all the sets of possible correct interpretations.
  Verbatim             ;The student's entry as they typed it.
  ParsedEqn            ;will contain the lisp (prefixed) form of the parsed equation
  ErrInterp            ;nil or an error interpretation
  ;; Some non-eqn student entries carry associated equation entries with them. 
  ;; These associated entries are dependent in that they must be deleted if 
  ;; the main entry is deleted.
  ;; Dependent equation entries hang off the following fields in 
  ;; the main entry:
  ImplicitEqns         ;list of any associated implicit equation entries
  GivenEqns	       ;list of any associated given value equation entries
  (Time (get-current-htime))  ;The entry's time.  
  ;; Typically the time it was create, but not necessarily.
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
  (format Stream "ErrInterp: ~A~%" (StudentEntry-ErrInterp Entry))
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


