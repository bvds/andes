;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TestRun.cl
;; Collin Lynch
;; 4/25/2003
;;
;; This code attempts to carry out the task(s) that were handled by the 
;; previous TestRun.sh shell code.  This is done simply to make my life
;; easier and to make the code more readable.  

(load "./Headers.cl")

(defparameter **Lisp-Path** "c:/Program Files/acl61/alisp.exe")
(defparameter **Output-Dir** "c:/Andes2/Helpdriver/Out/")
(defparameter **Load-Path** "c:/Andes2/HelpDriver/Headers.cl")



;;; This is the core execution function used for processing.
(defun Exec-Header-Func (Fname)
  (run-shell-command
   (format Nil "alisp.exe -d ~a~a.drib -L ~a -e '(~a)' -kill"
	   **Output-Dir** Fname **Load-Path** Fname)
   :Wait t))


;;; -------------------------------------------------------
;;; Core data code.
;;; This code is used to generate the core datasets from the
;;; pitt and USNA code.

(defparameter **Generation-funcs**
    '(generate-usna-fall2002
      post-process-USNA-Fall2002
      generate-usna-fall1999
      post-process-USNA-Fall1999
      generate-usna-fall2000
      post-process-USNA-Fall2000
      generate-usna-fall2001
      post-process-USNA-Fall2001
      
      generate-pitt-fall1999
      post-process-Pitt-Fall1999
      generate-pitt-fall2000
      post-process-Pitt-Fall2000
      generate-pitt-fall2001
      post-process-Pitt-Fall2001)
  "The functions used to generate the core datasets data.")

(defun generate-core-data ()
  (dolist (F **Generation-funcs**)
    (exec-header-func F)))
    

;;; ---------------------------------------------------------
;;; Comparison Data
;;; This code us used to generate the comparison data that 
;;; I use for regression testing.  Loosely it iterates over
;;; the same data but stores it using different names.
(defparameter **comparison-data-funcs**
    '(generate-usna-fall2002-comp
      generate-usna-fall2001-comp
      post-process-USNA-Fall2002-comp
      post-process-USNA-Fall2001-comp
      generate-usna-fall2000-comp
      generate-usna-fall1999-comp
      post-process-USNA-Fall2000-comp
      post-process-USNA-Fall1999-comp
      
      ;;generate-pitt-fall1999-comp
      ;;post-process-Pitt-Fall1999-comp
      ;;generate-pitt-fall2000-comp
      ;;post-process-Pitt-Fall2000-comp
      ;;generate-pitt-fall2001-comp
      ;;post-process-Pitt-Fall2001-comp
      )
   "The functions used to generate the core datasets data.")

(defun generate-comp-data ()
  (dolist (F **Comparison-data-funcs**)
    (exec-header-func F)))


;;; ---------------------------------------------------------
;;; Comparison2 Data
;;; This code us used to generate the comparison data that 
;;; I use for regression testing.  Loosely it iterates over
;;; the same data but stores it using different names.
(defparameter **comparison2-data-funcs**
    '(generate-usna-fall1999-comp2
      post-process-USNA-Fall1999-comp2
      generate-usna-fall2000-comp2
      post-process-USNA-Fall2000-comp2
      
      generate-usna-fall2001-comp2
      generate-usna-fall2002-comp2
      post-process-USNA-Fall2001-comp2
      post-process-USNA-Fall2002-comp2

      ;;generate-pitt-fall1999-comp
      ;;post-process-Pitt-Fall1999-comp
      ;;generate-pitt-fall2000-comp
      ;;post-process-Pitt-Fall2000-comp
      ;;generate-pitt-fall2001-comp
      ;;post-process-Pitt-Fall2001-comp
      )
   "The functions used to generate the core datasets data.")

(defun generate-comp2-data ()
  (dolist (F **Comparison2-data-funcs**)
    (exec-header-func F)))





(defun generate-comp3-F1999 ()
  (exec-header-func 'generate-usna-Fall199-comp3)
  (exec-header-func 'post-process-usna-Fall199-comp3))

(defun generate-comp3-F2000 ()
  (exec-header-func 'generate-usna-Fall199-comp3)
  (exec-header-func 'post-process-usna-Fall199-comp3))


(defun generate-userdata-1 ()
  (exec-header-func 'user-bin-USNA-Fall2001-comp2)
  (exec-header-func 'user-bin-USNA-Fall2002-comp)
  (exec-header-func 'user-bin-USNA-Fall1999-comp2)
  (exec-header-func 'user-bin-USNA-Fall2000-comp2))
						   
						   




