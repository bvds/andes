;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Makeprob -- Utility functions for batch generating problem files
;;
;; Generates only the problems tagged working and Andes2 which are intended 
;; to be part of the Andes2 distribution (ignoring test problems). 
;; Generates in alphabetical order by problem name so progress is obvious.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Andes2-prob (p)
"true if this problem is tagged as part of the main Andes2 distribution"
   (member 'Andes2 (problem-features p)))

(defun listprobs () 
  "list problems in alphabetical problem name order"
  (let (problist)
    (map-problems #'(lambda (p)
		      (push p problist)))
    ;; and return sorted list
    (sort problist #'(lambda (p1 p2) (string< (problem-name p1) 
					      (problem-name p2))))))

(defun choose-working-probs (topics)
  "list of problems with specified features or use given list of problems"
  (remove-if-not #'(lambda (p) 
		     (and (working-problem-p p)
			  (or (null topics)
			      (if (listp (first topics)) 
				  (member (problem-name p) 
					  (first topics))
				(intersection topics 
					      (problem-features p)))))) 
		 (listprobs)))

;; If included, optional topics list is or'd with problem's features. 
;; E.g. (make-prbs 'dynamics 'linmom 'work) to make all of these only.
;; Alternatively, one can give an explicit list of problems
(defun make-prbs (&rest topics)  
 "Dump problem files for all 'working' problems with any of features."
  (let ((Probs (choose-working-probs topics))
	;; for minimum of output, clear all trace/debug flags:
	(*S-Print-Steps* NIL)		;intermediate results of top-level step
	(*Debug-gg* NIL)		;graph building process steps
	(*debug-sp* NIL)		;solution point generation detail
	(*debug-pp* nil)                ;post-processing functions
	(*debug* NIL)			;debugging messages inside operators
	(*actions* NIL)			;traces all problem-solver actions
        (Errs))
    (dolist (P Probs)
	 (handler-case 
	       (store-problem-file P)	   
	  (error (E) 
	     (format T "~&!!! Error on ~A: ~A~%" (Problem-name P) E)
	     ; save it for report at end
	     (push (list (Problem-Name P) (format nil "~A" E)) Errs))))
   ; dump list of failures 
   (format T "~&Failures: ~{~W ~}~%" (mapcar #'first Errs))
   (format T "Errors:~%")
   (pprint Errs)))


;;; test-solve all the problems. Args as for make-prbs
(defun test-prbs (&rest topics)  
 "Test solve all 'working' problems with any of features."
  (let ((Probs (choose-working-probs topics))
	;; for minimum of output, clear all trace/debug flags:
	(*S-Print-Steps* NIL)		;intermediate results of top-level
	(*Debug-gg* NIL)		;graph building process steps
	(*debug-sp* NIL)		;solution point generation detail
	(*debug-pp* nil)                ;post-processing functions
	(*debug* NIL)			;debugging messages inside operators
	(*actions* NIL)			;traces all problem-solver actions
        (Errs))
    (dolist (P Probs)
	 (handler-case 
	       (test-solve (problem-name P)) ;test-solve takes name, not prob struct
	  (error (E) 
	     (format T "~&!!! Error on ~A: ~A~%" (Problem-name P) E)
	     ; save it for report at end
	     (push (list (Problem-Name P) (format nil "~A" E)) Errs))))
   ;; dump list of failures 
   (format T "~&Failures: ~{~W ~}~%" (mapcar #'first Errs))
   (format T "Errors:~%")
   (pprint Errs)))

;;; compare prb files in two directories
;;; (diff-prbs (Default-ProblemFile-Path) #P"/home/bvds/Andes2-old/Problems/" '(lmom1a lmom1b))
(defun diff-prbs (path1 path2 &rest topics)  
  "compare prb files in two directories."
  (let (Errs)
    (dolist (P (choose-working-probs topics))
      (format t "Comparing ~A:  " (problem-name P)) 
      (finish-output)			;flush output buffer
      (let (E P1 P2)
	(handler-case 
	    (progn (setf P1 (read-problem-file (string (problem-name P)) 
					       :Path path1))
		   (setf P2 (read-problem-file (string (problem-name P)) 
					       :Path path2)))
	  (error (c) (setf E (format nil "File read error: ~A" c))))
	(when (null E) (setf E (diff-problem-solutions P1 P2)))
	(format t "~:[OK~;~A~]~%" E E)
	(when E (push (list (Problem-Name P) (format nil "~A" E)) Errs))))
    ;; dump list of discrepencies 
    (format T "~&Discrepencies in: ~{~W ~}~%" (mapcar #'first Errs))
    (format T "Discrepencies:~%")
    (pprint Errs)))


;; need helpsystem loaded for this
;; (dump-html-prbs #P"/home/bvds/Andes2/Problems/" #P"/home/bvds/solutions/")
;; scp -r ~/solutions/ andes3.lrdc.pitt.edu:/home/andes/public_html/learnlab

(defun dump-html-prbs (in-path out-path &rest topics)  
  "write solutions to working problems into a directory"
  (andes-init)
  (dolist (P (choose-working-probs topics))
    ;; also initializes *sg-entries*
    (read-problem-info (string (problem-name P))
		       :path in-path)
       (if *cp* (dump-html-problem-solutions *cp* out-path)
	   (format t "Error:  Can't load ~A~%" (problem-name p))))
  (dump-style-file out-path))

;; need helpsystem loaded for this
;; (dump-entries-operators #P"/home/bvds/Andes2/Problems/" #P"/home/bvds/solutions/")
;; scp -r ~/solutions/ andes3.lrdc.pitt.edu:/home/andes/public_html/learnlab
(defun dump-entries-operators (in-path out-path &rest topics)  
  "write operators and entries to working problems into a directory"
  (andes-init)
  (dolist (P (choose-working-probs topics))
    ;; also initializes *sg-entries*
    (read-problem-info (string (problem-name P))
		       :path in-path)
       (if *cp* (dump-entries-operators-problem-solutions *cp* out-path)
	   (format t "Error:  Can't load ~A~%" (problem-name p)))))

;;; for dealing with problem sets:
;;; For each ANDES problem set, there should be some distinguished feature set 
;;; that can pick out all and only the problems in that set. Easiest way is to 
;;; use some tag for that problem set only.  But possibly more than one 
;;; feature required for things like (dynamics circular)
;;; if other problem use feature like circular. 
;;; Following maps friendly problem set names to distinguishing feature sets
;;;
;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
	*problem-sets* 
	'(
	  ;; Mechanics
	  ("Vectors"  vectors)
	  ("Translational Kinematics"  kinematics)
	  ("Free Body Diagrams"  fbd)
	  ("Statics"  statics)		; also in some fbd-only!
	  ("Translational Dynamics"  dynamics) ; also in some fbd-only
	  ("Circular Motion"  circular)
	  ;; !! work-only problem now in Work-Energy set
	  ("Work"  work)			; (and (not energy))
	  ("Energy"  energy)
	  ("Linear Momentum" linmom)
	  ("Rotational Kinematics"  rotkin)
	  ("Angular Momentum"  angmom)
	  ("Rotational Dynamics"  torque)
	  ;; Electricity and Magnetism
	  ("Electric Field" E-field)
	  ("Electric Potential" potential)
	  ("Capacitance" cap)
	  ("Resistance" res)
	  ;; ("DC Circuits"  (or kir rc)
	  ("Kirchoff's Laws" kir)
	  ("RC Circuits" rc)
	  ("Magnetic Field" mag)
	  ("Faraday's Law" fara)
	  ("Inductance" inductance)
	  ("Optics" optics)
	  ) 	#+sbcl #'equalp)

;;; List problem statements for given sets (for OLI)
;;; topic should be feature unique to this set
;;; File is of form
;;;   LIST<listname>
;;;   <problem-name>
;;;   statement
;;;   ...
;;;   <problem-name>
;;;   statement
;;;   ...
;;;   LIST<listname>
;;;   <problem-name>
;;;   statement
;;;   ....
;;;where lower-case words are replaced by actual names.
(defun make-stmt-list ()  
 "Write OLI-useable list file of problem statements in each problem set"
 (with-open-file (outf "C:\\Andes2\\kb\\Problems.lst"
                  :direction :output :if-exists :supersede)
 (dolist (pair *problem-sets*)
  (format outf "LIST\<~A\>~%" (first pair))
  (let ((Probs (remove-if-not #'(lambda(p)
                                 (and (working-problem-p p) 
				      (Andes2-prob p)
				      (intersection (cdr pair) (problem-features p))))
		              (listprobs)))
	(prob))
    (dolist (P Probs)
       (format outf "\<~A\>~%" (problem-name P))
       (dolist (line (Problem-Statement P))
	   (when (not (find #\[ line)) ;skip answer-box marker lines in stmt
              (format outf "~A~%" line))))))))

;; write one text file per problem, containing the problem statement text lines
;; puts into "Statements" directory. This is a convenience for the script that
;; generates OLI learning pages from problem sets.
(defun write-stmts()
   (map-problems 
      #'(lambda(p) 
         (with-open-file (outf (strcat ".\\Statements\\" (string (problem-name p)) ".txt")
	                  :direction :output :if-exists :supersede)
	   (dolist (line (Problem-Statement P))
	    (when (not (find #\[ line)) ; skip answer-box marker lines in stmt
              (format outf "~A~%" line)))))
      ; filter func to map: Include non-working problems in case we want stubs
      #'andes2-prob))
      
;;; list all possible entries for given problem. 
(defun show-entries (probname &optional (dest T))
  (read-problem-info probname) ; will do sg-setup
  (dolist (e *sg-entries*) 
    (format dest "~S~%     ~S~%" (systementry-prop e) ;(sg-map-systementry->opname e)
					; show full opinst, not just opname
	    (first (sg-map-systementry->opinsts e)))
    ))

;; list all entries in tab-delimited file
(defun write-entry-file (p)
   (with-open-file (outf (strcat ".\\Entries\\" (string (problem-name p)) ".txt")
                         :direction :output :if-exists :supersede)
    (read-problem-info (problem-name p)) 
    (dolist (e *sg-entries*)
     (let ((*print-pretty* NIL))
        (format outf "~S	~S	~S~%" 
             (problem-name p) (systementry-prop e) (sg-map-systementry->opname e))))))

(defun write-entry-files (plist)
   (dolist (p plist)
     (write-entry-file p)))      

(defun write-all-entry-files (&optional topics)
  (write-entry-files (choose-working-probs topics)))

;; dumping answers
(defun expr-value (expr)
 (let ((qvar (find expr (Problem-VarIndex *cp*) :key #'qvar-exp
                                               :test #'equal)))
     (when qvar 
       (if (qvar-units qvar) 
          (list (qvar-value qvar) (qvar-units qvar))
	(qvar-value qvar)))))

(defun write-answers (&optional topics)
(with-open-file (outf "Answers.txt" :direction :output :if-exists :supersede)
 (dolist (P (choose-working-probs topics))
   (format T "~S~%" (problem-name p))
   (ignore-errors
    (setf *cp* (read-problem-file (string (problem-name p))))
    (format outf "~A	~A~%" (problem-name p)
        (mapcar #'expr-value (problem-soughts p)))))
  (format T "End of problems~%"))
(format T "Done writing answers~%"))

;; need helpsystem loaded for this
(defun test-load-prbs (&optional topics)
"verify that prb files can be loaded into helpsystem"
 (andes-init)
 (let (Errs)
   (dolist (P (choose-working-probs topics))
    (handler-case 
     (let ((t0 (get-internal-run-time))) 
       (read-problem-info (string (problem-name p)))
	    (check-entry-opvars)
	    (format t "~A: ~A solution~:p loaded in ~,2F seconds.~%"
		    (problem-name *cp*) (length (problem-solutions *cp*))
		    (/ (- (get-internal-run-time) t0) 
		       internal-time-units-per-second)))
      (error (E) 
	     (format T "~&!!! Error loading ~A: ~A~%" (Problem-name P) E)
	     ; save it for report at end
	     (push (list (Problem-Name P) (format nil "~A" E)) Errs))))
  ;; dump list of failures 
  (format T "~&Failures: ~{~W ~}~%" (mapcar #'first Errs))
  (format T "Errors:~%")
  (pprint Errs)))

(defun check-entry-opvars ()
"check all entry operators for variable list mismatch"
  ; for each solution entry in current problem 
  (dolist (sysent *sg-entries*)  ; NB: requires sg-setup done by read-problem-info
   ; check that its opinst's variable value-list is congruent with kb operator list
   (let* ((step (first (systemEntry-sources sysent))) ; csdo struct for the DO stmt
          (vals (csdo-varvals step))		      ; value list in psm graph (i.e. prb file)
	  (op   (get-operator-by-tag (csdo-op step))) ; operator obj in current kb
	  (opvars (operator-variables op))	      ; variable list in current kb
	  errlist)
;;     (format t "op ~A vals (~A) opvars (~A)~%   ~A~%" (operator-name op) 
;;            (length vals) (length opvars) (mapcar #'cons vals opvars))
      (unless (= (length vals) (length opvars))
	(error "Operator variables don't match operator values: ~a~%    May need to regenerate problem file.~%    operator-variables:  ~A~%    values:  ~A~%" 
		(operator-name op) opvars vals)))))
   
