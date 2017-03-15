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
;;; (diff-prbs (Default-ProblemFile-Path) #P"/home/bvds/Andes2-old/solutions/" '(lmom1a lmom1b))
(defun diff-prbs (path1 path2 &rest topics)  
  "compare prb files in two directories."
  (let (Errs englishes)
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
	(when E (push (list (Problem-Name P) (format nil "~A" E)) Errs))
	;; This is not a very important test, since any updates
	;; to Englishes are done by hand.
	(when (and p1 p2
		   (not (unify (mapcar #'car (problem-English p1))
			       (mapcar #'car (problem-English p2)))))
	  (push (problem-name P) englishes))))
    ;; dump list of discrepencies 
    (format T "~&Discrepencies in: ~{~W ~}~%" (mapcar #'first Errs))
    (format T "Discrepencies:~%")
    (pprint Errs)
    (when Englishes 
      (format T "~&Englishes:~%")
      (pprint Englishes))))

(defun andes-init ()
  "initialize parser and start up solver"
  (parse-initialize)   ;set up memoization for parse functions
  (physics-algebra-rules-initialize) ;initialize grammar
  
  (solver-load)
  (solver-logging *solver-logging*))


;; (dump-html-prbs #P"/home/bvds/solutions/")
;; scp -r ~/solutions/ andes3.lrdc.pitt.edu:/home/andes/public_html/learnlab

(defun dump-html-prbs (out-path &rest topics)  
  "write solutions to working problems into a directory"
  (andes-init)
  (dolist (P (choose-working-probs topics))
    ;; also initializes *sg-entries*
    (read-problem-info (string (problem-name P)))
    (if *cp* (dump-html-problem-solutions *cp* out-path)
	(format t "Error:  Can't load ~A~%" (problem-name p))))
  (dump-style-file out-path))

(defun set-kcs ()
  (andes-init)
  (let (prob-kcs results)
    (dolist (set (cadr *sets*))
      (let (all required)
	(dolist (prob (second set))
	  (unless (assoc prob prob-kcs)
	    (push (cons prob (problem-kcs prob)) prob-kcs))
	  (let ((kcs (cdr (assoc prob prob-kcs))))
	    (when kcs
	      (setf all (union (car kcs) all))
	      (setf all (union (cdr kcs) all))
	      (setf required (union (car kcs) required)))))
	;; (format t "set ~A got:~%  ~A~%  ~A~%" (car set) required all)
	(push (list (car set) required (set-difference all required))
	      results)))
    (reverse results)))
	  

(defun problem-kcs (prob)
  "Returns a list of kc's found in all solutions and a list of kc's found in only some solutions."
  ;; need help system loaded to use this function.
  ;; run function (andes-init) first.
  (let (*cp*)
    (handler-case
	(read-problem-info prob)
      (error (c) (declare (ignore c))
	     (format T "File read error: ~A~%" prob)))
    (when *cp*
      (let (not-all any)
	(dolist (solution (problem-solutions *cp*))
	  (let (this)
	    (dolist (entry (distinct-SystemEntries 
			    (mappend #'bgnode-entries (EqnSet-nodes solution))))
	      (dolist (opinst (remove-duplicates 
			       (copy-list (SystemEntry-Sources entry))
			       :key #'csdo-op :test #'unify))
		(let ((kc (csdo-op opinst)))
		  (pushnew (car kc) any)
		  (pushnew (car kc) this))))
	  (dolist (op any)
	    (unless (member op this) (pushnew op not-all)))))
	(cons (set-difference any not-all) not-all)))))
  
;; need helpsystem loaded for this
;; (dump-entries-operators #P"/home/bvds/solutions/")
;; scp -r ~/solutions/ andes3.lrdc.pitt.edu:/home/andes/public_html/learnlab
(defun dump-entries-operators (out-path &rest topics)  
  "write operators and entries to working problems into a directory"
  (andes-init)
  (dolist (P (choose-working-probs topics))
    ;; also initializes *sg-entries*
    (read-problem-info (string (problem-name P)))
       (if *cp* (dump-entries-operators-problem-solutions *cp* out-path)
	   (format t "Error:  Can't load ~A~%" (problem-name p)))))

;; Quick analysis for Kasia Muldner (Katarzyna.Muldner@asu.edu)
(defun dump-step-principles-length (&rest topics)  
  "For each problem, write the number of PSMS and Entries (except implicit-eqn) for the first solution."
  ;; Might want to turn off *debug-help*
  (andes-init)
  (dolist (P (choose-working-probs topics))
    ;; also initializes *sg-entries*
    (read-problem-info (string (problem-name P)))
    (when *cp* 
      (let ((sol (first (problem-solutions *cp*))))
	(format t "~A ~A ~A~%" (problem-name *cp*)
		(length (Eqnset-Eqns sol)) 
		(length (remove-if #'(lambda (x) (eql (car x) 'implicit-eqn))
				   (distinct-SystemEntries 
				    (mappend #'bgnode-entries 
					     (EqnSet-nodes sol)))
				   :key #'systementry-prop)))))))

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
	+problem-sets+ 
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
 (dolist (pair +problem-sets+)
  (format outf "LIST\<~A\>~%" (first pair))
  (let ((Probs (remove-if-not #'(lambda(p)
                                 (and (working-problem-p p) 
				      (Andes2-prob p)
				      (intersection (cdr pair) (problem-features p))))
		              (listprobs))))
    (dolist (P Probs)
       (format outf "\<~A\>~%" (problem-name P))
       (dolist (line (Problem-Statement P))
	   (when (not (find #\[ line)) ;skip answer-box marker lines in stmt
              (format outf "~A~%" line))))))))

;; write one text file per problem, containing the problem statement text lines
;; puts into "Statements" directory. This is a convenience for the script that
;; generates OLI learning pages from problem sets.
(defun write-stmts ()
   (map-problems 
      #'(lambda(p) 
         (with-open-file (outf (merge-pathnames (strcat "Statements/" (string (problem-name p)) ".txt") *andes-path*)
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
                                               :test #'unify)))
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
	    (check-answers-vs-statement)
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

(defun check-answers-vs-statement ()
  "Check done button matches problem"
  ;; This is adapted from open-problem method in Help/sessions.cl
  (let ((id 0))
    (dolist  (line (problem-statement *cp*))
      (cond ((unify line '(answer . ?rest))
	     ;; Add to *StudentEntries* but don't evaluate in Help.
	     (let ((entry (make-studententry :id (format nil "~A" (incf id)) 
					     :mode "unknown" :type "statement")))
	       (select-sought-for-answer entry)
	       ;; sanity test
	       (unless (StudentEntry-prop entry)
		 (error "Problem answer ~A doesn't match soughts" line))))
	    
	    ;; Multiple choice.  See Bug #1551
	    ;; Checkboxes (with a done button) or radio buttons
	    ((or (unify line '(choose ?label ?a ?b . ?rest))
		 (unify line '(checkbox ?label ?a ?b . ?rest)))
	     (let* ((checkbox-p (eq (pop line) 'checkbox))
		    (button-type (if checkbox-p "checkbox" "radio"))
		    (label (pop line)))
	       ;; sanity check
	       (unless (member `(choose-answer ,label . ?rest) (problem-soughts *cp*)
			       :test #'unify)
		 (error "Invalid label ~A for ~A" label button-type))
	     
	     ;; Add weight and choices to grading
	     (let ((sysent (find-SystemEntry `(choose-answer ,label . ?rest))))
	       (unless sysent
		   (error "No SystemEntry for multiple-choice ~A" label)))))
	    
	  ;; "I am done" button.  See Bug #1551
	    ((unify line '(choose ?label ?a))
	     (pop line)
	     (let* ((label (pop line)) 
		    ;; If name of activity was supplied, use that.
		    ;; else search for any done button.
		    (label-match (if label (list 'done label) '(done . ?rest)))
		    ;; Find any done button SystemEntry.
		    (donners (remove label-match *sg-entries*
				     :key #'SystemEntry-prop :test-not #'unify)))
	       ;; Sanity checks
	       (when (or (null donners) (cdr donners))
		 ;; Student can't successfully solve the problem if
		 ;; this is broken.
		 (error "Bad SystemEntry match for button ~A." label))))))))
  
(defun check-entry-opvars ()
  "check all entry operators for variable list mismatch"
  ;; for each solution entry in current problem 
  (dolist (sysent *sg-entries*)  ; NB: requires sg-setup done by read-problem-info
    ;; check that its opinst's variable value-list is congruent with kb operator list
    (let* ((step (first (systemEntry-sources sysent))) ; csdo struct for the DO stmt
	   (vals (csdo-varvals step))		      ; value list in psm graph (i.e. prb file)
	   (op (get-operator-by-tag (csdo-op step))) ; operator obj in current kb
	   (opvars (operator-variables op)))	      ; variable list in current kb
;;     (format t "op ~A vals (~A) opvars (~A)~%   ~A~%" (operator-name op) 
;;            (length vals) (length opvars) (mapcar #'cons vals opvars))
      (unless (= (length vals) (length opvars))
	(error "Operator variables don't match operator values: ~a~%    May need to regenerate problem file.~%    operator-variables:  ~A~%    values:  ~A~%" 
		(operator-name op) opvars vals)))))
   
