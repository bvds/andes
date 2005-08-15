;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Makeprob -- Utility functions for batch generating problem files
;;
;; Generates only the problems tagged working and Andes2 which are intended 
;; to be part of the Andes2 distribution (ignoring test problems). 
;; Generates in alphabetical order by problem name so progress is obvious.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun working-Andes2-prob (p)
"true if this is both an Andes2 problem and working"
  (and (working-problem-p p)	
       (member 'Andes2 (problem-features p))))

(defun listprobs () 
"list problems in alphabetical problem name order"
   (let (problist)
     (map-problems #'(lambda (p)
        (push p problist)))
     ; and return sorted list
     (sort problist #'(lambda (p1 p2) (string< (problem-name p1) 
                                               (problem-name p2))))))

;; If included, optional topics list is or'd with problem's features. 
;; E.g. (make-prbs 'dynamics 'linmom 'work) to make all of these only.
;; Alternatively, one can give an explicit list of problems
(defun make-prbs (&rest topics)  
 "Dump problem files for all 'working' problems with any of features."
  (let ((Probs (remove-if-not 
		#'(lambda(p)
		    (and (working-Andes2-prob p)
			 (or (null topics)
			     (if (listp (first topics)) 
				 (member (problem-name p) 
					 (first topics))
			       (intersection topics 
					     (problem-features p))))))
		(listprobs)))
	;; for minimum of output, clear all trace/debug flags:
	(*s-print-steps* NIL)    ; intermediate results of top-level steps
	(*debug-gg* NIL)	 ; graph building process steps
	(*debug-sp* NIL)	 ; solution point generation detail
	(*debug* NIL)		 ; debugging messages inside operators
	(*actions* NIL)		 ; traces all problem-solver actions
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
  (let ((Probs (remove-if-not #'(lambda(p)
                                 (and (working-Andes2-prob p)
				      (or (null topics)
				          (intersection topics 
				                    (problem-features p)))))
		(listprobs)))
	; for minimum of output, clear all trace/debug flags:
	(*s-print-steps* NIL)    ; intermediate results of top-level steps
	(*debug-gg* NIL)	 ; graph building process steps
	(*debug-sp* NIL)	 ; solution point generation detail
	(*debug* NIL)		 ; debugging messages inside operators
	(*actions* NIL)		 ; traces all problem-solver actions
        (Errs))
    (dolist (P Probs)
	 (handler-case 
	       (test-solve P)	   
	  (error (E) 
	     (format T "~&!!! Error on ~A: ~A~%" (Problem-name P) E)
	     ; save it for report at end
	     (push (list (Problem-Name P) (format nil "~A" E)) Errs))))
   ; dump list of failures 
   (format T "~&Failures: ~{~W ~}~%" (mapcar #'first Errs))
   (format T "Errors:~%")
   (pprint Errs)))

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
                                 (and (working-Andes2-prob p)
				      (intersection (cdr pair) (problem-features p))))
		              (listprobs)))
	(prob))
    (dolist (P Probs)
       (format outf "\<~A\>~%" (problem-name P))
       (dolist (line (Problem-Statement P))
	   (when (not (find #\[ line)) ;skip answer-box marker lines in stmt
              (format outf "~A~%" line))))))))

; write one text file per problem, containing the problem statement text lines
; puts into "Statements" directory 
(defun write-stmts()
   (map-problems 
      #'(lambda(p) 
         (with-open-file (outf (strcat ".\\Statements\\" (string (problem-name p)) ".txt")
	                  :direction :output :if-exists :supersede)
	   (dolist (line (Problem-Statement P))
	    (when (not (find #\[ line)) ; skip answer-box marker lines in stmt
              (format outf "~A~%" line)))))
      ; filter func to map:
      #'working-andes2-prob))
      
