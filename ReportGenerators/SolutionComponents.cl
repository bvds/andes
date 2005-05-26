;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SolutionComponents.cl
;; Collin Lynch
;; 6/20/2003
;;
;; This file is used to generate the solution component
;; report from our current set of problems.  This code 
;; will print out a report of all problems by name listing
;; the necessary equations and entries within each solution.
;;
;; In order to do this it depends upon the Knowledge base 
;; and will load the necessary Andes modules at runtime.



;;;; ========================================================
;;;; Load the Knowledge Base
;;;; This code is used to load the Andes KB for use at runtime.

;; Ensure that the values will all be read in in the appropriate form.
(setf *read-default-float-format* 'double-float)

(load "c:/Andes2/AndesModules.cl")
(defparameter **Root-Path** "c:/Andes2/")
(defparameter **ModFiles** 
    '("Solver_Release/AMFile.cl"
      "Base/AMFile.cl"
      "HelpStructs/AMFile.cl"
      "Knowledge/AMFile.cl"
      "KB/AMFile.cl"
      "Help/AMFile.cl"
      "Testcode/AMFile.cl"
      ))

(setup-andes-module-system **Root-Path**)
(dolist (F **ModFiles**)
  (load (concatenate 'string **Root-Path** F)))
(load-andes-modules)
(solver-load)  ;; Initialize the solver so that we can extract entries.

;;;; ========================================================
;;;; Main Loop.
;;;; The main report generation function takes an output filename
;;;; and then iterates over the set of problems printing out a 
;;;; separate record for each one in the result file.  Each result
;;;; will specify the problem name, its description string, and
;;;; the list of necessary equations/variables.
;;;;
;;;; If the StdOut keyword is supplied then the results will be 
;;;; echoed to the stout as opposed to a file.


(defun generate-solcomp-report (&key (Filename Nil) (Working Nil) (Features Nil) (Right-Margin 70))
  (let ((**Width** 300))
    (if (null Filename) (solcomp-inner-loop t Working)
      (with-open-file (FILE Filename
		       :Direction :Output
		       :If-Exists :Error
		       :If-Does-Not-Exist :Create)
	(solcomp-inner-loop FILE Working Features)))))


;;; Note: not the most efficent that it could be but it'll do.
(defun solcomp-inner-loop (Stream Working &optional (Features Nil))
  (map-problem-files
   #'(lambda (P) 
       (when (not (equalp (problem-name P) 'exkt20B))
	 (solcomp-print-report P Stream)))
   :Working Working
   :Features Features))

  
  


;;; For each problem that is loaded print out the problem's name
;;; followed by the problem description and the equations and 
;;; the solutions.
(defun solcomp-print-report (Problem Stream)
  "Print out the file stream."
  (sg-setup Problem)  ;; Setup the specified problem.
  ;;(do-read-problem-info 
  (setq *cp* Problem)
  ;;(pprint *cp*)
  (reset-runtime-testset-scores)
  ;;(solcomp-printvals)
  (format Stream "~,,35,'=@a BEGIN: ~a ~,,35,'=@a~2%" #\= (problem-name Problem) #\=)
  (format Stream "Problem: ~a~%" (problem-name Problem))
  (format Stream "~a~2%" (apply #'concatenate 'string (problem-statement Problem)))
  (format Stream "Sought: ~{~a~%~}" (mapcar #'nlg (problem-soughts Problem)))
  (solcomp-print-givens (problem-givens Problem) Stream)
  ;; Iterate over the solutions printing eqns and entries.
  (format Stream "Starting Solutions.~%")
  (solcomp-print-solutions Problem Stream)
  (format Stream "~,,35,'=@a END: ~a ~,,35,'=@a~% ~3%" 
	  #\= (problem-name Problem) #\=))


(defun solcomp-printvals ()
  (pprint *test-cache-eqn-entries*)
  (pprint *test-cache-given-eqn-entries*)
  (pprint *test-cache-axis-entries*))

;;; Print out the given values, ibjet definitions and time point defs 
;;; ignoring all of the special flags.
(defun solcomp-print-givens (Givens Stream)
  (format Stream "Givens: ~%")
  (dolist (G Givens)
    (case (car G)
      (Object (format Stream "   There exists an object: ~a~%" (nlg (cadr G))))
      (Time 
       (format Stream "   There is a time: ~a~%" 
	       (if (atom (cadr G))  ;; Just a little hack to deal w times.
		   (format Nil "T~a" (- (cadr G) 1))
		 (nlg (cadr G) 'nlg-time))))
      (Given (format Stream "   ~a = ~a~%" (nlg (cadr G)) (nlg (caddr G)))))))
  


;;; Print out the problem solutions inorder. 
(defun solcomp-print-solutions (Problem Stream)
  (let (Solution (Solutions (problem-solutions Problem)))
    (dotimes (N (length Solutions))
      (format Stream "~%~,,50,'-@a~% Solution ~a~3%" #\; N)
      (setq Solution (nth N Solutions)) 
      (solcomp-print-eqns (car Solution) Stream)
      (format Stream "~2%")
      (solcomp-print-entries 
       (cadr Solution) (problem-graph Problem) Stream))))
  

(defun do1 (Problem Sol)
  (let ((Solution (nth Sol (problem-solutions Problem))))
    (solcomp-print-eqns (car Solution) t)
    (solcomp-print-entries
     (cadr Solution) (problem-graph Problem) t)))


;;; Given a problem solution print the equations that are 
;;; in it in algrbraic form along with some descriptive
;;; text.
(defun solcomp-print-eqns (Eqns Stream)
  (let (Eqn (Spacer "       "))
    (dotimes (N (length Eqns))
      (setq Eqn (nth N Eqns))
      (format Stream "~a: ~a ~a ~%~a~a ~%~a~{~a ~}~2%" N
	      (solcomp-explicit-flagtest Eqn) (eqn-type Eqn)
	      Spacer (solcomp-nlg (eqn-Type Eqn) (eqn-exp Eqn)) 
	      Spacer (pre2In (eqn-Algebra Eqn))))))

;;; Given an equation determine if the equation is of a type 
;;; that should be "flagged" for explicitness.  That is, if 
;;; the equation is a major equation.  Note that this is 
;;; intended to mirror the tests that take place in the 
;;; function test-cache-eqn-entry@Testcode/tests.cl
;;; Hopefully this will stay consistent.
(defun solcomp-explicit-flagtest (Eqn)
  "Handle the explicitness flagtest."
  (cond 
   ;; Given equations will be flagged as such using a *G*
   ((equalp (eqn-type Eqn) 'Given-eqn) "*G*")
   ;; Major psm equations or equations tied with the 
   ;; major psmclass.
   ((and (equalp (eqn-type Eqn) 'Eqn)
	     (or (and (setq Class (lookup-Expression->psmclass (eqn-exp Eqn)))
		      (psmclass-major-p Class))
		 (and (setq Class (lookup-expression->equation (eqn-exp Eqn)))
		      (equation-major-p Class))))
    "*M*")
   
   ;; Return none if it doesn't fit.
   (t "   ")))

;;; Nlg the equation expression as necessary to produce an 
;;; equation description that the physicists can use.
(defun solcomp-nlg (Type Exp)
  (case Type
    (given-eqn 
     (format Nil "The assignment statement for the given value of ~a" (nlg Exp 'def-np)))
    (implicit-eqn 
     (format Nil "The assignment statement for the implicit value of ~a" (nlg Exp 'def-np)))
    (eqn (format Nil "The ~a EQN" (nlg Exp 'nlg-equation)))
    (t (format Nil "UNKNOWN TYPE: ~a" (nlg Exp 'def-np)))))



;;; Print out the entries that are necessary to complete the solution
;;; for now print this as a single flat list although that may change
;;; exclude all equation entries.
(defun solcomp-print-entries (Indicies Graph Stream)
  (let* ((Nodes (map-indicies->bgnodes Indicies Graph))
	 ;;(a (pprint Nodes))
	 (AllEnts (mappend #'bgnode-entries Nodes))
	 ;;(b (pprint AllEnts))
	 (Entries (remove-duplicates AllEnts))
	 (N 0) Prop)
    (dolist (E Entries)
      (setq Prop (systementry-prop E))
      (when (not (or (equalp (car Prop) 'Eqn)
		     (equalp (car Prop) 'Implicit-eqn)))
	(format Stream "~a: ~a~%   ~a~%" N (nlg Prop 'nlg-Entryprop) Prop)
	(incf N)))))
		
      
	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function was copied from the Pre2in file defined by Linwood 
;; Taylor in the help module.  I am going to modify it for our purposes
;; but I willl not copy it back unless necessary.  

;; Start here making modifications.

(defun pre2in (eq)
  (cond
   ((null eq) nil)
   ((atom eq) eq)
   ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
    (list (first eq) (pre2in (second eq))))
   ((and (member (first eq) '(- +)) (= (length eq) 2))
    (if (equal (first eq) '+)
	(pre2in (second eq))
      (list (first eq) (pre2in (second eq)))))
   ((member (first eq) '(dnum))
    (let ((s (subseq eq 1 (- (length eq) 1))))
      (if (first (last eq)) ; has non-NIL units
         (list (pre2in s) (first (last eq)))
      ; else dimensionless number
	(list (pre2in s))) ; maybe drop parens?
      ))
   ((third eq)
    (if (equal (first eq) '=)
	(let ((f (pre2in (second eq)))
	      (s (pre2in (third eq))))
	  (append (if (consp f) f (list f)) '(=) (if (consp s) s (list s))))
      (if (member (first eq) '(+ *))
	  (let ((lst (list (pre2in (second eq)))))
	    (dolist (obj (cddr eq))
	      (setf lst (append lst (list (first eq) (pre2in obj)))))
	    lst)
	(list (pre2in (second eq)) (first eq) (pre2in (third eq))))))
   ((second eq))
   (t (car eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ganked from Linn's SLog2English code.
;; formats paragraphs to block width characters wide

;; returns the location of the character after the end of word
(defun z-next-word (str)
  (cond
   ((equal str "") 0)
   ((position #\Space str) (position #\Space str))
   (t (length str))))

(defun l-format (the-str &optional (indent "   "))
  (declare (special **width**))
  (let ((frm "") (cnt 0) (str (string-trim " " the-str)))
    (do ((done Nil (equal (length (string-trim " " str)) 0)))
	(done (string-trim " " frm))
      (let ((tmp (z-next-word str)))
	(when (> tmp 0)
	  (if (>= (+ tmp cnt) **width**)
	      (setf frm (concatenate 'string frm (format nil "~A" #\LineFeed) indent
				     (string-trim " " (subseq str 0 tmp))))
	    (setf frm (concatenate 'string frm " " (string-trim " " (subseq str 0 tmp)))))
	  (setf str (string-trim " " (subseq str tmp)))
	  (if (>= (+ tmp cnt) **width**)
	      (setf cnt tmp)
	    (setf cnt (+ cnt tmp 1))))))))
