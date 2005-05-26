;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SolutionComponents.cl
;; Collin Lynch
;; 6/20/2003
;;
;; This file is used to generate the solution component
;; report from our current set of problems.  The report
;; generator code will take in a set of pretests, runtests
;; and postests, as well as an optional set of features
;; and an output filename.  The code will open the output
;; file if a filename is supplied and will then call all of
;; the pretests with the output file stream or the stream t.
;;
;; The code will then iterate over the set of problems that 
;; contain the relavent features and call the supplied 
;; runtests with the loaded problem and the output stream 
;; as arguments.  Once the run is completed the system will
;; then call the postests with the supplied stream.
;;
;; All of the tests are assumed to take an output stream as 
;; an argument and to use that stream for all its output.
;; The runtests are assumed to also take a loaded problem
;; structure as input.  



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


;;;; ============================================================
;;;; Main Loop.
;;;; The main report function takes in the sets of tests as well
;;;; as the optional outputfilename and the features.  At runtime
;;;; it will then iterate over the solutions and print out the 
;;;; relavent info as necessary.  
;;;;
;;;; The Pretests will be called once before the problems are loaded.
;;;; The runtests will be called once on each problem.  The postests
;;;; will be called once after the previous problem is completed.  
;;;; All of the tests are supplied in lists and will be called inorder.
;;;; Botht the pretests and postests are assumed to take a single 
;;;; output stream as argument (t for echoing to stdout).  The runtests
;;;; all take to arguments a problem and an output stream.  

(defun generate-solcomp-report (&key (Filename Nil) (Working Nil) (Features Nil) 
				     (Pretests Nil) (Runtests Nil) (Posttests Nil)
				     (Right-Margin 70))
  (declare (ignore Right-Margin))
  ;;(let ((**Width** 300))
  ;; Start by opening the supplied filename or designating the stream
  ;; to be t if the user wishes to make use of standard out for logging.
  (let ((Output 
	 (if (null FileName) t
	   (open Filename 
		 :Direction :Output
		 :If-Exists :Error
		 :If-Does-Not-Exist :Create))))
    (when (not Output)
      (error "Designated output stream ~a not valid." Filename))
    
    ;; Once the stream has been opened we will execute
    ;; all of the pretests and return the result vals.
    (dolist (Pretest Pretests)
      (funcall Pretest Output))
    
    ;; We then perform the "inner loop" handling each
    ;; individual problem with the runtests.  
    (solcomp-inner-loop Runtests Output Working Features)
    
    ;; Finally execute the postests with the output stream
    ;; supplied.
    (dolist (Posttest Posttests)
      (funcall Posttest Output))
    
    ;; Close the open stream if necessary and return.
    (when (not (equalp Output t))
      (close Output))))

;;; On each iteration of the problem we will load a problem if it meets
;;; the required specification and then call each of the runtests on it
;;; with the problem and the stream as input.  Before the tests are 
;;; executed the system will print the standard problem begin and end 
;;; brackets.  
(defun solcomp-inner-loop (Runtests Stream Working &optional (Features Nil))
  (map-problem-files
   #'(lambda (P) (solcomp-inner-loop-func P Runtests Stream))
   :Working Working
   :Features Features))

(defun solcomp-inner-loop-func (Problem Runtests Stream)
  "The actual call."
  (when (not (equalp (problem-name Problem) 'exkt20B))
    (solcomp-print-beginprob-bracket Problem Stream)
    (dolist (Test Runtests)
      (funcall Test Problem Stream))
    (solcomp-print-endprob-bracket Problem Stream)))

(defun solcomp-print-beginprob-bracket (Problem Stream)
  (format Stream "~,,35,'=@a BEGIN: ~a ~,,35,'=@a~2%" 
	  #\= (problem-name Problem) #\=))

(defun solcomp-print-endprob-bracket (Problem Stream)
  (format Stream "~,,35,'=@a END: ~a ~,,35,'=@a~% ~3%" 
	  #\= (problem-name Problem) #\=))

;;;; ===============================================================
;;;; Tests
;;;; The code in this section defines the tests that can be used 
;;;; for the producetion of solution reports.  I will label in 
;;;; comments which ones are acceptable pre and post tests.  At the
;;;; top of this section "standard groups" will be present.  


;;; ------------------------------------------------------------
;;; Setup problem
;;; This is a dummy "runtime-test" that is used to prepare many
;;; internal settings for later use.  It generates no output but
;;; will call sg-setup and set internal values such as the *cp*
;;; problem flag.
(defun runtest-setup-problem (Problem Stream)
  "Setup the problem for use below."
  (declare (ignore Stream))
  (sg-setup Problem)
  (setq *cp* Problem)
  (reset-runtime-testset-scores)
  ;;(pprint *sg-eqns*)
  ;;(pprint *sg-entries*)
  )



;;; ------------------------------------------------------------
;;; Print-Header.
;;; Print a breif description of the problem including the name
;;; and description to the stream.
(defun runtest-print-Header (Problem Stream)
  "Print out the problem description."
  (format Stream "Problem: ~a~%" (problem-name Problem))
  (format Stream "~a~2%" 
	  (apply #'concatenate 'string 
		 (mapcar #'(lambda (s) (format Nil "~a" s))
			 (problem-statement Problem)))))

;;; ------------------------------------------------------------
;;; Print Soughts and Givens
;;; Print out the soughts and givens for the problem to the stream.
(defun runtest-print-givens (Problem Stream)
  (format Stream "Givens: ~%")
  (dolist (G (problem-givens Problem))
    (case (car G)
      (Object (format Stream "   There exists an object: ~a~%" (nlg (cadr G))))
      (Time 
       (format Stream "   There is a time: ~a~%" 
	       (if (atom (cadr G))  ;; Just a little hack to deal w times.
		   (format Nil "T~a" (- (cadr G) 1))
		 (nlg (cadr G) 'nlg-time))))
      (Given (format Stream "   ~a = ~a~%" (nlg (cadr G)) (nlg (caddr G)))))))



;;; ---------------------------------------------------------------------
;;; first principles.
;;; Locate the valid first-principles for the problem and print them out.
;;; Later on this will be used to compute statistics.  
;;; NOTE:: That this makes use of NSH calls that are not "top-level" 
;;;   So these may change.  

;;; Setup data collection vars for the soughts and first principles 
;;; so that they can be analized using the posttest.
(defun  pretest-setup-soughts-and-first-principles (Stream)
  "Setuop the soughts and first principles as a pretest."
  (declare (ignore Stream))
  (setq **First-principle-path-record** Nil))


;;; NOTE:: This necessitate that the Pretest-soughts-and-first-principles
;;;   func is called at pretest time. 
(defun runtest-print-Soughts-and-first-principles (Problem Stream)
  (format Stream "Soughts:~%")
  (let (Sought Principles Dist FPRecord)
    (dolist (S (problem-soughts Problem))
      (setq FPRecord 
	(append FPRecord       
		(if (member 'no-quant (problem-features Problem) :test #'equalp)
		    (runtest-psafp-noquant Problem Stream S)
		  (runtest-psafp-quant Problem Stream S)))))
    (push (cons (problem-name Problem) FPRecord)
	  **First-principle-path-record**)))

;;; For no-quant problems we want to print out the appropriate information
;;; namely the nlg'd goal and then we want to reflect the non-quant state
;;; in the problem counts.  
(defun runtest-psafp-noquant (Problem Stream S)
  "Non-quant handler."
  ;;(format Stream "    ~a~%" (nlg S))
  (format Stream "      Non-Quantity Sought: ~a.~%" (nlg S 'goal))
  '((no-quant)))

;;; For quantity problems we want to select the desired information and 
;;; then display the path for the user as well as the type info.
(defun runtest-psafp-quant (Problem Stream S)
  "Handle the quantity problem."
  (not (member 'no-quant (problem-features Problem) :test #'equalp))
  (setq Sought (nsh-convert-response->quantity S))
  (if (null Sought) (format Stream "Unable to match sought ~a.~%" S)
    (progn 
      (format Stream "    ~a~%" (nlg S))
      (rt-psafp-handle-principles Sought Problem Stream))))
 

;;; When we are handling the first-0principle connections it is also 
;;; necessary to tally the number and type of connections being made
;;; this code will classify the connections by length and print 
;;; appropriate information for each one. 
;;;
;;; When run this code will compile a list of abbreviated records
;;; containing the type of each first-principle path link and will
;;; store that record in the **first-principle-path-record** var.
(defun rt-psafp-handle-principles (Sought Problem Stream)
  (let* ((FirstPrinciples (or (nsh-collect-first-principles Sought)
			      (nsh-collect-old-first-principles)))
	 (Paths (rt-psafp-cpaths Sought FirstPrinciples Problem))
	 (FPRecord (if FirstPrinciples Nil '((no-principles)))))
    ;;(if (null FirstPrinciples) (push 'no-principles FPRecord))
    ;;(pprint Paths)
    ;;(dolist (SoughtPaths Paths)
    (dolist (PrinciplePaths Paths)
      (dolist (Path PrinciplePaths)
	(push (list (rt-psafp-handle-path Path Stream) Path) FPRecord)))
    FPRecord))

;;; Once we have a shortest path(s) from the sought to a first principle
;;; then we need to determine the type of the given path and inform 
;;; the user appropriately.  In this case there are several potential
;;; cases:
;;;  Direct: in which the sought connects directly to the FP.
;;;  Indirect-connect:  In which the sought is connected to a
;;;    first-principle via a "connect" principle.
;;;  Indirect-simple:  In which the sought is connected to a
;;;    first-principle via a "simple" principle.
;;;  Indirect-major:  In which the sought is connected to a
;;;    first-principle via a "major" principle.
;;;  Indirect-other:  In which the sought is connected to a
;;;    first-principle via a "other" principle. (should not arise).
;;;  Indirect-else:  In which the sought is connected to a
;;;    first-principle via more than one principle.
;;;
;;; This code will detect the cases, print the info for the 
;;; report and return a symbol defining the case so that it 
;;; can be examined for later use.  
(defun rt-psafp-handle-path (Path Stream)
  "Handle the individual path."
  ;; Start by incrementing the number of paths handled.
  ;;(incf **first-principle-path-count**)
  (cond
   ;; Detect cases in which the code is directly connected.
   ;; In this case inform the user of that fact and print
   ;; the first principle.
   ((= 2 (length Path))
    ;;(incf **first-principle-direct-connect-count**)
    (format Stream "     Directly connected to: ~a~%" 
	    (nlg (enode-id (nth 1 Path)) 'nlg-equation))
    'Direct)
   
   ;; When the length is greater than two then we have several cases
   ;; either the system is connected by a connect, simple, or minor
   ;; principle and we want to handle each one.
   ((and (= 4 (length Path)) (rt-psaft-complexity-test (nth 1 Path) 'connect))
    ;;(incf **first-principle-indirect-connect-count**)
    (format Stream "     Indirectly linked via a connect principle:~%")
    (rt-psaft-nlg-lst Path "      " Stream)
    'indirect-connect)
  
   ((and (= 4 (length Path)) (rt-psaft-complexity-test (nth 1 Path) 'simple))
    ;;(incf **first-principle-indirect-simple-count**)
    (format Stream "     Indirectly linked via a simple principle:~%")
    (rt-psaft-nlg-lst Path "      " Stream)
    'indirect-simple)
   
   ((and (= 4 (length Path)) (rt-psaft-complexity-test (nth 1 Path) 'minor))
    ;;(incf **first-principle-indirect-minor-count**)
    (format Stream "     Indirectly linked via a minor principle:~%")
    (rt-psaft-nlg-lst Path "      " Stream)
    'indirect-minor)
   
   ((= 4 (length Path))
    ;;(incf **first-principle-indirect-other-count**)
    (format Stream "     Indirectly linked via an other principle:~%")
    (rt-psaft-nlg-lst Path "      " Stream)
    'indirect-other)
   
   ;; The "other" case for all else.
   (t 
    ;;(incf **first-principle-other-other-count**)
    (format Stream "     Indirectly linked via more than one other principle:~%")
    (rt-psaft-nlg-lst Path "      " Stream)
    'other-other)))

;;; Given an enode determine the complexity of the principle that it matches and
;;; return t if it matches the type supplied.
(defun rt-psaft-complexity-test (Node Complexity)
  (let ((Class (lookup-expression->psmclass (enode-id Node))))
    (when Class
      (if (equalp (psmclass-complexity Class) Complexity) t))))

;;; Nlg and print a path appropriately for use in the printout.  
(defun rt-psaft-nlg-lst (Path Indent Stream)
  "Print the list appropriately."
  (dolist (Node Path)
    (format Stream "~a~a~%" Indent 
	    (if (qnode-p Node) (nlg (qnode-exp Node) 'def-np)
	      (nlg (enode-id Node) 'nlg-equation)))))
		      

;;; When collecting the paths we are only interested in the shortest 
;;; path from the sought to each first principle.  This code will
;;; collect the paths and then select the shortest path(s) from the
;;; sought to each principle.  
(defun rt-psafp-cpaths (Sought FirstPrinciples Problem)
  "Collect the shortest path(s) to the first principles from the sought."
  (mapcar 
   #'(lambda (F) 
       (collect-smallest
	 (collect-bubblegraph-paths Sought F (problem-graph Problem))))
   FirstPrinciples))




;;; First-principle-posttest
;;; Once we have iterated over the problems and collected the path record info
;;; it is necessary to process this data in order to determine the percentage of
;;; problems in which one case or another is present.  In this case what we are 
;;; searching for are the set of problems in which the connection between first 
;;; principles and soughts is only direct, only indirect, etc.  
(defun posttest-process-and-display-sought-and-first-principle-data (Stream)
  "Display the sought and first-principle data."
  (let (Dup Tmp Tmp2 (Totalprob 0) (totalpath 0)
	(probcount ()) (pathcount ()))
    (dolist (Rec **First-Principle-Path-Record**)
      ;;(pprint (car Rec))
      (incf TotalProb)
      (setq TotalPath (+ TotalPath (length (cdr Rec))))
      (setq Dup (remove-duplicates (cdr Rec) :key #'car :test #'equalp))
      ;; For each individual path we want to iterate over the list
      ;; and increment the appropriate types for later use.  
      (dolist (Type (cdr Rec))
	;;(pprint Type)
	(setq Tmp (assoc (car Type) PathCount :test #'equalp))
	(if Tmp (incf (nth 1 Tmp))
	  (push (list (car Type) 1) PathCount)))
      
      ;; For each problem given the dup we need to store the results 
      ;; incrementing the associated value by type.  For now we are
      ;; only recording the problem counts and so we will deal with 
      ;; those.
      (setq Tmp2 (mapcar #'car Dup))
      ;;(pprint tmp2)
      (setq Tmp (assoc Tmp2 ProbCount :test #'equalp))
      (if Tmp (incf (nth 1 Tmp))
	(push (list Tmp2 1) ProbCount)))

    ;; Once we have counted the vars display them to the user.
    (posttest-pdsfp-display Stream TotalProb ProbCount TotalPath PathCount)
    ;;(format Stream "~%~{~a~%~}" **First-Principle-Path-Record**)))
    ))

;;; Once we have obtained the counts necessary to run this test then we
;;; want to iterate over the results and display them to the user.
;;; This will provide more formatted printing later.
(defun posttest-pdsfp-display (Stream TotalProb ProbCount TotalPath PathCount)
  "Display the results to the user."
  (format Stream "Number of Problems: ~a~%" TotalProb)
  (dolist (P Probcount)
    (format Stream "    ~a~{, ~a~}:   ~a~%" (caar P) (cdar P) (cadr P)))
  (format Stream "Number of Paths: ~a~%" TotalPath)
  (dolist (P Pathcount)
    (format Stream "    ~a:   ~a~%" (car P) (cadr P))))
  
  
  



      
      

;;; ----------------------------------------------
;;; Print solution values.
;;; Print out scoring values for the problem including
;;; the test cache values.

(defun runtest-printvals (Problem Solution)
  "Print out the runtime testset info."
  (reset-runtime-testsets)
  (pprint *test-cache-eqn-entries*)
  (pprint *test-cache-given-eqn-entries*)
  (pprint *test-cache-axis-entries*))


;;;; ==========================================================
;;;; Solution Tests.
;;;; The code in this section is used to handle solution tests.
;;;; The first function in this is a wrapper loop that will 
;;;; iterate over the solutions of the current problem and will
;;;; call the supplied solution-tests like runtests on each problem
;;;; In essence this function is a modification of the primary 
;;;; loop specialized for problem solutions.

;;; Call the supplied pretests before iterating over the supplied
;;; solutions and calling the postests.  Then return the results.
;;; Each test takes a problem, a problem-solution, and a set of 

(defun runtest-handle-solutions (Problem Stream
				 &key (PreTests Nil) 
				      (Runtests Nil)
				      (Posttests Nil))
  "Handle the saved solutions for the current problem."
  (let (Solution (Solutions (problem-solutions Problem)))
    (format Stream "Starting Solutions.~%")
    (dolist (Pre Pretests)
      (funcall Pre Problem Stream))
    
    (dotimes (N (length Solutions))
      (format Stream "~,,50,'-@a~% Solution ~a~3%" #\; N)
      (dolist (Run Runtests)
	(funcall Run Problem (nth N Solutions) Stream))
      (format Stream "~2%"))
    
    (dolist (Post Posttests)
      (funcall Post Problem Stream))))



;;; -----------------------------------------------------
;;; Print Solution Equations.
;;; For a given solution print out the equations within
;;; it in algebraic form along with some description of 
;;; their text.

(defun solution-runtest-print-equations (Problem Solution Stream)
  (declare (ignore Problem))
  (let (Eqn Entry (Eqns (car Solution)) (Spacer "       "))
    (dotimes (N (length Eqns))
      ;;(pprint (nth N Eqns))
      (setq Eqn (nth N Eqns))
      (setq Entry (sg-find-eqn->entry (eqn-algebra Eqn) *Sg-Entries*))
      ;;(pprint Entry)
      (format Stream "~a: ~a ~a ~%~a~a ~%~a~{~a ~}~2%" N
	      (solcomp-explicit-flagtest Eqn Entry) (eqn-type Eqn)
	      Spacer (solcomp-nlg (eqn-Type Eqn) (eqn-exp Eqn)) 
	      Spacer (pre2In (eqn-Algebra Eqn))))))

;;; Given an equation determine if the equation is of a type 
;;; that should be "flagged" for explicitness.  That is, if 
;;; the equation is a major equation.  Note that this is 
;;; intended to mirror the tests that take place in the 
;;; function test-cache-eqn-entry@Testcode/tests.cl
;;; Hopefully this will stay consistent.
(defun solcomp-explicit-flagtest (Eqn Entry)
  "Handle the explicitness flagtest."
  (cond 
   ;; Given equations will be flagged as such using a *G*
   ((equalp (eqn-type Eqn) 'Given-eqn) "*G*")
   ;; Major psm equations or equations tied with the 
   ;; major psmclass.
   ((and (equalp (eqn-type Eqn) 'Eqn)
	 ;;(not (trivial-syseqn-p Entry))
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





;;; ------------------------------------------------------
;;; Print all the entries in a problem solution out to 
;;; the user.  For now this is printed as a single flat
;;; list although this may be changed to exclude equation
;;; entries later.
(defun solution-runtest-print-entries (Problem Solution Stream)
  (let* ((Indicies (cadr Solution))
	 (Graph (Problem-Graph Problem))
	 (Nodes (map-indicies->bgnodes Indicies Graph))
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



;;;; ================================================================
;;;; Wrapper Code.
;;;; The functions in this section exist as wrappers for the testing 
;;;; code above to be used and modified as necessary.

(defun generate-typical-report (&key (Filename Nil) (Working Nil) (Features Nil))
  "Generate the \"typical\" Report."
  (generate-solcomp-report
   :Filename Filename
   :Working Working
   :Features Features
   :PRetests (list #'pretest-setup-soughts-and-first-principles )
   :Runtests (list #'runtest-setup-problem
		   #'(lambda (P S) (format S "~%"))
		   #'runtest-print-header
		   #'(lambda (P S) (format S "~%"))
		   #'runtest-print-soughts-and-first-principles
		   #'(lambda (P S) (format S "~%"))
		   #'runtest-print-givens
		   #'(lambda (P S)
		       (runtest-handle-solutions 
			P S :Runtests (list #'solution-runtest-print-equations
					    #'solution-runtest-print-entries))))
   :Posttests (list #'posttest-process-and-display-sought-and-first-principle-data)))


(defun tstfoo (Problem)
  (solcomp-inner-loop-func 
   Problem
   (list #'runtest-setup-problem
	 #'(lambda (P S) (format S "~%"))
	 #'runtest-print-header
	 #'(lambda (P S) (format S "~%"))
	 #'runtest-print-soughts-and-first-principles
	 #'(lambda (P S) (format S "~%"))
	 #'runtest-print-givens
	 #'(lambda (P S)
	     (runtest-handle-solutions 
	      P S :Runtests (list #'solution-runtest-print-equations
				  #'solution-runtest-print-entries))))
   t))
		   





(defun do1 (Problem Sol)
  (let ((Solution (nth Sol (problem-solutions Problem))))
    (solcomp-print-eqns (car Solution) t)
    (solcomp-print-entries
     (cadr Solution) (problem-graph Problem) t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function was copied from the Pre2in file defined by Linwood 
;; Taylor in the help module.  I am going to modify it for our purposes
;; but I willl not copy it back unless necessary.  

;; Start here making modifications.

(defun pre2in (eq)
  (cond
   ((null eq)
    (Tell :pre2in "null -> ~W" eq)
    nil)
   ((atom eq)
    (Tell :pre2in "atom -> ~W" eq)
    eq)
   ((member (first eq) '(sin cos tan abs ln log10 sqrt exp))
    (Tell :pre2in "function -> ~W" eq)
    (list (first eq) (pre2in (second eq))))
   ((and (member (first eq) '(+)) (null (cdr eq)))
     (Tell :pre2in "0-element sum -> ~W" eq)
     0)   ; just turn empty sum into a zero - AW
   ((and (member (first eq) '(- +)) (= (length eq) 2))
    (Tell :pre2in "unary -> ~W" eq)
    (if (equal (first eq) '+)
	(pre2in (second eq))
      (list (first eq) (pre2in (second eq)))))
   ((member (first eq) '(dnum))
    (Tell :pre2in "dnum -> ~W" eq)
    (let ((s (subseq eq 1 (- (length eq) 1))))
      (if (first (last eq)) ; has non-NIL units
         (list (pre2in s) (first (last eq)))
      ; else dimensionless number
	(list (pre2in s))) ; maybe drop parens?
      ))
   ((third eq)
    (Tell :pre2in "other -> ~W" eq)
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
   ((second eq)
    (Tell :test "Error in pre2in"))
   (t (car eq))))

#|
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
|#

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
