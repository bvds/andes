;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem.cl
;; Collin Lynch
;; 12/8/2000
;;
;; The Problem struct is the core solution structure for the Andes
;; system.  It is defined in the initial problem database.  From
;; there it will be loaded and stored for later use in the .prb
;; files.  These files will then be loaded at runtime for solution
;; refrencing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters
;; Structure

;;=================================================================
;; Parameters

(defun Default-ProblemFile-Path ()
    ;; trailing / makes it a directory
    (merge-pathnames  "Problems/" *Andes-Path*))

(defparameter *Number-Of-Problems* 0 
  "The number of problems registered to the system.")
(defparameter *Problem-Registry* (make-hash-table) 
  "A list of all the currently known problem structs.")
(defparameter **read-old-problemfile-header** nil 
  "Read in oldstyle file headers. (sans forbidden and ignore).")

;;=================================================================
;; Problem structure.
;; The problem structure defines an individsual problem including 
;; specification, soughts, givens, etc.  It is stored by name at 
;; solver time in a problem database and can be saved and reloaded 
;; as needed but it must be done in a specified order.  

(defstruct (Problem (:print-function Print-Problem))
	    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; predefined elements from the defproblem
  ;; These fields are set in the initial problem-definition.
  
  Name            ;; Symbolic version of the problem name e.g newt1c.
  Statement       ;; Text giving the problem statement.  This should be
                  ;; Identical to what the student will see at runtime.
  ; following information is needed to display problem on workbench interface
  ; can be omitted if a .fbd file has been created for the problem
  Times		  ;; time legend as list of (time point, description) pairs. Ex:
                  ;; Ex: ((1 "car starts moving") (2 "car hits driveway"))
  Choices	  ;; list of choice category members for menus. Ex:
                  ;; ((bodies (car driveway wall Earth))
		  ;;  (branches (Br1 Br2 Br3)))
  Graphic         ;; name of file containing problem graphic 
                  ;; end workbench information
  Comments        ;; String commenting on the problem for developers.
  Features        ;; An optional list of atomic markings such as 
                  ;; 'Working' that will be queried as needed.
  Soughts         ;; The problem's sought quantities in KB form.
  Givens          ;; The given quantities for this problem in KB form.
  
  ForbiddenPSMS   ;; A list of psmclass and psmgroup names that cannot be
                  ;; used within this problem and will raise an error if tried. 
  
  IgnorePSMS      ;; A list of psmclass and psmgroup names that will not
                  ;; be used by the problem solver when it attempts to 
                  ;; solve this problem.
  
  VariableMarks   ;; A filed containing variable-specific marks that will be used
                  ;; to set restrictions or other variable-specific information on
                  ;; a problem specific basis.  When Variable restrictions are 
                  ;; looked up then they will also be taken from here.
                  ;; This should be a list of lists of the form:
                  ;;  (<Variable-Proposition> <Marks>)
                  ;; The Marks themselves should be atoms.
  
  ModDate	  ;; Modification date.
  Version	  ;; Problem version.
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; elements below this point are set by solve-problem
  
  Graph              ;;Storage for the bubblegraph. 
  VarIndex           ;;List of variable-values indicating solution point.
  EqnIndex           ;;Index of equations in the system.
  Solutions          ;; Set of solution bubbles. The first of these will always be the best.
  wm                 ;; Collection of the solver working memory.
  )

;;-----------------------------------------------------------------------------
;; Problem output

;; Print out the full problem.  
(defun print-problem (Problem &optional (Stream t) (Level 0))
  "Print out the specified problem to the specified stream at the specified level."
  (pprint-Indent :block Level Stream)
  (format Stream "Problem: ~W ~A ~A~%" 
	  (Problem-name Problem) (Problem-Version Problem) (Problem-ModDate Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Statement:      ~W~%" (Problem-statement Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Times:          ~W~%" (Problem-Times Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Choices:        ~W~%" (Problem-Choices Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Graphic:        ~W~%" (Problem-Graphic Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Comments:       ~W~%" (Problem-Comments Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Features:       ~W~%" (Problem-Features Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Sought:         ~W~%" (Problem-Soughts Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Givens:         ~W~%" (Problem-Givens Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  ForbiddenPSMS:  ~W~%" (Problem-ForbiddenPSMS Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  IgnorePSMS:     ~W~%" (Problem-IgnorePSMS Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  VariableMarks:     ~W~%" (Problem-VariableMarks Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Graph: ~W~%" (Problem-Graph Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Vars: ~A~%" (Problem-VarIndex Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Eqns: ~A~%" (Problem-EqnIndex Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Solutions: ~A~%" (Problem-Solutions Problem))
  (pprint-Indent :block Level Stream)
  (format Stream "  Working Mem: ~A~%" (Problem-wm Problem)))

;; Print out strictly the problem's descriptive info.
(defun print-problem-text (Problem &optional (Stream t))
  "Print the supplied problem text to the associated Stream."
  
  (format Stream "Problem: ~W ~A ~A~%" 
	  (Problem-name Problem) (Problem-Version Problem) (Problem-ModDate Problem))
  (format Stream "Statement: ")
  (format-string-list (Problem-Statement Problem) Stream)
  (format Stream "Comments: ")
  (format-string-list (Problem-Comments Problem) Stream)
  (format Stream "~%"))


;; Print out the bubblegraph from the problem
(defun Print-problem-bubblegraph (Problem &optional (form 'graph))
  "Print out the bubblegraph associated with a problem in the specified form."
  (print-bubblegraph (Problem-Graph Problem) t 0 form))


;;-----------------------------------------------------------------------------
;; Write-Problem-File
;; Writing a problem file is a simple one-pass loop thorough the struct itself.
;; The system generates a new file or overwrites the old one and then stores 
;; each independent field in turn by writing the field name followed by the 
;; field contents.  This facilitates human reading of the problem file and also
;; aids in error detection.  Future versions of a problem can add new fields 
;; without invalidating the storage code for the old ones.  
;;
;; Most field contents are stpored using a simple format call.  Some more 
;; complex fields such as the graph are written using specialized functions.  
;; In all cases the overall format is unchanged.  

(defun write-problem-file (Problem &key (Name nil) (Path (Default-Problemfile-Path))) 
  "Store the solved problem file and indicies to a file."
  (let ((Filename 
	 (if Name 
	     (problem-filename Name Path)
	   (problem-filename (string (Problem-Name Problem)) Path))))
    (ensure-directories-exist Filename)
    (wopen-scwrite 
     File Filename
     (print-mreadable-problem Problem File))))


(defun print-mreadable-problem (Problem Stream)
  "Print a Machine readable problem to the specified stream."
  (format Stream "<Andes2Problem>~%")
  (format Stream "Name          ~W~%" (problem-Name Problem))
  (format Stream "Version       ~W~%" (problem-Version Problem))
  (format Stream "Moddate       ~W~%" (problem-Moddate Problem))
  (format Stream "Statement     ~W~%" (problem-Statement Problem))
  (format Stream "Times         ~W~%" (problem-Times Problem))
  (format Stream "Choices       ~W~%" (problem-Choices Problem))
  (format Stream "Graphic       ~W~%" (problem-Graphic Problem))
  (format Stream "Comments      ~W~%" (problem-Comments Problem))
  (format Stream "Features      ~W~%" (problem-features Problem))
  (format Stream "Soughts       ~W~%" (problem-Soughts Problem))
  (format Stream "Givens        ~W~%" (problem-Givens Problem))
  (format Stream "ForbiddenPSMS ~W~%" (problem-ForbiddenPSMS Problem))
  (format Stream "IgnorePSMS    ~W~%" (problem-IgnorePSMS Problem))
  (format Stream "VariableMarks ~W~%" (problem-VariableMarks Problem))
  (format Stream "WorkingMemory ~W~%" (Problem-wm Problem))
  (format Stream "Bubblegraph   ") (print-mreadable-bubblegraph (Problem-Graph Problem) Stream)
  (format Stream "~%EqnIndex    ") (print-mreadable-eqns (Problem-EqnIndex Problem) Stream)
  (format Stream "~%VarIndex    ") (print-mreadable-qvars (Problem-VarIndex Problem) Stream)
  (format Stream "~%Solutions   ") (print-mreadable-EqnSets (problem-solutions Problem) Stream)
  (format Stream "</Andes2Problem>~%"))


(defun problem-filename (Pfname Path)
  "Generate the problem file name."
  (merge-pathnames (pathname (strcat pfname ".prb"))
		   Path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solution printing.

(defun print-solved-problem-vars (Problem)
  "Print all of the qvars in the problem with values."
  (loop for V in (Problem-VarIndex Problem)
      when (Qvar-Value V)
      do (format t "~A~%" V)))


(defun print-unsolved-problem-vars (Problem)
  "Print all of the qvars in the problem with values."
  (loop for V in (Problem-VarIndex Problem)
      when (null (Qvar-Value V))
      do (format t "~A~%" V)))


(defun print-unsolved-problem-eqns (Problem)
  "Print all of the qvars in the problem with values."
  (loop for E in (cadr (Problem-EqnIndex Problem))
      unless (Eqn-Solved E)
      do (format t "~A~%" E)))


;;===================================================================
;; read a problem file.
;; Reading in an Mreadable problem file is a process of iterating over 
;; the tagged elements in the file itself.  When a file is stored each 
;; field in the struct is stored as appropriate in the form <Name> <value>
;; when the file is read in the system will key up on these forms and 
;; restore them one at a time.  This format facilitates the modification
;; of the graph by adding new fields.  If the system does not find a new
;; field in old files it does not cause an error, nor is the order of
;; the file contents constrained by the system in any way.
;;
;; The initial read macros below handle input for the problem file with
;; appropriate errors.  They are located above the function for compiling
;; ease.  
;;
;; The main function read-problem-file opens the file stream and then calls
;; the iterative read-pfile-contents function which loads the individual 
;; fields until the terminating line is reached.  
;;
;; The read-pfile contents function is passed a field-name and then calls 
;; the appropriate read function to read and store it as necessary.  
;;
;; Once the system has finished reading in the fields it will then regenerate
;; the links within the file.  This includes regenerating links within the 
;; bubblegraphs and indicies.  

(defmacro mpf-read (S C)
  `(when (not (equalp (read ,S) ,C))
     (error "Malformed Problem file.")))
     
(defmacro mpf-readset (S L)
  `(setf ,L ,`(read ,S "Error: malformed problem file.~%")))

(defmacro mpf-readret (S)
  `(read ,S "Error: invalid problem file."))

(defun read-problem-file (Name &key (Path (Default-ProblemFile-Path)) 
				    (Read-old-format nil))
  "Read the specified problem file into a new Problem Struct."
  (declare (ignore read-old-format))
  (let ((Filename (problem-filename Name Path)) (Problem (make-Problem)))
    (when (probe-file (namestring FileName))
      (with-open-file (File 
		       (namestring Filename)
		       :direction :input
		       :if-does-not-exist :error)
	 
	(mpf-read File '<Andes2Problem>) ;Read the file ID tag.
	;; Read the contents until the close tag is found.
	(read-pfile-contents File Problem) 
	;; When the files are stored numerical indicies 
	;; are used for qvars in the BGNODES.  This call
	;; Regenerates those links for later use.
	(when (problem-varindex Problem)  
	  (regen-bg-vindex-links (Problem-Graph Problem)     
				 (Problem-VarIndex Problem)))  
	;; When the files are stored numerical indicies
	;; are used for eqns in the BGNODES.  This call	
	;; Regenerates those links for later use. 
	(when (problem-Eqnindex Problem)                       
	  (regen-bg-eindex-links (Problem-Graph Problem)      
				 (Problem-EqnIndex Problem))) 
	Problem))))                                         

;;; Return the problem struct.
;;; Regenerate the links between elements within
(defun read-pfile-contents (S P)
  "Read the headere contents of the Problem File."
  (let ((name (mpf-readret S)))
    (when (not (equal name '</Andes2Problem>))
      (set-pfile-contents name S P)
      (read-pfile-contents S P))))

(defun set-pfile-contents (name Stream Problem)
  "Store the problemfile header contents."
  (case name
    (Name          (setf (Problem-Name Problem) (mpf-readret Stream)))
    (version       (setf (Problem-version Problem) (mpf-readret Stream)))
    (Moddate       (setf (Problem-Moddate Problem) (mpf-readret Stream)))
    (statement     (setf (Problem-statement Problem) (mpf-readret Stream)))
    (Times         (setf (Problem-Times Problem) (mpf-readret Stream)))
    (Choices       (setf (Problem-Choices Problem) (mpf-readret Stream)))
    (Graphic       (setf (Problem-Graphic Problem) (mpf-readret Stream)))
    (comments      (setf (Problem-comments Problem) (mpf-readret Stream)))
    (features      (setf (Problem-features Problem) (mpf-readret Stream)))
    (soughts       (setf (Problem-soughts Problem) (mpf-readret Stream)))
    (givens        (setf (Problem-givens Problem) (mpf-readret Stream)))
    (ForbiddenPSMS (setf (Problem-forbiddenPSMS Problem) (mpf-readret Stream)))
    (IgnorePSMS    (setf (Problem-IgnorePSMS Problem) (mpf-readret Stream)))
    (VariableMarks (setf (Problem-VariableMarks Problem) (mpf-readret Stream)))
    (WorkingMemory (setf (Problem-wm Problem) (mpf-readret Stream)))
    
    (Bubblegraph (setf (problem-Graph Problem) (read-mreadable-bubblegraph Stream)))
    (EqnIndex (setf (Problem-EqnIndex Problem) (read-mreadable-eqns Stream (Problem-Graph Problem))))
    (VarIndex (setf (Problem-VarIndex Problem) (read-mreadable-qvars Stream (Problem-Graph Problem))))
    (Solutions (setf (Problem-Solutions Problem) (read-mreadable-EqnSets Stream (Problem-EqnIndex Problem))))

    (t (error "Undefined Problem-header tag. ~A" name))))



(defun problem-file-exists (Name &optional (Path (Default-ProblemFile-Path)))
  (probe-file (namestring (problem-filename (format nil "~A" Name) Path))))




			  
(defun trace-readpfile ()
  (trace read-problem-file
	 read-pfile-contents
	 set-pfile-header-contents
	 read-mreadable-bubblegraph
	 read-mreadable-qnode-lst
	 read-mreadable-qnode
	 read-mreadable-enode-lst
	 read-mreadable-Enode
	 read-mreadable-eqns
	 read-mreadable-qvars
	 read-mreadable-eqnsets))




;;===================================================================
;; Problem Storage.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defproblem (public)
;; Defines a problem struct for use by the system and registers it with
;; *Known-Problems*
;;
;; Arguments:
;;  Name:  The problem name as a symbol.
;;
;;  &key
;;    Sought:  The problem's sought valeue(s) represented as goals for the solver.
;;    Givens:  Given values alias Known values or initial working memory for the solver.
;;    InitBindings: The initial bindings for the problem stored in the initial state.
;;
;;    Statement:  A string giving the problem statement.
;;    Comments:   Developer comments (string) for the problem.
;;
;; Returns: The problem struct.

(defmacro defproblem (name &key (soughts ()) (Givens ())
				(Statement "")
				(Comments NIL) (Features nil)
				(ModDate nil) (Version nil) 
				(ForbiddenPSMS nil) (IgnorePSMS nil)
				(VariableMarks Nil) (Times nil)
				(Choices nil) (Graphic nil))
  "Define a problem struct and store it."
  (let ((Prob (eval `(make-problem :Name ',name                     ;;Define the problem struct.
				   :soughts ',(remove-answer
					       (force-to-ll soughts))
				   :Givens ',(force-to-ll Givens)   
				   :ForbiddenPSMS ',(force-to-list ForbiddenPSMS)
				   :IgnorePSMS ',(force-to-list IgnorePSMS)
				   :VariableMarks ',VariableMarks
				   :Statement ',(force-to-list Statement)
				   :Comments ',(force-to-list Comments)
				   :Features ',Features
				   :ModDate ',ModDate
				   :Version ',Version
				   :Times   ',Times
				   :Choices ',Choices
				   :Graphic ',Graphic))))
    (register-problem Prob)                                          ;;Store the problem for access.
    Prob))                                                           ;;Return the problem.


(defun remove-answer (S)
  "Backwards compatability."
  (if (not (listp S)) S
    (mapcar #'(lambda (a) 
		(if (and (listp s) (eq (car a) 'answer))
		    (cadr a)
		  a))
	    S)))
						
(defun register-problem (P)
  "Store problem P in the *Problem-Registry* Hashtable."
  (setf (gethash (Problem-Name P) *Problem-Registry*) P))


(defun clear-Problem-Registry ()
  "Clear out the problem registry."
  (format t "clearing *problem-registry*")
  (clrhash *Problem-Registry*))



;;------------------------------------------------------------------
;; problem access.

(defmacro gp (P)
  "Get the named problem from the *Problem-Registry* Hashtable."
  `(gethash ',P *Problem-Registry*))


(defun get-problem (P)
  "Get the named problem from the *Problem-Registry* Hashtable."
  (gethash P *Problem-Registry*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; map-problems
;; Map the specified function onto each registered problem in turn.
;; this is used primarily for dump-problems.
;;
;; non-null filter-fn is a predicate of a problem used to filter set 
;; of problems applied to.  Default is null filter for all problems.
;; working-problem-p is filter for problems tagged 'working only
(defun map-problems (Function &optional (filter-fn NIL))
  "Map the specified function onto each problem struct in turn."
  (maphash #'(lambda (Name Problem)
	       (declare (ignore Name))
	       (when (or (null filter-fn)
			 (funcall filter-fn Problem))
		 (funcall Function Problem)))
	   *Problem-Registry*))


;; Load the problem files as stored on the disk in the default
;; problemfile path loading each (if present) and calling the 
;; specified function on the struct.
(defun map-problem-files (Function &key (working t) (features Nil) (old-format nil))
  "Map the specified function onto each problem file in turn."
  (let (P R)
    (maphash #'(lambda (Name Problem)
		 (when (and (or (null Working) (working-problem-p Problem))
			    (or (null Features) (problem-has-features-p Problem Features))
			    (problem-file-exists (format nil "~A" Name)))
		   (format t "Mapping File: ~A~%" Name)
		   (handler-case (progn 
				   (setq P (read-problem-file (format nil "~A" Name) 
							      :Read-Old-Format old-format))
				   (push (funcall Function P) R))
		     (error (E) (format t "~A~%" E)))))
	     *Problem-Registry*)
    R))

(defun collect-func-problems (Function)
  "Collect all problems for which Function returns t."
  (let ((Storage))
    (maphash #'(lambda (name Problem)
		 (declare (ignore Name))
		 (pprint Problem)
		 (when (funcall Function Problem)
		   (push Problem Storage)))
	     *Problem-Registry*)
    Storage))

(defun working-problem-p (problem)
  "Test if problem is tagged as working"
   (member 'working (problem-features problem)))


(defun seeks-quantities-p (Problem)
  "Return t iff the problem seeks quantity values."
  (loop for Sought in (Problem-Soughts Problem)
      when (remove-answer (Quantity-expression-p Sought))
      return t))
  

(defun no-quant-problem-p (problem)
  "Return t iff the problem is a no-quant problem."
  (member 'no-quant (problem-features problem)))


(defun multi-sought-problem-p (Problem)
  "Does the problem seek more than 1 sought?"
  (< 1 (length (problem-soughts Problem))))


(defun single-sought-problem-p (Problem)
  "Does the problem have only 1 sought?"
  (= 1 (length (problem-soughts Problem))))

(defun problem-has-feature-p (Problem Feature)
  "Return t if the specified problem has the specified feature."
  (member Feature (problem-features Problem)))

(defun problem-has-features-p (Problem Features)
  "Return t if the problem has all the specified features."
  (when (null (remove-if 
	       #'(lambda (F) (member F (problem-features Problem) 
				     :test #'equalp))
	       Features))
    t))



;;;--------------------------------------------------------
;;; Rough-grain comparision of two solutions to a problem
;;;
(defun diff-problem-solutions (P1 P2)
  "Run a diff between the problem solutions."
  (cond
   ((or (null P1) (null P2)) 
    (format nil "Problem file missing."))
   ((not (= (length (Problem-Solutions P1)) (length (Problem-Solutions P2))))
    (format nil "Solution lengths ~A and ~A" 
	    (length (Problem-Solutions P1)) (length (Problem-Solutions P2))))
   ((not (= (length (Problem-EqnIndex P1)) (length (Problem-EqnIndex P2))))
    (format nil "EqnIndex lengths ~A and ~A" 
	    (length (Problem-EqnIndex P1)) (length (Problem-EqnIndex P2))))
   ((not (= (length (Problem-VarIndex P1)) (length (Problem-VarIndex P2))))
    (format nil "VarIndex lengths ~A and ~A" 
	    (length (Problem-VarIndex P1)) (length (Problem-VarIndex P2))))
   ((not (= (length (Problem-Graph P1)) (length (Problem-Graph P2))))
    (format nil "Bubblegraph lengths ~A and ~A" 
	    (length (Problem-Graph P1)) (length (Problem-Graph P2))))
  ; ((diff-var-Indicies (Problem-VarIndex P1) (Problem-Varindex P2))
  ;  (format nil "var indicies ~A" 
;	    (diff-var-Indicies (Problem-VarIndex P1) (Problem-Varindex P2))))
  ; (Eqn-Indicies-equalp (Problem-EqnIndex P1) (Problem-EqnIndex P2))
  ; (set-diff-bubblegraphs (Problem-Graph P1) (Problem-Graph P2))
   (t nil)))


;;;-----------------------------------------------------------------
;;; Problem-queries.
;;; For reporting purposes these functions can query specified info
;;; from the problem struct and return it as necessary.

;;; collect-kcd-uses 
;;; collect the kcds being used by each problem 0
;;; according to the problem name.
(defun collect-kcd-uses (&optional (stream t))
  "Collect the kcds being used by each problem."
  (let ((l (map-problem-files
	   #'(lambda (P) (cons (problem-Name P)
			       (collect-problem-kcds P))))))
    (dolist (lp l)
      (format stream "~A~%" (car lp))
      (dolist (lpp (cdr lp))
	(format stream "  ~A~%" lpp))
      (format stream "~2%"))))
   
   

;;; collect-problem-kcds
;;; collect all of the kcds that might be hinted inside the
;;; problem itself and return them in a list form.
(defun collect-problem-kcds (Problem)
  (remove-duplicates
   (append 
    (mapcan-bubblegraph-qnodes
     #'(lambda (n) (map-optags->minilessons 
		    (collect-psmgraph-optags
		     (qnode-path n))))
     (Problem-Graph Problem))
    (mapcan-bubblegraph-enodes
     #'(lambda (n) (map-optags->minilessons
		    (collect-psmgraph-optags
		     (enode-path n))))
    (problem-graph Problem)))))

;;; collect-problem-minilessons
;;; collect all of the minilessons that might be hinted 
;;; inside the problem itself and return them in a list form.
(defun collect-problem-minilessons (Problem)
  (remove-duplicates
   (append 
    (mapcan-bubblegraph-qnodes
     #'(lambda (n) (map-optags->minilessons 
		    (collect-psmgraph-optags
		     (qnode-path n))))
     (Problem-Graph Problem))
    (mapcan-bubblegraph-enodes
     #'(lambda (n) (map-optags->minilessons
		    (collect-psmgraph-optags
		     (enode-path n))))
    (problem-graph Problem)))))
	  

#|  This is disused at present since noone is using the flag.
;;; Problem testing
;;; Assorted utility functions for help code.

(defun multi-axis-problemp (Problem)
  "Is the problem a multi-axis problem?"
  (member 'multi-axis-problem 
	  (problem-features Problem)))

(defun single-axis-problemp (Problem)
  "Is the problem a multi-axis problem?"
  (not (member 'multi-axis-problem 
	       (problem-features Problem))))

|#
