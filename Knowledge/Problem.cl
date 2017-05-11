;; Problem.cl
;; Collin Lynch
;; 12/8/2000
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The Problem struct is the core solution structure for the Andes
;; system.  It is defined in the initial problem database.  From
;; there it will be loaded and stored for later use in the .prb
;; files.  These files will then be loaded at runtime for solution
;; refrencing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters
;; Structure

;;=================================================================
;; Parameters

(defun Default-ProblemFile-Path ()
    ;; trailing / makes it a directory
    (merge-pathnames  "solutions/" *Andes-Path*))

(defvar *Problem-Registry* (make-hash-table) 
  "A list of all the currently known problem structs.")

; We use the version slot in the external prb file to store
; a version number for the external file format used, in case 
; problem reading code has to conditionalize on version. Note this is 
; not a version number for the problem.  
; The latest version is assigned by default in defproblem so will be 
; included on writing out the prb. The only time a problem will not carry the 
; latest version number is if it is read in from an old file.
; Only file reading code should be sensitive to the file version; file 
; writing code should just always write out using the latest format.  
; NB: old files just used NIL.
(defparameter *latest-file-version* 2)


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
  English         ;; Ontology information specific to this problem
                  ;; format is an alist of pattern/model phrase pairs.
  Graphic         ;; name of file containing problem graphic 
  Predefs         ;; An alist of predefined solution items.  Each member
                  ;; is a cons of a systementry (or nil) and a list 
                  ;; of API call arguments.  If the sytementry is not nil,
                  ;; it can be used to help fill out the API call.
  Fade            ;; An alist of fade statements, to be sent to the client.
                  ;; Each member is a cons of an entryprop or bgnode Id
                  ;; and a list of API call arguments.
                  ;; The entryprop/bgnode is used to determine when the help
                  ;; system will delete the entry.  They may also be used
                  ;; to help fill out the API call.
;; The following information is Andes2-specific
  Times		  ;; time legend as list of (time point, description) pairs.
                  ;; Ex: ((1 "car starts moving") (2 "car hits driveway"))
                  ;; See problem-times-english for details.
  Choices	  ;; list of choice category members for menus. Ex:
                  ;; ((bodies (car driveway wall Earth))
		  ;;  (branches (Br1 Br2 Br3)))
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
  
  VariableMarks   ;; A field containing variable-specific marks that will be used
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
  
  Graph              ;Storage for the bubblegraph. 
                     ; ((list of Qnode) (list of Enode))
  VarIndex           ;Variable-values indicating solution point.
                     ; (list of qvar)
  EqnIndex           ;Index of equations in the system. (list of eqn)
  Solutions          ;Set of solution bubbles.  The first will always be
                     ;the best.  (list of Eqnset)
  wm                 ;; Collection of the solver working memory.
  phrases            ;list of student phrases.
  keywords           ;For each tool, list of keywords and propositions
                     ;containing that proposition.
  pointers           ;vector of pointer objects.
  )

(defvar *cp*)                  ; the current problem

;;-----------------------------------------------------------------------------
;; Problem output

;; Print out the full problem.  
(defun print-problem (Problem &optional (Stream t) (level 0))
  "Print out the specified problem to the specified stream at the specified level."
  (declare (ignore level))
  (format Stream "<Problem ~W with ~A solution~:p>~%" 
	  (Problem-name Problem) (length (Problem-solutions Problem))))

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
;; Most field contents are stored using a simple format call.  Some more 
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
  (format Stream "English       ~W~%" (problem-English Problem))
  (format Stream "Times         ~W~%" (problem-Times Problem))
  (format Stream "Choices       ~W~%" (problem-Choices Problem))
  (format Stream "Graphic       ~W~%" (problem-Graphic Problem))
  (format Stream "Predefs       ~W~%" (problem-Predefs Problem))
  (format Stream "Fade          ~W~%" (problem-Fade Problem))
  (format Stream "Comments      ~W~%" (problem-Comments Problem))
  (format Stream "Features      ~W~%" (problem-features Problem))
  (format Stream "Soughts       ~W~%" (problem-Soughts Problem))
  (format Stream "Givens        ~W~%" (problem-Givens Problem))
  (format Stream "ForbiddenPSMS ~W~%" (problem-ForbiddenPSMS Problem))
  (format Stream "IgnorePSMS    ~W~%" (problem-IgnorePSMS Problem))
  (format Stream "VariableMarks ~W~%" (problem-VariableMarks Problem))
  (format Stream "WorkingMemory ~W~%" (problem-wm Problem))
  (format Stream "Phrases       ~W~%" (problem-phrases Problem))
  (format Stream "Keywords      ~W~%" (problem-keywords Problem))
  (format Stream "Pointers      ~W~%" (problem-pointers Problem))
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

(defmacro mpf-readret (S)
  `(read ,S "Error: invalid problem file."))

(defun read-problem-file (Name &key (Path (Default-ProblemFile-Path)) 
				    (Read-old-format nil))
  "Read the specified problem file into a new Problem Struct."
  (declare (ignore read-old-format))
  ;; File format is double-precision
  (let ((*read-default-float-format* 'double-float)
	(Filename (problem-filename Name Path)) (Problem (make-Problem)))
    (when (probe-file (namestring FileName))
      (with-open-file (File 
		       (namestring Filename)
		       :direction :input
		       :if-does-not-exist :error)
	 
	(mpf-read File '<Andes2Problem>) ;Read the file ID tag.
	;; Read the contents until the close tag is found.
	(read-pfile-contents File Problem) 

	;; Overwrite parts of problem that are in memory
	;; This should slightly reduce memory needs of each session
	(let ((kb-prob (get-problem (problem-name Problem))))
          (if kb-prob
	    (progn
	      (setf (problem-soughts Problem) (problem-soughts kb-prob))
	      (setf (problem-givens Problem) (problem-givens kb-prob))
	      (setf (problem-statement Problem) (problem-statement kb-prob)))
	    (warn "Problem ~A not found in KB, using file." 
		  (problem-name Problem))))
	
	;; Remove pointers, creating compact memory structure.
	(unpointerize-problem Problem)

	;; Re-construct bubblegraph structures
	(dolist (enode (second (problem-graph Problem))) 
	  (psmg->help-psmg (enode-path enode)))
	(regen-en-qn-links (problem-graph Problem))
	(regen-qn-en-links (problem-graph Problem))

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
(defun read-pfile-contents (S P)
  "Read the header contents of the Problem File."
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
    (english       (setf (Problem-English Problem) (mpf-readret Stream)))
    (Times         (setf (Problem-Times Problem) (mpf-readret Stream)))
    (Choices       (setf (Problem-Choices Problem) (mpf-readret Stream)))
    (Graphic       (setf (Problem-Graphic Problem) (mpf-readret Stream)))
    (Predefs       (setf (Problem-Predefs Problem) (mpf-readret Stream)))
    (Fade          (setf (Problem-Fade Problem) (mpf-readret Stream)))
    (comments      (setf (Problem-comments Problem) (mpf-readret Stream)))
    (features      (setf (Problem-features Problem) (mpf-readret Stream)))
    (soughts       (setf (Problem-soughts Problem) (mpf-readret Stream)))
    (givens        (setf (Problem-givens Problem) (mpf-readret Stream)))
    (ForbiddenPSMS (setf (Problem-forbiddenPSMS Problem) (mpf-readret Stream)))
    (IgnorePSMS    (setf (Problem-IgnorePSMS Problem) (mpf-readret Stream)))
    (VariableMarks (setf (Problem-VariableMarks Problem) (mpf-readret Stream)))
    (WorkingMemory (setf (Problem-wm Problem) (mpf-readret Stream)))
    (Phrases       (setf (Problem-phrases Problem) (mpf-readret Stream)))
    (Keywords      (setf (Problem-keywords Problem) (mpf-readret Stream)))
    (pointers      (setf (Problem-pointers Problem) (mpf-readret Stream)))
    
    (Bubblegraph (setf (problem-Graph Problem) (read-mreadable-bubblegraph Stream)))
    (EqnIndex (setf (Problem-EqnIndex Problem) (read-mreadable-eqns Stream (Problem-Graph Problem))))
    (VarIndex (setf (Problem-VarIndex Problem) (read-mreadable-qvars Stream (Problem-Graph Problem))))
    (Solutions (setf (Problem-Solutions Problem) 
                  (read-mreadable-EqnSets Stream (Problem-EqnIndex Problem) (Problem-Graph Problem)
		  (problem-version Problem))))

    (t (error "Undefined Problem-header tag. ~A" name))))


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
				Statement
		                English
				Comments Features
				ModDate (Version *latest-file-version*) 
				ForbiddenPSMS IgnorePSMS
				VariableMarks Times
				Choices Graphic Predefs Fade)
  "Define a problem struct and store it."

  ;; compile-time tests

  (unless (list-of-lists-p soughts)
    (error "~A: Soughts must be a list of lists." name))

  (unless (list-of-lists-p givens)
    (error "~A: Givens must be a list of lists." name))

  (unless (listp ForbiddenPSMS) 
    (error "~A: ForbiddenPSMS must be a list." name))

  (unless (listp IgnorePSMS) (error "~A: IgnorePSMS must be a list." name))

  (unless (listp statement) (error "~A: Statement must be a list." name))

  `(let ((Prob (make-problem :Name ',name          ;Define the problem struct.
				   :soughts ',soughts
				   :Givens ',Givens
				   :ForbiddenPSMS ',ForbiddenPSMS
				   :IgnorePSMS ',IgnorePSMS
				   :VariableMarks ',VariableMarks
				   :Statement ',Statement
				   :English ',English
				   :Comments ',(force-to-list Comments)
				   :Features ',Features
				   :ModDate ',ModDate
				   :Version ',Version
				   :Times   ',Times
				   :Choices ',Choices
				   :Graphic ',Graphic
				   :Predefs ',Predefs
		                   :Fade ',Fade)))

    ;; Tests to occur at load time
   (dolist (rule ',English)
    (when (match:matches-model-syntax (car rule))
      (error "~A: Ontology member ~A matches model syntax" ',name rule)))

   (add-problem Prob))) ;Store the problem for access

(defun clear-Problem-Registry ()
  "Clear out the problem registry."
  (format t "clearing *problem-registry*")
  (clrhash *Problem-Registry*))


;;------------------------------------------------------------------
;; problem access.


(defun get-problem (P)
  "Get the named problem from the *Problem-Registry* Hashtable."
  (gethash P *Problem-Registry*))

(defun add-problem (Prob)
  "Add a problem to the *Problem-Registry* Hashtable."
    (setf (gethash (Problem-name Prob) *Problem-Registry*) Prob))

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

(defun listprobs () 
  "list of problems in alphabetical problem name order"
  (let ((values))
    (maphash #'(lambda (key value) (declare (ignore key))
		       (push value values)) *Problem-Registry*)
    (sort values #'string< :key #'problem-name)))

(defun working-problem-p (problem)
  "Test if problem is tagged as working"
   (and problem (member 'working (problem-features problem))))

(defun no-quant-problem-p (problem)
  "Return t iff the problem is a no-quant problem."
  (notany #'quantity-expression-p (problem-soughts problem)))


(defun multi-sought-problem-p (Problem)
  "Does the problem seek more than 1 sought?"
  (< 1 (length (problem-soughts Problem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;     Macros to apply to a problem after solutions have been found
;;;;
;;;;

(defparameter *debug-pp* t)

(defparameter *post-processing* () "list of operations to apply after a solution has been found")

(defun clear-post-processing ()
  (setq *post-processing* nil))

(defstruct postoperator
  name  ;name, for internal use
  comment ;any comment
  lambda ;function
)

(defmacro post-process (name args &rest body)

  (when (or (not (consp args)) (cdr args))
    (error "post-process functions take one argument, the problem name."))

  ;; Here, one might want to define a regular "defun" function 
  ;; which could be called directly.
  `(let ((e  (make-postoperator :name ',name
			       :comment ',(when (stringp (car body)) 
						(pop body))
			       :lambda (compile nil '(lambda ,args ,@body)))))
    (when (member ',name *post-processing* :key #'postoperator-name)
      (setf *post-processing* (delete ',name *post-processing* 
				      :key #'postoperator-name)))
    (push e *post-processing*)))

(defun run-post-processing (problem)
  (ps-bp "Apply post-process functions:  ~A" (Problem-Name Problem))
  (dolist (f *post-processing*)
    (when *debug-pp* (format t ";;;;  ~A~%" (postoperator-name f)))
    (funcall (postoperator-lambda f) problem)))




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

(defun problem-graphic-dimensions (graphic)
  "Return list with width and height of graphic file"
  ;; Assume all images are in this subdirectory:
  (let* ((graphic-dir (merge-pathnames "images/" *andes-path*))
	 (file (namestring (merge-pathnames graphic graphic-dir)))
	 (program-name "identify")
	 (id (sb-ext:run-program program-name
				 (list "-ping" "-format" "(%w %h)" file)
				 :output :stream :search t)))
    ;; Only return something if the program was successful.
    (prog1 (if (= 0 (sb-ext:process-exit-code id))
	       (read (sb-ext:process-output id))
	       (warn (or (read-line (sb-ext:process-output id) nil)
			 (format nil "command \"~A\" not found" program-name))))
      (sb-ext:process-close id))))

(defun proper-list-p (x) 
  (or (null x) (and (consp x) (proper-list-p (cdr x)))))

;; Currently, this makes a copy of the top levels of obj,
;; except for enodes.  Could instead do an "in place" version.
(defun remove-pointers (obj pointers)
  "Replace any pointer in an expression with its expansion, returning expression"
  (cond 
    ((and (consp obj) (eq (car obj) 'pointer))
     (svref pointers (cdr obj))) ;pointers itself should be free of pointers
    ;; Handle binding list elements
    ((and (consp obj) (variable-p (car obj)))
     (setf (cdr obj) (remove-pointers (cdr obj) pointers))
     obj)
    ((proper-list-p obj)
     (map-into obj #'(lambda (x) (remove-pointers x pointers)) obj))
    ((enode-p obj)
     (setf (enode-path obj) (remove-pointers (enode-path obj) pointers))
     obj)
    (t obj)))

(defun unpointerize-problem (problem)
  (let ((pointers (problem-pointers problem)))
    ;; Expand pointers themselves.  This assumes
    ;; that pointers only refer to ones earlier on list
    (dotimes (i (length pointers))
      (setf (svref pointers i)
	    (remove-pointers (svref pointers i) pointers)))
    (setf (problem-wm problem) 
	  (remove-pointers (problem-wm problem) pointers))
    (setf (second (problem-graph problem))
	  (remove-pointers (second (problem-graph problem)) pointers))
    ;; For debugging, it may be useful to set this to "pointers"
    (setf (problem-pointers problem) nil))
  problem)

(defvar *pointers*)

(defun add-pointers (obj) 
  "Replace subexpression with pointers, adding to the list of pointers."
  (cond 
    ;; Handle binding list element.
    ((and (consp obj) (variable-p (car obj)))
     (cons (car obj) (add-pointers (cdr obj))))
    ;; Pointerize proper lists.
    ((and obj (proper-list-p obj)) ;don't make pointer for nil.
     (let ((x (position obj *pointers* :test #'equal :key #'car)))
       (if x 
	   ;; position counting from end of list
	   (cons 'pointer (- (length *pointers*) 1 x))
	   (progn 
	     (push (cons obj (mapcar #'add-pointers obj)) *pointers*)
	     ;; position of new pointer in list
	     (cons 'pointer (- (length *pointers*) 1))))))
    ((enode-p obj)
     (setf (enode-path obj) (mapcar #'add-pointers (enode-path obj)))
     obj)
    (t obj)))

(defun pointerize-problem (problem)
  (let ((*pointers* nil))
    (setf (problem-wm problem) (mapcar #'add-pointers (problem-wm problem)))
    (setf (second (problem-graph problem))
	  (mapcar #'add-pointers (second (problem-graph problem))))
    (setf (problem-pointers problem) 
	  (apply #'vector (reverse (mapcar #'cdr *pointers*)))))
  problem)
