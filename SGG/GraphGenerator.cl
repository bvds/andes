#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GraphGnerator.cl
;; Collin Lynch
;; 2/7/2000
;;
;; This file defines the Bubble graph generation code for the Andes2
;; solution graph system.  It is designed to be fed a problem defined
;; according to the structs in Problems.cl and to generate a bubble
;; graph based upon it.  
;;
;; The file Problem.cl defines the specific problem interface.
;;
;; What is below is psudocode.

##########################  DataStructures  #######################

Quantity Node  -- Representation of a quantity such as 
                  (at (accel Blk 1) 1)

   Exp:   Lisp Expression representing this quantites' value.
   Marks:  List of marks such as 'invalid_path' for help.

   Equations: A list of equation nodes containing 
	      this quantity.

   (possible)
   Tree:  A reversal and union of the paths for indexing 
          by the help system.


Equation Node  -- Represntation of inidividual equations.

   ID:   Equation ID supplied by the solver.

   Path: The solution path used to build the equation.

   Quantities:  A list of quantity nodes in this eq.

   Marks: Markers such as 'invalid_path' for the HelpSys.

######################## Bubble Generation ########################

1. Initialize the list of Soughts with the sought elements in the
   problem struct.

2. Initialize the set of Knowns with each quantifiable given in
   the problem struct.

3. While soughts != {}

   4. Pop the top element of soughts into S.

   6. Call the turkey solver on S with the problem givens.

      The solver will return a a set of tuples of the form: 
	(<EqID> <Qs> <Path> <Assumpt>)
	Where:  EqID an equation ID for the result equation.
                Qs a set of quantities refrenced by equation.
	        Path a list of path nodes that defines the 
                     interface steps necessary to produce EqID.
		Assumpt is the set of assumptions made in Path.

   7. For each tuple T 

      8. If no corresponding equation node already exists.

         9. Then for each quatity Q in T (excluding the sought)

            10. If a preexisting quantity node Q' for Q does
 		not exists.

	       14. Generate a quantity node Q' for Q.

	       15. Add Q' to the list of Soughts.


	    15. Add Q' to the list of quantities in E.

	    16. Add E to the list of equations in Q'.

	    |#


;;========================================================================
;; Parameters.
;;

(defparameter *Debug-gg* t
  "Print runtime debugging output for the Graph Generation process.")

;;(defparameter *Equation-Solver* #'solve-for 
;;"Should be initialized to a function that will solve for a goal quantity gvien a value.")

;;(defparameter *S-Print-Graph* ()
;;"If t a long form of the Graph will be pretty-printed at the end of a call to solve-problem.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generate-bubblegraph
;;; Arguments:
;;;   Soughts : A list of kb-form sought quantities.
;;;   Givens  : A list of kb-form givens (quantitites and other params)
;;;  &key
;;;     IgnorePSMS : A list of psm-names (from ontology) never to be used.
;;;
;;; Returns:  A Bubblegraph comprising the solution(s).
;;;
;;;
;;; Bubblegraph generation follows a basic recursive model that is supprisingly 
;;; difficult to implement.  The search starts with a list of sought quantities
;;; each of these will be transformed into nodes and then searched for recursively.
;;; The recusrsive search will then identify the type of the sought and take the
;;; appropriate action based upon it.
;;;
;;; If the sought quantity is already a member of the graph then that node will
;;; be marked as sought and the search will move on.  If the node is a parameter
;;; then it will be marked as such and returned.  If it is A given then it will 
;;; be marked as such and its supporting given psm node will be added to the 
;;; graph and the resulting graph will be returned.
;;;
;;; Lastly and most likely if the node is a complex sought I.E. Requiring a series
;;; of psms to solve then that recursive solution process will occur and the 
;;; resulting graph will be returned.  

(defun generate-bubblegraph (Soughts Givens &key (IgnorePSMS nil))
  (let ((graph) (Q))
    (dolist (S Soughts)
      (cond ((not (quantity-expression-p S))
	     (format t "WARNING: Non quantity ~A listed as sought is being ignored." S))
	    
	    ((setq Q (match-exp->qnode S Graph))
	     (push 'Sought (Qnode-Marks Q)))
	    
	    (t (setq q (gg-solve-for-sought S Givens :graph Graph 
					    :ignorepSMS IgnorePSMS))
	       (push 'Sought (Qnode-Marks (car Q)))
	       (setq Graph (cdr Q)))))
    (index-bubblegraph Graph)))


;;; This is the top level recursive call for the generator.  Given a 
;;; sought expression thios will generate a qnode for it of the 
;;; appropriate type as well as any supporting graph info.  It will
;;; then return that qnode consd onto the resulting graph (which will
;;; include the qnode itself).  Thus this code allows for a sought
;;; quantity to be solved for depending upon its type and the appropriate
;;; values to be returned.
;;;
;;; An optional graph can be supplied which will be used in the search
;;; process to avoid duplication and to link in the sought quantities.
;;;
;;; If the sought is already presnet in the graph then that qnode will
;;; be returned along with the unchanged graph.
;;;
;;; If the sought is a parameter then that parametewr will be solved for 
;;; and the resulting qnode will be returned as well as the supplied graph
;;; with that qnode added.
;;;
;;; If the sought is a given or constant value then the qnode and its 
;;; associated psm node will be generated and added to the graph.  The 
;;; psm will then be returned along with its modified graph.
;;;
;;; Laslt y if the sought can be solved for by psm chaining then the 
;;; code will call the qsolver and recursively attempt to solve for the
;;; quantity adding the resulting nodes to the supplied graph.
(defun gg-solve-for-sought (Sought Givens &key (Graph Nil) (IgnorePSMS nil))
  "Solve for the sought with the givens and optional Graph."
  (when (not (quantity-expression-p Sought))
    (Error "Non-quantity sought supplied ~A." Sought))
  ; AW -- moved this case up so no trace output for preexisting sought
  (or (gg-solve-preexisting-sought Sought Graph)
    (progn
      (gg-debug-incdepth)
      (gg-debug-separator "#" 80)
      (gg-debug "Finding eqns for ~A~%" Sought)
      (let ((R (or ; (gg-solve-preexisting-sought Sought Graph)
	           (gg-solve-param-sought Sought Givens Graph)
	           (gg-solve-given-sought Sought Givens Graph)
	           (gg-solve-constant-sought Sought Givens Graph)
	           (gg-solve-complex-sought Sought Givens Graph))))
      ;(gg-debug "Done finding eqns for: ~A ~%" Sought)
      ;(gg-debug-separator "#" 80)
      (gg-debug-decdepth)
      (when (null R)
          (error "No equations returned for ~A~%" Sought))
      R))))


;;;-------------------------------------------------------------------
;;; Preexisting soughts 
;;; If a qnode alreasy exists within a graph that matches the supplied
;;; sought quantity then return that qnode along with the graph itself.
(defun gg-solve-preexisting-sought (Sought Graph)
  (let ((Q (match-exp->qnode Sought Graph)))
    (when Q 
      ;(gg-debug "Sought already processed.~%")
      (cons Q Graph))))


;;;-------------------------------------------------------------------
;;; Unnamed Parameters.
;;; Unnamed parameter nodes must be solved for using the 
;;; solve-for-param-var code in the qsolver.  This will generate
;;; the necessary path and variable information which will then
;;; be returned to the student.
(defun gg-solve-param-sought (Sought Givens Graph)
  "Generate the infor for an unnamed parameter."
  (let* ((Q) (P (find-sought-param Sought Givens)))
    (when (and P (setq Q (solve-for-param-var Sought Givens)))
      (gg-debug "Sought is the parameter: ~A" (Qsolres-id Q))
      (setq Q (make-qnode :exp Sought
			  :var (qsolres-id Q)
			  :path (qsolres-path Q)
			  :marks (cons 'Parameter 
				       (if (caddr P) (cddr P)))))
      (cons Q (add-nodes-to-bubblegraph Graph Q)))))


(defun find-sought-param (Sought Givens)
  "Find a parameter statement that matches Sought."
  (find-if #'(lambda (G) (and (eq (car G) 'Parameter)
			      (equalp (cadr G) Sought)))
	   Givens))


;;;-------------------------------------------------------------------
;;; Solve givens.
;;; Given nodes are solved for by solving for a given psm for the 
;;; sought itself.  Once that has been done then the enode qill be 
;;; connected to a qnode representing the sought and added to the 
;;; graph itself.  
;;;
;;; At present it is also necessary to store the matching given
;;; as that is the ID expression for the enode.  This might be
;;; nice to remove but is not necessary.
(defun gg-solve-given-sought (Sought Givens Graph)
  (let ((Q) (P) (R) (G (gg-find-matching-given Sought Givens)))
    (when G 
      (gg-debug "Sought is given, adding:~%") 
      ;(gg-debug " Qnode: ~A ~%" Sought)
      (gg-debug " PSM: ~A.~%" G)

      (Setq R (solve-for-given-eqn Sought Givens))
      (setq Q (make-qnode :exp Sought
			  :var (nth 1 (Qsolres-Nodes R))
			  :marks '(Given)))
      (setq P (gg-qsolres->Enode R :ID G :Algebra (Qsolres-ID R)
			     :marks 'Given :Qnodes Q))
      (setf (qnode-eqns Q) (list P))
      (cons Q (add-nodes-to-bubblegraph Graph Q P)))))
					

(defun gg-find-matching-given (Sought Givens)
  (find-if #'(lambda (G) (and (eq (car G) 'Given)
			      (equalp (cadr G) Sought)))
	   Givens))



;;;------------------------------------------------------------------------
;;; Solve constant sought
;;; Constants like givens consists of an enode and a qnode pair. 
;;; This function will solve for the sought similar to the way it
;;; solves for given quantities and will return the result if found.
(defun gg-solve-constant-sought (Sought Givens Graph)
  (let ((Q) (P) (R (solve-for-constant-quantity Sought Givens)))
    (when R
      (gg-debug "Sought is constant ~A~%" (caar (Qsolres-Nodes R)))
      (setq Q (make-qnode :exp Sought
			  :var (caar (Qsolres-nodes R))
			  :marks '(constant)))
      (Setq P (gg-Qsolres->enode R :Qnodes Q))
      (setf (qnode-eqns Q) (list P))
      (cons Q (add-nodes-to-bubblegraph Graph Q P)))))



;;;---------------------------------------------------------------------
;;; Solve for complex quantities.
;;; Solving for complex quantities is a recursive process.  The system 
;;; will generate a qnode for the sought and call the solve-for-psm
;;; code located in qsolver.  This will return a (possibly null) set of
;;; psms that contain the sought.  Each of these will be assigned a psmnode
;;; the resulting nodes will be added to the graph.  Then the system will 
;;; recursively solve for each of the new soughts that has been introduced.
;;; by the psms.  If any IgnorePSMS are supplied then the system will 
;;; remove those from the results list and not include them in the graph
;;; or recursively search based upon them.
;;;
;;; If the psm already exists in the graph then it will be pushed onto the
;;; qnode eqns else the system will search for the psm recutsively.
;;;
;;; This is probably the most debateable point of the code but what the hell.
(defun gg-solve-complex-sought (Sought Givens Graph &optional IgnorePSMS)
  "Attempt to solve for the complex sought provided."
  (let (Qnode Results)
    (setq Results (solve-for-psm-quantity Sought Givens))  ;; leaving only valid choices.
    (when (car Results)
      ;(gg-debug "Solving for Sought requires complex psm~%")
      (setq Results (gg-remove-ignore-psms IgnorePsms Results))  ;; remove the ignore psms.
      (setq Qnode (make-qnode :exp Sought))
      (gg-collect-complex-psms 
       Results Givens Qnode
       (add-nodes-to-Bubblegraph Graph Qnode)))))


;;; gg-remove-ignore-psms
;;; Given a list of psms that we do not wish to use and a list of psm solutions
;;; for the specified sought return a list of those that are not part of the 
;;; ignore set.  For debugging purposes this is accomplished via a loop.
(defun gg-remove-ignore-psms (Ignore PSMS)
  (let (r)
    ;(gg-debug-separator "=" 10)
    ;(gg-debug "Removing \"Ignore\" Psms~%")
    (dolist (p Psms)
      (cond ((exp-of-psmtype-set? (qsolres-id P) Ignore)
	     (gg-debug "  Removing ignore-psm ~%")
	     (gg-debug (format nil "~A.~%" (qsolres-id P))))
	
	    (t (gg-debug (format nil "  Got eqn ~A~%" (qsolres-id P)))
	       ;(gg-debug (format nil "~A.~%" (qsolres-id P)))
	       (push P R))))
    ;(gg-debug-separator "=" 10)
    R))

;;; Once we have a complex sought node it is necessary to generate all of the 
;;; psms that are attatched to it.  This function will cycle through the psms
;;; list that is provided to it.  (the contents of results from above) and
;;; test each pssm.  If the psm in question has already been generated then
;;; it will simply be added to the node.  Otherwize it will be generated
;;; recursively using gg-solve-complex-psm to produce the psm and will  add that
;;; to the updated graph.
(defun gg-collect-complex-psms (Psms Givens Qnode Graph)
  (let ((NewGraph Graph) (PSM))
    (dolist (P PSMs)
      ; AW -- changed to suppress output in case psm expanded already
      ;(gg-debug-incdepth)
      ;(gg-debug-separator "=" 80)
      ;(gg-debug "Processing quantities from PSM: ~A  ~%" (Qsolres-id P))
      (cond ((setq PSM (match-exp->enode (Qsolres-id P) Graph))
	     ;(gg-debug "PSM already processed.~%")
	     (pushnew PSM (Qnode-Eqns Qnode)))
	    (;; AW: *second* change marked ;-; eliminates this output altogether
	     ;; might want switch to turn it on.
	     T
               ;-;(gg-debug-incdepth)
               ;-;(gg-debug-separator "=" 80)
               ;-;(gg-debug "Processing quantities from ~A:~%" (Qsolres-id P))
	     (setq PSM (gg-solve-complex-psm P Givens NewGraph))
	     (pushnew (car PSM) (Qnode-Eqns Qnode))
	     (setq NewGraph (cdr PSM))
               ;-;(gg-debug "Done processing quantities from ~A~%" (Qsolres-id P))
               ;-;(gg-debug-separator "=" 80)
               ;-;(gg-debug-decdepth)
	     )))
      ;(gg-debug "Done processing quantities from ~A~%" (Qsolres-id P))
      ;(gg-debug-separator "=" 80)
      ;(gg-debug-decdepth)
    (cons Qnode NewGraph)))


;;; Generate the an enode for the psm and add it to the graph.  After that
;;; recurisively solve for each of the quantities connected to the psm.  
;;; Add each of these quantities to the psm and return the result iff all 
;;; of them can be solved for.  If not return nil.
(defun gg-solve-complex-psm (Result Givens Graph)
  "Solve for the specified psm result."
  (let ((R) (PSM))
    (setq PSM (gg-qsolres->enode Result))
    ; AW: simplify this:
    ;(gg-debug "PSM is new.~%")
    ;(gg-debug "Psm quantities: ~A~%" (qsolres-nodes Result))
    ;-;(gg-debug "~A~%" (qsolres-nodes Result))
    (when (setq R (gg-collect-psm-qnodes 
		   (Qsolres-Nodes Result) 
		   Givens (add-nodes-to-bubblegraph Graph PSM)))
      (setf (enode-qnodes PSM) (car R))
      (dolist (Q (car R))
	(pushnew PSM (Qnode-Eqns Q)))
      (cons PSM (cdr R)))))


;;; Collection of psm qnodes involves solving for each qnode in turn
;;; given the graph as defined by the previous solutions.  Thus the 
;;; System will recursively call the solve function with each sought
;;; using the graph from the previous solution iff it was successful.
;;; If the collection ever fails then it will return nil.  Otherwize the
;;; system will return the list of qnodes consed onto the resulting graph.
(defun gg-collect-psm-qnodes (Specs Givens Graph &optional Nodes)
  "Collect the nodes corresponding to each spec."
  (if (Null Specs) 
      (cons Nodes Graph)
    (gg-collect-next-psm-qnode Specs Givens Graph Nodes)))


(defun gg-collect-next-psm-qnode (Specs Givens Graph Nodes)
  (let ((Result (gg-solve-for-sought (cadar Specs) Givens :Graph Graph)))
    (when Result 
      (if (null (Qnode-Var (car Result)))
	  (setf (Qnode-Var (car Result)) (caar Specs)))
      (gg-collect-psm-qnodes 
       (cdr Specs) Givens (cdr Result) 
       (cons (car Result) Nodes))))) 


;;; gg-Qsolres->Enode
;;; Given a Qsolres it is necessary to generate an enode for it
;;; This function does so including setting up the subvars and
;;; subeqns properly and settng the contents as specified.
(defun gg-qsolres->Enode (R &key ID Algebra Path Marks 
			      Subeqns Subvars 
			      Qnodes Assumpts)
  (let ((E (make-Enode :ID (or ID (Qsolres-ID R))
		       :Algebra (or Algebra (Qsolres-Algebra R))
		       :path (or Path (Qsolres-Path R))
		       :marks (force-to-list Marks) 
		       :subeqns (or Subeqns (qsolres-subeqns R))
		       :subvars (or Subvars (qsolres-subvars R))
		       :Qnodes (force-to-list Qnodes)
		       :assumptions (or Assumpts (qsolres-Assumpts R)))))
;;    (dolist (Eq (Enode-Subeqns E))
;;      (setf (Eqn-Nodes Eq) (list E)))
;;    (dolist (Q (Enode-SubVars E))
;;      (setf (Qvar-Nodes Q) (list E)))
    E))

;;;=============================================================================
;;; Debug code.

(defun gg-debug-separator (sep count)
  (if *Debug-gg* (print-separator sep count)))

(defun gg-debug (&rest form)
  (when *Debug-gg* 
    (format t "~A: " *gg-debug-depth*)
    (apply #'format t form)))

(defparameter *gg-debug-depth* 0)

(defun gg-debug-incdepth ()
  (incf *gg-debug-depth*))

(defun gg-debug-decdepth ()
  (decf *gg-debug-depth*))

(defun trace-gbg ()
  (trace generate-bubblegraph
	 gg-solve-for-sought
	 gg-solve-preexisting-sought
	 gg-solve-given-sought
	 gg-find-matching-given
	 gg-solve-constant-sought
	 gg-solve-complex-sought
	 gg-solve-complex-psm
	 gg-collect-psm-qnodes
	 gg-collect-next-psm-qnode))



#|

  ;;;=========================================================================
  ;;; Solution functions
  ;;; The functions in this section are used to solve for the sought quants
  ;;; and interface with the qsolver in that respect.  
  
  ;;; gg-solve-param-soughtq
  ;;; Givena a sought quantity attempt to solve for it as a parameter.  
  ;;; If it is a parameter solve for it and return the resulting graph after
  ;;; adding the qnode to it.  If it is not a param then return nil.
  (defun gg-solve-param-soughtq (Q Givens Graph)
  "Solve for a parameter soughtq if possible."
  (let ((P (find-if #'(lambda (G) (and (eq (car G) 'Parameter)
  (equalp (cadr G) (qnode-exp Q))))
  Givens)))
  (when P 
  (if (caddr P)
  (add-named-param-qnode Q P Graph)
  (add-unnamed-param-qnode Q P Graph)))))
  
  ;;; If the qnode is a named parameter then simply add the param info
  ;;; to the qnode and add it to the graph.
  (defun add-named-param-qnode (Q P Graph)
  "If the qnode is named then update and add it."
  (setf (Qnode-var Q) (caddr P))
  (pushnew 'Parameter (Qnode-marks Q))
  (add-qnode-to-bubblegraph Q))
  
  
  ;;; If the Qnode is an unnamed parameter then the system will need
  ;;; to solve for it using the solve-for-param-var code in the qsolver.
  ;;; This information will then be added to the qnode and it will be 
  ;;; added to the 
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get-unnamed-parameter-nodes
  ;; Get the parameter qnodes for each unnamed parameter node in the givens.
  ;;
  ;; Arguments: Givens: A list of given s-expressions.
  ;;
  ;; Returns: A list of parameter Qnodes.

  
  
  ;;=========================================================================
  ;; Public Solve functions.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Generate-bubblegraph
  ;; Given a sought quantity and a set of given values Solve generates a bubblegraph
  ;; solution for the sought quantity with givens.
  ;;
  ;; Arguments: Soughts: A list of S-expressions representing the sought quantities.
  ;;            Givens:  A list of S-expressions reprenting the given problem values.
  ;;
  ;; Returns: The resulting bubblegraph solution for Soughts
  
  (defun Generate-Bubblegraph (Soughts Givens)
  "Solve for the sought quantities given the specified values."
  (let ((SoughtQ (Get-Qnodes Soughts '(Sought)))                  ;;Initialize the soughts list with sought nodes. 
  (S) (Qnodes) (Enodes) (R) (wm))
  
  (when *Debug* 
  (format t "######### Generating Graph for ~A~%" Soughts)
  (format t "######### Given: ~A~%" Givens))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Error checking 
  (if (not SoughtQ) (error "No sought quantities specified."))    ;;Test for errors.
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set initial values.
  (multiple-value-setq (wm Qnodes Enodes)                         ;;Get the initial given qnodes and Enodes.  
  (get-predefined-nodes Givens))  
  (setq Qnodes (append SoughtQ Qnodes))                           ;;Set the Qnodes List.
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Main Loop.
  (loop until (null SoughtQ)                                         ;; While Soughts != {}
  do (setq S (pop SoughtQ))                                      ;; Pop the car of Soughts into S.
  (when *Debug* (format t "~2%############### Solving for ~A #############~%" S))
  (setq R (generate-constant-nodes S Givens))                 ;; Determine if the node is given.
  (setq wm (union wm (cadr R) :test #'equalp))                ;; Set the wm if it is.
  (setq R (car R))                                            ;; And set the constant to the car.
  (cond (R (pushnew R Enodes)                                      ;; if so add it to the equation nodes.
  (pushnew R (Qnode-Eqns S))                              ;; Add it to the sought Qnode.
  (when *Debug*
  (format t "########### ~A is constant ~A~%" S R)))
  (t (setq R (generate-PSM-Nodes S Givens Qnodes Enodes)) ;; Then generate the PSM nodes for it.
  (setq SoughtQ (append (nth 0 R) SoughtQ))            ;; Modify the nodes as necessary.
  (setq Qnodes (nth 1 R))
  (setq Enodes (nth 2 R))
  (setq wm (union wm (nth 3 R) :test #'equalp)))))
  
  (when *Debug* 
  (format t "########### Done Generating Graph:~%")
  (format t "  ~A Qnodes~%" (length Qnodes))
  (format t "  ~A Enodes~%" (length Enodes)))
  (list (index-bubblegraph (list Qnodes Enodes)) wm)))                           ;; Return the graph.
  
  
  
  
  
  ;;-------------------------------------------------------------------------
  ;; private solve functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get-predefined-nodes
  ;; Get the Enodes and Qnodes that are predefined by the givens
  ;; this includes given node values and the parameters.
  ;;
  ;; Arguments: Givens: The given or predefined values.
  ;;
  ;; Returns: The resulting set of Qnodes and enodes in a list.
  
  (defun get-predefined-nodes (Givens)
  "Get the given nodes and parameter nodes."
  (let ((nodes) (wm) (R))                
  (setq R (get-given-nodes Givens))                   ;; Get the given qnodes.
  (setq nodes (car R))                                ;; Set the nodes list to them.
  (setq wm (cadr R))                                  ;; Store the wm for use.
  
  (setq R (get-named-parameter-nodes Givens))         ;; Get the parameter nodes.
  (setf (nth 0 nodes) (append (nth 0 nodes) (car R))) ;; add them to the qnodes. 
  (setq wm (union wm (cadr R) :test #'equalp))        ;; Store the wm.
  
  (setq R (get-unnamed-parameter-nodes Givens))       ;; Get the unnamed params.
  (setf (nth 0 nodes) (append (nth 0 nodes) (car R))) ;; add them to the qnodes. 
  (setq wm (union wm (cadr R) :test #'equalp))        ;; Store them in the wm.
  
  (when *Debug*
  (format t "####### Generated ~A predefined equations: ~% ~A~%"
  (length (cadr nodes)) (cadr nodes))
  (format t "####### Generated ~A predefined quantity nodes:~% ~A~%"
  (length (car nodes)) (car nodes)))
  
  (values-list (cons wm nodes))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get-given-nodes
  ;; The Bubblegraph begins life with the set of given quantities defined
  ;; ofr it as well as their equations.  This function defines the nodes
  ;; and returns them using values.
  
  (defun get-given-nodes (Givens)
  "Get the Quantity and Equation nodes from Givens."
  (let ((R) (wm) (nodes (list nil nil)))
  (loop for Exp in Givens
  when (equalp (car Exp) 'Given)
  do (setq R (solve-for-given-eqn (cadr Exp) Givens))
  
  (pushnew (make-Qnode :exp (cadr Exp) 
  :var (nth 1 (qsolres-nodes R))
  :marks '(Given))
  (nth 0 nodes))
  
  (pushnew (make-Enode :ID Exp 
  :Algebra (Qsolres-ID R)
  :path (Qsolres-Path R)
  :marks '(Given)
  :Qnodes (list (car (nth 0 nodes)))
  :subeqns (qsolres-subeqns R)
  :subvars (qsolres-subvars R)
  :assumptions (qsolres-Assumpts R))
  (nth 1 nodes))
  (pushnew (car (nth 1 nodes)) (Qnode-Eqns (car (nth 0 nodes))))
  (setq wm (union wm (qsolres-wm R) :test #'equalp)))
  (list nodes wm)))
			   


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get-names-parameter-nodes
  ;; Get qnodes for each prenamed parameter in the Givens.
  ;;
  ;; Arguments: Givens: A list of given s-expressions.
  ;;
  ;; Returns: A list of parameter Qnodes.
  
  (defun get-named-parameter-nodes (Givens)
  "Get a list of parameter nodes for each given value."
  (loop for N in Givens
  when (and (equalp (car N) 'Parameter)
  (caddr N))
  
  collect (make-qnode :exp (cadr N)
  :var (caddr N)
  :marks '(Parameter))))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; get-unnamed-parameter-nodes
  ;; Get the parameter qnodes for each unnamed parameter node in the givens.
  ;;
  ;; Arguments: Givens: A list of given s-expressions.
  ;;
  ;; Returns: A list of parameter Qnodes.
  
  (defun get-unnamed-parameter-nodes (Givens)
  "Get a list of parameter nodes for each given value."
  (let ((Wm) (nodes) (R))
  (dolist (N Givens)
  (when (and (equalp (car N) 'Parameter)
  (not (caddr N)))
  
  (setq R (solve-for-param-var (cadr N) Givens))
  (pushnew (make-qnode :exp (cadr N)
  :var (qsolres-id R)
  :path (qsolres-path R)
  :marks '(Parameter))
  Nodes)
  (setq wm (union wm (qsolres-wm R) :test #'equalp))))
  (list nodes wm)))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; generate-constant-nodes
  ;; Given a quantity attempts to solve for the specified quantity as a 
  ;; constant equation using the PSM overhead.  If that fails it returns nil.
  ;;
  ;; Arguments: S: The quantity bring sought.
  ;;            G: The givens.
  ;;
  ;; Returns: (<New Enodes>) 
  ;;          or nil if this is not a constant.
  
  (defun generate-constant-nodes (Sought Givens)
  "Attempt to generate Constant nodes for Sought node S given G."
  (let ((R (solve-for-constant-Quantity (Qnode-Exp Sought) Givens)))
  
  (cond ((> (Length R) 1)
  (error "Multiple Constant Results for ~A~%  ~A~%" Sought R))
  
  ((and (= (length R) 1)
  (car R))
  (list (make-enode :ID (qsolres-id (car R))                  ;;Generate a new equation node.
  :Algebra (qsolres-algebra (car R))           
  :Path (qsolres-path (car R))
  :Qnodes (list Sought)
  :subeqns (qsolres-subeqns (car R))
  :subvars (qsolres-subvars (car R))
  :assumptions (qsolres-assumpts (car R)))
  (qsolres-wm (car R)))))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; generate-PSM-Nodes
  ;; Givena a quantity attempt to generate PSM node solutions for it using
  ;; solve-for-quantity, return the updates.
  ;;
  ;; Arguments: S: The quantity bring sought.
  ;;            G: The givens.
  ;;            Q: The preexisting Qnodes
  ;;            E: The preexisting Enodes.
  ;;
  ;; Returns: (<New Soughts> <New Qnodes> <New Enodes>) 
  ;;          or nil if this is not a constant.
  
  (defun generate-PSM-nodes (Sought Givens Qnodes Enodes)
  "Given a sought generate new Qnodes and Enodes for it if possible."
  (let ((SR) (QR Qnodes) (ER Enodes) (Qp) (E) (EP) (wm))
  (loop for R in (solve-for-PSM-quantity (Qnode-Exp Sought) Givens)   ;;for each result R from solve-for
  do (when (not (find-matching-enode (Qsolres-id R) ER))          ;;If no preexisting Equation matches it.
  
  (setq wm (union wm (qsolres-wm R) :test #'equalp))          ;; Store the wm.
  
  (setq E (make-enode :ID (qsolres-id R)                        ;;Generate a new equation node.
  :Algebra (qsolres-algebra R)           
  :Path (qsolres-path R)
  :subeqns (qsolres-subeqns R)
  :subvars (qsolres-subvars R)
  :assumptions (qsolres-assumpts R)))
  (pushnew E ER)                                            ;;Add E to the equation nodes.
  (pushnew E EP)
  
  (loop for Q in (qsolres-nodes R)
  do (setq Qp (find-matching-Qnode (cadr Q) QR)) ;;Set the local Qp value.
  
  (cond ((null Qp)                                ;;If no prexisting Qnode exists for Q
  (setq Qp (make-qnode :exp (cadr Q)       ;;generate a new one
  :var (car Q)))       
  (pushnew Qp SR)                             ;;Add it to the list of soughts.
  (pushnew Qp QR))                            ;;And add it to the list of Qnodes.
  
  (t (when (null (Qnode-var Qp))            ;;Otherwize check to see if Qp's var  
  (setf (Qnode-var Qp) (car Q)))))     ;;has been set and if not do so.
  
  (pushnew Qp (Enode-Qnodes E))                ;;Add the Qnode (found or generated) to E's quantities.
  (pushnew E (Qnode-Eqns Qp)))))               ;;Add E to Qp's equations.  This completes the loop.
  
  (when *debug*
  (format t "###### Solved for ~A~%" Sought)      
  (format t "###### Generated ~A equations:~% ~A~%" (length EP) EP)
  ;;(format t "###### Generated ~A quantities:~% ~A~%" (length (nth 1 R)) (nth 1 R))
  (format t "###### Adding ~A soughts:~% ~A~%" (length SR) SR))
  
  (list SR QR ER wm)))
  
  
  
  (defun find-matching-qnode (Exp Nodes)
  "Find the Qnode that matches Exp in Nodes."
  (find Exp Nodes
  :key #'Qnode-Exp
  :test #'equalp))
  
  
  
  (defun find-matching-Enode (EqID Nodes)
  "Find the Enode whode ID matches EQID in Nodes."
  (find EqID Nodes 
  :key #'Enode-ID
  :test #'equalp))
  
  
  
  (defun get-qnodes (Exps &optional (Marks nil))
  "Generate and return a set of quantity nodes for each quantity in EXPS."
  (loop for Exp in (remove-if-not #'exp-Quantity-p Exps)
  collect (make-qnode :Exp Exp
  :marks Marks)))
  



  ;;=====================================================================
  ;; Debugging functions.
  
  (defun trace-GraphGenerator ()
  (dolist (Func '(generate-bubblegraph
  get-qnodes
  find-matching-enode
  find-matching-qnode
  solve-problem
  make-qnode
  make-enode
  print-problem-bubblegraph
  get-given-nodes))
  
  (eval `(trace ,Func))))


  |#




