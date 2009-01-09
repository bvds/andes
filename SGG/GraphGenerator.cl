;;; Modifications by Anders Weinstein 2000-2008
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;##########################  DataStructures  #######################
;;;
;;;Quantity Node  -- Representation of a quantity such as 
;;;                  (accel Blk 1 :time 1)
;;;
;;;   Exp:   Lisp Expression representing this quantites' value.
;;;   Marks:  List of marks such as 'invalid_path' for help.
;;;
;;;   Equations: A list of equation nodes containing 
;;;	      this quantity.
;;;
;;;   (possible)
;;;   Tree:  A reversal and union of the paths for indexing 
;;;          by the help system.
;;;
;;;
;;;Equation Node  -- Represntation of inidividual equations.
;;;
;;;   ID:   Equation ID supplied by the solver.
;;;
;;;   Path: The solution path used to build the equation.
;;;
;;;   Quantities:  A list of quantity nodes in this eq.
;;;
;;;   Marks: Markers such as 'invalid_path' for the HelpSys.
;;;
;;;######################## Bubble Generation ########################
;;;
;;;1. Initialize the list of Soughts with the sought elements in the
;;;   problem struct.
;;;
;;;2. Initialize the set of Knowns with each quantifiable given in
;;;   the problem struct.
;;;
;;;3. While soughts != {}
;;;
;;;   4. Pop the top element of soughts into S.
;;;
;;;   6. Call the turkey solver on S with the problem givens.
;;;
;;;      The solver will return a a set of tuples of the form: 
;;;	(<EqID> <Qs> <Path> <Assumpt>)
;;;	Where:  EqID an equation ID for the result equation.
;;;                Qs a set of quantities refrenced by equation.
;;;	        Path a list of path nodes that defines the 
;;;                     interface steps necessary to produce EqID.
;;;		Assumpt is the set of assumptions made in Path.
;;;
;;;   7. For each tuple T 
;;;
;;;      8. If no corresponding equation node already exists.
;;;
;;;         9. Then for each quatity Q in T (excluding the sought)
;;;
;;;            10. If a preexisting quantity node Q' for Q does
;;; 		not exists.
;;;
;;;	       14. Generate a quantity node Q' for Q.
;;;
;;;	       15. Add Q' to the list of Soughts.
;;;
;;;
;;;	    15. Add Q' to the list of quantities in E.
;;;
;;;	    16. Add E to the list of equations in Q'.
;;;
;;;
;;========================================================================
;; Parameters.
;;

(defparameter *Debug-gg* t
  "Print runtime debugging output for the Graph Generation process.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;; Bubblegraph generation follows a basic recursive model that is suprisingly 
;;; difficult to implement.  The search starts with a list of sought quantities
;;; each of these will be transformed into nodes and then searched for 
;;; recursively.  The recusrsive search will then identify the type of the 
;;; sought and take the appropriate action based upon it.
;;;
;;; If the sought quantity is already a member of the graph then that node will
;;; be marked as sought and the search will move on.  If the node is a 
;;; parameter then it will be marked as such and returned.  
;;; If it is A given then it will 
;;; be marked as such and its supporting given psm node will be added to the 
;;; graph and the resulting graph will be returned.
;;;
;;; Lastly and most likely if the node is a complex sought I.E. Requiring a 
;;; series of psms to solve then that recursive solution process will occur 
;;; and the resulting graph will be returned.  

(defun generate-bubblegraph (Soughts Givens &key (IgnorePSMS nil))
  (let ((graph) (Q) (qualpart-graph))

    (dolist (S Soughts)
      (cond ((not (quantity-expression-p S))
	     (format t "WARNING: Non quantity ~S being treated as qualitative part.~%" S)
	     ;; generate a bubblegraph as if this part were a non-quant 
	     ;; problem. Such a graph contains one pseudo enode per sought 
	     ;; goal,  distinguished by mark 'non-quant. 
	     ;; Then insert the (single) special enode for the current sought 
	     ;; from that result graph into the nodelist for the main graph. 
	     ;; Note we must take make sure these special enodes are not pruned
	     ;; from the graph later when dead-path equations are marked and 
	     ;; removed.
             (setq qualpart-graph (generate-no-quant-problem-graph (list S) 
								   Givens))
	     (setq Graph (add-nodes-to-bubblegraph 
			  Graph  (first (second qualpart-graph)))))
	    
	    ((setq Q (match-exp->qnode S Graph))
	     (push 'Sought (Qnode-Marks Q)))
	    
	    (t (setq q (gg-solve-for-sought S Givens :graph Graph 
					    :ignorepSMS IgnorePSMS))
	       (push 'Sought (Qnode-Marks (car Q)))
	       (setq Graph (cdr Q)))))

    (index-bubblegraph Graph)))


;;; This is the top level recursive call for the generator.  Given a 
;;; sought expression this will generate a Qnode for it of the 
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
;;; If the sought is a parameter then that parameter will be solved for 
;;; and the resulting qnode will be returned as well as the supplied graph
;;; with that qnode added.
;;;
;;; If the sought is a given or constant value then the qnode and its 
;;; associated psm node will be generated and added to the graph.  The 
;;; psm will then be returned along with its modified graph.
;;;
;;; Lastly if the sought can be solved for by psm chaining then the 
;;; code will call the qsolver and recursively attempt to solve for the
;;; quantity adding the resulting nodes to the supplied graph.
(defun gg-solve-for-sought (Sought Givens &key (Graph Nil) (IgnorePSMS nil))
  "Solve for the sought with the givens and optional Graph."
  (when (not (quantity-expression-p Sought))
    (Error "Non-quantity sought supplied ~S." Sought))
  ;; AW -- moved this case up so no trace output for preexisting sought
  (or (gg-solve-preexisting-sought Sought Graph)
    (progn
      (gg-debug-incdepth)
      (gg-debug "Finding eqns for ~S" Sought)
      (let ((R (or ; (gg-solve-preexisting-sought Sought Graph)
	           (gg-solve-param-sought Sought Givens Graph)
	           (gg-solve-given-sought Sought Givens Graph)
	           (gg-solve-constant-sought Sought Givens Graph)
	           (gg-solve-complex-sought Sought Givens Graph))))
      ;(gg-debug "Done finding eqns for: ~S" Sought)
      (gg-debug-decdepth)
      (when (null R)
          (error "No equations returned for ~S~%" Sought))
      R))))


;;;-------------------------------------------------------------------
;;; Preexisting soughts 
;;; If a qnode alreasy exists within a graph that matches the supplied
;;; sought quantity then return that qnode along with the graph itself.
(defun gg-solve-preexisting-sought (Sought Graph)
  (let ((Q (match-exp->qnode Sought Graph)))
    (when Q 
      ;(gg-debug "Sought already processed.")
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
			      (unify (cadr G) Sought)))
	   Givens))

(defun find-param-value (Quant Givens)
  "Find value specified for parameter Quant; NIL if none"
  ;; prop form is (parameter ?quant ?answer-var [?value])
  (fourth (find-sought-param Quant Givens))) ; safe if missing


;;;-------------------------------------------------------------------
;;; Solve givens.
;;; Given quantities are solved for by solving for a given-eqn goal 
;;; for the sought.  Once that has been done then the enode will be 
;;; connected to a qnode representing the sought and added to the 
;;; graph itself.  
;;;
;;; The enode id used a given eqn is an expression of the form
;;;     (given ?quantity ?value)
;;; as appears in the problem definition or working memory.
(defun gg-solve-given-sought (Sought Givens Graph)
  (let (P Q R)
      ;; make sure we can come up with an equation for this
      (Setq R (solve-for-given-eqn Sought Givens))
      (cond ((null R) (when (gg-find-matching-given Sought Givens)
                         (gg-debug "Sought is given, but cannot make given-eqn.")))
	    (t
	     (gg-debug "Sought is given, adding PSM ~S" (qsolres-id R)) 
	     (setq Q (make-qnode :exp Sought
				 :var (nth 1 (Qsolres-Nodes R))
				 :marks '(Given)))
	     (setq P (gg-qsolres->Enode R :ID (qsolres-id R) 
	                                :Algebra (Qsolres-algebra R)
					:marks 'Given :Qnodes Q))
	     (setf (qnode-eqns Q) (list P))
	     (cons Q (add-nodes-to-bubblegraph Graph Q P)))
	    )))

(defun gg-find-matching-given (Sought Givens)
  (find `(given ,Sought . ?rest) Givens :test #'unify))


;;;------------------------------------------------------------------------
;;; Solve constant sought
;;; Constants like givens consists of an enode and a qnode pair. 
;;; This function will solve for the sought similar to the way it
;;; solves for given quantities and will return the result if found.
(defun gg-solve-constant-sought (Sought Givens Graph)
  (let ((Q) (P) (R (solve-for-constant-quantity Sought Givens)))
    (when R
      (gg-debug "Sought is constant ~A" (caar (Qsolres-Nodes R)))
      (setq Q (make-qnode :exp Sought
			  :var (caar (Qsolres-nodes R))
			  :marks '(constant)))
      (Setq P (gg-qsolres->Enode R :Qnodes Q))
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
      ;; remove the ignore psms.
      (setq Results (gg-remove-ignore-psms IgnorePSMS Results))  
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
    (dolist (p Psms)
      (cond ((exp-of-psmtype-set? (qsolres-id P) Ignore)
	     (gg-debug "  Removing ignore-psm")
	     (gg-debug (format nil "~A.~%" (qsolres-id P))))
	    (t (gg-debug (format nil "  Got eqn ~S" (qsolres-id P)))
	       (push P R))))
    R))

;;; Once we have a complex sought node it is necessary to generate all of the 
;;; psms that are attached to it.  This function will cycle through the psms
;;; list that is provided to it.  (the contents of results from above) and
;;; test each PSM.  If the psm in question has already been generated then
;;; it will simply be added to the node.  Otherwize it will be generated
;;; recursively using gg-solve-complex-psm to produce the psm and will  add that
;;; to the updated graph.
(defun gg-collect-complex-psms (Psms Givens Qnode Graph)
  (let ((NewGraph Graph) (PSM))
    (dolist (P PSMs)
      (cond ((setq PSM (match-exp->enode (Qsolres-id P) Graph))
	     (pushnew PSM (Qnode-Eqns Qnode)))
	    (T
	     (setq PSM (gg-solve-complex-psm P Givens NewGraph))
	     (pushnew (car PSM) (Qnode-Eqns Qnode))
	     (setq NewGraph (cdr PSM))
	     )))
    (cons Qnode NewGraph)))


;;; Generate the an enode for the psm and add it to the graph.  After that
;;; recurisively solve for each of the quantities connected to the psm.  
;;; Add each of these quantities to the psm and return the result iff all 
;;; of them can be solved for.  If not return nil.
(defun gg-solve-complex-psm (Result Givens Graph)
  "Solve for the specified psm result."
  (let ((R) (PSM))
    (setq PSM (gg-qsolres->Enode Result))
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


;;; gg-qsolres->Enode
;;; Given a Qsolres it is necessary to generate an enode for it
;;; This function does so including setting up the subvars and
;;; subeqns properly and settng the contents as specified.
(defun gg-qsolres->Enode (R &key ID Algebra Path Marks 
			     Subvars Qnodes Assumpts)
  (let ((E (make-Enode :ID (or ID (Qsolres-ID R))
		       :Algebra (or Algebra (Qsolres-Algebra R))
		       :path (or Path (Qsolres-Path R))
		       :marks (force-to-list Marks) 
		       :subeqns (mapcar #'qsolres->eqn (qsolres-subeqns R))
		       :subvars (or Subvars (qsolres-subvars R))
		       :Qnodes (force-to-list Qnodes)
		       :assumptions (or Assumpts (qsolres-Assumpts R)))))
    ;; make back-pointer for each subeqn
    (dolist (S (Enode-subeqns E))
      (setf (eqn-nodes S) (list E)))
    ;;    (dolist (Q (Enode-SubVars E))
    ;;      (setf (Qvar-Nodes Q) (list E)))
    E))

(defun qsolres->eqn (S)
  (make-eqn :Type (first S)
	    :Algebra (second S)
	    :Exp (third S)))

;;;============================================================================
;;; Debug code.

(defun gg-debug (format &rest stuff)
  (when *Debug-gg* 
    (format t "~A ~A:  ~?~%" (print-outline-indent 1) *gg-debug-depth*
	    format stuff)))

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

