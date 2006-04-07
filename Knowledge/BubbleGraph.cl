;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BubbleGraph.cl
;; Collin Lynch
;; 2/7/2001
;;
;; Andes2 problem solutions are represented at two levels.  The top level
;; and the one at which the solutions are built is the bubblegraph level.  
;; Bubblegraphs are a partially connected graph of two node types Qnodes
;; and Enodes.  Each type is connected soley to the other.  Solutions are
;; gathered by traversing the edges of the graphs and obtaining a partial
;; coverage consisting of qnodes and enodes.  This coverage is determined
;; by rules located in other packages.  The purpose of this package is
;; simply to expose the bubblegraph structures for appropriate use.
;; 
;; Each Node type has a set of value fields and assorted functions 
;; associated with it such as predicates and marking functions.  There are
;; also functions for manipulating bubblegraphs as a whole and for dealing 
;; with nodes independent of their type.  I'll document these below.
;;
;; The Bubblegraph api is as follows:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public constants
;; **Dead-Path**      -- Marking for dead path nodes.
;;                       Defined in HelpStructs/SystemEntry.cl
;; **Optimal-Path**   -- Marking for the optimal path nodes.
;; **Parameter**      -- Marking for the param nodes.
;; **Answer-Var**     -- Marking for answer-var params.
;; **Cancelling-Var** -- Cancelling-var param markings.
;; **Sought**         -- Marking for the sought node(s).
;; **Given**          -- Marking for Given Qnodes and Enodes.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Qnode structure.
;;; The qnode structure describes a knowledge-base quantity such as
;;; '(duration (during 1 2))'  In a problem solution the qnodes represent 
;;; sought intermediate and given quantities and are conected to several 
;;; different enodes.  At Bubblegraph constructiontime the qnodes are used to 
;;; chain from one enode to the next.  At help or student solution time they
;;; represent final or intermediate goals that the students will be seeking 
;;; as necessary.
;;;
;;; The qnode is essentially a public structure.  All of its fields can be
;;; accessed directly by any code and no locking exists to prevent changes
;;; once they have been established.  If I were to re-encode this in CLOS
;;; I would make the Exp, Var and possibly path fields read-only after the
;;; initial creation.  The Gindex and Vindex values would need to be more
;;; flexible but only for specified situations.  I would place those under
;;; wraps in a master bgnode structure.  
;;;
;;; In any case all of the standard structure functions can be safely used 
;;; on qnodes.  In addition there are several modification functions and 
;;; predicates designed to facilitate access and testing.  Whenever possible
;;; use these functions if they can be because they will facilitate use.
;;;
;;; Qnode Fields:
;;;  Symbol (gensym "Qnode") ; Unique Gensym symbol for identification.
;;;  Gindex		     ; Integer index of the symbol in graph list.
;;;  VIndex		     ; The correxponding qvar problem's varindex.
;;;  Exp		     ; Lisp Expression representing this quantity.
;;;  var		     ; The Expression symbol used in the solver eqns.
;;;  Eqns		     ; Equation nodes containing this quantity.
;;;  Marks		     ; Markings added to the node for the help system.
;;;  path		     ; Optional path that may be used for data storage.
;;;  Assumptions	     ; Assumptions made in the path.
;;;  Entries                 ; List of entry props that are in this node's psm.
;;;
;;;---------------------------------------------------------------------------
;;; Qnode output functions:
;;;
;;; (form-print-qnode <Qnode> &optional <form (def nil)> <Stream> <level>) 
;;; -- Print out the qnode in the specified form (Graph, Full, Mreadable, 
;;;    and minimal (the default)).
;;;
;;; (print-Qnode <Quantity> &optional <Stream> <Level>)
;;;    Print the qnode in a minimal form.
;;;
;;;---------------------------------------------------------------
;;; Qnode reading code.
;;;
;;; (read-mreadable-qnode-lst <Stream>)  -- Read in a list of qnodes.
;;; (read-mreadable-qnode <Stream>)      -- read in a single qnode.
;;;
;;;------------------------------------------------------------------
;;; Qnode Testing
;;; Comparison functions for relating two qnodes.
;;; 
;;; (qnodes-equalp <Qnode> <Qnode>) -- Test for qnode equality.
;;; (qnode-solvedp <Qnode>)  -- Test if the qnode has a matching solved qvar.
;;; (qnode-contains-entryp <Entry> <Node>) 
;;;        -- search for systementry in Qnode-Entries.
;;;
;;;------------------------------------------------------------------
;;; Qnode Markings.
;;; Individual qnodes can have symbols added to their marks field.  
;;; The code in this section is intended to provide a wrapper to that
;;; field for modification and testing purposes and is indended for 
;;; public use.
;;;
;;; (Qnode-Dead-pathp <Qnode>)          - Return t iff qnode is **Dead-Path**
;;;
;;; (Add-qnode-sought-mark <Qnode>)     - Mark the qnode as **Sought**.
;;; (Qnode-soughtp <Qnode>)             - Returns t iff qnode is **Sought**.
;;;
;;; (mark-qnode-parameter <Qnode>)      - Mark the qnode as a parameter.
;;; (Qnode-Parameterp <Qnode>)          - Is qnode marked **Parameter**?
;;;
;;; (mark-qnode-answer-var <Qnode>)     - Mark the qnode as an **Answer-Var**
;;; (Qnode-Answer-VarP <Qnode>)         - Returns t iff Qnode is **Answer-Var**
;;;
;;; (mark-qnode-optimal-path <Qnode>)   - Mark QNODE as **Optimal-Path**
;;; (Qnode-optimal-pathP <Qnode>)       - Is Qnode marked **Optimal-Path**.
;;;
;;; (mark-qnode-cancelling-var <Qnode>) - Mark the qnode as **Cancelling-Var**
;;; (Qnode-cancelling-varp <Qnode>)     - Is qnode **cancelling-var**?
;;;
;;; (Qnode-Givenp <Qnode>)              - Is qnode **Given**?
;;;
;;; (mark-qnode <Mark> <Qnode>)         - Add MARK to QNODE's markings
;;; (remove-qnode-mark <Mark> <Qnode>)  - Remove MARK from QNODE if present.
;;; (Qnode-Has-Mark <Qnode> <Mark>)     - Return t iff QNODE has MARK.
;;;
;;;======================================================================
;;; Enode (Equation Node | PSM Node | Method Node)
;;; The Enodes define specific applications of psms or methods by the
;;; solver or student to find a sought quantity in terms of other
;;; quantities.  Each Enode is defined by it's top-level expression and
;;; the equation that it defines and the path of systementries and 
;;; goals located below it.  
;;;
;;; Each Enode is conneted to a set of qnodes.  Each of these qnodes 
;;; corresponds to a variable quantity in the equation.  The assumption
;;; of the system is that this equations is linear.  I.E. any one var 
;;; can be solved for by using the remaining vars.  
;;;
;;; Attatched to each enode a psm path.  This consists of a tree of 
;;; individual steps and soughts as generated by the Qsolver code.
;;; The purpose of these paths is to represent the set of steps
;;; that the student or solver must make in the workbench to produce
;;; the equation itself.  This can include body drawing, variable 
;;; definitions, vector and axes drawings and equation entries.
;;;
;;; At graph generation time the enodes represent the differing methods
;;; of solving for each individual quantity and connects from one 
;;; quantity to the next.  At Help time these represent the set of 
;;; major methods or psms that we want the student to break down the 
;;; problem solution into.  
;;;
;;; (Symbol (gensym "Enode")) ; Unique Gensym symbol.
;;; ID			      ; The Equation's Lisp Equation ID
;;; GIndex		      ; The index of this node in the Bubblegraph.
;;; EIndex		      ; The index within the graphs equations list.
;;; algebra		      ; Equation algebra.
;;; Path		      ; The solution path used to derive this equation.
;;; Assumptions	              ; Assumptions made in this Enode.
;;; Qnodes		      ; The quantities included in this equation.
;;; Marks		      ; Similar to quantity marks.
;;; Subeqns		      ; The subequations present in Enode's Paths.
;;; subvars		      ; The subvars present in the psm.
;;; State		      ; A marking such as Correct forbidden, etc.
;;;			      ;  or Inefficient. Not used until help time.
;;; Entries		      ; EntryProps that appear in this node's path.
;;;
;;;---------------------------------------------------------------------------
;;; Enode Output functions.
;;; (form-print-enode <Enode> &optional <form> <Stream> <level>)
;;;      Print out the enode in the specified form (full, graph, 
;;;      mreadable and minimal (the default).
;;;
;;; (print-Enode <Equation> &optional <Stream> <level>)
;;;      Print the enode in minimal form.
;;;
;;;-----------------------------------------------------------------------------
;;; Enode input functions.
;;; (read-mreadable-enode-lst <Stream>)  -- Read in a list of enodes from STREAM.
;;; (read-mreadable-Enode <Stream>)      -- Read in an enode from STREAM.
;;;
;;;---------------------------------------------------------------------
;;; enode-utilities
;;;
;;; (enodes-equalp <Enode1> <Enode2>) -- Test for all except path equality.
;;; (Enode-contains-entryp <Entry> <Node>) 
;;;        -- search for a systementry in Enode-Entries.
;;; (Enode-solvedp <Enode>)  
;;;        -- Test if the corresponding eqn for enode exists and is solved.
;;;
;;;---------------------------------------------------------------------
;;; Markings tests and modifications.
;;;
;;; (add-enode-mark <Enode> <mark> &optional (test #'equal))    -- Add MARK to ENODE.
;;; (remove-enode-mark <Enode> <mark> &optional (test #'equal)) -- Remove MARK from ENODE.
;;; (Enode-has-mark? <Enode> <mark> &optional (test #'equal))   -- Test for MARK on ENODE.
;;;
;;; (Enode-dead-pathp <Enode>)            -- Test if ENODE is **Dead-Path**
;;;
;;; (Enode-optimal-pathp <Enode>)         -- Test if Enode is **Optimal-Path**
;;;
;;; (Enode-forbiddenp <Enode>)            -- Test if Enode is **Forbidden**
;;;
;;; (Enode-givenp <Enode>)                -- Test if Enode is **Given**
;;;
;;;=============================================================================
;;; BubbleGraph Nodes
;;; Allthough Qnodes and enodes are employed at distinct times it is sometimes
;;; necessary to access them both as if they were a common item.  The functions
;;; in this section are used to perform such tasks.
;;; 
;;; (bgnode-p <Node>) -- t iff <Node> is a qnode or Enode.
;;; (form-print-node <Node> &optional <form>)  -- Form print an arbitrary node.
;;; (bgnode-dead-pathp <Node>)    -- Test for <Node_Type>-dead-pathp
;;; (bgnode-entries <Node>)       -- <Node_Type>-Entries.
;;; (bgnode-exp <Node>)           -- Qnode-Exp or Enode-ID (by type).
;;; (bgnode-path <Node>)          -- Get the bgnode's path.
;;; (bgnode-gindex <Node>)        -- Get the bgnode's index.
;;; (bgnode-links <Node>)         -- Get the qnodes connected to this enode
;;;                                  or enodes connected to this qnode.
;;;
;;; (bgnodes-equalp <Node> <Node>) -- Return t iff the two nodes are equalp.
;;;
;;;============================================================================
;;; Bubblegraph
;;; A Bubblegraph consists of a collection of interconnected Qnodes and Enodes
;;; that represents a full or partial solution to a specific problem.  The 
;;; problem is defined as a collection of sought and given values that are
;;; passed successiely to the quantity solver code for its use.  
;;;
;;; If the bubblegraph represents one or more complete solutions then there 
;;; will be a qnode for each sought quantity and a complete path from each
;;; sought to a complete set of solved qnodes.  
;;;
;;; The form of a bubblegraph is a list of two lists.  The first list is the
;;; set of quantity nodes and the second is the set of Enodes.  The code in 
;;; this section is setup for manipulating bubblegraphs as a whole.
;;;
;;; The fact that the bubblegraph is defined as a struct here is for code
;;; cleanliness.  A great deal of the running code still accesses the graph
;;; using car and cadr so it must, for now remain defined as a list as well.
;;; 
;;; bubblegraph Fields:
;;;   qnodes 
;;;   enodes
;;;
;;;----------------------------------------------------------------------------
;;; Bubblegraph Output functions.
;;; Output the graph for human-readable and Machine readable purposes.
;;;
;;; (print-Bubblegraph <Graph> &optional <form> <Stream> <Level>)
;;;        Print out the bubblegraph in one of the specified types
;;;        (Graph or Mreadable.)
;;;
;;; (form-print-bgnodes <Nodes> &optional <form> <Stream> <Level>)
;;;        Print out the nodes in the specified form. (Graph, Full, or Mreadable)
;;;
;;;-------------------------------------------------------------------
;;; Bubblegraph Input functions
;;; The functions in this section are devoted to reading in a
;;; bubblegraph from an mreadable stream form and re-creating it.
;;;
;;; (read-mreadable-bubblegraph <Stream>)  -- read in a Bubblegraph.
;;; (regen-bg-vindex-links <Graph> <Vindex>) -- Regenerate the qvar links in the graph nodes.
;;; (regen-bg-Eindex-links <Graph> <Eindex>) -- Regenerate the eqn index links in the graph nodes.
;;;
;;;-----------------------------------------------------------------------
;;; Graph modification code.
;;; The functions in this section are used to generate and modify graphs.
;;; This code allows you to treat graphs as distinct objects.
;;; 
;;; (make-bubblegraph &optional <BGNodes>) -- generate a new bubblegraph of the specified BGNodes.
;;; (add-nodes-to-bubblegraph <Graph> &rest <Nodes>) -- Add to the bubblegraph.
;;;
;;;----------------------------------------------------------------------
;;; Bubblegraph comparison.
;;; In comparing two problem solutions it is necessary to test
;;; for the equality of two or more bubblegraphs.  This function will
;;; test for equality of the graphs bu ensuiring that for each unique 
;;; node in the graph there exists a comprable node in the other graph
;;; and that the two comprable nodes are equalp.
;;;
;;; (defun bubblegraphs-equalp <Graph1> <Graph2>)
;;;
;;;--------------------------------------------------------------------------
;;; Graph searching code
;;; Given a bubblegraph these functions allow for searching and extraction
;;; of nodes or subsets of the graph.
;;;
;;; (get-nth-bgnode <#> <Graph>) -- Get the nth element (by index) in the graph.
;;; (map-indicies->bgnodes <Indicies> <Graph>)  -- Map the specified indicies to bgnodes. 
;;; (collect-unsolved-bgnodes <Graph>)  -- Collect unsolved enodes and qnodes.
;;; (collect-sought-qnodes <Graph>)  -- Collect the soughts.
;;; (collect-forbidden-bgnodes <Graph>) -- Collect all nodes marked **Forbidden**
;;; 
;;; (match-exp->qnode <exp> <Graph>)  -- lookup a qnode matching exp.
;;; (match-exp->enode <exp> <Graph>)  -- lookup an enode matching exp.
;;; (match-exp->bgnode <Exp> <Graph>) -- Lookup a qnode or enode matching exp.
;;;
;;; (collect-bgnodes->assumpts <Nodes>) -- Collect the assumptions from each bgnode.
;;;
;;; (collect-predicate-qnodes <Graph>) -- Collect the qnodes that match a predicate.
;;; (collect-predicate-enodes <Graph>) -- Collect the enodes that match a predicate.
;;; (collect-predicate-bgnodes <Graph>) -- Collect the bgnodes that match a predicate.
;;;
;;; Collect the approprate nodes with MARK.
;;; (collect-marked-qnodes (Mark Graph &key (test #'equal))
;;; (collect-marked-enodes (mark Graph &key (test #'equal))
;;; (collect-marked-bgnodes (Mark Graph &key (test #'equal)))
;;; 
;;; Collect the appropriate nodes without MARK.
;;; (collect-unmarked-enodes (mark Graph &key (test #'equal)))
;;; (collect-unmarked-qnodes (Mark Graph &key (test #'equal)))
;;; (collect-unmarked-bgnodes (Mark Graph &key (test #'equal)) 
;;;
;;; Collect the path(s) that link the start node to end node.
;;; (collect-bubblegraph-paths (Start End Graph))
;;;
;;;----------------------------------------------------------------------------
;;; mapping functions can be applied across the qnodes and enodes.
;;;
;;; (mapcar-bubblegraph-qnodes <Func> <Graph>) -- Map the specified func to the qnodes.
;;; (mapcar-bubblegraph-enodes <Func> <Graph>) -- Map the specified func to the enodes.
;;; (mapcar-bubblegraph-bgnodes <Func> <Graph>) -- Map the specified func to the bgnodes.
;;;
;;; (mapcan-bubblegraph-qnodes <Func> <Graph>) -- Map the specified func to the qnodes.
;;; (mapcan-bubblegraph-enodes <Func> <Graph>) -- Map the specified func to the enodes.
;;; (mapcan-bubblegraph-bgnodes <Func> <Graph>) -- Map the specified func to the bgnodes.
;;;--------------------------------------------------------------------
;;; Bubblegraph dead-path code
;;; Dead-path nodes are those nodes within the bubblegraph that do not 
;;; appear in any solution or in the case of marking in the UsedNodes 
;;; that are provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mark-bg-dead-paths
;;; mark the dead path nodes within the bubblegraph by adding 'dead-path
;;; to all unused nodes.  The unused nodes as defined in the Used-Nodes 
;;; list.
;;;
;;; (mark-bg-dead-path-nodes <Graph> <UsedNodes>) 
;;;      -- mark all the nodes not in UsedNodes as **dead-path**
;;;
;;; (remove-bg-dead-path-nodes <Graph>)
;;;      -- Delink and remove all **Dead-Path** nodes from the bubblegraph
;;;-------------------------------------------------------------------------
;;; Graph Indexing code.
;;; The Gidicies of the bubblegraph are numbers indicating simply the order 
;;; in which the nodes appear in the graph.  The code in this section
;;; assigns those index values to the elements in the graph.
;;;
;;; (index-bubblegraph <Graph>)  -- Generate the graph indicies.
;;;
;;;-------------------------------------------------------------
;;; Variable Index. 
;;; Once a problem has been solved it is necessary to generate 
;;; the variable index.  This necessitates collecting all of
;;; the variables mentioned within the graph into a single
;;; list accounting for duplicates.  The variables within qnodes
;;; and enodes are already located within the subvars locations
;;; this code extracts those, merges the duplicates and updates
;;; the graphs themselves.
;;; 
;;; (defun generate-bg-vindex <Graph>) -- Generate a varindex from GRAPH.
;;;
;;;-------------------------------------------------------------------------
;;; Equation Index.
;;; In the process of solving a problem, just after the graph has 
;;; been generated, it is necessary to generate the equation index
;;; this involves cycling through the graph collecting the equations
;;; from each node's subeqns and the top-level elements of the enodes,
;;; merging duplicate equations and returning the results.
;;;
;;; (defun generate-bg-eindex <Graph>) -- Generate an eqnindex from GRAPH.
;;;
;;; 
;;; ===========================================================================
;;; Changelog.
;;; 6/4/2002 --- Added functions and updated to deal with non-quantity problems.
;;;    New:  form-print-bgnodes, regen-bg-vindex-links, regen-bg-eindex-links
;;;          make-bubblegraph, collect-marked-*nodes, collect-unmarked-*nodes
;;;          add-enode-to-bubblegraph, add-qnode-to-bubblegraph.


;;;============================================================================
;;; Marking parameters
;;; These definitions might be replicated in other locations but the purpose
;;; of them here is to centralize code location.  The system is intended to 
;;; answer questions about the graph and these definitions facilitate that.
(defconstant **Optimal-Path** 'Optimal-Path "Optimal markings.")
(defconstant **Parameter** 'Parameter "The specified qnode is a parameter.")
(defconstant **Sought** 'Sought "Sought quantity marking.")
(defconstant **Answer-Var** 'Answer-Var "Sought qnode is an answer var.")
(defconstant **Cancelling-Var** 'Cancelling-Var "Sought qnode is a cancelling-var.")
(defconstant **Nogood** 'Nogood "Specified nodes are nogood (depreciated).")
(defconstant **Given** 'Given "The specified node is Given in the problem.")

;;;============================================================================
;;; Qnodes
;;; The wnose structure describes a knowledge-base quantity such as
;;; '(duration (during 1 2))'  In a problem solution the qnodes represent 
;;; sought intermediate and given quantities and are conected to several 
;;; different enodes.  At Bubblegraph constructiontime the qnodes are used to 
;;; chain from one enode to the next.  At help or student solution time they
;;; represent final or intermediate goals that the students will be seeking 
;;; as necessary.
;;;
;;; The qnode is essentially a public structure.  All of its fields can be
;;; accessed directly by any code and no locking exists to prevent changes
;;; once they have been established.  If I were to re-encode this in CLOS
;;; I would make the Exp, Var and possibly path fields read-only after the
;;; initial creation.  The Gindex and Vindex values would need to be more
;;; flexible but only for specified situations.  I would place those under
;;; wraps in a master bgnode structure.  
;;;
;;; In any case all of the standard structure functions can be safely used 
;;; on qnodes.  In addition there are several modification functions and 
;;; predicates designed to facilitate access and testing.  Whenever possible
;;; use these functions if they can be because they will facilitate cleanliness.

(defstruct (Qnode (:print-Function Print-Qnode))
  (Symbol (gensym "Qnode"))		; Unique Gensym symbol for identification.
  Gindex				; Integer index of the symbol in graph list (may supersede symbol.)
  VIndex				; Index of the variable in the varlist corresponding to this qnode.
  Exp					; Lisp Expression representing this quantity.
  var					; The Expression symbol used in the solver equations.
  Eqns					; Equation nodes containing this quantity.
  Marks					; Markings added to the node for help system purposes.
  path					; Optional path that may be used for data storage.
  Assumptions				; Assumptions made in the path.
  Entries                               ; List of entry props that are in this node's psm.
  )


;;---------------------------------------------------------------------------
;; Qnode Output code.

(defun form-print-qnode (Qnode &optional (form nil) (Stream t) (level 0))
  "Print the Qnode in the specified form."
  (case form
    (full (print-full-qnode Qnode Stream level))
    (graph (print-graph-qnode Qnode Stream level))
    (mreadable (print-mreadable-qnode Qnode Stream))
    (t (print-qnode Qnode Stream Level))))


(defun print-Qnode (Quantity &optional (Stream t) (Level 0))
  "Print out the specified quantity node."
  (pprint-indent :block Level Stream)
  (format Stream "<Q: ~A>" (Qnode-Exp Quantity)))


(defun print-full-qnode (Quantity &optional (Stream t) (Level 0))
  "Print the Qnode in full form."
  (pprint-indent :block Level Stream)
  (format Stream "<Quantity-Node: ~A " (Qnode-gindex Quantity))
  (if (Qnode-vindex Quantity)
      (format Stream "~A " (Qvar-Index (Qnode-vindex Quantity)))
    (format Stream "~A " Nil))
  (format Stream "~A~%" (Qnode-Exp Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Variable: ~A~%" (Qnode-var Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Equations: ~A~%" (Qnode-Eqns Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Marks: ~A~%" (Qnode-Marks Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  path: ~%")
  (fprint (Qnode-Path Quantity) Stream)
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Assumptions: ~A~%" (Qnode-Assumptions Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Entries: ~A>~%" (Qnode-Entries Quantity)))
  

(defun print-graph-qnode (Quantity &optional (Stream t) (Level 0))
  "Print out the Qnode in graph form."
  (pprint-indent :block Level Stream)
  (format Stream "<Quantity-Node: ~A " (Qnode-gindex Quantity))
  (if (Qnode-vindex Quantity)
      (format Stream "~A " (Qvar-Index (Qnode-vindex Quantity)))
    (format Stream "~A " Nil))
  (format Stream "~A~%" (Qnode-Exp Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Variable: ~A~%" (Qnode-Var Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Equations: ~A~%" (Qnode-Eqns Quantity))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Marks: ~A~%" (Qnode-Marks Quantity)))  

(defun print-mreadable-qnode (Q &optional (S t))
  "Dump the specified Machine-readable Qnode Q to stream S."
  (format S "<Qnode>~%")
  (format S "~W~%" (Qnode-Symbol Q))
  (format S "~W~%" (Qnode-gindex Q))
  (format S "~W~%" (Qvar-Index (Qnode-vindex Q)))
  (format S "~W~%" (Qnode-Exp Q))
  (format S "~W~%" (Qnode-Var Q))
  (format S "~W~%" (collect-enodes->gindicies (Qnode-Eqns Q)))
  (format S "~W~%" (Qnode-Marks Q))
  (format S "~W~%" (qnode-path Q))
  (format S "~W~%" (Qnode-Assumptions Q))
  (format S "~W~%" (Qnode-Entries Q))  
  (format S "</Qnode>~%"))
  



;;---------------------------------------------------------------
;; Qnode reading code.
(defun read-mreadable-qnode-lst (S)
  "Read in an mreadable list of Qnodes."
  (mg-srt S '<Qnodes>)  
  (do (N R)
      ((null (setq N (read-mreadable-qnode S))) R)
    (setq R (append R (list N)))))

(defun read-mreadable-qnode (S)
  "Read an Mreadable qnode from the stream."
  (when (not (equal (mg-srt2 S '<Qnode> '</Qnodes>) '</Qnodes>))
    (let ((N (make-qnode :Symbol (mg-sr S)
			 :gindex (mg-sr S)
			 :vindex (mg-sr S)
			 :Exp (mg-sr S)
			 :var (mg-sr S)
			 :Eqns (mg-sr S)
			 :marks (mg-sr S)
			 :path (psmg->help-psmg (mg-sr S))
			 :assumptions (mg-sr S)
			 :Entries (mg-sr S))))
      
      (mg-srt S '</Qnode>)
      N)))


;;------------------------------------------------------------------
;; Qnode comparison.
;; Comparison functions for relating two qnodes.
(defun qnodes-equalp (Q1 Q2)
  "Qnode equalp is tested for contents but ignores the path."
  (and (unify (qnode-Exp Q1) (Qnode-Exp Q2)) ;could just as well use "equal"
       (unify (qnode-Var Q1) (Qnode-Var Q2))
       (equal-sets (Qnode-Marks Q1) (Qnode-Marks Q2))
       (equal-sets (Qnode-Assumptions Q1) (Qnode-Assumptions Q2))
       (equal-sets (mapcar #'Enode-ID (Qnode-eqns Q1))
		    (mapcar #'Enode-ID (Qnode-eqns Q2)))))


;;; Each Qnode is connected to a set qvar which is generated
;;; when the problem solutionpoint is generated.  If this 
;;; qvar (stored in the qnode-vindex field) is labelled
;;; as solved then the Qnode is considered as solved.
(defun qnode-solvedp (Qnode)
  "Is the specified Qnode Solved?"
  (and (Qnode-Vindex Qnode)
       (Qvar-Value (Qnode-Vindex Qnode))))


;;; Qnodes contain SystemEntries within their Entries fields
;;; these entries are searched to identify the source when neccessary.
(defun qnode-contains-entryp (Entry Node)
  "Does the quantity node contain the specified entry."
  (member Entry (Qnode-Entries Node) :test #'unify)) ;could use "equal"


;;------------------------------------------------------------------
;; Qnode Markings.
;; Individual qnodes can have symbols added to their marks field.  
;; The code in this section is intended to provide a wrapper to that
;; field for modification and testing purposes and is indended for 
;; public use.


;; Return t iff the qnode is marked as **Dead-Path**
(defun qnode-dead-pathp (Q)
  "Is the Qnode on a dead path?"
  (qnode-has-mark? Q **dead-path**))


;; (Add-qnode-sought-mark <Qnode>) Mark the qnode as **Sought**.
(defun mark-qnode-sought (Q)
  "Mark the Qnode as sought."
  (mark-qnode Q **Sought**))

  
;;; (Qnode-soughtp <Qnode>) Returns t iff the qnode is marked as **Sought**.
(defun qnode-soughtp (Q)
  "Is the specified qnode sought?"
  (qnode-has-mark? Q **Sought**))


;;; (mark-qnode-parameter <Qnode>) Mark the qnode as a parameter.
(defun mark-qnode-parameter (Q)
  "Mark the qnode as a Parameter."
  (mark-qnode Q **Parameter**))
   
;;; (Qnode-Parameterp <Qnode>) Return t iff the qnode is marked **Parameter**.
(defun qnode-parameterp (Q)
  "Is the Qnode a parameter?"
  (qnode-has-mark? Q **Parameter**))


;;; (mark-qnode-answer-var <Qnode>) Mark the specified qnode as an answer-var.
(defun mark-qnode-answer-var (Q)
  "Mark the qnode as an answer-var."
  (mark-qnode Q **Parameter**)
  (mark-qnode Q **Answer-Var**))

;;; (Qnode-Answer-VarP <Qnode>) 
;;; Returns t iff Qnode has the **Answer-Var** mark.
(defun qnode-answer-varp (Q)
  "Is the Qnode an answer-var?"
  (qnode-has-mark? Q **Answer-Var**))


;;; (mark-qnode-optimal-path <Qnode>) Mark QNODE as **Optimal-Path**
(defun mark-qnode-optimal-path (Q)
  "Mark the qnode as an optimal path."
  (mark-qnode Q **Optimal-Path**))

;;; (Qnode-optimal-pathP <Qnode>) Returns t iff Qnode is marked **Optimal-Path**.
(defun qnode-optimal-pathp (Q)
  "Is the qnode a member of the optimal path?"
  (qnode-has-mark? Q **Optimal-path**))



;;; (mark-qnode-cancelling-var <Qnode>) Mark the qnode as a cancelling-var.
(defun mark-qnode-cancelling-Var (Q)
  "Mark the qnode as a cancelling var."
  (mark-qnode Q **Parameter**))

;;; (Qnode-cancelling-varp <Qnode>) Return t iff the qnode is a cancelling var.
(defun qnode-cancelling-varp (Q)
  "Is the qnode a cancelling-var?"
  (and (qnode-parameterp Q)
       (not (qnode-answer-varp Q))))

(defun qnode-Givenp (Q)
  "Is the qnode a given?"
  (Qnode-has-mark? Q **Given**))

(defun qnode-optionally-Givenp (Q)
  "Is the qnode optionally given?"
  (Qnode-has-mark? Q 'optionally-given))

;;; (mark-qnode <Mark> <Qnode>) Add MARK to QNODE's markings
(defun mark-qnode (mark Qnode)
  "Add the specified Marking to the specified qnode."
  (pushnew Mark (Qnode-Marks Qnode)))

;;; (remove-qnode-mark <Mark> <Qnode>) 
;;; Destructively remove MARK from QNODE if present.
(defun remove-qnode-mark (mark Qnode)
  "remove the specified mark (if-present) from the qnode's marks."
  (setf (Qnode-Marks Qnode)
    (remove-if Mark (Qnode-Marks Qnode))))

;;; (Qnode-Has-Mark <Qnode> <Mark>) Return t iff QNODE has MARK.
(defun qnode-has-mark? (Qnode Mark &key (test #'equal))
  "Does the specified qnode contain the marking?"
  (find Mark (Qnode-Marks Qnode) :test test))




;;======================================================================
;; Enode (Equation Node | PSM Node | Method Node)
;; The Enodes define specific applications of psms or methods by the
;; solver or student to find a sought quantity in terms of other
;; quantities.  Each Enode is defined by it's top-level expression and
;; the equation that it defines and the path of systementries and 
;; goals located below it.  
;;
;; Each Enode is conneted to a set of qnodes.  Each of these qnodes 
;; corresponds to a variable quantity in the equation.  The assumption
;; of the system is that this equations is linear.  I.E. any one var 
;; can be solved for by using the remaining vars.  
;;
;; Attatched to each enode a psm path.  This consists of a tree of 
;; individual steps and soughts as generated by the Qsolver code.
;; The purpose of these paths is to represent the set of steps
;; that the student or solver must make in the workbench to produce
;; the equation itself.  This can include body drawing, variable 
;; definitions, vector and axes drawings and equation entries.
;;
;; At graph generation time the enodes represent the differing methods
;; of solving for each individual quantity and connects from one 
;; quantity to the next.  At Help time these represent the set of 
;; major methods or psms that we want the student to break down the 
;; problem solution into.  

(defstruct (Enode (:Print-function Print-Enode))
  (Symbol (gensym "Enode"))		; Unique Gensym symbol.
  ID					; The Equation's Lisp Equation ID
  GIndex				; The index of this node in the Bubblegraph.
  EIndex				; The index within the graphs equations list.
  algebra				; Equation algebra.
  Path					; The solution path used to derive this equation.
  Assumptions				; Assumptions made in this Enode.
  Qnodes				; The quantities included in this equation.
  Marks					; Markings for this equation similar to quantity marks.
  Subeqns				; The subequations present in all of the paths of this PSM.
  subvars				; The subvars present in the psm.
  State					; One of: Correct, Incorrect, Forbidden, Dead-path 
					;  or Inefficient. Not used until help time.
  Entries				; EntryProps that appear in this node's path.
  )


;;---------------------------------------------------------------------------
;; Enode Output functions.
(defun form-print-enode (Enode &optional (form nil) (Stream t) (level 0))
  "Print the enode in the specified form."
  (case form
    (full (print-full-enode Enode Stream level))
    (graph (print-graph-enode Enode Stream level))
    (mreadable (print-mreadable-enode Enode Stream))
    (t (print-enode Enode Stream Level))))

(defun print-Enode (Equation &optional (Stream t) (level 0))
  "Print out the specified equation node."
  (pprint-indent :block Level Stream)
  (format Stream "<Eq:  ~A ~A>" 
	  (if (Enode-Eindex Equation)
	      (eqn-index (Enode-eindex equation)) nil) 
	  (Enode-ID Equation)))


(defun print-full-enode (Equation &optional (Stream t) (Level 0))
  "Print the Enode In full viewing form."
   (pprint-indent :block Level Stream)
   (format Stream "<Equation-Node: ~A " (Enode-gindex Equation))
   (if (Enode-Eindex Equation)
      (format Stream "~A " (Eqn-Index (Enode-Eindex Equation)))
     (format Stream "~A " Nil))
   (format Stream "~A~%" (Enode-ID Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Algebra: ~A~%" (Enode-Algebra Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Path: ~%")
   (fprint (Enode-Path Equation) Stream)
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Assumptions: ~A~%" (Enode-Assumptions Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Quantities: ~A~%" (Enode-Qnodes Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Marks: ~A~%" (Enode-Marks Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  State: ~A~%" (Enode-State Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Subeqns: ~A~%" (Enode-Subeqns Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Subvars: ~A~%" (Enode-Subvars Equation))
   (pprint-indent :block (+ Level 1) Stream)
   (format Stream "  Entries: ~A>~%" (Enode-Entries Equation)))

(defun print-graph-enode (Equation &optional (Stream t) (Level 0))
  "Print the equation node in graph form."
  (pprint-indent :block Level Stream)
  (format Stream "<Equation-Node: ~A " (Enode-gindex Equation))
  (if (Enode-Eindex Equation)
      (format Stream "~A " (Eqn-Index (Enode-Eindex Equation)))
    (format Stream "~A " Nil))
  (format Stream "~A~%" (Enode-ID Equation))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Algebra: ~A~%" (Enode-Algebra Equation))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Quantities: ~A~%" (Enode-Qnodes Equation))
  (pprint-indent :block (+ Level 1) Stream)
  (format Stream "  Marks: ~A~%" (Enode-Marks Equation)))
    
(defun print-mreadable-enode (E S)
  "Dump the specified Machine-readable Qnode Q to stream S."
  (format S "<Enode>~%")
  (format S "~W~%" (Enode-Symbol E))
  (format S "~W~%" (Enode-ID E))
  (format S "~W~%" (Enode-GIndex E))
  (format S "~W~%" (if (Enode-Eindex E) (Eqn-Index (Enode-eIndex E)))) 
  (format S "~W~%" (Enode-Algebra E))
  (format S "~W~%" (Enode-Path E))
  (format S "~W~%" (Enode-Assumptions E))
  (format S "~W~%" (if (Enode-Qnodes E) (collect-qnodes->gindicies 
					 (Enode-Qnodes E))))
  (format S "~W~%" (Enode-Marks E))
  (format S "~W~%" (Enode-State E))
  (format S "~W~%" (Enode-Entries E))  
  (format S "~W~%" (if (Enode-Subeqns E) (mapcar #'Eqn-Index 
					  (Enode-Subeqns E))))
  (format S "~W~%" (if (Enode-subvars E) (mapcar #'Qvar-Index 
					  (Enode-Subvars E))))
  (format S "</Enode>~%"))

;;------------------------------------------------------------------
;; Enode input functions.
(defun read-mreadable-enode-lst (S)
  "Read in a list of mredable enodes."
  (mg-srt S '<Enodes>)
  (do (N R)
      ((null (setq N (read-mreadable-Enode S))) R)
    (setq R (append R (list N)))))


(defun read-mreadable-Enode (S)
  "Read an Mreadable qnode from the stream."
  (when (not (equal (mg-srt2 S '<Enode> '</Enodes>) '</Enodes>))
    (let ((N (make-enode :Symbol (mg-sr S)
			 :id (mg-sr S)
			 :gindex (mg-sr S)
			 :eindex (mg-sr S)
			 :algebra (mg-sr S)
			 :path (psmg->help-psmg (mg-sr S))
			 :assumptions (mg-sr S)
			 :Qnodes (mg-sr S)
			 :marks (mg-sr S)
			 :state (mg-sr S)
			 :entries (mg-sr S)
			 :subeqns (mg-sr S)
			 :subvars (mg-sr S))))
      (mg-srt S '</Enode>)
      N)))



;;---------------------------------------------------------------------
;; enode-utilities

(defun enodes-equalp (E1 E2)
  "Enode equatlity does not take into account the path."
  (and (unify (Enode-ID E1) (Enode-ID E2)) ;could use "equal"
       (unify (Enode-Algebra E1) (Enode-Algebra E2))
       (equal-sets (Enode-Marks E1) (Enode-Marks E2))
       (equal-sets (Enode-Subeqns E1) (Enode-Subeqns E2))
       (equal-sets (Enode-Subvars E1) (Enode-Subvars E2))
       (equal-sets (Enode-Assumptions E1) (Enode-Assumptions E2))
       (equal-sets (mapcar #'Qnode-Exp (Enode-Qnodes E1))
		    (mapcar #'Qnode-Exp (Enode-Qnodes E2)))))


;;; Enodes contain lists of systementries within them.
;;; This function locates a matching entry within the
;;; node if present and returns it if found.
(defun enode-contains-entryp (Entry Node)
  "Does the quantity node contain the specified entry."
  (find Entry (Enode-Entries Node) :test #'unify)) ;could use "equal"


;;; An Enode represents (one :time level) a collection of equations
;;; therefore it is considered to be solved iff all of its subeqns
;;; (located in the SubEqns slot) are labelled as solved.
;;;
;;; Note that this function makes use of the EQWN code in the 
;;; knowledge base.
(defun enode-solvedp (Enode)
  "Determine iff the Enode is solved."
  (and (Enode-Eindex Enode)
       (Eqn-Solved (Enode-Eindex Enode))
       (or (null (Enode-Subeqns Enode))
	   (eval (cons 'and (mapcar #'Eqn-Solved (Enode-Subeqns Enode)))))))

;;---------------------------------------------------------------------
;; Markings tests and modifications

(defun enode-has-mark? (Enode mark &key (test #'equal))
  "Does the enode contain the specified Mark?"
  (find Mark (Enode-Marks Enode) :test test))

(defun enode-dead-pathp (E)
  "Is the Enode on a dead path?"
  (Enode-has-mark? E **Dead-Path**))

(defun enode-optimal-pathp (E)
  "Is the enode on the optimal path?"
  (Enode-has-mark? E **optimal-path**))

(defun enode-forbiddenp (E)
  (Enode-has-mark? E **Forbidden**))

(defun enode-givenp (E)
  (Enode-has-mark? E **Given**))

(defun enode-non-quantp (E)
  (Enode-has-mark? E 'non-quant))

;;;=============================================================================
;;; BubbleGraph Nodes
;;; Allthough Qnodes and enodes are employed at distinct times it is sometimes
;;; necessary to access them both as if they were a common item.  The functions
;;; in this section are used to perform such tasks.

;;; Determine if <NODE> is a qnode or enode.
(defun bgnode-p (Node)
  "Return t iff NODE is a Qnode or Enode."
  (or (qnode-p Node) (Enode-P Node)))

;;; Given an arbitrary node print it out in one of the 
;;; specified forms Full (default), Graph and Mreadable.
(defun form-print-node (Node &optional (form 'full))
  "Form print the supplied node as necessary."
  (if (qnode-p Node)
      (form-print-qnode Node form)
    (form-print-enode Node form)))


;;; Return t iff the specified 
;;; BGNode is marked dead-path.
(defun bgnode-dead-pathp (Node)
  (if (Qnode-P Node)
      (Qnode-Dead-pathp Node)
    (Enode-Dead-pathp Node)))

;;; Given an arbitrary bgnode collect
;;; the set of Systementries from it.
(defun bgnode-entries (Node)
  "Get entries from the node."
  (if (Qnode-p Node)
      (Qnode-Entries Node)
    (Enode-Entries Node)))


;;; Given an arbitrary BGnode get the 
;;; expression from it.
(defun bgnode-exp (N)
  "Get the node's expression."
  (if (Enode-P N)
      (Enode-ID N)
    (Qnode-Exp N)))


;;; Given an arbitary bgnode get the path 
;;; connected to it.
(defun bgnode-path (N)
  (if (Enode-P N)
      (Enode-Path N)
    (Qnode-Path N)))


;;; Get the gindex of the supplied bgnode.
(defun bgnode-gindex (N)
  "Get the gindex of the supplied bgnode."
  (if (Enode-p N) 
      (Enode-gindex N)
    (qnode-gindex N)))

;;; Get the other nodes in the graph that are linked
;;; to this node.  These will be enodes if the current
;;; node is a qnode or qnodes if the current node is an
;;; enode.
(defun bgnode-links (N)
  "Get the other nodes linked to this bgnode."
  (if (qnode-p N)
      (qnode-eqns N)
    (Enode-qnodes N)))


;;; Return t iff the two nodes are equalp this includes ensuring
;;; that they are both of the same type and that they are both 
;;; equalp when necessary.
(defun bgnodes-equalp (N1 N2)
  "Return two iff both nodes are equalp."
  (cond ((and (qnode-p N1) (qnode-p N2))
	 (qnodes-equalp N1 N2))
	((and (Enode-p N1) (Enode-p N2))
	 (enodes-equalp N1 N2))))


;;;============================================================================
;;; Bubblegraph
;;; A Bubblegraph consists of a collection of interconnected Qnodes and Enodes
;;; that represents a full or partial solution to a specific problem.  The 
;;; problem is defined as a collection of sought and given values that are
;;; passed successiely to the quantity solver code for its use.  
;;;
;;; If the bubblegraph represents one or more complete solutions then there 
;;; will be a qnode for each sought quantity and a complete path from each
;;; sought to a complete set of solved qnodes.  
;;;
;;; The form of a bubblegraph is a list of two lists.  The first list is the
;;; set of quantity nodes and the second is the set of Enodes.  The code in 
;;; this section is setup for manipulating bubblegraphs as a whole.
;;;
;;; The fact that the bubblegraph is defined as a struct here is for code
;;; cleanliness.  A great deal of the running code still accesses the graph
;;; using car and cadr so it must, for now remain defined as a list as well.
;;; 

(defstruct (bubblegraph (:type list) 
	    (:constructor nil))		;make-bubblegraph defined below
  qnodes
  enodes)

;;----------------------------------------------------------------------------
;; Bubblegraph Output functions.
;; Output the graph for human-readable and Machine readable purposes.

(defun form-print-bgnodes (Nodes &optional (form 'graph) (Stream t) (level 0))
  (mapcar #'(lambda (N) (if (qnode-p N) (form-print-qnode N form Stream Level)
			  (form-print-enode N form Stream Level)))
	  Nodes))

(defun print-Bubblegraph (Graph &optional (form 'graph) (Stream t) (Level 0)) 
  "Print out the specified bubblegraph."
  (pprint-Indent :block Level Stream)
  (format Stream "===== Quantity nodes: =====~%")

  (dolist (N (bubblegraph-qnodes Graph))
    (form-print-Qnode N form Stream (+ Level 1))
    (format Stream "~%"))

  (pprint-Indent :block Level Stream)
  (format Stream "~%===== Equation nodes: =====~%")
    
  (dolist (N (bubblegraph-enodes Graph))
    (form-print-Enode N form Stream (+ Level 1))
    (format Stream "~%"))
  
  (format Stream "~2%"))

(defun print-mreadable-bubblegraph (Graph Stream)
  "Dump a machine-readable (xml'ish) form of Graph to Stream."
  (format Stream "<BubbleGraph>~%<Qnodes>~%")
  
  (dolist (Q (bubblegraph-qnodes Graph))
    (print-mreadable-qnode Q Stream))
  
  (format Stream "</Qnodes>~%<Enodes>~%")
  
  (dolist (E (bubblegraph-enodes Graph))
    (print-mreadable-enode E Stream))

  (format Stream "</Enodes>~%</BubbleGraph>~%"))


;;-------------------------------------------------------------------
;; Bubblegraph Input functions
;; The functions in this section are devoted to reading in a
;; bubblegraph from an mreadable stream form and re-creating it.

(defun read-mreadable-bubblegraph (S)
  "Read in an mreadable graph from stream S."
  (mg-srt S '<BubbleGraph>)
  (let ((G (list (read-mreadable-qnode-lst S)
		 (read-mreadable-enode-lst S))))
    (mg-srt S '</BubbleGraph>)
    (regen-en-qn-links G)
    (regen-qn-en-links G)
    G))
    
(defun regen-en-qn-links (Graph)
  "Regenerate the Qnode-to-Enode links in Graph."
  (dolist (E (bubblegraph-enodes Graph))
    (setf (Enode-Qnodes E)
      (collect-gindicies->qnodes 
       (Enode-Qnodes E) Graph))))

(defun regen-qn-en-links (Graph)
  "Regenerate the Qnode-to-Enode links in Graph."
  (dolist (Q (bubblegraph-qnodes Graph))
    (setf (Qnode-Eqns Q)
      (collect-gindicies->enodes 
       (Qnode-Eqns Q) Graph))))
    			

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Once a Bubblegraph is read in we will have to regenerate
;; the appropriate Qnode->Qvar and Enode->Eqn sublinks.

;;(defun regen-bg-index-links (Graph Vindex Eindex)
;;  "Regenerate the links between the Graph and the specified indicies."
;;  (regen-bg-vindex-links Graph Vindex)
;;  (regen-bg-eindex-links Graph Eindex))


(defun regen-bg-vindex-links (Graph Vindex)
  "Regenerate the indicies with the indexes."
  (dolist (Q (bubblegraph-qnodes Graph))
    (setf (Qnode-Vindex Q)
      (collect-index->qvar (Qnode-Vindex Q) Vindex)))
  (dolist (E (bubblegraph-Enodes Graph))
    (setf (Enode-Subvars E)
      (collect-indicies->qvars (Enode-Subvars E) Vindex))))


(defun regen-bg-Eindex-links (Graph Eindex)
  "Regenerate the indicies with the indexes."
  (dolist (E (bubblegraph-Enodes Graph))
    (when (Enode-Eindex E)
      (setf (Enode-Eindex E)
	(collect-index->Eqn (Enode-Eindex E) Eindex)))
    (when (Enode-Subeqns E)
      (setf (Enode-Subeqns E)
	(collect-indicies->Eqns (Enode-Subeqns E) Eindex)))))


;;--------------------------------------------------------------------------
;; These macrosare used for Qnode and Enode IO functions hence they are 
;; located here.
;; (lht) movef these macros in attempt to fix <bubblegraph> undefed error
(defun mg-sr (S)
  "Read from Stream into Loc with error test."
  (read S "Malformed Bubblegraph"))

(defun mg-srt (Stream Comp)
  (let ((R (read Stream)))
    (when (not (unify R Comp))		;could use "equal"
      (error "Malformed Bubblegraph-file"))
    R))

(defun mg-srt2 (Stream Comp1 Comp2)
  (let ((R (read Stream)))
    (when (and (not (unify R Comp1))	;could use "equal"
	       (not (unify R Comp2)))	;could use "equal"
      (error "Malformed Bubblegraph-file"))
    R))





  
;;-----------------------------------------------------------------------
;; Graph modification code.
;; The functions in this section are used to generate and modify graphs.
;; This code allows you to treat graphs as distinct objects.

;;; Generate a new bubblegraph containing the specified BGNodes.
(defun make-bubblegraph (&optional bgnodes)
  "Generate a new Bubblegraph with the specified BGNodes."
  (if (null BGNodes) (list nil nil)
    (apply #'add-nodes-to-bubblegraph (list nil nil) bgnodes)))

;;; Given one or more bgnodes add them to the specified graph
;;; and return the resulting graph.  Note that this does not
;;; establish any links between the nodes and other nodes in
;;; the graph.  It merely adds the nodes to the graph's listing.
(defun add-nodes-to-bubblegraph (Graph &rest Nodes)
  "Add the supplied nodes to the graph."
  (let ((G (if Graph Graph (make-bubblegraph))))
    (dolist (N Nodes)
      (if (Enode-P N)
	  (push N (bubblegraph-enodes G))
	(push N (bubblegraph-qnodes G))))
    G))


;;; Add an enode to the Bubblegraph
(defun add-enode-to-bubblegraph (Enode Graph)
  "Add the specified enode to the bubblegraph."
  (list (bubblegraph-qnodes Graph)
	(cons Enode (bubblegraph-enodes Graph))))
 

;;; Add a qnode to the Bubblegraph
(defun add-qnode-to-bubblegraph (Qnode Graph)
  "Add the specified enode to the bubblegraph."
  (list (cons Qnode (bubblegraph-qnodes Graph))
	(bubblegraph-enodes Graph)))



;;----------------------------------------------------------------------
;; Bubblegraph comparison.
;; In comparing two problem solutions it is necessary to test
;; for the equality of two or more bubblegraphs.  This function will
;; test for equality of the graphs bu ensuiring that for each unique 
;; node in the graph there exists a comprable node in the other graph
;; and that the two comprable nodes are equalp.

(defun set-diff-bubblegraphs (GA GB)
  "Perform a set-difference style comparison of two bublegraphs."
  (list (set-difference (bubblegraph-qnodes GA) 
			(bubblegraph-qnodes GB) 
			:test #'Qnodes-Equalp)
	(set-difference (bubblegraph-enodes GA) 
			(bubblegraph-enodes GB) 
			:test #'Enodes-Equalp)))




;;--------------------------------------------------------------------------
;; Graph searching code
;; Given a bubblegraph these functions allow for searching and extraction
;; of nodes or subsets of the graph.

;;; Get the nt'h node in the graph according to the indexing.
(defun get-nth-bgnode (N Graph)
  "Get the Nth node from Graph."
  (let ((lenq (length (bubblegraph-qnodes Graph)))
	(lene (length (bubblegraph-enodes Graph))))
    
    (cond ((not (numberp N)) 
	   (error "Non-number ~A supplied to get-nth-bgnode." N))
	  
	  ((< N lenq) (nth N (bubblegraph-qnodes Graph)))
	  
	  ((< N (+ lenq lene)) (nth (- N lenq) (bubblegraph-enodes Graph)))
	  
	  (t (error "Invalid index ~A supplied to get-nth-bgnode" N)))))


;;; Map the specified indicies to the supplied graph nodes.
(defun map-indicies->bgnodes (Indicies Graph)
  "Map the specified indicies to the corresponding list of bgnodes."
  (mapcar #'(lambda (n) (get-nth-bgnode N Graph)) Indicies))


;;; Collect all of the unsolved nodes from the graph in a list.  
(defun collect-unsolved-bgnodes (Graph)
  "Collect the unsolved nodes in the graph."
  (append (remove-if #'Qnode-Solvedp (Bubblegraph-Qnodes Graph)) 
	  (remove-if #'Qnode-Solvedp (Bubblegraph-Qnodes Graph))))


;;; Collect all Qnodes within the graph 
;;; that have been marked as sought.
(defun collect-sought-qnodes (Graph)
  "Collect the sought qnodes from the graph."
  (reverse ; to get them in problem order, given how graph was built 
    (loop for Q in (bubblegraph-qnodes Graph)
      when (qnode-soughtp Q)
       collect Q)))


(defun collect-forbidden-bgnodes (Graph)
  "Collect all forbidden nodes (Enodes only) within the graph."
  (collect-predicate-enodes #'enode-forbiddenp Graph))
    

(defun match-exp->qnode (exp Graph)
  "Match the given exp to a coresponding qnode."
  (find Exp (bubblegraph-qnodes Graph)
	:key #'Qnode-Exp 
	:test #'unify))


(defun match-exp->enode (exp Graph)
  "Match the given exp to a coresponding qnode."
  (find Exp (bubblegraph-enodes Graph)
	:key #'enode-ID 
	:test #'unify))			;could use "equal"


(defun match-exp->bgnode (Exp Graph)
  "Given the expression return the node that matches it if any."
  (or (match-exp->qnode Exp Graph)
      (match-exp->Enode Exp Graph)))


;;; Given a list of bubblegraph nodes collect the
;;; set of assumptions from them for later use.
(defun collect-bgnodes->assumpts (Nodes)
  (loop for N in (force-to-list Nodes)
      append (if (Qnode-p N)
		 (Qnode-Assumptions N)
	       (Enode-Assumptions N))))



(defun collect-predicate-qnodes (Predicate Graph)
  "Collect all the qnodes from graph that match predicate."
  (loop for N in (Bubblegraph-Qnodes Graph)
      when (funcall Predicate N)
      collect N))


(defun collect-predicate-Enodes (Predicate Graph)
  "Collect all the enodes from graph that match predicate."
  (loop for N in (Bubblegraph-Enodes Graph)
      when (funcall Predicate N)
      collect N))


(defun collect-predicate-bgnodes (Predicate Graph)
  "Collect all of the bgnodes that match predicate in Graph."
  (append (collect-predicate-qnodes Predicate Graph)
	  (collect-predicate-enodes Predicate Graph)))


(defun collect-marked-qnodes (Mark Graph &key (test #'equal))
  "Collect all qnodes with the supplied mark."
  (collect-predicate-qnodes
   #'(lambda (Q) (qnode-has-mark? Q Mark :test test))
   Graph))


(defun collect-marked-enodes (mark Graph &key (test #'equal))
  "Collect enodes with the mark."
  (collect-predicate-enodes
   #'(lambda (E) (Enode-has-mark? E Mark :test test))
   Graph))


(defun collect-marked-bgnodes (Mark Graph &key (test #'equal)) 
  "Collect all bgnodes with the supplied mark."
  (append (collect-marked-qnodes Mark Graph :test Test)
	  (collect-marked-enodes Mark Graph :test Test)))


(defun collect-unmarked-qnodes (Mark Graph &key (test #'equal))
  "Collect all qnodes with the supplied mark."
  (collect-predicate-qnodes
   #'(lambda (Q) (not (qnode-has-mark? Q Mark :test test)))
   Graph))


(defun collect-unmarked-enodes (mark Graph &key (test #'equal))
  "Collect enodes with the mark."
  (collect-predicate-enodes
   #'(lambda (E) (not (Enode-has-mark? E Mark :test test)))
   Graph))


(defun collect-unmarked-bgnodes (Mark Graph &key (test #'equal))
  "Collect all bgnodes without the suplied mark."
  (append (collect-unmarked-qnodes Mark Graph :test Test)
	  (collect-unmarked-enodes Mark Graph :test Test)))


;;; -----------------------------------------------------------------
;;; Path collection.
;;; For some applications it is necessary to collect the paths or 
;;; trees that link from one of the nodes to another.  This will 
;;; be performed by the code located here.  Given a pair of nodes 
;;; an optional max-depth, and single-or-tree flags this code will
;;; search for the path(s) that connect one node to another.
;;;
;;; Because distance in our graph is defined entirely by path-length
;;; this code will make use of breadth-first-search.  If Max-depth
;;; is found then the code will cease searching once the maximum
;;; depth is reached.  If the single flag is supplied then the 
;;; system will stop searching once it finds the first path.  If not
;;; then it will continue searching until all possible paths are 
;;; exhausted.  
;;;
;;; This code uses depth-first search to produce the paths as it 
;;; will Speed uyp the memory requirements to do so.  I have 
;;; optimized this for tail recursion using the Next slot in 
;;; order to ensure that the system will Not need to maintain an 
;;; unnecessarily long stack.

(defun collect-bubblegraph-paths (Curr End Graph 
				  &key (Max-Depth Nil)
				       (Max-Paths Nil)
				       (Next Nil)
				       (Result Nil))
  "Collect the paths that link one bgnode to another."
  (let (Links NewPath)
    ;; We have to set the Links that we are examining and the
    ;; list that we will be testing.  I am doing both at the 
    ;; same time to spare a test.
    (if (listp Curr)
	(progn (setq Links (bgnode-links (car Curr)))
	       (setq Newpath Curr))
      (progn (setq Links (bgnode-links Curr))
	     (setq Newpath (list Curr))))
    
    ;;; Remove all nodes from links that are present in the 
    ;;; leading path.  (I.E. Already visited.  Unfortunately
    ;;; this is not just guarantee that it will be the 2nd 
    ;;; node in the list.
    (setq Links
      (loop for L in Links
	  when (not (member L NewPath :test #'bgnodes-equalp))
	  collect L))
    
    
    ;; At this point we need to handle the end-tests and the 
    ;; termination tests.  See the comments above and below 
    ;; for more of a discussion of the cases.  
    (cond 
     ;; When we have found the pend then return.
     ((member End Links :test #'bgnodes-equalp)
      ;;(pprint 1)
      (collect-bubblegraph-paths-recurse
       Nil End Graph
       Max-Depth Max-Paths Next 
       (cons (reverse (cons End NewPath)) Result)))
     
     ;; When we have not found the end then we need to test
     ;; for the max depth and end if necessary.
     ((and Max-Depth (= (length NewPath) Max-Depth))
      ;;(pprint 2)
      (collect-bubblegraph-paths-recurse 
       Nil End Graph Max-Depth Max-Paths 
       (cdr Next) Result))
     
     ;; When we can recurse then generate a new set paths
     ;; selecting the first to recurse on and adding the 
     ;; rest to the Next set.
     (Links
      ;;(pprint 3)
      ;;(pprint (mapcar #'(lambda (V) (cons V NewPath)) Links))
      ;;(pprint 3)
      ;;(pprint 3)
      
      (collect-bubblegraph-paths
       (cons (car Links) NewPath) End Graph 
       :Max-Depth Max-Depth 
       :Max-Paths Max-Paths
       :Next (append (mapcar #'(lambda (V) (cons V NewPath)) (cdr Links)) Next)
       :Result Result))
     
     ;; When we cannot recurse then move on to the next element.
     (t 
      ;;(pprint 4)
      (collect-bubblegraph-paths-recurse
	 Nil End Graph Max-Depth Max-Paths
	 Next Result)))))


;;; When recursing we test to see if the Next set is present.  If so 
;;; then the search proceeds by expanding the first of those paths. 
;;; If not then the system will return the Result value.
  ;;; 
(defun collect-bubblegraph-paths-recurse (Curr End Graph Max-Depth Max-Paths Next Result)
  "Test for the recurse state of the search and behave appropriately."
  (declare (ignore Max-Paths Curr))
  (if (null Next) Result
    (collect-bubblegraph-paths 
     (car Next) End Graph 
     :Max-Depth Max-Depth 
     :Next (cdr Next) 
     :Result Result)))


;;(defun tstseartcharr ()
;;  (setq q1 (make-qnode :exp 'q1))
;;  (setq e1 (make-enode :id 'e1))
;;  (setq e2 (make-enode :id 'e2))
;;  (setq g (make-qnode :exp 'g))
;;  (setq q2 (make-qnode :exp 'q2))
;;  (setq q3 (make-qnode :exp 'q3))
;;  (setq q4 (make-qnode :exp 'q4))
;;  
;;  (setf (qnode-eqns q1) (list e1 e2))
;;  (setf (qnode-eqns q2) (list e1 e2))
;;  (setf (qnode-eqns q3) (list e2))
;;  (setf (qnode-eqns q4) (list e2))
;;  (setf (qnode-eqns g) (list e1 e2))
;;  
;;  (setf (Enode-qnodes e1) (list q1 g))
;;  (setf (Enode-qnodes e2) (list q1 g q2 q3 q4)))


;;;------------------------------------------------------------------
;;; Bubblegraph mapping code.  
;;; map the specified function to the relevant elements of the 
;;; bubblegraph and return the results mapcar style.

(defun mapcar-bubblegraph-qnodes (func Graph)
  "Mapcar the function over the qnodes."
  (mapcar Func (bubblegraph-qnodes Graph)))

(defun mapcar-bubblegraph-enodes (func Graph)
  "Mapcar the function over the enodes."
  (mapcar Func (bubblegraph-enodes Graph)))

(defun mapcar-bubblegraph-bgnodes (func Graph)
  "Mapcar the function over the bgnodes."
  (append (mapcar Func (bubblegraph-qnodes Graph))
	  (mapcar Func (bubblegraph-enodes Graph))))

(defun mapcan-bubblegraph-qnodes (func Graph)
  "Mapcan the function over the qnodes."
  (mapcan Func (bubblegraph-qnodes Graph)))

(defun mapcan-bubblegraph-enodes (func Graph)
  "Mapcan the function over the enodes."
  (mapcan Func (bubblegraph-enodes Graph)))

(defun mapcan-bubblegraph-bgnodes (func Graph)
  "Mapcan the function over the bgnodes."
  (append (mapcan Func (bubblegraph-qnodes Graph))
	  (mapcan Func (bubblegraph-enodes Graph))))

;;--------------------------------------------------------------------
;; Bubblegraph dead-path code
;; Dead-path nodes are those nodes within the bubblegraph that do not 
;; appear in any solution or in the case of marking in the UsedNodes 
;; that are provided.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark-bg-dead-paths
;; mark the dead path nodes within the bubblegraph by adding 'dead-path
;; to all unused nodes.  The unused nodes as defined in the Used-Nodes 
;; list.
(defun mark-bg-dead-path-nodes (Graph UsedNodes) 
  "Mark the dead path nodes in the graph."
;;  (format t "mark-bg-dead-path-nodes, bubblegraph-enodes:~%     ~A~%" 
;;	  (bubblegraph-enodes graph))
;;  (format t "mark-bg-dead-path-nodes:  used nodes:~%     ~A~%" UsedNodes)
  (dolist (Q (bubblegraph-qnodes Graph))
    (when (not (member Q UsedNodes))
   ;;   (format t "mark-bg-dead-path-nodes:  removing Qnode ~A~%" Q)
      (push **Dead-Path** (Qnode-Marks Q))))
  
  ;; AW: for hybrid quant/non-quant problems: don't mark pseudo-enodes 
  ;; generated for qualitative problem parts as dead-paths.
  (dolist (E (remove-if #'enode-non-quantp (bubblegraph-enodes Graph)))
    (when (not (member E UsedNodes))
    ;;  (format t "mark-bg-dead-path-nodes:  removing Enode ~A~%" E)
      (push **Dead-Path** (Enode-Marks E))))
  
  Graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove-dead-path-nodes 
;; This function will destructively modify the graph
;; supplied to remove all links to dead-path nodes
;; leaving the graph smaller.
(defun remove-bg-dead-path-nodes (Graph)
  (list (loop for Q in (bubblegraph-qnodes Graph)
	    when (not (Qnode-Dead-pathp Q))
	    collect (remove-qnode-dead-path-links Q))
	(loop for E in (bubblegraph-enodes Graph)
	    when (not (Enode-Dead-pathp E))
	    collect (remove-Enode-dead-path-links E))))

(defun remove-qnode-dead-path-links (Q)
  "Remove all dead-path enodes from the qnode's links."
  (setf (Qnode-eqns Q)
    (remove-if #'enode-dead-pathp (Qnode-Eqns Q)))
  Q)

(defun remove-enode-dead-path-links (E)
  (setf (Enode-Qnodes E)
    (remove-if #'Qnode-dead-pathp (Enode-Qnodes E)))
  E)



;;-------------------------------------------------------------------------
;; Graph Indexing code.
;; The Gidicies of the bubblegraph are numbers indicating simply the order 
;; in which the nodes appear in the graph.  The code in this section
;; assigns those index values to the elements in the graph.
(defun index-bubblegraph (Graph)
  "Set the Gindicies of the bubblegraphs."
  (let ((Offset (Length (bubblegraph-qnodes Graph))))
    (dotimes (N (length (bubblegraph-qnodes Graph)))
      (setf (Qnode-Gindex (nth N (bubblegraph-qnodes Graph))) N))
    (dotimes (N (length (bubblegraph-Enodes Graph)))
      (setf (Enode-Gindex (nth N (bubblegraph-Enodes Graph)))
	(+ Offset N)))
    Graph))

(defun collect-nodes->gindicies (Nodes)
  "Substitute the indicies for the Enodes."
  (loop for N in Nodes
      collect (if (Enode-P N)
		  (Enode-Gindex N)
		(Qnode-Gindex N))))


(defun collect-enodes->gindicies (Nodes)
  "Substitute the indicies for the Enodes."
  (loop for N in Nodes
      collect (Enode-Gindex N)))


(defun collect-qnodes->gindicies (Nodes)
  "Substitute the indicies for the qnodes."
  (loop for N in Nodes
      collect (qnode-Gindex N)))


(defun collect-gindicies->nodes (Nodes Graph)
  "Substitute in the gindexed nodes."
  (let ((L (length (bubblegraph-qnodes Graph))))
    (loop for N in Nodes
	collect (if (< N L)
		    (nth N (bubblegraph-qnodes Graph))
		  (nth (- N L) (bubblegraph-Enodes Graph))))))


(defun collect-gindicies->qnodes (Nodes Graph)
  "Substitute in the gindexed nodes."
  (loop for N in Nodes
      collect (nth N (bubblegraph-qnodes Graph))))

(defun collect-gindicies->enodes (Nodes Graph)
  "Substitute in the gindexed nodes."
  (let ((L (length (bubblegraph-qnodes Graph))))
    (loop for N in Nodes
	collect (nth (- N L) (bubblegraph-Enodes Graph)))))



;;-------------------------------------------------------------
;; Variable Index. 
;; Once a problem has been solved it is necessary to generate 
;; the variable index.  This necessitates collecting all of
;; the variables mentioned within the graph into a single
;; list accounting for duplicates.  The variables within qnodes
;; and enodes are already located within the subvars locations
;; this code extracts those.  If there is more than one of them
;; it will then merge the duplicates and update the graphs itself.
;;
;; In some cases we will want to append problem-specific or other
;; temporary variable restrictions when this is done.  These will
;; be passed in (optionally) to this function and will be included
;; in the resulting variables.
;;
;; This list should be a list of lists of the form: (<Exp> <Marks>)
;; If The Variable prop in question unifies with the Expression then
;; the marks will be appended to the variable for use.  Note that 
;; these marks do not supersede those defined in Ontology they will 
;; only add to it.  
(defun generate-bg-vindex (Graph &key (ExpMarks ()))
  "Generate the var index for the graph."
  (let* ((vars (collect-bg-qvars Graph :ExpMarks ExpMarks))
	 (I (if (>= 1 (length Vars)) Vars
	      (merge-duplicate-qvars Vars))))
    (dotimes (N (length I))
      (setf (Qvar-Index (nth N I)) N))
    (vindex-qnodes (bubblegraph-qnodes Graph) I)
    (vindex-Enodes (bubblegraph-Enodes Graph) I)
    I))

(defun collect-active-bg-qvars (Graph)
  "Collect all the vars from the graph."
  (append (loop for Q in (bubblegraph-qnodes Graph)
	      unless (Qnode-dead-pathp Q)
	      collect (qnode->qvar Q)) 
	  (loop for E in (bubblegraph-Enodes Graph)
	      unless (Enode-Dead-pathp E)
	      append (Enode->qvars E))))


(defun collect-bg-qvars (Graph &key (ExpMarks Nil))
  "Collect all the vars from the graph."
  (append (loop for Q in (bubblegraph-qnodes Graph)
	      collect (qnode->qvar Q :ExpMarks ExpMarks)) 
	  (loop for E in (bubblegraph-Enodes Graph)
	      append (Enode->qvars E :ExpMarks ExpMarks))))


(defun qnode->qvar (Q &key (ExpMarks Nil))
  "convert Qnode Q to Qvar form including parameter info."
  (valid-expression-p (qnode-exp Q))
  (make-qvar :Var (Qnode-Var Q)
	     :Exp (Qnode-exp Q)
	     :units (lookup-expression-units (Qnode-Exp Q))
	     :marks (remove-duplicates  
		     (append (sets-intersect '(Parameter answer-var) (Qnode-Marks Q))
			     (lookup-expression-restrictions (Qnode-Exp Q)) ;; Found in ./Ontology.cl
			     (mappend #'cdr (collect-unifiers ExpMarks (Qnode-Exp Q) :key #'car))))
	     :nodes (list Q)))


(defun enode->qvars (E &key (ExpMarks Nil))
  "Collect qvars from an enode."
  (loop for V in (vars->qvars (Enode-Subvars E) :ExpMarks ExpMarks)
      do (push E (Qvar-Nodes V))
      collect V))


(defun vindex-qnodes (Qnodes Index)
  "Set the vindicies and subvars of the Qnodes."
    (dolist (Q Qnodes)
      (setf (Qnode-Vindex Q)
	(match-var->qvar (Qnode-Var Q) Index))))

(defun vindex-enodes (Enodes Index)
  "Variable index the Enode subvars."
    (dolist (E Enodes)
      (setf (Enode-Subvars E)
	(collect-vars->qvars 
	 (remove-duplicates (Enode-Subvars E) :test #'unify) ;could use "equal"
	 Index))))




;;-----------------------------------------------------------------
;; Equation Index.
;; In the process of solving a problem, just after the graph has 
;; been generated, it is necessary to generate the equation index
;; this involves cycling through the graph collecting the equations
;; from each node's subeqns and the top-level elements of the enodes,
;; merging duplicate equations and returning the results.
(defun generate-bg-eindex (Graph)
  "Generate the index for the eqns."
  (let* (
	 ;; Collect all the eqns in graph to be used for indexing.
	 (collected-eqns (mappend #'Enode-Subeqns (bubblegraph-Enodes Graph)))
	  ;; Reduce and number collected eqns.
	 (Index (when collected-eqns 
		  (Index-eqn-list (merge-duplicate-eqns collected-eqns)))))
  ;;  (format t "generate-bg-eindex eqn list length ~A~%" (length Index))
    ;; modify bubblegraph based on index of equations
    (dolist (E (bubblegraph-Enodes Graph))
      (setf (Enode-Eindex E) ;this is only place where Enode-Eindex is set
	    ;; since only Enode-Algebra is known, match using this.
	    (find-algebra->eqn (Enode-Algebra E) Index))
      ;; Go back and fix up Enode-Subeqns to point to Eqn's in Index
      (setf (Enode-Subeqns E)
	    (collect-index-eqns 
	     (remove-duplicates (Enode-Subeqns E) :test #'eqns-equalp) 
	     Index)))
    Index))

(defun collect-index-eqns (Subeqns Index)
  "Collect eqn's in index that match, using Eqns-equalp, a member of Subeqns."
  (let (R)
    (dolist (E Subeqns)
    ;;  (format t "trying to find ~A~%" E)
      (let ((I (find E Index :test #'eqns-equalp)))
	(when I (push I R))))
    R))

(defun collect-bgnodes->eqns (Nodes)
  "Collect the eqns from the selected nodes."
  (loop for N in Nodes
      when (Enode-P N)
      append (cons (Enode-Eindex N)
		   (Enode-Subeqns N))))