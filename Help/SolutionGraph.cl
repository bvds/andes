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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SolutionGraph.cl
;; Collin Lynch
;; 04/19/2001
;;
;; The solution graph module maintains a listing of 
;; system entries and their status for the help system.
;; It also maintains a list of solution sets that are used
;; to provide help and to decompose equation entries for 
;; help and entry use.  
;;
;; The system entries themselves are defined in the helpstructs.
;; they are stored in a list of the form: 
;; (<Active Eqns> . <Forbidden/DeadPath Eqns> . <Non Eqns>)
;; The purpose of this ordering is to keep the indicies
;; of the active equations identical within this module and
;; within the Algebra dll where they will also be stored.
;;
;; The solutions sets are lists (macroed as structs) containing:
;;  
;;  Count: an integer representing the number of 'objections' to 
;;  the solution.  Unless this integer is 0 the solutions will not
;;  be used to provide what's next help.
;;
;;  Number:  This is the Algebra DLL's set id assigned to the set
;;  of canonical (system) equations associated with this solution
;;  when a student equation is decomposed using the algebra system
;;  this id is passed to identify this particular set.
;;
;;  Entries:  This is a list of SystemEntries that are associated
;;  with this set.  When non eqn entries are made this set is
;;  searched to determine if the entry is compatible with it or
;;  not, if not the count is incremented.
;; 
;;  Nodes:  A list of Bubblegraph Nodes (Quantities and Equations)
;;  That comprie this solution and that must be entered in order 
;;  for this solution to be used.
;;
;; When the SolutionGraph module is setup it is passed the current
;; problem struct to be used.  The Bubblegraph is then used to obtain
;; the list of systementries.  In the process each node in the bubblegraph
;; will have it's entries field updated to contain the set of Entries
;; that appear within its path.  These entries are then merged to produce
;; the master *SG-Entries* index.  In the process the system will pass 
;; the solution point and the set of base equations to the Algebra DLL
;; So that it can later be used for testing student entries.  
;;
;; Once the list of system entries has been obtained the system will then 
;; set up the solutions (from the problem struct) by sending the equations
;; within them to the Algebra DLL.  It will also translate from the list of
;; bgnodes stored in the solution to the list of System Entries within the 
;; set for later searching.  
;;
;; This whole process is fairly extensive and could and should probably be 
;; streamlined but it has not become an issue as of yet.  One possibility
;; might be offloading some of the setup process to sgg time such as the 
;; collection of system entries within the graph and their storage, perhaps
;; storing them as props in the nodes for later use.
;;
;;;;;;;;;;;;;;;;;;; Ugly hack notes
;; The system merges the system entries by comparing their props and then 
;; later updates them in the master index. It would be better to remove this 
;; excess step by building the links correctly the first time around.


(in-package :user)


;; ================================================
;; Solution struct.  
;; This code exists primarily for code cleanliness
;; and is here defined and used only within this 
;; module.

(defstruct sgsol
  Num
  Entries)




;;=========================================================
;; Memory Storage.

(defvar *SG-Solutions* () "The set of solutions to be done.")
(defvar *SG-Entries* () "The System entries from the bubblegraph.")
(defvar *SG-Eqns* () "Equation list with eqn-index->entry mappings.")



(defvar **Test-For-Premature-Subst** nil "If t tests for premature substitutions.")
(defvar **Test-For-Prematurity** nil "If t tests for premature entries.")


;;; Constraint filtering is designed to prevent the students from walking 
;;; themselves down an unsolvable dead-end where they will have "entered"
;;; all of the necessary system entries by combining equations.  Yet they 
;;; will be algebraically unable to solve for the sought quantity.  This
;;; filtering is designed to prevent that by elminating those combinations
;;; from the system when the entries are made.
;;;
;;; AW: disable this because it is unreliable for projections as we write them:
;;; Our solutions do not use theta variables for known angles inside projections. 
;;; Instead we use the pair of equations:
;;;          (1) Fn_x = Fn * cos(120 deg)
;;;          (2) thetaFn = 120 deg
;;; (It caused trouble for the solution indyset collection to have the theta 
;;; vars in the equation, in some way because it couldn't make use of the fact
;;; that two angles denoted by different theta vars were in fact the same. So
;;; we plugged in known angle values instead.)
;;; So, when student enters
;;;           (3) Fn_x = Fn*cos(thetaFn)
;;; this looks like a weaker constraint than {1,2} -- it does not determine
;;; that thetaFn is 120 deg so determines a plane.
;;; There might be some better way of dealing with this, but for now easiest
;;; just to turn it off.

(defvar **Filter-Constraint-losses** NIL
  "If t will filter out systementries that cause constraint loss in the system.") 

;;========================================================================
;; Setup process.
;; The solution graph setup process operates in five distinct phases.
;; 
;; Step 0: Setup the eqns list by collecting the eqns from the system
;;         ignoring all Forbidden equations as they will never appear
;;         in a solution and they can raise 
;;
;; Step 1: The algebra system is setup passing the equations to it and
;;         priming it for later use.
;;
;; Step 2: the system sets up the system entries culling them from the
;;         psm graphs and storing them in the nodes and the *Sg-Entries* 
;;         index.
;;
;; Step 3: The Matching of equation entries with the *Sg-Eqns* index
;;         This allows for matching of eqn numbers with entries at 
;;         use time.
;;
;; Step 3: The Solutions are setup encoding them from their stored forms
;;         into the forms necessary for help time.
;;

(defun sg-setup (Problem)
  "Setup the solution graph by loading it and defining the entries."
  (setq *Sg-Eqns* (sg-collect-Eqns Problem))
  (sg-prime-algebra (problem-varindex Problem) *Sg-Eqns*)
  (sg-setup-systementries (Problem-Graph Problem))
  (setq *sg-eqns* (sg-match-eqn-entries *Sg-Eqns* *Sg-Entries*))
  (sg-setup-solutions (Problem-Solutions Problem) 
		      (Problem-Graph Problem) *Sg-Eqns*))



;;; -------------------------------------------------------------
;;; Equation collection.
;;; Given the problem collect the equations from it that we will
;;; use to prime the algebra system and to decompose equation 
;;; entries.  The resulting set should be all of the equations in
;;; the problem's eqnindex (translated to help-system form) minus
;;; those that are only part of forbidden nodes and therefore 
;;; won't appear in any solutions and can cause errors.
(defun sg-collect-eqns (Problem)
  (let ((tmp (remove-if #'sg-eqn-forbiddenp (Problem-EqnIndex Problem))))
    (eqns->Help-sys-eqns tmp)))


(defun sg-eqn-forbiddenp (E)
  "Return t if the eqn is only in forbidden eqns."
  (null (remove-if 
	 #'(lambda (N) (and (enode-p N) (enode-forbiddenp N)))
	 (Eqn-Nodes E))))

;;---------------------------------------------------------------
;; Algebra Setup.
;; Prior to being queried for student equations or any other
;; information the Algebra DLL needs to be passed the list of
;; variables (the solution point) that it will use and the list
;; of canonical equations that will be used to decompose
;; student equations.  This code collects those sets and then 
;; passes them into the algebra system.
  
(defun sg-prime-algebra (Vars Eqns)
  "Send the IndyVars and Help-Sys-Eqns to the Algebra System."
  (Solver-new-problem)
  (dolist (V (qvars->IndyVars Vars))
    (Solver-IndyAddVar V))
  (dolist (P (collect-qvars->Marks Vars))
    (Solver-Send-Problem-Statement P))
  (solver-IndyDoneAddVar)

  (dolist (E Eqns)
    (Solver-IndyAddEquation 
     (car E) (cadr E))))



;;------------------------------------------------------------------------
;; sg-setup-systementries
;; The system entry setup proceeds first by collecting the system entries 
;; from the paths of each node in the graph into their Entries fields.
;; 
;; Once that is done The index is collected from those nodes and with it
;; the set of valid equation entries that will be used to prime the algebra
;; system.  
;;
;; Once that has been done the entries within the nodes themselves are merged
;; in order that we can perform simple eq tests on them at a later date.
;;
;; this is an ugly hack and could doubtless be improved by storing only the
;; entryprops or other elements in the nodes for equalp testing and then 
;; offloading some of the search to sgg time.
;;
;; Similarly the precond hack could be improved by offloading work to sgg time.

(defun sg-setup-systementries (Graph)
  (sg-set-graph-sysents Graph)                        ;; Set the entries list for each graph node.
  (setq *SG-Entries* (sg-collect-sysent-index Graph)) ;; Collect the entry index.
  (subst-prereqs-sysents *Sg-Entries* *Sg-Entries*)   ;; Subst the preconds for the entries in *Sg-Entries*
  (sg-subst-graph-sysents *Sg-Entries* Graph))        ;; Substitute the merged entries into the graph.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sg-set-graph-sysents
;; For each node in the Bubblegraph, search its path to collect
;; the set of System Entries within it., Storing those entries
;; within its entries field.

(defun sg-set-graph-Sysents (Graph)
  "Collect the set of entries within each node of the graph."
  (dolist (Q (car Graph))
    (setf (Qnode-Entries Q)
      (sg-collect-qnode-Sysents Q)))
  (dolist (E (cadr Graph))
    (setf (Enode-Entries E)
      (sg-collect-enode-sysents E))))


(defun sg-collect-qnode-sysents (Q)
  "Collect the quantity node system entries."
  (merge-duplicate-systementries
   (sg-psmg->sysents 
    (Qnode-Path Q)
    :State (sg-marks->State (Qnode-Marks Q)))))


(defun sg-collect-Enode-sysents (E)
  "Collect the quantity node system entries."
  (merge-duplicate-systementries
   (sg-psmg->sysents 
    (Enode-Path E)
    :State (sg-marks->State (Enode-Marks E)))))


(defun sg-Marks->State (Marks)
  "Collect the specified marks from the list if they are present or return **Correct**."
  (or (loop for M in '(Dead-Path Invalid Forbidden)
	      when (find M Marks :test #'equalp) return M) 
      **Correct**))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sg-psmg->sysents
;; Collection of system entries from a psmg proceeds by cycling
;; through the graph and generating a SystemEntry (with the specified
;; state and for each cognitive do step encountered.  The search proceeds 
;; in depth-first fashon and a stack of SystemEntries is accumulated on 
;; the way.  This stack is uysed to set the preconditions for each 
;; SystemEntry s.t. each entry's proconditions consist of the set of 
;; entries that preceed it in the graph.  
;;
;; The search accounts for splits by searching each next separately and 
;; then unifying the results in the stuack.  Thus in the following case:
;;
;; (Entry0
;;  split
;;   Next 
;;    Entry1
;;   Next 
;;    Entry2
;;  Join
;;  Entry3
;;
;; Entry1 will have Entry0 in its preconditions.
;; Entry2 will have Entry0 but not Entry1 in its preconditions.
;; Entry3 will have Entry0, Entry1, AND Entry2 in its preconditions.
;;
;; When choose nodes are encountered the system responds by splitting
;; the search along several branches.  Choose nodes never rejoin
;; therefore they share a common initial stack but separate independent stacks.
;; thus:  (Entry0
;;          (choose
;;            (Entry1)
;;            (Entry2 Entry3)))
;;
;; Entry1 will have Entry0 in its preconditions.
;; Entry2 will have Entry0 but not Entry1 in its preconditions.
;; Entry3 will have Entry0 and Entry2 but not Entry1 in its preconditions.
;;

(defun sg-psmg->sysents (Graph &key (Stk NIL) (State **Correct**))
  "Collect the System entries from the specified PSM Graph."
  (let ((Stack Stk) (Entries) (tmp))
    (dolist (N Graph)
      (cond ((or (cssplit-p N) (csnext-p N))            ;Push ops and splits
	     (push N Stack))                            ;onto the stack.
	    
	    ((csjoin-p N)                        ;When a join is encountered clear
	     (setq Stack (sg-drop-snj Stack)))   ;the splits and nexts preceeding it.
	    
	    ((and (csdo-p N)                            ;When a do is encountered
		  (find-if #'help-EntryProp-p (csdo-effects N))) ;and it contains an entry prop.
	     (setq tmp (sg-generate-sysents N Stack State))  ;; Generate the systementries.
	     (setq stack (append tmp stack))                          ;; Add them to the stack.
	     (setq entries (append tmp Entries))                      ;; Add them to the list of entries.
	     (setf (csdo-Entries N) tmp))                             ;; Set the back pointer to the csdo.
	     	    
	    ((cschoose-p N)                             ;; When a choose is encountered 
	     (setq Entries                              ;; Split the search for each element.  
	       (append Entries (sg-split-psmg->sysents (cdr N) Stack State))))))
		       
    Entries))                                           ;; Return the entries.


(defun sg-split-psmg->sysents (Lst Stack State)
  "Split the psmg->sysents search."
  (loop for C in Lst       
      append (sg-psmg->sysents 
	      C :stk Stack :State State)))


(defun sg-drop-snj (Stack)
  "Remove the topmost set of split next elements from the stack."
  (let ((Loc (position 'Split Stack :test #'equalp)))
    (append (remove 'Next (subseq Stack 0 Loc))
	    (subseq Stack (+ Loc 1)))))


(defun sg-generate-sysents (Do Stack State)
  "Generate the sysnts for a csdo and return them."
  (loop for E in (csdo-effects do)
      when (help-entryprop-p E)
      collect (sg-generate-sysent Do E Stack State)))

; custom condition subclass to be signalled on apparent version errors
; This inherits from the simple-error class which defines initargs 
; :format-control and :format-arguments, used in the report function
; to format an error message. The "error" function can take a class
; name and init arguments to construct a condition object (see below)
(define-condition wrong-version-prb (simple-error)
   ())    ; no custom slots added to this subclass

(defun sg-generate-sysent (Do Entry Stack State)
  "Given a help entry prop generate the system entry for it and return."
  ; include helpful message for this error: 
  (when (not (get-operator-by-tag (csdo-op Do)))
    (format T "Solution operator ~A not found in current KB. Maybe need to regenerate .prb~%" 
            (first (csdo-op Do)))
    (error 'wrong-version-prb 
             :format-control "Solution operator ~A not found in current KB. Maybe need to regenerate .prb"
	     :format-arguments (list (first (csdo-op Do)))))
  ; else didn't signal error above:
  (make-SystemEntry 
   :Prop Entry
   :State State
   :CogLoad (operator-CogLoad (get-operator-by-tag (csdo-op Do)))
   :Sources (list Do)
   :Prereqs (wrap-if (sg-collect-sysent-prereqs Entry Stack))))
	      

;;; Determination of what is and is not a prerequisite depends upon the problem
;;; specification at hand.  At one time variable definitions were removed from 
;;; the list of prerequisites.  However, as we no longer use these for 
;;; prematurity checking I am now amending this list so that it includes the 
;;; variable definitions again.  We may have to be more sensitive about how
;;; we use the prerequisites list when we go to test for prematurity again.
(defun sg-collect-sysent-prereqs (Prop Stack)
  "Collect the prerequisites for the urrent element on the stack."
  #|(cond ((help-eqn-entryprop-p prop) 
  ;;(sg-collect-stack-prereqs stack))
  (remove-if #'sg-defvar-entryp 
  (sg-collect-stack-prereqs stack)))
  (t (sg-collect-stack-prereqs stack))))
  |#
  (declare (ignore Prop))
  (sg-collect-stack-prereqs stack))

(defun sg-collect-stack-prereqs (Stack)
  "collect the prereqs from a stack."
  (let ((S (position 'split Stack :test #'equalp))
	(n (position 'next Stack :test #'equalp)))
     
    (cond ((null N) (remove-if-not #'SystemEntry-p Stack)) ;; No splits and nexts
	  
	  (t ;; If the element is within a next collect all of the components between
	   (append (remove-if-not #'SystemEntry-p (subseq Stack 0 N))   ;; the top of the stack, and the
		   (remove-if-not #'SystemEntry-p (subseq Stack S))))))) ;; next and then between the split and the top. 


(defun sg-defvar-entryp (entry)
  "Return t iff the entry is a defvar entry."
  (eq (help-entryprop-type (SystemEntry-Prop entry))
      'define-var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collection of system entries.  
;; The System Entry index consists of a list of all unique
;; system entries sorted s.t. the solved equations are
;; listed first followed by the unsolved euqation entries
;; lastly listing the non equation entries.  
;;
;; This index is searched by the solution checking code below
;; for individual non-eqn entries and multiple equations.  
;;
;; The collection proceed by cycling though each node in the
;; graph and collecting the entries from each path accounting
;; for the splits and joins in the path.  Identical entries
;; are then merged and sorted to place the solved equations 
;; first in the listing.
;; 

(defun sg-collect-sysent-index (Graph)
  "Collect the system entries index from the bubblegraph."
  (let ((Index (sg-define-index Graph)))      
    (dotimes (n (length Index))
      (setf (SystemEntry-Index (nth N Index)) n))
    Index))
    

(defun sg-define-index (Graph)
  "Define the contents of the index."
  (sg-sort-sysents-for-eqns
   (merge-duplicate-systementries
    (append (loop for Q in (car Graph)
		append (Qnode-Entries Q))
	    (loop for E in (Cadr Graph)
		append (Enode-Entries E))))))
    

(defun sg-sort-sysents-for-eqns (Entries)
  "Sort the entries s.t the equation entries preceed the non-eqn entries."
  (let ((eq) (neq))
    (multiple-value-setq (eq neq)
      (set-split #'(lambda (x) 
		     (let ((type (help-entryprop-type (SystemEntry-Prop x))))
		       (or (equalp type 'eqn) (equalp type 'given-eqn))))
		 Entries))
    (append eq neq)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sg-subst-graph-sysents
;; Once the *sg-index is built the SystemEntries within the 
;; graph will contain redundancies.  Prior to using them the
;; entries need to be replaced by the fully merged entries 
;; within the *SG-Entries* index.  This code cycles through
;; the Bubblegraph performing those substitutions.

(defun sg-subst-graph-sysents (Entries Graph)
  "Substitute the entries back into the graph."
  (dolist (Q (car Graph))
    (setf (Qnode-Entries Q) 
      (sg-sysents->sysents (Qnode-Entries Q) Entries)))
	
  (dolist (E (cadr Graph))
    (setf (Enode-Entries E) 
      (sg-sysents->sysents (Enode-Entries E) Entries))))


(defun sg-sysents->Sysents (Entries Index)
  "Substitute the entries in Entries for matching ones in index."
  (let ((tmp))
    (remove-duplicates 
     (loop for E in Entries
	 do (setq tmp (find (SystemEntry-Prop E) Index
			    :key #'SystemEntry-Prop :test #'unify)) ; AW: was equal
	 when (null tmp)
	 do (error "No matching entry in index~% ~A~% ~A." E Index)
	 else collect tmp))))


;;-------------------------------------------------------------------------
;; matching of equations and entries.
;; Match the entries and equations in the system.  This allows 
;; for a mapping between equations and entries at runtime.
;; This is related to the match-eqn->entry code below used to match
;; between the elements for the algebra system.
(defun sg-match-eqn-entries (Eqns Entries)
  "Search the eqns for their entry matches."
  (dotimes (N (length Eqns))           ;; Add space for the 
    (setf (nth n Eqns)                 ;; entry storage.
      (append (nth n Eqns) (list nil))))
  (sg-pair-eqn-entries Eqns Entries)
  Eqns)


(defun sg-pair-eqn-entries (Eqns Entries)
  (let ((tmp))
    (dolist (E Eqns)
      (setq tmp (sg-find-eqn->entry (cadr E) Entries)) 
      (if (null tmp) (error "Unmatched eqn entry found in setup ~A." (cadr E)))
      (setf (nth 2 E) tmp))))


(defun sg-find-eqn->entry (Eqn Entries)
  "Find the matching entry for eqn if one exists."
  (find eqn Entries
	:key #'(lambda (e) (cadr (SystemEntry-Prop E)))
	:test #'unify))  ;handle keywords properly

;;-------------------------------------------------------------------------
;; Setup Solutions
;; The solutions as read in from the problem file consist of a set of 
;; base equations (any equation save derived equations) and a set of
;; bubblegraph nodes. 
;;
;; Prior to being used by the solution graph three things must happen.
;; Firstly the set of equations must be passed to the Algebra DLL to 
;; form a canonical equation set.  This process will return an integer ID.
;;
;; Secondly the set of entries within the solution must be collected.  
;; This is done by collecting the contents of the entries fields for each
;; node within the solution.
;;
;; Lastly the solution itself must be placed in an sgsol struct including
;; the count for use by the matching code.

(defun sg-setup-solutions (Solutions Graph Eqns)
  ; special handling for no-quant problems
  (if (no-quant-problem-p *cp*)
      (sg-setup-solutions-no-quant Graph Eqns)
   (sg-setup-solutions-quant Solutions Graph Eqns)))

(defun sg-setup-solutions-quant (Solutions Graph Eqns)
  "Define the list of solutions and store them in the algebra system."
  (setf *Sg-Solutions*
    (loop for S below (length Solutions)
	do (sg-add-solution S (nth S Solutions) Eqns)
	collect (sg-encode-solution S (nth S Solutions) Graph))))

(defun sg-add-solution (Num Solution Eqns)
  "Add the numbered solution to the system"
  (let ((Eq))
    (dolist (E (EqnSet-AllEqns Solution))
      (when (eqn-entry-type-p (eqn-type E)) 
	(if (setq eq (find (Eqn-Algebra E) Eqns
			   :key #'cadr :test #'equalp))
	    (Solver-IndyAddEq2Set Num (car Eq))
	  (error "NonIndy Solution Supplied ~A~% ~A." E Solution))))))

(defun sg-encode-solution (Num Solution Graph)
  "Translate elements of the solution for use."
  (make-sgsol 
   :num Num
   :Entries (remove-duplicates
	     (loop for N in (EqnSet-Nodes Solution)
		 append (BGNode-Entries N)))))

;; Variant for non-quant problems: these may include equation
;; writing steps (for entering given values of variables, say)
;; but have no solution sets since there is no quantity sought.
;; We collect any equations in the equation index into a dummy 
;; solution set 0 to be registered with the solver so these
;; equation entries can be interpreted through the existing
;; solver machinery. 
;; This still won't work for equations in a non-quant part
;; of a hybrid problem including quantitative soughts. 

(defun sg-setup-solutions-no-quant (Graph SG-Eqns)
 ; only do if we have some equations
 ; note each element in sg-eqns is a list formed by eqns->help-sys-eqns
 ; containing index, algebra, and system entry
 ; e.g (1 (= 0 foo) [System-Entry: ...])
 (when SG-Eqns
  ;; instead of sg-add-solution above:
  ;; look through eqn index. For every equation matching one 
  ;; in SG-Eqns, add it's sg index to current set 0
  (dolist (E (Problem-EqnIndex *cp*))
      (when (eqn-entry-type-p (eqn-type E)) 
	(if (setq eq (find (Eqn-Algebra E) SG-Eqns
			   :key #'cadr :test #'equalp))
	    (Solver-IndyAddEq2Set 0 (car Eq)))))
  ;; instead of sg-encode-solution above
  ;; map solution 0 to all entries in all eq-nodes
  (let ((entries 
	  ; maybe could use:
	  ; (reduce #'union (mapcar #'BGNode-Entries (bubble-graph-Enodes Graph)))
          (remove-duplicates
               (loop for N in (bubblegraph-Enodes Graph)
		     append (BGNode-Entries N)))))
     (setf *Sg-Solutions*
         (list (make-sgsol :num 0
	                   :Entries entries))))))







;;=========================================================================
;; check-solutions.
;; Given a student entry set its state depending upon its presence in the
;; set of SystemEntries.
;;
;;-------------------------------------------------------------------------
;; sg-match-entry
;; Given a student entry the code here ascertains it's type and then 
;; calls the appropriate subfunction to obtain the match(es).  The result
;; of this matching is then placed into the PossibleCinterps of the 
;; Studententry and returned.  
;; If the entry is an equation then it will be passed to the
;; equation matching code.  Else it will be passed to match-non-eq-entry.
;;
;; Once the prop has been matched it will be returned with the state of each
;; interp marked on the interp itself

(defun sg-match-studententry (Entry)
  (setf (StudentEntry-PossibleCInterps Entry)
	(if (help-eqn-entryprop-p (subseq (StudentEntry-prop entry) 0 2))
	    (sg-match-eqn-num (StudentEntry-ID Entry))
	    ;; Given a help entry prop locate the SystemEntry that matches 
	    ;; it from the *sg-entries* index and return it in a list.  
	    ;; If none is found return null.
	    (let ((match (find-ErrorInterp-correct entry)))
	      (when match
		(list (sg-mark-interp (ErrorInterp-intended match))))))))


(defun find-ErrorInterp-correct (entry)
  "Returns best match if correct, nil if incorrect or no match"
  (let (tests)
    ;; select tests that always apply 
    (dolist (eh (remove 'nil **entry-tests** :key #'EntryTest-apply 
			:test-not #'eql))
      (dolist (test (check-err-conds eh entry))
	;; each successful test will be either correct or incorrect
	(if (eq (ErrorInterp-state test) **correct**)
		       (push test tests)
		       (return-from find-ErrorInterp-correct nil))))
    ;; select best result from all applicable tests
    (when tests (select-error-interpretation tests))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sg-decompose-eqn
;; Occasionally for the purposes of the what's wrong help, the system
;; will need to decompose an arbitrary equation.  The code here passes
;; the equation into joel's algebra dll occupying the next availible space
;; above the student slots (out of the student's sight).  If it can be
;; entered it will then be decomposed and the decompositions returned
;; to the caller.  If it cannot be decomposed then nil will be returned.
(defun sg-decompose-eqn (Algebra)
  "Decompose the supplied eqn algebra."
  (when (not (solver-equation-redp algebra))
    (let ((R (solver-studentaddokay 
	    *Solver-temp-eqn-slot* Algebra)))    
      (setq R (when (and (not (stringp R)) (= 0 R))
		  (remove nil (sg-match-eqn-num *Solver-temp-eqn-slot*))))
      (solver-studentemptyslot *Solver-temp-eqn-slot*)
      R)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sg-match-eqn
;; Given a student entry id cycle through each solution in *sg-Solutions*
;; For each solution obtain the set of base equations (possibly null) from
;; the solution that this equation decomposes into.  Return the list of 
;; these sets.
;;
;; The equation decomposition is controlled by the Algebra DLL.  Given a
;; previously entered student equation id and a set id the system will
;; return an list of the form (<Status> <Definite> <Possible>)
;;
;; <Status> is an integer indication how independent the equation is from
;; the set ranging from 0: totally independent to 4 definitely dependent
;; on the equations in <Definite>  Midway values from **sg-nonindy-1-thesh*
;; to **sg-nonindy-2-thresh** are composed of the equations in <Definite>
;; and in <Possible>.
;;
;; <definite> and <possible> are lists of canonical equation id's which
;; correspond to the indicies of the canonical equation system entries
;; in the *sg-Entries* index.
;;
;; Then the eqn interps are marked in addition to the usual markings for 
;; premature substitutions of values and set activity.

(defun sg-match-eqn-num (Eqn)
  "Check the eqn number student Entry."
  (loop for S in *SG-Solutions*
      collect (sg-set-decomp-eqn (sgsol-Num S) Eqn)))

;;; Note that as of the code Nonindy 1 thresh now overrides the nonindy
;;; 2 thresh by executing in the test prior to it.  It may be reactivated
;;; later on as student experimentation provides more data.
(defparameter **sg-nonindy-1-thresh** 2 "Threshold for nonindy nodes taking from the linear expansion only")
(defparameter **sg-nonindy-2-thresh** 1 "Threshold for nonindy nodes taking from both linear expansion and mightdepends.")

(defun sg-set-decomp-eqn (Set Eqn)
  "Obtain the set of base equations for EQN from set SET."
  (let ((S (Solver-StudentisIndependent Set Eqn)))       
    (cond ((<= **sg-nonindy-1-thresh** (first S))              
	   (sg-mark-eqn-interp
	    (sg-subst-enums->entries (second S))))                
	  ((<= **sg-nonindy-2-thresh** (first S))              
	   (sg-mark-eqn-interp 
	    (append (sg-subst-enums->entries (second S)) 
		    (sg-subst-enums->entries (third S))))))))


(defun sg-mark-eqn-interp (Interp)
  "Mark the interps including premature subst and inactive."
  (let ((I (sg-mark-interp Interp)))
    (when (eq **Correct** (car I))
      (if (and **Test-For-Premature-Subst**
	       (SystemEntries-Premature-substp I))
	  (setf (car I) **Premature-Subst**)))
    I))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark interp(s)
;; Add the interp's state to the interp itself.

(defun sg-mark-interp (Interp)
  "Mark the state of the interp not accounting for sets."
  (when Interp
    (cons (sg-check-interp Interp) 
	  (sg-unmark-interp Interp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check interp(s)
;; Given an interpretation it is necessary to determine its state
;; which will be one of **Correct** **Incorrect** **Forbidden**
;; **Dead-Path** or **Premature-Entries** The selected state will
;; be pushed onto the car of the interp and can then be used by 
;; the help system to determine what interp (if given a choice) 
;; to use.
;;
;; At present this code does not test for set membership or 
;; premature substitution.  That is handled by the 
;; sg-mark-eqn-interps code above.

(defun sg-check-interp (Interp)
  "Mark the state of the interp not accounting for sets."
  (let ((State (Systementries->State (sg-unmark-interp Interp)))) 
    ;; Check for **Forbidden** **Dead-path**
    (if (and **Test-For-Prematurity**
	     (eq **Correct** State)
	     (SystemEntries-Prematurep (sg-unmark-interp Interp)))
	**Premature-Entry**
      State)))


;;----------------------------------------------------------------------
;; enter student entry
;; Given a student entry that has had its cinterp set by external
;; code the process of entering it involves pushing the 
;; StudentEntry is pushed onto the Entered field
;; of each SystemEntry in the Cinterp.  
(defun sg-enter-StudentEntry (Entry)
  "For each system entry in the cinterps mark it with entry."
	     
  ; For equation entries, apply constraint loss check if flag is set
  (when (and **Filter-Constraint-Losses** 
             (eqn-prop-p (StudentEntry-Prop Entry))) ; it's an eqn entry
    (sg-filter-constraint-losses Entry))
  
  ;; Print out Entry and Interps
  (when (sg-unmark-interp (StudentEntry-Cinterp Entry))
    (format *debug-help* 
	    "Entering Interp:=====================================~%")
    (format *debug-help* "~A~%" (StudentEntry-Cinterp Entry)))
  
  (dolist (E (sg-unmark-interp (studentEntry-Cinterp Entry)))
    (push Entry (SystemEntry-Entered E))))


;;---------------------------------------------------------------------
;; Filter-constraint-losses
;; When the student makes an eqn SystemEntry they may combine several
;; equations into a single one and in so doing lose some of the factors
;; that will be necessary later.  Thus they may walk themselves into 
;; a situation where the help system thinks that they are done but the
;; equations that they have entered cannot be used algebraically to 
;; solve for the sought.  This function will test for that and, if so
;; return a null interp.  Else it will return the interp itself.
;;
;; This function relies heavily on code located in solver.cl (for lack
;; of a better location as it is algebraic code) this code is passed 
;; the algebra of the interps themselves and returns the dimensions of 
;; the set.
;;
;; This code compares the dimensions of the eqn and the interp.  If they
;; are equal or the Student Eqn has less dimensions than the interp then
;; nothing is done.  If the eqn has more dimensions than the interp then
;; the cinterp will be set to nil so none of them are marked.
(defun sg-filter-constraint-losses (Entry)
  "Filter the StudentEntry for constraint loss."
  (let ((Mark (sg-get-interp-mark (StudentEntry-Cinterp Entry)))
	(DE (sg-calc-eqn-dimensions (StudentEntry-ParsedEqn Entry)))
	(DI (sg-calc-interp-dimensions (StudentEntry-Cinterp Entry)))) 
  
    (when (> DE DI) 
      (format t "Interp not entered due to constraint loss:~%")
      (format T "  student eqn dimensions=~a, interp dimensions=~a~%" DE DI)
      (setf (StudentEntry-Cinterp Entry) 
	(if (null Mark) nil
	  (sg-add-mark-to-interp Mark Nil))))))


;;; sg-calc-eqn-dimensions
;;; Given an equation calculate its dimensions according 
;;; to the number of canonical variables that it contains.
(defun sg-calc-eqn-dimensions (Eqn)
  "Calculate the dimensions of the eqn by sysvars."
  (calc-exp-dimensions #'sysvar-p Eqn))

;;; sg-calc-interp-dimensions
;;; Given an interp calculate the dimensions of the equations
;;; within it again according to the sysvars.
(defun sg-calc-interp-dimensions (Interp)
  "Calculate the eqn dimensions of the Interp."
  (calc-exp-set-dimensions 
   #'sysvar-p 
   (remove-if 
    #'null (mapcar #'get-eqn-SystemEntry-algebra 
		   (sg-unmark-interp Interp)))))

;;---------------------------------------------------------------------
;; sg-delete-Studententry
;; Deletion of a student entry is essentially a direct reversal
;; of the entry process.  Here the entry is removed from the entered
;; fields of the System Entries in its cinterp.  
(defun sg-delete-StudentEntry (Entry)
  "Remove the markings from each node in the Entrie's CInterp."
  ; AW: unneeded variable check will mark define-var sysentries
  ; as entered by some student equation entries, even though 
  ; the define-vars are NOT saved in the studentEntry's Cinterp.
  ; Change to loop over ALL system entries to allow for this case.
  ; Hairy technique, probably should be changed.
  (dolist (E *sg-entries*) ; was: (E (studentEntry-Cinterp Entry))
    (setf (SystemEntry-Entered E)
      (remove Entry (SystemEntry-Entered E)))))



;;==========================================================================
;; Utility functions.
;;----------------------------------------------------------
;; Given a list of integers collect the entries that they
;; index from *sg-entries*

(defun sg-subst-nth-entries (lst)
  "Collect a list of indexed entries from the entry index."
  (loop for E in lst 
      collect (nth E *sg-Entries*)))


;;--------------------------------------------------------------
;; sg-fetch-entry-props 
;; Helper returns list of all entry propositions matching given pattern.

(defun sg-fetch-entry-props (prop-pat) 
  "fetch all entry propositions that match given proposition form"
  (filter-expressions prop-pat (mapcar #'SystemEntry-Prop *SG-Entries*)))


;; utility function to find system entry for a given vector quantity
;; Depends on structure of vector entry proposition
(defun sg-find-vector-entry (vector-quant)
 "return the system entry for a vector quantity"
 (find `(vector ,vector-quant ?dont-care) *sg-entries* 
        :test #'unify :key #'SystemEntry-Prop))


;;-------------------------------------------------------------
;; given a canonical equation number match it with a system
;; entry as necessary.
(defun sg-subst-enums->entries (enums)
  (loop for e in enums
      collect (sg-match-enum->entry e)))

(defun sg-match-enum->entry (enum)
  (let ((e (nth 2 (nth enum *sg-eqns*))))
    (if (null e)
	(error "Unmatched eqn requested. ~A" (nth enum *sg-Eqns*))
      e)))

;;-------------------------------------------------------------
;; Remove the marking from an interp if it is present. 
(defun sg-unmark-interp (interp)
  (if (SystemEntry-P (car Interp))
      Interp
    (cdr Interp)))

;;-------------------------------------------------------------
;; Add a marking to an interp
(defun sg-add-mark-to-interp (Mark Interp)
  "Add the specified marking to the interp."
  (cons Mark Interp))

;;-------------------------------------------------------------
;; Get the marking from an interp.
(defun sg-get-interp-mark (Interp)
  (when (not (SystemEntry-p (car Interp)))
    (car Interp)))

;;--------------------------------------------------------------
;; Get the SystemEntry's op's hints

; Following returns spec to be used as tail of a list of specs passed
; to make-hint-seq when appending operator hints. It returns a spec
; for a single function hint. When this spec is expanded, make-hint-seq
; will wind up getting called (again) on the operator hint spec list. 
; We build this spec for a delayed call so it will include the :optail
; arg, which is needed to log the operator name in an :assoc.
; [This technique copied from hint-target-entry in nextstephelp.cl to
; fix bug 834 AW]
(defun sg-map-systementry->hints (entry)
  (let ((step (car (SystemEntry-sources entry)))) ; a csdo
    `((function make-hint-seq 
		       ,(collect-step-hints step)
		       :OpTail ,(list (csdo-op Step))))))

; collect tags of operator instances that made this system entry
; opinst tag is of form (WRITE-MASS-COMPOUND (BOOK PACKAGE))
(defun sg-map-systementry->opinsts (entry)
 (remove-duplicates (mapcar #'csdo-op (SystemEntry-sources entry))
                    :test #'equalp))

; collect set of names of operators that made this system entry
(defun sg-map-systementry->opnames (entry)
 (remove-duplicates 
   (mapcar #'first (sg-map-systementry->opinsts entry))))

; get name of one operator that made this system entry. 
; For equations, set of opinsts should be a singleton (don't have
; two operators generating the same equation), so this can be
; more convenient than list.
(defun sg-map-systementry->opname (entry)
  (first (sg-map-systementry->opnames entry)))

;; For treating entries as optional
;; An entry is optional iff for every enode in the graph:
;; either the node doesn't contain it at all or
;; there exists a path through that node that doesn't 
;; include the entry. This requires traversing the hairy
;; psm graph structure
;;
;; NB: Optionality here is defined by path structure and 
;; will not exclude implicit eqn entries.  Other code in 
;; next-step-help and grading will treat implicit eqn entries
;; as effectively optional in a different way.
(defun sg-systementry-optional-p (entry &optional (problem *cp*))
  (every #'(lambda (enode) (does-not-require enode entry))
         (bubblegraph-enodes (problem-graph problem))))  

(defun does-not-require (enode entry)
  (cond ((not (member entry (enode-entries enode))) T)
        (T (some-path-through-omits (enode-path enode) entry))))

(defun some-path-through-omits (path entry)
   (cond ((null path) T)
         ((and (csdo-p (car path)) ; hit a CSDO for this entry
               (member (car path) (SystemEntry-sources entry)))
	     NIL)
	 ((cschoose-p (car path)) 
	   ;; car path is (CHOOSE ((step 1) (step 2)) ((step1 step2)))
	   ;; so everything to the end is inside the current CHOOSE item.
	   (some #'(lambda (path) (some-path-through-omits path entry))
	         (cdr (car path))))
	 ;; !!! ignoring splits and joins since we don't
	 ;; generate them any more
	 (T (some-path-through-omits (cdr path) entry))))

; Utility func to pull out only the required entries in an enode.
; Because intended for use by grading, this DOES ignore implicit eqns. 
(defun enode-required-entries (enode)
   (remove-if #'(lambda (entry) 
                   (does-not-require enode entry))
     (remove-if #'SystemEntry-implicit-eqnp
              (enode-entries enode))))

;;-------------------------------------------------------------
;; debugging code.

(defun ploop ()
  (loop for S in *SG-Entries* do (print-full-SystemEntry S)))

(defun sg-trace ()
  (sg-trace-setup)
  (sg-trace-use))

(defun sg-trace-use ()
  (trace sg-match-eqn
	 sg-match-eqn-num
	 sg-set-decomp-eqn
	 sg-mark-eqn-interp
	 Systementries->State
	 sg-mark-interp
	 sg-check-interp
	 
	 sg-delete-StudentEntry
	 sg-enter-StudentEntry

	 ))

(defun sg-trace-setup ()
  (trace sg-psmg->sysents
	 sg-drop-snj
	 sg-generate-sysents
	 sg-generate-sysent
	 sg-collect-sysent-prereqs
	 
	 sg-collect-sysent-index
	 sg-sort-sysents-for-eqns
	 merge-duplicate-systementries
	 
	 sg-subst-graph-sysents
	 sg-sysents->sysents
  
	 sg-prime-algebra
	 solver-indyempty 
	 solver-indyaddvar 
	 solver-indydoneaddvar 
	 solver-indyaddequation

	 sg-match-eqn-entries
	 sg-pair-eqn-entries
	 
	 sg-setup-solutions
	 sg-add-solution
	 sg-encode-solution))
	

