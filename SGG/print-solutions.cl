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
;;;;
;;;;            Routines to print out problem solutions.
;;;;
;;;;   In principle, this could be in KB or Knowledge, but 
;;;;   Help is used to read the problem in and initialize csdo.
;;;;   It is also used to determine hints.
;;;; 

;;;;=================================================================
;;;; Reporting Code.
;;;;
;;;; For stephanies edification (and probably the NA people as well)
;;;; this code will take a problem and print out the solutions in
;;;; the following form:
;;;;
;;;; <Name>
;;;; <Solution Number>
;;;;   <List of psms>
;;;;   <List of Entries>
;;;;   <List of Equations>
;;;;
;;;; This will give the individual an idea of the format for the 
;;;; solution and its contents.

(defun ps ()
  (print-problem-solutions *cp*))

(defun print-problem-solutions (problem &optional (Stream t) 
					;; don't write out projections
					(ignore '(projection . ?whatever)))
  (format Stream "~&~A~%" (make-string 79 :initial-element #\=))
  (format Stream "Problem Solutions: ~A~%" (problem-name Problem))
  (format Stream "~&~A~%" (make-string 79 :initial-element #\-))
  (format Stream "Solution 0: ~%")
  (print-numbered-report-eqnset (first (Problem-Solutions Problem)) Stream)
  (format Stream "~&~A~%" (make-string 79 :initial-element #\-))
  (do ((n 1 (+ n 1))) ((>= n (length (Problem-Solutions Problem))))
    (format Stream "Solution ~A:  ~@[          ignoring ~A~]~%" n ignore)
    (format Stream "~&+:  ~A~%-:  ~A~%" 
	    (find-diff-ids (nth 0 (Problem-Solutions Problem))
			   (nth n (Problem-Solutions Problem))
			   ignore)
	    (find-diff-ids (nth n (Problem-Solutions Problem))
			   (nth 0 (Problem-Solutions Problem))
			   ignore))
    (format Stream "~&~A~%" (make-string 79 :initial-element #\-))
    ))


(defun any-turn-text (x) (when x (turn-text x)))

(defun any-psmclass-complexity (x) (when x (PSMClass-complexity x)))

(defun print-html-entry-pointers (bgnode n &optional (Stream t))
  "print html td giving pointers to entries associated with a given bgnode"
  (format stream "      <td class=\"entry\">")
  (loop for entry in (distinct-SystemEntries (bgnode-entries bgnode))
	and firstcol = t then nil 
	do
	(unless firstcol (format stream "~%      "))
	(format stream 
		"<p><a href=\"#p~D.~D\"><code>~S</code></a>"
		n (SystemEntry-index entry)
		(SystemEntry-prop entry)))
  (format stream "</td>~%"))


(defun distinct-SystemEntries (entries)
  "Make sorted list of distinct entries"
  (sort (remove-duplicates (copy-list entries) 
			   :key #'SystemEntry-prop :test #'unify)
	#'expr< :key #'SystemEntry-prop))


(defun print-html-entries (entries n &optional (Stream t))
  (format Stream "<table>~%")
  (format Stream "<caption>Entries and Operators</caption>~%")
  (format Stream "<tr><th>Entry</th><th>Operator</th><th colspan=\"0\">Hints</th></tr>~%")
  (dolist (entry entries)
    (let ((ops (remove-duplicates (copy-list (SystemEntry-Sources entry))
				  :key #'csdo-op :test #'unify)))
      (loop for opinst in ops
	 and firstcol = t then nil
	 do
	   (format Stream  "<tr>")
	   (when firstcol
	     (format Stream "<td id=\"p~D.~D\" rowspan=\"~A\" class=\"entry\"><code>~S</code></td>~%"			   
		     n (SystemEntry-index entry)
		     (length ops)
		     (SystemEntry-prop entry)))
	   (format Stream "        <td class=\"op\"><code>~S</code></td>~{<td>~@[~A~]</td>~}</tr>~%" 		       
		   (csdo-op opinst)
		   (mapcar #'(lambda (hint) 
			       ;; don't display hints of type teach
			       (if (eql (OpHint-type hint) 'teach)
				   "&#8230;"
				 (format-hintspec 
				  (pick-other-spec ;pick a string hint
				   (OpHint-hintspecs hint)))))
			   (collect-step-hints opinst)))
	   )))
  (format Stream "</table>~%~%"))

(defun print-entries-operators (entries n &optional (Stream t))
  (dolist (entry entries)
    (let ((ops (remove-duplicates (copy-list (SystemEntry-Sources entry))
				  :key #'csdo-op :test #'unify)))
      (loop for opinst in ops
	 and firstcol = t then nil
	 do
	   (when firstcol (format stream "~A" (SystemEntry-prop entry)))
	   (format stream "~C~A~C~a~%" #\tab 
		   (csdo-op opinst) #\tab (car (csdo-op opinst)))))))

(defun print-html-psms (eqns n &optional (Stream t))
  (format Stream "<table>~%")
  (format Stream "<caption>Principles (Problem Solving Methods)</caption>~%")
  (format Stream "<tr><th>Id</th><th>Name</th><th>Action</th><th colspan=\"0\">Entries</th></tr>~%")
  (dolist (eqn eqns)
    ;; see Bug #1305: some enodes have no associated PSMClass
    (let ((class (any-psmclass-complexity
		  (lookup-expression->PSMClass (Enode-id eqn)))))
      (format Stream "<tr><td id=\"e~D.~D\" class=\"~A\"><code>~S</code></td><td class=\"~A\">~A</td><td>~A</td>~%" 
	      n (enode-Gindex eqn)
	      class
	      (enode-id eqn)
	      class
	      (psm-english (enode-id eqn))
	      (psm-exp (enode-id eqn)))
      (print-html-entry-pointers eqn n stream)
      (format Stream "</tr>~%")))
  (format Stream "</table>~%~%"))

(defun print-html-quantities (eqns n &optional (Stream t))
  (format Stream "<table>~%")
  (format Stream "<caption>Quantities</caption>~%")
  (format Stream "<tr><th>Id</th><th>Name</th><th>Symbol</th><th colspan=\"0\">Entries</th></tr>~%")
  (dolist (eqn eqns)
    (format Stream "<tr><td id=\"q~D.~D\"><code>~S</code></td><td>~A</td><td>~A</td>~%" 
	    n (qnode-GIndex eqn)
	    (qnode-exp eqn)
	    (nlg (qnode-exp eqn))
	    (qnode-var eqn))
    (print-html-entry-pointers eqn n stream)
    (format Stream "</tr>~%"))
  (format Stream "</table>~%~%"))

(defun print-html-problem-solutions (problem &optional (Stream t))
  ;;  Assume stream has UTF-8 encoding (default for sbcl)
  ;;  Should test this is actually true or change the charset to match
  ;;  the actual character code being used by the stream
  ;;  something like:
  (when (streamp Stream) 
    #+sbcl (unless (eq (stream-external-format Stream) ':utf-8)
	     (error "Wrong character code ~A, should be UTF-8" 
		    (stream-external-format Stream))))
  (format Stream 
	  (strcat
	   "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%"
	   "<html> <head>~%"
	   ;;	   "   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">~%"
	   "   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">~%"
	   "<link rel=\"stylesheet\" type=\"text/css\" href=\"main.css\">~%"
	   "<title>~A</title>~%"
	   "</head>~%"
	   "<body>~%"
	   "<h1>Problem Solutions for ~A</h1>~%"
	   "<p>~%") 
	  (problem-name Problem) (problem-name Problem))
  (mapcar #'(lambda (x) (format Stream "   ~A<br>~%" x)) 
	  (problem-statement Problem))
  (format Stream "</p>~%")
  
  (dotimes (n 1) ;(length (Problem-Solutions Problem)))
    (format Stream "<h2>Solution ~A</h2>~%" n)
    (let ((soln (nth n (problem-solutions problem))))
      (print-html-psms (Eqnset-Eqns soln) n stream)
      (print-html-quantities (remove-if-not #'Qnode-p (EqnSet-nodes soln)) 
			     n Stream)
      (print-html-entries (distinct-SystemEntries 
			   (mappend #'bgnode-entries (EqnSet-nodes soln)))
			  n Stream)
      ))
;;; This should be broken up into different functions

  (do ((n 1 (+ n 1))) ((>= n (length (Problem-Solutions Problem))))
    (let ((nn (length (Problem-Solutions Problem)))
	  (sol0 (first (problem-solutions problem)))
	  (soln (nth n (problem-solutions problem))))
    (format Stream "<h2>Solution ~A versus Solution 0</h2>~%" n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format Stream "<table>~%")
    (format Stream "<caption>Principles (problem solving methods)</caption>~%")
    (format Stream "<tr><th>added</th><th>removed</th></tr>~%")
    (format Stream "  <tr><td class=\"entry\">~%")
    (dolist (eqn (set-difference 
		  (EqnSet-eqns soln)
		  (EqnSet-eqns sol0)
		  :key #'Enode-id :test #'unify))
      (format Stream "    <p><a href=\"#e~D.~D\" class=\"~A\"><code>~S</code></a>~%" 
	      nn (Enode-GIndex eqn)
	      ;; see Bug #1305: some enodes have no associated PSMClass
	      (any-psmclass-complexity
		 (lookup-expression->PSMClass (Enode-id eqn)))
	      (Enode-id eqn)))
    (format Stream "  </td><td class=\"entry\">~%")
    (dolist (eqn (set-difference 
		     (EqnSet-eqns sol0)
		     (EqnSet-eqns soln)
		     :key #'Enode-id :test #'unify))
      (format Stream "    <p><a href=\"#e~D.~D\" class=\"~A\"><code>~S</code></a>~%" 
	      nn (Enode-GIndex eqn) 
	      ;; see Bug #1305: some enodes have no associated PSMClass
	      (any-psmclass-complexity
	       (lookup-expression->PSMClass (Enode-id eqn)))
	      (Enode-id eqn)))
    (format Stream "  </td></tr>~%")
    (format Stream "</table>~%")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (format Stream "<table>~%")
    (format Stream "<caption>Quantities</caption>~%")
    (format Stream "<tr><th>added</th><th>removed</th>~%")
    (format Stream "  <tr><td class=\"entry\">~%")
    (dolist (qnode (set-difference 
		  (remove-if-not #'Qnode-p (EqnSet-nodes soln))
		  (remove-if-not #'Qnode-p (EqnSet-nodes sol0))
		  :key #'Qnode-exp :test #'unify))
      (format Stream "    <p><a href=\"#q~D.~D\"><code>~S</code></a>~%" 
	      nn (Qnode-GIndex qnode) (Qnode-exp qnode)))
    (format Stream "  </td><td class=\"entry\">~%")
    (dolist (qnode (set-difference 
		  (remove-if-not #'Qnode-p (EqnSet-nodes sol0))
		  (remove-if-not #'Qnode-p (EqnSet-nodes soln))
		  :key #'Qnode-GIndex :test #'unify))
      (format Stream "    <p><a href=\"#q~D.~D\"><code>~S</code></a>~%" 
	      nn (Qnode-GIndex qnode) (Qnode-exp qnode)))
    (format Stream "  </td></tr>~%")
    (format Stream "</table>~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (format Stream "<table>~%")
    (format Stream "<caption>Entries and Operators</caption>~%")
    (format Stream "<tr><th>added</th><th>removed</th>~%")
    (format Stream "  <tr><td class=\"entry\">~%")
    (dolist (entry (distinct-SystemEntries
		    (set-difference 
		     (mappend #'bgnode-entries (EqnSet-nodes soln))
		     (mappend #'bgnode-entries (EqnSet-nodes sol0))
		     :key #'SystemEntry-prop :test #'unify)))
            (format Stream "    <p><a href=\"#p~D.~D\"><code>~S</code></a>~%" 
		    nn (SystemEntry-index entry) (SystemEntry-prop entry)))
    (format Stream "  </td><td class=\"entry\">~%")
    (dolist (entry (distinct-SystemEntries
		    (set-difference 
		     (mappend #'bgnode-entries (EqnSet-nodes sol0))
		     (mappend #'bgnode-entries (EqnSet-nodes soln))
		     :key #'SystemEntry-prop :test #'unify)))
            (format Stream "    <p><a href=\"#p~D.~D\"><code>~S</code></a>~%" 
		    nn (SystemEntry-index entry) (SystemEntry-prop entry)))
    (format Stream "  </td></tr>~%")
    (format Stream "</table>~%")
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ))

  (when (> (length (problem-solutions problem)) 1)
    (format Stream "<h2>All Solutions</h2>~%")
    (let ((n (length (problem-solutions problem))))
      (print-html-psms (second (problem-graph problem)) n stream)
      (print-html-quantities (first (problem-graph problem)) n Stream)
      (print-html-entries (distinct-SystemEntries 
			   (mappend #'bgnode-entries 
				    (flatten1 (problem-graph problem))))
			  N Stream)))
    
    (format Stream (strcat
	   "</body>~%"
	   "</html>~%"))
  )

(defun print-solution-entries-operators (problem &optional (stream t))
  (format stream "~A~%~%" (problem-name Problem))
  (format stream "~{~A~%~}" (problem-statement Problem))
  (dotimes (n (length (Problem-Solutions Problem))) ;or 1
    (let ((soln (nth n (problem-solutions problem))))
      (format stream "~%")
      (print-entries-operators 
       (distinct-SystemEntries (mappend #'bgnode-entries (EqnSet-nodes soln)))
       n Stream))))

(defun dump-html-problem-solutions (problem &optional (path *andes-path*))
  (let ((*print-pretty* NIL) ;disble line breaks
	(str (open (merge-pathnames 
		    (format nil "~A.html" (problem-name problem)) path)
		   :direction :output :if-exists :supersede)))
    (print-html-problem-solutions problem str)
    (close str)))

(defun dump-entries-operators-problem-solutions (problem &optional (path *andes-path*))
  (let ((*print-pretty* NIL) ;disble line breaks
	(str (open (merge-pathnames 
		    (format nil "~A.txt" (problem-name problem)) path)
		   :direction :output :if-exists :supersede)))
    (print-solution-entries-operators problem str)
    (close str)))

(defun dump-style-file (&optional (path *andes-path*))
  ;; Make file with html styles
  (let ((css (open (merge-pathnames  "main.css" path)
 		   :direction :output :if-exists :supersede)))
    (format css 
	    (strcat
;	     "  th {vertical-align: top; text-align: right;}~%"
	     "  td {vertical-align: top;}~%"
             "  td.entry > p {margin:  0 0 0.75ex;}~%"
	     "  *.MAJOR {color: red;}~%"
	     "  *.DEFINITION {color: green;}~%"
	     "  caption {font-size: larger; font-weight: bolder;}~%"
	     "  table,caption {margin-left: auto; margin-right: auto; ~%"
	     "         border: 1px solid black; margin: 2px;"
;	     "         border-spacing: 4; margin-bottom: 5; margin-top: 5;
	     "  }~%"
	     ))
    (close css)))

(defun find-diff-ids (x y &optional ignore)
  (remove-if #'(lambda (match) (unify match ignore))
		   (set-difference (mapcar #'enode-id (EqnSet-eqns y)) 
				   (mapcar #'enode-id (EqnSet-eqns x)) 
				   :test #'equalp)))


