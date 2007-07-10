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


(defun print-html-entry-pointers (bgnode n &optional (Stream t))
  "print html td giving pointers to entries associated with a given bgnode"
  (format stream "      <td>")
  (loop for entry in (distinct-SystemEntries (bgnode-entries bgnode))
	and firstcol = t then nil 
	do
	(unless firstcol (format stream "<br>~%      "))
	(format stream 
		"<a href=\"#p~D.~D\"><code>~S</code></a>"
		n (SystemEntry-index entry)
		(SystemEntry-prop entry)))
  (format stream "</td>~%"))


(defun distinct-SystemEntries (entries)
  "Make sorted list of distinct entries"
  (sort (remove-duplicates (copy-list entries) 
			   :key #'SystemEntry-prop :test #'unify)
	#'expr< :key #'SystemEntry-prop))


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

  (do ((n 0 (+ n 1))) ((>= n (length (Problem-Solutions Problem))))
    (format Stream "<h2>Solution ~A</h2>~%" n)
    (let ((soln (nth n (problem-solutions problem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (format Stream "<table>~%")
      (format Stream "<caption>Entries and Operators</caption>~%")
      (format Stream "<tr><th>Entry</th><th>Operator</th><th colspan=\"0\">Hints</th></tr>~%")
      (dolist (entry (distinct-SystemEntries 
		      (mappend #'bgnode-entries (EqnSet-nodes soln))))
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
		       (mapcar #'any-turn-text
			       (mapcar #'(lambda (type) 
					 (make-hint-seq 
					  (collect-step-hints opinst :type type))) 
				       '(point bottom-out))))
	       )))
      (format Stream "</table>~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (format Stream "<table>~%")
      (format Stream "<caption>Principles (Problem Solving Methods)</caption>~%")
      (format Stream "<tr><th>Id</th><th>Name</th><th>Action</th><th colspan=\"0\">Entries</th></tr>~%")
      (dolist (eqn (Eqnset-Eqns soln))
	(format Stream "<tr><td class=\"~A\"><code>~S</code></td><td>~A</td><td>~A</td>~%" 
		(equation-complexity (lookup-expression->Equation 
				      (Enode-id eqn)))
		(enode-id eqn)
		(psm-english (enode-id eqn))
		(psm-exp (enode-id eqn)))
	(print-html-entry-pointers eqn n stream)
	(format Stream "</tr>~%"))
      (format Stream "</table>~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (format Stream "<table>~%")
      (format Stream "<caption>Quantities</caption>~%")
      (format Stream "<tr><th>Id</th><th>Name</th><th>Symbol</th><th colspan=\"0\">Entries</th></tr>~%")
      (dolist (eqn (remove-if-not #'Qnode-p (EqnSet-nodes soln)))
	(format Stream "<tr><td><code>~S</code></td><td>~A</td><td>~A</td>~%" 
		(qnode-exp eqn)
		(nlg (qnode-exp eqn))
		(qnode-var eqn))
	(print-html-entry-pointers eqn n stream)
	(format Stream "</tr>~%"))
      (format Stream "</table>~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ))
  (format Stream 
	  (strcat
	   "</body>~%"
	   "</html>~%"))
  )

;; This really needs to be broken up into a number of subroutines.
(defun print-html-problem-diffs (problem 
				     &optional (Stream t)
				     ;; don't write out projections
				     (ignore '(projection . ?whatever)))
  (format Stream 
	  (strcat
	   "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%"
	   "<html> <head>~%"
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
  (format Stream "<table>~%")
  (format Stream "<caption>Solution 0</caption>~%")
  (format Stream "~{<tr>~{<td class=\"~A\">~A</td><td>~A</td><td>~A</td>~}</tr>~%~}" 
	  (mapcar #'(lambda (x) (list 
				 (equation-complexity 
				  (lookup-expression->Equation x))
				 (psm-english x) (psm-exp x) 
				      (format nil "<code>~S</code>" x)))
		  (mapcar #'enode-id 
			  (EqnSet-Eqns (first (Problem-Solutions Problem))))))
  (format Stream "</table>~%~%")
  (do ((n 1 (+ n 1))) ((>= n (length (Problem-Solutions Problem))))
    (let ((diff1 (mapcar #'(lambda (x) (list 
					(equation-complexity 
					 (lookup-expression->Equation x))
					(psm-english x) 
					(format nil "<code>~S</code>" x)))
			 (find-diff-ids (nth 0 (Problem-Solutions Problem))
					(nth n (Problem-Solutions Problem))
					ignore)))
	  (diff2 (mapcar #'(lambda (x) (list 
					(equation-complexity 
					 (lookup-expression->Equation x))
					(psm-english x) 
					(format nil "<code>~S</code>" x)))
			 (find-diff-ids (nth n (Problem-Solutions Problem))
					(nth 0 (Problem-Solutions Problem))
					ignore))))
      (format Stream "<table>~%")
      (format Stream "  <caption>Solution ~A</caption>~%" n)
      (format Stream "  <tr><th rowspan=~A>Added:</th>~{<td class=\"~A\">~A</td><td>~A</td>~}</tr>~%" 
	      (length diff1) (car diff1))
      (format Stream "~{  <tr>~{<td class=\"~A\">~A</td><td>~A</td>~}</tr>~%~}" (rest diff1))
      (format Stream "  <tr><th rowspan=~A>Removed:</th>~{<td class=\"~A\">~A</td><td>~A</td>~}</tr>~%" 
	      (length diff2) (car diff2))
      (format Stream "~{  <tr>~{<td class=\"~A\">~A</td><td>~A</td>~}</tr>~%~}" (rest diff2))
      (format Stream "  <tr><td colspan=3>Ignoring ~A</td></tr>~%" 
	      (psm-english ignore))
      (format Stream "</table>~%~%")
      ))
  (format Stream 
	  (strcat
	   "</body>~%"
	   "</html>~%"))
  )

(defun dump-html-problem-solutions (problem &optional (path *andes-path*))
  (let ((*print-pretty* NIL) ;disble line breaks
	(str (open (merge-pathnames 
		    (format nil "~A.html" (problem-name problem)) path)
		   :direction :output :if-exists :supersede)))
    (print-html-problem-solutions problem str)
    (close str)))

(defun dump-style-file (&optional (path *andes-path*))
  ;; Make file with html styles
  (let ((css (open (merge-pathnames  "main.css" path)
 		   :direction :output :if-exists :supersede)))
    (format css 
	    (strcat
;	     "  th {vertical-align: top; text-align: right;}~%"
	     "  td {vertical-align: top;}~%"
	     "  td.MAJOR {color: red;}~%"
	     "  td.DEFINITION {color: green;}~%"
	     "  caption {font-size: larger; font-weight: bolder;}~%"
	     "  table,caption {margin-left: auto; margin-right: auto; ~%"
	     "         border: 1px solid black; margin: 2px; }~%"
;	     "         border-spacing: 4; margin-bottom: 5; margin-top: 5;}~%"
	     ))
    (close css)))

(defun find-diff-ids (x y &optional ignore)
  (remove-if #'(lambda (match) (unify match ignore))
		   (set-difference (mapcar #'enode-id (EqnSet-eqns y)) 
				   (mapcar #'enode-id (EqnSet-eqns x)) 
				   :test #'equalp)))


