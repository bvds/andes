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


(defun distinct-SystemEntries (entries)
  "Make sorted list of distinct entries"
  (sort (remove-duplicates (copy-list entries) 
			   :key #'SystemEntry-prop :test #'unify)
	#'expr< :key #'SystemEntry-prop))

;; This really needs to be broken up into a number of subroutines.
(defun print-html-problem-solutions (problem 
				     &optional (Stream t))
  (format Stream 
	  (strcat
	   "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">~%"
	   "<html> <head>~%"
	   "   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">~%"
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
      (dolist (entry (distinct-SystemEntries 
		      (mappend #'bgnode-entries (EqnSet-nodes soln))))
	(let ((ops (remove-duplicates (copy-list (SystemEntry-Sources entry))
				      :key #'csdo-op :test #'unify)))
	  (loop for opinst in ops
	     and firstcol = t then nil
	     do
	       (format Stream  "<tr>")
	       (when firstcol
		   (format Stream "<td id=\"~D.~A\" rowspan=\"~A\"><code>(~{~A~^<br>~})</code></td>~%"
			   
			   n (sxhash (SystemEntry-prop entry))
			   (length ops)
			   (SystemEntry-prop entry)))
	       (format Stream "        <td><code>~{~A~^<br>~}</code></td>~{<td>~@[~A~]</td>~}</tr>~%" 
		       
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
      (format Stream "<caption>Entries</caption>~%")
      (dolist (eqn (Eqnset-Eqns soln))
	(format Stream "<tr><td class=\"~A\"><code>~A</code></td><td>~A</td><td>~A</td>~%" 
		(equation-complexity (lookup-expression->Equation 
				      (Enode-id eqn)))
		(enode-id eqn)
		(psm-english (enode-id eqn))
		(psm-exp (enode-id eqn)))
	(dolist (entry (distinct-SystemEntries (bgnode-entries eqn)))
	  (format stream "      <td><a href=\"#~D.~A\"><code>(~{~A~^<br>~})</code></a></td>~%"
		  n (sxhash (SystemEntry-prop entry)) 
		  (SystemEntry-prop entry)))
	(format Stream "</tr>~%"))
      (format Stream "</table>~%~%")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (format Stream "<table>~%")
      (format Stream "<caption>Quantities</caption>~%")
      (dolist (eqn (remove-if-not #'Qnode-p (EqnSet-nodes soln)))
	(format Stream "<tr><td><code>~A</code></td><td>~A</td><td>~A</td>~%" 
		(qnode-exp eqn)
		(nlg (qnode-exp eqn))
		(qnode-var eqn))
	(dolist (entry (distinct-SystemEntries (bgnode-entries eqn)))
	  (format stream "      <td><a href=\"#~D.~A\"><code>(~{~A~^<br>~})</code></a></td>~%"
		  n (sxhash (SystemEntry-prop entry)) 
		  (SystemEntry-prop entry)))
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
  (let ((str (open (merge-pathnames 
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
	     "  th {vertical-align: top; text-align: right;}~%"
	     "  td {vertical-align: top;}~%"
	     "  td.MAJOR {color: red;}~%"
	     "  td.DEFINITION {color: green;}~%"
	     "  caption {font-size: larger; font-weight: bolder;}~%"
	     "  table,caption {margin-left: auto; margin-right: auto; ~%"
	     "         border-spacing: 4; margin-bottom: 5; margin-top: 5;}~%"
	     ))
    (close css)))

(defun find-diff-ids (x y &optional ignore)
  (remove-if #'(lambda (match) (unify match ignore))
		   (set-difference (mapcar #'enode-id (EqnSet-eqns y)) 
				   (mapcar #'enode-id (EqnSet-eqns x)) 
				   :test #'equalp)))


