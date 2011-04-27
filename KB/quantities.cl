;;; Copyright 2010 by Kurt Vanlehn and Brett van de Sande
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
;;;;       list of quantities grouped into catagories, with manual
;;;;   


(defparameter *quantity-list* 
  '((force . "When defining force, you should include the type of force,
if known.&nbsp;  Types of forces include: weight force, normal force, tension
force, kinetic or static friction, electric or magnetic force, buoyancy force, 
thrust force, spring force, drag force, and pressure.
<p>It can include what the force is acting on, what is causing the force, 
and the time.")
    (angle-between . "You can define the angle between two vectors or lines.&nbsp;
For instance, if you have defined two vectors <var><b>F</b></var> and <var><b>G</b></var>,
then you can define the angle using:&nbsp; <em>the angle between F and G</em>.")
    ;; This was copied from  draw-ang-displacement-rotating
    ;; In general, there will be some overlap between teaching hints
    ;; and manual information.  In that case, there should be a pointer
    ;; from the hint to the appropriate manual location.
    (ang-displacement . "The angular displacement of an object over an interval represents 
its net change in angular position as it rotates during that interval.&nbsp;  
The direction of this vector is given by the right hand rule.")
  (mag . "The magnitude (or length) of a vector can be expressed as:
<ul>
  <li><em>the magnitude of &hellip;</em>
  <li><em>mag of &hellip;</em>
</ul>")
  (dir . "The direction of a vector is the angle between the vector
and the horizontal axis.&nbsp; It can be expressed as:
<ul> 
  <li><em>the direction of &hellip;</em>
  <li><em>dir of &hellip;</em>
</ul>")
    (compo . "A component of a vector can be expressed as:
<ul>
  <li><em>the <var>x</var> component of &hellip;</em>
  <li><em><var>y</var> compo of  &hellip;</em>
</ul>
You must <a href=\"manual.html#axis-tool\" onclick='if(window.opener){window.opener.andes.principles.review(\"manual.html\",\"Manual\",\"axis-tool\");return false;}'>draw axes</a> 
before using any vector components.")
    ))

(defun quantity-html-file (&optional (path #P"review/quantities.html"))
  "construct html file for all quantities."
  (let ((*print-pretty* NIL) ;disble line breaks
	(stream (open path
		      :direction :output :if-exists :supersede
		      :external-format #+sbcl :utf-8 #-sbcl :default)))
    
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
	     ;;      "   <meta http-equiv=\"Content-Type\" content=\"text/html; "
	     ;;     "charset=iso-8859-1\">~%"
	     "   <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">~%"
	     "<title>Quantities</title>~%"
	     "<style type=\"text/css\">~%"
	     "    ul.big>li {~%"
	     "         margin-top: 0.5em;~%" 
	     "         margin-bottom: 0.5em;~%" 
	     "    }~%"
	     "</style>~%"
	     "</head>~%"
	     "<body>~%"
	     "<h1>scalar and vector quantities</h1>~%"
	     "<ul class=\"big\">~%"))
    
    (dolist (qexp 
	      ;; Listify quantity Ontology, then alphabetize
	      (sort (let (rules) (with-ontology-exptypes rule 
				   (push rule rules)) rules)
		    #'string-lessp :key #'exptype-short-name))
      ;; There is lots of junk in the quantity ontology that 
      ;; doesn't correspond directly to scalars or vectors.
      ;; Use rank as a filter for listable quantities.
      (when (member (exptype-rank qexp) '(scalar vector))
	(unless (exptype-short-name qexp)
	  (warn "missing short-name for ~A" (exptype-type qexp)))
	(format stream "  <li id=\"~A\"><strong>~A</strong>:&nbsp; ~(~A~)~@[<div>~A</div>~]~@[<div>Example:&nbsp;  <em>~A</em>.</div>~]~%" 
		(exptype-type qexp) ;used as pointer in hints.
		(exptype-short-name qexp)
		(exptype-rank qexp)
		(cdr (assoc (exptype-type qexp) *quantity-list*))
		;; get a random problem that contains this psm.
		(generate-random-quantity-phrase (exptype-form qexp))
		)))
    
    (format stream (strcat "</ul>~%"
			   "</body>~%" 
			   "</html>~%"))
    (when (streamp stream) (close stream))))

(defun generate-random-quantity-phrase (prop)
  (let ((probs (collect-problems-with-quantity prop)))
    (if probs
	(let ((prob (random-elt probs))
	      *sg-entries*)
	  (let ((*cp* prob)) 
	    (sg-setup prob)
	    (match:word-string 
	     (expand-vars 
	      (systementry-model 
	       (find prop *sg-entries* 
		     :key #'(lambda (x) (second (systementry-prop x))) 
		     :test #'unify))
	      :html-format t))))
	;; It is possible that there are quantities
	;; in Ontology that are not used.
	(warn "generate-random-quantity-phrase:  No problems found for ~A" prop))))

(defvar *sg-props-cache* () "Cache of propositions associated with each problem")

(defun collect-problems-with-quantity (pattern)
  "Go through working problems and collect problems that contain the given quantity proposition."
  (andes-init)
  (let (result)
    (dolist (prob (listprobs))
      (when (working-problem-p Prob)
	;; see if problem loaded, caching problem graphs
	(unless (problem-graph Prob)
	  (ignore-errors (read-problem-info (string (problem-name prob))))
	  (when *cp* 
	    (setf (problem-graph Prob) 
		  (problem-graph *cp*))))
	(when (problem-graph prob)
	  ;; cache list of propositions from each problem
	  (unless (assoc (problem-name prob) *sg-props-cache*)
	    (let ((*cp* prob) *sg-entries*) 
	      (sg-setup prob)
	      (push (cons (problem-name *cp*) 
			  (mapcar #'systementry-prop *sg-entries*))
		    *sg-props-cache*)))
	  (when (some #'(lambda (entry) 
			  ;; works for scalars
			  (unify (second entry) pattern))
		      (cdr (assoc (problem-name prob) *sg-props-cache*)))
	    (push prob result)))))
    result))
