
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;             Create set of lon-capa problems
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-file-name (set)
  (remove #\(
	  (remove #\)
		  (substitute #\x #\&
			      (substitute #\_ #\space set)))))

(defun lookup-set-name (problem)
  (loop for set in (cadr *sets*)
	do (when (member problem (second set))
	     (return-from lookup-set-name (car set))))
  (error "problem not found:  ~A" problem))
			
	  

(defun lon-capa-problem-sets (sets &optional (path #P"./"))
  "construct lon-capa xml files for all problems"
  ;; sets can be *sets* or *guerra-assigned*
  (dolist (set (cadr sets))
    (let ((path (merge-pathnames (strcat (set-file-name (car set)) "/") 
				 path)))
      (ensure-directories-exist path)
      ;; Need to save the homework set name and order somewhere.
      ;; This is just a dummy/placeholder.
      (lon-capa-meta-file (car set) path)
      (dolist (problem (second set))
	(lon-capa-meta-file (format nil "problem ~(~A~)" problem)
			    path 
			    (format nil "~(~A~).problem" problem))
	(lon-capa-problem-file problem path)))))

(defun lon-capa-meta-file (title &optional (path #P"./") (name "default"))
  "construct meta file containing title of problem set"
  (let ((*print-pretty* NIL) ;disble line breaks
	(stream (open (merge-pathnames 
		       (format nil "~A.meta" name) path)
		      :direction :output :if-exists :supersede
		      :external-format #+sbcl :utf-8 #-sbcl :default)))
    (when (streamp Stream) 
      #+sbcl (unless (eq (stream-external-format Stream) ':utf-8)
	       (error "Wrong character code ~A, should be UTF-8" 
		      (stream-external-format Stream))))
    (format Stream "<title>~A</title>~%" title)
    ;; Odd, but setting this in the default.meta is
    ;; not sufficient.
    (format Stream "<sourceavail>open</sourceavail>~%")
    (when (streamp stream) (close stream))))    

(defun lon-capa-problem-file (problem &optional (path #P"./"))
  "construct lon-capa xml file for given problem"
  (let ((*print-pretty* NIL) ;disble line breaks
	(stream (open (merge-pathnames 
		       (format nil "~(~A~).problem" 
			       problem) 
		       path)
		      :direction :output :if-exists :supersede
		      :external-format #+sbcl :utf-8 #-sbcl :default)))
    
    (format t "starting problem ~A at ~A~%" problem
	    (merge-pathnames 
	     (format nil "~(~A~).problem" 
		     problem) 
	     path))
    
    ;;  Assume stream has UTF-8 encoding (default for sbcl)
    ;;  Should test this is actually true or change the charset to match
    ;;  the actual character code being used by the stream
    ;;  something like:
    (when (streamp Stream) 
      #+sbcl (unless (eq (stream-external-format Stream) ':utf-8)
	       (error "Wrong character code ~A, should be UTF-8" 
		      (stream-external-format Stream))))
    (format Stream *lon-capa-problem-template*
	    (if (find problem *times-scores* :key #'car)
		(ceiling (/ (second (find problem *times-scores* 
					  :key #'car)) 60))
		10)
	    problem)
      (when (streamp stream) (close stream))))

(defun lon-capa-problem-files (&optional (path #P"./"))
  "construct lon-capa xml files for all problems"
  (dolist (prob (listprobs))
    (lon-capa-problem-file (problem-name prob) path)))


(defvar *lon-capa-problem-template*
  ;; This is a format string
  ;; Must escape the following:  ~ ->  ~~    \ -> \\    " -> \"
  ;; Format directives for weight and problem name.
  "<problem>
    
    <parameter name=\"weight\" id=\"12\" type=\"float_pos\" default=\"~A\" description=\"Weight\" />
<script type=\"loncapa/perl\">

# Name of the problem in ANDES
$problem='~(~A~)';

# Internal stuff to build a unique keys for student and seciton
$user=&EXT('user.name').'_'.&EXT('user.domain');  # Andy Pawl used 'user.name'
$class=&EXT('request.course.id').'_'.&sec();  # Andy Pawl used 'user.course.sec'
# Simulate a new submission
$timestamp=time;
$response=\"{\\\"user\\\": \\\"$user\\\", \\\"class\\\": \\\"$class\\\", \\\"problem\\\": \\\"$problem\\\"}\";

# create status to date.
$weight=&EXT('resource.0.weight');
if ((!defined($weight)) || ($weight eq '')) { $weight=1; }
$awarded=&EXT('user.resource.resource.0.awarded');
if (!defined($awarded)) { $awarded=0; }
$scoreformat=&EXT('resource.0.scoreformat');
if (!defined($scoreformat) || $scoreformat eq '') { $scoreformat=\"2f\"; }
$display='';
if (&EXT('resource.0.problemstatus')!~~/^no/) {
   if (!defined($awarded)) {
      $display=$weight.' possible points.';
   } else {
      $display='You have '.&format($awarded*$weight,$scoreformat).' out of '.
            $weight.' possible points.';
   }
}
</script>

<startouttext />
<a href=\"http://gideon.eas.asu.edu/web-UI/index.html?s=$class&amp;u=$user&amp;p=$problem&amp;e=\">Solve problem $problem</a>.&nbsp; 
$display
<endouttext />
<externalresponse answer=\"$response\" url=\"http://gideon.eas.asu.edu/get-score\">
    <hiddensubmission value=\"$timestamp\" />

</externalresponse>
</problem>")

(defvar *default-meta-template*
"<abstract></abstract>
<author>Brett van de Sande</author>
<copyright>default</copyright>
<customdistributionfile></customdistributionfile>
<highestgradelevel>0</highestgradelevel>
<keywords>physics mechanics electricity magnetism circuits optics fluids</keywords>
<language>notset </language>
<lowestgradelevel>0</lowestgradelevel>
<notes></notes>
<obsoletereplacement></obsoletereplacement>
<sourceavail>open</sourceavail>
<sourcerights></sourcerights>
<standards></standards>
<subject>physics</subject>
<title>~A</title>~%")


;; Login, choose role "author"
;; For all-maps and assigned-maps "Choose action" publish.
;;         check "include subdirectories" and "force republication"


(defun lon-capa-problem-maps (sets src &key
			      ;; flag for practice (versus assigned) problems 
			      practice-p 
			      ;; problems from problem sets to exclude
			      exclude-sets
			      title
			      (path #P"./"))
  "construct lon-capa map files for all problem sets."
  (ensure-directories-exist path)
  (ensure-directories-exist 
   (merge-pathnames (merge-pathnames 
		     (strcat (set-file-name title) "/") path)))
    
  ;; create *.sequence.meta file at top level
  (let ((stream (open (merge-pathnames 
		       (strcat (set-file-name title) ".sequence.meta") path)
		      :direction :output :if-exists :supersede
		      :external-format #+sbcl :utf-8 #-sbcl :default)))
    (format stream *default-meta-template*
	    (or title (car sets)))
    (close stream))
  
  (let ((exclude 
	 ;; Construct list of problems to exclude from supplied sets
	 (apply #'append (mapcar #'second 
				 (second exclude-sets))))
	;; Open sequence file for list of problem sets.
	(istream (open (merge-pathnames 
			(strcat (set-file-name title) ".sequence")
			path)
		       :direction :output :if-exists :supersede
		       :external-format #+sbcl :utf-8 #-sbcl :default)))
    (format istream "<map>~%")
    (loop for set in (cadr sets) and
	  ii from 1 and
	  ij from 1
	  do
	  (format t "starting set ~A:  ~A~%" ii (car set))
	  (let ((base-name (strcat 
			    (set-file-name title) "/"
			    (set-file-name (car set)) ".sequence")))
	    (when (> ii 1) (format iStream "<link to=\"~A\" index=\"~A\" from=\"~A\" />~%"
				   ii ij (- ii 1)))
	    (format iStream "<resource src=\"~A\" id=\"~A\" ~@[~*type=\"start\" ~]~@[~*type=\"finish\" ~]title=\"~A\" />~%"
		    base-name ii 
		    (= ii 1) (= ii (length (cadr sets)))
		    (car set))
	    (lon-capa-meta-file (car set) path base-name)
	    (let ((*print-pretty* NIL) ;disable line breaks
		  (stream (open (merge-pathnames 
				 base-name
				 path)
				:direction :output :if-exists :supersede
				:external-format #+sbcl :utf-8 #-sbcl :default)))
	      (format Stream "<map>~%")
	      (loop for problem in (second set) and
		    i from 1 and
		    j from 1
		    with lasti
		    ;; Excluding problems makes id non-consecutive
		    unless (member problem exclude)
		    do
		    (format t "  problem ~A~%" problem)
		    (when lasti (format Stream "<link to=\"~A\" index=\"~A\" from=\"~A\" />~%"
					  i j lasti))
		    (format Stream "<resource src=\"~A/~A/~(~A~).problem\" id=\"~A\" ~@[~*type=\"start\" ~]~@[~*type=\"finish\" ~]title=\"problem ~(~A~)\" />~%"
			    (or src  "/res/asu/bvds/all-problems")
			    ;; Find correct directory in all-problems
			    (set-file-name (lookup-set-name problem))
			    problem i 
			    (null lasti) (= i (length (second set)))
			    problem)
		    (setf lasti i)
		    (when practice-p
		      (format Stream "<param to=\"~A\" type=\"string_questiontype\" name=\"parameter_0_type\" value=\"practice\" />~%" i)))
	      (format Stream "</map>~%")
	      (when (streamp stream) (close stream)))))
    (format istream "</map>~%")
    (when (streamp istream) (close istream))))
