;;; model.cl
;;; Autor(s):  Bett van de Sande, 2011
;;; Copyright 2011 by Kurt Vanlehn and Brett van de Sande
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

;;
;;  Need to think of general structure for adding 
;;  hints associated with user interface skills.
;;     Generally want to fade out. Maybe something like this:
;;            Give pre-emptive hint at opportunity
;;            Give unsolicited hint in case of floundering.
;;            Solicited hint
;;
(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel)
  (use-package :andes-database))

(defun model-increment-state-property (property &key ceiling)
  ;; Increment property value, allowing for ceiling.
  (let ((x (get-state-property property)))
    (unless (eql x T) ;allow for ceiling
      (set-state-property property 
			  (cond ((null x) 1) ;doesn't exist
				((and ceiling (>= x ceiling)) t)
				(t (+ x 1)))))))

(defun incremented-property-test (property value)
  (let ((x (get-state-property property)))
	 (unless (eql x T) (or (null x) (< x value)))))

(defmacro incf-state-property (property value)
  `(set-state-property ',property
		      (+ (or (get-state-property ',property) 0) ,value)
		      :no-store t))

;; f-time/f-turns is time/turns of current floundering.
;; net-f-time/net-f-turns is time/turns of floundering 
;; episodes for whole session.  When re-running old sessions, these 
;; values are flushed at the beginning of each session.
;;
;; This should match code in LogProcessing/CognitiveModels/time.php
(defun model-flounder (time color)

  (cond ((eql color +color-green+)
	 (set-state-property 'last-c-time time :no-store t)
	 (set-state-property 'nongreen-steps 0 :no-store t)
	 (set-state-property 'nongreen-time 0 :no-store t))
	(t
	 (incf-state-property nongreen-steps 1)
	 (set-state-property 
	  'nongreen-time 
	  (- time (or (get-state-property 'last-c-time) 0))
	  :no-store t)))
  
  ;; Tutor has turned something green.
  (cond ((eql color +color-green+)
	 (incf-state-property session-correct 1)
	 (set-state-property 'count-turns nil :no-store t))
	
	((eql color +color-red+)
	 (incf-state-property session-incorrect 1)
	 (if (get-state-property 'count-turns)
	     (let ((dt (- time (get-state-property 'last-i-time))))
	       (incf-state-property f-time dt)
	       (incf-state-property net-f-time dt)
	       (incf-state-property count-turns 1)
	       (set-state-property 'f-turns
				   (get-state-property 'count-turns)
				   :no-store t))
	     (progn
	       (set-state-property 'count-turns 0 :no-store t)
	       (set-state-property 't-turns 0 :no-store t)))
	 (set-state-property 'last-i-time time :no-store t))
	
	;; In floundering mode, but turn is not colored.
	((get-state-property 'count-turns)
	 (incf-state-property count-turns 1))))

;; Likewise, time and turns spent in state of help refusal.
;; Called at the beginning of solution-step method, before
;; execute-andes-command is called.
;;
;; t-time/t-turns is amount of time/turns since last
;; request for help.  (Not used)
(defun model-no-tutor-turn (time)
  "Student has done solution step."
  (if (get-state-property 't-time)
      (set-state-property 't-turns 
			  (+ (get-state-property 't-turns) 1) 
			  :no-store t)
      (progn
	(set-state-property 't-time time :no-store t)
	(set-state-property 't-turns 0 :no-store t))))
 
;; Called by seek-help and record-action methods. 
(defun model-tutor-turn ()
  "Clicking help button, clicking on link (either kind) in tutor pane."
  ;; What to do about looking at manual, list of principles, et cetera?
  (set-state-property 't-time nil :no-store t)
  (set-state-property 't-turns nil :no-store t))

;; h-time & h-turns Time and turns since last 
;; hint sent by tutor, whether solicited or not.
(defun model-no-hint-turn (time)
  "Tutor is not sending hint back to student."
  (if (get-state-property 'h-time)
      (set-state-property 'h-turns 
			  (+ (get-state-property 'h-turns) 1) 
			  :no-store t)
      (progn
	(set-state-property 'h-time time :no-store t)
	(set-state-property 'h-turns 0 :no-store t))))
  
(defun model-hint-turn ()
  "Tutor sending help back to student, whether solicited or not."
  ;; What to do about looking at manual, list of principles, et cetera?
  (set-state-property 'h-time nil :no-store t)
  (set-state-property 'h-turns nil :no-store t))

(defun model-looking-at-video (time)
  "Start looking at video, whether from menu or given."
  (set-state-property 'intro-video-opened T) ;store open, in case blur fails
  (set-state-property 'intro-video-start-time time :no-store t))

(defun model-stop-looking-at-video (time)
  ;; What to do about looking at manual, list of principles, et cetera?
  (when (get-state-property 'intro-video-start-time)
    (set-state-property 
     'intro-video-time
     (+  (- time (get-state-property 'intro-video-start-time))
	 (or (get-state-property 'intro-video-time) 0)))
    ;; Reset timer.
    (set-state-property 'intro-video-start-time nil :no-store t)))


(defconstant +floundering-time+ 20 "time in seconds")
(defconstant +floundering-turns+ 3)
(defconstant +master-clicking+ 2)

(defun use-help-button-hint-test (time)
  (when nil  ;debug flag
    (format webserver:*stdout* "use-help-button-hint-test ~A ~A ~A ~A~%"
	    (and (get-state-property 'f-time)
		 (> (- time (get-state-property 'f-time)) +floundering-time+))
	    (and (get-state-property 'h-time)
		 (> (- time (get-state-property 'h-time)) +floundering-time+))
	    (and (get-state-property 'f-turns)
		 (> (get-state-property 'f-turns) +floundering-turns+))
	    (and (get-state-property 'h-turns)
		 (> (get-state-property 'h-turns) +floundering-turns+))))
  
  (and ;; Test that it was a red turn.
       (get-state-property 'f-time)
       (or 
	;; Detect a new user.
	(incremented-property-test 'CLICKED-HELP-BUTTON +master-clicking+)
	;; Detect floundering:  in a state of confusion
	;; and not having received any help.
	(and
	 (get-state-property 'f-time)
	 (> (- time (get-state-property 'f-time)) +floundering-time+)
	 (get-state-property 'h-time)
	 (> (- time (get-state-property 'h-time)) +floundering-time+))
	(and
	 (get-state-property 'f-turns)
	 (> (get-state-property 'f-turns) +floundering-turns+)
	 (get-state-property 'h-turns)
	 (> (get-state-property 'h-turns) +floundering-turns+)))))

(defun use-help-button-hint ()
  ;; use new user test, since that is simpler.
  (cond 
    
    ;; If they haven't looked at the intro video, start
    ;; by suggesting that.
    ;; These could be considered to be meta-hints.
    ;; but we need to contrast them with model-link-click below.
    ((if (get-state-property 'INTRO-VIDEO-TIME) 
	 ;; Logging for blur has succeeded, use time.
	 (< (get-state-property 'INTRO-VIDEO-TIME) 15)
	 ;; Logging for blur has failed, fall back on open flag.
	 (null (get-state-property 'INTRO-VIDEO-OPENED)))
     `((:action . "show-hint")
       (:text . ,(strcat "Perhaps you should " 
			 *intro-video-action* "."))))
    
    ((incremented-property-test 'CLICKED-HELP-BUTTON +master-clicking+)
     `((:action . "show-hint")
       (:text . ,(strcat "Your entry has turned red.&nbsp;  You can "
			 *help-button-action* " to get help."))))
    
    (t
     `((:action . "show-hint")
       (:text . ,(strcat "It looks like you are having difficulty.&nbsp;  Perhaps you should "
			 *help-button-action* " to get help."))))))

;; If student has never clicked on a link in the tutor pane,
;; give hint telling them that they can click on links.

(defun model-link-click-test ()
  (incremented-property-test 'CLICKED-HELP-LINK +master-clicking+))

(defun model-link-click ()
  '((:action . "show-hint")
    (:style . "meta-hint")
    (:text . "You can click on the above link to get more help.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;              Random Help Study 2012
;;;;
;; 
;;  We need a mechanism to make study manipulations be
;;  more "pluggable" in Andes.  Bug #1940
;;  For now, we use the setup that was used in the 2011 
;;  Raj lab studies and 2011 St. Anselm study.
;;  
;;
(in-package :cl-user)

(defpackage :random-help-experiment
  (:use :cl :cl-user :andes-database)
  (:export :set-current-object
	   :help-mod-p
	   :*help-mods*))

;; Should be eventually pushed to by various 
;; Help files at compile-time?
(defvar random-help-experiment:*help-mods* 
  '(
    (give-spontaneous-hint) (give-hints-backwards)
    (give-spontaneous-hint give-hints-backwards)
    (no-join-hints give-spontaneous-hint) (no-join-hints give-hints-backwards)
    (no-join-hints give-spontaneous-hint give-hints-backwards)
    ))

;; Set hook to test for backwards hints
(setf *backwards-hints-hook* 
      #'(lambda () (or (get-state-property 'backwards-hint :model "server")
		       (random-help-experiment:help-mod-p 
			'give-hints-backwards))))

(in-package :random-help-experiment)

;; Property names can either be a string or symbol;
;; using a string avoids issues with package names.
(defparameter +prob-flag+ "prob-flag")
(defparameter +current-object+ "current-object")
(defparameter +help-customizations+ "help-customizations")


;; In mysql, need to define sections, if they don't exist;
;; see random-help-experiment-setup.sql

;; (random-help-experiment::set-experiment-probability 0.5)
(defun set-experiment-probability (prob)
  "Run once with database open to set flag experiment using probability."
  (unless (and (numberp prob) (<= 0 prob 1))
    (warn "must be probablility, not ~s" prob))
  ;; Sections must already exist in database.
  (dolist (x (and '("random-help-test"
		   ;; Fall 2011 MIT sections
		   "MIT_96238198fb0774e40MITl1_"
		   "MIT_397367142947c4ec7MITl1_"
		   ;; Fall 2011 ASU Heckler, two of the sections
		   "asu_3u16472755e704e5fasul1_15865"
		   "asu_3u16472755e704e5fasul1_15854"
		   )
		 '(
		   ;; Sections for Spring 2012 at UW Plattville
		   ;; Andy Pawl sections
		   "uwplatt_51421910795174fcfuwplattl1_" ;physics 2240A1 
		   "uwplatt_6l13051599e174fb5uwplattl1_" ;physics 2240A2
		   "uwplatt_2Y1305989a5174f1cuwplattl1_" ;physics 2340C1
		   "uwplatt_3n13056a8a6174fbeuwplattl1_" ;physics 2340C2
		   ;; Tomm Scaife sections
		   "uwplatt_8p130495419184f26uwplattl1_" ;Physics 2240
		   "uwplatt_9047621c019184fdbuwplattl1_" ;Physics 2340
		   )))
    (set-state-property +prob-flag+ prob :model "server" :section x 
			:student nil :tid t)))

(defun set-current-object (x)
  (unless (stringp x) (warn "invalid object ~s" x))
  (when (get-state-property +prob-flag+ :model "server")
    (set-state-property +current-object+ 
			(concatenate 'string session:*problem* "_" x) 
			:model "server" :no-store t)))

(defun in-experiment-p ()
  "Randomly assign experimental vs. control condition."
  (let ((x (get-state-property +prob-flag+ :model "server")))
    ;; Each session gets its own seed based on problem name.
    (when x (< (mt19937:random 1.0) x))))

(defun help-mod-p (x)
  (when (get-state-property +prob-flag+ :model "server")
    (let ((current-object (or (get-state-property +current-object+
						  :model "server")
			      ;; When a problem is first opened,
			      ;; no object has been set.
			      (concatenate 'string session:*problem* 
				      "_no-current-object"))))
      (member x 
	      (multiple-value-bind (val is-set)
		  (get-state-property current-object :model "server")
		(if is-set
		    val
		    (let ((new-val 
			   ;; Randomly choose if Help will
			   ;; be modified
			   (when (in-experiment-p)
			     ;; Randomly choose one of the 
			     ;; help mod lists.
			     (random-elt *help-mods*))))
		      (set-state-property current-object new-val
					  :model "server")
		      new-val)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;              Learned Policy experiment 2012
;;;;
;; 
;;  We need a mechanism to make study manipulations be
;;  more "pluggable" in Andes.  Bug #1940
;;  
;;
(in-package :cl-user)
(defpackage :learned-help-experiment
  (:use :cl :cl-user :andes-database)
  (:export :set-policy :help-method
	   :help-mod-p
	   :*help-mods*))

(defparameter +policies+ '(spontaneous-hint backwards-hint))

(in-package :learned-help-experiment)

(defun random-perm (bag)
  "Return a random permutation of a list."
  (when bag
      (let ((e (random-elt bag)))
        (cons e (random-perm (remove e bag :count 1 :test #'eq))))))

(defun two-conditions (n)
  (let ((half (floor (/ n 2))))
    (random-perm (append (make-list (- n half) :initial-element t) 
			 (make-list half)))))

(defun two-condition-matrix (m n)
  (if (= m 1)
      (mapcar #'list (two-conditions n))
      (mapcar #'cons (two-conditions n) (two-condition-matrix (- m 1) n))))

(defun pick-problems (bool chapter) (when bool (cadr chapter)))

;; load *guerra-assigned* by hand from lon-capa/assignments.cl
;; start up help server.
;; get list of students using mysql
;; run something like this:
;; (learned-help-experiment::assign-condition "test-experiment" '("bvds-1" "bvds-2"))
;; Test result with something like:
;; select userName,value from STUDENT_STATE where userSection='test-experiment' and property='EXPERIMENT-PROBLEMS';  

;; This is just to get rid of warning messages.
;; Load by hand from lon-capa/assignments.cl
(defvar cl-user::*guerra-assigned*)

(defun assign-condition (section students)
  (let ((assignments (cadr cl-user::*guerra-assigned*)))
  (mapcar
    #'(lambda (student condition)
	(let ((problems (apply #'append 
			       (mapcar #'pick-problems 
				       condition
				       assignments))))
	  (when nil ;debug prints
	    (format t "problems for ~A are ~A~%" student problems)
	    (format t "assignments for ~A are ~A~%" student
		    (mapcar #'(lambda (x y) (when x (car y)))
			    condition assignments)))

	  (when t
	    (set-state-property 'cl-user::experiment-problems problems
			      :model "server" :tID t
			      :section section :student student))))
    students
    (two-condition-matrix (length assignments) 
			  (length students)))))

     
(defun help-method (time)
  (declare (ignore time))
  (cl-user::incf-state-property cl-user::session-helps 1))

;; Match function log0 in LogProcessing/CognitiveModels/state.php
(defun log0 (x)
  (if (and x (> x 0)) (log x) -1))

(defun linear-classifier (w state)
  ;; See function linearPolicy in LogProcessing/CognitiveModels/reward.nb
  (if (> (reduce '+ (mapcar '* w state)) 0)  1 0))

(defvar *debug-policy* nil)

(defun set-policy (time)

  ;; For pre-defs, there is no time.
  (unless time
    (return-from set-policy))

  (if (member session:*problem*
	      (get-state-property 'cl-user::experiment-problems 
				  :model "server")
	      :test #'(lambda (x y) 
			(string-equal x (string y))))

      (let ((dt (if (get-state-property 'last-turn-time)
		    (- time (get-state-property 'last-turn-time))
		    0)))
	(set-state-property 'last-turn-time time :no-store t)
	
	(when *debug-policy* 
	  (format webserver:*stdout* "Problem ~A is in experiment~%" 
		  session:*problem*))
	  
	(let ((state (list
		      ;; logSessionTime
		      (log0 time)
		      ;; fracSessionFlounderTime
		      (if (> time 0)
			  (/ (or (get-state-property 'cl-user::net-f-time) 0) 
			     time)
			  0)
		      ;; logNowRedTrans
		      (log0 (get-state-property 'cl-user::nongreen-steps))
		      ;; logNowRedTime
		      (log0 (get-state-property 'cl-user::nongreen-time))
		      ;; logNowIdleTime
		      (log0 dt)
		      ;; sessionCorrect
		      (or (get-state-property 'cl-user::session-correct) 0)
		      ;; sessionIncorrect
		      (or (get-state-property 'cl-user::session-incorrect) 0)
		      ;; sessionHelp
		      (or (get-state-property 'cl-user::session-helps) 0)
		      ;; fracSessionCorrect
		      (let ((net 
			     (+ (or (get-state-property 
				     'cl-user::session-correct) 0)
				(or (get-state-property 
				     'cl-user::session-incorrect) 0)
				(or (get-state-property 
				     'cl-user::session-helps) 0))))
			(if (> net 0)
			    (/ (or (get-state-property 
				    'cl-user::session-correct) 0)
			       net)
			    0))
		      ;; Dummy for doing constant term in linear classifier
		      ;; See function linearPolicy in 
		      ;; LogProcessing/CognitiveModels/reward.nb
		      1
		      ))
	      (model '((
			;; reward.nb command Export["junk.dat", soln[1/12][[2]]]
			;; for gamma=0.5, beta=1/12
2.538800860156868	2.2803328747460023	3.14590147370558	0.018859458291908493	-3.9811501134507585	1.9844137181914405	-1.5409915433978683	-0.7212502973981954	4.7746864538535405	1)
(0.5619599468110025	-3.170146371240845	-5.469995221644137	-6.320867938876214	-4.291569798230821	-2.9339618036379775	-3.553157996114464	3.742046728947423	5.0077662607773235	1))))
	  
	  ;; Sanity tests
	  (dolist (m model)
	    (unless (= (length m) (length state))
	      (error "Mismatch between model and state.")))
	  (unless (= (length model) (length cl-user::+policies+))
	    (error "Mismatch between model and policies."))
	  
	  ;; Need to verify state vector against actions
	  ;; and to see what policy it puts out in Mathematica
	  (when *debug-policy* 
	    (format webserver:*stdout* "   state {~A~{,~A~}}~%" 
		    (car state) (cdr state)))
	  
	  ;; set policies as server variables.
	  (loop for m in model and
		policy in cl-user::+policies+
		do
		(set-state-property policy
				    (linear-classifier m state) 
				    :model "server" :no-store t))
	  
	  (when *debug-policy* 
	    (format webserver:*stdout* "   ")
	    (dolist (policy cl-user::+policies+)
	      (format webserver:*stdout* "~A:~A " policy 
		      (get-state-property policy :model "server")))
	    
	    (format webserver:*stdout* "~%"))
	  ))
      (when *debug-policy* 
	(format webserver:*stdout* "Problem ~S not in experiment~%~%" 
		session:*problem*))
      ))
