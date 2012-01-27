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

(defun model-red-turn (time)
  (unless time (error "Null time supplied"))
  ;; Time and turns spent in a "state of frustration"
  (if (get-state-property 'f-time)
      (set-state-property 'f-turns 
			  (+ (get-state-property 'f-turns) 1) 
			  :no-store t)
      (progn
	(set-state-property 'f-time time :no-store t)
	(set-state-property 'f-turns 0 :no-store t))))

(defun model-green-turn ()
  (set-state-property 'f-time nil :no-store t)
  (set-state-property 'f-turns nil :no-store t))

;; Likewise, time and turns spent in state of help refusal.
(defun model-no-tutor-turn (time)
  (if (get-state-property 't-time)
      (set-state-property 't-turns 
			  (+ (get-state-property 't-turns) 1) 
			  :no-store t)
      (progn
	(set-state-property 't-time time :no-store t)
	(set-state-property 't-turns 0 :no-store t))))
  
(defun model-tutor-turn ()
  "Clicking help button, clicking on link (either kind) in tutor pane."
  ;; What to do about looking at manual, list of principles, et cetera?
  (set-state-property 't-time nil :no-store t)
  (set-state-property 't-turns nil :no-store t))

(defun model-no-hint-turn (time)
  (if (get-state-property 'h-time)
      (set-state-property 'h-turns 
			  (+ (get-state-property 'h-turns) 1) 
			  :no-store t)
      (progn
	(set-state-property 'h-time time :no-store t)
	(set-state-property 'h-turns 0 :no-store t))))
  
(defun model-hint-turn ()
  "Received some kind of help from tutor, whether solicited or not."
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
    (give-spontaneous-hint give-hints-backwards)))

;; Set hook to test for backwards hints
(setf *backwards-hints-hook* 
      #'(lambda () (random-help-experiment:help-mod-p 'give-hints-backwards)))

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
  (unless (stringp x)
    (warn "invalid object ~s" x))
  (when (get-state-property +prob-flag+ :model "server")
    (set-state-property +current-object+ x :model "server" :no-store t)))

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
			      "no-current-object"))
	  (h-c (get-state-property +help-customizations+ 
				   :model "server")))
      ;; Sanity test: make sure it is an alist
      (unless (and (listp h-c) (every #'consp h-c))
	(error "h-c not an alist:  ~S" h-c))
      (when nil ;debug      
	(format webserver:*stdout* "help-mod-p with ~S and ~S~%"
		current-object h-c))
      (unless (assoc current-object h-c :test #'equal) ;string equality
	;; current-object must be a new object, 
	;; randomly select experiment vs. control
	;; and add a help modification in the experimental case.
	(push (cons current-object 
		    (when (in-experiment-p)
		      ;; Randomly choose one of the help mod lists.
		      (random-elt *help-mods*)))
	      h-c)
	;;  (format webserver:*stdout* "help-mod-p setting h-c ~S~%" h-c)
	(set-state-property +help-customizations+ h-c
			    :model "server"))
      
      ;; Now, help customizations for object exists, 
      ;; look for this one.  Use string equality for matching object.
      (member x (cdr (assoc current-object h-c :test #'equal))))))
