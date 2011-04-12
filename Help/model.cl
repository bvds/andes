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

;; To test: 
;; login as student in section "study" to create section.
;; (set-state-property 'raj-experiment 'experiment :model "server" :section "study" :student nil :tid t)
;; (set-state-property 'intro-video-opened t :section "study" :student nil :tid t)
;; Monitor status with:
;;  (get-session-variable "bvdsvec1a1301333460364" 'session:*state-cache*)

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
  
  (and (equal (get-state-property 
	       'raj-experiment :model "server") 'experiment)
       ;; Test that it was a red turn.
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
  (and (equal (get-state-property 
	       'raj-experiment :model "server") 'experiment)
       (incremented-property-test 'CLICKED-HELP-LINK +master-clicking+)))

(defun model-link-click ()
    '((:action . "show-hint")
       (:text . "You can click on the above link to get more help.")))
