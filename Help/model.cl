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

(defun model-red-turn (time)
  (unless time (error "Null time supplied"))
  ;; Time and turns spent in a "state of frustration"
  (if (andes-database:get-state-property 'f-time)
      (andes-database:set-state-property 
       'f-turns (+ (andes-database:get-state-property 'f-turns) 1) :no-store t)
      (progn
	(andes-database:set-state-property 
	 'f-time time :no-store t)
	(andes-database:set-state-property 
	 'f-turns 0 :no-store t))))

(defun model-green-turn ()
  (andes-database:set-state-property 
   'f-time nil :no-store t)
  (andes-database:set-state-property 
   'f-turns nil :no-store t))

  ;; Likewise, time and turns spent in state of help refusal.
(defun model-no-tutor-turn (time)
  (if (andes-database:get-state-property 't-time)
      (andes-database:set-state-property 
       't-turns (+ (andes-database:get-state-property 't-turns) 1) :no-store t)
      (progn
	(andes-database:set-state-property 
	 't-time time :no-store t)
	(andes-database:set-state-property 
	 't-turns 0 :no-store t))))
  
(defun model-tutor-turn ()
  "Clicking help button, clicking on link (either kind) in tutor pane."
  ;; What to do about looking at manual, list of principles, et cetera?
  (andes-database:set-state-property 
   't-time nil :no-store t)
  (andes-database:set-state-property 
   't-turns nil :no-store t))

(defconstant +floundering-time+ 30 "time in seconds")
(defconstant +floundering-turns+ 5)

;; To test: 
;; login as student in section "study" to create section.
;; (andes-database:set-state-property 'raj-experiment 'experiment :model "server" :section "study" :student nil :tid t)

(defun use-help-button-hint-test (time)
  (when nil  ;debug flag
    (format webserver:*stdout* "use-help-button-hint-test ~A ~A ~A ~A~%"
	    (and (andes-database:get-state-property 'f-time)
		 (> (- time (andes-database:get-state-property 'f-time)) 
		    +floundering-time+))
	    (and (andes-database:get-state-property 't-time)
		 (> (- time (andes-database:get-state-property 't-time)) 
		    +floundering-time+))
	    (and (andes-database:get-state-property 'f-turns)
		 (> (andes-database:get-state-property 'f-turns) 
		    +floundering-turns+))
	    (and (andes-database:get-state-property 't-turns)
		 (> (andes-database:get-state-property 't-turns)
		    +floundering-turns+)))
    
  (and (equal (andes-database:get-state-property 
	       'raj-experiment :model "server") 'experiment)
       ;; Test that it was a red turn.
       (andes-database:get-state-property 'f-time)
       (or 
	;; Detect a new user.
	(or (null (andes-database:get-state-property 
		   'CLICKED-HELP-BUTTON))
	    (< (andes-database:get-state-property 
		'CLICKED-HELP-BUTTON) 3))
	;; Detect floundering:  in a state of confusion
	;; and not asking for help.
	(and
	 (andes-database:get-state-property 'f-time)
	 (> (- time (andes-database:get-state-property 'f-time)) 
	    +floundering-time+)
	 (andes-database:get-state-property 't-time)
	 (> (- time (andes-database:get-state-property 't-time)) 
	    +floundering-time+))
	(and
	 (andes-database:get-state-property 'f-turns)
	 (> (andes-database:get-state-property 'f-turns) 
	    +floundering-turns+)
	 (andes-database:get-state-property 't-turns)
	 (> (andes-database:get-state-property 't-turns)
	    +floundering-turns+)))))

(defun use-help-button-hint ()
  ;; use new user test, since that is simpler.
  (if (or (null (andes-database:get-state-property 
	    'CLICKED-HELP-BUTTON))
	  (< (andes-database:get-state-property 'CLICKED-HELP-BUTTON) 3))
      `((:action . "show-hint")
	(:text . ,(strcat "Your entry has turned red.&nbsp;  You can "
			  *help-button-action* " to get help.")))
      `((:action . "show-hint")
	(:text . ,(strcat "It looks like you are having difficulty.&nbsp;  Perhaps you should "
			  *help-button-action* " to get help.")))))
