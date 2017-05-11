;;; IEA.cl
;;; Collin Lynch
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file encapsulates the intended principle application
;;; help system.  This system is intended to supplement WWH
;;; By eliciting from the student what equation they intended
;;; to write and the guiding them to produce it.  As necessary
;;;
;;; The file is split into three sections.  The first handles 
;;; prompting to identify the equation.  The second prompting to 
;;; The second handles promting on how to perform the equation
;;; and the final one prompts the student to write the equation
;;; (or tells them that they alredy have perhaps pointing out
;;; when it was part of a combined equation and the equation that
;;; they used.  The last section contains utility code.
;;;
;;; ------------- Selection --------------------------------------
;;; The first section proceeds muych like NSH.  It starts with a
;;; general message that prompts the student to select from the 
;;; list of equations supplied.  Once they do this we will have 
;;; a partial binding for the desired equation.  If this partial
;;; binding yields no equation from the list then we will inform
;;; the student that they have made an invalid choice and prompt
;;; them to try NSH.
;;;
;;; If the specification yields more than one entry then we will
;;; follow up with other questions about the bodies and times 
;;; within the equation in order to get a single value.  Failing
;;; that, ww will cry like a baby.
;;;
;;; Once the system returns a single equation then we will move on
;;; to the next section.
;;;
;;; If the student selects the algebraic combination then we will 
;;; them a stock message stating that they are to use the solve tool
;;; rather than enter that directly.  
;;;
;;; Selection itself necessitates a great deal of dynamic prompting
;;; that will occur as necessary.  If the full prompts do not narrow
;;; the list down then random selection will occur.
;;; 
;;; ----------- Prompting ---------------------------------------
;;; Once we have an equation to prompt.  We will go ahead and 
;;; determine whether or not it has been entered.  Because it is a 
;;; system entry this will be as part of the interps in a 
;;; student entry.
;;;
;;; If the equation has already been entered then we will inform 
;;; the students that they have entered it and give the equation
;;; in which it occured perhaps giving a message about the 
;;; possibility of combining equations in the process.
;;;
;;; If the equation has not been entered, then we will determine
;;; what steps must be taken before the student has entered it.  
;;; If they need to complete some step prior to entering the 
;;; equation then we will prompt them to do so.  If not then we
;;; will prompt them to write the equation itself by giving them 
;;; the correct form to copy.
;;;
;;; If the equation is not in the problem in any way then we will 
;;; give them a message telling them that and inform them that
;;; they should move on with something else and prompt them to NSH.
;;;
;;; ------------ Notes --------------------------------------------
;;; If we know what entry the student is trying to make, it might
;;; be possible to have it supplied here and then simply move into 
;;; the prompting "what do you need to do that" component of part 2.


;;;; ======================= Parameters ==============================


;;;; ======================= Main ====================================
;;;; These are the initial calling functions that will proceed 
;;;; directly into selection


;;;; ======================= Selection ===============================
;;;; The first section proceeds muych like NSH.  It starts with a
;;;; general message that prompts the student to select from the 
;;;; list of equations supplied.  Once they do this we will have 
;;;; a partial binding for the desired equation.  If this partial
;;;; binding yields no equation from the list then we will inform
;;;; the student that they have made an invalid choice and prompt
;;;; them to try NSH.
;;;;
;;;; If the specification yields more than one entry then we will
;;;; follow up with other questions about the bodies and times 
;;;; within the equation in order to get a single value.  Failing
;;;; that, ww will cry like a baby.
;;;;
;;;; Once the system returns a single equation then we will move on
;;;; to the next section.
;;;;
;;;; If the student selects the algebraic combination then we will 
;;;; them a stock message stating that they are to use the solve tool
;;;; rather than enter that directly.  
;;;;
;;;; Selection itself necessitates a great deal of dynamic prompting
;;;; that will occur as necessary.  If the full prompts do not narrow
;;;; the list down then random selection will occur.

(defun IEA-Main (&optional eqn)
  (make-dialog-turn
   "What equation do you wish to write?"
   +equation-menu+ 
   :Responder #'(lambda (Response)
		  (iea-check-response Response))
   ;; This doesn't seem to go anywhere.
   :Assoc `((iea unknown-equation ,eqn))))

;;;; -------------------- Check Response ----------------------------
;;;; The initial IEA response will be a cons containing an equation
;;;; name and an optional set of variable bindings.   The name will 
;;;; be used to look up the equation followed by the bindings which
;;;; will be used to determine the equation from the equation list.

;;; Once we have a name then we will look up the equation in the
;;; list and return the appropriate struct if it is present or 
;;; give a warning if it is not.  Once that is done then we will
;;; move on to filtering.  
(defun iea-check-response (response)
  "Check the supplied iea-name."

  ;; Student has clicked on link in tutor pane.
  ;; Re-open principles menu.
  (when (eql response 'any-principle)
    (return-from iea-check-response (IEA-main)))

  (unless (consp response)
    ;; These cases should be handled more intelligently.
    ;; Often occurs when rerunning logs through help 
    ;; server and hint sequence has changed.
    ;; Bug #1947
    (warn 'log-condition:log-warn
	  :tag (list 'iea-check-response response)
	  :text "iea-check-response expecting cons")
    (return-from  iea-check-response))

  (let* ((name (car response))
	 (bindings (or (cdr response) no-bindings))
	 (equation (lookup-psmclass-name name))
	 Tmp)

    (cond ((null equation) 
	   ;; The student response is gotten by choosing from the
	   ;; principles menu.  So either principle menu is incompatible
	   ;; with KB or the log file is incompatible (if running help
	   ;; from a log file.
	   (warn "!!! IEA received bad equation name ~a." name))
	  ((iea-filter-equations-by-form Equation Bindings)
	   (iea-platonic-prompt Equation Bindings))
	  ((setq tmp (iea-test-alternate-axes Equation Bindings))
	   (iea-alternate-axes-prompt Equation Bindings Tmp))
	  (t (iea-incorrect-prompt Equation)))))


;;; ---------------- filter-equations by form --------------------------
;;; Given an equation, a set of bindings and an optional list to search
;;; through, filter out all the equations that do not unify with the 
;;; supplied form given the bindings.  
(defun iea-filter-equations-by-form (Equation Bindings 
				     &optional (Eqns (problem-eqnindex *cp*)))
  (let ((form (subst-bindings Bindings (PSMClass-form Equation))))
    (remove-if-not 
     #'(lambda (E) 
	 (and (not (equal (eqn-type E) 'Derived-eqn)) 
	      (unify form (eqn-exp E))))
     Eqns)))



;;; ------------------------- Platonic Response --------------------------------
;;; If the student picks an equation that does appear somewhere within the 
;;; system then we need to give them the correct platonic form along with
;;; a helpful message about writing that down in their own form.  This function
;;; gives them that information along with a potential link to NSH if they so 
;;; wish it.

(defparameter **IEA-Platonic-form**
    (strcat "Equations of that type appear within the problem solution.  "
	    "You should write the equation in the form ~a using your own "
	    "variables."))

(defun iea-platonic-prompt (Equation Bindings)
  (make-hint-seq
   (list (format Nil **IEA-platonic-form** 
	   (eval-print-spec (Psmclass-EqnFormat Equation) bindings))
	 '(function next-step-help))
   :Assoc `((IEA platonic-prompt ,(PSMClass-name Equation)))))




;;; ------------------------ Alternate Axes --------------------------------------
;;; When the students are selecting axes, it is likely that they will select the
;;; right principle but pick the wrong axis to project it upon.  In the event of
;;; that occuring we want to prompt them in the right direction rather than deal
;;; with their frustration and their angered responses.  This code test to see if
;;; the collection is correct on the other axis (x or y) and prompt them if the
;;; other is true.  

(defun iea-test-alternate-axes (Equation Bindings)
  (and (member '?axis Bindings :key #'car)
       (let* ((OldAxis (binding-val (get-binding '?Axis Bindings)))
	      (NewAxis (if (equal OldAxis 'X) 'Y 'X))
	      (NewBindings (change-bindings '?Axis NewAxis Bindings)))
	 (if (iea-filter-equations-by-form Equation NewBindings)
	     NewAxis))))



(defparameter **iea-alternate-axis-message**
    (strcat "Are you sure that you meant the ~a axis?  "
	    "There are no instances of the equation you "
	    "selected along that axis.  However there are "
	    "one or more correct instances along the ~a axis.  "
	    "Do you want to use the other axis?"))


(defparameter **IEA-alt-axes-yes**
    (strcat "You should write the equation in the form ~a using your own "
	    "variables."))


(defparameter **IEA-alt-axes-no**
    (strcat "OK then.  Why don't you try writing a different equation."))


;;; If the entry is correct on the alternate axes then we will go ahead and 
;;; alert the students to that fact and see what they think.  Once they have
;;; made their decision we will prompt them to make the correct form and move
;;; on. 
(defun iea-alternate-axes-prompt (Equation Bindings NewAxis)
  (make-dialog-turn
   (format Nil **IEA-Alternate-axis-Message**
	   (if (equal NewAxis 'X) 'Y 'X) NewAxis)
   '((yes . "yes") (no . "no"))
   :Responder #'(lambda (Response)
		  (cond ((eql Response 'yes)
			 (iea-prompt-alt-axes-yes Equation Bindings NewAxis))
			((eql Response 'no) (iea-prompt-alt-axes-no))
			(t (warn 'log-condition:log-warn
				 :tag (list 'iea-alternate-axes-prompt 
					    response)
				 :text "invalid response"))))
   :Assoc `((IEA prompt-alternate-axes ,(PSMClass-name equation) 
	     ,Bindings ,NewAxis))))



;;; If the student indicates that they do want to change axes then we want 
;;; to go ahead and prompt the new form for them using the new axis.
(defun iea-prompt-alt-axes-yes (Equation Bindings NewAxis)
  (let ((NewBinds (change-bindings '?Axis NewAxis Bindings)))
    (make-hint-seq
     (list (format Nil **IEA-alt-axes-yes**
		   (eval-print-spec (PSMClass-EqnFormat Equation) NewBinds))
	   '(function next-step-help))
     :Assoc `((IEA prompt-alt-axes-yes ,(PSMClass-name Equation) ,Bindings ,NewAxis)))))



;;; If the student prompts that they are uninterested in going with the new 
;;; axis then we will close with a message suggesting that they pull up NSH 
;;; if they are lost.
(defun iea-prompt-alt-axes-no ()
  (make-hint-seq
   (list **IEA-alt-axes-no** '(function next-step-help))
   :Assoc `((iea . iea-prompt-alt-axes-no))))


;;; ------------------------ incorrect prompt ------------------------------------
;;; If the student selects incorrectly, IE their chosen equation type is not 
;;; part of any solution then we want to tell them so and prompt them to 
;;; move on to NSH if necessary.

(defparameter **IEA-Incorrect-Form**
    (strcat "I'm afraid that the equation type that you selected does not appear in "
	    "~a."))

(defparameter **IEA-Incorrect-single**
    "the problem solution")

(defparameter **IEA-Incorrect-Multi**
    "any of the solutions to this problem")

(defun iea-incorrect-prompt (Equation)
  (make-hint-seq
   (list (format Nil **IEA-Incorrect-Form**
	   (if (< 1 (length (problem-Solutions *cp*)))
	       **IEA-Incorrect-Multi**
	     **IEA-Incorrect-Single**))
	 '(function next-step-help))
   :Assoc `((IEA Incorrect ,(PSMClass-name Equation)))))

