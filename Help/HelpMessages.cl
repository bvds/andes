;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help-messages.cl --
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  22 June 2001 - (lht) -- created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Premature-Entry-Help**
 '("Before you can do this, you need to do at least one other step first.  I'll leave your entry black so that you can easily re-enter it when you've finished."
   "You have skipped at least one prerequisite step.   Click on the light-bulb button for help in finding it.") #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Premature-Subst-Help**
    '("Although this equation is correct, you have made use of given numerical values from the problem statement before you have shown the applicable physical principle in symbolic form."
      "It is good practice to identify the applicable physical principle you are using by writing it explicitly in purely symbolic form before plugging in given values (numbers) from the problem statement.  This equation includes such numbers, but you have not yet entered the fundamental equation in symbolic form by itself. {\\l Review Physics Equations}{\\v Principles.html} for a list of equations or click the light-bulb for an appropriate next step.") #+sbcl #'equalp)


(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Forbidden-Help**
    '("The solution strategy you are using is forbidden for this particular problem."
      "There are multiple ways to solve this problem, but the instructors have marked some of them as 'forbidden.' You must use one of the others.  Click on the light bulb button for help suggestions.") #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Dead-Path-Help**
 '("Correct, but does not lead to a solution.  Try a different approach."
   "Sometimes guessing is required to solve problems.  For instance, you might know two different principles that mention the sought quantity, but you usually don't need to apply both, so you have to guess which one try first.  Whenever you have two or more correct choices, and you guess one that won't ultimately lead to a solution, Andes will tell you so, thus saving you some work.  If you don't see what other choices can be taken here, click on the light bulb."
   "Click on the light bulb button.") #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **NOGOOD-Help**
 '("No good entry.  Shouldn't happen.") #+sbcl #'equalp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of help-messages.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
