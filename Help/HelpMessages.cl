;; help-messages.cl --
;; Author(s):
;;  Linwood H. Taylor (lht) <lht@lzri.com>
;; Modified:
;;  22 June 2001 - (lht) -- created
;;; Modifications by Anders Weinstein 2001-2008
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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Premature-Entry-Help**
 '("Before you can do this, you need to do at least one other step first.  I'll leave your entry black so that you can easily re-enter it when you've finished."
   "You have skipped at least one prerequisite step."
   (function next-step-help)) #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Forbidden-Help**
    '("The solution strategy you are using is forbidden for this particular problem."
      "There are multiple ways to solve this problem, but the instructors have marked some of them as 'forbidden.' You must use one of the others."
   (function next-step-help)) #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **Dead-Path-Help**
 '("Correct, but does not lead to a solution.  Try a different approach."
   "Sometimes guessing is required to solve problems.  For instance, you might know two different principles that mention the sought quantity, but you usually don't need to apply both, so you have to guess which one try first.  Whenever you have two or more correct choices, and you guess one that won't ultimately lead to a solution, Andes will tell you so, thus saving you some work."
   (function next-step-help)) #+sbcl #'equalp)

(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx 
 **NOGOOD-Help**
 '("No good entry.  Shouldn't happen.") #+sbcl #'equalp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of help-messages.cl
;; Copyright (C) 2001 by <Linwood H. Taylor's Employer> -- All Rights Reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
