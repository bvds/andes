;;; Modifications by Anders Weinstein 2000-2008
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
;;;;
;;;;  Define constants
;;;;
;;;  The numerical value of universal constants are specified in
;;;  Algebra/src/pconsts.h
;;;
;;;  Any predefined universal constants have to be entered in the list 
;;;  *phys-constants* in the function enter-predefs.
;;;
;;;  Non-universal constants like g and Pr0 are handled differently.
;;;


;;; Symbols for certain constants are understood by the solver. These have
;;; to be removed from the list of variables in an equation in some contexts
;;; we don't change vars-in-eqn since might need all symbols in other contexts.
(defparameter *phys-constants* 
    '(|$p| |$P| |eps0| |kelec| |mu0| |kmag| |c| |hbar| |G| |Iref|))
(defun physconstp (exp) (member exp *phys-constants*))

;;; enter-predefs -- enter predefined symbols for *cp* into symbol table
;;; 
;;; Which symbols need to be predefined can depend on features of the problem.
;;; Adding new predefs requires adding rule-like statements to this routine.
;;;

(defun enter-predefs ()
  
  ;; If prob involves universal gravitation, predefine G. This is marked
  ;; by a proposition beginning with 'gravity in the givens
  (when (find 'gravity (problem-givens *cp*) :key #'first)
    (symbols-enter "G" 'G NIL '|G|))
  
  ;; Speed of light is "c"
  (when (or (member 'waves (problem-features *cp*))
	    (member 'EM-waves (problem-features *cp*)))
  (symbols-enter "c" 'c NIL '|c|))
  
  ;; Iref for defining decibels
  (when (or (member 'work-quants (problem-features *cp*))
	    (member 'work-quants-out (problem-features *cp*)))
  (symbols-enter "Iref" 'Iref NIL '|Iref|))
  
  ;; algebra system understands $P (upper-case only) as symbol for pi in
  ;; systemese equations.  We still need to predefine a student label for
  ;; pi in symbol table so student equations using it can get through
  ;; the student-to-system translation without appearing to have an undefined
  ;; variable. We install lower-case pi to be mapped to system's upper-case.
  (symbols-enter "$p" 'pi NIL '$P) 
  
  ;; If prob involves E&M, predefine kelec and epsilon_0 and mu_0.
  (when (member 'E&M (problem-features *cp*))
     ;; args are:  name, quantity, owning-entry, sysvar-translation
     ;; NB: need some dummy quantity to prevent inverse match to NIL quantity
     (symbols-enter "kelec" 'kelec NIL '|kelec|)
     (symbols-enter "$m0" '(physconst |mu0|) NIL '|mu0|)
     (symbols-enter "kmag" 'kmag NIL '|kmag|)
     (symbols-enter "$e0" '(physconst |eps0|) NIL '|eps0|))

  ;; add conditions for further predefs here:
)
