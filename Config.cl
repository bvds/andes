;;; Config.cl
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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains runtime initialization code for Andes
;;; This allows one to change parameters without recompiling 
;;; the lisp code.
;;;


; disable constraint loss filter
(setq **Filter-Constraint-losses** NIL)

(setq *followup-problems* '(PRETEST s1f s2e s4b s6a dt1a dt11a rots1a rots4a 
rots7a dt13b dt7b e1b e2b e8b e10a e6a e7a pow3a pow4a pow5a lmom2a lmom2b 
imp1 imp2 lmom3a lmom4a POSTTEST))

;(solver-logging T)
