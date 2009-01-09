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
;;;;  Reset problem and rule tables.
;;;;  This must be run before other problems in the module
;;;;

;; loading this file resets the problem database:
(clear-problem-registry)
;; clear out the old operators on load so that the new ones can be defined.
(clear-ops)
;; Reset ontology database on each load of this file.
(clear-ontology)
;; reset NewtonsNogoods list
(clear-nogoods)		
;; reset post-processing operations
(clear-post-processing)
;; reset entry tests
(clear-entry-tests)

;;;
;;; It may be convenient to define this in the initialization file
;;;
(defun rkb ()
  "Reset the lists in KB and reload all files using asdf"
  (asdf:operate 'asdf:load-op 'andes))
