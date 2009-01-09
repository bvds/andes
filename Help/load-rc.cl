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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-rc.cl -- variant Andes loader that forces a recompile of everything
;;                
;; This is mainly for use when loading Andes in context of 
;; generate-application-spawned child Lisp.  We force recompilation
;; to get any code that is different in the context of runtime-dist.
;;

; Ensure Lisp will read given values into double-precision floating points.
; This precision is needed in a few cases by the algebra module.
(setf *read-default-float-format* 'double-float)

(defparameter *Root-Path* "c:\\andes2\\")

(defparameter *Bsolver-Load-Files*                                     ;;Defining module paths.
    '("Solver_Release\\AMFile.cl"
      "Base\\AMFile.cl"
      "HelpStructs\\AMFile.cl"
      "Knowledge\\AMFile.cl"
      ; Now helpsys is loading kb dynamically 
      ; "KB\\AMFile-Helpsys.cl"  ; NB: use variant module description
      "Help\\AMFile.cl"
      "Testcode\\AMFile.cl"))
      

(load (pathname (concatenate 'string *Root-Path* "AndesModules.cl")))   ;; Load the module system.

(setup-Andes-Module-system *Root-Path*)                                 ;; Setup the system.

(dolist (F *Bsolver-Load-Files*)                                        ;; Setup each of the module definitions.
  (load (pathname (concatenate 'string *Root-Path* F))))

(format t ";;; Recompiling and loading all Andes modules~%")
(rsa) ; load all latest sources
(rca) ; recompile and reload
