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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makedis-kcdt -- command to build runtime image of Andes2 with the kcds 
;;                 installed and activated.
;;
;; In a console lisp do
;;
;;      :cd C:\Andes2\help
;;      :ld makedist
;;
;; Leaves Andes2.dxl in C:\Andes2 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :common-lisp-user)

;; Following is executed to make a new dist. This will spawn a new lisp,
;; which will load all the specified modules then dump an image of itself.
;; Note the normal Andes loader file will load existing compiled code if 
;; it exists.  But some modules might need to be compiled differently for 
;; the runtime distribution if conditionalized with #+*allegro-cl-runtime*
;; (Currently there is only one module, Andes2-main, that depends on this.
;; But could be more in the future). For that reason we specify a variant 
;; loader file "load-rc" that always recompiles the whole system before 
;; loading.

(generate-application 
 "Andes2"
 "c:\\Andes2\\"
 ; list copied from Andes1, not clear if all of these are needed
 '(:defsys :defctype :ffcompat :foreign :list2 :loop :seq2 :sock
   "load-kcd-rc.cl")
 :image-only t
 :allow-existing-directory t
; :application-files '("c:\\Andes2\\Solver_Release\\solver.dll")
 :application-type :exe
 :exit-after-image-build nil
 :discard-source-file-info t
 :discard-xref-info t
 :discard-local-name-info t
 :include-common-graphics nil
 :include-devel-env nil
 :splash-from-file nil
; :purify t
 :show-window :normal
 :include-compiler t
 :discard-compiler t
 :include-debugger nil
 :debug-on-error nil
 :include-ide nil
 :include-tpl nil
 :restart-init-function 'cl-user::andes-start
 :restart-app-function nil
 :runtime :standard)

(format T "Recompiling non-runtime Andes for development use~%")
(load "C:/Andes2/help/load-kcd-rc")
