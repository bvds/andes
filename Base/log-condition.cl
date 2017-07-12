;; Author(s):
;;   Brett van de Sande, March 2012
;;; Copyright 2012 by Kurt Vanlehn and Brett van de Sande
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
;; ctl-c ctl-k compiles entire file 

;; The :text field is supposed to be plain text (rather than html).

(in-package :cl-user)

(defpackage :log-condition
  (:use :cl)
  (:export :log-error :log-warn :log-tag))

(in-package :log-condition)

(define-condition log-error (error)
  ((tag :initarg :tag :reader log-tag)
   (text :initarg :text :reader text))
  (:report (lambda (c s) (write-string (text c) s))))

(define-condition log-warn (warning)
  ((tag :initarg :tag :reader log-tag)
   (text :initarg :text :reader text))
  (:report (lambda (c s) (write-string (text c) s))))
