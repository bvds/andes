;; garbage-collect.cl
;; Author: Brett van de Sande, 2010
;;; Copyright 2010 by Kurt Vanlehn and Brett van de Sande
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for supporting garbage collection
;;
  (defmacro dereference-with (func obj)
    `(if (consp ,obj)
	 (let ((y ,obj))
	   ;; in case of circular referencing, set object
	   ;; itself before recursion
	   (setf ,obj nil)
	   (dolist (yy y) (,func yy))
	   (fill y nil))
	 (when ,obj 
	   (warn "dereference-with ~A got type ~A:   ~a" 
		 ',func (type-of ,obj) ,obj))))
    

;; In principle, should store original values and restore when
;; Session is finished.
(defun tune-generational-gc ()
  #-sbcl (warn "No working tune-generational-gc")
  #+sbcl (let ((b (* 100 1024 1024)))
	   (setf (generation-bytes-consed-between-gcs 2) b)
	   (setf (generation-number-of-gcs-before-promotion 2) 24)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
