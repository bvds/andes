;;; random.cl:  interface routines for random number generator
;;; Authors:  Brett van de Sande and Anders Weinstein
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
;;; Platform-independent random number generator

(defun initialize-random-elt (name)
"Turn a string into a vector of unsigned 32 bit integers and use this to seed the random number generator."
  (set-mt19937 (coerce (loop for char across name 
     collect (char-code char)) '(vector (unsigned-byte 32)))))

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (mt19937:random (length seq))))

(defun random-choice (probability)
  "Return T randomly with given probability"
  ; Macro attempting to inline constant arg calls had bug causing
  ; inlining of compile-time *random-state* value. Now fixed, but
  ; safest to avoid dubious macro.
   (let ((one 1.0)) 
    (> probability (mt19937:random one))))

;; seeding mt19937 is not obvious.  Copied the following from a Maxima page  
(defun set-mt19937 (seed)
  (setq mt19937::*random-state* 
	 (mt19937::make-random-object 
	  :state (mt19937::init-random-state seed)))
  t)

#|
; following shows undesired results of bad macro def: 
(defun testrand (seed)
 (let ((one 1.0))
  (set-mt19937 seed)
  ;(format T "mt19937:*random-state* = ~A~%" mt19937:*random-state*)
  (format T "random one: ~A~%" (mt19937:random one))
  (set-mt19937 seed)
  ;(format T "mt19937*random-state* = ~A~%" mt19937:*random-state*)
  (format T "random 1.0: ~A~%" (mt19937:random 1.0))))

CL-USER(12): (testrand 13)
random one: 0.670090463354502
random 1.0: 0.4823905248516196
NIL
CL-USER(13): (testrand 13)
random one: 0.670090463354502
random 1.0: 0.35544005715928617
NIL
|#
