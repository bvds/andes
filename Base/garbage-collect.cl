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

;; print out statistics in user-friendly form,
;; This should allow better gc efficiency.
(defvar *last-user-time* 0)
(defvar *last-gc-time* 0)

(defun gen-stats (&optional (str t))
  (format str "Gen byte alloc gcs avgage~%")
  #-sbcl (warn "sbcl-specific")
  #+sbcl (loop for i from 0 to SB-VM:+pseudo-static-generation+ 
	       do (format str " ~2D ~10D ~3D ~6F~%" i
			  (generation-bytes-allocated i)
			  (generation-number-of-gcs i)
			  (generation-average-age i)))
  #+sbcl (format str "sum ~10D~%" (sb-vm::dynamic-usage))
  (let ((new-gc-time *gc-run-time*)
	(new-user-time (get-internal-run-time)))
    (format str "time:  ~Fs gc and ~Fs total run time~%" 
	    (/ (- new-gc-time *last-gc-time*) 
	       INTERNAL-TIME-UNITS-PER-SECOND)
	    (/ (- new-user-time *last-user-time*)
	       INTERNAL-TIME-UNITS-PER-SECOND))
    (setf *last-gc-time* new-gc-time)
    (setf *last-user-time* new-user-time)))

;; The total number of generations, and the size of generation 0
;; was found experimentally.  Still have not tuned the size
;; generation 1, or determined if something >100MB improves generation 0.

;; In principle, should store original values and restore when
;; Session is finished.
(defun tune-generational-gc ()
  #-sbcl (warn "No working tune-generational-gc")
  ;; Data sizes are roughly twice as big on x86-64,
  ;; but memory bandwidth is the same, so we use same gc rates.
  #+sbcl (let ((b (* 1024 1024)))
	   ;; (generation-bytes-consed-between-gcs 0) is never used
	   (setf (bytes-consed-between-gcs) (* 100 b))
	   (setf (generation-number-of-gcs-before-promotion 0) 1)
	   ;; generation-number-of-gcs-before-promotion has no
	   ;; effect on highest generation.
	   (setf (generation-bytes-consed-between-gcs 1) (* 100 b))
	   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
