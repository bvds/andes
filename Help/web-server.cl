;; Author(s):
;;   Brett van de Sande, Feb 2009
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
;; ctl-c ctl-k compiles entire file 
(in-package :cl-user)

;; for now, run these on command line:
;(asdf:operate 'asdf:load-op 'hunchentoot)
;(in-package :webserver)

(defpackage :webserver
  (:use :cl :hunchentoot))

(in-package :webserver)

(defvar *server* nil)

(defun start-help ()
  "Start server with simple echo service, for testing"
  (setq  *dispatch-table*
	 (list #'dispatch-easy-handlers
	       (create-prefix-dispatcher "/" 'handle-echo)
	       #'default-dispatcher))

  ;; This is just for the debugging stage
  (setq *show-lisp-errors-p* t
        *show-lisp-backtraces-p* t)

  (push
  (setf *server* (start-server :port 8080 :MOD-LISP-P t)))

(defun stop-help ()
      (stop-server *server*))

(defun handle-echo ()
  (setf (content-type) "application/json; charset=UTF-8")
  ;; this contains the json-rpc object itself ...
  ;; probably should see how hunchentoot itself handles this
  ;; and make a new handler, then convert to-from json.
;(raw-post-data :force-text t)
  (format nil "{\"result\": \"received ~A and post ~A\",\"error\": null,\"id\":1}~%" (request-uri) nil))
