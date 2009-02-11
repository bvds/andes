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

(defun |get-problem| (&optional x) "demo function" (format nil "~S" x))

;; for now, run these on command line:
;(asdf:operate 'asdf:load-op 'hunchentoot) 
;(asdf:operate 'asdf:load-op 'cl-json)
;(in-package :webserver)

(defpackage #:webserver
  (:use :cl :hunchentoot :json)
  (:export :start-help :stop-help :*stdout*))

(in-package :webserver)

(defvar *server* nil)
(defvar *stdout* *standard-output*)


(defun start-help ()
  "Start server with simple echo service, for testing"
  (setq  *dispatch-table*
	 (list #'dispatch-easy-handlers
	       (create-prefix-dispatcher "/help" 'handle-json-rpc)
	       #'default-dispatcher))
  
  ;; This is just for the debugging stage
  (setq *show-lisp-errors-p* t
        *show-lisp-backtraces-p* t)
  
  (setf *server* (start-server :port 8080 :MOD-LISP-P t)))

(defun stop-help ()
  (stop-server *server*))

;; This now works as far as it goes.
;; Need error handling with restart-case or handler-case
;;  need to handle errors associated with bad function call or inside call
;;  need to handle errors with bad json
;;  need to handle errors with bad rpc (test for :method & :params not nil)
;;  need to test input and reply against andes3.smd
(defun handle-json-rpc ()
  (setf (content-type) "application/json; charset=UTF-8")
  ;; Hunchentoot is supposed to take care of charset encoding
  (unless (search "application/json" (header-in :content-type))
    ;; with the wrong content-type, just send string back
    (return-from handle-json-rpc "\"content-type must be application/json\""))
  (let* (result error reply
		(in-json (decode-json-from-string 
			  (raw-post-data :force-text t)))
		(method (cdr (assoc :method in-json)))
		(params (cdr (assoc :params in-json)))
		(id (assoc :id in-json))
		(version (assoc :jsonrpc in-json)))
    (format *stdout* "calling ~S with ~S~%" method params)
    ;; when this function is executed, the package is cl-user
    ;; so we must supply any package explicitly
    (if (find-symbol method)
	(setq result (apply (intern method) params))
	(setq error (if version
			`((:code . -32601) (:message . "Method not found")
			  (:data . ,method))
			(format nil "Can't find method ~S" method))))
    (format *stdout* "  result ~S error ~S~%" result error)
    ;; only give a response when id is given
    (when id
      (when (or error (not version)) (push (cons :error error) reply))
      (when (or result (not version)) (push (cons :result result) reply))
      (push id reply)
      (when version (push version reply))
      (encode-json-alist-to-string reply))))

