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

(defpackage :webserver
  (:use :cl :hunchentoot :json)
  (:export :defun-method :start-json-rpc-service :stop-json-rpc-service 
	   :*stdout*))

(in-package :webserver)

(defvar *server* nil)
(defvar *stdout* *standard-output*)
(defvar *service-methods* (make-hash-table :test #'equal))

(defun start-json-rpc-service (uri &key (port 8080) (mod-lisp-p t))
  "Start a web server that runs a single service for handling json-rpc"
  ;; One could easily extend this to multiple web servers or multiple
  ;; services, but we don't need that now.
  (when *server* (error "server already started"))
  (setq  *dispatch-table*
	 (list #'dispatch-easy-handlers
	       (create-prefix-dispatcher uri 'handle-json-rpc)
	       #'default-dispatcher))

  ;; This is just for the debugging stage
  (setq *show-lisp-errors-p* t
        *show-lisp-backtraces-p* t)
  
  (setf *server* (start-server :port port :MOD-LISP-P mod-lisp-p)))

(defun stop-json-rpc-service ()
  (stop-server *server*) (setf *server* nil))

(defmacro defun-method (uri name lambda-list &body body)
  "Defines a function and registers it as a method associated with a service."
  `(progn (defun ,name ,lambda-list ,@body)
    (register-method-for-service ,uri #',name 
     (string-downcase (symbol-name ',name)))))

(defun register-method-for-service (uri method method-name)
  (setf (gethash (list uri method-name) *service-methods*) method))

;; This now works as far as it goes.
;; Need error handling with restart-case or handler-case
;;  need to handle errors associated with bad function call or inside call
;;  need to handle errors with bad json
;;  need to handle errors with bad rpc (test for :method & :params not nil)
;;  need to test input and reply against andes3.smd,
;;        should have macro to define method functions which compares
;;        the lambda list with the smd.  Also, handle-json-rpc should
;;        only allow methods that match smd.
(defun handle-json-rpc ()
  "Handles json-rpc 1.0 and 2.0"
  (setf (content-type) "application/json; charset=UTF-8")
  ;; Hunchentoot is supposed to take care of charset encoding
  (unless (search "application/json" (header-in :content-type))
    ;; with the wrong content-type, just send string back
    (return-from handle-json-rpc "\"content-type must be application/json\""))
  (let* (result error1 reply
		(in-json (decode-json-from-string 
			  (raw-post-data :force-text t)))
		(user-problem-session (header-in :client-id))
		(service-uri (request-uri))
		(method (cdr (assoc :method in-json)))
		(params (cdr (assoc :params in-json)))
		(id (assoc :id in-json))
		(version (assoc :jsonrpc in-json))
		(method-func (gethash (list service-uri method) 
				      *service-methods*)))
    (format *stdout* "session ~A calling ~A with ~S~%" 
	    user-problem-session method params)
    ;; when this function is executed, the package is cl-user
    (cond 
      ;; Here, we assume that the client generates the session id
      ;; alternatively, we could have the server generate it and
      ;; return it to the client at the beginning of a new session
      ((null user-problem-session)
       (setq error1 (if version
			`((:code . -32000) 
			  (:message . "missing http header client-id"))
			(format nil "missing http header client-id"))))
      ((null method-func)
       (setq error1 (if version
			`((:code . -32601) (:message . "Method not found")
			  (:data . ,method))
			(format nil "Can't find method ~S for service ~A" 
				method service-uri))))
      ;; need error handler for the case where the arguements don't 
      ;; match the function...
      (t (setq result 
	       (apply method-func 
		      ;; this is only temporary
		      (cons user-problem-session 
			    (cons (cdr id) 
				  (if (alistp params) 
				      (flatten-alist params) params)))))))
    (format *stdout* "  result ~S error ~S~%" result error1)
    ;; only give a response when id is given
    (when id
      (when (or error1 (not version)) (push (cons :error error1) reply))
      (when (or result (not version)) (push (cons :result result) reply))
      (push id reply)
      (when version (push version reply))
      (encode-json-alist-to-string reply))))

(defun alistp (x) 
  "determine if x is an alist" 
  (and (listp x) (every #'consp x)))

(defun flatten-alist (x) 
  "turn an alist into plain list"
  (mapcan #'(lambda (x) (list (car x) (cdr x))) x))

