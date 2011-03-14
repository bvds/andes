(rhelp)


;; Swank install not set up, Bug #1722
(defvar *swank-loaded* nil)
(if (and (probe-file #p"/usr/local/share/emacs/site-lisp/slime/swank-loader.lisp")
	 (load #p"/usr/local/share/emacs/site-lisp/slime/swank-loader"))
    (setf *swank-loaded* t))

;; Start help server.
(start-help)

;; Start a Swank server
#+swank (when *swank-loaded*
	  (setf swank-server
		(swank:create-server :port 4005 :style :spawn :dont-close t))
	  (princ "Swank server started on port 4005") (terpri))

;; While help server is running, listen for shutdown signal.
(let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			     :type :stream :protocol :tcp)))
  ;; Listen on local port 6440 for a TCP connection
  (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 6440)
  (sb-bsd-sockets:socket-listen socket 1)
  (princ "Shutdown listener started on port 6440") (terpri)
  
  ;; When it comes, just tell the caller we're shutting down
  (let ((client-socket (sb-bsd-sockets:socket-accept socket)))
    (let ((stream
	   (sb-bsd-sockets:socket-make-stream client-socket
					      :element-type 'character
					      :input t
					      :output t
					      :buffering :none)))
      (princ "Shutting down Andes help server." stream)
      (terpri stream))
	
    ;; Close up the sockets
    (sb-bsd-sockets:socket-close client-socket))
  (sb-bsd-sockets:socket-close socket))

;; May first want to close up open sessions, since we are leaving lisp


;; Shut down help server
(stop-help)

;; Exit lisp
(quit)
