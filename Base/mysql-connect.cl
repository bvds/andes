;;;;
;;;;  Obtained from http://paste.lisp.org/display/92632
;;;;  on March 26, 2010.  Added modifications to get it
;;;;  working with sbcl (instead of lispworks).
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dec, 2009
;;;
;;; Simple MySQL driver written in Common Lisp (Lispworks 5.1+)
;;;
;;; Access to MySQL via the MySQL Client/Server protocol.
;;; No third-party libraries are needed (i.e. mysqllib), no ffi calls.
;;; Based on http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
;;;
;;; Works with MySQL 4.1 or higher.
;;;
;;; Usage:
;;;
;;; mysql-connect:connect &key host port user password database => connection
;;;   host - string
;;;   port - number (optional, default is 3306)
;;;   user - string
;;;   password - string
;;;   database - string (optional)
;;; mysql-connect:disconnect connection
;;; mysql-connect:query connection query-strings => list of results
;;; mysql-connect:get-last-insert-id connection  => number 
;;;
;;; Notes:
;;; On connection sets the utf-8 encoding to comunicate with the server.
;;; The query function returns column fields as text.
;;; Signals simple-error if the server returns an error.
;;;
(in-package :cl-user)

(defpackage :mysql-connect
  #+lispworks (:add-use-defaults t)
  #+sbcl (:use :cl)
  (:export connect
           disconnect
           query
           get-last-insert-id))

(in-package :mysql-connect)

#+lispworks (require "comm")
;;-

(defstruct mysqlcon stream host port connection-id insert-id)

(defconstant +max-packet-size+ (* 1024 1024))

(defconstant +latin1-swedish-ci+ 8)
(defconstant +utf8-general-ci+  33)

(defconstant +com-quit+ 1)
(defconstant +com-query+ 3)

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx
	+capabilities+
	`((:client-long-password     .      1) ;new more secure passwords
	  (:client-found-rows        .      2) ;found instead of affected rows
	  (:client-long-flag         .      4) ;get all column flags
	  (:client-connect-with-db   .      8) ;one can specify db on connect
	  (:client-no-schema         .     16) ;don't allow database.table.column
	  (:client-compress          .     32) ;can use compression protocol
	  (:client-odbc              .     64) ;odbc client
	  (:client-local-files       .    128) ;can use LOAD DATA LOCAL
	  (:client-ignore-space      .    256) ;ignore spaces before '('
	  (:client-protocol-41       .    512) ;new 4.1 protocol
	  (:client-interactive       .   1024) ;this is an interactive client
	  (:client-ssl               .   2048) ;switch to SSL after handshake
	  (:client-ignore-sigpipe    .   4096) ;ignore sigpipes
	  (:client-transactions      .   8192) ;client knows about transactions
	  (:client-reserved          .  16384) ;old flag for 4.1 protocol
	  (:client-secure-connection .  32768) ;new 4.1 authentication
	  (:client-multi-statements  .  65536) ;enable/disable multi-stmt support
	  (:client-multi-results     . 131072));enable/disable multi-results
	#+sbcl #'equalp)

;;sbcl has problems with defconstant, see "sbcl idiosyncracies"
(#-sbcl defconstant #+sbcl sb-int:defconstant-eqx
	+client-capabilities+ '(:client-long-password
				:client-long-flag
				:client-protocol-41
				:client-secure-connection
				:client-ignore-space
				:client-transactions)
	#+sbcl #'equalp)

;;-

(defun connect (&key host (port 3306) user password (database nil))
  (let* ((stream (open-stream host port))
         (packet (read-packet stream)))
    (if (error-packet-p packet)
        (error (format-error-message packet))
	(let* ((server-info (parse-handshake-packet packet))
	       (scramble (getf server-info :scramble)))
	  (send-authorization-packet stream user password database scramble)
	  (let ((packet (read-packet stream))
		(connection nil))
	    ;; Behavior for the case of old passwords (pre Mysql-4.1)
	    ;; can be seen by examining the source file
	    ;; mysql-connector-c-6.0.2/libmysql/client.c
	    ;;
	    ;; RedHat linux 5.4 uses old passwords, by default.
	    (when (and (= (aref packet 0) 254) (= (length packet) 1))
	      ;; By sending this very specific reply, server asks us 
	      ;; to send scrambled password in old format.
	      (write-packet (scramble-323 scramble password) stream
			    :packet-number 3)
	      (setf packet (read-packet stream)))
	    (cond ((ok-packet-p packet)
		   (setq connection (initialize-connection stream host port server-info)))
		  ((error-packet-p packet)
		   (close-stream stream)
		   (error (format-error-message packet)))
		  (t
		   (close-stream stream)
		   (error "MYSQL-CONNECT: Unknown error during connection.")))
	    (query connection "set names 'utf8'")
	    connection)))))


(defun open-stream (host port)
  (let ((stream 
	 #+lispworks (comm:open-tcp-stream host port
					   :direction :io
					   :element-type '(unsigned-byte 8))
	 #+(or sbcl openmcl) (connect-to-server host port)))
    (unless stream
      (error (format nil "Cannot connect to ~a:~a" host port)))
    stream))

(defun close-stream (stream)
  (when stream (close stream)))

(defun send-authorization-packet (stream user password database scramble)
  (write-packet (prepare-auth-packet user password database scramble)
                stream
                :packet-number 1))

(defun initialize-connection (stream host port server-info)
  (make-mysqlcon :stream stream
                 :host host
                 :port port
                 :insert-id nil
                 :connection-id (getf server-info :thread-id)))

;;-

(defun disconnect (connection)
  (send-quit connection)
  (when (close-stream (mysqlcon-stream connection))
    (setf (mysqlcon-stream connection) nil)))

(defun send-quit (connection)
  (write-packet `#(,+com-quit+) (mysqlcon-stream connection)
                :packet-number 0))

;;-

(defun query (connection &rest args)
  (let ((stream (mysqlcon-stream connection))
        (query-string (apply #'concatenate 'string args)))
    (send-query-string query-string stream)
    (let ((packet (read-packet stream)))
      (cond ((error-packet-p packet)
             (error (format-error-message packet)))
            ((ok-packet-p packet)
             (update-connection-data connection packet))
            (t
             (parse-data-packets packet stream))))))

(defun send-query-string (string stream)
  (write-packet (concatenate 'vector `#(,+com-query+) (string-to-utf8 string))
                stream :packet-number 0))

(defun update-connection-data (connection packet)
  (let* ((ok (parse-ok-packet packet))
         (insert-id (getf ok :insert-id)))
    (setf (mysqlcon-insert-id connection) insert-id))
  nil)

;;-

(defun get-last-insert-id (connection)
  (mysqlcon-insert-id connection))

;;-

(defun read-packet-header (stream)
  (let ((length 0)
        (number 0))
    (setq length (+ (read-byte stream)
                    (ash (read-byte stream) 8)
                    (ash (read-byte stream) 16)))
    (setq number (read-byte stream))
    (values length number)))

(defun read-packet (stream)
  (multiple-value-bind (packet-length packet-number)
      (read-packet-header stream)
    (declare (ignore packet-number))
    (let ((buffer (make-array packet-length
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
      (read-sequence buffer stream)
      buffer)))  

(defun write-packet-header (length packet-number stream)
  (write-byte (logand #xff length) stream)
  (write-byte (logand #xff (ash length -8)) stream)
  (write-byte (logand #xff (ash length -16)) stream)
  (write-byte (logand #xff packet-number) stream))

(defun write-packet (data stream &key packet-number)
  (write-packet-header (length data) packet-number stream)
  (write-sequence data stream)
  (force-output stream))

;;-

(defun prepare-auth-packet (user password database scramble)
  (let ((buf (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
        (client-flags (capabilities-to-number
                       (if database
                           (cons :client-connect-with-db +client-capabilities+)
                         +client-capabilities+)))
        (max-packet-size +max-packet-size+)
        (charset-number  +utf8-general-ci+)
        (scramble-buf (password-to-token password scramble)))
    (put-int32-to-array  client-flags    buf :position 0)
    (put-int32-to-array  max-packet-size buf :position 4)
    (put-int8-to-array   charset-number  buf :position 8)
    (concatenate 'vector
                 buf
                 (string-to-cstring user)
                 (vector (length scramble-buf)) scramble-buf
                 (when database (string-to-cstring database)))))

(defun %to-list (seq) (coerce seq 'list))
(defun %to-vector (seq) (coerce seq 'vector))

(defun password-to-token (password scramble)
  (let* ((pwd (string-to-latin1 password))
         (stage1-hash (sha1 pwd))
         (stage2-hash (sha1 stage1-hash)))
    (%to-vector
     (mapcar #'logxor
             (%to-list (sha1 (concatenate 'vector scramble stage2-hash)))
             (%to-list stage1-hash)))))

;;-
        
(defun parse-handshake-packet (buffer)
  (let* ((protocol-version (aref buffer 0))
         (pos (position 0 buffer)) ;;; end position of a c-line (zero)
         (server-version (latin1-to-string buffer :start 1 :end pos))
         (thread-id (+ (aref buffer (+ pos 1))
                       (ash (aref buffer (+ pos 2)) 8)
                       (ash (aref buffer (+ pos 3)) 16)
                       (ash (aref buffer (+ pos 4)) 24)))
         (server-capabilities (number-to-capabilities
                               (+ (aref buffer (+ pos 14))
                                  (ash (aref buffer (+ pos 15)) 8))))
         (server-language (aref buffer (+ pos 16)))
         (server-status (+ (aref buffer (+ pos 17))
                           (ash (aref buffer (+ pos 18)) 8)))
         (scramble (make-array 20 :element-type '(unsigned-byte 8))))
    (dotimes (i 8)
      (setf (aref scramble i) (aref buffer (+ pos i 5))))
    (dotimes (i 12)
      (setf (aref scramble (+ i 8)) (aref buffer (+ pos i 32))))
    (list :protocol-version protocol-version
          :server-version server-version
          :thread-id thread-id
          :server-capabilities server-capabilities
          :server-language server-language
          :server-status server-status
          :scramble scramble)))

(defun error-packet-p (buffer)
  (= #xFF (aref buffer 0)))

(defun parse-error-packet (buffer)
  (let ((error (+ (aref buffer 1)
                  (ash (aref buffer 2) 8)))

        (sqlstate nil)
        (message nil))
    (if (char/= #\# (code-char (aref buffer 3)))
        (setf message (latin1-to-string buffer :start 3))
      (progn
        (setf sqlstate (latin1-to-string buffer :start 4 :end 8))
        (setf message (latin1-to-string buffer :start 9))))
    (list :error error :sqlstat sqlstate :message message)))

(defun ok-packet-p (buffer)
  (zerop (aref buffer 0)))

(defun parse-ok-packet (buffer)
  (multiple-value-bind (affected-rows len)
      (decode-length-coded-binary buffer 1)
    (multiple-value-bind (insert-id len2)
        (decode-length-coded-binary buffer (+ 1 len))
      (let ((pos (+ 1 len len2)))
        (let ((server-status (+ (aref buffer pos)
                                (ash (aref buffer (+ pos 1)) 8)))
              (warning-count (+ (aref buffer (+ pos 2))
                                (ash (aref buffer (+ pos 3)) 8)))
              (message (when (< (+ pos 4) (length buffer))
                         (utf8-to-string buffer :start (+ pos 4) :end (length buffer)))))
          (list :affected-rows affected-rows
                :insert-id insert-id
                :server-status server-status
                :warning-count warning-count
                :message message))))))

(defun eof-packet-p (buffer)
  (and (= #xFE (aref buffer 0))
       (= 5    (length buffer))))

(defun parse-eof-packet (buffer)
  (let ((warning-count (+ (aref buffer 1)
                          (ash (aref buffer 2) 8)))
        (status (+ (aref buffer 3)
                          (ash (aref buffer 4) 8))))
    (list :warning-count warning-count :status status)))

(defun parse-data-packets (packet stream)
  (let ((num (decode-length-coded-binary packet 0))) ; number of columns
    (dotimes (i num) (read-packet stream))
    (read-packet stream) ;read eof packet
    (let ((list nil))
      (do ((packet (read-packet stream) (read-packet stream)))
          ((eof-packet-p packet))
        (setq list (cons (parse-raw-packet packet) list)))
      (reverse list))))

(defun parse-raw-packet (buffer)
  (let ((buffer-length (length buffer))
        (pos 0)
        (list nil))
    (loop
     (multiple-value-bind (len start) (decode-length-coded-binary buffer pos)
       (if (= len 251) ;column value = NULL (only appropriate in a Row Data Packet)
           (progn
             (setq list (cons "NULL" list))
             (setq pos (+ start pos)))
         (progn
           (setq list (cons (utf8-to-string buffer :start (+ start pos)
                                                   :end (+ start pos len)) list))
           (setq pos (+ start pos len))))
       (when (>= pos buffer-length) (return))))
     (reverse list)))

;;-

(defun format-error-message (packet) 
  (let* ((error-list (parse-error-packet packet))
         (number  (getf error-list :error))
         (message (getf error-list :message)))
    (format nil "MySQL(~a): ~a" number message)))

(defun capabilities-to-number (capabilities)
  (let ((acc 0))
    (dolist (option capabilities)
      (incf acc (cdr (assoc option +capabilities+))))
    acc))

(defun number-to-capabilities (number)
  (remove nil (mapcar #'(lambda (cons)
                          (if (zerop (logand (cdr cons) number))
                              nil
                            (car cons)))
                      +capabilities+)))

(defun string-to-latin1 (str)
  ;; Converts a string to an encoded binary vector
  #+lispworks (external-format:encode-lisp-string str :latin-1)
  #+sbcl (sb-ext:string-to-octets str :external-format :latin-1)
  )

(defun string-to-cstring (str)
  (concatenate 'vector (string-to-latin1 str) #(0)))

(defun string-to-utf8 (str)
  #+lispworks (external-format:encode-lisp-string str :utf-8)
  #+sbcl (sb-ext:string-to-octets str :external-format :utf-8)
  )

(defun latin1-to-string (buf &key (start 0) (end (length buf)))
  ;; decodes a binary vector to make a srtring 
  #+lispworks (external-format:decode-external-string buf :latin-1 :start start :end end) 
  #+sbcl (sb-ext:octets-to-string buf  :external-format :latin-1 :start start :end end)
  ) 

(defun utf8-to-string (buf &key (start 0) (end (length buf)))
  #+lispworks (external-format:decode-external-string buf :utf-8 :start start :end end)
  #+sbcl (sb-ext:octets-to-string buf :external-format :utf-8 :start start :end end)
  ) 

(defun put-int8-to-array (int array &key position)
  (setf (aref array position) (logand #xFF int)))

(defun put-int32-to-array (int array &key position)
  (setf (aref array position) (logand #xFF int))
  (setf (aref array (+ position 1)) (logand #xFF (ash int -8)))
  (setf (aref array (+ position 2)) (logand #xFF (ash int -16)))
  (setf (aref array (+ position 3)) (logand #xFF (ash int -24))))

(defun decode-length-coded-binary (buffer pos)
  (let ((val (aref buffer pos)))
    (cond ((<= val 251) (values val 1))
          ((= val 252) (values (%lcb-get-int16 buffer pos) 3))
          ((= val 253) (values (%lcb-get-int24 buffer pos) 4))
          ((= val 254) (values (%lcb-get-int64 buffer pos) 9)))))

(defun %lcb-get-int16 (buffer pos)
  (+ (aref buffer (+ 1 pos))
     (ash (aref buffer (+ 2 pos)) 8)))

(defun %lcb-get-int24 (buffer pos)
  (+ (aref buffer (+ 1 pos))
     (ash (aref buffer (+ 2 pos)) 8)
     (ash (aref buffer (+ 3 pos)) 16)))

(defun %lcb-get-int64 (buffer pos)
  (+ (aref buffer (+ 1 pos))
     (ash (aref buffer (+ 2 pos)) 8)
     (ash (aref buffer (+ 3 pos)) 16)
     (ash (aref buffer (+ 4 pos)) 24)
     (ash (aref buffer (+ 5 pos)) 32)
     (ash (aref buffer (+ 6 pos)) 40)
     (ash (aref buffer (+ 7 pos)) 48)
     (ash (aref buffer (+ 8 pos)) 56)))



;;; Secure hash algorithms - SHA-1
;;; http://csrc.nist.gov/publications/fips/fips180-2/fips180-2.pdf
;;; Note: implemented for byte messages

(defmacro to-32-bits-word (&rest body)
  `(logand #xFFFFFFFF ,@body))

(defun rotl (n shift)
  (logior (to-32-bits-word (ash n shift))
	  (ash n (- shift 32))))

(defun pad-the-message (message)
  (flet ((padding-size (n)
           (let ((x (mod (- 56 (rem n 64)) 64)))
             (if (zerop x) 64 x))))
    (let* ((message-len (length message))
           (message-len-in-bits (* message-len 8))
           (buffer-len (+ message-len 8 (padding-size message-len)))
           (buffer (make-array buffer-len
                               :element-type '(unsigned-byte 8)
                               :initial-element 0)))
      (dotimes (i message-len)
        (setf (aref buffer i) (aref message i)))
      (setf (aref buffer message-len) #b10000000)
      (dotimes (i 8)
        (setf (aref buffer (- buffer-len (1+ i)))
              (logand #xFF (ash message-len-in-bits (* i -8)))))
      buffer)))

(defun prepare-message-block (n data)
  (let ((message-block (make-array 80))
        (offset (* n 64)))
    (dotimes (i 16)
      (setf (aref message-block i) (+ (ash (aref data (+ offset   (* i 4))) 24)
                              (ash (aref data (+ offset 1 (* i 4))) 16)
                              (ash (aref data (+ offset 2 (* i 4))) 8)
                                   (aref data (+ offset 3 (* i 4))))))
    (loop :for i :from 16 :to 79 :do
          (setf (aref message-block i) 
                (to-32-bits-word
                        (rotl (logxor (aref message-block (- i 3))
                                      (aref message-block (- i 8))
                                      (aref message-block (- i 14))
                                      (aref message-block (- i 16))) 1))))
    message-block))

(defun sha1-f (n x y z)
  (cond ((<= 0 n 19)
         (to-32-bits-word (logior (logand x y)
                                  (logand (lognot x) z))))
        ((or (<= 20 n 39) (<= 60 n 79))
         (to-32-bits-word (logxor x y z)))
        ((<= 40 n 59)
         (to-32-bits-word (logior (logand x y)
                                  (logand x z)
                                  (logand y z))))))

(defun sha1-k (n)
  (cond ((<=  0 n 19) #x5A827999)
        ((<= 20 n 39) #x6ED9EBA1)
        ((<= 40 n 59) #x8F1BBCDC)
        ((<= 60 n 79) #xCA62C1D6)))
  
(defun sha1-digest (message)
  (let* ((h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (padded-message (pad-the-message message))
         (n (/ (length padded-message) 64)))
    (dotimes (i n)
      (let ((a h0) (b h1) (c h2) (d h3) (e h4) (temp 0)
            (message-block (prepare-message-block i padded-message)))
        (dotimes (i 80)
          (setq temp (to-32-bits-word (+ (rotl a 5)
                                         (sha1-f i b c d)
                                         e
                                         (sha1-k i)
                                         (aref message-block i))))
          (setq e d)
          (setq d c)
          (setq c (to-32-bits-word (rotl b 30)))
          (setq b a)
          (setq a temp))  
        (setq h0 (to-32-bits-word (+ h0 a)))
        (setq h1 (to-32-bits-word (+ h1 b)))
        (setq h2 (to-32-bits-word (+ h2 c)))
        (setq h3 (to-32-bits-word (+ h3 d)))
        (setq h4 (to-32-bits-word (+ h4 e)))))
      (list h0 h1 h2 h3 h4)))

(defun sha1 (message &key (format 'vector))
  (case format
    (list   (sha1-digest message))
    (vector (let ((list nil))
               (flet ((%to-bytes (x)
                        (push (logand #xFF (ash x -24)) list)
                        (push (logand #xFF (ash x -16)) list)
                        (push (logand #xFF (ash x -8)) list)
                        (push (logand #xFF x) list)))
                 (mapcar #'%to-bytes (sha1-digest message)))
               (coerce (nreverse list) 'vector)))
    (string (let ((digest (sha1-digest message)))
              (string-downcase (format nil "~x~x~x~x~x"
                                       (first  digest)
                                       (second digest)
                                       (third  digest)
                                       (fourth digest)
                                       (fifth  digest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; from http://scompall.nocandysw.com/sbcl-listen-socket-test.lisp
;;; should return a byte stream.
;;;
(defun connect-to-server (host port &key (buffering :full))
  "Connect to server port via tcp, returning stream object."
  #+sbcl (if host
	     (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
					  :type :stream :protocol :tcp)))
	       (sb-bsd-sockets:socket-connect
		socket (resolve-hostname host) port)
	       (sb-bsd-sockets:socket-make-stream 
		socket :output t :input t :element-type '(unsigned-byte 8)
		:buffering buffering 
		#+sb-unicode :external-format #+sb-unicode :utf-8))
	     (let ((socket (make-instance 'sb-bsd-sockets:local-socket 
					  :type :stream)))
	       (sb-bsd-sockets:socket-connect 
		socket #+darwin "/tmp/mysql.sock" 
		#+linux "/var/lib/mysql/mysql.sock")
	       (sb-bsd-sockets:socket-make-stream 
		socket :output t :input t :element-type '(unsigned-byte 8)
		#+sb-unicode :external-format #+sb-unicode :utf-8)))
  #+openmcl (declare (ignore external-format buffering))
  #+openmcl (ccl:make-socket :remote-host host :remote-port port))

#+sbcl
(defun resolve-hostname (name)
  (car (sb-bsd-sockets:host-ent-addresses
        (sb-bsd-sockets:get-host-by-name name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Old mysql password handling
;;;   RedHat distributions (as of 5.4) still use the old
;;;   (pre mysql 4.1) password handling, by default.
;;;
(defconstant +scramble-length-323+ 8)


(defun scramble-323 (message password)
  "See scramble_323 in mysql-connector-c-6.0.2/libmysql/password.c"
  (let ((to (make-array (+ +scramble-length-323+ 1)
			:element-type '(unsigned-byte 8)
			:initial-element 0)))
    (when (> (length password) 0)
      (let* ((hash-pass (hash-password (string-to-latin1 password)))
	     (hash-message (hash-password 
			    (subseq message 0 +scramble-length-323+)))
	     (rand-st (my-rnd-init (logxor (first hash-pass) 
					   (first hash-message))
				   (logxor (second hash-pass) 
					   (second hash-message)))))
	(dotimes (i +scramble-length-323+)
	  (setf (aref to i) (+ (floor (* 31 (my-rnd rand-st))) 64)))
	(let ((extra (floor (* (my-rnd rand-st) 31))))
	  (dotimes (i +scramble-length-323+)
	    (setf (aref to i) (logxor (aref to i) extra))))))
    to))

(defun hash-password (password)
  "See hash_password in mysql-connector-c-6.0.2/libmysql/password.c"
  ;; accepts a latin1 string, returning list of two 32 bit integers
  (let ((nr 1345345333) (add 7) (nr2 #x12345671))
    (loop for this across password do
	  (unless (or (eql this #\space) (eql this #\tab))
	    (let ((tmp (to-32-bits-word this)))
	      (setf nr (logxor nr (+ (* (+ (logand nr 63) add) tmp) 
				     (ash nr 8))))
	      (setf nr2 (+ nr2 (logxor (ash nr2 8) nr)))
	      (setf add (+ add tmp)))))
    ;; drop sign bit (str2int)
    (list (logand nr #x7FFFFFFF) (logand nr2 #x7FFFFFFF))))

(defconstant +rand-max+ #x3FFFFFFF)

(defstruct my-rnd seed1 seed2)

(defun my-rnd-init (seed1 seed2)
  "See my_rnd_init in mysql-connector-c-6.0.2/mysys/my_rnd.c"
  (make-my-rnd :seed1 (mod seed1 +rand-max+)
	       :seed2 (mod seed2 +rand-max+)))

(defun my-rnd (rand-st)
  "See my_rnd in mysql-connector-c-6.0.2/mysys/my_rnd.c"
  (setf (my-rnd-seed1 rand-st) 
	(mod (+ (* (my-rnd-seed1 rand-st) 3) (my-rnd-seed2 rand-st))
	     +rand-max+))
  (setf (my-rnd-seed2 rand-st) 
	(mod (+ (my-rnd-seed1 rand-st) (my-rnd-seed2 rand-st) 33)
	     +rand-max+))
  (/ (coerce (my-rnd-seed1 rand-st) 'double-float) 
     (coerce +rand-max+ 'double-float)))
