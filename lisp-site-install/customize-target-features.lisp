    (lambda (features)
      (flet ((enable (x)
               (pushnew x features))
             (disable (x)
               (setf features (remove x features))))
        ;; Threading support, available only on x86/x86-64 Linux, x86 Solaris
        ;; and x86 Mac OS X (experimental).
        (enable :sb-thread)
	;; Get messages like:
	;;
	;; Warning: no pointer at 141d9be8 in hash table: this indicates 
	;; non-fatal corruption caused by concurrent access to a hash-table 
	;; from multiple threads. Any accesses to hash-tables shared between 
	;; threads should be protected by locks.
	;;
	;; The processs then eventually crashes after completing about
	;; 200 sessions.
	;;
	;; These messages, and the subsequent crash go away if this 
	;; feature is enabled.
	(enable :sb-hash-table-debug)))
