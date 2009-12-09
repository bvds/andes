    (lambda (features)
      (flet ((enable (x)
               (pushnew x features))
             (disable (x)
               (setf features (remove x features))))
        ;; Threading support, available only on x86/x86-64 Linux, x86 Solaris
        ;; and x86 Mac OS X (experimental).
        (enable :sb-thread)))
