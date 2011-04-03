(in-package :webserver)

(defun-method "/dashboard" test (&key param1 param2)
   "This is a method associated with the dashboard service"
   (cons :value (+ param1 param2)))