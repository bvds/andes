(in-package :webserver)

(defun-method "/dashboard" call-dashboard (&key param1 param2)
   "This is a method associated with the dashboard service"
   (call-dashboard :x param1  :y param2))

(defun call-dashboard (&key x y)
  (+ x y))