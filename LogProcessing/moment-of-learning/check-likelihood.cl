;; Check maximum likelihood for A+B exp(-C j) and step model

(defun binary-digits (n)
  (if (= n 0)
      (list nil)
      (let ((x (binary-digits (- n 1))))
      (append
       (mapcar #'(lambda (y) (cons 0 y)) x)
       (mapcar #'(lambda (y) (cons 1 y)) x)))))

(defun step-model (s g l) 
  (lambda (j) (if (< j l) g (- 1 s)))) 

(defun bkt-model (s g a) 
  (lambda (j) (- 1 s (* (- 1 s g) (exp (* (log a) j))))))

;; (log-like '(0 1 1) (bkt-model .2 .2 .3))
(defun log-like (data model)
  (loop for d in data and
       j from 0 
       sum (log (if (= d 1)
	       (funcall model j)
	       (- 1 (funcall model j))))))

(defvar *search-steps* 5)

(defmacro my-loop (var limits &rest body)
  `(loop for ,var from (car ,limits) to (cdr ,limits) 
      by (/ (- (cdr ,limits) (car ,limits)) *search-steps*) do
	,@body))

;; (find-minimum '(0 0 1) 'step-model '(0.1 . 0.9) '(0.01 . 0.99) '(0.001 . 2))
(defun find-minimum (data model-name l1 l2 l3)
 "Brute force search for minimum for given bit-string and model and limits"
 (let (best best-params)
   (my-loop x1 l1 
	   (my-loop x2 l2
		   (my-loop x3 l3
			   (let ((ll (log-like data (funcall model-name
							     x1 x2 x3))))
			   (when nil (format t " ~A ~A ~A ~A~%" x1 x2 x3 ll))
			     (when (or (null best) (> ll best))
			       (setf best ll)
			       (setf best-params (list x1 x2 x3)))))))
   (values best best-params)))

(defun compare-models (n)
  (dolist (data (binary-digits n))
    (let (
   
