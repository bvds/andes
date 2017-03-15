;; Check maximum likelihood for A+B exp(-C j) and step model

(defun binary-digits (n)
  (if (= n 0)
      (list nil)
      (let ((x (binary-digits (- n 1))))
      (append
       (mapcar #'(lambda (y) (cons 0 y)) x)
       (mapcar #'(lambda (y) (cons 1 y)) x)))))

(defun step-model (s g l) 
  (lambda (j n) (declare (ignore n)) (if (< j l) g (- 1 s)))) 

(defun bkt-model (s g a) 
  (lambda (j n) (- 1 s (* (- 1 s g) (expt a (/ j (- n 1) a))))))

;; (log-like '(0 1 1) (bkt-model .2 .2 .3))
(defun log-like (data model)
  (let ((n (length data)))
    (loop for d in data and
       j from 0 
       sum (log (if (= d 1)
		    (funcall model j n)
		    (- 1 (funcall model j n)))))))


(defmacro my-loop (var limits &rest body)
  `(loop for ,var from (car ,limits) to (cadr ,limits) 
      by (or (caddr ,limits) 1) do
	,@body))

;; (find-minimum '(0 0 1) 'step-model '(0 1 .25) '(0 1 .25) '(0 2))
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
   (cons best best-params)))

;; 114 sec for 10, 10 on laptop.
(defun compare-models (steps n)
  (let ((dx (/ 1 n)))
  (dolist (data (binary-digits steps))
    (let ((step-result (find-minimum data 'step-model 
				     (list 0 1 dx)
				     (list 0 1 dx)
				     (list 0 (- steps 1))))
	  (bkt-result (find-minimum data 'bkt-model 
				    (list 0 1 dx)
				    (list 0 1 dx)
				    ;; Can't be exactly zero
				    (list (/ dx 2) (- 1 (/ dx 2)) dx)
				    )))
      (when (> (car bkt-result) (car step-result))
	(format t "Results for ~A: step ~A and BKT ~A~%" 
		data step-result bkt-result))))))

(time (compare-models 10 20))
