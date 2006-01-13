;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(cap3a cap4a kir2a kir4a kir5a kir7a))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

