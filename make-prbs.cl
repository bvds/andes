;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(elec3a elec3b elec4b elec5b elec6b for11b for11c))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

