;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(FOR10B FOR10 FOR7B FOR7 FOR8B FOR8 FOR9B FOR9 POT1A POT1B POT2A POT2B POT2C POT3A POT3B POT4 COUL1A COUL1B COUL2A COUL2B))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

