;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(MAG2A MAG2B MAG3A MAG3B MAG4A MAG5A MAG5B
DR2A DR2B DR3A DR4A DR5A DR6A DR6B DR7A DR8A
DIP1A MAGDIP1 MAGDIP2))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

