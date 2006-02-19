;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(WE6 WE4B
     POW6A
     POW5C
     POW5B
     POW4B
     POW4A
     POW3A
     POW2A
     FOR11C
     FOR11B
     EROT1
     ELEC6B
     ELEC5B
     ELEC4B
     ELEC3B
     E9A
     E8A
     E3A
     E11A
     E10A
     DR8A))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

