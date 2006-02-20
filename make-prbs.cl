;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(DIP1A 
E10A 
E11A 
E1A 
E1B 
E1C 
E2A 
E2B 
E2C 
E3A 
E8AA 
E8A 
E8B 
E9A 
E9B 
ELEC3B 
ELEC4B 
ELEC5B 
ELEC6B 
FARA11A 
FARA11B 
FARA9 
FOR11B 
FOR11C 
GAUSS3 
POW2A 
POW3A 
POW5A 
POW5B 
POW5C 
POW5D 
POW6A 
WE1A 
WE1B 
WE2B 
WE4A 
WE4B 
WE5 
WE6))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

