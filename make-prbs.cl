;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
COUL1A
COUL1B
COUL1C
COUL2A
COUL2B
COUL2C
COUL3
EFIELD1E
ELEC1A
ELEC1B
ELEC2
ELEC3A
ELEC3B
ELEC4A
ELEC4B
ELEC5A
ELEC5B
ELEC6A
ELEC6B
ELEC7A
FOR11A
FOR11B
FOR11C
FOR1A
FOR1B
FOR1C
FOR2A
FOR2B
FOR4A
FOR4B
FOR4C
FOR5
MAG4A
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

