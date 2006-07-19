;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
DIP1A
DIP1B
EFIELD1A
EFIELD1B
ELEC3A
ELEC3B
ELEC4A
ELEC4B
ELEC7A
FARA11A
FARA11B
FARA9
FOR10B
FOR10
FOR1A
FOR1B
FOR2A
FOR2B
FOR7A
FOR7B
FOR8A
FOR8B
FOR9A
FOR9B
GAUSS3
IND3A
MAG1A
MAG1B
MAG1C
MAG1D
MAG1E
MAG2A
MAG3A
MAG3B
MAG4A
MAG4B
MAG5A
MAG5B
MAG6A
MAG7
MAG8B
MAGDIP1
MAGDIP2
MAGDIP3
POT1A
POT1B
POT3A
POT3B
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

