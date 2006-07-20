;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
DT1B
DT1C
DT3C
DT4A
DT4B
DT5A
DT6A
E12A
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
KT10A
KT10C
KT11A
KT11B
KT12A
KT12B
KT12C
KT13A
KT13B
KT13C
KT7A
KT8A
KT8B
KT9A
KT9B
WE3B
WE6
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

