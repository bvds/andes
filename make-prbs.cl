;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
DIP1B DT5A E10A E11A E12A E1A E1B E1C E2A E2B E2C E3A E4A E4B E4BB E4C E4CC E5A E5AA E5B E6A E7A E7B E8A E8AA E8B E9A E9B EROT1 EROT2 EROT3 EROT4 KT10A KT10C KT11A KT11B KT13A KT13B KT13C KT14B KT9A KT9B LMOM5 MAGDIP4 POT7 POT8 POW1A POW1B POW2A POW4A POW4B POW5A POW6A WE1A WE1B WE2A WE2B WE3A WE3B WE4A WE4B WE5 WE6
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

