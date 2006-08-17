;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
E10A
E12A
E4A
E4BB
E4B
E4CC
E4C
E5AA
E5A
E5B
E6A
E7A
E7B
E8AA
E8A
E8B
E9A
E9B
EROT2
EROT3
EROT4
LMOM5
POW5A
WE3A
WE3B
WE4A
WE4B
WE5
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

