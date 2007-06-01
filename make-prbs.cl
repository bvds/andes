;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(DIP1A
EROT2
EROT3
EROT4
KR2A
KR2B
KR3A
KR3B
KR3C
KR4A
KR4B
KR5A
KR6A
KR9
MOMR1A
MOMR1B
MOMR2A
MOMR2B
MOMR3A
MOMR4A
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

