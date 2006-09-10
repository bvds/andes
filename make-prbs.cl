;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
CAP1A
CAP1B
CAP2A
CAP2B
CAP3A
CAP6A
CAP6B
LC1A
LRC1A
RC1A
RC1B
RC3A
RC3B
RC4A
RC4B
RC5A
RC6A
RC7A
RC7B
RC9
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

