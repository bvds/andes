;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(DR6A DR6B DR7A EROT1 EROT2 EROT3 EROT4 KR1A KR1B KR2A KR2B KR3A KR3B KR4A KR5A KR6A KR7A MOMR1A MOMR1B MOMR2A MOMR2B MOMR3A MOMR4A))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

