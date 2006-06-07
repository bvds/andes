;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
EPOW1 EPOW2 EPOW3 IND4 KIR1A KIR1B KIR2A KIR3A KIR3B KIR3C KIR4A KIR5A KIR7A
LC1A LR1A LR1B LR1C LR1D LR2A LR2B LR3A LR3B LRC1A RC1C RC3B
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

