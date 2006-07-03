;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
AMP1 EPOW1 EPOW2 EPOW3 FARA10B FARA11A FARA11B FARA9 IND1A IND1B IND1C IND2A
IND3A IND3B IND3C IND4 KIR1A KIR1B KIR2A KIR3A KIR3B KIR3C KIR4A KIR5A KIR7A
LC1A LR1A LR1B LR1C LR1D LR2A LR2B LR3A LR3B LRC1A MAG6A MAG7 MAG8A MAG8B
MAGDIP1 MAGDIP2 MAGDIP3 RC1A RC1B RC1C RC2A RC3A RC3B RC4A RC4B RC5A RC6A RC7A
RC7B 
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

