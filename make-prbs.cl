;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls solutions/* | sed "s/solutions\/\(.*\)/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
  (make-prbs '(S10A S11A S11B S12A S13 S14 S1A S1B S1C S1D S1E S1F S2A S2B S2C S2D S2E S2ESOLVED S3A S3B S3C S4A S4B S5A S6A S7A S7B S8A S9A ))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

