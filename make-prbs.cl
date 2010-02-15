;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls solutions/*.prb | sed "s/solutions\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
  (make-prbs '(S10A.prb S11A.prb S11B.prb S12A.prb S13.prb S14.prb S1A.prb S1B.prb S1C.prb S1D.prb S1E.prb S1F.prb S2A.prb S2B.prb S2C.prb S2D.prb S2E.prb S2ESOLVED.prb S3A.prb S3B.prb S3C.prb S4A.prb S4B.prb S5A.prb S6A.prb S7A.prb S7B.prb S8A.prb S9A.prb ))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

