;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
EMWAVE5 IMP1 IMP2 IMP3A IMP3B IMP3C LMOM1A LMOM1B LMOM2A LMOM2B LMOM3A LMOM4A LMOM5 MOMR3A MOMR4A lmom6
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

