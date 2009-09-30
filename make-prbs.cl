;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls solutions/*.prb | sed "s/solutions\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
  (make-prbs '(COUL1A COUL1B COUL2A COUL2B E1A EDIAG1 ELEC10 FOR10A FOR10B FOR7A FOR7B FOR8A FOR8B FOR9A FOR9B POT1A POT1B POT2A POT2B POT2C POT3A POT3B POT4 POT5 POT6 Q5 WEQ5)
)
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

