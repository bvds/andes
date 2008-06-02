;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
  (make-prbs '(WEQ2 WE6 WE5 WE4A WE3B WE3A WE2A STATIC1T S6APLAN Q5 Q2 Q1 POW5APLAN POW5A POW4B POW4A POW2A POW1B POW1A POT8 POT7 MAGDIP4 MAG4B LMOM7 LMOM6 LMOM5 LMOM4APLAN LMOM3A GRAV1 FOR6 FOR4D FOR3 FOR11C FOR11B EROT4 EROT3 EROT2 ELEC6C ELEC6B ELEC5B ELEC4B ELEC3C ELEC3B EGRAV1 E9B E9A E8B E8A E7B E7APLAN E7A E6A E4BB E11A E10A DT7BPLAN DR1A-OLD DIP1B COMPO3 COMPO2 COMPO1))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

