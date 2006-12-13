;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
DIP1A DIP1B EFIELD1E ELEC1B ELEC2 ELEC3A ELEC3B ELEC4A ELEC4B ELEC5A ELEC5B
ELEC6A ELEC6B FARA11A FARA11B FARA9 FOR11A FOR11B FOR11C FOR4A FOR4B FOR4C
GAUSS10 GAUSS6 GAUSS8 GAUSS9 MAG11 MAG1C MAG1D MAG1E MAG2A MAG2B MAG3A MAG3B
MAG4A MAG4B MAG5A MAG5B MAG8A MAGDIP1 MAGDIP3 MAGDIP4 MAGTOR1A MAGTOR1B
MAGTOR1C MAGTOR1D
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

