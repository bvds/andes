;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
CAP1A CAP1B CAP2A CAP2B CAP3A CAP4A CAP5A CAP6A CAP6B COUL1A COUL1B COUL2A
COUL2B DIP1A DIP1B ELEC1A ELEC1B ELEC2 ELEC3A ELEC3B ELEC4A ELEC4B ELEC5A
ELEC5B ELEC6A ELEC6B FOR10B FOR10 FOR11A FOR11B FOR11C FOR1B FOR1 FOR2B FOR2
FOR4B FOR4 FOR5 FOR7B FOR7 FOR8B FOR8 FOR9B FOR9 LC1A LRC1A MAG2A MAG2B MAG3A
MAG3B MAG4A MAG5A MAG5B POT1A POT1B POT2A POT2B POT2C POT3A POT3B POT4 POT5
POT6 POT7 POT8 RC1A RC1B RC1C RC2A RC3A RC3B RC4A RC4B RC5A RC6A RC7A RC7B
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

