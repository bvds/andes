;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;; sbcl < make-prbs.cl >& make-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(make-prbs '(DR2A DR2B DR3A DR4A DR5A DR6A DR6B DR7A DR8A DT10A DT11A DT11B E10A E11A E1A E1B E1C E2A E2B E2C E3A E8A E8B E9A E9B ELEC3B ELEC4B ELEC5B ELEC6B FBD1B FLUIDS13 FOR11B FOR11C IMP1 IMP2 IMP3A IMP3B IMP3C LMOM1A LMOM1B LMOM2A LMOM2B LMOM3A LMOM4A LR1A LR1B LR1C LR1D LR2A LR2B LR3A LR3B MOMR3A MOMR4A OSC4 POW2A POW3A POW4A POW4B POW5A POW5B POW5C POW5D POW6A RC1A RC1B RC1C RC2A RC3A RC3B RC4A RC4B RC5A RC6A RC7A RC7B REF2AAA REF2AA REF2A REF2B REF2C S2D WE1A WE1B WE3B WE4A WE4B WE5 WE6))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

