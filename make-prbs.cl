;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
CAP1A CAP1B CAP2A CAP2B CAP3a cap4a cap5a DR1A dr2a dr2b dr3a dr4a dr5a dr6a
dr6b dr7a dt10a dt11a dt11b dt12a dt13a dt14a dt14b dt1a dt1b dt1c dt2a dt3a
dt3b dt3c dt4a dt4b dt5a dt6a dt7a dt7b dt8a dt9a e10a e11a e12a e1a e1b e1c
e2a e2b e2c e3a e4a e4b e4c e5a e5b e6a e7a e7b e8a e8b e9a e9b EPOW1 EPOW2
EPOW3 eqcap1a eqcap1b eqcap1c eqcap1d eqcap2a eqcap2b eqcap3a eqcap3b eqcap4a
eqcap4b eqcap5a eqcap6a eqres1a eqres1b eqres1c eqres2a eqres2b eqres3a eqres3b
eqres4a eqres4b eqres5a eqres6a fara1a fara1b fara2a fara2b fara3a fara3b
fara4a fara4b fara5a fara5b fara5c fara6a fara6b fara7a fara7b fara7c fara7d
fara8a fara8b fara8c fbd1a fbd1b fbd2a fbd3a fbd4a fbd5a fbd6a IND1A IND1B
IND1C IND2A KGRAPH1 KGRAPH2 KGRAPH3 KGRAPH4 KGRAPH5 KGRAPH6 KGRAPH7 KGRAPH8
kir1a kir1b kir2a kir2c kir2d kir3a kir3b kir3c kir4a kir5a kir7a kr1a kr3b
kr6a kr7a kt10a kt10c kt10d kt11a kt11b kt12a kt12b kt12c kt13a kt13b kt1a kt1b
kt2a kt3a kt3b kt4a kt5a kt6a kt6b kt7a kt8a kt8b kt9b LC1A lmom1a lmom1b
lmom2a lmom2b lmom3a lmom4a LR1A LR1B LR1C LR1D LR2A LR2B LR3A LR3B LRC1A MAG1A
PGRAPH1 PGRAPH2 PGRAPH3 pow1a pow1b pow2a pow3a pow4a pow4b pow5a rc1a rc1b
rc1c rc2a rc3a rc3b relvel1a relvel2a relvel3a rots1b rots1c rots2a rots3a
rots4a rots5a rots6a rots6b rots6c rots7a rots8a rots8b s10a s11a s11b s1a s1b
s1c s1d s1e s1f s2a s2b s2c s2d s2e s3a s3b s3c s4b s5a s6a s7a s8a s9a
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

