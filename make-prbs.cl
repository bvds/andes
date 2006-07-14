;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
KT14B
MAG5A
MAG5B
ROTS1A
ROTS1B
ROTS1C
ROTS3A
ROTS4A
ROTS5A
ROTS6B
ROTS6C
ROTS7A
ROTS8A
ROTS8B
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

