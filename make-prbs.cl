;;; script for regenerating all problem files
;;; grep minutes make-prbs.log | sort -g -k7
;;;  ls Problems/*.prb | sed "s/Problems\/\(.*\).prb/\1/"
;;; sbcl < make-prbs.cl >& make-prbs.log &
 (rkb)
 (defvar t0 (get-internal-run-time))
 (make-prbs '(
FOR1 FOR10B
FOR2 FOR4 FOR4C FOR7 FOR8 FOR9 FOR9B MAG1C MAG2A MAG2B MAG3A MAG5A
MAG5B POT1A POT2A POT2B POT2C POT3A POT3B POT4 POT5 POT7 
for1b for10 for2b for4b for7b mag1d mag1e mag3b mag4a pot1b pot6 pot8
))
;; time to do this is:
(format t "~F hours~%" (/ (- (get-internal-run-time) t0) 
			  (* 3600 internal-time-units-per-second)))
(quit)

