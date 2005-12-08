;;; sbcl < diff-prbs.cl >& diff-prbs.log &
(rkb)
(defvar t0 (get-internal-run-time))
(diff-prbs (Default-ProblemFile-Path) #P"/home/bvds/Andes2-old/Problems/")
(/ (- (get-internal-run-time) t0) internal-time-units-per-second)
(quit)
