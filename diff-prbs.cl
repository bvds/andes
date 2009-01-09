;;; sbcl < diff-prbs.cl >& diff-prbs.log &
;;;
;;; (print-problem-solutions (read-problem-file (string 'kt1a) :Path #P"/home/bvds/Andes2-old/solutions/"))
;;;
(rkb)
(defvar t0 (get-internal-run-time))
(diff-prbs (Default-ProblemFile-Path) #P"/home/bvds/Andes2-old/solutions/")
(/ (- (get-internal-run-time) t0) internal-time-units-per-second)
(quit)
