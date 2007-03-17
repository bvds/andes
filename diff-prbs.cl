;;; sbcl < diff-prbs.cl >& diff-prbs.log &
;;;
;;; (print-problem-solutions (read-problem-file (string 'kt1a) :Path #P"/home/bvds/Andes2-old/Problems/"))
;;;
(rkb)
(defvar t0 (get-internal-run-time))
(diff-prbs (Default-ProblemFile-Path) #P"/Users/bvds/Andes2-old/Problems/")
(/ (- (get-internal-run-time) t0) internal-time-units-per-second)
(quit)
