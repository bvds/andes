
(defun SystemEntries->csdos (entries)
  (sort (remove-duplicates (mappend #'SystemEntry-Sources entries)
			   :key #'csdo-op :test #'unify) #'expr< :key #'csdo-op))

(defun any-turn-text (x) (when x (turn-text x)))

;; This depends on stuff in Help
(progn (andes-init) (read-problem-info (string 'kt10a)))

(dolist (soln (problem-solutions *cp*))
  (format t "solution:~%  Eqn Nodes:~%")
  (dolist (eqn (EqnSet-Eqns soln)) 
   (let ((opinsts (SystemEntries->csdos  (bgnode-entries eqn))))
    (format t "    ~S ~A~%~{      ~S~%~}" (Enode-id eqn) (psm-exp (enode-id eqn)) 
	    (mapcar #'csdo-op opinsts))))
  (format t "  Quant Nodes:~%")
  (dolist (eqn (remove-if-not #'Qnode-p (EqnSet-nodes soln)))
   (let ((opinsts (SystemEntries->csdos  (bgnode-entries eqn))))
      (format t "    ~S ~A ~A~%~{      ~S~%~}" 
	      (Qnode-exp eqn) (nlg (qnode-exp eqn)) (qnode-var eqn)
	      (mapcar #'csdo-op opinsts))))
  (format t "  Operator instances:~%")
  (dolist (opinst (SystemEntries->csdos  
		   (mappend #'bgnode-entries (EqnSet-nodes soln))))
    (let ((turn (mapcar #'(lambda (type) (make-hint-seq 
					  (collect-step-hints opinst 
						  :type type)))
			'(point bottom-out))))
      (format t "    ~S~%~{      ~S~%~}" 
	      (csdo-op opinst)  (mapcar #'any-turn-text turn)))))
