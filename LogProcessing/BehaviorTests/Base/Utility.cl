#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility.cl
;; Collin Lynch
;;
;; Base Functions that are shared among the various files.
|#

(defun sum-lists2 (&rest Rlists)
  (apply #'mapcar #'+ Rlists))

(defun sum-lists (&rest Rlists)
  (declare (type (cons (cons integer *) *) Rlists)
	   (optimize speed (safety 0)))
  (let ((Len (length (car Rlists)))
	(A (make-list (length (car Rlists)) :initial-element 0)))
    (dolist (L Rlists)
      (dotimes (N Len)
	(setf (nth N A) (+ (nth N A) (nth N L)))))
    A))

(defun sum-int-list-pair (L1 L2)
  (declare (type (cons integer *) L1)
	   (type (cons integer *) L2)
	   (optimize speed (safety 0)))
  (let ((Len (length L1)) (R (make-list (length L1))))
    (dotimes (N Len)
      (setf (nth N R) (+ (nth N L1) (nth N L2))))
    R))


;;(defun foo ()
;;  (time (progn (setq R (fbr-select-results 10 **command-testset** "study_usna2000_2" C))
;;	       (fbr-combine-results **command-testset** R)
;;	       (close-qresult R))))

;;; --------------------------------------------------------
;;; list-diff
;;; Given a pair of lists determine the number of locations 
;;; within them that differ adding up excess lengths.  
(defun list-diff (A B &optional (count 0))
  (if (or (null A) (null B))
      (+ Count (length A) (length B))
    (if (equalp (car A) (car B))
	(list-diff (cdr A) (cdr B) Count)
      (list-diff (cdr A) (cdr B) (+ 1 Count)))))


(defun compile-date-str (Month Day Year)
  (format Nil "~a-~a-~a" Month Day Year))

(defun compile-time-str (Hour Min Sec)
  (format Nil "~a:~a:~a" Hour Min Sec))