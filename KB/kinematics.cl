;;;;
;;;;   Do motion diagrams as described in the first Chapter of Knight
;;;;

;; this goal used as sought in vector-drawing-only problem (magtor*)
(def-goalprop motion-diagram (motion-diagram ?b . ?rest)
  :english ("drawing a motion diagram for ~A" (nlg ?b)))

(defoperator do-motion-diagram (?b)
  :preconditions 
  ( 
;   (foreach ?time ?times (body ?b :time ?time)) 
   (bind ?intervals (mapcar #'(lambda (a b) `(during ,a ,b)) 
			  ?times (cdr ?times)))
   (foreach ?interval ?intervals 
	    (vector ?b (velocity ?b :time ?interval) ?dir))
   (bind ?atimes (butlast (cdr ?times)))
   (foreach ?atime ?atimes 
	    (vector ?b (accel ?b :time ?atime) ?dir))
   )
  :effects ( (motion-diagram ?b ?times) ))
