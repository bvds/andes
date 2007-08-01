;;; Need a platform-independent random number generator

(defun initialize-random-elt (name)
"Turn a string into a vector of unsigned 32 bit integers and use this to seed the random number generator."
  (set-mt19937 (coerce (loop for char across name 
     collect (char-code char)) '(vector (unsigned-byte 32)))))

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (mt19937:random (length seq))))

(defun random-choice (probability)
  "Return T randomly with given probability"
    (> probability (mt19937:random 1.0)))

;; seeding mt19937 is not obvious.  Copied the following from a Maxima page  
(defun set-mt19937 (seed)
  (setq mt19937::*random-state* 
	 (mt19937::make-random-object 
	  :state (mt19937::init-random-state seed)))
  t)

