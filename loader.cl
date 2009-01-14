; Allegro will load asdf.lisp from current directory if it is not
; part of the lisp installation:
(require "asdf") 

; shorthand:
(defun ra ()  ; reload all -- andes-help requires sgg as well
   (asdf:operate 'asdf:load-op 'andes-help))
(defun rkb () ; reload knowledge base
  (asdf:operate 'asdf:load-op 'problems))

; now load everything:
(ra)


