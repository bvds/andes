; Allegro will load asdf.lisp from current directory if it is not
; part of the lisp installation:
(require "asdf") 

; recompile this file to include runtime-version-only code
(compile-file "Help/Andes2-main.cl")

; now do the load
(asdf:operate 'asdf:load-op 'andes-help)
