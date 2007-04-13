; Allegro will load asdf.lisp from current directory if it is not
; part of the lisp installation:
(require "asdf") 

; recompile this file to include runtime-version-only code
(compile-file "Help/Andes2-main.cl")

; Following no longer has IDE-specific code:

; recompile this file to exclude possible IDE-specific code.
; (compile-file "SGG/Qsolver.cl")

; now do the load
(asdf:operate 'asdf:load-op 'andes-help)
