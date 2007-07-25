; Allegro will load asdf.lisp from current directory if it is not
; part of the lisp installation:
(require "asdf") 

; force these to be recompiled on load in a context that defines
; feature allegro-cl-runtime so as to include runtime-version
; conditional code
(compile-file "Help/Andes2-main.cl")
(compile-file "Help/Commands.cl")

; Following no longer has IDE-specific code:

; recompile this file to exclude possible IDE-specific code.
; (compile-file "SGG/Qsolver.cl")

; now do the load
(asdf:operate 'asdf:load-op 'andes-help)
