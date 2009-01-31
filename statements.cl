;;
;;  script to write out problem statementss
;;
(require "asdf")

; Windows can't handle symbolic links; we need to explicitly add
; problems directory to the asdf search path 
(pushnew #p"problems/" asdf:*central-registry*)

(asdf:operate 'asdf:load-op 'andes-help)
(write-stmts)
(exit)
