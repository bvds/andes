;;
;;  script to write out problem statementss
;;
(require "asdf")
(asdf:operate 'asdf:load-op 'andes-help)
(write-stmts)
(exit)
