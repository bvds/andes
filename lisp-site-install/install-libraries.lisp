;;; Install hunchentoot and json parser
(quicklisp-quickstart:install)
(push :hunchentoot-no-ssl *features*) ;we have apache to do this
(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8) ;for rfc2388.asd
(ql:quickload "hunchentoot")
(ql:quickload "cl-json")
(quit)  ; exit the lisp server
