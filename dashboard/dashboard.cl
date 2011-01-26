;;; Copyright 2011 by ...
;;;  This file is part of the Andes Intelligent Tutor Stystem.
;;;
;;;  The Andes Intelligent Tutor System is free software: you can redistribute
;;;  it and/or modify it under the terms of the GNU Lesser General Public 
;;;  License as published by the Free Software Foundation, either version 3 
;;;  of the License, or (at your option) any later version.
;;;
;;;  The Andes Solver is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU Lesser General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Lesser General Public License
;;;  along with the Andes Intelligent Tutor System.  If not, see 
;;;  <http:;;;www.gnu.org/licenses/>.


;; The method will also have to be added to the Andes3 API which
;; is defined in the file: web-UI/andes/andes3.smd


(webserver:defun-method "/help" dasboard (&key this or that)
  "Stub for dashboard service method."

  ;; lisp code to handle request would be here.
  ;; See file Help/sessions.cl for example methods.
  ;; You will note that the other methods use the macro (env-wrap ...)
  ;; which saves state information about a particular session;
  ;; I don't think you need this for the dashboard?

  ;; To learn about lisp, I suggest the book "ANSI Common Lisp" by
  ;; Paul Graham.  I have a copy in my office if you want to borrow it.
  ;; For reference, the LISP specification is at 
  ;;    http://www.lispworks.com/documentation/HyperSpec/Front

  ;; We will have to discuss where to put the static (HTML) content.
  ;; It might make sense to put it as a subdirectory under dashboard
  ;; and provide a link in /var/www/html to point to that subdirectory.
  ;; However, I assume it will be using Dojo and we need to figure
  ;; out how best to do that.
)
