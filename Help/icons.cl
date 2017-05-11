;;; Copyright 2009 by Kurt Vanlehn and Brett van de Sande
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
;;;  <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctl-c ctl-k compiles entire file 

;;;
;;;             References to User Interface elements.
;;;

(defparameter *images-path* "../review/ui-images/")
(defun manual-link (name html-id &key (pre "the"))
  (strcat pre " " (open-review-window-html name "manual.html"
					   :section html-id
					   :title "Manual" :value html-id)))

(defun open-review-window-html (text href &key section title value)
  "Html for opening web page in the review directory."
  ;; Any :title must not contain spaces, since it causes and error in IE.
  ;; Work-around in client breaks logging.
  ;; See http://developer.mozilla.org/En/DOM/Window.open
  (when (and title (find #\space title))
    (warn "open-review-window-html title \"~A\" contains space" title))
  ;; Keyword :title should match name in web-UI/andes/menu.js
  (format nil "<a href=\"#\" onClick=\"andes.help.link('~A'~@[,'~A'~]);andes.principles.review('~A','~A'~@[,'~A'~]);\">~A</a>" 
	  (or title href)
	  value ;logging value (optional)
	  href 
	  (or title href)
	  section
	  text))
  
(defparameter *intro-video-action*
   "view the <a href=\"#\" onClick=\"andes.help.link('IntroVideo');andes.principles.review('vec1a-video.html','IntroVideo',null,'width=650,height=395');\">introductory video</a>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *line-icon* 
  (strcat "<img src=\"" *images-path* "line-tool.png\" class=\"button\"  alt=\"vector tool\">"))
(defparameter *line-tool*  (manual-link "Line Tool" "line-tool"))
(defparameter *line-tool-action* (strcat "click on " *line-tool*))

(defparameter *axis-tool* (manual-link "Axis Tool" "axis-tool"))
(defparameter *axis-tool-action* (strcat "click on " *axis-tool*))
(defparameter *draw-axes* 
  (strcat "<img src=\"" *images-path* "draw-axes.png\" class=\"block\" alt=\"draw axes\">"))

(defparameter *ellipse-tool* 
  (strcat "<img src=\"" *images-path* "ellipse-tool.png\" class=\"button\" alt=\"ellipse tool\">"))
(defparameter *rectangle-tool* 
  (strcat "<img src=\"" *images-path* "rectangle-tool.png\" class=\"button\"  alt=\"rectangle-tool\">"))
(defparameter *body-tool*  (manual-link "Body Tool" "body-tool" :pre "a"))
(defparameter *body-tool-action* 
  (strcat "click on " *body-tool* " (" *ellipse-tool* " or " 
	  *rectangle-tool* ")"))

(defparameter *vector-icon* 
  (strcat "<img src=\"" *images-path* "vector-tool.png\" class=\"button\"  alt=\"vector tool\">"))
(defparameter *vector-tool*  (manual-link "Vector Tool" "vector-tool"))
(defparameter *vector-tool-action* (strcat "click on " *vector-tool*))

(defparameter *z-axis-icon* 
  (strcat "<img src=\"" *images-path* "z-axis.png\" class=\"button\"  alt=\"z-axis button\">"))
(defparameter *z-axis-vector*  (manual-link "Vector Tool (with z-axis)" "z-axis-vector"))
(defparameter *z-axis-vector-action* (strcat "click on " *vector-tool* " and " *z-axis-icon*))

(defparameter *equation-icon* 
  (strcat "<img src=\"" *images-path* "equation-tool.png\" class=\"button\"  alt=\"equation tool\">"))
(defparameter *equation-tool*  (manual-link "Equation Tool" "equation-tool"))
(defparameter *equation-tool-action* (strcat "click on " *equation-tool*))

(defparameter *text-tool*  (manual-link "Text Tool" "text-tool"))
(defparameter *text-tool-action* (strcat "click on " *text-tool*))

(defparameter *define-variable* (manual-link "define a variable" 
					     "define-quantities" :pre ""))
(defparameter *add-label* (manual-link "add a label" 
					     "define-quantities" :pre ""))

(defparameter *delete-object* (manual-link "delete your entry" 
					     "delete" :pre ""))

(defparameter *help-button* (manual-link "the hint button" 
					"help-button" :pre ""))
(defparameter *help-button-action* (strcat "click on " *help-button* " below"))

(defparameter *unevaluated-entry* (manual-link "comment" 
					       "comment" :pre ""))

(defparameter *solve-for-quantity*  
  (manual-link "solve for" "solve-for" :pre ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *constants-menu-action* 
  "select \"Constants\" in the \"Physics\" menu")

(defparameter *unknown-z-direction-action* 
  "draw it in in an unknown z-direction")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make an object that looks like the text input box.
(defun text-box (x) 
  (strcat "<span class=\"text-box\">" x "</span>"))

(defun begin-sentence (x)
  "Capitalize for beginning of sentence"
  ;; Abstract this because we might eventually have links.
  (string-upcase x :end 1))
