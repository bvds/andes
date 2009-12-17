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
;;;  <http:;;;www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ctl-c ctl-k compiles entire file 

;;;
;;;             References to User Interface elements.
;;;

(defparameter *images-path* "/review/ui-images/")

(defparameter *axis-tool* 
  (open-review-window-html "the Axis Tool" "introduction.html#axis-tool" 
			   :title "Manual"))

(defparameter *axis-tool-action* (strcat "click on " *axis-tool*))

(defparameter *draw-axes* 
  (strcat "<img src=\"" *images-path* "draw-axes.png\" class=\"block\" alt=\"draw axes\">"))

(defparameter *ellipse-tool* 
  (strcat "<img src=\"" *images-path* "ellipse-tool.png\" class=\"button\" alt=\"ellipse tool\">"))

(defparameter *rectangle-tool* 
  (strcat "<img src=\"" *images-path* "rectangle-tool.png\" class=\"button\"  alt=\"rectangle-tool\">"))

(defparameter *body-tool* 
  (open-review-window-html "the Body Tool" "introduction.html#body-tool" 
			   :title "Manual"))

(defparameter *body-tool-action* 
  (strcat "click on " *ellipse-tool* " or " *rectangle-tool*))

(defparameter *vector-icon* 
  (strcat "<img src=\"" *images-path* "vector-tool.png\" class=\"button\"  alt=\"vector tool\">"))
(defparameter *vector-tool* 
  (open-review-window-html "the Vector Tool" "introduction.html#vector-tool" 
			   :title "Manual"))
(defparameter *vector-tool-action* (strcat "click on " *vector-icon*))

(defparameter *equation-icon* 
  (strcat "<img src=\"" *images-path* "equation-tool.png\" class=\"button\"  alt=\"equation tool\">"))
(defparameter *equation-tool* 
  (open-review-window-html "the Equation Tool" "introduction.html#equation-tool"
			   :title "Manual"))
(defparameter *equation-tool-action* (strcat "click on " *equation-icon*))

(defparameter *text-tool* 
  (open-review-window-html "the Text Tool" "introduction.html#text-tool"
			   :title "Manual"))
(defparameter *text-tool-action* (strcat "click on " *text-tool*))

;; Make an object that looks like the text input box.
(defun text-box (x) 
  (strcat "<span class=\"text-box\">" x "</span>"))

(defun begin-sentence (x)
  "Capitalize for beginning of sentence"
  ;; Abstract this because we might eventually have links.
  (format nil "~@(~A~)~A" (elt x 0) (subseq x 1)))
