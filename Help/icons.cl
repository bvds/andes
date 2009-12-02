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
  (strcat "<img src=\"" *images-path* "axis-tool.png\" class=\"button\" alt=\"axis tool\">"))

(defparameter *axis-tool-action* (strcat "click on " *axis-tool*))

(defparameter *draw-axes* 
  (strcat "<img src=\"" *images-path* "draw-axes.png\" class=\"block\" alt=\"draw axes\">"))

(defparameter *ellipse-tool* 
  (strcat "<img src=\"" *images-path* "ellipse-tool.png\" class=\"button\" alt=\"ellipse tool\">"))

(defparameter *rectangle-tool* 
  (strcat "<img src=\"" *images-path* "rectangle-tool.png\" class=\"button\"  alt=\"rectangle-tool\">"))

(defparameter *body-tool-action* 
  (strcat "click on " *ellipse-tool* " or " *rectangle-tool*))

(defparameter *vector-icon* 
  (strcat "<img src=\"" *images-path* "vector-tool.png\" class=\"button\"  alt=\"vector tool\">"))
(defparameter *vector-tool-action* (strcat "click on " *vector-icon*))

(defparameter *equation-icon* 
  (strcat "<img src=\"" *images-path* "equation-tool.png\" class=\"button\"  alt=\"equation tool\">"))

(defparameter *equation-tool-action* (strcat "click on " *equation-icon*))

;; Make an object that looks like the text input box.
(defun text-box (x) 
  (strcat "<span class=\"text-box\">" x "</span>"))
