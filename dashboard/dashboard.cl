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

(in-package :cl-user)

(webserver:defun-method "/dashboard" dashboard (&key version (model () model-p) 
						     section (student () student-p) 
						     (assignment () assignment-p)) 
 (apply #'process-api-request (append (list :version version :section section)
				      (if model-p (list :model model))
				      (if assignment-p (list :assignment assignment))
				      (if student-p (list :student student)))))
