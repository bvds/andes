;;; Copyright 2011 by Kurt Vanlehn and Brett van de Sande
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
;;;;
;;;;       Create lon-capa homeworks
;;;;   


(asdf:operate 'asdf:load-op 'lon-capa)
(lon-capa-problem-sets *sets* #P"all-problems/")
(lon-capa-problem-maps *sets* "/res/asu/bvds/all-problems" 
		       :practice-p t
		       :exclude-sets *guerra-assigned*
		       :path #P"guerra-maps/"
		       :title "Practice problems")
(lon-capa-problem-maps *guerra-assigned* "/res/asu/bvds/all-problems" 
		       :path #P"guerra-maps/"
		       :title "Assigned problems")
(lon-capa-problem-maps *sets* "/res/asu/bvds/all-problems" 
		       :practice-p t
		       :exclude-sets *blackwood-assigned*
		       :path #P"blackwood-maps/"
		       :title "Practice problems")
(lon-capa-problem-maps *blackwood-assigned* "/res/asu/bvds/all-problems" 
		       :path #P"blackwood-maps/"
		       :title "Assigned problems")
(lon-capa-problem-maps *sets* "/res/asu/bvds/all-problems" 
		       :practice-p t
		       :exclude-sets *usna-fall-2007*
		       :path #P"usna-fall-maps/"
		       :title "Practice problems")
(lon-capa-problem-maps *usna-fall-2007* "/res/asu/bvds/all-problems" 
		       :path #P"usna-fall-maps/"
		       :title "Assigned problems")
(lon-capa-problem-maps *sets* "/res/asu/bvds/all-problems" 
		       :practice-p t
		       :exclude-sets *usna-spring-2008*
		       :path #P"usna-spring-maps/"
		       :title "Practice problems")
(lon-capa-problem-maps *usna-spring-2008* "/res/asu/bvds/all-problems" 
		       :path #P"usna-spring-maps/"
		       :title "Assigned problems")
(quit)
