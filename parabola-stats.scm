;;; This file is part of Adaptive Plot, a library for plotting
;;; functions from the MIT Scheme REPL
;;; Copyright (C) 2010-2011 Alexey Radul
;;;
;;; Adaptive Plot is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; Adaptive Plot is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with Adaptive Plot.  If not, see
;;; <http://www.gnu.org/licenses/>.

(declare (usual-integrations))

;;;; Some geometry of parabolas

;;; I am interested in refining my plots with the points that make the
;;; most visual difference.  My heuristic for visual difference is
;;; that the new point should, locally, bring the piecewise-linear
;;; plot closest to the piecewise-parabolic plot, where closeness is
;;; measured by the area of the difference between them.  For this, I
;;; need to be able to answer the following geometric question:
;;;
;;;  "Consider the parabola P passing through three points p1, p2, p3,
;;;   where p2 is taken to be between p1 and p3 on the curve of P.
;;;   Call the area bounded by P and the segment [p1,p2] the 'left
;;;   lobe', and the area bounded by P and the segment [p2,p3] the
;;;   'right lobe'.  For each lobe, what is its area, and what are the
;;;   coordinates of that point x on P that forms the triangle of
;;;   maximum area with the segment bounding that lobe?"
;;;
;;; I solve this question by the strategy of iterative simplification.
