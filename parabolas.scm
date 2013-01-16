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

;;;; Geometry of parabolas

;;; I am interested in refining plots with the points that make the
;;; most visual difference.  My heuristic for visual difference is
;;; that the new point should, locally, bring the piecewise-linear
;;; plot closest to the piecewise-parabolic plot, where closeness is
;;; measured by the area of the difference between them.  For this, I
;;; need to be able to answer the following geometric question:
;;;
;;;  "Consider the vertical parabola P passing through three points
;;;   p1, p2, p3, where the x coordinates are taken to be in order: x1
;;;   < x2 < x3.  [This parabola is given by Lagrange interpolation of
;;;   a polynomial quadratic in x through the given y coordinates.]
;;;   Call the area bounded by P and the segment [p1,p2] the 'left
;;;   lobe', and the area bounded by P and the segment [p2,p3] the
;;;   'right lobe'.  For each lobe, what is its area, and what are the
;;;   coordinates of that point x on P that forms the triangle of
;;;   maximum area with the segment bounding that lobe?"
;;;
;;; Let us call the answer to this question the "stats" of that lobe
;;; of that parabola.  I approach this question by the strategy of
;;; iterative simplification: from the general case, move p2 to the
;;; origin, then scale the vertical axis to make an obtuse parabola,
;;; then rotate the desired lobe so that segment lies on the x axis,
;;; and then the answer is obvious.  Unfortunately, I realized two
;;; years after writing the program that the rotation step at the end
;;; changes the parabola, so technically I am not actually answering
;;; the desired question but a different one.  However, the aesthetic
;;; results seem to be pretty good, so I haven't fixed it yet.  TODO.

;;; The stats of the given lobe of the parabola that goes through
;;; (x1, y1), (x2, y2), and (x3, y3), under the condition that x1 < x2
;;; < x3.  Legal values for LOBE are 'left for the p1,p2 lobe and
;;; 'right for the p2,p3 lobe.
(define (parabola-stats lobe x1 y1 x2 y2 x3 y3)
  (receive
   (area peak-x peak-y)
   (origin-parabola-stats lobe
    (- x1 x2) (- y1 y2) (- x3 x2) (- y3 y2))
   (values area (+ peak-x x2) (+ peak-y y2))))

;;; The stats of the given lobe of the parabola that goes through
;;; (x1, y1), (0, 0), and (x3, y3), under the condition that x1 < 0 <
;;; x3.  (The p1, 0, p3 angle may be acute.)
(define (origin-parabola-stats lobe x1 y1 x3 y3)
  (let* ((slope1 (/ y1 x1))
	 (slope3 (/ y3 x3))
	 (slope-max (max (abs slope1) (abs slope3))))
    (if (< slope-max 2/3)
	(obtuse-parabola-stats lobe x1 y1 x3 y3)
	(receive
	 (area peak-x peak-y)
	 (obtuse-parabola-stats lobe
	  x1 (/ y1 (* 2 slope-max)) x3 (/ y3 (* 2 slope-max)))
	 (values (* area 2 slope-max) peak-x (* peak-y 2 slope-max))))))

;;; The stats of the given lobe of the parabola that goes through
;;; (x1, y1), (0, 0), and (x3, y3), under the condition that x1 < 0 <
;;; x3 and the p1, 0, p3 angle is obtuse.
(define (obtuse-parabola-stats lobe x1 y1 x3 y3)
  (if (eq? lobe 'left)
      (obtuse-parabola-left-stats x1 y1 x3 y3)
      (obtuse-parabola-right-stats x1 y1 x3 y3)))


;;; The stats of the p1,0 lobe of the parabola that goes through (x1,
;;; y1), (0, 0), and (x3, y3), under the condition that x1 < 0 < x3
;;; and the p1, 0, p3 angle is obtuse.
(define (obtuse-parabola-left-stats x1 y1 x3 y3)
  (let* ((dist-p1 (sqrt (+ (square x1) (square y1))))
	 (cos-p1-angle (/ x1 dist-p1))
	 (sin-p1-angle (/ y1 dist-p1))
	 ;; rotation by -(angle - pi)
	 (cos-rot-angle (- cos-p1-angle))
	 (sin-rot-angle sin-p1-angle)
	 (new-x3 (- (* cos-rot-angle x3) (* sin-rot-angle y3)))
	 (new-y3 (+ (* sin-rot-angle x3) (* cos-rot-angle y3))))
    (receive
     (area peak-x peak-y)
     (grounded-parabola-stats (- dist-p1) new-x3 new-y3)
     ;; rotation back, by +(angle - pi)
     (let ((old-peak-x (+ (* cos-rot-angle peak-x) (* sin-rot-angle peak-y)))
	   (old-peak-y (+ (* -1 sin-rot-angle peak-x) (* cos-rot-angle peak-y))))
       (values area old-peak-x old-peak-y)))))

;;; The stats of the 0,p3 lobe of the parabola that goes through (x1,
;;; y1), (0, 0), and (x3, y3), under the condition that x1 < 0 < x3
;;; and the p1, 0, p3 angle is obtuse.
(define (obtuse-parabola-right-stats x1 y1 x3 y3)
  (let* ((dist-p3 (sqrt (+ (square x3) (square y3))))
	 (cos-p3-angle (/ x3 dist-p3))
	 (sin-p3-angle (/ y3 dist-p3))
	 ;; rotation by -angle
	 (new-x1 (+ (* cos-p3-angle x1) (* sin-p3-angle y1)))
	 (new-y1 (+ (* -1 sin-p3-angle x1) (* cos-p3-angle y1))))
    (receive
     (area peak-x peak-y)
     (grounded-parabola-stats dist-p3 new-x1 new-y1)
     ;; rotation back, by +angle
     (let ((old-peak-x (- (* cos-p3-angle peak-x) (* sin-p3-angle peak-y)))
	   (old-peak-y (+ (* sin-p3-angle peak-x) (* cos-p3-angle peak-y))))
       (values area old-peak-x old-peak-y)))))

;;; The stats of the p1,0 lobe of the parabola that goes through
;;; (x1, 0), (0, 0), and (x3, y), with the condition that x1 < 0 < x3
;;; or x3 < 0 < x1.  The stats of such a parabola are the area between
;;; the lobe and the axis, and the x and y coordinates of the point
;;; that takes the largest possible triangular bite out of it
;;; (returned in that order).
(define (grounded-parabola-stats x1 x3 y)
  (let* ((base (abs x1))
	 (peak-x (/ x1 2))
	 (peak-y (/ (* -1 y peak-x peak-x)
		    (* (- x3 x1) x3)))
	 (area (abs (* 2/3 base peak-y))))
    (values area peak-x peak-y)))

