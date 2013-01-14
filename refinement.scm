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

;;;; Refinement of plots until visual continuity
;;;; (up to the resolution held in the plot object).

;;; Uniform refinement

(define (plot-uniform-refine! plot)
  (plot-uniform-refine-x! plot)
  (plot-uniform-refine-y! plot))

(define (plot-uniform-refine-x! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation xlow xhigh (plot-xresolution plot)) car)
   (plot-sync-window! plot)))

(define (plot-uniform-refine-y! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation ylow yhigh (plot-yresolution plot)) cdr)
   (plot-sync-window! plot)))

(define (desired-separation low high desired-resolution)
  (/ (abs (- high low)) desired-resolution))

(define (plot-dim-refine! plot desired-separation dimension)
  (let* ((relevant-points (plot-relevant-points-alist plot))
	 (points-to-query
	  (needed-queries relevant-points desired-separation dimension))
	 (results (map (plot-point-source plot) points-to-query)))
    (plot-learn-point-set! plot (alist->point-set (map cons points-to-query results)))))

(define (needed-queries known-points desired-separation dimension)
  (define (needed-interpoint-queries low-point high-point)
    (let* ((d-low (dimension low-point))
	   (d-high (dimension high-point))
	   (d-distance (abs (- d-high d-low))))
      (if (> d-distance desired-separation)
	  (let* ((x-low (car low-point))
		 (x-high (car high-point))
		 (x-distance (abs (- x-high x-low)))
		 (num-steps (inexact->exact (floor (/ d-distance desired-separation))))
		 (step-size (/ x-distance (+ num-steps 1))))
	    (iota num-steps (+ x-low step-size) step-size))
	  '())))
  (append-map needed-interpoint-queries known-points (cdr known-points)))

;;; Adaptive refinement by parabolic interpolation

(define (plot-adaptive-refine! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation xlow xhigh 10) car)
   (plot-sync-window! plot)
   (plot-parabolic-interpolate! plot)
   (plot-sync-window! plot)))

;;; Iteratively refine the piecewise linear approximation that is the
;;; given plot by adding points in the places where it makes the
;;; biggest mistakes relative to a locally quadratic approximation of
;;; the function.
(define (plot-parabolic-interpolate! plot)
  (interpolate-approximation
   (plot-relevant-points-alist plot)
   (plot-watched-f plot)
   (plot-small-lobe plot)))

(define (plot-watched-f plot)
  (lambda (x)
    (let ((y ((plot-point-source plot) x)))
      (plot-learn-point! plot x y)
      y)))

(define (plot-small-lobe plot)
  (define (plot-data-area plot)
    (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
      (* (- xhigh xlow) (- yhigh ylow))))
  (define (plot-invisible-area plot)
    (/ (plot-data-area plot) (plot-pixels plot)))
  (lambda (seg)
    (< (segment-lobe-area seg) (plot-invisible-area plot))))
