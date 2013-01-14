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

(define (plot-uniform-refine! plot #!optional xres yres)
  (if (default-object? yres)
      (set! yres xres))
  (plot-uniform-refine-x! plot xres)
  (plot-uniform-refine-y! plot yres))

(define (plot-uniform-refine-x! plot #!optional xres)
  (plot-ensure-initialized! plot)
  (if (default-object? xres)
      (set! xres (plot-xresolution plot)))
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation xlow xhigh xres) car)
   (plot-sync-window! plot)))

(define (plot-uniform-refine-y! plot #!optional yres)
  (plot-ensure-initialized! plot)
  (if (default-object? yres)
      (set! yres (plot-yresolution plot)))
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation ylow yhigh yres) cdr)
   (plot-sync-window! plot)))

(define (desired-separation low high desired-resolution)
  (/ (abs (- high low)) desired-resolution))

;; Attempt to break each segment of the plot into the smallest number
;; of uniformly sized subsegments whose length along the given
;; dimension does not exceed the desired separation.  This always
;; works along the x dimension, and works along the y dimension for
;; functions that are lines in this region.
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
    (<= (segment-lobe-area seg) (plot-invisible-area plot))))
