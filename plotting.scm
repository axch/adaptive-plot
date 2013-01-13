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

(load-option 'synchronous-subprocess)
(load-option 'wt-tree)

;;;; Plotting

;;; I'm fed up with Mechanics's default plotting facilities, so in a
;;; fit of fed-up-itude I started writing my own.  This is not quite
;;; finished.

;;; TODO Generalize autorefinement to non-curves?  (e.g. chaotic trajectories)
;;; TODO Generalize to point sources that are not functions of the x dimension?
;;; TODO Generalize to multiple point sources
;;; TODO Clean up the code
;;; TODO A way to avoid autorefining randomized functions for too long
;;; TODO A way to specify refining by just densing up the x axis
;;;   without adaptation (good for randomized functions)
;;; TODO Make gnuplot output the default?  Find a way to give the repl
;;;   back while keeping a gnuplot window open?

(define (plot-draw-point! plot x y)
  (if (graphics-device? (plot-window plot))
      (receive
       (xlow xhigh ylow yhigh) (plot-dimensions plot)
       (and (<= xlow y xhigh)
	    (<= ylow y yhigh)
	    (%plot-point (plot-window plot) x y)))))

(define (plot-redraw! plot)
  (if (graphics-device? (plot-window plot))
      (begin
       (graphics-clear (plot-window plot))
       (plot-ensure-initialized! plot)
       (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
        (graphics-set-coordinate-limits (plot-window plot) xlow ylow xhigh yhigh)
        (graphics-set-clip-rectangle (plot-window plot) xlow ylow xhigh yhigh)
        (let ((relevant-points (plot-relevant-points plot)))
          #;
          (for-each (lambda (x.y)
          (%plot-point (plot-window plot) (car x.y) (cdr x.y)))
          relevant-points)

          (for-each (lambda (x.y1 x.y2)
                      (%plot-line (plot-window plot) (car x.y1) (cdr x.y1) (car x.y2) (cdr x.y2)))
                    relevant-points
                    (cdr relevant-points))
          (pp (list "X range was" (xmin relevant-points) (xmax relevant-points)))
          (pp (list "Y range was" (ymin relevant-points) (ymax relevant-points))))))))

(define (plotting-first-input operation)
  (let ((done-plotting? #f))
    (lambda (f)
      (let ((answer (operation f)))
	(if (not done-plotting?)
	    (begin (plot f -3. 4.)
		   (pp answer)
		   (set! done-plotting? #t)))
	answer))))

;;;; Refinement of plots until visual continuity

;;; Uniform refinement

(define (plot-uniform-refine! plot)
  (plot-uniform-refine-x! plot)
  (plot-uniform-refine-y! plot))

(define (plot-uniform-refine-x! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation xlow xhigh (plot-xresolution plot)) car)
   (plot-redraw! plot)))

(define (plot-uniform-refine-y! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation ylow yhigh (plot-yresolution plot)) cdr)
   (plot-redraw! plot)))

(define (desired-separation low high desired-resolution)
  (/ (abs (- high low)) desired-resolution))

(define (plot-dim-refine! plot desired-separation dimension)
  (let* ((relevant-points (plot-relevant-points plot))
	 (points-to-query
	  (needed-queries relevant-points desired-separation dimension))
	 (results (map (plot-point-source plot) points-to-query)))
    (set-plot-known-points!
     plot (point-set-union
           (plot-known-points plot)
           (alist->point-set (map cons points-to-query results))))))

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
   (plot-redraw! plot)
   (plot-parabolic-interpolate! plot)
   (plot-redraw! plot)))

;;; Iteratively refine the piecewise linear approximation that is the
;;; given plot by adding points in the places where it makes the
;;; biggest mistakes relative to a locally quadratic approximation of
;;; the function.
(define (plot-parabolic-interpolate! plot)
  (interpolate-approximation
   (plot-relevant-points plot)
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
  (define (plot-screen-area plot)
    (* (plot-xresolution plot) (plot-yresolution plot)))
  (define (plot-invisible-area plot)
    (/ (plot-data-area plot) (plot-screen-area plot)))
  (lambda (seg)
    (< (segment-lobe-area seg) (plot-invisible-area plot))))
