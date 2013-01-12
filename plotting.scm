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


(define-structure
  (plot safe-accessors (constructor %make-plot))
  xresolution
  yresolution
  xlow
  xhigh
  ylow
  yhigh
  known-points
  point-source
  window)

(define (make-plot xresolution yresolution point-source)
  (%make-plot
   xresolution yresolution #!default #!default #!default #!default
   (empty-point-set) point-source #f))

(define (plot-draw-point! plot x y)
  (if (graphics-device? (plot-window plot))
      (receive
       (xlow xhigh ylow yhigh) (plot-dimensions plot)
       (and (<= xlow y xhigh)
	    (<= ylow y yhigh)
	    (%plot-point (plot-window plot) x y)))))

(define (plot-draw! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (let ((xresolution (plot-xresolution plot))
	    (yresolution (plot-yresolution plot)))
	(plot-close-window! plot)
	(set-plot-window! plot (new-plot-window xlow xhigh ylow yhigh 960 1200))
	(plot-redraw! plot)))))

(define (plot-redraw! plot)
  (graphics-clear (plot-window plot))
  (plot-ensure-initialized! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
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
	(pp (list "Y range was" (ymin relevant-points) (ymax relevant-points)))))))

(define (plot-resize! plot #!optional new-xlow new-xhigh new-ylow new-yhigh)
  (set-plot-xlow!  plot new-xlow)
  (set-plot-xhigh! plot new-xhigh)
  (set-plot-ylow!  plot new-ylow)
  (set-plot-yhigh! plot new-yhigh))

(define (plot-resize-x! plot #!optional new-xlow new-xhigh)
  (set-plot-xlow!  plot new-xlow)
  (set-plot-xhigh! plot new-xhigh))

(define (plot-resize-y! plot #!optional new-ylow new-yhigh)
  (set-plot-ylow!  plot new-ylow)
  (set-plot-yhigh! plot new-yhigh))

(define (plot-dimensions plot)
  (let ((relevant-points (plot-relevant-points plot)))
    (define (... thing compute)
      (if (default-object? thing)
	  (compute relevant-points)
	  thing))
    (let ((xlow  (... (plot-xlow plot)  xmin))
	  (xhigh (... (plot-xhigh plot) xmax))
	  (ylow  (... (plot-ylow plot)  ymin))
	  (yhigh (... (plot-yhigh plot) ymax)))
      (values xlow xhigh ylow yhigh))))

(define (plot-relevant-points plot)
  (range-query-2d (plot-known-points plot)
   (plot-xlow plot) (plot-xhigh plot) (plot-ylow plot) (plot-yhigh plot)))

(define (plot-close-window! plot)
  (if (plot? plot)
      (if (graphics-device? (plot-window plot))
	  (graphics-close (plot-window plot)))))

(define (plot f xlow xhigh)
  (letrec ((new-plot
	    (make-plot 960 1200
	     (lambda (x)
	       (let ((answer (f x)))
		 (plot-draw-point! new-plot x answer)
		 answer)))))
    (set! last-plot new-plot)
    (plot-resize-x! new-plot xlow xhigh)
    (plot-ensure-initialized! new-plot)
    (plot-draw! new-plot)
    (plot-refine! new-plot)
    'ok))

(define last-plot #f)

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

;;; Utilities

(define (plot-learn-point! plot x y)
  (set-plot-known-points!
   plot (point-set-insert (plot-known-points plot) x y)))

(define (plot-ensure-initialized! plot)
  (define (ensure-x-point-known! x-value)
    (if (> (length (range-query-2d (plot-known-points plot) x-value x-value)) 0)
        'ok
        (plot-learn-point! plot x-value ((plot-point-source plot) x-value))))
  (if (default-object? (plot-xlow plot))
      (ensure-x-point-known! -1.)
      (ensure-x-point-known! (plot-xlow plot)))
  (if (default-object? (plot-xhigh plot))
      (ensure-x-point-known! 1.)
      (ensure-x-point-known! (plot-xhigh plot))))

;;; Uniform refinement

(define (plot-uniform-refine! plot)
  (plot-ensure-initialized! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (plot-dim-refine! plot (desired-separation xlow xhigh (plot-xresolution plot)) car)
      (plot-redraw! plot)
      (call-with-values (lambda () (plot-dimensions plot))
	(lambda (xlow xhigh ylow yhigh)
	  (plot-dim-refine! plot (desired-separation ylow yhigh (plot-yresolution plot)) cdr)
	  (plot-redraw! plot))))))

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
  (receive
   (xlow xhigh ylow yhigh) (plot-dimensions plot)
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
    (receive
        (xlow xhigh ylow yhigh)
      (plot-dimensions plot)
      (* (- xhigh xlow) (- yhigh ylow))))
  (define (plot-screen-area plot)
    (* (plot-xresolution plot) (plot-yresolution plot)))
  (define (plot-invisible-area plot)
    (/ (plot-data-area plot) (plot-screen-area plot)))
  (lambda (seg)
    (< (segment-lobe-area seg) (plot-invisible-area plot))))

(define plot-refine! plot-adaptive-refine!)
