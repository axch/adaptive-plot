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
  (plot-initialize! plot)
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
    (plot-initialize! new-plot)
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

(define (ensure-x-point-known! plot x-value)
  (if (> (length (range-query-2d (plot-known-points plot) x-value x-value)) 0)
      'ok
      (plot-learn-point! plot x-value ((plot-point-source plot) x-value))))

(define (plot-learn-point! plot x y)
  (set-plot-known-points!
   plot (point-set-insert (plot-known-points plot) x y)))

(define (plot-initialize! plot)
  (if (default-object? (plot-xlow plot))
      (ensure-x-point-known! plot -1.)
      (ensure-x-point-known! plot (plot-xlow plot)))
  (if (default-object? (plot-xhigh plot))
      (ensure-x-point-known! plot 1.)
      (ensure-x-point-known! plot (plot-xhigh plot))))

(define (plot-uniform-refine! plot)
  (plot-initialize! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (plot-dim-refine! plot (desired-separation xlow xhigh (plot-xresolution plot)) car)
      (plot-redraw! plot)
      (call-with-values (lambda () (plot-dimensions plot))
	(lambda (xlow xhigh ylow yhigh)
	  (plot-dim-refine! plot (desired-separation ylow yhigh (plot-yresolution plot)) cdr)
	  (plot-redraw! plot))))))

(define (plot-adaptive-refine! plot)
  (plot-initialize! plot)
  (receive
   (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation xlow xhigh 10) car)
   (plot-redraw! plot)
   (plot-line-interpolate! plot)
   (plot-redraw! plot)))

(define plot-refine! plot-adaptive-refine!)

(define (plot-line-interpolation-map plot big-lobe?)
  (let* ((relevant-points (plot-relevant-points plot))
	 (relevant-segments
	  (map make-segment (cons #f relevant-points) relevant-points 
	       (cdr relevant-points) (append (cddr relevant-points) (list #f))))
	 (meaningful-segments (filter big-lobe? relevant-segments)))
    (alist->wt-tree segment-wt-tree-type
		    (map (lambda (seg)
			   (cons seg #f))
			 meaningful-segments))))

(define (plot-update-interpolation-map tree new-p big-lobe?)
  (define (assert thing)
    (if (not thing)
	(error "Assertion failed")))
  (let ((biggest-segment (wt-tree/min tree)))
    (assert (< (car (segment-p1 biggest-segment)) (car new-p)
	       (car (segment-p2 biggest-segment))))
    (let* ((candidates (split-segment biggest-segment new-p))
	   (insertees (filter big-lobe? candidates)))
      (let loop ((tree (wt-tree/delete-min tree))
		 (insertees insertees))
	(if (null? insertees)
	    tree
	    (loop (wt-tree/add tree (car insertees) #t) (cdr insertees)))))))

(define (plot-line-interpolate! plot)
  (let ((big-lobe? (plot-big-lobe plot)))
    (let loop ((to-do (plot-line-interpolation-map plot big-lobe?)))
      (if (wt-tree/empty? to-do)
	  'ok
	  (let* ((new-x (segment-candidate-x (wt-tree/min to-do)))
		 (new-y ((plot-point-source plot) new-x)))
	    (pp (wt-tree/min to-do))
	    (plot-learn-point! plot new-x new-y)
	    (loop (plot-update-interpolation-map
		   to-do (cons new-x new-y) big-lobe?)))))))

(define (plot-data-area plot)
  (receive
   (xlow xhigh ylow yhigh)
   (plot-dimensions plot)
   (* (- xhigh xlow) (- yhigh ylow))))

(define (plot-screen-area plot)
  (* (plot-xresolution plot) (plot-yresolution plot)))

(define (plot-invisible-area plot)
  (/ (plot-data-area plot) (plot-screen-area plot)))

(define (plot-big-lobe plot)
  (lambda (seg)
    (> (segment-candidate-area seg) (plot-invisible-area plot))))

;;; This is the segment between p1 and p2 of the four points p0, p1,
;;; p2, and p3.  x0 < x1 < x2 < x3.  At most one of p0 or p3 may be
;;; #f.  There are one or two lobes over this segment, one defined by
;;; p0 and one by p3 (each if present).  The candidate-area is the
;;; area of the larger, and the candidate-x is the x-coordinate of the
;;; point that would take the biggest triangular bite out of it.  An
;;; invariant of the geomtery is that x1 < candidate-x < x2.  If
;;; candidate-area is 0, candidate-x may be #f.
(define-structure (segment safe-accessors (constructor %make-segment))
  p0 p1 p2 p3 candidate-x candidate-area)

(define (make-segment p0 p1 p2 p3)
  (receive
   (p0-area p0-x p0-y)
   (if p0
       (parabola-stats 'right
        (car p0) (cdr p0) (car p1) (cdr p1) (car p2) (cdr p2))
       (values 0 #f #f))
   (receive
    (p3-area p3-x p3-y)
    (if p3
	(parabola-stats 'left
         (car p1) (cdr p1) (car p2) (cdr p2) (car p3) (cdr p3))
	(values 0 #f #f))
    (if (> p0-area p3-area)
	(%make-segment p0 p1 p2 p3 p0-x p0-area)
	(%make-segment p0 p1 p2 p3 p3-x p3-area)))))

(define (segment-ignorable-< seg1 seg2)
  (cond ((< (segment-candidate-area seg1) (segment-candidate-area seg2))
	 #f)
	((> (segment-candidate-area seg1) (segment-candidate-area seg2))
	 #t)
	(else
	 (> (hash seg1) (hash seg2)))))

(define segment-wt-tree-type (make-wt-tree-type segment-ignorable-<))

;;; Assuming x0 < x1 < new-x < x2 < x3, produces a list of two new
;;; segments: x0 < x1 < new-x < x2 and x1 < new-x < x2 < x3.
(define (split-segment segment new-p)
  (let ((p0 (segment-p0 segment))
	(p1 (segment-p1 segment))
	(p2 (segment-p2 segment))
	(p3 (segment-p3 segment)))
    (list (make-segment p0 p1 new-p p2)
	  (make-segment p1 new-p p2 p3))))
