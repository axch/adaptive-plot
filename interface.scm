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

;;;; Facade: common patterns made simple

(define (plot f xlow xhigh)
  (let ((new-plot (start-plot f xlow xhigh)))
    (plot-draw! new-plot)
    (plot-refine! new-plot)
    new-plot))

(define (gnuplot f xlow xhigh #!optional gnuplot-extra gnuplot-prefix)
  (plot-gnu! (plot-quietly f xlow xhigh) gnuplot-extra gnuplot-prefix))

;;;; Interactive manipulation

(define last-plot #f)

(define (plot-quietly f xlow xhigh)
  (let ((new-plot (start-plot f xlow xhigh)))
    (plot-refine! new-plot)
    new-plot))

(define (plot-draw! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (let ((xresolution (plot-xresolution plot))
         (yresolution (plot-yresolution plot)))
     (plot-stop-drawing! plot)
     (set-plot-window! plot (new-plot-window xlow xhigh ylow yhigh 960 1200))
     (plot-sync-window! plot))))

(define (plot-stop-drawing! plot)
  (if (plot? plot)
      (if (graphics-device? (plot-window plot))
	  (begin (graphics-close (plot-window plot))
                 (set-plot-window! plot #f)))))

(define (plot-gnu! plot #!optional gnuplot-extra gnuplot-prefix)
  (gnuplot-plot-alist (plot-relevant-points-alist plot) gnuplot-extra gnuplot-prefix)
  plot)

(define (plot-zoom! plot #optional new-xlow new-xhigh new-ylow new-yhigh)
  (plot-resize! plot new-xlow new-xhigh new-ylow new-yhigh)
  (plot-refine! plot))

(define (plot-zoom-x! plot #optional new-xlow new-xhigh)
  (plot-resize-x! plot new-xlow new-xhigh)
  (plot-refine! plot))

(define (plot-zoom-y! plot #optional new-ylow new-yhigh)
  (plot-resize-y! plot new-ylow new-yhigh)
  (plot-refine! plot))

(define (plot-resolve! plot xres yres)
  (plot-new-resolution! plot xres yres)
  (plot-refine! plot))

;;;; No autorefinement

(define (start-plot f xlow xhigh)
  (letrec ((new-plot
	    (make-plot 960 1200
	     (lambda (x)
	       (let ((answer (f x)))
		 (plot-draw-point! new-plot x answer)
		 answer)))))
    (set! last-plot new-plot)
    (plot-resize-x! new-plot xlow xhigh)
    new-plot))

(define plot-refine! plot-adaptive-refine!)
