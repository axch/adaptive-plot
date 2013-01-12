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
  (letrec ((new-plot
	    (make-plot 960 1200
	     (lambda (x)
	       (let ((answer (f x)))
		 (plot-draw-point! new-plot x answer)
		 answer)))))
    (set! last-plot new-plot)
    (plot-resize-x! new-plot xlow xhigh)
    (plot-draw! new-plot)
    (plot-refine! new-plot)
    new-plot))

;;;; Interactive manipulation

(define last-plot #f)

(define (plot-draw! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (let ((xresolution (plot-xresolution plot))
         (yresolution (plot-yresolution plot)))
     (plot-close-window! plot)
     (set-plot-window! plot (new-plot-window xlow xhigh ylow yhigh 960 1200))
     (plot-redraw! plot))))

(define (plot-close-window! plot)
  (if (plot? plot)
      (if (graphics-device? (plot-window plot))
	  (graphics-close (plot-window plot)))))

(define (plot-gnuplot! plot #!optional gnuplot-extra gnuplot-prefix)
  (gnuplot-plot-alist (plot-relevant-points plot) gnuplot-extra gnuplot-prefix))

(define plot-refine! plot-adaptive-refine!)
