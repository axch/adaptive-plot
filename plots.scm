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

;;;; Plots

;;; A plot is a thing that knows what it's trying to plot, the x-y box
;;; in which it's trying to plot it (which may adapt to the data), the
;;; resolution within that box that it's trying to achieve, and the
;;; points it has so far.  A plot may also know a Scheme window into
;;; which to draw points as they appear, and updated line-drawings
;;; from time to time.

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
  window
  dimensions-cache)

(define (make-plot xresolution yresolution point-source)
  (%make-plot
   xresolution yresolution #!default #!default #!default #!default
   (empty-point-set) point-source #f #f))

(define (plot-clear-dimensions-cache! plot)
  (set-plot-dimensions-cache! plot #f)
  plot)

(define (plot-pixels plot)
  (* (plot-xresolution plot) (plot-yresolution plot)))

(define (plot-resize! plot #!optional new-xlow new-xhigh new-ylow new-yhigh)
  (plot-resize-x! plot new-xlow new-xhigh)
  (plot-resize-y! plot new-ylow new-yhigh))

(define (plot-resize-x! plot #!optional new-xlow new-xhigh)
  (plot-clear-dimensions-cache! plot)
  (set-plot-xlow!  plot new-xlow)
  (set-plot-xhigh! plot new-xhigh)
  plot)

(define (plot-resize-y! plot #!optional new-ylow new-yhigh)
  (plot-clear-dimensions-cache! plot)
  (set-plot-ylow!  plot new-ylow)
  (set-plot-yhigh! plot new-yhigh)
  plot)

(define (plot-new-resolution! plot xres yres)
  (set-plot-xresolution! plot xres)
  (set-plot-yresolution! plot yres)
  plot)

(define (plot-compute-dimensions plot)
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

(define plot-dimensions
  (slot-memoize plot-compute-dimensions
                plot-dimensions-cache set-plot-dimensions-cache! #f))

(define (plot-known-points-alist plot)
  (point-set->alist (plot-known-points plot)))

(define (plot-relevant-points plot)
  (range-query-2d (plot-known-points plot)
   (plot-xlow plot) (plot-xhigh plot) (plot-ylow plot) (plot-yhigh plot)))

(define (plot-relevant-points-alist plot)
  (point-set->alist (plot-relevant-points plot)))

(define (plot-learn-point! plot x y)
  (set-plot-known-points!
   plot (point-set-insert (plot-known-points plot) x y))
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (if ((in-box? xlow xhigh ylow yhigh) (cons x y))
       plot
       (plot-clear-dimensions-cache! plot))))

(define (plot-learn-point-set! plot points)
  (set-plot-known-points!
   plot (point-set-union (plot-known-points plot) points))
  ;; TODO Test whether all the new points are in the old dimensions?
  (plot-clear-dimensions-cache! plot))

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
      (ensure-x-point-known! (plot-xhigh plot)))
  plot)
