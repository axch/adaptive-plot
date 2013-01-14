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

;;;; Drawing

(define *scheme-plot-window-x-res* 1024)
(define *scheme-plot-window-y-res* 768)

(define (plot-draw! plot)
  (plot-stop-drawing! plot)
  (plot-ensure-initialized! plot)
  (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
   (set-plot-window! plot
    (new-plot-window xlow xhigh ylow yhigh
                     *scheme-plot-window-x-res*
                     *scheme-plot-window-y-res*))
   (plot-sync-window! plot)))

(define (plot-stop-drawing! plot)
  (if (graphics-device? (plot-window plot))
      (graphics-close (plot-window plot)))
  (set-plot-window! plot #f))

(define (plot-sync-window! plot)
  (if (graphics-device? (plot-window plot))
      (begin
       (graphics-clear (plot-window plot))
       (plot-ensure-initialized! plot)
       (receive (xlow xhigh ylow yhigh) (plot-dimensions plot)
        (graphics-set-coordinate-limits (plot-window plot) xlow ylow xhigh yhigh)
        (graphics-set-clip-rectangle (plot-window plot) xlow ylow xhigh yhigh)
        (let ((relevant-points (plot-relevant-points-alist plot)))
          #;
          (for-each
           (lambda (x.y)
             (%plot-point (plot-window plot) (car x.y) (cdr x.y)))
           relevant-points)
          (for-each
           (lambda (x.y1 x.y2)
             (%plot-line (plot-window plot)
                         (car x.y1) (cdr x.y1) (car x.y2) (cdr x.y2)))
           relevant-points
           (cdr relevant-points)))))))

(define (plot-draw-point! plot x y)
  (if (graphics-device? (plot-window plot))
      (receive
       (xlow xhigh ylow yhigh) (plot-dimensions plot)
       (and (<= xlow y xhigh)
	    (<= ylow y yhigh)
	    (%plot-point (plot-window plot) x y)))))
