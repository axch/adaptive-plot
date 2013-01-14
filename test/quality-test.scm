;;; This file is part of Adaptive Plot, a library for plotting
;;; functions from the MIT Scheme REPL
;;; Copyright (C) 2013 Alexey Radul
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

(define (abs-anti x)
  (/ (if (< x 0)
         (- (* x x))
         (* x x))
     2))

(define (step x)
  (if (< x 0)
      -1
      1))

;; This function has a smooth (4th-order) extremum at zero and
;; discontinuities at -1 and 1.
(define (quartic-mess x)
  (cond ((< x -1) 0.95)
        ((> x 1) 0.95)
        (else (expt x 4))))

(define (quartic-mess-anti x)
  (cond ((< x -1) (+ (* 0.95 x) 0.95 -1/5))
        ((> x 1)  (+ (* 0.95 x) 1/5 -0.95))
        (else (/ (expt x 5) 5))))

(in-test-group
 quality

 (define-each-check
   (generic-match
    #(328 0.86642 113.88)
    (plotting-stats sin (lambda (x) (- (cos x))) -10 10 -1 1))

   (generic-match
    ;; The plot of abs would only be discrepant at the kink, which is
    ;; contained in just one segment.
    #(40 0.63403 0.63403)
    (plotting-stats (offset abs) (offset abs-anti) -1 1 0 2))

   (generic-match
    ;; I am disappointed that there is a discrepancy in excess of 1
    ;; pixel somewhere; this means the parabolic approximation wasn't
    ;; aggressive enough.
    #(184 1.3498 34.412)
    (plotting-stats (offset quartic-mess) (offset quartic-mess-anti) -2 2 0 1))

   (generic-match
    ;; Step seems to be harder than abs...
    #(71 1.9542 1.9542)
    (plotting-stats (offset step) (offset abs) -1 1 -1 1))

   (generic-match
    ;; ... unless, of course, the geometry is outside the x-clip...
    #(12 0 0)
    (plotting-stats (offset step 2) (offset abs 2) -1 1 -1 1))

   (generic-match
    ;; ... in which case all straight lines are equally easy.
    #(12 0 0)
    (plotting-stats (offset abs 2) (offset abs-anti 2) -1 1 1 3))

   (generic-match
    ;; The quartic mess also gets easier if the geometry is mostly
    ;; outside the clip zone.
    #(84 0.67413 9.9951)
    (plotting-stats (offset quartic-mess 2.51) (offset quartic-mess-anti 2.51) -2 2 0 1))

   )

 (define-test (low-resolution-plots-need-fewer-points)
   (interaction
    (define a-plot (start-plot (offset quartic-mess) -2 2))
    (plot-resolve! a-plot 100 100)
    (plot-stats a-plot (offset quartic-mess-anti) -2 2 0 1)
    ;; Though it seems that in this case something makes the relative
    ;; quality of the plot worse.
    (produces #(46 3.0484 12.002))
    ;; It also turns out that at this resolution it misses the spikes
    ;; at 1 and -1.
    (receive (xlow xhigh ylow yhigh) (plot-dimensions a-plot) yhigh)
    (produces 0.95)

    ;; Resetting the resolution of the plot allows the process to
    ;; continue
    (plot-resolve! a-plot 960 1200)
    (plot-stats a-plot (offset quartic-mess-anti) -2 2 0 1)
    ;; And the initial 46 points are not wasted (in fact, in this case
    ;; the plotting uses fewer points than from plotting at the high
    ;; resolution directly.
    (produces #(175 1.0653 35.823))
    ;; With the resolution restored, the spikes are recovered.
    (receive (xlow xhigh ylow yhigh) (plot-dimensions a-plot) yhigh)
    (produces 0.99973)))

 (define-test (plotting-a-cusp)
   (interaction
    (define (cusp x)
      (sqrt (abs x)))
    (define (cusp-anti x)
      (if (< x 0)
          (- (/ (expt (- x) 3/2) 3/2))
          (/ (expt x 3/2) 3/2)))
    (define a-plot (plot-quietly (offset cusp) -1 1))
    (plot-stats a-plot (offset cusp-anti) -1 1 0 1.4)
    (produces #(117 1.1329 37.847))
    ;; Unfortunately, even at this resolution, it visibly misses the
    ;; trough of the cusp (which is zero, of course); but Gnuplot
    ;; native does around 7x worse.
    (receive (xlow xhigh ylow yhigh) (plot-dimensions a-plot) ylow)
    (produces 0.01291)))

 (define-test (sin-200x)
   (define (f x) (sin (* 200 x)))
   (define (f-anti x) (/ (- (cos (* 200 x))) 200))
   ;; This takes a lot of points because there are a lot of extrema to
   ;; cover (128 or so); but the maximum discrepancy remains under
   ;; control.
   (check
    (generic-match
     #(2749 1.1949 666.56)
     (plotting-stats f f-anti -1 1 -1 1))))
 )
