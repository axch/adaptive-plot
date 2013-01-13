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

;; Prevent accidental guessing of locations of geometric features.
(define (offset f #!optional amount)
  (if (default-object? amount)
      (set! amount (/ 1 (sqrt 2))))
  (lambda (x)
    (f (- x amount))))

(define (discrepancy f-area)
  (lambda (x1.y1 x2.y2)
    (let* ((x1 (car x1.y1))
           (y1 (cdr x1.y1))
           (x2 (car x2.y2))
           (y2 (cdr x2.y2))
           (area (f-area x1 x2))
           (base (abs (- x2 x1)))
           (avg-height (/ (+ y1 y2) 2))
           (trapezoid (* base avg-height)))
      (abs (- area trapezoid)))))

(define (discrepancies f-area points)
  (map (discrepancy f-area) points (cdr points)))

(define (sum numbers)
  (apply + numbers))

(define (maximum numbers)
  (apply max numbers))

(define (antiderivative->integrator f-antiderivative)
  (define (integrator x1 x2)
    (if (> x1 x2)
        (integrator x2 x1)
        (- (f-antiderivative x2)
           (f-antiderivative x1))))
  integrator)

;; Here ylow and yhigh are expected to be the true maxima of f in the
;; given x range.
(define (plot-stats f anti-f xlow xhigh ylow yhigh)
  (let* ((plot (plot-quietly f xlow xhigh))
         (points (plot-known-points plot))
         (disc (discrepancies (antiderivative->integrator anti-f) points))
         (data-area (* (abs (- xhigh xlow)) (abs (- yhigh ylow))))
         (pixels (plot-pixels plot))
         (data-per-pixel (/ data-area pixels))
         (pixel-disc (map (lambda (d) (/ d data-per-pixel)) disc)))
    (values
     (length points)
     (maximum pixel-disc)
     (sum pixel-disc))))

(define-syntax check-plot-stats
  (syntax-rules ()
    ((_ (spec-item ...) (pts max tot))
     (receive (num-points max-disc tot-disc)
       (plot-stats spec-item ...)
       (check (= pts num-points))
       (check (generic-match max max-disc))
       (check (generic-match tot tot-disc))))))
