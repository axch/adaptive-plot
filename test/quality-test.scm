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

(define (abs-anti x)
  (/ (if (< x 0)
         (- (* x x))
         (* x x))
     2))

;; Smooth extremum at zero; discontinuities at -1 and 1
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
    (plot-stats sin (lambda (x) (- (cos x))) -10 10 -1 1))

   (generic-match
    ;; The plot of abs would only be discrepant at the kink, which is
    ;; contained in just one segment.
    #(40 0.63403 0.63403)
    (plot-stats (offset abs) (offset abs-anti) -1 1 0 2))

   (generic-match
    ;; I am disappointed that there is a discrepancy in excess of 1
    ;; pixel somewhere; this means the parabolic approximation wasn't
    ;; aggressive enough.
    #(184 1.3498 34.412)
    (plot-stats (offset quartic-mess) (offset quartic-mess-anti) -2 2 0 1))
   ))
