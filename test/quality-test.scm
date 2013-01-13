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

(in-test-group
 quality

 (define-each-test
   (receive
    (num-points max-disc tot-disc)
    (plot-stats sin (lambda (x) (- (cos x))) -10 10 -1 1)
    (check (= 328 num-points))
    (check (generic-match 0.86642 max-disc))
    (check (generic-match 113.88 tot-disc)))

   (check-plot-stats 
    ((offset abs) (offset abs-anti) -1 1 0 2)
    ;; The plot of abs would only be discrepant at the kink.
    (40 0.63403 0.63403))))
