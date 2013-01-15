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

(load "../load")

;; Prevent accidental guessing of locations of geometric features.
(define (offset f #!optional amount)
  (if (default-object? amount)
      (set! amount (/ 1 (sqrt 2))))
  (lambda (x)
    (f (- x amount))))

(define (example-f x)
  (cond ((< x 0) (+ 0.1 (* 5 (exp (* 100 x)))))
        (else (+ 1.1 (* 4 (exp (* 100 (- x))))))))

(gnuplot (offset example-f) -1 2 '(adaptively-with 40)
         '(prefixing "set term png size 640,480; set output \"better.png\"")
         '(commanding "title \"Adaptive placement of the same 40 evaluations"))

(%exit 0)
