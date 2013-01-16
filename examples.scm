;;; This file is part of Adaptive Plot, a library for intelligently
;;; plotting functions from the MIT Scheme REPL.
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

;;;; Examples

;;; Type these expressions at the read-eval-print loop (after loading
;;; Adaptive Plot) to see what it does.

;; You should see a black on white plot of a sinusoid, with nicely
;; rounded extrema.
(plot sin -10 10)

;; You should see a gnuplot window of the same sinusoidal plot (with
;; more frippery).  The extrema should be much smoother than asking
;; gnuplot to plot sin(x) itself.
(plot-gnu! (out))

;; Sharp extremum at 1/(sqrt 2); the weird constant is there to
;; prevent the plotter from guessing the minimum by accident.
;; Gnuplot natively cuts the extremum off.
(plot (lambda (x) (abs (- x (/ 1 (sqrt 2))))) -1 1)

(plot-gnu! (out))

;; Smooth extremum at zero; trace out the discontinuities at -1 and 1
(define (quartic-mess x)
  (cond ((< x -1) 0.95)
        ((> x 1) 0.95)
        (else (expt x 4))))

(plot quartic-mess -3 3)

(plot-gnu! (out))

;; Still works with a larger range; plot it without showing the Scheme
;; plot window just for kicks.
(plot quartic-mess -10 10 'invisibly)

(plot-gnu! (out))

;; Offset so it can't guess the key points
(plot (lambda (x) (quartic-mess (+ x (sqrt 2)))) -3 3)

(plot-gnu! (out))

;; Gnuplot not only fails to follow the heights of the peaks, it
;; misses around half of them completely.
(plot (lambda (x) (sin (* 200 x))) -1 1)

(plot-gnu! (out) '(commanding "with lines title \"sin(200x)\""))

;; The following function is chosen to have a relatively sharp
;; transition (with interesting behavior) and relatively large flat
;; regions.  The idea is to make it look like it might be hard to
;; compute, so plot points are at a premium; and to make it have very
;; local geometry of interest, so that placing them well is valuable.
(define (example-f x)
  (cond ((< x 0) (+ 0.1 (* 5 (exp (* 100 x)))))
        (else (+ 1.1 (* 4 (exp (* 100 (- x))))))))
;; (gnuplot (offset example-f) -1 2 '(x-uniformly 28)) is duly terrible
