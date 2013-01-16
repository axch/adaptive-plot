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
;; prevent the plotter from guessing the minimum by accident.  It
;; still finds it pretty well.
(plot (lambda (x) (abs (- x (/ 1 (sqrt 2))))) -1 1)

;; Compare the same task with 100 uniformly spaced points:
(plot (lambda (x) (abs (- x (/ 1 (sqrt 2))))) -1 1 '(x-uniformly 98))

;; This function has a smooth extremum at zero and discontinuities at
;; -1 and 1.
(define (quartic-mess x)
  (cond ((< x -1) 0.95)
        ((> x 1) 0.95)
        (else (expt x 4))))

;; The plotter traces out both the extremum and the discontinuities.
(plot quartic-mess -3 3)

;; We can plot the same thing straight to gnuplot output.
(gnuplot quartic-mess -3 3)

;; Still works with a larger range; plot it without showing the Scheme
;; plot window just for kicks.
(plot quartic-mess -10 10 'invisibly)

;; There it is!
(plot-gnu! (out))

;; Compare this to plotting the same thing natively in Gnuplot.
(plot-gnu! (lambda (x) (sin (* 200 x))) -1 1 'visibly
           '(commanding "with lines title \"sin(200x)\""))
;; Using only 100 evenly spaced points not only fails to follow the
;; the extrema all the way, it misses around half of them completely.
