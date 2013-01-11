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

;;;; Examples

;;; Type these expressions at the read-eval-print loop (after loading
;;; Adaptive Plot) to see what it does.

;; You should see a black on white plot of a sinusoid, with nicely
;; rounded extrema.
(plot sin -10 10)

;; You should see a gnuplot window of the same sinusoidal plot (with
;; more frippery).  The extrema should be much smoother than asking
;; gnuplot to plot sin(x) itself.
(plot-gnuplot! last-plot)
