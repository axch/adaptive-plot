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

;;;; Plotting

(define (self-relatively thunk)
  (let ((place (ignore-errors current-load-pathname)))
    (if (pathname? place)
	(with-working-directory-pathname
	 (directory-namestring place)
	 thunk)
	(thunk))))

(define (load-relative filename)
  (self-relatively (lambda () (load filename))))

(load-option 'synchronous-subprocess)
(load-option 'wt-tree)

(load-relative "auto-compilation")
(load-relative-compiled "point-sets")
(load-relative-compiled "plots")
(load-relative-compiled "windowing")
(load-relative-compiled "drawing")
(load-relative-compiled "parabolas")
(load-relative-compiled "locally-quadratic")
(load-relative-compiled "refinement")
(load-relative-compiled "gnuplot")
(load-relative-compiled "interface")

;;; TODO Generalize autorefinement to non-curves?  (e.g. chaotic trajectories)
;;; TODO Generalize to point sources that are not functions of the x dimension?
;;; TODO Generalize to multiple point sources
;;; TODO A way to avoid autorefining randomized functions for too long
;;; TODO Make gnuplot output the default?  Find a way to give the repl
;;;   back while keeping a gnuplot window open?

(define (plotting-first-input operation)
  (let ((done-plotting? #f))
    (lambda (f)
      (let ((answer (operation f)))
	(if (not done-plotting?)
	    (begin (plot f -3. 4.)
		   (pp answer)
		   (set! done-plotting? #t)))
	answer))))
