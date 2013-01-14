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

;;;; Facade: common patterns made simple

(define (plot f xlow xhigh . adverbs)
  (define scheme-window-wanted?
    (not (eq? (last
               (filter (lambda (a) (or (eq? a 'quietly) (eq? a 'loudly)))
                       (cons 'loudly adverbs)))
              'quietly)))
  (let ((new-plot (start-plot f xlow xhigh)))
    (if scheme-window-wanted?
        (plot-draw! new-plot))
    (apply plot-refine! new-plot adverbs)
    new-plot))

(define (gnuplot f xlow xhigh . adverbs)
  (apply plot-gnu!
         (plot-stop-drawing!
          (apply plot f xlow xhigh (cons 'quietly adverbs))) adverbs))

;;;; Interactive manipulation

;; (plot ... 'quietly)

;; Also plot-draw! and plot-stop-drawing!

(define (plot-gnu! plot . adverbs)
  (apply gnuplot-alist (plot-relevant-points-alist plot) adverbs)
  plot)

(define (plot-zoom! plot #!optional new-xlow new-xhigh new-ylow new-yhigh)
  (plot-resize! plot new-xlow new-xhigh new-ylow new-yhigh)
  (plot-refine! plot))

(define (plot-zoom-x! plot #!optional new-xlow new-xhigh)
  (plot-resize-x! plot new-xlow new-xhigh)
  (plot-refine! plot))

(define (plot-zoom-y! plot #!optional new-ylow new-yhigh)
  (plot-resize-y! plot new-ylow new-yhigh)
  (plot-refine! plot))

(define (plot-resolve! plot xres yres)
  (plot-new-resolution! plot xres yres)
  (plot-refine! plot))

;;;; No autorefinement

(define (start-plot f xlow xhigh)
  (letrec ((new-plot
	    (make-plot 1200 960
	     (lambda (x)
	       (let ((answer (f x)))
		 (plot-draw-point! new-plot x answer)
		 answer)))))
    (plot-resize-x! new-plot xlow xhigh)
    new-plot))

(define (plot-refine! plot . adverbs)
  (interpret-refinement-adverb
   (last (filter refinement-adverb? (cons 'adaptively adverbs)))
   plot))

(define refinement-functions
  `((adaptively  . ,plot-adaptive-refine!)
    (uniformly   . ,plot-uniform-refine!)
    (x-uniformly . ,plot-uniform-refine-x!)
    (y-uniformly . ,plot-uniform-refine-y!)))

(define (refinement-adverb? adverb)
  (or (memq adverb (map car refinement-functions))
      (and (pair? adverb)
           (memq (car adverb)
                 (map car refinement-functions)))))

(define (interpret-refinement-adverb adverb plot)
  (define (refinement-function name)
    ;; Never errors out because I filter the adverbs
    (cdr (assq name refinement-functions)))
  (if (not (pair? adverb))
      (interpret-refinement-adverb (list adverb) plot)
      (apply (refinement-function (car adverb))
             plot (cdr adverb))))
