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

;;;; Evaluating piecewise linear approximations with locally quadratic
;;;; approximations.

;;; Any two consecutive segments of a piecewise linear curve can be
;;; evaluated by comparing them to the parabola defined by those three
;;; points.  The quality of a segment is given by how small a lobe the
;;; parabola in question makes over it.  Any one segment can be
;;; evaluated in one of two ways, using one of its two neighbors.  I
;;; take the more critical of the two evaluations.

;;; A `segment' record represents the segment between p1 and p2 of the
;;; four points p0, p1, p2, and p3.  x0 < x1 < x2 < x3.  At most one
;;; of p0 or p3 may be #f, indicating that the piecewise linear curve
;;; ends with this segment.  There are one or two lobes over this
;;; segment, one defined by p0 and one by p3 (if present).  The
;;; lobe-area is the area of the larger, and the candidate-x is the
;;; x-coordinate of the point that would take the biggest triangular
;;; bite out of it.  An invariant of the geomtery is that
;;; x1 < candidate-x < x2.  If lobe-area is 0, candidate-x may be #f.
(define-structure (segment safe-accessors (constructor %make-segment))
  p0 p1 p2 p3 candidate-x lobe-area)

(define (make-segment p0 p1 p2 p3)
  (receive
   (p0-area p0-x p0-y)
   (if p0
       (parabola-stats 'right
        (car p0) (cdr p0) (car p1) (cdr p1) (car p2) (cdr p2))
       (values 0 #f #f))
   (receive
    (p3-area p3-x p3-y)
    (if p3
	(parabola-stats 'left
         (car p1) (cdr p1) (car p2) (cdr p2) (car p3) (cdr p3))
	(values 0 #f #f))
    (if (> p0-area p3-area)
	(%make-segment p0 p1 p2 p3 p0-x p0-area)
	(%make-segment p0 p1 p2 p3 p3-x p3-area)))))

;;; Assuming x0 < x1 < new-x < x2 < x3, produces a list of two new
;;; segments: x0 < x1 < new-x < x2 and x1 < new-x < x2 < x3.
(define (split-segment segment new-p)
  (let ((p0 (segment-p0 segment))
	(p1 (segment-p1 segment))
	(p2 (segment-p2 segment))
	(p3 (segment-p3 segment)))
    (list (make-segment p0 p1 new-p p2)
	  (make-segment p1 new-p p2 p3))))

;;; Segments are ordered by quality: good segments have small lobe
;;; areas.
(define (segment-quality-< seg1 seg2)
  (cond ((< (segment-lobe-area seg1) (segment-lobe-area seg2))
	 #f)
	((> (segment-lobe-area seg1) (segment-lobe-area seg2))
	 #t)
	(else ; Arbitrary stable order
	 (> (hash seg1) (hash seg2)))))

(define segment-wt-tree-type (make-wt-tree-type segment-quality-<))

;;; Given a piecewise linear curve as a list of points, make a
;;; priority queue of the lowest-quality segments in that curve.  If
;;; supplied, the `drop?' argument is a predicate that picks out those
;;; segments that are so good that they need not be added to the
;;; queue.
(define (interpolation-queue points #!optional drop?)
  (if (default-object? drop?)
      (set! drop? (lambda (x) #f)))
  (let* ((segments
	  (map make-segment (cons #f points) points
	       (cdr points) (append (cddr points) (list #f))))
	 (meaningful-segments (remove drop? segments)))
    (alist->wt-tree segment-wt-tree-type
		    (map (lambda (seg)
			   (cons seg #f))
			 meaningful-segments))))

;;; Given an interpolation queue for a piecewise linear curve, and a
;;; point that breaks the worst segment therein, produce a new queue
;;; for the curve formed by splitting that segment with that point.  If
;;; supplied, the `drop?' argument is a predicate that picks out those
;;; segments that are so good that they need not be added to the
;;; new queue.  The existing queue elements are not filtered.
;;; TODO `drop?' should really be part of the interpolation queue
;;; datastructure itself, to avoid synchronization issues.
(define (update-interpolation-queue tree new-p #!optional drop?)
  (define (assert thing)
    (if (not thing)
	(error "Assertion failed")))
  (define (wt-tree/insert-alist tree items)
    (let loop ((tree tree)
               (items items))
      (if (null? items)
          tree
          (loop (wt-tree/add tree (caar items) (cdar items))
                (cdr items)))))
  (if (default-object? drop?)
      (set! drop? (lambda (x) #f)))
  (let ((biggest-segment (wt-tree/min tree)))
    (assert (< (car (segment-p1 biggest-segment)) (car new-p)
	       (car (segment-p2 biggest-segment))))
    (let* ((candidates (split-segment biggest-segment new-p))
	   (insertees (remove drop? candidates)))
      (wt-tree/insert-alist (wt-tree/delete-min tree)
                            (map (lambda (seg)
                                   (cons seg #f))
                                 insertees)))))

;;; Iteratively refine the piecewise linear approximation of the given
;;; function `f' given by the given `points', splitting the worst
;;; segment first.  Returns nothing useful; the communication
;;; mechanism is the x values that `f' is called with.  If supplied,
;;; the `drop?' argument is a predicate that picks out those segments
;;; that are so good they need not be further refined.  Interpolation
;;; proceeds until all remaining segments satisfy `drop?'.  If `drop?'
;;; is not supplied, interpolation will proceed forever (unless `f'
;;; escapes with a nonlocal control transfer of some kind).
(define (interpolate-approximation points f #!optional drop?)
  (let loop ((to-do (interpolation-queue points drop?)))
    (if (wt-tree/empty? to-do)
        'ok
        (let* ((new-x (segment-candidate-x (wt-tree/min to-do)))
               (new-y (f new-x)))
          (loop (update-interpolation-queue
                 to-do (cons new-x new-y) drop?))))))
