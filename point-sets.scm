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

;;;; Point sets and range queries

;;; This is the abstraction that can be replaced with a proper range
;;; tree if desired.

(define (xmin point-list)
  (apply min (map car point-list)))

(define (xmax point-list)
  (apply max (map car point-list)))

(define (ymin point-list)
  (apply min (map cdr point-list)))

(define (ymax point-list)
  (apply max (map cdr point-list)))

(define (range-query-2d point-list #!optional xlow xhigh ylow yhigh)
  (filter (in-box? xlow xhigh ylow yhigh) point-list))

(define (point-set-union point-list new-point-list)
  (append new-point-list point-list))

(define (point-set-insert point-list x y)
  (point-set-union point-list (alist->point-set `((,x . ,y)))))

(define (alist->point-set l) l)

(define (point-set->alist s)
  (sort s (lambda (pt1 pt2) (< (car pt1) (car pt2)))))

(define (empty-point-set)
  '())
