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

(declare (usual-integrations))

(define (in-box? #!optional xlow xhigh ylow yhigh)
  (lambda (x.y)
    (and (or (default-object? xlow)
             (<= xlow (car x.y)))
         (or (default-object? xhigh)
             (<= (car x.y) xhigh))
         (or (default-object? ylow)
             (<= ylow (cdr x.y)))
         (or (default-object? yhigh)
             (<= (cdr x.y) yhigh)))))

(define (slot-memoize f read-slot write-slot sentinel)
  (lambda (x)
    (let ((current (read-slot x)))
      (if (eq? sentinel current)
          (let ((answer (f x)))
            (write-slot x answer)
            answer)
          current))))

;; A "lax alist" is a list whose pairs are treated as alist elements,
;; but which is allowed to have non-pairs also (which are ignored).
(define (lax-alist-lookup alist item default #!optional =)
  (let ((binding (assoc item (filter pair? alist) =)))
    (if binding
        ;; I really want to be looking up from two-element lists
        ;; rather than pairs, so this does not iterpret proper alists.
        (cadr binding)
        default)))
