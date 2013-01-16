;;; This file is part of Adaptive Plot, a library for intelligently
;;; plotting functions from the MIT Scheme REPL.
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

;;;; Gnuplot output of alist data

;;; This is just writing files to disk and issuing shell commands.
;;; This file intentionally does not depend on any of the rest of
;;; Adaptive Plot, so can be lifted and used elsewhere.

(load-option 'synchronous-subprocess)

(define (gnuplot-write-alist alist filename)
  (with-output-to-file filename
    (lambda ()
      (for-each
       (lambda (x.y)
	 (write (exact->inexact (car x.y)))
	 (display " ")
	 (write (exact->inexact (cdr x.y)))
	 (newline))
       alist))))

(define (gnuplot-alist alist . adverbs)
  (let ((gnuplot-extra (lax-alist-lookup adverbs 'commanding ""))
        (gnuplot-prefix (lax-alist-lookup adverbs 'prefixing "")))
    (call-with-temporary-file-pathname
     (lambda (pathname)
       (gnuplot-write-alist alist pathname)
       (let ((command (string-append
                       "gnuplot -p -e \'"
                       "set style data lines; "
                       "set key noautotitles; "
                       gnuplot-prefix
                       "; plot \""
                       (->namestring pathname)
                       "\" "
                       gnuplot-extra
                       "'")))
         (display command)
         (newline)
         (run-shell-command command))))))

;; A "lax alist" is a list whose pairs are treated as alist elements,
;; but which is allowed to have non-pairs also (which are ignored).
(define (lax-alist-lookup alist item default #!optional =)
  (let ((binding (assoc item (filter pair? alist) =)))
    (if binding
        ;; I really want to be looking up from two-element lists
        ;; rather than pairs, so this does not iterpret proper alists.
        (cadr binding)
        default)))
