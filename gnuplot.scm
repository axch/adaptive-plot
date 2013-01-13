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

;;;; Gnuplot output

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

(define (gnuplot-alist alist #!optional gnuplot-extra gnuplot-prefix)
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (gnuplot-write-alist alist pathname)
     (let ((command (string-append
		     "gnuplot -p -e \'"
                     (if (default-object? gnuplot-prefix)
                         ""
                         (string-append gnuplot-prefix "; "))
                     "plot \""
		     (->namestring pathname)
		     "\""
		     (if (default-object? gnuplot-extra)
			 " with lines"
			 (string-append " " gnuplot-extra))
		     "'")))
       (display command)
       (newline)
       (run-shell-command command)))))

(define (gnuplot-histogram-alist alist #!optional data-name binsize)
  ;; TODO Abstract the commonalities among these two
  (define (compute-bin-size numbers)
    (let* ((sorted (sort numbers <))
           (minimum (car sorted))
           (maximum (last sorted)))
      (/ (- maximum minimum) 200)))
  (if (default-object? binsize)
      (set! binsize (compute-bin-size (map car alist))))
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (gnuplot-write-alist alist pathname)
     (let ((command (string-append
		     "gnuplot -p -e \'"
                     "binwidth=" (number->string binsize) "; "
                     "bin(x,width)=width*floor(x/width)+(binwidth+1)/2; "
                     "set boxwidth binwidth; "
                     "set style fill solid; "
                     "plot \"" (->namestring pathname) "\" "
                     "using (bin($1,binwidth)):($2/binwidth) smooth freq with boxes "
                     (if (default-object? data-name) "" (string-append "title \"" data-name "\" "))
		     "'")))
       (display command)
       (newline)
       (run-shell-command command)))))
