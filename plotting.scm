(declare (usual-integrations))

(load-option 'synchronous-subprocess)

;;;; Plotting

;;; I'm fed up with Mechanics's default plotting facilities, so in a
;;; fit of fed-up-itude I started writing my own.  This is not quite
;;; finished.

;;; TODO Make it work in MIT Scheme without mechanics
;;; TODO Generalize autorefinement to non-curves?  (e.g. chaotic trajectories)
;;; TODO Generalize to point sources that are not functions of the x dimension?


(define-structure
  (plot safe-accessors (constructor %make-plot))
  xresolution
  yresolution
  xlow
  xhigh
  ylow
  yhigh
  known-points
  point-source
  window)

(define (make-plot xresolution yresolution point-source)
  (%make-plot
   xresolution yresolution #!default #!default #!default #!default
   (empty-point-set) point-source #f))

(define (plot-draw-point! plot x y)
  (if (graphics-device? (plot-window plot))
      (plot-point (plot-window plot) x y)))

(define (plot-draw! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (let ((xresolution (plot-xresolution plot))
	    (yresolution (plot-yresolution plot)))
	(plot-close-window! plot)
	(set-plot-window! plot (frame xlow xhigh ylow yhigh 960 1200))
	(plot-redraw! plot)))))

(define (plot-redraw! plot)
  (graphics-clear (plot-window plot))
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (graphics-set-coordinate-limits (plot-window plot) xlow ylow xhigh yhigh)
      (let ((relevant-points (plot-relevant-points plot)))
	(for-each (lambda (x.y)
		    (plot-point (plot-window plot) (car x.y) (cdr x.y)))
		  relevant-points)
	(pp (list "X range was" (xmin relevant-points) (xmax relevant-points)))
	(pp (list "Y range was" (ymin relevant-points) (ymax relevant-points)))))))

(define (plot-resize! plot #!optional new-xlow new-xhigh new-ylow new-yhigh)
  (set-plot-xlow!  plot new-xlow)
  (set-plot-xhigh! plot new-xhigh)
  (set-plot-ylow!  plot new-ylow)
  (set-plot-yhigh! plot new-yhigh))

(define (plot-resize-x! plot #!optional new-xlow new-xhigh)
  (set-plot-xlow!  plot new-xlow)
  (set-plot-xhigh! plot new-xhigh))

(define (plot-resize-y! plot #!optional new-ylow new-yhigh)
  (set-plot-ylow!  plot new-ylow)
  (set-plot-yhigh! plot new-yhigh))

(define (plot-dimensions plot)
  (let ((relevant-points (plot-relevant-points plot)))
    (define (... thing compute)
      (if (default-object? thing)
	  (compute relevant-points)
	  thing))
    (let ((xlow  (... (plot-xlow plot)  xmin))
	  (xhigh (... (plot-xhigh plot) xmax))
	  (ylow  (... (plot-ylow plot)  ymin))
	  (yhigh (... (plot-yhigh plot) ymax)))
      (values xlow xhigh ylow yhigh))))

(define (plot-relevant-points plot)
  (range-query-2d (plot-known-points plot)
   (plot-xlow plot) (plot-xhigh plot) (plot-ylow plot) (plot-yhigh plot)))

(define (plot-close-window! plot)
  (if (plot? plot)
      (if (graphics-device? (plot-window plot))
	  (graphics-close (plot-window plot)))))

(define (plot f xlow xhigh)
  (letrec ((new-plot (make-plot 40 40
				(lambda (x)
				  (let ((answer (f x)))
				    (plot-draw-point! new-plot x answer)
				    answer)))))
    (set! last-plot new-plot)
    (plot-resize-x! new-plot xlow xhigh)
    (plot-initialize! new-plot)
    (plot-draw! new-plot)
    (plot-refine! new-plot)
    'ok))

(define last-plot #f)

(define (plotting-first-input operation)
  (let ((done-plotting? #f))
    (lambda (f)
      (let ((answer (operation f)))
	(if (not done-plotting?)
	    (begin (plot f -3. 4.)
		   (pp answer)
		   (set! done-plotting? #t)))
	answer))))

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
  (filter (lambda (x.y)
	    (and (or (default-object? xlow)
		     (<= xlow (car x.y)))
		 (or (default-object? xhigh)
		     (<= (car x.y) xhigh))
		 (or (default-object? ylow)
		     (<= ylow (cdr x.y)))
		 (or (default-object? yhigh)
		     (<= (cdr x.y) yhigh))))
	  point-list))

(define (point-set-insert point-list new-point-list)
  (sort
   (append point-list new-point-list)
   (lambda (pt1 pt2)
     (< (car pt1) (car pt2)))))

(define (empty-point-set)
  '())

;;;; Refinement of plots until visual continuity

(define (needed-queries known-points desired-separation dimension)
  (define (needed-interpoint-queries low-point high-point)
    (let* ((d-low (dimension low-point))
	   (d-high (dimension high-point))
	   (d-distance (abs (- d-high d-low))))
      (if (> d-distance desired-separation)
	  (let* ((x-low (car low-point))
		 (x-high (car high-point))
		 (x-distance (abs (- x-high x-low)))
		 (num-steps (inexact->exact (floor (/ d-distance desired-separation))))
		 (step-size (/ x-distance (+ num-steps 1))))
	    (iota num-steps (+ x-low step-size) step-size))
	  '())))
  (append-map needed-interpoint-queries known-points (cdr known-points)))

(define (desired-separation low high desired-resolution)
  (/ (abs (- high low)) desired-resolution))

(define (plot-dim-refine! plot desired-separation dimension)
  (let* ((relevant-points (plot-relevant-points plot))
	 (points-to-query 
	  (needed-queries relevant-points desired-separation dimension))
	 (results (map (plot-point-source plot) points-to-query)))
    (set-plot-known-points!
     plot (point-set-insert (plot-known-points plot) (map cons points-to-query results)))))

(define (ensure-x-point-known! plot x-value)
  (if (> (length (range-query-2d (plot-known-points plot) x-value x-value)) 0)
      'ok
      (set-plot-known-points!
       plot (point-set-insert (plot-known-points plot)
			      `((,x-value . ,((plot-point-source plot) x-value)))))))

(define (plot-initialize! plot)
  (if (default-object? (plot-xlow plot))
      (ensure-x-point-known! plot -1.)
      (ensure-x-point-known! plot (plot-xlow plot)))
  (if (default-object? (plot-xhigh plot))
      (ensure-x-point-known! plot 1.)
      (ensure-x-point-known! plot (plot-xhigh plot))))

(define (plot-refine! plot)
  (plot-initialize! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (plot-dim-refine! plot (desired-separation xlow xhigh (plot-xresolution plot)) car)
      (plot-redraw! plot)
      (call-with-values (lambda () (plot-dimensions plot))
	(lambda (xlow xhigh ylow yhigh)
	  (plot-dim-refine! plot (desired-separation ylow yhigh (plot-yresolution plot)) cdr)
	  (plot-redraw! plot))))))

;;;; Gnuplot output

(define (plot-gnuplot! plot #!optional gnuplot-extra)
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (with-output-to-file pathname
      (lambda ()
	(for-each
	 (lambda (x.y)
	   (write (car x.y))
	   (display " ")
	   (write (cdr x.y))
	   (newline))
	 (plot-relevant-points plot))))
     (let ((command (string-append
		     "gnuplot -p -e \'plot \""
		     (->namestring pathname)
		     "\""
		     (if (default-object? gnuplot-extra)
			 ""
			 (string-append " " gnuplot-extra))
		     "'")))
       (display command)
       (run-shell-command command)))))
