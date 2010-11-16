(declare (usual-integrations))

;;;; Plotting

;;; I'm fed up with Mechanics's default plotting facilities, so in a
;;; fit of fed-up-itude I started writing my own.  This is not quite
;;; finished.

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

(define (plot-draw! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (let ((xresolution (plot-xresolution plot))
	    (yresolution (plot-yresolution plot)))
	(plot-close-window! plot)
	(set-plot-window! plot (frame xlow xhigh ylow yhigh xresolution yresolution))
	(plot-redraw! plot)))))

(define (plot-redraw! plot)
  (graphics-clear (plot-window plot))
  (let ((relevant-points (plot-relevant-points plot)))
    (for-each (lambda (x.y)
		(plot-point (plot-window plot) (car x.y) (cdr x.y)))
	      relevant-points)
    (pp (list "X range was" (xmin relevant-points) (xmax relevant-points)))
    (pp (list "Y range was" (ymin relevant-points) (ymax relevant-points)))))

(define (plot-refine! plot)
  (let ((xlow  (plot-xlow plot))
	(xhigh (plot-xhigh plot))
	(xresolution (plot-xresolution plot)))
    (let* ((x-points (iota xresolution xlow (/ (- xhigh xlow) xresolution)))
	   (y-points (map (plot-point-source plot) x-points))
	   (new-points (map cons x-points y-points)))
      (set-plot-known-points!
       plot (point-set-insert (plot-known-points plot) new-points)))))

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
  (if (graphics-device? (plot-window plot))
      (graphics-close (plot-window plot))))

(define (plot f xlow xhigh)
  (let ((new-plot (make-plot 800 800 f)))
    (plot-resize-x! new-plot xlow xhigh)
    (plot-refine! new-plot)
    (plot-draw! new-plot)
    (set! last-plot new-plot)
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
  (append point-list new-point-list))

(define (empty-point-set)
  '())
