(declare (usual-integrations))

;;;; Plotting

;;; I'm fed up with Mechanics's default plotting facilities, so in a
;;; fit of fed-up-itude I started writing my own.  This is not quite
;;; finished.

(define (plot f xlow xhigh)
  (let* ((resolution 800)
	 (x-points (iota resolution xlow (/ (- xhigh xlow) resolution)))
	 (y-points (map f x-points))
	 (yhigh (apply max y-points))
	 (ylow (apply min y-points))
	 (window (frame xlow xhigh ylow yhigh 800 800)))
    (pp `("X range was" ,xlow ,xhigh))
    (pp `("Y range was" ,ylow ,yhigh))
    (for-each (lambda (x y)
		(plot-point window x y))
	      x-points y-points)
    (set! last-plot window)))

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
  (%make-plot xresolution yresolution 0 0 0 0 (empty-point-set) point-source #f))

(define (plot-draw! plot)
  (let ((xlow  (plot-xlow plot))
	(xhigh (plot-xhigh plot))
	(ylow  (plot-ylow plot))
	(yhigh (plot-yhigh plot))
	(xresolution (plot-xresolution plot))
	(yresolution (plot-yresolution plot)))
    (if (graphics-device? (plot-window plot))
	(graphics-close (plot-window plot)))
    (set-plot-window! plot (frame xlow xhigh ylow yhigh xresolution yresolution))
    (plot-redraw! plot)))

(define (plot-redraw! plot)
  (graphics-clear (plot-window plot))
  (let ((xlow  (plot-xlow plot))
	(xhigh (plot-xhigh plot))
	(ylow  (plot-ylow plot))
	(yhigh (plot-yhigh plot)))
    (let ((relevant-points (range-query-2d xlow xhigh ylow yhigh (plot-known-points plot))))
      (for-each (lambda (x.y)
		  (plot-point (plot-window plot) (car x.y) (cdr x.y)))
		relevant-points)
      (pp (list "X range was" (xmin relevant-points) (xmax relevant-points)))
      (pp (list "Y range was" (ymin relevant-points) (ymax relevant-points))))))

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
  (let* ((relevant-points (plot-known-points plot))
	 (new-xlow  (if (default-object? new-xlow) (xmin relevant-points) new-xlow))
	 (new-xhigh (if (default-object? new-xhigh) (xmax relevant-points) new-xhigh))
	 (new-ylow  (if (default-object? new-ylow) (ymin relevant-points) new-ylow))
	 (new-yhigh (if (default-object? new-yhigh) (ymax relevant-points) new-yhigh)))
    (set-plot-xlow!  plot new-xlow)
    (set-plot-xhigh! plot new-xhigh)
    (set-plot-ylow!  plot new-ylow)
    (set-plot-yhigh! plot new-yhigh)))

(define (plot-resize-x! plot #!optional new-xlow new-xhigh)
  (let* ((ylow  (plot-ylow plot))
	 (yhigh (plot-yhigh plot))
	 (relevant-points (range-query-y ylow yhigh (plot-known-points plot)))
	 (new-xlow  (if (default-object? new-xlow) (xmin relevant-points) new-xlow))
	 (new-xhigh (if (default-object? new-xhigh) (xmax relevant-points) new-xhigh)))
    (set-plot-xlow!  plot new-xlow)
    (set-plot-xhigh! plot new-xhigh)))

(define (plot-resize-y! plot #!optional new-ylow new-yhigh)
  (let* ((xlow  (plot-xlow plot))
	 (xhigh (plot-xhigh plot))
	 (relevant-points (range-query-x xlow xhigh (plot-known-points plot)))
	 (new-ylow  (if (default-object? new-ylow) (ymin relevant-points) new-ylow))
	 (new-yhigh (if (default-object? new-yhigh) (ymax relevant-points) new-yhigh)))
    (set-plot-ylow!  plot new-ylow)
    (set-plot-yhigh! plot new-yhigh)))

(define (plot f xlow xhigh)
  (let ((new-plot (make-plot 80 800 f)))
    (plot-resize-x! new-plot xlow xhigh)
    (plot-refine! new-plot)
    (plot-resize-y! new-plot)
    (plot-draw! new-plot)
    (set! last-plot new-plot)))

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

(define (range-query-x xlow xhigh point-list)
  (filter (lambda (x.y)
	    (<= xlow (car x.y) xhigh))
	  point-list))

(define (range-query-y ylow yhigh point-list)
  (filter (lambda (x.y)
	    (<= ylow (cdr x.y) yhigh))
	  point-list))

(define (range-query-2d xlow xhigh ylow yhigh point-list)
  (filter (lambda (x.y)
	    (and (<= xlow (car x.y) xhigh)
		 (<= ylow (cdr x.y) yhigh)))
	  point-list))

(define (point-set-insert point-list new-point-list)
  (append point-list new-point-list))

(define (empty-point-set)
  '())
