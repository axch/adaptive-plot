(declare (usual-integrations))

(load-option 'synchronous-subprocess)

;;;; Plotting

;;; I'm fed up with Mechanics's default plotting facilities, so in a
;;; fit of fed-up-itude I started writing my own.  This is not quite
;;; finished.

;;; TODO Generalize autorefinement to non-curves?  (e.g. chaotic trajectories)
;;; TODO Generalize to point sources that are not functions of the x dimension?
;;; TODO Generalize to multiple point sources
;;; TODO Clean up the code
;;; TODO Fix "Floating-point overflow signalled by x-graphics-draw-point."
;;; TODO Autorefinement to smooth out corners in line plots?


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
      (%plot-point (plot-window plot) x y)))

(define (plot-draw! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (let ((xresolution (plot-xresolution plot))
	    (yresolution (plot-yresolution plot)))
	(plot-close-window! plot)
	(set-plot-window! plot (new-plot-window xlow xhigh ylow yhigh 960 1200))
	(plot-redraw! plot)))))

(define (plot-redraw! plot)
  (graphics-clear (plot-window plot))
  (plot-initialize! plot)
  (call-with-values (lambda () (plot-dimensions plot))
    (lambda (xlow xhigh ylow yhigh)
      (graphics-set-coordinate-limits (plot-window plot) xlow ylow xhigh yhigh)
      (let ((relevant-points (plot-relevant-points plot)))
	#;
	(for-each (lambda (x.y)
		    (%plot-point (plot-window plot) (car x.y) (cdr x.y)))
		  relevant-points)
	(for-each (lambda (x.y1 x.y2)
		    (%plot-line (plot-window plot) (car x.y1) (cdr x.y1) (car x.y2) (cdr x.y2)))
		  relevant-points
		  (cdr relevant-points))
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

(define (plot-dump! plot filename)
  (with-output-to-file filename
    (lambda ()
      (for-each
       (lambda (x.y)
	 (write (exact->inexact (car x.y)))
	 (display " ")
	 (write (exact->inexact (cdr x.y)))
	 (newline))
       (plot-relevant-points plot)))))

(define (plot-gnuplot! plot #!optional gnuplot-extra)
  (call-with-temporary-file-pathname
   (lambda (pathname)
     (plot-dump! plot pathname)
     (let ((command (string-append
		     "gnuplot -p -e \'plot \""
		     (->namestring pathname)
		     "\""
		     (if (default-object? gnuplot-extra)
			 ""
			 (string-append " " gnuplot-extra))
		     "'")))
       (display command)
       (newline)
       (run-shell-command command)))))


;;;; Making windows

;;; This is copied from ScmUtils' FRAME and its immediate
;;; dependencies.  The copy is to avoid depending on ScmUtils; I also
;;; renamed the procedures to avoid conflicting with ScmUtils.
(define (new-plot-window xmin xmax ymin ymax
			 frame-width frame-height #!optional display)
  (if (not (and (integer? frame-width) (> frame-width 0)
		(integer? frame-height) (> frame-height 0)))
      (error "Bad frame width or height"))
  (let ((window (%plot-make-window frame-width frame-height -10 0 display)))
    (graphics-set-coordinate-limits window xmin ymin xmax ymax)
    (graphics-set-clip-rectangle window xmin ymin xmax ymax)
    (graphics-clear window)
    window))

(define (%plot-make-window width height x y #!optional display)
  (let ((window
         (let ((name (graphics-type-name (graphics-type #f))))
           (cond ((eq? name 'x)
		  (if (default-object? display)
		      (set! display #f))
		  (%plot-make-window/x11 width height x y display))
                 ((eq? name 'win32)
		  (if (not (default-object? display))
		      (error "No remote Win32 display"))
		  (%plot-make-window/win32 width height x y))
                 ((eq? name 'os/2)
		  (if (not (default-object? display))
		      (error "No remote OS/2 display"))
		  (%plot-make-window/os2 width height x y))
                 (else (error "Unsupported graphics type:" name))))))
    (graphics-set-coordinate-limits window 0 (- (- height 1)) (- width 1) 0)
    (graphics-operation window 'set-background-color "white")
    (graphics-operation window 'set-foreground-color "black")
    window))

(define (%plot-make-window/x11 width height x y #!optional display)
  (if (default-object? display)
      (set! display #f))
  (let ((window (make-graphics-device 'x display (x-geometry-string x y width height) true)))
    (if (not (string-ci=? "MacOSX" microcode-id/operating-system-variant))
        (x-graphics/disable-keyboard-focus window))
    (x-graphics/set-input-hint window false)
    (x-graphics/map-window window)
    (x-graphics/flush window)
    window))

(define (%plot-make-window/win32 width height x y)
  (let ((window (make-graphics-device 'win32 width height 'grayscale-128)))
    (graphics-operation window 'move-window x y)
    window))

(define (%plot-make-window/os2 width height x y)
  (let ((window (make-graphics-device 'os/2 width height)))
    (call-with-values
     (lambda ()
       (graphics-operation window 'desktop-size))
     (lambda (dx dy)
       (call-with-values
        (lambda ()
          (graphics-operation window 'window-frame-size))
        (lambda (fx fy)
          (graphics-operation window 'set-window-position x (- dy (+ y fy)))))))
    window))

(define (%plot-point window x y)
  (graphics-draw-point window (exact->inexact x) (exact->inexact y)))

(define (%plot-line window x0 y0 x1 y1)
  (graphics-draw-line window
                      (exact->inexact x0)
                      (exact->inexact y0)
                      (exact->inexact x1)
                      (exact->inexact y1)))
