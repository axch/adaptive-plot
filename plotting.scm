(declare (usual-integrations))

(load-option 'synchronous-subprocess)
(load-option 'wt-tree)

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
      (receive
       (xlow xhigh ylow yhigh) (plot-dimensions plot)
       (and (<= xlow y xhigh)
	    (<= ylow y yhigh)
	    (%plot-point (plot-window plot) x y)))))

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
      (graphics-set-clip-rectangle (plot-window plot) xlow ylow xhigh yhigh)
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
  (letrec ((new-plot
	    (make-plot 960 1200
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
      (plot-learn-point! plot x-value ((plot-point-source plot) x-value))))

(define (plot-learn-point! plot x y)
  (set-plot-known-points!
   plot (point-set-insert (plot-known-points plot) `((,x . ,y)))))

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

(define (plot-refine! plot)
  (plot-initialize! plot)
  (receive
   (xlow xhigh ylow yhigh)
   (plot-dimensions plot)
   (plot-dim-refine! plot (desired-separation xlow xhigh 10) car)
   (plot-redraw! plot)
   (plot-line-interpolate! plot)
   (plot-redraw! plot)))

;;; The stats of the parabola that goes through (x1, 0), (0, 0), and
;;; (x3, y), with the condition that x1 < 0 < x3 or x3 < 0 < x1.
;;; The stats of such a parabola are the area under the lobe and the x
;;; and y corrdinates of the point that takes the largest possible
;;; triangular bite out of it (returned in that order)
(define (grounded-parabola-stats x1 x3 y)
  (let* ((base (abs x1))
	 (peak-x (/ x1 2))
	 (peak-y (/ (* -1 y peak-x peak-x)
		    (* (- x3 x1) x3)))
	 (area (abs (* 2/3 base peak-y))))
    (values area peak-x peak-y)))

;;; The stats of the p1,0 lobe of the parabola that goes through (x1,
;;; y1), (0, 0), and (x3, y3), under the condition that x1 < 0 < x3
;;; and the p1, 0, p3 angle is obtuse.
(define (obtuse-parabola-left-stats x1 y1 x3 y3)
  (let* ((dist-p1 (sqrt (+ (square x1) (square y1))))
	 (cos-p1-angle (/ x1 dist-p1))
	 (sin-p1-angle (/ y1 dist-p1))
	 ;; rotation by -(angle - pi)
	 (cos-rot-angle (- cos-p1-angle))
	 (sin-rot-angle sin-p1-angle)
	 (new-x3 (- (* cos-rot-angle x3) (* sin-rot-angle y3)))
	 (new-y3 (+ (* sin-rot-angle x3) (* cos-rot-angle y3))))
    (receive
     (area peak-x peak-y)
     (grounded-parabola-stats (- dist-p1) new-x3 new-y3)
     ;; rotation back, by +(angle - pi)
     (let ((old-peak-x (+ (* cos-rot-angle peak-x) (* sin-rot-angle peak-y)))
	   (old-peak-y (+ (* -1 sin-rot-angle peak-x) (* cos-rot-angle peak-y))))
       (values area old-peak-x old-peak-y)))))

;;; The stats of the 0,p3 lobe of the parabola that goes through (x1,
;;; y1), (0, 0), and (x3, y3), under the condition that x1 < 0 < x3
;;; and the p1, 0, p3 angle is obtuse.
(define (obtuse-parabola-right-stats x1 y1 x3 y3)
  (let* ((dist-p3 (sqrt (+ (square x3) (square y3))))
	 (cos-p3-angle (/ x3 dist-p3))
	 (sin-p3-angle (/ y3 dist-p3))
	 ;; rotation by -angle
	 (new-x1 (+ (* cos-p3-angle x1) (* sin-p3-angle y1)))
	 (new-y1 (+ (* -1 sin-p3-angle x1) (* cos-p3-angle y1))))
    (receive
     (area peak-x peak-y)
     (grounded-parabola-stats dist-p3 new-x1 new-y1)
     ;; rotation back, by +angle
     (let ((old-peak-x (- (* cos-p3-angle peak-x) (* sin-p3-angle peak-y)))
	   (old-peak-y (+ (* sin-p3-angle peak-x) (* cos-p3-angle peak-y))))
       (values area old-peak-x old-peak-y)))))

;;; The stats of the given lobe of the parabola that goes through
;;; (x1, y1), (0, 0), and (x3, y3), under the condition that x1 < 0 <
;;; x3 and the p1, 0, p3 angle is obtuse.  Legal values for LOBE are
;;; 'left for the p1,0 lobe and 'right for the 0,p3 lobe
(define (obtuse-parabola-stats lobe x1 y1 x3 y3)
  (if (eq? lobe 'left)
      (obtuse-parabola-left-stats x1 y1 x3 y3)
      (obtuse-parabola-right-stats x1 y1 x3 y3)))

;;; The stats of the given lobe of the parabola that goes through
;;; (x1, y1), (0, 0), and (x3, y3), under the condition that x1 < 0 <
;;; x3.  (The p1, 0, p3 angle may be acute.)
(define (origin-parabola-stats lobe x1 y1 x3 y3)
  (let* ((slope1 (/ y1 x1))
	 (slope3 (/ y3 x3))
	 (slope-max (max (abs slope1) (abs slope3))))
    (if (< slope-max 2/3)
	(obtuse-parabola-stats lobe x1 y1 x3 y3)
	(receive
	 (area peak-x peak-y)
	 (obtuse-parabola-stats lobe
	  x1 (/ y1 (* 2 slope-max)) x3 (/ y3 (* 2 slope-max)))
	 (values (* area 2 slope-max) peak-x (* peak-y 2 slope-max))))))

;;; The stats of the given lobe of the parabola that goes through
;;; (x1, y1), (x2, y2), and (x3, y3), under the condition that x1 < x2
;;; < x3.
(define (parabola-stats lobe x1 y1 x2 y2 x3 y3)
  (receive
   (area peak-x peak-y)
   (origin-parabola-stats lobe
    (- x1 x2) (- y1 y2) (- x3 x2) (- y3 y2))
   (values area (+ peak-x x2) (+ peak-y y2))))

(define (plot-line-interpolation-map plot big-lobe?)
  (let* ((relevant-points (plot-relevant-points plot))
	 (relevant-segments
	  (map make-segment (cons #f relevant-points) relevant-points 
	       (cdr relevant-points) (append (cddr relevant-points) (list #f))))
	 (meaningful-segments (filter big-lobe? relevant-segments)))
    (alist->wt-tree segment-wt-tree-type
		    (map (lambda (seg)
			   (cons seg #f))
			 meaningful-segments))))

(define (plot-update-interpolation-map tree new-p big-lobe?)
  (define (assert thing)
    (if (not thing)
	(error "Assertion failed")))
  (let ((biggest-segment (wt-tree/min tree)))
    (assert (< (car (segment-p1 biggest-segment)) (car new-p)
	       (car (segment-p2 biggest-segment))))
    (let* ((candidates (split-segment biggest-segment new-p))
	   (insertees (filter big-lobe? candidates)))
      (let loop ((tree (wt-tree/delete-min tree))
		 (insertees insertees))
	(if (null? insertees)
	    tree
	    (loop (wt-tree/add tree (car insertees) #t) (cdr insertees)))))))

(define (plot-line-interpolate! plot)
  (let ((big-lobe? (plot-big-lobe plot)))
    (let loop ((to-do (plot-line-interpolation-map plot big-lobe?)))
      (if (wt-tree/empty? to-do)
	  'ok
	  (let* ((new-x (segment-candidate-x (wt-tree/min to-do)))
		 (new-y ((plot-point-source plot) new-x)))
	    (pp (wt-tree/min to-do))
	    (plot-learn-point! plot new-x new-y)
	    (loop (plot-update-interpolation-map
		   to-do (cons new-x new-y) big-lobe?)))))))

(define (plot-data-area plot)
  (receive
   (xlow xhigh ylow yhigh)
   (plot-dimensions plot)
   (* (- xhigh xlow) (- yhigh ylow))))

(define (plot-screen-area plot)
  (* (plot-xresolution plot) (plot-yresolution plot)))

(define (plot-invisible-area plot)
  (/ (plot-data-area plot) (plot-screen-area plot)))

(define (plot-big-lobe plot)
  (lambda (seg)
    (> (segment-candidate-area seg) (plot-invisible-area plot))))

;;; This is the segment between p1 and p2.  x0 < x1 < x2 < x3.  p0 or
;;; p3 may be #f.  There are two lobes over this segment, one defined
;;; by p0 and one by p3.  The candidate-area is the area of the
;;; larger, and the candidate-x is the x-coordinate of the point that
;;; would take the biggest triangular bite out of it.  An invariant of
;;; the geomtery is that x1 < candidate-x < x2.  If candidate-area is
;;; 0, candidate-x may be #f.
(define-structure (segment safe-accessors (constructor %make-segment))
  p0 p1 p2 p3 candidate-x candidate-area)

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

(define (segment-ignorable-< seg1 seg2)
  (cond ((< (segment-candidate-area seg1) (segment-candidate-area seg2))
	 #f)
	((> (segment-candidate-area seg1) (segment-candidate-area seg2))
	 #t)
	(else
	 (> (hash seg1) (hash seg2)))))

(define segment-wt-tree-type (make-wt-tree-type segment-ignorable-<))

;;; Assuming x0 < x1 < new-x < x2 < x3, produces two new segments:
;;; x0 < x1 < new-x < x2 and x1 < new-x < x2 < x3.
(define (split-segment segment new-p)
  (let ((p0 (segment-p0 segment))
	(p1 (segment-p1 segment))
	(p2 (segment-p2 segment))
	(p3 (segment-p3 segment)))
    (list (make-segment p0 p1 new-p p2)
	  (make-segment p1 new-p p2 p3))))

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
			 " with lines"
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
  (graphics-draw-point window (exact->inexact x) (exact->inexact y))
  (pp `("Plotting" ,(exact->inexact x) ,(exact->inexact y))))

(define (%plot-line window x0 y0 x1 y1)
  (graphics-draw-line window
                      (exact->inexact x0)
                      (exact->inexact y0)
                      (exact->inexact x1)
                      (exact->inexact y1)))
