;;; This file is part of Adaptive Plot, a library for plotting
;;; functions from the MIT Scheme REPL
;;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;;     2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
;;;     Technology
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

;;;; Making windows

;;; This is copied from ScmUtils' FRAME and its immediate dependencies
;;; (scmutils/src/open.scm).  The copy is to avoid depending on
;;; ScmUtils; I also renamed the procedures to avoid conflicting with
;;; ScmUtils.  That file is licensed "GPLv2 or later", which I believe
;;; permits me to incorporate it into Adaptive Plot under AGPLv3.
;;; -- axch
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
