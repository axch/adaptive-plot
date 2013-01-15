(load "../load")

;; Prevent accidental guessing of locations of geometric features.
(define (offset f #!optional amount)
  (if (default-object? amount)
      (set! amount (/ 1 (sqrt 2))))
  (lambda (x)
    (f (- x amount))))

(define (example-f x)
  (cond ((< x 0) (+ 0.1 (* 5 (exp (* 100 x)))))
        (else (+ 1.1 (* 4 (exp (* 100 (- x))))))))

(gnuplot (offset example-f) -1 2 '(x-uniformly 40)
         '(prefixing "set term png; set output \"oops.png\"")
         '(commanding "title \"A whole 40 data points of my expensive function"))

(%exit 0)
