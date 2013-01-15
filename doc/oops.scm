(load "../load")

(define (example-f x)
  (cond ((< x 0) (+ 0.1 (* 5 (exp (* 100 x)))))
        (else (+ 1.1 (* 4 (exp (* 100 (- x))))))))

(gnuplot example-f -1 2 '(x-uniformly 40)
         '(prefixing "set term png; set output \"oops.png\""))

(%exit 0)
