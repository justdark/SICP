(define (cont-frac N D k)
  (define (cont-frac-iter N D k p)
    (if (> p k)
        0
        (/ (N p) (+ (D p) (cont-frac-iter N D k (+ p 1))))))
  (cont-frac-iter N D k 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                                  x
                                  (- (* x x))))
                  (lambda (i) (- (* 2 i) 1))
                  k))
(define pi 3.1415926535898)
(tan-cf (/ pi 4) 100)