(define (cont-frac N D k)
  (define (cont-frac-iter N D k p)
    (if (> p k)
        0
        (/ (N p) (+ (D p) (cont-frac-iter N D k (+ p 1))))))
  (cont-frac-iter N D k 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)