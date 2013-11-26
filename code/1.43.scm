(define (repeated f k)
  (define (repeated-iter p)
    (if (> p k)
        (lambda (x) x)
        (lambda (x) (f ((repeated-iter (+ p 1)) x)))
        ))
  (repeated-iter 1)
  )

(define (square-1.43 x) (* x x))

((repeated square-1.43 2) 5)