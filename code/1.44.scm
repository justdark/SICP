(define (repeated f k)
  (define (repeated-iter p)
    (if (> p k)
        (lambda (x) x)
        (lambda (x) (f ((repeated-iter (+ p 1)) x)))
        ))
  (repeated-iter 1)
  )

(define dx 0.00001)
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3.0)))

(define (smooth-n g k)
  (lambda (x) ((repeated smooth k) x)))

