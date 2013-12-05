(define (f-mk num)
  (lambda (x)
  (if (= x num)
      1
      (begin
        (set! num x)
        0))))

(define f (f-mk 1))

(+ (f 0) (f 1))

(+ (f 1) (f 0))
