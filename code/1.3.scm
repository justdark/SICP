(define (max3 a b c)
  (- (+ a b c) (min a (min b c))))

(max3 1 2 3)