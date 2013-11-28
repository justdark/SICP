(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
;;;;;Racket return 0!!!!It's regular order!!
;;;;;otherwise it will expand (p) to ...(((p)))... until stack overflow
(test 0 (p))