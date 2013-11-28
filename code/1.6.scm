(define (new-if predicate then-clause else-clause)
        (cond (predicate then-clause)
              (else else-clause)))

(define (average x y)(/ (+ x y) 2))
(define (square x) (* x x))
(define (improve guess x)(average guess (/ x guess)))
(define (good_enough? guess x)
        (< (abs (- (square guess) x)) 0.000001))
(define (sqrt_iter guess x)
        (new-if (good_enough? guess x)
            guess
            (sqrt_iter (improve guess x) x)))
;;;;;lazy Racket is regular order..so I havn't find different
;;;;;but If it's Applicative order
;;;;;new-if will calculate the else-clause and which in (sqrt_iter)
;;;;;is (sqrt_iter (improve guess x) x),back to itself again,it's a infinite loop
(sqrt_iter 1.0 2)