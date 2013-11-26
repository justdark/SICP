
(define (cons-2.5 x y)
  (* (expt 2 x) (expt 3 y)))

(define (div-iter a b)
  (if (= 0 (remainder a b))
      (div-iter (/ a b) b)
      a))
(define (exp? a b)
  (define (exp?-iter t k)
    (if (= t b)
        k
        (exp?-iter (/ t b) (+ 1 k))))
  (cond ((= a 1) 0)
        ((= b 1) 1)
        (else (exp?-iter a 1))))

;or (exp? a b) to return a float
;(define (exp? a b)
;  (/ (log a) (log b)))

(define (car-2.5 z)
  (exp? (div-iter z 3) 2))
(define (cdr-2.5 z)
  (exp? (div-iter z 2) 3))

(define num1 (cons-2.5 7 9))
(car-2.5 num1)
(cdr-2.5 num1)


