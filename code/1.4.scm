; when b > 0 ,It's (+ a b)
; when b < 0 ,It's (- a b)
; actural it's (+ a (abs b))
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))