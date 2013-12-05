(define (count-pairs x)
  (define (loop x lists)
    (if (pair? x)
        (if (memq x lists)
            (loop (cdr x) lists)
            (+ 1 (loop (cdr x) (cons x lists))))
        0))
  (loop x '()))

(count-pairs (list 1 2 3))
(define x (list 'a 'b))



(define one(list 1))
(define two (list 1 2))
(define three (cons one one))
(count-pairs (cons two two))
(define seven (cons three three))
(count-pairs seven)  