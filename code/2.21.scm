(define (square x) (* x x))
(define (square-list a)
  (map square a))

(define (square-list2 a)
  (if (null? a)
      ()
      (cons (square (car a))  (square-list2 (cdr a)))))

(display (square-list2 (list 1 2 3 4)))
