(define (compose-1.42 f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

((compose-1.42 square inc) 6)