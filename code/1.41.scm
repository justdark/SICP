(define (double-1.41 p) (lambda (x) (p (p x))))

(define (inc x) (+ x 1))

(((double-1.41 (double-1.41 double-1.41)) inc) 5)
(newline)
((double-1.41 inc) 2)