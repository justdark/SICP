(define x 10)
(parallel-execute (lambda () (set! x (* x x))))