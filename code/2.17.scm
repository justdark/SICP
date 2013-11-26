(define (last-pair-2.17 a)
  (define (iters a)
    (if (null? (cdr a))
      a
      (last-pair (cdr a))))
  (car (iters a)))

(last-pair-2.17 (list 1))

