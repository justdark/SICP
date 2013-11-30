(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one
    (lambda (f)
        (lambda (x)
            (f x))))
(define two
    (lambda (f)
        (lambda (x)
            (f (f x)))))
(define add 
  (lambda (m n)
    (lambda (f) 
      (lambda (x) 
        ((m f) ((n f) x))))))
;;;;
;we can use the way below to watch the result to make sure we are right
(define (inc x)
  (+ x 1))
((one inc) 0)
((two inc) 0)
((zero inc) 0)
(((add one two) inc) 0)