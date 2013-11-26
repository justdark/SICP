(define (reverse-2.17 a)
  
  (reverse-iter (cdr a) (list (car a))))

(define (deep-reverse  x)
  (define (deep-reverse-iter b result)
    (if (null? b)
        result
        (deep-reverse-iter (cdr b) (cons (deep-reverse  (car b)) result))))
  (if (pair? x)
      (deep-reverse-iter x ())
      x)
  )
(display (deep-reverse  (list 1 2 3 (list 1 2))))
