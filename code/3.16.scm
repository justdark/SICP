(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 1 2 3))
(define x (list 'a 'b))



(define one(list 1))
(define two (list 1 2))
(define three (cons one one))
(count-pairs (cons two two))
(define seven (cons three three))
(count-pairs seven)     ;learn from website readthedocs.org