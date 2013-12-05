;;;Use R5RS environment to run this exercise
(define (last-pair x)
  (if(null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'd)))
(display z)
(last-pair z) ;infinite loops,it's a cycle and don't have last element