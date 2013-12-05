;;;Use R5RS environment to run this exercise
(define (append-2.2.1 x y)
  (if (null?x)
  y
  (cons (car x) (append-2.2.1 (cdr x)y))))

(define (last-pair x)
  (if(null? (cdr x))
     x
     (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(display z)
(cdr z)

(define w(append! x y))
(display w)
(cdr x)

;;the output is same,but the z is a new list
;;                   the w is the conjugate Existing x y