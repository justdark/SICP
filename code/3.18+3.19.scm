(define (last-pair x)
  (if(null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'd)))

;;;3.18 content;;;3.19 contant space? it is;;;;;;;;
(cdr z)
(cdddr z)
(define (cycle? t)
  (define temp 'a) ;temp is used to recover data
  (define (iters x )
    (cond
      ((null? x) (begin (set-car! t temp)(= 1 0)))
      ((eq? (car x) 'special-guard-value) 
       (begin (set-car! x temp)  
         (= 1 1)))
      (else (iters (cdr x)))))
  (begin
  (set! temp (car t))
  (set-car! t 'special-guard-value)
  (iters (cdr t))))

(cycle? z)
(display z)
(define w '(a b c d))
(newline)
(cycle? w)
(display w)