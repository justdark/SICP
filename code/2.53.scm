(define (memq-sicp item x)
  (cond  ((null? x) (= 1 0))
  ((eq? item (car x)) x)
  (else (memq item (cdr x)))
  ))

(display (list  'a 'b 'c))  
(newline)
(display  (list (list 'george)))
;ans:
;  (a b c)
;  ((george))
;  (y1 y2)
;  (x1 x2)
;  false
;  false
;  (red shoes blue socks)