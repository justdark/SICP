(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;;;;;;;;;;;;;;;;;2.35 content;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-leaves t)
  (accumulate (lambda (x y) 
              (if (pair? x)
                  (+ (count-leaves x) y)
                  (+ 1 y))) 
              0 t))

(count-leaves (list 2 3 2 (list 1 2 3 4)))