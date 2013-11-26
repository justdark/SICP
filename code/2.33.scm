(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;;;;;;;;;;;;;;;;;2.33 content;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (map-sicp p sequence)
  (accumulate (lambda (x y) (cons  (p x) y)) () sequence))

(define (append-sicp seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-sicp sequence)
  (accumulate (lambda (x y) (+ 1 y))0 sequence))

(define (square x) (* x x))
(display (map-sicp square (list 1 2 3)))(newline)
(display (append-sicp (list 1 2) (list 3 4)))(newline)
(display (length-sicp (list 1 2 3 1 2)))