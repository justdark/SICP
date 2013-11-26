(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (enumerate-interval a b)
  (if (> a b)
      ()
      (append (list a) (enumerate-interval (+ a 1) b))))

(define (flatmap proc  seq)
  (fold-right cons () (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (-  i 1)))) 
             (enumerate-interval 2 n)))

(display (unique-pairs 10))