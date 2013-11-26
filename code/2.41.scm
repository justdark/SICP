(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (flatmap proc  seq)
  (fold-right append () (map proc seq)))

(define (enumerate-interval a b)
  (if (> a b)
      ()
      (append (list a) (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (-  i 1)))) 
             (enumerate-interval 2 n)))

;;;;;;;;;;;;;;;;2.41 content;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define  (sum a)
  (fold-right + 0  a))

(define (triple n s)
  (filter (lambda (a) (= (sum  a) s))
  (flatmap (lambda (i) (map (lambda (j) (append (list i) j))
                            (unique-pairs (- i  1)))) 
             (reverse (enumerate-interval 3 n)))))

(display  (triple  4 8))