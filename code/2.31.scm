(define (square x) (* x x))

(define (treemap op t)
   (if (null? t)
      '()
  (if (number? t)
      (op t)
      (cons (square-tree (car t)) (square-tree (cdr t))))))

(define (square-tree t)
  (treemap square t))

(display (square-tree
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))