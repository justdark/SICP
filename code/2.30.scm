(define (square-tree t)
  (if (null? t)
      '()
  (if (number? t)
      (* t t)
      (cons (square-tree (car t)) (square-tree (cdr t))))))
(display (square-tree
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))