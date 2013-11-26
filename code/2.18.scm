(define (last-pair-2.17 a)
  (define (iters a)
    (if (null? (cdr a))
      a
      (last-pair (cdr a))))
  (car (iters a)))

(define (append-2.17 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;;;;;;;;;;2.17 content;;;;;;;;;;;;;
(define (reverse-2.17 a)
  (define (reverse-iter b result)
    (display result)
    (newline)
    
    (if (null? b)
        result
        (reverse-iter (cdr b) (cons (car b) result))))
  (reverse-iter (cdr a) (list (car a))))

(display(reverse-2.17 (list 1 2 3 3 5)))


