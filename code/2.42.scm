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
;;;;;;;;;;;;;;;;;;2.42   content;;;;;;;;;;;;;;;;;;;;;;;;

(define (adjoin-position new k rest-of-queens)
   (append (list new) rest-of-queens))

(define empty-board ())


(define (queens board-size)
  (define (safe? k positions)
    (define (safe-iter i kp p)
      (cond ((= i 0)  (= 1 1))
            (else 
              (if (or (= (abs  (-   i  k)) (abs  (- (car p) kp))) (= (car p) kp))
              (= 1 0)
              (safe-iter (- i 1) kp (cdr p))))))
    ;(display positions)
    ;(newline)
   ; (display (safe-iter (- k 1) (car positions) (cdr positions)))
    ;(newline)
    (safe-iter (- k 1) (car positions) (cdr positions)))
  
  (define  (queen-cols k)
    (if (=  k  0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                 (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))
          ))))
  (queen-cols board-size))
(define ass (queens 8))

(display (length ass))

;(display (adjoin-position (list 1 2 3) 1 ()))