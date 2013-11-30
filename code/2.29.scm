(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (left-branch a)
  (car a))
(define (right-branch  a)
  (cadr a))

(define (lengths a)
  (car a))
(define (weight a)
  (cadr a))

(define (total a)
  (define (total-struc s)
    (if (pair? s)  
        (total (weight s))
        s))
  (if (number? a)
      a
  (+ (total-struc (left-branch a)) (total-struc (right-branch a)))))

(define (balance? a)
   (= (* (lengths (left-branch a)) (weight (left-branch a)))
         (* (lengths (right-branch a)) (weight (right-branch a)))))

(define a (make-mobile (make-branch 2 4) (make-branch 3 7)))
(define b (make-mobile (make-branch 3 2) (make-branch 2 3)))
(display (left-branch a))
(newline)
(display (right-branch a))
(newline)
(total a)
(balance? a)
(balance? b)
;d) 
;you should change 'cadr' to 'cdr' in the code above
;
