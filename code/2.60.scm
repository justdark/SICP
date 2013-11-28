;element-of-set? :O(N)
;but cost more than before because of the repeated number
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;O(1)  much faster
(define (adjoin-set x set)
 (cons x set))

;hard to describe,if they both have two '2',should I add it twice?
;(1 2 2) (3 2 2) return (2 2)? 
;so I add delete-element-of-set element-of-set?
(define (delete-element-of-set x set)
  (cond ((null? set) set)
        ((equal? x (car set)) (cdr set))
        (else (cons (car set) (delete-element-of-set x (cdr set))))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '() )
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1 ) 
                                 (delete-element-of-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))

;easy
(define (union-set set1 set2)
  (append set1 set2))

(display (union-set (list 1 2 3 4) (list 3 4 5 6)))
;some tricky here
(display (intersection-set (list 1 2 2) (list 3 2 2)))
(display (adjoin-set 3 (list 1 2 3)))
(display (adjoin-set 4 (list 1 2 3)))
