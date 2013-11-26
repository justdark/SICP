(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;;;;;;;;;;;;;;;;;2.36 content;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter-first seq)
    (if (null? seq)
        ()
        (cons (car (car seq)) (filter-first (cdr seq)))))

(define (filter-first-out seq)
    (if (null? seq)
        ()
        (cons (cdr (car seq))  (filter-first-out (cdr seq)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (filter-first  seqs))
            (accumulate-n op init (filter-first-out seqs)))))

(define s (list (list 1 2 3) 
                    (list 4 5 6) 
                    (list 7 8 9) 
                    (list 10 11 12)))
(display (accumulate-n + 0 s))
