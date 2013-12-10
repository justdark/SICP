(define bias 0.01)
;;;;3.24 content;;;;;;;;;;;;;
(define (equal-num? a b)
  (if (<= (abs (- a b)) bias)
      (= 1 1) ; True
      (= 0 1))) ;False
;;;;change assoc-sicp!;;;;;;
(define (assoc-sicp key records)
  (cond ((null? records) (= 1 0))
        ((and (number? (caar records))(number? key))
         (if (equal-num?  (caar records) key)
             (car records)
             (assoc-sicp key (cdr records))))
        ((equal? key (caar records)) (car records))
        (else (assoc-sicp key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-sicp key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-sicp key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-sicp key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc-sicp key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;3.24 content;;;;;;;;;;;;;;;
(put 1 2 (lambda () (display "you find me!!")))
((get 1 2))
(newline)
((get 1.0001 2))
(newline)
((get 1.0001 1.999))