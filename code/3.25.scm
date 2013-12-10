(define false (= 1 0))
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup-iter keys table)
      (if (null? keys)
          (cdr table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (lookup-iter (cdr keys) subtable)
                false))))
    (define (lookup keys)
      (lookup-iter keys local-table))
    
    (define (insert-iter keys value table)
      (if (null? (cdr keys))
          (begin

          (let ((record (assoc (car keys) (cdr table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! table
                            (cons (cons (car keys) value)
                                  (cdr table)))))
          'ok)     
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
                (insert-iter (cdr keys) value subtable)
                (begin (set-cdr! table (cons (list (car keys)) (cdr table)))
                       (insert-iter (cdr keys) value (car (cdr table))))))))
    
    (define (insert! keys value)
      (insert-iter keys value local-table))
 
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(put '(asd) (lambda () (display "you find me!!")))
((get '(asd)))
(put '(abd abd) (lambda () (display "you find me!!")))
((get '(abd abd)))
;;but you can't put '(asd abd) into the table after you puting '(asd)!!