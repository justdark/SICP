;;get/put from 3.3.3
;;attention: you should change Racket to R5RS to run this exercise!!
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  (= 1 0)))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
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
            (else (error "Unknown operation -- TABLE"  m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x)(symbol? x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
;;;;;;;;;;;;;;;sum part;;;;;;;;;;;;;;;;;
(define (addend s) (car s))

(define (augend s) (cadr s))

(define (=number? exp num)
  (and (number?  exp) (= exp  num)))

(define (make-sum  a1 a2) 
  (cond  ((=number? a1 0) a2)
         ((=number? a2  0)  a1)
         ((and (number? a1) (number?  a2)) (+ a1 a2))
         (else (list '+ a1 a2))))

(define (sum-deriv exp var)
  (make-sum (deriv  (addend exp) var)
                  (deriv  (augend exp) var)))

(define install-sum
  (put 'deriv '+ sum-deriv))
;;;;;;;;;;;;;;;multiple part;;;;;;;;;;;;
(define (multiplier p)  (car  p))

(define (multiplicand p) (cadr  p))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '*  m1 m2))))

(define (mul-deriv exp var)
 (make-sum
      (make-product (multiplier exp)
                  (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                  (multiplicand  exp))))

(define install-mul
  (put 'deriv '* mul-deriv))
;;;;;;;;;;;;;;;minus part;;;;;;;;;;;;;;;;
(define (make-minus  a1 a2) 
  (cond  ((=number? a1 0) a2)
         ((=number? a2  0)  a1)
         ((and (number? a1) (number?  a2)) (- a1 a2))
         (else (list '- a1 a2))))

(define (sub-deriv exp var)
  (make-minus (deriv  (addend exp) var)
                  (deriv  (augend exp) var)))
(define install-minus
  (put 'deriv '- sub-deriv))
;;;;;;;;;;;;;;;exponentiation part;;;;;;
(define (base  s) (car s))
(define (exponent s) (cadr s))

(define (exponentiation? x)
  (and (pair? x)  (eq? (car x) '**)))

(define (make-exponentiation m1 m2)
  (cond ((=number? m2 1) m1)
        ((=number? m2 0) 1)
        ((=number? m1 1) 1)
        (else (list '** m1 m2))))

(define (exp-deriv exp var)
  (make-product 
      (make-product (exponent exp) 
                    (make-exponentiation  (base exp) 
                                          (make-minus (exponent exp) 1)))
      (deriv (base exp) var)))

(define install-exp
  (put 'deriv '** exp-deriv))
;;;;;;;;;;;;;;; test ;;;;;;;;;;;;;;;;;;;
install-sum
install-mul
install-exp
install-minus
(deriv '(* (** x 2) 3) 'x) 
(deriv '(* (* x y)  (+ x 3)) 'x)