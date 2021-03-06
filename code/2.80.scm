;;;2.78 content is in line 49-63 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;prerequisite ,Racket R5RS Scheme don't support error
(define (error reason . args)
      (display "Error: ")
      (display reason)
      (for-each (lambda (arg) 
                  (display " ")
		  (write arg))
		args)
      (newline)
     ) 
 (define (square x)
   (* x x))
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
;;;;;;;;;;tag tool--------2.78 content ;;;;;;;;;;;;;;;;

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum)(car datum))
        ((number? datum) 'scheme-number)
        (esle (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents  datum)
  (cond ((pair? datum)(cdr datum))
        ((number? datum) datum)
        (esle (error "Bad tagged datum -- CONTENT" datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc 
          (apply proc (map contents args))
          (error "No Method for these types -- APPLY-GENERIC" (list op type-tags))))))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  'done)

;;;;;;;;;;;;;;;;rational number part;;;;;;;;;;;;;;;;;
(define (install-rational-package)
  (define (make-rat n d)
    (define (right-pm a b)
      (if (> (* a b) 0)
          (cons (abs a) (abs b))
          (cons (- (abs a)) (abs b))))
    (let (( g (gcd n d)))
      (right-pm (/ n g) (/ d g))))

  (define (numer x) (car x))
  (define (denom x) (cdr x))

  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  (define (zeros-rat? x)
    (= (numer x) 0))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (tag (equal-rat? x y))))
  (put '=zero? '(rational)
       (lambda (x) (tag (zeros-rat? x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
;;;;;;;;;;;;;;;;;;;complex part;;;;;;;;;;;;;;;;;;;;;;
(define (install-complex-package)
  ;;;rectangular-package-part
  (define (install-rectangular-package)
    (define (real-part-sicp z) (car z))
    (define (imag-part-sicp z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude-sicp z)
      (sqrt (+ (square (real-part-sicp z))
               (square (imag-part-sicp z)))))
    (define (angle z)
    (atan (imag-part-sicp z) (real-part-sicp z)))
    (define (make-from-mag-ang r a) 
      (cons (* r (cos a)) (* r (sin a))))
    (define (tag x) (attach-tag 'rectangular x))
    (put 'real-part-sicp '(rectangular) real-part-sicp)
    (put 'imag-part-sicp '(rectangular) imag-part-sicp)
    (put 'magnitude-sicp '(rectangular) magnitude-sicp)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular 
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)
  ;;;polar package part
  (define (install-polar-package)
    (define (magnitude-sicp z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part-sicp z)
      (* (magnitude-sicp z) (cos (angle z))))
    (define (imag-part-sicp z)
      (* (magnitude-sicp z) (sin (angle z))))
    (define (make-from-real-imag x y) 
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part-sicp '(polar) real-part-sicp)
    (put 'imag-part-sicp '(polar) imag-part-sicp)
    (put 'magnitude-sicp '(polar) magnitude-sicp)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
    'done)
  ;;;;;;main part;;;;;;;;;;;;
  (install-polar-package)
  (install-rectangular-package)
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part-sicp z1) (real-part-sicp z2))
                         (+ (imag-part-sicp z1) (imag-part-sicp z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part-sicp z1) (real-part-sicp z2))
                         (- (imag-part-sicp z1) (imag-part-sicp z2))))
  
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude-sicp z1) (magnitude-sicp z2))
                       (+ (angle z1) (angle z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude-sicp z1) (magnitude-sicp z2))
                       (- (angle z1) (angle z2))))
  
  (define (equ-complex x y)
    (and (= (apply-generic 'real-part-sicp x) (apply-generic 'real-part-sicp y))
         (= (apply-generic 'imag-part-sicp x) (apply-generic 'imag-part-sicp y))))
  (define (zero-complex x)
    (and (= (apply-generic 'real-part-sicp x) 0)
         (= (apply-generic 'imag-part-sicp x) 0)))
  ;;;instalinnggggggggggggggggggggggggg
  (define (tag z)
    (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)(tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)(tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)(tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)(tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)(tag (equ-complex z1 z2))))
  (put '=zero? '(complex)
       (lambda  (z1) (tag (zero-complex z1))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)(tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y)(tag (make-from-mag-ang x y))))
  (define (real-part-sicp z) (apply-generic 'real-part-sicp z))
  (define (imag-part-sicp z) (apply-generic 'imag-part-sicp z))
  (define (magnitude-sicp z) (apply-generic 'magnitude-sicp z))
  (define (angle z) (apply-generic 'angle z))
  
  (put 'real-part-sicp '(complex) real-part-sicp)
  (put 'imag-part-sicp '(complex) imag-part-sicp)
  (put 'magnitude-sicp '(complex) magnitude-sicp)
  (put 'angle '(complex) angle)
  'done)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;install tree package
(install-complex-package)
(install-rational-package)
(install-scheme-number-package)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (real-part-sicp z) (apply-generic 'real-part-sicp z))
(define (imag-part-sicp z) (apply-generic 'imag-part-sicp z))
(define (magnitude-sicp z) (apply-generic 'magnitude-sicp z))
(define (angle-sicp z) (apply-generic 'angle z))

(define z ((get 'make-from-real-imag 'complex) 4 3))
;(define r ((get 'make 'rational) 1 3))
;(define t ((get 'make 'rational) 2 3))
;(add r t)
(=zero? 0)
(=zero? 2)
(=zero? ((get 'make-from-real-imag 'complex) 0 0))
(=zero? ((get 'make-from-real-imag 'complex) 4 3))
(=zero? ((get 'make 'rational) 0 3))
(=zero? ((get 'make 'rational) 4 3))
;;;in Racket,it output:
;done
;done
;done
;#t
;#f
;(complex . #t)
;(complex . #f)
;(rational . #t)
;(rational . #f)

