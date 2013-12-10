;;;;;;
;;;;; This exercise ,I can only get the right answer in MIT-SCHEME environment
;;;;;in Lazy Racket:It's 5 5,7 7, and I can make sure the stream is working,maybe there is the difference Racket handle stream
;;;;in R5RS:It ouputs 10 9 8 7 6 5 4 3 2 1 0 at the begining define x
;;;;while the right answer should be 1 2 3 4 5 when ref 5,and 6 7 when ref 7

(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
;;;;;
(define (cons-stream a b)
  (cons a (delay b)))
(define (delay exp)
  (memo-proc (lambda () exp)))
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
(define (force delayed-object)
  (delayed-object))
;;;;
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-map proc s)
  (if (null? s)
      '()
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define false (= 1 0))
;(define true (= 1 1))

(define (show x)
  (display-line x)
  x)
(define x (stream-map show (stream-enumerate-interval 0 11)))
(stream-ref x 5)
(stream-ref x 7)