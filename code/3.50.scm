;attention: don't run it in R5RS,will cause too much iterations
;;;;;;;;;;;;prime part;;;;;;;;;;;;;;;
(define	(smallest-divisor	n)
	(find-divisor	n	2))

(define (divides? a b)
	(= (remainder b a) 0))

(define (square a) (* a a))

(define (next n)
	(+ n 1))
(define	(find-divisor	n	test-divisor)
	(cond	((>	(square	test-divisor)	n)	n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor	n (next test-divisor)))))

(define (prime? n)
	(= n (smallest-divisor n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force-sicp (cdr stream)))
(define (delay exp)
  (memo-proc (lambda () exp)))
(define (force-sicp delayed-object)
  (delayed-object))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
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
(define the-empty-stream '())
(define stream-null? null?)
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
;test the prime
(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000  1000000))))

;;;;3.50 content;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define a (stream-enumerate-interval 1 20)) 
     
(display-stream (stream-map + a a a a a) )