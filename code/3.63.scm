;; stream fundamental;;;;;;;;;;;;;
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
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
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


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (get-stream s n)
  (if (= n 0)
      (stream-car s)
      (cons
         (stream-car s)
        (get-stream (stream-cdr s) (- n 1)))))
;;;;;;;;;;;;;;;;;;;;;3.63 content;;;;;;;;;;;;;;;;
(define (partial-sums S)
  (define ans 
    (cons-stream (stream-car S) (add-streams ans  (stream-cdr S))))
  ans)

;;;;;;;test of the textbook;;;;;;;;;;;;;;;;
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))    
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
(display (get-stream pi-stream 100))
(newline)
(display (get-stream (accelerated-sequence euler-transform
                                       pi-stream) 10))
;It works,but because of the accurate in Lazy Racket,I got nan.0 in 10th elements

;;;;;;;;;3.63.scm;;;;;;;;;;;;;;;;;;;;;;;;
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (average x1 x2) (/ (+ x1 x2) 2))
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
(newline)
(display-line (get-stream (sqrt-stream 2) 20))
;;O(N*2) algorithm,momo-proc is useless in it,He use it,and the result is gone with the temp variable





