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
;;;;;;;;;;3.54 content;;;;;;;;;;;;;;;
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1  (mul-streams factorials (add-streams ones integers))))
(stream-ref factorials 4) ;5!=120