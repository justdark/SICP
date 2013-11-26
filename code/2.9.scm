(define (make-interval a b) (cons a b))

(define (upper-bound a)
  (cdr a))
(define (lower-bound a)
  (car a))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval a b)
  (make-interval (- (upper-bound a) (lower-bound b))
                 (- (lower-bound a) (upper-bound b))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width a)
  (/ (- (upper-bound a) (lower-bound a)) 2.0))

(define (print-interval a)
  (display "[")
  (display (lower-bound a))
  (display ",")
  (display (upper-bound a))
  (display "]")(newline))

(define i1 (make-interval 1 2))

(define i2 (make-interval 2 3))
(print-interval (sub-interval i1 i2))
(width i2)
