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

(define (make-center-percent a percent)
  (make-interval (- a (* a percent))
                 (+ a (* a percent))))
(define (center a)
  (/ (+ (upper-bound a) (lower-bound a)) 2.0))
(define (percent a)
  (/ (width a) (center a)))
;;;;;;;;;;;;;; 2.13 content;;;;;;;;;;;;;
;;;(p1 +p2)/(1+p1*p2)
(define (cal-percent p1 p2)
  (/ (+ p1 p2) (+ (* p1 p2) 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define a1 (make-center-percent 10 0.01))
(define a2 (make-center-percent 10 0.1))
(percent  (mul-interval a1 a2))
(newline)
(cal-percent 0.01 0.1)
