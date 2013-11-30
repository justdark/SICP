(define (make-interval a b) (cons a b))

(define (upper-bound a)
  (cdr a))
(define (lower-bound a)
  (car a))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))



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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define one (make-interval 1 1))
(define r1 (make-interval 2.49 2.51))
(define r2 (make-interval 3 4))
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (div-interval one (add-interval (div-interval one r1)
                                  (div-interval one r2))))
(display (par1 r1 r2))
(newline)
(display (par2 r1 r2))

(display (div-interval r1 r1))
(display (div-interval r1 r2))
(newline)
(display (par1 (make-center-percent 2 0.01) (make-center-percent 3 0.01)))
(display (par2 (make-center-percent 2 0.01) (make-center-percent 3 0.01)))
;2.15 It's right
;
;2.16: add operate add two interval difference
;      mul expand it
;      div make it short
(newline)
(display (add-interval (make-center-percent 2 0.5) (make-center-percent 2 0.5)))
(display (mul-interval (make-center-percent 2 0.5) (make-center-percent 2 0.5)))
(display (div-interval (make-center-percent 2 0.5) (make-center-percent 2 0.5)))









