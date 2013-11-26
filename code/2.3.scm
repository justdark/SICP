(define (make-point a b)
  (cons a b))
(define (x-point a)
  (car a))
(define (y-point a)
  (cdr a))


(define (make-rect a b c d)
  (cons (cons a b) (cons c d)))
(define (lt-rect a) (car a)) ;left-top point
(define (rb-rect a) (cdr a)) ;right-bottom point
(define (circum-rect r)
  (* 2 (+ (abs (- (x-point (lt-rect r)) (x-point (rb-rect r)))) 
          (abs (- (y-point (lt-rect r)) (y-point (rb-rect r)))))))
(define (area-rect r)
  (* (abs (- (x-point (lt-rect r)) (x-point (rb-rect r)))) 
          (abs (- (y-point (lt-rect r)) (y-point (rb-rect r))))))

(define rect1 (make-rect 0 0 4 3))
(area-rect rect1)
(circum-rect rect1)
