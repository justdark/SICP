;;to get float value,I change the definition of random-in-range
;;because of Lazy Racket environment
(define (random-in-range low high)
  (let ((range (- (* 1000000 high) (* 1000000 low))))
    (/ (+ (* 1000000 low) (random range)) 1000000.0)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (square x) (* x x))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (define new-point
    (cons (random-in-range x1 x2)
          (random-in-range y1 y2)))
  (define (integral-test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
    (* (- x2 x1) (- y2 y1) (monte-carlo trials integral-test) 1.0))

(define (P x y)
  (<= (+ (square (- x 5)) (square (- y 7))) (square 3)))

(estimate-integral P 2 8 4 10 10000)
