(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (square x) (* x x))

(define (cubics a b c)
  (lambda (x ) (+ (* x x x) (* a (* x x)) (* b (* x)) c)))

(define (sqrts x)
  (newton-method (lambda (y) (- (square y) x))
                 1.0))

(newton-method (cubics 1 2 3) 1.0)