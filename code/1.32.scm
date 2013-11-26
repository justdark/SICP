(define (accumulate op term a next b)
    (if (> a b)
      1
      (op (term a) (product term (next a) next b))))
(define (product term a next b)
  (define op *)
  (accumulate op term a next b))

(define (cal_pi n)
  (define (term a) (/ (* (- a 1) (+ a 1)) (* a a)))
  (define (next a) (+ 2 a))
  (* 4 (product term 3.0 next n)))

(newline)
(display (cal_pi 10000))