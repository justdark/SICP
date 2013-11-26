(define (square a) (* a a))

(define (even? a)
	(= 0 (remainder a 2)))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
		((even? exp)
			(remainder (square (expmod base (/ exp 2) m)) 
				m))
		(else
			(remainder (* base (expmod base (- exp 1) m))
				m))))

(define (test_it n count)
	(if (= count n)
		"True"
		(if (= (expmod count n n) count)
			(test_it n (+ count 1))
			"False")))

(define (test n)
	(test_it n 1))

(newline)
(display (test 561))(newline)
(display (test 1105))(newline)
(display (test 1729))(newline)
(display (test 2465))(newline)
(newline)