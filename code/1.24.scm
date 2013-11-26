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
(define true (=  1 1))
(define false (not (= 1 1)))
(define (fermat-test n)
	(define (try-it a)
		(= (expmod  a n n) a))
	(try-it (+ 1 (random (- n  1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		((fermat-test n) (fast-prime? n (- times 1)))
		(else false)))


(define (runtime) (tms:clock (times)))

(define (timed-prime-test n)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (fast-prime? n 8)
		(report-prime (- (runtime) start-time))	
	))


(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
	(newline))

(define (for_test n)
	(if (fast-prime? n 8)
		(timed-prime-test n)
		(for_test (+ n 1))
	))

(begin
	(for_test 100000000000000000000)
	(for_test 100000000000000000))