(define	(smallest-divisor	n)
	(find-divisor	n	2))

(define (divides? a b)
	(= (remainder b a) 0))

(define (square a) (* a a))

(define (next n)
	(+ n 1))
(define	(find-divisor	n	test-divisor)
	(cond	((>	(square	test-divisor)	n)	n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor	n (next test-divisor)))))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (runtime) (tms:clock (times)))

(define (timed-prime-test n)
	(display n)
	(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
	(if (prime? n)
		(report-prime (- (runtime) start-time))
		
	))

(define (report-prime elapsed-time)
	(display " *** ")
	(display elapsed-time)
	(newline))

(define (for_test n)
	(if (prime? n)
		(timed-prime-test n)
		(for_test (+ n 1))
	))

(begin
	(for_test 100000000000)
	(for_test 1000000000000))