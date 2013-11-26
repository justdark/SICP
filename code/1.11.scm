
(define (f1 n)	
	(define (g a b) (* b (f1 (- a b))))
	
	(cond ((< n 3) n)
		(else (+ (g n 1) (g n 2) (g n 3)))))


(define (f2 n)
	(define (f-iter a b c count)
		(cond ((= count 0) (+ (* 3 a) (* 2 b) c))
			(else (f-iter b c (+ (* 3 a) (* 2 b) c) (- count 1)))))
	
	(cond ((< n 3) n)
		(else (f-iter  0  1  2 (- n 3))))
	)

(begin
(display (f1 5))
(newline)


(display (f2 5))
(newline)

)