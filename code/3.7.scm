(define (make-account balance p)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m d)
    (cond
    ((and (eq? m p) (eq? d 'withdraw)) withdraw)
    ((and (eq? m p) (eq? d 'deposit)) deposit)
    ((not (eq? m p)) (lambda (x) "Incorrect password"))
    (else (error "Unknown request -- MAKE-ACCOUNT" m ))))
  dispatch)


;;;;;;;;;3.7 content;;;;;;;;;;;;
(define (make-joint acc psword new-psword)
  (define (dispatch m d)
    (cond
    ((and (eq? m new-psword) (eq? d 'withdraw)) (acc psword 'withdraw))
    ((and (eq? m new-psword) (eq? d 'deposit)) (acc psword 'deposit))
    ((not (eq? m new-psword)) (lambda (x) "Incorrect password"))
    (else (error "Unknown request -- MAKE-ACCOUNT" m ))))
  dispatch)
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
(define acc2 (make-joint acc 'secret-password 'newpass))
((acc2 'newpass 'deposit )30)
((acc2 'newpass 'withdraw )20)