(define (make-account balance p)
  (define error_times 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! error_times 0)
          (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (call-the-cops) "CALL-THE-COPS!!!")
  (define (deposit amount)
    (set! error_times 0)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m d)
    (cond
    ((and (eq? m p) (eq? d 'withdraw)) withdraw)
    ((and (eq? m p) (eq? d 'deposit)) deposit)
    ((not (eq? m p)) (lambda (x) (begin (set! error_times (+ 1 error_times))
                                        (if (>= error_times 7)
                                            (call-the-cops)
                                        "Incorrect password"))))
    (else (error "Unknown request -- MAKE-ACCOUNT" m ))))
  dispatch)
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
((acc 'dsaasdasd 'withdraw) 40)
