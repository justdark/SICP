(define (or-gate a1 a2 output)
  (let ((n1 (make-wire))
        (n2 (make-wire))
        (n3 (make-wire)))
    (begin
    (inverter a1 n1)
    (inverter a2 n2)
    (and-gate n1 n2 n3)
    (inverter n3 output)
    'ok)))

;;;;havn't test on computer,but simulate it below
;;; a1=1 a2=1:
;             n1=0;n2=0;n3=0;output=1
;;; a1=0 a2=1:
;             n1=1;n2=0;n3=0;output=1
;;; a1=1 a2=0:
;             n1=0;n2=1;n3=0;output=1
;;; a1=0 a2=0:
;             n1=1;n2=1;n3=1;output=0