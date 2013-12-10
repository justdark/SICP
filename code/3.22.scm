(define (make-queue)
  (define queue (cons '() '()))
  (let ((front-ptr (car queue))
        (rear-ptr (cdr queue)))
    (define (set-front-ptr!  item) (set-car! queue item))
    (define (set-rear-ptr!  item) (set-cdr! queue item))
    (define (empty-queue? queue) (null? (car queue)))
    (define (front-queue queue)
      (if (empty-queue? queue)
          (error "FRONT called with an empty queue" queue)
          (car (car queue))))
    
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
               (set-front-ptr!  new-pair)
               (set-rear-ptr!  new-pair)
               queue)
              (else
               (set-cdr! (cdr queue) new-pair)
               (set-rear-ptr!  new-pair)
               queue)))) 
    
    (define (delete-queue!)
      (cond ((empty-queue? queue)
             (display "DELETE! called with an empty queue"))
            (else
             (set-front-ptr!  (cdr (car queue)))
             queue))) 
    (define (dispatch m)
      (cond ((eq? m 'front-queue) (front-queue queue))
            ((eq? m 'insert-queue) insert-queue!)
            ((eq? m 'delete-queue) (delete-queue!))
            (else error "undefined operation --QUEUE " m)))
    dispatch))

(define q1 (make-queue))
((q1 'insert-queue) 'a)
((q1 'insert-queue) 'b)

(q1 'delete-queue)