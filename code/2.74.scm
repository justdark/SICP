;;get/put from 3.3.3
;;attention: you should change Racket to R5RS to run this exercise!!
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  (= 1 0)))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE"  m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
;;;;;;;;;;tag tool;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents  datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENT" datum)))
;;;;;;;;;;;;;;;;;;;;;;;simple set from 2.59.scm;;;;;;;;;;;;;;;
(define (element-of-set? x set)
  (cond ((null? set) (= 1 0))
        ((equal? x (car set)) (= 1 1))
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '() )
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1 ) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (union-set (cdr set1) (cons (car set1) set2)))))

;;;;;;;;;;;;;;;;;;;;;;;2.74 content;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;a)
;;;;;;I have define two type of document,one is (sample-doc)
;                                    another is (another-doc)
;     We should install each of them to finish b) exercise
(define (get-record doc key val) 
  (define (search-iter docs)
    (cond ((null? docs) '())
      ((element-of-set? (list key val) (car docs)) (car docs))
      (else (search-iter (cdr  docs)))))
  (search-iter doc))

(define sample-doc
  (attach-tag 'sample-doc 
    (list (list (list 'Name 'DarkScope) (list 'Age 100) (list 'Salary 10000))
          (list (list 'Name 'Justdark) (list  'Age 23) (list 'Salary 2100)))))

(define another-doc
  (attach-tag 'another-doc
    (list (list (list 'Name 'Dark) (list 'Salary 1000))
          (list (list 'Name 'Just) (list 'Salary 2100)))))
(display "Exercise A:\n")
(get-record (contents sample-doc) 'Name 'DarkScope)

;;;;;b)
;;;use the table
(define (get-salary-from-sample name)
   (let ((record (get-record (contents sample-doc) 'Name name)))
    (if (pair? record)
        (cdaddr record)
        '())))

(define install-sample
  (put 'get-salary 'sample-doc get-salary-from-sample))

(define (get-salary-from-another name)
  (let ((record (get-record (contents another-doc) 'Name name)))
    (if (pair? record)
        (cdadr record)
        (= 0 1))))
(define install-another
  (put 'get-salary 'another-doc get-salary-from-another))

(define (get-salary doc name)
  ((get 'get-salary (type-tag doc)) name))

;;;;;;installllllllllllllllllllllllllllllll;;;;;;
(display "Exercise B:\n")
install-sample
install-another

(get-salary sample-doc 'DarkScope)
(get-salary another-doc 'Dark)


;;;;;c)
(define all-doc (list sample-doc another-doc))

(define (find-employee-record sets name)
  (define (finditer set)
    (let ((re (get-record (contents (car set)) 'Name name)))
      (cond ((null? set) '())
            ((null? re) (finditer (cdr set)))
            (else re))))
  (finditer sets))
(display "Exercise C:\n")
(find-employee-record all-doc 'Dark)
(find-employee-record all-doc 'DarkScope)



;;;;;;;;;d
;;;;;;;;;no change at all
;;but should wirte handle function and put it into the table


















