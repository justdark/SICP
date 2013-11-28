(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))
;;;;;;;;add the getkey function to get the structure's key
(define (element-of-set? getkey x set)
  (cond ((null? set) false)
        ((= (getkey x) (getkey (entry set))) true)
        ((< (getkey x) (getkey (entry set)))
         (element-of-set? getkey x (left-branch set)))
        ((> (getkey x) (getkey (entry set)))
         (element-of-set? getkey x (right-branch set)))))

(define (adjoin-set getkey x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (getkey x) (getkey (entry set))) set)
        ((< (getkey x) (getkey (entry set)))
         (make-tree (entry set)
                    (adjoin-set getkey x (left-branch set))
                    (right-branch set)))
        ((> (getkey x) (getkey (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set getkey x (right-branch set))))))

(define (tree->list tree)
  (if (null? tree)
      '()
      (append (tree->list (left-branch tree))
              (cons (entry tree)
                    (tree->list (right-branch tree))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
;;;;;;;;;;;;;;;2.65 content;;;;;;;;;;;;;;;;;;;
;a little different form the book
;I send in a function getkey,so this tree can fit any structure 
;if you have the corresponding key function
(define (key x)(car x))

(define (lookup getkey x set)
    (cond ((null? set) false)
        ((= x (getkey (entry set))) (entry set))
        ((< x (getkey (entry set)))
         (lookup getkey x (left-branch set)))
        ((> x (getkey (entry set)))
         (lookup getkey x (right-branch set)))))

(define set1 (list->tree (list (list 1 "a")(list 2 "b") (list 3 "c")
                               (list 7 "d"))))
;test
(display (tree->list set1))
(newline)
(display (lookup key 1 set1))
(newline)
(display (lookup key 3 set1))


         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         
