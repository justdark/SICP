(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (adjoin-set x set)
 (cons x set))
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)(cadr x))
(define (weight-leaf x)(caddr x))

(define (make-code-tree l r)
  (list l r
        (append (symbols l) (symbols r))
        (+ (weight l) (weight r))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set weight (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol m tree)
  (if (leaf? tree)
      '()
      (cond ((not (element-of-set? m (symbols tree))) (error "bad message --ENCODE-SYMBOL" m))
            ((element-of-set? m (symbols (left-branch tree)))
             (cons 0 (encode-symbol m (left-branch tree))))
            (else
             (cons 1 (encode-symbol m (right-branch tree)))))))
(define (encode message tree)
  (if (null? message)
  '()
  (append (encode-symbol (car message) tree)
          (encode (cdr message) tree))))

;;;;;;;;;set content;;;;;;;;;;;;;;;;;;;;;;


(define (adjoin-set key x set)
  (cond ((null? set)(list x))
        ((= (key x) (key (car set))) (cons x set))
        ((< (key x) (key (car set))) (cons x set))
        (else
         (cons (car set) (adjoin-set key x (cdr set))))))

;;;;;;;;;2.69 content;;;;;;;;;;;;;;;;;;;;;;;
(define (successive-merge leafs)
  (define (merge-iter leaf)
    (if (= 1 (length leaf))
        leaf
        (let ((new_node (make-code-tree  (cadr leaf) (car leaf))))
          (merge-iter (adjoin-set weight new_node (cddr leaf))))))
  (car (merge-iter leafs)))

(define (generate-huffman-tree pairs)
  (successive-merge (
                     make-leaf-set pairs)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(display (decode sample-message sample-tree))
(newline)
(display (encode '(A D A B B C A) sample-tree))
(newline)
(define tree_gen (generate-huffman-tree (list (list 'A 4)
                                              (list 'B 2)
                                              (list 'D 1)
                                              (list 'C 1)
                                              )))

;;;not same to (' A D A B B C A)
;;;our tree is different from sample_tree
;;;beacuse 'D and 'C have same weight
;;;out tree_gen is also right
(newline)
(display (decode sample-message tree_gen))
(display (encode (decode sample-message tree_gen) tree_gen))













