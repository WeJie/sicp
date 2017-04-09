#lang racket
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree) 
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; set { 1, 3, 5, 7, 9, 11}
(define test1 (make-tree  7 
                          (make-tree 3 
                                     (make-tree 1 '() '()) 
                                     (make-tree 5 '() '())) 
                          (make-tree 9 
                                     '() 
                                     (make-tree 11 '() '()))))

(define test2 (make-tree  3
                          (make-tree 1 '() '()) 
                          (make-tree 7 
                                     (make-tree 5 '() '()) 
                                     (make-tree 9 '() (make-tree 11 '() '())))))

(define test3 (make-tree  5
                          (make-tree 3 (make-tree 1 '() '()) '()) 
                          (make-tree 9
                                     (make-tree 7 '() '()) 
                                     (make-tree 11 '() '()))))

(tree->list-1 test1)
(tree->list-2 test1)

(tree->list-1 test2)
(tree->list-2 test2)

(tree->list-1 test3)
(tree->list-2 test3)
