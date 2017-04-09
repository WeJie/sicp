#lang racket

; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; leaf selector
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; tree
(define (make-code-tree left right)
  (list left 
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; tree selectors
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

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set)
                (adjoin-set x (cdr set))))))

; symbol-frequency pairs
; ((A 4) (B 2) (C 1) (D 1))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
           (adjoin-set (make-leaf (car pair) ;symbol
                                  (cadr pair)) ; frequency
                       (make-leaf-set (cdr pairs))))))

(define (successive-merge sets)
  (if (= 1 (length sets))
      (car sets)
      (successive-merge
       (adjoin-set
        (make-code-tree (car sets) (cadr sets)) (cddr sets)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
sample-tree
