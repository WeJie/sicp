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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((null? tree) '())
        ((leaf? tree) '())
        (else
         (let ((left-tree (left-branch tree))
               (right-tree (right-branch tree)))
           (cond ((element-of-set? symbol (symbols left-tree))
                  (cons 0 (encode-symbol symbol left-tree)))
                 ((element-of-set? symbol (symbols right-tree))
                  (cons 1 (encode-symbol symbol right-tree)))
                 (else (error "Symbol not in the tree" symbol)))))))

; symbol-frequency pairs
; ((A 4) (B 2) (C 1) (D 1))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree)

; '(A D A B B C A)
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))