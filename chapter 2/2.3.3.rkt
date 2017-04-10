#lang racket
(define (element-of-set? x set)
  (cond ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
    ((element-of-set? (car set1) set2)
     (cons (car set1) 
           (intersection-set (cdr set1) set2)))
    (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
    ((null? set2) set1)
    (else (if (element-of-set? (car set1) set2)
              (union-set (cdr set1) set2)
              (cons (car set1) (union-set (cdr set1) set2))))))

(define set-1 (list 1 2 3))
(define set-2 (list 3 4 5))

(union-set set-1 set-2)