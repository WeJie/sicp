#lang racket
(define (reverse lst)
  (define (reverse-iter lst-iter result)
    (if (null? lst-iter)
        result
        (reverse-iter (cdr lst-iter) (cons (car lst-iter) result))))
  (reverse-iter lst null))

(define test (list 1 2 3 4))

(reverse test)