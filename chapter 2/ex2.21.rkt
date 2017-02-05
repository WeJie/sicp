#lang racket
(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (square x)) items))

(define test (list 1 2 3 4 5 6))

(square-list test)
(square-list-map test)
