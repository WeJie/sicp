#lang racket
(define hello (list 1 2 3 4 5))

(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))

(last-pair hello)