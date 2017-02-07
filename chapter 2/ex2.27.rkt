#lang racket
(define (reverse lst)
  (define (reverse-iter lst-iter result)
    (if (null? lst-iter)
        result
        (reverse-iter (cdr lst-iter) (cons (car lst-iter) result))))
  (reverse-iter lst null))

(define (deep-reverse lst)
  (define (deep-reverse-iter lst-iter result)
    (if (null? lst-iter)
        result
        (deep-reverse-iter
         (cdr lst-iter)
         (cons (if (pair? (car lst-iter))
                   (deep-reverse (car lst-iter))
                   (car lst-iter))
               result))))
  (deep-reverse-iter lst null))

(define x (list (list 1 2) (list 3 4)))

x

(reverse x)

(deep-reverse x)