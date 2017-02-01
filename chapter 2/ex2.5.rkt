#lang racket
(require racket/trace)

(define (n-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (n-car x)
  (define (iter x expt-a)
    (if (= (remainder x 2) 0)
        (iter (/ x 2) (+ expt-a 1))
        expt-a))
  (iter x 0))
        
(define (n-cdr x)
  (define (iter x expt-a)
    (if (= (remainder x 3) 0)
        (iter (/ x 3) (+ expt-a 1))
        expt-a))
  (iter x 0))

(n-cons 2 3)

(n-car (n-cons 2 3))

(n-cdr (n-cons 2 3))

