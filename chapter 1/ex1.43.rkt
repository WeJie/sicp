#lang racket
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (> n 0)
        ((compose f f) x)
        (- n 1))))

(define (square x)
  (* x x))

((repeated square 2) 5)