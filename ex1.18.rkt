#lang racket
(define (double n)
  (+ n n))

(define (hive n)
  (/ n 2))

(define (fast-multi-iter a b)
  (multi-iter a b 0))

(define (multi-iter primary counter product)
  (cond ((= counter 0) product)
        ((even? counter) (multi-iter (double primary) (hive counter) product))
        ((odd? counter) (multi-iter primary (- counter 1) (+ product primary)))))