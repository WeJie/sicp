#lang racket
(define (make-accumulator sum)
  (lambda (num)
    (set! sum (+ sum num))
    sum))

(define test (make-accumulator 5))
(test 10)