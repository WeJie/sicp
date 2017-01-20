#lang racket

(define (smoothing f dx)
  (lambda (x)
          (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (> n 0)
        ((compose f f) x)
        (- n 1))))

(define (n-flod-smoothed n)
  (repeated smoothing n))