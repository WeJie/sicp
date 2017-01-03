#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square n)
  (* n n))

(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        ((odd? n) (expt-iter b (- n 1) (* b a)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (> n 0)
        ((compose f f) x)
        (- n 1))))

(define (n-root x n)
  (fixed-point ((repeated average-damp 1)
                (lambda (y) (/ x (fast-expt y (- n 1)))))
               1.0))

(n-root 16 4)
