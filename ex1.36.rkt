#lang racket
(require racket/trace)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (trace try)
  (try first-guess))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 5.0)

(define (average a b)
  (/ (+ a b) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 5.0)