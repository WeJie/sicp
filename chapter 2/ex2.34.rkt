#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define coefficient (list 1 3 0 5 0 1))

(define (horner-eval x coefficient-sequence)
        (accumulate (lambda (this-coeff higher-term) (+ this-coeff (* x higher-term)))
                    0
                    coefficient-sequence))

(horner-eval 2 coefficient)