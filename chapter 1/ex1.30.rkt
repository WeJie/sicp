#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))

(define (cube n)
  (* n n n))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-cubes2 a b)
  (sum2 cube a inc b))

(require racket/trace)
(trace sum)
(trace sum2)

(sum-cubes 2 8)
(sum-cubes2 2 8)