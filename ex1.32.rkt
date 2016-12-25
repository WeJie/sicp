#lang racket
(require racket/trace)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (inc n) (+ n 1))

(define (identity x) x)

(define (cube n)
  (* n n n))

; sum cube
(define (sum-cubes a b)
  (accumulate + 0 cube 1 inc 5))

(sum-cubes 1 5)

; product
(define (factorial n)
  (accumulate * 1 identity 1 inc n))

(trace accumulate)
(factorial 3)

; 迭代版本
(define (accumulate2 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (factorial2 n)
  (accumulate2 * 1 identity 1 inc n))

(trace accumulate2)
(factorial2 3)




   
  

