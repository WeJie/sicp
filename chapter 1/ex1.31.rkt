#lang racket
(require racket/trace)

(define (identity x) x)

(define (inc x)
  (+ x 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(trace product)
(factorial 3)

; 迭代版本
(define (product2 term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
    (iter a 1))

(define (factorial2 n)
  (product2 identity 1 inc n))

(trace product2)
(factorial2 3)

; 计算 pi
(define (pi-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))

(define pi  (* 4 (product2 pi-term 1.0 inc 1000000)))
pi
