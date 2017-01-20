#lang racket
(define (filter-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

; 求素数平方和
; filter prime?
; combiner +
; null-value 0
; term square
; next inc

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder  (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) (remainder 1 n)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100))

(define (sum-prime a b)
  (filter-accumulate prime? + 0 square a inc b))

(sum-prime 2 3)

; 求小于 n 且与 n 互素的数的乘积
; flter x < n, GCD(x, n) = 1
; combiner *
; null-value 1
; term indentity
; next inc

(define (identity x) x)

(define (rmd a b)
  (remainder a b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (rmd a b))))

(define (sum-product a b)
  (define (relatively-prime? x)
    (= (gcd x b) 1))
  
  (filter-accumulate relatively-prime? * 1 identity a inc b))

(sum-product 2 5)
