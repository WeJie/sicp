#lang racket
(define (sum-intergers a b)
  (if (> a b)
      0
      (+ a (sum-intergers a b))))

(define (cube n)
  (* n n n))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes a b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;(define (<name> a b)
;  (if (> a b)
;      0
;      (+ (<term> a)
;         (<name> (<next> a) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;(define (inc n) (+ n 1))
;
;(define (identity x) x)
;(define (sum-intergers a b)
;  (sum identity a inc b))
;
;(define (sum-cubes a b)
;  (sum cube a inc b))
;
;(define (pi-sum a b)
;  (define (pi-term x)
;    (/ 1.0 (* a (+ a 2))))
;  (define (pi-next x)
;    (+ x 4))
;  (sum pi-term  a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)