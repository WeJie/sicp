#lang racket
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* a (+ a 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term  a pi-next b))

; 用 lambda 改写 pi-sum
(define (pi-sum2 a b)
  (sum (lambda (x) (/ 1.0 (* x ( + x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; 用 lambda 改写 add-dx
(define (integral2 f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (square x)
  (* x x))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; 使用 lambda 定义匿名 procedure，来绑定本地变量
(define (f2 x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
     (+ 1 (* x y))
     (- 1 y)))

(define (f3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))