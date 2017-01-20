#lang racket
(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))
  
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))
  )

(define (good-enough2? guess next-guess)
  (<
   (/ (abs (- guess next-guess)) guess)
   0.01))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess (improve guess x))
      (improve guess x)
      (sqrt-iter (improve guess x) x))
  )

(define (sqrt x)
  (sqrt-iter 1.0 x))

;(sqrt 0.00009)
;(sqrt 90000000000000000000000000000000000000000)

(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define (cube-enough? guess next-guess)
  (<
   (/ (abs (- guess next-guess)) guess)
   0.001))

(define (cube-iter guess x)
  (if (cube-enough? guess (cube-improve guess x))
      (cube-improve guess x)
      (cube-iter (cube-improve guess x) x)))
   
(define (cube x)
  (cube-iter 1.0 x))

(cube 9)