#lang racket
(define (square x) (* x x))

(define (abs x)
  (if (< x 0) (- x) x))

(define (good-enough? v1 v2)
  (< (abs (- v1 v2)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x) x)))
;
;; 1.1.7
;(define (sqrt x)
;  (sqrt-iter 1.0 x))
;(sqrt 9)

(define (iterative-improve good-enough? improve)
  (lambda (first-guess)
    (define (iter guess)
      (if (good-enough? guess (improve guess))
          guess
          (iter (improve guess))))
    (iter first-guess)))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))


(sqrt 9)

; fixed point
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


(fixed-point cos 1.0)

(define (iterative-improve2 good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
          guess
          (try next))))
    (try first-guess)))

(define (fixed-point2 f)
  (define (improve guess)
        (f guess))
  ((iterative-improve2 good-enough? improve) 1.0))

(fixed-point2 cos)