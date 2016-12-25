#lang racket

(define (cube n)
  (* n n n))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (simpson-rule f a b n)
  (define h (/ (- b a) n))

  (define (y k)
    (f (+ a (* k h))))
  
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 4)
          ((odd? k) 2)))

  (define (sim-term k)
    (* (y k) (factor k)))
  
  (define (sim-next k)
    (+ k 1))

  (if (odd? n)
      "n must be even!"
      (* (sum  sim-term 0.0 sim-next n) (/ h 3))))

(simpson-rule cube 0 1 100)
(simpson-rule cube 0 1 1000)

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

