#lang racket
(require racket/trace)

(define (inc x)
  (+ x 1))


(define (dec x)
  (- x 1))

(define (plus a b)
  (if (= a 0)
      b
      (inc (plus (dec a) b))))


;(define (plus a b)
;  (if (= a 0)
;      b
;      (plus (dec a) (inc b))))


;(trace plus)
;(plus 4 5)

(define  (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

(trace f)
(f 5)
(trace g)
(g 0)
(g 5)
(trace h)
(h 0)
(h 1)
(h 2)
(h 3)
(h 4)
(trace k)
(k 5)


                 