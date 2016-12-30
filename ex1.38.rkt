#lang racket
(require racket/trace)
(define (cont-frac n d k)
  (define (next i)
    (- i 1))
  (define (iter i result)
    (if (= i 0)
        result
        (iter (next i) (/ (n i) (+ (d i) result)))))
  (trace iter)
  (iter k 0))

(define (d i)
  (if (= 0 (remainder (- i 2) 3))
      (/ (+ (* i 2) 2) 3)
      1))

(define e
  (+ 2 (cont-frac (lambda (i) 1.0)
                  d
                  10)))
e