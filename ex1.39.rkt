#lang racket

(require racket/trace)

(define (cont-frac n d k)
  (define (next i)
    (- i 1))
  (define (iter i result)
    (if (= i 0)
        result
        (iter (next i)
              (/ (n i)
                 (+ (d i) result)))))
  ;(trace iter)
  (iter (next k) (/ (n k) (d k))))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  
  (define (d i)
     (- (* i 2) 1))
  (cont-frac n d k))

(tan-cf 10.0 100)
(tan 10)