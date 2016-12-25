#lang racket
(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder  (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (fermat-iter (- n 1) n))

(define (fermat-iter count n)
  (cond ((= count 1) #t)
        ((= (expmod count n n) count) (fermat-iter (- count 1) n))
        (else #f)))
                                    
(require racket/trace)
(trace expmod)
(fermat-test 561)

