#lang racket
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt2 b n)
  (expt-iter b 1 n))

(define (expt-iter b product counter)
  (if (= counter 0)
      proudct
      (expt-iter (* b product) (- counter 1))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? b) (squre (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
