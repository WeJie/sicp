#lang racket
;(define (fast-expt b n)
;  (cond ((= n 0) 1)
;        ((even? n) (squre (fast-expt b (/ n 2))))
;        (else (* b (fast-expt b (- n 1))))))
;        
;(define (even? n)
;  (= remainder n 2) 0)

(define (square n)
  (* n n))

(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        ((odd? n) (expt-iter b (- n 1) (* b a)))))

(require racket/trace)
(trace expt-iter)
(fast-expt 2 4)