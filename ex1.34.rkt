#lang racket
(require racket/trace)
;(trace f)
(define (f g)
  ;(trace g)
  (g 2))

(define (square n)
  (* n n))

(f square)
; 4

(f (lambda (z) (* z (+ z 1))))

(f f)
