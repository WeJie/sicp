#lang racket
(define f-ltr
  (let ((state 1))
    (lambda (x)
      (begin
        (set! state (* state x))
        state))))

(define f-rtl
  (let ((state 1))
    (lambda (x)
      (begin
        (set! state (* state x))
        state))))

(+ (f-ltr 0) (f-ltr 1))
(+ (f-rtl 1) (f-rtl 0))


    