#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (cons (/ (+ (car (car segment)) (car (cdr segment))) 2)
        (/ (+ (cdr (car segment)) (cdr (cdr segment))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  )

(define start-point (make-point 1 2))
(define end-point (make-point 2 4))
(define segment (make-segment start-point end-point))
(define mid-point (midpoint-segment segment))
  
(print-point mid-point)