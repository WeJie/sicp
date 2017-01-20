#lang racket
(define (numer x) (car x)) 
  
(define (denom x) (cdr x)) 
  
(define (print-rat x)
  (newline) 
  (display (numer x)) 
  (display "/") 
  (display (denom x))) 

(define (make-rat n d) 
  (let ((g ((if (< d 0) - +) (gcd n d)))) 
    (cons (/ n g) (/ d g)))) 
  
(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))


