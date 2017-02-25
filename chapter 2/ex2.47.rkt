#lang racket
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-vect frame)
  (car frame))

(define (edge1-vect frame)
  (car (cdr frame)))


(define (edge2-vect frame)
  (cdr (cdr frame)))

(make-frame 1 2 3)

(origin-vect (make-frame 1 2 3))
(edge1-vect (make-frame 1 2 3))
(edge2-vect (make-frame 1 2 3))