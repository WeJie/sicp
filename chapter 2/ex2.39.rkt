#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse sequence)
  (fold-right (lambda (x y)
                (append y (list x))) null  sequence))

(reverse (list 1 2 3))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) `() sequence))

(reverse-left (list 1 2 3))