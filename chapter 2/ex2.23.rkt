#lang racket
(define (for-each proc items)
  (if (null? items)
      (newline)
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))