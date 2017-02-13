#lang racket
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op
                        init
                        (accumulate cons null (map (lambda (x) (car x)) seqs)))
            (accumulate-n op
                          init
                          (accumulate cons null (map (lambda (x) (cdr x)) seqs))))))

(define test (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 test)