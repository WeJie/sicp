#lang racket
(define (square x)
  (* x x))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
    ((not (pair? tree))
     (if (odd? tree) (square tree) 0))
    (else (+ (sum-odd-squares (car tree))
             (sum-odd-squares (cdr tree))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
             (if (even? f)
                 (cons f (next (+ k 1)))
                 (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
    ((predicate (car sequence))
     (cons (car sequence)
           (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3  4 5))
; 15

(accumulate * 1 (list 1 2 3  4 5))
; 120

(accumulate cons null (list 1 2 3 4 5))
; (1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)
; (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

(define (sum-odd-squares2 tree)
  (accumulate + 
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4)) 5))

(define (even-fibs2 n)
  (accumulate cons
              null
              (filter even? (map fib 
                                 (enumerate-interval 0 n)))))
(even-fibs2 5)

(define (list-fib-squares n)
  (accumulate cons
             null
             (map square 
                  (map fib
                       (enumerate-interval 0 n)))))
(list-fib-squares 10)


(define (product-of-squares-of-odd-elements sequences)
  (accumulate * 
              1
              (map square
                   (filter odd? sequences))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

;(define (salary-of-highest-paid-programmer records)
;  (accumulate max
;              0
;              (map salary
;                   (filter programmer? records))))
