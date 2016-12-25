#lang racket
(define (tri row col)
  (if (or (= row 0) (= col 0))
      1
      (+ (tri (- row 1) col) (tri row (- col 1)))))

;(tri 2 2)

; 递归求阶乘
(define (factorial n)
  (fact-iter 1 n))

(define (fact-iter product count)
  (if (< count 1)
      product
      (fact-iter (* product count) (- count 1))))

; Pascal's Triangle
(define (pascal row col)
  (/ (factorial row)
     (* (factorial col)
        (factorial (- row col)))))

(require racket/trace)
(pascal 3 2)