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

(define matrix-test (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9) (list 6 7 8 9)))
(define vector-test (list 1 2 3 4))
(define w (list 1 2 3 4))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product vector-test w)

(define (matrix-*-vector m v)
  (map (lambda (items)
         (dot-product items v)) m))

(matrix-*-vector matrix-test vector-test)

(define (transpose mat)
  (accumulate-n cons null mat))

(transpose matrix-test)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (items)
           (matrix-*-vector cols items)) m)))

(matrix-*-matrix matrix-test matrix-test)
