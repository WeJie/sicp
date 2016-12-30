#lang racket
(require racket/trace)

(define (cont-frac n d k)
  (define (next i)
    (+ i 1))
  (define (frac i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (frac (next i))))))
  ;(trace frac)
  (frac 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(define (cont-frac-iter n d k)
  (define (next i)
    (- i 1))
  (define (iter i result)
    (if (= i 0)
        result
        (iter (next i)
              (/ (n i)
                 (+ (d i) result)))))
  ;(trace iter)
  (iter (next k) (/ (n k) (d k))))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)