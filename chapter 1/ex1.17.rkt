#lang racket
(define (multi a b)
  (if (= b 0)
      0
      (+ a (multi a (- b 1)))))

(define (double n)
  (+ n n))

(define (hive n)
  (/ n 2))

(define (fast-multi a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-multi a (hive b))))
        ((odd? b) (+ a (fast-multi a (- b 1))))))

  
(define (fast-multi-iter a b)
  (multi-iter a b 0))

(define (multi-iter primary counter product)
  (cond ((= counter 0) product)
        ((even? counter) (multi-iter (double primary) (hive counter) product))
        ((odd? counter) (multi-iter primary (- counter 1) (+ product primary)))))
        
(fast-multi 3 4)
(fast-multi-iter 3 4)