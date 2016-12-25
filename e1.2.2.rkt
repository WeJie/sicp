#lang racket
(require racket/trace)
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
;(trace fib)
;(fib 1)

; 递减迭代
(define (fib2 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 1)
      b
      (fib-iter (+ a b) a (- count 1))))

;(trace fib2)
(fib2 2)

;递增迭代
(define (fib3 n)
  (fib-iter2 1 0 0 n))

(define (fib-iter2 a b i count)
  (if (= count i)
      b
      (fib-iter2 (+ a b) a (+ i 1) count)))


;(fib3 5)

; 练习 1.11 递归实现
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; 练习 1.11 迭代 计数递减实现
(define (f2 n)
  (f-iter 2 1 0 n))

(define (f-iter a b c n)
  (if (= n 0)
      b
      (f-iter (+ a (* 2 b) (* 3 c))
              a
              b
              (- n 1))))

; f(3) = f(2) + 2f(1) + 3f(0)
; f(4) = f(3) + 2f(2) + 3f(1)
; f(5) = f(4) + 2f(3) + 3f(2)
; n = 3, a = f(2), b = f(1), c = f(0)
; n = 4,
; a' = f(3) = a + 2b + 3c 
; b' = f(2) = a
; c' = f(1) = b 

;(trace f)
;(f 4)

; 练习 1.11 迭代 计数递增实现
(define (f3 n)
  (f-iter2 2 1 0 0 n))

(define (f-iter2 a b c i n)
  (if (= i n)
      a
      (f-iter2 (+ a (* 2 b) (* 3 c))
              a
              b
              (+ i 1)
              n)))
;(f3 4)