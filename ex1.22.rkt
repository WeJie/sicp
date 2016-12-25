#lang racket
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((= (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      (+ n 2)))

(define (search-for-primes start-num)
  (find-odd-prime start-num 3));
         
(define (find-odd-prime n counter)
  (cond ((= counter 0) (display "get 3 prime\n"))
        ((prime? n)
         (display n)
         (display " *** ")
         (find-odd-prime (next-odd n) (- counter 1)))
        (else (find-odd-prime (next-odd n) counter))))

(define (timed-prime-test n)
  (newline)
  (start-prime-test n (runtime)))

(define (runtime) (current-milliseconds))

(define (start-prime-test n start-time)
  (search-for-primes n)
  (report-prime (- (runtime) start-time)))

(define (report-prime elapsed-time)
  (display " time ")
  (display elapsed-time))

(timed-prime-test 1000)
(timed-prime-test 10000)
(timed-prime-test 100000)
(timed-prime-test 1000000)
