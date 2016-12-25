#lang racket
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((= (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))
;
(define (prime? n)
  (= (smallest-divisor n) n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2)))); n 是偶数时，计算步骤 = 以 2 为底 n 的对数
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      (+ n 2)))
;
(define (search-for-primes start-num)
  (find-odd-prime start-num 3));
         
(define (find-odd-prime n counter)
  (cond ((= counter 0) (display "get 3 prime\n"))
        ((fast-prime? n 10)
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

