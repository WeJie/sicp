#lang racket
(define (make-account balance password)
  (define error-account 0)
  (define error-boundary 7)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define call-the-cops "Too many bad password, cops is coming")
  
  (define (checkout-error-boundary)
    (if (>= error-account error-boundary)
        call-the-cops
        "Incorrect password"))

  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request --MAKE--ACCOUNT" m))))
  
  (define (verify-before-dispatch pwd m)
    (if (eq? pwd password)
        (begin
          (set! error-account 0)
          (dispatch m))
        (begin
          (set! error-account (+ error-account 1))
          (display error-account)
          (lambda (amount) (checkout-error-boundary)))))
  
  verify-before-dispatch)

(define acc (make-account 100 'hello))

((acc 'hello 'withdraw) 40)

((acc 'some-other-password 'deposit) 10)
((acc 'hello 'withdraw) 40)
((acc 'some-other-password 'deposit) 10)
((acc 'some-other-password 'deposit) 20)
((acc 'some-other-password 'deposit) 30)
((acc 'some-other-password 'deposit) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 60)
((acc 'some-other-password 'deposit) 70)
((acc 'some-other-password 'deposit) 80)

