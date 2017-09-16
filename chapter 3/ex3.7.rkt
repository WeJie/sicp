#lang racket
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request --MAKE--ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)

(define (make-joint acc password joint-password)
  (define (dispatch key m)
    (cond ((not (eq? key joint-password)) 
           (error "Incorrect password -- MAKE-JOINT"))
          ((eq? m 'withdraw) (acc password 'withdraw))
          ((eq? m 'deposit)  (acc password 'deposit))
          (else (error "Unknown request -- MAKE-JOINT" m))))
  dispatch)
 
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))
 
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 10)
