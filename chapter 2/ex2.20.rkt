#lang racket
(define (same-parity . items)
  (let ((even-odd (even? (car items))))
    (define (get-parity values)
      (cond ((null? values) null)
            ((not (xor even-odd (even? (car values))))
             (cons (car values) (get-parity (cdr values))))
            (else (get-parity (cdr values))))) 
    (get-parity items)))

(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5)
          
    