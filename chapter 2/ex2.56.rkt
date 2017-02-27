#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; ** 2 n
(define (** base exponent)
  (expt base exponent))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponent base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((and (number? base) (number? exponent)) (** base exponent))
        (else (list '** base exponent))))

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((exponentiation? exp)
     (make-product
      (make-product (exponent exp) (make-exponent (base exp) (- (exponent exp) 1)))
      (deriv (base exp) var)))
    (else
     (error "unknown expression type -- DERIV" exp))))

(deriv '(** x 8) 'x)
