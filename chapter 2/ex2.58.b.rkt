#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (cond ((not (pair? x)) false)
        ((eq? (cadr x) '+) true) ; 1 2 3
        (else (and (not (null? (cdddr x)))
                   (eq? (cadr x) '*)
                   (eq? (cadddr x) '+))))) ; 4

(define (addend s)
  (if (eq? (cadr s) '*)
      (list (car s) (cadr s) (caddr  s))
      (car s)))

(define (augend s)
  (cond ((null? (cdddr s)) (caddr s))
        ((eq? (cadr s) '*) (if (pair? (cdddr s))
                               (cdddr s)
                               (cadddr s)))
        (else (if (pair? (cddr s))
                  (cddr s)
                  (caddr s)))))

(define (product? x)
  (cond ((not (pair? x)) false)
        ((null? (cdddr x)) (eq? (cadr x) '*))
        (else (eq? (cadddr x) '*))))

(define (multiplier p) (car p))
(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cddr p)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
      (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                     (multiplicand exp))))
    (else
     (error "unknown expression type -- DERIV" exp))))

(require racket/trace)
;(trace product?)
;(trace make-product)
;(trace multiplicand)
;(trace sum?)

(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + x + x + x + x) 'x)

(deriv '(x * x) 'x)
(deriv '(x * x * x) 'x)