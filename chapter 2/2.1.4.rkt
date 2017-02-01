#lang racket
(define (make-interval a b) (cons a b))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
       (make-interval (min p1 p2 p3 p4)
                      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
                    (error 'divisor)
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
       (div-interval one 
                     (add-interval (div-interval one r1)
                                  (div-interval one r2)))))

(define R1 (make-interval 4 6))
(define R2 (make-interval 6 8))

(par1 R1 R2)
(par2 R1 R2)


(define (make-center-percent center percent)
  (make-interval (* center (- 1 percent)) (* center (+ 1 percent))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (let ((percent-value (/ (- (upper-bound i) (lower-bound i)) 2)))
       (/ percent-value (- (upper-bound i) percent-value))))

(define PR1 (make-center-percent 4 0.5))
(define PR2 (make-center-percent 6 0.5))

(par1 PR1 PR2)
(par2 PR1 PR2)

