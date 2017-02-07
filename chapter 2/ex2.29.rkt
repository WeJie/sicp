#lang racket
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile) (+ (total-weight (branch-structure (left-branch mobile)))
                           (total-weight (branch-structure (right-branch mobile)))))
        (else mobile)))

(define (balanced? mobile)
  (let ((left  (left-branch mobile))
        (right (right-branch mobile))
        (left-structure (branch-structure (left-branch mobile)))
        (right-structure (branch-structure (right-branch mobile))))
    (if (pair? left-structure)
        (if (pair? right-structure)
            (and (balanced? left-structure) (balanced? right-structure))
            (if (balanced? left-structure)
                (= (* (branch-length left) (total-weight left-structure))
                   (* (branch-length right) right-structure))
                false))
        (if (pair? right-structure)
            (if (balanced? right-structure)
                (= (* (branch-length left) left-structure)
                   (* (branch-length right) (total-weight right-structure)))
                false)
            (= (* (branch-length left) left-structure)
               (* (branch-length right) right-structure))))))

 (define mobile (make-mobile (make-branch 10 25)
                             (make-branch 5 20)))

(left-branch mobile)
(right-branch mobile)
(branch-length (right-branch mobile))
(branch-structure (right-branch mobile))

(total-weight mobile)
(balanced? mobile)