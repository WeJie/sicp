#lang racket
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define empty-board null)

(define (safe? k positions)
  (define (iter-sale kth-position rest-positions i)
    (if (null? rest-positions)
        true
        (if (or (= kth-position (car rest-positions))
                (= kth-position (- (car rest-positions) i))
                (= kth-position (+ (car rest-positions) i)))
            false
            (iter-sale kth-position (cdr rest-positions) (+ i 1)))))
  (iter-sale (car positions) (cdr positions) 1))
        
(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter  
         (lambda (positions) (safe? k positions))
         (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)