#lang racket
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-rectangle length-segment width-segment)
  (cons length-segment width-segment))

(define (length-r r)
  (car r))

(define (width-r r)
  (cdr r))

(define (square x)
  (* x x))

(define (segment-length segment)
  (sqrt (+ (square (- (x-point (start-segment segment))
                      (x-point (end-segment segment))))
           (square (- (y-point (start-segment segment))
                      (y-point (end-segment segment)))))))

(define (rectangle-perimeter rectangle)
  (* (+ (segment-length (car rectangle))
        (segment-length (cdr rectangle)))
     2))

(define (rectangle-area rectangle)
  (* (segment-length (car rectangle))
     (segment-length (cdr rectangle))))

;(define (make-rectangle length-segment width-segment)
;  (cons length-segment width-segment))
;
;(define (length-r r)
;  (car r))
;
;(define (width-r r)
;  (cdr r))


;(define (get-slope first-point second-point)
;  (/ (- (x-point second-point)
;        (x-point first-point))
;     (- (y-point second-point)
;        (y-point first-point))))

;(define (make-rectangle first-point second-point third-point)
;  (let ((length-slope (get-slope first-point second-point))
;        (width-slope (get-slope second-point third-point)))
;    (if (= (* length-slope width-slope) -1)
;        (cons (make-segment first-point second-point) (make-segment second-point third-point))
;        (display "this three point can't struct a rectangle"))))

;(define (make-rectangle lenght-segment height-point)