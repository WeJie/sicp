#lang racket
(require racket/draw)

(define target (make-bitmap 30 30)) ; A 30x30 bitmap
(define dc (new bitmap-dc% [bitmap target]))
(send dc draw-line
      0 30   ; Start at (0, 30), the bottom-left corner
      30 0)  ; and draw to (30, 0), the top-right corner
(send target save-file "box.png" 'png)

(define (make-vect x-coordinate y-coordinate)
  (cons x-coordinate y-coordinate))

(define (xcor-vect vector)
  (car vector))

(define (ycor-vect vector)
  (cdr vector))

(define (add-vect v1 v2)
  (cons (+ (xcor-vect v1) (ycor-vect v2))
        (+ (xcor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (cons (- (xcor-vect v1) (ycor-vect v2))
        (- (xcor-vect v1) (ycor-vect v2))))

(define (scale-vect vector s)
  (cons (* (xcor-vect vector) s) (* (ycor-vect vector) s)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (frame-coord-map frame)
  (lambda (v)
          (add-vect (origin-frame frame)
                    (add-vect (scale-vect (xcor-vect v)
                                          (edge1-frame frame))
                              (scale-vect (ycor-vect v)
                                          (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
          (for-each
           (lambda (segment)
                   (draw-line
                    ((frame-coord-map frame) (start-segment segment))
                    ((frame-coord-map frame) (end-segment segment))))
           segment-list)))

(define origin (make-vect 0 0))
(define x-vect (make-vect 0 1))
(define y-vect (make-vect 1 0))
(define designated-frame (make-frame (origin x-vect y-vect)))

; a
(define outline
  (list
   (make-segment (make-vect 0 0) (make-vect 0 1))
   (make-segment (make-vect 0 1) (make-vect 1 1))
   (make-segment (make-vect 1 1) (make-vect 1 0))
   (make-segment (make-vect 1 0) (make-vect 0 0))))

((segments->painter outline) designated-frame)

; b 
(define X
  (list
   (make-segment (make-vect 0 1) (make-vect 1 0))
   (make-segment (make-vect 0 0) (make-vect 1 1))))
((segments->painter X) designated-frame)

; c
(define diamond
  (list
   (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
   (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
   (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
   (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

((segments->painter diamond) designated-frame)

; d
(define wave
  (list
   ;; Left leg
   (make-segment (make-vect 0.3 1) (make-vect 0.4 0.6))
   (make-segment (make-vect 0.4 1) (make-vect 0.5 0.7))
   ;; Right leg
   (make-segment (make-vect 0.6 1) (make-vect 0.5 0.7))
   (make-segment (make-vect 0.7 1) (make-vect 0.6 0.6))
   ;; Torso
   (make-segment (make-vect 0.4 0.6) (make-vect 0.4 0.4))
   (make-segment (make-vect 0.6 0.6) (make-vect 0.6 0.4))
   ;; Left arm
   (make-segment (make-vect 0.4 0.4) (make-vect 0.2 0.35))
   (make-segment (make-vect 0.2 0.35) (make-vect 0.2 0.25))
   (make-segment (make-vect 0.2 0.25) (make-vect 0.45 0.3))
   ;; Right arm
   (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.45))
   (make-segment (make-vect 0.8 0.45) (make-vect 0.8 0.35))
   (make-segment (make-vect 0.8 0.35) (make-vect 0.55 0.3))
   ;; Neck
   (make-segment (make-vect 0.45 0.3) (make-vect 0.45 0.25))
   (make-segment (make-vect 0.55 0.3) (make-vect 0.55 0.25))
   ;; Head
   (make-segment (make-vect 0.45 0.25) (make-vect 0.425 0.25))
   (make-segment (make-vect 0.425 0.25) (make-vect 0.425 0.05))
   (make-segment (make-vect 0.425 0.05) (make-vect 0.575 0.05))
   (make-segment (make-vect 0.575 0.05) (make-vect 0.575 0.25))
   (make-segment (make-vect 0.575 0.25) (make-vect 0.55 0.25))))

((segments->painter wave) designated-frame)