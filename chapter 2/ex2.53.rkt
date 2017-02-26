#lang racket
(list 'a 'b 'c)

(list (list 'george))
(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shose) (blue socks)))

(memq 'red '(red shose blue socks))

(pair? (cdr '(a short list)))
