#lang racket
(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (get-value record)
  (cdr record))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define test-tree (make-tree
                     (make-record 2 "value2")
                     (make-tree (make-record 1 "value1") '() '())
                     (make-tree (make-record 3 "value3") '() '())))

(define (lookup given-key ordered-record-tree)
  (cond ((null? ordered-record-tree) false)
        ((= given-key (key (entry ordered-record-tree))) true)
        ((< given-key (key (entry ordered-record-tree)))
         (lookup given-key (left-branch ordered-record-tree)))
        ((> given-key (key (entry ordered-record-tree)))
         (lookup given-key (right-branch ordered-record-tree)))))

(require racket/trace)
(trace lookup)
(lookup 5 test-tree)
