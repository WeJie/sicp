# 3 Modularity, Objects, and States —— 模块化，对象和状态

[TOC]

> Even while it changes, it stands still.
>
> —— Heraclitus



We saw how primitive procedures and primitive data are combined to construct compund entities, and we learned that abstraction is vital in helping us to cope with the complexity of large systems. Bu these tools are not sufficient for designing programs. Effective program synthesis also requires organizational principles that can guide us in frmulating the overall design of a program.



One powerful design strategy, which is particularly appropriate to the construction of programs for modeling physical systems, is to base the structure of our programs on the structure of the system, we construct a coresponding computational object. for each system action, we define a symbolic operation in our computational model.

Our hope is using this stratege is that extending the model to accommodate new objects or new actions will require no strategic changes to the program, only the addition of the new symbolic analogs of those object or actons. 



Two organizational strategy, tow "world views":

- The first organizational strategy concentrates on objects, viewing a large system as **a collections of distinct objects** whose behaviors may change over time.
- An alternative organizational strategy concentrates on **the streams of information** that flow in the system, much as an electrical engineer views a signal-processing system.



**Environment model** help us concern with how a computaional object can change and yet maintain its identity.

**Delayed evaluation** help us decouple simulated time in our model from the order of the events that take place in the computer during evaluation.



## 3.1 Assignment and Local State



An object is said to "have state" if its behavior is influenced by its history.



### 3.1.1 Local State Variables

`(set! <name> <new-value>)`

```lisp
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

```

在上面 balance 是全局变量，下面改为本地变量。

```lisp

(define new-withdraw
  (let ((balance 100))
       (lambda (amount)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds"))))
```



Combining `set!` with local variables is the general programming technique we will use for constructing computational objects with local state.

通过 `set！` 给本地变量赋值。



```lisp
;; 
(define (make-withdraw balance)
  (lambda (amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds"))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request --MAKE--ACCOUNT" m))))
  dispatch)
```



##### Exercise 3.1

```lisp
; 闭包实现本地变量，及设置初始状态
(define (make-accumulator sum)
  (lambda (num)
    (set! sum (+ sum num))
    sum))

(define test (make-accumulator 5))
(test 10)
```



##### Exercise 3.2

```lisp
; 使用 let 设置本地变量
(define (make-monitored f)
  (let ((count 0))
    (lambda (message)
      (cond ((eq? message 'how-many-call?) count)
            ((eq? message 'reset-count) (set! count 0))
            (else (begin
                    (set! count (+ count 1))
                    (f message)))))))


(define s (make-monitored sqrt))

(s 100)
(s 'how-many-call?)
(s 100)
(s 100)
(s 'how-many-call?)
```



##### Exercise 3.3

Modify the `make-account`  procedure that it should take a password as an additional argument, as in

```lisp
(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
60

((acc 'some-other-password 'deposit) 50)
"Incorrect password"
```



```lisp
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; 通过闭包设置 password 
  (define (dispatch pwd m)
    (if (eq? pwd password)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request --MAKE--ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)

(define acc (make-account 100 'hello))

((acc 'hello 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
```



##### Exercise 3.4

Modify `make-account` of exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure `call-the-cops` 

```lisp
(define (make-account balance password)
  ; account 的本地状态，内部的全局变量
  (define error-account 0)
  (define error-boundary 7)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define call-the-cops "Too many bad password, cops is coming")
  
  (define (checkout-error-boundary)
    (if (>= error-account error-boundary)
        call-the-cops
        "Incorrect password"))

  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request --MAKE--ACCOUNT" m))))
  
  (define (verify-before-dispatch pwd m)
    (if (eq? pwd password)
        (begin
          (set! error-account 0)
          (dispatch m))
        (begin
          (set! error-account (+ error-account 1))
          (display error-account)
          (lambda (amount) (checkout-error-boundary)))))
  
  verify-before-dispatch)

(define acc (make-account 100 'hello))

((acc 'hello 'withdraw) 40)

((acc 'some-other-password 'deposit) 10)
((acc 'hello 'withdraw) 40)
((acc 'some-other-password 'deposit) 10)
((acc 'some-other-password 'deposit) 20)
((acc 'some-other-password 'deposit) 30)
((acc 'some-other-password 'deposit) 40)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 60)
((acc 'some-other-password 'deposit) 70)
((acc 'some-other-password 'deposit) 80)
```

