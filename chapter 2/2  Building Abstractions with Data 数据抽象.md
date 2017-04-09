# 2  数据抽象



> "We now  come to  the decisive step of mathematical abstraction: we forget about wat the symbols stand for, …[The mathematician] need not be idle; there are many operations which he may carry out with these symbols, without ever having to look at the things they stand for." 
>
> —— Hermann Weyl，The Mathematical Way of Thinking
>



The general technique of isolating the parts of a program that deal with how data objects are represented from the parts of a program that deal  with how data objects are used is a powerful design methodology called data abstraction.

将数据的表示和数据的使用分离，是一种有效的设计方法，称为数据抽象。



- 如何使用数据抽象在程序的不同部分之间建立抽象屏障，以克服复杂性。
- 关键在于提供 **glue** 机制，使数据对象可以组合成更加复杂的数据对象。
- 组合数据的一个关键思想就是 closure（闭包）。
- 另一个关键思想就是组合数据可以作为通用接口，以 mix-and-match 方式组合程序 modules 。




## 2.1 Introduction to Data Abstraction 



在 1.1.8 节中，我们注意到使用一个 procedure 作为元素创建一个更加复杂的 procedure，不仅可以看作是一些特定操作的集合，也可以看做是一种过程抽象。只要 procedure 本身的行为是一致的，内部的细节可以任意替换。



数据抽象是一种方法论，帮助我们将如何使用 compound data object 和如何使用更基本的 data object 构造 compound data object 这两种行为隔离开。

这两种行为分别称为 selectors 和 constructos。



abstract（抽象） — concrete（具象）



### 2.1.1 Example： Arithmetic Operation for [Rational Numbers](https://www.wikiwand.com/en/Rational_number)



这里使用了一个强大的策略 [wishful thinking](https://www.wikiwand.com/en/Wishful_thinking#/Representations_of_environment)。 

假设我们有 constructor —— (maker-rat <n> <d>)，selector —— (number <x>)、(denom <x>)

我们还不知道该如何表示有理数，或者说如何实现 procedure numer，denom 和 make-rat。但我们确实需要这三个 procedure，并可以依据下面的规则得到 add，subtract，multiply，divide 和 test equality。
$$
\frac {n_1} {d_1} - \frac {n_2}{d_2} = \frac {n_1d_2 - n_2d_1}{d_1d_2}
$$

$$
\frac {n_1}{d_1} \cdot \frac {n_2}{d_2} = \frac {n_1n_2}{d_1d_2}
$$

$$
\frac {n_1/d_1}{n_2/d_2} = \frac {n_1d_2}{d_1n_2}
$$

$$
\eqalign {\frac {n_1}{d_1} = \frac {n_2}{d_2}， & if  & and & only & if & n_1d_2 = n_2d_1}
$$

```lisp
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
               
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer x) (denom y))))
```

最后我们需要 glue numerator 和 denominator 来构造有理数 



#### Pairs

通过 primitive data 表示有理数

需要的 glue：

- cons
- car
- cdr

```lisp
; 构建 Pairs
(define x (cons 1 2))

(car x)
; 1

(cdr x)
; 2
```



> Data objects constructed from pairs are called list-structured data.



#### Representing rational numbers

```lisp
(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half
  (make-rat 1 2))
(print-rat one-half)
```



通过 gcd 约分有理数

```lisp
(define (make-rat n d)
  (let ((g (gcd n d)))
       (cons (/ n g) (/ d g))))

(define one-third (make-rat 1 3))

(print-rat (add-rat one-third one-third))
```



### 练习

2.1 定义一个更好的 make-rat ，可以处理正/负数的参数。make-rat 需要规范符号。如果有理数是正数，则分子和分母都是正数。如果有理数是负数，只有分子是负数。

$\frac n b = \frac {-n} {-b}$

$\frac {-n} b = \frac {-n} b$

$\frac n {-b} = \frac {-n} {b}$

```lisp
(define (numer x) (car x)) 
  
(define (denom x) (cdr x)) 
  
(define (print-rat x)
  (newline) 
  (display (numer x)) 
  (display "/") 
  (display (denom x))) 

(define (make-rat n d) 
  (let ((g ((if (< d 0) - +) (gcd n d)))) 
    (cons (/ n g) (/ d g)))) 
  
(print-rat (make-rat 1 2)) ; 2/3 
(print-rat (make-rat -1 2)) ; -2/3 
(print-rat (make-rat 1 -2)) ; -2/3 
(print-rat (make-rat -1 -2)) ; 2/3 
  
```

