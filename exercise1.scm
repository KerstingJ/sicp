#| Exercise 1.1: Below is a sequence of expressions. What is
the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in
the order in which it is presented. |#


10 ;--> 10
(+ 5 3 4) ;--> 12
(- 9 1) ;--> 8
(/ 6 2) ;--> 3
(+ (* 2 4) (- 4 6)) ;--> (+ 8 (-2)) --> 6
(define a 3) ;--> ;Value: a
(define b (+ a 1)) ;--> ;Value: b
(+ a b (* a b)) ;--> 19
(= a b) ;--> #f

(if 
    (and 
        (> b a) ;--> #t
        (< b (* a b)) ;--> #t
    ) ;--> #t
b a) ;--> b --> 4 


(cond
    ((= a 4) 6) ;If a is 4 return 6
    ((= b 4) (+ 6 7 a)) ;if b is 4 return 16
    (else 25) ;otherwise return 25
) ;--> 16

; add b to 2 if b > a otherwise add a
(+ 2 (if (> b a) b a)) ;--> (+ 2 b) --> 6

(* 
    (cond 
        ((> a b) a) ;--> predicate is #f
        ((< a b) b) ;--> predicate is #t evaluates to the value of b: 4
        (else -1) ;--> this is never reached
    )
    (+ a 1) ;--> 4
) ;--> (* 4 4) --> 16


#| Exercise 1.2: Translate the following expression into prefix |#

( / 
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7))
)

#| Exercise 1.3: Define a procedure that takes three numbers
as arguments and returns the sum of the squares of the two
larger numbers |#
(define (square x)(* x x))
(define (sums x y z)
    (cond
        ; If x is lowest
        ((and (> y x) (> z x)) (+ (square y) (square z)))
        ; if y is lowest
        ((and (> x y) (> z y)) (+ (square x) (square z)))
        ; if z i lowest
        ((and (> x z) (> y z)) (+ (square y) (square x)))
    )
)


#| Exercise 1.4:Observe that our model of evaluation allows 
for combinations whose operators are compound expressions.
Use this observation to describe the behavior of the 
following procedure: |#
(define(a-plus-abs-bab)
((if(> b 0) + -) a b))

; If b is > 0 our operand will evaluate to addition,
; otherwise it will evaluate to subtraction

#| Exercise 1.5: Ben Bitdiddle has invented a test to determine
whether the interpreter he is faced with is using applicative-
order evaluation or normal-order evaluation. He defines the
following two procedures: |#

; (define (p) (p))
; (define (tes t x y)
;     (if (= x 0) 0 y)
; ) 

; then he evaluates the expression

; (test 0 (p))

#| What behavior will Ben observe with an interpreter that
uses applicative-order evaluation? What behavior will he
observe with an interpreter that uses normal-order evalu-
ation? Explain your answer. 
(Assume that the evaluation-rule for the special form if 
is the same whether the in-terpreter is using normal or 
applicative order: the predicate expression is evaluated first,
and the result determineswhether to evaluate the consequent 
or the alternative ex-pression.) |#


#| ANSWER: 
Normal order will skip over the `(p)` letting the expression evaluate
to `0` while applicative order will try to full expand everything
before applying the operators ending in an expression that can never resolve|#



(define (abs x)
    ((if (> x 0) + -) 0 x)
)

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess))
)

; what makes a guess good enough? 
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)

; Using newtons method to find a better guess
; for sqrt y of x
; we can get closer to the actual root 
; by taking the average of y and x/y 

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)
    )
)

; we can start guessing any sqrt from 1
(define (sqrt x)
    (sqrt-iter 1.0 x)
)

(sqrt 2) ;Value: 1.4142156862745097

#| 
    Exercise 1.6 

    what happens if we define our own if using cond and applied it to newtons method
    (define (new-if predicate true-clause else-clause)
        (cond 
            (predicate true-clause)
            (else else-clause)
        )
    )

    This actually works on my machine... 
    in an applicative environment the condition would be continually expanded
    until you hit your recursion depth and error out because it could never
    resolve the value of a recursive clause.
    `if` is a special form and doesn't need to expand fully to evaluate and that
    lets us avoid that problem
|#

#| 
    Exercise 1.7
    How does out current form of `good-enough?` operate with large and small numbers?
    rewrite it to use a percentage of change

    for small numbers it is incredibly easy to hit the threshold of 0.001 making our 
    implementation potentially very inacurate likewise it is not very efficient for
    large numbers because that small of change may never occur.
|#

; a better implementation

(define (good-enough? guess x)
    (<= (abs (- (improve guess x) guess )) (* guess 0.001))
)


(sqrt 200)

#|  Excercise 1.8
    a cube-root procedure
    using newtons method for improving an estimation of
    the root of a cube |#
    
(define (cube-root x)
    (define (cbrt-improve guess x)
        #| Improve our guess using newtons formula
           x/y^2 + 2y
          ------------
               3
          where y is our guess and x is our operand |#

        (/
            (+ (/ x (square guess)) (* 2 guess))
            3
        )
    )

    (define (cbrt-good-enough? guess x)
        #| When the change of an improved guess 
            is less than or equal to .1% of our guess |#
        (<= (abs (- (cbrt-improve guess x) guess )) (* guess 0.0001))
    )

    (define (cbrt-iter guess x)
        ; While out guess is not good enough keep improving
        (if (cbrt-good-enough? guess x)
            guess
            (cbrt-iter (cbrt-improve guess x) x)
        )
    )
    (cbrt-iter 1.0 x)
)

(cube-root (* 5 5 5)) ;--> 5.000038

#|  Excercise 1.11 
    A function f is defined by 
    if n < 3, f(n) = n
    else f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

    write an iterative procedure and a recursive procedure
|#

; recursive
(define (func-11-rec n)
    (if (< n 3)
        n
        (+ 
            (func-11-rec (- n 1))
            (* 2 (func-11-rec (- n 2)))
            (* 3 (func-11-rec (- n 3)))
        )
    )
)

; iterative
; This is very similar to the iterative fib
; where you keep track of your previous value and work your way up
(define (func-11-iter n)
    (define (func-iter i a b c)
        (cond 
            ((< i 0) i)
            ((= i 0) a)
            (else (func-iter (- i 1) b c (+ c (* 2 b) (* 3 a))))
        )
    )

    (func-iter n 0 1 2)
)


#|
    excercise 1.12
    pacals triable recursive function

    1
    1   1
    1   2   1
    1   3   3   1
    1   4   6   4   1
    1   5   10  10  5   1

|#

(define (pascal row col)
    (cond 
        ((or (< row 1) (< col 1)) 0)
        ; If its the first row
        ((= row 1) 1)
        ; If its the first col
        ((= col 1) 1)
        ; if its the last col
        ((= col row) 1)
        ; any other location in the triangle
        (else (+
            (pascal (- row 1) (- col 1))
            (pascal (- row 1) col)
        ))
    )
)

#|
    
|#