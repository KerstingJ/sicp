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
    ((= b 4) (+ 6 7 a)) ;if b is 4 return 17
    (else 25) ;otherwise return 25
) ;--> 17

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


