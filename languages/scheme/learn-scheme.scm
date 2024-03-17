#lang racket


#| definitions and evalutation - S-expressions |#

(/ (- (sqrt 5) 1) 2)  ; 0.6180339887498949

(* pi (sqr 6))  ; 113.09733552923255

(define radius 6)
(* pi (sqr radius))

(define square (lambda (n) (* n n)))
(square 16)

(define calculate-area (lambda (r) (* pi (square r))))
(calculate-area radius)


#| lists |#

'(3 1 2)
(list 3 1 2)

(define lst '(3 1 2 5 4))
lst  ; '(3 1 2 5 4)

(define snd (lambda (lst) (first (rest lst))))
(define snd (lambda (lst) (car (cdr lst))))
(define snd (lambda (lst) (cadr lst)))

(snd lst)  ; 1

(null? lst)  ; #f
(null? '())  ; #t

(length lst)  ; 5

(reverse lst)  ; '(4 5 2 1 3)

(cons 0 lst)  ; '(0 3 1 2 5 4)

(append lst lst)  ; '(3 1 2 5 4 3 1 2 5 4)


#| conditional expressions |#

; if, cond

(if (zero? 1)
    "this"
    "that")

(cond ((zero? 1) "this")
      ((equal? 2 2) "that")
      (else "the other thing"))


#| recursion |#

; factorial, fibonacci



#| higher-order functions |#

; map, filter, reduce, any?, all?
(todo)


#| closures |#

(todo)


#| koans |#

(= _ 8)

(= (+ 3 2) _)

(= 8 (* _ 2 2))

(= _ (+ (sqr 3) (sqr 4)))

(equal? '(_) (list 3 1 2))

(equal? (car (list 3 1 2)) _)

(equal? _ (cdr '(3 1 2)))

(equal? (cons 0 (list 3 1 2)) _)

(equal? 5 (_ '(3 1 2 4 6)))

(equal? 5 (_ '(3 1 2 5)))

(define add2 (lambda (n) _))

(define (cube n) _)
