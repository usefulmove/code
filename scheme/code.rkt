#lang racket

;; misc

; init :: [T] -> [T]
(define (init lst)
    (if (null? lst)
        '()
        (reverse (cdr (reverse lst)))))

(init '(3 1 2 5 4))  ; '(3 1 2 5)


; macros
(define-syntax-rule (head lst) (car lst))
(define-syntax-rule (tail lst) (cdr lst))

(define-syntax-rule (1st lst) (car lst))
(define-syntax-rule (2nd lst) (car (cdr lst)))
(define-syntax-rule (end lst) (last lst))


; display-list :: [T] -> null  ( side effects only )
(define (display-list lst)
    (for-each displayln lst))

(display-list '(3 1 2))
; 3
; 1
; 2


; swap :: [T] -> Integer -> Integer -> [T]
(define (swap lst i j)
    (define tmp (list-ref lst i))
    (define out (list-set lst i (list-ref lst j)))
    (list-set out j tmp))

(swap '(3 1 2 5 4) 1 3)  ; '(3 5 2 1 4)


; square-root :: Number -> Number
(define (square-root n)  ; example only (Babylonian method) - use (sqrt n) primative
    (define epsilon 0.000000001)
    (define (improve-guess guess)
        (cond
            [(> epsilon (abs (- n (* guess guess)))) guess]
            [else (improve-guess (/ (+ guess (/ n guess)) 2))]))
    (cond
        [(< n 0) (error "square-root of negative number")]
        [else (improve-guess 1.0)]))

(square-root 618)  ; 24.859605789312106


;; functional programming

; higher-order functions
(define seq '(1 2 3 4 5 6 7 8))
(define (square a) (* a a))

(filter odd? seq)  ; '(1 3 5 7)
(map square seq)  ; '(1 4 9 16 25 36 49 64)
(foldl + 0 seq)  ; 36


; currying
; add :: Integer -> (Integer -> Integer)
(define (add a)
    (lambda (b) (+ a b)))

((add 2) 3)  ; 5

(define add2 (add 2))
(add2 3)  ; 5


; function composition
(define seq '(1 2 3 4 5 6 7 8))
(define (square a) (* a a))

(apply + (map square (filter odd? seq)))  ; 84

(define (f lst) (filter odd? lst))
(define (g lst) (map square lst))
(define (h lst) (apply + lst))
(define composed-f (compose h g f))
(composed-f seq)  ; 84


; threading
(require threading)
(~> seq f g h)  ; 84
