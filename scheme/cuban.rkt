#lang racket
(provide (all-defined-out))

; head :: [T] -> T
(define head car)
; tail :: [T] -> [T]
(define tail cdr)
; init :: [T] -> [T]
(define (init lst)
    (if (null? lst)
        '()
        (reverse (cdr (reverse lst)))))
; last :: [T] -> T  (build-in)

; atom? :: object -> boolean
(define (atom? obj)
    (and (not (null? obj))
         (not (pair? obj))))

; display-list :: [T] -> null  ( side effect only )
(define (display-list lst)
    (for-each displayln lst))

; swap :: [T] -> Integer -> Integer -> [T]
(define (swap lst i j)
    (define tmp (list-ref lst i))
    (define out (list-set lst i (list-ref lst j)))
    (list-set out j tmp))

; fixed-point convergence
; converge-fixed-point :: (Number -> Number) -> Number -> Number
(define (converge-fixed-point f guess)
    (define (close-enough? a b)
        (define epsilon 0.000000001)
        (> epsilon (abs (- a b))))
    (define (improve-guess guess)
        (cond
            [(close-enough? guess (f guess)) (f guess)]
            [else (improve-guess (f guess))]))
    (improve-guess guess))
