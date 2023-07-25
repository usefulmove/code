#lang racket
(provide (all-defined-out))

#|
    Cuban - a general purpose util library
|#
(require racket/contract)

;;; list operations ;;;

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
; snd :: [T] -> T
(define (snd lst) (cadr lst))

; swap :: [T] -> integer -> integer -> [T]
(define (swap lst i j)
    (define tmp (list-ref lst i))
    (define out (list-set lst i (list-ref lst j)))
    (list-set out j tmp))

; display-list :: [T] -> null  ( side effect only )
(define (display-list lst)
    (for-each displayln lst))

; find index of first instance of an item in a list (returns -1 if not found)
; list-index :: [T] -> T -> integer
(define/contract (list-index item lst [index 0])
    (-> exact-integer? (listof exact-integer?) exact-integer?)
    (if (null? lst)
        -1
        (if (equal? item (car lst))
            index
            (list-index item (cdr lst) (add1 index)))))



; atom? :: object -> boolean
(define (atom? obj)
    (and (not (null? obj))
         (not (pair? obj))))



; fixed-point convergence function
; converge-fixed-point :: (number -> number) -> number -> number
(define/contract (converge-fixed-point f guess)
    (-> (-> number? number?) number? number?)
    (define (close-enough? a b)
        (define epsilon 0.000000001)
        (> epsilon (abs (- a b))))
    (define (improve-guess guess)
        (cond
            [(close-enough? guess (f guess)) (f guess)]
            [else (improve-guess (f guess))]))
    (improve-guess guess))



;;; debug helper functions ;;;

; debug-print :: T -> null  ( side effect only )
(define-syntax-rule (debug-print var)
  (begin (displayln (list 'debug: 'var 'is var))))

; debug-args
(define (debug-args . args)
    (for-each displayln args))
