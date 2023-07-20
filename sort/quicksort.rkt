#lang racket

(define (swap seq i j)
    (define tmp (list-ref seq i))
    (define out (list-set seq i (list-ref seq j)))
    (list-set out j tmp))

; QuickSort algorithm
(define (qsort lst)
    (cond
        [(empty? lst) empty]
        [(equal? 1 (length lst)) lst]
        (define pivot (car lst))  ; choose pivot
        (define current (swap lst 0 (- (length lst) 1))) ; move pivot to the end
        (define ins 0)  ; insertion index
        (TODO)
    )
)

; playground
(define test (list 9 3 7 2 1 5 4 6 8))

