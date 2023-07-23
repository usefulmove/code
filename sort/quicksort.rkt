#lang racket

(define (swap seq i j)
    (define tmp (list-ref seq i))
    (define out (list-set seq i (list-ref seq j)))
    (list-set out j tmp))

; QuickSort algorithm
(define (qsort lst)
    (display "orig: ")
    (displayln lst)
    (cond
        [(empty? lst) empty]
        [(equal? 1 (length lst)) lst]
        [else
            (define pivot (car lst))  ; choose pivot
            (define current (swap lst 0 (sub1 (length lst)))) ; move pivot to the end
            (define ins 0)  ; insertion index
            (for ([i (in-range (sub1 (length current)))])
                (if (< (list-ref current i) pivot)
                    ; swap with insertion index and increment insertion index
                    (set! current (swap current i ins))
                    (set! ins (add1 ins))))
            ; swap pivot with insertion index
            (set! current (swap current ins (sub1 (length current))))
            (display "current: ")
            (displayln current)
            ; recursively sort left and right sublists
            (append (qsort (take current ins)) (list pivot) (qsort (drop current (add1 ins))))]))

; test case
(define test (list 3 1 2 5 4 0))
;(define test (list 9 3 7 2 1 5 4 6 8))

(qsort test)