#lang racket

; is ths an atom?
(define (atom? o)
    (and
        (not (pair? o))
        (not (null? o))))

(atom? (quote ()))

; does list contain only atoms?
(define (lat? l)
    (cond
    [(null? l) #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else #f]))


; playground
(define seq '(3 2 1 5 4))
(car seq)  ; legacy function name ("contents of address register")
(cdr seq)  ; legacy function name ("contents of decrement register")

(first seq)
(rest seq)

(define (init seq) (reverse (cdr (reverse seq))))

(init seq)
(last seq)

(cons 0 seq)  ; "construct"


(foldl + 0 (range 1 11))
(apply + (range 1 11))


(define (list-of-atoms? seq)
    (if (null? seq)
        #t
        (and
            (atom? (car seq))
            (list-of-atoms? (cdr seq)))))

