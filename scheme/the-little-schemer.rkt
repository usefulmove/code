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

; function composition
(define ())

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


(define (display-list seq)
    (for-each displayln seq))
(display-list seq)

; filter map reduce
(define (square a) (* a a))
(apply + (map square (filter odd? seq)))

(- (sqrt 10))
((compose - sqrt) 10)

(define (f seq) (filter odd? seq))
(define (m seq) (map square seq))
(define (r seq) (apply + seq))
(define fmr (compose r m f))
(fmr seq)


(define (square-root a)  ; TODO
    (define espilon 0.000001)
    (define guess 1.0)
    (cond
        [(< a 0) (error "square-root of negative number")]
        [(< (abs (- (* guess guess) a)) espilon) guess]
        [else (set! guess (/ (+ guess (/ a guess)) 2))]
    )


(define seq '(3 1 2 5 4))

(define (swap seq i j)
    (define tmp (list-ref seq i))
    (define out (list-set seq i (list-ref seq j)))
    (list-set out j tmp))

(for-each displayln seq)
(for-each displayln (swap seq 1 2))