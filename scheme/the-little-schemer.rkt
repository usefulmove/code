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
(define (cube a) (* a a a))
(define (square a) (* a a))

(define (sum seq) (foldl + 0 seq))

(foldl
    (lambda (a acc) (+ acc (square a)))
    0
    (range 1 (+ 8 1)))

(sum (map square (range 1 (+ 8 1))))


(define golden-ratio (/ (- (sqrt 5) 1) 2))
golden-ratio


(define seq '(3 2 1 5 4))
(car seq)  ; legacy function name ("contents of address register")
(cdr seq)  ; legacy function name ("contents of decrement register")

(first seq)
(rest seq)

(define (init seq)
    (reverse (cdr (reverse seq))))

(init seq)
(last seq)

(cons 0 seq)  ; "construct"


; recursive sequence sum definition
(define (add-all seq)
    (if (null? seq)
        0
        (+ (car seq) (sum (cdr seq)))))

(add-all seq)


(string-append "abc" "def")
(apply string-append '("abc" "def"))


(foldl + 0 (range 1 9))
(apply + (range 1 9))


(define (list-of-atoms? seq)
    (if (null? seq)
        #t
        (and
            (atom? (car seq))
            (list-of-atoms? (cdr seq)))))



(define stack '(1 2 3 4 5))
(cons (first stack) stack)