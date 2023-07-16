#lang racket

; playground
(define cube
    (lambda (a) (* a a a )))

(define square
    (lambda (a) (* a a )))

(cube 8)

(foldl
    (lambda (a acc) (+ acc (square a)))
    0
    (range 1 (+ 8 1)))

(define golden-ratio
    (/ (- (sqrt 5) 1) 2))

golden-ratio


; define The Little Schemer atom? function
(define atom?
    (lambda (o) (and (not (pair? o)) (not (null? o)))))

(atom? (quote ()))
