#lang racket

(define cube
    (lambda (a) (* a a a )))

(define square
    (lambda (a) (* a a )))

(cube 8)

(foldl
    (lambda (a acc) (+ acc (square a)))
    0
    (range 1 (+ 8 1)))
