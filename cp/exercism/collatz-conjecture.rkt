#lang racket

(provide collatz)

(define (collatz num (acc 0))
  (cond ((or (< num 1)
             (not (integer? num))) (error 'collatz "input error"))
        ((= 1 num) acc)
        ((odd? num) (collatz (+ (* 3 num) 1) (add1 acc)))
        ((even? num) (collatz (/ num 2) (add1 acc)))))
