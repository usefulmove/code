#lang racket

#| REPL (read-eval-print-loop) |#


#| definitions and evalutation - S-expressions |#
; golden ratio


#| procedures |#
; define, lambda, procedure definition shortcut, +, -, *, /, sqr


#| lists |#
; cons, car, cdr, first, rest, last, list, null?, length, append, reverse


#| compound procedures |#
; substitution model
; magic 8 ball


#| conditional expressions |#
; if, cond


#| recursion |#
; factorial, fibonacci


#| higher-order functions |#
; map, filter, reduce


#| koans (fill in the underscore) |#

(= _ 8)

(= (+ 3 2) _)

(= 8 (* _ 2 2))

(= _ (+ (sqr 3) (sqr 4)))

(equal? '(_) (list 3 1 2))

(equal? (car (list 3 1 2)) _)

(equal? _ (cdr '(3 1 2)))

(equal? (cons 0 (list 3 1 2)) _)

(equal? 5 (_ '(3 1 2 4 6)))

(equal? 5 (_ '(3 1 2 5)))

(define add2 (lambda (n) _))

(define (cube n) _)