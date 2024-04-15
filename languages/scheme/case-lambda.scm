#lang scheme

(define code
  (case-lambda
    ((a) (* a a))
    ((a b) (* a b))))

(code 8)
(code 3 2)
