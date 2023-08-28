#lang racket

(define (fib n)
  (cond [(< n 2) n]
        [else (+ (fib (- n 1))
                 (fib (- n 2)))]))


(define (fib2 n)
    (letrec
      ([fibonacci2 (lambda (n a b)
                       (cond [(= n 0) a]
                             [(= n 1) b]
                             [else (fibonacci2 (- n 1) b (+ a b))]))])
        (fibonacci2 n 0 1)))

(display (fib2 38))
