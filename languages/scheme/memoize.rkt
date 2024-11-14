#lang racket

(define (memoize f)
  (define cache (make-hash))
  (lambda (x)
    (hash-ref! cache x (lambda () (f x)))))

(define (slow-fib n)
  (if (< n 2)
      n
      (+ (slow-fib (- n 1))
         (slow-fib (- n 2)))))

(define fast-fib
  (memoize slow-fib))

(slow-fib 38)
(fast-fib 38)
