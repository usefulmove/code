#lang racket


(define (fib n)
  (cond ((< n 2) n)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))


(define (fib2 n (a 0) (b 1))
  (cond ((= n 0) a)
        ((= n 1) b)
        (else (fib2 (- n 1) b (+ a b)))))


(define fib3
  (case-lambda
    ((n) (fib3 n 0 1))
    ((n a b) (cond ((= n 0) a)
                   ((= n 1) b)
                   (else (fib3 (- n 1) b (+ a b)))))))


(display (fib3 38))
