#lang racket

(define (prime? n)
  (cond ((<= n 1) #f)
        ((= n 2) #t)
        ((even? n) #f)
        (else
         (let loop ((i 3))
           (cond
             ((> (* i i) n) #t)
             ((zero? (remainder n i)) #f)
             (else (loop (+ i 2))))))))
