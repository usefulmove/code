#lang racket

(let loop ((a 8))
  (cond ((= 0 a) 0)
        (else (+ (* a a)
                 (loop (- a 1))))))
