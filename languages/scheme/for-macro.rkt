#lang racket


(define-syntax o-for
  (syntax-rules (car cdr if lambda let letrec null? void)
    ((_ ((var lst)) body ...)
     (letrec ((recur (lambda (ls)
                       (if (null? ls)
                           (void)
                           (let ((var (car ls)))
                             body ...
                             (recur (cdr ls)))))))
       (recur lst)))))


(o-for ((a (range (add1 8))))
  (displayln (* a a a)))
