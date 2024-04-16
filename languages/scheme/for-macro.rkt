#lang racket


(define-syntax o-for
  (syntax-rules ()
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
