#lang racket


(let-syntax ((lettuce (syntax-rules ()
                        ((_ ((var val) ...) body1 body2 ...)
                         ((lambda (var ...)
                             body1 body2 ...)
                           val ...)))))
  (lettuce ((a 8))
    (* a a a)))


(let-syntax ((cube (syntax-rules ()
                     ((_ n)
                      ((lambda (a)
                          (* a a a))
                        n)))))
  (cube 8))
