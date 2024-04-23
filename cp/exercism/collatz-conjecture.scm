(import (rnrs))

(define collatz
  (case-lambda
    ((n) (collatz n 0))
    ((n acc) (cond ((= 1 n) acc)
                   ((even? n) (collatz (/ n 2) (add1 acc)))
                   ((odd? n) (collatz (+ 1 (* 3 n)) (add1 acc)))))))
