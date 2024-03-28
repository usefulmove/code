#lang scheme

(define (sum-up-to-threshold numbers threshold)
  (call/cc (lambda (return-to-context)
             (foldl
              (lambda (a acc)
                (if (> acc threshold)
                    (return-to-context acc)
                    (+ acc a)))
              0
              numbers))))

(sum-up-to-threshold '(1 2 3 4 5 6 7 8 9 10) 15) ; 21
