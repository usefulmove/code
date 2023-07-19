(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib2 n [a 0] [b 1])
  (cond
       [(= n 0) a]
       [(= n 1) b]
       [else (fib2 (- n 1) b (+ a b))]))

(displayln (fib2 38))
