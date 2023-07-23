(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))

(define (fib2 n [a 0] [b 1])
    (cond [(zero? n) a]
          [(= n 1) b]
          [else (fib2 (sub1 n) b (+ a b))]))

(displayln (fib2 38))
