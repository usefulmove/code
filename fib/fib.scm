(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib2 n)
    (letrec
      ([fibonacci2 (lambda (x a b)
                       (cond [(= x 0) a]
                             [(= x 1) b]
                             [else (fibonacci2 (- x 1) b (+ a b))]))])
        (fibonacci2 n 0 1)))

(display (fib2 38))
