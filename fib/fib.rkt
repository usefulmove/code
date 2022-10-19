(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

(define (fibonacci2 n a b)
  (if (= n 0)
    a
    (if (= n 1)
      b 
      (fibonacci2 (- n 1) b (+ a b)))))
  
(writeln (fib 38))
