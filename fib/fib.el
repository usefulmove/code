; default solution
(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; tail recursion
(defun fibonacci2 (n a b)
  (if (equal 0 n)
       a
       (if (equal 1 n)
           b
           (fibonacci2 (- n 1) b (+ a b)))))

(defun fib2 (n) (fibonacci2 n 0 1))
