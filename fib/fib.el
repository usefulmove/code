; default solution
(defun fib (n)
  (cond ((< n 2) n)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

; tail recursion
(defun fibonacci2 (n a b)
  (cond ((= 0 n) a)
        ((= 1 n) b)
        (t (fibonacci2 (- n 1) b (+ a b)))))

(defun fib2 (n) (fibonacci2 n 0 1))
