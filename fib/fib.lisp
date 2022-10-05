(defun fib (n)
  (if (< n 3)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(write (fib 38)) (terpri)