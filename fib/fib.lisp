(defun fib (n)
  (if (= n 0)
    0
    (if (< n 3)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(write (fib 38)) (terpri)