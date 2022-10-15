(defun fib (n)
  (if (< n 3)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defun fib2 (n)
  (defun fibonnaci2 (n a b)
    (if (= n 0)
      a
      (if (= n 1)
        b
        (fibonnaci2 (- n 1) b (+ a b)))))
  (fibonnaci2 n 0 1))

(write (fib2 38)) (terpri)