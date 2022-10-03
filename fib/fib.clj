(def fib
  (fn [n] 
    (if (<= n 0)
      0
      (if (< n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))))

(println (fib 38))
