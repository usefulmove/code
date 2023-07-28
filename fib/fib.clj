(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn fib2 [n]
  (let [fibonacci2 (fn [n a b] (cond (= n 0) a
                                     (= n 1) b
                                     :else (fibonacci2 (- n 1) b (+ a b))))]
    (fibonacci2 n 0 1)))

(println (fib2 38))
