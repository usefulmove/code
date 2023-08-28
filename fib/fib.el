(defun fib (n)
  (cond ((< n 2) n)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

; tail recursion
(defun fib2 (n)
  (letrec
      ((fibonacci2 (lambda (n a b)
                     (cond ((= 0 n) a)
                           ((= 1 n) b)
                           (t (funcall fibonacci2 (- n 1) b (+ a b)))))))
    (funcall fibonacci2 n 0 1)))

(fib2 38)
