; scan higher-order function
(define (scanl fn lst)
  (foldl
    (lambda (a acc) 
      (if (null? acc) (append acc (list a))
          (append acc (list (+ a (last acc))))))
    '()
    lst))
